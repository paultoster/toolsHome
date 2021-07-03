#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sstream>
#include <iomanip>
#include <memory>
#include "SlfModCtrl.h"
#include "SlfPar.h"
#include "SlfPrint.h"
#include "SlfSolveODE_ERK.h"


namespace slf
{


  // register model     =========================================
  //==============================================================
  // hand over model definition and time step size for the model
  // for n models call function n-times
  //==============================================================
  okay_t CMod::registerModul(CModelBase &model,const double dt)
  {
    SModStruct mod(model, dt);
    mModVec.push_back(mod);
    
    //from all registered moduls input and out wil be registered
    registerIOPSOfModuls(mNMod);
    
    ++mNMod;

    if (pLog) pLog->writeMessage(mMessage);
    mMessage.Clear();

    return mMessage.IsOkay();
  }
  // register time     =========================================
  //==============================================================
  // Hand over end time and dt_ouput
  //==============================================================
  okay_t CMod::registerTime(const double tend, const double dt)
  {
    mTSEnd = SLF_TIME_TO_TSTAMP(tend+ SLF_TSTAMP_HALF_STEP);
    mDtTS  = SLF_TIME_TO_TSTAMP(dt+ SLF_TSTAMP_HALF_STEP);
    return mMessage.IsOkay();
  }
  // initialize model control ====================================
  //==============================================================
  // make initialize by handover time step size for one general step
  // 1) all exteral input will be registered as outputs
  // 2) all exteral outputs will be registered as inputs
  // 3) build and check all connections to find an output for each input
  // 4) build time frames based on smallest time step and collect all states from modules
  // 5) write log output
  //==============================================================
  okay_t CMod::init(void)
  {
    // 1) register External Input from ExtInpDef
    if( mMessage.IsOkay() ) registerExtInp();

    // 2) register External Output from ExtOutDef
    if( mMessage.IsOkay() ) registerExtOut();

    // 3) build input, output connection
    if (mMessage.IsOkay()) buildModulsIO();

    // 4) check if all parameter are set from outside or from default
    if (mMessage.IsOkay()) checkParameter();

    // 5) build time frame and state definition
    if (mMessage.IsOkay()) buildModulsState();

    // 6) Log Modul and their connection
    if (mMessage.IsOkay()) logModulIO();

    // 7) make init of Modul (send over all timestep)
    if (mMessage.IsOkay())
    {
      for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
      {
        // call Modul
        if (!mModVec[index].pmodel->Init(SLF_TIMESTAMP_TO_TIME(mDtTS, double)))
        {
          mMessage.Collect(mModVec[index].pmodel->GetMessagePointer());
          if (pLog) pLog->writeMessage(mMessage);
          mMessage.Clear();
          return false;
        }

        // Set Integration Funktion for each modul
        // go over each model
        if (!CalcIntegrationInit(index))
        {
          if (pLog) pLog->writeMessage(mMessage);
          mMessage.Clear();
          return false;
        }
      }
    }

    if (pLog) pLog->writeMessage(mMessage);
    mMessage.Clear();

    return true;
  }
  // first calculation  ==========================================
  //==============================================================
  // make first calculation with t = 0, external input must be set
  //==============================================================
  okay_t CMod::first(void)
  {
    mTSCalc = 0;
    double t0 = 0.;
    for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
    {
      // Simtime
      mModVec[index].tTS = 0;
      mModVec[index].t = SLF_TIMESTAMP_TO_TIME(mModVec[index].tTS, double);

      // Set Input in Modul
      SetModInput(index);

      // call Modul
      if( !mModVec[index].pmodel->First(t0) )
      {
        mMessage.Collect(mModVec[index].pmodel->GetMessagePointer());
        if (pLog) pLog->writeMessage(mMessage);
        mMessage.Clear();
        return false;
      }

      // call Integration
      if( !CalcIntegrationFirst(index) )
      {
        if (pLog) pLog->writeMessage(mMessage);
        mMessage.Clear();
        return false;
      }


    }

    // Set external Output values
    SetModInput(INDEX_EXT_OUTPUT);

    if (pLog) pLog->writeMessage(mMessage);
    mMessage.Clear();

    return true;
  }
  // loop calculation  ===========================================
  //==============================================================
  // make first calculation with t = 0, external input must be set
  //==============================================================
  okay_t CMod::step(void)
  {
    timestamp_t ts = mTSCalc + mDtTS;
    while (mTSCalc < ts)
    {
      // Add Calculation Time
      mTSCalc += mDtTSCalc;

      // go over moduls
      for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
      {
        // check if Calculation time is at modul calculation time
        if (mTSCalc >= (mModVec[index].tTS + mModVec[index].dtTS))
        {
          // Set Input in Modul
          SetModInput(index);

          // Add modul calculation time
          mModVec[index].tTS += mModVec[index].dtTS;
          mModVec[index].t   = SLF_TIMESTAMP_TO_TIME(mModVec[index].tTS, double);

          // Integration
          if (!CalcIntegrationStep(index))
          {
            if (pLog) pLog->writeMessage(mMessage);
            mMessage.Clear();
            return false;
          }

          // output calculation          
          if (!mModVec[index].pmodel->Output(mModVec[index].t, mModVec[index].state.statedef.stateVec))
          {
            mMessage.Collect(mModVec[index].pmodel->GetMessagePointer());
            if (pLog) pLog->writeMessage(mMessage);
            mMessage.Clear();
            return false;
          }
        }
      }
    }

    // Set external Output values
    SetModInput(INDEX_EXT_OUTPUT);

    if (pLog) pLog->writeMessage(mMessage);
    mMessage.Clear();

    return true;
  }
  bool CMod::isEndReached(void)
  {
    if ((mTSCalc >= mTSEnd) && (mTSEnd)) return true;
    else                                 return false;
  }

  //Externer Input regestrieren ==================================
  //==============================================================
  // Übergabe Variablen-Vektor Externer Input als Output
  //==============================================================
  okay_t CMod::registerExtInp(void)
  {
    mModVec[INDEX_EXT_INPUT].ninp = 0;
    mModVec[INDEX_EXT_INPUT].nout = ExtInpDef.Size();
    mModVec[INDEX_EXT_INPUT].outvec.resize(mModVec[INDEX_EXT_INPUT].nout);
    for (std::size_t i = 0; i < mModVec[INDEX_EXT_INPUT].nout; ++i)
    {
      mModVec[INDEX_EXT_INPUT].outvec[i].vardef = ExtInpDef.Get(i);
    }
    return mMessage.IsOkay();
  }
  //Externer Ouput regestrieren ==================================
  //==============================================================
  // Übergabe Variablen-Vektor Externer Output als input
  //==============================================================
  okay_t CMod::registerExtOut(void)
  {
    mModVec[INDEX_EXT_OUTPUT].ninp = ExtOutDef.Size();
    mModVec[INDEX_EXT_OUTPUT].inpvec.resize(mModVec[INDEX_EXT_OUTPUT].ninp);
    for (std::size_t i = 0; i < mModVec[INDEX_EXT_OUTPUT].ninp; ++i)
    {
      mModVec[INDEX_EXT_OUTPUT].inpvec[i].vardef = ExtOutDef.Get(i);
    }
    mModVec[INDEX_EXT_OUTPUT].nout = 0;
    return mMessage.IsOkay();
  }

  okay_t CMod::registerIOPSOfModuls(std::size_t &index)
  {
    //- input ------------------------------------------
    CVarDefCollect col = mModVec[index].pmodel->DefineInput();

    mModVec[index].ninp = col.Size();

    mModVec[index].inpvec.resize(mModVec[index].ninp);
    for (std::size_t i = 0; i < mModVec[index].ninp; ++i)
    {
      mModVec[index].inpvec[i].vardef = col.Get(i);
    }

    //- output ------------------------------------------
    col = mModVec[index].pmodel->DefineOutput();
    mModVec[index].nout = col.Size();

    mModVec[index].outvec.resize(mModVec[index].nout);
    for (std::size_t i = 0; i < mModVec[index].nout; ++i)
    {
      mModVec[index].outvec[i].vardef = col.Get(i);
    }

    //- parameter ----------------------------------------
    CParVarDefCollect pcol = mModVec[index].pmodel->DefineParameter();
    mModVec[index].npar = pcol.Size();

    mModVec[index].parvec.resize(mModVec[index].npar);
    for (std::size_t i = 0; i < mModVec[index].npar; ++i)
    {
      mModVec[index].parvec[i].pardef = pcol.Get(i);
    }

    //- state --------------------------------------------
    mModVec[index].state.statedef = mModVec[index].pmodel->DefineState();

    // add state0-definition as parameter for each state
    registerState0AsAdditionalToParam(index);

    // Set Integration Funktion for each modul
    registerIntegrationFunktion(index);
    
    return mMessage.IsOkay();
  }
  okay_t CMod::registerState0AsAdditionalToParam(std::size_t &index)
  {
    slf::CParVarDefCollect statepar;

    //double s0;
    //double massVeh;

    statepar.SetMainGroup(mModVec[index].name);



    for (std::size_t i = 0; i < mModVec[index].state.statedef.nstate; ++i)
    {
      // is Name defined
      if (i < mModVec[index].state.statedef.stateNameVec.size())
      {
        if (mModVec[index].state.statedef.stateNameVec[i].size())
        {
          CStr name = mModVec[index].state.statedef.stateNameVec[i];
          CStr unit;
          if (i < mModVec[index].state.statedef.stateUnitVec.size())
          {
            unit = mModVec[index].state.statedef.stateUnitVec[i];
          }

          // set state as parameter with default 0.0 as value
          statepar.SetParDef(mModVec[index].state.statedef.stateVec[i], "state", name.c_str(), unit.c_str(), "0.0", "velocity");

        }
      }
    }

    // Add collected parameter to parvec
    std::size_t n = mModVec[index].npar;
    mModVec[index].npar += statepar.Size();
    mModVec[index].parvec.resize(mModVec[index].npar);
    for (std::size_t i = n; i < mModVec[index].npar; ++i)
    {
      mModVec[index].parvec[i].pardef = statepar.Get(i - n);
    }

    return OKAY;
  }
  okay_t CMod::buildModulsIO(void)
  {
    // IO-Verknüpfung für jedes Modul, welcher output zu dem jeweligen Modul-Input
    //============================================================================

    // Schleife über alle Module
    for (std::size_t index = 0; index < mNMod; ++index)
    {
      // Schleife über alle inputs
      for (std::size_t i = 0; i < mModVec[index].ninp; ++i)
      {
        mModVec[index].inpvec[i].found = searchInOutputs(mModVec[index].inpvec[i].vardef.name
          , index
          , mModVec[index].inpvec[i].igetmod
          , mModVec[index].inpvec[i].igetvar);

        if (!mModVec[index].inpvec[i].found)
        {
          std::stringstream ss;
          ss << "For Modul [" << mModVec[index].name << "] ";
          ss << "Inputvariable " << mModVec[index].inpvec[i].vardef.name << " ";
          ss << "no connection to Input or a Modul is found";
          mMessage.SetErr(INPUT_NOT_FOUND, ss.str().c_str());
        }
        // finde Faktor and offset
        else
        {
          if( !mModVec[index].inpvec[i].vardef.calcFrom(mModVec[mModVec[index].inpvec[i].igetmod].outvec[mModVec[index].inpvec[i].igetvar].vardef
            , mModVec[mModVec[index].inpvec[i].igetmod].name
            , mModVec[index].name)
            )
          {
            mMessage.Collect(mModVec[index].inpvec[i].vardef.message);
          }
        }
      }
    }

    // factor und offset bei jeder Zuordnung finden
    //============================================================================
    return mMessage.IsOkay();
  }
  // check parameter if set or will be set with default
  okay_t CMod::checkParameter(void)
  {

    // go over each model
    for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
    {
      // check each parameter
      for (std::size_t i = 0; i < mModVec[index].npar; ++i)
      {
        // Check if value is set
        if (!mModVec[index].parvec[i].pardef.ValueIsSet())
        {
          CStr defaultstr;

          // if not look for default value
          defaultstr = mModVec[index].parvec[i].pardef.defaultvalue;

          if (defaultstr.size())
          {
            CStr texterr;
            if (SetParameterFromDefault(defaultstr, mModVec[index].parvec[i].pardef, texterr) != OKAY)
            {
              std::stringstream ss;
              ss << "For Modul [" << mModVec[index].name << "] ";
              ss << "in Parameter " << mModVec[index].parvec[i].pardef.name << " ";
              ss << "defualt value: " << defaultstr << " could not be read!!!";
              mMessage.SetErr(PAR_DEFAULT_VALUE, ss.str().c_str());
              return NOT_OKAY;
            }
          }

          // if default is set
          if (!mModVec[index].parvec[i].pardef.ValueIsSet())
          {
            std::stringstream ss;
            ss << "For Modul [" << mModVec[index].name << "] ";
            ss << "and Parameter " << mModVec[index].parvec[i].pardef.name << " ";
            ss << "no value set!!! ";
            mMessage.SetErr(PAR_PARSET, ss.str().c_str());
            return NOT_OKAY;

          }
        } // Check if value is set
        
      }
    }
    return OKAY;
  }
  // build time frame and state definition
  void CMod::buildModulsState(void)
  {
    // identify smallest timestep
    mDtTSCalc = mDtTS;
    for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
    {
      mDtTSCalc = SLF_MIN(mModVec[index].dtTS, mDtTSCalc);
    }

  }

  void CMod::logModulIO(void)
  {
    std::stringstream ss;
    char *p1 = "======================================================================================";
    char *pi = "--------------------input-------------------------------------------------------------";
    char *po = "--------------------output------------------------------------------------------------";
    char *pp = "--------------------parameter---------------------------------------------------------";

    // Schleife über alle Module für IO-Beschreibung
    //==============================================
    for (std::size_t index = 0; index < mNMod; ++index)
    {
      ss << p1 << std::endl << "modul : [" << mModVec[index].name << "]";
      mMessage.SetLog(ss.str().c_str());
      ss.str("");
      if (index != INDEX_EXT_INPUT)
      {
        if (index == INDEX_EXT_OUTPUT) mMessage.SetLog(po);
        else                           mMessage.SetLog(pi);
        // Schleife über alle inputs
        for (std::size_t i = 0; i < mModVec[index].ninp; ++i)
        {
          ss << " name : " << std::left << std::setw(20) << mModVec[index].inpvec[i].vardef.name << "unit : " << std::setw(20) << mModVec[index].inpvec[i].vardef.unit << "comment : " << mModVec[index].inpvec[i].vardef.comment;
          mMessage.SetLog(ss.str().c_str());
          ss.str("");

          if (mModVec[index].inpvec[i].found)
          {
            ss << " from : [";
            ss << mModVec[mModVec[index].inpvec[i].igetmod].name;
            ss << "].";
            ss << mModVec[mModVec[index].inpvec[i].igetmod].outvec[mModVec[index].inpvec[i].igetvar].vardef.name;
            ss << "   ";

            if (mModVec[index].inpvec[i].vardef.status == DEFAULT_FAC_OFFSET)
            {
              ss << "(factor : 1.0;  offset : 0.0" << ")" << std::endl;
            }
            else if (mModVec[index].inpvec[i].vardef.status == FAC_OFFSET_SET)
            {
              ss << "(factor : " << mModVec[index].inpvec[i].vardef.factor << "; offset : " << mModVec[index].inpvec[i].vardef.offset << ")" << std::endl;
            }
            else
            {
              ss << "(factor and offset not found)" << std::endl;
            }
            mMessage.SetLog(ss.str().c_str());
            ss.str("");
          }
          else
          {
            ss << " from : not found";
            mMessage.SetLog(ss.str().c_str());
            ss.str("");
          }
          mMessage.SetLog(" ");
        }
      }
      if (index != INDEX_EXT_OUTPUT)
      {
        if (index == INDEX_EXT_INPUT) mMessage.SetLog(pi);
        else                          mMessage.SetLog(po);
        // Schleife über alle inputs
        for (std::size_t i = 0; i < mModVec[index].nout; ++i)
        {
          ss << " name : " << std::left << std::setw(20) << mModVec[index].outvec[i].vardef.name << "unit : " << std::setw(20) << mModVec[index].outvec[i].vardef.unit << "comment : " << mModVec[index].outvec[i].vardef.comment;
          mMessage.SetLog(ss.str().c_str());
          ss.str("");

          mMessage.SetLog(" ");
        }
      }

      // Parameter
      if ((index != INDEX_EXT_INPUT) && (index != INDEX_EXT_OUTPUT))
      {
        mMessage.SetLog(pp);

        // Schleife über alle Parameter
        for (std::size_t i = 0; i < mModVec[index].npar; ++i)
        {
          ss << " name : " << std::left << std::setw(20) << mModVec[index].parvec[i].pardef.name << "unit : " << std::setw(20) << mModVec[index].parvec[i].pardef.GetUnit() << "comment : " << mModVec[index].parvec[i].pardef.comment;
          mMessage.SetLog(ss.str().c_str());

          ss.str("");
          ss << " type : " << std::left << std::setw(20) << mModVec[index].parvec[i].pardef.GetType();
          ss << " default : " << std::left << std::setw(20) << mModVec[index].parvec[i].pardef.defaultvalue;
          mMessage.SetLog(ss.str().c_str());

          ss.str("");
          ss << " value: " << mModVec[index].parvec[i].pardef.PrintValue();
          mMessage.SetLog(ss.str().c_str());

          ss.str("");
          mMessage.SetLog(" ");
        }
      }
    }
    // Time Steps
    //===========
    ss.str("");
    ss << p1 << std::endl;
    ss << "EndTime                       TSend     = " << SLF_TIMESTAMP_TO_TIME(mTSEnd, double) << "[s]" << std::endl;
    ss << "General time step of modul    dtTS      = " << SLF_TIMESTAMP_TO_TIME(mDtTS, double) << "[s]" << std::endl;
    ss << "Calculation time step         dtTSCalc  = " << SLF_TIMESTAMP_TO_TIME(mDtTSCalc, double) << "[s]" << std::endl;

    for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
    {
      ss << "modul [" << mModVec[index].name << "] time step     dtTSModul = " << SLF_TIMESTAMP_TO_TIME(mModVec[index].dtTS, double) << "[s]" << std::endl;
    }
    mMessage.SetLog(ss.str().c_str());
    ss.str("");

    // End
    //====
    ss << p1 << std::endl;
    mMessage.SetLog(ss.str().c_str());
  }
  bool CMod::searchInOutputs(const CStr &name, std::size_t indexnot, std::size_t &indexfound, std::size_t &ifound)
  {
    // Schleife über alle Module
    for (std::size_t index = 0; index < mNMod; ++index)
    {
      // Schleife über alle outputs
      for (std::size_t i = 0; i < mModVec[index].nout; ++i)
      {
        if (index != indexnot)
        {
          if (name.compareText(mModVec[index].outvec[i].vardef.name))
          {
            indexfound = index;
            ifound = i;
            return true;
          }
        }
      }
    }
    return false;
  }
  void CMod::SetModInput(std::size_t index)
  {
    for (std::size_t i = 0; i < mModVec[index].ninp; ++i)
    {
      mModVec[index].inpvec[i].vardef.GetValue(mModVec[mModVec[index].inpvec[i].igetmod].outvec[mModVec[index].inpvec[i].igetvar].vardef);
    }
  }
  // printParameterTemplate all parameter and states registered in registerIOPSOfModuls() are printed in a template file
  okay_t CMod::printParameterTemplate(const char *filename)
  {
    CStrV temp_file_vec;
    for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
    {
      CParVarDefCollect pardefcollect;

      for (std::size_t i = 0; i < mModVec[index].npar; ++i)
      {
        pardefcollect.Add(mModVec[index].parvec[i].pardef);
      }
      WriteParameterStructure(pardefcollect, temp_file_vec);
    }

    if (SlfFktWriteAsciiToFile(temp_file_vec, filename) != OKAY)
    {
      std::stringstream ss;
      ss << "Error printParameterTemplate: File <" << filename << "> ";
      ss << "could not be written!!!";
      mMessage.SetErr(PAR_TEMPLATE_FILE, ss.str().c_str());
      
      return NOT_OKAY;
    }

    return OKAY;
  }
  // set Parameter in Models
  okay_t CMod::setParameter(const CParVarValCollect &pvvc)
  {
    for (std::size_t index = INDEX_MODUL_FIRST; index < mNMod; ++index)
    {
      for (std::size_t i = 0; i < mModVec[index].npar; ++i)
      {
        SetParameterFromVarValCollect(pvvc, mModVec[index].parvec[i].pardef);
      }
    }
    return OKAY;
  }
} // slf
