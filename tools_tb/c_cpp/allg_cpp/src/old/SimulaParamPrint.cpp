//
// SimulaParamPrint.cpp
//
#include <fstream>      // std::ofstream

#include "Simula.h"
#include "SimulaParam.h"
#include "SimulaHelp.h"
#include "HlpString.h"


//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
// Parameter Klasse File Funtionen Print
//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
namespace Simula
{

  //====================================================================================
  //====================================================================================
  // print-Functions
  //====================================================================================
  //====================================================================================
  // public: print  ParList in File
  //====================================================================================
  status_type CPar::printParList(char *fullfilename,bool allinst)
  {
    s_t          numberOfInstances;
    vec_string_t groupVec;
    std::string  actGroup;

    // number of instances to print
    //--------------------------------------------------
    if( allinst )
    {
      numberOfInstances = getMaxInstancesOfParList();
    }
    else
    {
      // only first isntance
      numberOfInstances = 1;
    }

    // Dateiausgabe
    //-------------------------------------------------------
    std::ofstream ofs (fullfilename, std::ofstream::out);

    // Prüfen
    if( !ofs.is_open() )
    {
      Status = STATUS_NOT_OKAY;
      std::string t  = "\nError sprintParList: Output-File: ";
      t             += fullfilename;
      t             += "could not be opened !!!" ;
      ErrText.append(t);
      Log.setLogPrintToScreen(true);
      Log.writeEndL(ErrText);
      return Status;
    }
    else
    {
      std::string t  = "Log sprintParList: open  Output-Par-File: ";
      t             += fullfilename;
      Log.writeEndL(t);
    }
    //
    if( printParListVarAndGroup(groupVec,actGroup,numberOfInstances,ofs) != STATUS_OKAY )
    {
      Status = STATUS_NOT_OKAY;
	    // Dateiausgabe schliessen
	    ofs.close();
      std::string t = "Error printParList: printParListVarAndGroup()";
      Log.setLogPrintToScreen(true);
      Log.writeEndL(t);
      Log.writeEndL(ErrText);
      return Status;
    }

  
    // Dateiausgabe schliessen
    //-----------------------------
    ofs.close();
    std::string t  = "Log sprintParList: close Output-Par-File: ";
    t             += fullfilename;
    Log.writeEndL(t);
    return STATUS_OKAY;
  }
  //====================================================================================
  // private: print  Var-liste and all Groupmemebers
  //====================================================================================
  status_type CPar::printParListVarAndGroup(vec_string_t &groupvec,std::string &actgroup,s_t &numberofinstances,std::ofstream &ofs)
  {
    std::size_t nvarnamelen,nunitlen,i,nhierachie = groupvec.size();
    vec_string_t listOfGroup;
    vec_uint_t   uiListOfVar;

    // print group with its hierachie
    //------------------------------------------------
    if( printParGroupItem(nhierachie, actgroup,ofs) != STATUS_OKAY )
    {
      return STATUS_NOT_OKAY;
    }

    // Liste aller Variablen ihierachie. Hierarchie
    //---------------------------------------------
    getListOfVarFromParList(groupvec,uiListOfVar);

    // Variablen aus dieser Liste ausdrucken
    //----------------------------------------------
    nvarnamelen = 0;
    nunitlen    = 0;
    for(i=0;i<uiListOfVar.size();++i)
    {
      nvarnamelen = SIMULA_MAX(nvarnamelen,ParList[uiListOfVar[i]]->getNameLength());
      nunitlen    = SIMULA_MAX(nunitlen,ParList[uiListOfVar[i]]->getUnitLength());
    }
    // Variablen aus dieser Liste ausdrucken
    //----------------------------------------------
    for(i=0;i<uiListOfVar.size();++i)
    {
      CParVarBase *pvar = ParList[uiListOfVar[i]];
      if( printParVarItem(pvar,nhierachie,nvarnamelen,nunitlen,numberofinstances,ofs) != STATUS_OKAY )
      {
        return STATUS_NOT_OKAY;
      }
    }


    // Liste aller Gruppen ihierachie. Hierachie
    //---------------------------------------------
    getListOfGroupFromParList(groupvec,listOfGroup);

    // Ausdrucken der nächsten Sub-Groupebene
    //---------------------------------------------
    for(i=0;i<listOfGroup.size();++i)
    {
      groupvec.push_back(listOfGroup[i]);
      if( printParListVarAndGroup(groupvec,listOfGroup[i],numberofinstances,ofs) != STATUS_OKAY )
      {
        return STATUS_NOT_OKAY;
      }
      groupvec.pop_back();
    }


    return STATUS_OKAY;
  }
  status_type CPar::printParGroupItem(std::size_t nhierarchie,std::string &group,std::ofstream &ofs)
  {

    if( nhierarchie && group.size() )  // print group if exist and nhierachie > 0   first, second
    {
      if( nhierarchie > (std::size_t)NGROUPSIGN ) // not enough subgroups
      {
        nhierarchie = NGROUPSIGN;
        std::stringstream t;
        t << "printParGroupItem: nhierarchie > NGROUPSIGN = " << NGROUPSIGN <<  "(need mor subgroups in SimulaDef.cpp)" << std::endl;
        ErrText.append(t.str().c_str());
        Log.setLogPrintToScreen(true);
        Log.writeEndL(ErrText);
      }

      ofs << std::endl << COMMENTSIGN << COMMENTLINE << std::endl;
      for(std::size_t i=0;i<nhierarchie;++i) ofs << SPACETOSEPERAT;
      --nhierarchie;
      ofs << GROUPSIGN0[nhierarchie] << group << GROUPSIGN1[nhierarchie] << std::endl;
      ofs.flush();
    }

    return STATUS_OKAY;
  }
  //####################################################################################
  // private: print  Var-Item
  //####################################################################################
  // CParVarBase *pvar          ointer to variable
  // s_t &numberofinstances    Anzahl der Instanzen, die ausgedruckt werden
  // std::ofstream &ofs        outputstream
  status_type CPar::printParVarItem(CParVarBase *pvar,s_t &nhierarchie, s_t &nvarnamelen, s_t &nunitlen, s_t &numberofinstances,std::ofstream &ofs)
  {
    std::size_t i,n;
    // print space due to hierachie
    for(i=0;i<nhierarchie;++i) ofs << SPACETOSEPERAT;
      
    // print name of variable
    //-----------------------
    ofs << pvar->getNameC();

    if( nvarnamelen >= pvar->getNameLength()) n = nvarnamelen - pvar->getNameLength();
    else                                      n = 0;
    for(i=0;i<n;++i) ofs << SPACE;

    // print unit
    //-----------------------
    if( pvar->getUnitLength() )
    {
      ofs << " " << UNITSIGN0 << pvar->getUnitC() << UNITSIGN1;

      if( nunitlen >= pvar->getUnitLength()) n = nunitlen - pvar->getUnitLength();
      else                                   n = 0;
      for(i=0;i<n;++i) ofs << SPACE;
    }
    else
    {
      n = nunitlen+strlen(UNITSIGN0)+strlen(UNITSIGN1);
      for(i=0;i<n;++i) ofs << SPACE;
    }


    // print values
    //-------------
    ofs << " " << EQUALSIGN;
    if( pvar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_INT  )
    {
      if( printParVarItemInt((CParVar<int> *)pvar,numberofinstances,ofs) != STATUS_OKAY )
      {
        return STATUS_NOT_OKAY;
      }
    }
    else if( pvar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_DOUBLE  )
    {
      if( printParVarItemDouble((CParVar<double> *)pvar,numberofinstances,ofs) != STATUS_OKAY )
      {
        return STATUS_NOT_OKAY;
      }
    }
    else if( pvar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_STRING  )
    {
      if( printParVarItemString((CParVar<std::string> *)pvar,numberofinstances,ofs) != STATUS_OKAY )
      {
        return STATUS_NOT_OKAY;
      }
    }
    ofs << std::endl;
    ofs.flush();


    return STATUS_OKAY;
  }
  // Int-Variable
  // CParVar<int> *pvar         pointer to variable
  // s_t &numberofinstances    Anzahl der Instanzen, die ausgedruckt werden
  // std::ofstream &ofs        outputstream
  //####################################################################################
  status_type CPar::printParVarItemInt(CParVar<int> *pvar,s_t &numberofinstances,std::ofstream &ofs)
  {
    // Schleife über entities
    //-----------------------
    for(std::size_t i=0;i<SIMULA_MAX(1,pvar->getNumOfEntities());++i)
    {
      // print Entities
      if( (numberofinstances > 1) && (pvar->getNumOfEntities() > 1 ) )
      {
        if( i == 0 ) ofs << " " << ENTITYSIGN0;
        else         ofs << " " << ENTITYDELIM; 
      }
      if( pvar->isVecSet(i) )
      {
        ofs << " " << pvar->getVal(i);
      }
      else
      {
        ofs << " " << "-99";
      }
    }
    // print Entities
    if( (numberofinstances > 1) && (pvar->getNumOfEntities() > 1 ) )
    {
      ofs << " " << ENTITYSIGN1;
    }

    return STATUS_OKAY;
  }
  // Double-Variable
  // CParVar<double> *pvar      pointer to variable
  // s_t &numberofinstances    Anzahl der Instanzen, die ausgedruckt werden
  // std::ofstream &ofs        outputstream
  //####################################################################################
  status_type CPar::printParVarItemDouble(CParVar<double> *pvar,s_t &numberofinstances,std::ofstream &ofs)
  {
    // Schleife über entities
    //-----------------------
    for(std::size_t i=0;i<SIMULA_MAX(1,pvar->getNumOfEntities());++i)
    {
      // print Entities
      if( (numberofinstances > 1) && (pvar->getNumOfEntities() > 1 ) )
      {
        if( i == 0 ) ofs << " " << ENTITYSIGN0;
        else         ofs << " " << ENTITYDELIM; 
      }
      if( pvar->isVecSet(i) )
      {
        ofs << " " << pvar->getVal(i);
      }
      else
      {
        ofs << " " << "-99";
      }
    }
    // print Entities
    if( (numberofinstances > 1) && (pvar->getNumOfEntities() > 1 ) )
    {
      ofs << " " << ENTITYSIGN1;
    }

    return STATUS_OKAY;
  }
  // string-Variable
  // CParVar<string> *pvar      pointer to variable
  // s_t &numberofinstances    Anzahl der Instanzen, die ausgedruckt werden
  // std::ofstream &ofs        outputstream
  //####################################################################################
  status_type CPar::printParVarItemString(CParVar<std::string> *pvar,s_t &numberofinstances,std::ofstream &ofs)
  {
    // Schleife über entities
    //-----------------------
    for(std::size_t i=0;i<SIMULA_MAX(1,pvar->getNumOfEntities());++i)
    {
      // print Entities
      if( (numberofinstances > 1) && (pvar->getNumOfEntities() > 1 ) )
      {
        if( i == 0 ) ofs << " " << ENTITYSIGN0;
        else         ofs << " " << ENTITYDELIM; 
      }
      if( pvar->isVecSet(i) )
      {
        ofs << " " << STRINGQUOT << pvar->getVal(i) << STRINGQUOT;
      }
      else
      {
        ofs << " " << STRINGQUOT << "-99" << STRINGQUOT;
      }
    }
    // print Entities
    if( (numberofinstances > 1) && (pvar->getNumOfEntities() > 1 ) )
    {
      ofs << " " << ENTITYSIGN1;
    }

    return STATUS_OKAY;
  }
}