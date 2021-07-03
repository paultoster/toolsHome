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
  void CMod::registerIntegrationFunktion(std::size_t index)
  {

    switch (mModVec[index].state.statedef.intType)
    {
    case DEF_DOPRI45:

      // Build Input structure
      mModVec[index].state.pERKinp.reset();
      mModVec[index].state.pERKinp = std::make_shared<CIntegratorERKInp>();

      mModVec[index].state.pERKinp->n = mModVec[index].state.statedef.nstate;
      mModVec[index].state.pERKinp->pobj = mModVec[index].pmodel;
      mModVec[index].state.pERKinp->meth = 1; //choice of the coefficients 1:dopri45

                                              // add integration input as parameter
      registerIntegrationERKAdditionalToParam(index);
      break;
    case DEF_PIEULER:
    case DEF_PGEARS:
    case DEF_IEULER:
    case DEF_GEARS:

      // Build Input structure
      mModVec[index].state.pGEARinp.reset();
      mModVec[index].state.pGEARinp = std::make_shared<CIntegratorGEARInp>();

      mModVec[index].state.pGEARinp->n = mModVec[index].state.statedef.nstate;
      mModVec[index].state.pGEARinp->pobj = mModVec[index].pmodel;
      // switch for the choice of the coefficients
      if (mModVec[index].state.statedef.intType == DEF_PIEULER)     mModVec[index].state.pGEARinp->meth = 1; //1:paritial impl. Euler
      else if (mModVec[index].state.statedef.intType == DEF_PGEARS) mModVec[index].state.pGEARinp->meth = 2; //2 : Paratial Gears
      else if (mModVec[index].state.statedef.intType == DEF_IEULER) mModVec[index].state.pGEARinp->meth = 3; //3:implicit Euler
      else if (mModVec[index].state.statedef.intType == DEF_GEARS)  mModVec[index].state.pGEARinp->meth = 4; //4: Gears
                                                                                                             //                          //             
                                                                                                             // add integration input as parameter
      registerIntegrationGEARAdditionalToParam(index);
      break;
    case DEF_RAD5:

      // Build Input structure
      mModVec[index].state.pRAD5inp.reset();
      mModVec[index].state.pRAD5inp = std::make_shared<CIntegratorRAD5Inp>();

      mModVec[index].state.pRAD5inp->n = mModVec[index].state.statedef.nstate;
      mModVec[index].state.pRAD5inp->pobj = mModVec[index].pmodel;
      mModVec[index].state.pRAD5inp->itol = 1;
      mModVec[index].state.pRAD5inp->vatol = mModVec[index].state.statedef.absErrVec;
      mModVec[index].state.pRAD5inp->vrtol = mModVec[index].state.statedef.relErrVec;
      mModVec[index].state.pRAD5inp->ijac = mModVec[index].state.statedef.useJacobiFkt;


      // add integration input as parameter
      registerIntegrationRAD5AdditionalToParam(index);
      break;
    default:
      break;
    }


  }

  okay_t CMod::registerIntegrationERKAdditionalToParam(std::size_t &index)
  {
    slf::CParVarDefCollect statepar;

    statepar.SetMainGroup(mModVec[index].name);

    CStr str;

    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->itol);
    statepar.SetParDef(mModVec[index].state.pERKinp->itol, "dopri45", "itol", "", str.c_str(), "itol=0, skalar err(y[i]) < rtol*abs(y[i])+atol; itol=1, vector err(y[i]) < vrtol[i]*abs(y[i])+vatol[i]");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->rtol);
    statepar.SetParDef(mModVec[index].state.pERKinp->rtol, "dopri45", "rtol", "", str.c_str(), "relative  Toleranz itol = 0");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->atol);
    statepar.SetParDef(mModVec[index].state.pERKinp->atol, "dopri45", "atol", "", str.c_str(), "absolute Toleranz itol = 0");

    str = SlfPrintStringVector(mModVec[index].state.pERKinp->vrtol);
    statepar.SetParDef(mModVec[index].state.pERKinp->vrtol, "dopri45", "vrtol", "", str.c_str(), "relative  Vektor Toleranz itol = 1");
    str = SlfPrintStringVector(mModVec[index].state.pERKinp->vatol);
    statepar.SetParDef(mModVec[index].state.pERKinp->vatol, "dopri45", "vatol", "", str.c_str(), "absolute Vektor Toleranz itol = 1");

    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->hmax);
    statepar.SetParDef(mModVec[index].state.pERKinp->hmax, "dopri45", "hmax", "", str.c_str(), "maximal step size when 0.0 default=xend-x");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->hmin);
    statepar.SetParDef(mModVec[index].state.pERKinp->hmin, "dopri45", "hmin", "", str.c_str(), "kleinste Schrittweite defoult 0.");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->nmax);
    statepar.SetParDef(mModVec[index].state.pERKinp->nmax, "dopri45", "nmax", "", str.c_str(), "maximal number of allowed steps, default 100000");

    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->safe);
    statepar.SetParDef(mModVec[index].state.pERKinp->safe, "dopri45", "safe", "", str.c_str(), "safety factor, default 0.9");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->fac1);
    statepar.SetParDef(mModVec[index].state.pERKinp->fac1, "dopri45", "fac1", "", str.c_str(), "parameters for step size selection, wenn 0.0, dann default fac1=0.2 and fac2=10.0");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->fac2);
    statepar.SetParDef(mModVec[index].state.pERKinp->fac2, "dopri45", "fac2", "", str.c_str(), "fac1 <= hnew/hold <= fac2.");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->beta);
    statepar.SetParDef(mModVec[index].state.pERKinp->beta, "dopri45", "beta", "", str.c_str(), "for stabilized step size control, default beta=0.04");
    str = SlfPrintStringSingle(mModVec[index].state.pERKinp->safe);
    statepar.SetParDef(mModVec[index].state.pERKinp->safe, "dopri45", "safe", "", str.c_str(), "safety factor, default 0.9");

    // Add collected parameter to parvec
    std::size_t n = mModVec[index].npar;
    mModVec[index].npar += statepar.Size();
    mModVec[index].parvec.resize(mModVec[index].npar);
    for (std::size_t i = n; i < mModVec[index].npar; ++i)
    {
      mModVec[index].parvec[i].pardef = statepar.Get(i-n);
    }

    return OKAY;
  }
  okay_t CMod::registerIntegrationGEARAdditionalToParam(std::size_t &index)
  {
    slf::CParVarDefCollect statepar;

    statepar.SetMainGroup(mModVec[index].name);

    CStr str;

    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->itol);
    statepar.SetParDef(mModVec[index].state.pGEARinp->itol, "gear", "itol", "", str.c_str(), "itol=0, skalar err(y[i]) < rtol*abs(y[i])+atol; itol=1, vector err(y[i]) < vrtol[i]*abs(y[i])+vatol[i]");
    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->rtol);
    statepar.SetParDef(mModVec[index].state.pGEARinp->rtol, "gear", "rtol", "", str.c_str(), "relative  Toleranz itol = 0");
    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->atol);
    statepar.SetParDef(mModVec[index].state.pGEARinp->atol, "gear", "atol", "", str.c_str(), "absolute Toleranz itol = 0");

    str = SlfPrintStringVector(mModVec[index].state.pGEARinp->vrtol);
    statepar.SetParDef(mModVec[index].state.pGEARinp->vrtol, "gear", "vrtol", "", str.c_str(), "relative  Vektor Toleranz itol = 1");
    str = SlfPrintStringVector(mModVec[index].state.pGEARinp->vatol);
    statepar.SetParDef(mModVec[index].state.pGEARinp->vatol, "gear", "vatol", "", str.c_str(), "absolute Vektor Toleranz itol = 1");

    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->hmax);
    statepar.SetParDef(mModVec[index].state.pGEARinp->hmax, "gear", "hmax", "", str.c_str(), "maximal step size when 0.0 default=xend-x");
    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->hmin);
    statepar.SetParDef(mModVec[index].state.pGEARinp->hmin, "gear", "hmin", "", str.c_str(), "kleinste Schrittweite defoult 0.");
    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->nmax);
    statepar.SetParDef(mModVec[index].state.pGEARinp->nmax, "gear", "nmax", "", str.c_str(), "maximal number of allowed steps, default 100000");

    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->njacmax);
    statepar.SetParDef(mModVec[index].state.pGEARinp->njacmax, "gear", "njacmax", "", str.c_str(), "after how many iteration to calculate Jacobi");
    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->nit);
    statepar.SetParDef(mModVec[index].state.pGEARinp->nit, "gear", "nit", "", str.c_str(), "max iteration per time step");
    str = SlfPrintStringSingle(mModVec[index].state.pGEARinp->ijac);
    statepar.SetParDef(mModVec[index].state.pGEARinp->ijac, "gear", "ijac", "", str.c_str(), "calculation jacobi analytically (wih function)");

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
  okay_t CMod::registerIntegrationRAD5AdditionalToParam(std::size_t &index)
  {
    slf::CParVarDefCollect statepar;

    statepar.SetMainGroup(mModVec[index].name);

    CStr str;

    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->itol);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->itol, "rad5", "itol", "", str.c_str(), "itol=0, skalar err(y[i]) < rtol*abs(y[i])+atol; itol=1, vector err(y[i]) < vrtol[i]*abs(y[i])+vatol[i]");
    
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->rtol);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->rtol, "rad5", "rtol", "", str.c_str(), "relative  Toleranz itol = 0");
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->atol);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->atol, "rad5", "atol", "", str.c_str(), "absolute Toleranz itol = 0");

    str = SlfPrintStringVector(mModVec[index].state.pRAD5inp->vrtol);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->vrtol, "rad5", "vrtol", "", str.c_str(), "relative  Vektor Toleranz itol = 1");
    str = SlfPrintStringVector(mModVec[index].state.pRAD5inp->vatol);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->vatol, "rad5", "vatol", "", str.c_str(), "absolute Vektor Toleranz itol = 1");

    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->hmax);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->hmax, "rad5", "hmax", "", str.c_str(), "maximal step size when 0.0 default=xend-x");
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->hinit);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->hmin, "rad5", "hinit", "", str.c_str(), "kleinste Schrittweite defoult 0.");
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->nmax);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->nmax, "rad5", "nmax", "", str.c_str(), "maximal number of allowed steps, default 100000");
    
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->iout);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->iout, "rad5", "iout", "", str.c_str(), "write output to log");
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->nit);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->nit, "rad5", "nit", "", str.c_str(), "max iteration per time step");
    str = SlfPrintStringSingle(mModVec[index].state.pRAD5inp->ijac);
    statepar.SetParDef(mModVec[index].state.pRAD5inp->ijac, "rad5", "ijac", "", str.c_str(), "calculation jacobi analytically (wih function)");

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
  // calc Integration Init
  okay_t CMod::CalcIntegrationInit(std::size_t index)
  {
    if (mModVec[index].state.statedef.intType == DEF_DOPRI45)
    {
      mModVec[index].state.psolvERK.reset();
      mModVec[index].state.psolvERK = std::make_shared<CSolveODE_ERK>();
      //mModVec[index].state.psolver = std::shared_ptr<CSolveODE_ERK>(new CSolveODE_ERK);

      if (mModVec[index].state.psolvERK->init(*(mModVec[index].state.pERKinp)) != OKAY)
      {
        mMessage.Collect(mModVec[index].state.psolvERK->Message);
        return false;
      }
    }
    else if (mModVec[index].state.statedef.intType == DEF_RAD5)
    {
      mModVec[index].state.psolvRAD5.reset();
      mModVec[index].state.psolvRAD5 = std::make_shared<CSolveODE_RAD5>();
      //mModVec[index].state.psolver = std::shared_ptr<CSolveODE_RAD5>(new CSolveODE_RAD5);

      if (mModVec[index].state.psolvRAD5->init(*mModVec[index].state.pRAD5inp) != OKAY)
      {
        mMessage.Collect(mModVec[index].state.psolvRAD5->Message);
        return false;
      }
    }

    return true;
  }
  // calc Integration First
  okay_t CMod::CalcIntegrationFirst(std::size_t index)
  {

    if (mModVec[index].state.statedef.intType == DEF_DOPRI45)
    {
    
      if (mModVec[index].state.psolvERK->calcFirst( mModVec[index].t, 
                                                   mModVec[index].state.statedef.stateVec, 
                                                   mModVec[index].state.pERKinp->hmin) != OKAY)
      {
        mMessage.Collect(mModVec[index].state.psolvERK->Message);
        return false;
      }
    }
    else if (mModVec[index].state.statedef.intType == DEF_RAD5)
    {
      if (mModVec[index].state.psolvRAD5->calcFirst(mModVec[index].t,
        mModVec[index].state.statedef.stateVec,
        mModVec[index].state.pRAD5inp->hinit) != OKAY)
      {
        mMessage.Collect(mModVec[index].state.psolvRAD5->Message);
        return false;
      }

    }

    return true;
  }
  // calc Integration step
  okay_t CMod::CalcIntegrationStep(std::size_t index)
  {
    
    if (mModVec[index].state.statedef.intType == DEF_DOPRI45)
    {
      if (mModVec[index].state.psolvERK->calc(mModVec[index].t) != OKAY)
      {
        mMessage.Collect(mModVec[index].state.psolvERK->Message);
        return false;
      }

      // get states
      mModVec[index].state.statedef.stateVec = mModVec[index].state.psolvERK->yRead();
    }
    else if (mModVec[index].state.statedef.intType == DEF_RAD5)
    {
      if (mModVec[index].state.psolvRAD5->calc(mModVec[index].t) != OKAY)
      {
        mMessage.Collect(mModVec[index].state.psolvRAD5->Message);
        return false;
      }

      // get states
      mModVec[index].state.statedef.stateVec = mModVec[index].state.psolvRAD5->yRead();
    }

    return true;
  }


} // slf
