// SlfModState.h
//
// Variablen der Modelle 
//
#include "SlfBase.h"
#include "SlfStr.h"
#include "SlfStrV.h"
#include "SlfStrM.h"
#include "SlfNum.h"
#include "SlfMessage.h"
#include "SlfFkt.h"

#ifndef DS_MODSTATE_H_INCLUDED
#define DS_MODSTATE_H_INCLUDED

namespace slf
{
  // Var Types
  enum EIntegrationType 
  { DEF_STEP = 0   // no intergation 
  , DEF_DOPRI45    // expliziter Runge-Kutta, Dormand & Prince 4(5) Ordnung
  , DEF_PIEULER    // partial implicit euler (Teilimpliziter Euler)
  , DEF_PGEARS     // partial implicit Gears-Mehrschrittverfahren (Teilimpliziter Gears)
  , DEF_IEULER     // implicit Euler
  , DEF_GEARS      // implizites Gears-Mehrschrittverfahren
  , DEF_RAD5      // impliziter Runge-Kutta, Radau
  };



  //===============================================================================================
  // class definition of state information CVectorD List
  //===============================================================================================
  class CStateDef
  {
  public:

    EIntegrationType     intType;           // Type of integration
    std::size_t          nstate;
    CVectorD             stateVec;          // statevector set with nstate
    CVectorD             stateDerVec;       // state derivitive vector set with nstate
    CVectorD             relErrVec;         // relative Error for each state
    CVectorD             absErrVec;         // absolute Error for each state
    bool                 usePrestateFkt;    // flag if a prestatefunktion is provided
    bool                 useJacobiFkt;      // flag if a jacobi function is provided

    CStrV                stateNameVec;      // state names for each element can be set to be set in Parameterfile
    CStrV                stateUnitVec;       // unit definition for each element can be set to be set in Parameterfile
  public:
                                            // must be additional set
    CStateDef(EIntegrationType inttype,std::size_t n,bool useprestatfkt,bool usejacobifkt)
      : intType(inttype)
      , nstate(n)
      , stateVec(n)
      , stateDerVec(n)
      , relErrVec(n)
      , absErrVec(n)
      , usePrestateFkt(useprestatfkt)
      , useJacobiFkt(usejacobifkt)
      , stateNameVec()
      , stateUnitVec()
    {
      for (std::size_t i = 0; i < nstate; i++)
      {
        relErrVec[i] = 1.e-3;
        absErrVec[i] = 1.e-3;

        CStr name;
        name.catFormat("state_%i", i);
        stateNameVec.append(name);
        stateUnitVec.append("");
      }
    }

    // Add 
    void SetStateName(const std::size_t i,const char *name, const char *unit = "")
    {
      if (i < nstate)
      {
        stateNameVec.set(name,i);
        stateUnitVec.set(unit,i);
      }
    }
    void SetStateName(const std::size_t i, const CStr &name, const CStr &unit = CStr()) 
    { 
      SetStateName(i,name.c_str(), unit.c_str()); 
    }
    // set error to indexed state
    void SetErr(const std::size_t i, const double relErr, const double absErr)
    {
      if (i < nstate)
      {
        relErrVec[i] = relErr;
        absErrVec[i] = absErr;
      }
    }
    // set error to all states
    void SetErr(const double relErr, const double absErr)
    {
      for (std::size_t i = 0; i < nstate; i++)
      {
        relErrVec[i] = relErr;
        absErrVec[i] = absErr;
      }
    }

    CStateDef &operator=(const CStateDef& v)
    {
      nstate = v.nstate;
      intType = v.intType;
      stateVec = v.stateVec;
      stateDerVec = v.stateDerVec;
      relErrVec = v.relErrVec;
      absErrVec = v.absErrVec;
      stateUnitVec = v.stateUnitVec;
      usePrestateFkt = v.usePrestateFkt;
      useJacobiFkt = v.useJacobiFkt;
      stateNameVec = v.stateNameVec;
      return *this;
    }
  };

} // namespace slf

#endif