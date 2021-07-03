// DsModStruct.h
//
// Modell struktur 
//
#include "SlfBase.h"
#include "SlfStr.h"
#include "SlfModelBase.h"
#include "SlfSolveODE_ERK.h"
#include "SlfSolveODE_GEAR.h"
#include "SlfSolveODE_RAD5.h"

#include <memory>

#ifndef DS_MODSTRUCT_H_INCLUDED
#define DS_MODSTRUCT_H_INCLUDED

namespace slf
{
  struct SModStructInp
  {
    CVarDef       vardef;
    std::size_t   igetmod;
    std::size_t   igetvar;
    bool          found;
  };
  struct SModStructOut
  {
    CVarDef       vardef;
  };
  struct SModStructPar
  {
    CParVarDef  pardef;
  };
  struct SmodStructState
  {
    CStateDef                           statedef;
    std::shared_ptr<CSolveODE_ERK>      psolvERK;
    std::shared_ptr<CSolveODE_GEAR>     psolvGEAR;
    std::shared_ptr<CSolveODE_RAD5>     psolvRAD5;
    std::shared_ptr<CIntegratorERKInp>  pERKinp;
    std::shared_ptr<CIntegratorGEARInp> pGEARinp;
    std::shared_ptr<CIntegratorRAD5Inp> pRAD5inp;
    SmodStructState() :statedef(CStateDef(DEF_STEP, 0, false, false)){}
    SmodStructState &operator=(const SmodStructState& s)
    {
      pERKinp = s.pERKinp;
      pGEARinp = s.pGEARinp;
      pRAD5inp = s.pRAD5inp;
      psolvERK = s.psolvERK;
      psolvGEAR = s.psolvGEAR;
      psolvRAD5 = s.psolvRAD5;
      statedef = s.statedef;
      return *this;
    }
  };
  //===============================================================================================
  // struct definition of modul: name, ....
  //===============================================================================================
  struct SModStruct
  {
    CModelBase                   *pmodel;
    CStr                         name;
    timestamp_t                  dtTS;                                  // desired loop time in timestamp [µs]
    timestamp_t                  tTS;
    double                       t;
    EIntegrationType             intType;
    std::vector<SModStructInp>   inpvec;
    std::size_t                  ninp;
    std::vector<SModStructOut>   outvec;
    std::size_t                  nout;
    std::vector<SModStructPar>   parvec;
    std::size_t                  npar;
    SmodStructState              state;
    SModStruct(CModelBase  &m,const double dtin) :pmodel(&m), name(m.GetName()), dtTS(SLF_TIME_TO_TSTAMP(dtin+SLF_TSTAMP_HALF_STEP)) {}
    SModStruct(const char *namein) :pmodel(0), name(namein), dtTS(0) {}
    SModStruct(const SModStruct &s)
    {
      pmodel = s.pmodel;
      name = s.name;
      dtTS = s.dtTS;
      tTS = s.tTS;
      t = s.t;
      intType = s.intType;
      inpvec = s.inpvec;
      ninp = s.ninp;
      outvec = s.outvec;
      nout = s.nout;
      parvec = s.parvec;
      npar = s.npar;
      state = s.state;
    }
  };


} // namespace slf

#endif