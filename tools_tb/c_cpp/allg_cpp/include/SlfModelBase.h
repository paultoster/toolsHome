// SlfModelBase.h
//
// Basisbeschreibung für ein Model 
//
#ifndef SLF_MODEL_BASE_H_INCLUDED
#define SLF_MODEL_BASE_H_INCLUDED

#include "SlfBase.h"
#include "SlfStr.h"
#include "SlfNum.h"
#include "SlfModVar.h"
#include "SlfParVar.h"
#include "SlfModState.h"
#include "SlfMessage.h"
#include <vector>

namespace slf
{


  // Basismodell 
  //=================================================================================================
  class CModelBase {
  public:
    inline  CModelBase(const CStr &name) :mMessage(name.c_str()) { mModelName = name; }
    inline  CModelBase(const char *name) :mMessage(name) { mModelName = name; }
    virtual ~CModelBase() { }

    virtual CParVarDefCollect   DefineParameter(void)  { CParVarDefCollect p; return p; }    /* Prototype Parameter Dfeinition Liste*/
    virtual CVarDefCollect   DefineInput(void)      { CVarDefCollect i; return i; }    /* Prototype Input Dfeinition Liste*/
    virtual CVarDefCollect   DefineOutput(void)     { CVarDefCollect o; return o; }    /* Prototype Output Dfeinition Liste*/
    virtual CStateDef DefineState(void)      { CStateDef s(DEF_STEP,0,false,false); return s; }   /* Prototype State Dfeinition Liste*/

    virtual okay_t  Init(const double &dt)                                       { mMessage.SetErr(FUNCTION_NOT_DEFINED, "Init() not defined"); return NOT_OK; } /* Prototype Init */
    virtual okay_t  First(const double &t0)                                      { mMessage.SetErr(FUNCTION_NOT_DEFINED, "First() not defined"); return NOT_OK; } /* Prototype Erste Berechnung */
    virtual okay_t  Prestate(const double &tact)                                 { mMessage.SetErr(FUNCTION_NOT_DEFINED, "Prestate() not defined"); return NOT_OK; } /* Prototype Vorverarbeitung */
    virtual okay_t  StateDer(const double &tact, const CVectorD &x, CVectorD &xp) { mMessage.SetErr(FUNCTION_NOT_DEFINED, "State() not defined"); return NOT_OK; } /* Prototype State */
    virtual okay_t  Jacobian(const double &tact, const CVectorD &x, CMatrixD &dxpdx, uint32_t lddxpdx){ mMessage.SetErr(FUNCTION_NOT_DEFINED, "Jacobi() not defined"); return NOT_OK; } /* Prototype Jacobi */
    virtual okay_t  Mass(CMatrixD &mass){ mMessage.SetErr(FUNCTION_NOT_DEFINED, "Mass() not defined"); return NOT_OK; } /* Prototype Mass-Fkt */
    virtual okay_t  Output(const double &tact, const CVectorD &x)                { mMessage.SetErr(FUNCTION_NOT_DEFINED, "Output() not defined"); return NOT_OK; } /* Prototype Output */
    virtual okay_t  Done()                                                       { mMessage.SetErr(FUNCTION_NOT_DEFINED, "Done() not defined"); return NOT_OK; } /* Prototype Done */

    const char *GetName(void)            { return mModelName.c_str(); }
    const CMessage *GetMessagePointer(void)     { return &mMessage; }
    const char *GetErrText(void) { return mMessage.GetLastErrMess(); }

  protected:
    CStr                    mModelName;
    CMessage                mMessage;
  };

  struct SDsModFkt {
    char             *Name;     // Name
    CModelBase      **ppFkt;    // Wert
    char             *Comment;  // Comment
  };
}
#endif // SLF_MODEL_BASE_H_INCLUDED
