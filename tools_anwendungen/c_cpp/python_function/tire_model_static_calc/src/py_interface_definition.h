
#ifndef CAR_MODEL_PY_INTERFACE_H_FILE
#define CAR_MODEL_PY_INTERFACE_H_FILE

#include "py_interface.h"

#ifndef PY_SSIZE_T_CLEAN
  #define PY_SSIZE_T_CLEAN
#endif

#ifdef _DEBUG
 #define CAR_MODEL_PY_INTERFACE_H_FILE_DEBUG_HOLD_SPEZIAL
 #undef _DEBUG
#endif

#include "Python.h"

#ifdef CAR_MODEL_PY_INTERFACE_H_FILE_DEBUG_HOLD_SPEZIAL
 #define  _DEBUG
#endif

#define NAME_OKAY_FLAG "okayflag"
#define NAME_ERR_TXT "errtxt"

struct SDefCarModelInterface
{
  char *NAME_Y;
  char *NAME_PARAM;
  char *TXT_OKAY_FLAG;
  char *TXT_ERR_TXT;
  char *TXT_DT_SIM;
  double DEFAULT_DT_SIM;
  SDefCarModelInterface()
    : NAME_Y("y")
    , NAME_PARAM("param")
    , TXT_OKAY_FLAG("okay")
    , TXT_ERR_TXT("errtext")
    , TXT_DT_SIM("dt_sim")
    , DEFAULT_DT_SIM(0.01)
  {}
};
extern const SDefCarModelInterface defstruct;


#endif