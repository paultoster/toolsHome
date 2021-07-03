// SlfParVar.cpp
//
// Parameter Variablen der Modelle 
//
#include "SlfParVar.h"

namespace slf
{
  CParVarDef& CParVarDef::operator=(const CParVarDef& v)
  {
    CVarDef::operator=(v);
    pargroupvec = v.pargroupvec;
    defaultvalue     = v.defaultvalue;
    return *this;
  }


} // namespace slf
