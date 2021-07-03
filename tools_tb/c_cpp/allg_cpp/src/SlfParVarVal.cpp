// SlfParVar.cpp
//
// Parameter Variablen der Modelle 
//
#include "SlfParVarVal.h"

namespace slf
{

  // search for name and group return true and give back isearch
  bool CParVarValCollect::Search(const CStr &name, const CStrV &group, std::size_t &isearch) const
  {
    std::size_t index;
    for (index = 0; index < parvarDefVec.size(); ++index)
    {
      if (parvarDefVec[index].name.compareText(name) && parvarDefVec[index].pargroupvec.compare(group))
      {
        isearch = index;
        return true;
      }
    }

    return false;
  }

} // namespace slf
