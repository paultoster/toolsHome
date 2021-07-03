//
// SimulaParamVar.h
//
// Parameter zur Simulation Simula einlesen und weiterverarbeiten

#ifndef SIMULA_PARAM_INTERPR_H
#define SIMULA_PARAM_INTERPR_H

#include <string>
#include <vector>
#include <string>
#include <sstream>
#include <typeinfo.h>
#include "SimulaDef.h"
#include "HlpString.h"


namespace Simula
{
  // class to intepretate readstring from parameter-File to Build Instnazes, Groups and Values, but left in strings
  class CParInterpr
  {
  private:

    std::string                ErrText;          // Error-String
    status_type                Status;           // status: STATUS_OKAY, STATUS_NOT_OKAY

    struct SValDef
    {
      std::vector<std::string>      Items;
      std::size_t                   n;
      std::size_t                   nrows;
      std::size_t                   ncols;
    };
    struct SVarDef
    {
      std::string                   Name;
      std::string                   Unit;
      std::vector<SValDef>          Values;   // possible to set instances
      std::string                   Copy;
      std::string                   Faktor;
      std::string                   Offset;
    };
    struct SMatDef
    {
      std::string                   FileName;
      std::vector<std::string>      Variables;
    };
    struct SGroupDef
    {
      std::string                   Name;
      std::string                   Copy;
      std::vector<struct SVarDef>   Variables;
      std::vector<struct SGroupDef> SubGroups;
      std::vector<struct SMatDef>   MatReads;
    };
    struct SInstGlob     // Instances global defined 
    {
      std::vector<struct SVarDef>    Variables;
      std::vector<struct SMatDef>    MatReads;
      std::vector<struct SGroupDef>  Groups;

    };

    std::vector<SInstGlob> InstGlobList;
 

  public:
    CParInterpr()
    {
      Status = STATUS_OKAY; 
    }
    ~CParInterpr()
    {
    }
	  status_type getStatus() {return Status;}
	  char *getErrText() {return (char *)ErrText.c_str();}

    // interpret string buffer from file
    status_type interprStringBuffer(std::string buffer);
  };
}

#endif