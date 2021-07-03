//
// SimulaHelp.h
//
// Help Function in Simula

#ifndef SIMULA_HELP_H
#define SIMULA_HELP_H

#include <string>
#include <vector>
#include "SimulaDef.h"

namespace Simula
{
  status_type convertUnit(const std::string &u_from,const std::string &u_to,double *pfak,double *poffset,std::string &errtext);
  status_type convertUnit(const char *u_from,const char *u_to,double *pfak,double *poffset,std::string &errtext);

}

#endif