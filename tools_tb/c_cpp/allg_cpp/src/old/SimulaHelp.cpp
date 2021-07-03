//
// SimulaHelp.cpp
//
// Help-Functions

#include "SimulaHelp.h"
#include <sstream>
#include <string>

//internal Functions
void SimulaHelpUnitConvBereinigen(std::string &str);
//====================================================================================
//====================================================================================
// Unit-Convert
//====================================================================================
//====================================================================================

//================================================================
// EInheiten konvertieren
//================================================================
struct SSimulaHelpUnitConv {
    char * usearch;
    char * ubase;
    double faktor;
    double offset;
};

SSimulaHelpUnitConv SimulaHelpUnitConvList[] = {
#include "SlfFktUnitTable.h"
};
std::size_t NSimulaHelpUnitConvList = sizeof(SimulaHelpUnitConvList)/sizeof(SSimulaHelpUnitConv);
  
Simula::status_type Simula::convertUnit(const std::string &u_from,const std::string &u_to,double *pfak,double *poffset,std::string &errtext)
{
  return Simula::convertUnit(u_from.c_str(),u_to.c_str(),pfak,poffset,errtext);
}
  
Simula::status_type Simula::convertUnit(const char *u_from,const char *u_to,double *pfak,double *poffset,std::string &errtext)
{
    std::string UFrom = u_from;
    std::string UTo   = u_to;

    bool from_found = false;
    bool to_found   = false;
    
    std::size_t ifrom,ito;
    
    char *ubase_from;
    char *ubase_to;

    std::size_t i;

    // Bereinigen
    //-----------
    SimulaHelpUnitConvBereinigen(UFrom);
    SimulaHelpUnitConvBereinigen(UTo);

    // Sonderfälle
    //============
    if( UFrom == "0" ) { // Nullwert
    
        *pfak    = 0.0;
        *poffset = 0.0;

        return Simula::STATUS_OKAY;
    }
    if( UFrom == UTo ) {

        *pfak    = 1.0;
        *poffset = 0.0;
        return Simula::STATUS_OKAY;
    
    }

    // 1. Einheit (from) suchen
    for(i=0;i<NSimulaHelpUnitConvList;i++) {

        if( UFrom == SimulaHelpUnitConvList[i].usearch ) {
            from_found = true;
            ubase_from = SimulaHelpUnitConvList[i].ubase;
            ifrom      = i;

            break;
        }
    }

    if( !from_found ) 
    {
      std::stringstream out;
      out << "\nError SlfFktUnitConv: Die 1. Einheit (from) [" << UFrom.c_str() << "] konnte in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. ";
      out << "(2. Einheit (to) [" << UTo.c_str() << "])\n";
      errtext = out.str();
      return Simula::STATUS_NOT_OKAY;
    }

    // 2. Einheit (to) suchen
    for(i=0;i<NSimulaHelpUnitConvList;i++) {

        if( UTo == SimulaHelpUnitConvList[i].usearch ) {
            to_found = true;
            ubase_to = SimulaHelpUnitConvList[i].ubase;
            ito    = i;

            break;
        }
    }
    if( !to_found ) 
    {
      std::stringstream out;
      out << "\nError SlfFktUnitConv: Die 2. Einheit (to) [" << UTo.c_str() << "] konnte in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. ";
      out << "(1. Einheit (to) [" << UFrom.c_str() << "])\n";
      errtext = out.str();
      return Simula::STATUS_NOT_OKAY;
    }

    // Basis vergleichen
    //------------------
    if( strcmp(ubase_from,ubase_to) != 0 ) 
    {
      std::stringstream out;
      out << "\nError SlfFktUnitConv: Die 1. Einheit (to) [" << UFrom.c_str() << "] passt laut Umrechentabelle <SlfFktUnitTable.h> nicht mit der ";
      out << "2. Einheit (to) [" << UTo.c_str() << "] zusammen.\n";
      errtext = out.str();
      return Simula::STATUS_NOT_OKAY;
    }
    // Umrechnung
    // ifrom: unit_base = unit_from * fak_from + off_from;
    // ito:   unit_base = unit_to   * fak_to   + off_to;
    // =>
    // unit_to = unit_from*fak_from/fak_to + (off_from-off_to)/fak_to
    // fak     = fak_from/fak_to
    // off     = (off_from-off_to)/fak_to
    //
    *pfak    = SimulaHelpUnitConvList[ifrom].faktor
             / SIMULA_NOT_ZERO(SimulaHelpUnitConvList[ito].faktor);

    *poffset = (SimulaHelpUnitConvList[ifrom].offset-SimulaHelpUnitConvList[ito].offset)
             / SIMULA_NOT_ZERO(SimulaHelpUnitConvList[ito].faktor);

    return Simula::STATUS_OKAY;
}
void SimulaHelpUnitConvBereinigen(std::string &str)
{
    size_t i0;

    while( (i0=str.find_first_of(" ")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of(" ")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_first_of("[")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of("]")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_first_of("<")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of(">")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_first_of(" ")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of(" ")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
}

