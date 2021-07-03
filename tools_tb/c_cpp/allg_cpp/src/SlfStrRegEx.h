//==========================================
//regex-Funktion mit CStr ==================
//==========================================



#ifndef SLF_STR_REG_EX_H_INCLUDED
#define SLF_STR_REG_EX_H_INCLUDED

#include "SlfStrV.h"

namespace slf
{
  //------------------------------------------
  // build regex search string vector of string
  // each string will be put in or structure
  // possible names and symbols
  // 1) "number"  => will put in search string for a number "[+-]?(([0-9]*\\.[0-9]*|[0-9]+)[eEdD][+-]?[0-9]+|[0-9]+\\.?[0-9]*|\\.?[0-9]+)"
  // 2) [         => "\\["
  // 3) ]         => "\\]"
  // 4) (         => "\\("
  // 5) )         => "\\)"
  // 6) {         => "\\{"
  // 7) }         => "\\}"
  // 8) ,         => ","
  // 9) ;         => ";"
  // 10) .        => "\."
  // 11) word     => "word"   any word longer then 1 symbol e.g. "\\#\\d{5}"
  //
  CStr SlfBuildRegExString(const CStrV &strv);

  //------------------------------------------
  // search and match in buffer with regex_string
  // return in vector found text
  CStrV SlfMatchWithRegExString(const CStr &buffer_string, const CStr &regex_string);

   //------------------------------------------
   // build regex search string and match in buffer 
   // return in vector found text
   CStrV SlfMatchWithRegExString(const CStr &buffer_string, const CStrV &strv);

} //namespace slf
#endif // SLF_STR_REG_EX_H_INCLUDED