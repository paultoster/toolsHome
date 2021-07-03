//===================================================================

#include "SlfStrRegEx.h"
#include <string>
#include <iostream>
#include <regex>
#include <sstream>

namespace slf
{ 

  //===================================================================
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
  // 10) .        => "\\."
  // 11) word     => "word"   any word longer then 1 symbol e.g. "\\#\\d{5}"
  //
  CStr SlfBuildRegExString(const CStrV &strv)
  {
    CStr rx;
    CStr symbols;
    CStrV words;

    std::size_t i_number;
    
    // search for number
    if (!strv.find("number", &i_number)) i_number = std::string::npos;

    // search for single symbol and words
    for (std::size_t i = 0; i < strv.size(); ++i)
    {
      if (strv.lengthOf(i) == 1) // single symbols
      {
        if (strv[i] == "[")
        {
          symbols.cat("\\[");
        }
        else if (strv[i] == "]")
        {
          symbols.cat("\\]");
        }
        else if (strv[i] == "(")
        {
          symbols.cat("\\(");
        }
        else if (strv[i] == ")")
        {
          symbols.cat("\\)");
        }
        else if (strv[i] == "{")
        {
          symbols.cat("\\{");
        }
        else if (strv[i] == "}")
        {
          symbols.cat("\\}");
        }
        else if (strv[i] == ".")
        {
          symbols.cat("\\.");
        }
        else
        {
          symbols.cat(strv[i]);
        }
      }
      else if ((strv.lengthOf(i) > 1) && (i != i_number)) // words
      {
        words.append(strv[i]);
      }
    }
    // build string
    //  "([+-]?(([0-9]*\\.[0-9]*|[0-9]+)[eEdD][+-]?[0-9]+|[0-9]+\\.?[0-9]*|\\.?[0-9]+)|[;,\\[\\]]|\\#\\d{5})";
    rx = "(";
    if (i_number != std::string::npos)
    {
      rx.append("[+-]?(([0-9]*\\.[0-9]*|[0-9]+)[eEdD][+-]?[0-9]+|[0-9]+\\.?[0-9]*|\\.?[0-9]+)");

      if ((symbols.size() == 0) && (words.size() == 0))
      {
        rx.add(')');
      }
      else
      {
        rx.add('|');
      }
    }

    if (symbols.size())
    {
      rx.add('[');
      rx.append(symbols);
      rx.add(']');
      if (words.size() == 0)
      {
        rx.add(')');
      }
      else
      {
        rx.add('|');
      }

    }

    if (words.size())
    {
      for (std::size_t i = 0; i < words.size(); ++i)
      {
        rx.append(words[i]);

        if (i + 1 == words.size())
        {
          rx.add(')');
        }
        else
        {
          rx.add('|');
        }

      }
    }
    return rx;
  }


  //------------------------------------------
  // search and match in buffer with regex_string
  // return in vector found text
  CStrV SlfMatchWithRegExString(const CStr &buffer, const CStr &regex_string)
  {
    std::regex num_regex(regex_string);
    
    std::sregex_iterator match(buffer.begin(), buffer.end(), num_regex);
    std::sregex_iterator lastmatch;

    CStrV strv;
    while (match != lastmatch)
    {
      std::smatch m = *match;
      strv.append( m.str() );
      match++;
    }

    return strv;

  }

  //------------------------------------------
  // build regex search string and match in buffer 
  // return in vector found text
  CStrV SlfMatchWithRegExString(const CStr &buffer, const CStrV &strv)
  {
    CStr regex_string = SlfBuildRegExString(strv);
    return SlfMatchWithRegExString(buffer, regex_string);
  }

} // namespace