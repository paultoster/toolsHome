//
// SimulaDef.cpp
//
// Definitionen von defines

#include "Simula.h"

//====================================================================================
//====================================================================================
// String-Konstanten
//====================================================================================
//====================================================================================
namespace Simula
{

  const char *LOGFILENAME                 = "LogFile.log";                // Logfilename
  const char *DELIM_HIERACHY              = ".";
  const char *EQUALSIGN                   = "=";
  const char *UNITSIGN0                   = "[";
  const char *UNITSIGN1                   = "]";
  const char *ENTITYSIGN0                 = "<";
  const char *ENTITYSIGN1                 = "<";
  const char *ENTITYDELIM                 = ",";
  const char *STRINGQUOT                  = "\"";
  const char *GROUPSIGN0[]                = {"[","(","((","(((","((((","(((((","((((((","(((((((","((((((((","((((((((("};
  const char *GROUPSIGN1[]                = {"]",")","))",")))","))))",")))))","))))))",")))))))","))))))))",")))))))))"};
  const char NGROUPSIGN                   = 10;
  const char *COMMENTSIGN                 = "!";
  const char *COMMENTLINE                 = "----------------------------------------------------------------------------------------------------";
  const char *SPACETOSEPERAT              = "  ";
  const char *SPACE                       = " ";
  const char *TABULATOR                   = "\t";
  const char *ENDOFLINESIGN               = "|";
  const char *EMPTYLINESIGN               = "||";
  const char NENDOFLINESIGN               = strlen(ENDOFLINESIGN);
  const char *INCLUDESIGN                 = "#include";

  const std::size_t Simula::npos = -1;

}