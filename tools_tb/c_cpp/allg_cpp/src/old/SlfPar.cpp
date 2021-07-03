#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfPar.h"

// Parametertyp als String bereitstellen
char *SlfParTypeStr[] = {
  "PAR_VOID",            // nicht belegt
  "PAR_D64",             // Pointer eines Einzelwerts type (d64_t),
  "PAR_F32",             //                           type (f32_t),
  "PAR_SINT64",             // ...
  "PAR_UINT64",
  "PAR_SINT32",        
  "PAR_UINT32",
  "PAR_SINT16",
  "PAR_UINT16",
  "PAR_SINT8",
  "PAR_UINT8",
  "PAR_slf::CStr",             // Einzelwert String-Klasse slf::CStr
  "PAR_PTR_VEC",         // Pointer von Vektor-Struktur Vector_t
  "PAR_PTR_MAT",         // Pointer von Matrix_t-Struktur Matrix_t
  "PAR_VEC",             // Vektor-Struktur Vector_t
  "PAR_MAT",             // Matrix_t-Struktur Matrix_t
  "PAR_ARR_D64",         // Array mit festgelegter Länge vondouble-Werten, 
  "PAR_ARR_F32",         // Länge muß übergerdneter Struct definiert sein
  "PAR_ARR_SINT64",         // ...
  "PAR_ARR_UINT64",
  "PAR_ARR_SINT32",        
  "PAR_ARR_UINT32",
  "PAR_ARR_SINT16",
  "PAR_ARR_UINT16",
  "PAR_ARR_SINT8",
  "PAR_ARR_UINT8",
  "PAR_ARR_slf::CStr",         // Array String-Klasse slf::CStr
  "PAR_CHAR_STRING",        // String aus charater mit '\0' beendet z.B. matlab eingabe

  "PAR_1D_TAB",          // einfache Tabellen-Struktur SSfl1DTab y = f(x)
  "PAR_2D_TAB"           // zweidim. Tabellenstruktur  SSfl2DTab z = f(x,y)
};

// Es wird eine Klasse zum registrieren von  Parameter bereitgestellt
//-------------------------------------------------------------------
CSlfParReg SlfParReg;
