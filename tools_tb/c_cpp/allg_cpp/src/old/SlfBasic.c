#define SLFBASIC_VAR_TYPE_STR
#include "SlfBasic.h"


char *VarTypeStr[] = {
    "DEF_VOID",
    "DEF_DOUBLE",
    "DEF_FLOAT",
    "DEF_SIGNED_LONG",
    "DEF_UNSIGNED_LONG",
    "DEF_SIGNED_SHORT",
    "DEF_UNSIGNED_SHORT",
    "DEF_SIGNED_CHAR",
    "DEF_UNSIGNED_CHAR",
    "DEF_STRING",
    "DEF_CHAR_STRING",

    "DEF_VEC_DOUBLE",
    "DEF_VEC_FLOAT",
    "DEF_VEC_SIGNED_LONG",
    "DEF_VEC_UNSIGNED_LONG",
    "DEF_VEC_SIGNED_SHORT",
    "DEF_VEC_UNSIGNED_SHORT",
    "DEF_VEC_SIGNED_CHAR",
    "DEF_VEC_UNSIGNED_CHAR",
    "DEF_VEC_STRING",

    "DEF_VEC",             // Vektor-Struktur SSlfVec
    "DEF_MAT",             // Matrix_t-Struktur SSlfMat
    "DEF_SLFVEC",          // Vektor-Struktur mit SSlfVec (Numeric.h)
    "DEF_SLFMAT",          // Matrix_t-Struktur mit SSlfMat (Numeric.h)
    "DEF_STRINGV",        // MatrixString-Klasse slf::CStrM
    "DEF_STRINGM",        // MatrixString-Klasse slf::CStrM
    "DEF_1D_TAB",            // einfache Tabellen-Struktur SSflTab y = f(x)
    "DEF_2D_TAB",         // zweidim. Tabellenstruktur  SSfl2DTab z = f(x,y)

    "DEF_DOUBLE_PTR",
    "DEF_FLOAT_PTR",
    "DEF_SIGNED_LONG_PTR",
    "DEF_UNSIGNED_LONG_PTR",
    "DEF_SIGNED_SHORT_PTR",
    "DEF_UNSIGNED_SHORT_PTR",
    "DEF_SIGNED_CHAR_PTR",
    "DEF_UNSIGNED_CHAR_PTR",
    "DEF_STRING_PTR",
  
    "DEF_VEC_PTR",         // Pointer der Vectorstuktur
    "DEF_MAT_PTR",         // Pointer der Matrixstruktur
    "DEF_1D_TAB_PTR",
};

