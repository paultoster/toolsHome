// SlfParGet.h
// Klasse zum Einlesen/Eingabe von Parameter
// 
#ifndef SLF_PAR_GET
#define SLF_PAR_GET
//
 
#include <stdio.h>
#include <stdlib.h>
#include "SlfStr.h"
#include "SlfLogFile.h"

#include "SlfParDef.h"


class CSlfParGet {

public:
  CSlfParGet();
  CSlfParGet(CSlfLogFile *plogfile);
  ~CSlfParGet();

  struct SSlfParRegVar {
    slf::CStr                 VarName;        // Name
    slf::CStrV                GroupHierachie; // Gruppenhierachie
    slf::CStr                 Unit;           // Einheit
    slf::CStr                 Comment;        // Comment
    slf::CStr                 Default;        // Defaultwert
    enum EVarType           Type;           // Typ
    void                    *pVal;          // übergebener Pointer
    uint32_t                NVal;           // Anzahl der Werte bei array oder
                                            // Länge des char strings oder
                                            // bei Vector_t und Matrix_t ist Länge in Struktur zu finden
    uint8_t                 ErrFlag:1;      // ErrorFlag, wenn Pointer nicht stimmt
    uint8_t                 HasLength:1;    // Ist Länge vorgegeben vom Typ ARR, VEC, MAT oder CHAR_STRING mit fest
                                            // vorgegebener Länge NArray
    uint8_t                 dum    :6;
    SSlfParRegVar           *pNext;
  };

private:

    status_t        Status;
    slf::CStr         ErrText;
    CSlfLogFile     *pLogFile;

};

#endif