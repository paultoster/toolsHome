// SlfParReg.h
// Klasse zur Registrierung von Parameter in Funktionen
//
#ifndef SLF_PAR_REG
#define SLF_PAR_REG
//
 
#include <stdio.h>
#include <stdlib.h>
#include "SlfStrV.h"

#include "SlfParDef.h"


class CSlfParReg {

public:
  CSlfParReg();
  ~CSlfParReg();

  // Registrierfunktionen
  // F�r Einzelwerte vom Typ   PAR_D64,PAR_F32,PAR_SINT64,...,PAR_UINT8,PAR_slf::CStr,
  // oder Vektor mit festgelegter L�nge (GET_NROWS(xvec)) Typ: PAR_VEC
  // oder Matrix_t mit festgelegter L�nge (GET_NROWS(xmat),GET_NCOLS(xmat)) Typ: PAR_MAT
  // oder Vektor der von Parametereingabe definiert wird Typ: PAR_PTR_VEC
  // oder Matrix_t der von Parametereingabe definiert wird Typ: PAR_PTR_MAT
  //
  status_t reg(char             *Name    // Name
              ,char             *Group   // Gruppenhierachie mit "." getrennt
              ,char             *Unit    // Unit
              ,enum ESlfParType Type     // Typ siehe SlfParDef
              ,void             *pVal    // Pointer 
              ,char             *Default // Default-Wert, wenn zugelassen 
              ,char             *Comment // Comment
              );
  // Registrierfunktionen
  // F�r festgelegte Arrays(L�nge)vom Typ   
  // PAR_ARR_D64,PAR_ARR_F32,...,PAR_ARR_UINT8,PAR_ARR_slf::CStr,PAR_CHAR_STRING

  //
  status_t reg(char             *Name    // Name
              ,char             *Group   // Gruppenhierachie mit "." getrennt
              ,char             *Unit    // Unit
              ,enum ESlfParType Type     // Typ siehe SlfParDef
              ,void             *pVal    // Pointer
              ,uint32_t         NVal     // L�nge des Arrays oder char string
              ,char             *Default // Default-Wert, wenn zugelassen 
              ,char             *Comment // Comment
              );

  struct SSlfParRegVar {
    slf::CStr                 VarName;        // Name
    slf::CStrV                GroupHierachie; // Gruppenhierachie
    slf::CStr                 Unit;           // Einheit
    slf::CStr                 Comment;        // Comment
    slf::CStr                 Default;        // Defaultwert
    enum EVarType           Type;           // Typ
    void                    *pVal;          // �bergebener Pointer
    uint32_t                NVal;           // Anzahl der Werte bei array oder
                                            // L�nge des char strings oder
                                            // bei Vector_t und Matrix_t ist L�nge in Struktur zu finden
    uint8_t                 ErrFlag:1;      // ErrorFlag, wenn Pointer nicht stimmt
    uint8_t                 HasLength:1;    // Ist L�nge vorgegeben vom Typ ARR, VEC, MAT oder CHAR_STRING mit fest
                                            // vorgegebener L�nge NArray
    uint8_t                 dum    :6;
    SSlfParRegVar           *pNext;
  };

private:

    status_t        Status;
    slf::CStr         ErrText;
    SSlfParRegVar   *pRegVar;

    uint8_t regCheckType(enum ESlfParType type,void *pval);
    uint8_t regCheckTypeWithLength(enum ESlfParType type,void *pval,uint32_t nval);

};

#endif