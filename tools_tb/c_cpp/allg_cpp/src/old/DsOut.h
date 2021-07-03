// DsOut.h
// Outputstruktur verarbeiten

#ifndef DS_OUT_H_INCLUDED
#define DS_OUT_H_INCLUDED
//

 
#include <stdio.h>
#include <stdlib.h>


#include "SlfStr.h"
#include "SlfLogFile.h"
#include "DsModBase.h"
#ifdef Allg_C_Lib
#include "DsProject_default.h"
#else
#include "DsProject.h"
#endif
#if DS_OUT_USE_MEXWRITE != 0
#endif
#if DS_OUT_USE_FILE != 0
#endif



// Variablendefinition vom Modell, mit dieser Struktur
// wird im Modell eine Variable definiert und an DsOutSet() weitergegebe
// Zuordnung Type und Pointer pVal:
// DEF_VOID             :     
// DEF_DOUBLE           :  pointer auf double-Variable übergeben  Variable muß angelegt sein 
//                         (z.B. double dvar Übergabe mit &dvar)
// DEF_FLOAT            :  dito float
// DEF_SIGNED_LONG      :  dito signed long
// DEF_UNSIGNED_LONG    :  dito unsigned long
// DEF_SIGNED_SHORT     :  dito signed short
// DEF_UNSIGNED_SHORT   :  dito unsigned short
// DEF_SIGNED_CHAR      :  dito signed char
// DEF_UNSIGNED_CHAR    :  dito unsigned char
// DEF_STRING,          :  dito string-Klasse CSlfStr (SlfStr.h)
// DEF_VEC              :  pointer auf Vector-Variable übergeben, Vector muß angelegt vorhanden sein
//                        (z.B. Vector vec; siehe SlfNum.h), wenn vec = 0; gesetzt wird automatisch 
//                        Speicher reserviert; wenn vorher mit vec = NewVector(n); gesetzt, 
//                        wird Länge n mit den zuzuweisenden gelesenen Outameterdaten verglichen                          
//                        !!! Vector besteht aus einer struktur mit double-array und Länge

// DEF_ARR_DOUBLE           :  pointer auf double-array übergeben  Variable muß angelegt sein 
//                            (z.B. double dvar[10] Übergabe mit dvar, aber auch Länge übergeben)
// DEF_ARR_FLOAT            :  dito float
// DEF_ARR_SIGNED_LONG      :  dito signed long
// DEF_ARR_UNSIGNED_LONG    :  dito unsigned long
// DEF_ARR_SIGNED_SHORT     :  dito signed short
// DEF_ARR_UNSIGNED_SHORT   :  dito unsigned short
// DEF_ARR_SIGNED_CHAR      :  dito signed char
// DEF_ARR_UNSIGNED_CHAR    :  dito unsigned char
// DEF_ARR_STRING,          :  dito string-Klasse CSlfStr (SlfStr.h)
//                             (z.B. CSlfStr str[10] Übergabe mit str, aber auch Länge übergeben)


// Von den Modulen gesetzte Variablenstruktur
struct SDsOutSetVar {
  CSlfStr                 ModName;     // Name
  CSlfStr                 VarName;     // Name
  void                    *pVal;       // Wert
  uint32_t                NVal;        // Anzahl Werte (bei arrays)
  uint32_t                NRow;        // Anzahl Werte (bei arrays)
  uint32_t                NCol;        // Anzahl Werte (bei arrays)
  enum EVarType           Type;        // Typ
  CSlfStr                 Unit;        // Unit
  CSlfStr                 Comment;     // Comment

  SDsOutSetVar           *pNext;
};

// Output-Klasse 
class CDsOut {

public:

    CDsOut();
    CDsOut(CSlfLogFile *plogfile);
    ~CDsOut();

    // Mit LogFile initialisieren
    status_t ini(CSlfLogFile *plogfile=0);

    // Output-Struktur setzen
    status_t set(char *mod_name,SDsVar *varlist,uint16_t nvar);

#if DS_OUT_USE_MEXWRITE != 0
    // Output-Struktur für Matlab einrichten
    status_t getMex(const mxArray *par,uintt16 npar);
#endif
#if DS_OUT_USE_FILE != 0
    // Outputstruktur aus Datei auslesen
    status_t writeFile(char *out_file);           // 
#endif

    void     deleteModul(char *mod_name);


    //Beenden
    //=======
    status_t done(void);

    void reset(void);

    uint32_t   getLenErrText(void) {return ErrText.getLen();}
    char *   getErrText(void) {return ErrText.c_str();}
    void     resetErrText(void) {ErrText.clear();}
    

private:

    status_t        Status;
    CSlfStr         ErrText;

    CSlfLogFile       *pLogFile;             // Logfilehandling
    uint8_t             LogFileSet;            // Sagt ob LogFile geschrieben wird

    SDsOutSetVar   *pDSOutSetVar;

    void writeLogFile(char *name);

};


#endif