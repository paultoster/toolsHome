// DsModBase.h
//
// Steuerung der Modelle 
//
#ifndef DS_MOD_BASE_H_INCLUDED
#define DS_MOD_BASE_H_INCLUDED

#include "SlfBasic.h"
#include "SlfStr.h"

#ifdef Allg_C_Lib
#include "DsProject_default.h"
#else
#include "DsProject.h"
#endif
#define DS_MOD_BASE_ERR_TEXT  "Vorsicht!! virtuelle Funktion aufgerufen,\n selbst eine Funtion definieren oder Funktionsaufruf unterschiedlich definiert!!\n"

// Function Types
enum EModStaType {

#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
    DEF_step,       // keine dynamische Berechnung
    DEF_EULER,      // einfacher Vorwärts-Euler
    DEF_PC1,        // Prediktor-Korrektor einmal
    DEF_PC2,        // Prediktor-Korrektor zweimal
    DEF_PC3,        // Prediktor-Korrektor dreimal
    DEF_PCN,        // Prediktor-Korrektor mit Fehlerabfrage
    DEF_PIEULER,    // partial implicit euler (Teilimpliziter Euler)
    DEF_IEULER,     // implicit Euler
	DEF_GEARS,      // implizites Gears-Mehrschrittverfahren

// Die Modelle müssen in DsProject.h freigeschaltet werden
//--------------------------------------------------------
#else
    DEF_step,       // keine dynamische Berechnung
#if DS_MOD_INTEGRATION_PIEULER == 1
    DEF_PIEULER,    // partial implicit euler (Teilimpliziter Euler)
#endif
#if DS_MOD_INTEGRATION_PGEARS == 1
    DEF_PGEARS,     // partial implicit Gears-Mehrschrittverfahren (Teilimpliziter Gears)
#endif
#if DS_MOD_INTEGRATION_IEULER == 1
    DEF_IEULER,     // implicit Euler
#endif
#if DS_MOD_INTEGRATION_GEARS == 1
	DEF_GEARS,      // implizites Gears-Mehrschrittverfahren
#endif
#if DS_MOD_INTEGRATION_DOPRI45 == 1
	DEF_DOPRI45,    // expliziter Runge-Kutta, Dormand & Prince 4(5) Ordnung
#endif
#if DS_MOD_INTEGRATION_RADAU == 1
   DEF_RADAU,      // impliziter Runge-Kutta, Radau
#endif
#if DS_MOD_INTEGRATION_LSODA == 1
    DEF_LSODA,      // livermore solver ordinary diffequ atomatic method switching stiff/nonstiff
#endif
#endif
};

// Structs

// erweiterte Variablen-Struktur
#if 0
#endif
// Variablen-Struktur für Input und Output
struct SDsModVar {
  char             *Name;     // Name
  void             *pVal;    // Wert
  enum EVarType    Type;        // Typ
  char             *Unit;     // Unit
  char             *Comment;  // Comment
  uint8_t            LinInter;
};

struct SDsModSta {
    
  char             *Name;       // Name
  double           *pStaVal;       // State of a Variable
  double           *pStaValDer;    // Derivative of a Variable
  double           *pStaValIni;    // Init-Value
  enum EModStaType IntType;     // Integrationstyp
  double           MaxError;    // maximaler Fehler in der Iteration
  char             *Comment;    // Comment
};

// Basismodell 
class CDsModBase {
public:

    //SDsModVar  Input[1];
    //SDsModVar  Output[1];
    //SDsModSta  State[1];

    inline CDsModBase() {  }
    virtual ~CDsModBase() { }

    virtual status_t first(double t0)                           { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY; } /* Prototype Derivitive */
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
    virtual status_t init(double dt)                           { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Init */
    virtual status_t state(double tact)                         { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Derivitive */
#else
    virtual status_t init(double dt, uint32_t nstate)             { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Init */
    virtual status_t prestate(double tact)                      { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Vorverarbeitung */
    virtual error_t  state(double tact, double *y, double *f)   { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Derivitive */
    virtual error_t  jacobi(double x, double *y, Matrix_t dfdy,uint32_t ldfy)   { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Derivitive */
    virtual error_t  mass(uint32_t n, uint32_t lmas, Matrix_t am)   { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Derivitive */
#endif
    virtual status_t output(double tact)                        { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY; } /* Prototype Output */
    virtual status_t done()                                     { /*ErrText.cat(DS_MOD_BASE_ERR_TEXT);*/return NOT_OKAY;  } /* Prototype Init */

  virtual char * getName(void)                                        {return "";}

  virtual uint32_t getLenErrText(void)                                  {return 0;}
  virtual char * getErrText(void)                                     {return "";}
  virtual void   resetErrText(void)                                   {}

  virtual uint32_t getLenLogText(void)                                  {return 0;}
  virtual char * getLogText(void)                                     {return "";}
  virtual void   resetLogText(void)                                   {}


protected:
  //CSlfStr         Comment;
};

struct SDsModFkt {
  char             *Name;     // Name
  CDsModBase      **ppFkt;    // Wert
  char             *Comment;  // Comment
};

#endif