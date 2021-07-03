// DsCtrl.h
//
// Steuerung der Modelle 
//
#ifndef DS_CTRL_H_INCLUDED
#define DS_CTRL_H_INCLUDED

#include "SlfBasic.h"
#include "SlfStr.h"
#include "SlfLogFile.h"
#include "SlfTab.h"
#include "DsMod.h"
#include "DsPar.h"
#include "DsCtrlIO.h"

#if DS_CTRL_IO_MEX
#include "mex.h"
#endif


// 
// Modellhandling
class CDsCtrl {
public:

  // Konstruktor mit Modellklasse, um Modellhandling zu machen
  // LogFile zum rausschreiben und Parameter Struktur, um eignes
  // PArameter anzumelden delta_t etc...
  CDsCtrl(CDsMod *pdsmod,CSlfLogFile *plogfile,CDsPar *pdspar);
  // Destruktur
  ~CDsCtrl();
  // Initialisieren nachdem Parameter geladen und zugeordnet werden
  // Input Liste mit zugeordneten Pointern für alle Inputs der Modelle
  // Output Liste mit zugeordneten Pointern für alle gewünschten Outputs der Modelle zu ordnet 
  status_t init(char flag_debug_info
               ,SDsModVar *ext_inp_list,          uint16_t n_ext_inp
               ,SDsModVar *ext_out_list,          uint16_t n_ext_out
               ,SDsModVarNameChange *change_list, uint16_t n_change_list
               );
  // Nachdem Parameter seperat eingeladen sind und zugeordnet,
  // kann für Inpu eine Vector-Liste eingelesen werden (aus dem jeweiligen Tool)
  // und erstellt wenn gewünscht eine Ausgabe Vector
#if DS_CTRL_IO_MEX
  status_t setMexOutputVector(mxArray **ppout);
  // sucht nach Zeitvektor und in der gleichen Länge die passenden
  // (nach Inputstruktur) Vektoren
  status_t getMexInputVectorWithTimeVector(const mxArray *pin);
#endif

  status_t run(void);
  status_t done(void);
  
  status_t buildOutputList(void);
  status_t buildInputList(void);
  


  status_t getStatus(void) {return Status;}
  char *getRunName(void) {return RunName.c_str();}
  char *getLogName(void) {LogName=RunName;LogName.cat("_log.dat");return LogName.c_str();}
  double getDeltaT(void) {return DeltaT;}
  double getSimTime(void){return TEnd-TStart;}

  uint32_t   getLenErrText(void) {return ErrText.getLen();}
  char *   getErrText(void) {return ErrText.c_str();}
  void     resetErrText(void) {ErrText.clear();}

  uint32_t   getLenLogText(void) {return LogText.getLen();}
  char *   getLogText(void) {return LogText.c_str();}
  void     resetLogText(void) {LogText.clear();}

protected:

  status_t          Status;                // Status OKAY/NOT_OKAY
  CSlfStr           ErrText;               // Fehlertext
  CSlfStr           LogText;               // fürs Logfile gedacht
  char              FlagDebugInfo;          // Sollen debug-Infos generiert werden
  CSlfLogFile       *pLogFile;             // Logfilehandling
  uint8_t             LogFileSet;            // Sagt ob LogFile geschrieben wird
  CDsMod            *pDsMod;               // Pointer der Modellklasse
  CDsCtrlIO         DSCtrlIO;              // IO-Klasse

  CSlfStr           RunName;               // SSimulationslaufname
  CSlfStr           LogName;

  double            DeltaT;                // interne Rechenschrittweite
  double            DeltaTOut;             // Ausgaberechenschrittweite
  double            TStart;                // Startzeit
  double            TEnd;                  // Endzeit
  double            TAct;                  // aktuelle Zeit
  uint32_t            NOut;                  // Ausgabewerte
  uint32_t            NZw;                   // Zwischenschritte

  CSlf1DTab         WegX1DTab;             // Tabelle zum testen

};
#endif