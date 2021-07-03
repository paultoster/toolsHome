// DsRunMod.h
//
// Steuerung der Modelle 
//
#ifndef DS_RUN_MOD_H_INCLUDED
#define DS_RUN_MOD_H_INCLUDED

#include "SlfBasic.h"
#include "SlfStr.h"
#include "SlfLogFile.h"
#include "SlfTab.h"
#include "DsMod.h"
#include "DsPar.h"
#include "DsParFile.h"


// 
// Modellhandling
class CDsRunMod {
public:

  CDsMod            *pDsMod;               // Pointer der Modellklasse
  double            DeltaT;                // Rechenschrittweite
  double            TStart;                // Startzeit
  double            TEnd;                  // Endzeit

  CDsRunMod();
  CDsRunMod(char *log_file_name);
  ~CDsRunMod();

  // aktuelle Berechnungszeit
  //-------------------------
  double getActTime(void){return pDsMod->getTime();}

  // Initialisieren
  // Die Modelle müssen vor dem aufruf angelegt werden, um die Verknüpfung 
  // zu finden
  //==============================================================================
  status_t init(SDsModVar           *p_ext_inp_list // Input-Variablenliste
               ,uint16_t              n_ext_inp       // Anzahl Input Variablen
               ,SDsModVar           *p_ext_out_list // Output-Variablenliste
               ,uint16_t              n_ext_out       // Anzahl Output Variablen
               ,SDsModVarNameChange *p_change_list  // Change Variablennamen-Liste
               ,uint16_t              n_change_list   // Anzahl der Change Variablennamen
               ,EParType            par_type        // Parametertype (PAR_DS; ...
               ,char                *par_file_name  // Parameterfilename
               ,double              delta_t_sim     // äussere Simulationszeit
               ,double              t_start         // Startzeitpunkt
               );

  // Funktion beenden
  // wenn Rückgabe == 1, dann Ausgabetext in LogText
  //------------------------------------------------
  uint8_t done(void);
  
  char *getLogFileName(void){if( pLogFile )return pLogFile->getLogFileName();else return "";}
  
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
  CSlfLogFile       *pLogFile;             // Logfilehandling

  
  //CDsPar            *pDsPar;               // Pointer der Parameterklasse
  CDsParFile        *pDsParFile;           // Pointer der Parameter-File-Klasse



  // Logfile erstellen
  status_t setLogFile(char *log_file_name);

  // IO- Initialisieren
  // Die Modelle müssen vor dem aufruf angelegt werden, um die Verknüpfung 
  // zu finden
  //==============================================================================
  status_t ioinit(SDsModVar           *p_ext_inp_list         // Input-Variablenliste
                 ,uint16_t              n_ext_inp             // Anzahl Input Variablen
                 ,SDsModVar           *p_ext_out_list         // Output-Variablenliste
                 ,uint16_t              n_ext_out             // Anzahl Output Variablen
                 ,SDsModVarNameChange *p_change_list          // Change Variablennamen-Liste
                 ,uint16_t              n_change_list         // Anzahl der Change Variablennamen
                 );

  // Parameter - Initialisieren
  // Möglichkeit verschiedene Parameterstrukturen zu verweden
  // Bislang nur par_type = DS_PAR_DS implementiert
  //==============================================================================
  status_t parinit(EParType par_type              // Parametertype (DS_PAR; ...
                  ,char *par_file_name            // Parameterfilename
                  );

  // Funktionen - Initialisieren
  // Initlialisierung der Function und first-Berechnung
  //==============================================================================
  status_t funcinit(double delta_t_sim     // äussere Simulationszeit 
                   ,double t_start         // Startzeitpunkt
                   );

};
#endif