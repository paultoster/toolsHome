#if 0
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DsMod.h"
#include "SlfFkt.h"

//11111111111111111111111111111111111111111111111111111111111
//11111111111111111111111111111111111111111111111111111111111
//11111111111111111111111111111111111111111111111111111111111
// Konstruktor ==============================================
//===========================================================
CDsMod::CDsMod(CSlfLogFile *plogfile,CDsPar *ppar/*=0*/) {

  Status = OKAY;

  // Defaultwerte
  pLogFile      = 0;
  LogFileSet    = 0;

  //Parameterklasse
  if( ppar == 0 ) { // keine

      pPar   = 0;
      ParSet = 0;
  } else {
      pPar   = ppar;
      ParSet = 1;
  }


  // Texte
  ErrText = "";
  LogText = "";

  //Registrierliste
  pRegListMod = 0;
  pRegListFkt   = 0;

  //Modell
  pModList      = 0;
  NModels       = 0;
  NFkts         = 0;
  MaxIter       = 0;
  DtMast        = 0.0;
  DtModMin      = -1.;

#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
  DtMast05      = 0.0;
#endif
  CntDownIFSum  = 0;
  IterFlagSum   = 0;
  DoneFlag      = 0;
  BuildFlag     = 0;

  // Dummywerte
  WertNull = 0.0;

    resetLogText();
    resetErrText();

    Status        = OKAY;

    if( plogfile == 0 ) { // kein Logfile benutzen

        pLogFile   = 0;
        LogFileSet = 0;

    } else { // Logfile anlegen

        pLogFile   = plogfile;
        LogFileSet = 1;
    }
}
CDsMod::CDsMod(CDsPar *ppar/*=0*/) {

  Status = OKAY;

  // Defaultwerte
  pLogFile      = 0;
  LogFileSet    = 0;

  //Parameterklasse
  if( ppar == 0 ) { // keine

      pPar   = 0;
      ParSet = 0;
  } else {
      pPar   = ppar;
      ParSet = 1;
  }


  // Texte
  ErrText = "";
  LogText = "";

  //Registrierliste
  pRegListMod = 0;
  pRegListFkt   = 0;

  //Modell
  pModList      = 0;
  NModels       = 0;
  NFkts         = 0;
  MaxIter       = 0;
  DtMast        = 0.0;
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
  DtMast05      = 0.0;
#endif
  CntDownIFSum  = 0;
  IterFlagSum   = 0;
  DoneFlag      = 0;
  BuildFlag     = 0;

  // Dummywerte
  WertNull = 0.0;


}
// Destruktor ===============================================
//===========================================================
CDsMod::~CDsMod() {

    // Modellliste zerstören
    if( pModList != 0 )
        destroyMod();

    // Anmeldeliste zerstören
    destroyRegMod();
    destroyRegFkt();

    Status = OKAY;
}
//22222222222222222222222222222222222222222222222222222222222
//22222222222222222222222222222222222222222222222222222222222
//22222222222222222222222222222222222222222222222222222222222
//Modelle aufsetzen =========================================
//===========================================================

status_t CDsMod::ini(CSlfLogFile *plogfile /*=0*/) {


    resetLogText();
    resetErrText();

    Status        = OKAY;

    if( plogfile ) { // Logfile benutzen

        pLogFile   = plogfile;
        LogFileSet = 1;
    }

    DoneFlag      = 0;
    BuildFlag     = 0;

    return OKAY;
}

// Setup Modell
//=======================
status_t CDsMod::setup(SDsModVar *ext_inp_list, uint16_t n_ext_inp
                      ,SDsModVar *ext_out_list, uint16_t n_ext_out
                      ,SDsModVarNameChange *change_list/*=NULL*/
                      ,uint16_t n_change_list/*=0*/
                      ) {

    if( Status != OKAY ) {
        writeLogFile("Setup: Status != OKAY when starting setup");
        Status = OKAY;
    }
        
    resetLogText();
    resetErrText();

    destroyMod();


    //============================================
    // Modell, externe Inputs und Output einordnen
    //============================================
    if( Status == OKAY )
        Status=buildMod(ext_inp_list, n_ext_inp
                       ,ext_out_list, n_ext_out
                       ,change_list,  n_change_list);
   


    
    //========================================================
    // Variablenzuordnung suchen, prüfen und bei debug ausgeben
    //========================================================
    if( Status == OKAY )
        Status=connectInpVar();

    //========================================================
    // Fkt-Pointerzuordnung suchen, prüfen und bei debug ausgeben
    //========================================================
    if( Status == OKAY )
        Status=connectFkt();

    //=================
    // Modell auflisten
    //=================
#if DS_DEBUG_MODE & 1
    Status=listMod(true,true,true,true,true,true);
#else
    if(  Status == OKAY)
        Status=listMod(true,true,true,true,true,true);
#endif
    //=======================
    // Ausgabe in das LogFile
    //=======================
#if DS_DEBUG_MODE & 1
    writeLogFile("SetUp");
#else
    if( Status != OKAY )
        writeLogFile("SetUp");
#endif

	if( Status == OKAY )
		  DoneFlag      = 0;


    return Status;
}
//Modelle initialisieren ====================================
//===========================================================
status_t CDsMod::init(double dt) {

    resetLogText();
    resetErrText();

    // Schrittweiten überprüfen
    //=========================
    initDt(dt);


    // Parameter zuordnen (tbd)
    //=========================

    // Modelle initialisieren
    //=========================
    if( Status  == OKAY )
        calcInitMod();

    // Funktionen initialisieren
    //=========================
    if( Status  == OKAY )
        calcInitFkt();

    //=======================
    // Ausgabe in das LogFile
    //=======================
#if DS_DEBUG_MODE & 1
    writeLogFile("Init");
#else
    if( Status != OKAY )
        writeLogFile("Init");
#endif

    return Status;
}
//Modelle Zeitpunkt Null ====================================
//===========================================================

status_t CDsMod::first(double t0) {

    resetLogText();
    resetErrText();

    // Zeit Setzen
    //============
    TimeAct = t0;

#if DS_MOD_NEW_STA == 0  // vorerst nicht notwendig
    // Initial State zuordnen (tbd)
    //=============================
    setStaIni();
#endif

    // Initialisierung Extener Input
    //==============================
    calcExtInp(2);

    // Input zuordnen
    //===============
    for(uint16_t i=0;i<NModels;i++) {
        pModList[i].TimeAct = TimeAct;
        // Input Variablen setzen
        setInpVar(&pModList[i]);
    }


    // Modelleberechnung
    //==================
    if( calcFirstMod() != OKAY )
        return Status;

    // externer Output zuordnen
    //=========================
    setExtOutVar();

    //=======================
    // Ausgabe in das LogFile
    //=======================
#if DS_DEBUG_MODE & 1
    writeLogFile("First");
#else
    if( Status != OKAY )
        writeLogFile("First");
#endif
    return Status;
}
//Modelle Loop-Berechnung ===================================
//===========================================================
status_t CDsMod::loop() {

    uint32_t i;
    double tend = TimeAct+DtMast;

    if( Status != OKAY )
        return Status;

    TimeOld = TimeAct;

    // Interpolation Externer Inpu  initialisieren
    //============================================
    calcExtInp(1);

    for(i=0;i<NDt;i++) {

        TimeAct = MIN(TimeAct+DtModMin,tend);
    
        // Interpolation Extener Input
        //==============================
        calcExtInp(0);

        // Integration
        //============
        Status =  calcMod(TimeAct);
    }
    
    // externer Output zuordnen
    //=========================
    if( Status == OKAY )
        setExtOutVar();

#if DS_DEBUG_MODE & 2
    writeLogFile("Loop");
#else
    if( Status != OKAY )
        writeLogFile("Loop");
#endif


    return Status;
}
//Modelle Done-Berechnung ===================================
//===========================================================

status_t CDsMod::done() {

	if( !DoneFlag  ) {

        
		calcDoneMod();

		destroyMod();
    
        destroyRegMod();
		
#if DS_DEBUG_MODE & 1
        writeLogFile("Done");
#else
		if( Status != OKAY )
			writeLogFile("Done");
#endif
		DoneFlag      = 1;
	}
    return OKAY;
}



#endif