#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define SLFBASIC_VAR_TYPE_STR
#include "DsCtrl.h"
#include "SlfFkt.h"

CDsCtrl::CDsCtrl(CDsMod *pdsmod,CSlfLogFile *plogfile,CDsPar *pdspar) {

    Status = OKAY;

    // Modellhandlingsklasse
    pDsMod         = pdsmod;
    // Logfilehandling
    if( plogfile ) {

        pLogFile   = plogfile;
        LogFileSet = 1;
    } else {

        pLogFile   = 0;
        LogFileSet = 0;
    }

  // Texte
  ErrText = "";
  LogText = "";

  NOut = 0;
  NZw  = 0;


  // Parameterliste
  SDsParVar ParList[] = {
  {"delta_t",     "control", &DeltaT,      DEF_DOUBLE,   "s",  "",          "Rechenschrittweite"},
  {"delta_t_out", "control", &DeltaTOut,   DEF_DOUBLE,   "s",  "",          "Ausgabeschrittweite"},
  {"t_start",     "control", &TStart,      DEF_DOUBLE,   "s",  ".0",       "Startzeit"},
  {"t_end",       "control", &TEnd,        DEF_DOUBLE,   "s",  "",          "Endzeit"},
  {"run_name",    "control", &RunName,     DEF_STRING,   "",   "sim_run",   "Laufname"},
  };
  pdspar->set("CDsCtrl",ParList,sizeof(ParList)/sizeof(SDsParVar));

}
// Destruktor ===============================================
//===========================================================
CDsCtrl::~CDsCtrl() {

    Status = OKAY;

    DSCtrlIO.deleteInput();
    DSCtrlIO.deleteOutput();
}
status_t CDsCtrl::init(char flag_debug_info
                      ,SDsModVar *ext_inp_list,          uint16_t n_ext_inp
                      ,SDsModVar *ext_out_list,          uint16_t n_ext_out
                      ,SDsModVarNameChange *change_list, uint16_t n_change_list
               
                       ) {

    Status = OKAY;

    FlagDebugInfo = flag_debug_info;

    DSCtrlIO.deleteInput();
    DSCtrlIO.deleteOutput();

    // Texte
    ErrText = "";
    LogText = "";

    //Zeit initialisieren
    //===================
    if( fabs(TEnd-TStart) < EPSILON ) {
        ErrText.cat("Fehler mit Anfangs- bzw- Endzeit\n");
        ErrText.catFormat("TStart = %g [s]\n",TStart);
        ErrText.catFormat("TEnd = %g [s]\n",TEnd);
        Status = NOT_OKAY;
        return Status;
    }
    if( TEnd < TStart ) {

        double T = TEnd;
        TEnd   = TStart;
        TStart = T;
    }
    if( fabs(DeltaT) < EPSILON ) {
        ErrText.cat("Fehler mit Rechenschrittweite\n");
        ErrText.catFormat("DeltaT = %g [s]\n",DeltaT);
        Status = NOT_OKAY;
        return Status;
    }
    if( DeltaTOut < EPSILON )
        DeltaTOut = DeltaT;



    // Anzahl der Ausgabepunkte und Zwischenschritte festlegen
    NOut = (uint32_t)((TEnd - TStart)/MAX(1.0e-10,DeltaTOut)+1.5);
    NZw  = (uint32_t)(DeltaTOut/DeltaT);

    if( NZw == 0 ) {
        ErrText.cat("Fehler> mit Rechenschrittweiten\n");
        ErrText.catFormat("Rechenschrittweite DeltaT = %g [s] > Ausgabeschrittweite DeltaTOut = %g [s]\n",DeltaT,DeltaTOut);
        Status = NOT_OKAY;
        return Status;
    }



    // Modelhandling initialisieren
    //==============================
    pDsMod->ini(pLogFile);

    // Modelle bilden
    //===============
    if( pDsMod->setup(ext_inp_list, n_ext_inp
                     ,ext_out_list, n_ext_out
                     ,change_list,  n_change_list) != OKAY ) {

        ErrText.cat("Fehler DsMod.setup()\n");
        ErrText.cat(pDsMod->getErrText());
        pDsMod->resetErrText();

        Status = NOT_OKAY;
        return Status;
    }


    // Modell initialisieren
    //======================
    if( pDsMod->init(DeltaT) != OKAY ) {

        ErrText.cat("Fehler DsMod.init()\n");
        ErrText.cat(pDsMod->getErrText());
        pDsMod->resetErrText();

        Status = NOT_OKAY;
        return Status;
    }


    // Inputliste aus Modell erstellen
    //==================================
    buildInputList();
    // Ausgabeliste aus Modell erstellen
    //==================================
    buildOutputList();

    return Status;
}
status_t CDsCtrl::run(void) {

    uint32_t iout,izw;

    // Check Inputliste mit externen Input
    //====================================
    if( DSCtrlIO.proofInput() != OKAY )
    {
      ErrText.cat("Fehler DSCtrlIO.proofInput()\n");
      ErrText.cat(DSCtrlIO.getErrText());
      Status = NOT_OKAY;
      return Status;
    }

    // Firstberechnung
    //================
    TAct    = TStart;
    iout    = 0;

    DSCtrlIO.setInput(TAct);

    if( pDsMod->first(TAct) != OKAY ) {

        ErrText.cat("Fehler DsMod.first()\n");
        ErrText.cat(pDsMod->getErrText());

        Status = NOT_OKAY;
        return Status;
    }

    DSCtrlIO.setOutput(iout++);


    //Loopberechnung
    //==============
    while( iout < NOut ) {

        izw = 0;
        while( izw < NZw ) {


            TAct += DeltaT;
    
            DSCtrlIO.setInput(TAct);

            if( pDsMod->loop() != OKAY ) {

                ErrText.cat("Fehler DsMod.loop()\n");
                ErrText.cat(pDsMod->getErrText());

                Status = NOT_OKAY;
                return Status;
            }

            ++izw;
        }

        DSCtrlIO.setOutput(iout);

        ++iout;
    }

    return Status;

}
status_t CDsCtrl::done(void) {


    if( pDsMod->done() != OKAY ) {

        ErrText.cat("Fehler DsMod.done()\n");
        ErrText.cat(pDsMod->getErrText());

        Status = NOT_OKAY;
        return Status;
    }

    DSCtrlIO.deleteInput();
    DSCtrlIO.deleteOutput();


    return Status;
}
//
//========================================
status_t CDsCtrl::buildOutputList(void) {

    
    uint16_t            i;
    SDsModVarInfo     *pExtOutMod;

    // Ausgabewerte aus Modell sammeln
    //================================
    for(i=0;i<pDsMod->getNExtOut();i++) {


        pExtOutMod = pDsMod->getExtOut(i);

        if( DSCtrlIO.buildOutputVector(pExtOutMod->Name.c_str()
                                      ,pExtOutMod->Unit.c_str()
                                      ,pExtOutMod->Comment.c_str()
                                      ,pExtOutMod->Type
                                      ,pExtOutMod->pVal
                                      ,NOut
                                      ) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
    }

    // Zeitvektor ausgeben
    //====================
    if( DSCtrlIO.buildOutputVector("time"
                                  ,"s"
                                  ,"Zeitvektor"
                                  ,DEF_DOUBLE
                                  ,&TAct
                                  ,NOut
                                   ) != OKAY ) {

        Status = NOT_OKAY;
        return Status;
    }

    return Status;
}
#if DS_CTRL_IO_MEX
status_t CDsCtrl::setMexOutputVector(mxArray **ppout) {

    if( DSCtrlIO.setMexOutputVector(ppout) != OKAY ) {

        ErrText.cat(DSCtrlIO.getErrText());
        DSCtrlIO.resetErrText();
        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
#endif
status_t CDsCtrl::buildInputList(void) {

    
    uint16_t            i;
    SDsModVarInfo     *pExtInpMod;

    // Eingabewerte aus Modell sammeln
    //================================
    for(i=0;i<pDsMod->getNExtInp();i++) {


        pExtInpMod = pDsMod->getExtInp(i);

        if( DSCtrlIO.buildInput(pExtInpMod) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
    }

    return Status;
}
#if DS_CTRL_IO_MEX
status_t CDsCtrl::getMexInputVectorWithTimeVector(const mxArray *pin) {

    // Eine zusätzlichen Zeitvektor bilden, der mit einem Vektor aus Matlab 
    // zugepordent wird, um nachher den Index aus der aktuellen Zeit Tact zu bestimmen
    DSCtrlIO.setInputTimeStruct("time","s","Berechnungszeit");

    if( DSCtrlIO.getMexInputVectorWithTimeVector(pin) != OKAY ) {

        ErrText.cat(DSCtrlIO.getErrText());
        DSCtrlIO.resetErrText();
        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
#endif

