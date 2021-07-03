#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DsRunMod.h"
#include "SlfFkt.h"
#include "SlfSys.h"
CDsPar DsPar;
CDsRunMod::CDsRunMod(void) {

    CDsRunMod("");
}
CDsRunMod::CDsRunMod(char *log_file_name) {

    Status = OKAY;

    // Logfile 
    //========
    if( strlen(log_file_name) ) {
      pLogFile = new CSlfLogFile;
      if( !pLogFile ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CDsRunMod: Logfile <%s> konnte nicht angelegt werden"
                         ,log_file_name);
      } else {
        if( pLogFile->opennew(log_file_name) != OKAY) {
		      Status = NOT_OKAY;
          ErrText.catFormat("CDsRunMod: Logfile <%s> konnte nicht geöffnet werden"
                           ,log_file_name);
        }
      }
    } else {
      pLogFile   = 0;
    }

    // Parameterhandlingsklasse
    //pDsPar     = new CDsPar(pLogFile);
    //if( !pDsPar ) {
    //  Status = NOT_OKAY;
    //  ErrText.cat("CDsRunMod: Parameterklasse konnte nicht angelegt werden");
    //}
    pDsParFile = new CDsParFile(pLogFile);
    if( !pDsParFile ) {
      Status = NOT_OKAY;
      ErrText.cat("CDsRunMod: ParameterFileklasse konnte nicht angelegt werden");
    }
    // Modellhandlingsklasse noch nicht gesetzt
    pDsMod     =  new CDsMod(pLogFile,&DsPar);
    if( !pDsMod ) {
      Status = NOT_OKAY;
      ErrText.cat("CDsRunMod: Modellklasse konnte nicht angelegt werden");
    }


    // Texte
    ErrText = "";
    LogText = "";

    // Zeitwerte
    TStart = -1.;
    TEnd   = -1.;
    DeltaT = -1.;

}
// Destruktor ===============================================
//===========================================================
CDsRunMod::~CDsRunMod() {

  // pDsMod vor pDSPar zerstören
  // da Abhängigkeiten da sind
  delete pDsMod;
  if( pDsParFile )
    delete pDsParFile;
  //delete pDsPar;

  // Logfile schliessen
  if( pLogFile )
    pLogFile->closeFinal();

  Status = OKAY;
}
// Initialisieren
//==============================================================================
status_t CDsRunMod::init(SDsModVar           *p_ext_inp_list // Input-Variablenliste
                        ,uint16_t              n_ext_inp       // Anzahl Input Variablen
                        ,SDsModVar           *p_ext_out_list // Output-Variablenliste
                        ,uint16_t              n_ext_out       // Anzahl Output Variablen
                        ,SDsModVarNameChange *p_change_list  // Change Variablennamen-Liste
                        ,uint16_t              n_change_list   // Anzahl der Change Variablennamen
                        ,EParType            par_type        // Parametertype (PAR_DS; ...
                        ,char                *par_file_name  // Parameterfilename
                        ,double              delta_t_sim     // äussere Simulationszeit
                        ,double              t_start         // Startzeitpunkt
                        ) {

  // Input- Output-Initialisierung
  //-----------------------------
  Status = ioinit(p_ext_inp_list,n_ext_inp
                 ,p_ext_out_list,n_ext_out
                 ,p_change_list,n_change_list);

  // Parameter-Initialisierung
  //-----------------------------
  if( Status == OKAY && par_type != DS_PAR_TYPE_NO )
    Status = parinit(par_type,par_file_name); 

  //Funktionen-Initialisierung und t_start berechnung
  //-------------------------------------------------
  if( Status == OKAY )
    Status = funcinit(delta_t_sim,t_start);



  return Status;
}
// Beenden    ===============================================
//===========================================================
uint8_t CDsRunMod::done(void) {
  
  uint8_t ret = 0;

  // Modelle abschliessen
  //---------------------
  TEnd = pDsMod->getTime();
  pDsMod->done();

  // Parametere reset
  //-----------------
  //pDsPar->reset();
  DsPar.resetParData();

	//Logfile
  if( pLogFile && pLogFile->hasValue() ) {
    LogText.catFormat("CDsRunMod: siehe LogFile <%s> !!!!"
                     ,pLogFile->getLogFileFullName());
    ret = 1;
  }

  // LogFile
  if( pLogFile )
    pLogFile->close();

  return Status;
}
// Privat ===================================================
//===========================================================
// IO- Initialisieren
//==============================================================================
status_t CDsRunMod::ioinit(SDsModVar           *p_ext_inp_list         // Input-Variablenliste
                          ,uint16_t              n_ext_inp             // Anzahl Input Variablen
                          ,SDsModVar           *p_ext_out_list         // Output-Variablenliste
                          ,uint16_t              n_ext_out             // Anzahl Output Variablen
                          ,SDsModVarNameChange *p_change_list          // Change Variablennamen-Liste
                          ,uint16_t              n_change_list         // Anzahl der Change Variablennamen
                          ) {


  // Modelle mit den IOs initialisieren
  // Modelle müssen bei diesem Aufruf bereits existieren
  //----------------------------------------------------
  Status = pDsMod->setup(p_ext_inp_list,n_ext_inp
                        ,p_ext_out_list,n_ext_out
                        ,p_change_list,n_change_list);

  if( Status != OKAY )
    return Status;

  return Status;
}
// Parameter - Initialisieren
//==============================================================================
status_t CDsRunMod::parinit(EParType par_type              // Parametertype (PAR_DS; ...
                           ,char *par_file_name            // Parameterfilename
                           ) {


  if( par_type == DS_PAR_TYPE_NO ) {
  } else if( par_type == DS_PAR_TYPE_DS ) {

    if( strlen(par_file_name) == 0 ) {

      par_type = DS_PAR_TYPE_NO;
//      Status = NOT_OKAY;
//      ErrText.append("CDsRunMod::parinit: Error, not parameter file name given, while DS_PAR choosen");
//      if( pLogFile )
//        pLogFile->writeEnd(ErrText);
//      return Status;
    }
  } else {
      Status = NOT_OKAY;
      ErrText.append("CDsRunMod::parinit: Error, par_type not known");
      if( pLogFile )
        pLogFile->writeEnd(ErrText);
      return Status;
  }    
  if( par_type == DS_PAR_TYPE_DS ) {
    if( !SlfSysExistFile(par_file_name) ) {
      Status = NOT_OKAY;
      ErrText.catFormat("CDsRunMod::parinit: Error, parameter file %s not found",par_file_name);
      if( pLogFile )
        pLogFile->writeEnd(ErrText);
      return Status;
    }

    CSlfStr FullFileName;
			
		SlfSysGetActPath(FullFileName);
		FullFileName.append(par_file_name);


    //Status = pDsParFile->read(par_file_name,pDsPar->getRefParData());
    Status = pDsParFile->read(par_file_name,DsPar.getRefParData());

    if( Status != OKAY ) {
      ErrText.catFormat("CDsRunMod::parinit: Error, parameter file %s could not be read\n",par_file_name);
      
      if( pLogFile )
        pLogFile->writeEnd(ErrText);

      ErrText.cat(pDsParFile->getErrText());
      
      return Status;
    }
#ifdef DS_DEBUG

		//DSParFile.write("test_out.dat",DSPar.getRefParData());

		LogText.clear();
		LogText.catFormat("Parameter-File <%s> eingelesen\n",FullFileName.c_str());
	
    if( pLogFile ) {
      pLogFile->writeLine("-",30);
			pLogFile->writeEnd(LogText);
			pLogFile->writeLine("-",30);
    }
      
#endif
  }
  //------------------------------------------------
  // Parameter an die zugeordneten Modelle übergeben
  //------------------------------------------------
  //Status = pDsPar->get(0);
  Status = DsPar.get(0);

  if( Status != OKAY ) {
    ErrText.catFormat("CDsRunMod::parinit: Error, with parameter file %s problem to address\n",par_file_name);
    
    if( pLogFile )
      pLogFile->writeEnd(ErrText);
    
    //ErrText.cat(pDsPar->getErrText());
    ErrText.cat(DsPar.getErrText());

    return Status;
  }
#ifdef DS_DEBUG
  if( pLogFile )
    Status = pDsParFile->write(pLogFile,DsPar.getRefParDataUsed());
    //Status = pDsParFile->write(pLogFile,pDsPar->getRefParDataUsed());
  
  if( Status != OKAY ) {
    ErrText.cat("CDsRunMod::parinit: Error, with writeing parameter for debug purpose\n");
    
    if( pLogFile )
      pLogFile->writeEnd(ErrText);
    
    ErrText.cat(pDsParFile->getErrText());

    return Status;
  }
#endif

  return Status;
}
status_t CDsRunMod::funcinit(double delta_t_sim     // äussere Simulationszeit 
                            ,double t_start         // Startzeitpunkt
                            ) {

  // Modelle initialisieren
  //-----------------------
  DeltaT = delta_t_sim;
  Status = pDsMod->init(delta_t_sim);

  // Modell Startberechnung
  //-----------------------
  if( Status == OKAY ) {
    TStart = t_start;
    Status = pDsMod->first(t_start);
  }

  return Status;
}

// Logfile öffnen
//===========================================================
status_t CDsRunMod::setLogFile(char *log_file_name) {

  if( !pLogFile ) {

    pLogFile = new CSlfLogFile(log_file_name);
    if( !pLogFile ) {
      Status = 0;
    } else {
      Status = pLogFile->open();
      if( Status != OKAY )
        ErrText.append("Error CDsCtrl::setLogFile: LogFile could not be opened");
    }
  } else {
    Status = NOT_OKAY;
    ErrText.append("Error CDsCtrl::setLogFile: LogFile is already opened");
  }
  return Status;
}
#if 0
status_t CDsCtrl::run(void) {

    uint32_t iout,izw;

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

    DSCtrlIO.deleteInputList();
    DSCtrlIO.deleteOutputList();


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
status_t CDsCtrl::constructOutput(mxArray **ppout) {

    if( DSCtrlIO.constructOutput(ppout) != OKAY ) {

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

    // Ausgabewerte aus Modell sammeln
    //================================
    for(i=0;i<pDsMod->getNExtInp();i++) {


        pExtInpMod = pDsMod->getExtInp(i);

        if( DSCtrlIO.buildInputSingle(TStart,TEnd,"s"
                                     ,pExtInpMod->Name.c_str()
                                     ,pExtInpMod->Unit.c_str()
                                     ,pExtInpMod->Comment.c_str()
                                     ,pExtInpMod->Type
                                     ,pExtInpMod->pVal
                                     ) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
    }

    return Status;
}
#if DS_CTRL_IO_MEX
status_t CDsCtrl::constructInput(const mxArray *pin) {

    if( DSCtrlIO.constructInput(pin) != OKAY ) {

        ErrText.cat(DSCtrlIO.getErrText());
        DSCtrlIO.resetErrText();
        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
#endif
#endif