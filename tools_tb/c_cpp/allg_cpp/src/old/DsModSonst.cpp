#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DsMod.h"
#include "SlfFkt.h"


// Logfile schreiben
//=======================
void CDsMod::writeLogFile(char *name) {

    // Logfile starten
    //================
    if( pLogFile && !pLogFile->isOpen() ) {
      if( pLogFile->open() != OKAY)
		  Status = NOT_OKAY;
	}
    if( pLogFile ) {

        // Fehler aufgetreten
        //===================
        if( Status != OKAY && getLenErrText() > 0 ) {

            pLogFile->writeLine("#",30);
            pLogFile->write(name);
            pLogFile->writeEnd(": Error !!!");
            pLogFile->writeLine("#",30);
            pLogFile->writeEnd(getErrText());
            pLogFile->writeLine("#",30);

        }
        // Logtext
        //========
        if( getLogText() != 0 && getLenLogText() > 0 ) {

          pLogFile->writeLine("=",30);
          pLogFile->write(name);
          pLogFile->writeEnd(":");
          pLogFile->writeLine("=",30);
          pLogFile->writeEnd(getLogText());
          pLogFile->writeLine("=",30);
            
          resetLogText();
        }
    }
}
//=======================================================
// Findet die Änderungsnamen in der Liste
//=======================================================
char *CDsMod::findChangeName(char *ISPOType
                            ,char *mod_name
                            ,char *var_name
                            ,SDsModVarNameChange **pchange_list
                            ,uint16_t n_change_list) {

    char *pName = 0;
    SDsModVarNameChange *change_list = *pchange_list;

    for(uint16_t i=0;i<n_change_list;i++) {

        if(  (change_list[i].ModName == mod_name)
          && (change_list[i].VarName == var_name)
          && ( (   (   (*ISPOType == 'i')
                   &&  (  (change_list[i].ISPOType[0] == 'i')
                       || (change_list[i].ISPOType[0] == 'I')
                       )
                   )
               )
               ||
               (   (   (*ISPOType == 's')
                   &&  (  (change_list[i].ISPOType[0] == 's')
                       || (change_list[i].ISPOType[0] == 'S')
                       )
                   )
               )
               ||
               (   (   (*ISPOType == 'o')
                   &&  (  (change_list[i].ISPOType[0] == 'o')
                       || (change_list[i].ISPOType[0] == 'O')
                       )
                   )
               )
             )
          ) {
          pName = change_list[i].NewVarName;
          return pName;
        }
    }
    return pName;
}
//=======================================================
// Auflisten des Modells
//=======================================================
status_t CDsMod::listMod(bool fmod/*=true*/, bool finp/*=false*/, bool fsta/*=false*/, bool fout/*=false*/, bool fextinp/*=false*/, bool fextout/*=false*/ ) {

    uint16_t i,j,k,l;
    uint32_t ii=0;
   
    LogText.cat("\n");
    if( fmod ) {
        LogText.cat("Modellbeschreibung");
        ii += SlfStrLen("Modellbeschreibung");
    }
    if( finp ) { 
        LogText.cat(" / Modellinputs");
        ii += SlfStrLen("Modellbeschreibung");
    }
    if( fout ) {
        LogText.cat(" / Modelloutputs");
        ii += SlfStrLen("Modellbeschreibung");
    }
    if( fextinp ) {
        LogText.cat(" / externer Inputs");
        ii += SlfStrLen("Modellbeschreibung");
    }
    if( fextout )  {
        LogText.cat(" / externer Outputs");
        ii += SlfStrLen("Modellbeschreibung");
    }

    LogText.cat("\n");
    while( ii ) {
        LogText.cat("=");
        --ii;
    }
    LogText.cat("\n");

    if( fextinp ) {
        
        LogText.catFormat("externer Input\n");
        LogText.catFormat("--------------\n");
 
        for(j=0;NModels && j<pModList[NModels].NOut;j++) {

            LogText.catFormat("%i.) %s [%s]  (%s) <%s>\n"
                             ,j+1
                             ,pModList[NModels].pOut[j].Name.c_str()
                             ,pModList[NModels].pOut[j].Unit.c_str()
                             ,pModList[NModels].pOut[j].OldName.c_str()
                             ,pModList[NModels].pOut[j].Comment.c_str()
                             );
        }
        LogText.cat("\n");
    }
    for(i=0;i<NModels;i++) {

        if( fmod || finp || fout ) {
            LogText.catFormat("\n%c)Modell:%s\n",i+65,pModList[i].Name.c_str());

            for(j=0;j<pModList[i].Name.getLen()+9;j++)
                LogText.cat("=");
            LogText.cat("\n");
        }
        if( fmod || finp ) {
            LogText.catFormat("Inputs\n");
            LogText.catFormat("------\n");
 
            for(j=0;j<pModList[i].NInp;j++) {

                LogText.catFormat("%s  [%s]  (%s) <%s>\n"
                                 ,pModList[i].pInp[j].Name.c_str()
                                 ,pModList[i].pInp[j].Unit.c_str()
                                 ,pModList[i].pInp[j].OldName.c_str()
                                 ,pModList[i].pInp[j].Comment.c_str()
                                 );

                if( pModList[i].pInp[j].pValGet != 0 ) {

                    k = pModList[i].pInp[j].IModelGet;
                    l = pModList[i].pInp[j].JVarGet;
                
                    LogText.catFormat("    <= Mod%c:%s  Out%i:%s [%s] Fak:%f Off:%f\n\n"
                                     ,k+65
                                     ,pModList[k].Name.c_str()
                                     ,l+1
                                     ,pModList[k].pOut[l].Name.c_str()
                                     ,pModList[k].pOut[l].Unit.c_str()
                                     ,pModList[i].pInp[j].FaktorGet
                                     ,pModList[i].pInp[j].OffsetGet
                                     );

                }

            }
            LogText.cat("\n");
        }
        if( fmod || fsta ) {

            LogText.catFormat("States\n");
            LogText.catFormat("------\n");
 
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten
            
            for(j=0;j<pModList[i].NSta;j++) {

                if(  pModList[i].IntType == DEF_PIEULER 
                  || pModList[i].IntType == DEF_IEULER
                  ) {

                   LogText.catFormat("%i.) %s  "
                                     ,j+1
                                     ,pModList[i].VStaName.get_str(j)
                                     );


                    switch( pModList[i].IntType ) {

                    case DEF_PIEULER:
                        LogText.cat(" {Partial Implicit Euler} ");
                        break;
                    case DEF_IEULER:
                        LogText.cat(" {Implicit Euler} ");
                        LogText.catFormat(" err:%g",pModList[i].VError[j]);
                        break;
                    }                    

                    LogText.catFormat("<%s>\n"
                                     ,pModList[i].VStaComment.get_str(j)
                                     );
                } else {

                    LogText.catFormat("%i.) %s  (%s)"
                                     ,j+1
                                     ,pModList[i].pSta[j].Name.c_str()
                                     ,pModList[i].pSta[j].OldName.c_str()
                                     );
                    switch( pModList[i].pSta[j].IntType ) {

                    case DEF_EULER:
                        LogText.cat(" {Explicit Euler} ");
                        break;
                    case DEF_PC1:
                        LogText.cat(" {Pred: Euler/1Iter Korr:Trapez} ");
                        break;
                    case DEF_PC2:
                        LogText.cat(" {Pred: Euler/2Iter Korr:Trapez} ");
                        break;
                    case DEF_PC3:
                        LogText.cat(" {Pred: Euler/3Iter Korr:Trapez} ");
                        break;
                    case DEF_PCN:
                        LogText.catFormat(" {Pred: Euler/nIter(Err:%g) Korr:Trapez } "
                                         ,pModList[i].pSta[j].MaxError);
                        break;
                    }

                    LogText.catFormat("<%s>\n"
                                     ,pModList[i].pSta[j].Comment.c_str()
                                     );
                }
            }
#else
            for(j=0;j<pModList[i].NSta;j++) {

                    
               LogText.catFormat("%i.) %s  "
                                 ,j+1
                                 ,pModList[i].pSta->VStaName.get_str(j)
                                 );


                switch( pModList[i].pSta->IntType ) {

#if DS_MOD_INTEGRATION_PIEULER == 1
                case DEF_PIEULER:
                    LogText.cat(" {Partial Implicit Euler} ");
                    break;
#endif
#if DS_MOD_INTEGRATION_PGEARS == 1
                case DEF_PIEULER:
                    LogText.cat(" {Partial Gears} ");
                    break;
#endif
#if DS_MOD_INTEGRATION_IEULER == 1
                case DEF_IEULER:
                    LogText.cat(" {Implicit Euler} ");
                    LogText.catFormat(" err:%g",pModList[i].pSta->VError[j]);
                    break;
#endif
#if DS_MOD_INTEGRATION_GEARS == 1
                case DEF_GEARS:
                    LogText.cat(" {Implicit Gears} ");
                    LogText.catFormat(" err:%g",pModList[i].pSta->VError[j]);
                    break;
#endif
#if DS_MOD_INTEGRATION_DOPRI45 == 1
                case DEF_DOPRI45:
                    LogText.cat(" {Dormand-Prince 4./5. Ordnung} ");
                    LogText.catFormat(" err:%g",pModList[i].pSta->VError[j]);
                    break;
#endif
                case DEF_step:
                    LogText.cat(" {Step-Funktion keine Integration} ");
                    LogText.catFormat(" err:%g",pModList[i].pSta->VError[j]);
                    break;
                }                    

                LogText.catFormat("<%s>\n"
                                 ,pModList[i].pSta->VStaComment.get_str(j)
                                 );
            }
#endif
            LogText.cat("\n");
        }
        if( fmod || fout ) {
            LogText.catFormat("Oututs\n");
            LogText.catFormat("------\n");
 
            for(j=0;j<pModList[i].NOut;j++) {

                LogText.catFormat("%i.) %s  [%s]  (%s) <%s>\n"
                                 ,j+1
                                 ,pModList[i].pOut[j].Name.c_str()
                                 ,pModList[i].pOut[j].Unit.c_str()
                                 ,pModList[i].pOut[j].OldName.c_str()
                                 ,pModList[i].pOut[j].Comment.c_str()
                                 );
            }
            LogText.cat("\n");
        }
    }

    if( fextout ) {
        
        LogText.catFormat("%c)externer Output\n",NModels+65);
        LogText.catFormat("=================\n");
    
        for(j=0;NModels && j<pModList[NModels].NInp;j++) {

            LogText.catFormat("%i.) %s  [%s]  (%s) <%s>\n"
                             ,j+1
                             ,pModList[NModels].pInp[j].Name.c_str()
                             ,pModList[NModels].pInp[j].Unit.c_str()
                             ,pModList[NModels].pInp[j].OldName.c_str()
                             ,pModList[NModels].pInp[j].Comment.c_str()
                             );
            if( pModList[NModels].pInp[j].pValGet != 0 ) {

                k = pModList[NModels].pInp[j].IModelGet;
                l = pModList[NModels].pInp[j].JVarGet;
                
                LogText.catFormat("    <= Mod%c:%s  Out%i:%s [%s] Fak:%f Off:%f\n\n"
                                 ,k+65
                                 ,pModList[k].Name.c_str()
                                 ,l+1
                                 ,pModList[k].pOut[l].Name.c_str()
                                 ,pModList[k].pOut[l].Unit.c_str()
                                 ,pModList[NModels].pInp[j].FaktorGet
                                 ,pModList[NModels].pInp[j].OffsetGet
                                 );

            }

        }
        LogText.cat("\n");

    }

    return Status;


}
//=======================================================
// Initialisieren der Modelllist
//=======================================================
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten
void CDsMod::initModList() {


    // Modellliste zerstören
    if( pModList != 0 ) {
        for(uint16_t i=0;i<NModels+2+NFkts;i++) {

            pModList[i].pFunc = 0;
            pModList[i].Name  = "";

            pModList[i].NInp = 0;
            pModList[i].pInp = 0;

            pModList[i].NSta = 0;
            pModList[i].pSta = 0;

            pModList[i].InvMat    = 0;
            pModList[i].InvMatOld = 0;

            pModList[i].VSta0  = 0;
            pModList[i].VStam1  = 0;
            pModList[i].VStam2  = 0;
            pModList[i].Vdelta  = 0;
            
            pModList[i].pStaVal0 = 0;

            pModList[i].pStaDerVal0 = 0;

            pModList[i].NOut = 0;
            pModList[i].pOut = 0;

            pModList[i].NFkt = 0;
            pModList[i].pFkt = 0;

        }            

    }

}
#else
void CDsMod::initModList() {


    // Modellliste initialisiern
    if( pModList != 0 ) {
        for(uint16_t i=0;i<NModels+2+NFkts;i++) {

            pModList[i].pFunc = 0;
            pModList[i].Name  = "";

            pModList[i].NInp = 0;
            pModList[i].pInp = 0;

            pModList[i].NSta = 0;
            pModList[i].pSta = 0;
            
            pModList[i].NOut = 0;
            pModList[i].pOut = 0;

            pModList[i].NFkt = 0;
            pModList[i].pFkt = 0;

        }            

    }

}
#endif
//=======================================================
// Zerstören der Modelle
//=======================================================
void CDsMod::destroyMod() {

    if( BuildFlag  ) {
		// Modellliste zerstören
		if( pModList != 0 ) {
			for(uint16_t i=0;i<NModels+2+NFkts;i++) {

				if( pModList[i].NInp > 0 && pModList[i].pInp != 0)
					delete []pModList[i].pInp;

#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten   
				if( pModList[i].NSta > 0 && pModList[i].pSta != 0)
					delete []pModList[i].pSta;

				if( pModList[i].NSta > 0 && pModList[i].InvMat != 0 )
					FreeMatrix(pModList[i].InvMat);

				if( pModList[i].NSta > 0 && pModList[i].InvMatOld != 0 )
					FreeMatrix(pModList[i].InvMatOld);

				if( pModList[i].NSta > 0 && pModList[i].VSta0 != 0 )
					FreeVector(pModList[i].VSta0);
				if( pModList[i].NSta > 0 && pModList[i].VStam1 != 0 )
					FreeVector(pModList[i].VStam1);
				if( pModList[i].NSta > 0 && pModList[i].VStam2 != 0 )
					FreeVector(pModList[i].VStam2);
				if( pModList[i].NSta > 0 && pModList[i].Vdelta != 0 )
					FreeVector(pModList[i].Vdelta);

				if( pModList[i].NSta > 0  && pModList[i].pStaVal0 != 0)
					delete []pModList[i].pStaVal0;

				if( pModList[i].NSta > 0  && pModList[i].pStaDerVal0 != 0)
					delete []pModList[i].pStaDerVal0;
#else

                if( pModList[i].pSta != 0) {

                    if( pModList[i].pSta->VError )
                        FreeVector(pModList[i].pSta->VError);

#if DS_MOD_INTEGRATION_DOPRI45 == 1
                    if(  pModList[i].pSta->IntType == DEF_DOPRI45 )
                        delete pModList[i].pSta->pIntFunc;
#endif

					delete pModList[i].pSta;
                }


#endif

				if( pModList[i].NOut > 0 && pModList[i].pOut != 0 )
					delete []pModList[i].pOut;

				if( pModList[i].NFkt > 0 && pModList[i].pFkt != 0 )
					delete []pModList[i].pFkt;

			}            
			delete []pModList;
			NModels     = 0;
            NFkts       = 0;
			pModList    = 0;

		}
		BuildFlag     = 0;
	}

}
//========================================================
// Variablenzuordnung suchen, prüfen und bei debug ausgeben
//========================================================
status_t CDsMod::connectInpVar(void) {

    uint16_t i,j,k,l;

	if(     BuildFlag    ) {
		//=======================================
		// zu jedem Input muß es ein Output geben
		// Alle Outputs durchsuchen
		//========================================

		for(i=0;i<NModels+1;i++) { // einschl. externe Outputs


			for(j=0;j<pModList[i].NInp;j++) {

				// Variablennamen in der Outputliste suchen
				//-----------------------------------------
				if( findVarOut(/*Inp*/    pModList[i].pInp[j].Name.c_str(),i
							  ,/*in Out*/ &k,&l) != OKAY ) {

					Status = NOT_OKAY;
					ErrText.catFormat("Problem in CDsMod::connectVar mit findVarOut()\n");
					if( i == NModels )
						ErrText.catFormat("Zu dem externen Output <%s> konnte kein\n"
										 ,pModList[i].pInp[j].Name.c_str());
					else
						ErrText.catFormat("Zu Modell <%s> und dem Input <%s> konnte keine\n"
										 ,pModList[i].Name.c_str()
										 ,pModList[i].pInp[j].Name.c_str());
					ErrText.catFormat("Output oder externer Input gefunden werden !!!! (Siehe Liste mit Outputs in LogText)\n");

					listMod(false,false,false,true,true,false);

					return Status;
				}
				pModList[i].pInp[j].IModelGet = k;
				pModList[i].pInp[j].JVarGet   = l;


				// Faktor und Offset bestimmen
				//============================
				if( (Status = SlfFktUnitConv(pModList[k].pOut[l].Unit.c_str() /*=>*/
											,pModList[i].pInp[j].Unit.c_str()
											,&(pModList[i].pInp[j].FaktorGet)
											,&(pModList[i].pInp[j].OffsetGet)
											,ErrText
											))          != OKAY )  {

					char *pstring;

					ErrText.catFormat("Problem in SlfFktUnitConv mit findVarOut()\n");
					if( k == NModels )
						pstring = "externer Input";
					else if( k == NModels+1 )
						pstring = "Sonderwert";
					else
						pstring = "Output";

					ErrText.catFormat("Umrechnung von Einheit [%s] %s <%s> Modell <%s>\n"
									 ,pModList[k].pOut[l].Unit.c_str()
									 ,pstring
									 ,pModList[k].pOut[l].Name.c_str()
									 ,pModList[k].Name.c_str()
									 );
					ErrText.catFormat("zu Einheit [%s] Input <%s> Modell <%s>\n"
									 ,pModList[i].pInp[j].Unit.c_str()
									 ,pModList[i].pInp[j].Name.c_str()
									 ,pModList[i].Name.c_str()
									 );
					listMod(false,false,false,true,true,false);

					return Status;
				}
				// Pointer und Typ aus dem Output an  Input übergeben
				//===================================================
				pModList[i].pInp[j].pValGet = pModList[k].pOut[l].pVal;
				pModList[i].pInp[j].TypeGet = pModList[k].pOut[l].Type;



				// Typeprüfung
				if( proofType(pModList[i].pInp[j].Type
							 ,pModList[i].pInp[j].TypeGet) != OKAY ) {

					ErrText.catFormat(
						"Problem in CDsMod::connectVar mit proofType()\n");
					ErrText.catFormat(
						"Zu Modell <%s>, Input <%s>, Typ <%i> paßt\n"
						,pModList[i].Name.c_str()
						,pModList[i].pInp[j].Name.c_str()
						,(int)pModList[i].pInp[j].Type);
					ErrText.catFormat(
						"der Zuordnung <%s>, Typ <%i> nicht !!!! (Siehe eine Liste mit Outputs in LogText)\n"
						,pModList[k].pOut[l].Name.c_str()
						,(int)pModList[k].pOut[l].Type);
                
					return NOT_OKAY;
				}



			}

                
		}
	} else {
		Status = NOT_OKAY;
		ErrText.cat("CDsMod::connectInpVar error: Input kann nicht verbunden werden vor build (setup registrierte Modelle)\n");
	}
    return Status;
}
//========================================================
// Funktionpointerzuordnung suchen, prüfen 
//========================================================
status_t CDsMod::connectFkt(void) {

    uint16_t i,j,k;
    uint8_t  found_flag;

	if( BuildFlag ) {

		for(i=0;i<NModels;i++) {

       
			for(j=0;j<pModList[i].NFkt;j++) {

                found_flag = 0;

                // Funktion in der Funkttionsliste suchen
                //=======================================
                for(k=0;k<NFkts;k++) {

                    // Funktion
                    //=========
                    if( pModList[i].pFkt[j].Name == pModList[k+NModels+2].Name ) {
                        found_flag = 1;
                        *(pModList[i].pFkt[j].ppFkt) = pModList[i+NModels+2].pFunc;
                        break;
                    }
                }

                if( !found_flag ) {

                    Status = NOT_OKAY;
    		        ErrText.catFormat("CDsMod::connectFkt error: Funktionspointer von (gesuchter Name <%s>) kann nicht gefunden werden in den registrierten Modellen\n"
                                     ,pModList[i].pFkt[j].Name.c_str());

                    if( NFkts == 0 ) {
    		            ErrText.cat("Es wurden keine Funktionenen registriert!!!!!!\n");
                    } else {
                        ErrText.cat("Registrierte Funktionen:\n");

                        for(k=0;k<NFkts;k++) {
                            ErrText.catFormat("%s \n"
                                             ,pModList[k+NModels+2].Name.c_str()
                                             );
                        }
                    }
                }
            }
        }
	}
    return Status;
}
//========================================================
// Input Variablenzuordnung
//========================================================
void CDsMod::setInpVar(SDsModInfo *pMod) {

    uint16_t j;


        for(j=0;j<pMod->NInp;j++) {


            setVar(&(pMod->pInp[j]));
        }
}
//========================================================
// Initial Stateszuordnung
//========================================================
#if DS_MOD_NEW_STA == 0
void CDsMod::setStaIni(void) {
    
    for(uint16_t i=0;i<NModels;i++) {

        // Bei invers Euler Fehlerschranke prüfen
        if( pModList[i].IntType == DEF_IEULER ) {

            for(uint16_t j=0;j<pModList[i].NSta;j++) {

                pModList[i].VError[j] = fabs(pModList[i].VError[j]);
                if( pModList[i].VError[j] < EPSILON )
                    pModList[i].VError[j] = 2*EPSILON;

#if DS_DEBUG_MODE & 2
                LogText.catFormat("Modell:%s <%s> verr[%i] = %g\n"
					             ,pModList[i].Name.c_str()
								 ,pModList[i].VStaName.get_str(j)
								 ,j
					             ,pModList[i].VError[j]);
#endif
            }

			ZeroMatrix(pModList[i].InvMat);
			ZeroMatrix(pModList[i].InvMatOld);
        }

        for(uint16_t j=0;j<pModList[i].NSta;j++) {


            if(  pModList[i].IntType == DEF_PIEULER 
              || pModList[i].IntType == DEF_IEULER
              ) {


                pModList[i].VSta[j] = pModList[i].VStaIni[j];
                pModList[i].VSta0[j] = pModList[i].VStaIni[j];
                pModList[i].VStam1[j] = pModList[i].VStaIni[j];
                pModList[i].VStam2[j] = pModList[i].VStaIni[j];
                pModList[i].Vdelta[j] = 0.0;
            
            } else {

            *pModList[i].pSta[j].pStaVal =
                *pModList[i].pSta[j].pStaValIni;
            }

		    // Invertieren der Jacobimatrix wenn diese konstant bleibt
			if(  (   pModList[i].IntType == DEF_PIEULER
				 ||  pModList[i].IntType == DEF_IEULER
				 )
			  && pModList[i].FlagMJacobi 
			  ) 
				calcInvertDMatrix(&pModList[i]);
        }
    }
}
#else
#if 0 // falls noch gebraucht
void CDsMod::setStaIni(void) {
    
    for(uint16_t i=0;i<NModels;i++) {


    }
}
#endif
#endif
//========================================================
// externer Output Variablenzuordnung
//========================================================
void CDsMod::setExtOutVar(void) {

    uint16_t j;

    for(j=0;j<pModList[NModels].NInp;j++) {


        setVar(&pModList[NModels].pInp[j]);
    }
}
//========================================================
// Variablenzuordnung
//========================================================
void CDsMod::setVar(SDsModVarInfo   *pVar) {

    switch(pVar->Type) {

    case DEF_VOID:
        // keinen Wert zuweisen
        break;
    case DEF_STRING:

        pVar->pVal = 
                (char *)pVar->pValGet;

        break;
    default:
        {
        double yGetD;

        switch(pVar->TypeGet) {

        case DEF_DOUBLE:
            yGetD = *(double *)pVar->pValGet;                        
            break;
        case DEF_FLOAT:                    
            yGetD = (double)*(float *)pVar->pValGet;
            break;
        case DEF_SIGNED_LONG:                    
            yGetD = (double)*(signed long *)pVar->pValGet;
            break;
        case DEF_UNSIGNED_LONG:                    
            yGetD = (double)*(unsigned long *)pVar->pValGet;
            break;
        case DEF_SIGNED_SHORT:
            yGetD = (double)*(signed short *)pVar->pValGet;
            break;
        case DEF_UNSIGNED_SHORT:
            yGetD = (double)*(unsigned short *)pVar->pValGet;
            break;
        case DEF_SIGNED_CHAR:
            yGetD = (double)*(signed char *)pVar->pValGet;
            break;
        case DEF_UNSIGNED_CHAR:
            yGetD = (double)*(unsigned char *)pVar->pValGet;
            break;
        }
        switch(pVar->Type) {
        case DEF_DOUBLE:
            *(double *)pVar->pVal 
                = yGetD * pVar->FaktorGet + pVar->OffsetGet;
            break;
        case DEF_FLOAT:
            *(float *)pVar->pVal 
                = (float)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        case DEF_SIGNED_LONG:
            *(signed long *)pVar->pVal 
                = (signed long)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        case DEF_UNSIGNED_LONG:
            *(unsigned long *)pVar->pVal 
                = (unsigned long)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        case DEF_SIGNED_SHORT:
            *(signed short *)pVar->pVal 
                = (signed short)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        case DEF_UNSIGNED_SHORT:
            *(unsigned short *)pVar->pVal 
                = (unsigned short)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        case DEF_SIGNED_CHAR:
            *(signed char *)pVar->pVal 
                = (signed char)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        case DEF_UNSIGNED_CHAR:
            *(unsigned char *)pVar->pVal 
                = (unsigned char)(yGetD * pVar->FaktorGet + pVar->OffsetGet);
            break;
        }

        break;
        }

    }

}
//========================================================
// Variablennamen in der Outputliste suchen
//========================================================
status_t CDsMod::findVarOut(char *name,uint16_t imod0,uint16_t *pk,uint16_t *pl) {

    uint16_t i,j;
    
    Status = OKAY;
    
    //=======================================
    // zu dem Namen ein Output finden
    // Alle Outputs durchsuchen
    //========================================
    for(i=0;i<NModels+2;i++) { // in pModList[NModels] befinden sich die externen Inputs
                              // und in pModList[NModels] befinden sich Sonderwerte (NULL)
        if( i != imod0 ) {
            for(j=0;j<pModList[i].NOut;j++) {

                if( pModList[i].pOut[j].Name.compare(name) ) {
                    *pk = i;
                    *pl = j;
                    return Status;
                }
            }
        }
    }
    Status = NOT_OKAY;

    return Status;
}
//========================================================
// Typezuordnung prüfen
//========================================================
status_t CDsMod::proofType(EVarType TypeI
                          ,EVarType TypeO) {


    switch( TypeI ) {
    case DEF_VOID: //Kein Wert wird zugewiesen, ist okay
        break;
    case DEF_DOUBLE:
    case DEF_FLOAT:
    case DEF_SIGNED_LONG:
    case DEF_UNSIGNED_LONG:
    case DEF_SIGNED_SHORT:
    case DEF_UNSIGNED_SHORT:
    case DEF_SIGNED_CHAR:
    case DEF_UNSIGNED_CHAR: 
    
        // Darf alles sein nur klein string und void
        if( TypeO == DEF_VOID || TypeO == DEF_STRING ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_STRING:

        // Darf nur string sein
        if( TypeO != DEF_STRING ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    }
        
    return Status;
}

uint16_t CDsMod::getNExtOut(void) {

    if( NModels > 0 )
        return pModList[NModels].NInp;

    return 0;
}
SDsModVarInfo *CDsMod::getExtOut(uint16_t iout) {

    if( NModels > 0 ) {

        if( iout < pModList[NModels].NInp )
            return &pModList[NModels].pInp[iout];

    }
    return 0;
}
uint16_t CDsMod::getNExtInp(void) {

    if( NModels > 0 )
        return pModList[NModels].NOut;

    return 0;
}
SDsModVarInfo *CDsMod::getExtInp(uint16_t iinp) {

    if( NModels > 0 ) {

        if( iinp < pModList[NModels].NOut )
            return &pModList[NModels].pOut[iinp];

    }
    return 0;
}

