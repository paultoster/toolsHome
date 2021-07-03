#if 0
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DsMod.h"
#include "SlfFkt.h"


//========================================================
// REchenschrittweite initialisieren
//========================================================
status_t CDsMod::initDt(double dtmast) {

    uint16_t i;

    if( fabs(dtmast) < EPSILON ) {

        ErrText.catFormat("Problem in CDsMod::initDt \n");
        ErrText.catFormat("Die Master-Rechenschrittweite Dt=%g ist zu klein/null\n "
                         ,dtmast);
        Status = NOT_OKAY;
        return Status;
    }

    DtMast   = dtmast;
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
    DtMast05 = dtmast*0.5;
#endif
    if( DtModMin > 0.0 ) { 
        DtModMin = MIN(DtMast,DtModMin);
        NDt      = 0;
        while( DtMast - DtModMin*NDt > 0.0 )
            NDt++;
    } else {
        DtModMin = DtMast;
        NDt      = 1;
    }

    // Im ersten Schritt sollen alle Schrittweiten gleich
    // später soll auch mit unterschiedlichen gearbeitet werden
    for(i=0;i<NModels;i++) {

        pModList[i].Dt = DtModMin; 
#if 0
        if( fabs(DtModMin-pModList[i].Dt) > EPSILON ) {

            ErrText.catFormat("Problem in CDsMod::initDt \n");
            ErrText.catFormat("Die Rechenschrittweite Dt=%g von Modell <%s> "
                              "ist nicht gleich der übergeordneten Schrittweite Dt=%g \n"
                             ,pModList[i].Dt
                             ,pModList[i].Name.c_str()
                             ,DtModMin);
            Status = NOT_OKAY;
            return Status;
        }
#endif
    }

    // Wenn NDt = 1, dann brauchen externe Eingänge nicht interpoliert zu werden
    // Ursprünglicher Pointer und Type werden gesetzt und Flag zurückgesetzt
    //==========================================================================
    if( NDt == 1 ) {

        for(i=0;i<pModList[NModels].NOut;i++) {

            if( pModList[NModels].pOut[i].LinInter ) {

                pModList[NModels].pOut[i].LinInter = 0;

                pModList[NModels].pOut[i].pVal = pModList[NModels].pOut[i].pValExtInp;
                pModList[NModels].pOut[i].Type = pModList[NModels].pOut[i].TypeExtInp;
            }
        }

        //========================================================
        // Variablenzuordnung nochmal durchführen, suchen, prüfen und bei debug ausgeben
        //========================================================
        if( Status == OKAY )
            Status=connectInpVar();



    }

    return Status;

}
//========================================================
// Initialisierung der Modelle
//========================================================
status_t CDsMod::calcInitMod(void) {

    uint16_t i;

    // Modell initialisieren
    //======================
    for(i=0;i<NModels;i++) {

        // Input Variablen setzen
        setInpVar(&pModList[i]);

#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
        // Ableitung der Zustände zu null setzen
        if(  pModList[i].IntType == DEF_IEULER  
          || pModList[i].IntType == DEF_GEARS  ) { 
            
            ZeroVector(pModList[i].VStaDer);
        }
#endif

#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
        if( pModList[i].pFunc->init(pModList[i].Dt) != OKAY ) {
#else
        if( pModList[i].pFunc->init(pModList[i].Dt,pModList[i].NSta) != OKAY ) {
#endif
            ErrText.catFormat("init-Fehler in Modell <%s> !!!\n",pModList[i].Name.c_str());
            ErrText.cat(pModList[i].pFunc->getErrText());
            pModList[i].pFunc->resetErrText();
            Status = NOT_OKAY;
        }
        if( pModList[i].pFunc->getLenLogText() > 0 ) {

            LogText.catFormat("Init Modell <%s>:\n",pModList[i].Name.c_str());
            LogText.cat(pModList[i].pFunc->getLogText());
            pModList[i].pFunc->resetLogText();
        }
        if( Status != OKAY)
            return Status;

    }

    return Status;
}
//========================================================
// Initialisierung der Funktionen
//========================================================
status_t CDsMod::calcInitFkt(void) {

    uint16_t i;

    // Modell initialisieren
    //======================
    for(i=0;i<NFkts;i++) {

#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
      if( pModList[i+NModels+2].pFunc->init(pModList[i+NModels+2].Dt) != OKAY ) {
#else
      if( pModList[i+NModels+2].pFunc->init(pModList[i+NModels+2].Dt,pModList[i+NModels+2].NSta) != OKAY ) {
#endif
            ErrText.catFormat("init-Fehler in Funktion <%s> !!!\n",pModList[i+NModels+2].Name.c_str());
            ErrText.cat(pModList[i+NModels+2].pFunc->getErrText());
            pModList[i+NModels+2].pFunc->resetErrText();
            Status = NOT_OKAY;
        }
        if( pModList[i+NModels+2].pFunc->getLenLogText() > 0 ) {

            LogText.catFormat("Init Modell <%s>:\n",pModList[i+NModels+2].Name.c_str());
            LogText.cat(pModList[i+NModels+2].pFunc->getLogText());
            pModList[i+NModels+2].pFunc->resetLogText();
        }
        if( Status != OKAY)
            return Status;

    }

    return Status;
}
//========================================================
// erste Berechnung der Modelle
//========================================================
status_t CDsMod::calcFirstMod(void) {

    uint16_t i;

   
    for(i=0;i<NModels;i++) {

        // Modell erste Berechnung
        //======================
        if( pModList[i].pFunc->first(pModList[i].TimeAct) != OKAY ) {

            ErrText.catFormat("first-Fehler in Modell <%s> !!!\n",pModList[i].Name.c_str());
            ErrText.cat(pModList[i].pFunc->getErrText());
            pModList[i].pFunc->resetErrText();
            Status = NOT_OKAY;
        }
        if( pModList[i].pFunc->getLenLogText() > 0 ) {

            LogText.catFormat("First Modell <%s>:\n",pModList[i].Name.c_str());
            LogText.cat(pModList[i].pFunc->getLogText());
            pModList[i].pFunc->resetLogText();
        }
        if( Status != OKAY)
            return Status;

#if DS_MOD_NEW_STA == 1
        
        // Integration initialisieren
        //===========================
        if( 
#if DS_MOD_INTEGRATION_PIEULER == 1
          pModList[i].pSta->IntType == DEF_PIEULER    ||  
#endif
#if DS_MOD_INTEGRATION_PGEARS == 1
          pModList[i].pSta->IntType == DEF_PGEARS     ||  
#endif
#if DS_MOD_INTEGRATION_DOPRI45 == 1
          pModList[i].pSta->IntType == DEF_DOPRI45    ||  
#endif
#if DS_MOD_INTEGRATION_RADAU == 1
          pModList[i].pSta->IntType == DEF_RADAU      ||
#endif
#if DS_MOD_INTEGRATION_LSODA == 1
          pModList[i].pSta->IntType == DEF_LSODA      ||
#endif
#if DS_MOD_INTEGRATION_IEULER == 1
          pModList[i].pSta->IntType == DEF_IEULER     ||
#endif
#if DS_MOD_INTEGRATION_GEARS == 1
          pModList[i].pSta->IntType == DEF_GEARS      ||
#endif
          0 ) {
            if( pModList[i].pSta->pIntFunc->calcFirst(pModList[i].TimeAct
                                                     ,pModList[i].pSta->VState
                                                     ,pModList[i].Dt
                                                     ) != OKAY ) {

                ErrText.catFormat("Error CDsMod::calcFirstMod in Modell <%s> !!!\n",pModList[i].Name.c_str());
                ErrText.cat(pModList[i].pSta->pIntFunc->getErrText());
                Status = NOT_OKAY;
                return Status;
            }
        }
#endif

    }
    return Status;
}
//========================================================
// Derivative-Berechnung der Modelle
//========================================================
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
status_t CDsMod::calcStaMod(SDsModInfo *pMod) {


    // Modell Berechnung
    //======================
    if( pMod->pFunc->state(pMod->TimeAct) != OKAY ) {

        ErrText.catFormat("state-Fehler in Modell <%s> !!!\n",pMod->Name.c_str());
        ErrText.cat(pMod->pFunc->getErrText());
        pMod->pFunc->resetErrText();
        Status = NOT_OKAY;
    }
    if( pMod->pFunc->getLenLogText() > 0 ) {

        LogText.catFormat("Derivative Modell <%s>:\n",pMod->Name.c_str());
        LogText.cat(pMod->pFunc->getLogText());
        pMod->pFunc->resetLogText();
    }
    
    return Status;
}
#endif
//========================================================
// Vorverarbeitung-Zustandsberechnung der Modelle
//========================================================
#if DS_MOD_NEW_STA == 1  
status_t CDsMod::calcPreStaMod(SDsModInfo *pMod) {

    // Modell Berechnung
    //======================
    if( pMod->pFunc->prestate(pMod->TimeAct) != OKAY ) {

        ErrText.catFormat("prestate-Fehler in Modell <%s> !!!\n",pMod->Name.c_str());
        ErrText.cat(pMod->pFunc->getErrText());
        pMod->pFunc->resetErrText();
        Status = NOT_OKAY;
    }
    if( pMod->pFunc->getLenLogText() > 0 ) {

        LogText.catFormat("Derivative Modell <%s>:\n",pMod->Name.c_str());
        LogText.cat(pMod->pFunc->getLogText());
        pMod->pFunc->resetLogText();
    }
    return Status;
}
#endif
//========================================================
// Loop-Berechnung der Modelle
//========================================================
status_t CDsMod::calcOutMod(SDsModInfo *pMod) {


    // Modell Berechnung
    //======================
    if( pMod->pFunc->output(pMod->TimeAct) != OKAY ) {

        ErrText.catFormat("loop-Fehler in Modell <%s> !!!\n",pMod->Name.c_str());
        ErrText.cat(pMod->pFunc->getErrText());
        pMod->pFunc->resetErrText();
        Status = NOT_OKAY;
    }
    if( pMod->pFunc->getLenLogText() > 0 ) {

        LogText.catFormat("Loop Modell <%s>:\n",pMod->Name.c_str());
        LogText.cat(pMod->pFunc->getLogText());
        pMod->pFunc->resetLogText();
    }
    
    return Status;
}
//========================================================
// Done-Berechnung der Modelle
//========================================================
status_t CDsMod::calcDoneMod(void) {

    uint16_t i;

	if(     BuildFlag   ) {
		// Modell Berechnung
		//======================
		for(i=0;i<NModels;i++) {

#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
            if(  pModList[i].IntType == DEF_IEULER 
              || pModList[i].IntType == DEF_GEARS
              )
                LogText.catFormat("Model %s, IntType:%i, MaxIter:%i\n "
                                 ,pModList[i].Name.c_str()
                                 ,pModList[i].IntType
                                 ,pModList[i].Niter
                                 );
#endif
			if( pModList[i].pFunc->done() != OKAY ) {

				ErrText.catFormat("done-Fehler in Modell <%s> !!!\n",pModList[i].Name.c_str());
				ErrText.cat(pModList[i].pFunc->getErrText());
				pModList[i].pFunc->resetErrText();
				Status = NOT_OKAY;
			}
			if( pModList[i].pFunc->getLenLogText() > 0 ) {

				LogText.catFormat("Done Modell <%s>:\n",pModList[i].Name.c_str());
				LogText.cat(pModList[i].pFunc->getLogText());
				pModList[i].pFunc->resetLogText();
			}

#if DS_MOD_NEW_STA == 1
            // Ingetrationsfunktion zurücksetzen
            //==================================
        if( 
#if DS_MOD_INTEGRATION_PIEULER == 1
          pModList[i].pSta->IntType == DEF_PIEULER    ||  
#endif
#if DS_MOD_INTEGRATION_PGEARS == 1
          pModList[i].pSta->IntType == DEF_PGEARS     ||  
#endif
#if DS_MOD_INTEGRATION_DOPRI45 == 1
          pModList[i].pSta->IntType == DEF_DOPRI45    ||  
#endif
#if DS_MOD_INTEGRATION_RADAU == 1
          pModList[i].pSta->IntType == DEF_RADAU      ||
#endif
#if DS_MOD_INTEGRATION_LSODA == 1
          pModList[i].pSta->IntType == DEF_LSODA      ||
#endif
#if DS_MOD_INTEGRATION_IEULER == 1
          pModList[i].pSta->IntType == DEF_IEULER     ||
#endif
#if DS_MOD_INTEGRATION_GEARS == 1
          pModList[i].pSta->IntType == DEF_GEARS      ||
#endif
          0 )
                pModList[i].pSta->pIntFunc->reset();
#endif
			if( Status != OKAY)
				return Status;

		}
	}
    return Status;
}
//========================================================
// Done-Berechnung der Funktionen
//========================================================
status_t CDsMod::calcDoneFkt(void) {

    uint16_t i;

	if(     BuildFlag   ) {
		// Modell Berechnung
		//======================
		for(i=0;i<NFkts;i++) {

			if( pModList[i+NModels+2].pFunc->done() != OKAY ) {

				ErrText.catFormat("done-Fehler in Funktion <%s> !!!\n",pModList[i+NModels+2].Name.c_str());
				ErrText.cat(pModList[i+NModels+2].pFunc->getErrText());
				pModList[i+NModels+2].pFunc->resetErrText();
				Status = NOT_OKAY;
			}
			if( pModList[i+NModels+2].pFunc->getLenLogText() > 0 ) {

				LogText.catFormat("Done Modell <%s>:\n",pModList[i+NModels+2].Name.c_str());
				LogText.cat(pModList[i+NModels+2].pFunc->getLogText());
				pModList[i+NModels+2].pFunc->resetLogText();
			}

			if( Status != OKAY)
				return Status;

		}
	}
    return Status;
}
//=======================================
// Integration
//=======================================
#if DS_MOD_NEW_STA == 0
status_t CDsMod::calcMod(double timeact) {    

    uint16_t ii,i,j;
    double state_last;


    // nächsten Step oder erste Integration berechnen
    //===============================================
    for(i=0;i<NModels;i++) {

        // Input Variablen setzen
        setInpVar(&pModList[i]);

        // ohne Integration
        //=================
        if( pModList[i].IntType == DEF_step ) {

            // Zeit setzen
            //============
            pModList[i].TimeAct = timeact;

            if( calcOutMod(&pModList[i]) != OKAY )
                return Status;

        // Partial Implcit Euler
        //======================
        } else if( pModList[i].IntType == DEF_PIEULER ) {

            Vector_t vec = NewVector(pModList[i].NSta);

            // Zeit setzen
            //============
            pModList[i].TimeAct = timeact;

#if DS_DEBUG_MODE & 2
            LogText.catFormat("TimAct = %g Partial Implicit Euler Modell: <%s>\n\n"
					         ,pModList[i].TimeAct
							 ,pModList[i].Name.c_str()
							 );
#endif
            // Ableitung und gegebenenfalls Jacobimatrix bilden
            //=================================================
            if( calcStaMod(&pModList[i]) != OKAY )
                return Status;


            // bei nicht konstanter Jycobimatrix Invertiere (E-h*J)^-1 bilden
            //===============================================================
            if( !pModList[i].FlagMJacobi )
                if( calcInvertDMatrix(&pModList[i]) != OKAY )
                    return Status;
                
            // h * (E-h*J)^-1 * f(t+h,x(t))
            MatrixTimesVector(pModList[i].InvMat,pModList[i].VStaDer,vec);
            ScalarTimesVector(pModList[i].Dt,vec,vec);

            // x(t+h) = x(t) + h * (E-h*J)^-1 * f(t+h,x(t))
            VectorAdd(pModList[i].VSta,vec,pModList[i].VSta);

            FreeVector(vec);

#if DS_DEBUG_MODE & 2

            for(j=0;j<pModList[i].NSta;j++)
            LogText.catFormat("<%s>: VSta[%i] = %g\n"
                             ,pModList[i].pSta[j].Name.c_str()
							 ,j
                             ,pModList[i].VSta[j]);
            for(j=0;j<pModList[i].NSta;j++)
            LogText.catFormat("     VStaDer[%i] = %g\n"
                             ,j
                             ,pModList[i].VStaDer[j]);
#endif
            // Calc Output
            if( calcOutMod(&pModList[i]) != OKAY )
                return Status;

            //pModList[i].CntDownIFSum = 0;

        // Implicit Euler
        //===============
        } else if( pModList[i].IntType == DEF_IEULER ) {

            //Vector_t vec1    = NewVector(pModList[i].NSta);
            Vector_t vec     = NewVector(pModList[i].NSta);
            Vector_t delta   = NewVector(pModList[i].NSta);
            uint8_t  niter   = DS_MOD_MAX_ITERATION;
            uint8_t  errflag = 1;
            //double factor;

            // Zeit setzen
            //============
            pModList[i].TimeAct = timeact;

#if DS_DEBUG_MODE & 2
            LogText.catFormat("TimAct = %g Implicit Euler Modell: <%s>\n\n"
					         ,pModList[i].TimeAct
							 ,pModList[i].Name.c_str()
							 );
#endif

            // alten Zustand merken
            //=====================
#if 0
            ScalarTimesVector(1.0,pModList[i].VStam1,pModList[i].VStam2);
            ScalarTimesVector(1.0,pModList[i].VSta0,pModList[i].VStam1);
#endif
            ScalarTimesVector(1.0,pModList[i].VSta,pModList[i].VSta0);
            ScalarTimesVector(1.0,pModList[i].VStaDer,pModList[i].VStaDer0);
            
            // Erste Schätzung: Verfahren nach Heun
            //=====================================
            // x(t)+h*f(t,x(t)
            ScalarTimesVector(pModList[i].Dt,pModList[i].VStaDer0,vec);
            VectorAdd(vec,pModList[i].VSta0,pModList[i].VSta);
            // f(t+h,x(t)+h*f(t,x(t)) bilden
            if( calcStaMod(&pModList[i]) != OKAY )
                return Status;
            // PHI = 0.5*(f(t,x(t))+f(t+h,x(t)+h*f(t,x(t)))
            VectorAdd(pModList[i].VStaDer0,pModList[i].VStaDer,vec);
            ScalarTimesVector(0.5*pModList[i].Dt,vec,vec);

            // x(t+h) = x(t) + h * PHI
            VectorAdd(vec,pModList[i].VSta0,pModList[i].VSta);


            while(errflag && niter) { // Newton iteration


                // bei nicht konstanter Jacobimatrix Invertiere [E-h*J]^-1 bilden
                //===============================================================
                if( !pModList[i].FlagMJacobi )
                    if( calcInvertDMatrix(&pModList[i]) != OKAY )
                        return Status;
#if 0
                // [xn(t+h)-18/11*x(t)+9/11*x(t-h)-2/11*x(t-2h)-h*6/11f(t+h,xn(t+h)]
			    //==================================================================
                factor = pModList[i].Dt*(-6.)/11.;
                ScalarTimesVector(factor,pModList[i].VStaDer,vec);

                factor = (-2.)/11.;
                ScalarTimesVector(factor,pModList[i].VStam2,vec1);
                VectorAdd(vec1,vec,vec);

                factor = 9./11.;
                ScalarTimesVector(factor,pModList[i].VStam1,vec1);
                VectorAdd(vec1,vec,vec);

                factor = (-18.)/11.;
                ScalarTimesVector(factor,pModList[i].VSta0,vec1);
                VectorAdd(vec1,vec,vec);


                VectorAdd(vec,pModList[i].VSta,vec);
#endif
                // delta = [E-h*J]^-1 *h*f(t+h,xn(t+h)-xn(t+h)+x(t)
			    //=================================================
                ScalarTimesVector(pModList[i].Dt,pModList[i].VStaDer,vec);
                MatrixTimesVector(pModList[i].InvMat,vec,delta);

                VectorSub(delta,pModList[i].VSta,delta);
                VectorAdd(delta,pModList[i].VSta0,delta);

                // neuer wert merken
				//==================
                ScalarTimesVector(1.0,delta,pModList[i].Vdelta);


                // neuer Wert xn+1(t+h) = xn(t+h) + delta
				//=======================================
                VectorAdd(pModList[i].VSta,delta,pModList[i].VSta);

#if DS_DEBUG_MODE & 2

                for(j=0;j<pModList[i].NSta;j++)
                LogText.catFormat("<%s> %i: VSta[%i] = %g, VStaDer = %g, VSta0 = %g\n"
								 ,pModList[i].VStaName.get_str(j)
                                 ,DS_MOD_MAX_ITERATION-niter
                                 ,j
                                 ,pModList[i].VSta[j]
								 ,pModList[i].VStaDer[j]
								 ,pModList[i].VSta0[j]);
                for(j=0;j<pModList[i].NSta;j++)
                LogText.catFormat("     %i: delta[%i] = %g\n"
                                 ,DS_MOD_MAX_ITERATION-niter
                                 ,j
                                 ,delta[j]);
#endif                //Abbruchbedingung
                errflag = 0;
                for(j=0;j<pModList[i].NSta;j++) {

                    if( fabs(delta[j]) > pModList[i].VError[j] ) {

                        errflag = 1;
                        break;
                    }
                }
                --niter;

                // Ableitung bilden
                //=================
                if( calcStaMod(&pModList[i]) != OKAY )
                    return Status;
                
            }
#if DS_DEBUG_MODE & 2
            if( niter != pModList[i].Niter )
                LogText.catFormat("t=%g     %d Iterationen\n"
                                 ,pModList[i].TimeAct
                                 ,DS_MOD_MAX_ITERATION-niter
                                 );
            if( !niter )
                LogText.catFormat("Modell <%s> t=%g maxiter\n"
                                 ,pModList[i].Name.c_str()
                                 ,pModList[i].TimeAct);
#endif
            if( niter > pModList[i].Niter ) 
                pModList[i].Niter = niter;

            //FreeVector(vec1);
            FreeVector(vec);
            FreeVector(delta);

            // Calc Output
            if( calcOutMod(&pModList[i]) != OKAY )
                return Status;
            //pModList[i].CntDownIFSum = 0;


        // Euler oder Prediktor/Korrektor
        //===============================
        } else { 



            if( calcStaMod(&pModList[i]) != OKAY )
                return Status;

#if DS_DEBUG_MODE & 2
            LogText.catFormat("\nTimAct = %g Euler Modell: <%s>\n"
					         ,timeact
							 ,pModList[i].Name.c_str()
							 );
#endif    
			for(j=0;j<pModList[i].NSta;j++) {

                // alter Wert merken
                pModList[i].pStaVal0[j] = *pModList[i].pSta[j].pStaVal;
                
#if DS_DEBUG_MODE & 2
	            LogText.catFormat("<%s> j: %i, s0: %g,"
						         ,pModList[i].pSta[j].Name.c_str()
		                         ,j
			                     ,*pModList[i].pSta[j].pStaVal);
#endif
                // Euler
                *pModList[i].pSta[j].pStaVal +=
                    *pModList[i].pSta[j].pStaValDer * DtMast;

#if DS_DEBUG_MODE & 2
				LogText.catFormat(" s: %g, sp: %g\n"
						         ,*pModList[i].pSta[j].pStaVal
						         ,*pModList[i].pSta[j].pStaValDer);
#endif
            }

            // Zeit setzen
            //============
            pModList[i].TimeAct = timeact;

            if( calcOutMod(&pModList[i]) != OKAY )
                return Status;

#if DS_DEBUG_MODE & 2
	            LogText.catFormat("t: %g  Iteration:\n",TimeAct);   
#endif
            // nur für Korrektor-Iteration
            if( pModList[i].MaxIter > 0 ) {


                // Inistialisieren fürs abzählen
                pModList[i].CntDownIFSum = pModList[i].IterFlagSum;

                for(j=0;j<pModList[i].NSta;j++) {

                    // Iteration initialisieren
                    if( pModList[i].pSta[j].MaxIter > 0 ) {

                        // Zustand merken
                        pModList[i].pStaDerVal0[j] = *pModList[i].pSta[j].pStaValDer;

                        pModList[i].pSta[j].ActIterFlag = 1;
                        pModList[i].pSta[j].NIter       = 0;
                    }
                }
            }


        }
    }

    CntDownIFSum = IterFlagSum;

    // Schleife über maximale Anzal Korrektorverfahren
    for(ii=0;ii<MaxIter && CntDownIFSum;ii++) {

        // Korrektor-Integration
        //======================
        for(i=0;i<NModels;i++) {


            // Prüfen, ob noch notwendig
            if( pModList[i].CntDownIFSum ) {
            
                // Input Variablen erneut setzen
                setInpVar(&pModList[i]);

                // Zustandsänderung berechnen
                if( calcStaMod(&pModList[i]) != OKAY )
                    return Status;

                // Zustand einzeln berechnen
                for(j=0;j<pModList[i].NSta;j++) {

                    // Wenn noch notwendig
                    if( pModList[i].pSta[j].ActIterFlag ) {
#if DS_DEBUG_MODE & 2
						LogText.catFormat("<%s> j: %i i: %i:\n"
                          ,pModList[i].pSta[j].Name.c_str()
                          ,j
                          ,pModList[i].pSta[j].NIter+1);
#endif

                        // Merker
                        state_last = *pModList[i].pSta[j].pStaVal;

                        // Trapez integration
                        *pModList[i].pSta[j].pStaVal =
                            pModList[i].pStaVal0[j] +
                            (
                            *pModList[i].pSta[j].pStaValDer 
                            +pModList[i].pStaDerVal0[j]
                            )* DtMast05;


#if DS_DEBUG_MODE & 2
						LogText.catFormat("<%s> j: %i, s0: %g, s: %g, sp: %g, sp0: %g\n"
						  ,pModList[i].pSta[j].Name.c_str()
                          ,j
                          ,pModList[i].pStaVal0[j]
                          ,*pModList[i].pSta[j].pStaVal
                          ,*pModList[i].pSta[j].pStaValDer
                          ,pModList[i].pStaDerVal0[j]
						  );
#endif
            
                        // Anzahl Iteration hochzählen
                        pModList[i].pSta[j].NIter++;

                        // Iteration mit Iterationsfehlerabschätzung
                        if(   (pModList[i].pSta[j].IntType == DEF_PCN)
                          &&  (fabs(state_last-*pModList[i].pSta[j].pStaVal)
                              <= pModList[i].pSta[j].MaxError)
                              ) {
                        
                              pModList[i].pSta[j].ActIterFlag = 0;
                              pModList[i].CntDownIFSum--;

                              if( !pModList[i].CntDownIFSum )
                                  CntDownIFSum--;
                        }

                        // Iteration ohne Abschätzung
                        if(  (pModList[i].pSta[j].IntType != DEF_PCN)
                          && ii+1 == pModList[i].pSta[j].MaxIter
                          ) {
                              pModList[i].pSta[j].ActIterFlag = 0;
                              pModList[i].CntDownIFSum--;

                              if( !pModList[i].CntDownIFSum )
                                  CntDownIFSum--;
                        }


                    } 
                }

                if( calcOutMod(&pModList[i]) != OKAY )
                    return Status;
            }

        }
    }

    return Status;

}
#else
status_t CDsMod::calcMod(double timeact) {    

    uint16_t i;


    // nächsten Step oder erste Integration berechnen
    //===============================================
    for(i=0;i<NModels;i++) {

        // Input Variablen setzen
        //=======================
        setInpVar(&pModList[i]);

        // Zeit setzen
        //============
        pModList[i].TimeAct = timeact;

        // Partial implizit Euler / Partial Gears / implizit Euler / Gears
        //================================================================
        if(  
#if DS_MOD_INTEGRATION_IEULER == 1
          pModList[i].pSta->IntType == DEF_IEULER      ||
#endif
#if DS_MOD_INTEGRATION_GEARS == 1
          pModList[i].pSta->IntType == DEF_GEARS       ||
#endif
#if DS_MOD_INTEGRATION_PIEULER == 1
          pModList[i].pSta->IntType == DEF_PIEULER     ||
#endif
#if DS_MOD_INTEGRATION_PGEARS == 1
          pModList[i].pSta->IntType == DEF_PGEARS      ||
#endif
          0 ) {

            // Vorverarbeitung vor Zustandsberechnung
            //=======================================
            if( calcPreStaMod(&pModList[i]) != OKAY )
                return Status;

            Status = pModList[i].pSta->pIntFunc->calc(pModList[i].TimeAct);
#if DS_DEBUG_MODE & 2

            LogText.catFormat("%s: fcn=%li step=%li njac=%li\n"
                             ,pModList[i].Name.c_str()
                             ,pModList[i].pSta->pIntFunc->nfcnRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             ,pModList[i].pSta->pIntFunc->njacRead()
                             );
            LogText.catFormat("x=%g h=%g\n"
                             ,pModList[i].pSta->pIntFunc->xRead()
                             ,pModList[i].pSta->pIntFunc->hRead());
#endif

            if( Status != OKAY ) {

                ErrText.catFormat("Error CDsMod::calcMod in Modell <%s> !!!\n",pModList[i].Name.c_str());
                ErrText.cat(pModList[i].pSta->pIntFunc->getErrText());
                return Status;
            }        
				
				
		// Dormand-Prince 4(5)
		//====================
		} 
#if DS_MOD_INTEGRATION_DOPRI45 == 1
        else if(  pModList[i].pSta->IntType == DEF_DOPRI45 ) {

            // Vorverarbeitung vor Zustandsberechnung
            //=======================================
            if( calcPreStaMod(&pModList[i]) != OKAY )
                return Status;

            Status = pModList[i].pSta->pIntFunc->calc(pModList[i].TimeAct);
#if DS_DEBUG_MODE & 2

            LogText.catFormat("%s: fcn=%li step=%li accpt=%li reject=%li\n"
                             ,pModList[i].Name.c_str()
                             ,pModList[i].pSta->pIntFunc->nfcnRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             ,pModList[i].pSta->pIntFunc->naccptRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             -pModList[i].pSta->pIntFunc->naccptRead());
            LogText.catFormat("x=%g h=%g\n"
                             ,pModList[i].pSta->pIntFunc->xRead()
                             ,pModList[i].pSta->pIntFunc->hRead());
#endif

            if( Status != OKAY ) {

                ErrText.catFormat("Error CDsMod::calcMod in Modell <%s> !!!\n",pModList[i].Name.c_str());
                ErrText.cat(pModList[i].pSta->pIntFunc->getErrText());
                return Status;
            }        

        } 
#endif
#if DS_MOD_INTEGRATION_RADAU == 1
        else if(  pModList[i].pSta->IntType == DEF_RADAU ) {

            // Vorverarbeitung vor Zustandsberechnung
            //=======================================
            if( calcPreStaMod(&pModList[i]) != OKAY )
                return Status;


            Status = pModList[i].pSta->pIntFunc->calc(pModList[i].TimeAct);
#if DS_DEBUG_MODE & 2

            LogText.catFormat("%s: fcn=%li step=%li accpt=%li reject=%li njac=%li\n"
                             ,pModList[i].Name.c_str()
                             ,pModList[i].pSta->pIntFunc->nfcnRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             ,pModList[i].pSta->pIntFunc->naccptRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             -pModList[i].pSta->pIntFunc->naccptRead()
                             ,pModList[i].pSta->pIntFunc->njacRead()
                             );
            LogText.catFormat("x=%g h=%g\n"
                             ,pModList[i].pSta->pIntFunc->xRead()
                             ,pModList[i].pSta->pIntFunc->hRead());
#endif

            if( Status != OKAY ) {

                ErrText.catFormat("Error CDsMod::calcMod in Modell <%s> !!!\n",pModList[i].Name.c_str());
                ErrText.cat(pModList[i].pSta->pIntFunc->getErrText());
                return Status;
            }        
        }				
#endif
#if DS_MOD_INTEGRATION_LSODA == 1
        else if(  pModList[i].pSta->IntType == DEF_LSODA ) {

            // Vorverarbeitung vor Zustandsberechnung
            //=======================================
            if( calcPreStaMod(&pModList[i]) != OKAY )
                return Status;


            Status = pModList[i].pSta->pIntFunc->calc(pModList[i].TimeAct);
#if DS_DEBUG_MODE & 2

            LogText.catFormat("%s: fcn=%li step=%li accpt=%li reject=%li order=%li meth=%li njac=%li\n"
                             ,pModList[i].Name.c_str()
                             ,pModList[i].pSta->pIntFunc->nfcnRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             ,pModList[i].pSta->pIntFunc->naccptRead()
                             ,pModList[i].pSta->pIntFunc->nstepRead()
                             -pModList[i].pSta->pIntFunc->naccptRead()
                             ,pModList[i].pSta->pIntFunc->norderRead()
                             ,pModList[i].pSta->pIntFunc->methRead()
                             ,pModList[i].pSta->pIntFunc->njacRead()
                             );
            LogText.catFormat("x=%g h=%g\n"
                             ,pModList[i].pSta->pIntFunc->xRead()
                             ,pModList[i].pSta->pIntFunc->hRead());
#endif

            if( Status != OKAY ) {

                ErrText.catFormat("Error CDsMod::calcMod in Modell <%s> !!!\n",pModList[i].Name.c_str());
                ErrText.cat(pModList[i].pSta->pIntFunc->getErrText());
                return Status;
            }        
				
        }
#endif

		if( calcOutMod(&pModList[i]) != OKAY )
			return Status;

    }

    return Status;

}
#endif
//========================================================
// Invertieren von (E-h*Jacobi)^-1 für DEF_PIEULER
//========================================================
#if DS_MOD_NEW_STA == 0
status_t CDsMod::calcInvertDMatrix(SDsModInfo *pMod) {
    
    uint16_t j;
    double det;
    Matrix_t m = MatrixCopy(pMod->MJacobi);

    // Jacobi * (-h)
    ScalarTimesMatrix(-DtMast,m,m);

    // + E
    for(j=0;j<pMod->NSta;j++)
        m[j][j] += 1.0;

    det = InvertMatrix(m,pMod->InvMat);

    FreeMatrix(m);

    if( fabs(det) < EPSILON ) {

		PutMatrixToMatrix(pMod->InvMatOld, pMod->InvMat, GET_NROWS(pMod->InvMat), GET_NCOLS(pMod->InvMat));
        LogText.catFormat("Error CDsMod::calcInvertDMatrix: In Modell <%s> ist (E-h*Jycobi)^-1 singulär !!!!\n"
                         ,pMod->Name.c_str()
                         );
        //Status = NOT_OKAY;
    } else {
		PutMatrixToMatrix(pMod->InvMat, pMod->InvMatOld, GET_NROWS(pMod->InvMat), GET_NCOLS(pMod->InvMat));
	}
    return Status;
}
#else
#endif

//========================================================
// bestimme externen Input
//========================================================
void CDsMod::calcExtInp(uint8_t init) {
// Berechnet externen Input 
// init=2 erster Aufruf bei First, zum Initialisieren
//     =1 erster Aufruf bei einer neuen externenn Loop
//        um neue Steigung von letztem zu neuem Endwert
//        zu generieren
//     =0 Berechnung aktuellen Werts
//
// DVal[0] Wert aus letzen übergeordneten Zeitschritt DtMast
// DVal[1] Endwert zum jetzigen Zeitschritt
// DVal[2] Steigung von DVal[0] zu DVal[1] d.h.
//
// val = DVal[0]+DVal[2]*dt und DVal[1] = DVal[0]+DVal[2]*DtMast

    uint32_t i;
    double dt = TimeAct-TimeOld;

    // Schleife über alle externen Eingänge
    //=====================================
    for(i=0;i<pModList[NModels].NOut;i++) {

        if( pModList[NModels].pOut[i].LinInter ) {

            // erstmal initialisieren
            //=======================
            if( init == 2 ) {


                pModList[NModels].pOut[i].DVal[2] = 0.;

                switch(pModList[NModels].pOut[i].TypeExtInp) {

                case DEF_DOUBLE:
                    pModList[NModels].pOut[i].DVal[0] = *(double *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_FLOAT:                    
                    pModList[NModels].pOut[i].DVal[0] = (double)*(float *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_SIGNED_LONG:                    
                    pModList[NModels].pOut[i].DVal[0] = (double)*(sint32_t *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_UNSIGNED_LONG:                    
                    pModList[NModels].pOut[i].DVal[0] = (double)*(uint32_t *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_SIGNED_SHORT:
                    pModList[NModels].pOut[i].DVal[0] = (double)*(sint16_t *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_UNSIGNED_SHORT:
                    pModList[NModels].pOut[i].DVal[0] = (double)*(uint16_t *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_SIGNED_CHAR:
                    pModList[NModels].pOut[i].DVal[0] = (double)*(sint8_t *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                case DEF_UNSIGNED_CHAR:
                    pModList[NModels].pOut[i].DVal[0] = (double)*(uint8_t *)pModList[NModels].pOut[i].pValExtInp;
                    break;
                }

                pModList[NModels].pOut[i].DVal[1] = pModList[NModels].pOut[i].DVal[0];

            // Zur neuen übergeordneten Schleife initialisieren
            //=================================================
            } else if( init == 1 ) {


                pModList[NModels].pOut[i].DVal[0] = pModList[NModels].pOut[i].DVal[1];

                switch(pModList[NModels].pOut[i].TypeExtInp) {
                case DEF_DOUBLE:
                    pModList[NModels].pOut[i].DVal[2] = (*(double *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_FLOAT:                    
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(float *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_SIGNED_LONG:                    
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(sint32_t *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_UNSIGNED_LONG:                    
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(uint32_t *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_SIGNED_SHORT:
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(sint16_t *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_UNSIGNED_SHORT:
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(uint16_t *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_SIGNED_CHAR:
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(sint8_t *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                case DEF_UNSIGNED_CHAR:
                    pModList[NModels].pOut[i].DVal[2] = ((double)*(uint8_t *)pModList[NModels].pOut[i].pValExtInp
                                                        -pModList[NModels].pOut[i].DVal[1]
                                                        )/DtMast;
                    break;
                }

            // Interpolation
            //==============
            } else { 


                pModList[NModels].pOut[i].DVal[1] = pModList[NModels].pOut[i].DVal[0]
                                                  + pModList[NModels].pOut[i].DVal[2]
                                                  * dt;

                // Integerumrechnung
                //==================
                if(  (pModList[NModels].pOut[i].TypeExtInp == DEF_SIGNED_LONG) 
                  || (pModList[NModels].pOut[i].TypeExtInp == DEF_UNSIGNED_LONG) 
                  || (pModList[NModels].pOut[i].TypeExtInp == DEF_SIGNED_SHORT) 
                  || (pModList[NModels].pOut[i].TypeExtInp == DEF_UNSIGNED_SHORT) 
                  || (pModList[NModels].pOut[i].TypeExtInp == DEF_SIGNED_CHAR) 
                  || (pModList[NModels].pOut[i].TypeExtInp == DEF_UNSIGNED_CHAR) 
                  ) {

                    pModList[NModels].pOut[i].DVal[1] = (double)((sint32_t)(pModList[NModels].pOut[i].DVal[1]+0.5));
                }

            }
        }
    }

}

#endif