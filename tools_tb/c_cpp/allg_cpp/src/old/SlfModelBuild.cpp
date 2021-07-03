#if 0
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DsMod.h"
#include "SlfFkt.h"


// Code der Integratoren wird hier eingbunden
//#if DS_MOD_INTEGRATION_RADAU == 1
//    #include "IntegratorRAD.cpp"
//#endif
//#if DS_MOD_INTEGRATION_DOPRI45 == 1
//    #include "IntegratorERK.cpp"
//#endif
//#if (DS_MOD_INTEGRATION_GEARS == 1) || (DS_MOD_INTEGRATION_IEULER == 1) 
//    #include "IntegratorGEAR.cpp"
//#endif
//#if DS_MOD_INTEGRATION_LSODA == 1
//    #include "IntegratorLSODA.cpp"
//#endif

//=======================================================
// Modell, externe Inputs und Output einordnen
//=======================================================
status_t CDsMod::buildMod(SDsModVar *ext_inp_list, uint16_t n_ext_inp
                         ,SDsModVar *ext_out_list, uint16_t n_ext_out
                         ,SDsModVarNameChange *change_list/*=NULL*/
                         ,uint16_t n_change_list/*=0*/
                         ) {

  uint16_t i,j;
  SDsRegVar *pVar;
  SDsRegFkt *pFkt;
  char *pName;

  // Anzahl der Modelle
  NModels = getRegNMod();
  NFkts   = getRegNFkt();

  // mindestens ein Modell muß vorhanden sein
  if( !NModels ) {
	  //ErrText.cat("CDsMod::buildMod error: Es sind keine Modelle angemeldet");
    LogText.cat("CDsMod::buildMod warning: Es sind keine Modelle angemeldet");
	  //Status = NOT_OKAY;
	  //return NOT_OKAY;
  } 

  // Modellnamen sollen sich unterscheiden
  if( checkRegModName() != OKAY )
    return NOT_OKAY;

  // Modellliste + externer Input/Output + dummy + Fktliste
  //=======================================================
  if( pModList != 0 ) destroyMod();
  pModList      = new SDsModInfo[NModels+2+NFkts];

  initModList();

  // externe Input/Output
  //=====================
  pModList[NModels].pFunc   = NULL;
  pModList[NModels].Name    = "IO";
  
  // Default
  pModList[NModels].Dt          = -1.0;
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten   
  pModList[NModels].NSta        = 0;
  pModList[NModels].pSta        = 0;
  pModList[NModels].pStaDerVal0 = 0;
  pModList[NModels].pStaVal0    = 0;
  pModList[NModels].IntType     = DEF_step;
  pModList[NModels].MaxIter     = 0;
#else
  pModList[NModels].NSta        = 0;
#endif


  // externer Output  auf Input legen
  //=================================
  pModList[NModels].NInp    = n_ext_out;
  if( n_ext_out )
    pModList[NModels].pInp    = new SDsModVarInfo[pModList[NModels].NInp];
  else
    pModList[NModels].pInp    = 0;

  for(j=0;j<pModList[NModels].NInp;j++) {

    pModList[NModels].pInp[j].Name      = ext_out_list[j].Name;
    pModList[NModels].pInp[j].pVal      = ext_out_list[j].pVal;
    pModList[NModels].pInp[j].Type      = ext_out_list[j].Type;
    pModList[NModels].pInp[j].Unit      = ext_out_list[j].Unit;
    pModList[NModels].pInp[j].Comment   = ext_out_list[j].Comment;
    pModList[NModels].pInp[j].pValGet   = 0;
    pModList[NModels].pInp[j].FaktorGet = 1.0;
    pModList[NModels].pInp[j].OffsetGet = 0.0;

  }

  // externer Input  auf Output legen
  //=================================
  pModList[NModels].NOut    = n_ext_inp;
  if( n_ext_inp )
    pModList[NModels].pOut    = new SDsModVarInfo[pModList[NModels].NOut];
  else
    pModList[NModels].pOut    = 0;

  for(j=0;j<pModList[NModels].NOut;j++) {

    pModList[NModels].pOut[j].Name       = ext_inp_list[j].Name;
    pModList[NModels].pOut[j].pValExtInp = ext_inp_list[j].pVal;
    pModList[NModels].pOut[j].TypeExtInp = ext_inp_list[j].Type;
    pModList[NModels].pOut[j].Unit       = ext_inp_list[j].Unit;
    pModList[NModels].pOut[j].Comment    = ext_inp_list[j].Comment;
    pModList[NModels].pOut[j].LinInter   = ext_inp_list[j].LinInter;

    // pVal wird einer internen Größe zugeordent, 
    // die bei jedem untergeordneten Zeitschritt
    // interpoliert wird
    //-------------------------------------------
    if(  pModList[NModels].pOut[j].LinInter
      && (  (ext_inp_list[j].Type == DEF_DOUBLE)
         || (ext_inp_list[j].Type == DEF_FLOAT)
         || (ext_inp_list[j].Type == DEF_SIGNED_LONG)
         || (ext_inp_list[j].Type == DEF_SIGNED_SHORT)
         || (ext_inp_list[j].Type == DEF_SIGNED_CHAR)
         || (ext_inp_list[j].Type == DEF_UNSIGNED_LONG)
         || (ext_inp_list[j].Type == DEF_UNSIGNED_SHORT)
         || (ext_inp_list[j].Type == DEF_UNSIGNED_CHAR)
         )
      ) { 
        pModList[NModels].pOut[j].Type = DEF_DOUBLE;
        pModList[NModels].pOut[j].pVal = &pModList[NModels].pOut[j].DVal[1];

    } else {

      // Könnte ja gesetzt sein, auf keinen Fall intepolieren
      pModList[NModels].pOut[j].LinInter = 0;

      pModList[NModels].pOut[j].pVal = ext_inp_list[j].pVal; // keine skalierung
      pModList[NModels].pOut[j].Type = ext_inp_list[j].Type;
    }

    pModList[NModels].pOut[j].pValGet = 0;

  }


  // Dummy Modell
  pModList[NModels+1].pFunc   = NULL;
  pModList[NModels+1].Name    = "Dummy";

  // Default
  pModList[NModels+1].Dt          = -1.0;
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten   
  pModList[NModels+1].NSta        = 0;
  pModList[NModels+1].pSta        = 0;
  pModList[NModels+1].pStaDerVal0 = 0;
  pModList[NModels+1].pStaVal0    = 0;
  pModList[NModels+1].IntType     = DEF_step;
  pModList[NModels+1].MaxIter     = 0;
#else
  pModList[NModels].NSta       = 0;
#endif

  // keinen Input
  //=============
  pModList[NModels+1].NInp    = 0;
  pModList[NModels+1].pInp    = NULL;

  // Dummyvariablen  auf Output
  //===========================
  pModList[NModels+1].NOut    = 1;
  pModList[NModels+1].pOut    = new SDsModVarInfo[pModList[NModels+1].NOut];

  pModList[NModels+1].pOut[0].Name    = "NULL";
  pModList[NModels+1].pOut[0].pVal    = &WertNull;
  pModList[NModels+1].pOut[0].Type    = DEF_DOUBLE;
  pModList[NModels+1].pOut[0].Unit    = "0";
  pModList[NModels+1].pOut[0].Comment = "Wert Null";
  pModList[NModels+1].pOut[0].pValGet = 0;

  //===========================================================
  //===========================================================
  // Modelliste füllen
  //===========================================================
  //===========================================================
  IterFlagSum = 0;
  for(i=0;i<NModels;i++) {

    // Modell
    //=======
    pModList[i].pFunc      = getRegModPointer(i);
    pModList[i].Name       = getRegModName(i);
    pModList[i].Dt         = 0.0;

    // Inputs
    //=======

    //Anzahl Inputs
    pModList[i].NInp = getRegNInp(i);

    //Liste füllen
    if( pModList[i].NInp == 0 )
        pModList[i].pInp    = 0;
    else {

        // Anlegen
        pModList[i].pInp    = new SDsModVarInfo[pModList[i].NInp];

        for(j=0;j<pModList[i].NInp;j++) {

            // nächste Struktur holen
            pVar = getRegInpStruct(i,j);

            if( pVar == 0 ) {
                
                Status = NOT_OKAY;
                ErrText.cat("Fehler in CDsMod::buildMod bei Aufruf von DsModGetModelInp()"
                            " pVar ist null");
                return Status;
            }
                          

            pModList[i].pInp[j].Name      = pVar->VarName;
            pModList[i].pInp[j].pVal      = pVar->pVal;
            pModList[i].pInp[j].Type      = pVar->Type;
            pModList[i].pInp[j].Unit      = pVar->Unit;
            pModList[i].pInp[j].Comment   = pVar->Comment;
            pModList[i].pInp[j].pValGet   = 0;
            pModList[i].pInp[j].FaktorGet = 1.0;
            pModList[i].pInp[j].OffsetGet = 0.0;
    
            //Namensumnennung suchen
            if( (pName=findChangeName("i"
                                     ,pModList[i].Name.c_str()
                                     ,pModList[i].pInp[j].Name.c_str()
                                     ,&change_list
                                     ,n_change_list
                                     )) != 0 ) {

                pModList[i].pInp[j].OldName = pModList[i].pInp[j].Name;
                pModList[i].pInp[j].Name    = pName;
            }
        }
    }

    // States
    //=======
  if( buildModSta(&pModList[i],i,change_list,n_change_list) != OKAY )
	  return NOT_OKAY;


    // Outputs
    //========

    //Anzahl Outputs
    pModList[i].NOut = getRegNOut(i);

    //Liste füllen
    if( pModList[i].NOut == 0 )
        pModList[i].pOut    = 0;
    else {

        // Anlegen
        pModList[i].pOut    = new SDsModVarInfo[pModList[i].NOut];

        for(j=0;j<pModList[i].NOut;j++) {

            // nächste Struktur holen
            pVar = getRegOutStruct(i,j);

            if( pVar == 0 ) {
                
                Status = NOT_OKAY;
                ErrText.cat("Fehler in CDsMod::buildMod bei Aufruf von DsModGetModelOut()"
                            " pVar ist null");
                return Status;
            }

            pModList[i].pOut[j].Name    = pVar->VarName;
            pModList[i].pOut[j].pVal    = pVar->pVal;
            pModList[i].pOut[j].Type    = pVar->Type;
            pModList[i].pOut[j].Unit    = pVar->Unit;
            pModList[i].pOut[j].Comment = pVar->Comment;
            pModList[i].pOut[j].pValGet = 0;
    
            if( (pName=findChangeName("o"
                                     ,pModList[i].Name.c_str()
                                     ,pModList[i].pOut[j].Name.c_str()
                                     ,&change_list
                                     ,n_change_list
                                     )) != 0 ) {

                pModList[i].pOut[j].OldName = pModList[i].pOut[j].Name;
                pModList[i].pOut[j].Name    = pName;
            }
        }
    }

    // Fkt-Pointer
    //============

    //Anzahl 
    pModList[i].NFkt = getRegNFkt(i);

    //Liste füllen
    if( pModList[i].NFkt == 0 )
        pModList[i].pFkt    = 0;
    else {

        // Anlegen
        pModList[i].pFkt    = new SDsModFktInfo[pModList[i].NFkt];

        for(j=0;j<pModList[i].NFkt;j++) {

            // nächste Struktur holen
            pFkt = getRegFktStruct(i,j);

            if( pFkt == 0 ) {
                
                Status = NOT_OKAY;
                ErrText.cat("Fehler in CDsMod::buildMod bei Aufruf von getRegFktStruct()"
                            " pFkt ist null");
                return Status;
            }

            pModList[i].pFkt[j].Name    = pFkt->VarName;
            pModList[i].pFkt[j].ppFkt   = pFkt->ppFkt;
            pModList[i].pFkt[j].Comment = pFkt->Comment;
    
        }
    }


#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten   
  // MaxIterationen über alles 
  MaxIter = 0;
  for(i=0;i<NModels;i++) 
      MaxIter = MAX(MaxIter,pModList[i].MaxIter);

#endif
  }

  //===========================================================
  //===========================================================
  // Funktionsliste füllen
  //===========================================================
  //===========================================================
  for(i=0;i<NFkts;i++) {

    // Funktion
    //=======
    pModList[i+NModels+2].pFunc      = getRegFktPointer(i);
    pModList[i+NModels+2].Name       = getRegFktName(i);

  }

  BuildFlag     = 1;

  return Status;
}
//=======================================================
// Nimmt die Zustände der Funtionen auf
//=======================================================
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten   
status_t CDsMod::buildModSta(SDsModInfo *pMod
							,uint16_t index
							,SDsModVarNameChange *change_list
                            ,uint16_t n_change_list) {

	SDsRegSta *pSta;
	char      *pName;
	uint16_t    j;

    // States
    //=======
    pMod->pSta        = 0;
    pMod->pStaDerVal0 = 0;
    pMod->pStaVal0    = 0;
    pMod->InvMat      = 0;
    pMod->InvMatOld   = 0;
    pMod->VStaDer0    = 0;
    pMod->VSta0       = 0;
    pMod->VStam1      = 0;
    pMod->VStam2      = 0;
    pMod->Vdelta      = 0;
    pMod->MaxIter     = 0;
    pMod->IterFlagSum = 0;
    pMod->CntDownIFSum= 0;
    pMod->Niter       = 0;
    //Anzahl Stats
    pMod->NSta = getRegNSta(index);


    // IntTypes prüfen, da nicht alle gleichzeitig verwendet werden könnnen
    if(  pMod->NSta > 0
      && !proofRegStaTypes(index) 
      ) {

        ErrText.catFormat("In Modell <%s> sind zur Berechnung der Zustände neben PIEULER oder IEULER auch ander Verfahren gewählt, was nicht geht !!!\n" 
                         ,pMod->Name.c_str());
        Status = NOT_OKAY;
        return Status;
    }



    //Liste füllen

    // keine States
    //=============
    if( pMod->NSta == 0 ) {

        // keine Zustände keine Iteration
        pMod->IntType     = DEF_step;

    // Teilimpliziter & implizite Euler
    //==================================
    } else if(  getRegStaType(index) == DEF_PIEULER 
             || getRegStaType(index) == DEF_IEULER
             ) {

        int ii;

        pMod->IntType     = getRegStaType(index);

        // einzige Struktur holen
        pSta = getRegStaStruct(index,0);

        pMod->VStaName    = pSta->VStaName;
        pMod->VStaComment = pSta->VStaComment;
        pMod->VSta        = pSta->VSta;
        pMod->VStaDer     = pSta->VStaDer;
        pMod->VStaIni     = pSta->VStaIni;
        pMod->MJacobi     = pSta->MJacobi;        
        pMod->FlagMJacobi = pSta->FlagMJacobi;
        pMod->VError      = pSta->VError;


        // Auffüllen VStaName
        ii = pMod->NSta - pMod->VStaName.getNrows();
        if( ii > 0 ) {

            while(ii--)
                pMod->VStaName.append("");
        }

        // Auffüllen VStaComment
        ii = pMod->NSta - pMod->VStaComment.getNrows();
        if( ii > 0 ) {

            while(ii--)
                pMod->VStaComment.append("");
        }

        // Überprüfen VStaDer
        ii = pMod->NSta - GET_NROWS(pMod->VStaDer);
        if( ii != 0 ) {

            ErrText.catFormat("Error CDsMod::buildMod: Aus Modell <%s> paßt Länge des StateDerivetiveVektors VStaDer <%i> nicht mit StateVektor VSta <%i> überein !!\n"
                             ,pMod->Name.c_str()
                             ,GET_NROWS(pMod->VStaDer)
                             ,pMod->NSta
                             );
            Status = NOT_OKAY;
            return Status;
        }

        // Überprüfen VStaIni
        ii = pMod->NSta - GET_NROWS(pMod->VStaIni);
        if( ii != 0 ) {

            ErrText.catFormat("Error CDsMod::buildMod: Aus Modell <%s> paßt Länge des StateIniVektors VStaIni <%i> nicht mit StateVektor VSta <%i> überein !!\n"
                             ,pMod->Name.c_str()
                             ,GET_NROWS(pMod->VStaIni)
                             ,pMod->NSta
                             );
            Status = NOT_OKAY;
            return Status;
        }
        
        // Überprüfen MJacobi
        if(  pMod->NSta - GET_NROWS(pMod->MJacobi) != 0 
          || GET_NCOLS(pMod->MJacobi) != GET_NROWS(pMod->MJacobi) ) {

            ErrText.catFormat("Error CDsMod::buildMod: Aus Modell <%s> paßt Dimension  LakobiMatrix <%i/%i> nicht mit StateVektor VSta <%i> überein !!\n"
                             ,pMod->Name.c_str()
                             ,GET_NROWS(pMod->MJacobi)
                             ,GET_NCOLS(pMod->MJacobi)
                             ,pMod->NSta
                             );
            Status = NOT_OKAY;
            return Status;
        } else {

            // Speicher für Inverse Marix anlegen
            //)==================================
            pMod->InvMat    = MatrixCopy(pMod->MJacobi);
            pMod->InvMatOld = MatrixCopy(pMod->MJacobi);
        }

        // Überprüfen VError
        if(  pMod->IntType == DEF_IEULER ) {

            if( pMod->NSta    != GET_NROWS(pMod->VError) ) {
                ErrText.catFormat("Error CDsMod::buildMod: Aus Modell <%s> paßt Länge des ErrorVektors VError <%i> nicht mit StateVektor VSta <%i> überein !!\n"
                                 ,pMod->Name.c_str()
                                 ,GET_NROWS(pMod->VError)
                                 ,pMod->NSta
                                 );
                Status = NOT_OKAY;
                return Status;
          }
        }

        // VSta0 Vektor für alten Zustand anlegen
        if( pMod->IntType == DEF_PIEULER
		  ||pMod->IntType == DEF_IEULER
		  ) {

            pMod->VSta0  = NewVector(pMod->NSta);
            pMod->VStaDer0  = NewVector(pMod->NSta);
            pMod->VStam1 = NewVector(pMod->NSta);
            pMod->VStam2 = NewVector(pMod->NSta);
            pMod->Vdelta = NewVector(pMod->NSta);
        }


        // Namensgebung prüfen (unwichtig)
        //================================
        for(j=0;j<pMod->NSta;j++) {

            //Namensumnennung suchen
            if( (pName=findChangeName("s"
                                     ,pMod->Name.c_str()
                                     ,pMod->VStaName.get_str(j)
                                     ,&change_list
                                     ,n_change_list
                                     )) != 0 ) {

                pMod->VStaName.cpy(pName,j);

            }
        }


    // Euler, Predictor/Korrektor
    //===========================
    } else {


        // Berecich Anlegen
        pMod->pSta    = new SDsModStaInfo[pMod->NSta];


        // Struktur anlegen
        //=================
        for(j=0;j<pMod->NSta;j++) {

            // nächste Struktur holen
            pSta = getRegStaStruct(index,j);

            if( pSta == 0 ) {
                
                Status = NOT_OKAY;
                ErrText.cat("Fehler in CDsMod::buildMod bei Aufruf von CDsMod::getRegStaStruct()"
                            " pSta ist null");
                return Status;
            }
                          

            pMod->pSta[j].Name        = pSta->VarName;
            pMod->pSta[j].pStaVal        = pSta->pStaVal;
            pMod->pSta[j].pStaValDer     = pSta->pStaValDer;
            pMod->pSta[j].pStaValIni     = pSta->pStaValIni;
            pMod->pSta[j].IntType     = pSta->IntType;
            pMod->pSta[j].MaxError    = pSta->MaxError;
            pMod->pSta[j].Comment     = pSta->Comment;

            pMod->pSta[j].ActIterFlag = 0;
    
            //Namensumnennung suchen
            if( (pName=findChangeName("s"
                                     ,pMod->Name.c_str()
                                     ,pMod->pSta[j].Name.c_str()
                                     ,&change_list
                                     ,n_change_list
                                     )) != 0 ) {

                pMod->pSta[j].OldName = pMod->pSta[j].Name;
                pMod->pSta[j].Name    = pName;
            }

            // Anzahl Iteration festlegen
            if( pMod->pSta[j].IntType == DEF_EULER )

                pMod->pSta[j].MaxIter = 0;
            else if( pMod->pSta[j].IntType == DEF_PC1 )

                pMod->pSta[j].MaxIter = 1;
            else if( pMod->pSta[j].IntType == DEF_PC2 )

                pMod->pSta[j].MaxIter = 2;
            else if( pMod->pSta[j].IntType == DEF_PC3 )

                pMod->pSta[j].MaxIter = 3;
            else if( pMod->pSta[j].IntType == DEF_PCN )

                pMod->pSta[j].MaxIter = DS_MOD_MAX_ITERATION;

        }

        // Sta-Struktur prüfen
        //====================

        // default
        pMod->IntType     = DEF_EULER;
        pMod->MaxIter     = 0;
        pMod->IterFlagSum = 0;

        for(j=0;j<pMod->NSta;j++) {

            // maximale Iteration pro Modell
            pMod->MaxIter = MAX(pMod->MaxIter
                                     ,pMod->pSta[j].MaxIter);

            // Anzahl der zu iterierenden States
            if( pMod->pSta[j].MaxIter > 0 )
                pMod->IterFlagSum++;

        }

        // wenn Predictor-Korrektor dann Speicher für Vergangenheit 
        // der Zustandsänderung anlegen
        if( pMod->MaxIter > 0 ) {

            pMod->pStaDerVal0 = new double[pMod->NSta];
            pMod->IntType     = DEF_PCN; // einfach auf die höchter Iterationsmöglichkeit gesetzt
            IterFlagSum++;                  // die zu iterierenden Modell hochzählen


        }
        // auf jedenfalls Speicher für Vergageheit und aktueller Zustand
        pMod->pStaVal0 = new double[pMod->NSta];


    }

	return Status;
}
#else // neue Beschreibung

status_t CDsMod::buildModSta(SDsModInfo *pMod
							,uint16_t index
							,SDsModVarNameChange *change_list
                            ,uint16_t n_change_list) {

	char       *pName;
	uint16_t    j;
	SDsRegSta *pRegSta;

    // States
    //=======
    pMod->pSta        = 0;

    // Struktur holen
    //===============
    pRegSta = getRegStaStruct(index);

    //Anzahl States
    //=============
    pMod->NSta = getRegNSta(index);
	
    // Registrierung überprüfen
    //=========================
    if( pRegSta->Err ) {
        
        Status = NOT_OKAY;
        ErrText.cat("Fehler CDsMod::buildModSta !!!");
        return Status;
    }
    //Liste für andere Integrationsverfahren füllen
	//=============================================
    // Struktur Anlegen
    pMod->pSta  = new SDsModStaInfo;

    if( pMod->NSta == 0 ) {
        
        pMod->pSta->IntType     = DEF_step;
        pMod->pSta->VState      = 0;
        pMod->pSta->VError      = 0;

    } else {

        pMod->pSta->IntType     = pRegSta->IntType;
        pMod->pSta->VState      = pRegSta->VState;
        pMod->pSta->VStaName    = pRegSta->VStaName;
        pMod->pSta->VStaComment = pRegSta->VStaComment;
        pMod->pSta->VError      = VectorCopy(pRegSta->VError);

        // Dt
        if( pRegSta->Dt > 0. ) {

            if( DtModMin > 0.0 ) // ist bereits definiert
                DtModMin = MIN(DtModMin,pRegSta->Dt);
            else // erstdefinition
                DtModMin = pRegSta->Dt;
        }
        // Initialisieren
        //===============
#if DS_MOD_INTEGRATION_PIEULER == 1
        if( pMod->pSta->IntType == DEF_PIEULER ) {
#error def_pieuler muß noch erstellt werden !!!!!!!!!
        } else
#endif
#if DS_MOD_INTEGRATION_PGEARS == 1
        if( pMod->pSta->IntType == DEF_PIEULER ) {
#error def_pieuler muß noch erstellt werden !!!!!!!!!
        } else
#endif
#if DS_MOD_INTEGRATION_IEULER == 1
        if( pMod->pSta->IntType == DEF_IEULER ) {

            SIntegratorBaseInp inp;

            pMod->pSta->pIntFunc = new CIntegratorGEAR(&inp);

            inp.n       = (uint16_t)pMod->NSta;           // Anzahl Zustände
            inp.pclass  = pMod->pFunc;          // Zeiger auf Funktionsklasse
            inp.ystate  = pMod->pSta->VState;   // Zustandsvektor
            inp.itol    = 1;                    // Toleranz als array
            inp.vatol   = pMod->pSta->VError;   // absoluter Fehler
            inp.vrtol   = pMod->pSta->VError;   // relativer Fehler

            inp.gear.meth = 3;

            inp.gear.ijac  = 1;                   // Jacobi-Funktion vorhanden



            Status = pMod->pSta->pIntFunc->init(&inp);

            if( Status != OKAY ) {

                ErrText.cat(pMod->pSta->pIntFunc->getErrText());
                ErrText.cat("Fehler in CDsMod::buildModSta: Problem mit CIntegratorRAD implizit Runge-Kutta\n");
                return Status;
            }
        } else 
#endif
#if DS_MOD_INTEGRATION_GEARS == 1
        if( pMod->pSta->IntType == DEF_GEARS ) {

            SIntegratorBaseInp inp;

            pMod->pSta->pIntFunc = new CIntegratorGEAR(&inp);

            inp.n       = (uint16_t)pMod->NSta;           // Anzahl Zustände
            inp.pclass  = pMod->pFunc;          // Zeiger auf Funktionsklasse
            inp.ystate  = pMod->pSta->VState;   // Zustandsvektor
            inp.itol    = 1;                    // Toleranz als array
            inp.vatol   = pMod->pSta->VError;   // absoluter Fehler
            inp.vrtol   = pMod->pSta->VError;   // relativer Fehler

            inp.gear.meth = 4;

            inp.gear.ijac  = 1;                   // Jacobi-Funktion vorhanden



            Status = pMod->pSta->pIntFunc->init(&inp);

            if( Status != OKAY ) {

                ErrText.cat(pMod->pSta->pIntFunc->getErrText());
                ErrText.cat("Fehler in CDsMod::buildModSta: Problem mit CIntegratorGEAR implizit Runge-Kutta\n");
                return Status;
            }
        } else 
#endif
#if DS_MOD_INTEGRATION_DOPRI45 == 1
        if( pMod->pSta->IntType == DEF_DOPRI45 ) {

            SIntegratorBaseInp inp;

            pMod->pSta->pIntFunc = new CIntegratorERK(&inp);

            inp.n       = (uint16_t)pMod->NSta;
            inp.pclass  = pMod->pFunc;
            inp.ystate  = pMod->pSta->VState;
            inp.itol    = 1;
            inp.vatol   = pMod->pSta->VError;
            inp.vrtol   = pMod->pSta->VError;

            inp.erk.meth = 1;

            Status = pMod->pSta->pIntFunc->init(&inp);

            if( Status != OKAY ) {

                ErrText.cat(pMod->pSta->pIntFunc->getErrText());
                ErrText.cat("Fehler in CDsMod::buildModSta: Problem mit CIntegratorERK expizit Runge-Kutta\n");
                return Status;
            }

        } else 
#endif
#if DS_MOD_INTEGRATION_RADAU == 1
        if( pMod->pSta->IntType == DEF_RADAU ) {

            SIntegratorBaseInp inp;

            pMod->pSta->pIntFunc = new CIntegratorRAD(&inp);

            inp.n       = (uint16_t)pMod->NSta;           // Anzahl Zustände
#if INTEGRATOR_USE_DSMODBASE == 1
            inp.pclass  = pMod->pFunc;          // Zeiger auf Funktionsklasse
#endif
            inp.ystate  = pMod->pSta->VState;   // Zustandsvektor
            inp.itol    = 1;                    // Toleranz als array
            inp.vatol   = pMod->pSta->VError;   // absoluter Fehler
            inp.vrtol   = pMod->pSta->VError;   // relativer Fehler

            inp.rad.ijac  = 1;                   // Jacobi-Funktion vorhanden
            inp.rad.mljac = (uint16_t)pMod->NSta;  // Jacobi vollbesetzt, wenn n
                                                 // ansonsten unteres Band, oeres band muß
                                                 // dann auch definiert werden



            Status = pMod->pSta->pIntFunc->init(&inp);

            if( Status != OKAY ) {

                ErrText.cat(pMod->pSta->pIntFunc->getErrText());
                ErrText.cat("Fehler in CDsMod::buildModSta: Problem mit CIntegratorRAD implizit Runge-Kutta\n");
                return Status;
            }


        } else 
#endif
#if DS_MOD_INTEGRATION_LSODA == 1
        if( pMod->pSta->IntType == DEF_LSODA ) {

            SIntegratorBaseInp inp;

            pMod->pSta->pIntFunc = new CIntegratorLSODA(&inp);

            inp.n       = (uint16_t)pMod->NSta;           // Anzahl Zustände
            inp.pclass  = pMod->pFunc;          // Zeiger auf Funktionsklasse
            inp.ystate  = pMod->pSta->VState;   // Zustandsvektor
            inp.itol    = 1;                    // Toleranz als array
            inp.vatol   = pMod->pSta->VError;   // absoluter Fehler
            inp.vrtol   = pMod->pSta->VError;   // relativer Fehler

            inp.lsoda.ijac  = 1;                   // Jacobi-Funktion vorhanden
            inp.lsoda.mljac = (uint16_t)pMod->NSta;  // Jacobi vollbesetzt, wenn n
                                                   // ansonsten unteres Band, oeres band muß
                                                   // dann auch definiert werden
            inp.lsoda.mxordn = 12;                 // Ordnung nonstiff (adams) max=12
            inp.lsoda.mxords = 5;                  // Ordnung stiff (BDF) max=5


            Status = pMod->pSta->pIntFunc->init(&inp);

            if( Status != OKAY ) {

                ErrText.cat(pMod->pSta->pIntFunc->getErrText());
                ErrText.cat("Fehler in CDsMod::buildModSta: Problem mit CIntegratorRAD implizit Runge-Kutta\n");
                return Status;
            }

        } else 
#endif
        {

            Status = NOT_OKAY;
            ErrText.catFormat("Error CDsMod::buildModSta: Intergrationstyp <%i> in  Modell <%s> nicht programmiert oder über #define in DSProject.h gesetzt !!\n"
                             ,pMod->Name.c_str()
                             );
            return Status;
        }

        // Namensgebung prüfen (unwichtig)
        //================================
        for(j=0;j<pMod->NSta;j++) {

            //Namensumnennung suchen
            if( (pName=findChangeName("s"
                                     ,pMod->Name.c_str()
                                     ,pMod->pSta->VStaName.get_str(j)
                                     ,&change_list
                                     ,n_change_list
                                     )) != 0 ) {

                pMod->pSta->VStaName.cpy(pName,j);

            }
        }


    }

	return Status;
}
#endif // buildModSta
#endif