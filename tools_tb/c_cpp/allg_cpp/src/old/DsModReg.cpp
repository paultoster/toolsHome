#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "DsMod.h"
#include "SlfFkt.h"

//=======================================================
// Registrieren des Modells
//=======================================================
//=======================================================
SDsRegInfo *CDsMod::regModul(CDsModBase *pfunc,char *name/*="__new_name_not__"*/) {

    uint8_t      find_flag = 0;
    SDsRegInfo *p        = pRegListMod;
    uint16_t     ncount = 0;

    // In der Liste suchen
    while(p) {

        ncount++;
        if( p->pFunc == pfunc ) {
            find_flag = 1;
            break;
        }
        p = p->pNext;
    }

    // Wenn nicht gefunden neu anlegen
    if( !find_flag ) {

        ncount++;
        p = new SDsRegInfo;

        // Werte Zuweisen
        p->pInp        = 0;
        p->pOut        = 0;

#if DS_MOD_NEW_STA == 0
        p->pSta        = 0;
#else
        p->Sta.VError      = 0;
        p->Sta.NSta        = 0;
        p->Sta.IntType     = DEF_step;
        p->Sta.Err         = 0;
#endif
        p->pFkt        = 0;
        p->ParSet      = 0;
        
        if( SlfStrCompare(name,"__new_name_not__") )
            p->Name.catFormat("model_%i",ncount);
        else
            p->Name  = name;

        p->pFunc      = pfunc;
        p->pNext      = pRegListMod;
        pRegListMod = p;
    }
    

    return p;
    
}
//=======================================================
// Registrieren einer Funktion
//=======================================================
//=======================================================
SDsRegInfo *CDsMod::regFkt(CDsModBase *pfunc,char *name/*="__new_name_not__"*/) {

    uint8_t      find_flag = 0;
    SDsRegInfo *p        = pRegListFkt;
    uint16_t     ncount = 0;

    // In der Liste suchen
    while(p) {

        ncount++;
        if( p->pFunc == pfunc ) {
            find_flag = 1;
            break;
        }
        p = p->pNext;
    }

    // Wenn nicht gefunden neu anlegen
    if( !find_flag ) {

        ncount++;
        p = new SDsRegInfo;

        // Werte Zuweisen
        p->pInp        = 0;
        p->pOut        = 0;

#if DS_MOD_NEW_STA == 0
        p->pSta        = 0;
#else
        p->Sta.VError      = 0;
        p->Sta.NSta        = 0;
        p->Sta.IntType     = DEF_step;
        p->Sta.Err         = 0;
#endif
        p->ParSet      = 0;
        
        if( SlfStrCompare(name,"__new_name_not__") )
            p->Name.catFormat("fkt_%i",ncount);
        else
            p->Name  = name;

        p->pFunc      = pfunc;
        p->pNext      = pRegListFkt;
        pRegListFkt   = p;
    }
    

    return p;
    
}
//=======================================================
// Registrieren der Inputs
//=======================================================
//=======================================================
void CDsMod::regInp(CDsModBase *pfunc,SDsModVar *pinp,uint16_t ninp) {

    SDsRegInfo *p; 
    SDsRegVar  *pNeu;
    uint16_t     i;

    // Überprüfen, dass Klasse keine Funktion ist, muß Modul sein
    //===========================================================
    if( (p=findRegFkt(pfunc)) ) {
		ErrText.catFormat("CDsMod::regInp: Die Funktion <%s> will Input registrieren, ist aber für Funktionen nicht vorgesehen \n"
						 ,p->Name.c_str() 
						 );

		Status = NOT_OKAY;

        return;
    } 

    // In Modulliste suchen
    //=====================
    p   = regModul(pfunc,"__new_name_not__");
    
    // Liste einsortieren
    for(i=0;i<ninp;i++) {

		if(  SlfStrLen(pinp[i].Name) 
		  || pinp[i].Type != DEF_VOID 
		  ) {

			pNeu = new SDsRegVar;

			// Neue Struktur füllen
			//=====================
			pNeu->VarName = pinp[i].Name;
			pNeu->pVal    = pinp[i].pVal;
			pNeu->Type    = pinp[i].Type;
			pNeu->Unit    = pinp[i].Unit;
			pNeu->Comment = pinp[i].Comment;
			pNeu->pNext   = p->pInp;

			p->pInp = pNeu;
		}
  }
    
}
//=======================================================
// Registrieren der Outputs
//=======================================================
//=======================================================
void CDsMod::regOut(CDsModBase *pfunc,SDsModVar *pout,uint16_t nout) {

    SDsRegInfo *p;
    SDsRegVar  *pNeu;
    uint16_t     i;
  
    // Überprüfen, dass Klasse keine Funktion ist, muß Modul sein
    //===========================================================
    if( (p=findRegFkt(pfunc)) ) {
		ErrText.catFormat("CDsMod::regOut: Die Funktion <%s> will Output registrieren, ist aber für Funktionen nicht vorgesehen \n"
						 ,p->Name.c_str() 
						 );

		Status = NOT_OKAY;

        return;
    } 

    // In Modulliste suchen
    //=====================
    p   = regModul(pfunc,"__new_name_not__");

    for(i=0;i<nout;i++) {

		if(  SlfStrLen(pout[i].Name) 
		  || pout[i].Type != DEF_VOID 
		  ) {
			pNeu = new SDsRegVar;

			// Neue Struktur füllen
			//=====================
			pNeu->VarName = pout[i].Name;
			pNeu->pVal    = pout[i].pVal;
			pNeu->Type    = pout[i].Type;
			pNeu->Unit    = pout[i].Unit;
			pNeu->Comment = pout[i].Comment;
			pNeu->pNext   = p->pOut;

			p->pOut = pNeu;
		}
    }
}
#if DS_MOD_NEW_STA == 0
//=======================================================
// Registrieren der States 
//=======================================================
//=======================================================
void CDsMod::regSta(CDsModBase *pfunc,SDsModSta *psta,uint16_t nsta) {

    SDsRegInfo *p;
    SDsRegSta  *pNeu;
    uint16_t     i;

    // Überprüfen, dass Klasse keine Funktion ist, muß Modul sein
    //===========================================================
    if( (p=findRegFkt(pfunc)) ) {
		ErrText.catFormat("CDsMod::regSta: Die Funktion <%s> will States registrieren, ist aber für Funktionen nicht vorgesehen \n"
						 ,p->Name.c_str() 
						 );

		Status = NOT_OKAY;

        return;
    } 

    // In Modulliste suchen
    //=====================
    p   = regModul(pfunc,"__new_name_not__");

    for(i=0;i<nsta;i++) {

        pNeu = new SDsRegSta;

        // Neue Struktur füllen
        //=====================
        pNeu->VarName     = psta[i].Name;
        pNeu->pStaVal     = psta[i].pStaVal;
        pNeu->pStaValDer  = psta[i].pStaValDer;
        pNeu->pStaValIni  = psta[i].pStaValIni;
        if( psta[i].IntType == DEF_PIEULER ) // ausschliessen
            pNeu->IntType = DEF_EULER;
        else
            pNeu->IntType = psta[i].IntType;
        pNeu->MaxError    = psta[i].MaxError;
        pNeu->Comment     = psta[i].Comment;
        pNeu->VSta        = 0;
        pNeu->VStaDer     = 0;
        pNeu->VStaIni     = 0;
        pNeu->MJacobi     = 0;
        pNeu->pNext       = p->pSta;

        p->pSta           = pNeu;

    }
    
}
//=======================================================
// Registrieren der States Teilimpliziter Euler (PIE) oder impliziter
// Euler (IE)
//=======================================================
//=======================================================
void CDsMod::regSta(CDsModBase *pfunc,enum EModStaType type
                   ,CSlfStrV &vname, CSlfStrV &vcomment
                   ,Vector_t vx,Vector_t vxdot,Vector_t vx0
                   ,Matrix_t mjacobi,uint8_t mjacobi_konst
                   ,Vector_t verr) {

    SDsRegInfo *p;
    SDsRegSta  *pNeu;

    // Überprüfen, dass Klasse keine Funktion ist, muß Modul sein
    //===========================================================
    if( (p=findRegFkt(pfunc)) ) {
		ErrText.catFormat("CDsMod::regSta: Die Funktion <%s> will Input registrieren, ist aber für Funktionen nicht vorgesehen \n"
						 ,p->Name.c_str() 
						 );

		Status = NOT_OKAY;

        return;
    } 

    // In Modulliste suchen
    //=====================
    p   = regModul(pfunc,"__new_name_not__");

    pNeu = new SDsRegSta;

    // Neue Struktur füllen
    //=====================
    pNeu->VStaName    = vname;
    pNeu->VStaComment = vcomment;
    pNeu->VSta        = vx;
    pNeu->VSta        = vx;
    pNeu->VStaDer     = vxdot;
    pNeu->VStaIni     = vx0;
    pNeu->MJacobi     = mjacobi;
    pNeu->FlagMJacobi = mjacobi_konst;
    pNeu->VError      = verr;

    pNeu->VarName     = "";
    pNeu->pStaVal     = 0;
    pNeu->pStaValDer  = 0;
    pNeu->pStaValIni  = 0;
    pNeu->IntType     = type;
    pNeu->MaxError    = 0;
    pNeu->Comment     = "";
    pNeu->pNext       = p->pSta;

    p->pSta           = pNeu;

}
#else
//=======================================================
// Registrieren der States 
//=======================================================
//=======================================================
void CDsMod::regSta(CDsModBase  *pfunc                 // Funktionspointer
                   ,Vector_t      ystate                 // Zustandsvektor
                   ,char        *name[]/*=0*/          // Liste mit Namen (kann auch NULL sein)
                   ,char        *comment[]/*=0*/       // Liste mit Kommentaren
                   ,double      err[]/*=0*/            // Liste mit Fehlergröße
                   ,EModStaType inttype                // Integrationstype siehe DsMOdBase.h
                   ,double      dt/*=-1.*/){

    uint16_t     i;
    SDsRegInfo *p;

    // Überprüfen, dass Klasse keine Funktion ist, muß Modul sein
    //===========================================================
    if( (p=findRegFkt(pfunc)) ) {
		ErrText.catFormat("CDsMod::regSta: Die Funktion <%s> will State registrieren, ist aber für Funktionen nicht vorgesehen \n"
						 ,p->Name.c_str() 
						 );

		Status = NOT_OKAY;

        return;
    } 

    // In Modulliste suchen
    //=====================
    p   = regModul(pfunc,"__new_name_not__");

    if( !ystate || GET_NROWS(ystate) == 0 ) {

        p->Sta.Err = 1;
        Status = NOT_OKAY;
        ErrText.catFormat("In Modul %s ist Vektor ystate nicht koorekt initialisiert",p->Name.c_str());
        return;
    } else {
        p->Sta.Err = 0;
    }

    // Integrationstype / Anzahl Zustände
    //===================================
    p->Sta.IntType = inttype;
    p->Sta.NSta    = GET_NROWS(ystate);
    p->Sta.VState  = ystate;
    
    // Liste mit Namen 
    //================
    p->Sta.VStaName.clear();    
    for(i=0;i<p->Sta.NSta;i++) {
        try {
            p->Sta.VStaName.append(name[i]);
        } catch(...) {
            char d[20];
#if _MSC_VER > MSC_VER_BIS_VS2005
                  sprintf_s(d,20,"sta%i",i);
#else
                  sprintf(d,"sta%i",i);
#endif
            
            p->Sta.VStaName.append(d);
        }
    }

    // Liste mit Kommentar 
    //====================
    p->Sta.VStaComment.clear();    
    for(i=0;i<p->Sta.NSta;i++) {
        try {
            p->Sta.VStaComment.append(name[i]);
        } catch(...) {
            char d[20];
#if _MSC_VER > MSC_VER_BIS_VS2005
            sprintf_s(d,20,"sta%i",i);
#else
            sprintf(d,"sta%i",i);
#endif
            p->Sta.VStaComment.append(d);
        }
    }

    // Liste mit Fehlergröße 
    //======================
    if( p->Sta.VError )
        FreeVector(p->Sta.VError);
    p->Sta.VError = NewVector(p->Sta.NSta);    
    for(i=0;i<p->Sta.NSta;i++) {
        try {
            p->Sta.VError[i] = err[i];
        } catch(...) {

            if( i > 0 )
                p->Sta.VError[i] = p->Sta.VError[i-1];
            else
                p->Sta.VError[i] = 1e-3;
        }
    }

    // Dt initialisieren (default -1. heißt Dt von min(übergeordnet,andere Module) benutzen)
    //======================================================================================
    p->Sta.Dt = dt;
    
}
#endif
//=======================================================
// Registrieren der Parameter
//=======================================================
//=======================================================
void CDsMod::regPar(CDsModBase *pfunc,SDsModParVar *parlist,uint16_t npar,uint8_t nodefault/*=0*/) {

    SDsRegInfo *p;

    // Überprüfen, dass Klasse Funktion oder Modul ist
    //================================================
    p=findRegFkt(pfunc);

    // In Modulliste suchen, wenn keine Funktion
    //==========================================
    if( !p ) p   = regModul(pfunc,"__new_name_not__");
    
    if( ParSet ) {

        pPar->set(p->Name.c_str(),(SDsParVar *)parlist,npar);

        p->ParSet = 1;

        if( nodefault )
            pPar->setNoDefault(p->Name.c_str());
    
    } else {

        ErrText.catFormat("CDsMod::regPar: Das Modul <%s> will Parameter setzen. \n"
                          "Parameterfunktion ist aber im Konstruktor nicht gesetzt !!!\n"
                         ,p->Name.c_str()
                         );
        Status = NOT_OKAY;
    }
}
//=======================================================
// Registrieren der Parameter 1D-Tabelle
//=======================================================
//=======================================================
void CDsMod::regPar(CDsModBase *pfunc,SDsModParTab *parlist,uint16_t npar,uint8_t nodefault/*=0*/) {

    SDsRegInfo *p;

    // Überprüfen, dass Klasse Funktion oder Modul ist
    //================================================
    p=findRegFkt(pfunc);

    // In Modulliste suchen, wenn keine Funktion
    //==========================================
    if( !p ) p   = regModul(pfunc,"__new_name_not__");
    
    if( ParSet ) {

        pPar->set(p->Name.c_str(),(SDsParTab *)parlist,npar);

        p->ParSet = 1;
    
        if( nodefault )
            pPar->setNoDefault(p->Name.c_str());

    } else {

        ErrText.catFormat("CDsMod::regPar(Tab): Das Modul <%s> will Parameter setzen. \n"
                          "Parameterfunktion ist aber im Konstruktor nicht gesetzt !!!\n"
                         ,p->Name.c_str()
                         );
        Status = NOT_OKAY;
    }
}
//=======================================================
// Parameter, Asuwahl zwischen Gruppen
//=======================================================
//=======================================================
void CDsMod::regParDecideGroup(CDsModBase *pfunc,char *group,CSlfStrV &subgrouplist,char *switchpar,char *defvalue,uint8_t nodefault/*=0*/) {

    SDsRegInfo *p;

    // Überprüfen, dass Klasse Funktion oder Modul ist
    //================================================
    p=findRegFkt(pfunc);

    // In Modulliste suchen, wenn keine Funktion
    //==========================================
    if( !p ) p   = regModul(pfunc,"__new_name_not__");
    
    if( ParSet ) {


		if( subgrouplist.getNrows() == 0 ) {
			ErrText.catFormat("CDsMod::regParDecideGroup: Das Modul <%s> will für die Gruppe <%s> ein Auswahl von Gruppenparameter setzen. \n"
							  "Die Liste ist leer  !!!\n"
							 ,p->Name.c_str() 
							 ,group
							 );

			Status = NOT_OKAY;
			return;
		}
		if( subgrouplist.isAnyIdentical() ) {
			ErrText.catFormat("CDsMod::regParDecideGroup: Das Modul <%s> will für die Gruppe <%s> ein Auswahl von Gruppenparameter setzen. \n"
							  "In der Liste darf kein identische Auswahl vorhanden sein  !!!\n"
							 ,p->Name.c_str() 
							 ,group
							 );
			ErrText.cat("subgroulist=");
			for(uint16_t i=0;i<subgrouplist.getNrows();i++)
				ErrText.catFormat("%s/",subgrouplist.get_str(i));
			ErrText.cat("\n");


			Status = NOT_OKAY;
		}

        if( nodefault )
            pPar->decideGroup(p->Name.c_str(),group,subgrouplist,switchpar,"");
        else
            pPar->decideGroup(p->Name.c_str(),group,subgrouplist,switchpar,defvalue);

    } else {

        ErrText.catFormat("CDsMod::regParDecideGroup: Das Modul <%s> will Parameter setzen. \n"
                          "Parameterfunktion ist aber im Konstruktor nicht gesetzt !!!\n"
                         ,p->Name.c_str()
                         );
        Status = NOT_OKAY;
    }

}
//=======================================================
// Registrieren der Funktionen
//=======================================================
//=======================================================
void CDsMod::regPointerToFkt(CDsModBase *pfunc,SDsModFkt *pfkt,uint16_t nfkt) {

    SDsRegInfo *p;
    SDsRegFkt  *pNeu;
    uint16_t     i;
  
    // Überprüfen, dass Klasse keine Funktion ist, muß Modul sein
    //===========================================================
    if( (p=findRegFkt(pfunc)) ) {
		ErrText.catFormat("CDsMod::regOut: Die Funktion <%s> will Output registrieren, ist aber für Funktionen nicht vorgesehen \n"
						 ,p->Name.c_str() 
						 );

		Status = NOT_OKAY;

        return;
    } 

    // In Modulliste suchen
    //=====================
    p   = regModul(pfunc,"__new_name_not__");

    for(i=0;i<nfkt;i++) {

		if(  SlfStrLen(pfkt[i].Name) 
		  ) {
			pNeu = new SDsRegFkt;

			// Neue Struktur füllen
			//=====================
			pNeu->VarName = pfkt[i].Name;
			pNeu->ppFkt    = pfkt[i].ppFkt;
			pNeu->Comment = pfkt[i].Comment;
			pNeu->pNext   = p->pFkt;

			p->pFkt = pNeu;
		}
    }
}
//=======================================================
// Anzahl der registrierten Modelle
//=======================================================
//=======================================================
uint16_t CDsMod::getRegNMod(void) {

    SDsRegInfo *p        = pRegListMod;
    uint16_t     nmod=0;

    // In der Liste suchen
    while(p) {

        nmod++;
        p = p->pNext;
    }
    return nmod;
}
//=======================================================
// Anzahl der registrierten Funktionen
//=======================================================
//=======================================================
uint16_t CDsMod::getRegNFkt(void) {

    SDsRegInfo *p        = pRegListFkt;
    uint16_t     nfkt=0;

    // In der Liste suchen
    while(p) {

        nfkt++;
        p = p->pNext;
    }
    return nfkt;
}
//=======================================================
// Namen der registrierten Modelle und Funktionen überprüfen
// Sie dürfen nicht doppelt vorkommen
//=======================================================
//=======================================================
status_t CDsMod::checkRegModName(void) {

    SDsRegInfo *p0        = pRegListMod;
	SDsRegInfo *p1;
    uint16_t     imod0=0,imod1;

    // Modell-Liste durchgehen
    while(p0) {

        // In Modellliste suchen
        p1    = pRegListMod;
		imod1 = 0;
		while(p1) {

			if(  imod0 != imod1
			  && SlfStrCompare(p0->Name,p1->Name)
			  ) {

				ErrText.catFormat("CDsMod::checkRegModName error: Der Name <%s> des Modells wird zwei mal benutzt"
					             ,p0->Name.c_str()
								 );
				Status = NOT_OKAY;
				return Status;
			}
			p1 = p1->pNext;
			imod1++;
		}

        // In Funktionsliste suchen
        p1    = pRegListFkt;
		while(p1) {

			if(  SlfStrCompare(p0->Name,p1->Name)
			  ) {

				ErrText.catFormat("CDsMod::checkRegModName error: Der Modellname <%s> wird auch in einer Fkt benutzt"
					             ,p0->Name.c_str()
								 );
				Status = NOT_OKAY;
				return Status;
			}
			p1 = p1->pNext;
		}
        
		p0 = p0->pNext;
		imod0++;
    }

    // Funktions-Liste durchgehen
    p0    = pRegListFkt;
    imod0 = 0;
    while(p0) {

        // In Modellliste suchen
        p1    = pRegListFkt;
		imod1 = 0;
		while(p1) {

			if(  imod0 != imod1
			  && SlfStrCompare(p0->Name,p1->Name)
			  ) {

				ErrText.catFormat("CDsMod::checkRegModName error: Der Name <%s> der Funktion wird zwei mal benutzt"
					             ,p0->Name.c_str()
								 );
				Status = NOT_OKAY;
				return Status;
			}
			p1 = p1->pNext;
			imod1++;
		}
        
		p0 = p0->pNext;
		imod0++;
    }

    return OKAY;
}
//=======================================================
// Funktionspointer Modul
//=======================================================
//=======================================================
CDsModBase  *CDsMod::getRegModPointer(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    uint16_t     imod = 0;

    // In der Liste suchen
    while(p) {

        if( imod == ifunc )
            return p->pFunc;
      
        imod++;
        p = p->pNext;
    }
    return 0;
}
//=======================================================
// Funktionspointer Funktion
//=======================================================
//=======================================================
CDsModBase  *CDsMod::getRegFktPointer(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListFkt;
    uint16_t     ifkt = 0;

    // In der Liste suchen
    while(p) {

        if( ifkt == ifunc )
            return p->pFunc;
      
        ifkt++;
        p = p->pNext;
    }
    return 0;
}
//=======================================================
// Modellname
//=======================================================
//=======================================================
char *CDsMod::getRegModName(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    uint16_t     imod = 0;

    // In der Liste suchen
    while(p) {

        if( imod == ifunc )
            return p->Name.c_str();
      
        imod++;
        p = p->pNext;
    }
    return 0;
}
//=======================================================
// Funktionsname
//=======================================================
//=======================================================
char *CDsMod::getRegFktName(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListFkt;
    uint16_t     ifkt = 0;

    // In der Liste suchen
    while(p) {

        if( ifkt == ifunc )
            return p->Name.c_str();
      
        ifkt++;
        p = p->pNext;
    }
    return 0;
}
//=======================================================
// Anzahl Inputs
//=======================================================
//=======================================================
uint16_t CDsMod::getRegNInp(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegVar  *pi;
    uint16_t     k = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        pi = p->pInp;

    k = 0;
    while(pi) {

        k++;
        pi = pi->pNext;
    }
    return k;
}
//=======================================================
// j. Inputstruktur der i. Funktion
//=======================================================
//=======================================================
SDsRegVar *CDsMod::getRegInpStruct(uint16_t ifunc,uint16_t jinp) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegVar  *pi;
    uint16_t     k = 0;

    // In der Modell-Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        pi = p->pInp;

    // In der Input-Liste suchen
    k = 0;
    while(pi) {

        if( k == jinp )
            return pi;

        k++;
        pi = pi->pNext;
    }
    return 0;
}
//=======================================================
// Anzahl Zustände
//=======================================================
//=======================================================
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
uint16_t CDsMod::getRegNSta(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegSta  *ps;
    uint16_t     k = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        ps = p->pSta;

    k = 0;
    while(ps) {

        if(  ps->IntType == DEF_PIEULER 
          || ps->IntType == DEF_IEULER
            )
            return (uint16_t)GET_NROWS(ps->VSta);
        k++;
        ps = ps->pNext;
    }
    return k;
}
#else
uint32_t CDsMod::getRegNSta(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    uint16_t     k = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
 
    return p->Sta.NSta;
}
#endif
//=======================================================
// Zustandkombinationen prüfen
// Euler, Predictor-Corrector können
// nicht mit teilimplizit und implizien Euler kombiniert e´werden
// return 0 : nicht okay
// return 1 : ist okay
//=======================================================
//=======================================================
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
uint8_t CDsMod::proofRegStaTypes(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegSta  *ps;
    uint16_t     k = 0;

    uint8_t flag_type1 = 0;
    uint8_t flag_type2 = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        ps = p->pSta;

    while(ps) {

        if(  ps->IntType == DEF_EULER
          || ps->IntType == DEF_PC1
          || ps->IntType == DEF_PC2
          || ps->IntType == DEF_PC3
          || ps->IntType == DEF_PCN
          )
          flag_type1 = 1;
        
        else if(  ps->IntType == DEF_PIEULER 
               || ps->IntType == DEF_IEULER
               )
          flag_type2 = 1;

        ps = ps->pNext;
    }

    if( flag_type1 && flag_type2 )
        return 0;
    else
        return 1;
}
#else
#endif
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
//=======================================================
// Typ Zustände (gibt Typ des ersten Zustands zurück,
// entweder (PIR oder IE, ansonsten gibt es eine ganz Liste)
//=======================================================
//=======================================================
enum EModStaType CDsMod::getRegStaType(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegSta  *ps;
    uint16_t     k = 0;

    uint8_t flag_type1 = 0;
    uint8_t flag_type2 = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return DEF_step;
    else
        ps = p->pSta;

    if( ps )
        return ps->IntType;
    else
        return DEF_step;
}
#endif
//=======================================================
// j. Zustandsstruktur der i. Funktion
//=======================================================
//=======================================================
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
SDsRegSta *CDsMod::getRegStaStruct(uint16_t ifunc,uint16_t jsta) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegSta  *ps;
    uint16_t     k = 0;

    // In der Modell-Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        ps = p->pSta;

    // In der Input-Liste suchen
    k = 0;
    while(ps) {

        if( k == jsta )
            return ps;

        k++;
        ps = ps->pNext;
    }
    return 0;
}
#else
SDsRegSta *CDsMod::getRegStaStruct(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    uint16_t     k = 0;

    // In der Modell-Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        return &(p->Sta);

}
#endif
//=======================================================
// Anzahl Outputs
//=======================================================
//=======================================================
uint16_t CDsMod::getRegNOut(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegVar  *po;
    uint16_t     k = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        po = p->pOut;

    k = 0;
    while(po) {

        k++;
        po = po->pNext;
    }
    return k;
}
//=======================================================
// j. Outputstruktur der i. Funktion
//=======================================================
//=======================================================
SDsRegVar *CDsMod::getRegOutStruct(uint16_t ifunc,uint16_t jout) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegVar  *po;
    uint16_t     k = 0;

    // In der Modell-Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        po = p->pOut;

    // In der Input-Liste suchen
    k = 0;
    while(po) {

        if( k == jout )
            return po;

        k++;
        po = po->pNext;
    }
    return 0;
}
//=======================================================
// Anzahl Funktionspointer
//=======================================================
//=======================================================
uint16_t CDsMod::getRegNFkt(uint16_t ifunc) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegFkt  *pf;
    uint16_t     k = 0;

    // In der Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        pf = p->pFkt;

    k = 0;
    while(pf) {

        k++;
        pf = pf->pNext;
    }
    return k;
}
//=======================================================
// j. Inputstruktur der i. Funktion
//=======================================================
//=======================================================
SDsRegFkt *CDsMod::getRegFktStruct(uint16_t ifunc,uint16_t jfkt) {

    SDsRegInfo *p   = pRegListMod;
    SDsRegFkt  *pf;
    uint16_t     k = 0;

    // In der Modell-Liste suchen
    while(p) {

        if( k == ifunc )
            break;
      
        k++;
        p = p->pNext;
    }
    if( p == 0 )
        return 0;
    else
        pf = p->pFkt;

    // In der Input-Liste suchen
    k = 0;
    while(pf) {

        if( k == jfkt )
            return pf;

        k++;
        pf = pf->pNext;
    }
    return 0;
}
//=======================================================
// Anmeldeliste zerstören
//=======================================================
//=======================================================
#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
void CDsMod::destroyRegMod(void) {

    SDsRegInfo *p;
    SDsRegVar  *po;
    SDsRegVar  *pi;
    SDsRegSta  *ps;
    SDsRegVar  *pv;
    SDsRegSta  *psv;
    SDsRegFkt  *pf,*pf1;
    uint16_t     k = 0;

    // Modell-Liste nacheinander zerstören
    while(pRegListMod) {

        p = pRegListMod->pNext; // Merker

        // Inputs zerstören
        pi = pRegListMod->pInp;

        while(pi) {

            pv = pi->pNext; // Merker
            delete pi;
            pi = pv;
        }
        // Zustände zerstören
        ps = pRegListMod->pSta;

        while(ps) {

            psv = ps->pNext; // Merker
            delete ps;
            ps = psv;
        }
        // Outputs zerstören
        po = pRegListMod->pOut;

        while(po) {

            pv = po->pNext; // Merker
            delete po;
            po = pv;
        }

        // Parameter löschen
        if( pRegListMod->ParSet )
            pPar->deleteModul(pRegListMod->Name.c_str());

        // Fkt-Pointer zerstören
        pf = pRegListMod->pFkt;

        while(pf) {

            pf1 = pf->pNext; // Merker
            delete pf;
            pf = pf1;
        }

        // Schlussendlich Modellstruct zerstören 
        delete pRegListMod;
        pRegListMod = p;
    }
}
#else
void CDsMod::destroyRegMod(void) {

    SDsRegInfo *p;
    SDsRegVar  *po;
    SDsRegVar  *pi;
    SDsRegVar  *pv;
    SDsRegFkt  *pf,*pf1;
    uint16_t     k = 0;

    // Modell-Liste nacheinander zerstören
    while(pRegListMod) {

        p = pRegListMod->pNext; // Merker

        // Inputs zerstören
        pi = pRegListMod->pInp;

        while(pi) {

            pv = pi->pNext; // Merker
            delete pi;
            pi = pv;
        }

        // Fehlergrößen der Zustände zerstören
        if( pRegListMod->Sta.VError )
            FreeVector(pRegListMod->Sta.VError);

        // Outputs zerstören
        po = pRegListMod->pOut;

        while(po) {

            pv = po->pNext; // Merker
            delete po;
            po = pv;
        }

        // Parameter löschen
        if( pRegListMod->ParSet )
            pPar->deleteModul(pRegListMod->Name.c_str());
        
        // Fkt-Pointer zerstören
        pf = pRegListMod->pFkt;

        while(pf) {

            pf1 = pf->pNext; // Merker
            delete pf;
            pf = pf1;
        }

        // Schlussendlich Modellstruct zerstören 
        delete pRegListMod;
        pRegListMod = p;
    }
}
#endif
void CDsMod::destroyRegFkt(void) {

    SDsRegInfo *p;

    // Modell-Liste nacheinander zerstören
    while(pRegListFkt) {

        p = pRegListFkt->pNext; // Merker


        // Parameter löschen
        if( pRegListFkt->ParSet )
            pPar->deleteModul(pRegListFkt->Name.c_str());
        

        // Schlussendlich Modellstruct zerstören 
        delete pRegListFkt;
        pRegListFkt = p;
    }
}

//=======================================================
// Suche Funktionspointer in registrierter Funktionsliste
//=======================================================
//=======================================================
SDsRegInfo *CDsMod::findRegFkt(CDsModBase *pfunc) {

    SDsRegInfo *p   = pRegListFkt;

    // In der Funktions-Liste suchen
    while(p) {

        if( p->pFunc == pfunc )
            return p;
        p = p->pNext;
    }
    return 0;
}
