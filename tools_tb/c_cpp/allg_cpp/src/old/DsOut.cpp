#include <stdarg.h>

#include "DsOut.h"
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfNum.h"

    
// Konstruktor CDsOut
CDsOut::CDsOut() {

    Status  = OKAY;
    ErrText = "";

    pLogFile  = 0;             // Logfilehandling
    LogFileSet = 0;            // Sagt ob LogFile geschrieben wird

    pDSOutSetVar = 0;
}
// Konstruktor CDsOut
CDsOut::CDsOut(CSlfLogFile *plogfile) {

    Status  = OKAY;
    ErrText = "";

    if( plogfile == 0 ) { // kein Logfile benutzen

        pLogFile   = 0;
        LogFileSet = 0;

    } else { // Logfile anlegen

        pLogFile   = plogfile;
        LogFileSet = 1;
    }

    pDSOutSetVar = 0;

}
// Destruktor 
CDsOut::~CDsOut() {

    setVarDelete();

}
status_t CDsOut::ini(CSlfLogFile *plogfile/*=0*/) {


    resetErrText();

    Status        = OKAY;

    if( plogfile ) { 

        pLogFile   = plogfile;
        LogFileSet = 1;
    }

    if( pDSOutSetVar )
      setVarDelete();

    return OKAY;
}
#if DS_OUT_USE_MEXWRITE != 0
status_t CDsOut::getMex(const mxArray *par,uint16_t npar)
{
}
#endif
void CDsOut::reset(void) {

    Status = OKAY;
    ErrText = "";
    setVarDelete();

}
status_t CDsOut::done(void) {


    reset();

    return Status;
}
// Logfile schreiben
//=======================
void CDsOut::writeLogFile(char *name) {

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

            resetErrText();
        }
    }
}
//========================================================
// Output-Struktur setzen
// Zuerst die Struktur setzen bevor andere write/get-Funktion
//========================================================
status_t CDsOut::set(char *mod_name,SDsVar *var,uint16_t nvar) {
    
  SDsOutSetVar *pNeu;

  uint16_t i;

  for(i=0;i<npar;i++) {

    // Zuweisung
    switch( var[i].Type)
    {
    case DEF_VOID:
      break;
    case DEF_VEC:
      {
        Vector vec = var[i].pVal;

      pNeu = new SDsOutSetVar;

      // Neue Struktur füllen
      //=====================
      pNeu->ModName   = mod_name;
      pNeu->VarName   = var[i].Name;
      pNeu->pVal      = var[i].pVal;
      pNeu->NRow      = GET_NROWS(vec);
      pNeu->NCol      = GET_NCOLS(vec);
      pNeu->NVal      = pNeu->NRow*pNeu->NCol;
      pNeu->Type      = var[i].Type;
      pNeu->Unit      = var[i].Unit;
      pNeu->Comment   = var[i].Comment;
      pNeu->pNext     = pDSOutSetVar;

      pDSOutSetVar = pNeu;
    }
      break;
    default:

        ErrText.catFormat("CDsOut::set: mod_name:<%s>, var_name: <%s>, has wrong type: <%>\n"
                         ,mod_name, var[i].Name, VarTypeStr[var[i].Type]);
        return NOT_OKAY;

    } 

  }
  
  return OKAY;
}
#if DS_OUT_USE_MEXWRITE != 0
//========================================================
// Output-Struktur an MAtlab einrichten
// Zuerst die Struktur setzen bevor andere write/get-Funktion
//========================================================
    
// Output-Struktur für Matlab einrichten
status_t CDsOut::getMex(const mxArray *out,uintt16 nnout)
{


}

#endif

void CDsOut::setVarDelete(void) {

    SDsOutSetVar *pvar;
    
    while( pDSOutSetVar ) {

        pvar = pDSOutSetVar->pNext;

        if( pDSOutSetVar->Type == DEF_VEC )
            deleteVec(pDSOutSetVar);


        delete pDSOutSetVar;
        pDSOutSetVar = pvar;
    }
}
void CDsOut::deleteModul(char *mod_name) {

    SDsOutSetVar *p = pDSOutSetVar;
    SDsOutSetVar *pvar;
    
    while( p ) {

        if( p->ModName == mod_name ) {

            pvar = p->pNext;

            if( pDSOutSetVar->Type == DEF_VEC )
                deleteVec(p);

            delete p;

            if( p == pDSOutSetVar )
                pDSOutSetVar = pvar;

            p = pvar;
        
        } else {
        
            p = p->pNext;
        }
    }
}
void CDsOut::deleteVec(SDsOutSetVar *pvar) {

        // Hier geildeter Vektorvariable
        //===============================
        if(  pDSOutSetVar->VecBuild ) {

            Vector **ppvec = (Vector **) pvar->pVal;
            if(  pvar->VecSet 
              && **ppvec != 0 
              )
                FreeVector(**ppvec);
            
            delete *ppvec;

        } else
        // selbsterstellter Vektor löschen
        if(  pDSOutSetVar->VecSet 
          && *(Vector *)pDSOutSetVar->pVal != 0
          ) {
            FreeVector(*(Vector *)pDSOutSetVar->pVal);
        }
}
