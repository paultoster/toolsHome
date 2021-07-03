#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define SLFBASIC_VAR_TYPE_STR
#include "DsCtrlIO.h"
#include "SlfFkt.h"

#define IS_PARAM_DOUBLE(pVal) (mxIsNumeric(pVal) && !mxIsLogical(pVal) &&\
!mxIsEmpty(pVal) && !mxIsSparse(pVal) && !mxIsComplex(pVal) && mxIsDouble(pVal))


CDsCtrlIO::CDsCtrlIO() {

    Status = OKAY;

    // Liste externer Ein- u. Ausgaben
    pIOOut = 0;
    pIOInp = 0;

    UseInputTimeStruct   = 0;
    index0TimeStruct     = 0;
    index1TimeStruct     = 0;
    factor0TimeStruct    = 0.0;
    factor1TimeStruct    = 0.0;

    InputProofedFlag = 0;

    //Schlüsselwörter für Mexinput
    TYPE       = "type";
    NAME       = "name";
    VAL        = "val";
    UNIT       = "unit";
    COMMENT    = "comment";

    XNAME      = "xname";
    XVEC       = "xvec";
    XUNIT      = "xunit";
    XCOMMENT   = "xcomment";
    YNAME      = "yname";
    YVEC       = "yvec";
    YUNIT      = "yunit";
    YCOMMENT   = "ycomment";
    ZNAME      = "zname";
    ZMAT       = "zmat";
    ZUNIT      = "zunit";
    ZCOMMENT   = "zcomment";
    ORDER      = "order";

    TYPE_SINGLE[0] = "single";
    TYPE_SINGLE[1] = "scalar";
    TYPE_STRING    = "string";
    TYPE_VECTOR    = "vector";
    TYPE_MATRIX    = "matrix";
    TYPE_1DTAB[0]  = "1dtable";
    TYPE_1DTAB[1]  = "table1d";

}
// Destruktor ===============================================
//===========================================================
CDsCtrlIO::~CDsCtrlIO() {

    Status = OKAY;

    deleteOutput();
    deleteInput();
}
void CDsCtrlIO::reset(void) {

    deleteOutput();
    deleteInput();
}
//===========================================================
//===========================================================
// Output-Funktionen ========================================
//===========================================================
//===========================================================
//
// Erstellt ausDsCtrl-Klasse die angebotenen Ausgaben
//===================================================
status_t CDsCtrlIO::buildOutputVector(SDsModVar *output
                                     ,uint32_t nval
                                     ,uint32_t nout) {

    uint32_t i;

    for(i=0;i<nval;i++) {

        if( buildOutputVector(output[i].Name
                             ,output[i].Unit
                             ,output[i].Comment
                             ,output[i].Type
                             ,output[i].pVal
                             ,nout) != OKAY )
            return Status;
    }

    return Status;
}
// Erstellt ausDsCtrl-Klasse die angebotenen Ausgaben
//===================================================
status_t CDsCtrlIO::buildOutputVector(char *name
                                     ,char *unit
                                     ,char *comment
                                     ,EVarType type
                                     ,void *pval
                                     ,uint32_t nout) {

    SDsCtrlIOOut     *pO;

    if( !name ) {

        ErrText.catFormat("CDsCtrlIO::buildOutputVector: Variablenname ist nicht angegeben !!\n");
        Status = NOT_OKAY;
        return Status;
    }
    if( !unit    ) unit    = "-";
    if( !comment ) comment = "";
    if( !pval ) {

        ErrText.catFormat("CDsCtrlIO::buildOutputVector: Pointer Variable <%s> ist NULL !!\n"
                         ,name);
        Status = NOT_OKAY;
        return Status;
    }

    // Name überprüfen
    //================
    pO = pIOOut;
    while( pO ) {

        if( pO->Name != name ) {
            pO = pO->pNext;
        } else{
            break;
        }
    }
    if( !pO ) {

        pO = new SDsCtrlIOOut;

        pO->Name        = name;
        pO->Unit        = unit;
        pO->TypeGet     = type;
        pO->pValGet     = pval;
    

        pO->RefType     = DEF_ARR_DOUBLE;
        pO->NDim        = nout;

        pO->Type        = DEF_VOID;
        pO->pVal        = 0;

        pO->pNext       = pIOOut;
        pIOOut          = pO;
    }

    return Status;
}
// bildet die zur Schnittstelle entsprechende Outputs
//===================================================
#if DS_CTRL_IO_MEX
status_t CDsCtrlIO::setMexOutputVector(mxArray **ppout) {

    uint32_t nout=0;
    uint32_t iout;
    SDsCtrlIOOut     *pO;
	mxArray          *pmxtmp;

    //Outputs zählen
    pO = pIOOut;
    while( pO ) {
        nout++;
        pO = pO->pNext;
    }

    // MAtlab-Strukturvariable generieren und Namen übergeben 
    //=======================================================
    if( nout > 0 ) {
        
        const unsigned int   dims[] = {1,1};
        char  **ppnameliste;

        // Für Namenslsite Speicher anlegen
        ppnameliste = new char *[nout];

        // Namen zuweisen
        pO   = pIOOut;
        iout = 0;
        while( pO ) {

            ppnameliste[iout++] = pO->Name.c_str();
            pO                  = pO->pNext;
        }

        // Struktur für MAtlab anlegen
        *ppout = mxCreateStructArray(2,dims,nout,(const char **)ppnameliste);

        delete []ppnameliste;

    } else {

        return OKAY;
    }

    // Outputliste auswerten und Speicher generieren und Matlab-Struktur 
    // zuordenen                                                         
    //===================================================================
    pO   = pIOOut;
    iout = 0;
    while( pO ) {

		/* Aufteilung nach typ */
		switch(pO->RefType) {

			case DEF_DOUBLE: /* Einzelwert double */
                {        
                    const unsigned int dims[] = {1,1};

                    /* double anlegen */
                    pmxtmp  = mxCreateNumericArray(2,dims, mxDOUBLE_CLASS,
                                                    mxREAL);
                    pO->pVal = (void *)mxGetPr(pmxtmp);
                }
                mxSetFieldByNumber(*ppout,0,iout,pmxtmp);

                pO->Type = DEF_DOUBLE;
				break;
				
			case DEF_ARR_DOUBLE: /* Vector_t double */
                {        
                    /* Länge des Vektors */
                    const unsigned int dims[2] = {pO->NDim,1};


                    /* array anlegen */
                    pmxtmp  = mxCreateNumericArray(2,dims, mxDOUBLE_CLASS,
                                                    mxREAL);
                
                    /* array-ptr zuordnen */
                    pO->pVal = (void *)mxGetPr(pmxtmp);

                    pO->Type = DEF_ARR_DOUBLE;
                }
                mxSetFieldByNumber(*ppout,0,iout,pmxtmp);
				break;
			
			default:
                ErrText.cat("Fehler CDsCtrlIO::setMexOutputVector(mxArray **)\n");
                ErrText.catFormat("Ausgabetyp für mex-Output <%s> ist nicht implementiert\n",VarTypeStr[pO->RefType]);
                Status = NOT_OKAY;
                return Status;
		}

        pO       = pO->pNext;
        ++iout;

	}

    return Status;
}
#endif
// Output zum aktuellen index setzen
//=========================================
void CDsCtrlIO::setOutput(uint32_t iactout) {

    SDsCtrlIOOut *pO = pIOOut;

    while( pO ) {

	    switch(pO->Type) {

        case DEF_DOUBLE:

            SlfFktConvertSingle(pO->pValGet
                               ,pO->TypeGet
                               ,pO->pVal
                               ,pO->Type
                               ,1.0
                               ,0.0);

            break;
        case DEF_ARR_DOUBLE:
          switch(pO->TypeGet) {
            case DEF_DOUBLE:
            case DEF_FLOAT:
            case DEF_SIGNED_LONG:
            case DEF_UNSIGNED_LONG:
            case DEF_SIGNED_SHORT:
            case DEF_UNSIGNED_SHORT:
            case DEF_SIGNED_CHAR:
            case DEF_UNSIGNED_CHAR:


              SlfFktConvertSingleToVector(pO->pValGet
                                         ,pO->TypeGet
                                         ,pO->pVal
                                         ,pO->Type
                                         ,iactout
                                         ,1.0
                                         ,0.0);

              break;
            case DEF_VEC:
              Vector_t *pvec = (Vector_t *)pO->pValGet;            
              Vector_t vec   = *pvec;
              SlfFktConvertSingleToVector((void *)(&vec[iactout])
                                         ,DEF_DOUBLE
                                         ,pO->pVal
                                         ,pO->Type
                                         ,iactout
                                         ,1.0
                                         ,0.0);
              break;
          }
        }

        pO = pO->pNext;
    }    
}
// Löscht Outputstruktur
//=======================================================
void CDsCtrlIO::deleteOutput() {

    SDsCtrlIOOut  *pO;
    
    while( pIOOut != 0 ) {

        pO = pIOOut->pNext;
        delete pIOOut;
        pIOOut = pO;
    }
}
//===========================================================
// Input-Funktionen =========================================
//===========================================================
//===========================================================
// Erstellt aus einer SDsModVarInfo die gewünschten Inputs
//==========================================================
status_t CDsCtrlIO::buildInput(SDsModVarInfo *pinp) {

    SDsCtrlIOInp *pI;
    InputProofedFlag = 0;

    pI = buildInputStruct(pinp->Name);
    if( !pI ) return Status;

    switch(pinp->Type) {
    case DEF_DOUBLE:
    case DEF_FLOAT:
    case DEF_SIGNED_LONG:
    case DEF_UNSIGNED_LONG:
    case DEF_SIGNED_SHORT:
    case DEF_UNSIGNED_SHORT:
    case DEF_SIGNED_CHAR:
    case DEF_UNSIGNED_CHAR:
            pI->Name          = pinp->Name;
            pI->UnitIntern    = pinp->Unit;
            pI->TypeIntern    = pinp->Type;
            pI->pValIntern    = pinp->pVal;
            pI->CommentIntern = pinp->Comment;
        break;
    case DEF_VEC:
            pI->Name          = pinp->Name;
            pI->UnitIntern    = pinp->Unit;
            pI->TypeIntern    = pinp->Type;
            pI->pValIntern    = pinp->pVal;
            pI->CommentIntern = pinp->Comment;

            // Ist Vector_t schon gebildet
            if( *(Vector_t *)pI->pValIntern ) {
                
                pI->NColIntern           = 1;
                pI->NRowIntern           = GET_NDIMS(*(Vector_t *)pI->pValIntern);
                pI->NDimIntern           = pI->NRowIntern;
            } else {
                pI->BuildInternVecByExternSize = 1; // Vector_t wird nach Get-Größe gebildet
            }

            break;
    case DEF_STRING:
            pI->Name          = pinp->Name;
            pI->TypeIntern    = pinp->Type;
            pI->pValIntern    = pinp->pVal;
            pI->CommentIntern = pinp->Comment;
        break;
    default:
            ErrText.catFormat("Fehler CDsCtrlIO::regInpt() Der <%s> ist noch nicht programmiert\n"
                       ,VarTypeStr[pI->TypeIntern]);
            Status = NOT_OKAY;
            return Status;
    }
    return Status;
}
//===========================================================
// Die Zeit struktur InputTimeStruct wird aktiviert, da von aussen mit einem Zeitvektor belegt wird
// Name und Einheit wird gesetzt
//==========================================================
status_t CDsCtrlIO::setInputTimeStruct(char *name,char *unit, char *comment)
{
  UseInputTimeStruct            = 1;
  InputTimeStruct.Name          = name;
  InputTimeStruct.UnitIntern    = unit;
  InputTimeStruct.TypeIntern    = DEF_VOID;
  InputTimeStruct.CommentIntern = comment;
  InputTimeStruct.pValIntern    = 0;        // wird nicht belegt, weil zur Berechnung später die aktuelle als Parameter
                                            // in die Funktion kommt
  return Status;
}
#if 0
// Erstellt aus einer SDsModVar Liste die gewünschten Inputs
//==========================================================
status_t CDsCtrlIO::buildInputVector(SDsModVar *pinp,uint32_t ninp) {

    SDsCtrlIOInp *pI;
    uint32_t       i;
    InputProofedFlag = 0;

    for(i=0;i<ninp;i++) {

        pI = buildInputStruct(pinp[i].Name);
        if( !pI ) return Status;

        switch(pinp[i].Type) {
        case DEF_DOUBLE:
        case DEF_FLOAT:
        case DEF_SIGNED_LONG:
        case DEF_UNSIGNED_LONG:
        case DEF_SIGNED_SHORT:
        case DEF_UNSIGNED_SHORT:
        case DEF_SIGNED_CHAR:
        case DEF_UNSIGNED_CHAR:
                pI->Name          = pinp[i].Name;
                pI->UnitIntern    = pinp[i].Unit;
                pI->TypeIntern    = pinp[i].Type;
                pI->pValIntern    = pinp[i].pVal;
                pI->CommentIntern = pinp[i].Comment;
            break;
        case DEF_VEC:
                pI->Name       = pinp[i].Name;
                pI->UnitIntern    = pinp[i].Unit;
                pI->TypeIntern    = pinp[i].Type;
                pI->pValIntern    = pinp[i].pVal;
                pI->CommentIntern = pinp[i].Comment;

                // Ist Vector_t schon gebildet
                if( *(Vector_t *)pI->pValIntern ) {
                    
                    pI->NColIntern           = 1;
                    pI->NRowIntern           = GET_NDIMS(*(Vector_t *)pI->pValIntern);
                    pI->NDimIntern           = pI->NRowIntern;
                } else {
                    pI->BuildInternVecByExternSize = 1; // Vector_t wird nach Get-Größe gebildet
                }

                break;
        case DEF_STRING:
                pI->Name          = pinp[i].Name;
                pI->TypeIntern    = pinp[i].Type;
                pI->pValIntern    = pinp[i].pVal;
                pI->CommentIntern = pinp[i].Comment;
            break;
        default:
                ErrText.catFormat("Fehler CDsCtrlIO::regInpt() Der <%s> ist noch nicht programmiert\n"
                           ,VarTypeStr[pI->TypeIntern]);
                Status = NOT_OKAY;
                return Status;
        }
    }
    return Status;
}
status_t CDsCtrlIO::regInput1DTable(char      *tabname     // Tabellenname
                                   ,char      *comment     // Kommentar
                                   ,char      *xunit       // x-Einheit
                                   ,char      *yunit       // y-Einheit
                                   ,uint8_t     order        // 0/1
                                   ,CSlf1DTab *p1dtab      // Tabellenpointer
                                   ) {
    SDsCtrlIOInp     *pI;
    InputProofedFlag = 0;

    if( !tabname ) {

        ErrText.catFormat("CDsCtrlIO::regInput1DTable: Tabellenname ist nicht angegeben !!\n");
        Status = NOT_OKAY;
        return Status;
    }
    if( !xunit    ) xunit    = "-";
    if( !yunit    ) yunit    = "-";
    if( !comment )  comment = "";

    // Struktur suchen
    //================
    pI = buildInputStruct(tabname);
    if( !pI ) return Status;

    pI->Name           = tabname;
    pI->CommentIntern     = comment;
    pI->UnitIntern        = yunit;
    pI->ReqXUnit       = xunit;
    pI->ReqOrder       = order;
    pI->TypeIntern        = DEF_1D_TAB;

    if( !p1dtab ) {
        
        ErrText.catFormat("CDsCtrlIO::regInput1DTable: Pointer vom type:<%s> muß einen Wert haben (Ansonsten Pointer-Wert nehmen)  !!\n"
                         ,VarTypeStr[pI->TypeIntern]);
        Status = NOT_OKAY;
        return Status;
    }
    pI->pValIntern        = p1dtab;
        
    return Status;
}
//
// Angeforderter Single Input für zeitabhängige Berechnung
// (Der externe Input muß dann eine Tabelle mit Zeitinfo sein)
//============================================================
status_t CDsCtrlIO::regInputSingleTime(char *timeunit
                                      ,char *name
                                      ,char *unit
                                      ,char *comment
                                      ,EVarType type
                                      ,void *pval) {

    SDsCtrlIOInp     *pI;
    InputProofedFlag = 0;

    if( !name ) {

        ErrText.catFormat("CDsCtrlIO::buildInputSingle: Variablenname ist nicht angegeben !!\n");
        Status = NOT_OKAY;
        return Status;
    }
    if( !unit    ) unit    = "-";
    if( !comment ) comment = "";
    if( !pval ) {

        ErrText.catFormat("CDsCtrlIO::buildInputSingle: Pointer Variable <%s> ist NULL !!\n"
                         ,name);
        Status = NOT_OKAY;
        return Status;
    }

    pI = buildInputStruct(name);
    if( !pI ) return Status;


    switch(type) {
    case DEF_DOUBLE:
    case DEF_FLOAT:
    case DEF_SIGNED_LONG:
    case DEF_UNSIGNED_LONG:
    case DEF_SIGNED_SHORT:
    case DEF_UNSIGNED_SHORT:
    case DEF_SIGNED_CHAR:
    case DEF_UNSIGNED_CHAR:
        pI->TimeUnit    = timeunit;
        pI->Name        = name;
        pI->UnitIntern     = unit;
        pI->TypeIntern     = type;
        pI->pValIntern     = pval;
        pI->NDimIntern     = 1;
        pI->NRowIntern     = 1;
        pI->NColIntern     = 1;
        break;
    default:
        ErrText.catFormat("Fehler CDsCtrlIO::regInptSingleTime() Die Variable <%s> ist kein Single-Typ <%s>\n"
                   ,name
                   ,VarTypeStr[type]);
        Status = NOT_OKAY;
        return Status;

    }
    return Status;
}
#endif
SDsCtrlIOInp *CDsCtrlIO::buildInputStruct(char * name) {

    SDsCtrlIOInp *pI = pIOInp;

    while( pI ) {
        if( pI->Name == name )
            break;
        pI = pI->pNext;
    }
    if( pI ) {
        ErrText.catFormat("Fehler CDsCtrlIO::regInpt() Die Variable <%s> wurde bereits registriert, d.h sie doppelt\n"
                   ,pI->Name.c_str());
        Status = NOT_OKAY;
        return 0;
    }
    try {
        pI = new SDsCtrlIOInp;
    } catch(...) {
        exit(NOT_ENOUGH_MEMORY);
    }
    pI->ReqTimeVal        = 0;
    pI->pValIntern           = 0;
    pI->ppReqVal          = 0;
    pI->TypeIntern           = DEF_VOID;
    pI->BuildInternVecByExternSize = 0;
    pI->ReqPtrIsHereBuild = 0;
    pI->NDimIntern           = 0;
    pI->NRowIntern           = 0;
    pI->NColIntern           = 0;

    pI->FactorExtToInt            = 1.0;
    pI->OffsetExtToInt            = 0.0;

    pI->pValExtern           = 0;
    pI->TypeExtern           = DEF_VOID;
    pI->NDimExtern           = 0;
    pI->NRowExtern           = 0;
    pI->NColExtern           = 0;

    pI->InternVarInExternFound     = 0;

    pI->pNext             = pIOInp;
    pIOInp                = pI;

    return pI;
}
void CDsCtrlIO::deleteInput(void) {

    SDsCtrlIOInp *pI;

    while(pIOInp) {

        pI = pIOInp->pNext;
        deleteInput(pIOInp);
        delete pIOInp;
        pIOInp = pI;
    }
}
void CDsCtrlIO::deleteInput(SDsCtrlIOInp *pI) {

    switch(pI->TypeIntern) {
    case DEF_VEC:
        if( pI->BuildInternVecByExternSize )
        {
          FreeVector(*(Vector_t *)pI->pValIntern);
          pI->pValIntern = 0;
        }
        break;
    }
    switch(pI->TypeExtern) {
    case DEF_DOUBLE:
      if( pI->pValExtern )
      {
        double *pd = (double *)pI->pValExtern;
        delete pd;
        pI->pValExtern = 0;
      }
      break;
    case DEF_VEC:
        if( pI->pValExtern )
        {
          FreeVector(*(Vector_t *)pI->pValExtern);
          pI->pValExtern = 0;
        }
        break;
    case DEF_1D_TAB:
        if( pI->pValExtern )
        {
          CSlf1DTab *p1dtab = (CSlf1DTab *)pI->pValExtern;
          delete p1dtab;
          pI->pValExtern = 0;
        }
        break;
    }
}


// bildet die zur Schnittstelle entsprechende externen Input
//==========================================================
#if DS_CTRL_IO_MEX
status_t CDsCtrlIO::getMexInputVectorWithTimeVector(const mxArray *pin) {

  mwSize   num_of_elements;
  mwIndex  index;

  int      nfields = mxGetNumberOfFields(pin);
  int      ifield;

  mxArray  *parray;
  const char *fieldname;
  SDsCtrlIOInp *pI;


  num_of_elements = mxGetNumberOfElements(pin); 

  for (index=0; index<num_of_elements; index++)  {


    // Schleife über alle Strukturelemente
    //====================================
    for( ifield=0;ifield<nfields;ifield++) {


      parray = mxGetFieldByNumber(pin, index, ifield);

      fieldname = mxGetFieldNameByNumber(pin,ifield);

      // Zeitvektor
      if(  UseInputTimeStruct 
        && (InputTimeStruct.Name == fieldname)
        && isInputMexValue(parray)
        )
      {
        if( readInputMexValue(parray,fieldname,&InputTimeStruct) != OKAY )
          return Status;
      }
      else
      {
        // In der Inputliste nach diesem Namen suchen
        pI = pIOInp;
        while( pI ) {
          if( pI->Name == fieldname )
            break;
          pI = pI->pNext;
        }
        // Wenn in Liste gefunden und als Wert erkannt
        if( pI && isInputMexValue(parray) ) { 

          if( readInputMexValue(parray,fieldname,pI) != OKAY )
            return Status;

        }
      }
    }
  }

  return Status;
}
#if 0
status_t CDsCtrlIO::constructInput(const mxArray *pin) {

    mwSize   num_of_elements;
    mwIndex  index;

    int      nfields = mxGetNumberOfFields(pin);
    int      ifield;

    mxArray  *parray;
    const char *fieldname;
    SDsCtrlIOInp *pI;

  
    num_of_elements = mxGetNumberOfElements(pin); 

    for (index=0; index<num_of_elements; index++)  {


        // Schleife über alle Strukturelemente
        //====================================
        for( ifield=0;ifield<nfields;ifield++) {


            parray = mxGetFieldByNumber(pin, index, ifield);

            fieldname = mxGetFieldNameByNumber(pin,ifield);

            // In der Inputliste nach diesem Namen suchen
            pI = pIOInp;
            while( pI ) {
                if( pI->Name == fieldname )
                    break;
                pI = pI->pNext;
            }
            // Wenn in Liste gefunden und als Wert erkannt
            if( pI && isInputMexValue(parray) ) { 

                if( readInputMexValue(parray,fieldname,pI) != OKAY )
                    break;

            }
        }
    }

    return Status;
}
#endif
// Prüft, ob Variable-Struktur (name.v ist minimum)
//===============================================================
bool CDsCtrlIO::isInputMexValue(const mxArray *par) {

    int      nf = mxGetNumberOfFields(par);
    int      ifield;
    mxArray  *parray;
    const char *fn;
    uint8_t    find_flag = 0;

    // Schleife über alle Strukturelemente
    //====================================
    for( ifield=0;ifield<nf;ifield++) {


        parray = mxGetFieldByNumber(par, 0, ifield);

        fn = mxGetFieldNameByNumber(par,ifield);

        if(  !mxIsStruct(parray)  ){

            if(  (*fn == *VAL) 
              || (SlfStrLen(fn)>1 && *fn == *XVEC && *fn+1 == *XVEC+1)
              ){
                find_flag = 1;
                break;
            }
        }
    }

    if( find_flag )
        return true;

    return false;
}
// Input, behandelt Variable-Struktur name.v,name.u,name.c,name.t
//===============================================================
status_t CDsCtrlIO::readInputMexValue(const mxArray *par
                                     ,const char *cname
                                     ,SDsCtrlIOInp *pI) {

    int      nfields = mxGetNumberOfFields(par);
    int      ifield;
    mxArray  *parray;
    const char *fieldname;

    // Variablen für die einzelnen Attribute
    //CSlfStr  name = cname;
    char     text[DS_CTRL_TEXTLENGTH];
    //CSlfStr  unit;
    //CSlfStr  comment;
    //CSlfStr  xname, xunit, xcomment;
    //CSlfStr  yname, yunit, ycomment;
    //CSlfStr  zname, zunit, zcomment;
    double   *pyd,*pxd;
    uint32_t   nxrow=0,nxcol=0;
    uint32_t   nyrow=0,nycol=0;
    //uint32_t   nzrow=0,nzcol=0;
    uint8_t    order=1;
    uint8_t    flag = 0;
    //CSlfStr  sval;
    Vector_t *pvec;
    Vector_t   vec;
    CSlf1DTab *p1dtab;


    flag = 0;
    //============================================================
    //============================================================
    // Zuerst Kommentar suchen (muß nicht notwendigerweise da sein)
    //============================================================
    //============================================================
    for( ifield=0;ifield<nfields;ifield++) {

        fieldname = mxGetFieldNameByNumber(par,ifield);

        if( *fieldname == *COMMENT ) { // Kommentar auslesen

            parray = mxGetFieldByNumber(par, 0, ifield);

            if( !mxIsEmpty(parray) && mxIsChar(parray) ) {

    			      mxGetString(parray
                           ,text
                           ,MIN(mxGetM(parray)*(mxGetN(parray)+1),DS_CTRL_TEXTLENGTH));

                pI->CommentExtern = text;
            }
            break;
        }
    }

    //==================
    //==================
    // Dann Typ suchen
    //==================
    //==================
    flag = 0;
    for( ifield=0;ifield<nfields;ifield++) {

        fieldname = mxGetFieldNameByNumber(par,ifield);

        if( *fieldname == *TYPE ) { // Typ auslesen

            parray = mxGetFieldByNumber(par, 0, ifield);

            if( mxIsEmpty(parray) || !mxIsChar(parray) ) {
                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or no string (char)\n"
                                 ,cname
                                 ,fieldname);
                Status = NOT_OKAY;
                return Status;
            }
            if( mxGetM(parray)*(mxGetN(parray)+1) > DS_CTRL_TEXTLENGTH ) {
                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem size of variable type <%s> is longer than %i \n"
                                 ,fieldname
                                 ,DS_CTRL_TEXTLENGTH);
                Status = NOT_OKAY;
                return Status;
            }
			      mxGetString(parray,text,mxGetM(parray)*(mxGetN(parray)+1));

            if( !strcmp(text,TYPE_SINGLE[0]) || !strcmp(text,TYPE_SINGLE[1]) ) {
                pI->TypeExtern = DEF_DOUBLE;
                // Verträglichkeit prüfen
                if(  pI->TypeIntern == DEF_VEC
                  || pI->TypeIntern == DEF_MAT
                  || pI->TypeIntern == DEF_1D_TAB )
                    flag = 1;
                break;
            } else if( !strcmp(text,TYPE_STRING) ) {
                pI->TypeExtern = DEF_STRING;
                // Verträglichkeit prüfen
                if(  pI->TypeIntern == DEF_DOUBLE
                  || pI->TypeIntern == DEF_VEC
                  || pI->TypeIntern == DEF_MAT
                  || pI->TypeIntern == DEF_1D_TAB )
                    flag = 1;
                break;            
            } else if( !strcmp(text,TYPE_VECTOR) ) {
                pI->TypeExtern = DEF_VEC;
                // Verträglichkeit prüfen
                if(  pI->TypeIntern == DEF_STRING
                  || pI->TypeIntern == DEF_1D_TAB )
                    flag = 1;
                break;
            } else if( !strcmp(text,TYPE_MATRIX) ) {
                pI->TypeExtern = DEF_MAT;
                // Verträglichkeit prüfen
                if(  pI->TypeIntern == DEF_STRING
                  || pI->TypeIntern == DEF_1D_TAB )
                    flag = 1;
                break;
            } else if( !strcmp(text,TYPE_1DTAB[0]) || !strcmp(text,TYPE_1DTAB[1]) ){
                pI->TypeExtern = DEF_1D_TAB;            
                // Verträglichkeit prüfen
                if(  pI->TypeIntern != pI->TypeExtern
                  && !(pI->TypeIntern == DEF_DOUBLE && pI->ReqTimeVal)
                  )
                    flag = 1;
                break;
            }

        }
        if(flag)
            break;
    }
    if(flag) {
        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: type internal <%s> does not fit to external <%s> \n"
                         ,cname
                         ,VarTypeStr[pI->TypeIntern]
                         ,VarTypeStr[pI->TypeExtern]);
        Status = NOT_OKAY;
        return Status;
    }
    if( pI->TypeExtern == DEF_VOID ) {
        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem type <%s> of variable <%s> is not recognized \n"
                         ,text
                         ,cname);
        Status = NOT_OKAY;
        return Status;
    }

    //==================================
    //==================================
    // Entsprechend dem typ Unit suchen
    //==================================
    //==================================
    flag = 0;
    if(  pI->TypeExtern == DEF_DOUBLE 
      || pI->TypeExtern == DEF_VEC
      ) {

        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( *fieldname == *UNIT ) { // Wert auslesen

                flag = 2;
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsChar(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or no string (char)\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( mxGetM(parray)*(mxGetN(parray)+1) > DS_CTRL_TEXTLENGTH ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem size of unit of variable<%s> is longer than %i \n"
                                     ,fieldname
                                     ,DS_CTRL_TEXTLENGTH);
                    Status = NOT_OKAY;
                    return Status;
                }
			          mxGetString(parray,text,mxGetM(parray)*(mxGetN(parray)+1));

                pI->UnitExtern = text;

                // Faktor und Offset bestimmen
                //============================
                if( (Status = SlfFktUnitConv(pI->UnitExtern.c_str() /*=>*/
                                            ,pI->UnitIntern.c_str()
                                            ,&pI->FactorExtToInt
                                            ,&pI->OffsetExtToInt
                                            ,ErrText
                                            ))          != OKAY )  {

                    ErrText.catFormat("Umrechnung Inputvariable <%s> von extern Einheit [%s] in interne Einheit [%s] geht nicht !!!\n"
                                     ,pI->Name.c_str()
                                     ,pI->UnitExtern.c_str()
                                     ,pI->UnitIntern.c_str()
                                     );
                    return Status;
                }
                break;
            }
        }
    } else if(  pI->TypeExtern == DEF_STRING ) {
        flag = 2;
    } else if(  pI->TypeExtern == DEF_1D_TAB ) {
        
        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( SlfStrLen(fieldname)>1
              && *fieldname     == *XUNIT
              && *(fieldname+1) == *(XUNIT+1) ) { // Wert auslesen

                flag++;
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsChar(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or no string (char)\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( mxGetM(parray)*(mxGetN(parray)+1) > DS_CTRL_TEXTLENGTH ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem size of unit of variable<%s> is longer than %i \n"
                                     ,fieldname
                                     ,DS_CTRL_TEXTLENGTH);
                    Status = NOT_OKAY;
                    return Status;
                }
			          mxGetString(parray,text,mxGetM(parray)*(mxGetN(parray)+1));

                pI->XUnitExtern = text;

                // Faktor und Offset bestimmen
                //============================
                if( (Status = SlfFktUnitConv(pI->XUnitExtern.c_str() 
                                            ,pI->XUnitIntern.c_str()
                                            ,&pI->XFactorExtToInt
                                            ,&pI->XOffsetExtToInt
                                            ,ErrText
                                            ))          != OKAY )  {

                    ErrText.catFormat("Umrechnung Inputvariable <%s> von extern Einheit x-vektor [%s] in interne Einheit [%s] geht nicht !!!\n"
                                     ,pI->Name.c_str()
                                     ,pI->XUnitExtern.c_str()
                                     ,pI->XUnitIntern.c_str()
                                     );
                    return Status;
                }
            } else if( SlfStrLen(fieldname)>1
                     && *fieldname     == *YUNIT
                     && *(fieldname+1) == *(YUNIT+1) ) { // Wert auslesen

                flag++;
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsChar(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or no string (char)\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( mxGetM(parray)*(mxGetN(parray)+1) > DS_CTRL_TEXTLENGTH ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem size of unit of variable<%s> is longer than %i \n"
                                     ,fieldname
                                     ,DS_CTRL_TEXTLENGTH);
                    Status = NOT_OKAY;
                    return Status;
                }
			          mxGetString(parray,text,mxGetM(parray)*(mxGetN(parray)+1));

                pI->UnitExtern = text;

                // Faktor und Offset bestimmen
                //============================
                if( (Status = SlfFktUnitConv(pI->UnitExtern.c_str() /*=>*/
                                            ,pI->UnitIntern.c_str()
                                            ,&pI->FactorExtToInt
                                            ,&pI->OffsetExtToInt
                                            ,ErrText
                                            ))          != OKAY )  {

                    ErrText.catFormat("Umrechnung Inputvariable <%s> von extern Einheit y-vektor [%s] in interne Einheit [%s] geht nicht !!!\n"
                                     ,pI->Name.c_str()
                                     ,pI->UnitExtern.c_str()
                                     ,pI->UnitIntern.c_str()
                                     );
                    return Status;
                }
            }
        }    
    }

    if( flag != 2) {
        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: extern unit (maybe xunit and yunit for 1dtable) could not be found <%s> \n"
                         ,cname);
        Status = NOT_OKAY;
        return Status;
    }

    //==================================
    //==================================
    // Entsprechend dem typ Namen suchen
    //==================================
    //==================================
    if(  pI->TypeExtern == DEF_1D_TAB ) {
        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( SlfStrLen(fieldname)>1
              && *fieldname     == *XNAME
              && *(fieldname+1) == *(XNAME+1) ) { // Kommentar auslesen
     
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( !mxIsEmpty(parray) && mxIsChar(parray) ) {

    			    mxGetString(parray
                               ,text
                               ,MIN(mxGetM(parray)*(mxGetN(parray)+1),DS_CTRL_TEXTLENGTH));

                    pI->XNameExtern = text;
                }
                
            } else
            if( SlfStrLen(fieldname)>1
              && *fieldname     == *YNAME
              && *(fieldname+1) == *(YNAME+1) ) { // Kommentar auslesen
     
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( !mxIsEmpty(parray) && mxIsChar(parray) ) {

    			    mxGetString(parray
                         ,text
                         ,MIN(mxGetM(parray)*(mxGetN(parray)+1),DS_CTRL_TEXTLENGTH));

                    pI->YNameExtern = text;
                }
                
            }
        }
        if( !(pI->XNameExtern.getLen()) ) pI->XNameExtern = "x";
        if( !(pI->YNameExtern.getLen()) ) pI->YNameExtern = "y";
    }
    //==================================
    //==================================
    // Entsprechend dem typ Kommenatar suchen
    //==================================
    //==================================
    if(  pI->TypeExtern == DEF_1D_TAB ) {
        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( SlfStrLen(fieldname)>1
              && *fieldname     == *XCOMMENT
              && *(fieldname+1) == *(XCOMMENT+1) ) { // Kommentar auslesen
     
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( !mxIsEmpty(parray) && mxIsChar(parray) ) {

    			    mxGetString(parray
                               ,text
                               ,MIN(mxGetM(parray)*(mxGetN(parray)+1),DS_CTRL_TEXTLENGTH));

                    pI->XCommentExtern = text;
                }
                
            } else
            if( SlfStrLen(fieldname)>1
              && *fieldname     == *YCOMMENT
              && *(fieldname+1) == *(YCOMMENT+1) ) { // Kommentar auslesen
     
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( !mxIsEmpty(parray) && mxIsChar(parray) ) {

    			    mxGetString(parray
                               ,text
                               ,MIN(mxGetM(parray)*(mxGetN(parray)+1),DS_CTRL_TEXTLENGTH));

                    pI->YCommentExtern = text;
                }
                
            }
        }
    }

    //===========================
    //===========================
    // Werte suchen
    //===========================
    //===========================
    switch(pI->TypeExtern) {
    case DEF_DOUBLE:  // double Einzelwert

        // Wert suchen
        //============
        //============
        pI->NColExtern = 1;        // da singlewert
        pI->NRowExtern = 1;
        pI->NDimExtern = 1;
        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( *fieldname == *VAL ) { // Wert auslesen

              double *pdouble;
               pI->InternVarInExternFound = 1;
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }
                pdouble = new double; 
                *pdouble = *mxGetPr(parray);
                pI->pValExtern = (void *)pdouble;
                break;
            }
        }
        break;
    case DEF_STRING:

        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( *fieldname == *VAL ) { // Wert auslesen

               pI->InternVarInExternFound = 1;
                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsChar(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not char\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( mxGetM(parray)*(mxGetN(parray)+1) > DS_CTRL_TEXTLENGTH ) {

                    CSlfStr *pstr  = new CSlfStr;
                    char    *pchar;
                    try {
                        pchar = new char[mxGetM(parray)*(mxGetN(parray)+1)];
                    } catch(...) {
                        exit(NOT_ENOUGH_MEMORY);
                    }
                    mxGetString(parray,pchar,mxGetM(parray)*(mxGetN(parray)+1));
                    *pstr = pchar;
                    pI->pValExtern = (void *)pstr;
                    delete []pchar;

                } else {
			    
                    CSlfStr *pstr = new CSlfStr;                    
                    mxGetString(parray,text,mxGetM(parray)*(mxGetN(parray)+1));
                    *pstr = text;
                    pI->pValExtern = (void *)pstr;
                }
                break;
            }
        }
        break;
    case DEF_VEC:

        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( *fieldname == *VAL ) { // Wert auslesen

                pI->InternVarInExternFound = 1;

                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }

                pI->NRowExtern  = (uint32_t)mxGetM(parray);
                pI->NColExtern  = (uint32_t)mxGetN(parray);

                if( pI->NRowExtern == 0 || pI->NColExtern == 0 ) {

                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem externer Vector_t <%s> is null <%lu x %lu>\n"
                                     ,cname
                                     ,pI->NRowExtern
                                     ,pI->NColExtern);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( pI->NRowExtern > 1 && pI->NColExtern > 1 ) {

                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem externer Vector_t <%s> is a Matrix_t with <%lu x %lu>\n"
                                     ,cname
                                     ,pI->NRowExtern
                                     ,pI->NColExtern);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( pI->NColExtern > pI->NRowExtern ) {

                    pI->NDimExtern = pI->NColExtern;
                    pI->NColExtern = 1;
                    pI->NRowExtern = pI->NDimExtern;
                } else 
                    pI->NDimExtern = pI->NRowExtern;

                vec = NewVector(pI->NDimExtern);
                pvec = &vec;
                // Vekotor kopieren
                //=================
                memcpy(vec, mxGetPr(parray),pI->NRowExtern*sizeof(double));
                
                pI->pValExtern = (void *)pvec;
                
                break;
            }
        }
        break;
    case DEF_1D_TAB:
        flag = 0;
        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if(  SlfStrLen(fieldname)>1
              && *fieldname     ==   *XVEC
              && *(fieldname+1) ==   *(XVEC+1) ) { // Wert auslesen

                flag++;

                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }

                nxrow  = (uint32_t)mxGetM(parray);
                nxcol  = (uint32_t)mxGetN(parray);

                if( nxrow == 0 || nxcol == 0 ) {

                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is null <%lu x %lu>\n"
                                     ,cname
                                     ,nxrow
                                     ,nxcol);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( nxrow > 1 && nxcol > 1 ) {

                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is a Matrix_t with <%lu x %lu>\n"
                                     ,cname
                                     ,nxrow
                                     ,nxcol);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( nxcol > nxrow ) {

                    nxrow = nxcol;
                    nxcol = 1;
                }
                pxd = mxGetPr(parray);
                
            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     ==   *YVEC
                     && *(fieldname+1) ==   *(YVEC+1) ) { // Wert auslesen

                flag++;

                parray = mxGetFieldByNumber(par, 0, ifield);

                if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
                                     ,cname
                                     ,fieldname);
                    Status = NOT_OKAY;
                    return Status;
                }

                nyrow  = (uint32_t)mxGetM(parray);
                nycol  = (uint32_t)mxGetN(parray);

                if( nyrow == 0 || nycol == 0 ) {

                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is null <%lu x %lu>\n"
                                     ,cname
                                     ,nxrow
                                     ,nxcol);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( nyrow > 1 && nycol > 1 ) {

                    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is a Matrix_t with <%lu x %lu>\n"
                                     ,cname
                                     ,nxrow
                                     ,nxcol);
                    Status = NOT_OKAY;
                    return Status;
                }
                if( nycol > nyrow ) {

                    nyrow = nycol;
                    nycol = 1;
                }
                pyd = mxGetPr(parray);
                
            }

        }
        if( flag != 2 ) {
            ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: extern variable <%s> 1dtable with %s and %s could not be found <%s> \n"
                             ,cname
                             ,XVEC
                             ,YVEC);
            Status = NOT_OKAY;
            return Status;
        }
        pI->InternVarInExternFound = 1;

        // Tabelle erstellen
        //==================
        p1dtab = new CSlf1DTab;
        if( p1dtab->set(pI->Name.c_str()
                       ,pI->CommentExtern.c_str()
                       ,pI->XNameExtern.c_str()
                       ,pI->XUnitExtern.c_str()
                       ,pI->XCommentExtern.c_str()
                       ,pxd
                       ,nxrow
                       ,pI->YNameExtern.c_str()
                       ,pI->UnitExtern.c_str()
                       ,pI->YCommentExtern.c_str()
                       ,pyd
                       ,nyrow
                       ,pI->ReqOrder) != OKAY ) {

            ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem with variable <%s>: building table:%s\n"
                             ,cname
                             ,p1dtab->getErrText());
            Status = NOT_OKAY;
            return Status;
        }

        pI->pValExtern = (void *)p1dtab;

        // Länge der Tabelle
        //==================
        pI->NRowExtern = p1dtab->NRows;
        pI->NColExtern = 1;
        pI->NDimExtern = p1dtab->NRows;

        break;
    default:
        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: type external <%s> not programmed \n"
                         ,cname
                         ,VarTypeStr[pI->TypeExtern]
                         ,VarTypeStr[pI->TypeExtern]);
        Status = NOT_OKAY;
        return Status;
    }



    ////==================================
    ////==================================
    //// Entsprechend dem typ Wert suchen
    ////==================================
    ////==================================
    //pI->InternVarInExternFound = 0;

    //// Wenn TypeIntern = Singlewert und TypeExtern Vektor oder Matrix_t,
    //// dann Gettype auf DEF_DOUBLE setzen (wir wollen nur einen Singlewert)
    //if(  pI->TypeExtern == DEF_VEC
    //  && (  pI->TypeIntern == DEF_DOUBLE 
    //     || pI->TypeIntern == DEF_SIGNED_LONG
    //     || pI->TypeIntern == DEF_UNSIGNED_LONG
    //     || pI->TypeIntern == DEF_SIGNED_SHORT
    //     || pI->TypeIntern == DEF_UNSIGNED_SHORT
    //     || pI->TypeIntern == DEF_SIGNED_CHAR
    //     || pI->TypeIntern == DEF_UNSIGNED_CHAR
    //     )         
    //  )
    //    pI->TypeExtern = DEF_DOUBLE;

    //switch(pI->TypeExtern) {
    //case DEF_DOUBLE:  // double Einzelwert

    //    // Wert suchen
    //    //============
    //    //============
    //    pI->NColExtern = 1;        // da singlewert
    //    pI->NRowExtern = 1;
    //    pI->NDimExtern = 1;
    //    for( ifield=0;ifield<nfields;ifield++) {

    //        fieldname = mxGetFieldNameByNumber(par,ifield);

    //        if( *fieldname == *VAL ) { // Wert auslesen

    //            pI->InternVarInExternFound = 1;

    //            parray = mxGetFieldByNumber(par, 0, ifield);

    //            if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
    //                                 ,cname
    //                                 ,fieldname);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }


    //            if( pI->TypeIntern == DEF_STRING ) { // double -> string

    //                CSlfStr *pstr = (CSlfStr *)pI->pValIntern;
    //                pstr->catFormat("%g",*mxGetPr(parray));
    //            } else {


    //                // Get-Wert nach Req-Wert konvertieren
    //                // nur Eingachwerte sind zugelassen
    //                //=================================
    //                SlfFktConvertSingle(mxGetPr(parray)
    //                                   ,pI->TypeExtern
    //                                   ,pI->pValIntern
    //                                   ,pI->TypeIntern
    //                                   ,pI->FactorExtToInt
    //                                   ,pI->OffsetExtToInt);
    //            }
    //            break;
    //        }
    //    }
    //    break;
    //case DEF_STRING:

    //    for( ifield=0;ifield<nfields;ifield++) {

    //        fieldname = mxGetFieldNameByNumber(par,ifield);

    //        if( *fieldname == *VAL ) { // Wert auslesen

    //            pI->InternVarInExternFound = 1;

    //            parray = mxGetFieldByNumber(par, 0, ifield);

    //            if( mxIsEmpty(parray) || !mxIsChar(parray) ) {
    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not char\n"
    //                                 ,cname
    //                                 ,fieldname);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( mxGetM(parray)*(mxGetN(parray)+1) > DS_CTRL_TEXTLENGTH ) {

    //                CSlfStr *pstr  = (CSlfStr *)pI->pValIntern;
    //                char    *pchar;
    //                try {
    //                    pchar = new char[mxGetM(parray)*(mxGetN(parray)+1)];
    //                } catch(...) {
    //                    exit(NOT_ENOUGH_MEMORY);
    //                }
    //                mxGetString(parray,pchar,mxGetM(parray)*(mxGetN(parray)+1));
    //                *pstr = pchar;
    //                delete []pchar;

    //            } else {
			 //   
    //                CSlfStr *pstr = (CSlfStr *)pI->pValIntern;                    
    //                mxGetString(parray,text,mxGetM(parray)*(mxGetN(parray)+1));
    //                *pstr = text;
    //            }
    //            break;
    //        }
    //    }
    //    break;
    //case DEF_VEC:

    //    for( ifield=0;ifield<nfields;ifield++) {

    //        fieldname = mxGetFieldNameByNumber(par,ifield);

    //        if( *fieldname == *VAL ) { // Wert auslesen

    //            pI->InternVarInExternFound = 1;

    //            parray = mxGetFieldByNumber(par, 0, ifield);

    //            if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
    //                                 ,cname
    //                                 ,fieldname);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }

    //            pI->NRowExtern  = (uint32_t)mxGetM(parray);
    //            pI->NColExtern  = (uint32_t)mxGetN(parray);

    //            if( pI->NRowExtern == 0 || pI->NColExtern == 0 ) {

    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem externer Vector_t <%s> is null <%lu x %lu>\n"
    //                                 ,cname
    //                                 ,pI->NRowExtern
    //                                 ,pI->NColExtern);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( pI->NRowExtern > 1 && pI->NColExtern > 1 ) {

    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem externer Vector_t <%s> is a Matrix_t with <%lu x %lu>\n"
    //                                 ,cname
    //                                 ,pI->NRowExtern
    //                                 ,pI->NColExtern);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( pI->NColExtern > pI->NRowExtern ) {

    //                pI->NDimExtern = pI->NColExtern;
    //                pI->NColExtern = 1;
    //                pI->NRowExtern = pI->NDimExtern;
    //            } else 
    //                pI->NDimExtern = pI->NRowExtern;

    //            
    //            pvec = (Vector_t *)pI->pValIntern;

    //            if( pI->BuildInternVecByExternSize ) { // wird erst gebildet

    //                pI->NColIntern  = pI->NColExtern;
    //                pI->NRowIntern  = pI->NRowExtern;
    //                pI->NDimIntern  = pI->NDimExtern;

    //                // Vector_t mit speicher anlegen
    //                //============================
    //                *pvec = NewVector(pI->NRowIntern);
			 //       

    //            } else { // ist schon gebildet Länge prüfen

    //                ZeroVector(*pvec);

    //                pI->NRowExtern = MIN( pI->NRowExtern,pI->NRowIntern);
    //                pI->NDimExtern = pI->NRowExtern;
    //            }

    //            // Vekotor kopieren
    //            //=================
    //            memcpy(*pvec, mxGetPr(parray),pI->NRowIntern*sizeof(double));
    //            
    //            // Get-Wert nach Req-Wert konvertieren
    //            //====================================
    //            VectorFactorOffset(pI->Factor,pI->Offset,*pvec);
    //            
    //            break;
    //        }
    //    }
    //    break;
    //case DEF_1D_TAB:
    //    flag = 0;
    //    for( ifield=0;ifield<nfields;ifield++) {

    //        fieldname = mxGetFieldNameByNumber(par,ifield);

    //        if(  SlfStrLen(fieldname)>1
    //          && *fieldname     ==   *XVEC
    //          && *(fieldname+1) ==   *(XVEC+1) ) { // Wert auslesen

    //            flag++;

    //            parray = mxGetFieldByNumber(par, 0, ifield);

    //            if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
    //                                 ,cname
    //                                 ,fieldname);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }

    //            nxrow  = (uint32_t)mxGetM(parray);
    //            nxcol  = (uint32_t)mxGetN(parray);

    //            if( nxrow == 0 || nxcol == 0 ) {

    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is null <%lu x %lu>\n"
    //                                 ,cname
    //                                 ,nxrow
    //                                 ,nxcol);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( nxrow > 1 && nxcol > 1 ) {

    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is a Matrix_t with <%lu x %lu>\n"
    //                                 ,cname
    //                                 ,nxrow
    //                                 ,nxcol);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( nxcol > nxrow ) {

    //                nxrow = nxcol;
    //                nxcol = 1;
    //            }
    //            pxd = mxGetPr(parray);
    //            
    //        } else
    //        if(  SlfStrLen(fieldname)>1
    //          && *fieldname     ==   *YVEC
    //          && *(fieldname+1) ==   *(YVEC+1) ) { // Wert auslesen

    //            flag++;

    //            parray = mxGetFieldByNumber(par, 0, ifield);

    //            if( mxIsEmpty(parray) || !mxIsNumeric(parray) ) {
    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem Variable <%s> with field <%s> is empty or not numeric\n"
    //                                 ,cname
    //                                 ,fieldname);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }

    //            nyrow  = (uint32_t)mxGetM(parray);
    //            nycol  = (uint32_t)mxGetN(parray);

    //            if( nyrow == 0 || nycol == 0 ) {

    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is null <%lu x %lu>\n"
    //                                 ,cname
    //                                 ,nxrow
    //                                 ,nxcol);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( nyrow > 1 && nycol > 1 ) {

    //                ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem extern 1dtable <%s> length x-Vector_t is a Matrix_t with <%lu x %lu>\n"
    //                                 ,cname
    //                                 ,nxrow
    //                                 ,nxcol);
    //                Status = NOT_OKAY;
    //                return Status;
    //            }
    //            if( nycol > nyrow ) {

    //                nyrow = nycol;
    //                nycol = 1;
    //            }
    //            pyd = mxGetPr(parray);
    //            
    //        }

    //    }
    //    if( flag != 2 ) {
    //        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: extern variable <%s> 1dtable with %s and %s could not be found <%s> \n"
    //                         ,cname
    //                         ,XVEC
    //                         ,YVEC);
    //        Status = NOT_OKAY;
    //        return Status;
    //    }
    //    pI->InternVarInExternFound = 1;

    //    // Tabelle erstellen
    //    //==================
    //    p1dtab = (CSlf1DTab *)pI->pValIntern;
    //    if( p1dtab->set(pI->Name.c_str()
    //                   ,pI->CommentExtern.c_str()
    //                   ,pI->XNameExtern.c_str()
    //                   ,pI->XUnitExtern.c_str()
    //                   ,pI->XCommentExtern.c_str()
    //                   ,pxd
    //                   ,nxrow
    //                   ,pI->YNameExtern.c_str()
    //                   ,pI->UnitExtern.c_str()
    //                   ,pI->YCommentExtern.c_str()
    //                   ,pyd
    //                   ,nyrow
    //                   ,pI->ReqOrder) != OKAY ) {

    //        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem with variable <%s>: building table:%s\n"
    //                         ,cname
    //                         ,p1dtab->getErrText());
    //        Status = NOT_OKAY;
    //        return Status;
    //    }

    //    // Einheiten konvertieren
    //    //=======================
    //    p1dtab->convXVec(pI->ReqXUnit);
    //    p1dtab->convYVec(pI->UnitIntern);

    //    // Länge der Tabelle
    //    //==================
    //    pI->NRowExtern = p1dtab->NRows;
    //    pI->NColExtern = 1;
    //    pI->NDimExtern = p1dtab->NRows;
    //    if( pI->TypeIntern = DEF_1D_TAB ) {
    //        pI->NRowIntern = p1dtab->NRows;
    //        pI->NColIntern = 1;
    //        pI->NDimIntern = p1dtab->NRows;
    //    }

    //    break;
    //default:
    //    ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: type external <%s> not programmed \n"
    //                     ,cname
    //                     ,VarTypeStr[pI->TypeExtern]
    //                     ,VarTypeStr[pI->TypeExtern]);
    //    Status = NOT_OKAY;
    //    return Status;
    //}

    if( !pI->InternVarInExternFound ) {
        ErrText.catFormat("CDsCtrlIO::readInputMexValue: Problem variable <%s>: extern variable with <%s> could not be found <%s> \n"
                         ,cname
                         ,VAL);
        Status = NOT_OKAY;
        return Status;
    }

        
        
#if 0        
        
        
        if( mxIsChar(parray) ) { // String

            // Kopiert den String heraus
            char *p_text = new char[((size_t)mxGetM(parray) * (size_t)mxGetN(parray)+1)];
			if( p_text == NULL ) {
				ErrText.catFormat("malloc-Problem");
				Status = NOT_OKAY;
                return Status;
			}

            // Feldname, der einen String zugewiesen bekommt
            // identifizieren
            //========================
            if( *fieldname == *TYPE )

                type = p_text;

            else if( *fieldname == *VAL )

                sval = p_text;

            else if( *fieldname == *COMMENT )

                comment = p_text;

            else if( *fieldname == *UNIT )

                unit = p_text;
            
            else if(  SlfStrLen(fieldname)>1
                   && *fieldname     == *XNAME
                   && *(fieldname+1) == *(XNAME+1)
                   )

                xname = p_text;

            else if(  SlfStrLen(fieldname)>1
                   && *fieldname     == *XUNIT
                   && *(fieldname+1) == *(XUNIT+1)
                   )

                xunit = p_text;

            else if(  SlfStrLen(fieldname)>1
                   && *fieldname     == *XCOMMENT
                   && *(fieldname+1) == *(XCOMMENT+1)
                   )

                xcomment = p_text;

            else if(  SlfStrLen(fieldname)>1
                   && *fieldname     == *YNAME
                   && *(fieldname+1) == *(YNAME+1)
                   )

                yname = p_text;

            else if(  SlfStrLen(fieldname)>1
                   && *fieldname     == *YUNIT
                   && *(fieldname+1) == *(YUNIT+1)
                   )

                yunit = p_text;

            else if(  SlfStrLen(fieldname)>1
                   && *fieldname == *YCOMMENT
                   && *fieldname+1 == *YCOMMENT+1
                   )

                ycomment = p_text;


            //String wieder freigeben
            delete []p_text;
            
        
        } else if( mxIsClass(parray,"double") /*IS_PARAM_DOUBLE(parray)*/ ) {


            if( *fieldname == *VAL ) {

                nrow  = (uint32_t)mxGetM(parray);
                ncol  = (uint32_t)mxGetN(parray);

                if( nrow > 1 && ncol == ) { // Vector_t

                    pvec  = new Vector_t;
                    *pvec = NewVector(nrow);
                pdvalue = new double[nrow*ncol];
				if( pdvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdvalue, mxGetPr(parray), sizeof(double)*nrow*ncol);

            
            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     ==   *XVEC
                     && *(fieldname+1) ==   *(XVEC+1) ) {

                nxrow  = (uint32_t)mxGetM(parray);
                nxcol  = (uint32_t)mxGetN(parray);

                pdxvalue = new double[nxrow*nxcol];
				if( pdxvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdxvalue, mxGetPr(parray), sizeof(double)*nxrow*nxcol);

            
            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     ==   *YVEC
                     && *(fieldname+1) ==   *(YVEC+1) ) {

                nyrow  = (uint32_t)mxGetM(parray);
                nycol  = (uint32_t)mxGetN(parray);

                pdyvalue = new double[nyrow*nycol];
				if( pdyvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdyvalue, mxGetPr(parray), sizeof(double)*nyrow*nycol);

            
            
            } else if( *fieldname == *ORDER ) {

                if( fabs(*mxGetPr(parray)) > 0.5 )
                    order = 1;
                else
                    order = 0;
			
            }
        } else if(  mxIsLogical(parray)
                 || mxIsNumeric(parray)
                 ) {

            if( *fieldname == *ORDER ) {

                if( fabs(mxGetScalar(parray)) > EPSILON )
                    order = 1;
                else
                    order = 0;
			
            }

        } else {
            ErrText.catFormat("CDsParMex::readValue: Problem Variable <%s> with field <%s> is not detected\n"
                             ,name.c_str()
                             ,fieldname);
            Status = NOT_OKAY;
            return Status;

        }
    } // ende fields


    // Type bestimmen
    if( type == TYPE_SINGLE[0] || type == TYPE_SINGLE[1] ) {

        if( readInputSingleData(&name
                                ,&unit
                                ,&comment
                                ,*pdvalue
                                ) != OKAY ) {

            Status = NOT_OKAY;
            delete []pdvalue;
        }
    } else if( type == TYPE_STRING ) {

        if( readInputStringData(&name
                                ,&comment
                                ,&sval
                                ) != OKAY ) {

            Status = NOT_OKAY;
            delete []pdvalue;
        }
    } else if( type == TYPE_VECTOR ) {
        if( readInputVectorData(&name
                               ,&unit
                               ,&comment
                               ,pdvalue
                               ,nrow*ncol
                               ) != OKAY ) {

            Status = NOT_OKAY;
            delete []pdvalue;
        }
    } else if( type == TYPE_1DTAB[0] || type == TYPE_1DTAB[1] ) {

        if( readInput1DTableData(&name
                                ,&comment
                                ,&xname
                                ,&xunit
                                ,&xcomment
                                ,pdxvalue
                                ,nxrow*nxcol
                                ,&yname
                                ,&yunit
                                ,&ycomment
                                ,pdyvalue
                                ,nyrow*nycol
                                ,order
                                ) != OKAY ) {

            Status = NOT_OKAY;
            delete []pdvalue;
        }
    } else {

        ErrText.catFormat("Problem CDsCtrlIO::buildInputMex: Der type <%s> der Variable <%s> konnte nicht identifiziert werden !!!\n"
                         ,type.c_str()
                         ,name.c_str());
        Status = NOT_OKAY;
        return Status;
    }


    if( pdvalue )  delete []pdxvalue;
    if( pdxvalue ) delete []pdxvalue;
    if( pdyvalue ) delete []pdyvalue;
    if( pdzvalue ) delete []pdzvalue;

#endif
    return Status;
}
#endif

#if 0

// Setzt einen Inputwert (Single)
//===============================================================
status_t CDsCtrlIO::readInputSingleData(CSlfStr *pname
                                        ,CSlfStr *punit
                                        ,CSlfStr *pcomment
                                        ,double  dval) {

    return readInputData(pname
                         ,punit
                         ,pcomment
                         ,DEF_DOUBLE
                         ,1
                         ,1
                         ,(void *)&dval);
}
// Setzt einen Inputwert (String)
//===============================================================
status_t CDsCtrlIO::readInputStringData(CSlfStr *pname
                                        ,CSlfStr *pcomment
                                        ,CSlfStr *pstr) {

    return readInputData(pname
                         ,0
                         ,pcomment
                         ,DEF_STRING
                         ,1
                         ,1
                         ,(void *)pstr);
}
// Setzt einen Inputwert (Vector_t)
//===============================================================
status_t CDsCtrlIO::readInputVectorData(CSlfStr *pname
                                        ,CSlfStr *punit
                                        ,CSlfStr *pcomment
                                        ,double  *pvec
                                        ,uint32_t  nvec) {

    return readInputData(pname
                        ,punit
                        ,pcomment
                        ,DEF_VEC
                        ,nvec
                        ,1
                        ,(void *)pvec);
}
// Setzt einen Inputwert (1d-table)
//===============================================================
status_t CDsCtrlIO::readInput1DTableData
                           (CSlfStr  *ptabname     // Tabellenname
                           ,CSlfStr  *pcomment     // Kommentar
                           ,CSlfStr  *pxname       // Variablenname x
                           ,CSlfStr  *pxunit       // Einheit x
                           ,CSlfStr  *pxcomment    // Kommentar x
                           ,double   *pxvec        // Vektor x
                           ,uint32_t   nxvec
                           ,CSlfStr  *pyname       // Variablenname x
                           ,CSlfStr  *pyunit       // Einheit x
                           ,CSlfStr  *pycomment    // Kommentar x
                           ,double   *pyvec        // Vektor x
                           ,uint32_t   nyvec
                           ,uint8_t    order      // Linear(1) / Step(0)
                           ) {

   CSlfStr  tabname;     // Tabellenname
   CSlfStr  comment;     // Kommentar
   CSlfStr  xname;       // Variablenname x
   CSlfStr  xunit;       // Einheit x
   CSlfStr  xcomment;    // Kommentar x
   CSlfStr  yname;       // Variablenname x
   CSlfStr  yunit;       // Einheit x
   CSlfStr  ycomment;    // Kommentar x

    // Tabelle anlegen
    //================
    if( !ptabname || !pyname || !pxname || !pxvec || !pyvec ) {

        ErrText.cat("CDsCtrlIO::readInput1DTableData: Inputübergae nicht richtig erstellt\n");
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( ptabname->getLen() == 0 ) {

        ErrText.cat("CDsCtrlIO::readInput1DTableData: Inputübergae nicht richtig erstellt Tabellenname nicht übergeben\n");
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pxname->getLen() == 0 ) {

        ErrText.catFormat("CDsCtrlIO::readInput1DTableData: Inputübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für x-Vektor übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pyname->getLen() == 0 ) {

        ErrText.catFormat("CDsCtrlIO::readInput1DTableData: Inputübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für y-Vektor übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }

    if( ptabname  ) tabname  = ptabname->c_str();
    if( pcomment  ) comment  = pcomment->c_str();
    if( pxname    ) xname    = pxname->c_str();
    if( pxunit    ) xunit    = pxunit->c_str();
    if( pxcomment ) xcomment = pxcomment->c_str();
    if( pyname    ) yname    = pyname->c_str();
    if( pyunit    ) yunit    = pyunit->c_str();
    if( pycomment ) ycomment = pycomment->c_str();

    CSlf1DTab *p1dtab = new CSlf1DTab;

    // Tabelle füllen
    if( p1dtab->set(tabname.c_str()
                   ,comment.c_str()
                   ,xname.c_str()
                   ,xunit.c_str()
                   ,xcomment.c_str()
                   ,pxvec
                   ,nxvec
                   ,yname.c_str()
                   ,yunit.c_str()
                   ,ycomment.c_str()
                   ,pyvec
                   ,nyvec
                   ,order) != OKAY ) {

                 ErrText.catFormat(
                         "CDsCtrlIO::readInput1DTable Probleme mit CSlf1DTab mit"
                         " Tabelle: <%s> xvec: <%s> yvec: <%s>\n"
                        ,ptabname->c_str()
                        ,pxname->c_str()
                        ,pyname->c_str());
                 ErrText.cat(p1dtab->getErrText());
                 p1dtab->resetErrText();

                Status = NOT_OKAY;
                return Status;
    }

    return readInputData(ptabname
                        ,0
                        ,pcomment
                        ,DEF_1D_TAB
                        ,1
                        ,1
                        ,(void *)p1dtab);
}
// Setzt einen Inputwert (allgemein)
//===============================================================
status_t CDsCtrlIO::readInputData(CSlfStr     *pvn     // Variablenname
                                  ,CSlfStr     *pu        // Einheit
                                  ,CSlfStr     *pcom     // Kommentar
                                  ,EVarType    type      // DatenType
                                  ,uint16_t      nrows     // Anzahl Reihen
                                  ,uint16_t      ncols     // Anzahl Spalten
                                  ,void        *pval     // Pointer Wert
                                  ) {


    uint32_t      i;

    CSlfStr        varname;
    CSlfStr        unit;
    CSlfStr        comment;

    SDsCtrlIOInp  *pI; 

    if( pvn  ) varname = pvn->c_str();
    if( pu   ) unit    = pu->c_str();
    if( pcom ) comment = pcom->c_str();
 

    // Gültigkeit
    //===========
    if( varname.getLen() == 0 ) {

        ErrText.catFormat("CDsCtrlIO::buildInputMex: Es wurde kein Variablenname übergeben !!!");
        Status = NOT_OKAY;
        return Status;
    }
        
    //Variable suchen
    //=========================
    pI = pIOInp;
    while( pI ) {

        if( pI->Name != varname ) {

            pI = pI->pNext;
        } else {
            break;
        }
    }
    // neue Struktur bilden
    if( !pI ) {

        ErrText.catFormat("CDsCtrlIO::readInputValue: Die Variable <%s> konnte nicht in der Inputlist (SDsCtrlIOInp) gefunden werden !!\n"
                         ,varname.c_str());

        ErrText.cat("In der Inputliste enthalten:\n");
        pI = pIOInp;
        while( pI ) {

            ErrText.catFormat("<%s>\n",pI->Name.c_str());
            pI = pI->pNext;
        }
        Status = NOT_OKAY;
        return Status;
    }


    // Varibale setzen
    //================
    pI->CommentGet = comment;
    pI->UnitGet    = unit;

    if( type == DEF_VEC && nrows*ncols == 1 )
        type = DEF_DOUBLE;

    switch(type) {

    case DEF_DOUBLE:
        {
        double *pd = (double *)pval;
        double *pv;

        pI->NRowGet    = nrows;
        pI->NColGet    = ncols;
        pI->NVecGet    = ncols*nrows;

        // Variable löschen falls besetzt
        if( pI->pValGet )
            deleteInputVal(&pI->pValGet,&pI->TypeGet);

        pv   = new double[pI->NVecGet];
        for(i=0;i<pI->NVecGet;i++)
            pv[i]  = pd[i];

        pI->pValGet = (void *)pv;
        
        pI->TypeGet = type;
        }

        // Faktor und Offset bestimmen
        //============================
        if( (Status = SlfFktUnitConv(pI->UnitGet.c_str() /*=>*/
                                    ,pI->Unit.c_str()
                                    ,&pI->Factor
                                    ,&pI->Offset
                                    ,&ErrText
                                    ))          != OKAY )  {

            ErrText.catFormat("Umrechnung Inputvariable <%s> von extern Einheit [%s] in interne Einheit [%s] geht nicht !!!\n"
                             ,pI->Name
                             ,pI->UnitGet.c_str()
                             ,pI->UnitGet.c_str()
                             );
            return Status;
        }
        
        break;
    case DEF_STRING:
        {
        CSlfStr *pstr = (CSlfStr *)pval;
        CSlfStr *pstrn;

        pI->NRowGet    = 0;
        pI->NColGet    = 0;
        pI->NVecGet    = 0;

        // Variable löschen falls besetzt
        if( pI->pValGet )
            deleteInputVal(&pI->pValGet,&pI->TypeGet);

        pstrn   = new CSlfStr;
        *pstrn = pstr->c_str();

        pI->pValGet = (void *)pstrn;
        pI->TypeGet = DEF_STRING;
        }
        
        break;
    case DEF_VEC:  // Wird in TAbelle gewandelt
        {

        double *py = (double *)pval; //y-Vektor
        double *px = new double[ncols*nrows]; // x-Vektor

        uint32_t n = ncols*nrows;
        for(i=0;i<n;i++) 
            px[i] = pI->TimeStart
                  + (pI->TimeEnd - pI->TimeStart) * i / (n-1);

        CSlf1DTab *p1dtab = new CSlf1DTab;

        if( pI->p1DTabGet )
            delete pI->p1DTabGet;

       // Tabelle füllen
        if( p1dtab->set(pI->Name.c_str()
                       ,""
                       ,"time"
                       ,pI->TimeUnit.c_str()
                       ,""
                       ,px
                       ,n
                       ,pI->Name.c_str()
                       ,pI->UnitGet
                       ,""
                       ,py
                       ,n
                       ,1) != OKAY ) {

                     ErrText.catFormat(
                             "CDsCtrlIO::readInput1DTable Probleme mit CSlf1DTab mit"
                             " Tabelle: <%s> xvec: <%s> yvec: <%s>\n"
                            ,pI->Name.c_str()
                            ,"time"
                            ,pI->Name.c_str());
                     ErrText.cat(p1dtab->getErrText());
                     p1dtab->resetErrText();

                    Status = NOT_OKAY;
                    return Status;
        }
        delete px;

        pI->p1DTabGet  = p1dtab;        
        
        pI->TypeGet = DEF_1D_TAB;
        
        // Einheitenkonvertierung
        if( pI->p1DTabGet->convXVec(pI->TimeUnit.c_str()) != OKAY ) {

            ErrText.cat(pI->p1DTabGet->getErrText());
            pI->p1DTabGet->resetErrText();

            Status = NOT_OKAY;
            return Status;
        }
        if( pI->p1DTabGet->convYVec(pI->Unit.c_str()) != OKAY ) {

            ErrText.cat(pI->p1DTabGet->getErrText());
            pI->p1DTabGet->resetErrText();

            Status = NOT_OKAY;
            return Status;
        }
        }
        break;
    case DEF_1D_TAB:
        {
        CSlf1DTab *p1dtab = (CSlf1DTab  *)pval;


        if( pI->p1DTabGet )
            delete pI->p1DTabGet;

        pI->p1DTabGet = p1dtab;

        pI->NRowGet    = 1;
        pI->NColGet    = 1;
        pI->NVecGet    = 1;


        // Einheitenkonvertierung
        if( pI->p1DTabGet->convXVec(pI->TimeUnit.c_str()) != OKAY ) {

            ErrText.cat(pI->p1DTabGet->getErrText());
            pI->p1DTabGet->resetErrText();

            Status = NOT_OKAY;
            return Status;
        }
        if( pI->p1DTabGet->convYVec(pI->Unit.c_str()) != OKAY ) {

            ErrText.cat(pI->p1DTabGet->getErrText());
            pI->p1DTabGet->resetErrText();

            Status = NOT_OKAY;
            return Status;
        }

        }
        break;
    }

    return Status;

}
// Löscht Inputstruktur
//=======================================================
void CDsCtrlIO::deleteInputList() {

    SDsCtrlIOInp  *pInp;
    
    while( pIOInp != 0 ) {

        pInp = pIOInp->pNext;

        deleteInputVal(&pIOInp->pValGet,&pIOInp->TypeGet);
        delete pIOInp;
        pIOInp = pInp;
    }
}
// Löscht aus einer Inputvariablen den Wert(e)
//===============================================================
void CDsCtrlIO::deleteInputVal(void **ppv, EVarType *ptype) {

    

    if( *ppv) {

        switch(*ptype) {

        case DEF_DOUBLE:
        case DEF_VEC:
            {
            double *pd = (double *)*ppv;
            
            delete []pd;
            }
            break;
        case DEF_STRING:
            {
            CSlfStr *pstr = (CSlfStr *)*ppv;
            
            delete pstr;
            }
        }
        *ppv   = 0;
        *ptype = DEF_VOID;
    }
}
#endif

// Überprüft, ob alle rqeuested Inputs belegt sind
// und Passen
//================================================
status_t CDsCtrlIO::proofInput(void) {

    SDsCtrlIOInp *pI = pIOInp;

    while( pI ) {

        if( !pI->InternVarInExternFound ) {

            ErrText.catFormat("CDsCtrlIO::proofInput: Die angeforderte Variable <%s> hat keine Zurdnung, d.h. der Input ist nicht besetzt !!\n"
                         ,pI->Name.c_str());
            Status = NOT_OKAY;
            return Status;

        }

        pI = pI->pNext;
    }
    InputProofedFlag = 1;

    return Status;
}
// Input zum aktuellen Zeitpunkt setzen
//=========================================
status_t CDsCtrlIO::setInput(double tact) {

    SDsCtrlIOInp *pI = pIOInp;

    if( UseInputTimeStruct )
    {
      Vector_t *pvec = (Vector_t *)InputTimeStruct.pValExtern;
      Vector_t vec   = *pvec;

      // Index und Faktoren aus dem Zeitvektor bestimmen
      SlfNumInterp0D(vec
                    ,InputTimeStruct.NDimExtern
                    ,tact
                    ,&index0TimeStruct
                    ,&index1TimeStruct
                    ,&factor0TimeStruct
                    ,&factor1TimeStruct);

      while( pI ) {

	      switch(pI->TypeExtern) {

          case DEF_DOUBLE:
          case DEF_FLOAT:
          case DEF_SIGNED_LONG:
          case DEF_UNSIGNED_LONG:
          case DEF_SIGNED_SHORT:
          case DEF_UNSIGNED_SHORT:
          case DEF_SIGNED_CHAR:
          case DEF_UNSIGNED_CHAR:

            switch(pI->TypeIntern) {
              case DEF_DOUBLE:
              case DEF_FLOAT:
              case DEF_SIGNED_LONG:
              case DEF_UNSIGNED_LONG:
              case DEF_SIGNED_SHORT:
              case DEF_UNSIGNED_SHORT:
              case DEF_SIGNED_CHAR:
              case DEF_UNSIGNED_CHAR:

              SlfFktConvertSingle(pI->pValExtern
                                 ,pI->TypeExtern
                                 ,pI->pValIntern
                                 ,pI->TypeIntern
                                 ,pI->FactorExtToInt
                                 ,pI->OffsetExtToInt);
              break;
              default:
              ErrText.catFormat("CDsCtrlIO::setInput: Für die angeforderte Variable <%s> gibt es keine Zurdnung, der Typ passt nicht !!\n"
                               ,pI->Name.c_str());
              Status = NOT_OKAY;
              return Status;
              break;
            }
            break;

          case DEF_VEC:
            {
            Vector_t vec = *(Vector_t *)pI->pValExtern;
            double val = vec[MIN(pI->NDimExtern,index0TimeStruct)] * factor0TimeStruct
                       + vec[MIN(pI->NDimExtern,index1TimeStruct)] * factor1TimeStruct;

            switch(pI->TypeIntern) {
              case DEF_DOUBLE:
              case DEF_FLOAT:
              case DEF_SIGNED_LONG:
              case DEF_UNSIGNED_LONG:
              case DEF_SIGNED_SHORT:
              case DEF_UNSIGNED_SHORT:
              case DEF_SIGNED_CHAR:
              case DEF_UNSIGNED_CHAR:

              SlfFktConvertSingle(&val
                                 ,DEF_DOUBLE
                                 ,pI->pValIntern
                                 ,pI->TypeIntern
                                 ,pI->FactorExtToInt
                                 ,pI->OffsetExtToInt);
              break;
              default:
              ErrText.catFormat("CDsCtrlIO::setInput: Für die angeforderte Variable <%s> gibt es keine Zurdnung, der Typ passt nicht !!\n"
                               ,pI->Name.c_str());
              Status = NOT_OKAY;
              return Status;
              break;
            }
            }
            break;
          case DEF_1D_TAB:
            {
              CSlf1DTab *p1dtab = (CSlf1DTab *)pI->pValExtern;
              p1dtab->get(tact,(double *)pI->pValIntern);
            }
              break;
          default:
              ErrText.catFormat("CDsCtrlIO::setInput: Für die angeforderte Variable <%s> gibt es keine Zurdnung, der externe Typ passt nicht !!\n"
                               ,pI->Name.c_str());
              Status = NOT_OKAY;
              return Status;
              break;

          }
          pI = pI->pNext;
      }


    }


    while( pI ) {

	    switch(pI->TypeExtern) {

        case DEF_DOUBLE:

            SlfFktConvertSingle(pI->pValExtern
                               ,pI->TypeExtern
                               ,pI->pValIntern
                               ,pI->TypeIntern
                               ,pI->FactorExtToInt
                               ,pI->OffsetExtToInt);
            break;

        case DEF_1D_TAB:

            CSlf1DTab *p1dtab = (CSlf1DTab *)pI->pValExtern;
            p1dtab->get(tact,(double *)pI->pValIntern);
        }
        pI = pI->pNext;
    }
    return OKAY;
}
