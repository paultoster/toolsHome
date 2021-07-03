// DsCtrl.h
//
// Steuerung der Modelle 
//
#ifndef DS_CTRL_IO_H_INCLUDED
#define DS_CTRL_IO_H_INCLUDED

#include "SlfBasic.h"
#include "SlfStr.h"
#include "SlfTab.h"
#include "DsMod.h"

#define DS_CTRL_IO_MEX             1

//#define DS_CTRL_IO_MEX_INIT        1
#define DS_CTRL_TEXTLENGTH         255

#if DS_CTRL_IO_MEX
#include "mex.h"
#endif

struct SDsCtrlIOOut {

	CSlfStr             Name;
  CSlfStr             Unit;
  EVarType            TypeGet;
	void                *pValGet;
  EVarType            RefType;

  void                *pVal;
  EVarType            Type;
  uint32_t            NDim;


  SDsCtrlIOOut       *pNext;

};
struct SDsCtrlIOInp { // Intern ist der angeforderte Input von der Input-Modell-Liste
                      // Extern  ist der gefundene Input-Vektor von aussen
	  CSlfStr             Name;               // Variablennamen
    CSlfStr             UnitIntern;            // angeforderte Einheit
    EVarType            TypeIntern;            // Variablentyp
    void                *pValIntern;           // Pointer, wenn angeford. Variable schon angelegt
    CSlfStr             CommentIntern;         // Kommentar von angeforderter Variable
    uint32_t            NDimIntern;            // Anzahl angeforderter Elemente
    uint32_t            NRowIntern;            // Anzahl Zeilen
    uint32_t            NColIntern;            // Anzahl Spalten
    CSlfStr             XUnitIntern;           // von externen Input erhaltene Einheit
    CSlfStr             XNameIntern;           // von externen Input erhaltene x-Name
    CSlfStr             YNameIntern;           // von externen Input erhaltene y-Name
    CSlfStr             XCommentIntern;        // von externen Input erhaltene x-Kommentar
    CSlfStr             YCommentIntern;        // von externen Input erhaltene y-Kommentar
    uint8_t             BuildInternVecByExternSize;  // Soll Variable gebildet(Speicher anlegen) werden
                                                     // (gilt für Vector_t/Matrix_t, wennn pVal=0)

    uint8_t            InternVarInExternFound;      // gibt an, ob im angeforderte Variable (Request)
                                           // in externen Input gefunden wurde (Get)
    CSlfStr            UnitExtern;            // von externen Input erhaltene Einheit
    EVarType           TypeExtern;            // externer Type
    CSlfStr            CommentExtern;         // externer Kommentar
	  void               *pValExtern;           // externer Pointer, wenn z.B zeitabhängige (Tabelle z.B.)                            
    uint32_t           NDimExtern;            // Anzahl externer Elemente
    uint32_t           NRowExtern;            // Reihen
    uint32_t           NColExtern;            // Spalten
    CSlfStr            XUnitExtern;           // von externen Input erhaltene Einheit
    CSlfStr            XNameExtern;           // von externen Input erhaltene x-Name
    CSlfStr            YNameExtern;           // von externen Input erhaltene y-Name
    CSlfStr            XCommentExtern;        // von externen Input erhaltene x-Kommentar
    CSlfStr            YCommentExtern;        // von externen Input erhaltene y-Kommentar

    double             FactorExtToInt;            // Faktor Umrechnung von Get -> Req
    double             OffsetExtToInt;            // Offset Umrechnung von get -> Req
    double             XFactorExtToInt;           // Faktor Umrechnung von Get -> Req
    double             XOffsetExtToInt;           // Offset Umrechnung von get -> Req

    uint8_t             ReqTimeVal;         // gibt an, ob zeitabhängiger Wert
    CSlfStr             TimeUnit;           // angeforderte Zeiteinheit
    void                **ppReqVal;         // Pointer des angfeorderten Pointers, Variable muß
                                            // angelegt werden
    uint8_t             ReqPtrIsHereBuild;  // Soll Variable an sich gebildet werden
                                            // z.B. Typ DEF_DOUBLE_PTR
    uint8_t             ReqOrder;           // Ordnung in Tabelle 0:step,1:linear



    SDsCtrlIOInp       *pNext;             // verketteter Pointer

};

// 
// Modellhandling
class CDsCtrlIO {
public:
  SDsCtrlIOInp     InputTimeStruct; // seperate Struktur, um Zeitvektor, der von aussen kommt zu belegen
  CDsCtrlIO();
  ~CDsCtrlIO();

  void reset(void);

  status_t buildOutputVector(SDsModVar *output
                            ,uint32_t nval
                            ,uint32_t nout);


  status_t buildOutputVector(char *name
                            ,char *unit
                            ,char *comment
                            ,EVarType type
                            ,void *pval
                            ,uint32_t nout);
#if DS_CTRL_IO_MEX
  status_t setMexOutputVector(mxArray **ppout);
#endif
  void setOutput(uint32_t iout);

  // erstellt eine eine Struct für InputListe mit den gewünschten Inputs aus dem Modell
  status_t buildInput(SDsModVarInfo *pinp);
  // erstellt eine neu Struktur, wenn mit dem Namen noch nicht vorhanden
  // und gibt Struktur zurück
  SDsCtrlIOInp *buildInputStruct(char *name);

  // Die Zeit struktur InputTimeStruct wird aktiviert, da von aussen mit einem Zeitvektor belegt wird
  // Name und Einheit wird gesetzt
  status_t setInputTimeStruct(char *name,char *unit,char *comment);
#if DS_CTRL_IO_MEX
  // sucht nach Zeitvektor und in der gleichen Länge die passenden
  // (nach Inputstruktur) Vektoren
  status_t getMexInputVectorWithTimeVector(const mxArray *pin);
#endif
  status_t setInput(double tact);
  status_t proofInput(void);

#if 0
  status_t regInput1DTable(char      *tabname     // Tabellenname
                          ,char      *comment     // Kommentar
                          ,char      *xunit       // x-Einheit
                          ,char      *yunit       // y-Einheit
                          ,uint8_t     order        // 0/1
                          ,CSlf1DTab *p1dtab      // Tabellenpointer
                          );
                                
  status_t regInputSingleTime(char *timeunit
                             ,char *name
                             ,char *unit
                             ,char *comment
                             ,EVarType type
                             ,void *pval);
#endif

  void deleteInput(void);
  void deleteInput(SDsCtrlIOInp *pI);
  void deleteOutput(void);

  status_t getStatus(void) {return Status;}

  uint32_t   getLenErrText(void) {return ErrText.getLen();}
  char *   getErrText(void) {return ErrText.c_str();}
  void     resetErrText(void) {ErrText.clear();}

  uint32_t   getLenLogText(void) {return LogText.getLen();}
  char *   getLogText(void) {return LogText.c_str();}
  void     resetLogText(void) {LogText.clear();}

protected:

#if DS_CTRL_IO_MEX
    bool isInputMexValue(const mxArray *par);
    status_t readInputMexValue(const mxArray *par,const char *cname,SDsCtrlIOInp *pI);
#endif
    status_t readInputSingleData(CSlfStr *pname
                                 ,CSlfStr *punit
                                 ,CSlfStr *pcomment
                                 ,double  dval);
    status_t readInputStringData(CSlfStr *pname
                                 ,CSlfStr *pcomment
                                 ,CSlfStr *pstr);
    status_t readInputVectorData(CSlfStr *pname
                                 ,CSlfStr *punit
                                 ,CSlfStr *pcomment
                                 ,double  *pvec
                                 ,uint32_t  nvec);
    
    status_t readInput1DTableData(CSlfStr  *ptabname     // Tabellenname
                                 ,CSlfStr  *pcomment     // Kommentar
                                 ,CSlfStr  *pxname       // Variablenname x
                                 ,CSlfStr  *pxunit       // Einheit x
                                 ,CSlfStr  *pxcomment    // Kommentar x
                                 ,double   *pxvec        // Vektor x
                                 ,uint32_t   nxvec         // Länge Vektor x
                                 ,CSlfStr  *pyname       // Variablenname y
                                 ,CSlfStr  *pyunit       // Einheit x
                                 ,CSlfStr  *pycomment    // Kommentar y
                                 ,double   *pyvec        // Vektor y
                                 ,uint32_t   nyvec         // Länge Vektor y
                                 ,uint8_t    order         // order 1:Linear /0:Step
                                 );

    status_t readInputData(CSlfStr     *pvn     // Variablenname
                           ,CSlfStr     *pu        // Einheit
                           ,CSlfStr     *pcom     // Kommentar
                           ,EVarType    type      // DatenType
                           ,uint16_t      nrows     // Anzahl Reihen
                           ,uint16_t      ncols     // Anzahl Spalten
                           ,void        *pval     // Pointer Wert
                           );

    void deleteInputVal(void **ppv, EVarType *ptype);


  status_t          Status;                // Status OKAY/NOT_OKAY
  CSlfStr           ErrText;               // Fehlertext
  CSlfStr           LogText;               // fürs Logfile gedacht


  SDsCtrlIOOut     *pIOOut;         // Pointer mit Liste externen Ausgaben
  SDsCtrlIOInp     *pIOInp;         // Pointer mit Liste externen Eingaben

  uint8_t          UseInputTimeStruct;
  uint32_t         index0TimeStruct,index1TimeStruct;
  double           factor0TimeStruct,factor1TimeStruct;
  uint8_t          InputProofedFlag;


    //Schlüsselwörter für Mexinput
    char *TYPE;
    char *NAME;
    char *VAL;
    char *UNIT;
    char *COMMENT;
    
    char *XNAME;
    char *XVEC;
    char *XUNIT;
    char *XCOMMENT;
    char *YNAME;
    char *YVEC;
    char *YUNIT;
    char *YCOMMENT;
    char *ZNAME;
    char *ZMAT;
    char *ZUNIT;
    char *ZCOMMENT;
    char *ORDER;

    char *TYPE_SINGLE[2];
    char *TYPE_STRING;
    char *TYPE_VECTOR;
    char *TYPE_MATRIX;
    char *TYPE_1DTAB[2];


};
#endif