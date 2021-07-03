// DsMod.h
//
// Steuerung der Modelle 
//


// 
#define DS_MOD_MAX_ITERATION 20

#define regStaPIE(pfunc,vname,vcomment,vx,vxdot,vx0,mjacobi,mjacobi_konst) \
    regSta(pfunc,DEF_PIEULER,vname,vcomment,vx,vxdot,vx0,mjacobi,mjacobi_konst,0)
#define regStaIE(pfunc,vname,vcomment,vx,vxdot,vx0,mjacobi,mjacobi_konst,verr) \
    regSta(pfunc,DEF_IEULER,vname,vcomment,vx,vxdot,vx0,mjacobi,mjacobi_konst,verr)

//==============================================================================
//==============================================================================

// Registrierliste Variable Input/Output
//
typedef struct SDsRegVar_tag {
  CStr                 ModName;     // Name
  CStr                 VarName;     // Name
  void                    *pVal;    // Wert
  enum EVarType           Type;        // Typ
  CStr                    Unit;     // Unit
  CStr                    Comment;  // Comment

  SDsRegVar_tag    *pNext;

} SDsRegVar;

typedef struct SDsRegFkt_tag {
  CStr                    ModName;     // Name
  CStr                    VarName;     // Name
  CDsModBase              **ppFkt;    // Wert
  CStr                    Comment;  // Comment

  SDsRegFkt_tag    *pNext;

} SDsRegFkt;

// Registrierliste Zustände (EULER)
//
#if DS_MOD_NEW_STA == 0
typedef struct SDsRegSta_tag {

  CStr                    VarName;     // Name
  double                  *pStaVal;    // Wert
  double                  *pStaValDer;    // Wert
  double                  *pStaValIni;    // Wert
  enum EModStaType        IntType;     // Integrationstyp
  double                  MaxError;    // Iterationsfehler
  CStr                    Comment;  // Comment

  CStr   V                VStaName;     // VEktor mit Namen
  CStr   V                VStaComment;
  Vector_t                  VSta;         // Vektor mit Zustandsvars
  Vector_t                  VStaDer;      // Vektor mit Ableitung Zustandsvars
  Vector_t                  VStaIni;      // Vektor mit Initialisierung Zustandsvars
  Matrix_t                  MJacobi;      // Jacobi-Matrix_t
  uint8_t                   FlagMJacobi;  // ist Jacobimatrix konstant
  Vector_t                  VError;       // Fehler-Vektor

  SDsRegSta_tag    *pNext;

}SDsRegSta;
#else
typedef struct SDsRegSta_tag {

  enum EModStaType        IntType;     // Integrationstyp
  uint32_t                  NSta;        // Anzahl Zustände
  uint8_t                   Err;         // Fehler bei Registrierung
  CStr   V                VStaName;    // VEktor mit Namen
  CStr   V                VStaComment;
  Vector_t                  VError;       // Fehler-Vektor
  Vector_t                  VState;      // Zustandsvektor
  double                  Dt;          // vorgegebnene Schrittweite

}SDsRegSta;
#endif


// Registrierliste
//================
typedef struct SDsRegInfo_tag {

  CStr        Name;			// Name des Moduls
  CDsModBase  *pFunc;		// Funktionspointer


  SDsRegVar       *pInp;
  SDsRegVar       *pOut;
  
#if DS_MOD_NEW_STA == 0
  SDsRegSta       *pSta;     // verkettete Liste 
#else
  SDsRegSta        Sta;      // Zustandsstruktur
#endif

  uint8_t           ParSet;		// Flag, ob Parameter gesetzt sind

  SDsRegFkt       *pFkt;    // Funktionspointer
  
  SDsRegInfo_tag  *pNext;

} SDsRegInfo;

//==============================================================================
//==============================================================================

// Parameterstrucktur
typedef SDsParVar SDsModParVar;
typedef SDsParTab SDsModParTab;
// Output
//typedef SDsVar SDsModVar;
// State
//typedef SDsSta SDsModSta;
//==============================================================================
//==============================================================================

//Variablennamen um den Namen zu wechseln
struct SDsModVarNameChange{
  char      *ModName;
  char      *ISPOType;
  char      *VarName;
  char      *NewVarName;
};
//==============================================================================
//==============================================================================


// interne Variablenliste:
//========================
struct SDsModVarInfo {
  CStr                    Name;       // Name
  void                    *pVal;      // Pointer des Werts
  enum EVarType           Type;       // Typ
  CStr                    Unit;       // Unit
  CStr                    Comment;    // Comment

  CStr                    OldName;    // alter Name
  void                    *pValGet;   // Pointer der Variable, 
                                      // von der Wert bezogen wird
  enum EVarType           TypeGet;    // Typ
  double                  FaktorGet;  // Umrechenfaktor
  double                  OffsetGet;  // Offset
  uint16_t                  IModelGet;  // Modell index
  uint16_t                  JVarGet;    // Variablen (Out) index
  uint8_t                   LinInter;   // nur für extern Input, soll linear Interpoliert werden
  double                  DVal[3];    // alter[0], interpolierter[1] und delta-Wert[2] double Wert, nur externer Inpu
  void                    *pValExtInp;  // Pointer des Werts, wenn externer Input
  enum EVarType           TypeExtInp; // Typ des externen Inputs;
};
// interne ZustandsVariablenliste:
//================================
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten   
struct SDsModStaInfo {
  CStr                    Name;          // Name
  double                  *pStaVal;      // Pointer des Zustandswert aktuell
  double                  *pStaValDer;   // Pointer des Zustandswert Ableitung
  double                  *pStaValIni;   // Pointer des Zustandswert Ini-Wert
  CStr                    Comment;       // Comment

  CStr                    OldName;     // alter Name
  EModStaType             IntType;     // Integrationstyp
  uint8_t                   MaxIter;    // maximaler Iteration
  uint8_t                   NIter;      // aktuelle Anzahl Iteration

  uint8_t                   ActIterFlag;// aktuelle Iterationsflag
  double                  MaxError;   // maximaler Iterationsfehler
};
#else
typedef struct SDsModStaInfo_tag {

  CSlfStrV        VStaName;
  CSlfStrV        VStaComment;

  EModStaType     IntType;     // Integrationstyp
  Vector_t          VError;
  Vector_t          VState;

  CIntegratorBase     *pIntFunc;

} SDsModStaInfo;

#endif
// Funktionspointer-Info
//======================
struct SDsModFktInfo {
  CStr                    Name;       // Name
  CDsModBase              **ppFkt;      // Pointer auf Funktionspointer
  CStr                    Comment;    // Comment

};
// interne Modellliste:
//====================
struct SDsModInfo {

  // Modell
  CStr        Name;
  CDsModBase  *pFunc;

  //IOs
  uint16_t          NInp;
  SDsModVarInfo   *pInp;
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten   
  uint16_t          NSta;
  SDsModStaInfo   *pSta;
  
  double          *pStaDerVal0;         // alter Zustandsänderung
  double          *pStaVal0;            // alter Zustand
  EModStaType     IntType;           // höchster Integrationstyp
  uint8_t           MaxIter;          // maximale Iteration
  uint16_t          IterFlagSum;      // Die Summe aller zu iterierenden Zustände 
  uint16_t          CntDownIFSum;     // Variable zum runterzählen

  CSlfStrV        VStaName;
  CSlfStrV        VStaComment;
  Vector_t          VSta;       // Vektorenbeschreibung für PIEULER/IEULER
  Vector_t          VSta0;      // alter Zustand
  Vector_t          VStam1;      // alter Zustand
  Vector_t          VStam2;      // alter Zustand
  Vector_t          Vdelta;
  Vector_t          VStaDer;
  Vector_t          VStaDer0;
  Vector_t          VStaIni;
  uint16_t          Niter;       // runtergezählte Iterationen

  Matrix_t          MJacobi;     // Jacobimatrix für TIEULER
  Matrix_t          InvMat;      // Ergebnis von (E-h*Jacobi)^-1
  Matrix_t          InvMatOld;
  uint8_t           FlagMJacobi; // Jacobimatrix konstant
  Vector_t          VError;
#else
  uint32_t          NSta;   // Anzahl komplexer Zustände
  SDsModStaInfo   *pSta;
#endif



  uint16_t          NOut;
  SDsModVarInfo   *pOut;

  uint16_t          NFkt;
  SDsModFktInfo   *pFkt;

  double          TimeAct;     // Modellbezogene Zeit
  double          Dt;
};



// Modellhandlingsklasse
//======================
class CDsMod {
public:

  CDsMod(CDsPar *ppar=0);
  CDsMod(CSlfLogFile *plogfile,CDsPar *ppar=0);
  ~CDsMod();

  // Register Function
  SDsRegInfo *regModul(CDsModBase *pfunc,char *name="__new_name_not__");
  SDsRegInfo *regFkt(CDsModBase *pfunc,char *name="__new_name_not__");
  void       regInp(CDsModBase *pfunc,SDsModVar *input,uint16_t ninp);
  void       regOut(CDsModBase *pfunc,SDsModVar *output,uint16_t nout);

#if DS_MOD_NEW_STA == 0
  void       regSta(CDsModBase *pfunc,SDsModSta *psta,uint16_t nsta);
  void       regSta(CDsModBase *pfunc,enum EModStaType type
                   ,CSlfStrV &vname, CSlfStrV &vcomment
                   ,Vector_t vx,Vector_t vxdot,Vector_t vx0
                   ,Matrix_t mjacobi,uint8_t mjacobi_konst
                   ,Vector_t verr);
#else
  // Zustände anmelden
  //==================
  void       regSta(CDsModBase  *pfunc              // Funktionspointer
                   ,Vector_t      ystate                 // Zustandsvektor
                   ,char        *name[]=0           // Liste mit Namen (kann auch NULL sein)
                   ,char        *comment[]=0        // Liste mit Kommentaren
                   ,double      err[]=0             // Liste mit Fehlergröße
                   ,EModStaType inttype=DEF_step    // Integrationstype siehe DsMOdBase.h
                   ,double      dt=-1.              // Vorgabe Schrittweite
                   );
#endif
  void       regPar(CDsModBase *pfunc,SDsModParVar *parlist,uint16_t npar,uint8_t nodefault=0);
  void       regPar(CDsModBase *pfunc,SDsModParTab *parlist,uint16_t npar,uint8_t nodefault=0);
  void       regParDecideGroup(CDsModBase *pfunc,char *group,CSlfStrV &subgrouplist,char *switchpar,char *defvalue,uint8_t nodefault=0);
  void       regPointerToFkt(CDsModBase *pfunc,SDsModFkt *pfkt,uint16_t nfkt);
    
  status_t ini(CSlfLogFile *plogfile=0);
  status_t setup(SDsModVar *ext_inp_list,            uint16_t n_ext_inp
                ,SDsModVar *ext_out_list,            uint16_t n_ext_out
                ,SDsModVarNameChange *change_list, uint16_t n_change_list);
  status_t init(double dt);
  status_t first(double t0);
  status_t loop();
  status_t done();

  uint16_t        getNExtOut(void);
  SDsModVarInfo *getExtOut(uint16_t iout);

  uint16_t        getNExtInp(void);
  SDsModVarInfo *getExtInp(uint16_t iout);

  status_t getStatus(void) {return Status;}
  uint8_t    getLogFileSet(void) {return LogFileSet;}
  char *   getLogFileName(void){if( pLogFile )return pLogFile->getLogFileName();else return "";}

  double   getTime(void) {return TimeAct;}
  
  uint32_t   getLenErrText(void) {return ErrText.getLen();}
  char *   getErrText(void) {return ErrText.c_str();}
  void     resetErrText(void) {ErrText.clear();}

  uint32_t   getLenLogText(void) {return LogText.getLen();}
  char *   getLogText(void) {return LogText.c_str();}
  void     resetLogText(void) {LogText.clear();}

protected:

  status_t          Status;                // Status OKAY/NOT_OKAY
  CStr              ErrText;               // Fehlertext
  CStr              LogText;               // fürs Logfile gedacht

  CSlfLogFile       *pLogFile;             // Logfilehandling
  uint8_t             LogFileSet;            // Sagt ob LogFile geschrieben wird
  
  SDsModInfo        *pModList;             // Modellliste
  uint16_t            NModels;               // Anzahl der Modell einschl. IO
  uint16_t            NFkts;                 // Anzahl der Funktionen

  CDsPar            *pPar;                 // Parameterklasse
  uint8_t             ParSet;                // gesetzt, wenn pParübergeben wurde

  SDsRegInfo        *pRegListMod;          // Registrierungsliste Modells
  SDsRegInfo        *pRegListFkt;          // Registrierungsliste Funktionen

  double            TimeAct;               // aktuelle Zeit
  double            TimeOld;               // alter Zeitwert
  double            DtMast;                // Master Schrittweite
  double            DtModMin;              // kleinste Schrittweite der Modelle
  uint32_t            NDt;                   // Anzahl der Schleifen pro Master Zeitschritt

#if DS_MOD_NEW_STA == 0 // alte Möglichkeiten 
  double            DtMast05;              // halbe Master Schrittweite
#endif
  uint8_t             MaxIter;               // maximale Iterationsschrittweite
  uint16_t            IterFlagSum;           // Die Summe aller zu iterierenden Modell 
  uint16_t            CntDownIFSum;          // Variable zum runterzählen

  uint8_t             DoneFlag;              // Variable um zu verhindern done zweimal auszuführen
  uint8_t             BuildFlag;             // Variable, die den abgeschlossenen uild-Prozess angebibt

  double            WertNull;              // Dummywert

  // Anzahl der angemeldeten Modelle
  uint16_t           getRegNMod(void);
  // Anzahl der angemeldeten Funktionen;
  uint16_t           getRegNFkt(void);
  // Namen der Modelle checken
  status_t         checkRegModName(void);
  // Funktionspointer Modul
  CDsModBase      *getRegModPointer(uint16_t ifunc);
  // Funktionspointer Funktion
  CDsModBase      *getRegFktPointer(uint16_t ifunc);
  // Modellname
  char            *getRegModName(uint16_t ifunc);
  // Funktionsname
  char            *getRegFktName(uint16_t ifunc);
  // Anzahl Input ites Modell
  uint16_t           getRegNInp(uint16_t imod);
  // Inputstruktur ites Modell
  SDsRegVar       *getRegInpStruct(uint16_t iimod,uint16_t jinp);
#if DS_MOD_NEW_STA == 0
  uint16_t           getRegNSta(uint16_t ifunc);
  uint8_t            proofRegStaTypes(uint16_t ifunc);
  SDsRegSta       *getRegStaStruct(uint16_t ifunc,uint16_t jsta);
#else
  uint32_t           getRegNSta(uint16_t ifunc);
  uint8_t            proofRegStaETypes(uint16_t ifunc);
  SDsRegSta       *getRegStaStruct(uint16_t ifunc);
#endif
  enum EModStaType getRegStaType(uint16_t ifunc);
  uint16_t           getRegNOut(uint16_t ifunc);
  SDsRegVar       *getRegOutStruct(uint16_t ifunc,uint16_t jout);

  uint16_t           getRegNFkt(uint16_t ifunc);

  // Funktionspointer von angemeldeten Funktionen (als IO)
  //======================================================
  SDsRegInfo      *findRegFkt(CDsModBase *pfunc);
  SDsRegFkt       *getRegFktStruct(uint16_t ifunc,uint16_t jfkt);
  
  void             destroyRegMod(void);
  void             destroyRegFkt(void);
  

  char *findChangeName( char *ISPOType
                      , char *mod_name
                      , char *var_name
                      , SDsModVarNameChange **pchange_list
                      , uint16_t n_change_list);
  // Sucht und verbindet alle Inputs (externer Output wird wie Input behandelt)
  status_t connectInpVar(void);
  // Sucht und verbindet alle gesuchten Funktionspointer
  status_t connectFkt(void);
  //void     setExtInp(void);
  void     setInpVar(SDsModInfo *pMod);
#if DS_MOD_NEW_STA == 0
  void     setStaIni(void);
#endif
  void     setExtOutVar(void);
  void     setVar(SDsModVarInfo   *pVar);
  status_t findVarOut(char *name,uint16_t imod0,uint16_t *im,uint16_t *io);
  status_t proofType(EVarType TypeI,EVarType TypeO);

  status_t buildMod(SDsModVar *ext_inp_list,            uint16_t n_ext_inp
                   ,SDsModVar *ext_out_list,            uint16_t n_ext_out
                   ,SDsModVarNameChange *change_list, uint16_t n_change_list);
  status_t buildModSta(SDsModInfo *pMod
					  ,uint16_t index
					  ,SDsModVarNameChange *change_list
                      ,uint16_t n_change_list);
  status_t listMod(bool fmod=false, bool finp=false, bool fsta=false, bool fout=false , bool fextinp=false, bool fextout=false );
  void     destroyMod(void);
  void     initModList(void);

  status_t initDt(double dt);
  status_t calcInitMod(void);
  status_t calcInitFkt(void);
  

  status_t calcFirstMod(void);
#if DS_MOD_NEW_STA == 0 // altre Möglichkeiten 
  status_t calcStaMod(SDsModInfo *pMod);
#else
  status_t calcPreStaMod(SDsModInfo *pMod);
#endif
  status_t calcInvertDMatrix(SDsModInfo *pMod);
  status_t calcOutMod(SDsModInfo *pMod);

  status_t calcDoneMod(void);
  status_t calcDoneFkt(void);
  

  status_t calcMod(double timeact);
  void calcExtInp(uint8_t init);        // Berechnet externen Input 
                                      // init=2 erster Aufruf bei First, zum Initialisieren
                                      //     =1 erster Aufruf bei einer neuen externenn Loop
                                      //        um neue Steigung von letztem zu neuem Endwert
                                      //        zu generieren
                                      //     =0 Berechnung aktuellen Werts

  void writeLogFile(char *name);

};




#endif