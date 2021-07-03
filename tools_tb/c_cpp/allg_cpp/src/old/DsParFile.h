// DsPar.h
// Parameter einlesen
// Struktur für Parametereingabe
//
// Grupen
// ======
// <Instanz>        ! Kennzeichnung einer Instanz, wenn weggelassen automatisch
//                  ! wird instanz <instanz0> gesetzt
// [group]          ! Gruppenhierachie
// (subgroup)       ! Untergruppe
// ((subsubgroup))  ! Unteruntergruppe
// ((( ... )))      ! usw.
// 
// [groupA] = [groupB]           ! Kopie der Gruppe A vollständig Gruppe B
//                               ! gleicher Instanz
// [groupA] = <Instanz1>[groupB] ! vollständige Beschreibung
//
// (sgroup2) = (sgroup3)                   ! Kopie aus gleicher Übergruppe
// (sgroup2) = [groupA](sgroup3)           !       aus Gruppe A
// (sgroup2) = <Instanz1>[groupA](sgroup3) !       aus Instanz 1 und Gruppe A
//
//  Attribute
//
// (groupXY).comment = Kommentar
// (groupXY).copy = [groupA](sgroup3)           !       aus Gruppe A
//
//
//
// Variablen
// =========
//  Varname [unit] = Wert                       ! Einzelwert
//  Varname [unit] = [Wert1,Wert2,Wert3,Wert4]  ! Vektor
//  Varname [unit] = [Wert1,Wert2;Wert3,Wert4]  ! Matrix_t ,: trennt Zeile
//                                                       ;: trennt Spalte
//
//  Attribute
//
//  Varname.copy   = [groupA](sgroup3)Var1name   ! aus Gruppe A kopieren
//  Varname.copy   = Var1name                    ! aus gleicher Gruppe kopieren
//  Varname.factor = Wert                        ! FAktor
//  Varname.offset = Wert                        ! Offset
//                                               ! = Wert*Faktor + Offset
//  Varname.comment = String                     ! Kommentar
//  Varname.unit    = Einheit                    ! Einheit
//  
//
//  Tabelle
//  =======
//
//  {{Tabelle}}                                  ! 1D-Tabelle
//  {{Tabelle}}.comment = Kommentar
//  XVarname [unit] = [,,,]                      ! X-Vektor Reihenfolge beachten
//  YVarname [unit] = [,,,]                      ! Y-Vektor
//
//  {{{Tabelle}}}                                  ! 2D-Tabelle
//  {{Tabelle}}.comment = Kommentar
//  XVarname [unit] = [,,]                       ! X-Vektor Reihenfolge beachten
//  YVarname [unit] = [,,,]                      ! Y-Vektor
//  ZVarname [unit] = [,,,;,,,;,,,]              ! Z-Matrix_t
//
//  Attribute TAbelle
//
//  {{Tabelle}}=[group]{{Tabelle0}}              ! Möglichkeiten Tabelle zu kopieren
//  {{Tabelle}}.copy=[group]{{Tabelle0}}             ! mit Attribute
//  {{Tabelle}}.comment = Kommentar
//
//  Attribute Variable X,Y,Z
//
//  XVarname.copy   = [groupA](sgroup3)Var1name   ! aus Gruppe A kopieren
//  XVarname.copy   = Var1name                    ! aus gleicher Gruppe kopieren
//  XVarname.factor = Wert                        ! FAktor
//  XVarname.offset = Wert                        ! Offset
//                                                ! = Wert*Faktor + Offset
//  XVarname.comment = String                     ! Kommentar
//  XVarname.unit    = Einheit                    ! Einheit
//
//  Spezial Tabelle (time orbit format)
//  ===================================
//  {var1 var2 var3}
//  0 0 10
//  1 2 11
//  2 2 11
//  3 1 12
//  4 0 15
//
//  Entspricht:
//  var1 = 0,1,2,3,4;
//  var2 = 0,2,2,1,0;
//  var3 = 10,11,11,12,15;
//
// Kommentar: Möglichkeiten
// ==========
//
// var.comment = "Das ist ein Kommenatr !!"
// var.comment = "
// Das ist ein Kommenatr !!
// "
//
// Werte: Möglichkeiten
// ======
//
// var = [1,2,3,4;5,6,7,8]
// var = [1,2,3,4;
//        5,6,7,8]
//
// Variablen aus einem File einladen
// Matlab:
// matread(file=v6_dc_4398.mat,variable=omega_Teff)
//
#ifndef DS_PAR_FILE_H_INCLUDED
#define DS_PAR_FILE_H_INCLUDED
//
 
#include <stdio.h>
#include <stdlib.h>
#include "SlfStr.h"
#include "SlfLogFile.h"
#include "DsParData.h"

#define DEVELOP_PAR_FILE         0
#define DUMP_FILE_INPUT_STRUCT   0

#define DS_PAR_FILE_START        0     /* Zustand zu Beginn */
#define DS_PAR_FILE_NORMAL       1     /* normale Zeileninterpretation */
#define DS_PAR_FILE_VAL_MATSTOP  2     /* Suche Werte-Liste, Endung mit mat-stop-zeichen "]" */
#define DS_PAR_FILE_VAL_MATTRENN 3     /* Suche Werte-Liste, Endung  nicht mit "]" */
#define DS_PAR_FILE_TAB_VAL      4     /* Sucht Tabellenwerte oder wieder normale eingabe */
#define DS_PAR_FILE_TAB_MAT      5     /* Sucht nach Matrix_t und Größe in der nächsten Zeile */
#define DS_PAR_FILE_TAB_MAT_CONT 6     /* Sucht nach Matrix_t in der nächsten Zeile */


#define DS_PAR_FILE_MAX_QUOT 12

#define DS_PAR_FILE_NEW_ASSIGNMENT  0
#define DS_PAR_FILE_NEXT_LINE       1


#define DS_PAR_FILE_NVAL_PER_LINE   10
#define DS_PAR_READ_MAX_FILES       10    /* maximale Anzahl von verschachtelten Parameter-Files (#include file)*/

#define DS_PAR_FILE_MAX_VAR_ACT_LIST 10   /* Liste für Spezialtabelle von Variablen, die gefüllt werden */

#if DS_PAR_USE_MATFILELOAD == 1
#include "SlfMat.h"
#endif
enum ESlfFileReadState {
    READ_DEFAULT,
    READ_NEW_INST,
    READ_NEW_LINE,
};


enum EDsParFileReadType {
    IS_UNKOWN,
    IS_EMPTY,
    IS_GROUP,
    IS_GROUP_COPY,
    IS_GROUP_COMMENT,
    IS_VARIABLE,
    IS_VARIABLE_UNIT,
    IS_VARIABLE_FACTOR,
    IS_VARIABLE_OFFSET,
    IS_VARIABLE_COMMENT,
    IS_VARIABLE_COPY,
    IS_VARIABLE_TRANSPOSE,
    IS_1DTABLE,
    IS_1DTABLE_COPY,
    IS_1DTABLE_COMMENT,
    IS_2DTABLE,
    IS_2DTABLE_COPY,
    IS_2DTABLE_COMMENT,
    IS_SPEZTABLE,
    IS_SPEZTABLE_VALUE,
};
#ifdef DS_PAR_FILE_GET_TYPE_NAME_DEFINE
char *DsParFileGetTypeName(EDsParFileReadType type) {

    char *cstr;

    switch(type) {
    case IS_UNKOWN:
        cstr = "IS_UNKOWN";
        break;
    case IS_EMPTY:
        cstr = "IS_EMPTY";
        break;
    case IS_GROUP:
        cstr = "IS_GROUP";
        break;
    case IS_GROUP_COPY:
        cstr = "IS_GROUP_COPY";
    case IS_GROUP_COMMENT:
        cstr = "IS_GROUP_COMMENT";
        break;
    case IS_VARIABLE:
        cstr = "IS_VARIABLE";
        break;
    case IS_VARIABLE_COPY:
        cstr = "IS_VARIABLE_COPY";
        break;
    case IS_VARIABLE_TRANSPOSE:
        cstr = "IS_VARIABLE_TRANSPOSE";
        break;
    case IS_VARIABLE_COMMENT:
        cstr = "IS_VARIABLE_COMMENT";
        break;
    case IS_VARIABLE_UNIT:
        cstr = "IS_VARIABLE_UNIT";
        break;
    case IS_VARIABLE_FACTOR:
        cstr = "IS_VARIABLE_FACTOR";
        break;
    case IS_VARIABLE_OFFSET:
        cstr = "IS_VARIABLE_OFFSET";
        break;
    case IS_1DTABLE:
        cstr = "IS_1DTABLE";
        break;
    case IS_1DTABLE_COPY:
        cstr = "IS_1DTABLE_COPY";
        break;        
    case IS_1DTABLE_COMMENT:
        cstr = "IS_1DTABLE_COMMENT";
        break;        
    case IS_2DTABLE:
        cstr = "IS_2DTABLE";
        break;
    case IS_2DTABLE_COPY:
        cstr = "IS_2DTABLE_COPY";
        break;        
    case IS_2DTABLE_COMMENT:
        cstr = "IS_2DTABLE_COMMENT";
        break;        
    case IS_SPEZTABLE:
        cstr = "IS_SPEZTABLE";
        break;    
    case IS_SPEZTABLE_VALUE:
        cstr = "IS_SPEZTABLE_VALUE";
        break;            
    default:
        cstr = "UNDEFINED";
        break;
    }
    return cstr;
}
#endif

enum EDsParExFileType {
	IS_NON_FILE,
	IS_MAT_FILE
};

struct SDsParReadLineFile {
    uint32_t                      Zeile;
    CSlfStr                     ParFile;
    FILE                        *Fid;
	char                        ret;
};
struct SDsParReadLine {

    CSlfStr                     Tline;
	  SDsParReadLineFile          ReadFile[DS_PAR_READ_MAX_FILES];
	  uint8_t                     iRF;
    uint8_t                     ReadFlag;
    enum EDsParFileReadType     TypeRead;
    enum EDsParFileReadType     TypeNext;
    enum EDsParFileReadType     Type;
    CSlfStr                     GroupName;
    CSlfStr                     Comment;
    uint8_t                       Hierach;
    CSlfStr                     VariableName;
    CSlfStr                     Unit;
    CSlfStrM                    Values;
    CSlfStr                     TableName;
    CSlfStrV                    TableUnit;
    CSlfStrV                    TableValue;
    CSlfStr                     TableOrder;
    CSlfStrV                    SpezTableVar;
    CSlfStrV                    CopyGroup;
    CSlfStr                     CopyVariable;
    uint8_t                       CopyHierach;
    CSlfStr                     CopyTable;
    CSlfStr                     Factor;
    CSlfStr                     Offset;
    CSlfStr                     Transpose;
	EDsParExFileType            FileType;
	CSlfStr                     FileName;
	CSlfStr                     FileVarName;
};

// Parameter-VAriablen-Strukturr
struct SDsParFileVariable {

    CSlfStr         Name;           // NAme
    CSlfStr         Unit;           // EInheit
    EVarType        Type;           // Typ
    CSlfStrM        StrMVal;        // Werte als String in MAtrix Type = DEF_STRINGM
	Vector_t          Vec;            // Vektor mit Doublewerte mit Type = DEF_VEC 
	Matrix_t          Mat;            // Matrix_t mit Doublewerte mit Type = DEF_VEC 
    //void            *pVal;          // Pointer auf Werte
    //CSlf1DTab       *p1DTab;        // Pointer auf 1D-Tabelle
    CSlfStr         Comment;        // Kommentar
    uint32_t          NVal;           // Anzahl der Werte
    uint32_t          NRow;           // Anzahl Reihen
    uint32_t          NCol;           // Anzahl Spalten
    double          Factor;         // FAktor
    uint8_t           FSet;
    double          Offset;         // Offset
    uint8_t           OSet;
    uint8_t           Transpose;       // Transponieren
    uint8_t           TSet;
    char            *ParamFile;     // verwendetes PArameterfile
    uint32_t          ILine;          // Zeile in Parameterfile
    CSlfStr         CopyVariable;
    CSlfStrV        CopyGroup;
    uint8_t           CopyFlag;
	EDsParExFileType FileType;
	CSlfStr 		FileName;
	CSlfStr 		FileVarName;
    
    SDsParFileVariable  *pNext;         // nächster Pointer
};
// Parameter-Tabellen-Strukturr
struct SDsParFileTable {

    CSlfStr         Name;           // NAme
    EVarType        Type;           // Typ
    CSlfStr         Comment;        // Kommentar
    uint32_t          ILine;          // Zeile in Parameterfile

    SDsParFileVariable XVec;
    SDsParFileVariable YVec;
    SDsParFileVariable ZMat;
    uint8_t              VecSet;

    uint8_t           Order;
    uint8_t           OrderSet;

    CSlfStr         CopyTable;
    CSlfStrV        CopyGroup;
    uint8_t           CopyFlag;
    
    SDsParFileTable  *pNext;         // nächster Pointer
};
// Parameter-Gruppen-Struktur
struct SDsParFileGroup {

    CSlfStr         Name;           // Gruppenname
    CSlfStr         Comment;        // Kommentar
    uint32_t          Hierach;        // Hierachie 0,1,2,3, ...
    uint32_t          NSub;           // Anzahl von Untergruppen
    SDsParFileGroup    *pSub;          // Pointer zu untergeordneten Gruppe
    uint32_t          NVar;           // Anzahl von Variablen
    SDsParFileVariable *pVar;          // Pointer zu den Variablen
    uint32_t          NTab;           // Anzahl von Tabellen
    SDsParFileTable *pTab;          // Pointer zu den Tabellen
    CSlfStrV        CopyGroup;
    uint8_t           CopyFlag;

    SDsParFileGroup    *pNext;         // neächster Pointer
};

#if DS_PAR_USE_MATFILELOAD == 1
struct SDsExternFileVarList {

	CSlfStr              Name;
	SDsParFileVariable   *pVar;
	uint8_t                isRead;

	SDsExternFileVarList *pNext;
};
struct SDsExternFileList {


	CSlfStr					FileName;
	EDsParExFileType		FileType;
	SDsExternFileVarList	*pVar;
	uint32_t                  NVar;
	SDsExternFileList		*pNext;
};
#endif
// Parameter-Klasse 
class CDsParFile {

public:
    CDsParFile();
	CDsParFile(CSlfLogFile *plogfile);
    ~CDsParFile();

    void reset(void);

    status_t ini(CSlfLogFile *plogfile=0);

    status_t read(char *par_file,CDsParData &ParData);

    status_t write(CSlfLogFile *plogfile     // Logfileklasse
                  ,CDsParData &ParData);     // Parameterklasse in der gespeichert wird
    status_t write(char *par_file            // Name der Ausgabedatei
                  ,CDsParData &ParData);     // Parameterklasse in der gespeichert wird

    uint32_t   getLenErrText(void) {return ErrText.getLen();}
    char *   getErrText(void) {return ErrText.c_str();}
    void     resetErrText(void) {ErrText.clear();}

private:

    status_t        Status;
    CSlfStr         ErrText;

    uint32_t          NMas;

    CSlfLogFile       *pLogFile;             // Logfilehandling
    uint8_t             LogFileSet;            // Sagt ob LogFile geschrieben wird

    // gesamte Datenstruktur
    SDsParFileGroup    *pInstanz;
    // aktuelle Gruppenstrukturliste
    SDsParFileGroup    *pListe[20];
    // aktuelle Variable
    SDsParFileGroup    *pGroupAct;
    SDsParFileVariable *pVarAct;
    SDsParFileTable    *pTabAct;
    SDsParFileVariable *pVarActList[DS_PAR_FILE_MAX_VAR_ACT_LIST];
    // aktuelle Gruppe aus Liste
    uint32_t             iHierAct;

#if DS_PAR_USE_MATFILELOAD == 1
	// Liste mit externen Datendateien
	SDsExternFileList  *pExternFileList;
#endif


    //Zeichen
    char *cQuot0Gr[20];
    char *cQuot1Gr[20];
    uint8_t QuotGrLen;
    char *cComment[4];
    uint8_t CommentLen;

    char *cQuot0SpezTab;
    char *cQuot1SpezTab;
    char *cQuot01DTab;
    char *cQuot11DTab;
    char *cQuot02DTab;
    char *cQuot12DTab;

    char *cZuweis;
    char *cQuot0;
    char *cQuot1;
    char *cQuota;
    char *cQuotb;
    char *cQuotUnit0;
    char *cQuotUnit1;
    char *cQuotMat0;
    char *cQuotMat1;
    char *cTrennZeich;
    char *cTrennZeichCol;
    char *cTrennZeichAttr;

    char *cAttrFactor;
    char *cAttrOffset;
    char *cAttrCopy;
    char *cAttrType;
    char *cAttrComment;
    char *cAttrUnit;
    char *cAttrTranspose;

    char *cValLinear;
    char *cValNotLinear;

#if DS_PAR_USE_MATFILELOAD == 1
	char *cAttrMatFileRead;
	char *cQuotFileBracket0;
	char *cQuotFileBracket1;
	char *cAttrFileName;
	char *cAttrFileVarName;
#endif

	char *cAttrInclude;

	// alle Zeichen/Strings setzen
	void setQuot();
    
	char readLine(CSlfStr &tline   // Zeilentext
                 ,SDsParReadLine *pstruct    // Hilfsstruktur
                 );
    void resetStruct(CSlfStr &tline,SDsParReadLine *pstruct);

    // neue Struktur aus Zeile lesen
    status_t findStruct(CSlfStr &Tline              // Zeilentext
                       ,SDsParReadLine *pstruct    // Hilfsstruktur, zur Bestimmung
                       );                           // Attribute aus einer Zeile
#if DUMP_FILE_INPUT_STRUCT
    // Dump-TEstausgabe
    status_t dumpStruct(CSlfStr &tline
                       ,SDsParReadLine *pstruct);
#endif
    // Eliminiert Kommentar aus tline
    void elimComment(CSlfStr &tline);

    // Tests
    uint8_t isGroup(CSlfStr &tline);
    uint8_t isGroupAttribut(CSlfStr &tline,SDsParReadLine *pstruct);
    uint8_t is1DTable(CSlfStr &tline);
    uint8_t is2DTable(CSlfStr &tline);
    uint8_t isSpezTable(CSlfStr &tline);
    uint8_t isSpezTableValue(CSlfStr &tline);
    uint8_t isVariable(CSlfStr &tline);

    // nach einer Gruppe suchen
    status_t findGroup(CSlfStr &tline
                      ,SDsParReadLine *pstruct);
    // nach Gruppeattribut suchen
    status_t findGroupAttribut(CSlfStr &tline
                              ,SDsParReadLine *pstruct);
    // nach einer Variablensuchen
    status_t findVariable(CSlfStr &txtl
                         ,SDsParReadLine *pstruct);

    // Spezialtabelle erkennen (Kopf)
    status_t findSpezTable(CSlfStr &tline
                          ,SDsParReadLine *pstruct);
    
    // aus Spezialtabelle Werte auslesen
    status_t findSpezTableValue(CSlfStr &tline
                               ,SDsParReadLine *pstruct);
    // die zu kopierende Gruppe bestimmen
    uint8_t getGroupCopy(CSlfStr &tline
                         ,SDsParReadLine *pstruct);
    
    // die zu kopierende Variable bestimmen
    uint8_t getVariableCopy(CSlfStr &tline
                            ,SDsParReadLine *pstruct);
    // die Einheit bestimmen
    status_t getUnit(CSlfStr &tline
                    ,SDsParReadLine *pstruct);

    // den/dir Werte bestimmen
    status_t getValue(CSlfStr &tline
                     ,SDsParReadLine *pstruct);
    // den/dir Werte ohne Begrenzung bestimmen
    status_t getBlankValue(CSlfStr &tline
                     ,SDsParReadLine *pstruct);
    
    // Kommentar rauslesen
    status_t getComment(CSlfStr &tline
                         ,SDsParReadLine *pstruct);

    status_t find1DTable(CSlfStr &tline                        
                        ,SDsParReadLine *pstruct);
    uint8_t    get1DTableCopy(CSlfStr &tline
                           ,SDsParReadLine *pstruct);
    status_t find2DTable(CSlfStr &tline
                        ,SDsParReadLine *pstruct);
    uint8_t    get2DTableCopy(CSlfStr &tline
                           ,SDsParReadLine *pstruct);

    //aus der eingelsenen Zeile gefundene Struktur einsortieren
    status_t setInternParStruct(SDsParReadLine *pstruct);    

    //pListe von istart an resetten
    void setInternGroupListe(uint8_t istart,SDsParFileGroup *pi);

    // Erstellen einer Instanz
    void makeInternInstanz(char *name);

    // Gruppe setzen
    status_t setInternGroup(SDsParReadLine *pstruct);

    // GruppeKopie setzen
    status_t setInternGroupCopy(SDsParReadLine *pstruct);

    // GruppeKommentar setzen
    status_t setInternGroupComment(SDsParReadLine *pstruct);

    // Variable setzen
    status_t setInternVariable(SDsParReadLine *pstruct);

    // Variable Struct setzen
    status_t setInternVariableStruct(SDsParReadLine *pstruct);

    // 1D-Tabelle setzen
    status_t setIntern1DTable(SDsParReadLine *pstruct);
    
    // 2D-Tabelle setzen
    status_t setIntern2DTable(SDsParReadLine *pstruct);
    
    // Spezial Tabelle setzen
    status_t setInternSpezTable(SDsParReadLine *pstruct);

    // 1D-Tabelle prüfen
    uint8_t isIntern1DTable(SDsParReadLine *pstruct);

    // 2D-Tabelle prüfen
    uint8_t isIntern2DTable(SDsParReadLine *pstruct);
    
    // Table Struct setzen
    status_t setInternTableStruct(SDsParReadLine *pstruct);

    // Sucht die interne Gruppenstruktur
    SDsParFileGroup *findInternGroup(CSlfStrV &groupV);

    // Sucht die interne Variable in Gruppe pg
    SDsParFileVariable *findInternVariable(SDsParFileGroup *pg
                                          ,CSlfStr &varname);

    SDsParFileTable *findInternTable(SDsParFileGroup *pg
                                    ,CSlfStr &tabname);
    // Gruppe Löschen
    void deleteInternGroup(SDsParFileGroup *pSub);

    // Variable Löschen
    void deleteInternVariable(SDsParFileVariable *pVar);

    // Tabelle Löschen
    void deleteInternTable(SDsParFileTable *pTab);

#if DS_PAR_USE_MATFILELOAD == 1

	// Liest die externen Daten ein
	status_t readExternPar(void);

	// sucht bestehende Struktur nach File und Variablen
	status_t findandsetExternFileList(SDsParFileGroup *pg);

	// anlegen der externen Filestruktur mit pv
	status_t setExternFileList(SDsParFileVariable *pv);

	// Einlesen der Daten
	status_t readExternFileList(void);

	// Matlabfile einlesen
	status_t readExternMatFile(SDsExternFileList *pf);

	// Matlabevariable verarbeiten
	status_t readExternMatVar(SDsExternFileVarList *pfv,SDsExternFileList *pf,smxArray *pDsParArray);

	//Liste löschen
	void deleteExternFileList(void);

#endif

    // Übergibt interne Struktur an ParameterDaten-Klasse
    status_t setParData(CDsParData &par_data);

    // kopiert aus aktueller Gruppe in ParameterDaten-Klasse
    status_t setParDataCopyGroup(SDsParFileGroup *pg,CSlfStr &instanz,CSlfStrV &groupV,CDsParData &par_data);

    // Die Variablen der Gruppe werden gesetzt
    status_t setParDataVariable(SDsParFileVariable *pv,CSlfStr &instanz,CSlfStrV &groupV,CDsParData &par_data);

    // Die Variablen durch eine Kopie setzen
    status_t setParDataCopyVariable(SDsParFileVariable *pv,SDsParFileVariable *pvmod
                                   ,CSlfStr &instanz,CSlfStrV &groupV);

    // Setzt den Wert/Werte der Variablen
    status_t setParDataSetValue(CSlfStr &varname
                               ,SDsParFileVariable *pv
                               ,CSlfStr &instanz
                               ,CSlfStrV &groupV
                               ,CDsParData &par_data);

    // Die Untergruppe der Gruppe wird gesetzt
    status_t setParDataSubGroup(SDsParFileGroup *pg,CSlfStr &instanz,CSlfStrV groupV,CDsParData &par_data);

    // Die Tabelle wird gesetzt
    status_t setParDataTable(SDsParFileTable *pt
                            ,CSlfStr &instanz
                            ,CSlfStrV &groupV
                            ,CDsParData &par_data);

    // Die zu kopierende Tabelle wird gesetzt
    status_t setParDataCopyTable(SDsParFileTable *pt
                                ,SDsParFileTable *ptmod
                                ,CSlfStr &instanz
                                ,CSlfStrV &groupV);

    // Vec in pvmod kopieren
    status_t setParDataCopyTableVec(SDsParFileVariable *pv
                                   ,SDsParFileVariable *pvmod
                                   ,SDsParFileVariable *pvc);
                                           
    // Tabelle an Datenstruktur übergeben
    status_t setParDataSetTable(CSlfStr &varname
                               ,SDsParFileTable *pt
                               ,CSlfStr &instanz
                               ,CSlfStrV &groupV
                               ,CDsParData &par_data);

    void writeGroup(FILE *fid
                   ,CDsParData &ParData
                   ,uint32_t *g_list
                   ,uint32_t ihier);

    void writeVar(FILE *fid
                 ,CDsParData &ParData
                 ,uint32_t *g_list
                 ,uint32_t ihier);
    void writeVal(FILE *fid
                 ,CDsParData &ParData
                 ,uint32_t *g_list
                 ,uint32_t ihier
                 ,uint32_t ivar
                 ,EVarType type);
    void writeString(FILE *fid
                 ,CDsParData &ParData
                 ,uint32_t *g_list
                 ,uint32_t ihier
                 ,uint32_t ivar
                 ,EVarType type);
    void writeTab(FILE *fid
                 ,CDsParData &ParData
                 ,uint32_t *g_list
                 ,uint32_t ihier
                 ,uint32_t ivar
                 ,EVarType type);
    void writeVec(FILE *fid
                 ,char *name
                 ,char *unit
                 ,double *pvec
                 ,uint32_t nrwos
                 ,char *comment
                 ,double *pfactor
                 ,double *poffset);
    void writeMat(FILE *fid
                 ,char *name
                 ,char *unit
                 ,Matrix_t mat
                 ,char *comment
                 ,double *pfactor
                 ,double *poffset);

    void makeGroupName(CSlfStrV &groupV,uint32_t ioff,CSlfStr &grouph);


    void writeLogFile(char *name);

};

#endif