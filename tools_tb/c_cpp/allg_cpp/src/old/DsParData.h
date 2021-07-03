// DsPar.h
// Parameter einlesen und weiterverarbeiten

#ifndef DS_PAR_DATA_H_INCLUDED
#define DS_PAR_DATA_H_INCLUDED
//
 
#include <stdio.h>
#include <stdlib.h>

#include "SlfStr.h"
#include "SlfTab.h"
#include "SlfNum.h"

//#include "DsProject.h"

// Parameter-VAriablen-Strukturr
struct SDsParDataVariable {

    CSlfStr				Name;           // NAme
    CSlfStr				Unit;           // EInheit
    EVarType			Type;           // Typ
    CSlfStrM			StrMVal;        // Werte als String in MAtrix
    void				  *pVal;          // Pointer auf Werte
    CSlf1DTab			*p1DTab;        // Pointer auf 1D-Tabelle
    CSlf2DTab			*p2DTab;        // Pointer auf 2D-Tabelle
    //Vector_t				*pVec;          // Pointer aus Vector_t;
    Vector_t			Vec;            // Vector_t;
	  Matrix_t      Mat;            // Matrix_t
    CSlfStr				Comment;        // Kommentar
    uint32_t				NVal;           // Anzahl der Werte
    uint32_t				NRow;           // Anzahl Reihen
    uint32_t				NCol;           // Anzahl Spalten
    double				XFactor;         // FAktor
    double				XOffset;         // Offset
    double				YFactor;         // FAktor
    double				YOffset;         // Offset
    double				ZFactor;         // FAktor
    double				ZOffset;         // Offset
    char				*ParamFile;     // verwendetes PArameterfile
    uint32_t				ILine;          // Zeile in Parameterfile
    SDsParDataVariable  *pNext;         // nächster Pointer
};
// Parameter-Gruppen-Struktur
struct SDsParDataGroup {

    CSlfStr				Name;           // Gruppenname
    CSlfStr				Comment;        // Kommentar
    uint32_t				Hierach;        // Hierachie 0,1,2,3, ...
    uint32_t				NSub;           // Anzahl von Untergruppen
    SDsParDataGroup		*pSub;          // Pointer zu untergeordneten Gruppe
    uint32_t				NVar;           // Anzahl von Variablen
    SDsParDataVariable	*pVar;          // Pointer zu den Variablen
    SDsParDataGroup		*pNext;         // neächster Pointer
};




// Parameter-Klasse 
class CDsParData {

public:
    CDsParData();
    ~CDsParData();

    uint32_t   getLenErrText(void) {return ErrText.getLen();}
    char *   getErrText(void) {return ErrText.c_str();}
    void     resetErrText(void) {ErrText.clear();}

    void     reset(void);    // Daten zurücksetzen

    // Einzelwert setzen
    //==================
    status_t setSingleData(CSlfStr  *pinstanz     // Instanzname
                          ,CSlfStrV *pgroup       // Gruppenhierachie
                          ,CSlfStr  *pvarname     // Variablenname
                          ,CSlfStr  *punit        // Einheit
                          ,double   factor
                          ,double   offset
                          ,CSlfStr  *pcomment     // Kommentar
                          ,double   dval          // Singlewert
                          );
    // STring setzen
    //==================
    status_t setStringData(CSlfStr  *pinstanz     // Instanzname
                          ,CSlfStrV *pgroup       // Gruppenhierachie
                          ,CSlfStr  *pvarname     // Variablenname
                          ,CSlfStr  *punit        // Einheit
                          ,double   factor
                          ,double   offset
                          ,CSlfStr  *pcomment     // Kommentar
                          ,CSlfStr  *psval        // Stringwert
                          );
    // STringMatrix setzen
    //==================
    status_t setStringMData(CSlfStr  *pinstanz     // Instanzname
                          ,CSlfStrV *pgroup       // Gruppenhierachie
                          ,CSlfStr  *pvarname     // Variablenname
                          ,CSlfStr  *punit        // Einheit
                          ,double   factor
                          ,double   offset
                          ,CSlfStr  *pcomment     // Kommentar
                          ,CSlfStrM *psm          // Stringmatrix
                          );
    // Vektor setzen
    //==================
    status_t setVectorData(CSlfStr  *pinstanz     // Instanzname
                          ,CSlfStrV *pgroup       // Gruppenhierachie
                          ,CSlfStr  *pvarname     // Variablenname
                          ,CSlfStr  *punit        // Einheit
                          ,double   factor
                          ,double   offset
                          ,CSlfStr  *pcomment     // Kommentar
                          ,Vector_t   *pvec          // Vektor
                          ,uint32_t   nvec
                          );
    // Matrix_t setzen
    //==================
    status_t setMatrixData(CSlfStr  *pinstanz     // Instanzname
                          ,CSlfStrV *pgroup       // Gruppenhierachie
                          ,CSlfStr  *pvarname     // Variablenname
                          ,CSlfStr  *punit        // Einheit
                          ,double   factor
                          ,double   offset
                          ,CSlfStr  *pcomment     // Kommentar
                          ,Matrix_t   mat          // Matrix_t
                          ,uint32_t   nrows
                          ,uint32_t   ncols
                          );
    // Matrix_t setzen
    //==================
    status_t setMatrixData(CSlfStr  *pinstanz     // Instanzname
                          ,CSlfStrV *pgroup       // Gruppenhierachie
                          ,CSlfStr  *pvarname     // Variablenname
                          ,CSlfStr  *punit        // Einheit
                          ,double   factor
                          ,double   offset
                          ,CSlfStr  *pcomment     // Kommentar
                          ,Matrix_t   mat           // Matrix_t
                          );

    // Tabelle setzen
    //===============
    status_t set1DTableData(CSlfStr  *pinstanz     // Instanzname
                           ,CSlfStrV *pgroup       // Gruppenhierachie
                           ,CSlfStr  *ptabname     // Tabellenname
                           ,CSlfStr  *pcomment     // Kommentar
                           ,CSlfStr  *pxname       // Variablenname x
                           ,CSlfStr  *pxunit       // Einheit x
                           ,double   xfactor
                           ,double   xoffset
                           ,CSlfStr  *pxcomment    // Kommentar x
                           ,uint32_t   nxvec         // Länge Vektor x
                           ,double   *pxvec        // Vektor x
                           ,CSlfStr  *pyname       // Variablenname y
                           ,CSlfStr  *pyunit       // Einheit x
                           ,double   yfactor
                           ,double   yoffset
                           ,CSlfStr  *pycomment    // Kommentar y
                           ,uint32_t   nyvec         // Länge Vektor y
                           ,double   *pyvec        // Vektor y
                           ,uint8_t    lin_flag=1    // Linear / Step
                           );

    // Tabelle setzen
    //===============
    status_t set2DTableData(CSlfStr  *pinstanz     // Instanzname
                           ,CSlfStrV *pgroup       // Gruppenhierachie
                           ,CSlfStr  *ptabname     // Tabellenname
                           ,CSlfStr  *pcomment     // Kommentar
                           ,CSlfStr  *pxname       // Variablenname x
                           ,CSlfStr  *pxunit       // Einheit x
                           ,double   xfactor
                           ,double   xoffset
                           ,CSlfStr  *pxcomment    // Kommentar x
                           ,uint32_t   nxvec         // Länge Vektor x
                           ,double   *pxvec        // Vektor x
                           ,CSlfStr  *pyname       // Variablenname y
                           ,CSlfStr  *pyunit       // Einheit y
                           ,double   yfactor
                           ,double   yoffset
                           ,CSlfStr  *pycomment    // Kommentar y
                           ,uint32_t   nyvec         // Länge Vektor y
                           ,double   *pyvec        // Vektor y
                           ,CSlfStr  *pzname       // Variablenname z
                           ,CSlfStr  *pzunit       // Einheit z
                           ,double   zfactor
                           ,double   zoffset
                           ,CSlfStr  *pzcomment    // Kommentar z
                           ,Matrix_t   zmat          // Vektor z
                           ,uint8_t    lin_flag      // Linear / Step
                           );
    // Werte allgemein setzen
    //=======================
    status_t setData(CSlfStr     *pi     // Instanzname
                    ,CSlfStrV    *pg       // Gruppenhierachie
                    ,CSlfStr     *pvn     // Variablenname
                    ,CSlfStr     *pu        // Einheit
                    ,CSlfStr     *pcom     // Kommentar
                    ,EVarType    type      // DatenType
                    ,uint32_t      nrows     // Anzahl Reihen
                    ,uint32_t      ncols     // Anzahl Spalten
                    ,double      xfactor
                    ,double      xoffset
                    ,double      yfactor
                    ,double      yoffset
                    ,double      zfactor
                    ,double      zoffset
                    ,void        *pval     // Pointer Wert
                    );

    // Abfragen
    // nh = 1 => Instanz
    // nh = 2 => Instanz und erste UnterGruppe
    // group_list ist ein Vektor mit den Index der einzelnen Hierachien
    // angefangen bei der Instamz
    // Rückgabe ist NULL, wenn nicht erfolgreich
    uint32_t getNInstanz(void);
    char  *getNameInstanz(uint32_t iinst);

    bool   existGroup(CSlfStr &instanz,CSlfStrV &vgroup);

    uint32_t getNSubGroup(uint32_t *group_list,uint32_t nhierachie);
    char  *getNameSubGroup(uint32_t *group_list,uint32_t nhierachie,uint32_t ig);

    char  *getNameGroup(uint32_t *group_list,uint32_t nhierachie);
    bool   existCommentGroup(uint32_t *group_list,uint32_t nhierachie);
    char  *getCommentGroup(uint32_t *group_list,uint32_t nhierachie);

    bool   existVar(CSlfStr &instanz,CSlfStrV &vgroup,CSlfStr &varname);
    SDsParDataVariable *getVar(CSlfStr &instanz,CSlfStrV &vgroup,CSlfStr &varname);

    uint32_t getNVar(uint32_t *group_list,uint32_t nhierachie);
    char  *getNameVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    char  *getUnitVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    char  *getCommentVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    
    EVarType getTypeVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    
    uint32_t getNColVar(uint32_t *group_list,uint32_t nh,uint32_t iv);
    
    uint32_t getNRowVar(uint32_t *group_list,uint32_t nh,uint32_t iv);

    CSlfStr *getVStringVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv,uint32_t irow,uint32_t icol);
    double  *getVDoubleVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv,uint32_t irow,uint32_t icol);
    uint32_t  *getVUint32Var(uint32_t *group_list,uint32_t nhierachie,uint32_t iv,uint32_t irow,uint32_t icol);
    sint32_t  *getVSint32Var(uint32_t *group_list,uint32_t nhierachie,uint32_t iv,uint32_t irow,uint32_t icol);
    
    Vector_t  getVVecVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    CSlf1DTab  *getV1DTabVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    CSlf2DTab  *getV2DTabVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);

    double *getXFactorVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    double *getXOffsetVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    double *getYFactorVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    double *getYOffsetVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    double *getZFactorVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
    double *getZOffsetVar(uint32_t *group_list,uint32_t nhierachie,uint32_t iv);
private:

    status_t        Status;
    CSlfStr         ErrText;

    SDsParDataGroup *pData;

    uint8_t           Init_flag;

    SDsParDataGroup *findorbuildGroup(CSlfStr  &instanz     // Instanzname
                                  ,CSlfStrV &vgroup       // Gruppenhierachie
                                  );
    SDsParDataGroup *findGroup(CSlfStr  &instanz     // Instanzname
                          ,CSlfStrV &vgroup       // Gruppenhierachie
                          );

    SDsParDataVariable *findorbuildVariable(SDsParDataGroup  *pg  // Pointer auf Gruppe
                                   ,CSlfStr &varname       // Variablenname
                                   );
    SDsParDataVariable *findVariable(SDsParDataGroup  *pg  // Pointer auf Gruppe
                                ,CSlfStr &varname       // Variablenname
                                );

    void deleteParStruct(SDsParDataGroup *pd);      // Struktur zerstören
    void deleteVarStruct(SDsParDataVariable *pv);
    void deleteVal(SDsParDataVariable *pv);

    SDsParDataGroup *getGroup(uint32_t *group_list,uint32_t nh);
    SDsParDataGroup *getSubGroup(SDsParDataGroup *pg,uint32_t index);
    SDsParDataVariable *getVar(SDsParDataGroup *pg,uint32_t iv);


};

#endif