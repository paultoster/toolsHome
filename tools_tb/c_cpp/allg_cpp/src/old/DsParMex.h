// DsPar.h
// Parameter einlesen und weiterverarbeiten
//
// Auslesen der Matlabstruktur
//
// par[i].type    = 'single'
//
// par[i].instanz = '';
// par[i].name    = 'name';
// par[i].group   = 'groupname.subgroupname';
// par[i].val     = value;
// par[i].unit    = 'unit';

#ifndef DS_PAR_MEX_H_INCLUDED
#define DS_PAR_MEX_H_INCLUDED
//
 
#include <stdio.h>
#include <stdlib.h>

extern "C" {
#include "mex.h"
}

#include "SlfStr.h"
#include "DsParData.h"


// Beschreibungsliste, wenn nur Strukturname bekannt
// mit Wert
typedef struct SDsParMexSimpleStruct_tag {
  char                 *varname;     // Name = group.structname
  char                 *unit;        // Unit
  enum EVarType         type;        // Typ
  char                 *xvarname;    // Bei vektor oder  matrix den x-Vektor
  char                 *yvarname;    // Bei matrix den y-Vektor
  char                 *comment;     // Comment
 
} SDsParMexSimpleStruct;

typedef size_t    mwSize;         /* unsigned pointer-width integer */
typedef size_t    mwIndex;        /* unsigned pointer-width integer */

// Parameter-Klasse 
class CDsParMex {

public:
    CDsParMex();
    ~CDsParMex();

    void reset(void);

    // Einlesen von Datenstruktur in Ds-Format
    //========================================
    void addFilterGroup(char *pstring);        // Gruppenfilter, wenn nicht alle Untergruppen gebraucht werden
                                               // , für mehrere Gruppe mehrfach aufrufen
    status_t read(const mxArray *par           // Parameter cell/struct von Matlab
                 ,CDsParData &ParData);        // Parameterklasse in der gespeichert wird

    // Einlesen einer einfachen Datenstruktur 
    //=======================================
    uint8_t   isSimpleStruct(const mxArray *par);                // prüft, ob einfache Datenstruktur
                                                               // benutzt.
    status_t readSimpleStruct(const mxArray *par               // Parameter cell/struct von Matlab
                             ,CDsParData &ParData              // Parameterklasse in der gespeichert wird
                             ,SDsParMexSimpleStruct *pvardes   // Variablenbeschreibung für simple Structeingabe
                             ,uint16_t nvardes);                 // Anzahl der Variablen


    // Sonstige Funktionen
    //====================
    uint32_t   getLenErrText(void) {return ErrText.getLen();}
    char *   getErrText(void) {return ErrText.c_str();}
    void     resetErrText(void) {ErrText.clear();}

private:

    status_t        Status;
    CSlfStr         ErrText;

    //Schlüsselwörter
    char *TYPE;
    char *INSTANZ;
    char *GROUP;
    char *NAME;
    char *VAL;
    char *UNIT;
    char *COMMENT;
    char *FACTOR;
    char *OFFSET;
    char *SPLITGROUP;

    char *XNAME;
    char *XVEC;
    char *XUNIT;
    char *XCOMMENT;
    char *XFACTOR;
    char *XOFFSET;
    char *YNAME;
    char *YVEC;
    char *YUNIT;
    char *YCOMMENT;
    char *YFACTOR;
    char *YOFFSET;
    char *ZNAME;
    char *ZMAT;
    char *ZUNIT;
    char *ZCOMMENT;
    char *ZFACTOR;
    char *ZOFFSET;
    char *LIN;

    char *TYPE_SINGLE[2];
    char *TYPE_VEC[2];
    char *TYPE_STRING;
    char *TYPE_1DTAB[2];
    char *TYPE_2DTAB[2];

	CSlfStrV GroupNameFilterList;

	// erste Ebene mit der Möglichkeit auszufiltern
	//=============================================
    status_t readGroup0(const mxArray *par            // Parameter struct von Matlab
                 ,mwIndex index                 // Index der struct (0 wenn kein array)
                 ,CDsParData &par_data         // Parameterklasse in der gespeichert wird
                 ,CSlfStr &instanz              // Instanzname (üblich "0","1","2", ...)
                 ,CSlfStrV vgroup);            // Vektor mit Gruppennamenhierachie
	// weitere Ebenen
	//===============
    status_t readGroup(const mxArray *par            // Parameter struct von Matlab
                 ,mwIndex index                 // Index der struct (0 wenn kein array)
                 ,CDsParData &par_data         // Parameterklasse in der gespeichert wird
                 ,CSlfStr &instanz              // Instanzname (üblich "0","1","2", ...)
                 ,CSlfStrV vgroup);            // Vektor mit Gruppennamenhierachie
    bool isValue(const mxArray *par);           // Prüfft par, ob Gruppe oder VAriable
    status_t readValue(const mxArray *par
                      ,CDsParData &par_data
                      ,CSlfStr &instanz
                      ,CSlfStrV &vgroup
                      ,CSlfStr &name);

    status_t readValueSimpleStruct(const mxArray *par
                                  ,CDsParData &par_data
                                  ,CSlfStr    &instanz
                                  ,CSlfStrV   &vgroup
                                  ,SDsParMexSimpleStruct *pvardes
                                  ,uint16_t     nvardes);
    const mxArray *findGroupSimpleStruct(CSlfStrV   &vgroup          // Stringvekktor mit Gruppennamenhierachie
                                         ,const mxArray *par);        // die auszuwertende Struktur von Simulink
    uint8_t    isSimpleStructIntern(const mxArray *par,CSlfStrV &gliste);
    status_t readValueSimpleStructDS(const mxArray *parray
                                    ,CDsParData &par_data
                                    ,CSlfStr    &instanz
                                    ,CSlfStrV   &vgroup
                                    ,CSlfStr    &varname
                                    ,SDsParMexSimpleStruct *pvardes);
    status_t readValueSimpleStruct1DTable(const mxArray *parray
                                         ,CDsParData &par_data
                                         ,CSlfStr    &instanz
                                         ,CSlfStrV   &vgroup
                                         ,CSlfStr    &varname
                                         ,SDsParMexSimpleStruct *pvardes
                                         ,const mxArray *pxarray
                                         ,CSlfStr    &xvarname
                                         ,SDsParMexSimpleStruct *pxvardes);
};

#endif