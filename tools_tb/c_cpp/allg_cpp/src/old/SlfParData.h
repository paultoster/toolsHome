//SlfParData.h
// Klasse zum Handeln der eingelesenen Parameterstruktur
//
// Mit 
// #include datei.ext
// wird eine eingebettete Parameterdatei eingefügt

#ifndef SLF_PAR_READ_Data
#define SLF_PAR_READ_Data
//
 
#include "SlfStr.h"
#include "SlfParDef.h"
#include "SlfBasicClass.h"
#include "SlfStrV.h"
#include "SlfStrM.h"

namespace slf
{
  class CSlfParData : public CSlfBasic
  {

  public:

    struct SSlfParVar
    {
      slf::CStr				      Name;           // Variablenname
      slf::CStr				      Comment;        // Kommentar
      slf::CStr				      Unit;           // EInheit
      bool                isString;       // Wert ist string
      Matrix_t            Mat;            // Matrix
      slf::CStrM			      StrMVal;        // Werte als String in MAtrix
      SSlfParVar          *pNext;
    };
    struct SSlfPar1DTab
    {
      slf::CStr				      Name;           // Variablenname
      slf::CStr				      Comment;        // Kommentar
      SSlfParVar          *pXVec;           // XVec[1:n] 
      SSlfParVar          *pYVec;           // YVec[1:n] 
      SSlfPar1DTab        *pNext;
    };
    struct SSlfPar2DTab
    {
      slf::CStr				      Name;           // Variablenname
      slf::CStr				      Comment;        // Kommentar
      SSlfParVar          *pXVec;           // XVec[1:nrows]
      SSlfParVar          *pYVec;           // YVec[1:ncols]
      SSlfParVar          *pZMat;           // ZMat[1:nrows][1:ncols]
      SSlfPar2DTab        *pNext;
    };
    struct SSlfParGroup
    {
      slf::CStr				      Name;           // Gruppenname
      slf::CStr				      Comment;        // Kommentar
      sint16_t            iHir;           // Hirachie -1: Instanz <> 0:erste (oberste)Gruppe [] 
                                          //           1:() 2.Gruppe, 2:(()) nächst tiefere usw.
      uint16_t            iPos;           // Position in verketetter Liste der übergeordneten Gruppe
      SSlfParGroup        *pParent;       // übergeordneter Pointer
      SSlfParGroup        *pSubGroup;     // Pointer verkettete Liste UnterGruppe
      uint16_t            nSubGroup;      // Anzahl der UnterGruppen
      SSlfPar1DTab        *p1DTab;        // Pointer verketteter Liste 1D-TAbelle
      uint16_t            n1DTab;         // Anzahl 1D-Tabellen
      SSlfPar2DTab        *p2DTab;        // Pointer verketteter Liste 2D-TAbelle
      uint16_t            n2DTab;         // Anzahl 2D-Tabellen
      SSlfParVar          *pVar;          // Pointer verketteter Liste Variablen
      uint16_t            nVar;           // Anzahl Variablen
      SSlfParGroup        *pNext;         // nächster Pointer
    };

    CSlfParData();
    ~CSlfParData();

    status_t setInstance(const char *pname, const char *pcomment = "");
    //status_t setActInstanceComment(char *pcomment);
    status_t setGroup(const char *pname, uint16_t hierachie, const char *pcomment = "");
    status_t set1DTab(const char *pname, const char *pcomment = "");
    status_t set2DTab(const char *pname, const char *pcomment = "");
    status_t setVar(const char *pname, const char *punit, const char *pcomment = "");
    status_t setVarVal(slf::CStrM &strm);
    status_t setVarVal(Matrix_t &mat);
    CSlfParData& operator=(const CSlfParData& s);

    status_t getActInstance(slf::CStrV &strv);
    status_t getActGroup(slf::CStrV &strv);
    status_t getActVar(slf::CStrV &strv);
    status_t getAct1DTab(slf::CStrV &strv);
    status_t getAct2DTab(slf::CStrV &strv);

    uint16_t   getNInstance(void) { return nI; }
    const char *getInstName(uint16_t isearch);
    const char *getGroupComment(slf::CStrV &strv);
    uint16_t   getNSubGroup(slf::CStrV &strv);
    const char *getSubGroupName(slf::CStrV &strv, uint16_t isearch);
    uint16_t   getNVar(slf::CStrV &strv);
    const char *getVarName(slf::CStrV &strv, uint16_t isearch);
    const char *getVarUnit(slf::CStrV &strv);
    const char *getVarComment(slf::CStrV &strv);
    bool     getVarIsString(slf::CStrV &strv);
    status_t getVarStringM(slf::CStrV &strv, slf::CStrM &strvm);
    status_t getVarMat(slf::CStrV &strv, Matrix_t &mat);

  private:


    SSlfParGroup    *pI;              // pointer rekursiver Struktur
    uint16_t         nI;               // Anzahl der aktuellen geschachtellten Instanzen

    SSlfParGroup    *pIActual;        // aktuelle Instance
    SSlfParGroup    *pGActual;        // aktuelle Gruppe
    SSlfPar1DTab    *p1DTActual;
    SSlfPar2DTab    *p2DTActual;
    SSlfParVar      *pVActual;        // aktuelle Variable

    //----------------------------------------------------------
    //                 Löscht Gruppenstruktur pg und alles darin
    void               destroyGroups(SSlfParGroup *pg);
    //                 Löscht 1DTabellenstruktur
    void               destroy1DTab(SSlfPar1DTab *pt);
    //                 Löscht 2DTabellenstruktur
    void               destroy2DTab(SSlfPar2DTab *pt);
    //                 Löscht Variablenstruktur
    void               destroyVar(SSlfParVar *pv);

    //---------------------------------------------------------------------------
    //                 Hängt die Gruppenstruktur p in die verkettete Liste pg ein
    SSlfParGroup * setGroupInVerketteteListe(SSlfParGroup *p, SSlfParGroup *pg);
    //---------------------------------------------------------------------------
    //                 Hängt die Variablenstruktur p in die verkettete Liste pv ein
    SSlfParVar * setVarInVerketteteListe(SSlfParVar *p, SSlfParVar *pv);
    //---------------------------------------------------------------------------
    //                 Hängt die Tabellenstruktur p in die verkettete Liste pt ein
    SSlfPar1DTab *set1DTabInVerketteteListe(SSlfPar1DTab *p, SSlfPar1DTab *pt);
    SSlfPar2DTab *set2DTabInVerketteteListe(SSlfPar2DTab *p, SSlfPar2DTab *pt);

    //----------------------------------------------------------------------------
    //                 Kopiert die Gruppenstruktur ps in die angelegte Struktur pd
    //                 mit Angabe der Eltern (Istanz = 0)
    void               copyGroup(SSlfParGroup *ps, SSlfParGroup *pd, SSlfParGroup *pparent);
    void               copyVar(SSlfParVar *pvs, SSlfParVar *pvd);

    //----------------------------------------------------------------------------
    // Sucht Pointer der Gruppenstruktur
    // strv enthält <instance>[Gruppe](Untergruppe) etc.
    SSlfParGroup *getGroupPointer(slf::CStrV &strv);
    // Sucht Pointer der Variablenstruktur
    // strv enthält <instance>[Gruppe](Untergruppe)...VarName etc.
    SSlfParVar *getVarPointer(slf::CStrV &strv);


  };
} // namespace
#endif