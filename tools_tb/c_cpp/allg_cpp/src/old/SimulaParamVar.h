//
// SimulaParamVar.h
//
// Parameter zur Simulation Simula einlesen und weiterverarbeiten

#ifndef SIMULA_PARAM_VAR_H
#define SIMULA_PARAM_VAR_H

#include <string>
#include <vector>
#include <string>
#include <sstream>
#include <typeinfo.h>
#include "SimulaDef.h"
#include "HlpString.h"


namespace Simula
{
  //====================================================================================
  //====================================================================================
  // Basis-Klasse Parameter-Variablen
  //====================================================================================
  //====================================================================================
  class CParVarBase
  {
  protected:
    status_type     Status;
    std::string     ErrText;
    std::string     Name;
    vec_string_t    GroupHierachy;
    std::string     Unit;
    enum var_type   VarType;
    bool            AllowDefVal;
    bool            HasDefValue;
    bool            HasUnit;
  public:
    CParVarBase()
    {
      Status              = STATUS_OKAY;
      Name                = "";
      GroupHierachy.clear();
      Unit                = "";
      VarType             = SIMULA_VARTYPE_NON;
      AllowDefVal         = false;
      HasDefValue         = false;
      HasUnit             = false;
    }
    void setName(std::string &name) {Name = name;}
    void setName(const char *name) {Name = name;}
    void setGroupHierarchy(std::string &grouphierarchy) { GroupHierachy = Hlp::StringSplit((char *)grouphierarchy.c_str(),DELIM_HIERACHY); }
    void setGroupHierarchy(const char *grouphierarchy)  { GroupHierachy = Hlp::StringSplit(grouphierarchy,DELIM_HIERACHY); }
    void setUnit(std::string &name) {setUnit(name.c_str());}
    void setUnit(const char *cname) {if(strlen(cname) > 0 ){Unit = cname;HasUnit=true;}else{HasUnit=false;}}
    void setAllowDefVal(bool allow) {AllowDefVal = allow;}
    status_type getStatus() {return Status;}
    std::string getName() {return Name;}
    const char *getNameC() {return Name.c_str();}
    std::size_t getNameLength() {return Name.size();}
    vec_string_t getGroupHierachy() {return GroupHierachy;}
    std::string getUnit() {return Unit;}
    const char *getUnitC() {return Unit.c_str();}
    std::size_t getUnitLength() {return Unit.size();}
    bool getAllowDefVal() {return AllowDefVal;}
    bool getHasUnit() {return HasUnit;}
    enum var_type getVarType() {return VarType;}
    char *getErrText() {return (char *)ErrText.c_str();}

  };

  template<typename T>
  class CParVar : public CParVarBase
  {
  protected:
    T                 DefVal;
    T                 *pVal;
    std::vector<T>    Vec;
    std::vector<bool> VecSet;
  public:
    CParVar()
    {
      pVal                = NULL;

      if(      strcmp(typeid( T ).name(),typeid( int         ).name()) == 0 ) VarType = SIMULA_VARTYPE_SINGLE_INT;
      else if( strcmp(typeid( T ).name(),typeid( double      ).name()) == 0 ) VarType = SIMULA_VARTYPE_SINGLE_DOUBLE;
      else if( strcmp(typeid( T ).name(),typeid( std::string ).name()) == 0 ) VarType = SIMULA_VARTYPE_SINGLE_STRING;
      else
      {
        Status = STATUS_NOT_OKAY;
        ErrText.append("\nParVar: Error in template type, unkown template type, see contructor CParVar");
      }
    }
    void setValuePointer(T *pvalue) {pVal = pvalue;}
    status_type setDefVal(char *pvaltext)
	  {
      std::stringstream strstr(pvaltext);
      T val;
      strstr >> val;
	    if ( !strstr ) return STATUS_NOT_OKAY;
	    else           
	    {
		    setDefVal(val);
	      return STATUS_OKAY;
	    }
    }
    void setDefVal(T &value)  {DefVal = value;HasDefValue = true;}
    T  getDefVal() {return DefVal;}
    void addVal(T &value) {Vec.push_back(value);VecSet.push_back(true);}    
    void setVal(T &value,std::size_t instance) 
    { 
      s_t n = Vec.size();
      if( (instance+1) > n )
      { 
        Vec.resize(instance+1);
        VecSet.resize(instance+1);
        for(s_t i=n;i<Vec.size();i++) VecSet[i] = false;
      }
      Vec[instance]    = value;
      VecSet[instance] = true;
    }    
    bool isVecSet(s_t instance)
    {
      if( instance < VecSet.size() ) return VecSet[instance];
      else if( HasDefValue )         return true;
      else                           return false;
    }
    T  getVal(s_t instance) 
    {
      // ABfrage, ob die Instanz exitiert
      if( (instance < Vec.size()) && VecSet[instance] ) return Vec[instance];
      else if( HasDefValue ) return DefVal;
      else                   throw "No Value and Default is set";
    }
    std::size_t getNumOfEntities() 
    {
      if( HasDefValue ) return SIMULA_MAX(1,Vec.size());
      else return Vec.size();
    }
    void resizeVec(std::size_t n) {Vec.resize(n);}
  };
}
//
//
//// Variablendefinition vom Modell, mit dieser Struktur
//// wird im Modell eine Variable definiert und an DsParSet() weitergegebe
//// Zuordnung Type und Pointer pVal:
//// DEF_VOID             :     
//// DEF_DOUBLE           :  pointer auf double-Variable übergeben  Variable muß angelegt sein 
////                         (z.B. double dvar Übergabe mit &dvar)
//// DEF_FLOAT            :  dito float
//// DEF_SIGNED_LONG      :  dito signed long
//// DEF_UNSIGNED_LONG    :  dito unsigned long
//// DEF_SIGNED_SHORT     :  dito signed short
//// DEF_UNSIGNED_SHORT   :  dito unsigned short
//// DEF_SIGNED_CHAR      :  dito signed char
//// DEF_UNSIGNED_CHAR    :  dito unsigned char
//// DEF_STRING,          :  dito string-Klasse CSlfStr (SlfStr.h)
//// DEF_VEC              :  pointer auf Vector_t-Variable übergeben, Vector_t muß angelegt vorhanden sein
////                        (z.B. Vector_t vec; siehe SlfNum.h), wenn vec = 0; gesetzt wird automatisch 
////                        Speicher reserviert; wenn vorher mit vec = NewVector(n); gesetzt, 
////                        wird Länge n mit den zuzuweisenden gelesenen Parameterdaten verglichen                          
////                        !!! Vector_t besteht aus einer struktur mit double-array und Länge
//
//struct SDsParVar {
//  char             *Name;    // Name
//  char             *Group;   // Gruppenhierachie mit "." getrennt
//  void             *pVal;    // Pointer 
//  enum EVarType    Type;     // Typ siehe 
//  char             *Unit;    // Unit
//  char             *Default; // Default-Wert, wenn zugelassen 
//  char             *Comment; // Comment
//};
//
//// DEF_ARR_DOUBLE           :  pointer auf double-array übergeben  Variable muß angelegt sein 
////                            (z.B. double dvar[10] Übergabe mit dvar, aber auch Länge übergeben)
//// DEF_ARR_FLOAT            :  dito float
//// DEF_ARR_SIGNED_LONG      :  dito signed long
//// DEF_ARR_UNSIGNED_LONG    :  dito unsigned long
//// DEF_ARR_SIGNED_SHORT     :  dito signed short
//// DEF_ARR_UNSIGNED_SHORT   :  dito unsigned short
//// DEF_ARR_SIGNED_CHAR      :  dito signed char
//// DEF_ARR_UNSIGNED_CHAR    :  dito unsigned char
//// DEF_ARR_STRING,          :  dito string-Klasse CSlfStr (SlfStr.h)
////                             (z.B. CSlfStr str[10] Übergabe mit str, aber auch Länge übergeben)
//struct SDsParArr {
//  char             *Name;    // Name
//  char             *Group;   // Gruppenhierachie mit "." getrennt
//  void             *pVal;    // Pointer 
//  uint32_t           NRow;     // Länge des Vektors
//  enum EVarType    Type;     // Typ siehe 
//  char             *Unit;    // Unit
//  char             *Default; // Default-Wert, wenn zugelassen 
//  char             *Comment; // Comment
//};
//
//struct SDsParTab {
//  char             *Name;       // Name
//  char             *Group;      // Gruppenhierachie mit "." getrennt
//  void             *pVal;       // Pointer 1DTab oder 2DTab
//  enum EVarType    Type;        // Typ
//  char             *Comment;    // Comment
//  uint8_t            Order;       // 0: step 1: linear
//  char             *XName;      // Name X-Vektor
//  char             *XUnit;      // Einheit X-Vektor
//  char             *XDefault;   // Default-Wert X-Vektor
//  char             *XComment;   // Comment X-Vektor
//  char             *YName;      // Name X-Vektor
//  char             *YUnit;      // Einheit X-Vektor
//  char             *YDefault;   // Default-Wert X-Vektor
//  char             *YComment;   // Comment X-Vektor
//  char             *ZName;      // Name Z-Matrix_t
//  char             *ZUnit;      // Einheit Z-Matrix_t
//  char             *ZDefault;   // Default-Wert Z-Matrix_t
//  char             *ZComment;   // Comment Z-Matrix_t
//
//};
//
//// Von den Modulen gesetzte Variablennamenstruktur
//struct SDsParSetVar {
//  CSlfStr                 ModName;     // Name
//  CSlfStr                 VarName;     // Name
//  CSlfStr                 GroupName;   // Gruppenhierachie
//  CSlfStrV                VGroup;      // Gruppenhierachie in Vektor aufgeteilt
//  void                    *pVal;       // Wert
//  uint32_t                  NVal;        // Anzahl Werte (bei arrays)
//  uint32_t                  NRow;        // Anzahl Werte (bei arrays)
//  uint32_t                  NCol;        // Anzahl Werte (bei arrays)
//  enum EVarType           Type;        // Typ
//  CSlfStr                 Unit;        // Unit
//  CSlfStr                 Default;     // Defaultvalue
//  CSlfStr                 Comment;     // Comment
//  uint8_t                   TabOrder;    // Ordnung 0:step 1:lin
//  CSlfStr                 XTabName;    // Name X-VEktor
//  CSlfStr                 XTabUnit;    // Einheit X-VEktor
//  CSlfStr                 XTabDefault; // Default X-VEktor 
//  CSlfStr                 XTabComment; // Kommentar X-VEktor
//  CSlfStr                 YTabName;    // Name Y-VEktor
//  CSlfStr                 YTabUnit;    // Einheit Y-VEktor
//  CSlfStr                 YTabDefault; // Default Y-VEktor 
//  CSlfStr                 YTabComment; // Kommentar Y-VEktor
//  CSlfStr                 ZTabName;    // Name Z-Matrix_t
//  CSlfStr                 ZTabUnit;    // Einheit Z-Matrix_t
//  CSlfStr                 ZTabDefault; // Default Z-Matrix_t 
//  CSlfStr                 ZTabComment; // Kommentar Z-Matrix_t
//
//  uint8_t                   VecSet;      // Vektor wurde in CPar gebildet
//  uint8_t                   VecBuild;    // Vektor wurde über new erst erstellt
//  uint8_t                   VarSet;      // Variable ist gesetzt, kann ausgeschaltet werden
//
//  SDsParSetVar           *pNext;
//};
//
//// Von den Modulen gesetzte Gruppenausstruktur
//struct SDsParSetGroup {
//  CSlfStr                 ModName;     // Name
//  CSlfStr                 MasterGroup; // übergeordnete Gruppe
//  CSlfStrV                SubGroupList;   // Liste mit Auswahl der Gruppen
//  CSlfStr                 VarName;     // Variablenname, der die Auswahl bestimmt
//  CSlfStr                 Default;
//
//  SDsParSetGroup         *pNext;
//};
//
//
//// Parameter-Klasse 
//class CDsPar 
//{
//
//public:
//
//    CDsPar();
//    CDsPar(CSlfLogFile *plogfile);
//    ~CDsPar();
//
//    // Mit LogFile initialisieren
//    status_t ini(CSlfLogFile *plogfile=0);
//
//#if DS_PAR_USE_MEXLOAD != 0
//    // Parameterstruktur aus Matlab auslesen
//    status_t readMex(const mxArray *par);           // Parameter cell/struct von Matlab
//#endif
//#if DS_PAR_USE_PAR_FILE != 0
//    // Parameterstruktur aus Datei auslesen
//    status_t readFile(char *par_file);           // 
//#endif
//
//    // Paramter setzen, die Parameter füllen die Liste pDSParSetVar
//    // mit modul name und gruppen
//    status_t set(char *mod_name,SDsParVar *parlist,uint16_t npar);
//    status_t set(char *mod_name,SDsParArr *parlist,uint16_t npar);
//    status_t set(char *mod_name,SDsParTab *par,uint16_t npar);
//    // Damit können die Parameter in der pDSParSetVar-Liste auf nodefault gestzt werden
//    // d.h. die Parameter müssen von aussen gesetzt werden
//    status_t setNoDefault(char *mod_name);
//    // Damit können mit Parameter bestimmte Gruppen aus einem Modell gesetzt werden
//    // z.B. zwei Gruppen haben den gleichen Parametersatz zwischen diesen kann ausgewählt werden
//    status_t decideGroup(char *mod_name,char *group,CSlfStrV &subgrouplist,char *switchpar,char *defvalue="");
//    // Parametersatz eines Moduls aus pDSParSetVar löschen
//    void     deleteModul(char *mod_name);
//
//    // Pointer der Data-Struktur
//    CDsParData &getRefParData(void){return ParData;}
//    CDsParData &getRefParDataUsed(void){return ParDataUsed;}
//
//    // Wertet die mit DsParSet() übergebenen Parameter aus
//    // und Schreibt Parameterwert in Variable
//    // iinst : index Instanz
//    status_t get(uint16_t iinst);
//
//
//    //Beenden
//    //=======
//    status_t done(void);
//
//    // Löscht alle eingelesenen Parameter (ParData)
//    // und benutzten Parameter (ParDataUsed), aber nicht die
//    // registrierten Paramter und Gruppen
//    void resetParData(void);
//
//    uint32_t   getLenErrText(void) {return ErrText.getLen();}
//    char *   getErrText(void) {return ErrText.c_str();}
//    void     resetErrText(void) {ErrText.clear();}
//    
//    void   setExtLoopTime(double dt){ExtLoopTime=dt;}
//    uint8_t  isExtLoopTime(void){return ExtLoopTime!=0.0;}
//    double getExtLoopTime(void){return MAX(ExtLoopTime,EPSILON);}
//
//
//private:
//
//    status_t        Status;
//    CSlfStr         ErrText;
//
//    CSlfLogFile       *pLogFile;             // Logfilehandling
//    uint8_t             LogFileSet;            // Sagt ob LogFile geschrieben wird
//
//    double ExtLoopTime;
//
//    CDsParData     ParData;
//    CDsParData     ParDataUsed;
//#if DS_PAR_USE_MEXLOAD != 0
//    CDsParMex      DSParMex;      
//#endif
//#if DS_PAR_USE_PAR_FILE != 0
//    CDsParFile     DSParFile;
//#endif
//    SDsParSetVar   *pDSParSetVar;
//    SDsParSetGroup *pDSSDsParSetGroup;
//
//    char *cTrennZeichGroup;
//
//
//    status_t getPar(uint16_t iinst);
//    status_t getVar(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    status_t getVarSingle(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    status_t getVarString(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    status_t getVar1DTab(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    status_t getVar2DTab(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    status_t getVarVec(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    status_t getVarArray(SDsParSetVar *pvar,SDsParDataVariable *pvardata,CSlfStr &instanz);
//    
//    status_t proofSubGroupSwitch(uint16_t iinst,CSlfStr &instanz);
//    status_t proofType(EVarType TypeI,EVarType TypeO); 
//    
//    status_t getDefaultVar(SDsParSetVar *pvar);
//    status_t getDefaultVarSingle(SDsParSetVar *pvar);
//    status_t getDefaultVarString(SDsParSetVar *pvar);
//    status_t getDefaultVar1DTab(SDsParSetVar *pvar);
//    status_t getDefaultVar2DTab(SDsParSetVar *pvar);
//    status_t getDefaultVarVec(SDsParSetVar *pvar);
//    status_t getDefaultVarArray(SDsParSetVar *pvar);
//
//    void writeLogFile(char *name);
//
//    status_t logParDataUsed(SDsParSetVar *pvar,CSlfStr &instanz,CSlfStrV &vgroup);
//
//
//    void setVarDelete(void);
//    void deleteVec(SDsParSetVar *pvar);
//    void setGroupDelete(void);
//
//};

#endif