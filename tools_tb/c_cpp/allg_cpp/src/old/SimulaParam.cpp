//
// SimulaParam.cpp
//
// Parameter zur Simulation Simula einlesen und weiterverarbeiten
#include <fstream>      // std::ofstream

#include "Simula.h"
#include "SimulaParam.h"
#include "SimulaHelp.h"
#include "HlpString.h"

//====================================================================================
//====================================================================================
// globale Parameter-Klasse anlegen
// (diese wird von allen Modulen benutzt)
//====================================================================================
//====================================================================================
namespace Simula
{
  CPar          Par;
  Hlp::CLog     Log(LOGFILENAME,false);
}

//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
// Parameter Klasse
//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================

//====================================================================================
//====================================================================================
// declareVar Funktionen
//====================================================================================
//####################################################################################
// declare Integer-Parameter-Variable 
//####################################################################################
// name              name of param-variable 
// grouphierarchy    nested hierachy names e.g. "vehicle.body"
// unit              unit if needed, if not punit = 0
// setdef            setdefault value true/false
// defvalue          Default value e.g.  "23"
Simula::status_type Simula::CPar::declareVar(char *name, char *grouphierarchy, char *punit, int  *pvar,bool allowdef, char *defvalue)
{
  // neue Variable anlegen
  Simula::CParVar<int> * pParVar = new Simula::CParVar<int>;
  if( pParVar->getStatus() != STATUS_OKAY )
  {
    Status = STATUS_NOT_OKAY;
    ErrText.append("declareVar(int): Error with new CParVar\n");
    ErrText.append(pParVar->getErrText());
    Log.setLogPrintToScreen(true);
    Log.writeEndL(ErrText);
    delete pParVar;
    return Status;
  }
  // Name setzen
  pParVar->setName(name);
  // max length
  maxLengthVarName = SIMULA_MAX(pParVar->getNameLength(),maxLengthVarName);

  // Gruppen Hierarchie 
  pParVar->setGroupHierarchy(grouphierarchy);
  // Einheit setzen
  pParVar->setUnit(punit);
  // max length
  maxLengthUnit = SIMULA_MAX(pParVar->getUnitLength(),maxLengthUnit);

  // Pointer
  pParVar->setValuePointer(pvar);
  // Default Value
  if( allowdef )
  {
	  if( pParVar->setDefVal(defvalue) != STATUS_OKAY )
	  {
        Status = STATUS_NOT_OKAY;
        std::string t = "Error declareVar: Parameter (Name: " + pParVar->getName() + ",GroupHierachy: " + Hlp::StringConcate(pParVar->getGroupHierachy(),Simula::DELIM_HIERACHY) + ") Defaultwert <" + defvalue + "> konnte nicht gelesen werden !!!" ;
        ErrText.append("\n");
        ErrText.append(t);
        Log.setLogPrintToScreen(true);
        Log.writeEndL(ErrText);
        delete pParVar;
        return Status;
	  }
  }

  // Parameter in die Liste anfuegen
  ParList.push_back(pParVar);

  return Status;
}
//####################################################################################
// declare Doubel-Parameter-Variable 
//####################################################################################
// name              name of param-variable 
// grouphierarchy    nested hierachy names e.g. "vehicle.body"
// unit              unit if needed, if not punit = 0
// setdef            setdefault value true/false
// defvalue          Default value e.g.  "23.1"
Simula::status_type Simula::CPar::declareVar(char *name, char *grouphierarchy, char *punit, double  *pvar,bool allowdef, char *defvalue)
{
  // neue Variable anlegen
  Simula::CParVar<double> * pParVar = new Simula::CParVar<double>;
  if( pParVar->getStatus() != STATUS_OKAY )
  {
    Status = STATUS_NOT_OKAY;
    ErrText.append("Error declareVar(double): Error with new CParVar\n");
    ErrText.append(pParVar->getErrText());
    Log.setLogPrintToScreen(true);
    Log.writeEndL(ErrText);
    delete pParVar;
    return Status;
  }
  // Name setzen
  pParVar->setName(name);
  // max length
  maxLengthVarName = SIMULA_MAX(pParVar->getNameLength(),maxLengthVarName);

  // Gruppen Hierarchie 
  pParVar->setGroupHierarchy(grouphierarchy);
  // Einheit setzen
  pParVar->setUnit(punit);
  // max length
  maxLengthUnit = SIMULA_MAX(pParVar->getUnitLength(),maxLengthUnit);

  // Pointer
  pParVar->setValuePointer(pvar);
  // Default Value
  if( defvalue == NULL ) allowdef = false;
  if( allowdef )
  {

	  if( pParVar->setDefVal(defvalue) != STATUS_OKAY )
	  {
        Status = STATUS_NOT_OKAY;
        std::string t = "Error declareVar: Parameter (Name: " + pParVar->getName() + ",GroupHierachy: " + Hlp::StringConcate(pParVar->getGroupHierachy(),Simula::DELIM_HIERACHY) + ") Defaultwert <" + defvalue + "> konnte nicht gelesen werden !!!" ;
        ErrText.append("\n");
        ErrText.append(t);
        Log.setLogPrintToScreen(true);
        Log.writeEndL(ErrText);
        delete pParVar;
        return Status;
	  }
  }

  // Parameter in die Liste anfuegen
  ParList.push_back(pParVar);

  return Status;
}
//####################################################################################
// declare string-Parameter-Variable 
//####################################################################################
// name              name of param-variable 
// grouphierarchy    nested hierachy names e.g. "vehicle.body"
// unit              unit if needed, if not punit = 0
// setdef            setdefault value true/false
// defvalue          Default value e.g.  "23.1"
Simula::status_type Simula::CPar::declareVar(char *name, char *grouphierarchy, char *punit, std::string  *pvar,bool allowdef, char *defvalue)
{
  // neue Variable anlegen
  Simula::CParVar<std::string> * pParVar = new Simula::CParVar<std::string>;
  if( pParVar->getStatus() != STATUS_OKAY )
  {
    Status = STATUS_NOT_OKAY;
    ErrText.append("Error declareVar(string): Error with new CParVar\n");
    ErrText.append(pParVar->getErrText());
    Log.setLogPrintToScreen(true);
    Log.writeEndL(ErrText);
    delete pParVar;
    return Status;
  }
  // Name setzen
  pParVar->setName(name);
  // max length
  maxLengthVarName = SIMULA_MAX(pParVar->getNameLength(),maxLengthVarName);

  // Gruppen Hierarchie 
  pParVar->setGroupHierarchy(grouphierarchy);
  // Einheit setzen
  pParVar->setUnit(punit);
  // max length
  maxLengthUnit = SIMULA_MAX(pParVar->getUnitLength(),maxLengthUnit);

  // Pointer
  pParVar->setValuePointer(pvar);
  // Default Value
  if( allowdef )
  {
	  if( pParVar->setDefVal(defvalue) != STATUS_OKAY )
	  {
        Status = STATUS_NOT_OKAY;
        std::string t = "Error declareVar: Parameter (Name: " + pParVar->getName() + ",GroupHierachy: " + Hlp::StringConcate(pParVar->getGroupHierachy(),Simula::DELIM_HIERACHY) + ") Defaultwert <" + defvalue + "> konnte nicht gelesen werden !!!" ;
        ErrText.append("\n");
        ErrText.append(t);
        Log.setLogPrintToScreen(true);
        Log.writeEndL(ErrText);
        delete pParVar;
        return Status;
	  }
  }

  // Parameter in die Liste anfuegen
  ParList.push_back(pParVar);

  return Status;
}
//====================================================================================
//====================================================================================
// setVal Funktionen
//====================================================================================
//####################################################################################
// set Integer-Parameter-Variable-Value 
//####################################################################################
Simula::status_type Simula::CPar::setVal(std::string &name, Simula::vec_string_t &grouphierarchy, std::string &unit, int &val, std::size_t &instance)
{
  std::size_t ivec = searchName(name,grouphierarchy);
  double fac=1.0, offset=0.0;

  // Parameter not found
  if( ivec == Simula::npos )
  {
    Status = STATUS_NOT_OKAY;
    std::string t = "Error setVal(int): Single-Int-Parameter (Name: " + name + ",GroupHierachy: " + Hlp::StringConcate(grouphierarchy,Simula::DELIM_HIERACHY) + " not found in ParList !!!" ;
    ErrText.append("\n");
    ErrText.append(t);
    Log.setLogPrintToScreen(true);
    Log.writeEndL(ErrText);
    return Status;
  }

  Simula::CParVar<int> *pParVar = (Simula::CParVar<int> *)ParList[ivec];

  // proof Unit
  if( pParVar->getHasUnit() )
  {
    std::string set_unit = pParVar->getUnit();
    // Faktor und Offset bestimmen
    //============================
    if( Simula::convertUnit(unit /*=>*/, set_unit,&fac,&offset,ErrText) != Simula::STATUS_OKAY )
    {
      Status = STATUS_NOT_OKAY;
      std::string t = "Error setVal(int): Single-Int-Parameter (Name: " + name + ",GroupHierachy: " + Hlp::StringConcate(grouphierarchy,Simula::DELIM_HIERACHY) + " could not find unitconvertion from<"+unit+"> to <"+set_unit+">!!!" ;
      ErrText.append("\n");
      ErrText.append(t);
      Log.setLogPrintToScreen(true);
      Log.writeEndL(ErrText);
      return Status;
    }
  }

  // fill Value
  pParVar->setVal(val,instance); 
  return Simula::STATUS_OKAY;
}
//####################################################################################
// set Double-Parameter-Variable-Value 
//####################################################################################
Simula::status_type Simula::CPar::setVal(std::string &name, Simula::vec_string_t &grouphierarchy, std::string &unit, double &val, std::size_t &instance)
{
  std::size_t ivec = searchName(name,grouphierarchy);
  double fac=1.0, offset=0.0;

  // Parameter not found
  if( ivec == Simula::npos )
  {
    Status = STATUS_NOT_OKAY;
    std::string t = "Error setVal(double): Single-Int-Parameter (Name: " + name + ",GroupHierachy: " + Hlp::StringConcate(grouphierarchy,Simula::DELIM_HIERACHY) + " not found in ParList !!!" ;
    ErrText.append("\n");
    ErrText.append(t);
    Log.setLogPrintToScreen(true);
    Log.writeEndL(ErrText);
    return Status;
  }

  Simula::CParVar<double> *pParVar = (Simula::CParVar<double> *)ParList[ivec];

  // proof Unit
  if( pParVar->getHasUnit() )
  {
    std::string set_unit = pParVar->getUnit();
    // Faktor und Offset bestimmen
    //============================
    if( Simula::convertUnit(unit /*=>*/, set_unit,&fac,&offset,ErrText) != Simula::STATUS_OKAY )
    {
      Status = STATUS_NOT_OKAY;
      std::string t = "Error setVal(double): Single-Int-Parameter (Name: " + name + ",GroupHierachy: " + Hlp::StringConcate(grouphierarchy,Simula::DELIM_HIERACHY) + " could not find unitconvertion from<"+unit+"> to <"+set_unit+">!!!" ;
      ErrText.append("\n");
      ErrText.append(t);
      Log.setLogPrintToScreen(true);
      Log.writeEndL(ErrText);
      return Status;
    }
  }

  // fill Value
  pParVar->setVal(val,instance); 
  return Simula::STATUS_OKAY;
}
//####################################################################################
// set String-Parameter-Variable-Value 
//####################################################################################
Simula::status_type Simula::CPar::setVal(std::string &name, Simula::vec_string_t &grouphierarchy, std::string &unit, std::string &val, std::size_t &instance)
{
  std::size_t ivec = searchName(name,grouphierarchy);
  double fac=1.0, offset=0.0;

  // Parameter not found
  if( ivec == Simula::npos )
  {
    Status = STATUS_NOT_OKAY;
    std::string t = "Error setVal(string): Single-Int-Parameter (Name: " + name + ",GroupHierachy: " + Hlp::StringConcate(grouphierarchy,Simula::DELIM_HIERACHY) + " not found in ParList !!!" ;
    ErrText.append("\n");
    ErrText.append(t);
    Log.setLogPrintToScreen(true);
    Log.writeEndL(ErrText);
    return Status;
  }

  Simula::CParVar<std::string> *pParVar = (Simula::CParVar<std::string> *)ParList[ivec];

  // proof Unit
  if( pParVar->getHasUnit() )
  {
    std::string set_unit = pParVar->getUnit();
    // Faktor und Offset bestimmen
    //============================
    if( Simula::convertUnit(unit /*=>*/, set_unit,&fac,&offset,ErrText) != Simula::STATUS_OKAY )
    {
      Status = STATUS_NOT_OKAY;
      std::string t = "Error setVal(string): Single-Int-Parameter (Name: " + name + ",GroupHierachy: " + Hlp::StringConcate(grouphierarchy,Simula::DELIM_HIERACHY) + " could not find unitconvertion from<"+unit+"> to <"+set_unit+">!!!" ;
      ErrText.append("\n");
      ErrText.append(t);
      Log.setLogPrintToScreen(true);
      Log.writeEndL(ErrText);
      return Status;
    }
  }

  // fill Value
  pParVar->setVal(val,instance); 
  return Simula::STATUS_OKAY;
}
//====================================================================================
//====================================================================================
// get Funktionen
//====================================================================================
//####################################################################################
// get max number of instances in  ParList
//####################################################################################
Simula::s_t Simula::CPar::getMaxInstancesOfParList(void)
{
  Simula::s_t nmax=0;
  Simula::s_t i,n=ParList.size();
  for(i=0;i<n;i++)
  {
    CParVarBase *pVar = ParList[i];
    if( pVar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_INT )
    {
      Simula::CParVar<int> *pVarT = (Simula::CParVar<int> *)pVar;
      nmax = SIMULA_MAX(nmax,pVarT->getNumOfEntities());
    }
    else if( pVar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_DOUBLE )
    {
      Simula::CParVar<double> *pVarT = (Simula::CParVar<double> *)pVar;
      nmax = SIMULA_MAX(nmax,pVarT->getNumOfEntities());
    }
    else if( pVar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_STRING )
    {
      Simula::CParVar<std::string> *pVarT = (Simula::CParVar<std::string> *)pVar;
      nmax = SIMULA_MAX(nmax,pVarT->getNumOfEntities());
    }
  }
  return nmax;
}
//====================================================================================
// get list of groupnames  ParList with grouplistabove
//====================================================================================
void Simula::CPar::getListOfGroupFromParList(Simula::vec_string_t &grouplistabove, Simula::vec_string_t &listGroup)
{
  Simula::s_t          ngroupsabove = grouplistabove.size();
  Simula::s_t i,n=ParList.size();

  listGroup.clear();
  for(i=0;i<n;i++)
  {
    CParVarBase *pVar = ParList[i];
    Simula::vec_string_t list = pVar->getGroupHierachy();                                   // group-hierachy-list of variable
    if( Hlp::IsStringVec0InStringVec1(grouplistabove,list) && list.size() > ngroupsabove )  // group-hierachie-list above is in variable-list
                                                                                            // and minimum one group more
    {
      // if the new group which is next above-group-list is not in collectlist psuh into
      if( !Hlp::IsStringInStringVec(list[ngroupsabove],listGroup) )
      {
		    listGroup.push_back(list[ngroupsabove]);
      }
    }
  }
}
//====================================================================================
// get list of Varnames of  ParList with grouplist
//====================================================================================
void Simula::CPar::getListOfVarFromParList(Simula::vec_string_t &grouplist,Simula::vec_uint_t &uilist)
{
  Simula::s_t          ngroups = grouplist.size();
  Simula::s_t i,n=ParList.size();

  uilist.clear();
  for(i=0;i<n;i++)
  {
    CParVarBase *pVar = ParList[i];
    Simula::vec_string_t list = pVar->getGroupHierachy();
    if( Hlp::IsStringVec0InStringVec1(grouplist,list) && list.size() == ngroups )
    {
		uilist.push_back(i);
    }
  }
}
//====================================================================================
//====================================================================================
// search Funktionen
//====================================================================================
//====================================================================================
// search name and  grouphierarchy in  ParList
//====================================================================================
std::size_t Simula::CPar::searchName(std::string &name, Simula::vec_string_t &grouphierarchy)
{
  //ParList durchsuchen
  for(s_t i=0;i<ParList.size();++i)
  {
    // Versuche mit Pbase-Pointer auf GrupHierarchy und Name zuzugreifen ????
    //=======================================================================
    Simula::CParVarBase *pParVar = (Simula::CParVarBase *)ParList[i];
    Simula::vec_string_t gh = pParVar->getGroupHierachy();
    if( compareGroupHierarchy(gh,grouphierarchy) )
    {
      if( pParVar->getName() == name )
      {
        return i;
      }
    }
  }
  return Simula::npos;
}
//====================================================================================
//====================================================================================
// compare-Functions
//====================================================================================
//====================================================================================
// compare  grouphierarchies if same
//====================================================================================
bool Simula::CPar::compareGroupHierarchy(Simula::vec_string_t &grouphierarchy0,Simula::vec_string_t &grouphierarchy1)
{
  std::size_t i,n0 = grouphierarchy0.size();
  std::size_t n1 = grouphierarchy1.size();

  if( n0 != n1 ) return false;
  for(i=0;i<n0;i++)
  {
    if( grouphierarchy0[i] != grouphierarchy0[i] )
    {
      return false;
    }
  }
  return true;
}
//====================================================================================
//====================================================================================
// delete-Functions
//====================================================================================
//####################################################################################
// delete  ParList
//####################################################################################
void Simula::CPar::deleteParList(void)
{
  Simula::s_t i,n=ParList.size();
  for(i=0;i<n;i++)
  {
    CParVarBase *pVar = ParList[i];
    if( pVar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_INT )
    {
      Simula::CParVar<int> *pVarT = (Simula::CParVar<int> *)pVar;
      delete pVarT;
    }
    else if( pVar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_DOUBLE )
    {
      Simula::CParVar<double> *pVarT = (Simula::CParVar<double> *)pVar;
      delete pVarT;
    }
    else if( pVar->getVarType() == Simula::SIMULA_VARTYPE_SINGLE_STRING )
    {
      Simula::CParVar<std::string> *pVarT = (Simula::CParVar<std::string> *)pVar;
      delete pVarT;
    }
    else
    {
      enum Simula::var_type type = pVar->getVarType();
      throw "falsch generierter Type";
    }
  }
  ParList.resize(0);
}
