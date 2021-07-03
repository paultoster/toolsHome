#include "SlfParDef.h"
#include "SlfParData.h"

namespace slf
{ 


//=================================================================    
//=================================================================    
// Konstruktor
//=================================================================    
//=================================================================    
CSlfParData::CSlfParData() {

    Status     = OKAY;
    pI         = 0;
    nI         = 0;

    pIActual   = 0;
    pGActual   = 0;
    pVActual   = 0;
}
//=================================================================    
//=================================================================    
// Destruktor 
//=================================================================    
//=================================================================    
CSlfParData::~CSlfParData() {

  destroyGroups(pI);
  pI = 0;
  nI = 0;
}
//=================================================================    
//=================================================================    
// setInstance
//=================================================================    
//=================================================================    
status_t CSlfParData::setInstance(const char *pname,const char *pcomment/*=""*/)
{
  SSlfParGroup *p = pI;
  slf::CStr          string;
  
  // Name bilden (wenn nicht vorhanden, wird Nummer gewandelt in string)
  //--------------------------------------------------------------------
  if( SlfStrLen(pname) == 0 )
  {
    string.catFormat("%i",nI);
  }
  else
  {
    string = pname;
  }

  // Suchen nach einer Instanz mit dem Namen
  //----------------------------------------
  p        = pI;
  while( p )
  {
    if( string.compare(p->Name) )
    {
      pIActual = p;
      pGActual = 0;
      pVActual = 0;

      /* Kommentar */
      if( SlfStrLen(pcomment) > 0 )
      {
        if( SlfStrLen(p->Comment) > 0 )
        {
          p->Comment.append(SLF_PAR_READ_STRING_SPACE);
        }
        p->Comment.append(pcomment);
      }
      return Status;
    }
    p = p->pNext;
  }

  // neue Instanz erstellen
  //-----------------------
  p = new SSlfParGroup;

  // Name, Kommentar
  p->Name       = string;
  p->Comment    = pcomment;
  p->iHir       = -1; // Kennzeichen Instanz
  p->iPos       = nI;
  p->pParent    = 0;  // gibt keine übergeordnete Struktur
  p->pSubGroup  = 0;
  p->nSubGroup  = 0;
  p->p1DTab     = 0;
  p->n1DTab     = 0;
  p->p2DTab     = 0;
  p->n2DTab     = 0;
  p->pVar      = 0;
  p->nVar      = 0;
  p->pNext      = 0;

  pI = setGroupInVerketteteListe(p,pI);
  ++(nI);
  pIActual      = p;
  pGActual      = 0;
  pVActual      = 0;

  return Status;
}
//=================================================================    
//=================================================================    
// setGroup
// hierachie = 0, bedeutet oberste Hierachie 1, die nächst 
// tiefer Gruppe usw.
//=================================================================    
//=================================================================    
status_t CSlfParData::setGroup(const char *pname,uint16_t hierachie,const char *pcomment/*=""*/)
{
  SSlfParGroup *p;
  SSlfParGroup *pParent;
  slf::CStr          string;
  sint16_t         i;

  // Überprüfen, ob bereits eine Instanz gebildet wird
  // wenn nicht wird angelegt
  //--------------------------------------------------
  if( pI == 0 )
  {
    if( setInstance("") != OKAY )
    {
      return Status;
    }
  }
  
  // Hierachie muss zur aktuellen Hierachie passen
  //----------------------------------------------
  if( hierachie == 0 ) // oberste Gruppe
  {
    pParent = pIActual;
  }
  else if( !pGActual   )
  {
    ErrText.catFormat("CSlfParData::setGroup Error: Es muß die nächste tiefere  Hierachieebene angelegt werden. \n"
                      "Die Gruppenhierachie muss lauten ");
    ErrText.catFormat(SLF_PAR_READ_STRING_START_GROUP);
    ErrText.catFormat("%s%s",pname,SLF_PAR_READ_STRING_END_SUBGROUP);
    Status = NOT_OKAY;
    return Status;
  }
  else if( (hierachie > pGActual->iHir+1)  )
  {
    ErrText.catFormat("CSlfParData::setGroup Error: Es muß die nächste tiefere  Hierachieebene angelegt werden. \n"
                      "Die Gruppenhierachie muss lauten ");
    ErrText.catFormat(SLF_PAR_READ_STRING_START_SUBGROUP);
    for(i=0;i<pGActual->iHir;i++)
    {
      ErrText.cat(SLF_PAR_READ_STRING_START_SUBGROUP);
    }
    ErrText.catFormat("%s%s",pname,SLF_PAR_READ_STRING_END_SUBGROUP);
    for(i=0;i<pGActual->iHir;i++)
    {
      ErrText.cat(SLF_PAR_READ_STRING_END_SUBGROUP);
    }
    Status = NOT_OKAY;
    return Status;
  }
  else // übergeordnete bestimmen
  {
    pParent = pGActual;
    while( pParent->iHir >= hierachie)
    {
      pParent = pParent->pParent;
    }
  }
  // Name darf nicht leer sein
  //--------------------------
  if( SlfStrLen(pname) == 0 )
  {
    ErrText.cat("CSlfParData::setGroup Error: Der Gruppenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  else
  {
    string = pname;
  }

  // Gruppe suchen
  //----------------------------------------
  if( pParent ) 
  {
    p        = pParent->pSubGroup;
    while( p )
    {
      if( string.compare(p->Name) )
      {
        pGActual = p;
        pVActual = 0;
        /* Kommentar */
        if( SlfStrLen(pcomment) > 0 )
        {
          if( SlfStrLen(p->Comment) > 0 )
          {
            p->Comment.append(SLF_PAR_READ_STRING_SPACE);
          }
          p->Comment.append(pcomment);
        }
        return Status;
      }
      p = p->pNext;
    }
  }
  // neue Gruppe erstellen
  //-----------------------
  p = new SSlfParGroup;

  // Name, Kommentar
  p->Name      = string;
  p->Comment   = pcomment; 
  p->iPos      = pParent->nSubGroup;
  p->iHir      = hierachie;
  p->pParent   = pParent;
  p->pSubGroup = 0;
  p->nSubGroup = 0;
  p->p1DTab    = 0;
  p->n1DTab    = 0;
  p->p2DTab    = 0;
  p->n2DTab    = 0;
  p->pVar      = 0;
  p->nVar      = 0;
  p->pNext     = 0;

  pParent->pSubGroup = setGroupInVerketteteListe(p,pParent->pSubGroup);
  ++(pParent->nSubGroup);

  pGActual     = p;
  pVActual     = 0;

  return Status;
}
//=================================================================    
//=================================================================    
// setSpzTab
// Spz-Tabelle wird in aktueller Gruppe gesucht oder angelegt
// 
//=================================================================    
//=================================================================    
//status_t CSlfParData::setSpzTab(char *pname,char *pcomment/*=""*/)
//{
//  return Status;
//}
//=================================================================    
// set1DTab
// Tabelle wird in aktueller Gruppe gesucht oder angelegt
// 
//=================================================================    
//=================================================================    
status_t CSlfParData::set1DTab(const char *pname,const char *pcomment/*=""*/)
{

  SSlfPar1DTab *p;
  SSlfParGroup *pParent;
  slf::CStr      s;

  // Name darf nicht leer sein
  //--------------------------
  if( SlfStrLen(pname) == 0 )
  {
    ErrText.cat("CSlfParData::set1DTab Error: Der Tabellenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  else
  {
    s = pname;
  }
  // Überprüfen, ob bereits eine Instanz gebildet wird
  // wenn nicht wird angelegt
  //--------------------------------------------------
  if( pI == 0 )
  {
    if( setInstance("") != OKAY )
    {
      return Status;
    }
  }

  /* aktuelle Gruppe ist definiert */
  if( pGActual )
  {
    pParent = pGActual;
  }
  /* wird Instance zugeordnet */
  else
  {
    pParent = pIActual;
  }

  p = pParent->p1DTab;
  while( p )
  {
    if( s.compare(p->Name) )
    {
      p1DTActual = p;
      /* Kommentar */
      if( SlfStrLen(pcomment) > 0 )
      {
        if( SlfStrLen(p->Comment) > 0 )
        {
          p->Comment.append(SLF_PAR_READ_STRING_SPACE);
        }
        p->Comment.append(pcomment);
      }
      return Status;
    }
    p = p->pNext;
  }

  // neue Variable erstellen
  //-----------------------
  p = new SSlfPar1DTab;

  // Name, Kommentar
  p->Name      = s;
  p->Comment   = pcomment;
  p->pXVec     = 0;
  p->pYVec     = 0;
  p->pNext     = 0;

  p1DTActual     = p;

  pParent->p1DTab = set1DTabInVerketteteListe(p,pParent->p1DTab);
  ++(pParent->n1DTab);

  return Status;
}
//=================================================================    
//=================================================================    
// set2DTab
// Tabelle wird in aktueller Gruppe gesucht oder angelegt
// 
//=================================================================    
//=================================================================    
status_t CSlfParData::set2DTab(const char *pname,const char *pcomment/*=""*/)
{

  SSlfPar2DTab *p;
  SSlfParGroup *pParent;
  slf::CStr      s;

  // Name darf nicht leer sein
  //--------------------------
  if( SlfStrLen(pname) == 0 )
  {
    ErrText.cat("CSlfParData::set2DTab Error: Der Tabellenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  else
  {
    s = pname;
  }
  // Überprüfen, ob bereits eine Instanz gebildet wird
  // wenn nicht wird angelegt
  //--------------------------------------------------
  if( pI == 0 )
  {
    if( setInstance("") != OKAY )
    {
      return Status;
    }
  }

  /* aktuelle Gruppe ist definiert */
  if( pGActual )
  {
    pParent = pGActual;
  }
  /* wird Instance zugeordnet */
  else
  {
    pParent = pIActual;
  }

  p = pParent->p2DTab;
  while( p )
  {
    if( s.compare(p->Name) )
    {
      p2DTActual = p;
      /* Kommentar */
      if( SlfStrLen(pcomment) > 0 )
      {
        if( SlfStrLen(p->Comment) > 0 )
        {
          p->Comment.append(SLF_PAR_READ_STRING_SPACE);
        }
        p->Comment.append(pcomment);
      }
      return Status;
    }
    p = p->pNext;
  }

  // neue Variable erstellen
  //-----------------------
  p = new SSlfPar2DTab;

  // Name, Kommentar
  p->Name      = s;
  p->Comment   = pcomment;
  p->pXVec     = 0;
  p->pYVec     = 0;
  p->pZMat     = 0;
  p->pNext     = 0;

  p2DTActual     = p;

  pParent->p2DTab = set2DTabInVerketteteListe(p,pParent->p2DTab);
  ++(pParent->n2DTab);

  return Status;
}
//=================================================================    
//=================================================================    
// setVar
// Variable wird in aktueller Gruppe gesucht oder angelegt
// 
//=================================================================    
//=================================================================    
status_t CSlfParData::setVar(const char *pname,const char *punit,const char *pcomment/*=""*/)
{

  SSlfParVar   *p;
  SSlfParGroup *pParent;
  slf::CStr      s;

  // Name darf nicht leer sein
  //--------------------------
  if( SlfStrLen(pname) == 0 )
  {
    ErrText.cat("CSlfParData::setVar Error: Der Variablenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  else
  {
    s = pname;
  }
  // Überprüfen, ob bereits eine Instanz gebildet wird
  // wenn nicht wird angelegt
  //--------------------------------------------------
  if( pI == 0 )
  {
    if( setInstance("") != OKAY )
    {
      return Status;
    }
  }

  /* aktuelle Gruppe ist definiert */
  if( pGActual )
  {
    pParent = pGActual;
  }
  /* wird Instance zugeordnet */
  else
  {
    pParent = pIActual;
  }

  p = pParent->pVar;
  while( p )
  {
    if( s.compare(p->Name) )
    {
      pVActual = p;
      /* Einheit */
      if( SlfStrLen(punit) > 0 )
      {
        p->Unit = punit;
      }
      /* Kommentar */
      if( SlfStrLen(pcomment) > 0 )
      {
        if( SlfStrLen(p->Comment) > 0 )
        {
          p->Comment.append(SLF_PAR_READ_STRING_SPACE);
        }
        p->Comment.append(pcomment);
      }
      return Status;
    }
    p = p->pNext;
  }

  // neue Variable erstellen
  //-----------------------
  p = new SSlfParVar;

  // Name, Kommentar
  p->Name      = s;
  p->Comment   = pcomment;
  p->Unit      = punit;
  p->Mat       = 0;
  //p->StrMVal.clear();
  p->isString  = false;
  p->pNext     = 0;

  pVActual     = p;

  pParent->pVar = setVarInVerketteteListe(p,pParent->pVar);
  ++(pParent->nVar);

  return Status;
}
//=================================================================    
//=================================================================    
// setVarVal(strm)
// Matrix mit strings setzen
// 
//=================================================================    
//=================================================================    
status_t CSlfParData::setVarVal(slf::CStrM &strm)
{
  pVActual->StrMVal  = strm;
  pVActual->isString = true;
  return Status;
}
//=================================================================    
//=================================================================    
// setVarVal(mat)
// Matrix mit double setzen
// 
//=================================================================    
//=================================================================    
status_t CSlfParData::setVarVal(Matrix_t &mat)
{
  pVActual->Mat = MatrixCopy(mat);
  pVActual->isString = false;
  return Status;
}
//=================================================================    
//=================================================================    
// setActInstanceComment
//=================================================================    
//=================================================================    
//status_t CSlfParData::setActInstanceComment(char *pcomment)
//{
//  // neue Instanz anlegen, falls noch keine vorhanden
//  //-------------------------------------------------
//  if( nI == 0 )
//  {
//    return setInstance("",pcomment);
//  }
//  else
//  {
//    pI->Comment = pcomment;
//  }
//  return Status;
//}
//=================================================================    
//=================================================================    
// getActInstance
// aktuelle instanz in hierachische Struct Liste (strv)
//=================================================================    
//=================================================================
status_t CSlfParData::getActInstance(slf::CStrV &strv)
{
  strv.clear();
  strv.append(pIActual->Name);
  return Status;
}
//=================================================================    
//=================================================================    
// getActGroup
// aktuelle Gruppe in hierachische Struct Liste (strv)
//=================================================================    
//=================================================================
status_t CSlfParData::getActGroup(slf::CStrV &strv)
{
  strv.clear();
  if( pGActual )
  {
    strv.append(pGActual->Name);
    SSlfParGroup *p = pGActual->pParent;
    while( p != 0 )
    {
      strv.insert(p->Name,0);
      p = p->pParent;
    }
  }
  else if( pIActual )
  {
    strv.append(pIActual->Name);
  }
  return Status;
}
//=================================================================    
//=================================================================    
// getActVar
// aktuelle Variable in hierachische Struct Liste (strv)
//=================================================================    
//=================================================================
status_t CSlfParData::getActVar(slf::CStrV &strv)
{
  if( !pVActual )
  {
    ErrText.cat("CSlfParData::getActVar Error: Variable ist noch nicht definiert");
    Status = NOT_OKAY;
    return Status;
  }
  if( getActGroup(strv) != OKAY )
  {
    return Status;
  }
  /* Variable */
  strv.append(pVActual->Name);

  return Status;
}
//=================================================================    
//=================================================================    
// getAct1DTab
// aktuelle Tabellenname in hierachische Struct Liste (strv)
//=================================================================    
//=================================================================
status_t CSlfParData::getAct1DTab(slf::CStrV &strv)
{
  if( !p1DTActual )
  {
    ErrText.cat("CSlfParData::getAct1DTab Error: 1D-Tabelle ist noch nicht definiert");
    Status = NOT_OKAY;
    return Status;
  }
  if( getActGroup(strv) != OKAY )
  {
    return Status;
  }
  /* Variable */
  strv.append(p1DTActual->Name);

  return Status;
}
//=================================================================    
//=================================================================    
// getAct2DTab
// aktuelle Tabellename in hierachische Struct Liste (strv)
//=================================================================    
//=================================================================
status_t CSlfParData::getAct2DTab(slf::CStrV &strv)
{
  if( !p2DTActual )
  {
    ErrText.cat("CSlfParData::getAct2DTab Error: 1D-Tabelle ist noch nicht definiert");
    Status = NOT_OKAY;
    return Status;
  }
  if( getActGroup(strv) != OKAY )
  {
    return Status;
  }
  /* Variable */
  strv.append(p2DTActual->Name);

  return Status;
}
//=================================================================    
//=================================================================    
// getIInstanceName
// Name der iten Instanz
//=================================================================    
//=================================================================
const char *CSlfParData::getInstName(uint16_t isearch)
{
  if( isearch >= nI )
  {
    return 0;
  }
  uint16_t icount = 0;
  SSlfParGroup *p = pI;
  while( p )
  {
    if( isearch == icount )
    {
      return p->Name.c_str();
    }
    ++icount;
    p = p->pNext;
  }
  return 0;

}
//=================================================================    
//=================================================================    
// getGroupComment
// Gibt den Kommentar zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe) etc
//=================================================================    
//=================================================================
const char *CSlfParData::getGroupComment(slf::CStrV &strv)
{
  SSlfParGroup *p = getGroupPointer(strv);
  if( p ) return p->Comment.c_str();
  else    return 0;
}
//=================================================================    
//=================================================================    
// getNSubGroup
// Gibt Anzahl Untergruppen zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe) etc
//=================================================================    
//=================================================================
uint16_t CSlfParData::getNSubGroup(slf::CStrV &strv)
{
  SSlfParGroup *p = getGroupPointer(strv);
  if( p ) return p->nSubGroup;
  else    return 0;
}
//=================================================================    
//=================================================================    
// getSubGroupName
// Gibt Name der iten Untergruppe zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe) etc
//=================================================================    
//=================================================================
const char *CSlfParData::getSubGroupName(slf::CStrV &strv, uint16_t isearch)
{
  SSlfParGroup *p = getGroupPointer(strv);
  if( !p ) return 0;

  if( isearch >= p->nSubGroup )
  {
    return 0;
  }
  uint16_t icount = 0;
  p = p->pSubGroup;
  while( p )
  {
    if( isearch == icount )
    {
      return p->Name.c_str();
    }
    ++icount;
    p = p->pNext;
  }
  return 0;
}
//=================================================================    
//=================================================================    
// getNVar
// Gibt Anzahl Variablen zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe)...VarName etc
//=================================================================    
//=================================================================
uint16_t CSlfParData::getNVar(slf::CStrV &strv)
{
  SSlfParGroup *p = getGroupPointer(strv);
  if( p ) return p->nVar;
  else    return 0;
}
//=================================================================    
//=================================================================    
// getVarName
// Gibt Name der iten Variable zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe) etc
//=================================================================    
//=================================================================
const char *CSlfParData::getVarName(slf::CStrV &strv, uint16_t isearch)
{
  SSlfParGroup *p = getGroupPointer(strv);
  if( !p ) return 0;

  if( isearch >= p->nVar )
  {
    return 0;
  }
  uint16_t icount = 0;
  SSlfParVar *pv = p->pVar;
  while( pv )
  {
    if( isearch == icount )
    {
      return pv->Name.c_str();
    }
    ++icount;
    pv = pv->pNext;
  }
  return 0;
}
//=================================================================    
//=================================================================    
// getVarComment
// Gibt Kommentar der Variable zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe)...VarName
//=================================================================    
//=================================================================
const char *CSlfParData::getVarComment(slf::CStrV &strv)
{
  SSlfParVar *pv = getVarPointer(strv);
  if( pv )   return pv->Comment.c_str();
  else       return "";
}
//=================================================================    
//=================================================================    
// getVarUnit
// Gibt Einheit der Variable zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe)...VarName
//=================================================================    
//=================================================================
const char *CSlfParData::getVarUnit(slf::CStrV &strv)
{
  SSlfParVar *pv = getVarPointer(strv);
  if( pv )   return pv->Unit.c_str();
  else       return "";
}
//=================================================================    
//=================================================================    
// getVarIsString
// Gibt isString-Flag der Variable zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe)...VarName
//=================================================================    
//=================================================================
bool CSlfParData::getVarIsString(slf::CStrV &strv)
{
  SSlfParVar *pv = getVarPointer(strv);
  if( pv )   return pv->isString;
  else       return false;
}
//=================================================================    
//=================================================================    
// getVarStringM
// Gibt string-Matrix der Variable zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe)...VarName
//=================================================================    
//=================================================================
status_t CSlfParData::getVarStringM(slf::CStrV &strv,slf::CStrM &strvm)
{
  SSlfParVar *pv = getVarPointer(strv);
  if( pv )
  {
    strvm = pv->StrMVal;
  }
  else
  {
    ErrText.catFormat("CSlfParData::getVarStringM Error: Die VAriable konnte nicht gefunden werden: ");
    for(uint32_t i=0;i<strv.getNrows();i++)
    { 
      ErrText.cat(strv.get_str(i));
      if( i < strv.getNrows()-1 )
      {
        ErrText.cat(SLF_PAR_READ_STRING_ATRIBUTE_DELIM);
      }
    }
    Status = NOT_OKAY;
  }
  return Status;
}
//=================================================================    
//=================================================================    
// getVarMat
// Gibt Matrix mat der Variable zurück zur Gruppenstruktur mit Name
// aus strv sortiert nach <instance>[Gruppe](Untergruppe)...VarName
//=================================================================    
//=================================================================
status_t CSlfParData::getVarMat(slf::CStrV &strv,Matrix_t &mat)
{
  SSlfParVar *pv = getVarPointer(strv);
  if( pv )
  {
    mat = MatrixCopy(pv->Mat);
  }
  else
  {
    ErrText.catFormat("CSlfParData::getVarMat Error: Die VAriable konnte nicht gefunden werden: ");
    for(uint32_t i=0;i<strv.getNrows();i++)
    { 
      ErrText.cat(strv.get_str(i));
      if( i < strv.getNrows()-1 )
      {
        ErrText.cat(SLF_PAR_READ_STRING_ATRIBUTE_DELIM);
      }
    }
    Status = NOT_OKAY;
  }
  return Status;
}
//=================================================================    
//=================================================================    
//=================================================================    
// private :
//=================================================================    
//=================================================================  
//=================================================================    
//=================================================================    
// copyGroup class
//=================================================================    
//=================================================================    
CSlfParData& CSlfParData::operator=(const CSlfParData& s)
{

  destroyGroups(pI);

  SSlfParGroup *ps = s.pI;
  SSlfParGroup *p;
  while( ps )
  {
    p = new SSlfParGroup;
    copyGroup(ps,p,0);
    pI = setGroupInVerketteteListe(p,pI);
    ++(nI);
    pIActual = p;

    ps = ps->pNext;
  }

  return *this;                 // return *this
}
void CSlfParData::copyGroup(SSlfParGroup *ps,SSlfParGroup *pd,SSlfParGroup *pparent)
{

  pd->Name              = ps->Name;
  pd->Comment           = ps->Comment;
  pd->iHir              = ps->iHir;
  pd->iPos              = ps->iPos;
  pd->pNext             = 0;
  pd->pParent           = pparent;

  // Untergruppen kopieren
  //----------------------
  pd->nSubGroup    = 0;
  pd->pSubGroup    = 0;
  SSlfParGroup *p1 = ps->pSubGroup;
  SSlfParGroup *p;
  while( p1 )
  {
    p = new SSlfParGroup;
    copyGroup(p1,p,pd);
    pd->pSubGroup = setGroupInVerketteteListe(p,pd->pSubGroup);
    ++(pd->nSubGroup);
    pGActual = p;

    p1 =p1->pNext;
  }

  // 1D-Tabelle kopieren
  //--------------------
  pd->n1DTab      = 0;
  pd->p1DTab      = 0;

  // 2D-Tabelle kopieren
  //--------------------
  pd->n2DTab      = 0;
  pd->p2DTab      = 0;

  // Variablen kopieren
  //-------------------
  pd->nVar        = 0;
  pd->pVar        = 0;
  SSlfParVar *pv1 = ps->pVar;
  SSlfParVar *pv;
  while( pv1 )
  {
    pv = new SSlfParVar;
    copyVar(pv1,pv);
    pd->pVar = setVarInVerketteteListe(pv,pd->pVar);
    ++(pd->nVar);
    pVActual = pv;

    pv1 = pv1->pNext;
  }
}
void CSlfParData::copyVar(SSlfParVar *pvs,SSlfParVar *pvd)
{
  pvd->Name        = pvs->Name;
  pvd->Comment     = pvs->Comment;
  pvd->Unit        = pvs->Unit;
  pvd->isString    = pvs->isString;
  pvd->pNext       = 0;

  if(pvd->isString)
  {
    pvd->StrMVal = pvs->StrMVal;
    pvd->Mat     = 0; 
  }
  else
  {
    pvd->Mat = MatrixCopy(pvs->Mat);
  }
  
}
//=================================================================    
//=================================================================    
// destroyGroups
//=================================================================    
//=================================================================    
void CSlfParData::destroyGroups(SSlfParGroup *pg)
{
  while( pg )
  {
    /* 1DTab */
    destroy1DTab(pg->p1DTab);

    /* 2DTab */
    destroy2DTab(pg->p2DTab);

    /* Variable */
    destroyVar(pg->pVar);

    /* Subgroup */
    destroyGroups(pg->pSubGroup);

    SSlfParGroup *p = pg->pNext;
    delete pg;
    pg = p;
  }
}
//=================================================================    
//=================================================================    
// destroy1DTab
//=================================================================    
//=================================================================    
void CSlfParData::destroy1DTab(SSlfPar1DTab *pt)
{
  while( pt )
  {
    destroyVar(pt->pXVec);
    destroyVar(pt->pYVec);

    SSlfPar1DTab *p = pt->pNext;
    delete pt;
    pt = p;
  }
}
//=================================================================    
//=================================================================    
// destroy2DTab
//=================================================================    
//=================================================================    
void CSlfParData::destroy2DTab(SSlfPar2DTab *pt)
{
  while( pt )
  {
    destroyVar(pt->pXVec);
    destroyVar(pt->pYVec);
    destroyVar(pt->pZMat);

    SSlfPar2DTab *p = pt->pNext;
    delete pt;
    pt = p;
  }
}
//=================================================================    
//=================================================================    
// destroyVar
//=================================================================    
//=================================================================    
void CSlfParData::destroyVar(SSlfParVar *pv)
{
  while( pv )
  {
    if( !pv->isString )
    {
      FreeMatrix(pv->Mat);
    }

    SSlfParVar *p = pv->pNext;
    delete pv;
    pv = p;
  }
}
//=================================================================    
//=================================================================    
// setGroupInVerketteteListe
// Hängt an die Gruppe/Untergruppe pg als nächstes p an
//=================================================================    
//=================================================================    
CSlfParData::SSlfParGroup * CSlfParData::setGroupInVerketteteListe(SSlfParGroup *p,SSlfParGroup *pg)
{
  if( !pg )
  {
    pg = p;
  }
  else
  {
    SSlfParGroup *p1 = pg;
    while( p1->pNext )
    {
      p1 = p1->pNext;
    }
    p1->pNext     = p;
  }

  return pg;
}
//=================================================================    
//=================================================================    
// getGroupPointer
// Sucht Pointer der Gruppenstruktur
// strv enthält <instance>[Gruppe](Untergruppe) etc.
//=================================================================    
//=================================================================    
CSlfParData::SSlfParGroup *CSlfParData::getGroupPointer(slf::CStrV &strv)
{
  SSlfParGroup *p = pI;

  uint32_t i,n = strv.getNrows();
  uint8_t   flag;
  /* Instance */
  for(i=0;i<n;i++)
  {
    flag = 0;
    while(p)
    {
      if( p->Name == strv.get_str(i)  )
      {
        flag = 1;
        
        // prüfen ob das die gewünschte Gruppe ist
        if( i+1 == n )
        {
          return p;
        }
        else
        {
          // Nächste Untergruppe
          p = p->pSubGroup;
          break;
        }
      }
      p = p->pNext;
    }
    if( !flag )
    {
      return 0;
    }
  }
  return 0;
}
//=================================================================    
//=================================================================    
// getVarPointer
// Sucht VarPointer in der Gruppenstruktur
// strv enthält <instance>[Gruppe](Untergruppe)...VarName etc.
//=================================================================    
//=================================================================    
CSlfParData::SSlfParVar *CSlfParData::getVarPointer(slf::CStrV &strv)
{
  slf::CStr  s  = strv.get_last_str();
  slf::CStrV sv = strv;
  /* letzte ist Varname => löschen */
  sv.delete_last();

  SSlfParGroup *p = getGroupPointer(sv);
  if( !p ) return 0;

  SSlfParVar *pv = p->pVar;
  while( pv )
  {
    if( pv->Name == s )
    {
      return pv;
    }
    pv = pv->pNext;
  }
  return 0;
}
//=================================================================    
//=================================================================    
// set1DTabInVerketteteListe
// Hängt an die 1DTabellenliste pt als nächstes p an
//=================================================================    
//=================================================================    
CSlfParData::SSlfPar1DTab * CSlfParData::set1DTabInVerketteteListe(SSlfPar1DTab *p,SSlfPar1DTab *pt)
{
  if( !pt )
  {
    pt = p;
  }
  else
  {
    SSlfPar1DTab *p1 = pt;
    while( p1->pNext )
    {
      p1 = p1->pNext;
    }
    p1->pNext     = p;
  }

  return pt;
}
//=================================================================    
//=================================================================    
// set2DTabInVerketteteListe
// Hängt an die 1DTabellenliste pt als nächstes p an
//=================================================================    
//=================================================================    
CSlfParData::SSlfPar2DTab * CSlfParData::set2DTabInVerketteteListe(SSlfPar2DTab *p,SSlfPar2DTab *pt)
{
  if( !pt )
  {
    pt = p;
  }
  else
  {
    SSlfPar2DTab *p1 = pt;
    while( p1->pNext )
    {
      p1 = p1->pNext;
    }
    p1->pNext     = p;
  }

  return pt;
}
//=================================================================    
//=================================================================    
//=================================================================    
// setVarInVerketteteListe
// Hängt an die Variablenliste pv als nächstes p an
//=================================================================    
//=================================================================    
CSlfParData::SSlfParVar * CSlfParData::setVarInVerketteteListe(SSlfParVar *p,SSlfParVar *pv)
{
  if( !pv )
  {
    pv = p;
  }
  else
  {
    SSlfParVar *p1 = pv;
    while( p1->pNext )
    {
      p1 = p1->pNext;
    }
    p1->pNext     = p;
  }

  return pv;
}
} // namespace