#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfParRead.h"


//=================================================================    
//=================================================================    
// Konstruktor
//=================================================================    
//=================================================================    
CSlfParRead::CSlfParRead() {

    Status   = OKAY;

    ParReadCopyStruct = 0;
    ParReadShapeStruct = 0;

    pParReadData = 0;

}
//=================================================================    
//=================================================================    
// Destruktor 
//=================================================================    
//=================================================================    
CSlfParRead::~CSlfParRead() {

  SSlfParReadCopyStruct *p = ParReadCopyStruct;
  while( p )
  {
    ParReadCopyStruct = p->pNext;
    delete p;
    p = ParReadCopyStruct;
  }

  SSlfParReadShapeStruct *ps = ParReadShapeStruct;
  while( ps )
  {
    ParReadShapeStruct = ps->pNext;
    delete ps;
    ps = ParReadShapeStruct;
  }

}
//=================================================================    
//=================================================================    
// Read Parameterfile
//=================================================================    
//=================================================================    
status_t CSlfParRead::readParFile(char *par_file,CSlfParData *ppar_read_data)
{
  pParReadData = ppar_read_data;
  
  // Parameterdatei öffnen
  //----------------------------------
  Status = ParReadFile.open(par_file);
  if( Status != OKAY )
  {
    ErrText.cat("CSlfParRead::readParFile: Error in ParReadFile.open\n");
    ErrText.cat(ParReadFile.getErrText());
    ParReadFile.close();
    return Status;
  }

  Status = startReadParFile();


  // Parameterdatei schliessen
  //---------------------------
  ParReadFile.close();

  return Status;
}
status_t CSlfParRead::startReadParFile(void)
{
  bool flag = true;
  char c;
  char  *csuch[3] = {SLF_PAR_READ_STRING_SPACE 
                    ,SLF_PAR_READ_STRING_END_LINE 
                    ,SLF_PAR_READ_STRING_TAB};
  uint16_t n;

  while( flag )
  {
    /* Das nächste gültige Zeichen lesen */
    /*-----------------------------------*/
    n = ParReadFile.search_until_not(csuch,3);
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
      Status = NOT_OKAY;
      return Status;
    }
    if( ParReadFile.getReadStatus() == CSlfParReadFile::PAR_READ_FILE_END )
    {
      flag = 0;
    }
    else
    {
      ParReadFile.throwaway(n);

      c = ParReadFile.peekChar();

      switch(c)
      {
      /* Fehler */
      case 0:
        if( ParReadFile.isStatusNotOkay() )
        {
          ErrText.cat(ParReadFile.getErrText());
          Status = NOT_OKAY;
        }
        return Status;
        break;

      /* Kommentar */
      /*-----------*/
      case SLF_PAR_READ_COMMENT1:
      case SLF_PAR_READ_COMMENT2:
      case SLF_PAR_READ_COMMENT3:

        /* Kommentar überlesen */
        n = ParReadFile.search_until(SLF_PAR_READ_END_LINE);
        if( ParReadFile.isStatusNotOkay() )
        {
          ErrText.cat(ParReadFile.getErrText());
          Status = NOT_OKAY;
          return Status;
        }
        ParReadFile.throwaway(n);
        break;

      /* Instanz definieren */
      /*--------------------*/
      case SLF_PAR_READ_START_INSTANCE:
        
        if( readInstance() != OKAY )
        {
          return Status;
        }
        break;

      /* Gruppe definieren */
      /*--------------------*/
      case SLF_PAR_READ_START_GROUP:
        
        if( readGroup() != OKAY )
        {
          return Status;
        }
        break;
      
      /* Unter Gruppe definieren */
      /*-------------------------*/
      case SLF_PAR_READ_START_SUBGROUP:
        
        if( readSubGroup() != OKAY )
        {
          return Status;
        }
        break;
      
      /* Tabellen definieren */
      /*---------------------*/
      case SLF_PAR_READ_START_TABLE0:

        c = ParReadFile.peekChar(1);

        if( c == SLF_PAR_READ_START_TABLE1 )
        {
          c = ParReadFile.peekChar(2);

          if( c == SLF_PAR_READ_START_TABLE2 )
          {
            if( read2DTable() != OKAY )
            {
              return Status;
            }
          }
          else
          {
            if( read1DTable() != OKAY )
            {
              return Status;
            }
          }
        }
        else
        {
          if( readSpzTable() != OKAY )
          {
            return Status;
          }
        }
        break;
      
      /* Variable definieren */
      /*--------------------*/
      default:

        /* Prüfen, ob Variable vorhanden (erster Buchstabe */
        /* muss alphanumerisch sein                        */
        if( !SlfCharIsAlphNum(c) )
        {
          ErrText.cat("Es wurde keine VAriable, Gruppe oder Tabelle erkannt\n");
          ErrText.cat("<");
          ParReadFile.get_until_eol(ErrText,SLF_PAR_READ_END_LINE,&flag);
          ErrText.cat(">");
          ParReadFile.getZeilenInfo(ErrText);
          Status = NOT_OKAY;
          return Status;
        }
        
        if( readVar() != OKAY )
        {
          return Status;
        }
        break;
      }
    }
  }
  return Status;
}
status_t CSlfParRead::readInstance(void)
{
  bool     flag;
  uint16_t n;
  slf::CStr  string;
  slf::CStrV strv;

  // SLF_PAR_READ_START_INSTANCE weg
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_START_INSTANCE));
  // Lesen bis SLF_PAR_READ_END_INSTANCE
  n = ParReadFile.get_until_eol(string,SLF_PAR_READ_STRING_END_INSTANCE,&flag);

  if( !flag )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readInstance Error: Instance konnte nicht gelesen werden");
    Status = NOT_OKAY;
    return Status;
  }
  if( n == 0 )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readInstance Error: Der Instanzname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  // SLF_PAR_READ_END_INSTANCE rauslesen
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_END_INSTANCE));

  // Instanz anlegen
  //-----------------
  if( pParReadData->setInstance(string.c_str()) != OKAY )
  {
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.append(pParReadData->getErrText());
    Status = NOT_OKAY;
    return Status;
  }

  pParReadData->getActInstance(strv);
  readAttribute(strv,CSlfParRead::TYPE_INSTANCE);

  return Status;
}
status_t CSlfParRead::readGroup(void)
{
  bool     flag;
  uint16_t n;
  slf::CStr  string;
  slf::CStrV strv;

  // SLF_PAR_READ_START_GROUP weg
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_START_GROUP));
  // Lesen bis SLF_PAR_READ_END_GROUP
  n = ParReadFile.get_until_eol(string,SLF_PAR_READ_STRING_END_GROUP,&flag);

  if( !flag )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readGroup Error: Gruppe konnte nicht gelesen werden");
    Status = NOT_OKAY;
    return Status;
  }
  if( n == 0 )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readGroup Error: Der Gruppenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  // SLF_PAR_READ_STRING_END_GROUP rauslesen
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_END_GROUP));

  // Gruppe anlegen (hierachieebene null)
  //-------------------------------------
  if( pParReadData->setGroup(string.c_str(),0) != OKAY )
  {
    ErrText.cat(pParReadData->getErrText());
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readGroup Error:");
    Status = NOT_OKAY;
    return Status;
  }

  // Atribute auslesen
  pParReadData->getActGroup(strv);
  readAttribute(strv,CSlfParRead::TYPE_GROUP);

  return Status;
}
status_t CSlfParRead::readSubGroup(void)
{
  bool     flag;
  uint16_t n;
  uint16_t hierachie;
  slf::CStr  string;
  slf::CStrV strv;

  // Hierachie bestimmen
  n = ParReadFile.search_until_not(SLF_PAR_READ_STRING_START_SUBGROUP);
  hierachie = n;
  if( ParReadFile.isStatusNotOkay() )
  {
    ErrText.cat(pParReadData->getErrText());
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readSubGroup Error");
    Status = NOT_OKAY;
    return Status;
  }

  // entsprechende Anzahl SLF_PAR_READ_START_SUBGROUP weg
  ParReadFile.throwaway(n);
  // Lesen bis SLF_PAR_READ_END_SUBGROUP
  n = ParReadFile.get_until_eol(string,SLF_PAR_READ_STRING_END_SUBGROUP,&flag);

  if( !flag )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readSubGroup Error: Gruppe konnte nicht gelesen werden");
    Status = NOT_OKAY;
    return Status;
  }
  if( n == 0 )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readSubGroup Error: Der Gruppenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }

  // SLF_PAR_READ_STRING_END_GROUP rauslesen
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_END_SUBGROUP)*hierachie);

  // UnterGruppe anlegen 
  //-------------------------------------
  if( pParReadData->setGroup(string.c_str(),hierachie) != OKAY )
  {
    ErrText.cat(pParReadData->getErrText());
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readGroup Error: Der Gruppenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }

  // Atribute auslesen
  pParReadData->getActGroup(strv);
  readAttribute(strv,CSlfParRead::TYPE_GROUP);

  return Status;
}
status_t CSlfParRead::readSpzTable(void)
{
  return Status;
}
status_t CSlfParRead::read1DTable(void)
{
  bool     flag;
  uint16_t n;
  slf::CStr  string;
  slf::CStrV strv;

  // SLF_PAR_READ_STRING_START_TABLE0 weg
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_START_TABLE0));
  // Lesen bis SLF_PAR_READ_END_GROUP
  n = ParReadFile.get_until_eol(string,SLF_PAR_READ_STRING_END_TABLE0,&flag);

  if( !flag )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::read1DTable Error: Tabellenname konnte nicht gelesen werden");
    Status = NOT_OKAY;
    return Status;
  }
  if( n == 0 )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::read1DTable Error: Der Tabellenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }
  // SLF_PAR_READ_STRING_END_GROUP rauslesen
  ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_END_TABLE0));

  // Tabelle anlegen
  //-------------------------------------
  if( pParReadData->set1DTab(string.c_str()) != OKAY )
  {
    ErrText.cat(pParReadData->getErrText());
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::read1DTab Error:");
    Status = NOT_OKAY;
    return Status;
  }

  // Atribute auslesen
  pParReadData->getAct1DTab(strv);
  readAttribute(strv,CSlfParRead::TYPE_1DTAB);


  return Status;
}
status_t CSlfParRead::read2DTable(void)
{
  bool     flag;
  uint16_t n;
  slf::CStr  string;
  slf::CStrV strv;

  // zwei Zeichen für 2D-Tabelle
  ParReadFile.throwaway(2);

  // Lesen bis SLF_PAR_READ_END_TABLE1
  n = ParReadFile.get_until_eol(string,SLF_PAR_READ_STRING_END_TABLE1,&flag);
  if( !flag )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::read2DTab Error: Tabellenname konnte nicht gelesen werden");
    Status = NOT_OKAY;
    return Status;
  }
  if( n == 0 )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::read2DTab Error: Der Tabellenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }

  /* Endezeichen Tabelle weg */
  ParReadFile.search_throw_until_not(SLF_PAR_READ_STRING_END_TABLE1);
  ParReadFile.search_throw_until_not(SLF_PAR_READ_STRING_END_TABLE0);

  // Tabelle anlegen 
  //-------------------------------------
  if( pParReadData->set2DTab(string.c_str()) != OKAY )
  {
    ErrText.cat(pParReadData->getErrText());
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::read1 Error: Der Gruppenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }

  return Status;
}
status_t CSlfParRead::readVar(void)
{
  bool     flag;
  uint16_t n;
  char  c;
  char  *csuch[9] = {SLF_PAR_READ_STRING_SPACE 
                    ,SLF_PAR_READ_STRING_TAB
                    ,SLF_PAR_READ_STRING_END_LINE 
                    ,SLF_PAR_READ_STRING_START_UNIT
                    ,SLF_PAR_READ_STRING_EQUAL
                    ,SLF_PAR_READ_STRING_ATRIBUTE_DELIM
                    ,SLF_PAR_READ_STRING_COMMENT1
                    ,SLF_PAR_READ_STRING_COMMENT2
                    ,SLF_PAR_READ_STRING_COMMENT3
                    };
  slf::CStr  varname,unit;
  slf::CStrV strv;

  // Variablenname lesen bis 
  n = ParReadFile.get_until_eol(varname,csuch,9,&flag);

  if( !flag )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readGroup Error: Variable konnte nicht gelesen werden");
    Status = NOT_OKAY;
    return Status;
  }
  if( n == 0 )
  {
    if( ParReadFile.isStatusNotOkay() )
    {
      ErrText.cat(ParReadFile.getErrText());
    }
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.cat("\nCSlfParRead::readVar Error: Der Variablenname ist leer");
    Status = NOT_OKAY;
    return Status;
  }

  /* nächstes Zeichen */
  ParReadFile.search_throw_until_not(csuch,2);
  c = ParReadFile.peekChar();
  
  if( c == SLF_PAR_READ_END_LINE )
  {
    ErrText.catFormat("\nCSlfParRead::readVar Error: Der Variablenname <%s> hat keinen Wert"
                     ,varname.c_str());
    ParReadFile.getZeilenInfo(ErrText);
    Status = NOT_OKAY;
    return Status;
  }

  /* Enheit auslesen */
  if( c == SLF_PAR_READ_START_UNIT )
  {
    // SLF_PAR_READ_START_UNIT weg
    ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_START_UNIT));
    // Lesen bis SLF_PAR_READ_END_UNIT
    n = ParReadFile.get_until_eol(unit,SLF_PAR_READ_STRING_END_UNIT,&flag);

    if( !flag )
    {
      if( ParReadFile.isStatusNotOkay() )
      {
        ErrText.cat(ParReadFile.getErrText());
      }
      ParReadFile.getZeilenInfo(ErrText);
      ErrText.catFormat("\nCSlfParRead::readVar Error: Variable <%s> konnte nicht gelesen werden"
                       ,varname.c_str());
      Status = NOT_OKAY;
      return Status;
    }
    if( n == 0 )
    {
      if( ParReadFile.isStatusNotOkay() )
      {
        ErrText.cat(ParReadFile.getErrText());
      }
      ParReadFile.getZeilenInfo(ErrText);
      ErrText.catFormat("\nCSlfParRead::readVar Error: Die Einheitsangabe zu Variablenname <%s> ist leer"
                       ,varname.c_str());
      Status = NOT_OKAY;
      return Status;
    }
    // SLF_PAR_READ_STRING_END_UNIT rauslesen
    ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_END_UNIT));

    /* bereinigen */
    unit.elimAnfEndC();

    /* nächstes Zeichen */
    ParReadFile.search_throw_until_not(csuch,2);
    c = ParReadFile.peekChar();
  }

  /* Variable setzen */
  if( pParReadData->setVar(varname.c_str(),unit.c_str()) != OKAY )
  {
    ParReadFile.getZeilenInfo(ErrText);
    ErrText.append(pParReadData->getErrText());
    Status = NOT_OKAY;
    return Status;
  }

  /* Wert auslesen */
  if( c == SLF_PAR_READ_EQUAL )
  {
    ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_EQUAL));
    if( readVarVal(varname.c_str()) != OKAY )
    {
      return Status;
    }
  }

  /* Attribute auslesen */
  pParReadData->getActVar(strv);
  readAttribute(strv,CSlfParRead::TYPE_VAR);
  return Status;
}
status_t CSlfParRead::readVarVal(const char *pname)
{
  char        c;
  char        *csuch[2] = {SLF_PAR_READ_STRING_SPACE 
                          ,SLF_PAR_READ_STRING_TAB};
  slf::CStr     str;
  slf::CStrM    strM;
  Matrix_t    mat = 0;
  bool        flag;
  uint16_t    n;
  ParReadFile.search_throw_until_not(csuch,2);
  
  c = ParReadFile.peekChar();

  /* Klammer gefunden */
  if( c == SLF_PAR_READ_START_VAL )
  {
    // SLF_PAR_READ_START_UNIT weg
    ParReadFile.throwaway(1);
    /* Werte bis Klammer ende finden */
    n = ParReadFile.get_until(str,SLF_PAR_READ_END_VAL,&flag);

    if( !flag )
    {
      if( ParReadFile.isStatusNotOkay() )
      {
        ErrText.cat(ParReadFile.getErrText());
      }
      ParReadFile.getZeilenInfo(ErrText);
      ErrText.catFormat("\nCSlfParRead::readVarVal Error: Der Wert der Variable <%s> konnte nicht gelesen werden,\n Das Endezeichen <%s> konnte nicht gefunden werden"
                       ,pname,SLF_PAR_READ_STRING_END_VAL);
      Status = NOT_OKAY;
      return Status;
    }
    if( n == 0 )
    {
      if( ParReadFile.isStatusNotOkay() )
      {
        ErrText.cat(ParReadFile.getErrText());
      }
      ParReadFile.getZeilenInfo(ErrText);
      ErrText.catFormat("\nCSlfParRead::readVar Error: Der Wertangabe zu Variablenname <%s> ist leer"
                       ,pname);
      Status = NOT_OKAY;
      return Status;
    }
    // SLF_PAR_READ_STRING_END_UNIT rauslesen
    ParReadFile.throwaway((uint16_t)SlfStrLen(SLF_PAR_READ_STRING_END_VAL));

  }
  else
  {
    char  *csuch1[6] = {SLF_PAR_READ_STRING_SPACE 
                       ,SLF_PAR_READ_STRING_TAB
                       ,SLF_PAR_READ_STRING_END_LINE 
                       ,SLF_PAR_READ_STRING_COMMENT1
                       ,SLF_PAR_READ_STRING_COMMENT2
                       ,SLF_PAR_READ_STRING_COMMENT3
                       };
    // Lesen bis 
    n = ParReadFile.get_until_eol(str,csuch1,6,&flag);

    if( !flag )
    {
      if( ParReadFile.isStatusNotOkay() )
      {
        ErrText.cat(ParReadFile.getErrText());
      }
      ParReadFile.getZeilenInfo(ErrText);
      ErrText.cat("\nCSlfParRead::readGroup Error: Variable konnte nicht gelesen werden");
      Status = NOT_OKAY;
      return Status;
    }
    if( n == 0 )
    {
      if( ParReadFile.isStatusNotOkay() )
      {
        ErrText.cat(ParReadFile.getErrText());
      }
      ParReadFile.getZeilenInfo(ErrText);
      ErrText.cat("\nCSlfParRead::readVar Error: Der Variablenname ist leer");
      Status = NOT_OKAY;
      return Status;
    }

  }
  /* str analysieren */
  if( analyzeVal(pname,str,strM,&mat,&flag) != OKAY )
  {
    return Status;
  }

  if( flag )
  {
    pParReadData->setVarVal(strM);
  }
  else
  {
    pParReadData->setVarVal(mat);
  }
  FreeMatrix(mat);
  strM.clear();

  return Status;
}
status_t CSlfParRead::analyzeVal(const char *pname,slf::CStr &str,slf::CStrM &strM,Matrix_t *pMat,bool *pIsString)
{
  slf::CStr   strval;
  slf::CStrV  strv,strvv;
  uint32_t  irow,icol;
  double    dval;

  *pIsString = 0;

  /* \n, \t rausnahmen */
  str.elim(SLF_PAR_READ_STRING_END_LINE);
  str.elim(SLF_PAR_READ_STRING_TAB);

  /* Zeilen Teilen */
  SlfStrVSplit(strv,str.c_str(),SLF_PAR_READ_STRING_VAL_DELIM_ROW,SLF_PAR_READ_STRING_QUOT,SLF_PAR_READ_STRING_QUOT);

  for(irow=0;irow<strv.getNrows();irow++)
  {
    /* Spalten teilen */
    SlfStrVSplit(strvv,strv.get_str(irow),SLF_PAR_READ_STRING_VAL_DELIM_COL,SLF_PAR_READ_STRING_QUOT,SLF_PAR_READ_STRING_QUOT);

    for(icol=0;icol<strvv.getNrows();icol++)
    {
      strval = strvv.get_str(icol);
      strval.elimAnfEndC();

      // Quots suchen
      if( strval.getChar(0) == SLF_PAR_READ_QUOT )
      {
        if( strval.getLen() < 3 )
        {
          ErrText.catFormat("\nCSlfParRead::analyzeVal Error: Variablenname <%s> an Stelle irow=%i, icol=%i hat keinen korrekten Wert:<%s>"
                           ,pname,irow,icol,strval.c_str());
          Status = NOT_OKAY;
          return Status;
        }
        if( str.getChar(str.getLen()-1) != SLF_PAR_READ_QUOT )
        {
          ErrText.catFormat("\nCSlfParRead::analyzeVal Error: Variablenname <%s> an Stelle irow=%i, icol=%i hat kein Quot:<%s> am Ende Wert:<%s>"
                           ,pname,irow,icol,SLF_PAR_READ_STRING_QUOT,str.c_str());
          Status = NOT_OKAY;
          return Status;
        }
        // Quots entfernen
        strval.elimAnfEnd(SLF_PAR_READ_STRING_QUOT);
        // als string kennzeichenen
        *pIsString = 1;
      }
      // Prüfen, ob es ein Stringwert ist
      if( !(*pIsString) )
      {
        if( !SlfFktIsStringToNum(strval.c_str()) )
        {
          *pIsString = 1;
        }
      }
      // String-Matrix füllen
      strM.cpy(strval,irow,icol);
    }
  }

  // in double wandlen
  if( !(*pIsString) )
  {
    Matrix_t mat = NewMatrix(strM.getNrows(),strM.getNcols());
    *pMat = mat;
    for(irow=0;irow<strM.getNrows();irow++)
    {
      for(icol=0;icol<strM.getNcols();icol++)
      {
        SlfFktConvertStringToDouble(strM.get_str(irow,icol),&dval);
        mat[irow][icol] = dval;
      }
    }
  }

  return Status;
}
status_t CSlfParRead::readAttribute(slf::CStrV &strv,enum CSlfParRead::EReadType type)
{
  char c;
  uint16_t hierach;
  slf::CStr keystring;
  slf::CStr valuestring;
  char  *csuch[2] = {SLF_PAR_READ_STRING_SPACE 
                    ,SLF_PAR_READ_STRING_TAB};

  bool flag,commentflag = false;

  if( strv.getNrows() > 1 )     hierach = (uint16_t)strv.getNrows()-2;
  else                          hierach = 0;
  
  ParReadFile.search_throw_until_not(csuch,2);

  c = ParReadFile.peekChar();

  /* Atribute auslesen, da Atribute-Trennzeichen gefunden*/
  /*=====================================================*/
  if( c == SLF_PAR_READ_ATRIBUTE_DELIM )
  {
    // SLF_PAR_READ_ATRIBUTE_DELIM rauslesen
    ParReadFile.getChar(); 
    if( readAttributeKey(keystring,valuestring) != OKAY ) 
    {
      return Status;
    }
    /* Wenn Kommentar gefunden */
    /*-------------------------*/
    if( keystring.compare(SLF_PAR_READ_ATRIBUTE_COMMENT) )
    {
      switch( type )
      {
      case CSlfParRead::TYPE_INSTANCE:
        pParReadData->setInstance(strv.get_last_str(),valuestring.c_str());
        break;
      case CSlfParRead::TYPE_GROUP:
        pParReadData->setGroup(strv.get_last_str(),hierach,valuestring.c_str());
        break;
      case CSlfParRead::TYPE_VAR:
        pParReadData->setVar(strv.get_last_str(),"",valuestring.c_str());
        break;
      default:
        assert(0);
      }
    }
    /* Wenn Copy gefunden */
    else if( keystring.compare(SLF_PAR_READ_ATRIBUTE_COPY) )
    {
      slf::CStrV strvsrc;
      SlfStrVSplit( strvsrc, valuestring.c_str(), SLF_PAR_READ_STRING_ATRIBUTE_DELIM);
      switch( type )
      {
      case CSlfParRead::TYPE_INSTANCE:
      case CSlfParRead::TYPE_GROUP:
      case CSlfParRead::TYPE_VAR:

        setCopyStruct(strvsrc,strv,type);
        break;

      default:
        assert(0);
      }
      commentflag = true;
    }
    /* Wenn Faktor gefunden */
    else if( keystring.compare(SLF_PAR_READ_ATRIBUTE_FACTOR) && (type == CSlfParRead::TYPE_VAR) )
    {
      double dval;
      if( SlfFktConvertStringToDouble(valuestring.c_str(),&dval) == OKAY )
      {
        setValShape(strv,dval,1);// Faktor
      }
      else
      {
        
        ErrText.catFormat("CSlfParRead::readAttribute Error: Die Variable <%s> konnte mit dem Attribut <%s> den Wert <%s> nicht in double wandeln"
                         ,strv.get_last_str(),keystring.c_str(),valuestring.c_str());
      }
      commentflag = true;
    }
    /* Wenn offset gefunden */
    else if( keystring.compare(SLF_PAR_READ_ATRIBUTE_OFFSET) && (type == CSlfParRead::TYPE_VAR) )
    {
      double dval;
      if( SlfFktConvertStringToDouble(valuestring.c_str(),&dval) == OKAY )
      {
        setValShape(strv,dval,0); // Offset
      }
      else
      {
        
        ErrText.catFormat("CSlfParRead::readAttribute Error: Die Variable <%s> konnte mit dem Attribut <%s> den Wert <%s> nicht in double wandeln"
                         ,strv.get_last_str(),keystring.c_str(),valuestring.c_str());
      }
      commentflag = true;
    }
    /* Wenn unit gefunden */
    else if( keystring.compare(SLF_PAR_READ_ATRIBUTE_UNIT) && (type == CSlfParRead::TYPE_VAR) )
    {
      pParReadData->setVar(strv.get_last_str(),valuestring.c_str(),"");
      commentflag = true;
    }
    else
    {
      slf::CStr str1;
      if( type == CSlfParRead::TYPE_INSTANCE ) str1  = "Instance";
      else if(type == CSlfParRead::TYPE_GROUP ) str1 = "Group";
      else if(type == CSlfParRead::TYPE_VAR )   str1 = "Variable";
      ErrText.catFormat("Atttributzuweisung zur %s wurde nicht erkannt \n<%s=%s>"
                       ,str1.c_str(),keystring.c_str(),valuestring.c_str());
      Status = NOT_OKAY;
      return Status;
    }
  }
  /* Copy - Struktur nur Gruppe und Instanz */
  else if( c == SLF_PAR_READ_EQUAL && (type != CSlfParRead::TYPE_VAR)  )
  {
    // SLF_PAR_READ_EQUAL rauslesen
    ParReadFile.getChar(); 

    ParReadFile.get_until(valuestring,SLF_PAR_READ_END_LINE,&flag);
    if( !flag )
    {
      ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Wert nach <%s> konnte nicht ausgelesen werden"
                       ,SLF_PAR_READ_STRING_EQUAL);
      ParReadFile.getZeilenInfo(ErrText);
      Status = NOT_OKAY;
      return Status;
    }
    // Leerzeichen bereinigen
    valuestring.elimAnfEnd(SLF_PAR_READ_STRING_SPACE);

    slf::CStrV strvsrc;
    SlfStrVSplit( strvsrc, valuestring.c_str(), SLF_PAR_READ_STRING_ATRIBUTE_DELIM);

    switch( type )
    {
    case CSlfParRead::TYPE_INSTANCE:
    case CSlfParRead::TYPE_GROUP:
  
      setCopyStruct(strvsrc,strv,type);
      break;
    default:
      assert(0);
    }
    commentflag = true;
  }
  /* Kommentar */
  else if( (c == SLF_PAR_READ_COMMENT1) || (c == SLF_PAR_READ_COMMENT2) || (c == SLF_PAR_READ_COMMENT3) )
  {
    // SLF_PAR_READ_COMMENT rauslesen
    ParReadFile.getChar(); 

    ParReadFile.get_until(valuestring,SLF_PAR_READ_END_LINE,&flag);
    if( !flag )
    {
      ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Wert nach Kommentar konnte nicht ausgelesen werden"
                       );
      ParReadFile.getZeilenInfo(ErrText);
      Status = NOT_OKAY;
      return Status;
    }
    valuestring.elimAnfEnd(SLF_PAR_READ_STRING_SPACE);
    switch( type )
    {
    case CSlfParRead::TYPE_INSTANCE:
      pParReadData->setInstance(strv.get_last_str(),valuestring.c_str());
      break;
    case CSlfParRead::TYPE_GROUP:
      pParReadData->setGroup(strv.get_last_str(),hierach,valuestring.c_str());
      break;
    case CSlfParRead::TYPE_VAR:
      pParReadData->setVar(strv.get_last_str(),"",valuestring.c_str());
      break;
    default:
      assert(0);
    }
    commentflag = true;
  }
  /* neue Zeile */
  else if( (c == SLF_PAR_READ_END_LINE ) )
  {
    commentflag = true;
  }
  ParReadFile.search_throw_until(SLF_PAR_READ_STRING_END_LINE);
  ParReadFile.getChar();

  /* weiteren Kommentar suchen */
  if( commentflag )
  {
    c = ParReadFile.peekChar();
    while( (c == SLF_PAR_READ_COMMENT1) || (c == SLF_PAR_READ_COMMENT2) || (c == SLF_PAR_READ_COMMENT3) )
    {
      valuestring.clear();

      // SLF_PAR_READ_COMMENT rauslesen
      ParReadFile.getChar(); 

      ParReadFile.get_until(valuestring,SLF_PAR_READ_END_LINE,&flag);
      if( !flag )
      {
        ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Wert nach Kommentar konnte nicht ausgelesen werden"
                         );
        ParReadFile.getZeilenInfo(ErrText);
        Status = NOT_OKAY;
        return Status;
      }
      valuestring.elimAnfEnd(SLF_PAR_READ_STRING_SPACE);

      switch( type )
      {
      case CSlfParRead::TYPE_INSTANCE:
        pParReadData->setInstance(strv.get_last_str(),valuestring.c_str());
        break;
      case CSlfParRead::TYPE_GROUP:
        pParReadData->setGroup(strv.get_last_str(),hierach,valuestring.c_str());
        break;
      case CSlfParRead::TYPE_VAR:
        pParReadData->setVar(strv.get_last_str(),"",valuestring.c_str());
        break;
      default:
        assert(0);
      }
      
      
        /* bis zur neuen Zeile gehen */
      ParReadFile.search_throw_until(SLF_PAR_READ_STRING_END_LINE);
      ParReadFile.getChar();
      
      /* restes Zeichen in d er neuen Zeile auslesen */
      c = ParReadFile.peekChar();
    }
  }
  return Status;
}
status_t CSlfParRead::readAttributeKey(slf::CStr &keystring,slf::CStr &valuestring)
{
  uint16_t n;
  bool     flag;
  char  c;
  char  *csuch0[2] = {SLF_PAR_READ_STRING_SPACE  
                     ,SLF_PAR_READ_STRING_TAB};
  char  *csuch1[3] = {SLF_PAR_READ_STRING_SPACE
                     ,SLF_PAR_READ_STRING_EQUAL
                     ,SLF_PAR_READ_STRING_TAB};

  // eventuell Leerzeichen rauslesen
  n = ParReadFile.search_until_not(csuch0,2);
  ParReadFile.throwaway(n);
  // Attributende finden    
  n = ParReadFile.get_until_eol(keystring,csuch1,3,&flag); 

  if( !flag )
  {
    ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Attribute <%s> konnte nicht richtig gelesen werden"
                     ,keystring.c_str());
    ParReadFile.getZeilenInfo(ErrText);
    Status = NOT_OKAY;
    return Status;
  }

  // Trennzeichen suchen
  n = ParReadFile.search_until_eol(SLF_PAR_READ_STRING_EQUAL,&flag);
  if( !flag )
  {
    ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Trennzeichen <%s> nach Attribute <%s> konnte nicht richtig gelesen werden"
                     ,SLF_PAR_READ_STRING_EQUAL,keystring.c_str());
    ParReadFile.getZeilenInfo(ErrText);
    Status = NOT_OKAY;
    return Status;
  }
  ParReadFile.throwaway(n+(uint16_t)SlfStrLen(SLF_PAR_READ_STRING_EQUAL));

  // Wert auslesen
  n = ParReadFile.search_until_not_eol(SLF_PAR_READ_STRING_SPACE,&flag);
  if( !flag )
  {
    ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Wert zu Attribute <%s> konnte nicht gefunden werden"
                     ,keystring.c_str());
    ParReadFile.getZeilenInfo(ErrText);
    Status = NOT_OKAY;
    return Status;
  }
  ParReadFile.throwaway(n);

  c = ParReadFile.peekChar();
  // Wert in quot
  if( c == SLF_PAR_READ_QUOT )
  {
    ParReadFile.getChar();
    ParReadFile.get_until(valuestring,SLF_PAR_READ_QUOT,&flag);
    if( !flag )
    {
      ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Wert zu Attribute <%s> konnte nicht ausgelesen werden"
                       ,keystring.c_str());
      ParReadFile.getZeilenInfo(ErrText);
      Status = NOT_OKAY;
      return Status;
    }
    ParReadFile.throwaway(1);
    
  }
  // Wert bis Ende der ZEile auslesen
  else
  {
    ParReadFile.get_until(valuestring,SLF_PAR_READ_END_LINE,&flag);
    if( !flag )
    {
      ErrText.catFormat("\nCSlfParRead::readInstanceAtribute Error: Wert zu Attribute <%s> konnte nicht ausgelesen werden"
                       ,keystring.c_str());
      ParReadFile.getZeilenInfo(ErrText);
      Status = NOT_OKAY;
      return Status;
    }
    // Leerzeichen bereinigen
    valuestring.elimAnfEnd(SLF_PAR_READ_STRING_SPACE);
  }
  return Status;
}
status_t CSlfParRead::setCopyStruct(slf::CStrV &strvsrc,slf::CStrV &strvdes,enum EReadType type)
{
  // neue Kopier Struktur erstellen
  //-------------------------------
  SSlfParReadCopyStruct *p = new SSlfParReadCopyStruct;

  // Name, Kommentar
  p->strvsrc    = strvsrc;
  p->strvdes    = strvdes;
  p->type       = type;
  p->pNext      = ParReadCopyStruct;

  ParReadCopyStruct = p;

  return Status;
}
status_t CSlfParRead::setValShape(slf::CStrV &strvsrc,double val,uint8_t isFactor)
{
  SSlfParReadShapeStruct *p = ParReadShapeStruct;
  while(p)
  {
    if( p->strvsrc.compare(strvsrc) )
    {
      if( isFactor )
      {
        p->factor = val;
      }
      else
      {
        p->offset = val;
      }
      return Status;
    }
    p = p->pNext;
  }

  // neue Kopier Struktur erstellen
  //-------------------------------
  p = new SSlfParReadShapeStruct;

  // Name, Kommentar
  p->strvsrc    = strvsrc;
  if( isFactor )
  {
    p->factor = val;
  }
  else
  {
    p->offset = val;
  }
  p->pNext      = ParReadShapeStruct;

  ParReadShapeStruct = p;

  return Status;
}