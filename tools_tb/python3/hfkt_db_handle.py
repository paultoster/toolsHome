# -*- coding: cp1252 -*-
# hfkt_db_handle
#
# Handle db-Daten einer Datenbank
#
# dbfile                 Dateiname des db-Files
# dbdefile               Definition-Datei mit Tabellen-Beschreibung
# dbdefdict              Definitions-Dictionary anstat Datei
#
# Aufbau Definition-Datei:
#=========================
#
# Schlüsselwörter Tabelle DB_TAB_TYPE_BUILD       Tabelle soll gebildet werden
#                         DB_TAB_TYPE_SCHABLONE   Tabellenschablone, wird erst bei Bedarf gebildet
#
# Schlüsselwörter Zellen  DB_DATA_TYPE_DATUM       Datum-Typ
#                         DB_DATA_TYPE_STR         String-Typ
#                         DB_DATA_TYPE_FLOAT       Float-Typ
#                         DB_DATA_TYPE_INT         Integer-Typ
#                         DB_DATA_TYPE_KEY         Key-Typ       Key einer anderen Tabelle
# - section ist Tabellenname
# - Variablen sind type,comment,zelle1,zelle2,zelle3,...
# - Bei DB_DATA_TYPE_KEY wird zellname@Tabellenname geschrieben, d.h key aud der Tabelle
# - Für jede Tabelle wird automatisch immer eine Zelle mit Namen "key" (Interger-Typ) zur eindeutigen Zuordnung erstellt und bei neu Anlegen eindeutig zugeordnet
# - bei Verwendung dieses Keys in einer anderen Tabelle kann dann aber ein beliebiger Name verwendet mit der Zuordnung zur Tabelle
# - Bei Verwendung tabellen-Typ DB_TAB_TYPE_SCHABLONE wird zun�chst keine Tabelle angelegt, erst bei Bedarf mit eigenem Namen
# - Bei Verwendung Datei wird Type als String verwendet (z.B. DB_TAB_TYPE_BUILD)
# - bei dict wird der Zahlenwert eingesetzt (z.B. hdb.DB_TAB_TYPE_BUILD).
# - für den primkey wird der NAme self.PRIMARY_KEY_NAME für die Zelle verwendet
#
# Beispiel Datei:
#
# [Material]
# type                  = DB_TAB_TYPE_BUILD
# comment               = Tabelle mit den Materialsorten
# zelle1                = Materialart,          DB_DATA_TYPE_STR,          [],        Bezeichnung des Materials
# zelle2                = Mengeneinheit,        DB_DATA_TYPE_STR,          [],        In welchen Mengeneinheiten wird gekauft (z.B. kg)
# zelle3                = Abrechnungsmenge,     DB_DATA_TYPE_STR,          [],        In welchen Mengeneinheiten wird abgerechnet (z.B. Tasse)
# zelle4                = Kontoname,            DB_DATA_TYPE_STR,          [],        Name der Tabelle f�r Konto Material
#
# [Einkauf]
# type                  = DB_TAB_TYPE_BUILD
# comment               = Tabelle f�r den Materialeinkauf
# zelle1                = Datum,                DB_DATA_TYPE_DATUM,        [],        Datum
# zelle2                = Key1@Material,        DB_DATA_TYPE_KEY,          [],        Key von Tabelle Material
# zelle3                = Menge,                DB_DATA_TYPE_FLOAT,        [Tasse],   Menge
# zelle4                = Wert,                 DB_DATA_TYPE_INT,          [Cent],    Wert in Cent
# zelle5                = Herkunft,             DB_DATA_TYPE_STR,          [],        Herkunft des Materials
#
#
# Aufbau Definition-Liste:
#=========================
#
# Beispiel Datei:
#
#  TAB_MATERIAL        = 'Material'
#  CELL_MATERIALNAME   = 'Materialname'
#  CELL_MATERIALUNIT   = 'Materialeinheit'
#  CELL_ABRECHUNGSUNIT = 'Abrechnungseinheit'
#  TAB_MATERIALEINKAUF = 'Einkauf'
#  CELL_DATUM          = 'Datum'
#  CELL_KEY1           = 'Key1'
#  CELL_MENGE          = 'Menge'
#  CELL_WERT           = 'Wert'
#
#   DBDEFLISTE = {TAB_MATERIAL:     {'type':hdb.DB_TAB_TYPE_BUILD,'comment':'Tabelle mit den Materialsorten'            \
#                                   ,'cells':{CELL_MATERIALNAME:     {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Welches MAterial'} \
#                                            ,CELL_MATERIALUNIT:     {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'In welchen Mengeneinheiten wird gekauft (z.B. kg)'} \
#                                            ,CELL_ABRECHUNGSUNIT:   {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'In welchen Mengeneinheiten wird abgerechnet (z.B. Tasse)'}        \
#                                            }
#                                   }
#                ,TAB_MATERIALEINKAUF: {'type':hdb.DB_TAB_TYPE_BUILD,'comment':'Tabelle f�r den Materialeinkauf'            \
#                                      ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
#                                               ,CELL_KEY1+'@'+TAB_MATERIAL: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'In welchen Mengeneinheiten wird abgerechnet (z.B. Tasse)'}        \
#                                               ,CELL_MENGE:                 {'type':hdb.DB_DATA_TYPE_FLOAT,'unit':'[Einheit]','comment':'Menge'} \
#                                               ,CELL_WERT:                  {'type':hdb.DB_DATA_TYPE_INT,'unit':'[Cent]','comment':'Wert in Cent'} \
#                                               ,CELL_HERKUNFT:              {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Herkunft des Materials'} \
#                                               }
#                                      }
#                }
#
# Aufbau Definition-Struktur:
#============================
#
# self.DbDefTab[i]                  i = 0, ... , self.nDbDefTab - 1
# self.DbDefTab[i].name             Name
#                 .type             Schl�sselw�rter Tabelle siehe oben
#                 .comment
#                 .isdb             ist in database enthalten
#                 .cells[j]         j = 0, ..., self.DbDefTab[i].ncells - 1
#                 .cells[j].name       Name Zelle
#                          .datatype   datatype  siehe oben
#                          .unit       Enheit, wenn vorhanden
#                          .comment    Kommentar
#                          .tablelink  Wenn ein key einer anderen TAbelle eingetragen wird, steht hier der Tabellenname
#
# Funktionen:
#============
# dbh = self.dbhandle(DBDEFLISTE,dbdatafile) Klasse anlegen dabdatafile �ffnen und Tabellen anlegen
#
# In init() wird dbh.DefTab angelegt
#   entweder a) mit Übergabe dictionary:        dbh.read_dbdef_dict(dbdef)
#   oder     b) mit Übergabe Beschreibungsdatei dbh.read_dbdef_file(dbdef)
#
#   mit dbh.check_db_def() wird definition mit db-data-file abgeglichen
#       |
#       +-> dbh.check_and_build_table(deftab) �berpr�ft Tabellen/Zellen definition
#           |
#           +-> dbh.build_db_table(deftab) Legt neue Tabelle an
#
# status dbh.check_tab_in_tabdef_und_db(self,tabname): Prüft, ob Tabelle in def und
#                                                      database vorhanden, wenn nicht
#                                                      erstellen
#
# tabname dbh.build_table_schablone(schablone_tab_name,additional_tab_name): erstellt mit der Schblonen Tabellendefinition eine neu Tabelle mit
#                                                                            Namen: schablone_tab_name+additional_tab_name
#                                                                            Rückgabe tabname wenn okay oder leer
#
# flag dbh.is_tabname_in_data(tabname)
#
# tabname_liste = dbh.get_tabname_liste_from_deftab()
#    Sucht in self.DbDefTab alle tabellen und gibt Liste mit Namen zur�ck
#
# tabname_liste = dbh.get_tabname_liste_from_data()
#    Sucht in self.DbDefTab alle tabellen, die auch existieren und gibt Liste mit Namen zur�ck
#
# deftab    = dbh.get_tab_from_deftab(tabname)
# name_list = dbh.get_cell_names_from_deftab(tabname)
# flag      = dbh.is_cell_name_tabdef(tabname,cellname)
# unit      = dbh.get_unit_cell_name_in_deftab(tabname,cellname)
# datatype  = get_type_cell_name_in_deftab(tabname,cellname)
#
# dbh.get_tab_primary_key_name(self,tabname)
#
#    Abfrage des Keynamens der Tabelle tabname
#    key_name = hdb.get_tab_primary_key_name(self,tabname)
#
# dbh.get_tab_primary_key_with_value(self,tabname,cellname,value)
#
#    Rückgabe des primkeys als liste f�r den Wert in dem Zellen von cellname
#    primkey_liste = hdb.get_tab_primary_key_with_value(self,tabname,cellname,value)
#
# dbh.get_tab_data(tabname,cellnames=None)
#
#     Inhalt der Tabelle abfragen, iype1 = 0, itype2=None:   gesamte Tabelle
#     (header_liste,data_liste) = dbh.get_tab_data(tabelename) <= nix
#        header_liste : m x 1  m rows/Zellen
#        data_liste   : n x m  n Datens�tze
#     (header_liste,data_liste) = dbh.get_tab_data(tabelename,cellname) <= string
#        header_liste : 1
#        data_liste   : n x 1
#    (header_liste,data_liste) = dbh.get_tab_data(tabelename,cellnames_liste) <= liste
#        header_liste : m x 1
#        data_liste   : n x m
#
# dbh.get_tab_data_with_primkey(tabname,primkey)
#
#     Gesamter Inhalt mit dem entspr. Primekey
#     (header_liste,data_liste) = dbh.get_tab_data_with_primkey(tabelename,primkey)
#
# # dbh.get_tab_data_with_value(tabname,cellname,value)
#
#    Von Tabelle tabname mit dem Zellenname = value die Daten zur�ckgeben
#     (header_liste,data_liste) = dbh.get_tab_data_with_value(tabname,cellname,value)
#
# dbh.get_tab_data_with_value(tabname,cellname,value,header_liste)
#
#    Von Tabelle tabname mit dem Zellenname = value die Daten zur�ckgeben, aber
#    nur die von header_liste vorgegeben
#     (header_liste,data_liste) = dbh.get_tab_data_with_value(tabname,cellname,value,header_liste)
#
# dbh.sort_data_liste_with_cellname(header_liste,data_liste,cellname):
#
#    sortiert data_liste aufsteigend nach der cellname
#    return (header_liste,data_liste)
#
# datas          = dbh.get_colums_from_data_liste(header_liste,data_liste,cellname)
# datacols_liste = dbh.get_colums_from_data_liste(header_liste,data_liste,cellname)
#
#
#    datas = dbh.get_colums_from_data_liste(header_liste,data_liste,cellname_liste) <= liste
#    header_lsite: k >= m Tupel mit cellnames
#    data_liste: n rows mit jeweils einem Datensatz entsprechend header_liste
#    cellname_liste : m Tupel
#    datas : [data1,data2, ...]: m Listen mit  data1,data2,... : n-Tupeln
#
# datas          = dbh.get_rows_from_data_liste(header_liste,data_liste,cellname)
# datarows_liste = dbh.get_rows_from_data_liste(header_liste,data_liste,cellname_liste)
#
#    datas = dbh.get_rows_from_data_liste(header_liste,data_liste,cellname) <= string
#    datas = dbh.get_rows_from_data_liste(header_liste,data_liste,cellname_liste) <= liste
#    header_lsite: k >= m Tupel mit cellnames
#    data_liste: n rows mit jeweils einem Datensatz entsprechend header_liste
#    cellname_liste : m Tupel
#
#    datas : [[valueA0,valueB0, ...],[valueA1,valueB1, ...], ...]: n Listen mit je m-Tupeln => liste mit cellname_liste
#    datas : [valueX0,valueX1,...]                                 1 Liste         n-Tupeln => string mit cellname
#
#  flag = dbh.is_item_in_cell_tab_data(tabname(string),cellname(string),item)
#
#  flag = dbh.add_new_data_set(tabname(string),d(dict)) (d[cellname1]=item1,d[cellname2]=item2,...)
#
#  flag = dbh.is_datum_type(tabname(string),cellname(string))
#
#  nrows = dbh.get_num_of_items(tabname)
#
#  def dbh.modify_data_by_primkey(tabname,primkey,d)
#
#    Aus d (dictionary) werden die items ausgelesen und gesetzt und eingeben
#    und mit dem primkey ge�ndert
#    d[cellnameA]=WertA
#    d[cellnameB]=WertB
#    ...
#    Alle Zellen einer Tabelle m�ssen vorhanden sein
#    return status
#  def dbh.delete_data_by_primkey(tabname,primkey)
#
#    delete_data_by_primkey(self,tabname,primkey)
#    delete_data_by_primkey(self,tabname,primkey_liste):
#    Es werden aus der Tabelle mit tabname die Zellen mit primkey bzw liste von primkeys
#    gel�scht
#    return status
#
# flag = dbh.data_base_is_modified()
#      True: database has changed in this run



import os, types, sqlite3
import hfkt     as h
import hfkt_def as hdef
import hfkt_ini as hini
import hfkt_db  as hdb



DB_TAB_TYPE_BUILD     = 0
DB_TAB_TYPE_SCHABLONE = 1
DB_TAB_TYPE_STRING_LISTE = ["DB_TAB_TYPE_BUILD","DB_TAB_TYPE_SCHABLONE"]
I0_GETKEY = 0
def getKey(s):
  return s[I0_GETKEY]

class dbhandle:
  PRIMARY_KEY_NAME = u"key"
  DELIM_TABLINK    = "@"
  OKAY     = hdef.OK
  NOT_OKAY = hdef.NOT_OK
  errText  = ""
  logText  = ""
  status   = hdef.OK
  # DB-Def-Table:
  class db_def_tab:
    def __init__(self,name,type,comment):
      self.name    = name        # Name (bei type=DB_TAB_TYPE_SCHABLONE wird spezifischer Name mit '_' angh�ngt)
      self.type    = type        # siehe hfkt_db
      self.comment = comment     # Kommentar
      self.cells   = []          # List mit Zellen
      self.ncells  = 0           # Anzahl der Zellen
      self.isdb    = 0           # Tabelle ist angelegt!

  class db_def_cell:
    def __init__(self,name,datatype,unit,comment,tablelink):
      self.name      = name      # Name
      self.datatype  = datatype  # Datentype siehe hfkt_db
      self.unit      = unit      #
      self.comment   = comment   # Kommentar
      self.tablelink = tablelink # Wenn ein key einer anderen TAbelle eingetragen wird, steht hier der Tabellenname
# public
#===============================================================================
#===============================================================================
  def __init__(self,dbdef,dbfile):
    """
    Check Db - Define - LIste with Dbfile
    and build or modify table of Dbfile
    """

    # Number of Database Table
    self.DbDefTab  = []
    self.nDbDefTab = 0

    if( isinstance(dbdef,dict) ):
      self.read_dbdef_dict(dbdef)
    # dbdef ist eine Datei
    else:
      self.read_dbdef_file(dbdef)
    #endif

    if( self.status != self.OKAY ): return

    self.dbfile     = dbfile

    # datafile �ffnen
    self.db = hdb.db(self.dbfile)
    if( self.db.status  != self.db.OKAY ):
      self.status  = self.NOT_OKAY
      self.errText = self.db.errText
      return
    elif( self.db.has_log_text() ):
      self.add_log_text(self.db.get_log_text())
    #endif

    # dbdef in datafile �berpr�fen und auff�llen
    self.check_db_def()
    if( self.db.status  != self.db.OKAY ):
      self.status  = self.NOT_OKAY
      self.errText = self.db.errText
      return
    elif( self.db.has_log_text() ):
      self.add_log_text(self.db.get_log_text())

#===============================================================================
#===============================================================================
  def close(self):
    """
    delete db-connection
    """
    self.db.close_dbfile()
    if( self.db.has_log_text() ):
      self.add_log_text(self.db.logText)
#===============================================================================
#===============================================================================
  def get_log_text(self):
    log_text = self.logText
    self.logText = ""
    return log_text
#===============================================================================
#===============================================================================
  def has_log_text(self):
    if( len(self.logText) > 0 ):
      return True
    else:
      return False
#===============================================================================
#===============================================================================
  def add_log_text(self,text):
    if( len(self.logText) > 0 ):
      self.logText += "\n"+text
    else:
      self.logText += text
#-------------------------------------------------------------------------------
  def get_tab_type_number(tabtypename):
    """
    vergleicht tabtypename mit den Definitionen und gibt Integer-Wert zur�ck
    Wenn nicht vorhanden wird None zur�ck gegeben
    """
    index = 0
    for type in DB_TAB_TYPE_STRING_LISTE:
      if( tabtypename == type ):
        return index
      else:
        index = index + 1

    # wenn nicht vorhanden
    return None

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#-------------------------------------------------------------------------------
  def read_dbdef_file(self,dbdeffile):
    """
    dbdeffile wird intepretiert um die Datenstruktur festzulegen
    """
    if( not os.path.exists(dbdeffile) ):
       self.errText = "Data-Definition-File <%s> does not exist" % dbdeffile
       self.status  = self.NOT_OKAY
       return

    # read db-definition-File
    #------------------------
    (self.status,errtext,out) = hini.readini(dbdeffile,None)

    if( self.status != hdef.OK ):
      self.errText = errtext
      return self.status
    #endif
    #------------------------

    # Tabellen Definition DbDefTab
    self.DbDefTab  = []
    self.nDbDefTab = 0;
    # Schleife mit Tabelendefinition
    #===============================
    for key in out:

      # Tabellename
      name = key

      # Tabellentype
      #-------------
      if( "type" in out[key] ):
        type = out[key]["type"]
      else:
        type = "DB_TAB_TYPE_BUILD"
      #endif

      type_index = self.get_tab_type_number(type)

      if( type_index == None ):
        self.status = hdef.NOT_OK
        tt          = "table-type <%s> not found in hfkt_db.py !!!" % type
        self.errText = tt
        return self.status
      #endif
      #-------------

      # Tabellencomment
      #----------------
      if( "comment" in out[key] ):
        comment = out[key]["comment"]
      else:
        comment = ""
      #endif
      #----------------

      #Tabellendefinition
      #------------------
      DbDefTab = self.db_def_tab(name,type_index,comment)
      #------------------

      # Zelldefinitionen
      #+++++++++++++++++
      keyliste   = out[key].keys()
      # Liste mit allen keys, die zelle enthalten
      indexliste = h.such_in_liste(keyliste,"zelle",regel='n')

      # alle Indices f�r jeweils ein Zelle abarbeiten
      #----------------------------------------------
      for index in indexliste:

        # Zelldefinition auslesen
        tt = out[key][keyliste[index]]
        liste = h.split_not_quoted(tt,',','"','"',1)

        # Name auslesen
        #..............
        if( len(liste) > 0 ):
          name = h.elim_ae(liste[0],' ')
          # pr�fen, ob Tabellen name zu z.B. key angeh�ngt ist
          if( h.such(name,self.DELIM_TABLINK,"vs") >= 0):
            lliste=name.split(self.DELIM_TABLINK)
            name    = lliste[0]
            if( len(lliste) > 1 ):
              tablink = lliste[1]
            else:
              tablink = ""
            #endif
          else:
            tablink = ""
          #endif
        else:
          self.status = hdef.NOT_OK
          tt          = "Zelle: <s>: Hat keinen Namen in Datei:<%s> !!!" % (keyliste[index],dbdeffile)
          self.errText = tt
          return self.status
        #endif
        #..............

        # Data-Type auslesen und data_type_index festlegen
        #.................................................
        if( len(liste) > 1 ):
          data_type_index = hdb.get_data_type_number(h.elim_ae(liste[1],' '))
          if( data_type_index == None ):
            self.status = hdef.NOT_OK
            tt          = "Zelle: <%s>: zell-data-type <%s> not found in hfkt_db.py !!!" % (keyliste[index],h.elim_ae(liste[1],' '))
            self.errText = tt
            return self.status
          #endif
        else:
          self.status = hdef.NOT_OK
          tt          = "Zelle: <s>: Hat keinen Data-Type in Datei:<%s> !!!" % (keyliste[index],dbdeffile)
          self.errText = tt
          return self.status
        #endif
        #.................................................

        #..........
        # Einheit
        if( len(liste) > 2 ):
          lliste = h.get_string_quoted(liste[2],"[","]")
          if( len(lliste) == 0 ): unit = ""
          else:                  unit  = lliste[0]
        else:
          unit = ""
        #endif
        #..........

        # Kommentar
        #..........
        if( len(liste) > 3 ):
          comment = h.elim_ae(liste[3],' ')
        else:
          comment = ""
        #endif
        #..........

        # Zelle definieren
        #.................
        zelle = self.db_def_cell(name,data_type_index,unit,comment,tablink)
        #.................

        # Zelle in Tablle einf�gen
        #.........................
        DbDefTab.cells.append(zelle)
        DbDefTab.ncells += 1
        #.........................
      #endfor
      #--------------

      #~~~~~~~~~~~~~~+
      # Primary Key
      zelle = self.db_def_cell(self.PRIMARY_KEY_NAME,hdb.DB_DATA_TYPE_PRIMKEY,"","primary key","")
      DbDefTab.cells.append(zelle)
      DbDefTab.ncells += 1
      #~~~~~~~~~~~~~~+

      #
      #+++++++++++++++

      # Tablle DbDefTab in self.DbDefTab einf�gen
      #.........................
      self.DbDefTab.append(DbDefTab)
      self.nDbDefTab += 1
    #endfor
    #===============================
    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def read_dbdef_dict(self,dbdefdict):
    """
    dbdefdict wird intepretiert um die Datenstruktur festzulegen
    """

    # alle Tabellen
    #------------------------
    tabliste = dbdefdict.keys()
    #------------------------

    # Tabellen Definition DbDefTab
    self.DbDefTab  = []
    self.nDbDefTab = 0;
    # Schleife mit Tabelendefinition
    #===============================
    for tabname in tabliste:

      tabdict = dbdefdict[tabname]

      # Tabellename
      name = tabname

      # Tabellentype
      #-------------
      if( 'type' in tabdict ):
        type_index = tabdict['type']
      else:
        type_index = self.get_tab_type_number("DB_TAB_TYPE_BUILD")
      #endif

      if( type_index == None ):
        self.status = hdef.NOT_OK
        tt          = "table-type <%s> not found in hfkt_db.py !!!" % type
        self.errText = tt
        return self.status
      #endif
      #-------------

      # Tabellencomment
      #----------------
      if( 'comment' in tabdict ):
        comment = tabdict['comment']
      else:
        comment = ""
      #endif
      #----------------

      #Tabellendefinition
      #------------------
      DbDefTab = self.db_def_tab(name,type_index,comment)
      #------------------

      # Zelldefinitionen
      #+++++++++++++++++
      if( 'cells' in tabdict ):
        cellsdict   = tabdict['cells']
      else:
        self.status = hdef.NOT_OK
        tt          = "tabellen definition <%s> has no cells !!!" % name
        self.errText = tt
        return self.status

      # Liste mit allen keys, die zelle enthalten

      cellnames = cellsdict.keys()

      # alle Indices f�r jeweils ein Zelle abarbeiten
      #----------------------------------------------
      for cellname in cellnames:

        cellname = cellname
        celldict = cellsdict[cellname]

        # Name auslesen
        #..............
        if( len(cellname) > 0 ):
          # pr�fen, ob Tabellen name zu z.B. key angeh�ngt ist
          if( h.such(cellname,self.DELIM_TABLINK,"vs") >= 0):
            lliste   = cellname.split(self.DELIM_TABLINK)
            cellname = lliste[0]
            if( len(lliste) > 1 ):
              tablink = lliste[1]
            else:
              tablink = ""
            #endif
          else:
            tablink = ""
          #endif
        else:
          self.status = hdef.NOT_OK
          tt          = "Zelle: <s>: Hat keinen Namen in Datei:<%s> !!!" % (cellname,tabname)
          self.errText = tt
          return self.status
        #endif
        #..............

        # Data-Type auslesen und data_type_index festlegen
        #.................................................
        if( 'type' in celldict ):
          data_type_index = celldict['type']
          if( data_type_index == None ):
            self.status = hdef.NOT_OK
            tt          = "Zelle: <%s>: zell-data-type <%s> not found in hfkt_db.py !!!" % (cellname,data_type_index)
            self.errText = tt
            return self.status
          #endif
        else:
          self.status = hdef.NOT_OK
          tt          = "Zelle: <s>: Hat keinen Data-Type in dict !!!" % (cellname)
          self.errText = tt
          return self.status
        #endif
        #.................................................

        #..........
        # Einheit
        if( 'unit' in celldict ):
          lliste = h.get_string_quoted(celldict['unit'],"[","]")
          if( len(lliste) == 0 ): unit = ""
          else:                  unit  = lliste[0]
        else:
          unit = ""
        #endif
        #..........

        # Kommentar
        #..........
        if( 'comment' in celldict ):
          comment = celldict['comment']
        else:
          comment = ""
        #endif
        #..........

        # Zelle definieren
        #.................
        zelle = self.db_def_cell(cellname,data_type_index,unit,comment,tablink)
        #.................

        # Zelle in Tablle einf�gen
        #.........................
        DbDefTab.cells.append(zelle)
        DbDefTab.ncells += 1
        #.........................
      #endfor
      #--------------

      #~~~~~~~~~~~~~~+
      # Primary Key
      zelle = self.db_def_cell(self.PRIMARY_KEY_NAME,hdb.DB_DATA_TYPE_PRIMKEY,"","primary key","")
      DbDefTab.cells.append(zelle)
      DbDefTab.ncells += 1
      #~~~~~~~~~~~~~~+

      #
      #+++++++++++++++

      # Tablle DbDefTab in self.DbDefTab einf�gen
      #.........................
      self.DbDefTab.append(DbDefTab)
      self.nDbDefTab += 1
    #endfor
    #===============================
    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def check_db_def(self):
    """
    �berpr�ft, ob die in self.DbDefTab definierten Tabellen in datafile self.db
    erstellt sind, bzw. wenn nicht, dann erstellen
    """
    #----------------------------------------
    # Schleife �ber definierte Tabellen
    for i in range(len(self.DbDefTab)):
      deftab = self.DbDefTab[i]
      # Tabelle muss eine build tabelle sein
      if( deftab.type == DB_TAB_TYPE_BUILD ):
        # check, ob die Tabelle erstellt ist
        if( self.check_and_build_table(deftab) != self.db.OKAY ):
          self.errText += self.db.errText
          self.status  = self.NOT_OKAY
          return self.status
        else:
          self.DbDefTab[i].isdb = 1
        #endif
      #endif
    #endfor

    return self.status

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def check_and_build_table(self,deftab):
    """
    Checkt die Tabellendefinition, ob erstellt, wenn nein, dann erstellen
    return status
    """
    # Wenn deftabelle existiert
    if( self.db.exist_table(deftab.name) ):
      # suche alle cells der tabelle mit [[cellname, index, type],[cellname, index, type], ... ]
      db_cell_liste = self.db.get_db_table_info(deftab.name)

      # Schauen �ber definierten alle Zellen
      for defcell in deftab.cells:
        flag = False
        # In db_cells suchen
        for db_cell in db_cell_liste:
          db_cell_name = db_cell[0]
          db_cell_type = db_cell[2]
          # gefunden
          if( defcell.name == db_cell_name ):
            flag = True
            break
          #endif
        #endfor
        # wenn Zelle in db nicht existiert
        if( not flag ):
          # bilden einer neuen Zelle
          self.db.build_new_cell_in_table(deftab.name,defcell.name,defcell.datatype)
          if( self.db.status != self.db.OKAY ):
            self.errText += self.db.errText;
            self.status  = self.NOT_OKAY
            return self.status
          #endif
        #endif
      #endfor

      # Pr�fen ob eine Zelle aus definiton weggefallen ist
      # Schleife �ber alle Zellen der Table
      # suche alle cells der tabelle mit [[cellname, index, type],[cellname, index, type], ... ]
      db_cell_liste = self.db.get_db_table_info(deftab.name)
      # �ber alle db_cells
      for db_cell in db_cell_liste:
        db_cell_name = db_cell[0]
        db_cell_type = db_cell[2]
        flag = False
        # Suchen, ob definition vorhanden
        for defcell in deftab.cells:
          if( defcell.name == db_cell_name ):
            flag = True
            break
          #endif
        #endfor
        # Zelle in Tabelle l�schen wenn nicht vorhanden
        if( not flag ):
          self.db.del_cell_in_table(deftab.name,db_cell_name)
      #endfor
    # table does not exist
    else:
      # build table
      self.build_db_table(deftab)
      if( self.status != self.OKAY ):
        return self.status
    #endif



    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def build_db_table(self,deftab):
    """
    Bildet Tabelle mit  cells aus deftab Definition
    """
    cell_liste = []
    for cell in deftab.cells:
      liste = [cell.name,cell.datatype]
      cell_liste.append(liste)
    #endfor
    if( self.db.build_db_table(deftab.name,cell_liste) != self.db.OKAY ):
      self.errText += "Tabelle <%s> konnte in Datei <%s> nicht erstellt  werden:\n(%s)" % (tabname,self.dbfile,self.db.errText)
      self.status  = self.NOT_OKAY
    #endif

    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def check_tab_in_tabdef_und_db(self,tabname):
    """
    Überprüft Tabelle, ob in definition und erstellt
    return status
    """

    # Wenn Tabellenname für DB_TAB_TYPE_SCHABLONE gebildet, dann prüfen, ob Name enthaltne ist
    if( self.is_tabname_in_schablone_from_deftab(tabname) ):
      if( not self.is_tabname_in_data(tabname) ):
        deftab = self.get_deftab_from_schablone(tabname)
        if( self.db.build_db_table(tabname,deftab.cells) != self.OKAY ):
          self.status  = self.NOT_OKAY
          return self.status
      #endif

    else:
      deftab = self.get_tab_from_deftab(tabname)
      if( not deftab ):
        self.status  = self.NOT_OKAY
        self.errText = "Tabelle <%s> konnte DefTabStruktur in DBDefTab nicht gefunden werden <%s> nicht gelesen werden !!!" % (tabname)
        return self.status
      #endif

      #existiert die Tabelle in der Datei nicht, dann anlegen
      if( not deftab.isdb ):
        if( self.db.build_db_table(deftab.name,deftab.cells) != self.OKAY ):
          self.status  = self.NOT_OKAY
        else:
          self.set_isdb_in_deftab(deftab.name,1)
        #endif
      #endif
    #endif
    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def build_table_schablone(self,schablone_tab_name,additional_tab_name): 
    """
    erstellt mit der Schblonen Tabellendefinition eine neu Tabelle mit
    Namen: schablone_tab_name+additional_tab_name
    Rückgabe tabname wenn okay oder leer
    """
    deftab = self.get_tab_from_deftab(schablone_tab_name)

    if( not deftab ):
      self.status  = self.NOT_OKAY
      self.errText = "Schablonen Tabelle <%s> konnte in DefTabStruktur DBDefTab nicht gefunden werden <%s> nicht gelesen werden !!!" % (schablone_tab_name)
      return ""
    #endif

    #Tabellenname
    tabname = schablone_tab_name + "_" + additional_tab_name

    if( not self.is_tabname_in_data(tabname) ):
      if( self.db.build_db_table(tabname,deftab.cells) != self.OKAY ):
        self.status  = self.NOT_OKAY
        tabname = ""
      #endif
    #endif
      

    return tabname
      
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_tabname_in_data(self,tabname):

    tabname_liste = self.get_tabname_liste_from_data()

    if( tabname in tabname_liste ):
      return True
    #endif
      
    return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_primary_key(self,tabname,cellname,cellvalue):
    """
    Abfrage primary key value von Tabelle tabname mit cellname = cellvalue
    """
    primkey = 0
    (header_liste,data_liste) = self.get_tab_data_with_value(tabname,cellname,cellvalue)
    if( len(data_liste) > 0 ):
      # erster Datensatz
      dataset = data_liste[0]
      for i in range(len(header_liste)):
        if( header_liste[i] == self.PRIMARY_KEY_NAME ):
          primkey = dataset[i]
        #endif
      #endofr
    #endif
    return primkey
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_primary_key_name(self,tabname):
    """
    Abfrage des Keynamens der Tabelle tabname
    key_name = hdb.get_tab_primary_key_name(self,tabname)
    """

    # Sucht Tabelle in Definition
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return ""
    #endif

    if( self.is_cell_name_tabdef(tabname,self.PRIMARY_KEY_NAME) ):
      return self.PRIMARY_KEY_NAME
    else:
      self.status = self.NOT_OKAY
      tt          = "Tabelle <%s>  enth�lt den primary key <%s>  nicht: " % (tabname,self.PRIMARY_KEY_NAME)
      self.errText = tt
    #endif

    return ""
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_primary_key_with_value(self,tabname,cellname,value):
    """
     R�ckgabe des primkeys als liste f�r den Wert in dem Zellen von cellname
     primkey_liste = hdb.get_tab_primary_key_with_value(self,tabname)
    """
    primkey_liste = []
    (header_liste,data_liste) = self.get_tab_data(tabname,[cellname,self.PRIMARY_KEY_NAME])
    if( self.status != self.OKAY ): return primkey_liste

    for row in data_liste:
      if( row[0] == value ):
        primkey_liste.append(row[1])
    #endfor
    return primkey_liste
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_data(self,tabname,cellnames=None):
    """
     Inhalt der Tabelle abfragen:
     (header_liste,data_liste) = dbh.get_tab_data(tabelename) <= gesamte Tabelle
        header_liste : m        rows/Zellen
        data_liste   : n x [m]  n Datens�tze
     (header_liste,data_liste) = dbh.get_tab_data(tabelename,cellnames_liste) <= Auswahlliste
        header_liste : m        rows/Zellen
        data_liste   : n x [m]  n Datens�tze
     (header_liste,data_liste) = dbh.get_tab_data(tabelename,cellname) <= string
        header_liste : 1
        data_liste   : n x [1]
    """
    header_liste = []
    data_liste   = []

    # Sucht Tabelle in Definition
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return (header_liste,data_liste)
    #endif

    # A) gesamte Tabelle
    if( not cellnames ):
      #Header-Liste
      header_liste = self.get_cell_names_from_deftab(tabname)
    # B) Auswahlliste
    elif( h.is_list(cellnames) ):
      for cellname in cellnames:
        if( self.is_cell_name_tabdef(tabname,cellname) ):
          header_liste.append(cellname)
        #endif
      #endfor
      if( len(header_liste) == 0 ):
        self.status = self.NOT_OKAY
        tt          = "Tabelle <%s> enth�lt die genannten Zellennamen nicht:  " % tabname
        for cellname in cellnames:
          tt += "%s, " % cellname

        self.errText = tt
        return (header_liste,data_liste)
    # C) Ein Name
    else:
      if( self.is_cell_name_tabdef(tabname,cellnames) ):
        header_liste.append(cellnames)
      if( len(header_liste) == 0 ):
        self.status = self.NOT_OKAY
        tt          = "Tabelle <%s>  enth�lt die genannten Zellennamen nicht: " % tabname
        tt += " %s" % cellnames
        #endfor
        self.errText = tt
        return (header_liste,data_liste)
      #endif
    #endif

    # Daten holen
    data_liste = self.db.get_data_from_tab(tabname,header_liste)

    # DAta-Liste
    if( self.db.status != self.db.OKAY):
      self.status = self.NOT_OKAY
      tt          = "Tabelle <%s> konnte in Datei <%s> nicht gelesen werden !!!" % (tabname,self.dbfile)
      self.errText = tt
      return (header_liste,data_liste)
    #endif

    return (header_liste,data_liste)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_data_with_primkey(self,tabname,primkey):
    """
    Gesamter Inhalt mit dem entspr. Primekey
    (header_liste,data_liste) = dbh.get_tab_data_with_primkey(tabelename,primkey)
    """
    data_liste   = []

    # Sucht Tabelle in Definition
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return ([],[])
    #endif

    header_liste = self.get_cell_names_from_deftab(tabname)
    data_liste   = self.db.get_data_from_tab(tabname,header_liste,self.PRIMARY_KEY_NAME,primkey)

    return (header_liste,data_liste)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_data_with_value(self,tabname,cellname,value,header_liste=[]):
    """
    Von Tabelle tabname mit dem Zellenname = value die Daten zur�ckgeben
     (header_liste,data_liste) = dbh.get_tab_data_with_value(tabname,cellname,value)
     (header_liste,data_liste) = dbh.get_tab_data_with_value(tabname,cellname,value,header_liste)
    """
    data_liste   = []

    # Sucht Tabelle in Definition
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return ([],[])
    #endif

    if( not self.is_cell_name_tabdef(tabname,cellname) ):
      self.status = self.NOT_OKAY
      tt          = "Tabelle <%s>  enth�lt den Zellennamen <%s> nicht: " % (tabname,cellname)
      self.errText = tt
      return ([],[])
    #endif

    if( len(header_liste) == 0 ):
      header_liste = self.get_cell_names_from_deftab(tabname)
    else:
      for item in header_liste:
        if( not self.is_cell_name_tabdef(tabname,item) ):
          self.status = self.NOT_OKAY
          tt          = "Tabelle <%s>  enth�lt den Zellennamen <%s> aus header_liste nicht: " % (tabname,item)
          self.errText = tt
          return ([],[])
        #endif
      #endfor
    #endif

    # Daten holen
    data_liste = self.db.get_data_from_tab(tabname,header_liste,cellname,value)

    return (header_liste,data_liste)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def sort_data_liste_with_cellname(self,header_liste,data_liste,cellname):
    """
    sortiert data_liste aufsteigend nach der cellname
    """
    i0 = -1
    for i in range(len(header_liste)):
      if( cellname == header_liste[i] ):
        i0 = i
        t  = type(data_liste[0][i])
        break
      #endif
    #endfor
    if(   (i0 > -1)                                \
      and (  isinstance(t, float)                  \
          or isinstance(t, int)                    \
          or (isinstance(t, str))                  \
          )                                        \
      ):
      I0_GETKEY = i0
      data_liste = sorted(data_liste, key=getKey)
    #endif

    return (header_liste,data_liste)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_rows_from_data_liste(self,header_liste,data_liste,cellname_liste):
    """
    datas = dbh.get_rows_from_data_liste(header_liste,data_liste,cellname) <= string
    datas = dbh.get_rows_from_data_liste(header_liste,data_liste,cellname_liste) <= liste
    header_lsite: k >= m Tupel mit cellnames
    data_liste: n rows mit jeweils einem Datensatz entsprechend header_liste
    cellname_liste : m Tupel

    datas : [[valueA0,valueB0, ...],[valueA1,valueB1, ...], ...]: n Listen mit je m-Tupeln => liste mit cellname_liste
    datas : [valueX0,valueX1,...]                                 1 Liste         n-Tupeln => string mit cellname
    """
    datas = self.get_colums_from_data_liste(header_liste,data_liste,cellname_liste)

    if(  isinstance(cellname_liste, str) ):
      return datas
    else:
      m = len(datas)
      n = len(datas[0])
      datas_liste = []
      for i in range(n):
        liste = []
        for j in range(m):
          liste.append(datas[j][i])
        #endfor
        datas_liste.append(liste)
      #endfor
    #endif

    return datas_liste
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_colums_from_data_liste(self,header_liste,data_liste,cellname_liste):
    """
    datas = dbh.get_colums_from_data_liste(header_liste,data_liste,cellname) <= string
    datas = dbh.get_colums_from_data_liste(header_liste,data_liste,cellname_liste) <= liste
    header_lsite: k >= m Tupel mit cellnames
    data_liste: n rows mit jeweils einem Datensatz entsprechend header_liste
    cellname_liste : m Tupel
    datas : [data1,data2, ...]: m Listen mit  data1,data2,... : n-Tupeln => liste mit cellname_liste
    datas : data1               1 Liste                         n-Tupeln => string mit cellname
    """
    flag_string = False
    if( isinstance(cellname_liste, str)  ):
      cellname_liste = [cellname_liste]
      flag_string    = True
    #endif

    datas = []
    for cellname in cellname_liste:
      datai = []
      i0    = -1
      for i in range(len(header_liste)):
        if( cellname == header_liste[i] ):
          i0 = i
          break
        #endif
      #endfor
      if( i0 > -1 ):
        for rows in data_liste:
          datai.append(rows[i0])
        #endfor
        if( flag_string ):
          datas = datai
        else:
          datas.append(datai)
        #endif
      #endif
    #endfor

    return datas

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tabname_liste_from_deftab(self):
    """
    Sucht in self.DbDefTab alle tabellen und gibt Liste mit Namen zur�ck
    """
    tabnamen_liste = []
    for tabdef in self.DbDefTab:
      tabnamen_liste.append(tabdef.name)
    #endfor
    return tabnamen_liste

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tabname_liste_from_data(self):
    """
    Sucht in self.DbDefTab alle tabellen, die existieren und gibt Liste mit Namen zur�ck
    """
    
    return self.db.get_all_tables()


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tab_from_deftab(self,tabname):
    """
    Sucht in self.DbDefTab die tabelle und gibt die Struktur zur�ck
    wenn nicht vorhanden, dann None R�ckgabe
    """
    if( self.is_tabname_in_schablone_from_deftab(tabname) ):
      return self.get_deftab_from_schablone(tabname)
    else:
      for tabdef in self.DbDefTab:
        if( tabdef.name == tabname ):
          return tabdef
        #endif
      #endfor
    #endif
    return None
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_tabliste_schablone_from_deftab(self):
    liste = []
    for tabdef in self.DbDefTab:
      if( tabdef.type == DB_TAB_TYPE_SCHABLONE):
        liste.append(tabdef.name)
      #endif
    #endfor
    return liste
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_tabname_in_schablone_from_deftab(self,tabname):
    """
    Prüft ob tabname aus Schablone
    """
    liste = self.get_tabliste_schablone_from_deftab()

    for tabschab in liste:
      i = tabname.find(tabschab)
      if( i == 0):
        return True
    
    return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_deftab_from_schablone(self,tabname):
    """
    gebe deftab für die entsprechende Schablone zurück
    """
    
    for deftab in self.DbDefTab:
      if( deftab.type == DB_TAB_TYPE_SCHABLONE):
        i = tabname.find(deftab.name)
        if( i == 0 ):
          return deftab
        #endif
      #endif
    #endfor
    return None
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_cell_names_from_deftab(self,tabname):
    cell_names = []
    flag = True
    if( self.is_tabname_in_schablone_from_deftab(tabname) ):
      deftab = self.get_deftab_from_schablone(tabname)
      if( tabname == deftab.name ):
        for cell in deftab.cells:
          cell_names.append(cell.name)
          flag = False
        #endfor
      #endif
    else:
      for deftab in self.DbDefTab:
        if( tabname == deftab.name ):
          for cell in deftab.cells:
            cell_names.append(cell.name)
            flag = False
          #endfor
        #endif
      #endfor

    if( flag ):
      self.status = hdef.NOT_OK
      tt          = "Tabelle <%s> konnte in der Definition nicht gefunden werden !!!" % (tabname)
      self.errText = tt
    #endif
    return cell_names
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_cell_name_tabdef(self,tabname,cellname):

    # Tabellen definition suchen
    tabdef = self.get_tab_from_deftab(tabname)
    if( not tabdef ):
      self.status = hdef.NOT_OK
      tt          = "Tabelle <%s> konnte in der Definition nicht gefunden werden !!!" % (tabname)
      self.errText = tt
      return False
    #endif

    # cellname suchen
    for celldef in tabdef.cells:
      if( celldef.name == cellname ):
        return True
      #endif
    #endfor

    return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_unit_cell_name_in_deftab(self,tabname,cellname):
    """
    unit      = dbh.get_unit_cell_name_in_deftab(tabname,cellname)
    """
    for tabdef in self.DbDefTab:
      if( tabdef.name == tabname ):
        for celldef in tabdef.cells:
          if( celldef.name == cellname):
            unit = celldef.unit
            return unit
          #endif
        #endfor
      #endif
    #endfor
    return ""
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def get_type_cell_name_in_deftab(self,tabname,cellname):
    """
    datatype = dbh.get_type_cell_name_in_deftab(tabname,cellname)
    # Schl�sselw�rter Zellen  DB_DATA_TYPE_DATUM       Datum-Typ
    #                         DB_DATA_TYPE_STR         String-Typ
    #                         DB_DATA_TYPE_FLOAT       Float-Typ
    #                         DB_DATA_TYPE_INT         Integer-Typ
    #                         DB_DATA_TYPE_KEY         Key-Typ       Key einer anderen Tabelle
    """
    for tabdef in self.DbDefTab:
      if( tabdef.name == tabname ):
        for celldef in tabdef.cells:
          if( celldef.name == cellname):
            return celldef.datatype
          #endif
        #endfor
      #endif
    #endfor
    return 0
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_datum_type(self,tabname,cellname):
    datatype = self.get_type_cell_name_in_deftab(tabname,cellname)

    if( datatype == hdb.DB_DATA_TYPE_DATUM ): return True
    else:                                         return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_str_type(self,tabname,cellname):
    datatype = self.get_type_cell_name_in_deftab(tabname,cellname)

    if( datatype == hdb.DB_DATA_TYPE_STR ): return True
    else:                                       return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_float_type(self,tabname,cellname):
    datatype = self.get_type_cell_name_in_deftab(tabname,cellname)

    if( datatype == hdb.DB_DATA_TYPE_FLOAT ): return True
    else:                                         return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_int_type(self,tabname,cellname):
    datatype = self.get_type_cell_name_in_deftab(tabname,cellname)

    if( datatype == hdb.DB_DATA_TYPE_INT ): return True
    else:                                       return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_key_type(self,tabname,cellname):
    datatype = self.get_type_cell_name_in_deftab(tabname,cellname)

    if( datatype == hdb.DB_DATA_TYPE_KEY ): return True
    else:                                       return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_primkey_type(self,tabname,cellname):
    datatype = self.get_type_cell_name_in_deftab(tabname,cellname)

    if( datatype == hdb.DB_DATA_TYPE_PRIMKEY ): return True
    else:                                       return False

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def is_item_in_cell_tab_data(self,tabname,cellname,item):

    if( not self.db.exist_table(tabname) ):
      self.status = hdef.NOT_OK
      tt          = "Tabelle <%s> konnte in der Definition nicht gefunden werden !!!" % (tabname)
      self.errText = tt
      return False
    else:
      if( not self.db.exist_cell(tabname,cellname) ):
        self.status = hdef.NOT_OK
        tt          = "Zellenname <%s> konnte in Tabelle <%s> nicht gefunden werden !!!" % (cellname,tabname)
        self.errText = tt
        return False
      else:
        (header_liste,data_liste) = self.get_tab_data(tabname,cellname)

        flag = False
        for row in data_liste:
          if( item in row ):
            flag = True
            break
          #endif
        #endofr
        return flag
      #endif
    #endif
    return False
  #enddef
  def exist_table_in_deftab(self,tabname):
    """
    Pr�ft, ob tabname in der Definition der Tabellen vorhanden ist
    R�ckgabe True/FAlse
    """
    for tabdef in self.DbDefTab:
      if( tadef.name == tabname ):
        return True
      #endif
    #endfor
    return False
  def set_isdb_in_deftab(self,tabname,isdb):
    """
    setzt DbDefTAb[i] auf den Wert isdb 1: TAbelle ist in database erstellt
    0: is nicht erstellt
    """
    flag = True
    for i in range(len(self.DbDefTab)):
      if( self.DbDefTab[i].name == tabname ):
        self.DbDefTab[i].isdb = isdb
        flag = False
        break
      #endif
    #endfor
    if( flag ):
      self.status  = self.NOT_OKAY
      self.errText = "Tabelle <%s> konnte DefTabStruktur DBDefTab nicht gefunden werden <%s> nicht gelesen werden !!!" % (tabname)
    #endif

    return self.status

#===============================================================================
#===============================================================================
  def get_num_of_items(self,tabname):
    """
    Anzahl Daten Items
    """
    # Sucht Tabelle in Definition und db
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return 0
    #endif

    return self.db.get_num_of_items(tabname)
#===============================================================================
#===============================================================================
  def add_new_data_set(self,tabname,d):
    """
    Aus d (dictionary) werden die items ausgelesen und gestzt und eingeben
    d[cellnameA]=WertA
    d[cellnameB]=WertB
    ...
    Alle Zellen einer Tabelle m�ssen vorhanden sein
    return status
    """

    # Sucht Tabelle in Definition und db
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return self.status
    #endif

    # get tabstruct von deftab
    deftab = self.get_tab_from_deftab(tabname)

    # Namensliste des dictonary erstellen
    names = d.keys()
    # Liste mit [CellName,data] zur �bergabe erstellen
    liste = []
    # cellnames pr�fen, ob vollst�ndig
    for defcell in deftab.cells:
      # nicht nach primary key suchen
      if( defcell.name != self.PRIMARY_KEY_NAME ):
        flag = True
        for name in names:
          if( name == defcell.name ): # gefunden
            liste.append([name,self.convert_value(defcell,d[name])])
            flag = False
            break
          #endif
        #endfor
        if( flag ):
          self.status  = self.NOT_OKAY
          self.errText = "In dictionary d ist die Zelle <%s> nicht vorhanden (nach DbDefTab wird diese ben�tigt)"
          return self.status
        #endif
      #endif
    #endfor

    if( self.db.add_new_data_set(tabname,liste) != self.db.OKAY ):
      self.errText += self.db.errText;
      self.status  = self.NOT_OKAY
      return self.status
    #endif
    return self.status
#===============================================================================
#===============================================================================
  def modify_data_by_primkey(self,tabname,primkey,d):
    """
    Aus d (dictionary) werden die items ausgelesen und gesetzt und eingeben
    und mit dem primkey ge�ndert
    d[cellnameA]=WertA
    d[cellnameB]=WertB
    ...
    Alle Zellen einer Tabelle m�ssen vorhanden sein
    return status
    """
    # Sucht Tabelle in Definition und db
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return self.status
    #endif

    # get tabstruct von deftab
    deftab = self.get_tab_from_deftab(tabname)

    # Namensliste des dictonary erstellen
    names = d.keys()

    # Liste mit [CellName,data] zur �bergabe erstellen
    liste = []
    # names pr�fen, ob in deftab.cells
    for cellname in names:
      if(   (cellname != self.PRIMARY_KEY_NAME) ):
        for defcell in deftab.cells:
          if( cellname == defcell.name ): # gefunden
            liste.append([cellname,self.convert_value(defcell,d[cellname])])
            break
          #endif
        #endfor
      #endif
    #endfor

    if( len(liste) != 0 ):
      if( self.db.modify_data_set_by_key(tabname,self.PRIMARY_KEY_NAME,primkey,liste) != self.db.OKAY ):
        self.errText += self.db.errText;
        self.status  = self.NOT_OKAY
        return self.status
      #endif
    #endif
    return self.status

#===============================================================================
  def delete_data_by_primkey(self,tabname,primkey_liste):
    """
    delete_data_by_primkey(self,tabname,primkey)
    delete_data_by_primkey(self,tabname,primkey_liste):
    Es werden aus der Tabelle mit tabname die Zellen mit primkey bzw liste von primkeys
    gel�scht
    return status
    """
    # Liste erstellen wenn notwendig
    if( not h.is_list(primkey_liste)):
      primkey_liste = [primkey_liste]

    # Sucht Tabelle in Definition und db
    if( not self.check_tab_in_tabdef_und_db(tabname) ):
      return self.status
    #endif

    # get tabstruct von deftab
    deftab = self.get_tab_from_deftab(tabname)

    for primkey in primkey_liste:
      if( self.db.exist_data_in_tab(tabname,self.PRIMARY_KEY_NAME,primkey) ):
        self.db.delete_data_set_by_key(tabname,self.PRIMARY_KEY_NAME,primkey)
      else:
        self.errText +="Primary Key %i existiert in Tabelle %s nicht und kann nicht gel�scht werden" % (primkey,tabname)
        self.status  = self.NOT_OKAY
        return self.status
      #endif
    #enfor
    return self.status


#===============================================================================
#===============================================================================
  def convert_value(self,defcell,value):
    """
    Konvertiert Wert, wenn notwendig
    return converted value
    """
    if(  (defcell.datatype == hdb.DB_DATA_TYPE_PRIMKEY) \
      or (defcell.datatype == hdb.DB_DATA_TYPE_DATUM)       \
      or (defcell.datatype == hdb.DB_DATA_TYPE_INT)         \
      or (defcell.datatype == hdb.DB_DATA_TYPE_KEY)         \
      ):
      conv_value = int(value)
    elif(  (defcell.datatype == hdb.DB_DATA_TYPE_STR) ):
      if( not isinstance(value, str) ):
        conv_value = h.to_unicode(str(value))
      else:
        conv_value = h.to_unicode(value)
      #endif
    elif(  (defcell.datatype == hdb.DB_DATA_TYPE_FLOAT) ):
      conv_value = float(value)
    else:
      self.errText = "In Tabelle: konnte aus der Zelle: <%s> nicht der type(%i) umgesetzt werden " % (defcell.name,defcell.type)
      self.status  = self.NOT_OKAY
      conv_value    = -1.0
    #endif

    return conv_value

  def data_base_is_modified(self):
    """
    ask if any item in database has changed
    """

    return self.db.data_base_is_modified()