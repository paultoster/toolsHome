# -*- coding: cp1252 -*-
# datdb
#
# Dbfile                 Dateiname des db-Files
#
#
# Funktionen
#
# db = hdb.db(dbfile) Klasse anlegen
# db.close_dbfile()   db-Datei schliessen
#
# db.status
# db.errText
#
# db.has_log_text()
# db.get_log_text()
#
# db.exist_table(tabname)
#   return True/False
#
# db.exist_cell(tabname,cellname)
#   return True/False
#
# db.data_base_is_modified()
#   return True/False
#
# cells = db.get_db_table_info(tablename):
#    cells = [[cellname, index, type],[cellname, index, type], ... ]
#            cellname string
#            index    = i>=0
#            type     = INTEGER,REAL,TEXT
#
# status = db.build_db_table(tabname,cell_list):
#
#    cell_list = [[cellname,datatype],[cellname,datatype],...]
#           cellname    Name der Zelle
#           datatype    DatenType integer-Wert siehe oben
#
# status = db.del_cell_in_table(tabname,cellname)
#
# nrows = db.get_num_of_items(tabname)
#
# flag = db.exist_data_in_tab(tabname,cellname,cellvalue):
#
# data = db.get_data_from_tab(tabname,cellnames)
#       Sucht alle items
# data = db.get_data_from_tab(tabname,cellnames,cellname_w_value,value)
#       Sucht items bei denen cellname_w_value = value ist
#
#       data = [[data1],[data3], ...] wenn cellnames = [cellname1,cellname3,...]
#
# status = db.add_new_data_set(tabname,liste)
#
#       liste = [[Zelle1,val1],[Zelle2,val2],...]
#
# status = db.modify_data_set_by_key(tabname,keyname,keywert,liste):
#       Es wird ein nach der ersten row mit keyname = keywert gesucht
#       und liste in diese row eingearbeitet
#       liste = [[Zelle1,val1],[Zelle2,val2],...]
#
# status = db.delete_data_set_by_key(tabname,keyname,keywert):
#       Es wird ein nach keyname = keywert gesucht
#       und diesen datansatz gel�scht
#
# flag = db.data_base_is_modified()
#      True: database has changed in this run
# intern:
#
# self.open_dbfile(file)
# self.add_log_text(text)
#
# index = db.get_data_type_number(datatypename) liest index aus Datatype-Liste
#
#
import os, types, sqlite3
import re
import random
import hfkt     as h
import hfkt_def as hdef
import types


DB_DATA_TYPE_DATUM   =   0
DB_DATA_TYPE_PRIMKEY =   1
DB_DATA_TYPE_STR     =   2
DB_DATA_TYPE_FLOAT   =   3
DB_DATA_TYPE_INT     =   4
DB_DATA_TYPE_KEY     =   5
DB_DATA_TYPE_STRING_LISTE = ["DB_DATA_TYPE_DATUM","DB_DATA_TYPE_PRIMKEY","DB_DATA_TYPE_STR","DB_DATA_TYPE_FLOAT","DB_DATA_TYPE_INT","DB_DATA_TYPE_KEY"]



def get_data_type_number(datatypename):
  """
  vergleicht datatypename mit Definition und gibt Integer-Wert zur�ck
  Wenn nicht vorhanden wird None zur�ck gegeben
  """
  index = 0
  for type in DB_DATA_TYPE_STRING_LISTE:
    if( datatypename == type ):
      return index
    else:
      index = index + 1

  # wenn nicht vorhanden
  return None
def format_sql(sql):
    sql = sql.replace(",", ",\n")
    sql = sql.replace("(", "(\n")
    sql = sql.replace(")", "\n)")
    return sql

class db:
  OKAY     = hdef.OK
  NOT_OKAY = hdef.NOT_OK
  errText  = ""
  logText  = ""
  status   = hdef.OK
  con      = None
  modify_flag  = False
# public
#===============================================================================
#===============================================================================
  def __init__(self,dbfile):
    """
    Check Db - Define - LIste with Dbfile
    and build or modify table of Dbfile
    """


    self.dbfile  = dbfile

    #------------------------------------------
    # Open DbFile:
    #------------------------------------------
    self.status = self.open_dbfile()
    if( self.status != self.OKAY ): return
#===============================================================================
#===============================================================================
  def __del__(self):
    """
    Close Connection
    """
    self.close_dbfile()
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

#===============================================================================
#===============================================================================
  def data_base_is_modified(self):
    if( self.modify_flag ): return True
    else:                   return False
    #endif
#===============================================================================
#===============================================================================
  def open_dbfile(self):
    """
    open db-File and get version
    out:
      self.con
      self.cur

    return status
    """
    if( self.status   == self.OKAY ):
      try:
        self.con = sqlite3.connect(self.dbfile)

        self.cur = self.con.cursor()
        self.cur.execute('SELECT SQLITE_VERSION()')

        data = self.cur.fetchone()

        self.add_log_text("db: %s" % self.dbfile)
        self.add_log_text("SQLite version: %s" % data)

      except (sqlite3.Error, e):

        self.errText = "sqlite3.connect(%s) is not working (Fehler %s)" % (elf.dbfile,e.args[0])
        self.status  = self.NOT_OKAY

    return self.status
#===============================================================================
#===============================================================================
  def close_dbfile(self):
    """
    close if connected
    """
    if( self.con != None ):
      self.con.close()
      self.con = None
      self.add_log_text("Close Connection to <%s>" % self.dbfile )
    #endif
#===============================================================================
#===============================================================================
  def exist_table(self,tabname):
    """
    check table exist
    out:

    """

    if( self.status   == self.OKAY ):
      # Abfrage aller Tabellen
      #-----------------------
      self.cur.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY Name")
      liste = self.cur.fetchall()
                                      # self.tables = map(lambda t: t[0], a )


      for item in liste:
        if( item[0] == tabname ):
          return True
        #endif
      #endfor
    #endif

    return False
#===============================================================================
#===============================================================================
  def get_all_tables(self):
    """
    get list of tables
    out:

    """
    listeout = []
    if( self.status   == self.OKAY ):
      # Abfrage aller Tabellen
      #-----------------------
      self.cur.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY Name")
      liste = self.cur.fetchall()

      for item in liste:
        listeout.append(item[0])
                                      # self.tables = map(lambda t: t[0], a )
    #endif

    return listeout
#===============================================================================
#===============================================================================
  def exist_cell(self,tabname,cellname):
    """
    Sucht ob in Tabelle tabname die Zeller cellname enthalten ist
    return True/False

    """
    if( self.exist_table(tabname) ):
      cell_list = self.get_db_table_info(tabname)

      for cell in cell_list:
        if( cell[0] == cellname ):
          return True
          
        #endif
      #endfor
    else:
      self.errText = "Tabelle <%s> is nicht in database <%s> vorhanden" % (tabname,self.dbfile)
      self.status  = self.NOT_OKAY
    #endif
    return False
#===============================================================================
#===============================================================================
  def get_db_table_info(self,tname):   # interne Funktion
    """
    Tabellen info zur�ckgeben
    return cells

    cells = [[cellname, index, type],[cellname, index, type], ... ]
    """
    cells = []
    if( self.status   == self.OKAY ):

      try:

        # Zellen abfragen
        #-------------------
        self.cur.execute("PRAGMA table_info(%s)" % tname)

        # Liste mit Tabellen Info
        #------------------------
        liste = self.cur.fetchall()

      except (sqlite3.Error, e):
        self.errText = "Tabelle: command PRAGMA table_info(%s) gibt Fehler:<%s>" % (tname,e.args[0])
        self.status  = self.NOT_OKAY

      # Attribute eintragen
      #--------------------
      for item in liste:
        #                name,   index,  type
        cells.append([item[1],item[0],item[2]])

    return cells
#===============================================================================
#===============================================================================
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
##  def get_index_of_table(self,tabname):
##    """
##    Sucht Index mit tabname in DbTab
##    """
##    index  = -1
##    if( self.status   == self.OKAY ):
##      icount = 0
##      for tab in self.DbTab:
##        if( tab.name == tabname ):
##          index = icount
##          break
##        else:
##          icount += 1
##        #endIf
##      #endFor
##    return index
#-------------------------------------------------------------------------------
##  def get_cell_index_of_table(self, dbtab,cellname):
##    """
##    Sucht nach Index der Zelle
##    """
##    index  = -1
##    icount = 0
##    for cell in dbtab.cells:
##      if( cell.name == cellname ):
##        index = icount
##        break
##      else:
##        icount += 1
##      #endIf
##    #endFor
##    return index
#-------------------------------------------------------------------------------
##  def get_tab(self, tabname):
##    """
##    Gibt die Tabelle aus Liste zur�ck
##    """
##    for dbtab in self.DbTab:
##      if( dbtab.name == tabname ):
##        return dbtab
##      #endif
##    #endfor
##    return None
#-------------------------------------------------------------------------------
##  def get_cell(self, tabname, cellname):
##    """
##    Gibt die Zelle aus Tabelle aus Liste zur�ck
##    return cell struct if not return None
##    """
##    for dbtab in self.DbTab:
##      if( dbtab.name == tabname ):
##        for cell in dbtab.cells:
##          if( cell.name == cellname ):
##            return cell
##          #endif
##        #endfor
##      #endif
##    #endfor
##    return None
##
##
#-------------------------------------------------------------------------------
  def build_db_table(self,tabname,cell_list):
    """
    Erstellt neu Tabell nach Definition
    tabname                   Name der Tabelle
    cell_list = [[cellname,datatype],[cellname,datatype],...]

    or 
    from deftype:
    cell_list = [cell1,cell2,,...] with cell1.name and cell1.datatype

    or

    cell_list = [{"name":cellname,"datatype":datatype},{"name":cellname,"datatype":datatype},...]

    cellname    Name der Zelle
    datatype    DatenType integer-Wert siehe oben
    return status
    """

    # Kommando table name und Start
    #------------------------------
    command = "CREATE TABLE %s (" % tabname

    # Suche primary  key in attributs
    #--------------------------------
    for cell in cell_list:
      
      if(isinstance(cell,dict)):
        cellname = cell["name"]
        datatype = cell["datatype"]
      if isinstance(cell,list):
        cellname = cell[0]
        datatype = cell[1]
      else:
        cellname = cell.name
        datatype = cell.datatype

      if( datatype == DB_DATA_TYPE_PRIMKEY ): # primary key
        command = command + " %s INTEGER PRIMARY KEY " % cellname

      elif(  (datatype == DB_DATA_TYPE_DATUM) \
          or (datatype == DB_DATA_TYPE_INT)   \
          or (datatype == DB_DATA_TYPE_KEY)\
          ):
        command = command + " %s INTEGER" % cellname
      elif(  (datatype == DB_DATA_TYPE_FLOAT) ):
        command = command + " %s REAL" % cellname
      elif(  (datatype == DB_DATA_TYPE_STR) ):
        command = command + " %s TEXT" % cellname
      else:
        self.errText = "In Tabelle:<%s> konnte aus der Zelle: <%s> nicht der type(%i) umgesetzt werden " % (tabname,cellname,datatype)
        self.status  = self.NOT_OKAY
        return self.status
      #endIf
      command = command + ","
    #endFor
    command = command[0:-1] + ")"

    try:
      # sqlite Kommando ausf�hren
      #--------------------------------
      self.cur.execute(command)

      self.modify_flag = True

    except:
      self.errText = "Tabelle: <%s> command <%s> gibt Fehler:<%s>" % (tabname,command,e.args[0])
      self.status  = self.NOT_OKAY

    return self.status
#-------------------------------------------------------------------------------
  def build_new_cell_in_table(self,deftabname,defcellname,defcelldatatype):
    """
    Erstellt eine neu Zelle in Tablle
    """
    if( self.exist_table(deftabname) ):
      if( not self.exist_cell(deftabname,defcellname) ):

        if( defcelldatatype == DB_DATA_TYPE_PRIMKEY ):
          self.errText = "In Tabelle:<%s> kann keine Zelle: <%s> mit Typ DB_DATA_TYPE_PRIMKEY hinzugef�gt werden " % (deftabname,defcellname)
          self.status = self.NOT_OKAY
        else:

          nrows = self.get_num_of_items(deftabname)
          if( self.status != self.OKAY ):
            return self.status

          if( nrows > 0 ):  flag = 1
          else:             flag = 0
          flag = 0


          # Kommando table name und Start
          #------------------------------
          command = "ALTER TABLE %s ADD COLUMN" % deftabname


          if(  (defcelldatatype == DB_DATA_TYPE_DATUM) \
              or (defcelldatatype == DB_DATA_TYPE_INT)   \
              or (defcelldatatype == DB_DATA_TYPE_KEY)\
              ):
            if( flag ): command = command + " %s INTEGER DEFAULT %s" % (defcellname,"0")
            else:       command = command + " %s INTEGER" % defcellname
          elif(  (defcelldatatype == DB_DATA_TYPE_FLOAT) ):
            if( flag ): command = command + " %s REAL DEFAULT %s" % (defcellname,"0.0")
            else:       command = command + " %s REAL" % defcellname
          elif(  (defcelldatatype == DB_DATA_TYPE_STR) ):            
            if( flag ): command = command + " %s TEXT default %s" % (defcellname,"-")
            else:       command = command + " %s TEXT" % defcellname
          else:
            self.errText = "In Tabelle:<%s> konnte aus der Zelle: <%s> nicht der type(%i) umgesetzt werden " % (deftabname,defcelldatatype)
            self.status  = self.NOT_OKAY
            return self.status
          #endIf

        #endif
      #endif
    #endif
    try:
      # sqlite Kommando ausf�hren
      #--------------------------------
      self.cur.execute(command)
      self.modify_flag = True

    except:
      self.errText = "Tabelle: <%s> command <%s> gibt Fehler" % (deftabname,command)
      self.status  = self.NOT_OKAY

    # neue Tabelle in Liste aufnehmen
    #--------------------------------
    self.get_db_table_info(deftabname)
    return self.status

## BEGIN TRANSACTION;
## CREATE TEMPORARY TABLE t1_backup(a,b);
## INSERT INTO t1_backup SELECT a,b FROM t1;
## DROP TABLE t1;
## CREATE TABLE t1(a,b);
## INSERT INTO t1 SELECT a,b FROM t1_backup;

### Connecting to the database file
##conn = sqlite3.connect(sqlite_file)
##c = conn.cursor()
##
### A) Adding a new column without a row value
##c.execute("ALTER TABLE {tn} ADD COLUMN '{cn}' {ct}"\
##        .format(tn=table_name, cn=new_column1, ct=column_type))
##
### B) Adding a new column with a default row value
##c.execute("ALTER TABLE {tn} ADD COLUMN '{cn}' {ct} DEFAULT '{df}'"\
##        .format(tn=table_name, cn=new_column2, ct=column_type, df=default_val))
##
### Committing changes and closing the connection to the database file
##conn.commit()
##conn.close()
#============================================================================================================================
#============================================================================================================================

##  def del_cell_in_table(self,deftabname,defcellname):
##    """
##    L�scht eine Zelle in Tablle
##    """
##    if( self.status == self.OKAY ):
##      if( self.exist_table(deftabname) ):
##        if( self.exist_cell(deftabname,defcellname) ):
##
##          # Kommando table name und Start
##          #------------------------------
##          command = "ALTER TABLE %s DROP COLUMN %s" % (deftabname,defcellname)
##          try:
##            # sqlite Kommando ausf�hren
##            #--------------------------------
##            self.cur.execute(command)
##
##          except (sqlite3.Error, e):
##            self.errText = "Tabelle: <%s> command <%s> gibt Fehler:<%s>" % (deftabname,command,e.args[0])
##            self.status  = self.NOT_OKAY
##          #endtry
##
##          # neue Tabelle in Liste aufnehmen
##          #--------------------------------
##          self.get_db_table_info(deftabname)
##        #endif
##      #endfi
##    #endif
##
##    return self.status
#============================================================================================================================
#============================================================================================================================
  def del_cell_in_table(self,deftabname,defcellname):
    """
    L�scht eine Zelle in Tablle
    """
    try:
      columns = [ c[1] for c in self.cur.execute("PRAGMA table_info(%s)" % deftabname) ]
      columns = [ c for c in columns if c != defcellname ]
      sql = self.cur.execute("SELECT sql from sqlite_master where name = '%s'"
          % deftabname).fetchone()[0]
      sql = format_sql(sql)
      lines = sql.splitlines()
      findcol = r'\b%s\b' % defcellname
      keeplines = [ line for line in lines if not re.search(findcol, line) ]
      create = '\n'.join(keeplines)
      create = re.sub(r',(\s*\))', r'\1', create)
      temp = 'tmp%d' % random.randint(1e8, 1e9)
      self.cur.execute("ALTER TABLE %(old)s RENAME TO %(new)s" % {
          'old': deftabname, 'new': temp })
      self.cur.execute(create)
      self.cur.execute("""
          INSERT INTO %(new)s ( %(columns)s )
          SELECT %(columns)s FROM %(old)s
      """ % {
          'old': temp,
          'new': deftabname,
          'columns': ', '.join(columns)
      })
      self.cur.execute("DROP TABLE %s" % temp)
      self.modify_flag = True

    except (sqlite3.Error, e):
      self.errText = "Tabelle: <%s> gibt Fehler:<%s>" % (deftabname,e.args[0])
      self.status  = self.NOT_OKAY
    #endtry

    return self.status

#============================================================================================================================
#============================================================================================================================
  def get_num_of_items(self,tabname):
    """
    Wieviele DAtens�tze entahlaten
    """
    try:
      # sqlite Kommando ausf�hren
      #--------------------------------
      self.cur.execute("SELECT * FROM %s" % tabname)
      rows = self.cur.fetchall()
      nrows = len(rows)


    except (sqlite3.Error, e):
      self.errText = "Tabelle: <%s> command <%s> gibt Fehler:<%s>" % (tabname,command,e.args[0])
      self.status  = self.NOT_OKAY

    return nrows

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def exist_data_in_tab(self,tabname,cellname,cellvalue):
    """
    Pr�ft Existenz der Zelle mit dem Zellenwert in der Tabelle
    r = c.execute("SELECT EXISTS(SELECT 1 FROM airports WHERE ICAO='EHAM')")
    "SELECT count(*) FROM people WHERE pin=?"
    """
    #command = "SELECT EXISTS(SELECT 1 FROM %s WHERE %s=%i)" % (tabname,cellname,100)
    command  = "SELECT count(*) FROM %s WHERE %s=?" % (tabname,cellname)
    vliste   = (cellvalue,)
    try:
      # sqlite Kommando ausf�hren
      #--------------------------------
      vals = self.con.execute(command,vliste)
      if( vals.fetchone()[0] > 0 ):
        return True
      else:
        return False
    except (sqlite3.Error, e):
      self.errText = "Tabelle: <%s> command <%s> gibt Fehler:<%s>" % (tabname,command,e.args[0])
      self.status  = self.NOT_OKAY
    return False
#============================================================================================================================
#============================================================================================================================
  def get_data_from_tab(self,tabname,cellnames,cellname_w_value=None,value=None):
    """
    hole alle Daten von Tabelle mit den Zellnames
    datas = db.get_data_from_tab(tabname,cellnames)
    hole alle Daten von Tabelle mit dem Wert value in der Zelle cellname_w_value
    datas = db.get_data_from_tab(tabname,cellnames,cellname_w_value,value)
    """
    datas   = []
    d       = None
    command = "SELECT  "
    for i in range(len(cellnames)):
      if( i == 0 ):
        command += cellnames[i]
      else:
        command += ", "+cellnames[i]
      #endif
    #endfor
    command += " from %s" % tabname

    if( (cellname_w_value) and (value) ):
      command += " WHERE %s=:%s" % (cellname_w_value,cellname_w_value)
      d       =  {cellname_w_value:value}
    #endif

    try:
      # sqlite Kommando ausf�hren
      #--------------------------------
      if( d ):
        datas = self.con.execute(command,d)
      else:
        datas = self.con.execute(command)

    except (sqlite3.Error, e):
      self.errText = "Tabelle: <%s> command <%s> gibt Fehler:<%s>" % (tabname,command,e.args[0])
      self.status  = self.NOT_OKAY

    data_liste = []
    for liste in datas:
      row = []
      for item in liste:
        row.append(item)
      data_liste.append(row)

    return data_liste
#============================================================================================================================
#============================================================================================================================
  def add_new_data_set(self,tabname,liste):
    """
    Es wird ein neuer Datensatz erstellt in liste sind Tupel mit Zellname und Wert
    return status liste = [[Zelle1,val1],[Zelle2,val2],...]

    INSERT INTO Tabellenname (Zelle1,Zelle2, ...) VALUES (,val2, ...)
    """
    command = "INSERT INTO %s " % tabname
    vliste  = []
    for i in range(len(liste)):
      if( i == 0 ):
        command += "("+liste[i][0]
      else:
        command += ","+liste[i][0]
      #endif
    #endfor
    command += ") VALUES "
    for i in range(len(liste)):
      if( i == 0 ):
        command += "(?"
        vliste.append(liste[i][1])
      else:
        command += ",?"
        vliste.append(liste[i][1])
      #endif
    #endfor
    command += ")"

    try:

      self.con.execute(command,vliste);
      self.con.commit()
      self.modify_flag = True
    except (sqlite3.Error, e):
      self.errText = "Tabelle: <%s> gibt Fehler:<%s>" % (deftabname,e.args[0])
      self.status  = self.NOT_OKAY
    #endtry

    return self.status
#============================================================================================================================
#============================================================================================================================
  def modify_data_set_by_key(self,tabname,keyname,keywert,liste):
    """
    Es wird ein nach der ersten row mit keyname = keywert gesucht
    und liste in diese row eingearbeitet
    liste = [[Zelle1,val1],[Zelle2,val2],...]
    return status

    "UPDATE tabname SET Zelle1=? WHERE keyname=?",(val1,keywert)
    "UPDATE tabname SET Zelle2=? WHERE keyname=?",(val2,keywert)
    ...
    """

    for items in liste:
      if( len(items) >= 2 ):
        command = "UPDATE %s SET %s=? WHERE %s=?" % (tabname,items[0],keyname)
        vliste  = (items[1],keywert)
        try:
          self.con.execute(command,vliste);
          self.con.commit()
          self.modify_flag = True
        except (sqlite3.Error, e):
          self.errText = "Tabelle: <%s> gibt Fehler:<%s>" % (deftabname,e.args[0])
          self.status  = self.NOT_OKAY
          return self.status
        #endtry
      #endif
    #endfor
    return self.status
#============================================================================================================================
#============================================================================================================================
  def delete_data_set_by_key(self,tabname,keyname,keywert):
    """
    Es wird ein nach keyname = keywert gesucht
    und diesen datansatz gel�scht
    return status
    DELETE FROM tabname WHERE keyname=keywert'
    """

    command = "DELETE FROM %s WHERE %s=?" % (tabname,keyname)
    vliste  = (keywert,)
    try:
      self.con.execute(command,vliste)
      self.con.commit()
      self.modify_flag = True
    except (sqlite3.Error, e):
      self.errText = "Tabelle: <%s> gibt Fehler:<%s>" % (deftabname,e.args[0])
      self.status  = self.NOT_OKAY
      return self.status
    #endtry
    return self.status
