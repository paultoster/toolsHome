# -*- coding: utf8 -*-
#
# AV_main.py: Abrechnung Verbrauch
#
# cp1252
#
# self.par.logfile       log-filename to write log file
# self.par.dbdeffile     data-defenition-file-name
# self.par.dbdatafile    data-base-file-name
#
# self.log               log-Klasse zum schreiben der log-Botschaft
#
# self.ntabdef                     Anzahl der Tabellen in self.tabdef
# self.tabdef[i].name              Name (bei type=DB_TAB_TYPE_SCHABLONE wird spezifischer Name mit '_' anghängt)
# self.tabdef[i].type              siehe hfkt_db: DB_TAB_TYPE_BUILD, DB_TAB_TYPE_SCHABLONE
# self.tabdef[i].comment           Kommentar
# self.tabdef[i].cells[j]          Zellendefinition
# self.tabdef[i].cells[j].name     ZellenName
# self.tabdef[i].cells[j].datatyp  Zellendatatyp   # Datentype siehe hfkt_db:DB_DATA_TYPE_DATUM,DB_DATA_TYPE_STR,DB_DATA_TYPE_FLOAT,DB_DATA_TYPE_INT,DB_DATA_TYPE_KEY
# self.tabdef[i].cells[j].comment  Kommentar
# self.tabdef[i].ncells            Anzahl der Zahlen

import sys, os

tools_path = "D:\\tools_tb\\python3"

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

# Hilfsfunktionen
import hfkt as h
import hfkt_def as hdef
import hfkt_log as hlog
import hfkt_ini as hini
import hfkt_db_handle  as hdbh
import hfkt_db         as hdb
import sgui

import AbrechnungVerbrauchMaterial as avm
import AbrechnungVerbrauchPerson as avp
# import AbrechnungAlteDaten as avad
import AbrechnungVerbrauchAuflisten as ava

#===============================================================================
#===============================================================================
class AV:
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  status     = hdef.OK
  OKAY       = hdef.OK
  NOT_OKAY   = hdef.NOT_OK
  DEBUG_FLAG = False

  AKTIVESTAT_AKTIV    = 1
  AKTIVESTAT_PASSIV   = 2
  TAB_MATERIAL        = 'Material'
  CELL_MATERIALNAME   = 'Materialname'
  CELL_MATERIALUNIT   = 'Materialeinheit'
  CELL_ABRECHUNGSUNIT = 'Abrechnungseinheit'
  TAB_MATERIALEINKAUF = 'Einkauf'
  CELL_DATUM          = 'Datum'
  CELL_KEY1           = 'Key1'
  CELL_MENGE          = 'Menge'
  CELL_WERT           = 'Wert'
  CELL_HERKUNFT       = 'Herkunft'
  TAB_MATERIALBESTAND = 'Materialbestand'
  CELL_BEGINN           = 'Beginn'
  TAB_MATERIALABRPREIS  = 'MaterialAbrechnunspreis'
  CELL_ABRPREIS         = 'Abrechnungspreis'
  TAB_PERSONEN          = 'Personen'
  CELL_NAME             = 'Name'
  CELL_ABTEILUNG        = 'Abteilung'
  CELL_TELEFON          = 'Telefon'
  CELL_TABKONTO         = 'TabelleKonto'
  TAB_PERSONENSTAT      = 'PersonenStatus'
  CELL_AKTIVESTAT       = 'AktivStatus'
  TAB_PERSONENVERBRAUCH  = 'PersonenVerbrauch'
  CELL_KEY2              = 'Key2'
  TAB_PERSONENEINZAHLUNG = 'PersonenEinzahlung'
  TAB_ABRECHNUNG         = 'Abrechnung'

  INIVARLIST = [[u'allg',        u'logfile',          hdef.DEF_STR,         u'log.txt'      ] \
               ,[u'allg',        u'dbdeffile',        hdef.DEF_STR,         u'bddef.txt'    ] \
               ,[u'allg',        u'dbdatafile',       hdef.DEF_STR,         u'dbdata.sql'   ] \
               ,[u'allg',        u'backup_flag',      hdef.DEF_INT,         0               ] \
               ,[u'allg',        u'backup_dir',      hdef.DEF_STR,         u''             ] \
               ]

  DBDEFLISTE = {TAB_MATERIAL:     {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Tabelle mit den Materialsorten'            \
                                  ,'cells':{CELL_MATERIALNAME:     {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Welches MAterial'} \
                                           ,CELL_MATERIALUNIT:     {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'In welchen Mengeneinheiten wird gekauft (z.B. kg)'} \
                                           ,CELL_ABRECHUNGSUNIT:   {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'In welchen Mengeneinheiten wird abgerechnet (z.B. Tasse)'}        \
                                           }
                                  }
               ,TAB_MATERIALEINKAUF: {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Tabelle für den Materialeinkauf'            \
                                     ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
                                              ,CELL_KEY1+'@'+TAB_MATERIAL: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'In welchen Mengeneinheiten wird abgerechnet (z.B. Tasse)'}        \
                                              ,CELL_MENGE:                 {'type':hdb.DB_DATA_TYPE_FLOAT,'unit':'[Einheit]','comment':'Menge'} \
                                              ,CELL_WERT:                  {'type':hdb.DB_DATA_TYPE_INT,'unit':'[Cent]','comment':'Wert in Cent'} \
                                              ,CELL_HERKUNFT:              {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Herkunft des Materials'} \
                                              }
                                     }
               ,TAB_MATERIALBESTAND:  {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Tabelle für den Materialbestand'            \
                                        ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
                                                 ,CELL_KEY1+'@'+TAB_MATERIAL: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Material'}        \
                                                 ,CELL_MENGE:                 {'type':hdb.DB_DATA_TYPE_FLOAT,'unit':'[Tasse]','comment':'Menge'} \
                                                 ,CELL_WERT:                  {'type':hdb.DB_DATA_TYPE_INT,'unit':'[Cent]','comment':'Wert in Cent'} \
                                                 }
                                        }
               ,TAB_MATERIALABRPREIS:   {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Tabelle für den Preis pro Abrechnungseinheit'            \
                                        ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
                                                 ,CELL_KEY1+'@'+TAB_MATERIAL: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Material'}        \
                                                 ,CELL_ABRPREIS:              {'type':hdb.DB_DATA_TYPE_INT,'unit':'[Cent]','comment':'Abrechungspreis pro Einheit'} \
                                                 }
                                        }
               ,TAB_PERSONEN:     {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Tabelle mit den Personen'            \
                                  ,'cells':{CELL_NAME:                  {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Name der Person'} \
                                           ,CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum Aufnahme'} \
                                           ,CELL_ABTEILUNG:             {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Abteilung Person'}        \
                                           ,CELL_TELEFON:               {'type':hdb.DB_DATA_TYPE_STR,'unit':'[]','comment':'Telefon Person'} \
                                           ,CELL_AKTIVESTAT:            {'type':hdb.DB_DATA_TYPE_INT,'unit':'[]','comment':'0: gelöscht, 1: aktive, 2: passiv'}        \
                                           }
                                  }
               ,TAB_PERSONENSTAT: {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Status der Person'            \
                                  ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum Aktiv/Passivstatus änderung'} \
                                           ,CELL_KEY1+'@'+TAB_PERSONEN: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Personen'}        \
                                           ,CELL_AKTIVESTAT:            {'type':hdb.DB_DATA_TYPE_INT,'unit':'[]','comment':'0: gelöscht, 1: aktive, 2: passiv'}        \
                                           }
                                  }
               ,TAB_PERSONENVERBRAUCH:  {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Verbrauch der Person'             \
                                        ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
                                                 ,CELL_KEY1+'@'+TAB_PERSONEN: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Person'}        \
                                                 ,CELL_KEY2+'@'+TAB_MATERIAL: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Material'}        \
                                                 ,CELL_MENGE:                 {'type':hdb.DB_DATA_TYPE_FLOAT,'unit':'[Einheit]','comment':'Menge'} \
                                                 }
                                        }
               ,TAB_PERSONENEINZAHLUNG: {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Einzahlungen der Personen'            \
                                        ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
                                                 ,CELL_KEY1+'@'+TAB_PERSONEN: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Person'}        \
                                                 ,CELL_WERT:                  {'type':hdb.DB_DATA_TYPE_INT,'unit':'[Cent]','comment':'Wert in Cent'} \
                                                 }
                                        }
               ,TAB_ABRECHNUNG:         {'type':hdbh.DB_TAB_TYPE_BUILD,'comment':'Abrechungsdatum'            \
                                        ,'cells':{CELL_DATUM:                 {'type':hdb.DB_DATA_TYPE_DATUM,'unit':'[]','comment':'Datum'} \
                                                 ,CELL_KEY1+'@'+TAB_MATERIAL: {'type':hdb.DB_DATA_TYPE_KEY,'unit':'[]','comment':'Key von Tabelle Material'}        \
                                                 }
                                        }
               }


  class parameter_struct:
      def __init__(self):
          self.logfile       = ""      # log-filename to write log file
          self.dbdeffile     = ""      # data-defenition-file-name
          self.dbdatafile    = ""      # data-base-file-name


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def __init__(self,inifile,debug_flag=0):

    self.par = self.parameter_struct()

    # ini-File einlesen
    self.read_ini_file(inifile)
    if( self.status != hdef.OK ): return

    # LogFile erstellen
    self.log = hlog.log(log_file=self.par.logfile)
    self.log.write_e("Start LogFile "+ h.str_akt_datum() + " " + h.str_akt_time())

    # dbdeffile self.dbdeffile lesen
    self.dbh = hdbh.dbhandle(self.DBDEFLISTE,self.par.dbdatafile)
    if( self.dbh.status != hdef.OK ):
      self.log.write_err("Error hfkt_db_handle: "+ self.dbh.errText)
      return
    elif( self.dbh.has_log_text() ):
      self.log.write_e(self.dbh.logText)
      self.dbh.logText = ""

    if( debug_flag ):
      self.DEBUG_FLAG = True
    else:
      self.DEBUG_FLAG = False

    # alte Buchungen einlesen
    # avad.run_alte_daten(self)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run(self):
    AUSWAHL_VERWALTEN         = 0
    AUSWAHL_MATERIAL_EINKAUF  = 1
    AUSWAHL_PERSON_EINZAHLUNG = 2
    AUSWAHL_EINGABEABRECHNUNG = 3
    AUSWAHL_AUSGABE           = 4
    AUSWAHL_MODIFY            = 5
    ABFRAGE_ENDE              = 1

    auswahlliste   = [u"Verwalten"              \
                     ,u"Einkauf Material"       \
                     ,u"Einzahlung Personen"    \
                     ,u"Abrechnung eingeben"    \
                     ,u"Abrechnungen anzeigen"  \
                     ]
    if( self.DEBUG_FLAG ):auswahlliste.append(u"Tabellen modifizieren")
    abfrageliste   = [u"auswahl" \
                     ,u"ende"    \
                     ]
    title          = "Abfrage Menue:"
    condition = True
    while condition:

      [index,indexAbfrage] = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrageliste,title)

      # Ende ---------------------------------
      if( indexAbfrage == ABFRAGE_ENDE ):
        condition = False
      # Abbruch ------------------------------
      elif( index < 0 ):
        pass
##        self.status = hdef.NOT_OK
##        self.log.write_err("Error Abbruch")
##        condition = False
      # Verwalten ---------------------------------------
      elif( index == AUSWAHL_VERWALTEN ):
        self.run_verwalten()
      # Material Einkauf-----------------------------
      elif( index == AUSWAHL_MATERIAL_EINKAUF ):
        avm.run_material_einkauf(self)
      # Person Einzahlung-------------------------------
      elif( index == AUSWAHL_PERSON_EINZAHLUNG  ):
        avp.run_person_einzahlung(self)
      # EingabeAbrechnung -------------------------------
      elif( index == AUSWAHL_EINGABEABRECHNUNG  ):
        self.run_eingabeabrechnung()
      # Ausgabe -----------------------------------------
      elif( index == AUSWAHL_AUSGABE  ):
        self.run_ausgabe()
      # Modify table -----------------------------------------
      elif( index == AUSWAHL_MODIFY ):
        self.run_mod_table()
      #endif
      if( self.status != hdef.OK ):
          condition = False
      #endif
    #enwhile

    return self.status

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_verwalten(self):
    AUSWAHL_MATERIAL    = 0
    AUSWAHL_PERSON_AKT  = 1
    AUSWAHL_PERSON_PAS  = 2
    AUSWAHL_PERSON_NEU  = 3
    ABFRAGE_ENDE        = 1
    auswahlliste   = [u"Material"         \
                     ,u"Aktive Personen"   \
                     ,u"Passive Personen"  \
                     ,u"Person neu"       \
                     ]
    abfrageliste   = [u"auswahl" \
                     ,u"ende"    \
                     ]
    title          = "Abfrage Verwalten:"
    condition = True
    while condition:

      [index,indexAbfrage] = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrageliste,title)

      # Ende ---------------------------------
      if( indexAbfrage == ABFRAGE_ENDE ):
        condition = False
      # Abbruch ------------------------------
      elif( index < 0 ):
        pass
##        self.status = hdef.NOT_OK
##        self.log.write_err("Error Abbruch")
##        condition = False
      # Material -----------------------------
      elif( index == AUSWAHL_MATERIAL ):
        avm.run_material(self)
        condition = False
      # Person -------------------------------
      elif( index == AUSWAHL_PERSON_AKT  ):
        avp.run_person_akt(self)
        condition = False
      elif( index == AUSWAHL_PERSON_PAS  ):
        avp.run_person_pas(self)
        condition = False
      elif( index == AUSWAHL_PERSON_NEU  ):
        avp.run_person_neu_auswahl(self)
        condition = False
      #endif
      if( self.status != hdef.OK ):
          condition = False
      #endif
    #enwhile

    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_eingabeabrechnung(self):
    """
    Eingabe einer Abrechnung bzw. Preis pro Abrechnungsenheit
    """
    # Auswahl Material
    [materialprimkey,materialname,materialunit,materialabreinheit] = avm.run_auswahl_material(self)

    AUSWAHL_PREIS        = 0
    AUSWAHL_BESTAND      = 1
    AUSWAHL_ABRECHNUNG   = 2
    AUSWAHL_EINZELABRECH = 3
    ABFRAGE_BACK         = 1
    auswahlliste   = [u"1) Preis pro %s ändern" % materialabreinheit   \
                     ,u"2) Bestand eingeben"     \
                     ,u"3) Eingabe Abrechnung"   \
                     ,u"3a)Einzel Abrechnung"    \
                     ]
    abfrageliste   = [u"auswahl" \
                     ,u"zurück"    \
                     ]
    title          = "Abrechnung %s:" % materialname
    condition = True
    count     = 0
    while condition:

      [index,indexAbfrage] = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrageliste,title)

      # Ende ---------------------------------
      if( indexAbfrage == ABFRAGE_BACK ):
        condition = False
      # Abbruch ------------------------------
      elif( index < 0 ):
        pass
##        self.status = hdef.NOT_OK
##        self.log.write_err("Error Abbruch")
##        condition = False
      # Material Preis pro Eiheit-----------------------------
      elif( index == AUSWAHL_PREIS ):
        avm.run_material_preisabr(self,materialprimkey)
        if( not(count & 1) ): count += 1
      # Bestand Eingabe-------------------------------
      elif( index == AUSWAHL_BESTAND  ):
        avm.run_bestand_eingabe(self,materialprimkey)
        if( not(count & 2) ): count += 2
      # Person Einzahlung-------------------------------
      elif( index == AUSWAHL_ABRECHNUNG  ):
        avm.run_person_gesamtabrechnung(self,materialprimkey)
        if( not(count & 2) ): count += 4
      elif( index == AUSWAHL_EINZELABRECH ):
        avm.run_person_einzelabrechnung(self,materialprimkey)
        if( not(count & 2) ): count += 4
      #endif
      if( (count == 7) or self.status != hdef.OK ):
          condition = False
      #endif
    #enwhile

    return self.status


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_ausgabe(self):
    AUSWAHL_EINZEL   = 0
    AUSWAHL_SALDO    = 1
    AUSWAHL_GESAMT   = 2
    ABFRAGE_ENDE     = 1
    auswahlliste   = [u"Einzelabrechnung"   \
                     ,u"Saldo Kasse"        \
                     ,u"Gesamtabrechnung"   \
                     ]
    abfrageliste   = [u"auswahl" \
                     ,u"zurück"  \
                     ]
    title          = "Abfrage Abrechnung:"
    condition = True
    while condition:

      [index,indexAbfrage] = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrageliste,title)

      # Ende ---------------------------------
      if( indexAbfrage == ABFRAGE_ENDE ):
        condition = False
      # Abbruch ------------------------------
      elif( index < 0 ):
        pass
      # Einzelabrechnung-----------------------------
      elif( index == AUSWAHL_EINZEL ):
        self.run_ausgabe_einzel()
        condition = False
      # Saldo Kasse-------------------------------
      elif( index == AUSWAHL_SALDO  ):
        ava.run_abrechung_auflisten_saldo_kasse(self)
        condition = False
      # Gesamtabrechnung-------------------------------
      elif( index == AUSWAHL_GESAMT  ):
        self.run_ausgabe_gesamt()
        condition = False
      #endif
      if( self.status != hdef.OK ):
          condition = False
      #endif
    #enwhile

    return self.status
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_ausgabe_einzel(self):
    ABFRAGE_AUSWAHL   = 0
    ABFRAGE_BACK      = 1
    abfrage_liste = [u"auswahl"        \
                    ,u"zurück"         \
                    ]
    title = "Personen-Auswahl"
    condition = True
    while condition:

      # get primary ke name from table person
      primkeyname = self.dbh.get_tab_primary_key_name(self.TAB_PERSONEN)
      if( self.dbh.status != self.dbh.OKAY ):
        self.log.write_e("Fehler in dbh.get_tab_key_name: <%s>" % self.dbh.errText,1)
        self.status = self.NOT_OKAY
        return

      # get all data from table person from cell primary key and Materialname
      (header_liste,data_liste) = self.dbh.get_tab_data(self.TAB_PERSONEN,[primkeyname,self.CELL_NAME,self.CELL_AKTIVESTAT])
      auswahlliste   = []
      prim_key_liste = []
      name_liste     = []
      for row in data_liste:
        ## nur aktive auflisten
        #if( row[2] == self.AKTIVESTAT_AKTIV ):
          prim_key_liste.append(row[0])
          auswahlliste.append("("+str(row[0])+")"+row[1])
          name_liste.append(row[1])
      #endfor

      if(  len(auswahlliste) == 0 ):
        self.log.write_e("noch keine Person eingegeben",1)
        return
      #endif

      (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
      # Zurück -----------------------------
      if( indexAbfrage == ABFRAGE_BACK ):
        condition = False
      # Weiter
      elif( index < 0 ):
        pass
      # Auswahl -----------------------------
      elif( indexAbfrage == ABFRAGE_AUSWAHL ):
        ava.run_abrechung_auflisten_person(self,prim_key_liste[index],name_liste[index])
        condition = False
      #endif
      if( self.status != self.OKAY ):
        return
    #endwhile

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_ausgabe_gesamt(self):
    """
    Abrechnung für einen Zeitraum
    """

    # Auswahl Material
    [materialprimkey,materialname,materialunit,materialabreinheit] = avm.run_auswahl_material(self)

    # Alle Verbrauchsdaten von diesem Material
    (header_liste,data_liste) = self.dbh.get_tab_data_with_value(self.TAB_PERSONENVERBRAUCH,self.CELL_KEY2,materialprimkey,[self.CELL_DATUM])
    # umsortieren
    datas                     = self.dbh.get_colums_from_data_liste(header_liste,data_liste,self.CELL_DATUM)

    # Datumsliste reverse erzeugen
    data_liste.reverse()

    datum_liste = []
    for item in data_liste:
      datum_liste.append(h.secs_time_epoch_to_str(item[0]))
    #endfor
    datum_liste   = h.reduce_double_items_in_liste(datum_liste)
    n_datum_liste = len(datum_liste)

    # aktuelle Abrechnung hinzu fügen
    if( n_datum_liste > 0):
      datum_liste.insert(0, 'aktuell')
      n_datum_liste = n_datum_liste + 1;
    #endif


    # Auswahl eines Datums:
    #----------------------
    ABFRAGE_AUSWAHL   = 0
    ABFRAGE_BACK      = 1
    abfrage_liste = [u"auswahl"        \
                    ,u"zurück"         \
                    ]
    title = "Datums-Auswahl"
    condition = True
    while condition:

      if(  n_datum_liste == 0 ):
        self.log.write_e("noch keine Verbrauchsabrechnung eingegeben",1)
        return
      #endif

      (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(datum_liste,abfrage_liste,title)
      # Zurück -----------------------------
      if( indexAbfrage == ABFRAGE_BACK ):
        condition = False
      # Weiter
      elif( index < 0 ):
        pass
      # Auswahl -----------------------------
      elif( indexAbfrage == ABFRAGE_AUSWAHL ):

        if( index == 0 ):
          flagakt = True
        else:
          flagakt = False
        #endif

        # wieder bereinigen
        n_datum_liste -= 1
        dummy         =  datum_liste.pop(0)

        # aktueller Wert
        if( flagakt ):

          if( n_datum_liste == 1 ): start_zeit_secs = 0
          else:                     start_zeit_secs = int(h.secs_time_epoch_from_str(datum_liste[1]))+82800

          end_zeit_secs   = int(h.secs_akt_time_epoch())+82800

        # ein Abrechnungszeitwert
        else:

          index  -= 1
          if( index == (n_datum_liste-1) ):
            start_zeit_secs = 0
            end_zeit_secs   = int(h.secs_time_epoch_from_str(datum_liste[index]))+82800
          else:
            start_zeit_secs = int(h.secs_time_epoch_from_str(datum_liste[index+1]))+82800
            end_zeit_secs   = int(h.secs_time_epoch_from_str(datum_liste[index]))+82800
          #endif
        #endif
        ava.run_abrechung_auflisten_verbrauch(self,materialprimkey,start_zeit_secs,end_zeit_secs)
        condition = False
      #endif
      if( self.status != self.OKAY ):
        return
    #endwhile

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_mod_table(self):
    """
    Eine Tabelle aus der Datenbank modifizieren
    """
    tab_liste = self.dbh.get_tabname_liste_from_data()

    ABFRAGE_EINGABE   = 0
    ABFRAGE_BACK      = 1
    abfrage_liste = [u"auswählen"       \
                    ,u"zurück"          \
                    ]
    title = "Tabelle auswählen"
    condition = True
    while condition:

      tab_liste = self.dbh.get_tabname_liste_from_data()

      n = len(tab_liste)
      if( n == 0 ): return

      auswahlliste   = []
      for i in range(n):
        tabdef = self.dbh.get_tab_from_deftab(tab_liste[i])
        auswahlliste.append(str(tab_liste[i])+"           ("+tabdef.comment+")")
      #endfor


      (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
      # Zurück -----------------------------
      if( indexAbfrage == ABFRAGE_BACK ):
        condition = False
      # Weiter
      elif( index < 0 ):
        pass
      # Eingabe ------------------------------
      elif( indexAbfrage == ABFRAGE_EINGABE ):
        self.run_mod_table_row(tab_liste[index])
        condition = False
      #endif
      if( self.status != self.OKAY ):
        return
    #endwhile

    return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_mod_table_row(self,tabname):
    """
    Tabelle tabname row auswählen und modifizieren
    """
    cellname_liste = self.dbh.get_cell_names_from_deftab(tabname)
    primkeyname    = self.dbh.get_tab_primary_key_name(tabname)

    n = self.dbh.get_num_of_items(tabname)
    if( n == 0 ): return

    (header_liste,data_liste) = self.dbh.get_tab_data(tabname,cellname_liste)

    # modify data for a certain date:
    # for data in data_liste:
      
    #   if( data[0] == 1580485811):
    #     d = {}
    #     d[header_liste[0]]  = 1580601600
    #     d[header_liste[1]]  = data[1]
    #     d[header_liste[2]]  = data[2]
    #     d[header_liste[3]]  = data[3]

    #     self.dbh.modify_data_by_primkey(tabname,data[4],d)
    #   #endif
    # #endfor
    ABFRAGE_EINGABE   = 0
    ABFRAGE_DELETE    = 1
    ABFRAGE_BACK      = 2
    # Abfrage liste
    abfrage_liste = [u"row auswählen"       \
                    ,u"row löschen"         \
                    ,u"zurück"              \
                    ]
    #Title
    title = "%s row: " % tabname
    index = 0
    primkeyindex = index
    for cellname in header_liste:
      if( cellname == primkeyname):
        primkeyindex = index
      else:
        title += cellname+"/"
      #endif
      index += 1
    #endfor

    # Asuwahlliste
    auswahlliste   = []
    primkeyliste   = []
    n              = 0
    icount         = 0
    for row in data_liste:
      icount += 1
      cell_string = u"(%i.)" % icount
      primkey = 0
      for i in range(len(row)):
        # primkey rauslassen
        if( self.dbh.is_primkey_type(tabname,header_liste[i]) ):
          pass
        # datum
        elif( self.dbh.is_datum_type(tabname,header_liste[i]) ):
          cell_string += str(h.secs_time_epoch_to_int(row[i]))+"/"
        # int/key
        elif(  self.dbh.is_int_type(tabname,header_liste[i]) \
            or self.dbh.is_key_type(tabname,header_liste[i]) ):
          cell_string += str(row[i])+"/"
        # float
        elif( self.dbh.is_float_type(tabname,header_liste[i]) ):
          cell_string += str(row[i])+"/"
        # string
        else:
          try:
            cell_string += row[i]+"/"
          except:
            a = self.dbh.is_key_type(tabname,header_liste[i])
        if( i == primkeyindex ):
          primkey = row[i]
        #endif
      #endfor

      #endfor
      auswahlliste.append(cell_string)
      primkeyliste.append(primkey)
      n += 1
    #endfor

    condition = True
    while condition:

      (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
      # Zurück -----------------------------
      if( indexAbfrage == ABFRAGE_BACK ):
        condition = False
      # Weiter
      elif( index < 0 ):
        pass
      # Eingabe ------------------------------
      elif( indexAbfrage == ABFRAGE_EINGABE ):
        self.run_mod_table_row_modify(tabname,header_liste,data_liste[index],primkeyliste[index],index)
        condition = False
      # Löschen ------------------------------
      elif( indexAbfrage == ABFRAGE_DELETE ):
        self.run_mod_table_row_delete(tabname,header_liste,data_liste[index],primkeyliste[index],index)
        condition = False
      #endif
      if( self.status != self.OKAY ):
        return
    #endwhile

    return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_mod_table_row_modify(self,tabname,header_liste,row,primkey,irow):
    """
    Modifizieren einer row einer Tabelle
    """
    listeAbfrage = []
    listeVorgabe = []
    headerNListe = []
    n = len(header_liste)
    ncount = 0
    for i in range(n):
      # no primkey
      if( not self.dbh.is_primkey_type(tabname,header_liste[i]) ):

        unit = self.dbh.get_unit_cell_name_in_deftab(tabname,header_liste[i])
        listeAbfrage.append(header_liste[i] + " "+unit+" :")
        headerNListe.append(header_liste[i])

        # datum
        if( self.dbh.is_datum_type(tabname,header_liste[i]) ):
          tt = str(h.secs_time_epoch_to_int(row[i]))
        # str
        elif( self.dbh.is_str_type(tabname,header_liste[i]) ):
          tt = row[i]
        else:
          tt = str(row[i])
        #endif
        listeVorgabe.append(tt)
        ncount += 1
      #endif
    #endfor
    title         = 'Zeile %i in Tabelle %s ändern' % (irow,tabname)
    listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAbfrage,vorgabe_liste=listeVorgabe,title=title)

    if( len(listeErgebnis)==0): return

    d = {}
    for i in range(ncount):
      # Datum
      if( self.dbh.is_datum_type(tabname,headerNListe[i]) ):
        ival = h.str_to_int_possible(listeErgebnis[i])
        d[headerNListe[i]]   = h.secs_time_epoch_from_int(ival,1)
      # int/key
      elif(  self.dbh.is_int_type(tabname,headerNListe[i]) \
          or self.dbh.is_key_type(tabname,headerNListe[i]) ):
        ival = h.str_to_int_possible(listeErgebnis[i])
        d[headerNListe[i]]   = ival
      # float
      elif( self.dbh.is_float_type(tabname,headerNListe[i]) ):
        val = h.str_to_float_possible(listeErgebnis[i])
        d[headerNListe[i]]   = val
      # string
      else:
        d[headerNListe[i]]   = listeErgebnis[i]
      #endif
    #endfor
    self.dbh.modify_data_by_primkey(tabname,primkey,d)

    if( self.dbh.status != self.dbh.OKAY ):
      self.log.write_err(self.dbh.errText,1)
      self.status = self.NOT_OKAY

    return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def run_mod_table_row_delete(self,tabname,header_liste,row,primkey,irow):
    """
    delete einer row einer Tabelle
    """
    self.dbh.delete_data_by_primkey(tabname,primkey)

    if( self.dbh.status != self.dbh.OKAY ):
      self.log.write_err(self.dbh.errText,1)
      self.status = self.NOT_OKAY

    return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def __del__(self):

    # close database connection
    self.dbh.close()
    if( self.dbh.has_log_text() ):
      self.log.write_e(self.dbh.logText,1)
      self.dbh.logText = ""
    #endif

    #copy backup
    if( self.par.backup_flag and self.dbh.data_base_is_modified() ):
      if( not os.path.isdir(self.par.backup_dir) ):
        self.log.write_err("Backup-Dir <%s> not available" % self.par.backup_dir,1)
      elif( not os.path.isfile(self.par.dbdatafile) ):
        self.log.write_err("No database-Fiel <%s> found" % self.par.dbdatafile,1)
      else:
        (path,fbody,ext) = h.get_pfe(self.par.dbdatafile)
        backup_file_name = os.path.join(self.par.backup_dir,fbody+"_"+str(h.int_akt_datum())+"_"+str(h.int_akt_time())+"."+ext)
        try:
          h.copy(self.par.dbdatafile,backup_file_name,silent=1)
          self.log.write_e("backup-copy: %s " % backup_file_name,1)
        except:
          self.log.write_err("No backup-copy possible to <%s> " % self.par.dbdatafile,1)
        #endtry
      #endif
    #endif

    # Close Log
    self.close_log_file()



#-------------------------------------------------------------------------------
  def read_ini_file(self,inifile):
    """
    read ini-File to get
    self.par.logfile                Logfilename
    self.par.dbdeffile              Datenbasis-Beschreibungs-Dateiname
    self.par.dbdatafile             Datenbasis-Dateiname
    """
    (self.status,errtext,out) = hini.readini(inifile,self.INIVARLIST)

    if( self.status != hdef.OK ):
      print("Fehler Einlesen Ini-DAtei: <%s> error: %s" % (inifile,errtext))
      return self.status



    self.par.logfile     = out['allg']['logfile']
    self.par.dbdeffile   = out['allg']['dbdeffile']
    self.par.dbdatafile  = out['allg']['dbdatafile']
    self.par.backup_flag = out['allg']['backup_flag']
    self.par.backup_dir = out['allg']['backup_dir']

    if( self.par.backup_flag and (len(self.par.backup_dir)==0) ):
      self.par.backup_flag = 0

    print("================================================================================")
    print("%-20s: %s" % ("Logfilename",self.par.logfile))
    print("%-20s: %s" % ("Datbasefilename",self.par.dbdatafile))
    print("%-20s: %i" % ("BackupFlag",self.par.backup_flag))
    print("%-20s: %s" % ("Backuppath",self.par.backup_dir))
    print("================================================================================")

    return self.status
#-------------------------------------------------------------------------------
  def close_log_file(self):
    self.log.write_e("End   LogFile "+ h.str_akt_datum() + " " + h.str_akt_time())
    print("Close Logfile <%s>" % self.par.logfile)
    self.log.close()
if __name__ == '__main__':
  pass
