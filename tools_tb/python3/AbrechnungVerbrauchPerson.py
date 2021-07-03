# -*- coding: utf8 -*-
#
#-------------------------------------------------------------------------------
# Name:        AbrechnungVerbrauchPerson
# Purpose:
#
# Author:      tom
#
# Created:     21.11.2016
# Copyright:   (c) tom 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------
import sys, os

tools_path = "D:\\tools_tb\\python3" 

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

# Hilfsfunktionen
import hfkt as h
import hfkt_def as hdef
import sgui

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_akt(av):
  ABFRAGE_AENDERN   = 0
  ABFRAGE_NEU       = 1
  ABFRAGE_PASSIV    = 2
  ABFRAGE_LOESCHEN  = 3
  ABFRAGE_BACK      = 4
  abfrage_liste = [u"ändern"         \
                  ,u"neu"            \
                  ,u"passiv machen"  \
                  ,u"löschen"        \
                  ,u"zurück"         \
                  ]
  title = "Personen-Liste aktiv"
  condition = True
  while condition:

    [prim_key_liste,namen_liste] = run_person_akt_get_liste(av)

    n = len(prim_key_liste)
    if( n == 0 ): return

    auswahlliste   = []
    for i in range(n):
      auswahlliste.append("("+str(prim_key_liste[i])+")"+namen_liste[i])
    #endfor

    if( len(auswahlliste) == 0 ):
      auswahlliste = ["noch keine Person eingegeben"]
    #endif

    (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
    # Zurück -----------------------------
    if( indexAbfrage == ABFRAGE_BACK ):
      condition = False
    # Neu ------------------------------
    elif( indexAbfrage == ABFRAGE_NEU or (len(prim_key_liste)==0) ):
      run_person_neu(av)
      condition = False
    # Weiter
    elif( index < 0 ):
      pass
    # Passiv -----------------------------
    elif( indexAbfrage == ABFRAGE_PASSIV ):
      run_person_make_status(av,prim_key_liste[index],namen_liste[index],h.secs_akt_time_epoch(),av.AKTIVESTAT_PASSIV)
      condition = False
    # Löschen -----------------------------
    elif( indexAbfrage == ABFRAGE_LOESCHEN ):
      run_person_loeschen(av,prim_key_liste[index],auswahlliste[index])
      condition = False
    # Ändern -----------------------------
    elif( indexAbfrage == ABFRAGE_AENDERN ):
      run_person_aendern(av,prim_key_liste[index])
      condition = False
    #endif
    if( av.status != av.OKAY ):
      return
  #endwhile
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_akt_get_liste(av):
  """
  Übergibt Liste mit aktiben Personen (primkey und Name)
  [prim_key_liste,namen_liste] = run_person_akt_get_liste(av)
  """
  prim_key_liste = []
  namen_liste     = []

  # get primary ke name from table person
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
  if( av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("Fehler in dbh.get_tab_key_name: <%s>" % av.dbh.errText,1)
    av.status = av.NOT_OKAY
    return (prim_key_liste,namen_liste)
  #endif

  # get all data from table person from cell primary key and Materialname
  (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_PERSONEN,[primkeyname,av.CELL_NAME,av.CELL_AKTIVESTAT])
  for row in data_liste:
    # nur aktive auflisten
    if( row[2] == av.AKTIVESTAT_AKTIV ):
      prim_key_liste.append(row[0])
      namen_liste.append(row[1])
    #endif
  #endfor
  return (prim_key_liste,namen_liste)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_neu_auswahl(av):
  ABFRAGE_NEU       = 0
  ABFRAGE_AKTIVE    = 1
  ABFRAGE_BACK      = 2
  abfrage_liste = [u"neu"            \
                  ,u"aktiv machen"  \
                  ,u"zurück"         \
                  ]
  title = "Personen-Liste gesamt zum Überprüfen"
  condition = True
  while condition:

    [prim_key_liste,namen_liste,aktiv_liste] = run_person_gesamt_get_liste(av)

    n = len(prim_key_liste)
    #if( n == 0 ): return

    auswahlliste   = []
    for i in range(n):
      t = 'passiv'
      if( aktiv_liste[i]== av.AKTIVESTAT_AKTIV):
        t = 'aktiv'
      auswahlliste.append("("+str(prim_key_liste[i])+")"+namen_liste[i]+" <"+t+">")
    #endfor

    if( len(auswahlliste) == 0 ):
      auswahlliste = ["noch keine Person eingegeben"]
    #endif

    (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
    # Zurück -----------------------------
    if( indexAbfrage == ABFRAGE_BACK ):
      condition = False
    # Neu ------------------------------
    elif( indexAbfrage == ABFRAGE_NEU or (len(prim_key_liste)==0) ):
      run_person_neu(av)
      condition = False
    # Weiter
    elif( index < 0 ):
      pass
    # Aktive -----------------------------
    elif( indexAbfrage == ABFRAGE_AKTIVE ):

      if( is_personenkey_passiv(av,prim_key_liste[index]) ):
        run_person_make_status(av,prim_key_liste[index],namen_liste[index],h.secs_akt_time_epoch(),av.AKTIVESTAT_AKTIV)
      #endif
      condition = False
    #endif
    if( av.status != av.OKAY ):
      return
  #endwhile
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_gesamt_get_liste(av):
  """
  Übergibt Liste mit allen Personen (primkey und Name)
  [prim_key_liste,namen_liste] = run_person_gesamt_get_liste(av)
  """
  prim_key_liste = []
  namen_liste    = []
  aktiv_liste    = []

  # get primary ke name from table person
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
  if( av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("Fehler in dbh.get_tab_key_name: <%s>" % av.dbh.errText,1)
    av.status = av.NOT_OKAY
    return (prim_key_liste,namen_liste)
  #endif

  # get all data from table person from cell primary key and Materialname
  (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_PERSONEN,[primkeyname,av.CELL_NAME,av.CELL_AKTIVESTAT])
  for row in data_liste:
    prim_key_liste.append(row[0])
    namen_liste.append(row[1])
    aktiv_liste.append(row[2])
  #endfor
  return (prim_key_liste,namen_liste,aktiv_liste)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def is_personenkey_passiv(av,person_prim_key):
  """
  Überprüft ob Person passiv ist
  """
  [prim_key_liste,namen_liste] = run_person_akt_get_liste(av)

  if( person_prim_key in prim_key_liste ):
    flag = False
  else:
    flag = True
  #endif
  return flag
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_neu(av):
  """
  neues Material vorgeben
  """

  # Materialdaten eingeben
  listeAnzeige = [av.CELL_NAME \
                 ,av.CELL_ABTEILUNG \
                 ,av.CELL_TELEFON \
                 ]
  title         = 'Neues Person eingeben'
  listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title)
  if( len(listeErgebnis)==0): return

  Personenname               = h.elim_ae(listeErgebnis[0],' ')
  Personendatum              = h.secs_akt_time_epoch()
  Personenabteilung          = h.elim_ae(listeErgebnis[1],' ')
  Personentelefon            = h.elim_ae(listeErgebnis[2],' ')
  Personenstatus             = av.AKTIVESTAT_AKTIV

  run_person_neu_eintragen(av,Personenname,Personendatum,Personenabteilung,Personentelefon,Personenstatus)

  return

def run_person_neu_eintragen(av,Personenname,Personendatum,Personenabteilung,Personentelefon,Personenstatus):

  # Prüfen und in Datenbank speichern
  flag = av.dbh.is_item_in_cell_tab_data(av.TAB_PERSONEN,av.CELL_NAME,Personenname)
  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("Fehler in dbh.is_item_in_cell_tab_data: <%s>" % av.dbh.errText,1)
    av.status = av.NOT_OKAY
    return
  #endif

  # zwei Tabellen füllen:
  if( flag ):
    av.log.write_e("Der Personenname <%s> ist in der Tabelle schon entahlten" % Personenname,1)
  else:
    d = {}
    d[av.CELL_NAME]      = Personenname
    d[av.CELL_DATUM]     = Personendatum
    d[av.CELL_ABTEILUNG] = Personenabteilung
    d[av.CELL_TELEFON]   = Personentelefon
    d[av.CELL_AKTIVESTAT] = Personenstatus

    av.dbh.add_new_data_set(av.TAB_PERSONEN,d)

    d = {}
    d[av.CELL_DATUM]      = Personendatum
    primkey               = av.dbh.get_tab_primary_key(av.TAB_PERSONEN,av.CELL_NAME,Personenname)
    if( primkey == 0 ):
      av.log.write_e("PrimaryKey von <%s> konnte in Tablle <%s> nicht gefunden werden" % (Personenname,av.TAB_PERSONEN),1)
      av.status = av.NOT_OKAY
      return

    d[av.CELL_KEY1]       = primkey
    d[av.CELL_AKTIVESTAT] = Personenstatus
    av.dbh.add_new_data_set(av.TAB_PERSONENSTAT,d)

  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_aendern(av,prim_key):
  """
  Daten Person ändern
  """

  prim_key_name = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)

  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONEN,prim_key_name,prim_key)

  # Es sollte nur eine Row vorhanden sein
  data_liste = data_liste[0]

  listeAbfrage = []
  listeVorgabe = []
  for i in range(len(header_liste)):
    if(  (header_liste[i] == av.CELL_NAME) ):
      listeAbfrage.append(header_liste[i])
      listeVorgabe.append(data_liste[i])
    #endif
  #endfor
  for i in range(len(header_liste)):
    if(  (header_liste[i] == av.CELL_ABTEILUNG) ):
      listeAbfrage.append(header_liste[i])
      listeVorgabe.append(data_liste[i])
    #endif
  #endfor
  for i in range(len(header_liste)):
    if( (header_liste[i] == av.CELL_TELEFON) ):
      listeAbfrage.append(header_liste[i])
      listeVorgabe.append(data_liste[i])
    #endif
  #endfor
  if( len(listeVorgabe)!=3):
    av.log.write_e("Fehler run_person_aendern: keine drei Items/Zellen gefunden",1)
    av.status = av.NOT_OKAY
    return

  title         = 'Person ändern'
  listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAbfrage,vorgabe_liste=listeVorgabe,title=title)

  if( len(listeErgebnis)==0): return

  Personenname               = h.elim_ae(listeErgebnis[0],' ')
  Personenabteilung          = h.elim_ae(listeErgebnis[1],' ')
  Personentelefon            = listeErgebnis[2]

  d = {}
  d[av.CELL_NAME]      = Personenname
  d[av.CELL_ABTEILUNG] = Personenabteilung
  d[av.CELL_TELEFON]   = Personentelefon


  av.dbh.modify_data_by_primkey(av.TAB_PERSONEN,prim_key,d)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_make_status(av,prim_key,Personenname,Personendatum,Personenstatus):
  """
  Person passiv machen
  """
  # Prüfen ob in Datenbank vorhanden
  flag = av.dbh.is_item_in_cell_tab_data(av.TAB_PERSONEN,av.CELL_NAME,Personenname)
  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("Fehler in dbh.is_item_in_cell_tab_data: <%s>" % av.dbh.errText,1)
    av.status = av.NOT_OKAY
    return
  #endif

  # zwei Tabellen füllen:
  if( not flag ):
    av.log.write_e("Der Personenname <%s> ist in der Tabelle nicht entahlten" % Personenname,1)
  else:
    d = {}
    d[av.CELL_DATUM]      = Personendatum
    primkey               = av.dbh.get_tab_primary_key(av.TAB_PERSONEN,av.CELL_NAME,Personenname)
    if( primkey == 0 ):
      av.log.write_e("PrimaryKey von <%s> konnte in Tablle <%s> nicht gefunden werden" % (Personenname,av.TAB_PERSONEN),1)
      av.status = av.NOT_OKAY
      return

    d[av.CELL_KEY1]       = primkey
    d[av.CELL_AKTIVESTAT] = Personenstatus
    av.dbh.add_new_data_set(av.TAB_PERSONENSTAT,d)

    d = {}
    d[av.CELL_AKTIVESTAT] = Personenstatus

    av.dbh.modify_data_by_primkey(av.TAB_PERSONEN,primkey,d)

  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

def run_person_loeschen(av,prim_key,text):
  """
  Daten Person löschen
  """


  flag = sgui.abfrage_janein(text="Soll Person %s wirklich aus Liste gestrichen werden?" % text,title="loeschen")

  if( flag ):
    av.dbh.delete_data_by_primkey(av.TAB_PERSONEN,prim_key)
    if(av.dbh.status != av.dbh.OKAY ):
      av.log.write_e("Fehler in dbh.delete_data_by_primkey: <%s>" % av.dbh.errText,1)
      av.status = av.NOT_OKAY
      return
    #endif
  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_pas(av):
  ABFRAGE_AKTIV    = 0
  ABFRAGE_BACK     = 1
  abfrage_liste = [u"aktive machen"  \
                  ,u"zurück"         \
                  ]
  title = "Passiv-Personen-Liste"
  condition = True
  while condition:

    # get primary ke name from table person
    primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
    if( av.dbh.status != av.dbh.OKAY ):
      av.log.write_e("Fehler in dbh.get_tab_key_name: <%s>" % av.dbh.errText,1)
      av.status = av.NOT_OKAY
      return

    # get all data from table person from cell primary key and Materialname
    (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_PERSONEN,[primkeyname,av.CELL_NAME,av.CELL_AKTIVESTAT])
    auswahlliste   = []
    prim_key_liste = []
    name_liste     = []
    for row in data_liste:
      # nur aktive auflisten
      if( row[2] == av.AKTIVESTAT_PASSIV ):
        prim_key_liste.append(row[0])
        auswahlliste.append("("+str(row[0])+")"+row[1])
        name_liste.append(row[1])
    #endfor

    if(  len(auswahlliste) == 0 ):
      av.log.write_e("Keine passive Person eingetragen: ",1)
      return
    #endif

    (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
    # Zurück -----------------------------
    if( indexAbfrage == ABFRAGE_BACK ):
      condition = False
    # Weiter
    elif( index < 0 ):
      pass
    # Aktiv -----------------------------
    elif( indexAbfrage == ABFRAGE_AKTIV ):
      run_person_make_status(av,prim_key_liste[index],name_liste[index],h.secs_akt_time_epoch(),av.AKTIVESTAT_AKTIV)
      condition = False
    #endif
    if( av.status != av.OKAY ):
      return
  #endwhile
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_einzahlung(av):
  """
  Einzahlung einer Person eingeben
  """

  ABFRAGE_EINGABE   = 0
  ABFRAGE_BACK      = 1
  abfrage_liste = [u"eingeben"       \
                  ,u"zurück"         \
                  ]
  title = "Personen auswählen"
  condition = True
  while condition:

    [prim_key_liste,namen_liste] = run_person_akt_get_liste(av)

    n = len(prim_key_liste)
    if( n == 0 ): return

    auswahlliste   = []
    for i in range(n):
      auswahlliste.append("("+str(prim_key_liste[i])+")"+namen_liste[i])
    #endfor

    if( len(auswahlliste) == 0 ):
      av.log.write_warn("noch keine Person eingegeben",1)
      return
    #endif

    (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
    # Zurück -----------------------------
    if( indexAbfrage == ABFRAGE_BACK ):
      condition = False
    # Weiter
    elif( index < 0 ):
      pass
    # Eingabe ------------------------------
    elif( indexAbfrage == ABFRAGE_EINGABE ):
      run_person_einzahlung_eingabe(av,prim_key_liste[index],namen_liste[index])
      condition = False
    #endif
    if( av.status != av.OKAY ):
      return
  #endwhile

def run_person_einzahlung_eingabe(av,primkey,pname):
  """
  Eingabe der Einzahlung
  """

  # Eingabe der Einzahlung
  #-----------------------
  # Personendaten eingeben
  listeAnzeige = [av.CELL_DATUM                        \
                 ,av.CELL_WERT+" (€)"\
                 ]
  listeVorgabe = [h.str_akt_datum()                    \
                 ,"0.0"                                \
                 ]
  title         = 'Einzahlung von ('+str(primkey)+") "+ pname

  condition = True
  while   condition:
    listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,vorgabe_liste=listeVorgabe,title=title)

    if( len(listeErgebnis)==0): return

    flag = True
    if( h.datum_str_is_correct(listeErgebnis[0]) == False ):
      listeVorgabe[0] = h.str_akt_datum()
      flag = False
    #endif
    ValEuro = h.str_to_float_possible(h.change_max(listeErgebnis[1],',','.'))
    if( ValEuro == None ):
      listeVorgabe[1] = '0.0'
      flag = False
## auch negative Werte also Auszahlung zulassen
##    elif( ValEuro < 0.01 ):
##      listeVorgabe[1] = '0.0'
##      flag = False
    #endif

    if( flag ):
      condition = False
    #endif
  #endWhile


  Einzahldatum               =h.secs_time_epoch_from_str(listeErgebnis[0])
  EinzahlPersonenkey         = primkey
  Einzahlwert                = int(ValEuro*100)

  run_person_einzahlung_eintragen(av,Einzahldatum,EinzahlPersonenkey,Einzahlwert)

  return

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_einzahlung_eintragen(av,Einzahldatum,EinzahlPersonenkey,Einzahlwert):
    d = {}
    d[av.CELL_DATUM]     = int(Einzahldatum)
    d[av.CELL_KEY1]      = EinzahlPersonenkey
    d[av.CELL_WERT]      = Einzahlwert

    av.dbh.add_new_data_set(av.TAB_PERSONENEINZAHLUNG,d)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_verbrauch_eintragen(av,Personendatum,Personenkey,Materialkey,Personenmenge):
    d = {}
    d[av.CELL_DATUM]     = int(Personendatum)
    d[av.CELL_KEY1]      = Personenkey
    d[av.CELL_KEY2]      = Materialkey
    d[av.CELL_MENGE]     = Personenmenge

    av.dbh.add_new_data_set(av.TAB_PERSONENVERBRAUCH,d)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_auflisten(av):
  pass


