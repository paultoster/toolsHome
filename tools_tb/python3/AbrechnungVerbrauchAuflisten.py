# -*- coding: utf8 -*-
#
#-------------------------------------------------------------------------------
# Name:        AbrechnungVerbrauchAusflisten
# Purpose:
#
# Author:      tom
#
# Created:     21.11.2016
# Copyright:   (c) tom 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------
import sys, os
import webbrowser

tools_path = "D:\\tools_tb\\python" 

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

# Hilfsfunktionen
import hfkt as h
import hfkt_def as hdef
import sgui

class person:
  def __init__(self,name,datum,abteilung,telefon,status):
    self.name          = name
    self.startdatum    = datum
    self.abteilung     = abteilung
    self.telefon       = telefon
    self.status        = status
    self.liste         = [] # Liste mit [datum,text,-kosten/+einzahl,anzahlverbauchteeinheit,summe]
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_person(av,primkey,pname):

  # Personen daten
  p = run_abrechung_auflisten_person_get_person(av,primkey)

  filename       = run_abrechung_auflisten_person_html(av,p)

  browsefilename = h.html_get_filename_for_browser(filename)

  webbrowser.open_new_tab(browsefilename)
  return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def getKey(item):
  return item[0]
def run_abrechung_auflisten_person_get_person(av,primkey):
  """
  erstellt Struktur mit den Daten der Person
  """
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONEN,primkeyname,primkey)

  Pname      = ""
  Pdatum     = 0
  Pabteilung = ""
  Ptelefon   = ""
  Paktivstat = av.AKTIVESTAT_PASSIV

  data_liste = data_liste[0]
  for i in range(len(header_liste)):
    if(  (header_liste[i] == av.CELL_NAME) ):
      Pname = data_liste[i]
    elif(  (header_liste[i] == av.CELL_DATUM) ):
      Pdatum = data_liste[i]
    elif(  (header_liste[i] == av.CELL_ABTEILUNG) ):
      Pabteilung = data_liste[i]
    elif(  (header_liste[i] == av.CELL_TELEFON) ):
      Ptelefon = data_liste[i]
    elif(  (header_liste[i] == av.CELL_AKTIVESTAT) ):
      Paktivstat = data_liste[i]
    #endif
  #endfor

  p = person(Pname,Pdatum,Pabteilung,Ptelefon,Paktivstat)

  # Verbrauchsliste
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONENVERBRAUCH,av.CELL_KEY1,primkey,[av.CELL_DATUM,av.CELL_KEY2,av.CELL_MENGE])


  for row in data_liste:
    preisproeinheit                                   = get_preis_pro_tasse_am_datam(av,row[0],row[1])
    [Materialname,Einkaufseinheit,Abrechnungseinheit] = get_material_abr_einheit(av,row[1])
    # Bilde Liste mit [datum,text,-kosten]
    tt            = "%i %s %s Preis %i Cent" % (row[2],Abrechnungseinheit,Materialname,preisproeinheit)
    liste         = [row[0],tt,preisproeinheit*row[2]*(-1),row[2],0]
    p.liste.append(liste)
  #endfor

  # Einzahlliste
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONENEINZAHLUNG,av.CELL_KEY1,primkey,[av.CELL_DATUM,av.CELL_WERT])
  for row in data_liste:
    # Bilde Liste mit [datum,text,+einzahl]
    tt            = "Einzahlung"
    liste         = [row[0],tt,row[1],-1,0]
    p.liste.append(liste)
  #endfor

  p.liste = sorted(p.liste,key=getKey)

  summe_saldo_cent = 0
  for i in range(len(p.liste)):
    summe_saldo_cent += p.liste[i][2]
    p.liste[i][4] = summe_saldo_cent
  #endfor

  # Liste rückwaerts
  liste = []
  for i in range(len(p.liste)-1,-1,-1):
    liste.append(p.liste[i])
  #endfor
  p.liste = liste

  return p
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_person_html(av,p):
  """
  filename = run_abrechung_auflisten_person_html(p)
  """
  file_name = "Einzelabrechnung_"+h.change_max(p.name,' ','_')+"_"+str(h.int_akt_datum())+".htm"

   
  with open(file_name,'w') as f:

    cwd = os.getcwd()
    file_name = h.join(cwd,f.name)
    #start
    h.html_write_start(f,"Kaffekasse TZS Einzelabrechnung")

    # Text
    h.html_write_Ueberschrift(f,"Kaffekasse TZS Einzelabrechnung")
    h.html_write_text(f,"Name      : %s\n" % p.name)
    h.html_write_text(f,"Datum     : %s\n" % h.secs_time_epoch_to_str(p.startdatum))
    h.html_write_text(f,"Abteilung : %s\n" % p.abteilung)
    h.html_write_text(f,"Telefon   : %s\n" % p.telefon)
    h.html_write_text(f,"Status    : %s\n" % ("aktiv" if p.status == av.AKTIVESTAT_AKTIV else "passiv"))

    # Tabelle
    tt = u"Einzelabrechnung %s" % p.name
    h.html_write_start_tab(f,tt)
    h.html_write_start_colgroup(f)

    h.html_write_set_col_align(f,"left")

    for i in range(4):
        h.html_write_set_col_align(f,"right")

    h.html_write_end_colgroup(f)

    h.html_write_start_tab_zeile(f)
    # Überschrift 1. Zeile ------------------------------------
    h.html_write_tab_zelle(f,1,0,"black",p.name)
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_end_tab_zeile(f)

    # Überschrift 2. Zeile ------------------------------------
    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,1,0,"black","Datum")
    h.html_write_tab_zelle(f,1,0,"black","Text")
    h.html_write_tab_zelle(f,1,0,"black","Kosten/Einzahl")
    h.html_write_tab_zelle(f,1,0,"black","Saldo")
    h.html_write_end_tab_zeile(f)

    for liste in p.liste: # [datum,text,kost/ein]

      h.html_write_start_tab_zeile(f)
      # Datum                File, header, bold, colour, string
      h.html_write_tab_zelle(f,0,1,"black",h.secs_time_epoch_to_str(liste[0]))
      # Text                 File, header, bold, colour, string
      h.html_write_tab_zelle(f,0,0,"black",liste[1])
      # Kosten/Einnahme      File, header, bold, colour, string
      str_euro = h.string_cent_in_euro(liste[2])
      if( liste[2] < 0 ): farbe = "red"
      else:              farbe = "black"
      h.html_write_tab_zelle(f,0,0,farbe,str_euro)
      # Saldo
      str_euro = h.string_cent_in_euro(liste[4])
      if( liste[4] < 0 ): farbe = "red"
      else:               farbe = "black"
      h.html_write_tab_zelle(f,0,0,farbe,str_euro)
      h.html_write_end_tab_zeile(f)
    #endfor


    #ende
    h.html_write_end(f)
  #endwith

  return file_name
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def get_preis_pro_tasse_am_datam(av,secs,material_key):
  """
  Sucht preis  pro Tasse aus Tabelle TAB_MATERIALABRPREIS
  Rückgabe: (preisprotasse,matreialname)
  """
  preis_pro_tasse_cent = 0

  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIALABRPREIS,av.CELL_KEY1,material_key)
  (header_liste,data_liste) = av.dbh.sort_data_liste_with_cellname(header_liste,data_liste,av.CELL_DATUM)

  datas = av.dbh.get_colums_from_data_liste(header_liste,data_liste,[av.CELL_DATUM,av.CELL_ABRPREIS])

  if( len(datas) < 2 ):
    av.log.write_e("In Tabelle <%s> sind keine Zellen mit (%s,%s) enthalten" % (av.TAB_MATERIALABRPREIS,av.CELL_DATUM,av.CELL_ABRPREIS) ,1)
    return preis_pro_tasse_cent
  #endif
  data_datum = datas[0]
  data_abrpreis = datas[1]

  #Schleife über das Datum rückwaerts
  for i in range(len(data_datum)-1,-1,-1):
    if( secs >= data_datum[i] ):
      preis_pro_tasse_cent = data_abrpreis[i]
      break
    #endif
  #endfor

  return preis_pro_tasse_cent
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def get_material_abr_einheit(av,material_key):
  """
  Sucht [Materialname,Einkaufseinheit,Abrechnungseinheit] in Materiallsite
  """
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,primkeyname,material_key)


  if( len(data_liste) == 0 ):
    av.log.write_e("In Tabelle <%s> ist der primkey (%i) nicht enthalten" % (av.TAB_MATERIAL,material_key) ,1)
    return ("","","")
  #endif

  datas = av.dbh.get_rows_from_data_liste(header_liste,data_liste,[av.CELL_MATERIALNAME,av.CELL_MATERIALUNIT,av.CELL_ABRECHUNGSUNIT])

  return datas[0]

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_saldo_kasse(av):
  """
  Auflisten Kasse
  """
  # Kasse
  liste          = run_abrechung_auflisten_saldo_kasse_auflistung(av)

  filename       = run_abrechung_auflisten_kasse_html(av,liste)

  browsefilename = h.html_get_filename_for_browser(filename)

  webbrowser.open_new_tab(browsefilename)
  return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_saldo_kasse_auflistung(av):
  """
  Liste mit Einzahlung und EInkauf erstellen
  """
  # Einzahlliste
  (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_PERSONENEINZAHLUNG,[av.CELL_DATUM,av.CELL_KEY1,av.CELL_WERT])

  # liste für [datum,text,+/-wert] anlegen
  kliste = []

  # primkeyname in TAB_PERSONEN
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)

  # Schleife über alle Einzahlungen:
  for row in data_liste:
    datum = row[0]
    key1  = row[1]
    wert  = row[2]

    # Personenname
    (header_liste1,data_liste1) = av.dbh.get_tab_data_with_value(av.TAB_PERSONEN,primkeyname,key1,[av.CELL_NAME])

    # Text mit Personenname
    pname = data_liste1[0][0]
    tt    = "Einzahlung: %s" % pname

    # liste füllen
    kliste.append([datum,tt,wert,0])
  #endfor

  # Materialeinkauf
  (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIALEINKAUF,[av.CELL_DATUM,av.CELL_KEY1,av.CELL_MENGE,av.CELL_WERT,av.CELL_HERKUNFT])

  # primkeyname in TAB_MATERIAL
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)

  # Schleife über alle Einkäufe:
  for row in data_liste:
    datum = row[0]
    key1  = row[1]
    menge = row[2]
    wert  = row[3]
    herk  = row[4]

    # Material
    (header_liste1,data_liste1) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,primkeyname,key1,[av.CELL_MATERIALNAME,av.CELL_MATERIALUNIT])

    # Text mit Materialname
    mname = data_liste1[0][0]
    munit = data_liste1[0][1]
    tt    = "Einkauf von %s: %f %s (%s)" % (mname,menge,munit,herk)
    kliste.append([datum,tt,wert*(-1.),0])
  #endfor

  kliste = sorted(kliste,key=getKey)

  summe_saldo_cent = 0
  for i in range(len(kliste)):
    summe_saldo_cent += kliste[i][2]
    kliste[i][3]      =  summe_saldo_cent
  #endfor

  # Liste rückwaerts
  liste = []
  for i in range(len(kliste)-1,-1,-1):
    liste.append(kliste[i])

  return liste
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_kasse_html(av,kliste):
  """
  filename = run_abrechung_auflisten_kasse_html(liste)
  """
  file_name = "Kasse_"+str(h.int_akt_datum())+".htm"

  with open(file_name,'w') as f:
    cwd = os.getcwd()
    file_name = h.join(cwd,f.name)

    #start
    h.html_write_start(f,"Kaffekasse TZS Kasse")

    # Text
    h.html_write_Ueberschrift(f,"Kaffekasse TZS Saldo Kasse")

    # Tabelle
    tt = u"Saldo Kasse"
    h.html_write_start_tab(f,tt)
    h.html_write_start_colgroup(f)

    h.html_write_set_col_align(f,"left")

    for i in range(3):
      h.html_write_set_col_align(f,"right")

    h.html_write_end_colgroup(f)

    h.html_write_start_tab_zeile(f)
    
    # Überschrift 1. Zeile ------------------------------------
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_end_tab_zeile(f)

    # Überschrift 2. Zeile ------------------------------------
    h.html_write_start_tab_zeile(f)
  
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,1,0,"black","Datum")
    h.html_write_tab_zelle(f,1,0,"black","Text")
    h.html_write_tab_zelle(f,1,0,"black","Einkauf/Einzahl")
    h.html_write_tab_zelle(f,1,0,"black","Saldo")
    h.html_write_end_tab_zeile(f)

    for liste in kliste: # [datum,text,kost/ein]

      h.html_write_start_tab_zeile(f)
      # Datum                File, header, bold, colour, string
      h.html_write_tab_zelle(f,0,1,"black",h.secs_time_epoch_to_str(liste[0]))
      # Text                 File, header, bold, colour, string
      h.html_write_tab_zelle(f,0,0,"black",liste[1])
      # Kosten/Einnahme      File, header, bold, colour, string
      str_euro = h.string_cent_in_euro(liste[2])
      if( liste[2] < 0 ): farbe = "red"
      else:               farbe = "black"
      h.html_write_tab_zelle(f,0,0,farbe,str_euro)
      # Saldo
      str_euro = h.string_cent_in_euro(liste[3])
      if( liste[3] < 0 ): farbe = "red"
      else:               farbe = "black"
      h.html_write_tab_zelle(f,0,0,farbe,str_euro)
      h.html_write_end_tab_zeile(f)
    #endfor


    #ende
    h.html_write_end(f)
  #endwith

  return file_name
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
class verbrauch_abr:
  def __init__(self,start_datum_secs,abr_datum_secs,preisproabreinheit):
    self.abr_datum_secs         = abr_datum_secs
    self.start_datum_secs       = start_datum_secs
    self.preisproabreinheit     = preisproabreinheit
    self.verbrauch_person_liste = []
    self.verbrauch_person_gesamt= None
    self.saldo_passiv           = 0                # Summe aller Passiven Personnen
    self.einzahlung_passiv      = 0
    self.verbrauch_material     = None
    self.bilanz_kasse           = 0
    self.bilanz_theor           = 0
class verbrauch_person:
  def __init__(self,name,saldo_start,saldo_abr,einzahlung,verbrauch,n_verbrauch,einzahlung_gesamt):
    self.name               = name
    self.saldo_start        = saldo_start
    self.saldo_abr          = saldo_abr
    self.einzahlung         = einzahlung
    self.einzahlung_gesamt  = einzahlung_gesamt
    self.verbrauch          = verbrauch
    self.n_verbrauch        = n_verbrauch
class verbrauch_material:
  def __init__(self,materialname,materialeinkaufseinheit,matrialabreinheit,einkauf_menge,einkauf_wert \
              ,bestand_menge,bestand_wert,verbrauch_menge,verbrauch_wert,saldo_einkauf_start,saldo_einkauf_abr,einkauf_gesamt_wert):
    self.name                   = materialname
    self.einkaufseinheit        = materialeinkaufseinheit
    self.abreinheit             = matrialabreinheit
    self.einkauf_menge          = einkauf_menge
    self.einkauf_wert           = einkauf_wert
    self.bestand_menge          = bestand_menge
    self.bestand_wert           = bestand_wert
    self.verbrauch_menge        = verbrauch_menge
    self.verbrauch_wert         = verbrauch_wert
    self.saldo_einkauf_start    = saldo_einkauf_start
    self.saldo_einkauf_abr      = saldo_einkauf_abr
    self.einkauf_gesamt_wert    = einkauf_gesamt_wert
def run_abrechung_auflisten_verbrauch(av,materialprimkey,start_zeit_secs,abrechnungs_zeit_secs):
  """
  """

  # Abrechnungsdaten
  v = run_abrechung_auflisten_get_verbrauch(av,materialprimkey,start_zeit_secs,abrechnungs_zeit_secs)

  if( not v ):
    return
  #endif

  filename       = run_abrechung_auflisten_verbrauch_html(av,v)

  browsefilename = h.html_get_filename_for_browser(filename)

  webbrowser.open_new_tab(browsefilename)

  return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_get_verbrauch(av,materialprimkey,start_zeit_secs,abrechnungs_zeit_secs):
  """
  Füllen der Abrechnungsdaten für  den Zeitraum
  """

  # Material:
  [materialname,materialeinkaufseinheit,matrialabreinheit] = get_material_abr_einheit(av,materialprimkey)

  if( len(materialname) == 0 ):
    return None
  #endif

  # Preis pro EInheit
  preisproabreinheit                                  = get_preis_pro_tasse_am_datam(av,abrechnungs_zeit_secs,materialprimkey)

  # Verbrauchsabrechnung anlegen
  v = verbrauch_abr(start_zeit_secs,abrechnungs_zeit_secs,preisproabreinheit)

  # über alle Personen den Verbrauch und EInzahlung sammeln
  #--------------------------------------------------------
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
  (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_PERSONEN,[primkeyname,av.CELL_NAME])

  saldo_abr       = 0
  saldo_bis_start = 0
  einzahlung      = 0
  verbrauch       = 0
  n_verbrauch     = 0
  einzahlung_g    = 0

  for data_row in data_liste:
    personkey  = data_row[0]
    personname = data_row[1]

    if( personkey == 76 ):
      a = 0

    vp = run_abrechung_auflisten_get_verbrauch_person(av,materialprimkey,start_zeit_secs,abrechnungs_zeit_secs,personkey,personname)
    # Berechnung aktive Person
    if( person_is_aktiv_at_datum(av,personkey,abrechnungs_zeit_secs) ):
      v.verbrauch_person_liste.append(vp)
      saldo_abr       += vp.saldo_abr
      saldo_bis_start += vp.saldo_start
      einzahlung      += vp.einzahlung
      verbrauch       += vp.verbrauch
      n_verbrauch     += vp.n_verbrauch

    # Aufsummieren nicht aktive Person
    else:
      v.saldo_passiv += vp.saldo_abr
    #endif
    einzahlung_g    += vp.einzahlung_gesamt

  #endfor
  saldo_abr += v.saldo_passiv
  v.verbrauch_person_gesamt = verbrauch_person("Gesamt",saldo_bis_start,saldo_abr,einzahlung,verbrauch,n_verbrauch,einzahlung_g)

  # Einkauf, Bestand, Verbrauch
  #--------------------------------------------------------
  vm = run_abrechung_auflisten_get_verbrauch_material(av,materialprimkey,materialname,materialeinkaufseinheit,matrialabreinheit,start_zeit_secs,abrechnungs_zeit_secs)
  v.verbrauch_material = vm

  # reale Bilanz in Kasse
  v.bilanz_kasse = v.verbrauch_person_gesamt.einzahlung_gesamt - v.verbrauch_material.einkauf_gesamt_wert

  # theoretische Bilanz
  v.bilanz_theor = v.bilanz_kasse - v.verbrauch_person_gesamt.saldo_abr + v.verbrauch_material.bestand_wert

  return v
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def person_is_aktiv_at_datum(av,personkey,abrechnungs_zeit_secs):
  """
  Überprüfen, ob Person aktiv ist im Zeitraum

  return True/False
  """
  # Liste des Personen status
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONENSTAT,av.CELL_KEY1,personkey,[av.CELL_DATUM,av.CELL_AKTIVESTAT])

  datas = av.dbh.get_colums_from_data_liste(header_liste,data_liste,[av.CELL_DATUM,av.CELL_AKTIVESTAT])

  if( len(datas) < 2 ):
    av.log.write_e("In Tabelle <%s> sind keine Zellen mit (%s,%s) enthalten" % (av.TAB_PERSONENSTAT,av.CELL_DATUM,av.CELL_AKTIVESTAT) ,1)
    return False
  #endif
  data_datum    = datas[0]
  data_stat     = datas[1]

  stat = None
  #Schleife über das Datum rückwaerts
  for i in range(len(data_datum)-1,-1,-1):
    if( abrechnungs_zeit_secs >= data_datum[i] ):
      stat = data_stat[i]
      break
    #endif
  #endfor

  if( stat == None ):
    stat = data_stat[0]
  #endif

  if( stat == av.AKTIVESTAT_AKTIV ):
    return True
  else:
    return False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_get_verbrauch_person(av,materialprimkey,start_zeit_secs,abrechnungs_zeit_secs,personkey,personname):
  """
  Abrechnungsdaten der Person für den Zeitraum
  """
  saldo_bis_start  = 0
  saldo_abr        = 0
  einzahlung       = 0
  n_verbrauch      = 0
  verbrauch        = 0
  einzahlung_g     = 0

  # Verbrauch und Einzahlung nach datum sortiert
  p = run_abrechung_auflisten_person_get_person(av,personkey)

  for items in p.liste:
    # datum,text,-kosten/+einzahl,anzahlverbauchteeinheit
    datum_secs = items[0]
    wert       = items[2]
    anzahl     = items[3]

    # Aufsummieren bis Startzeit einschliesslich
    if( datum_secs <= start_zeit_secs ):
      saldo_bis_start += wert
      if( anzahl == -1 ):
        einzahlung_g += wert
    # Abrechnung im Zeitraum
    elif( datum_secs <= abrechnungs_zeit_secs ):
      if( anzahl == -1 ): # Einzahlung
        einzahlung += wert
        einzahlung_g += wert
      else:
        verbrauch   += wert
        n_verbrauch += anzahl
      #endif
    else:
      pass
    #endif
  #endfor
  saldo_abr = saldo_bis_start + verbrauch + einzahlung

  vp = verbrauch_person(p.name,saldo_bis_start,saldo_abr,einzahlung,verbrauch,n_verbrauch,einzahlung_g)

  return vp

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_get_verbrauch_material(av,materialprimkey,materialname,materialeinkaufseinheit,matrialabreinheit,start_zeit_secs,abr_zeit_secs):
  """
  vm = run_abrechung_auflisten_get_verbrauch_material(av,materialprimkey,materialname,materialeinkaufseinheit,matrialabreinheit,start_zeit_secs,abrechnungs_zeit_secs)
  """
  # materialname,matrialabreinheit,
  # einkauf_menge,einkauf_wert,bestand_menge,bestand_wert,verbrauch_menge,verbrauch_wert,saldo_einkauf_start,saldo_einkauf_abr
  einkauf_menge          = 0
  einkauf_wert           = 0
  bestand_menge          = 0
  bestand_wert           = 0
  verbrauch_menge        = 0
  verbrauch_wert         = 0
  saldo_einkauf_start    = 0
  saldo_einkauf_abr      = 0

  letzte_menge           = 0
  letzte_wert            = 0

  einkauf_gesamt_wert    = 0

  # Liste mit Einkauf und Bestand nach Datum sortiert
  mliste = run_abrechung_auflisten_material(av,materialprimkey)

  for items in mliste:
    # datum,menge,+einkauf/+bestand,flag Bestand
    datum_secs = items[0]
    menge      = items[1]
    wert       = items[2]
    flag       = items[3]

    # Aufsummieren bis Startzeit einschliesslich
    if( datum_secs <= start_zeit_secs ):
      if( flag ): # Bestand
        letzte_menge = menge
        letzte_wert  = wert
      else:       # Einkauf
        saldo_einkauf_start += wert
        einkauf_gesamt_wert += wert
        pass
      #endif
    # Abrechnung im Zeitraum
    elif( datum_secs <= abr_zeit_secs ):
      if( flag ): # Bestand
        bestand_menge = menge
        bestand_wert  = wert
      else:       # Einkauf
        einkauf_menge += menge
        einkauf_wert  += wert
        einkauf_gesamt_wert += wert
      #endif
    else:
      pass
    #endif
  #endfor
  verbrauch_menge = letzte_menge + einkauf_menge - bestand_menge
  verbrauch_wert  = letzte_wert  + einkauf_wert  - bestand_wert

  saldo_einkauf_start -= letzte_wert
  saldo_einkauf_abr    = saldo_einkauf_start + verbrauch_wert

  vm = verbrauch_material(materialname,materialeinkaufseinheit,matrialabreinheit  \
                         ,einkauf_menge,einkauf_wert                              \
                         ,bestand_menge,bestand_wert                              \
                         ,verbrauch_menge,verbrauch_wert                          \
                         ,saldo_einkauf_start,saldo_einkauf_abr,einkauf_gesamt_wert)

  return vm
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_material(av,materialprimkey):
  """
    mliste = run_abrechung_auflisten_material(av,materialprimkey)
    mliste = [[datum,menge,wert,bestand_flag],[datum,menge,wert,einkauf_flag],...]
  """
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)

  # Materialeinkauf
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIALEINKAUF,av.CELL_KEY1,materialprimkey,[av.CELL_DATUM,av.CELL_MENGE,av.CELL_WERT])
  mliste = []
  for row in data_liste:
    datum = row[0]
    menge = row[1]
    wert  = row[2]
    mliste.append([datum,menge,wert,False])
  #endfor

  # Materialbestand
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIALBESTAND,av.CELL_KEY1,materialprimkey,[av.CELL_DATUM,av.CELL_MENGE,av.CELL_WERT])
  for row in data_liste:
    datum = row[0]
    menge = row[1]
    wert  = row[2]
    mliste.append([datum,menge,wert,True])
  #endfor

  mliste = sorted(mliste,key=getKey)
  return mliste
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_abrechung_auflisten_verbrauch_html(av,v):
  """
  filename = run_abrechung_auflisten_verbrauch_html(v)
  """
  file_name = "Gesamtabrechnung_"+str(h.secs_time_epoch_to_int(v.abr_datum_secs))+".htm"

  with open(file_name,'w') as f:
    cwd = os.getcwd()
    file_name = h.join(cwd,f.name)
    #=============================================================================
    #=============================================================================
    h.html_write_start(f,"Kaffekasse TZS")
    h.html_write_start_tab(f,"Kaffeekassenabrechnung ")
    h.html_write_start_colgroup(f)

    h.html_write_set_col_align(f,"left")

    for i in range(1,7,1):
      h.html_write_set_col_align(f,"right")

    h.html_write_end_colgroup(f)

    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,1,0,"black","Kaffekasse")
    h.html_write_tab_zelle(f,1,0,"black","Abrechnung\n"+str(h.secs_time_epoch_to_str(v.abr_datum_secs)))
    h.html_write_tab_zelle(f,1,0,"black","letzte\n"+str(h.secs_time_epoch_to_str(v.start_datum_secs)))
    h.html_write_tab_zelle(f,1,0,"black","")
    h.html_write_tab_zelle(f,1,0,"black","Preis pro %s:" % v.verbrauch_material.abreinheit)
    h.html_write_tab_zelle(f,1,0,"black",h.string_cent_in_euro(v.preisproabreinheit))
    h.html_write_end_tab_zeile(f)

    # Überschrift 2. Zeile ------------------------------------
    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,1,0,"black","Name")
    h.html_write_tab_zelle(f,1,0,"black","Saldo")
    h.html_write_tab_zelle(f,1,0,"black","Saldo\nStart")
    h.html_write_tab_zelle(f,1,0,"black","Einzahlung")
    h.html_write_tab_zelle(f,1,0,"black","Verbrauch")
    h.html_write_tab_zelle(f,1,0,"black","Anzahl %s:" % v.verbrauch_material.abreinheit)
    h.html_write_end_tab_zeile(f)

    # Personen ------------------------------------------------
    for vp in v.verbrauch_person_liste:

        h.html_write_start_tab_zeile(f)
        # Name File, header, bold, colour, string
        h.html_write_tab_zelle(f,0,1,"black",vp.name)

        cents = vp.saldo_abr
        str_euro = h.string_cent_in_euro(cents)
        if( cents < 0 ): farbe = "red"
        else:            farbe = "black"
        # Saldo File, header, bold, colour, string
        h.html_write_tab_zelle(f,0,0,farbe,str_euro)

        cents = vp.saldo_start
        str_euro = h.string_cent_in_euro(cents)
        if( cents < 0 ): farbe = "red"
        else:            farbe = "black"
        # Saldo Start File, header, bold, colour, string
        h.html_write_tab_zelle(f,0,0,farbe,str_euro)

        cents = vp.einzahlung
        str_euro = h.string_cent_in_euro(cents)
        if( cents < 0 ): farbe = "red"
        else:            farbe = "black"
        # Einzahlung File, header, bold, colour, string
        h.html_write_tab_zelle(f,0,0,farbe,str_euro)

        cents = vp.verbrauch
        str_euro = h.string_cent_in_euro(cents)
        if( cents < 0 ): farbe = "red"
        else:            farbe = "black"
        # Verbrauch File, header, bold, colour, string
        h.html_write_tab_zelle(f,0,0,farbe,str_euro)

        # Anzahl File, header, bold, colour, string
        h.html_write_tab_zelle(f,0,0,"black",str(int(vp.n_verbrauch)))

        h.html_write_end_tab_zeile(f)

    # Restwert -------------------------------------------------------------------
    h.html_write_start_tab_zeile(f)

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Rest")

    cents = v.saldo_passiv
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Saldo File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)


    for i in range(0,4,1)    :
        h.html_write_tab_zelle(f,0,0,"black","")

    h.html_write_end_tab_zeile(f)


    # Gesamt ------------------------------------------
    h.html_write_leer_zeile(f,6)

    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Gesamtsumme:")

    cents = v.verbrauch_person_gesamt.saldo_abr
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Saldo, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    cents = v.verbrauch_person_gesamt.saldo_start
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Saldo start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    cents = v.verbrauch_person_gesamt.einzahlung
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:             farbe = "black"
    # Einzahlung, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    cents = v.verbrauch_person_gesamt.verbrauch
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Verbrauch, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    # n-Verbrauch, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black",str(int(v.verbrauch_person_gesamt.n_verbrauch)))

    h.html_write_end_tab_zeile(f)

    # Einkauf ------------------------------------------
    ##    self.name                   = materialname
    ##    self.einkaufseinheit
    ##    self.abreinheit             = matrialabreinheit
    ##    self.einkauf_menge          = einkauf_menge
    ##    self.einkauf_wert           = einkauf_wert
    ##    self.bestand_menge          = bestand_menge
    ##    self.bestand_wert           = bestand_wert
    ##    self.verbrauch_menge        = verbrauch_menge
    ##    self.verbrauch_wert         = verbrauch_wert
    ##    self.saldo_einkauf_start    = saldo_einkauf_start
    ##    self.saldo_einkauf_abr      = saldo_einkauf_abr
    h.html_write_leer_zeile(f,6)

    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","%s:" % v.verbrauch_material.name )

    ##  cents = v.verbrauch_material.saldo_einkauf_abr
    ##  str_euro = h.string_cent_in_euro(cents)
    ##  if( cents < 0 ): farbe = "red"
    ##  else:            farbe = "black"
    ##  # Saldo, header, bold, colour, string
    ##  h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    ##  cents = v.verbrauch_material.saldo_einkauf_start
    ##  str_euro = h.string_cent_in_euro(cents)
    ##  if( cents < 0 ): farbe = "red"
    ##  else:            farbe = "black"
    ##  # Saldo Start, header, bold, colour, string
    ##  h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Einkauf:")

    cents = v.verbrauch_material.einkauf_wert * (-1)
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Saldo Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    menge = "%5.2f %s" % (v.verbrauch_material.einkauf_menge,v.verbrauch_material.einkaufseinheit)
    # Menge Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black",menge)

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","" )

    # Saldo, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black","")

    h.html_write_end_tab_zeile(f)

    # 2. Zeile
    h.html_write_start_tab_zeile(f)

    # Saldo Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black","")

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Verbrauch:")

    cents = v.verbrauch_material.verbrauch_wert * (-1)
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Saldo Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    menge = "%5.2f %s" % (v.verbrauch_material.verbrauch_menge,v.verbrauch_material.einkaufseinheit)
    # Menge Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black",menge)

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","%s pro %s" % (v.verbrauch_material.abreinheit,v.verbrauch_material.einkaufseinheit))
    if( v.verbrauch_material.verbrauch_menge > 0.01 ):
      menge = "%5.2f" % (v.verbrauch_person_gesamt.n_verbrauch/v.verbrauch_material.verbrauch_menge)
    else:
      menge = "inf"
    # Saldo, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black",menge)

    h.html_write_end_tab_zeile(f)

    # 3. Zeile
    h.html_write_start_tab_zeile(f)

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","" )

    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Bestand:")

    cents = v.verbrauch_material.bestand_wert
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    # Saldo Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    menge = "%5.2f %s" % (v.verbrauch_material.bestand_menge,v.verbrauch_material.einkaufseinheit)
    # Menge Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black",menge)


    # Saldo, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black","")

    # Saldo Start, header, bold, colour, string
    h.html_write_tab_zelle(f,0,0,"black","")

    h.html_write_end_tab_zeile(f)

    # Saldo in Kasse ------------------------------------------


    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Bilanz Kasse:")

    cents = v.bilanz_kasse
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    for i  in range(1,5):
        h.html_write_tab_zelle(f,0,0,"black","")

    h.html_write_end_tab_zeile(f)
    # theor. Saldo in Kasse ------------------------------------------

    h.html_write_start_tab_zeile(f)
    # File, header, bold, colour, string
    h.html_write_tab_zelle(f,0,1,"black","Bilanz theor:")

    cents = v.bilanz_theor
    str_euro = h.string_cent_in_euro(cents)
    if( cents < 0 ): farbe = "red"
    else:            farbe = "black"
    h.html_write_tab_zelle(f,0,0,farbe,str_euro)

    for i  in range(1,5):
        h.html_write_tab_zelle(f,0,0,"black","")

    h.html_write_end_tab_zeile(f)

    ##  # gesamt verfügbar ------------------------------------------
    ##
    ##  h.html_write_start_tab_zeile(f)
    ##  # File, header, bold, colour, string
    ##  h.html_write_tab_zelle(f,0,1,"black","Gesamt verfügbar:\nSaldo - Summe")
    ##
    ##  cents = self.g_insgesamt
    ##  str_euro = h.string_cent_in_euro(cents)
    ##  if( cents < 0 ): farbe = "red"
    ##  else:              farbe = "black"
    ##  h.html_write_tab_zelle(f,0,0,farbe,str_euro)
    ##
    ##  h.html_write_tab_zelle(f,0,0,"black","")
    ##  h.html_write_tab_zelle(f,0,0,"black","")
    ##  h.html_write_tab_zelle(f,0,1,"black","Kaffeverbrauch")
    ##  str_kg = h.string_gramm_in_kg(self.last_b_menge+self.akt_k_menge-self.akt_b_menge)
    ##  h.html_write_tab_zelle(f,0,0,"black",str_kg)
    ##  h.html_write_tab_zelle(f,0,0,"black","")
    ##
    ##  h.html_write_end_tab_zeile(f)
    ##
    ##  # Kaffeebestand ------------------------------------------
    ##
    ##  h.html_write_start_tab_zeile(f)
    ##  # File, header, bold, colour, string
    ##  h.html_write_tab_zelle(f,0,1,"black","Kaffeebestand:\naktuell")
    ##
    ##  cents = self.b_wert
    ##  str_euro = h.string_cent_in_euro(cents)
    ##  if( cents < 0 ): farbe = "red"
    ##  else:              farbe = "black"
    ##  h.html_write_tab_zelle(f,0,0,farbe,str_euro)
    ##
    ##  h.html_write_tab_zelle(f,0,0,"black","")
    ##  h.html_write_tab_zelle(f,0,0,"black","")
    ##  h.html_write_tab_zelle(f,0,1,"black","Tassen pro kg")
    ##  a = float(self.last_b_menge+self.akt_k_menge-self.akt_b_menge)
    ##  if( a != 0.0 ):
    ##    str_t = "%.2f" % (float(self.akt_tassen)/a*1000.)
    ##  else:
    ##    str_t = ""
    ##  h.html_write_tab_zelle(f,0,0,"black",str_t)
    ##  h.html_write_tab_zelle(f,0,0,"black","")
    ##
    ##  h.html_write_end_tab_zeile(f)
    ##
    ##  # virtueller Gesamtwert ------------------------------------------
    ##
    ##  h.html_write_start_tab_zeile(f)
    ##  # File, header, bold, colour, string
    ##  h.html_write_tab_zelle(f,0,1,"black","Gesamtwert:\nverfügbar+Bestand")
    ##
    ##  cents = self.g_virtuell
    ##  str_euro = h.string_cent_in_euro(cents)
    ##  if( cents < 0 ): farbe = "red"
    ##  else:              farbe = "black"
    ##  h.html_write_tab_zelle(f,0,0,farbe,str_euro)
    ##
    ##  for i  in range(1,6,1):
    ##      h.html_write_tab_zelle(f,0,0,"black","")
    ##
    ##  h.html_write_end_tab_zeile(f)
    # Tabellenende ------------------------------------------
    h.html_write_end_tab(f)
    h.html_write_end(f)

    #=============================================================================
    #=============================================================================
  #endwith

  return file_name
