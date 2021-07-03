# -*- coding: utf8 -*-
#
#-------------------------------------------------------------------------------
# Name:        AbrechnungVerbrauchMaterial
# Purpose:
#
# Author:      tom
#
# Created:     21.11.2016
# Copyright:   (c) tom 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------
import sys, os

tools_path = "D:\\tools_tb\\python" 

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

import AbrechnungVerbrauchPerson as avp

# Hilfsfunktionen
import hfkt as h
import hfkt_def as hdef
import sgui

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material(av):
  ABFRAGE_AENDERN   = 0
  ABFRAGE_NEU       = 1
  ABFRAGE_LOESCHEN  = 2
  ABFRAGE_BACK      = 3
  abfrage_liste = [u"ändern" \
                  ,u"neu" \
                  ,u"löschen" \
                  ,u"zurück" \
                  ]
  title = "Material-Liste"
  condition = True
  while condition:

    # get primary ke name from table material
    primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
    if( av.dbh.status != av.dbh.OKAY ):
      av.log.write_e("Fehler in dbh.get_tab_key_name: <%s>" % av.dbh.errText,1)
      av.status = av.NOT_OKAY
      return

    # get all data from table matrial from cell primary key and Materialname
    (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIAL,[primkeyname,av.CELL_MATERIALNAME])
    auswahlliste = []
    prim_key_liste = []
    for row in data_liste:
      prim_key_liste.append(row[0])
      auswahlliste.append("("+str(row[0])+")"+row[1])
    #endfor

    if(  len(auswahlliste) == 0 ):
      auswahlliste = ["noch kein Material eingegeben"]
    #endif

    (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
    # Zurück -----------------------------
    if( indexAbfrage == ABFRAGE_BACK ):
      condition = False
    # Neu ------------------------------
    elif( indexAbfrage == ABFRAGE_NEU or (len(prim_key_liste)==0) ):
      run_material_neu(av)
      condition = False
    # Weiter
    elif( index < 0 ):
      pass
    # Löschen -----------------------------
    elif( indexAbfrage == ABFRAGE_LOESCHEN ):
      run_material_loeschen(av,prim_key_liste[index],auswahlliste[index])
      condition = False
    # Ändern -----------------------------
    elif( indexAbfrage == ABFRAGE_AENDERN ):
      run_material_aendern(av,primkeyname,prim_key_liste[index])
      condition = False
    #endif
    if( av.status != av.OKAY ):
      return
  #endwhile
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_neu(av):
  """
  neues Material vorgeben
  """

  # Materialdaten eingeben
  listeAnzeige = [av.CELL_MATERIALNAME \
                 ,av.CELL_MATERIALUNIT \
                 ,av.CELL_ABRECHUNGSUNIT \
                 ]
  title         = 'Neues Material vorgeben'
  listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title)

  if( len(listeErgebnis)==0): return

  Materialname               = listeErgebnis[0]
  Materialmesseinheit        = listeErgebnis[1]
  Materialabrechnungseinheit = listeErgebnis[2]

  run_material_neu_eintragen(av,Materialname,Materialmesseinheit,Materialabrechnungseinheit)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_neu_eintragen(av,Materialname,Materialmesseinheit,Materialabrechnungseinheit):

  # Prüfen und in Datenbank speichern
  flag = av.dbh.is_item_in_cell_tab_data(av.TAB_MATERIAL,av.CELL_MATERIALNAME,Materialname)
  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("Fehler in dbh.is_item_in_cell_tab_data: <%s>" % av.dbh.errText,1)
    av.status = av.NOT_OKAY
    return
  #endif
  if( flag ):
    av.log.write_e("Der Materialname <%s> ist in der Tabelle schon entahlten" % Materialname,1)
  else:
    d = {}
    d[av.CELL_MATERIALNAME]   = Materialname
    d[av.CELL_MATERIALUNIT]   = Materialmesseinheit
    d[av.CELL_ABRECHUNGSUNIT] = Materialabrechnungseinheit

    av.dbh.add_new_data_set(av.TAB_MATERIAL,d)
  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_aendern(av,prim_key_name,prim_key):
  """
  Daten Material ändern
  """

  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,prim_key_name,prim_key)

  # Es sollte nur eine Row vorhanden sein
  data_liste = data_liste[0]

  listeAbfrage = []
  listeVorgabe = []
  for i in range(len(header_liste)):
    if(  (header_liste[i] == av.CELL_MATERIALNAME) ):
      listeAbfrage.append(header_liste[i])
      listeVorgabe.append(data_liste[i])
    #endif
  #endfor
  for i in range(len(header_liste)):
    if(  (header_liste[i] == av.CELL_MATERIALUNIT) ):
      listeAbfrage.append(header_liste[i])
      listeVorgabe.append(data_liste[i])
    #endif
  #endfor
  for i in range(len(header_liste)):
    if( (header_liste[i] == av.CELL_ABRECHUNGSUNIT) ):
      listeAbfrage.append(header_liste[i])
      listeVorgabe.append(data_liste[i])
    #endif
  #endfor
  if( len(listeVorgabe)!=3):
    av.log.write_e("Fehler run_material_aendern: keine drei Items/Zellen gefunden",1)
    av.status = av.NOT_OKAY
    return

  title         = 'Material ändern'
  listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAbfrage,vorgabe_liste=listeVorgabe,title=title)

  if( len(listeErgebnis)==0): return

  Materialname               = listeErgebnis[0]
  Materialmesseinheit        = listeErgebnis[1]
  Materialabrechnungseinheit = listeErgebnis[2]

  d = {}
  d[av.CELL_MATERIALNAME]   = Materialname
  d[av.CELL_MATERIALUNIT]   = Materialmesseinheit
  d[av.CELL_ABRECHUNGSUNIT] = Materialabrechnungseinheit

  av.dbh.modify_data_by_primkey(av.TAB_MATERIAL,prim_key,d)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_loeschen(av,prim_key,text):
  """
  Daten Material löschen
  """


  # flag = h.abfrage_ok_box(text="Soll Material %s wirklich aus Liste gestrichen werden?" % text)
  flag = sgui.abfrage_janein(text="Soll Material %s wirklich aus Liste gestrichen werden?" % text,title="loeschen")

  if( flag ):
    av.dbh.delete_data_by_primkey(av.TAB_MATERIAL,prim_key)
    if(av.dbh.status != av.dbh.OKAY ):
      av.log.write_e("Fehler in dbh.delete_data_by_primkey: <%s>" % av.dbh.errText,1)
      av.status = av.NOT_OKAY
      return
    #endif
  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_einkauf(av):

  # primkey des Materials festlegen
  #--------------------------------
  [materialprimkey,materialname,materialunit,materialabreinheit] = run_auswahl_material(av)

  # Eingabe der Einkauf
  #--------------------
  # Materialdaten eingeben
  listeAnzeige = [av.CELL_DATUM                        \
                 ,av.CELL_MENGE+" ("+materialunit+")"  \
                 ,av.CELL_WERT+" (€)"                  \
                 ,av.CELL_HERKUNFT                     \
                 ]
  listeVorgabe = [h.str_akt_datum()                    \
                 ,"0"                                  \
                 ,"0.0"                                \
                 ,""                                   \
                 ]
  title         = 'Einkauf von '+ materialname
  condition = True
  while   condition:
    listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,vorgabe_liste=listeVorgabe,title=title)

    if( len(listeErgebnis)==0): return

    flag = True
    if( h.datum_str_is_correct(listeErgebnis[0]) == False ):
      listeVorgabe[0] = h.str_akt_datum()
      flag = False
    #endif
    ValEuro = h.str_to_float_possible(h.change_max(listeErgebnis[2],',','.'))
    if( ValEuro == None ):
      listeVorgabe[2] = '0.0'
      flag = False
    elif( ValEuro < 0.01 ):
      listeVorgabe[2] = '0.0'
      flag = False
    #endif

    if( flag ):
      condition = False
    #endif
  #endWhile


    if( len(listeErgebnis)==0): return

    if( h.datum_str_is_correct(listeErgebnis[0]) == True ):
      condition = False
    #endif
  #endWhile



  Einkaufdatum               = h.secs_time_epoch_from_str(listeErgebnis[0])
  Einkaufkey                 = materialprimkey
  Einkaufmenge               = float(listeErgebnis[1])
  if( h.such(listeErgebnis[2],",") > -1 ):
    Einkaufwert                = h.string_euro_in_int_cent(listeErgebnis[2],delim=",")
  else:
    Einkaufwert                = h.string_euro_in_int_cent(listeErgebnis[2],delim=".")
  Einkaufherkunft              = listeErgebnis[3]

  #Eingabe asuführen
  run_material_einkauf_eintragen(av,Einkaufdatum,Einkaufkey,Einkaufmenge,Einkaufwert,Einkaufherkunft)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_auswahl_material(av):
  """
  Auswahl des Materials je nach ANzahl
  [materialprimkey,materialname,materialunit,materialabreinheit] = run_auswahl_material(av)
  """
  materialprimkey    = 0
  materialname       = ""
  materialunit       = ""
  materialabreinheit = ""

  nrows = av.dbh.get_num_of_items(av.TAB_MATERIAL)
  # Keine Auswahl, wenn nur ein MAterial
  if( nrows == 0 ):
    av.log.write_e("Fehler kein Material in Material Liste",1)
    return (materialprimkey,materialname,materialunit,materialabreinheit)
  elif( nrows == 1 ):
    primkeyname               = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
    (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIAL,[primkeyname,av.CELL_MATERIALNAME,av.CELL_MATERIALUNIT,av.CELL_ABRECHUNGSUNIT])
    materialprimkey           = data_liste[0][0]
    materialname              = data_liste[0][1]
    materialunit              = data_liste[0][2]
    materialabreinheit        = data_liste[0][3]
  #Auswahl
  else:
    ABFRAGE_AUSWAHL   = 0
    ABFRAGE_BACK      = 1
    abfrage_liste = [u"auswahl" \
                    ,u"zurück" \
                    ]
    title = "Material-Liste"
    condition = True
    while condition:

      # get primary ke name from table material
      primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
      if( av.dbh.status != av.dbh.OKAY ):
        av.log.write_e("Fehler in dbh.get_tab_key_name: <%s>" % av.dbh.errText,1)
        av.status = av.NOT_OKAY
        return

      # get all data from table matrial from cell primary key and Materialname
      (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIAL,[primkeyname,av.CELL_MATERIALNAME,av.CELL_MATERIALUNIT,av.CELL_ABRECHUNGSUNIT])
      auswahlliste        = []
      prim_key_liste      = []
      material_u_liste    = []
      material_a_liste    = []
      material_name_liste = []
      for row in data_liste:
        prim_key_liste.append(row[0])
        auswahlliste.append("("+str(row[0])+")"+row[1])
        material_u_liste.append(row[2])
        material_a_liste.append(row[3])
        material_name_liste.append(row[1])
      #endfor

      (index,indexAbfrage) = sgui.abfrage_liste_index_abfrage_index(auswahlliste,abfrage_liste,title)
      # Zurück -----------------------------
      if( indexAbfrage == ABFRAGE_BACK ):
        condition = False
      # Weiter
      elif( index < 0 ):
        pass
      # Auswahl -----------------------------
      elif( indexAbfrage == ABFRAGE_AUSWAHL ):
        materialprimkey    = prim_key_liste[index]
        materialunit       = material_u_liste[index]
        materialabreinheit = material_a_liste[index]
        materialname       = material_name_liste[index]
        condition          = False
      #endif
      if( av.status != av.OKAY ):
        return (materialprimkey,materialname,materialunit,materialabreinheit)
    #endwhile
  #endif
  return (materialprimkey,materialname,materialunit,materialabreinheit)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_einkauf_eintragen(av,Einkaufdatum,Einkaufkey,Einkaufmenge,Einkaufwert,Einkaufherkunft):
  """
  Führt EInfügen in Tabelle durch
  """
  d = {}
  d[av.CELL_DATUM]    = int(Einkaufdatum)
  d[av.CELL_KEY1]     = Einkaufkey
  d[av.CELL_MENGE]    = Einkaufmenge
  d[av.CELL_WERT]     = Einkaufwert
  d[av.CELL_HERKUNFT] = Einkaufherkunft

  av.dbh.add_new_data_set(av.TAB_MATERIALEINKAUF,d)

  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("av.dbh.add_new_data_set(av.TAB_MATERIALEINKAUF,d)",1)
    av.status = av.NOT_OKAY
    return
  #endif

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_bestand_eintragen(av,Bestanddatum,Bestandkey,Bestandmenge,Bestandwert):
  """
  Führt EInfügen in Tabelle durch
  """
  d = {}
  d[av.CELL_DATUM]    = int(Bestanddatum)
  d[av.CELL_KEY1]     = Bestandkey
  d[av.CELL_MENGE]    = Bestandmenge
  d[av.CELL_WERT]     = Bestandwert

  av.dbh.add_new_data_set(av.TAB_MATERIALBESTAND,d)

  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("av.dbh.add_new_data_set(av.TAB_MATERIALBESTAND,d)",1)
    av.status = av.NOT_OKAY
    return
  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_abrechungspreis_eintragen(av,Materialkey,Abrpreis):
  """
  Einfügen von neuen Abrechenpreis in Tabelle
  """
  d = {}
  d[av.CELL_DATUM]    = h.secs_akt_time_epoch()
  d[av.CELL_KEY1]     = Materialkey
  d[av.CELL_ABRPREIS] = Abrpreis

  av.dbh.add_new_data_set(av.TAB_MATERIALABRPREIS,d)

  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("av.dbh.add_new_data_set(av.TAB_MATERIALABRPREIS,d)",1)
    av.status = av.NOT_OKAY
    return
  #endif
  return
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_abrechnung_eintragen(av,Abrechnungdatum,Abrechnungkey):
  """
  Führt EInfügen in Tabelle durch
  """
  d = {}
  d[av.CELL_DATUM]    = int(Abrechnungdatum)
  d[av.CELL_KEY1]     = Abrechnungkey

  av.dbh.add_new_data_set(av.TAB_ABRECHNUNG,d)

  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("av.dbh.add_new_data_set(av.TAB_ABRECHNUNG,d)",1)
    av.status = av.NOT_OKAY
    return
  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_abrechnungspreis_eintragen(av,Abrechnungdatum,Abrechnungkey,Abrechnungspreis):
  """
  Führt EInfügen in Tabelle durch
  """
  d = {}
  d[av.CELL_DATUM]    = int(Abrechnungdatum)
  d[av.CELL_KEY1]     = Abrechnungkey
  d[av.CELL_ABRPREIS] = Abrechnungspreis

  av.dbh.add_new_data_set(av.TAB_MATERIALABRPREIS,d)

  if(av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("av.dbh.add_new_data_set(av.TAB_MATERIALABRPREIS,d)",1)
    av.status = av.NOT_OKAY
    return
  #endif
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def get_material_abrechnungspreis_aktuell(av,materialkey):
  """
  Abrechnungspreis = get_material_abrechnungspreis_aktuell(av,materialkey)
  [in Cent]
  """
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIALABRPREIS,av.CELL_KEY1,materialkey,[av.CELL_ABRPREIS])

  if( av.dbh.status != av.dbh.OKAY ):
    av.log.write_e("get_material_abrechnungspreis_aktuell: kein Material gefunden <%s>" % av.dbh.errText,1)
    av.status = av.NOT_OKAY
    return 0

  return data_liste[-1][0]

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_auflisten(av):
  pass
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_material_preisabr(av,materialprimkey):
  """
  Preisabfrage für Material mit primkey
  """

  # Name Material:
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,primkeyname,materialprimkey,[av.CELL_MATERIALNAME,av.CELL_ABRECHUNGSUNIT])
  materialname = data_liste[0][0]
  abreinheit   = data_liste[0][1]

  # aktueller Preis
  abrpreis = get_material_abrechnungspreis_aktuell(av,materialprimkey)


  # Eingabe des Preises
  #--------------------
  listeAnzeige = ["neuer Preis für "+str(abreinheit)+" : Cent pro Einheit"  \
                 ]
  listeVorgabe = [str(abrpreis)]
  title         = 'Preis von '+ materialname + '(bisher %s Cent)' % abrpreis

  condition = True
  while condition:

    listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title,vorgabe_liste=listeVorgabe)

    if( len(listeErgebnis)==0): return

    ival  = h.str_to_int_possible(listeErgebnis[0])
    if( ival != None ):
      condition = False
      Abrdatum  = h.secs_akt_time_epoch()
      Abrpreis  = ival
    #endif
  #while

  #Eingabe asuführen
  run_material_abrechnungspreis_eintragen(av,Abrdatum,materialprimkey,Abrpreis)

  return

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_gesamtabrechnung(av,materialprimkey):
  """
  Gesamtabrechnung für ein Material und allen aktiven Personen
  """

  flag_old = False
  # Name Material:
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,primkeyname,materialprimkey,[av.CELL_MATERIALNAME,av.CELL_ABRECHUNGSUNIT])
  materialname = data_liste[0][0]
  abreinheit   = data_liste[0][1]
  # aktueller Preis
  abrpreis = get_material_abrechnungspreis_aktuell(av,materialprimkey)

  # Namen Personen:
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONEN,av.CELL_AKTIVESTAT,av.AKTIVESTAT_AKTIV,[av.CELL_NAME,primkeyname])
  datas                     = av.dbh.get_colums_from_data_liste(header_liste,data_liste,[av.CELL_NAME,primkeyname])
  person_name_liste         = datas[0]
  person_primkey_liste      = datas[1]

  n                         = len(person_name_liste)
  Abrdatum                  = h.secs_akt_time_epoch()

  # Eingabe des Anzahl der einheiten
  #---------------------------------
  title         = 'Abrechnung von '+ materialname + '(%s Cent/%s) ' % (abrpreis,abreinheit) + h.secs_time_epoch_to_str(Abrdatum)

  if( flag_old ):
    # für alle Personen abfragen
    person_abr_liste = []
    for i in range(n):
      listeAnzeige     = [person_name_liste[i] + " :"]
      listeVorgabe     = [0]

      person_abr_liste.append(0)

      condition = True
      while condition:

        listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title,vorgabe_liste=listeVorgabe)

        if( len(listeErgebnis)==0): return

        ival  = h.str_to_int_possible(listeErgebnis[0])
        if( ival != None ):
          person_abr_liste[i] = ival
          condition = False
      #endwhile
    #endfor

    # Abrechnung eintragen
    run_material_abrechnung_eintragen(av,Abrdatum,materialprimkey)

    # für alle Personen eintragen
    for i in range(n):

      #Eingabe eintragen
      avp.run_person_verbrauch_eintragen(av,Abrdatum,person_primkey_liste[i],materialprimkey,person_abr_liste[i])
    #endfor

  else:
    listeAnzeige     = []
    listeVorgabe     = []
    person_abr_liste = []
    for pname in person_name_liste:
      listeAnzeige.append(pname + " :")
      listeVorgabe.append(0)
      person_abr_liste.append(0)
    #endfor
    condition = True
    while condition:

      listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title,vorgabe_liste=listeVorgabe)

      if( len(listeErgebnis)==0): return

      ncount = 0
      for i in range(len(listeErgebnis)):
        ival  = h.str_to_int_possible(listeErgebnis[i])
        if( ival != None ):
          ncount += 1
          person_abr_liste[i] = ival
        #endif
      #endfor

      if( ncount == n ):
        condition = False
      #endif
    #while

    # Abrechnung eintragen
    run_material_abrechnung_eintragen(av,Abrdatum,materialprimkey)

    # für alle Personen eintragen
    for i in range(n):

      #Eingabe eintragen
      avp.run_person_verbrauch_eintragen(av,Abrdatum,person_primkey_liste[i],materialprimkey,person_abr_liste[i])
    #endfor
  #endif

  return
#enddef

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_person_einzelabrechnung(av,materialprimkey):
  """
  Einzelabrechnung Eingabe Striche
  """

  flag_old = False
  # Name Material:
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,primkeyname,materialprimkey,[av.CELL_MATERIALNAME,av.CELL_ABRECHUNGSUNIT])
  materialname = data_liste[0][0]
  abreinheit   = data_liste[0][1]
  # aktueller Preis
  abrpreis = get_material_abrechnungspreis_aktuell(av,materialprimkey)

  # Namen Personen:
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_PERSONEN)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_PERSONEN,av.CELL_AKTIVESTAT,av.AKTIVESTAT_AKTIV,[av.CELL_NAME,primkeyname])
  datas                     = av.dbh.get_colums_from_data_liste(header_liste,data_liste,[av.CELL_NAME,primkeyname])
  person_name_liste         = datas[0]
  person_primkey_liste      = datas[1]

  n                         = len(person_name_liste)
  Abrdatum                  = h.secs_akt_time_epoch()

  ABFRAGE_EINGABE   = 0
  ABFRAGE_BACK      = 1
  abfrage_liste = [u"eingeben"       \
                  ,u"zurück"         \
                  ]
  title = "Personen auswählen"
  prim_key_act = 0
  namen_act    = ''
  condition = True
  while condition:

    n = len(person_primkey_liste)
    if( n == 0 ): return

    auswahlliste   = []
    for i in range(n):
      auswahlliste.append("("+str(person_primkey_liste[i])+")"+person_name_liste[i])
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
      prim_key_act = person_primkey_liste[index]
      namen_act    = person_name_liste[index]
      condition = False
    #endif
    if( av.status != av.OKAY ):
      return
  #endwhile

  if( len(namen_act) == 0 ):
    return

  person_name_liste    = [namen_act]
  person_primkey_liste = [prim_key_act]
  n                 = len(person_name_liste)
  # Eingabe des Anzahl der einheiten
  #---------------------------------
  title         = 'Abrechnung von '+ materialname + '(%s Cent/%s) ' % (abrpreis,abreinheit) + h.secs_time_epoch_to_str(Abrdatum)

  if( flag_old ):
    # für alle Personen abfragen
    person_abr_liste = []
    for i in range(n):
      listeAnzeige     = [person_name_liste[i] + " :"]
      listeVorgabe     = [0]

      person_abr_liste.append(0)

      condition = True
      while condition:

        listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title,vorgabe_liste=listeVorgabe)

        if( len(listeErgebnis)==0): return

        ival  = h.str_to_int_possible(listeErgebnis[0])
        if( ival != None ):
          person_abr_liste[i] = ival
          condition = False
      #endwhile
    #endfor

    # Abrechnung eintragen
    run_material_abrechnung_eintragen(av,Abrdatum,materialprimkey)

    # für alle Personen eintragen
    for i in range(n):

      #Eingabe eintragen
      avp.run_person_verbrauch_eintragen(av,Abrdatum,person_primkey_liste[i],materialprimkey,person_abr_liste[i])
    #endfor

  else:
    listeAnzeige     = []
    listeVorgabe     = []
    person_abr_liste = []
    for pname in person_name_liste:
      listeAnzeige.append(pname + " :")
      listeVorgabe.append(0)
      person_abr_liste.append(0)
    #endfor
    condition = True
    while condition:

      listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,title=title,vorgabe_liste=listeVorgabe)

      if( len(listeErgebnis)==0): return

      ncount = 0
      for i in range(len(listeErgebnis)):
        ival  = h.str_to_int_possible(listeErgebnis[i])
        if( ival != None ):
          ncount += 1
          person_abr_liste[i] = ival
        #endif
      #endfor

      if( ncount == n ):
        condition = False
      #endif
    #while

    # Abrechnung eintragen
    # run_material_abrechnung_eintragen(av,Abrdatum,materialprimkey)

    # für alle Personen eintragen
    for i in range(n):

      #Eingabe eintragen
      avp.run_person_verbrauch_eintragen(av,Abrdatum,person_primkey_liste[i],materialprimkey,person_abr_liste[i])
    #endfor
  #endif

  return
#enddef
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_bestand_eingabe(av,materialprimkey):
  """
  Bestand eingeben
  """
  # Name Material:
  primkeyname = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
  (header_liste,data_liste) = av.dbh.get_tab_data_with_value(av.TAB_MATERIAL,primkeyname,materialprimkey,[av.CELL_MATERIALNAME,av.CELL_MATERIALUNIT])
  materialname   = data_liste[0][0]
  materialunit   = data_liste[0][1]

  listeAnzeige = [av.CELL_DATUM                        \
                 ,av.CELL_MENGE+" ("+materialunit+")"  \
                 ,av.CELL_WERT+" (€)"                  \
                 ]
  listeVorgabe = [h.str_akt_datum()                    \
                 ,"0"                                  \
                 ,"0.0"                                \
                 ,""                                   \
                 ]
  title         = 'Bestand von '+ materialname
  condition = True
  while   condition:
    listeErgebnis = sgui.abfrage_n_eingabezeilen(liste=listeAnzeige,vorgabe_liste=listeVorgabe,title=title)

    if( len(listeErgebnis)==0): return

    flag = True
    if( h.datum_str_is_correct(listeErgebnis[0]) == False ):
      listeVorgabe[0] = h.str_akt_datum()
      flag = False
      av.log.write_e("Eingegebenes Datum falsches Format TT.MM.JJJJ <%s>" % listeErgebnis[0],1)
    #endif

    Menge = h.str_to_int_possible(listeErgebnis[1])
    if( Menge == None ):
      listeVorgabe[1] = '0'
      flag = False
    #endif

    ValEuro = h.str_to_float_possible(h.change_max(listeErgebnis[2],',','.'))
    if( ValEuro == None ):
      listeVorgabe[2] = '0.0'
      flag = False
    #endif

    if( flag ):
      condition = False
    #endif

    if( len(listeErgebnis)==0): return

  #endWhile



  Bestanddatum               = h.secs_time_epoch_from_str(listeErgebnis[0])
  Bestandkey                 = materialprimkey
  Bestandmenge               = Menge
  if( h.such(listeErgebnis[2],",") > -1 ):
    Betsandwert                = h.string_euro_in_int_cent(listeErgebnis[2],delim=",")
  else:
    Bestandwert                = h.string_euro_in_int_cent(listeErgebnis[2],delim=".")

  #Eingabe asuführen
  run_material_bestand_eintragen(av,Bestanddatum,Bestandkey,Bestandmenge,Bestandwert)


