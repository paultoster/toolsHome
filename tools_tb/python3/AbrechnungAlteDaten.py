# -*- coding: utf8 -*-
#
#-------------------------------------------------------------------------------
# Name:        AbrechnungVerbrauchAlteDaten
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
import AbrechnungVerbrauchMaterial as avm

class person:
    def __init__(self,name):
        self.version       = 1
        self.name          = name
        self.anzahl_tassen = 0
        self.einzahlung    = 0
class buchung:
    def __init__(self):
        self.version   = 1
        self.datum     = 0
        self.ppt       = 0
        self.person    = []
        self.e_kosten  = 0     # Einkauf
        self.e_comment = 0
        self.e_menge   = 0
        self.b_wert    = 0     # Bestand
        self.b_comment = 0
        self.b_menge   = 0
class kasse:
    def __init__(self,name):
        self.version               = 1
        self.name                  = name
        self.akt_ppt               = 0;
        self.liste_akt_pers        = []
        self.liste_pas_pers        = []
        self.buchung               = []

# Hilfsfunktionen
import hfkt as h
import hfkt_def as hdef
import sgui

Dateiname = "D:\\tools_beispiele\\python\\_entw\\AbrechnungVerbrauch\\kkasse.txt"
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
def run_alte_daten(av):

  flag_akt_personen      = 1
  flag_pas_personen      = 1

  flag_material_einkauf  = 1
  flag_material_bestand  = 1
  flag_person_einzahlung = 1

  flag_abrechnung        = 1

  f = file(Dateiname,'r')
  lines = f.readlines()
  f.close()
  for line in lines:
    exec line

  # Material eintragen
  avm.run_material_neu_eintragen(av,u'Kaffee',u'kg',u'Tassen')

  if( flag_akt_personen ):
    print "Abrechnung Alte Daten flag_akt_personen"
    # Schleife über akt Personen
    for name in kk.liste_akt_pers:

      # nach dem älstesten Datum suchen
      int_datum = h.int_akt_datum()
      for b in kk.buchung:
        for p in b.person:
          if( (p.name == name) and (b.datum < int_datum) ):
            int_datum = b.datum
          #endif
        #endfor
      #endfor
      Personenname      = name
      Personendatum     = h.secs_time_epoch_from_int(int_datum,12)
      Personenabteilung = ""
      Personentelefon   = ""
      Personenstatus    = av.AKTIVESTAT_AKTIV

      avp.run_person_neu_eintragen(av,Personenname,Personendatum,Personenabteilung,Personentelefon,Personenstatus)
    #endfor
  #endif

  if( flag_pas_personen ):
    print "Abrechnung Alte Daten flag_pas_personen"
    # Schleife über passive Personen
    for name in kk.liste_pas_pers:

      # nach dem älstesten Datum suchen
      int_datum = h.int_akt_datum()
      for b in kk.buchung:
        for p in b.person:
          if( (p.name == name) and (b.datum < int_datum) ):
            int_datum = b.datum
          #endif
        #endfor
      #endfor
      Personenname      = name
      Personendatum     = h.secs_time_epoch_from_int(int_datum,12)
      Personenabteilung = ""
      Personentelefon   = ""
      Personenstatus    = av.AKTIVESTAT_AKTIV

      avp.run_person_neu_eintragen(av,Personenname,Personendatum,Personenabteilung,Personentelefon,Personenstatus)
    #endfor
  #endif

  #Schleife über Buchungen für index_liste aufsteigend Datum
  index_liste = []
  for j in range(len(kk.buchung)):
    int_datum   = h.int_akt_datum()
    index = j
    for i in range(len(kk.buchung)):
      b = kk.buchung[i]
      if( (b.datum < int_datum) and (i not in index_liste) ):
        index = i
        int_datum = b.datum
      #endif
    #endfor
    index_liste.append(index)
  #endfor

  if( flag_material_einkauf ):
    print "Abrechnung Alte Daten flag_material_einkauf"
    for i in index_liste:
      b = kk.buchung[i]
      Einkaufdatum = h.secs_time_epoch_from_int(b.datum,12)
      primkeyname  = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
      (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIAL,[primkeyname])
      Einkaufkey                = data_liste[0][0]
      Einkaufmenge              = float(b.e_menge)/1000.
      Einkaufwert               = b.e_kosten
      Einkaufherkunft           = b.e_comment

      avm.run_material_einkauf_eintragen(av,Einkaufdatum,Einkaufkey,Einkaufmenge,Einkaufwert,Einkaufherkunft)
    #endfor
  #endif

  if( flag_material_bestand ):
    print "Abrechnung Alte Daten flag_material_bestand"
    for i in index_liste:
      b = kk.buchung[i]
      Bestanddatum = h.secs_time_epoch_from_int(b.datum,12)
      primkeyname  = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
      (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIAL,[primkeyname])
      Bestandkey                = data_liste[0][0]
      Bestandmenge              = float(b.b_menge)/1000.
      Bestandwert               = b.b_wert

      avm.run_material_bestand_eintragen(av,Bestanddatum,Bestandkey,Bestandmenge,Bestandwert)
    #endfor
  #endif

  if( flag_person_einzahlung ):
    print "Abrechnung Alte Daten flag_person_einzahlung"
    for i in index_liste:
      b = kk.buchung[i]
      for p in b.person:
        # Wenn etwas eingezahlt wurde, eintragen
        if( p.einzahlung > 0 ):
          primkey = av.dbh.get_tab_primary_key_with_value(av.TAB_PERSONEN,av.CELL_NAME,p.name)
          if( len(primkey) > 0 ):

            Personenkey       = primkey[0]
            Personendatum     = h.secs_time_epoch_from_int(b.datum,12)
            Personenwert      = p.einzahlung

            avp.run_person_einzahlung_eintragen(av,Personendatum,Personenkey,Personenwert)
          #endif
        #endif
      #endfor
    #endfor
  #endif


  if( flag_abrechnung ):
    print "Abrechnung Alte Daten flag_abrechnung"
    for i in index_liste:
      b = kk.buchung[i]
      Abrechnungdatum = h.secs_time_epoch_from_int(b.datum,12)
      primkeyname  = av.dbh.get_tab_primary_key_name(av.TAB_MATERIAL)
      (header_liste,data_liste) = av.dbh.get_tab_data(av.TAB_MATERIAL,[primkeyname])
      Abrechnungkey                = data_liste[0][0]

      avm.run_material_abrechnung_eintragen(av,Abrechnungdatum,Abrechnungkey)

      Abrechnungspreis = b.ppt
      avm.run_material_abrechnungspreis_eintragen(av,Abrechnungdatum,Abrechnungkey,Abrechnungspreis)

      for p in b.person:
        # Wenn etwas eingezahlt wurde, eintragen
        primkey = av.dbh.get_tab_primary_key_with_value(av.TAB_PERSONEN,av.CELL_NAME,p.name)
        if( len(primkey) > 0 ):

          Personenkey       = primkey[0]
          Personendatum     = h.secs_time_epoch_from_int(b.datum,12)
          Personenmenge     = p.anzahl_tassen

          avp.run_person_verbrauch_eintragen(av,Personendatum,Personenkey,Abrechnungkey,Personenmenge)

        #endif
      #endfor

    #endfor

    # Schleife über passive Personen
    for name in kk.liste_pas_pers:

      primkey = av.dbh.get_tab_primary_key_with_value(av.TAB_PERSONEN,av.CELL_NAME,name)
      if( len(primkey) > 0 ):
        Personenname      = name
        Personenkey       = primkey[0]
        # nach dem jüngsten Datum suchen
        int_datum = 20000101
        for b in kk.buchung:
          for p in b.person:
              if( (p.name == name) and (b.datum > int_datum) ):
                int_datum = b.datum
              #endif
          #endfor
        #endfor
        Personendatum     = h.secs_time_epoch_from_int(int_datum,12)+6*60*60
        Personenstatus    = av.AKTIVESTAT_PASSIV

        avp.run_person_make_status(av,Personenkey,Personenname,Personendatum,Personenstatus)
      #endif
    #endfor
  #endif

