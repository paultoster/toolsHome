# -*- coding: utf8 -*-
#
# 14.02.10 file_split hinzugefügt
# 11.03.10 read_csv_file
# 17.04.10 read_csv_file_header_data, write_csv_file_header_data
# hilfsfkt Hilfsfunktionen 1.0
#############################
#Stringbearbeitung
##################
# get_str_from_int(intval,intwidth) gibt string mit vorangestellten nullen aus
# such(text,muster,regel) Sucht muster im text return index or -1
# such_alle(t,muster) Sucht nach Muster und erstellt Index-Liste, die leer sein kann
# such_in_liste(liste,muster,regel) Suche nach dem string-Muster in der Liste nach der Regel
# get_index_quot(text,quot0,quot1) Gibt Indexpaare in dem der Text gequotet ist
# get_index_quoted(text,quot0,quot1) das gleiche
# get_index_no_quot(text,quot0,quot1) Gibt Indexpaare in dem der Text nicht gequotet ist
# get_index_not_quoted(text,quot0,quot1) das gleiche
# get_string_quoted(text,quot0,quot1) Gibt Liste mit string in dem der gequotet Text steht
# get_string_not_quoted(text,quot0,quot1) Gibt Liste mit string in dem der nicht gequotet Text steht
# elim_a(text,muster) Eliminiert text am Anfang
# elim_e(text,muster) Eliminiert text am Ende
# elim_ae(text,muster) Eliminiert text am Anfang und Ende
# elim_a_liste(text,muster_liste) Eliminiert text am Anfang mit einer liste von mustern z.B. [" ","\t"]
# elim_e_liste(text,muster_liste) Eliminiert text am Ende mit einer liste von mustern z.B. [" ","\t"]
# elim_ae_liste(text,muster_liste) Eliminiert text am Anfang und Ende mit einer liste von mustern z.B. [" ","\t"]
# elim_comment_not_quoted(text,comment_list,quot0,quot1) eliminiert Kommentar aus dem Text
# split_text(t,trenn)   trennt t nach trenn
# split_not_quoted(text,split,quot0,quot1,elim_leer) Trennt nicht gequoteten text mit split auf
#                                                    elim_leer=0/1 eliminiert leere zelle (def 0)
# split_with_quot(text,quot0,quot1) Trennt text mit quots
# slice(text,length) Zerlegt text in Stücke von l1-Länge
# change(text,muster_alt,muster_neu) Ersetzt in text alt gegen neu (einmal durch)
# change_max(text,muster_alt,muster_neu) Ersetzt in text alt gegen neu (solange geht)
#
# (body,ext) = file_splitext(file_name) Trennt in Pfad, Bodyname und Extension (ohne .)
# (path,body,ext) = file_split(file_name) Trennt in Pfad, Bodyname und Extension (ohne .)
# search_file_extension(file)             Sucht Fileextension in string file und gibt ihn zurÃ¼ck
# (okay,new_file_name) = file_exchange_path(file_name,source_path,target_path) exchanges source_path with target_path if possible
# liste =string_to_num_list(string) zerlegt string in eine numerische Liste
# vergleiche_text(text1,text2) Vergleicht, Ausgabe in Anteil, text als ganzes
# vergleiche_worte(text1,text2) Vergleicht, Ausgabe in Anteil, dir Leerzeichen getrennt Worte, und jedes Wort dem anderen
# commented out: doubledecode(s, as_unicode=True): decodiert in cp1252 (as_unicode=False) oder zurÃ¼ck in unicode (as_unicode=True)
# make_unicode(object) makes unicode, if not already (utf8)
# convert_to_unicode(value) converts to string if int, float into unicode
# to_unicode(string)   converts byte-string into unicode utf8
# to_bytes(unicode)    converts unicode into byte-string
# is_letter(t)  ist text ein Buchstabe RÃ¼ckgabe Liste mit 1/0  "et23sf" => [1,1,0,0,1,1]
# is_letter_flag(t) wenn alles Literale sind dann True ansonsten False
# is_digit(t)  ist text ein Digit Rückgabe Liste mit 1/0  "et23sf" => [0,0,1,1,0,0]
# is_digit_flag(t) wenn alles Digits sind dann True ansonsten False
# merge_string_liste(liste)   merged die Liste mit strings in einen string
# convert_string_to_float(value)
###################################################################################
# Listenbearbeitung
###################################################################################
# such_in_liste(liste,muster,regel='n') Suche nach dem string-Muster in der Liste nach der Regel
#                                       Input:
#                                       liste   list    mit strings zu durchsuchen
#                                       muster  string  muster nachdem gesucht wird
#                                       regel   string  Regel nach der gesucht wird:
#                                               e  exakt in den items der liste
#                                               n  nicht exakt, d.h. muß enthalten sein, auch groß/klein
#                                      Output:
#                                       index_list liste(int)   Index Liste mit den Übereinstimmungen
#                                                               wenn keine liste ist leer []
#
# reduce_double_items_in_liste(liste)  Reduziert doppelte Einträge in Liste
#                                      liste_r = reduce_double_items_in_liste(liste)
#
# string_is_in_liste(tt,liste)  True wenn tt vollständig in einem item der Liste
#                               False wenn nicht
# string_is_not_in_liste(tt,liste)
###################################################################################
# Fileoperating
###################################################################################
# def get_free_size(path): freie Größe Verzeichnis
# def get_parent_path_files(path): dir auf parent path and get files
# def get_parent_path_dirs(path): dir auf parent path and get dirs
# def get_dattime_float(full_filename): gibt int-Wert für datum und zeit jjjjmmtthhmmss
# get_subdir_files(path):  Kann in Iteration genutzt werden >>> for a in get_subdir_files("D:\\temp")
# def get_liste_of_subdir_files(Path,liste=[],search_ext=[]):Gibt eine Liste von allen Dateien in Unterverzeichnissen an (ext kann angegeben werden), liste als input wird weiter gefüllt
# def get_liste_of_subdirs(Path,liste=[]):Gibt eine Liste von allen Unterverzeichnissen an
# def get_liste_of_files_in_dir(DirName,extensionListe=[],subdirFlag=0,liste=[]) Sucht nach Dateien mit der angegbene extension (default alle)
#                                                                                trägt sie in die Liste ein, und untersucht auch subdirs (default nicht)
#                                                                                Rückgabe der Liste
# def get_size_of_dir(Path,size=0): Gibt Größe des gesamten Unterverzeichnispfad an
# def join(part0="",part1="",part2="",part3="",part4="",part5=""):Setzt Dateiname zusammen
# (path,fbody,ext) = get_pfe(full_file): Gibt Pfad,Filebody und Extension zurück
# def remove_dir_all(dir_name): Löscht den Pfad
# def remove_named_dir(dir_name,delete_name,recursive): Loescht von dir_name die Ordber delete_name rekursiv oder nicht weg
# def is_textfile(filename, blocksize = 512) checks if file is an text-file
# def copy(s_path_filename,t_path_filename,silent=1)  kopiert s_path_filename nach t_path_filename return hfkt.OK=1
# def make_backup_file(fullfilename,backup_dir)
# def move_file(filename,target)
# def change_text_in_file(filename,textsearch,textreplace):
# def build_path(pathname): erstellt Pfad wenn nicht vorhanden
# def clear_path(pathname): löscht Inhalt des Pfades
###################################################################################
# Eingabe/Ausgabe
###################################################################################
# def abfrage_listbox1(liste,smode):
# def abfrage_listbox(liste,smode):
# def abfrage_liste_scroll(liste,comment=None,cols=70,rows=20,multi=0):
# def abfrage_liste(liste,comment=None):
# def eingabe_int(comment):
# def eingabe_float(comment):
# def eingabe_string(comment):
# def eingabe_jn(comment,default=None):
# bool abfrage_ok_box(text="Ist das okay") Rückgabe True/False
# def abfrage_dir(comment=None,start_dir=None):
# def abfrage_sub_dir(comment=None,start_dir=None):
# def abfrage_file(file_types="*.*",comment=None,start_dir=None):
# def eingabe_file(file_types="*",comment="Waehle oder benenne neue Datei",start_dir=None):
# def abfrage_str_box(comment="",width=400): String aus eigenem Fenster einlesen (width Fenster-Breite)
###########################################################################################
# DAten lesen/schreiben
##################################################################################
# def read_csv_file(file_name=name,delimiter=";")
# def (state,header,data)=read_csv_file_header_data(file_name,delim=";")
# def write_csv_file_header_data(file_name,csv_header,csv_data,delim=";")
# def read_ascii(file_name=name) return (okay,txt) entire ascii-text
# def read_ascii_build_list_of_lines(file_name) return(okay,lines) list of lines from ascii-text
##################################################################################
# Schreiben in HTML-File
##################################################################################
# def html_write_start(f,title) Beginn HTML-Seite
# def html_write_end(f):Ende HTML-Seite
# def html_write_start_tab(f,title) Start Tabelle
# def html_write_end_tab(f) Ende TAbelle
# def html_write_start_colgroup(f): Start Spaltengruppe
# def html_write_end_colgroup(f):Ende Spaltengruppe
# def html_write_set_col_align(f,ausr):Ausrichtung "center","right","left"
# def html_write_start_tab_zeile(f): Zeilenstart einer Tabelle
# def html_write_end_tab_zeile(f): Ende einer Zeile in TAbelle
# def html_write_tab_zelle(f,h_flag,fett_flag,farbe,inhalt): Beschreib eine Zelle in einer Tabellezeile
# def html_write_leer_zeile(f,n_spalten): Leerzeilen über n Spalten
# def html_get_filename_for_browser(filename): Umsetzen für browser
###################################################################################
# Sonstiges
###################################################################################
# value = str_to_float_possible(string_val),  if not possible value = None
# ival  = str_to_int_possible(string_val),  if not possible ival = None
# def int_to_dec36(int_val,digits=0):
# def dec36_to_int(string_val):
# def summe_euro_to_cent(text,delim=","):
# def suche_in_liste(liste,gesucht):
# def int_akt_datum():
# def int_akt_time():
# def str_akt_datum():
# def str_datum(int_dat):
# secs = secs_akt_time_epoch()
# secs = secs_time_epoch_from_int(intval)
# secs = secs_time_epoch_from_str(str_dat,delim=".")
# secs = secs_time_epoch_from_int(intval,plus_hours)
# string = secs_time_epoch_to_str(secs): Wandelt in string Datum
# int    = secs_time_epoch_to_int(secs): Wandelt in int Datum
# flag   = datum_str_is_correct(str_dat,delim=".")
# daystr = day_from_secs_time_epoch(secs): Man bekommt den Tag in Mo,Di,Mi,Do,Fr,Sa,So
# daystr = day_from_datum_str(str_dat,delim="."): Man bekommt den Tag in Mo,Di,Mi,Do,Fr,Sa,So
# def datum_str_to_intliste(str_dat,delim="."): Wandelt string in eine Liste
# def datum_intliste_to_int(dat_l):  Wandelt Liste in ein int
# def datum_int_to_intliste(intval): int -> liste
# def datum_str_to_int(str_dat,delim="."): Wandelt string in ein int
# def datum_intliste_to_str(int_liste,delim="."):
# def datum_str_to_year_int(str_dat,delim="."):
# def datum_str_to_month_int(str_dat,delim="."):
# def datum_str_to_day_int(str_dat,delim="."):
# def secs_time_epoch_find_next_day(secs,idaynext):
# flag =  is_datum_str(str_dat,delim=".")   flag = True/False
# def get_name_by_dat_time(pre_text,post_text) Gibt Name gebildet aus localtime und text
# def diff_days_from_time_tuples(time_tuple_start,time_tuple_end) Bildet die Differenz der Tage über 0:00
# def string_cent_in_euro(cents):
# def num_cent_in_euro(cents):
# int = string_euro_in_int_cent(teuro,delim=",") Wandelt einen String mit Euro in Cent
# def dat_last_act_workday_datelist(year,mon,day): Sucht letzten aktuellen Werktag in  [year,month,day]
# (okay,value) =  str_to_float(string) wandelt string in float, wenn nicht erreichr okay = false
# flag = is_string(val)  Prüft, ob Type string flag = True/False
# flag = is_unicode(val)  Prüft, ob Type unicode flag = True/False
# flag = is_list(val)  Prüft, ob Type liste flag = True/False
# flag = is_dict(val)  Prüft, ob Type dictionary flag = True/False
# flag = isfield(c,'valname') Prüft ob eine Klasse oder ein dict die instance hat
# flag = isempty(val) Prüft, ob leer nach type
# print_python_is_32_or_64_bit
#---------------------------------
# multiply_constant(list,const)  multiplies a list with const value

from tkinter import *
from tkinter.constants import *
import tkinter.filedialog
import tkinter.messagebox
import tkinter.tix
import string
import types
import copy
import os
import stat
import time
import datetime
import calendar
import csv
import array
import shutil
import math
import struct


KITCHEN_MODUL_AVAILABLE = False


OK     = 1
NOT_OK = 0
QUOT    = 1
NO_QUOT = 0
TCL_ALL_EVENTS = 0
#############################
#Stringbearbeitung
##################
def get_str_from_int(intval,intwidth):
#-------------------------------------------------------
  """
  gibt string mit vorangestellten nullen aus
  """
  if( intval == 0 ):
    mzeichen = 0
    width    = 1
    val      = intval
  elif( intval < 0 ):
    mzeichen = 1
    width    = int(math.floor(math.log10(-intval)))+1
    val      = -intval
  else:
    mzeichen = 0
    width    = int(math.floor(math.log10(intval)))+1
    val      = intval

  if( mzeichen == 1 ): t = "-"
  else:                t = ""

  n = int(math.floor(math.fabs(intwidth))) - width
  i = 0
  while( (n > 0) and (i < n) ):
    t = t + "0"
    i += 1

  t = t + "%s" % val

  return t
#-------------------------------------------------------
def such(text,muster,regel="vs"):
    """
    Suche nach dem Muster in dem Text nach der Regel:

    Input:
    text    string  zu durchsuchender String
    muster  string  muster nachdem gesucht wird
    regel   string  Regel nach der gesucht wird:
                    vs  vorwaerts muster suchen
                    vn  vorwaerts suchen, wann muster nicht mehr
                        vorhanden ist
                    rs  rueckwrts muster suchen
                    rn  rueckwaerts suchen, wann muster nicht mehr
                        vorhanden ist

    Output:
    index   int     Index im String in der die Regel wahr geworden ist
                    oder -1 wenn die Regel nicht wahr geworden ist
    """
    lt = len(text)
    lm = len(muster)

    if (regel.find("v") > -1) or (regel.find("V") > -1):
        v_flag = 1
    else:
        v_flag = 0

    if (regel.find("n") > -1) or (regel.find("n") > -1):
        n_flag = 1
    else:
        n_flag = 0

    if v_flag == 1:  # vorwaerts

        if n_flag == 1: # nicht mehr vorkommen

            ireturn = -1
            for i in range(0,lt,1):

                if( text[i:i+lm].find(muster) == -1 ):
                    ireturn = i
                    break
                #endif
            #endof
            return ireturn

        else: #soll vorkommen

            ireturn = text.find(muster)
            # print "ireturn = %i" % ireturn
            return ireturn
        #endif
    else: # rueckwaerts

        if n_flag == 1:

            ireturn = -1
#           print "lm=%i" % lm
#           print "lt=%i" % lt
            for i in range(0,lt,1):

#               print "i=%i" % i
                i0 = text.rfind(muster,0,lt-i)
#               print "i0=%i" % i0
#               print "lt-lm-i=%i" % (lt-lm-i)

                if i0 < lt-lm-i:
                    ireturn = lt-i-1
                    break
            return ireturn

        else:

            ireturn = text.rfind(muster)
            return ireturn
#-------------------------------------------------------
def such_alle(t,muster):
    """
    Suche nach dem Muster in gesamten Text
    erstellt eine Index-Liste, wenn leer
    dann nixhts gefunden
    """
    liste = []
    i0    = 0
    i1    =  such(t[i0:],muster)
    while( i1 >= 0 ):
      liste.append(i1+i0)
      i0 = i0+i1+1
      i1    =  such(t[i0:],muster)
    return liste

def such_in_liste(liste,muster,regel=""):
    """
    Suche nach dem string-Muster in der Liste nach der Regel
    Input:
    liste   list    mit strings zu durchsuchen
    muster  string  muster nachdem gesucht wird
    regel   string  Regel nach der gesucht wird:
                    e  exakt in den items der liste
                    n  nicht exakt, d.h. muß enthalten sein, auch groß/klein

    Output:
    index_list liste(int)   Index Liste mit den �bereinstimmungen
                            wenn keine liste ist leer []
    """
    len1 = len(liste)
    index_liste = []
    for il in range(len1):
        if( regel == "e" ):
            if( liste[il] == muster):
                index_liste.append(il)
        else:
            ll = liste[il].lower()
            mm = muster.lower()
            i0 = ll.find(mm)
            if( i0 > -1 ):
                index_liste.append(il)
    return index_liste
#-------------------------------------------------------
def reduce_double_items_in_liste(liste):
  """
  Reduziert doppelte Einträge in Liste
  liste_r = reduce_double_items_in_liste(liste)
  """
  n = len(liste)
  liste_new = []
  for item in liste:
    flag = True
    for newitem in liste_new:
      if( newitem == item ):
        flag = False
        break
      #endif
    #endfor
    if( flag ):
      liste_new.append(item)
    #endif
  #endfor
  return liste_new
#-------------------------------------------------------
def elim_a(text,muster):
    """
    Schneidet muster am Anfang von text weg, wenn vorhanden
    """
    liste = []
    if( isinstance(muster, str) ):
        liste.append(muster)
    elif( isinstance(muster,list) ):
        liste = muster
    else:
        return text

    if( len(text) == 0 ):
        return text

    lt = len(text)
    if(  lt == 0 ):
        return text

    l = len(liste)
    check_liste = [1 for i in range(l)]

    while( True ):
        for i in range(l):
            i0 = such(text,liste[i],"vn")
            if( i0 < 0 ):
                text = ""
            elif( i0 == 0 ):
                check_liste[i] = 0
            else:
                text = text[i0:len(text)]
                check_liste[i] = 1
        if( sum(check_liste) == 0 or len(text) == 0 ):
            break
    return text
#-------------------------------------------------------
def elim_e(text,muster):
    """
    Schneidet muster am Ende von text weg, wenn vorhanden
    """
    liste = []
    if( isinstance(muster, str) ):
        liste.append(muster)
    elif( isinstance(muster,list) ):
        liste = muster
    else:
        return text
    lt = len(text)
    if(  lt == 0 ):
        return text

    l = len(liste)
    check_liste = [1 for i in range(l)]

    while( True ):
        for i in range(l):
            i0 = such(text,liste[i],"rn")
            if( i0 < 0 ):
                text = ""
            elif( i0 == lt-1 ):
                check_liste[i] = 0
            else:
                text = text[0:i0+1]
                lt = len(text)
                check_liste[i] = 1
        if( sum(check_liste) == 0 or len(text) == 0 ):
            break
    return text
#-------------------------------------------------------
def elim_ae(text,muster):
    """
    Schneidet muster am Anfang und Ende von text weg, wenn vorhanden
    """
    text = elim_a(text,muster)
    text = elim_e(text,muster)
    return text
#
#-------------------------------------------------------
def elim_a_liste(text,muster_liste):
    """
    elim_a_liste(text,muster_liste) Eliminiert text am Anfnag mit einer liste von mustern z.B. [" ","\t"]
    """
    flag = True

    while( flag ):
      n1 = len(text)
      for muster in muster_liste:
        text = elim_a(text,muster)
      n2 = len(text)
      if( n1 == n2 ):
        flag = False
    return text
#
#-------------------------------------------------------
def elim_e_liste(text,muster_liste):
    """
    elim_e_liste(text,muster_liste) Eliminiert text am Ende mit einer liste von mustern z.B. [" ","\t"]
    """
    flag = True

    while( flag ):
      n1 = len(text)
      for muster in muster_liste:
        text = elim_e(text,muster)
      n2 = len(text)
      if( n1 == n2 ):
        flag = False
    return text
#
# elim_ae_liste(text,muster_liste) Eliminiert text am Anfang und Ende mit einer liste von mustern z.B. [" ","\t"]
#
#-------------------------------------------------------
def elim_ae_liste(text,muster_liste):
    """
    elim_a_liste(text,muster_liste) Eliminiert text am Anfang mit einer liste von mustern z.B. [" ","\t"]
    """
    text = elim_a_liste(text,muster_liste)
    text = elim_e_liste(text,muster_liste)
    return text

def get_index_quoted(text,quot0,quot1):
    return get_index_quot(text,quot0,quot1)
def get_index_quot(text,quot0,quot1):
    """
    Gibt Indexpaare (Tuples) in dem der Text gequotet ist z.B.

    text  = "abc {nest} efg  {plab}"
    #        0123456789012345678901
    quot0 = "{"
    quot1 = "}"

    a = get_index_quot(text,quot0,quot1)

    a ergibt [(5,9),(17,21)]
    """

    liste = []

    i0  = 0
    i1  = len(text)
    # print "i1 = %i" % i1
    lq0 = len(quot0)
    lq1 = len(quot1)

    while i0 < i1:

        istart = text.find(quot0,i0,i1)
#       print "istart = %i" % istart
        if istart > -1:

            iend = text.find(quot1,istart+lq0,i1)
#           print "iend = %i" % iend

            if iend == -1:
                iend = i1

            tup = (istart+lq0,iend)
            liste.append(tup)

            i0 = iend+lq1
        else:

            i0 = i1

    return liste

def get_index_not_quoted(text,quot0,quot1):
    return get_index_no_quot(text,quot0,quot1)
def get_index_no_quot(text,quot0,quot1):
    """
    Gibt Indexpaare (Tuples) in dem der Text nicht gequotet ist z.B.

    text  = "abc {nest} efg  {plab}"
    #        0123456789012345678901
    quot0 = "{"
    quot1 = "}"

    a = get_index_quot(text,quot0,quot1)

    a ergibt [(0,4),(10,16)]
    """
    liste = []

    i0  = 0
    i1  = len(text)
#   print "i1 = %i" % i1
    lq0 = len(quot0)
    lq1 = len(quot1)

    istart = 0

    while istart < i1:

        iend = text.find(quot0,istart,i1)
#       print "iend = %i" % iend
        if iend == -1:
            iend = i1

        tup = (istart,iend-1)
        if( istart != iend ):
            liste.append(tup)

        i0 = text.find(quot1,iend+lq0,i1)

        if i0 == -1:
            istart = i1
        else:
            istart = i0+lq1


    return liste
def get_string_quoted(text,quot0,quot1):
    """
    Gibt Liste mit string in dem der gequotet Text steht z.B.

    text  = "abc {nest} efg  {plab}"
    #        0123456789012345678901
    quot0 = "{"
    quot1 = "}"

    a = get_string_quoted(text,quot0,quot1)

    a ergibt ["nest","plab"]
    """
    iliste = get_index_quot(text,quot0,quot1)
    liste = []
    for t in iliste:
        tdum = text[t[0]:t[1]]
        liste.append(tdum)
    return liste
def get_string_not_quoted(text,quot0,quot1):
    """
    Gibt Liste mit string in dem der nicht gequotet Text steht z.B.

    text  = "abc {nest} efg  {plab}"
    #        0123456789012345678901
    quot0 = "{"
    quot1 = "}"

    a = get_string_not_quoted(text,quot0,quot1)

    a ergibt ["abc "," efg "]
    """
    iliste = get_index_not_quoted(text,quot0,quot1)
    liste = []
    for t in iliste:
        tdum = text[t[0]:t[1]]
        liste.append(tdum)
    return liste
def elim_comment_not_quoted(text,comment_liste,quot0,quot1):
    """
    eliminiert Kommentar nichtgequoteten aus dem Text, wenn ein Kommentarzeichen
    aus der Liste comment_list vorkommt z.B.
    text = "abc{abc#def} # ddd"
    tneu = elim_comment_not_quoted(text,["#","%"],"{","}")
    tneu = "abc{abc#def} "
    """
    text1 = text
    a_liste = get_index_no_quot(text,quot0,quot1)
    i0 = len(text1)
#   print "i0= %i" % i0
#   print a_liste
    for a in a_liste:
        for comment in comment_liste:
#           print "a[0]= %i" % a[0]
#           print "a[1]= %i" % a[1]
            b = text1.find(comment,a[0],a[1])
#           print "b= %i" % b
            if b > -1:
                i0 = min(b,i0)
#               print "i0= %i" % i0
    return text1[0:i0]
def split_not_quoted(text,spl,quot0,quot1,elim_leer=0):
    """
    Trennt text nach dem split-string auf, aber nur in nicht quotierten
    Teil.
    Input:
    text        string  z.B.    "abc|edf||{|||}|ghi"
    split       string          "|"
    quot0       string          "{"
    quot1       string          "}"
    elim_leer   int     wenn gesetzt, werden leere Items der Liste gel�scht
                        z.B.    1
    Output:
                        Liste mit den String-Teilen
                        z.B.    ["abc","edf","{|||}","ghi"]
    """
    ls     = len(spl)
    i_list = get_index_quoted(text,quot0,quot1)

    flag = 1
    i0   = 0
    i01  = 0
    i1   = len(text)
    liste = []
    while( flag ):
        i= text.find(spl,i01,i1)

        # Kein spl gefunden
        if( i < 0 ):
            liste.append(text[i0:i1])
            flag = 0
        else:
            # Prüfen ob i im quot liegt
            nimm_flag = 1
            for ii in i_list:
                # Wenn im quot nimm nicht
                if( i >= ii[0] and i <= ii[1] ):
                    nimm_flag = 0
                    break
            if( nimm_flag ):
                # Wenn leer
                if( i0 == i ):
                    liste.append("")
                else:
                    liste.append(text[i0:i])
                i0  = i+ls
                i01 = i0
                # Wenn Ende erreicht
                if( i0 > i1 ):
                    # Wenn am Ende noch ein spl zu finden
                    if( i0 == i1-1 ):
                        liste.append("")
                    flag = 0
            # weiter suchen
            else:
                i01 = i+ls

    # Wenn leere Listen-Items
    # gelöscht werden sollen
    if( elim_leer ):
        liste1 = []
        for t in liste:
            if( len(t) > 0 ):
                liste1.append(t)
        liste = liste1
    return liste
def split_text(t,trenn):
  """
  sucht in text nach trenn und bildet
  eine Liste den verbleibenden Textteile
  """
  liste = []

  ltrenn = len(trenn)


  flag = 1

  while(flag):

    i = such(t,trenn,"vs")

    if( i < 0 ):
      liste.append(t)
      flag = 0
      t = ""

    elif( i == 0 ):
      liste.append("")
    else:
      liste.append(t[0:i])
    #endif

    if( i+ltrenn <= len(t) ):

      t = t[i+ltrenn:]

    else:
      t = ""
    #endif

    if( len(t) <= ltrenn ):
      flag = 0
  #endwhile
  return liste

def split_with_quot(text,quot0,quot1):
    """
    Trennt Text mit den quots in seine Teile und gibt an, ob
    gequteter Text oder nichtgequotet (außerhalb)
    Input:
    text    string      z.B.    "abc[def]ghi"
    quot0   string              "["
    quot1   string              "]"
    Output
            Liste mit Tuple     [["abc",NO_QUOT],["def",QUOT],["ghi",NO_QUOT]]
                                QUOT == 1, NO_QUOT == 0
    """
    i_list = get_index_no_quot(text,quot0,quot1)
    #print "no_quot"
    #print i_list

    liste = []
    # kein nicht quotierter Text
    if( len(i_list) == 0 ):
        i_list = get_index_quot(text,quot0,quot1)
        if( len(i_list) > 0 ):
            i0 = l_list[0][0]
            i1 = l_list[0][1]
            liste.append([text[i0:i1],QUOT]) #alles im quot
    else:
        i     = 0
        ilast = 0
        for il in i_list:
            i0 = il[0]
            i1 = il[1]
            if( i == 0 ):
                if( i0 != 0 ): #als erstes kommt gequoteter Text
                    liste.append([text[0:i0],QUOT])
                liste.append([text[i0:i1],NO_QUOT])
                ilast = i1
            else:
                liste.append([text[ilast:i0],QUOT])
                liste.append([text[i0:i1],NO_QUOT])
                ilast = i1
                i = i+1

        if( ilast+1 < len(text) ): # noch ein Rest vorhanden
            liste.append([text[ilast:],QUOT])

    return liste
def change(text,muster_alt,muster_neu):
    """
    ersetzt einmal alle muster_alt mit muster_neu im Text
    """
    if( isinstance(text,str) ):
        text = text.replace(muster_alt,muster_neu)
    #liste = string.split(text,muster_alt)
    #n0 = len(liste)
    #if( n0 <= 1 ):
    #    text1 = text
    #else:
    #    text1 = ""
    #text1 = ""
    #for i in range(0,n0-1):
    #    text1=text1+liste[i]+muster_neu
    #text1=text1+liste[n0-1]
    return text

def change_max(text,muster_alt,muster_neu):
    """
    ersetzt sooft es geht alle muster_alt mit muster_neu im Text
    """
    while( 1 ):
        text1 = change(text,muster_alt,muster_neu)

        if( text1 == text ):
            break
        else:
            text = text1

    return text1

def slice(text,l1):
    """
    Zerlegt text in Stücke von l1-Länge
    """
    liste = []
    t  = text
    while( len(t) > l1 ):
        liste.append(t[0:l1])
        t = t[l1:]
    if( len(t) > 0 ):
        liste.append(t)
    return liste

def file_splitext(file_name):
    """
    ZErlegt file_name in body,ext
    file_name = "d:\\abc\\def\\ghj.dat"
    body      = "d:\\abc\\def\\ghj"
    ext       = "dat"
    """
    name = change_max(file_name,"/","\\")

    i0 = such(name,"\\","rs")
    i1 = such(name,".","rs")
    n  = len(name)

    if( i1 > 0 and i1 > i0 ):
        b = file_name[0:i1]
        e = file_name[i1+1:n]
    else:
        b = file_name[0:n]
        e = ""

    return (b,e)
def file_split(file_name):
    """
    ZErlegt file_name in path,body,ext
    file_name = "d:\\abc\\def\\ghj.dat"
    path      = "d:\\abc\\def"
    body      = "ghj"
    ext       = "dat"
    """
    name = change_max(file_name,"/","\\")

    i0 = such(name,"\\","rs")
    i1 = such(name,".","rs")
    n  = len(name)

    if( i0 > 0 ):
        p = file_name[0:i0]
    else:
        p = ""

    if( i1 > 0 and i1 > i0 ):
        b = file_name[i0+1:i1]
        e = file_name[i1+1:n]
    else:
        b = file_name[i0+1:n]
        e = ""

    return (p,b,e)
def search_file_extension(file):

    i1 = such(file,".","rs")
    return file[i1+1:len(file)]

def file_exchange_path(file_name,source_path,target_path):
  okay = NOT_OK
  new_file_name = ""
  file_name   = os.path.normcase(change_max(file_name,'\\','/'))
  source_path = os.path.normcase(change_max(source_path,'\\','/'))
  target_path = os.path.normcase(change_max(target_path,'\\','/'))

  if( such(file_name,source_path,"vs") == 0 ): # Am Anfang gefunden
    ll = len( source_path )
    new_file_name = os.path.join(target_path,file_name[ll:])
    okay          = OK
  return (okay,new_file_name)

def string_to_num_list(txt):
  ''' Zerlegt string wie '2, 3,  10, 1 ' oder '[1.2, 3.2, 4.55]'
      in eine numerische Liste
  '''

  txt = elim_a(txt,[' ','[','('])
  txt = elim_e(txt,[' ',']',')'])

  tliste = txt.split(',')

  liste = []

  for item in tliste:

    (okay,value) = str_to_float(item)
    if( okay ):
      liste.append(value)
    else:
      liste = []
      return liste

  return liste
def is_letter(tt):
  '''  ist text ein Buchstabe Rückgabe Liste mit 1/0
       "et23sf" => [1,1,0,0,1,1]
  '''
  liste = []
  for t in tt:
    if( t in string.ascii_letters ):
      liste.append(1)
    else:
      liste.append(0)
def is_letter_flag(tt):
  n = len(tt)
  ncount = 0
  for t in tt:
    if( t in string.ascii_letters ):
      ncount += 1

  if( ncount == n ): return True
  return False
def is_digit(tt):
  '''  ist text ein Digit Rückgabe Liste mit 1/0
       "et23sf" => [0,0,1,1,0,0]
  '''
  liste = []
  for t in tt:
    if( t in string.digits ):
      liste.append(1)
    else:
      liste.append(0)
def is_digit_flag(tt):
  n = len(tt)
  ncount = 0
  for t in tt:
    if( t in string.digits ):
      ncount += 1

  if( ncount == n ): return True
  return False
def merge_string_liste(liste):
  '''
     merged die Liste mit strings in einen string
  '''
  tt = ""
  for l in liste:
    if( isinstance(type(l), str) ):
      tt += l
  return tt
def convert_string_to_float(text):
    '''
    convert from string into float
    check for komma, point
    '''

    return float(change_max(text=text,muster_alt=",",muster_neu="."))


def vergleiche_text(text1,text2):
  '''
  vergleiche_text(text1,text2) Vergleicht, Ausgabe in Anteil, text als ganzes
  '''
  l1 = len(text1)
  l2 = len(text2)
  # Abruchbeingung
  if( (l1 == 0) or (l2 == 0) ):
    return 0.0
  elif(  isinstance(type(text1), str)  ):
    return 0.0
  elif( isinstance(type(text2), str) ):
    return 0.0

  # der grössere Text wird base
  if( l1 > l2 ):
    lbase = l1
    tbase = text1
    lvar0 = l2
    tvar0 = text2
  else:
    lbase = l2
    tbase = text2
    lvar0 = l1
    tvar0 = text1

  # Var-Wort von hinten i=0 u. von vorne i=1 kürzen
  lfound = 0
  for i in range(2):
    run_flag = True
    lvar     = lvar0
    tvar     = tvar0
    while( run_flag ):
      i0 = such(tbase,tvar,"vs")
      if( i0 >= 0 ):
        if( lvar > lfound ):
          lfound = lvar
        run_flag = False
        break
      else:
        # Ende wenn tvar leer wird
        if( lvar == 1 ):
          run_flag = False
        else:
          if( i == 0 ):
            tvar = tvar[0:lvar-1]
          else:
            tvar = tvar[1:lvar]
          lvar = len(tvar)

  return float(lfound)/float(lbase)

def vergleiche_worte(text1,text2):
  '''
  vergleiche_worte(text1,text2) Vergleicht, Ausgabe in Anteil
  dir Leerzeichen getrennt Worte, und jedes Wort dem anderen
  '''

  if(  not isinstance(obj, str)  ):
    return 0.0

  t1 = change_max(text1,'\t',' ')
  t1 = change_max(t1,'  ',' ')
  t1 = elim_ae(t1,' ')
  t1 = t1.split(' ')
  l1 = len(t1)
  t2 = change_max(text2,'\t',' ')
  t2 = change_max(t2,'  ',' ')
  t2 = elim_ae(t2,' ')
  t2 = t2.split(' ')
  l2 = len(t2)

  if( (l2 == 0) or (l1 == 0) ):
    return 0.0

  if( l1 > l2 ):
    lbase = l1
    tbase = t1
    lvar0 = l2
    tvar0 = t2
  else:
    lbase = l2
    tbase = t2
    lvar0 = l1
    tvar0 = t1

  # Bezugsgrösse
  lmax1 = 0
  for t in tbase:
    lmax1 += len(t)
  lmax2 = 0
  for t in tvar0:
    lmax2 += len(t)
  lmax = float(max(max(lmax1,lmax2),1))


  # Var-Wort von hinten i=0 u. von vorne i=1 kürzen
  lfound = 0.0
  for i in range(2):
    run_flag = True
    lvar     = lvar0
    tvar     = tvar0
    while( run_flag ):
      for j in range(lbase-lvar+1):
        lfz = 0.0
        for k in range(lvar):
          lmz = float(max(len(tvar[k]),len(tbase[k+j])))
          lfz += vergleiche_text(tvar[k],tbase[k+j])*lmz/lmax
        if( lfz > lfound ):
          lfound = lfz

      # Ende wenn tvar leer wird
      if( lvar == 1 ):
        run_flag = False
      else:
        if( i == 0 ):
          tvar = tvar[0:lvar-1]
        else:
          tvar = tvar[1:lvar]
        lvar = len(tvar)

  return lfound

##def doubledecode(s, as_unicode=True):
##  s = s.decode('utf8')
##  # remove the windows gremlins O^1
##  for src, dest in cp1252_liste.items():
##    s = s.replace(src, dest)
##  s = s.encode('raw_unicode_escape')
##  if as_unicode:
##    # return as unicode string
##    s = s.decode('utf8', 'ignore')
##  return s
##
##cp1252_liste = {
### from http://www.microsoft.com/typography/unicode/1252.htm
##u"\u20AC": u"\x80", # EURO SIGN
##u"\u201A": u"\x82", # SINGLE LOW-9 QUOTATION MARK
##u"\u0192": u"\x83", # LATIN SMALL LETTER F WITH HOOK
##u"\u201E": u"\x84", # DOUBLE LOW-9 QUOTATION MARK
##u"\u2026": u"\x85", # HORIZONTAL ELLIPSIS
##u"\u2020": u"\x86", # DAGGER
##u"\u2021": u"\x87", # DOUBLE DAGGER
##u"\u02C6": u"\x88", # MODIFIER LETTER CIRCUMFLEX ACCENT
##u"\u2030": u"\x89", # PER MILLE SIGN
##u"\u0160": u"\x8A", # LATIN CAPITAL LETTER S WITH CARON
##u"\u2039": u"\x8B", # SINGLE LEFT-POINTING ANGLE QUOTATION MARK
##u"\u0152": u"\x8C", # LATIN CAPITAL LIGATURE OE
##u"\u017D": u"\x8E", # LATIN CAPITAL LETTER Z WITH CARON
##u"\u2018": u"\x91", # LEFT SINGLE QUOTATION MARK
##u"\u2019": u"\x92", # RIGHT SINGLE QUOTATION MARK
##u"\u201C": u"\x93", # LEFT DOUBLE QUOTATION MARK
##u"\u201D": u"\x94", # RIGHT DOUBLE QUOTATION MARK
##u"\u2022": u"\x95", # BULLET
##u"\u2013": u"\x96", # EN DASH
##u"\u2014": u"\x97", # EM DASH
##u"\u02DC": u"\x98", # SMALL TILDE
##u"\u2122": u"\x99", # TRADE MARK SIGN
##u"\u0161": u"\x9A", # LATIN SMALL LETTER S WITH CARON
##u"\u203A": u"\x9B", # SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
##u"\u0153": u"\x9C", # LATIN SMALL LIGATURE OE
##u"\u017E": u"\x9E", # LATIN SMALL LETTER Z WITH CARON
##u"\u0178": u"\x9F", # LATIN CAPITAL LETTER Y WITH DIAERESIS
##}
#===============================================================================
# (text2,errtext) = make_unicode(text1)
# (text2,errtext) = make_unicode(text1,type)
#
# errtext == "" wenn kein Fehler
# ansonsten Fehler Text
#
def make_unicode(text1,texttype="utf-8"):
  errtext = ""
  text2   = ""
  if( isinstance(text1, str)  ):
    text2 = h.to_unicode(text1,texttype)
  else:
    text2 = text1
  #endif
  if( not isinstance(text1, str) ):
    errtext = "Text: %s kann nicht in unicode gewandelt werden " % text1
  #endif
  return (text2,errtext)
#===============================================================================
def convert_to_unicode(text1,texttype="utf-8"):
  if( not isinstance(text1, str)   ):
    tt = str(text1)
  #endif
  tt = to_unicode(text1,texttype=texttype)

  return tt

#===============================================================================
def to_unicode(text1,texttype="utf-8"):
  '''
  to_unicode(string)   converts byte-string into unicode utf8
  '''
  if( KITCHEN_MODUL_AVAILABLE ):
    text2 = kitchen.text.converters.to_unicode(text1, encoding=texttype, errors='replace', nonstring=None, non_string=None)
  else:
    text2 = text1   # unicode.encode(text1,'utf-8')
  return text2
def to_bytes(text1,texttype="ascii"):
  '''
  to_bytes(unicode)    converts unicode into byte-string
  '''
  if( KITCHEN_MODUL_AVAILABLE ):
    text2 = kitchen.text.converters.to_bytes(text1, encoding=texttype, errors='replace', nonstring=None, non_string=None)
  else:
    text2 = text1   # unicode.decode(text1)
  return text2
##def to_cp1252(text1):
##  '''
##  to_cp1252(unicode)    converts unicode into byte-string
##  '''
##    text2 = unicode.encoode(text1,'cp1252')
##  return text2

#

###################################################################################
# Listenbearbeitung
###################################################################################
def such_in_liste(liste,muster,regel=""):
    """
    Suche nach dem string-Muster in der Liste nach der Regel
    Input:
    liste   list    mit strings zu durchsuchen
    muster  string  muster nachdem gesucht wird
    regel   string  Regel nach der gesucht wird:
                    e  exakt in den items der liste
                    n  nicht exakt, d.h. muß enthalten sein, auch groß/klein

    Output:
    index_list liste[int]   Index Liste mit den Übereinstimmungen
                            wenn keine liste ist leer []
    """
    len1 = len(liste)
    index_liste = []
    for il in range(len1):
        if( regel == "e" ):
            if( liste[il] == muster):
                index_liste.append(il)
        else:
            ll = liste[il].lower()
            mm = muster.lower()
            i0 = ll.find(mm);
            if( i0 > -1 ):
                index_liste.append(il)
    return index_liste
def string_is_in_liste(tt,liste):
  """
    True wenn tt vollständig in einem item der Liste
    False wenn nicht
  """
  index_list_liste = such_in_liste(liste,tt,regel="e")

  if( len(index_list_liste) > 0 ):
    return True
  else:
    return False
def string_is_not_in_liste(tt,liste):
  """
    False wenn tt vollständig in einem item der Liste
    True wenn nicht
  """
  return not string_is_in_liste(tt,liste)
###################################################################################
# Fileoperating
###################################################################################
def is_file_type(file_type_list,file_name):
    return 0

def get_free_size(path):
    """ Gibt freie Groesse des Verzeichnis path in byte (int) an
    """
    if os.path.isdir(path):
        # print "test ",path
        dirlist  = os.popen('dir '+path).read()
        if( len(dirlist) == 0 ):
            print('Der Pfad <',path,'> ist nicht richtig')
            return 0
        liste = dirlist.split()
        # print liste
        t = liste[-3].replace('.','')
        freesize = int(liste[-3].replace('.',''))
        return freesize
    else:
        print("Der Pfad <",path,"> existiert nicht")
        return 0
def get_parent_path(pp):

  p = os.path.normpath(pp)

  ll = p.split('\\')

  n = len(ll)
  if( n > 1): n -= 1

  parent_dir = ''
  for i in range(n):
    parent_dir += ll[i]
    if( i < n-1 ): parent_dir += '\\'
  #endFor
  return parent_dir

def get_parent_path_files(pp,name_only=0):
  """
  liste_of_fullfiles  = get_parent_path_files(path)
  liste_of_files      = get_parent_path_files(path,1)
  dir auf parent path and get files
  """
  liste = []
  if( os.path.isdir(pp) ):

    parent_dir = get_parent_path(pp)


    dirlist = os.listdir(parent_dir)

    for basename in dirlist:
      name = os.path.join(parent_dir,basename)
      if( os.path.isfile(name) ):
        if( name_only ): liste.append(basename)
        else:            liste.append(name)
      #endIf
    #endFor
  #endIf
  return liste

def get_parent_path_dirs(pp,name_only=0):
  """
  liste_of_dirs  = get_parent_path_dirs(path)
  liste_of_names = get_parent_path_dirs(path,1)
  dir auf parent path and get dirs
  """
  liste = []
  if( os.path.isdir(pp) ):

    parent_dir = get_parent_path(pp)

    dirlist = os.listdir(parent_dir)

    for basename in dirlist:
      name = os.path.join(parent_dir,basename)
      if( os.path.isdir(name) ):
        if( name_only ): liste.append(basename)
        else:            liste.append(name)
      #endIf
    #endFor
  #endIf
  return liste

def get_dattime_float(full_filename):
    """ gibt int-Wert für datum und zeit jjjjmmtthhmmss
    """
    dattime = 0
    if os.path.isfile(full_filename):

        t = time.gmtime(os.path.getmtime(full_filename))
        dattime = t[0]*1e10+t[1]*1e8+t[2]*1e6+t[3]*1e4+t[4]*1e2+t[5]

    return dattime
class get_subdir_files:
    def __init__ (self, *rootDirs):
        self.dirQueue    = list (rootDirs)
        self.includeDirs = None
        self.fileQueue   = []

    def __getitem__ (self, index):
        while len (self.fileQueue) == 0:
            self.nextDir ()
        result = self.fileQueue [0]
        del self.fileQueue [0]
        return result

    def nextDir (self):
        dir = self.dirQueue [0]   # fails with IndexError, which is fine
                                  # for iterator interface
        del self.dirQueue [0]
        list = os.listdir (dir)
        join = os.path.join
        isdir = os.path.isdir
        for basename in list:
            fullPath = join(dir, basename)
            if isdir (fullPath):
                self.dirQueue.append (fullPath)
                if self.includeDirs:
                    self.fileQueue.append (fullPath)
            else:
                self.fileQueue.append (fullPath)
def get_liste_of_subdir_files(Path,liste=[],search_ext=[]):
    """ Gibt eine Liste von allen Dateien in Unterverzeichnissen an
        trägt Ergebnis in liste ein un gibt sie zurück
        liste kann vorggeben werden
        liste = get_liste_of_subdir_files("d:\\abc"):
        oder
        vorgegebene_liste = get_liste_of_subdir_files("d:\\abc",vorgegebene_liste):
        oder
        liste = get_liste_of_subdir_files("d:\\abc",search_ext='mp3'): Alle mp3-Datein
        oder
        liste = get_liste_of_subdir_files("d:\\abc",search_ext=['mp3','wav']): Alle mp3- und wav-Datein
    """
    if(  isinstance(search_ext, str) ):
      search_ext = [seach_ext]

    dirlist = os.listdir (Path)
    join = os.path.join
    isdir = os.path.isdir
    for basename in dirlist:
        fullPath = join (Path, basename)
        if isdir (fullPath):
            liste = get_liste_of_subdir_files(fullPath,liste,search_ext)
        else:
            if( len(search_ext) == 0 ):
              liste.append(fullPath)
            else:
              for extsearch in search_ext:
                ext = search_file_extension(fullPath)
                if( extsearch == ext ):
                  liste.append(fullPath)
    return liste
def get_liste_of_files_in_dir(DirNameListe,extensionListe=[],subdirFlag=0,liste=[]):
    """ Sucht nach Dateien mit der angegbene extension (default alle),
        trägt sie in die Liste ein, und untersucht auch subdirs (default nicht)
        Rückgabe der Liste
    """
    if( isinstance(DirNameListe, str) ):
      DirNameListe = [DirNameListe]

    for DirName in DirNameListe:

      if( not os.path.isdir(DirName) ):
        print("DirName <%s> ist nicht vorhanden !!!!!!!" % DirName)
        return liste

      dirlist = os.listdir (DirName)
      for basename in dirlist:
          fullPath = os.path.join (DirName, basename)
          if( subdirFlag and os.path.isdir (fullPath) ):
              liste = get_liste_of_files_in_dir(fullPath,extensionListe,subdirFlag,liste)
          elif( os.path.isfile(fullPath) ):
              (body,ext) = file_splitext(basename)
              flag = 0
              if( len(extensionListe) == 0 ):
                flag = 1
              else:
                for e in extensionListe:
                  if( e == "*" ):
                    flag = 1
                  elif( ext == e ):
                    flag = 1

              if( flag == 1 ):
                  liste.append(fullPath)

    return liste

def get_liste_of_subdirs(Path,liste=[],include_start_dir=0):
    """ Gibt eine Liste von allen Unterverzeichnissen an
        trägt Ergebnis in liste ein un gibt sie zurück
        liste kann vorggeben werden
        liste = get_liste_of_subdir_files("d:\\abc"):
        oder
        vorgegebene_liste = get_liste_of_subdir_files("d:\\abc",vorgegebene_liste):
    """

    if( include_start_dir ):
        liste.append(Path)

    dirlist = os.listdir (Path)
    join = os.path.join
    isdir = os.path.isdir
    for basename in dirlist:
        fullPath = join (Path, basename)
        if isdir (fullPath):
            liste.append(fullPath)
            liste = get_liste_of_subdirs(Path=fullPath,liste=liste,include_start_dir=0)
    return liste

def get_size_of_dir(Path,size=0):
    """ Gibt Größe des gesamten Unterverzeichnispfad an
        get_size_of_dir(Path_name)
    """

    list = os.listdir (Path)
    join = os.path.join
    isdir = os.path.isdir
    for basename in list:
        fullPath = join (Path, basename)
        if isdir (fullPath):
            size = get_size_of_dir(fullPath,size)
        else:
            size = size + os.path.getsize(fullPath)
    return size

def join(part0="",part1="",part2="",part3="",part4="",part5=""):
    """ Setzt Dateiname zusammen aus maximal 6 Teilen
        fname = join("f:\\def","\\abc.dat")
    """
    list = []
    if( len(part0) > 0 ): list.append(part0)
    if( len(part1) > 0 ): list.append(part1)
    if( len(part2) > 0 ): list.append(part2)
    if( len(part3) > 0 ): list.append(part3)
    if( len(part4) > 0 ): list.append(part4)
    if( len(part5) > 0 ): list.append(part5)

    slist = []
    for item in list:

        list0 = item.split("/")
        for item0 in list0:

            list1 = item0.split(string.punctuation[23]) # "\\"

            for item1 in list1:

                if( len(item1) > 0 ): slist.append(item1)

    ret_text = ""
    for isl in range(len(slist)):

        if( isl == 0 ): ret_text = slist[isl]
        else:           ret_text = ret_text + os.sep + slist[isl]

    return ret_text

def get_pfe(full_file):
    """ Gibt Pfad, Filebody und Extension zurück
        (path,fbody,extension) = get_pfe("d:\\abc.dat")
    """
    ext   = ""
    fbody = ""
    path  = ""
    if( full_file and len(full_file) > 0 ):

        iex = full_file.rfind(".")
        if( iex > -1 and iex < len(full_file) ):
            ext  = full_file[iex+1:]
            rest = full_file[0:iex]
        else:
            ext  = ""
            rest = full_file

        rest = join(rest,os.sep)

        ipath = rest.rfind(os.sep)
        if( ipath > -1 ):

            if( ipath+1 < len(rest) ):

                path  = rest[0:ipath+1]
                fbody = rest[ipath+1:]
            else:
                path  = rest
                fbody = ""
        else:

            path  = ""
            fbody = rest

    return path,fbody,ext

def remove_dir_all(dir_name):

    try:
        liste   = os.listdir(dir_name)
    except WindowsError:
        print("remove_dir_all.error: os.listdir(\"%s\") not found" % dir_name)
        return

    for aname in liste:

        aname = os.path.normcase(aname)
        bname = os.path.join(dir_name,aname)

        os.chmod(bname,stat.S_IWRITE )




        if( os.path.isdir(bname) ):

            remove_dir_all(bname)

        elif( os.path.isfile(bname)  ):

            os.remove(bname)
    os.rmdir(dir_name)

def remove_all_in_dir(dir_name):
    try:
        liste   = os.listdir(dir_name)
    except WindowsError:
        print("remove_dir_all.error: os.listdir(\"%s\") not found" % dir_name)
        return

    for aname in liste:

        aname = os.path.normcase(aname)
        bname = os.path.join(dir_name,aname)

        os.chmod(bname,stat.S_IWRITE )




        if( os.path.isdir(bname) ):

            remove_dir_all(bname)

        elif( os.path.isfile(bname)  ):

            os.remove(bname)

def remove_named_dir(dir_name,delete_name,recursive):


    delete_name = os.path.normcase(delete_name)

    try:
        liste   = os.listdir(dir_name)
    except WindowsError:
        print("loesche_ordner_function.error: os.listdir(\"%s\") not found" % dir_name)
        return

    for aname in liste:

        aname = os.path.normcase(aname)
        bname = os.path.join(dir_name,aname)


        if( os.path.isdir(bname) ):

            if( aname == delete_name or bname == delete_name):
                print("remove %s" % bname)
                remove_dir_all(bname)
            elif(recursive):
                remove_named_dir(bname,delete_name,recursive)


        elif( os.path.isfile(bname)  ):

            if( aname == delete_name or bname == delete_name):
                print("remove %s" % bname)
                os.remove(bname)


def is_textfile(filename, blocksize = 512):

    text_characters_in = "".join(list(map(chr, range(32, 127))) + list("\n\r\t\b"))

    text_characters_out = ""

    for i in range(len(text_characters_in)):
        text_characters_out += '#'

    _eins_trans = str.maketrans(text_characters_in,text_characters_out)

    try:
      s = open(filename).read(blocksize)
    except:
      return 0


    if "\0" in s:
        return 0

    if not s:  # Empty files are considered text
        return 1

    # Get the non-text characters (maps a character to itself then
    # use the 'remove' option to get rid of the text characters.)
    t = s.translate(_eins_trans)

    # count #
    n = 0
    for i in range(len(t)):
        if( t[i] == '#'):
            n += 1
        #endif
    #endfor


    # If more than 30% non-text characters, then
    # this is considered a binary file
    if n/len(s) < 0.30:
        return 0
    return 1

def copy_build_path(s_path_filename,t_path_filename,silent=1):

  # source prüfen
  if( not os.path.isfile(s_path_filename)):
    return NOT_OK

  # Zielpfad extrahieren
  (t_path,t_body,t_ext) = file_split(t_path_filename)

  # Zielpfad pruefen
  if( not os.path.isdir(t_path) ):
    try:
      os.makedirs(t_path)
    except OSError:
      print("copybackup.backup_walk_tree.error: os.makedir(\"%s\") not possible" % t_path)
      return NOT_OK

  return copy(s_path_filename,t_path_filename,silent)

def copy(s_path_filename,t_path_filename,silent=1):
  """
    kopiert s_path_filename nach t_path_filename return hfkt.OK=1
  """

  if( not os.path.isfile(s_path_filename)):
    return NOT_OK

  len_name = len(s_path_filename)

  copy_flag = 0

  (pathname,body,ext) = file_split(t_path_filename)

  if( not os.path.isdir(pathname)):
    print("target_path: \"%s\" does not exist" % pathname)
    return NOT_OK

  if( not os.path.isfile(t_path_filename)):
    copy_flag = 1
    copy_text = "New"
  else:
    # maketime abfragen
    s_mtime = int(os.path.getmtime(s_path_filename))
    t_mtime = int(os.path.getmtime(t_path_filename))

    if( s_mtime > t_mtime ):
      copy_flag = 1
      copy_text = "OVERWRITE"

  if( copy_flag ):

    if( len_name > 32 ):
      # print "Voricht: Filenamelänge >32 File <%s>" % s_path_filename
      # print "Vorsicht:len>32"
      # print s_path_filename
      # copy_flag = 0
      if( not silent ):
        print("%s(len>32): %s->%s" % (copy_text,s_path_filename,t_path_filename))

      bytelength = os.path.getsize(s_path_filename)
      stime=os.path.getmtime(s_path_filename)
      try:
        fileobj  = open(s_path_filename, mode='rb')
        try:
          fileobj1 = open(t_path_filename, mode='wb')

          while(bytelength):
            binvalues = array.array('B')
            if( bytelength<1048576 ):
              binvalues.fromfile(fileobj, bytelength)
              bytelength = 0
            else:
              binvalues.fromfile(fileobj, 1048576)
              bytelength -= 1048576

            binvalues.tofile(fileobj1)

          fileobj.close()
          fileobj1.close()
          os.utime(t_path_filename,(-1,stime))
        except IOError:
          print("warning: (IOError) open file %s was not possible" % t_path_filename)
          return NOT_OK
        except WindowsError:
          print("warning: (WindowsError) open file %s was not possible" % t_path_filename)
          return NOT_OK
      except IOError:
        print("warning: (IOError) open file %s was not possible" % s_path_filename)
        return NOT_OK
      except WindowsError:
        print("warning: (WindowsError) open file %s was not possible" % s_path_filename)
        return NOT_OK

    else:
        if( not silent ):
          print("%s: %s->%s" % (copy_text,s_path_filename,t_path_filename))
        while( copy_flag != 3 ):
          try:
            shutil.copy2(s_path_filename,t_path_filename)
            # os.system("copy " + '"' + s_path_filename + '"' + " " + '"' + t_path_filename + '"' + " > .log")
            # os.system("copy " + '"' + s_path_filename + '"' + " " + '"' + t_pathfilename + '"')
            copy_flag = 3
          except IOError:
            copy_flag += 1
            #print "warning: (IOError) copy file %s was not possible" % s_path_filename
          except WindowsError:
            copy_flag += 1
            #print "warning: (WindowsError) copy file %s was not possible" % s_path_filename

          if(copy_flag != 3 ):
            if( os.path.isfile(t_path_filename) ):
              try:
                os.remove(t_path_filename)
                #os.rename(t_path_filename,"M_"+t_path_filename)
              except IOError:
                print("warning: (IOError) copy/delete file %s was not possible" % t_path_filename)
                copy_flag = 3
              except WindowsError:
                print("warning: (WindowsError) copy/delete file %s was not possible" % t_path_filename)
                copy_flag = 3
            else:
              copy_flag = 3
              print("warning: copy file %s was not possible" % s_path_filename)
    if( not silent ):
      print("-----------------------------------------------------------")
    return OK
  else:
    return NOT_OK

def make_backup_file(fullfilename,backup_dir):
    """
    builds from fullfilename a backup filename with actual date and copies the file

    return (flag,errText)
    if( flag == OK) => no Text
    if( flag == NOT_OK) => error text

    """

    errText = ""

    if( not os.path.isfile(fullfilename)):
        errText = "make_backup_file File: <%s> does not exist" % fullfilename
        return (NOT_OK,errText)

    if( not os.path.isdir(backup_dir)):
        errText = "make_backup_file Backup Dir: <%s> does not exist" % backup_dir
        return (NOT_OK,errText)


    (path,fbody,ext) = get_pfe(fullfilename)
    backup_file_name = os.path.join(backup_dir,fbody+"_"+str(int_akt_datum())+"_"+str(int_akt_time())+"."+ext)
    try:
        flag = copy(fullfilename,backup_file_name,silent=1)
    except:
        flag = NOT_OK

    if( flag == NOT_OK):
        errText = "copy(%s,%s) did not function" % (fullfilename,backup_file_name)

    return(flag,errText)

def move_file(s_filename,targetdir):
  """
    verschiebt s_filename nach target-path
  """
  if( os.path.isfile(s_filename) and os.path.isdir(targetdir) ):
    try:
      (path,fbody,ext) = get_pfe(s_filename)
      t_filename = join(targetdir,fbody+"."+ext)
      status = copy(s_filename,t_filename,silent=1)
      if( status == OK ): os.remove(s_filename)

      return OK

    except IOError:
      print("warning: (IOError) copy/delete file %s was not possible" % s_filename)
      return NOT_OK
    except WindowsError:
      print("warning: (WindowsError) copy/delete file %s was not possible" % s_filename)
      return NOT_OK

  return NOT_OK

def change_text_in_file(filename,textsearch,textreplace):
  if( not os.path.isfile(filename) ):
    print("Fehler change_words_in_file: zu bearbeitende Datei <%s> konnte nicht gefunden werden" % filename)
    exit(1)
  if(  isinstance(textsearch, str) \
    and isinstance(textreplace, str) ):

    f = file(filename,"r")
    lines = f.readlines()
    f.close()
    flag = False
    for i in range(len(lines)):
      if( such(lines[i],textsearch,'vs') > -1 ):
        flag = True
        line = change(lines[i],textsearch,textreplace)
        lines[i] = line

    if( flag ):
      f = file(filename,"w")
      for line in lines:
        f.write("%s" % line )
      f.close()
# check_path(pathname)
# check if exist, if not build path
def check_path(pathname):
  if not os.path.isdir(pathname):
    return build_path(pathname)
  else:
    return OK
def build_path(pathname):
  if not os.path.isdir(pathname):
    try:
      os.makedirs(pathname)
      return OK
    except OSError:
      print("Das Zielverzeichnis %s kann nicht erstellt werden" % pathname)
      return NOT_OK
  else:
    return True
#
# löscht Inhalt des Pfades
def clear_path(pathname):
  if os.path.isdir(pathname):
    try:
      shutil.rmtree(pathname)
      build_path(pathname)
      return OK
    except OSError:
      print("Das Zielverzeichnis %s kann nicht gelöscht werden" % pathname)
      return NOT_OK
  return OK
###################################################################################
# Eingabe/Ausgabe
###################################################################################
class slistbox1:
    def __init__(self,master,liste,smode="E"):
        self.name = "Listbox"
        self.items = []

        self.toplevel=Toplevel(master)
        self.toplevel.withdraw()

        self.result=StringVar()

        self.listbox = Listbox(self.toplevel,selectmode=smode)
        self.listbox.pack()

##        # liste auf Höhe und Breite auswerten
##        lh = len(liste)
##        lw = 0
##        for item in liste:
##            lw = max(lw,len(item))
##        if lh > 30 :
##            lh1     = 30
##            hscroll = 1
##        else:
##            lh1     = lh
##            hscroll = 0
##
##        if( smode == "S" or smode == "s" ):
##            selectm = SINGLE
##        else:
##            selectm = EXTENDED
##
##        # Listbox erstellen
##        if hscroll > 0: # mit vertikal Scrollbar
##
##            hscrollbar = Scrollbar(self.toplevel)
##            hscrollbar.pack(side=RIGHT, fill=Y)
##            self.listbox = Listbox( self.toplevel,height=lh1, width=30
##                                  , selectmode=selectm
##                                  , yscrollcommand=hscrollbar.set)
##            self.listbox.pack(side=LEFT, fill=Y)
##            hscrollbar.config(command=self.listbox.yview)
##
##        else: # ohne Scrollbar
##            self.listbox = Listbox( self.toplevel,height=lh1, width=30
##                                  , selectmode=selectm)
##            self.listbox.pack()

        # Button
        self.button1 = Button(self.toplevel,text="OK",command=self.get_items_and_close)
        self.button1.pack()

        # Bindung zum Beenden
        #self.listbox.bind_all('<Return>', self.get_items_and_close)

        # Listbox füllen
        for item in liste:
            print("items: %s" % item)
            self.listbox.insert('end',item)



    def get_items_and_close(self):

        # items abfragen
        print("a")
        items = self.listbox.curselection()

        # wegen alter Version transformieren
        try:
            items = map(string.atoi, items)
        except ValueError: pass

        print("b")
        # items an struktur übergeben
        for i in items:
            self.items.append(int(i))

        # widget löschen
        self.result.set("ok")
        self.toplevel.destroy()
        print("d")
        return

class DirList:
    def __init__(self, w,master_dir=None,comment=None,no_new_dir_flag=False):
        self.root = w
        self.exit = -1
        self.quit_flag = False
        if master_dir:
            dir_start = master_dir
        else:
            dir_start = "C:\\"

        ## print dir_start
        if not os.path.exists(dir_start):
            dir_start = "C:\\"

        z = w.winfo_toplevel()
        if( comment ):
            z.wm_title(comment)
        else:
            z.wm_title("choose dir")

        # Create the tkinter.tixDirList and the tixLabelEntry widgets on the on the top
        # of the dialog box

        # bg = root.tk.eval('tix option get bg')
        # adding bg=bg crashes Windows pythonw tk8.3.3 Python 2.1.0

        top = tkinter.tix.Frame( w, relief=RAISED, bd=1)

        # Create the DirList widget. By default it will show the current
        # directory
        #
        #
        top.dir = tkinter.tix.DirList(top)
        top.dir.chdir(dir_start)
        top.dir.hlist['width'] = 40

        # When the user presses the ".." button, the selected directory
        # is "transferred" into the entry widget
        #
        top.btn = tkinter.tix.Button(top, text = "  >>  ", pady = 0)

        # We use a LabelEntry to hold the installation directory. The user
        # can choose from the DirList widget, or he can type in the directory
        # manually
        #
        if( not no_new_dir_flag ):
            label_text = "chosen Directory (and add name for new dir):"
        else:
            label_text = "chosen Directory:"
        top.ent = tkinter.tix.LabelEntry(top, label=label_text,
                                  labelside = 'top',
                                  options = '''
                                  entry.width 50
                                  label.anchor w
                                  ''')
        self.entry = top.ent.subwidget_list["entry"]

        font = self.root.tk.eval('tix option get fixed_font')
        # font = self.root.master.tix_option_get('fixed_font')
        top.ent.entry['font'] = font

        self.dlist_dir = copy.copy("")

        # This should work setting the entry's textvariable
        top.ent.entry['textvariable'] = self.dlist_dir
        top.btn['command'] = lambda dir=top.dir, ent=top.ent, self=self: \
                             self.copy_name(dir,ent)

        # top.ent.entry.insert(0,'tix'+`self`)
        top.ent.entry.bind('<Return>', lambda  dir=top.dir, ent=top.ent, self=self: self.okcmd (dir,ent) )

        top.pack( expand='yes', fill='both', side=TOP)
        top.dir.pack( expand=1, fill=BOTH, padx=4, pady=4, side=LEFT)
        top.btn.pack( anchor='s', padx=4, pady=4, side=LEFT)
        top.ent.pack( expand=1, fill=X, anchor='s', padx=4, pady=4, side=LEFT)

        # Use a ButtonBox to hold the buttons.
        #
        box = tkinter.tix.ButtonBox (w, orientation='horizontal')
##        box.add ('ok', text='Ok', underline=0, width=6,
##                     command = lambda self=self: self.okcmd () )
        box.add ('ok', text='Ok', underline=0, width=6,
                     command = lambda  dir=top.dir, ent=top.ent, self=self: self.okcmd (dir,ent) )
        box.add ('cancel', text='Cancel', underline=0, width=6,
                     command = lambda self=self: self.quitcmd () )

        box.pack( anchor='s', fill='x', side=BOTTOM)
        z.wm_protocol("WM_DELETE_WINDOW", lambda self=self: self.quitcmd())

    def copy_name (self, dir, ent):
        # This should work as it is the entry's textvariable
        self.dlist_dir = dir.cget('value')
        # but it isn't so I'll do it manually
        ent.entry.delete(0,'end')
        ent.entry.insert(0, self.dlist_dir)

##    def okcmd (self):
##        # tixDemo:Status "You have selected the directory" + $self.dlist_dir
##
##        self.quitcmd()
    def okcmd (self, dir, ent):
        # tixDemo:Status "You have selected the directory" + $self.dlist_dir

        value = self.entry.get()
        if len(value) > 0:
            self.dlist_dir = value
        else:
            self.dlist_dir = dir.cget('value')

        # but it isn't so I'll do it manually
        ent.entry.delete(0,'end')
        ent.entry.insert(0, self.dlist_dir)

        self.exit = 0

    def quitcmd (self):
        # self.root.destroy()
#        print "quit"
        self.exit = 0
        self.quit_flag = True

    def mainloop(self):
        while self.exit < 0:
            self.root.tk.dooneevent(TCL_ALL_EVENTS)
        # self.root.tk.dooneevent(TCL_DONT_WAIT)

    def destroy (self):
        self.root.destroy()

class SFileSelectBox:
    TCL_ALL_EVENTS = 0
    SELECTED_FILE  = ""
    def __init__(self, w, file_types = None, text = None, start_dir = None):

        self.root = w
        self.exit_flag = False

        z = w.winfo_toplevel()

        #Extension
        if( not file_types or (not isinstance(file_types, str)) ):
            file_types = "*.*"

        #Comment
        if( text and isinstance(text, str) ):
            z.wm_title(text)
        else:
            z.wm_title("Waehle eine Datei aus")

        # Start Directory
        if( not start_dir or not isinstance(start_dir, str) ):
            start_dir = os.getcwd()



        top = tkinter.tix.Frame( w            \
                       , relief=FLAT  \
                       , bd=20        \
                       )

        top.fselect = tkinter.tix.FileSelectBox( top                \
                                       , dir=start_dir      \
                                       , pattern=file_types \
                                       )


        top.okbtn = tkinter.tix.Button(top                                 \
                              ,text='Ok'                           \
                              ,width = 10                          \
                              ,command=lambda x=top: self.okcmd(x) \
                              )

        top.quitbtn = tkinter.tix.Button(top                                   \
                                ,text='Quit'                           \
                                ,width = 10                            \
                                ,command=lambda x=top: self.quitcmd(x) \
                                )

        top.pack( expand='yes' \
                , fill='both'  \
                , side=TOP     \
                )

        top.fselect.pack( expand=1  \
                        , fill=BOTH \
                        , padx=4    \
                        , pady=4    \
                        , side=LEFT \
                        )

        top.okbtn.pack( side=tkinter.tix.BOTTOM )

        top.quitbtn.pack( side=tkinter.tix.BOTTOM )

        z.wm_protocol( "WM_DELETE_WINDOW"            \
                     , lambda x=top: self.quitcmd(x) \
                     )

    def okcmd(self,top):
        self.SELECTED_FILE = top.fselect.cget('value')
        self.exit_flag     = True

    def quitcmd (self,top):
        self.SELECTED_FILE = ""
        self.exit_flag     = True

    def mainloop(self):
        while not self.exit_flag:
            self.root.tk.dooneevent(TCL_ALL_EVENTS)

    def destroy (self):
        self.root.destroy()


class SStringBox:
    TCL_ALL_EVENTS = 0
    SELECTED_STRING  = ""
    def __init__(self, w, text = None):

        self.root = w
        self.exit_flag = False

        z = w.winfo_toplevel()

        #Comment
        if( text and isinstance(text, str) ):
            z.wm_title(text)
        else:
            z.wm_title("Text eingeben")

        top = tkinter.tix.Frame( w            \
                       , relief=FLAT  \
                       , bd=20        \
                       )

        top.combo = tkinter.tix.ComboBox( top )
        top.combo.entry['state'] = "normal"
        top.combo['editable']    = True


        top.okbtn = tkinter.tix.Button(top                                 \
                              ,text='Ok'                           \
                              ,width = 10                          \
                              ,command=lambda x=top: self.okcmd(x) \
                              )

        top.quitbtn = tkinter.tix.Button(top                                   \
                                ,text='Quit'                           \
                                ,width = 10                            \
                                ,command=lambda x=top: self.quitcmd(x) \
                                )

        top.pack( expand='yes' \
                , fill='both'  \
                , side=TOP     \
                )

        top.combo.pack( expand=1  \
                        , fill=BOTH \
                        , padx=4    \
                        , pady=4    \
                        , side=LEFT \
                        )

        top.okbtn.pack( side=tkinter.tix.BOTTOM )

        top.quitbtn.pack( side=tkinter.tix.BOTTOM )

        z.wm_protocol( "WM_DELETE_WINDOW"            \
                     , lambda x=top: self.quitcmd(x) \
                     )

    def okcmd(self,top):
        self.SELECTED_STRING = top.combo['selection']
        self.exit_flag     = True

    def quitcmd (self,top):
        self.SELECTED_STRING = ""
        self.exit_flag     = True

    def mainloop(self):
        while not self.exit_flag:
            self.root.tk.dooneevent(TCL_ALL_EVENTS)

    def destroy (self):
        self.root.destroy()


class slistbox(Tk):
    def __init__(self,liste,smode="E"):
        Tk.__init__(self)
        self.name = "Listbox"
        self.items = []

        # liste auf Höhe und Breite auswerten
        lh = len(liste)
        lw = 0
        for item in liste:
            lw = max(lw,len(item))
        if lh > 30 :
            lh1     = 30
            hscroll = 1
        else:
            lh1     = lh
            hscroll = 0

        if( smode == "S" or smode == "s" ):
            selectm = SINGLE
        else:
            selectm = EXTENDED

        # Listbox erstellen
        if hscroll > 0: # mit vertikal Scrollbar

            hscrollbar = Scrollbar(self)
            hscrollbar.pack(side=RIGHT, fill=Y)
            self.listbox = Listbox( self,height=lh1, width=30
                                  , selectmode=selectm
                                  , yscrollcommand=hscrollbar.set)
            self.listbox.pack(side=LEFT, fill=Y)
            hscrollbar.config(command=self.listbox.yview)

        else: # ohne Scrollbar
            self.listbox = Listbox( self,height=lh1, width=30
                                  , selectmode=selectm)
            self.listbox.pack()

        # Bindung zum Beenden
        self.listbox.bind_all('<Return>', self.get_items_and_close)

        # Listbox füllen
        for item in liste:
            self.listbox.insert('end',item)

    def get_items_and_close(self, event):

        # items abfragen
        items = self.listbox.curselection()

        # wegen alter Version transformieren
        try:
            items = map(string.atoi, items)
        except ValueError: pass

        # items an struktur übergeben
        for i in items:
            self.items.append(int(i))

        # widget löschen
        self.listbox.destroy()
        return

class SOkCancelBox:
    TCL_ALL_EVENTS = 0
    def __init__(self, w, comment = None):

        self.root = w
        self.exit_flag = False
        self.OK_FLAG = False

        z = w.winfo_toplevel()


        #Comment
        if( comment and isinstance(comment, str) ):
            pass
        else:
            comment = "Ok oder Cancel"




        top = tkinter.tix.Frame( w            \
                       , relief=FLAT  \
                       , bd=20        \
                       )

##        top.label = tkinter.tix.Label(w, padx=20, pady=10, bd=1, relief=tkinter.tix.RAISED, \
##                              anchor=tkinter.tix.CENTER, text=comment)
        top.label = tkinter.tix.Label(top, bd=1, relief=tkinter.tix.RAISED, \
                              anchor=tkinter.tix.CENTER, text=comment)

        top.okbtn = tkinter.tix.Button(top                                 \
                              ,text='Ok'                           \
                              ,width = 10                          \
                              ,command=lambda x=top: self.okcmd(x) \
                              )

        top.quitbtn = tkinter.tix.Button(top                                   \
                                ,text='Cancel'                           \
                                ,width = 10                            \
                                ,command=lambda x=top: self.quitcmd(x) \
                                )

        top.pack( expand='yes' \
                , fill='both'  \
                , side=TOP     \
                )

        top.label.pack( side=tkinter.tix.TOP )
        top.okbtn.pack( side=tkinter.tix.BOTTOM )

        top.quitbtn.pack( side=tkinter.tix.BOTTOM )

        z.wm_protocol( "WM_DELETE_WINDOW"            \
                     , lambda x=top: self.quitcmd(x) \
                     )

    def okcmd(self,top):
        self.OK_FLAG = True
        self.exit_flag     = True

    def quitcmd (self,top):
        self.OK_FLAG = False
        self.exit_flag     = True

    def mainloop(self):
        while not self.exit_flag:
            self.root.tk.dooneevent(TCL_ALL_EVENTS)

    def destroy (self):
        self.root.destroy()


def abfrage_listbox1(liste,smode):
    """ Listenabfrage Auswahl eines oder mehrere items aus einer Liste
        Beispiel:
        liste       Liste von auszuwählend
        smode       "s" singlemode, nur ein item darf ausgewählt werden
                    "e" extended mehrere items
        Beispiel
        liste = ["Dieter","Roland","Dirk"]
        items = abfrage_listbox1(liste,"s")
        for item in items:
            print "\nName: %s" % liste[item]
    """

    tk = Tk()
    t = slistbox1(tk,liste,smode)

    return t.items

def abfrage_listbox(liste,smode):
    """ Listenabfrage Auswahl eines oder mehrere items aus einer Liste
        Beispiel:
        liste       Liste von auszuwählend
        smode       "s" singlemode, nur ein item darf ausgewählt werden
                    "e" extended mehrere items
        Beispiel
        liste = ["Dieter","Roland","Dirk"]
        items = abfrage_listbox(liste,"s")
        for item in items:
            print "\nName: %s" % liste[item]
    """

    t = slistbox(liste,smode)
    t.mainloop()
    return t.items

def abfrage_liste_scroll(liste,comment=None,cols=70,rows=20,multi=0):
    """ Listenabfrage Auswahl eines items aus einer Liste
        wobei auf dem Bildschirm gescrollt wird (da lange und groß)
        Wenn kein Wert zurückgegeben, dann okay = 0
        Es werden die Werte in einer Liste zurückgegeben, wenn multi=1
        Es können mehrere Werte getrennt mit Komma eingeben werden.
        Es wird nur ein Wert zurückgegeben, wenn multi=0 (default)

        Beispiel:
        liste = (("","Ueberschrift"),
                 ("a","Daten einladen/anlegen"),
                 ("b","Daten speichern"),
                 ("c","Daten speichern"))
        (val,okay) = abfrage_liste(liste,cols=70,rows=20,multi=0)

        EIngabe a   => val = "a", okay = 1

        val = abfrage_liste(liste,cols=70,rows=20,multi=0)
        Eingabe a,c => val = ["a","c"]
        oder Beispiel:
        liste = ("Daten einladen/anlegen",
                 "Daten speichern",
                 "Daten speichern")
        val = abfrage_liste(liste,cols=70,rows=20)

        EIngabe 1   => val = 0, okay = 1

    """
    icount = 0
    if(  not isinstance(liste[0],list) \
      and not isinstance(liste[0],tuple) ):

        nliste = []
        ic     = 0
        for item in liste:
            ic = ic+1
            nliste.append(["%i"%ic,item])
        liste = nliste
##        print "abfrage_liste.error: liste hat nicht das korrekte Format"
##        print liste
##        return None

    # alles in eine Liste formatieren
    lliste = []
    if( comment ):
        t = comment
        while len(t) > 0:
            idum = min(cols,len(t))
            tdum = t[0:idum]
            lliste.append(tdum)
            t    = t[idum:len(t)]

    auswahl_liste = []
    for i in range(len(liste)):

        if( isinstance(liste[i][0], str) ):
            tdum = "%3s" % liste[i][0]
        elif( isinstance(liste[i][0],int) ):
            tdum = "%3i" % liste[i][0]
        elif( isinstance(liste[i][0],float) ):
            tdum = "%3f" % liste[i][0]
        else:
            print("abfrage_liste_scroll.error: liste[i][0] hat nicht das korrekte Format")
            print(liste[i][0])
            return None, 0
        auswahl_liste.append(elim_ae(tdum,' '))
        if( len(tdum) > 0 ):
            tdum = tdum + ":"
        else:
            tdum = tdum + " "

        t = liste[i][1]

        while len(t) > 0:
            if( len(tdum) >= cols ):
                lliste.append(tdum)
                tdum = "    "
            l1 = min(cols-len(tdum),len(t))
            tdum = tdum + t[0:l1]
            t    = t[l1:len(t)]
        lliste.append(tdum)

    end_sign = "e"
    while( end_sign in auswahl_liste ):
        end_sign = end_sign+"e"

    iact = 0
    inp  = "-"
    l1   = len(lliste)
    while( True ):

        if( inp == "+" ):
            iact = min(iact + rows - 1,max(0,l1-rows+1))
        if( inp == "-" ):
            iact = max(0,iact-rows+1)

        ic = iact
        ie = max(min(l1,iact+rows-1),0)
        while( ic < ie ):
            print(lliste[ic])
            ic = ic + 1
        if( l1 > rows-1 ):
            if( multi ):
                inp = raw_input("<+,-,"+end_sign+",Wert(e)(,)> : ")
            else:
                inp = raw_input("<+,-,"+end_sign+",Wert> : ")
        else:
            if( multi ):
                inp = raw_input("<"+end_sign+",Wert(e)(,)> : ")
            else:
                inp = raw_input("<"+end_sign+",Wert> : ")

        if( inp == end_sign ):
            return None, 0

        if( not(l1 > rows-1 and (inp == "+" or inp == "-")) ):
            if( multi ):
                linp = inp.split(",")
                liste = []
                for inp in linp:
                    if( inp in auswahl_liste and len(inp) > 0):
                        for i in range(len(auswahl_liste)):
                            if( inp == auswahl_liste[i] ):
                                liste.append(inp)
                if( len(liste) > 0 ):
                    return liste, 1
            else:
                if( inp in auswahl_liste and len(inp) > 0 ):
                    for i in range(len(auswahl_liste)):
                        if( inp == auswahl_liste[i]):
                            return inp, 1

def abfrage_liste(liste,comment=None):
    """ Listenabfrage Auswahl eines items aus einer Liste
        Beispiel:
        liste = (("0","Daten einladen/anlegen"),
                 ("1","Daten speichern"),
                 ("","Ist nur Kommenatr"))
        val = abfrage_liste(liste)
    """
    icount = 0
    if(  not isinstance(liste[0],list) \
      and not isinstance(liste[0],tuple) ):

        print("abfrage_liste.error: liste hat nicht das korrekte Format")
        print(liste)
        return None
    else:
        print(" ")
        if( comment ):
            print(comment)
            print(" ")
        while icount < 10:
            icount = icount + 1
            for i in range(0,len(liste),1):

                if( isinstance(liste[i][0], str)  ):
                    print("%3s  %s" % (liste[i][0],liste[i][1]))
                elif( isinstance(liste[i][0], int) ):
                    print("%3i  %s" % (liste[i][0],liste[i][1]))
                elif( isinstance(liste[i][0], float) ):
                    print("%3f  %s" % (liste[i][0],liste[i][1]))
                else:
                    print("abfrage_liste.error: liste[i][0] hat nicht das korrekte Format")
                    print(liste[i][0])
                    return None

            x = input("\nAuswahl : ")

            if( len(x) > 0 ):

                for i in range(0,len(liste),1):

                    if( isinstance(liste[i][0], str) ):
                        if x == liste[i][0]:
                            return x
                    elif( isinstance(liste[i][0], int) ):
                        if int(x) == liste[i][0]:
                            return int(x)
                    elif( isinstance(liste[i][0], float) ):
                        if( abs(float(x)-liste[i][0]) < 1.0e-10 ):
                            return float(x)

            print("\nFalsche Eingabe: %s " % x)
        return None
    return None

def eingabe_int(comment):
    """ Eingabe von einem int-Wert mit Kommentar abgefragt """
    ival = int(0)
    io_flag = 0
    c = comment+" : "
    while( io_flag == 0 ):
        x = raw_input(c)
        if( len(x) == 0 ):
            ival = int(0)
        else:
            try:
                ival = int(x)
                io_flag = 1
            except ValueError:
                print("\n Falsche Eingabe, nochmal !!!")

    return ival
def eingabe_string(comment):
    """ Eingabe von string-Wert mit Kommentar abgefragt """
    strval = ""
    io_flag = 0
    c = comment+" : "
    while( io_flag == 0 ):
        x = raw_input(c)
        if( len(x) > 0 ):
            try:
                strval = x
                io_flag = 1
            except ValueError:
                print("\n Falsche Eingabe, nochmal !!!")

    return strval
def eingabe_float(comment):
    """ Eingabe von einem float-Wert mit Kommentar abgefragt """
    fval = float(0)
    io_flag = 0
    c = comment+" : "
    while( io_flag == 0 ):
        x = raw_input(c)
        if( len(x) == 0 ):
            fval = float(0)
        else:
            try:
                fval = float(x)
                io_flag = 1
            except ValueError:
                print("\n Falsche Eingabe, nochmal !!!")

    return fval

def eingabe_jn(comment,default=None):
    """ Eingabe von einem ja oder nein-Wert mit Kommentar abgefragt
        comment: Kommentar
        default: True oder "ja", False oder "nein"
        Rückgabe True (ja) oder False (nein)
    """
    if( default != None ):
        if( isinstance(default, bool) ):
            if( default ):
                def_sign = 'j'
            else:
                def_sign = 'n'
        elif( isinstance(default, str) ):
            if( default[0] == 'j' or default[0] == 'J' or \
                default[0] == 'y' or default[0] == 'Y' ):
                def_sign = 'j'
            else:
                def_sign = 'n'
        else:
            def_sign = 'n'

    if( default != None ):
        frage = "j/n <def:%s> : " % def_sign
    else:
        frage = "j/n : "

    io_flag = 0
    while( io_flag == 0 ):
        print(comment)
        x = raw_input(frage)
        if( len(x) == 0 ):
            if( default != None ):
                x = def_sign
            else:
                x = 'p'
        if( x[0] == 'j'  or x[0] == 'J' or \
            x[0] == 'y' or x[0] == 'Y' ):

            erg = True
            io_flag = 1
        elif( x[0] == 'n'  or x[0] == 'N' ):
            erg = False
            io_flag = 1
        else:
            print("\n Falsche Eingabe, nochmal !!!")

    return erg
#===============================================================================
def abfrage_ok_box(text="Ist das okay"):
    ''' Fragt nach einem Ja für Okay
        return True, wenn okay (ja)
        return False, wenn nicht okay (nein)
    '''
    ans = tkinter.messagebox.askyesno(title='OkBox', message=text)

    return ans

def abfrage_dir(comment=None,start_dir=None):
    """ gui für Pfad auszuwählen """
##    import tkinter.messagebox, traceback, tkinter.tix
##
##    global dirlist
##
##    dirname = None
##
##    if( not start_dir ):
##        start_dir = abfrage_root_dir()
##
##    try:
##        root=tkinter.tix.Tk()
##        dirlist = DirList(root,start_dir,comment)
##        dirlist.mainloop()
##
##        if( dirlist.quit_flag ):
##            dirlist.destroy()
##            return None
##
##        if( dirlist.dlist_dir == "" ):
##            dirname = None
##        else:
##            dirname = dirlist.dlist_dir+"\\"
##        dirlist.destroy()
##
##        if(dirname and not os.path.exists(dirname) ):
##
##            comment = "Soll das Verzeichnis <%s> angelegt werden??" % dirname
##            if( abfrage_ok_box(comment) ):
##                os.makedirs(dirname)
##            else:
##                dirname = None
##
##        return dirname
##    except:
##        t, v, tb = sys.exc_info()
##        dirname = None
##        text = "Error running the demo script:\n"
##        for line in traceback.format_exception(t,v,tb):
##            text = text + line + '\n'
##            d = tkinter.messagebox.showerror ( 'tkinter.tix Demo Error', text)
##    return dirname
    root = Tk()
    dir = tkinter.filedialog.askdirectory(master=root, title=comment, initialdir=start_dir)
    root.destroy()
    dir = change(dir,"/",os.sep)
    return dir
def abfrage_sub_dir(comment=None,start_dir=None):
    """ gui für Unterpfad von start_dir auszuwählen """
    import tkinter.messagebox, traceback, tkinter.tix

    global dirlist

    if( not os.path.exists(start_dir) ):

        print("Das angegebene Start-Verzeichnis <%s> exsistiert nicht !!!" % start_dir)
        return None

    dirname = None
    dir_not_found = True
    while dir_not_found :
        try:
            root=tkinter.tix.Tk()
            dirlist = DirList(root,start_dir,comment,True)
            dirlist.mainloop()

            if( dirlist.quit_flag ):
                dirlist.destroy()
                return None

            if( dirlist.dlist_dir == "" ):
                dirname = None
            else:
                dirname = dirlist.dlist_dir+"\\"
            dirlist.destroy()
        except:
            t, v, tb = sys.exc_info()
            dirname = None
            text = "Error running the demo script:\n"
            for line in traceback.format_exception(t,v,tb):
                text = text + line + '\n'
                d = tkinter.messagebox.showerror ( 'tkinter.tix Demo Error', text)

        dirname = change_max(dirname,"/","\\")

        if( (not os.path.exists(dirname))           or \
            (not dirname)                           or \
            (such(string.lower(dirname),          \
                  string.lower(start_dir),"vs") != 0) ):

            print("Verzeichnis <%s> liegt nicht in der start_dir <%s>" \
                  % (dirname,start_dir))
        else:
            dir_not_found = False


    return dirname
##def abfrage_root_dir():
##    """
##        abfrage_root_dir: welche root-dir will man
##        erstellt erst ein Liste möglicher root-dirs (aber nur mit einem Buchstaben)
##        und fragt nach der gewünschten
##    """
##    abc_str ="abcdefghijklmnopqrstuvwxyz"
##    liste = []
##    for i in range(len(abc_str)):
##        item = abc_str[i]+":"
##        if( os.path.isdir(item)):
##            liste.append(item)
##    if( len(liste) > 0 ):
##        item = abfrage_listbox(liste,"s")
##    else:
##        item = None
##
##    return item
def abfrage_file(file_types="*.*",comment=None,start_dir=None,default_extension=None,file_names=None):
    """
    abfrage_file (FileSelectBox) um ein bestehendes Fiele einzuladen mit folgenden Parameter
    file_types = ["*.c","*.h"]      Filetypen (auch feste namen möglich "abc.py")
    comment    = "Suche Datei"  Windows Leisten text
    start_dir  = "d:\\abc"	Anfangspfad
    default_extension = "txt"
    file_names = ["C-Files","H-Files"]
    """
##    root = tkinter.tix.Tk()
##    f = SFileSelectBox(root,file_types,comment,start_dir)
##    f.mainloop()
##    f.destroy()
##    return f.SELECTED_FILE

    if( default_extension and such(default_extension,".","vs") != 0 ):
        default_extension = "."+default_extension


    if( isinstance(file_types, str) ):
        file_types = [file_types]

    format_liste = []
    if( file_names and isinstance(file_names, str)  ):
        file_names = [file_names]
    for i in range(len(file_types)):
        if( file_names and i < len(file_names) ):
            format_liste.append([file_types[i],file_names[i]])
        else:
            format_liste.append((file_types[i],file_types[i]))

    root = Tk()
    name = tkinter.filedialog.askopenfilename(master=root,
                                        defaultextension=default_extension,
                                        filetypes=format_liste,
                                        initialdir=start_dir,
                                        title=comment)
    root.destroy()
    name = change(name,"/",os.sep)
    if( isinstance(name, str)  ):
        name = name.encode('ascii')

    return name
def eingabe_file(file_types="*",comment="Waehle oder benenne neue Datei",start_dir=None):
    """
    eingabe_file (FileSelectBox) um ein neues File zu generieren
                                mit folgenden Parameter
    file_types = "*.c *.h"      Filetypen (auch feste namen möglich "abc.py")
    start_dir  = "d:\\abc"	Anfangspfad
    comment    = "Suche Datei"  Windows Leisten text
    """
    selected_file = None
    count = 0
    while( count < 10 ):
        count = count + 1
        root = tkinter.tix.Tk()
        f = SFileSelectBox(root,file_types,comment,start_dir)
        f.mainloop()
        f.destroy()
        print(f.SELECTED_FILE)
        selected_file = f.SELECTED_FILE
        del f


        if( os.path.exists(selected_file) ):

            if( abfrage_ok_box(text="Die Datei <%s> existiert bereits" % selected_file) == OK ):
                return selected_file
        else:
            return selected_file

def abfrage_str_box(comment="",width=400):
    root = tkinter.tix.Tk()

    geotext = str(max(width,300))+"x90"
    root.geometry(geotext)
    f = SStringBox(root,comment)
    f.mainloop()
    f.destroy()
    return f.SELECTED_STRING

###########################################################################################
# DAten lesen
##################################################################################
def read_csv_file(file_name,delim=";"):
    liste = []
    if( os.path.isfile(file_name) ):
      with open(file_name,'r') as f:

        lines = f.readlines()

        f.close()

        for line in lines:

           row = split_text(line,delim)
           liste.append(row)
        # reader = csv.reader(open(file_name, "rb"), delimiter=delim, quoting=csv.QUOTE_MINIMAL)
        # try:
        #     for row in reader:
        #         liste.append(row)

        # except csv.Error:
        #     sys.exit('file %s, line %d: %s' % (file_name, reader.line_num))

    return liste
def read_csv_file_header_data(file_name,delim=";"):
    ''' csv_header = ['name1','name2',...]
        csv_data   = [[val_name1_zeile1,val_name2_zeile1,...],[val_name1_zeile2,val_name2_zeile2,...],...]
    '''
    csv_header = []
    csv_data = []
    if(os.path.isfile(file_name)):

        csv_liste = read_csv_file(file_name, delim)

        if(len(csv_liste) < 2):
            print("csv Datei <%s> zu klein (1. Zeile Header, weitere Zeilen Vokabeln)" % file_name)
            return (NOT_OK, csv_header, csv_data)
        # header separieren
        csv_header = csv_liste[0]
        n = len(csv_header)
        # Alle Spalten so groß wie header line
        csv_data = []
        for i in range(1, len(csv_liste), 1):
            row = csv_liste[i]
            m = len(row)
            if(m > n):
                del row[n:m]
            elif(m < n):
                for j in range(m, n, 1):
                    row.append("")
            csv_data.append(row)
    else:
        print("DAtei nicht vorhanden <%s>" % file_name)
        return (NOT_OK, csv_header, csv_data)

    return (OK, csv_header, csv_data)

def write_csv_file_header_data(file_name,csv_header,csv_data,delim=";"):
    ''' Write DAta with
        csv_header = ['name1','name2',...]
        csv_data   = [[val_name1_zeile1,val_name2_zeile1,...],[val_name1_zeile2,val_name2_zeile2,...],...]
        in file as

        name1;name2
        val_name1_zeile1;val_name2_zeile1
        val_name1_zeile2;val_name2_zeile2

        return OK/NOT_OKAY

    '''
    f=file(file_name,"w")
    n = len(csv_header)
    for i in range(n):
      f.write(str(csv_header[i]))
      if( i+1 < n ):
        f.write(delim)
      else:
        f.write("\n")
    for line in csv_data:
      n = len(line)
      for i in range(n):
        f.write(str(line[i]))
        if( i+1 < n ):
          f.write(delim)
        else:
          f.write("\n")

    f.close()

    return OK

def read_ascii_build_list_of_lines(file_name):

    (okay,txt) = read_ascii(file_name)
    if( okay):

      lines = txt.split("\n")
      if( len(lines[-1]) == 0 ):lines = lines[:-1]
    else:
      lines = []
    #endif

    return (okay,lines)

def read_ascii(file_name):
 
    okay  = NOT_OK
    if( os.path.isfile(file_name) ):
        with open(file_name) as f:
            data = f.read()
        okay = OK
    else:
        print("Datei: "+file_name+" besteht nicht !!!")
    #endif
    return (okay,data)
##################################################################################
# Schreiben in HTML-File
##################################################################################
def html_write_start(f,title):

    if( f.closed != 1 ):
        f.write("<!DOCTYPE html>")
        # f.write("\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>")
        f.write("\n<html>\n<head>\n<title>")
        f.write(str(title))
        f.write("</title>\n</head>")
def html_write_end(f):

    if( f.closed != 1 ):
        f.write("\n</html>\n")

def html_write_Ueberschrift(f,text,size=100,font='verdana'):
    if( f.closed != 1 ):
         tt = "\n<body><h1 style=\"font-size:%i%%;font-family:%s;\">%s</h1></body>" % (size,font,text)
         f.write(str(tt))
def html_write_text(f,text,size=100,font='verdana'):
    if( f.closed != 1 ):
         tt = "\n<body><p style=\"font-size:%i%%;font-family:%s;\">%s</p></body>" % (size,font,text)
         f.write(str(tt))

def html_write_start_tab(f,title):

    if( f.closed != 1 ):

        f.write("\n\n<body>")

        if( len(title) > 0 ):
            tt = "\n<h1><font size=\"3\">%s</font></h1>" % title
            f.write(str(tt))

        f.write("\n\n<table border=1 cellspacing=0 cellpadding=0 style='border-collapse:collapse;border:none;'>")

def html_write_end_tab(f):

    if( f.closed != 1 ):
        f.write("\n\n</table>\n</body>")

def html_write_start_colgroup(f):
    if( f.closed != 1 ):
        f.write("\n<colgroup>")

def html_write_end_colgroup(f):
    if( f.closed != 1 ):
        f.write("\n</colgroup>")

def html_write_set_col_align(f,ausr):

    if( ausr[0] == "c" or ausr[0] == "C" ):
        name = "center"
    elif( ausr[0] == "r" or ausr[0] == "R" ):
        name = "right"
    else:
        name = "left"

    if( f.closed != 1 ):
        f.write("\n\n  <col align=\"%s\">" % name)



def html_write_start_tab_zeile(f):
    """ Zeilenstart einer Tabelle
        f               file-Objekt
    """


    if( f.closed != 1 ):
        f.write("\n\n  <tr>")

def html_write_end_tab_zeile(f):

    if( f.closed != 1 ):
        f.write("\n  </tr>")

def html_write_tab_zelle(f,h_flag,fett_flag,farbe,inhalt):
    """ Beschreib eine Zelle in einer Tabellezeile:
        f file-objekt
        h_flag      1/0     Headerflag
        fett_flag   1/0     Fett drucken
        farbe       string  black (def), red, blue, green, yellow, ...
        inhalt      string  Zelleninhalt, Trennung mit \n wird beachtet
    """

    if( h_flag != 0 ):
        tstr = "th"
    else:
        tstr = "td"

    if( len(farbe) == 0 ):
        farbe = "black"


    if( f.closed != 1 ):
        # start
        f.write(("\n    <%s style='border:solid windowtext 2 pt;'>" % tstr))
        # font
        f.write(("<font color=\"%s\">" % farbe))
        # fett
        if( fett_flag != 0 ):
            f.write("<b>")
        # inhalt in Zeile aufteilen
        inhalt = convert_to_unicode(inhalt)
        words = inhalt.split("\n")
        n_words = len(words)
        i_words = 0

        for word in words:
            i_words += 1
            f.write(word)
            if( i_words < n_words ):
                f.write("<br>")
        # end fett
        if( fett_flag != 0 ):
            f.write("</b>")
        # end font
        f.write("</font>")
        # end
        f.write(("</%s>" % tstr))

def html_write_leer_zeile(f,n_spalten):

    html_write_start_tab_zeile(f)

    for i in range(1,n_spalten,1):
        html_write_tab_zelle(f,0,0,"black","")

    html_write_end_tab_zeile(f)

def html_get_filename_for_browser(filename):

  return "file:///"+change_max(filename,'\\','/')
###################################################################################
# Sonstiges
###################################################################################
def str_to_float_possible(string_val):
  """
  value= str_to_float_possible(string_val)
  if okay value = float(string_val)
  else    value = None
  """
  okay = 0
  index0 = 0
  n1     = 0
  maxdig = 0
  n = len(string_val)
  try:
    return float(string_val)
  except ValueError:
    for istart in range(n):
      for nend in range(istart+1,n+1,1):
        tt = string_val[istart:nend]
        try:
          f = float(tt)
          if( len(tt) > maxdig ):
            maxdig = len(tt)
            index0 = istart
            n1     = nend
          #endif
        except ValueError:
          pass
        #endtry
      #endfor
    #endfor
  #endtry
  if( maxdig > 0 ):
    return float(string_val[index0:n1])
  else:
    return None
#enddef
def str_to_int_possible(string_val):
  """
  value= str_to_int_possible(string_val)
  if okay value = int(string_val)
  else    value = None
  """
  try:
    return int(string_val)
  except ValueError:
    value = str_to_float_possible(string_val)
    if( value ): return int(value)
    else:        return None
#enddef

dec36_dict={0 :'0', 1:'1', 2:'2', 3:'3', 4:'4', 5:'5', 6:'6', 7:'7', 8:'8', 9:'9', \
            10:'a',11:'b',12:'c',13:'d',14:'e',15:'f',16:'g',17:'h',18:'i',19:'j', \
            20:'k',21:'l',22:'m',23:'n',24:'o',25:'p',26:'q',27:'r',28:'s',29:'t', \
            30:'u',31:'v',32:'w',33:'x',34:'y',35:'z'}

def int_to_dec36(int_val,digits=0):


    int_val = int(int_val)
    liste = []
##    if( int_val < 36 ):
##
##        value = dec36_dict[int_val]
##    else:

    while True :

        if( int_val > 35 ):

            liste.append(int_val%36)
            int_val = int_val/36
        else:
            liste.append(int_val)
            break

    value = ''
    if( len(liste) < digits ):
        for i in range(digits,len(liste),-1):
            value += '0'

    try:
      for i in range(len(liste)-1,-1,-1):
          value += dec36_dict[liste[i]]

    except:
      print("wraning: int_to_dec36(int_val) ging schief")

    return value

def dec36_to_int(string_val):

    string_val = str(string_val)
    erg = 0
    for s in string_val:
        for (i0,a0) in dec36_dict.items():

            if( a0 == s ):

                erg = erg*36+i0
                break

    return erg
def summe_euro_to_cent(text,delim=","):
    """ Umrechnung Euro (Text) in Cent (int) """
    text1 = elim_ae(text," ")

    fact = 1
##    if( text1 == "-92,7" ):
##        flag = True
##    else:

    flag = False

    if( text1[0] == "-" ):
        fact  = -1
        text1 = text1[1:]
    elif( text[0] == "+" ):
        fact  = 1
        text1 = text1[1:]

    text1 = elim_ae(text1," ")

    liste = text1.split(delim)

    if( flag ):
        print(liste)
    summe = 0

    if( len(liste) >= 1 ):
        summe = summe + int(liste[0])*100

    if( len(liste) >= 2 ):
        tdum = liste[1]
        if( len(tdum) >= 1 ):
            summe = summe + int(tdum[0])*10
        if( len(tdum) >= 2 ):
            summe = summe + int(tdum[1])


    return summe*fact

def suche_in_liste(liste,gesucht):
    """ Sucht in liste nach gesucht """
#    print "start find_in_list"
#    print "gesucht:"
#    print gesucht
    for item in liste:
#        print "item:"
#        print item
        if item == gesucht:
            return True
    return False

def secs_akt_time_epoch():
  """
  Aktuelle Zeit in Sekunden seit 1.1.1970 12:00 am
  """
  return int(time.time())

def secs_time_epoch_from_int(intval,plus_hours=0):
  """
  Das Int-Datum intval (z.B. 20161217) wird in Sekunden in epochaler Zeit umgerechnet
  Zusätzlich können Stunden dazu addiert werden
  secs = secs_time_epoch_from_int(intval)
  secs = secs_time_epoch_from_int(intval,12)
  """
  liste = datum_int_to_intliste(intval)
  t = (liste[2], liste[1], liste[0], plus_hours, 0, 0, 0, 0, 0)
  return time.mktime( t )
def secs_time_epoch_from_str(str_dat,delim="."):
  """
  Das Str-Datum intval (z.B. "17.12.2004") wird in Sekunden in epochaler Zeit umgerechnet
  secs = secs_time_epoch_from_str(str_dat,delim=".")
  """
  form = "%d"+delim+"%m"+delim+"%Y"
  if( isinstance(str_dat,list) ):
    ll = []
    for stri in str_dat:
        str_to_dt = datetime.datetime.strptime(stri, form)
        ll.append(int(str_to_dt.timestamp()))
    #endfor
    return ll
  else:
    str_to_dt = datetime.datetime.strptime(str_dat, form)
    return int(str_to_dt.timestamp())
def secs_time_epoch_to_str(secs):
  """
  Wandelt epochen Zeist in secs nach Datum tt.m.yyyy
  """
  if( isinstance(secs,list)):
      strtime = []
      for seci in secs:
          strtime.append(datetime.datetime.fromtimestamp(seci).strftime("%d.%m.%Y"))
      return strtime
  else:
      dt = datetime.datetime.fromtimestamp(secs)
      return dt.strftime("%d.%m.%Y")
def secs_time_epoch_to_int(secs):
  """
  Wandelt epochen Zeit in secs nach int jjjjmmtt
  """
  return datum_str_to_int(secs_time_epoch_to_str(secs))
def datum_str_to_secs(str_dat,delim="."):
  """
  Wandelt Datum tt.m.yyyy nach epochen Zeist in secs
  """
  int_dat = datum_str_to_int(str_dat,delim)
  return secs_time_epoch_from_int(int_dat)
def int_akt_datum():
    """ Das aktuelle Datum wird in integer zurückgegeben
        format: jjjjmmtt
    """
    t = time.localtime()
    return t[2]+t[1]*100+t[0]*10000

def int_akt_time():
    """ Das aktuelle Datum wird in integer zurückgegeben
        format: hhmmss
    """
    t = time.localtime()
    return t[5]+t[4]*100+t[3]*10000

def str_akt_datum():
    """ Das aktuelle Datum wird als string zurückgegeben
        format: tt.mm.jjjj
    """
    t = time.localtime()

    st = get_str_from_int(t.tm_mday,2)+"."+get_str_from_int(t.tm_mon,2)+"."+get_str_from_int(t.tm_year,4)
    return st

def str_akt_time():
    """ Das aktuelle Zeit wird als string zurückgegeben
        format: hh.mm.ss
    """
    t = time.localtime()
    st = get_str_from_int(t.tm_hour,2)+":"+get_str_from_int(t.tm_min,2)+":"+get_str_from_int(t.tm_sec,2)
    return st
def str_datum(int_dat):
    """ Das mit int_akt_datum erstellte Datum int_dat wird als string zurückgegeben
        format: jjjjmmtt -> tt.mm.jjjj
    """
    jahr  = int(int_dat/10000)
    monat = int((int_dat - jahr*10000)/100)
    tag   = int(int_dat - jahr*10000 - monat*100)

    st = ("%s" % tag) + (".%s" % monat) + (".%s" % jahr)
    return st
#----------------------------------------------
def datum_str_is_correct(str_dat,delim="."):
    """
    string datum muss tt.mm.jjjj sein
    flag = datum_str_is_correct(str_dat)
    flag = True/False
    """
    liste  = str_dat.split(delim)

    if( len(liste) < 3 ): return False

    intliste = datum_str_to_intliste(str_dat=str_dat,delim=delim)

    if( intliste[0] > 31 ): return False
    if( intliste[1] > 12 ): return False

    return True

#----------------------------------------------
def day_from_secs_time_epoch(secs):
    """
    Man bekommt den Tag in Mo,Di,Mi,Do,Fr,Sa,So
    """
    return day_from_datum_str(secs_time_epoch_to_str(secs))
#----------------------------------------------
def day_from_datum_str(str_dat,delim="."):
    """
    Man bekommt den Tag in Mo,Di,Mi,Do,Fr,Sa,So
    von 'dd.mm.yyyy'
    """
    days =["Mo", "Di", "Mi", "Do","Fr", "Sa", "So"]
    return days[daynum_from_datum_str(str_dat,delim)]
#----------------------------------------------
def daynum_from_secs_time_epoch(secs):
    """
    Man bekommt den Tag in 0-6  0:Mo, 6:So
    """
    return daynum_from_datum_str(secs_time_epoch_to_str(secs))
#----------------------------------------------
def daynum_from_datum_str(str_dat,delim="."):
    """
    Man bekommt den Tag in 0-6  0:Mo, 6:So
    von 'dd.mm.yyyy'
    """
    day, month, year = (int(i) for i in str_dat.split(delim))
    dayNumber = calendar.weekday(year, month, day)

    return dayNumber
#----------------------------------------------
def secs_time_epoch_find_next_day(secs,idaynext):
    """
    von akt_datum (in secs_time_epoch) den nächsten Wochentag finden
    idaynext = 0:Mo - 6:So
    """
    idayact = daynum_from_secs_time_epoch(secs)

    diday = idaynext - idayact

    if( diday <= 0 ):
        diday = 7+diday

    return (secs + 86400*diday)
#----------------------------------------------
def datum_str_to_intliste(str_dat,delim="."):
    """ Das string-Datum z.B <12.5.04> wird
        in [tag,monat,jahr] gewandelt [12,5,2004]
    """
    liste  = str_dat.split(delim)

    l=[]
    for v in liste:
        l.append(int(v))

    n = len(l)

    if( n == 0 ):
        return [0,0,0]
    else:
        if( l[n-1] < 70 ):
            l[n-1] = l[n-1]+2000
        elif( l[n-1] < 100 ):
            l[n-1] = l[n-1]+1900

    if( n == 1 ):
            return [1,1,l[0]]
    if( n == 2 ):
            return [1,l[0],l[1]]
    if( n >= 3 ):
            return [l[0],l[1],l[2]]
#----------------------------------------------
def datum_intliste_to_int(dat_l):
    """ Die Liste [tag,monat,jahr] wird in int gewandelt
        [12,5,2004] => 20040512
    """
    return dat_l[0]+dat_l[1]*100+dat_l[2]*10000
#----------------------------------------------
def datum_int_to_intliste(intval):
    """ Die Zahle intval wird in eine Liste [tag,monat,jahr] gewandelt
        20040512 => [12,5,2004]
    """
    dat_l = []
    dat_l.append(intval - int(intval /100)*100)
    dat_l.append(int((intval - int(intval /10000)*10000)/100))
    dat_l.append(int(intval /10000))

    return dat_l
#----------------------------------------------
def datum_str_to_int(str_dat,delim="."):
    """
    Das string-Datum z.B <12.5.04> wird
    in eine int Zahl gewandelt z.B. 20040504
    """
    return datum_intliste_to_int(datum_str_to_intliste(str_dat,delim))

def datum_intliste_to_str(int_liste,delim="."):
    """ Wandelt Datumliste z.B (12,5,2004) in
        in "tag.monat.jahr" um "12.05.2004"
    """
    if( len(int_liste) >= 1 ):
        datum = "%2.2i"%int_liste[0] + delim
    if( len(int_liste) >= 2 ):
        datum += "%2.2i"%int_liste[1] + delim
    if( len(int_liste) >= 3 ):
        datum += "%2.2i"%int_liste[2]
    return datum
def datum_akt_year_int():
    """
    aktuelle Jahr in int z.B. 2017
    """
    t = time.localtime()
    return t.tm_year

def datum_str_to_year_int(str_dat,delim="."):
    """ Das string-Datum z.B <12.5.04> wird
        in 2004 gewandelt Nur das Jahr
    """
    liste  = str_dat.split(delim)

    year = int(liste[-1]);

    if( year < 70 ):
            year = year+2000
    elif( year < 100 ):
            year = year+1900
def datum_str_to_month_int(str_dat,delim="."):
    """ Das string-Datum z.B <12.5.04> wird
        in 5 gewandelt Nur den Monat
    """
    liste  = str_dat.split(delim)

    month = int(liste[1]);

    if( month < 1 ):
            month = 1
    elif( month > 12 ):
            month = 12

    return month
def datum_str_to_day_int(str_dat,delim="."):
    """ Das string-Datum z.B <12.5.04> wird
        in 12 gewandelt Nur den Tag
    """
    liste  = str_dat.split(delim)

    day = int(liste[0]);

    if( day < 1 ):
            day = 1
    elif( day > 31 ):
            day = 12

    return day
def is_datum_str(str_dat,delim="."):
    """ Prüft, ob str_dat ein Datum wie 01.03.2005 ist
    """
    flag = True
    liste  = str_dat.split(delim)
    try:
        if( len(liste) < 3 ): #dreiteilig
            flag = False
        elif( int(liste[0]) > 31 or int(liste[0]) < 1 ): #tag 1-31
            flag = False
        elif( int(liste[1]) > 12 or int(liste[1]) < 1 ): #monat 1-12
            flag = False
        elif( int(liste[2]) > 99 and int(liste[2]) < 1970 ): #jahr 0-99 oder 1970-20xx
            flag = False
    except:
        flag = False
    return flag
def get_name_by_dat_time(pre_text="",post_text="",form_type=0):
    """
    Gibt Name gebildet aus localtime und pre_text und post_text
    form_type == 0
    zB 25.8.2006 12:43:02 => pre_text+20060825_124302+post_text
    form_type == 1
    zB 25.8.2006 12:43:02 => pre_text+0608251243+post_text
    """
    t = time.localtime()
    text =  pre_text
    if( form_type == 0 ):
      text += "%4.4i" % t[0]  #jahr
      text += "%2.2i" % t[1]  # mon
      text += "%2.2i_" % t[2] # tag
      text += "%2.2i" % t[3]  # stunde
      text += "%2.2i" % t[4]  # minute
      text += "%2.2i" % t[5]  # sekunde
    elif( form_type == 1 ):
      jahr = t[0]-int(t[0]/100)*100
      text += "%2.2i" % jahr  #jahr
      text += "%2.2i" % t[1]  # mon
      text += "%2.2i" % t[2]  # tag
      text += "%2.2i" % t[3]  # stunde
      text += "%2.2i" % t[4]  # minute
    text += post_text
    return text

def diff_days_from_time_tuples(time_tuple_start,time_tuple_end):
  """
  Gibt an die Differenz an Tagen zu time_tuple
  z.B. time_tuple = time.localtime()
  """
  fac = 1.0
  time_sec_start = time.mktime(time_tuple_start)
  time_sec_end   = time.mktime(time_tuple_end)

  if( time_sec_start > time_sec_end ):
    fac = -1.0
    dum = time_sec_start
    time_sec_start = time_sec_end
    time_sec_end   = dum

  delta_sec = time_sec_end -time_sec_start
  days      = int(delta_sec / 86400.)
  time_tuple = time.gmtime(time_sec_start+days*86400.)
  if( (time_tuple[0] == time_tuple_end[0]) and \
      (time_tuple[1] == time_tuple_end[1]) and \
      (time_tuple[2] == time_tuple_end[2])     ):
    pass
  else:
    days += 1

  return int(days*fac)

def string_cent_in_euro(cents):
    """ Wandelt string cents in "xx,yy €"
    """
    return str(float(cents)/100)+" €"

def num_cent_in_euro(cents):
    """ Wandelt num cents in "xx,yy €"
    """
    return "%.2f"%(float(cents)/100)+" €"
def string_euro_in_int_cent(teuro,delim=","):
    """
    Wandelt einen String mit Euro z.B. "4.885,66"
    in (int)488566
    """
    # Wenn Trennzeichen nicht Punkt, dann
    # nehm den Tausender-Punkt raus
    if( delim != "." ):
        teuro = change(teuro,delim,"")
    # Tausche Trennzeichen gegen Punkt
    teuro = change(teuro,delim,".")
    # Wandele in cent
    return int(float(teuro)*100)
##def dat_last_act_workday_datelist(year=None,month=None,day=None):
##    """
##    Sucht letzten Werktag von year,month,day Ausgabe in  [year,month,day]
##    bzw. aktuelles Datum, wenn kein Argument
##    """
##    if( is_list(year) ):
##        ll = len(year)
##        if( ll >= 3 ):
##            d = datetime.date(int(year[0]) \
##                             ,max(1,min(int(year[1]),12)) \
##                             ,max(1,min(int(year[2]),)
##        elif( ll == 2 ):
##            d = datetime.date(int(year[0]),int(year[1]),1)
##        elif( ll == 1 ):
##            d = datetime.date(int(year[0]),1,1)
##    elif( year == None or month == None or day == None):
##        d = datetime.date.today()
##    else:
##        d = datetime.date(int(year),int(month),int(day))
##
##    if( d.weekday() == 5 ):    # Samstag
##        d = d + datetime.timedelta(days=-1)
##    elif( d.weekday() == 6 ):    # Sonntag
##        d = d + datetime.timedelta(days=-2)
##
##    return d.timetuple()[0:3]
##
def str_to_float(txt):
  try:
    value = float(txt)
    okay  = True
  except:
    value = 0.0
    okay = False

  return (okay,value)
###########################################################################
###########################################################################
def is_int(val):
  '''
  Prüft, ob Type int flag = True/False
  '''
  return isinstance(val, int)
def is_string(val):
  '''
  Prüft, ob Type string flag = True/False
  '''
  return isinstance(val, str)
def is_unicode(val):
  '''
  Prüft, ob Type unicode flag = True/False
  '''
  return isinstance(val, str)
def is_list(val):
  '''
  Prüft, ob Type Liste flag = True/False
  '''
  return isinstance(val,list)
def is_tuple(val):
  '''
  Prüft, ob Type Tuple flag = True/False
  '''
  return isinstance(val,tuple)
def is_dict(val):
  '''
  Prüft, ob Type Liste flag = True/False
  '''
  return isinstance(val,dict)

def isfield(c,valname):
  '''
  Prüft ob eine Klasse oder ein dict die instance hat
  '''
  flag = False
  if( isinstance(valname, str) ):
    if( not isinstance(c,dict) ):
      a = c.__dict__
      flag = valname in a
    elif( isinstance(c,dict)  ):
      flag = valname in c

  return flag

#-------------------------------------------------------
def isempty(val):
  """
  prüft nach type, ob wert leer
  """
  flag = True
  if( val == None ):
    pass
  elif( isinstance(val, str)):
    if( len(val) > 0 ): flag = False
  elif( is_list(val) or is_tuple(val) or isinstance(val,dict) ):
    if( len(val) > 0 ): flag = False
  elif( isinstance(val, int) or isinstance(val, float) ):
    flag = False
  else:
    print("hfkt.isempty kennt den type nicht: %s" % type(val))
    raise NameError('hfkt.isempty')

  return flag

#-------------------------------------------------------
def print_python_is_32_or_64_bit():
  print(struct.calcsize("P") * 8)

def multiply_constant(ll,value):
    """
    multiplies a list with const value
    """
    if( isinstance(ll,list)):
        out = []
        for i in range(len(ll)):
            if( isinstance(ll[i],str)):
                val = float(ll[i])*value
                out.append(str(val))
            else:
                out.append(ll[i] * value)
    else:
        if( isinstance(ll,str)):
            val = float(ll)*value
            out = str(val)
        else:
           out =  ll * value

    return out

def add_constant(ll,value):
    """
    add to a list const value
    """
    if( isinstance(ll,list)):
        for i in range(len(ll)):
            if( isinstance(ll[i],str)):
                ll[i] = float(ll[i]) + value
                ll[i] = str(ll[i])
            else:
                ll[i] += value
    else:
        if( isinstance(ll,str)):
            ll = float(ll) + value
            ll = str(ll)
        else:
            ll += value

    return ll

###########################################################################
# testen mit main
###########################################################################
if __name__ == '__main__':

    #secs_time_epoch_from_str('14.01.2020')
    liste = string_to_num_list('[1.2, 3.2, 4.55]')
    print(liste)
    t = "abcd|efgh"
    liste = split_text(t,"|")
    print(t)
    print(liste)
    print("gdgdg")
    #i0 = such("abcdef",'cd',"vs")
    #b=get_free_size('d:\\temp')
    #liste = get_parent_path_dirs('d:\\temp\\Grid')
    #liste = get_parent_path_files('d:\\temp\\Grid')
    #a = elim_comment_not_quoted("abcdef{l12#3ghi}",["#","%"],"{","}")
    #text = "aaaabcdefccc"
    #print "text = %s" % text

    #print such(text,"a","vn")
    #print split_with_quot("abc{def}ghi{jkl}","{","}")
    #print split_not_quoted("abc|edf||{|||}|ghi","|","{","}",0)
    #print such_in_liste(["abc","1abc"],"abc")
    #for i in range(0,100):
    #    print i,int_to_dec36(i,6)
    #    #print i,int_to_dec36(i,6),dec36_to_int(int_to_dec36(i,6))

    #print "%-30.0f" % get_dattime_float("sos.py")
    #print get_name_by_dat_time(pre_text="backup_")
    #d = dat_last_act_workday_datelist([2009,11,7])

##    liste = ["a","b","c"]
##    item = abfrage_listbox(liste,"s")

    #filename = abfrage_file(file_types="*.*",comment="Wähle aus",start_dir="d:\\temp")

    #(p,b,e) = file_split("..\\abc\\ssj.f")
    #print (p,b,e)

    #os.chdir("d:\\tools\\python\\_entw\\AdressBook")
    #read_mab_file("abook.mab")

##    for a in get_subdir_files("d:\\temp"):
##
##      print a

##    t1 = 'aébßcò'
##    tu = to_unicode(t1)
##
##    t2 = to_bytes(tu,'utf-8')
##    #t3 = to_cp1252(tu)
##
##    print tu
##    print t1
##    print t2

##     liste = datum_int_to_intliste(20161219)
##     t = (liste[2], liste[1], liste[0], 12, 0, 0, 0, 0, 0)
##     #t = time.localtime()
##     secs1 = time.mktime( t )
##     print liste
##     print (time.time() - secs1)/60/60
##
##     secs2 = secs_time_epoch_from_int(20161219,12)
##     print secs1 - secs2
##
##     print secs_time_epoch_to_str(int(time.time()))

##    v = str_to_float_possible("e10.0nwklnkn20354")
##    if( v ): print "v = %f" % v

##    val  = str_to_float_possible('')
##    if( val == None ): print "not okay"
##    else:              print "okay"

##    print_python_is_32_or_64_bit()