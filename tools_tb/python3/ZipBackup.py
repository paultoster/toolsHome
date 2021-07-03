# -*- coding: cp1252 -*-
# 14.02.10 neue Version mit ge�nderter mtime
# 12.02.10 erste kommentierte Version
# Klasse Backup:
# Der Klasse Backup wird ein dictionary �bergeben mit
# Quellpfad, Zielpfad, Ausschlusspfade, Ausschlussextensions und
# maximale Anzahl Backups aufzuheben. Die Werte werden alle gespeichert,
# so da� bei einem wiederholtem Aufruf nur das Target-Verzeichnis angegeben werden
# mu�.
# Die Ausschlu�pfade sind relativ oder absolut und beziehen sich auf das source-Verzeichnis
# Beispiel einschlieslich Initialisieren der Klasse und Backup ausf�hren
#-----------------------------------------------------------------------------
# import sys
# if( "D:\\tools_tb\python" not in sys.path ):
#     sys.path.append("D:\\tools_tb\\python")
#
# import ZipBackup
#
# dict = {}
# # Quellpfad
# dict["source_path"]  = "D:\\tools\\python\\ZipBackup\\Source"
# # Zielpfad
# dict["target_path"]  = "D:\\tools\\python\\ZipBackup\\Target"
# # Ausschlusspfade
# dict["exclude_path_rel"] = ["verz_xy"] # => alle verz_xy Verzeichnisse
# dict["exclude_path"] = ["verz_ab"] # => D:\\tools\\python\\ZipBackup\\Source\\verz_ab
# # Ausschlussextensions
# dict["exclude_ext"]  = ["te"]
# # maximale Backups aufheben
# dict["max_backups"]  = 2
# # Pr�ft den target path, ob alle zip-Files eine Zuordnung zu den versionen hat
# # wenn nein l�schen
# dict["proof_tar_path"]  = 0 #(default)
#
# a = ZipBackup.Backup(dict)
# if( a.STATUS == ZipBackup.OKAY ):
#     a.make_backup()
#
# x = raw_input("<pause>")
#
#--------------------------------
# Die dictionary-Namen k�nnen durch die Variablen ge�ndert werden:
# A_TARGET_PATH, A_SOURCE_PATH, A_EXCLUDE_PATH, A_EXCLUDE_EXT
#
# Zuss�tzlich zu ZipBackup.py mu�
# hilfsfkt.py vorhanden sein
#
# BeimAusf�hren des Backups wird beim erstenmal eine Versiondatei im target_path
# erstellt, bei weiteren ausf�hren wird die Versionsdatei eingelesen
# Die Versionsdatei enth�lt ein dictionary und wird in D eingelesen
#
# D['status'] (int)         gibt den Status an 4: Start, beziehungsweise Backup
#                           noch nicht beendet (Versionsnummer wird erst nach beenden
#                           hochgez�hlt
# D['version_list'] (list)  Liste mit den gespeicherten Versionen ANzahl entsprechend
#                           der eingestellten 'max_backups'
# D['source_path'] (str)    Der Pfadname, Quelle
# D['target_path'] (str)    Der Pfadname, Ziel
# D["exclude_path"] (list)  Liste mit den Ausschlu�pfaden absolut
# D["exclude_path_rel"] (list)  Liste mit den Ausschlu�pfaden relativ
# D["exclude_ext"] (list)   Liste mit den Ausschlu�extentions
# D["max_backups"] (int)    Anzahl der zu speichernden Backups
# D['act_version'] (int)    Aktuelle Version
# D['mtime_start'] (int)    Liste mit der Startzeit mit der Funktion int(time.time())
# D['mtime_list'] (list)    Liste mit den jeweiligen Speicherzeiten
# D['tree']       (list)    Liste mit den Dateien und Verzeichnissen und deren Versionen:
#                           = [{'datei1':[versions_liste1,mtime_liste1,exist_liste1],
#                               'datei2':[versions_liste2,mtime_liste2,exist_liste2],
#                               ...},
#                              {'verzeichnis1':[{...},{...}],
#                               'verzeichnis2':[{...},{...}},
#                               ...}
#                             ]
#
#                             versions_liste enth�lt Versionsnummer
#                             mtime_liste    enth�lt die Zeit time.time()
#                             exist_liste    enth�lt True (existiert)oder False (existiert nicht)
#
#                             Die L�nge der Liste ist die Anzahl der Versionen, die gespeichert sind
#

import os, sys, time, string, zipfile, types
import gzip, pickle
import sys

tools_path = "D:\\tools_tb\\python" 

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

import hfkt as h
import hfkt2 as h2
import nzipfile
from stat import *

OKAY     = 1
NOT_OKAY = 0

DEBUG_FLAG = False

DEFAULT_MAX_BACKUPS    = 10
DEFAULT_PROOF_TAR_PATH = 0
UNDERLINE_CHANGE       = "120559"
VERSION_FILE_NAME = "zip_backup.zic"


A_TARGET_PATH      = "target_path"
A_SOURCE_PATH      = "source_path"
A_VERSION_LIST     = "version_list"
A_MTIME_LIST       = "mtime_list"
A_MTIME_START      = "mtime_start"
A_ACT_VERSION      = "act_version"
A_STATUS           = "status"
A_TREE             = "tree"
A_EXCLUDE_PATH     = "exclude_path"
A_EXCLUDE_REL_PATH = "exclude_path_rel"
A_EXCLUDE_EXT      = "exclude_ext"
A_MAX_BACKUPS      = "max_backups"
A_PROOF_TAR_PATH   ="proof_tar_path"

A_VER_FILE            = "version_file"
A_RESTORE_VERSION     = "restore_version"
A_RESTORE_SOURCE_PATH = "restore_source_path"
A_RESTORE_TARGET_PATH = "restore_target_path"



##############################################################################
##############################################################################
##############################################################################
class Proof:
    """ �berpr�fen des gezippten Archives durch Vergleich mit
        VER_FILE Anzeigen der nicht vorhandenen Dateien
    """
    STATUS         = OKAY
    TARGET_PATH    = ""
    N_ZUVIEL       = 0
    N_ZUWENIG      = 0
##############################################################################
    def __init__(self,target_path=None):
        """ Initialisieren
        """
##############################################################################
        #Zielverzeichnis pruefen
        #-----------------------
        found_flag = False
        if( target_path ):
            if( type(target_path) is types.StringType ):
                self.TARGET_PATH = target_path
            elif( type(target_path) is types.ListType ):
                self.TARGET_PATH = target_path[0]


            self.TARGET_PATH = os.path.normcase(self.TARGET_PATH)
            if os.path.isdir(self.TARGET_PATH):
                self.VERSION_FILE = os.path.join(self.TARGET_PATH, VERSION_FILE_NAME)
                self.VERSION_FILE = os.path.normcase(self.VERSION_FILE)
                if( os.path.exists(self.VERSION_FILE) ):
                    found_flag = True

        if not( found_flag ):
            self.VERSION_FILE = h.abfrage_file(file_types=VERSION_FILE_NAME, \
                            comment="Waehle Versionsfile aus einem Backupverzeichnis fuer Restore aus")

            if( not self.VERSION_FILE or len(self.VERSION_FILE) == 0 ):
                print("ZipBackup.Clean: Es wurde kein Versionsfile ausgewaehlt")
                self.STATUS = NOT_OKAY
            else:
                self.TARGET_PATH = os.path.split(self.VERSION_FILE)
                self.TARGET_PATH = self.TARGET_PATH[0]

        self.VERSION_FILE = os.path.normcase(self.VERSION_FILE)
        self.TARGET_PATH  = os.path.normcase(self.TARGET_PATH)

        print("VERSION_FILE: |%s|" % self.VERSION_FILE)
        print("TARGET_PATH:  %s" % self.TARGET_PATH)


        #Datei einlesen
        #==============
##        print "VERSION_FILE: %s" % self.VERSION_FILE
##        f = file(self.VERSION_FILE,"rb")
##        self.D = pickle.load(f)
##        f.close()
        print("Start Einlesen Versionsfile: %s" % self.VERSION_FILE)
        f = gzip.open(self.VERSION_FILE,"rb")
        self.D = pickle.load(f)
        f.close()
        print("Ende Einlesen Versionsfile")

    def make_proof(self):

        if( self.STATUS == OKAY ):

            # ausgewaehltes Zielverzeichnis durchlaufen
            #=========================================
            self.proof_walk_tree(self.TARGET_PATH,self.D[A_TREE])

            print("Anzahl ueberflüssiger Dateien: %i" % self.N_ZUVIEL)
            print("Anzahl fehlender Dateien:      %i" % self.N_ZUWENIG)

        return self.STATUS
############################################################################
    def proof_walk_tree(self, path, d):
############################################################################
        '''proof walk tree'''


        # Alle Dateien im Verzeichnis path pr�fen
        try:
            liste   = os.listdir(path)
        except WindowsError:
            print("ZipBackup.backup_walk_tree.error: os.listdir(\"%s\") not possible" % s_path)
            liste = []

        proof_liste = []
        for i in range(0,len(liste),1):

            liste[i] = os.path.normcase(liste[i])
            liste[i] = os.path.join(path,liste[i])

            if( os.path.isfile(liste[i]) ): #File

                if( liste[i] == self.VERSION_FILE ):
                    print("gefunden")
                    proof_liste.append(0)
                else:
                    proof_liste.append(1)
            else:
                proof_liste.append(0)


        # Alle Dateien im Verzeichnis �berpr�fen
        #=======================================
        for key in d[0].keys():
            if( isinstance(key,str) ):
                # Liste mit den Versionsfiles
                zip_names = self.get_zip_file_names(key,d[0][key])

                flag1 = 1
                for i in range(0,len(zip_names),1):

                    zip_names[i] = os.path.join(path,zip_names[i])
                    zip_names[i] = os.path.normcase(zip_names[i])

                    if not os.path.isfile(zip_names[i]):
                        if( flag1 ):
                            flag1 = 0
                            print("Fehlende Files:")
                        self.N_ZUWENIG += 1
                        print(zip_names[i])
            else:
                prin(key)

            for j in range(0,len(liste),1):

                for i in range(0,len(zip_names),1):

                    if( zip_names[i] == liste[j] ):

                        proof_liste[j] = 0
                        break

        # Alle �berfl�ssigen Dateien anzeigen
        #====================================
        flag1 = 1
        for i in range(0,len(liste),1):

            if( proof_liste[i] == 1 ):
                if( flag1 ):
                    flag1 = 0
                    print("Ueberfl�ssige Files:")
                self.N_ZUVIEL += 1
                print(liste[i])


        # Alle Unterverzeichnisse durchlaufen
        #====================================
        for key in d[1].keys():

            new_path = os.path.join(path,key)

            if( os.path.isdir(new_path) ):

                if( self.proof_walk_tree(new_path,d[1][key]) == NOT_OKAY ):
                    return self.STATUS

        return self.STATUS


##############################################################################
    def get_zip_file_names(self,file_name,zip_liste):
        """ Sucht alle Versionen in Liste und gibt zip-Filenamen zurueck
        """
##############################################################################

        #Name zusammenbauen
        (body,ext) =h.file_splitext(file_name)
        name_list = []

        for i in range(0,len(zip_liste[0]),1):

            if( zip_liste[2][i] ):
                # File wurde nicht geloescht
                name_list.append( body+"_"+ext+"_" +                \
                                  dec36.int_to_dec36(zip_liste[1][i],6) \
                                  +".zip")
        # File wurde geloescht
        return name_list


##############################################################################
##############################################################################
##############################################################################
class Clean:
    """ Bereinigung des gezippten Archives durch Vergleich mit
        VER_FILE Loeschen der nicht eingetragenen Dateien
    """
    STATUS         = OKAY
    TARGET_PATH    = ""
    TEMP_DIR       = ""
    N_CLEAN        = 0
##############################################################################
    def __init__(self,target_path=None,temp_dir="D:\\tmp_backup"):
##############################################################################
        """ Initia�isiren
        """
        #Zielverzeichnis pruefen
        #-----------------------
        found_flag = False
        if( target_path ):
            if( type(target_path) is types.StringType ):
                self.TARGET_PATH = target_path
            elif( type(target_path) is types.ListType ):
                self.TARGET_PATH = target_path[0]


            self.TARGET_PATH = os.path.normcase(self.TARGET_PATH)
            if os.path.isdir(self.TARGET_PATH):
                self.VERSION_FILE = os.path.join(self.TARGET_PATH, VERSION_FILE_NAME)
                self.VERSION_FILE = os.path.normcase(self.VERSION_FILE)
                if( os.path.exists(self.VERSION_FILE) ):
                    found_flag = True

        if not( found_flag ):
            self.VERSION_FILE = h.abfrage_file(file_types=VERSION_FILE_NAME, \
                            comment="Waehle Versionsfile aus einem Backupverzeichnis fuer Restore aus")

            if( not self.VERSION_FILE or len(self.VERSION_FILE) == 0 ):
                print("ZipBackup.Clean: Es wurde kein Versionsfile ausgewaehlt")
                self.STATUS = NOT_OKAY
            else:
                self.TARGET_PATH = os.path.split(self.VERSION_FILE)
                self.TARGET_PATH = self.TARGET_PATH[0]

        self.VERSION_FILE = os.path.normcase(self.VERSION_FILE)
        self.TARGET_PATH  = os.path.normcase(self.TARGET_PATH)

        #Tempdir anlegen
        self.TEMP_DIR = temp_dir
        if( not os.path.exists(self.TEMP_DIR) ):
            os.makedirs(self.TEMP_DIR)


        print("VERSION_FILE: |%s|" % self.VERSION_FILE)
        print("TARGET_PATH:  %s" % self.TARGET_PATH)
        print("TEMP_DIR:     %s" % self.TEMP_DIR)


        #Datei einlesen
        #==============
##        print "VERSION_FILE: %s" % self.VERSION_FILE
##        f = file(self.VERSION_FILE,"rb")
##        self.D = pickle.load(f)
##        f.close()
        print("Start Einlesen Versionsfile: %s" % self.VERSION_FILE)
        f = gzip.open(self.VERSION_FILE,"rb")
        self.D = pickle.load(f)
        f.close()
        print("Ende Einlesen Versionsfile")

############################################################################
    def make_clean(self):
############################################################################

        if( self.STATUS == OKAY ):

            # ausgewaehltes Zielverzeichnis durchlaufen
            #=========================================
            self.clean_walk_tree(self.TARGET_PATH,self.D[A_TREE])

            print("Anzahl geloeschter Dateien: %i" % self.N_CLEAN)


        return self.STATUS
############################################################################
    def clean_walk_tree(self, path, d):
############################################################################
        '''clean walk tree'''


        # Alle Dateien im Verzeichnis path pr�fen
        try:
            liste   = os.listdir(path)
        except WindowsError:
            print("ZipBackup.backup_walk_tree.error: os.listdir(\"%s\") not possible" % s_path)
            liste = []

        proof_liste = []
        for i in range(0,len(liste),1):

            liste[i] = os.path.normcase(liste[i])
            liste[i] = os.path.join(path,liste[i])

            if( os.path.isfile(liste[i]) ): #File

                if( liste[i] == self.VERSION_FILE ):
                    print("gefunden")
                    proof_liste.append(0)
                else:
                    proof_liste.append(1)
            else:
                proof_liste.append(0)



        # Alle Dateien im Verzeichnis �berpr�fen
        #=======================================
        for key in d[0].keys():

            # Liste mit den Versionsfiles
            if( isinstance(key,str) ):
                zip_names = self.get_zip_file_names(key,d[0][key])

                for i in range(0,len(zip_names),1):

                    zip_names[i] = os.path.join(path,zip_names[i])
                    zip_names[i] = os.path.normcase(zip_names[i])

                for j in range(0,len(liste),1):

                    for i in range(0,len(zip_names),1):

                        if( zip_names[i] == liste[j] ):

                            proof_liste[j] = 0
                            break
            else:
                print(key)



        # Alle �berfl�ssigen Dateien l�schen
        #===================================
        for i in range(0,len(liste),1):

            if( proof_liste[i] == 1 ):
                print("copy: %s->%s" % (liste[i],self.TEMP_DIR))
                shutil.copy(liste[i],self.TEMP_DIR)
                print("rm: %s" % liste[i])
                os.remove(liste[i])
                self.N_CLEAN += 1


        # Alle Unterverzeichnisse durchlaufen
        #====================================
        for key in d[1].keys():

            new_path = os.path.join(path,key)

            if( os.path.isdir(new_path) ):

                if( self.clean_walk_tree(new_path,d[1][key]) == NOT_OKAY ):
                    return self.STATUS

        return self.STATUS


##############################################################################
    def get_zip_file_names(self,file_name,zip_liste):
        """ Sucht alle Versionen in Liste und gibt zip-Filenamen zurueck
        """
##############################################################################

        #Name zusammenbauen
        (body,ext) = h.file_splitext(item)

        name_list = []

        for i in range(0,len(zip_liste[0]),1):

            if( zip_liste[2][i] ):
                # File wurde nicht geloescht
                name_list.append( body+"_"+ext+"_" +                \
                                  dec36.int_to_dec36(zip_liste[1][i],6)  \
                                  +".zip")
        # File wurde geloescht
        return name_list


##############################################################################
##############################################################################
##############################################################################
class Restore:
    """ Wiederherstellen von gezippten backups (ZipBackup)
    """
    STATUS = OKAY
############################################################################
    def __init__(self,dict=None,start_search_dir=None):
############################################################################
        """ Initialisieren
        """

        # Voreinstellung auslesen
        #========================
        if( dict ):

            if( A_VER_FILE in dict ):
                self.VER_FILE = dict[A_VER_FILE]
            else:
                if( A_TARGET_PATH in dict ):
                    self.VER_FILE = os.path.join(dict[A_TARGET_PATH],VERSION_FILE_NAME)
                    self.VER_FILE = os.path.normcase(self.VER_FILE)
                else:
                    self.VER_FILE = None

            if( A_RESTORE_VERSION in dict ):
                self.RESTORE_VERSION = dict[A_RESTORE_VERSION]
            else:
                self.RESTORE_VERSION = None

            if( A_RESTORE_SOURCE_PATH in dict ):
                self.RESTORE_SOURCE_PATH = dict[A_RESTORE_SOURCE_PATH]
            else:
                self.RESTORE_SOURCE_PATH = None

            if( A_RESTORE_TARGET_PATH in dict ):
                self.RESTORE_TARGET_PATH = dict[A_RESTORE_TARGET_PATH]
            else:
                self.RESTORE_TARGET_PATH = None

        else:
            self.VER_FILE            = None
            self.RESTORE_VERSION     = None
            self.RESTORE_SOURCE_PATH = None
            self.RESTORE_TARGET_PATH = None



        # Versionsdatei bestimmen
        #========================
        if( not self.VER_FILE ):
            self.VER_FILE = h.abfrage_file(file_types=VERSION_FILE_NAME, \
                            comment="Waehle Versionsfile <%s> aus dem entsprechenden Backupverzeichnis fuer Wiederherstellung aus" % VERSION_FILE_NAME, \
                            start_dir=start_search_dir)

            if( not self.VER_FILE or len(self.VER_FILE) == 0 ):
                print("ZipBackup.ZipRestore: Es wurde kein Versionsfile ausgewaehlt")
                self.STATUS = NOT_OKAY



        # Versionsdatei einlesen
        #========================
        if( self.STATUS == OKAY ):

            if( os.path.exists(self.VER_FILE) ): # Datei vorhanden
                #Datei einlesen
                #==============
                print("Lese Versionsdatei <%s> ein !!!!" % self.VER_FILE)
                f = gzip.open(self.VER_FILE,"rb")
                self.D = pickle.load(f,encoding='bytes')
                f.close()
                (self.D[A_TARGET_PATH],temp) = os.path.split(self.VER_FILE)

                self.D=h2.change_item_byte_to_string(self.D)

                # Target path aus self.VER_FILE erstellen, da nicht in Datenbasis
                # verwendetes benutzt werden soll, da ge�ndert sein kann

            else:
                print("ZipBackup.ZipRestore: Versionsfile <%s> konnte nicht geoeffnet werden" % self.VER_FILE)
                self.D = None
                self.STATUS = NOT_OKAY

            if( self.STATUS == OKAY  ):

                # Sicherung auswaehlen
                #====================
                # print(self.D[A_VERSION_LIST])
                
                if( not self.RESTORE_VERSION ):
                    liste = []
                    for i in range(0,len(self.D[A_VERSION_LIST])):
                        liste.append((self.D[A_VERSION_LIST][i], \
                                      time.ctime(self.D[A_MTIME_LIST][i])))


                    self.RESTORE_VERSION = h.abfrage_liste(liste, \
                                           "Waehle Version fuer Restore aus")
                elif( self.RESTORE_VERSION == -1 ):
                    self.RESTORE_VERSION = self.D[A_VERSION_LIST][len(self.D[A_VERSION_LIST])-1]

                # Sicherungsversion pruefen
                #=========================
                if( self.RESTORE_VERSION not in self.D[A_VERSION_LIST] ):

                    print("ZipBackup.ZipRestore: ausgewaehlte Version <%i> ist nicht in Versionsliste enthalten" \
                          % self.RESTORE_VERSION)
                    print("self.D[A_VERSION_LIST]:")
                    print(self.D[A_VERSION_LIST])
                    self.STATUS = NOT_OKAY

                if( self.STATUS == OKAY ):

                    # Waehle Unterverzeichnis aus
                    #===========================
                    if( not self.RESTORE_SOURCE_PATH ):
                        text = "Suche das wiederherzustellende Unterverzeichnis in <%s> " \
                               % self.D[A_TARGET_PATH]
                        self.RESTORE_SOURCE_PATH = h.abfrage_dir(text, \
                                                        self.D[A_TARGET_PATH])
                        if( not self.RESTORE_SOURCE_PATH ):
                            print("ZipBackup.ZipRestore: Es wurde kein Unterverzeichnis ausgewaehlt")
                            self.STATUS = NOT_OKAY
                    else:

                        if( not os.path.exists(self.RESTORE_SOURCE_PATH) ):
                            print("ZipBackup.ZipRestore: RESTORE_SOURCE_PATH <%s> existiert nicht" \
                                  % self.RESTORE_SOURCE_PATH)
                            self.STATUS = NOT_OKAY

                        if( h.such(self.RESTORE_SOURCE_PATH.lower(),self.D[A_TARGET_PATH].lower(),"vs") != 0 ):
                            print("ZipBackup.ZipRestore: RESTORE_SOURCE_PATH <%s> ist nicht im backup_verzeichnis <%s> vorhanden" \
                                  % (self.RESTORE_SOURCE_PATH,self.D[A_TARGET_PATH]))
                            self.STATUS = NOT_OKAY


                    if( self.STATUS == OKAY ):

                        # self.RESTORE_SOURCE_PATH = os.path.normcase(self.RESTORE_SOURCE_PATH)
                        self.RESTORE_SOURCE_PATH = h.elim_e(self.RESTORE_SOURCE_PATH,"\\")

                        # Waehle Zielverzeichnis aus
                        #===========================
                        if( not self.RESTORE_TARGET_PATH ):
                            text = "Suche Verzeichnis f�r die Wiederherstellung von <%s> aus" % self.RESTORE_SOURCE_PATH
                            self.RESTORE_TARGET_PATH = h.abfrage_dir(text)

                            if( not self.RESTORE_TARGET_PATH ):
                                print("ZipBackup.ZipRestore: Es wurde kein Unterverzeichnis ausgewaehlt")
                                self.STATUS = NOT_OKAY
                        else:
                            if( not os.path.exists(self.RESTORE_TARGET_PATH) ):
                                os.makedirs(self.RESTORE_TARGET_PATH)

                        if( self.STATUS == OKAY ):

                            # self.RESTORE_TARGET_PATH = os.path.normcase(self.RESTORE_TARGET_PATH)
                            self.RESTORE_TARGET_PATH = h.elim_e(self.RESTORE_TARGET_PATH,"\\")

############################################################################
    def make_restore(self):
############################################################################

        if( self.STATUS == OKAY ):

            # im tree, RESTORE_SOURCE_PATH finden:
            #=====================================
            ####
            if( os.path.normcase(self.RESTORE_SOURCE_PATH) == os.path.normcase(self.D[A_TARGET_PATH]) ):
                d             = self.D[A_TREE]
                rs_path_rest  = ""
            else:
                (d,rs_path_rest) = self.find_tree_leave(self.RESTORE_SOURCE_PATH, \
                                                        self.D[A_TREE])

            if( d == None):
                self.STATUS = NOT_OKAY
                return self.STATUS

##            print d
##            print rs_path_rest
            rt_path_start = os.path.join(self.RESTORE_TARGET_PATH,rs_path_rest)
            if( not os.path.isdir(rt_path_start) ):
                try:
                    os.makedirs(rt_path_start)
                except OSError:
                    print("ZipBackup.ZipRestore.make_restore error: Das Zielverzeichnis %s kann nicht erstellt werden" % rt_path_start)
                    self.STATUS = NOT_OKAY
                    return self.STATUS

            #Start
            start_text = time.ctime()

            # ausgewaehltes Zielverzeichnis durchlaufen
            #=========================================
            self.restore_walk_tree(self.RESTORE_SOURCE_PATH,  \
                                   rt_path_start,             \
                                   d                          )
            #Start,Ende
            print("Start: %s" % start_text)
            print("Ende: %s" % time.ctime())

        else:
            self.STATUS = NOT_OKAY

        return self.STATUS


############################################################################
    def find_tree_leave(self, rs_path, path_liste):
############################################################################
        '''restore walk tree'''

        # root path von rs_path entfernen und aufteilen
        #==============================================
        i0 = os.path.normcase(rs_path).find(os.path.normcase(self.D[A_TARGET_PATH]))
        if( i0 < 0 ):
            print("Error: restore_path (%s) is not part of (%s)" % (rs_path,self.D[A_TARGET_PATH]))
            return (None,None)
#        elif( i0 == 0 ):
#            rs_path_rest     = os.path.normcase(rs_path)
        else:
            rs_path_rest     = rs_path[i0+len(self.D[A_TARGET_PATH]):len(rs_path)]

        rs_path_rest     = h.elim_ae(rs_path_rest,"\\")
        pathes  = rs_path_rest.splitfields("\\")

##        print "rs_path %s" % rs_path
##        print "rs_path_rest %s" % rs_path_rest
##        print "pathes "
##        print pathes
##        print "TARGET_PATH %s" % self.TARGET_PATH
##        print "i0 %i" % i0

        # suche das Blatt
        #================
        d = path_liste
        if( len(rs_path_rest) > 0 ):
            for path in pathes:

                found_flag = False
                for key in d[1].keys():


                    if( path == key ):
                        d = d[1][key]
                        found_flag = True
                        break
                if( not found_flag ):
                    print("Error: sub-path (%s) could not be found" % path)
                    print("     List of restore target pathes:")
                    print(path_liste[1].keys())
                    return (None,rs_path_rest)

        return (d,rs_path_rest)


############################################################################
    def restore_walk_tree(self, rs_path, rt_path, path_liste):
############################################################################
        '''restore walk tree'''


        # Alle Dateien mit der richtigen Version zurueckspielen
        #=====================================================
        for key in path_liste[0].keys():

##            if( h.such(key,"Trash","vs") >= 0 ):
##                test = 1
            if( isinstance(key,str) ):
                zip_name = self.get_zip_file_name(key,path_liste[0][key], \
                                                self.RESTORE_VERSION    )
                if( zip_name ):

                    full_zip_name  = os.path.join(rs_path,zip_name)
                    full_rest_name = os.path.join(rt_path,key)

                    self.restore_file(full_zip_name,full_rest_name,rt_path)
            else:
                print(key)

        # Alle Unterverzeichnisse durchlaufen
        #====================================
        for key in path_liste[1].keys():

            if( isinstance(key,str)):
                new_rs_path = os.path.join(rs_path,key)
                new_rt_path = os.path.join(rt_path,key)

                if( not os.path.isdir(new_rt_path) ):
                    try:
                        os.makedirs(new_rt_path)
                    except OSError:
                        print("ZipBackup.error: Das Zielverzeichnis %s kann nicht erstellt werden" % new_rt_path)
                        self.STATUS = NOT_OKAY
                        return self.STATUS

                if( self.restore_walk_tree(new_rs_path, \
                                        new_rt_path, \
                                        path_liste[1][key]) == NOT_OKAY ):
                    return self.STATUS

        return self.STATUS


##############################################################################
    def get_zip_file_name(self,file_name,zip_liste,rs_ver):
        """ Sucht Version in Liste und gibt zip-Filename zurueck
        """
##############################################################################

##        print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
##        print "%s" % zip_liste
##        print rs_ver

        #Version suchen
        if( rs_ver in zip_liste[0] ):
            dum = rs_ver
        else:
            dum = max(zip_liste[0])

        for i in range(0,len(zip_liste[0]),1):

            if( zip_liste[0][i] == dum ):

                iver = i


        #Name zusammenbauen
        (body,ext) =h.file_splitext(file_name)

        if( h.such(ext,"_","vs") >= 0 ):
            ext = h.change_max(ext,"_",UNDERLINE_CHANGE)


##        print "iver=%i" % iver
##        print body+"_"+ext+"_" +                         \
##           h.int_to_dec36(zip_liste[1][iver],6)    \
##                    +".zip"

        if( zip_liste[2][iver] ):
            # File wurde nicht geloescht
            return body+"_"+ext+"_" +                         \
                   h.int_to_dec36(zip_liste[1][iver],6)       \
                                     +".zip"
        # File wurde geloescht
        return None

############################################################################
    def restore_file(self,full_zip_name,full_rest_name,rest_path):
        """ Holt File aus dem zip-File und schreibt zurueck
        """
        if( os.path.exists(full_zip_name) ):

            print("res: %s -> %s" % (full_zip_name,full_rest_name))
            if( nzipfile.ZipFile.UnzipSimple(full_zip_name,rest_path) != 1 ):
                try:
                    i = os.system("unzip -qo \"" + full_zip_name + "\" -d \"" + rest_path + "\"" )
                    print("!!! unzip.exe verwendet")
                except:
                    print(">>>>>> restore from zip-file failed !!!!!")

#            print "unzip -qo " + full_zip_name + " -d " + rest_path
            # os.system("unzip -qo " + full_zip_name + " -d " + rest_path )
#            print "unzip -j -q " + '"' + full_zip_name + '"' + " -d "+rest_path + " -o"
#            os.system("unzip -j -q " + '"' + full_zip_name + '"' + " -d " + rest_path + " -o")
##            z = zipfile.ZipFile(full_zip_name,'r')
##            liste = z.namelist()
##            # Zipfile lesen
##            if( len(liste) > 0 ):
##                erg = z.read(liste[0])
##            # Zipfile schliessen
##            z.close()
##
##            # File schreiben
##            f=file(full_rest_name,"wb")
##            f.write(erg)
##            f.close()
        else:
            print("err:  %s -> %s" % (full_zip_name,full_rest_name))
            print("File does not exist")



############################################################################
############################################################################
############################################################################

############################################################################
############################################################################
############################################################################

class Backup:
    """ Variablen:
        CONFIG_DATEI        Konfigurationsdatei
        TARGET_PATH         Zielpfad
        TARGET_PATH_NORM    Zielpfad in norm case
        NEW_TARGET_PATH_SET Bit ob neu gesetzt
        SOURCE_PATH         Quellpfad
        SOURCE_PATH_NORM    Quellpfad in norm case
        EXCLUDE_PATH_REL_LIST   Liste mit Verzeichnissen (relativ),
                            die nicht gespeichert werden
        EXCLUDE_PATH_ABS_LIST   Liste mit Verzeichnissen (absolut),
                            die nicht gespeichert werden
        EXCLUDE_EXT_LIST    Liste mit Extensions, die nicht gespeichert werden
        VERSION_LIST        Liste mit allen Versionen
        MTIMELIST           Liste mit den zu VERSION_LIST zugeordneten
                            maketimes in sekunden

    """

    START    = 4
    FINISH   = 5

    ISDIR    = 2
    ISFILE   = 3

    DELTA_T_SAVE = 3*60    #Zeit bis zur naechsten speicherung in sec

    STATUS  = OKAY

    NEW_TARGET_PATH_SET = 0
    VERSION_FILE_SAVE_DESIRED = 0

    D                 = {}

############################################################################
    def __init__(self,dict):
############################################################################

        #-------------------------------
        # vorgegebene PArameter auslesen
        #-------------------------------
        if( self.parameter_read(dict) != OKAY):
            return

        #-------------------------------------
        # Version auslesen oder initialisieren
        #-------------------------------------
        if( self.version_read() != OKAY ):
            return

        #---------------------
        #Standard-Exclude-Path
        #---------------------
        dum = self.SOURCE_PATH
        dum = h.elim_ae(dum,"\\")
        dum = h.elim_ae(dum,":")
        if( len(dum) == 1 and dum in string.ascii_letters ):
            self.EXCLUDE_PATH_LIST.append(os.path.join(self.SOURCE_PATH, \
                                                       "Recycler")       )
            self.EXCLUDE_PATH_LIST.append(os.path.join(self.SOURCE_PATH, \
                                                       "System Volume Information")       )

        #------------------------------------------
        # EXCLUDE_EXT_LIST vom Punkt <.> bereinigen
        #------------------------------------------
        for i in range(0,len(self.EXCLUDE_EXT_LIST)):
            val = self.EXCLUDE_EXT_LIST[i]
            i0  = val.find(".")
            if( i0 > -1 ):
                i1 = len(val)
                self.EXCLUDE_EXT_LIST[i] = val[i0+1:i1]

        #---------
        # Normcase
        #---------
        self.SOURCE_PATH_NORM = os.path.normcase(self.SOURCE_PATH)

        self.TARGET_PATH_NORM = os.path.normcase(self.TARGET_PATH)

        self.EXCLUDE_REL_PATH_LIST_NORM = []
        for i in range(0,len(self.EXCLUDE_REL_PATH_LIST)):
            self.EXCLUDE_REL_PATH_LIST_NORM.append(os.path.normcase(self.EXCLUDE_REL_PATH_LIST[i]))

        self.EXCLUDE_PATH_LIST_NORM = []

        for i in range(0,len(self.EXCLUDE_PATH_LIST)):
            self.EXCLUDE_PATH_LIST_NORM.append(os.path.normcase(self.EXCLUDE_PATH_LIST[i]))

        self.EXCLUDE_EXT_LIST_NORM = []

        for i in range(0,len(self.EXCLUDE_EXT_LIST)):
            self.EXCLUDE_EXT_LIST_NORM.append(os.path.normcase(self.EXCLUDE_EXT_LIST[i]))


        #-------------
        # Proof pathes
        #-------------
        if not os.path.isdir(self.SOURCE_PATH_NORM):

            print("ZipBackup.error: Das Quellverzeichnis %s kann nicht gefunden werden" % self.SOURCE_PATH)
            self.STATUS = NOT_OKAY
            return

        if not os.path.isdir(self.TARGET_PATH_NORM):
            try:
                os.makedirs(self.TARGET_PATH)
                self.NEW_TARGET_PATH_SET = 1
            except OSError:
                print("ZipBackup.error: Das Zielverzeichnis %s kann nicht erstellt werden" % self.TARGET_PATH)
                self.STATUS = NOT_OKAY
                return

        #----------------------
        # aktuelle Zeit Sec von Epoche
        #----------------------
        self.ACTUAL_MTIME = time.time()


############################################################################
    def parameter_read(self,dict):
        """ Steuerparameter, die ueber ein dictonary uebergeben werden,
            wird ausgelesen
        """
############################################################################

        #----------------
        #Quellverzeichnis
        #----------------
        liste = dict.get(A_SOURCE_PATH)
        if( liste == None or len(liste) == 0):
            print("ZipBackup.error: keyword: <%s> exisistiert nicht im uebergebenen Dictionary dict" % A_SOURCE_PATH)
            print(dict)
            self.STATUS = NOT_OKAY
            return self.STATUS
        else:
            if( type(liste) is types.StringType ):
                self.SOURCE_PATH = liste
            elif( type(liste) is types.ListType ):
                self.SOURCE_PATH = liste[0]

        #---------------
        #Zielverzeichnis
        #---------------
        liste = dict.get(A_TARGET_PATH)
        if( liste == None or len(liste) == 0):
            print("ZipBackup.error: keyword: <%s> exisistiert nicht im uebergebenen Dictionary dict" % A_TARGET_PATH)
            print(dict)
            self.STATUS = NOT_OKAY
            return self.STATUS
        else:
            if( type(liste) is types.StringType ):
                self.TARGET_PATH = liste
            elif( type(liste) is types.ListType ):
                self.TARGET_PATH = liste[0]

        #----------------------
        #Exclude-Path (relativ)
        #----------------------
        liste = dict.get(A_EXCLUDE_REL_PATH)
        if( liste != None and len(liste) != 0):
            self.EXCLUDE_REL_PATH_LIST = liste
        else:
            self.EXCLUDE_REL_PATH_LIST = []

        if( type(self.EXCLUDE_REL_PATH_LIST) is not types.ListType ):
            print("ZipBackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % A_EXCLUDE_REL_PATH)
            self.STATUS = NOT_OKAY
            return

        #======================
        #Exclude-Path (absolut)
        #======================
        liste = dict.get(A_EXCLUDE_PATH)
        if( liste != None and len(liste) != 0):

            self.EXCLUDE_PATH_LIST = liste
        else:
            self.EXCLUDE_PATH_LIST = []


        # absolute Pfad erstellen
        if( self.EXCLUDE_PATH_LIST ):
            for i in range(0,len(self.EXCLUDE_PATH_LIST)):
                self.EXCLUDE_PATH_LIST[i] = os.path.join(self.SOURCE_PATH,self.EXCLUDE_PATH_LIST[i])


        #===========
        #Exclude-Ext
        #===========
        liste = dict.get(A_EXCLUDE_EXT)
        if( liste != None and len(liste) != 0):
            self.EXCLUDE_EXT_LIST = liste
        else:
            self.EXCLUDE_EXT_LIST = []


        #-------------------
        #max-backup-versions
        #-------------------
        liste = dict.get(A_MAX_BACKUPS)
        if( liste != None):
            if( type(liste) is types.StringType ):
                self.MAX_BACKUPS = int(liste)
            elif( type(liste) is types.ListType ):
                liste            = liste[0]
                if( type(liste) is types.StringType ):
                    self.MAX_BACKUPS = int(liste)
                else:
                    self.MAX_BACKUPS = liste
            else:
                self.MAX_BACKUPS = liste
        else:
            self.MAX_BACKUPS = None

        #============================
        #flag f�r target path pr�fung
        #============================
        liste = dict.get(A_PROOF_TAR_PATH)
        if( liste != None):
            if( type(liste) is types.StringType ):
                self.PROOF_TAR_PATH = int(liste)
            elif( type(liste) is types.ListType ):
                liste            = liste[0]
                if( type(liste) is types.StringType ):
                    self.PROOF_TAR_PATH = int(liste)
                else:
                    self.PROOF_TAR_PATH = liste
            else:
                self.PROOF_TAR_PATH = liste
        else:
            self.PROOF_TAR_PATH = None


        return self.STATUS
############################################################################
    def version_read(self):
############################################################################
        """ Die Versionsimformationen, die im Target_path gespeichert wird,
            wird ausgelesen oder neu angelegt
        """

        self.VERSION_FILE = os.path.join(self.TARGET_PATH, VERSION_FILE_NAME)
        self.VERSION_FILE = os.path.normcase(self.VERSION_FILE)


        if( os.path.exists(self.VERSION_FILE) ): # Datei vorhanden

            #Datei einlesen
            #==============
##            print "VERSION_FILE: %s" % self.VERSION_FILE
##            f = file(self.VERSION_FILE,"rb")
##            self.D = pickle.load(f)
##            f.close()
            f = gzip.open(self.VERSION_FILE,"rb")
            self.D = pickle.load(f,encoding='bytes')
            f.close()
            self.D=h2.change_list_to_string(self.D)
            #Daten ueberpruefen
            #================

            #Source-Pathangabe
            if( self.D[A_SOURCE_PATH] != self.SOURCE_PATH ):
                print("ZipBackup.version_read.warning:")
                print("Das Quellverzeichnis <%s> stimmt nicht mit der Angabe <%s> im Versionsfile <%s> ueberein !!!"  \
                      % (self.SOURCE_PATH,self.D[A_SOURCE_PATH],self.VERSION_FILE))
                print("(<%s> wird übernommen)" % self.SOURCE_PATH)
                self.D[A_SOURCE_PATH] = self.SOURCE_PATH

            #TArget-Pathangabe
            if( self.D[A_TARGET_PATH] != self.TARGET_PATH ):
                print("ZipBackup.version_read.warning:")
                print("Das ZIELverzeichnis <%s> stimmt nicht mit der Angabe <%s> im Versionsfile <%s> ueberein !!!"  \
                      % (self.TARGET_PATH,self.D[A_TARGET_PATH],self.VERSION_FILE))
                print("(<%s> wird �bernommen)" % self.TARGET_PATH)
                self.D[A_TARGET_PATH] = self.TARGET_PATH


            #Source-Pathangabe
            if( len(self.SOURCE_PATH) == 0 ):
                self.SOURCE_PATH      = self.D[A_SOURCE_PATH]


            if( (self.D[A_SOURCE_PATH] != self.SOURCE_PATH) ):
                print("")
                print("Quellverzeichnis wurde ge�ndert:")
                print("source_path (old) <%s>"  % self.D[A_SOURCE_PATH])
                print("in source_path (new) <%s>"  % self.SOURCE_PATH)
                flag = h.eingabe_jn("Soll das Quellverzeichnis ge�ndert werden ?",default=True)
                if( not flag ):
                    self.STATUS = NOT_OKAY
                    return self.STATUS
                else:
                    self.D[A_SOURCE_PATH] = self.SOURCE_PATH


            # Exlude Path:
            if( len(self.EXCLUDE_PATH_LIST) == 0 ):
                self.EXCLUDE_PATH_LIST = self.D[A_EXCLUDE_PATH]
            else:
                self.D[A_EXCLUDE_PATH] = self.EXCLUDE_PATH_LIST

            if( len(self.EXCLUDE_REL_PATH_LIST) == 0 ):
                self.EXCLUDE_REL_PATH_LIST = self.D[A_EXCLUDE_REL_PATH]
            else:
                self.D[A_EXCLUDE_REL_PATH] = self.EXCLUDE_REL_PATH_LIST

            # Exlude Extensions:
            if( len(self.EXCLUDE_EXT_LIST) == 0 ):
                self.EXCLUDE_EXT_LIST = self.D[A_EXCLUDE_EXT]
            else:
                self.D[A_EXCLUDE_EXT] = self.EXCLUDE_EXT_LIST

            # Maxbackups
            if( self.MAX_BACKUPS == None ):
                self.MAX_BACKUPS = self.D[A_MAX_BACKUPS]
            else:
                self.D[A_MAX_BACKUPS] = self.MAX_BACKUPS

            # flag proof target path
            if( self.PROOF_TAR_PATH == None ):
                if( A_PROOF_TAR_PATH in self.D ):
                    self.PROOF_TAR_PATH = self.D[A_PROOF_TAR_PATH]
                else:
                    self.PROOF_TAR_PATH = 0
            else:
                self.D[A_PROOF_TAR_PATH] = self.PROOF_TAR_PATH

        else:  # keine Versionsdatei vorhanden => Struktur anlegen

            # Ziel-Wurzelpfad, mu� vorhanden sein
            self.D[A_TARGET_PATH]  = self.TARGET_PATH

            # Quell-Wurzelpfad
            if( len(self.SOURCE_PATH) > 0 ):
                self.D[A_SOURCE_PATH]  = self.SOURCE_PATH
            else:
                print("Da Versionsliste neu angelegt wird mus <%s> " % A_SOURCE_PATH \
                + "im uebergebenen Dictionary übergeben werden")
                self.STATUS = NOT_OKAY
                return self.STATUS

            #Exclude Path
            self.D[A_EXCLUDE_PATH] = self.EXCLUDE_PATH_LIST
            self.D[A_EXCLUDE_REL_PATH] = self.EXCLUDE_REL_PATH_LIST

            #Exclude Extensions
            self.D[A_EXCLUDE_EXT] = self.EXCLUDE_EXT_LIST

            # Maxbackups
            if( self.MAX_BACKUPS == None ):
                self.MAX_BACKUPS = DEFAULT_MAX_BACKUP
                self.D[A_MAX_BACKUPS] = DEFAULT_MAX_BACKUP
            else:
                self.D[A_MAX_BACKUPS] = self.MAX_BACKUPS

            # flag proof target path
            if( self.PROOF_TAR_PATH == None ):
                self.PROOF_TAR_PATH = DEFAULT_PROOF_TAR_PATH
                self.D[A_PROOF_TAR_PATH] = self.PROOF_TAR_PATH
            else:
                self.D[A_PROOF_TAR_PATH] = self.PROOF_TAR_PATH

            # Versionsliste
            self.D[A_VERSION_LIST] = []

            # dazugehoerige Make-Time-Liste
            self.D[A_MTIME_LIST]   = []
            # Starttime
            self.D[A_MTIME_START] = int(time.time())
            # Verzeichnisbaum mit der Liste der Zipfiles und Informationen
            # erstes Objekt in Liste zip_file_liste mit [count_liste,mtime_liste]
            # zweites Objekt in Liste dictionary mit Namen der Unterpfade
            self.D[A_TREE]         = [{},{}]

        return self.STATUS


############################################################################
##############################################################################
##    def action(self):
##        """ Hauptprogramm zu Ausfuehrung der Aktionen
##        """
##############################################################################
##
##        if( self.STATUS == self.OKAY ):
##
##            if( self.ACTION[0] == 'b' or self.ACTION[0] == 'B'): # backup
##
##                self.make_backup()
##
##            elif( self.ACTION[0] == 'r' or self.ACTION[0] == 'R'): # restore
##
##                if( len(self.D[A_VERSION_LIST]) > 0 and max(self.D[A_VERSION_LIST]) > 0 ):
##                    self.make_restore()
##                else:
##                    print "Kein Backup zum restoren vorhanden"
##            else:
##                print "ZipBackup.warning: no action "
##
##
##        return self.STATUS
##
############################################################################
    def make_backup(self):
        """ Startet den backup-Vorgang
        """
############################################################################


        # Version hochzaehlen
        if( len(self.D[A_VERSION_LIST]) == 0 ):

            self.D[A_ACT_VERSION] = 1
        elif( self.D[A_STATUS] == self.FINISH ):
            self.D[A_ACT_VERSION] += 1

        # Status Versionszaelung zuruecksetzen
        self.D[A_STATUS] = self.START

        #Erste gueltige Version (beim Aufraeumen)
        self.FIRST_VALID_VERSION = max(1,                          \
                                       (self.D[A_ACT_VERSION] \
                                        -self.MAX_BACKUPS+1)       \
                                      )

        # letze Sicherungszeit initialisieren
        self.LAST_SAVE = int(time.time())

        #Start
        start_text = time.ctime()

        # Flag, ob �berhaupt sich etwas ge�ndert hat
        self.NEW_VERSIONS_ZIPPED = 0

        # Test
        self.count = 0

        # backup starten
        self.STATUS = self.backup_walk_tree(self.D[A_SOURCE_PATH], \
                                            self.D[A_TARGET_PATH], \
                                            self.D[A_TREE])

        # Status Versionszaelung zuruecksetzen
        if( self.STATUS == OKAY ):
            self.D[A_STATUS] = self.FINISH

        if( self.NEW_VERSIONS_ZIPPED ):

            # Version hochzaehlen
            self.D[A_VERSION_LIST].append(self.D[A_ACT_VERSION])

            # maketime speichern
            self.D[A_MTIME_LIST].append(int(time.time()))

        else:

            print("no new version zipped")
            if( self.D[A_ACT_VERSION] > 1 ):
                self.D[A_ACT_VERSION] -= 1
                self.FIRST_VALID_VERSION -= 1

        #Bereinigen
        flag = True
        while flag:
            flag = False
            for i in range(0,len(self.D[A_VERSION_LIST])):

                if( self.D[A_VERSION_LIST][i] < self.FIRST_VALID_VERSION ):
                    del self.D[A_VERSION_LIST][i]
                    del self.D[A_MTIME_LIST][i]
                    flag = True
                    break

        # print self.D
        # Dictionary speichern
        if( self.VERSION_FILE_SAVE_DESIRED ):
            self.save_version("ende")


        #Start,Ende
        print("Start: %s" % start_text)
        print("Ende: %s" % time.ctime())

############################################################################
    def backup_walk_tree(self, s_path, t_path, path_liste):
############################################################################
        '''recursively descend the directory tree rooted at top,
           calling the callback function for each regular file'''


        #===================
        # DirListe erstellen
        #===================
        try:
            s_liste   = os.listdir(s_path)
        except WindowsError:
            print("ZipBackup.backup_walk_tree.error: os.listdir(\"%s\") not possible" % s_path)
            s_liste = []

        #=====================
        # DirListe durchlaufen
        #=====================
        for item in s_liste:

            self.count += 1

            if( item == "hfkt.py" ):
                s_pathname = ""

            s_pathname   = os.path.join(s_path,item)


            (s_body,s_ext) = h.file_splitext(item)
            s_ext_n        = os.path.normcase(s_ext)




            #===============
            # Filebehandlung
            #===============
            if( os.path.isfile(s_pathname) ): #File

                #==========================================
                # Prueft die Ausschlussliste der extensions
                #==========================================
                if( self.proof_extension_for_backup(s_ext_n) ):

                    # Prueft Versionen im backup-Verzeichnis
                    #=======================================
                    if( item in path_liste[0] ):
                        self.proof_versions_with_zip_files(      \
                                            path_liste[0][item], \
                                            t_path,              \
                                            s_body,              \
                                            s_ext)
                    # ansonsten neues item in liste bilden
                    #=======================================
                    else:
                        # [ver_liste,mtime_liste,exist_liste]
                        #====================================
                        path_liste[0][item] = [[],[],[]]

                    # Zielpfad pruefen, ob vorhanden und erstellen
                    #=============================================
                    go_on_flag = 1
                    if( not os.path.isdir(t_path) ):
                        try:
                            os.makedirs(t_path)
                        except WindowsError:
                            print("ZipBackup.backup_walk_tree.error: os.makedirs(\"%s\") not possible" % t_path)
                            go_on_flag = 0

                    if( go_on_flag ):
                        # maketime abfragen
                        #==================
                        s_mtime = int(os.path.getmtime(s_pathname))

                        if( s_mtime < 0 or s_mtime > self.ACTUAL_MTIME ):
                            s_mtime = self.ACTUAL_MTIME


                        # item auf Version pr�fen und zip-backup erstellen
                        #==================================================
                        make_flag = self.proof_ver_and_make_zip_ver(         \
                                                        path_liste[0][item], \
                                                        s_pathname,          \
                                                        t_path,              \
                                                        s_body,              \
                                                        s_ext,               \
                                                        s_mtime)



                        # zip-Versionen pr�fen, ob eine alte gel�scht wird
                        #=================================================
                        self.proof_zip_files_with_version(path_liste[0][item],  \
                                                           t_path,              \
                                                           s_body,              \
                                                           s_ext)

                # exluded extension, wird gepr�ft
                else:

                    # wird gepr�ft, ob noch in der Liste
                    # wenn ja l�schen
                    if( item in path_liste[0].keys() ):
                        del path_liste[0][item]


            #===============
            # Dirbehandlung
            #===============
            elif( os.path.isdir(s_pathname) ): #Dir

                # Prueft die Ausschlussliste der pathes
                #======================================
                if( self.proof_path_for_backup(os.path.normcase(s_pathname),os.path.normcase(item)) ):

                    if item not in path_liste[1]:
                        path_liste[1][item] = [{},{}]

                    if( self.backup_walk_tree(                   \
                               s_pathname,                       \
                               os.path.join(t_path,item),        \
                               path_liste[1][item]) == NOT_OKAY ):
                        return self.STATUS



                # Prueft ob noch etwas in dem Pfad enthalten ist
                #===============================================
                if( item in path_liste[1] ):
                    liste = path_liste[1][item]

                    # Alles leer
                    #===========
                    if( len(liste[0]) == 0 and len(liste[1]) == 0):
                        del path_liste[1][item]

                        t_full_name = os.path.join(t_path,item)

                        if( os.path.isdir(t_full_name) ):

                            for fname in h.get_subdir_files (t_full_name):
                                print("rm: %s" % fname)
                                os.remove(fname)
                            #os.rmdir(t_full_name)
                            h.remove_dir_all(t_fullname_n)


        #=============================================
        # Pr�fen, ob Files auf t_path okay sind
        # Es werden alle nicht dazugeh�rigen Files gel�scht
        #=============================================
        #==============================
        # DirListe von t_path erstellen
        #==============================
        try:
            if( os.path.isdir(t_path) ):
                t_liste   = os.listdir(t_path)
            else:
                t_liste = []
        except WindowsError:
            print("ZipBackup.backup_walk_tree.error: (target) os.listdir(\"%s\") not possible" % t_path)
            t_liste = []

        for item in t_liste:

            if( item == "hfkt.py" ):
                s_pathname = ""

            #item_n       = os.path.normcase(item)
            #t_fullname   = os.path.join(t_path,item)
            t_fullname_n = os.path.normcase(os.path.join(t_path,item))

            (t_path,t_body,t_ext)       = h.file_split(item)
            (t_path_n,t_body_n,t_ext_n) = h.file_split(os.path.normcase(item))

            #===============
            # Filebehandlung
            #===============
            if( os.path.isfile(t_fullname_n) ): #File

                # Wenn zip-File
                #==============
                if( t_ext_n == "zip" ):

                    # zerlegen
                    #=========
                    (s_file,mtime,okay)   = self.get_decomposed_zip_file_name(t_body)
                    #s_file_n              = os.path.normcase(s_file)

                    # Wenn zerlegbar, File in Liste und mtime gefunden
                    # dann Version pr�fen, ansosnten weg
                    if( okay                              and \
                        s_file in path_liste[0]           and \
                        mtime in path_liste[0][s_file][1]     ):

                        i   = path_liste[0][s_file][1].index(mtime)
                        ver = path_liste[0][s_file][0][i]

                        # Wenn Version kleiner erstg�ltigen, l�schen
                        #===========================================
                        if( ver < self.FIRST_VALID_VERSION ):

                            os.remove(t_fullname_n)
                    else:
                        os.remove(t_fullname_n)

                # Versionsfilename ist gesch�tzt
                #===============================
                elif( item != VERSION_FILE_NAME ):

                    os.remove(t_fullname_n)


            #===============
            # Dirbehandlung
            #===============
            elif( os.path.isdir(t_fullname_n) ): #Dir


                if( item not in path_liste[1] ):
                    h.remove_dir_all(t_fullname_n)
                    #os.rmdir(t_fullname_n)



        #=================================================
        # Von Zeit zu Zeit wird das Versionsfile gesichert
        # falls mal abgebrochen wird
        #=================================================
        if( int(time.time()) > (self.LAST_SAVE + self.DELTA_T_SAVE) ):
            if( self.VERSION_FILE_SAVE_DESIRED ):
                self.save_version("zwischen")
            self.LAST_SAVE = int(time.time())

        if( self.STATUS != OKAY ):
            print("backup_walk_tree.error: Status nicht okay")
            test = 0

        return self.STATUS


############################################################################
    def proof_extension_for_backup(self,s_ext_n):
        """ Prueft die Extension Liste aus der Eingabe, ob backup oder nicht
        """
############################################################################
        for ext_n in self.EXCLUDE_EXT_LIST_NORM:
            if( ext_n == s_ext_n ):
                return False
        return True
############################################################################
    def proof_path_for_backup(self,s_path,rel_path):
        """ Prueft die exclude path Liste aus der Eingabe, ob backup oder nicht
        """
############################################################################
        for path in self.EXCLUDE_PATH_LIST_NORM:
            if( path == s_path ):
                return False

        for path in self.EXCLUDE_REL_PATH_LIST_NORM:
            if( path == rel_path ):
                return False

        return True

############################################################################
    def proof_ver_and_make_zip_ver(self,zip_liste,full_file_name, \
                                   path,body,ext,mtime):
############################################################################
        """ 1. Pruefen ob File in Zip-Liste und ob Version erstellt werden soll
            2. Version erstellen

            Parameter:
            zip_liste = [[versions_liste],[mtime_liste],[exist_liste]]
        """
############################################################################

        if( len(zip_liste[0]) > 0 ): # Es gibt eine Version

            # maketime vergleichen
            if( mtime > zip_liste[1][-1] or \
                zip_liste[2][-1] == False    ): #Neue Version anlegen

                self.make_zip(zip_liste,full_file_name,path,body,ext,mtime)

                return 1

        else: # Es gibt noch keine Version

            #leere Liste anlegen
            self.make_zip(zip_liste,full_file_name,path,body,ext,mtime)

            return 1

        return 0

############################################################################
    def proof_and_unmake_zip_version(self,zip_liste,path,body,ext):
############################################################################
        """ 1. Pruefen ob letzte Version schon mit not exist gespeichert
            2. not exist Version erstellen

            Parameter:
            zip_liste = [[versions_liste],[mtime_liste],[exist_liste]]

            Rueckgabe:
            true: dict soll geloescht werden
        """
############################################################################

        if( len(zip_liste[0]) > 0 ): # Es gibt eine Version

            # maketime vergleichen
            if( zip_liste[2][-1] ): #letzte Version existierte noch

                zip_liste[0].append(self.D[A_ACT_VERSION])
                zip_liste[1].append(int(time.time()))
                zip_liste[2].append(False)

            return self.proof_zip_version_for_del(zip_liste,path,body,ext)
        else:
            return True
############################################################################
    def proof_versions_with_zip_files(self,zip_liste,path,body,ext):
############################################################################
        """ Pr�ft ob alle Versionen in zipliste auf Platte vorhanden sind
            Wenn nein, aus zipliste l�schen
        """
        if( len(zip_liste[0]) > 0 ): # Es gibt eine Version

            flag = True
            i    = 0
            while flag :

                (zip_file_name,zip_full_file_name) = self.get_full_zip_file_name( \
                                                            zip_liste[1][i], \
                                                            path,            \
                                                            body,            \
                                                            ext)

                # Wenn File vorhanden, hochz�hlen
                if( os.path.isfile(zip_full_file_name) ):

                    i += 1
                # ansonsten l�sche aus Liste
                else:

                    del zip_liste[0][i]
                    del zip_liste[1][i]
                    del zip_liste[2][i]

                # Auf Ende der Liste pr�fen
                if( i >= len(zip_liste[1]) ):
                    flag = False

############################################################################

############################################################################
    def proof_zip_files_with_version(self,zip_liste,path,body,ext):
############################################################################
        """ 1. Pruefen ob Zip-Liste bereinigt werden muss

            Parameter:
            zip_liste = [[versions_liste],[mtime_liste],[exist_liste]]

            Rueckgabe:
            true: dict soll geloescht werden
        """
############################################################################
        if( len(zip_liste[0]) > 0 ): # Es gibt eine Version

##            ver_liste   = zip_liste[0]
##            mtime_liste = zip_liste[1]
##            exist_liste = zip_liste[2]
##            n           = min(len(ver_liste),len(mtime_liste),len(exist_liste))
            #Pruefe die Versionen
            # Wenn die letzte Version nicht zu den noch zu speichernden gehoert
            # muss geprueft werden, ob diese noch vorhanden, wenn ja, dann muss
            # die Version hochgesetzt werden, ansonsten loeschen, da nicht mehr
            # existiert
            if( max(zip_liste[0]) < self.FIRST_VALID_VERSION ):
                max_flag = True
                max_ver  = max(zip_liste[0])
            else:
                max_flag = False
                max_ver  = 0

            flag = True
            i_liste = []
            while flag :
                flag = False
                for i in range(0,len(zip_liste[0])):

                    # Version hochsetzen, wenn File noch existiert
                    if( max_flag and                \
                        max_ver == zip_liste[0][i] and \
                        zip_liste[2][i]              ):

                        zip_liste[0][i] = self.FIRST_VALID_VERSION
                        max_flag        = False

                    # Wenn Version nicht mehr aufgehoben werden soll, loeschen
                    if( zip_liste[0][i] < self.FIRST_VALID_VERSION ):

                        # ZipFilename
                        if( zip_liste[2][i] ): # existiert

                            (zip_file_name,zip_full_file_name) =         \
                                            self.get_full_zip_file_name( \
                                                        zip_liste[1][i],  \
                                                        path,            \
                                                        body,            \
                                                        ext)

                            # Zipfile loeschen
                            if( os.path.isfile(zip_full_file_name) ):
                                print("Del(v%i): %s" % (zip_liste[0][i], zip_full_file_name))
                                os.remove(zip_full_file_name)

                        #Aus Liste loeschen
                        del zip_liste[0][i]
                        del zip_liste[1][i]
                        del zip_liste[2][i]

                        flag = True
                        break


############################################################################
    def make_zip(self,zip_l,full_file_name,path,body,ext,mtime):
        """ Erstellt ein Zip-File und Fuellst zip_liste auf
        """
############################################################################

        if( len(zip_l[0]) > 0 ):
            flag_new_item = 0
        else:
            flag_new_item = 1

        # zip_liste erweitern
        # version-liste
        zip_l[0].append(self.D[A_ACT_VERSION])
        # mtime-liste
        zip_l[1].append(mtime)
        # xist-liste
        zip_l[2].append(True)

        # ZipFilename
        (zip_file_name,zip_full_file_name) = self.get_full_zip_file_name( \
                                                    mtime, \
                                                    path,            \
                                                    body,            \
                                                    ext)
        if( len(ext) > 0 ):
            filename = body+"."+ext
        else:
            filename = body

        # Zipfile erstellen
        if( flag_new_item ):
            print("New File(%i): %s" % (int(self.D[A_ACT_VERSION]), zip_full_file_name))
        else:
            print("New Ver(%i): %s" % (int(self.D[A_ACT_VERSION]), zip_full_file_name))

        okay = zipfile.ZipSimple(zip_full_file_name,full_file_name,filename)

##        if( self.count == 170 ):
##            okay = okay
        if( not okay ):

            print("Fehler in ZipFile ")
            # kein Beenden des Backups
            # self.STATUS = NOT_OKAY

        # mindestens eine Version wurde erstellt
        self.NEW_VERSIONS_ZIPPED = 1
        self.VERSION_FILE_SAVE_DESIRED = 1

##        try:
##            print "New: %s" % zip_full_file_name
##            os.system("zip -q -j " + '"' + zip_full_file_name + '"' + " " + '"' + full_file_name + '"')
##
##            z = zipfile.ZipFile(zip_full_file_name,'w',zipfile.ZIP_DEFLATED)
##            # Zipfile fuellen
##            z.write(full_file_name)
##            # Zipfile schliessen
##            z.close()
##        except IOError:
##            print "warning: (IOError) new file %s was not possible" % zip_full_file_name
##        except WindowsError:
##            print "warning: (WindowsError) new file %s was not possible" % zip_full_file_name

############################################################################
    def get_full_zip_file_name(self,mtime,path,body,ext):
############################################################################

        # ZipFilename
        if( h.such(ext,"_","vs") >= 0 ):
            ext = h.change_max(ext,"_",UNDERLINE_CHANGE)

        zip_file_name = body+"_"+ext+"_" +  \
                        h.int_to_dec36(mtime,6) \
                        +".zip"
        zip_full_file_name = os.path.join(path,zip_file_name)

        return (zip_file_name,zip_full_file_name)

############################################################################
    def get_decomposed_zip_file_name(self,zipbody):
############################################################################
## (file_name,mtime,okay) = self.get_decomposed_zip_file_name(self,zip_body)
##  zerlegt zip_body aus "filename_ext_dec36code" in
## file_name = "filename.ext"
## mtime     = h.dec36_to_int(dec36code)
## okay      = 1, wenn alles gefunden
############################################################################

        file_name = ""
        mtime     = -1
        okay      = 0

        word = zipbody
        l = len(word)

        # erstes Trennungszeichen von hinten suchen
        #==========================================
        i = h.such(word,"_","rs")
        if( i > 0 ): # gefunden

            mtime    = h.dec36_to_int(word[i+1:l])
            # mtime mu� gr��er null sein
            if( mtime > 0 ):
                word = word[0:i]
                l    = len(word)

                # zweites Trennzeichen suchen
                i = h.such(word,"_","rs")
                if( i > 0 ): # gefunden

                    e = word[i+1:l]
                    b = word[0:i]
                    if( h.such(e,UNDERLINE_CHANGE,"vs") >= 0 ):
                        e = h.change_max(e,UNDERLINE_CHANGE,"_")
                    if( len(e) > 0 ):
                        file_name = b+"."+e
                    else:
                        file_name = b
                    okay = 1


        return (file_name,mtime,okay)

############################################################################
    def remove_zip_version(self,zip_l,i,path,body,ext):
############################################################################
        """ Loescht ein Zip-File und bereinigt zip_liste
        """
############################################################################


############################################################################
    def save_version(self,text):
        """ Speichert Version
        """
############################################################################
        f = gzip.open(self.VERSION_FILE,"wb")
        print("save(%s) versionfile: %s %s" % (text,time.ctime(),self.VERSION_FILE))
        pickle.dump(self.D,f,1)
        f.close()
        self.VERSION_FILE_SAVE_DESIRED = 0

############################################################################
if __name__ == '__main__':

    dict = {}
    # Quellpafad
    dict[A_SOURCE_PATH]  = "D:\\tools\\tools_anwendungen\\python\\_entw\\ZipBackup\\Source"
    # Zielpfad
    dict[A_TARGET_PATH]  = "D:\\tools\\tools_anwendungen\\python\\_entw\\ZipBackup\\Ziel"
    # Ausschlusspfade
    dict[A_EXCLUDE_PATH] = ["NoBackup"]
    dict[A_EXCLUDE_REL_PATH] = ["ex_path"]
    # Ausschlussextensions
    dict[A_EXCLUDE_EXT]  = [".obj","bak"]
    # maximale Backups aufheben
    dict[A_MAX_BACKUPS]  = 2



    b = Restore()
    if( b.STATUS == OKAY ):
        b.make_restore()
##    a = Backup(dict)
##    if( a.STATUS == OKAY ):
##        a.make_backup()

##    dict = {}
##    # Versionsfile
##    dict[A_VER_FILE]  = "Q:\\_frx10141\\laufwerk_d\\"+VERSION_FILE_NAME
##    # Welche Version zurueckholen (-1 letzte)
##    dict[A_RESTORE_VERSION]  = 25
##    # welcher Pfad zurueckholen
##    dict[A_RESTORE_SOURCE_PATH] = "Q:\\_frx10141\\laufwerk_d\\projekte\\hybrid\\sim_hybrid\\vmot_kennfeld"
##    # wohin zurueckholen
##    dict[A_RESTORE_TARGET_PATH] = "D:\\temp"
##
##    b = Restore("D:\\tools\\python\\_entw\\ZipBackup\\Restore",dict)
##    if( b.STATUS == OKAY ):
##        b.make_restore()

