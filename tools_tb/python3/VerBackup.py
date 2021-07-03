# -*- coding: utf8 -*-
############################################################################
############################################################################
############################################################################
import os, shutil, sys, time, string
import gzip, pickle
import hfkt as h
from stat import *
#import nzipfile
import zipfile

NAME_PROJECT_PATH          = "project_path"
NAME_EXCLUDE_REL_PATH_LIST = "exclude_rel_path_list"
NAME_EXCLUDE_ABS_PATH_LIST = "exclude_abs_path_list"
NAME_EXCLUDE_EXT_LIST      = "exclude_ext_list"
NAME_VERSION_COMMENT       = "version_comment"

NAME_PJ_VER                = "ver"
NAME_PJ_COMMENT            = "comment"
NAME_PJ_FILE_LIST          = "file_list"
NAME_PJ_VER_LIST           = "ver_list"
NAME_PJ_DAT_LIST           = "dat_list"

OKAY     = 1
NOT_OKAY = 0

VERSION_PATH_NAME          = "ver"
CANCEL_PATH_NAME           = "cancel"
PROJEKT_FILE_NAME          = "verproject.zic"
LOG_FILE_NAME              = "verlogfile.txt"

class project_data:
    def __init__(self):
        self.ver       = 0
        self.comment   = ""
        self.full_file_list = [] # volle Angabe
        self.ver_file_list  = [] # Verionsliste
        self.dat_file_list  = [] # Datum und Zeitliste
        self.pj_path_list   = [] # Pfadnameangabe
        self.file_name_list = [] # Dateiname

class VerBackup:
    """ Variablen:
        PROJECT_PATH            Zielpfad
        SOURCE_PATH_list        Quellpfad
        EXCLUDE_REL_PATH_LIST   Liste mit Verzeichnissen (relativ zu Quellpfad),
                                die nicht gespeichert werden
        EXCLUDE_REL_PATH_LIST   Liste mit Verzeichnissen (relativ zu Quellpfad),
                                die nicht gespeichert werden
        EXCLUDE_EXT_LIST        Liste mit Extensions, die nicht gespeichert werden

    """

    D                 = {}

############################################################################
    def __init__(self,dict):
############################################################################
        """ Steuerparameter, die ueber ein dictonary uebergeben werden,
            wird ausgelesen
        """
############################################################################

        self.STATUS = OKAY

        #------------------------
        # Zielverzeichnis pruefen
        #------------------------
        liste = dict.get(NAME_PROJECT_PATH)
        if( liste == None or len(liste) == 0):
            print("VerBackup.error: keyword: <%s> exisistiert nicht im uebergebenen Dictionary dict" % NAME_PROJECT_PATH)
            print(dict)
            self.STATUS = NOT_OKAY
            return

        if( h.is_string(liste) ):
            self.PROJECT_PATH = liste
        elif( h.is_list(liste) ):
            self.PROJECT_PATH = liste[0]

        if(  not h.is_string(self.PROJECT_PATH) ):
            print("VerBackup.error: <%s> aus Eingabe dictionary ist kein string " % NAME_PROJECT_PATH)
            self.STATUS = NOT_OKAY
            return

        self.PROJECT_PATH = h.elim_ae(self.PROJECT_PATH," ")

        self.PROJECT_PATH = os.path.normcase(self.PROJECT_PATH)
        if not os.path.isdir(self.PROJECT_PATH):
            try:
                os.makedirs(self.PROJECT_PATH)
                self.NEW_PROJECT_PATH_SET = 1
            except OSError:
                print("VerBackup.error: Das Zielverzeichnis %s kann nicht erstellt werden" % self.PROJECT_PATH)
                self.STATUS = NOT_OKAY
                return





        #----------------------
        #Exclude-Path (relativ)
        #----------------------
        liste = dict.get(NAME_EXCLUDE_REL_PATH_LIST)
        if( (liste != None) and (len(liste) != 0)):
            self.EXCLUDE_REL_PATH_LIST = liste
        else:
            self.EXCLUDE_REL_PATH_LIST = []

        if( not h.is_list(self.EXCLUDE_REL_PATH_LIST) ):
            print("VerBackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_EXCLUDE_REL_PATH_LIST)
            self.STATUS = NOT_OKAY
            return

        #----------------------
        #Exclude-Path (absolut)
        #----------------------
        liste = dict.get(NAME_EXCLUDE_ABS_PATH_LIST)
        if( liste == None or len(liste) == 0):
            liste = []

        if( not h.is_list(liste) ):
            print("VerBackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_EXCLUDE_ABS_PATH_LIST)
            self.STATUS = NOT_OKAY
            return
        else:
            self.EXCLUDE_PATH_LIST = liste





        #------------
        # Exclude-Ext
        #------------
        liste = dict.get(NAME_EXCLUDE_EXT_LIST)
        if( liste != None and len(liste) != 0):
            self.EXCLUDE_EXT_LIST = liste
        else:
            self.EXCLUDE_EXT_LIST = []

        if(  not h.is_list(self.EXCLUDE_EXT_LIST) ):
            print("VerBackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_EXCLUDE_EXT_LIST)
            self.STATUS = NOT_OKAY
            return self.STATUS

        # vom Punkt <.> bereinigen und normcase
        if( self.EXCLUDE_EXT_LIST ):
            for i in range(0,len(self.EXCLUDE_EXT_LIST)):
                val = self.EXCLUDE_EXT_LIST[i]
                i0  = val.find(".")
                if( i0 > -1 ):
                    i1 = len(val)
                    self.EXCLUDE_EXT_LIST[i] = os.path.normcase(val[i0+1:i1])



        return

############################################################################
    def make_ver(self,source_path_list,comment):
        """ Startet den versionsbackup-Vorgang
        """
############################################################################

        #-------------------------------
        # Logdatei anlegen, falls Fehler
        #-------------------------------
        self.LOG_FILE_FULL = os.path.join(self.PROJECT_PATH,LOG_FILE_NAME)
        self.LOG_FID       = open(self.LOG_FILE_FULL,'w')

        #-----------------
        # aktuelle Version
        #-----------------
        self.check_akt_version()
        if( self.STATUS != OKAY ):
            return self.STATUS

        #-------------------------
        # erste vorhandene Version
        #-------------------------
        self.check_min_version()
        if( self.STATUS != OKAY ):
            return self.STATUS

        #---------------------
        # neue Version anlegen
        #---------------------
        self.VER_NEW = self.VER_AKT+1

        # Pfadname erstellen
        self.VER_PATH_NAME_NEW  = VERSION_PATH_NAME + str(self.VER_NEW)
        # vollst�ndiger Pfadname
        self.VER_PATH_NEW       = os.path.join(self.PROJECT_PATH,self.VER_PATH_NAME_NEW)
        #Projektdaten anlegen
        self.VER_PJ_NEW         = project_data()
        #Version hinzuf�gen
        self.VER_PJ_NEW.ver = self.VER_NEW
        #Kommentar hinzuf�gen
        self.VER_PJ_NEW.comment = comment


        try:
            os.makedirs(self.VER_PATH_NEW)
        except OSError:
            print("VerBackup.error: Das Versionsverzeichnis %s kann nicht erstellt werden" % self.VER_PATH)
            self.STATUS = NOT_OKAY
            return

        #---------------------------------
        # vor�bergehende Logdatei beenden
        #---------------------------------
        self.LOG_FID.close()
        #----------------------------
        # echte Logdatei anlegen
        #----------------------------
        self.LOG_FILE_FULL = os.path.join(self.VER_PATH_NEW,LOG_FILE_NAME)
        self.LOG_FID       = open(self.LOG_FILE_FULL,'w')

        #----------------------------
        # checkt source_path_list
        #----------------------------

        self.STATUS = self.check_source_path(source_path_list)
        if( self.STATUS == NOT_OKAY ):
            self.cancel_new_version()
            return self.STATUS

        #-------
        # Header
        #-------
        self.LOG_FID.write("Version:        %i\n" % self.VER_NEW)
        self.LOG_FID.write("------------------------------------------------\n")
        self.LOG_FID.write("ProjectPath:    %s\n" % self.PROJECT_PATH)
        for s_path in self.FULL_SOURCE_PATH_LIST:
            self.LOG_FID.write("FullSourcePath: %s\n" % s_path)
        for s_path in self.SOURCE_PATH_NAME_LIST:
            self.LOG_FID.write("SourcePathName: %s\n" % s_path)
        for s_path in self.SOURCE_PATH_BASE_NAME_LIST:
            self.LOG_FID.write("SourcePathBase: %s\n" % s_path)
        for s_path in self.EXCLUDE_REL_PATH_LIST:
            self.LOG_FID.write("ExcludeRelPath: %s\n" % s_path)
        for s_path in self.EXCLUDE_ABS_PATH_LIST:
            self.LOG_FID.write("ExcludeAbsPath: %s\n" % s_path)
        for s_path in self.EXCLUDE_EXT_LIST:
            self.LOG_FID.write("ExcludeExt:     %s\n" % s_path)

        self.LOG_FID.write("------------------------------------------------\n")
        self.LOG_FID.write("VerPathNAme: %s\n" % self.VER_PATH_NAME_NEW)
        self.LOG_FID.write("%s\n" % time.strftime("%d.%m.%Y %H:%M:%S",time.localtime()))
        self.LOG_FID.write("------------------------------------------------\n")

        if(  h.is_string(comment) ):
            self.LOG_FID.write("Comment: %s\n" % comment)

        self.LOG_FID.write("------------------------------------------------\n")


        #Start
        start_text = time.ctime()

        # VerBackup starten
        #---------------
        for i in range(0,len(self.SOURCE_PATH_NAME_LIST)):

            # Startpunkt ist source-Pfad, (target-Pfad+ver+source-Pfad ergibt gesamtpfad f�r die Version),
            # in pj-File wird aber nur tail+file gespeichert, um unabh�ngig von target-path zu
            # sein (bei kopie z.B.), version kommt in extra-liste
            #----------------------------------------------------------------------------------
            self.SOURCE_PATH_BASE_NAME = self.SOURCE_PATH_BASE_NAME_LIST[i]

            self.STATUS = self.vbackup_walk_tree(self.SOURCE_PATH_NAME_LIST[i])

            if( self.STATUS == NOT_OKAY ):
                self.cancel_new_version()
                break

        #-----------------------------------
        # neue Versionprojektdatei speichern
        #-----------------------------------
        if( self.STATUS == OKAY ):
            self.write_projectfile(self.VER_PJ_NEW,self.VER_PATH_NEW)

        #-----------------
        # Logdatei beenden
        #-----------------
        self.LOG_FID.close()

        #Start,Ende
        print("Start: %s" % start_text)
        print("Ende: %s" % time.ctime())

        print("Version: %i" % self.VER_NEW)
        print("LogFile: %s" % self.LOG_FILE_FULL)

        return self.STATUS

############################################################################
    def check_source_path(self,source_path_list):
        """ checkt source_path_liste
        """
############################################################################

        #------------------------------
        # Quellverzeichnisliste pruefen
        #-------------------------------
        if( source_path_list == None or len(source_path_list) == 0):
            print("VerBackup.check_source_path.error: es wurde keine source_path_liste übergeben")
            self.LOG_FID.write("check_source_path.error: es wurde keine source_path_liste übergeben\n")
            self.STATUS = NOT_OKAY
            return self.STATUS

        if(  h.is_string(source_path_list)  ):
            self.FULL_SOURCE_PATH_LIST = []
            self.FULL_SOURCE_PATH_LIST.append(source_path_list)
        elif(  h.is_list(source_path_list) ):
            self.FULL_SOURCE_PATH_LIST = source_path_list

        if(  not h.is_list(self.FULL_SOURCE_PATH_LIST) ):
            print("VerBackup.check_source_path.error: aus Eingabe source_path_liste ist keine liste")
            self.LOG_FID.write("check_source_path.error: aus Eingabe source_path_liste ist keine liste\n")
            self.STATUS = NOT_OKAY
            return self.STATUS

        # Verzeichnisname
        self.SOURCE_PATH_NAME_LIST      = []
        self.SOURCE_PATH_BASE_NAME_LIST = []
        for i in range(0,len(self.FULL_SOURCE_PATH_LIST)):

            self.FULL_SOURCE_PATH_LIST[i] = h.elim_ae(self.FULL_SOURCE_PATH_LIST[i]," ")
            self.FULL_SOURCE_PATH_LIST[i] = os.path.normcase(self.FULL_SOURCE_PATH_LIST[i])

            if not os.path.isdir(self.FULL_SOURCE_PATH_LIST[i]):

                print("VerBackup.check_source_path.error: Das Quellverzeichnis <%s> kann nicht gefunden werden" % self.FULL_SOURCE_PATH_LIST[i])
                self.LOG_FID.write("VerBackup.check_source_path.error: Das Quellverzeichnis <%s> kann nicht gefunden werden\n" % self.FULL_SOURCE_PATH_LIST[i])
                self.STATUS = NOT_OKAY
                return self.STATUS

            i0 = h.such(self.FULL_SOURCE_PATH_LIST[i],"\\","rs")

            spath = self.FULL_SOURCE_PATH_LIST[i][0:i0]
            if( spath[len(spath)-1] == ':' ): # z. B. "c:" => "c:\\"
                spath = spath+"\\"

            self.SOURCE_PATH_NAME_LIST.append(self.FULL_SOURCE_PATH_LIST[i][i0+1:])
            self.SOURCE_PATH_BASE_NAME_LIST.append(spath)



        #gleiche Namen pr�fen
        for i in range(len(self.SOURCE_PATH_NAME_LIST)-1,-1,-1):

            for j in range(i-1,-1,-1):

                if( self.SOURCE_PATH_NAME_LIST[i] == self.SOURCE_PATH_NAME_LIST[j] ):
                    print("VerBackup.check_source_path.error: Source-Pfad <%s> kommt mehrfach vor" % self.SOURCE_PATH_NAME_LIST[i])
                    self.LOG_FID.write("VerBackup.check_source_path.error: Source-Pfad <%s> kommt mehrfach vor\n" % self.SOURCE_PATH_NAME_LIST[i])
                    print("Vergleiche <%s> " % self.FULL_SOURCE_PATH_LIST[i])
                    self.LOG_FID.write("Vergleiche <%s> \n" % self.FULL_SOURCE_PATH_LIST[i])
                    print("mit <%s> " % self.FULL_SOURCE_PATH_LIST[j])
                    self.LOG_FID.write("mit <%s> \n" % self.FULL_SOURCE_PATH_LIST[j])
                    self.STATUS = NOT_OKAY
                    return self.STATUS

        # absolute Ausschlu�-Pfad erstellen
        self.EXCLUDE_ABS_PATH_LIST = []
        if( self.EXCLUDE_PATH_LIST ):
            for item in self.EXCLUDE_PATH_LIST:

                item  = os.path.normcase(item)

                for path in self.FULL_SOURCE_PATH_LIST:

                    path  = os.path.normcase(path)
                    if( h.such(item,path,"vs") != 0 ):
                        self.EXCLUDE_ABS_PATH_LIST.append(os.path.join(path,item))
                    else:
                        self.EXCLUDE_ABS_PATH_LIST.append(item)

        return self.STATUS
############################################################################
    def check_akt_version(self):
        """ Sucht aktuelle Version
        """
############################################################################
        #---------------------------------------------
        # aktuelle Version anhand der Ordner bestimmen
        #---------------------------------------------
        self.VER_AKT = 0

        try:
            s_liste   = os.listdir(self.PROJECT_PATH)
        except WindowsError:
            print("VerBackup.check_akt_version.error: os.listdir(\"%s\") not possible" % self.PROJECT_PATH)
            self.LOG_FID.write("check_akt_version.error: os.listdir(\"%s\") not possible\n" % self.PROJECT_PATH)
            self.STATUS = NOT_OKAY
            return self.STATUS

        for item in s_liste:

            item = h.elim_ae(item," ")
            if( os.path.isdir(os.path.join(self.PROJECT_PATH,item)) ):

                i = h.such(item,VERSION_PATH_NAME,"vs")

                if( i == 0 ):

                    item = item[i+len(VERSION_PATH_NAME):]

                    try:
                        ver   = int(item)
                        self.VER_AKT = max(self.VER_AKT,ver)

                    except TypeError:

                        pass
                    except ValueError:
                        pass

        if( self.VER_AKT == 0 ):
            self.VER_PATH_NAME_AKT = ""
            self.VER_PATH_AKT      = ""
            self.VER_PJ_AKT        = project_data()
        else:
            self.VER_PATH_NAME_AKT = VERSION_PATH_NAME + str(self.VER_AKT)
            self.VER_PATH_AKT      = os.path.join(self.PROJECT_PATH,self.VER_PATH_NAME_AKT)
            self.VER_PJ_AKT        = self.read_projectfile(self.VER_PATH_AKT)

        return

############################################################################
    def check_min_version(self):
        """ Sucht aktuelle Version
        """
############################################################################
        #---------------------------------------------
        # aktuelle Version anhand der Ordner bestimmen
        #---------------------------------------------
        try:
            self.VER_MIN = self.VER_AKT
        except:
            self.check_akt_version()
            self.VER_MIN = self.VER_AKT

        try:
            s_liste   = os.listdir(self.PROJECT_PATH)
        except WindowsError:
            print("VerBackup.check_min_version.error: os.listdir(\"%s\") not possible" % self.PROJECT_PATH)
            self.LOG_FID.write("check_min_version.error: os.listdir(\"%s\") not possible\n" % self.PROJECT_PATH)
            self.STATUS = NOT_OKAY
            return self.STATUS

        for item in s_liste:

            item = h.elim_ae(item," ")
            if( os.path.isdir(os.path.join(self.PROJECT_PATH,item)) ):

                i = h.such(item,VERSION_PATH_NAME,"vs")

                if( i == 0 ):

                    item = item[i+len(VERSION_PATH_NAME):]

                    try:
                        ver   = int(item)
                        self.VER_MIN = min(self.VER_MIN,ver)

                    except TypeError:

                        pass
                    except ValueError:
                        pass

        if( self.VER_MIN == 0 ):
            self.VER_PATH_NAME_MIN = ""
            self.VER_PATH_MIN      = ""
            self.VER_PJ_MIN        = project_data()
        else:
            self.VER_PATH_NAME_MIN = VERSION_PATH_NAME + str(self.VER_MIN)
            self.VER_PATH_MIN      = os.path.join(self.PROJECT_PATH,self.VER_PATH_NAME_MIN)
            self.VER_PJ_MIN        = self.read_projectfile(self.VER_PATH_MIN)

        return

############################################################################
    def check_res_version(self,rver):
        """ Sucht restore Version
        """
############################################################################
        #-----------------------
        # retsore Version suchen
        #-----------------------
        found_flag = False
        try:
            s_liste   = os.listdir(self.PROJECT_PATH)
        except WindowsError:
            print("VerBackup.check_res_version.error: os.listdir(\"%s\") not possible" % self.PROJECT_PATH)
            self.LOG_FID.write("check_res_version.error: os.listdir(\"%s\") not possible\n" % self.PROJECT_PATH)
            self.STATUS = NOT_OKAY
            return self.STATUS

        for item in s_liste:

            item = h.elim_ae(item," ")
            if( os.path.isdir(os.path.join(self.PROJECT_PATH,item)) ):

                i = h.such(item,VERSION_PATH_NAME,"vs")

                if( i == 0 ):

                    item = item[i+len(VERSION_PATH_NAME):]

                    try:
                        ver   = int(item)
                        if( ver == rver ):
                            self.VER_RES = ver
                            found_flag   = True
                    except TypeError:

                        pass
                    except ValueError:
                        pass

        if( found_flag ):
            self.VER_PATH_NAME_RES = VERSION_PATH_NAME + str(self.VER_RES)
            self.VER_PATH_RES      = os.path.join(self.PROJECT_PATH,self.VER_PATH_NAME_RES)
            self.VER_PJ_RES        = self.read_projectfile(self.VER_PATH_RES)
        else:
            print("VerBackup.check_res_version.error: retsore version: <%i> not found, look in project_path: <%s>" % (rver,self.PROJECT_PATH))
            self.LOG_FID.write("VerBackup.check_res_version.error: retsore version: <%i> not found, look in project_path: <%s>\n" % (rver,self.PROJECT_PATH))
            self.STATUS = NOT_OKAY
            return self.STATUS

        return

############################################################################
    def read_projectfile(self,version_path):



        project_file = os.path.join(version_path,PROJEKT_FILE_NAME);

        if( os.path.exists(project_file) ):
            f = gzip.open(project_file,"rb")
            r = pickle.load(f)
            f.close()
        else:
            r = project_data()

        return r
############################################################################
    def write_projectfile(self,r,version_path):

        project_file = os.path.join(version_path,PROJEKT_FILE_NAME);
        f = gzip.open(project_file,"wb")
        pickle.dump(r,f,1)
        f.close()


##        if( os.path.isfile(project_file) ):
##
##            # Parameter einlesen
##            p = Param1.Param1(project_file)
##            if( p.STATUS == Param1.OKAY ):
##
##                # Versionsnummer
##                r.ver = p.get_int_single(NAME_PJ_VER)
##                if( not r.ver ): r.ver = 0
##
##                # Kommentar
##                r.comment = p.get_string_single(NAME_PJ_COMMENT)
##                if( not r.comment ): r.comment = ""
##
##                # volle Dateiliste
##                r.full_file_list = p.get_string_list(NAME_PJ_FILE_LIST)
##                if( not r.full_file_list ): r.full_file_list = []
##
##                # Pfadliste (ohne Versionspfad),Dateinamenlist
##                if( len(r.full_file_list) > 0 ):
##                    for f in r.full_file_list:
##                        pathname,fbody,ext = h.get_pfe(f)
##                        r.pj_path_list.append(pathname)
##                        r.file_name_list.append(fobdy+"."+ext)
##
##                # Versionsliste zu der Dateiliste
##                r.ver_file_list = p.get_int_list(NAME_PJ_VER_FILE_LIST)
##                if( not r.ver_file_list ): r.ver_file_list = []
##
##                # Datumsliste zu der Dateiliste
##                r.dat_file_list = p.get_int_list(NAME_PJ_DAT_FILE_LIST)
##                if( not r.dat_file_list ): r.dat_file_list = []
##
##        return r
############################################################################
    def cancel_new_version(self):

        # Cancel Pfadname
        flag = 1
        i    = 1
        while(flag):
            cancel_path_name = CANCEL_PATH_NAME+str(i)
            cancel_path   = os.path.join(self.PROJECT_PATH,cancel_path_name)
            if( os.path.exists(cancel_path) ):
                i += 1
            else:
                flag = 0

        # Pfad umbenennen
        try:
            os.renames(self.VER_PATH_NEW,cancel_path)
            self.LOG_FID.write("Cancel: Versionspad <%s>\n" % self.VER_PATH_NEW)
            self.LOG_FID.write("wurde umbenannt <%s>\n" % cancel_path)
        except:
            self.LOG_FID.write("error Cancel: Versionspad <%s>\n" % self.VER_PATH_NEW)
            self.LOG_FID.write("konnte nicht in <%s> umbenannt werden\n" % cancel_path)


############################################################################
    def vbackup_walk_tree(self, source_path_tail):
############################################################################
        '''recursively descend the directory tree rooted at top,
           calling the callback function for each regular file'''

        # Build  pathes
        #--------------
        source_path = os.path.join(self.SOURCE_PATH_BASE_NAME,source_path_tail)
        try:
            s_liste   = os.listdir(source_path)
        except WindowsError:
            print("VerBackup.vbackup_walk_tree.error: os.listdir(\"%s\") not possible" % source_path)
            self.LOG_FID.write("vbackup_walk_tree.error: os.listdir(\"%s\") not possible\n" % source_path)
            self.STATUS = NOT_OKAY
            return self.STATUS

        for s_filename in s_liste:

            s_full_filename = os.path.join(source_path,s_filename)

            if( os.path.isfile(s_full_filename) ): #File


                (s_body,s_ext) = os.path.splitext(s_filename)
                s_ext          = s_ext[1:]

                # Prueft die Ausschlussliste der extensions
                #-------------------------------------------
                if( self.proof_extension_for_backup(s_ext) ):

                    t_path_filename = os.path.join(source_path_tail,s_filename)

                    dattime = h.get_dattime_float(s_full_filename)
                    (flag,ind) = self.proof_new_version(dattime,t_path_filename)

                    # Neue Version erstellen
                    #-----------------------
                    if( flag ):

                        # Zielpfad bilden + pruefen
                        t_path = os.path.join(self.PROJECT_PATH,self.VER_PATH_NEW,source_path_tail)
                        if( not os.path.isdir(t_path) ):
                            try:
                                os.makedirs(t_path)
                            except OSError:
                                print("ZipBackup.vbackup_walk_tree.error: os.makedir(\"%s\") not possible" % t_path)
                                self.LOG_FID.write("vbackup_walk_tree.error: os.makedir(\"%s\") not possible \n" % t_path)
                                self.STATUS = NOT_OKAY
                                return self.STATUS


                        # ZipFilename
                        zip_file_name =s_body+"_"+s_ext+".zip"
                        zip_file_name =os.path.join(t_path,zip_file_name)
                        #okay = nzipfile.ZipSimple(zip_file_name,s_full_filename,s_filename)

                        with zipfile.ZipFile(zip_file_name,'w') as zf:
                            zf.write(s_full_filename,arcname=s_filename)
                            okay = 1
                        #end with

                        if( not okay ):

                            print("Fehler in ZipFile ")
                            self.STATUS = NOT_OKAY
                        else:
                            print("New File(%i): %s" % (int(self.VER_NEW), s_full_filename))

                        # neues Version in die Projectliste
                        try:
                            self.VER_PJ_NEW.full_file_list.append(t_path_filename)
                        except:
                            a = 0
                        
                        self.VER_PJ_NEW.file_name_list.append(s_filename)
                        self.VER_PJ_NEW.pj_path_list.append(source_path_tail)
                        self.VER_PJ_NEW.dat_file_list.append(dattime)
                        self.VER_PJ_NEW.ver_file_list.append(self.VER_NEW)

                        # Logfile protokollieren
                        self.LOG_FID.write("New File: %s\n" % t_path_filename)
                        self.LOG_FID.write("    Date: %-15.0f\n" % dattime)
                        self.LOG_FID.write("    Ver : %i\n" % self.VER_NEW)
                        self.LOG_FID.write("------------------------------------------------\n")
                    else:
                        # alte Version in Projekt-Liste
                        self.VER_PJ_NEW.full_file_list.append(self.VER_PJ_AKT.full_file_list[ind])
                        self.VER_PJ_NEW.file_name_list.append(self.VER_PJ_AKT.file_name_list[ind])
                        self.VER_PJ_NEW.pj_path_list.append(self.VER_PJ_AKT.pj_path_list[ind])
                        self.VER_PJ_NEW.dat_file_list.append(self.VER_PJ_AKT.dat_file_list[ind])
                        self.VER_PJ_NEW.ver_file_list.append(self.VER_PJ_AKT.ver_file_list[ind])


            elif( os.path.isdir(s_full_filename) ): #Dir

                # Prueft die Ausschlussliste der extensions
                if( self.proof_path_for_backup(s_full_filename) ):

                    if( self.vbackup_walk_tree(os.path.join(source_path_tail,s_filename)) != OKAY ):
                        return self.STATUS



        return self.STATUS


############################################################################
    def proof_extension_for_backup(self,s_ext):
        """ Prueft die Extension Liste aus der Eingabe, ob backup oder nicht
        """
############################################################################
        for ext in self.EXCLUDE_EXT_LIST:
            if( ext == s_ext ):
                return False
        return True

############################################################################
    def proof_new_version(self,dattime,t_path_filename):
        """ Prueft ob eine neue Version
        """

        # Pr�fen, ob in Versionsliste
        if( t_path_filename in self.VER_PJ_AKT.full_file_list ):
            ind = self.VER_PJ_AKT.full_file_list.index(t_path_filename)
        else:
            return (True,-1)

        # Datum pr�fen
        if( dattime > self.VER_PJ_AKT.dat_file_list[ind]+1 ):
            return (True,ind)

        return (False,ind)

############################################################################
    def proof_path_for_backup(self,s_path):
        """ Prueft die exclude path Liste aus der Eingabe, ob backup oder nicht
        """
############################################################################
        for path in self.EXCLUDE_ABS_PATH_LIST:
            if( path == s_path ):
                return False

        (body,leave) = os.path.split(s_path)
        for path in self.EXCLUDE_REL_PATH_LIST:
            if( path == leave ):
                return False

        return True

    def restore_version(self,rver,restore_path):

        # Restore-PAth erstellen und Logfile dort �ffnen
        #-----------------------------------------------
        if( not os.path.exists(restore_path) ):
            try:
                os.makedirs(restore_path)
            except OSError:
                print("ZipBackup.restore_version.error: os.makedir(\"%s\") restore_path not possible" % restore_path)
                self.LOG_FID.write("restore_version.error: os.makedir(\"%s\") restore_path not possible\n" % restore_path)
                self.STATUS = NOT_OKAY
                return self.STATUS

        #----------------------------
        # Logdatei anlegen
        #----------------------------
        self.LOG_FILE_FULL = os.path.join(restore_path,LOG_FILE_NAME)
        self.LOG_FID       = open(self.LOG_FILE_FULL,'w')


        #-----------------
        # aktuelle Version
        #-----------------
        self.check_akt_version()
        if( self.STATUS != OKAY ):
            self.LOG_FID.close()
            return self.STATUS

        #-------------------------
        # erste vorhandene Version
        #-------------------------
        self.check_min_version()
        if( self.STATUS != OKAY ):
            self.LOG_FID.close()
            return self.STATUS

        #-------------------------
        # restore Version
        #-------------------------
        self.check_res_version(rver)
        if( self.STATUS != OKAY ):
            self.LOG_FID.close()
            return self.STATUS

        #-------
        # Header
        #-------
        self.LOG_FID.write("Version:        %i\n" % self.VER_RES)
        self.LOG_FID.write("ProjectPath:    %s\n" % self.PROJECT_PATH)
        self.LOG_FID.write("------------------------------------------------\n")

        #Start
        start_text = time.ctime()

        # restoreliste abarbeiten
        #------------------------
        for i in range(0,len(self.VER_PJ_RES.full_file_list)):


            pathname,fbody,ext = h.get_pfe(self.VER_PJ_RES.full_file_list[i])
            full_zip_file = os.path.join(self.PROJECT_PATH,VERSION_PATH_NAME + str(self.VER_PJ_RES.ver_file_list[i]),pathname,fbody+"_"+ext+".zip")

            pj_path       = self.VER_PJ_RES.pj_path_list[i]
            rpj_path = os.path.join(restore_path,pj_path)

            if( not os.path.exists(rpj_path) ):
                try:
                    os.makedirs(rpj_path)
                except OSError:
                    print("ZipBackup.restore_version.error: os.makedir(\"%s\") project_restore_path not possible" % rpj_path)
                    self.LOG_FID.write("restore_version.error: os.makedir(\"%s\") project_restore_path not possible\n" % rpj_path)
                    self.STATUS = NOT_OKAY
                    return self.STATUS

            # Logfile protokollieren
            self.LOG_FID.write("New File: %s\n" % os.path.join(rpj_path,self.VER_PJ_RES.file_name_list[i]))
            self.LOG_FID.write("    Date: %-15.0f\n" % self.VER_PJ_RES.dat_file_list[i])
            self.LOG_FID.write("    Ver : %i\n" % self.VER_PJ_RES.ver_file_list[i])
            self.LOG_FID.write("------------------------------------------------\n")

            #okay = nzipfile.UnzipSimple(full_zip_file,rpj_path)
            if( os.path.isfile(full_zip_file)):
                with zipfile.ZipFile(full_zip_file,'r') as zf:
                    zf.extractall(path=rpj_path)
                    okay = 1
                #end with
            else:
                okay = 0
            #endif

            if( not okay ):
                print("ZipBackup.restore_version.warning: extract(\"%s\",\"%s\") not possible" % (full_zip_file,rpj_path))
                self.LOG_FID.write("ZipBackup.restore_version.error: UnzipSimple(\"%s\",\"%s\") not possible" % (full_zip_file,rpj_path))
                #self.STATUS = NOT_OKAY
                #self.LOG_FID.close()
                #return self.STATUS
            #endif




        #-----------------
        # Logdatei beenden
        #-----------------
        self.LOG_FID.close()

        #Start,Ende
        print("Start: %s" % start_text)
        print("Ende: %s" % time.ctime())

        print("Restored Version: %i" % self.VER_RES)
        print("LogFile:          %s" % self.LOG_FILE_FULL)

        return self.STATUS

    def restore_ver_gui(self):

        rver  = 0
        rpath = ""

        #----------------------------
        # Logdatei anlegen
        #----------------------------
        self.LOG_FILE_FULL = os.path.join(self.PROJECT_PATH,LOG_FILE_NAME)
        self.LOG_FID       = open(self.LOG_FILE_FULL,'w')

        #-----------------
        # aktuelle Version
        #-----------------
        self.check_akt_version()
        if( self.STATUS != OKAY ):
            self.LOG_FID.close()
            return self.STATUS,rver,rpath

        #-------------------------
        # erste vorhandene Version
        #-------------------------
        self.check_min_version()
        if( self.STATUS != OKAY ):
            self.LOG_FID.close()
            return self.STATUS,rver,rpath

        #-------------------------
        # Version ausw�hlen
        #-------------------------
        liste = []
        for i in range(self.VER_MIN,self.VER_AKT-self.VER_MIN+2):
            self.check_res_version(i)
            if( self.STATUS != OKAY ):
                self.LOG_FID.close()
                return self.STATUS,rver,rpath

            liste.append([self.VER_RES,self.VER_PJ_RES.comment])

        rver = h.abfrage_liste(liste,"Waehle Version fuer Restore aus")

        #----------------------------------
        # Zielpfad ausw�hlen
        #----------------------------------
        text  = "Suche Ziel-Verzeichnis aus"
        rpath = h.abfrage_dir(text)

        #-----------------
        # Logdatei beenden
        #-----------------
        self.LOG_FID.close()

        return self.STATUS,rver,rpath
############################################################################
if __name__ == '__main__':

    d = {}
    # Zielpfad
    d[NAME_PROJECT_PATH]  = 'D:\\temp\\backup'

    val = isinstance(d[NAME_PROJECT_PATH], str)
    # Extension
    d[NAME_EXCLUDE_EXT_LIST]  = [".obj","bak"]
    # Path
    d[NAME_EXCLUDE_REL_PATH_LIST]  = [".svn",".git",".vs",".vscode"]
    d[NAME_EXCLUDE_ABS_PATH_LIST]  = ["path_c"]


    # Kommentar
    comment = "erste Version"
    source_path_list = ["D:\\tools\\tools_anwendungen\\c_cpp"]

    b = VerBackup(d)
    if( b.STATUS == OKAY ):
        b.make_ver(source_path_list,comment)

    b.restore_version(1,"d:\\temp\\restore")
