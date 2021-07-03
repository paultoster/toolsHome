# -*- coding: cp1252 -*-
############################################################################
############################################################################
############################################################################
import os, shutil, sys, time, string, array
import hfkt as h
from stat import *

NAME_TARGET_PATH           = "target_path"
NAME_SOURCE_PATH           = "source_path"
NAME_EXCLUDE_REL_PATH_LIST = "exclude_rel_path_list"
NAME_EXCLUDE_ABS_PATH_LIST = "exclude_abs_path_list"
NAME_INCLUDE_EXT_LIST      = "include_ext_list"    # Wenn gesetzt, dann wird nur das kopiert
NAME_EXCLUDE_EXT_LIST      = "exclude_ext_list"
NAME_COPY_ALWAYS_FILE_LIST = "copy_always_file_list"
NAME_EXACT_COPY_FLAG       = "exact_copy_flag"
NAME_BACKUP_PATH           = "backup_path"

NOT_REMOVE_TARGET_LIST     = ["$recycle.bin","$RECYCLE.BIN","recycler","RECYCLER"]

OKAY     = 1
NOT_OKAY = 0

class copybackup:
    """ Variablen:
        TARGET_PATH         Zielpfad
        SOURCE_PATH         Quellpfad
        EXCLUDE_REL_PATH_LIST   Liste mit Verzeichnissen (relativ zu Quellpfad),
                                die nicht gespeichert werden
        EXCLUDE_REL_PATH_LIST   Liste mit Verzeichnissen (relativ zu Quellpfad),
                                die nicht gespeichert werden
        EXCLUDE_EXT_LIST    Liste mit Extensions, die nicht gespeichert werden
        INCLUDE_EXT_LIST    Liste mit Extensions, die gespeichert werden soll, d.h dann ist dir exlude-Liste inaktive
        NAME_COPY_ALWAYS_FILE_LIST Liste mit Filenamen, die immer copiert werden sollen
        EXACT_COPY_FLAG     auf beiden Seiten gleiche Dateien
        BACKUP_PATH         wenn angegeben, dann wird bevor �berschrieben sicherung kopiert, wird am Anfang gel�scht
    """

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
        liste = dict.get(NAME_TARGET_PATH)
        if( liste == None or len(liste) == 0):
            print("copybackup.error: keyword: <%s> exisistiert nicht im uebergebenen Dictionary dict" % NAME_TARGET_PATH)
            print(dict)
            self.STATUS = NOT_OKAY
            return

        if( h.is_string(liste) ):
            self.TARGET_PATH = liste
        elif( h.is_list(liste) ):
            self.TARGET_PATH = liste[0]

        if( not h.is_string(self.TARGET_PATH) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein string " % NAME_TARGET_PATH)
            self.STATUS = NOT_OKAY
            return

        self.TARGET_PATH = h.elim_ae(self.TARGET_PATH," ")

        self.TARGET_PATH = os.path.normcase(self.TARGET_PATH)
        if not os.path.isdir(self.TARGET_PATH):
            try:
                os.makedirs(self.TARGET_PATH)
                self.NEW_TARGET_PATH_SET = 1
            except OSError:
                print("copybackup.error: Das Zielverzeichnis %s kann nicht erstellt werden" % self.TARGET_PATH)
                self.STATUS = NOT_OKAY
                return

        #-------------------------
        # Quellverzeichnis pruefen
        #-------------------------
        liste = dict.get(NAME_SOURCE_PATH)
        if( liste == None or len(liste) == 0):
            print("copybackup.error: keyword: <%s> exisistiert nicht im uebergebenen Dictionary dict" % NAME_SOURCE_PATH)
            print(dict)
            self.STATUS = NOT_OKAY
            return

        if( h.is_string(liste) ):
            self.SOURCE_PATH = liste
        elif( h.is_list(liste) ):
            self.SOURCE_PATH = liste[0]

        if( not h.is_string(self.SOURCE_PATH) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein string " % NAME_SOURCE_PATH)
            self.STATUS = NOT_OKAY
            return

        self.SOURCE_PATH = h.elim_ae(self.SOURCE_PATH," ")

        self.SOURCE_PATH = os.path.normcase(self.SOURCE_PATH)

        if not os.path.isdir(self.SOURCE_PATH):

            print("copybackup.error: Das Quellverzeichnis <%s> kann nicht gefunden werden" % self.SOURCE_PATH)
            self.STATUS = NOT_OKAY
            return

        #----------------------
        #Exclude-Path (relativ)
        #----------------------
        liste = dict.get(NAME_EXCLUDE_REL_PATH_LIST)
        if( liste != None and len(liste) != 0):
            self.EXCLUDE_REL_PATH_LIST = liste
        else:
            self.EXCLUDE_REL_PATH_LIST = []

        if( not h.is_list(self.EXCLUDE_REL_PATH_LIST) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_EXCLUDE_REL_PATH_LIST)
            self.STATUS = NOT_OKAY
            return

        #----------------------
        #Exclude-Path (absolut)
        #----------------------
        liste = dict.get(NAME_EXCLUDE_ABS_PATH_LIST)
        if( liste != None and len(liste) != 0):
            self.EXCLUDE_ABS_PATH_LIST = liste
        else:
            self.EXCLUDE_ABS_PATH_LIST = []

        # absolute Pfad erstellen
        if( self.EXCLUDE_ABS_PATH_LIST ):
            for i in range(0,len(self.EXCLUDE_ABS_PATH_LIST)):

                self.EXCLUDE_ABS_PATH_LIST[i]  = os.path.normcase(self.EXCLUDE_ABS_PATH_LIST[i])

                if( h.such(self.EXCLUDE_ABS_PATH_LIST[i],self.SOURCE_PATH,"vs") != 0 ):
                    self.EXCLUDE_ABS_PATH_LIST[i] = os.path.join(self.SOURCE_PATH,self.EXCLUDE_ABS_PATH_LIST[i])

        if( not h.is_list(self.EXCLUDE_ABS_PATH_LIST) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_EXCLUDE_ABS_PATH_LIST)
            self.STATUS = NOT_OKAY
            return


        #------------
        # Include-Ext
        #------------
        liste = dict.get(NAME_INCLUDE_EXT_LIST)
        if( liste != None and len(liste) != 0):
            self.INCLUDE_EXT_LIST     = liste
            self.INCLUDE_EXT_LIST_SET = 1
        else:
            self.INCLUDE_EXT_LIST     = []
            self.INCLUDE_EXT_LIST_SET = 0

        if( not h.is_list(self.INCLUDE_EXT_LIST) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_INCLUDE_EXT_LIST)
            self.STATUS = NOT_OKAY
            return self.STATUS

        # vom Punkt <.> bereinigen und normcase
        if( self.INCLUDE_EXT_LIST ):
            for i in range(0,len(self.INCLUDE_EXT_LIST)):
                val = self.INCLUDE_EXT_LIST[i]
                i0  = val.find(".")
                if( i0 > -1 ):
                    i1 = len(val)
                    self.INCLUDE_EXT_LIST[i] = os.path.normcase(val[i0+1:i1])

        #------------
        # Exclude-Ext
        #------------
        liste = dict.get(NAME_EXCLUDE_EXT_LIST)
        if( liste != None and len(liste) != 0):
            self.EXCLUDE_EXT_LIST = liste
        else:
            self.EXCLUDE_EXT_LIST = []

        if( not h.is_list(self.EXCLUDE_EXT_LIST) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_EXCLUDE_EXT_LIST)
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

        #------------
        # Copy Always
        #------------
        liste = dict.get(NAME_COPY_ALWAYS_FILE_LIST)
        if( liste != None and len(liste) != 0):
            self.COPY_ALWAYS_FILE_LIST = liste
        else:
            self.COPY_ALWAYS_FILE_LIST = []

        if( not h.is_list(self.COPY_ALWAYS_FILE_LIST) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein Liste [] " % NAME_COPY_ALWAYS_FILE_LIST)
            self.STATUS = NOT_OKAY
            return self.STATUS                    

        #-------------------------
        # exact copy flag
        #-------------------------
        liste = dict.get(NAME_EXACT_COPY_FLAG)
        if( liste == None ):
            self.exact_copy_flag = 0
        else:
            if( h.is_list(liste) ):
                if( liste[0] ):
                    self.exact_copy_flag = 1
                else:
                    self.exact_copy_flag = 0
            elif( h.is_int(liste) ):
                if( liste ):
                    self.exact_copy_flag = 1
                else:
                    self.exact_copy_flag = 0
            else:
                self.exact_copy_flag = 0

        #------------------------
        # Backup Pfad
        #------------------------
        self.BACKUP_PATH_CLEANED = 0
        liste = dict.get(NAME_BACKUP_PATH)
        if( liste == None or len(liste) == 0):
            self.USE_BACKUP = 0

        if( h.is_string(liste) ):
            self.BACKUP_PATH = liste
            self.USE_BACKUP  = 1
        elif( h.is_list(liste) ):
            self.BACKUP_PATH = liste[0]
            self.USE_BACKUP  = 1

        if( self.USE_BACKUP ):
          if( not h.is_string(self.BACKUP_PATH) ):
            print("copybackup.error: <%s> aus Eingabe dictionary ist kein string " % NAME_BACKUP_PATH)
            self.STATUS = NOT_OKAY
            return

          self.BACKUP_PATH = h.elim_ae(self.BACKUP_PATH," ")

          self.BACKUP_PATH = os.path.normcase(self.BACKUP_PATH)
          if not os.path.isdir(self.BACKUP_PATH):
            try:
                os.makedirs(self.BACKUP_PATH)
                self.NEW_BACKUP_PATH_SET = 1
            except OSError:
                print("copybackup.error: Das Zielverzeichnis %s kann nicht erstellt werden" % self.BACKUP_PATH)
                self.STATUS = NOT_OKAY
                return




        print("")
        print("%30s = %s" % (NAME_TARGET_PATH,self.TARGET_PATH))
        print("%30s = %s" % (NAME_SOURCE_PATH,self.SOURCE_PATH))
        if( self.USE_BACKUP ):
          print("%30s = %s" % (NAME_BACKUP_PATH,self.BACKUP_PATH))

        for item in self.EXCLUDE_REL_PATH_LIST:
            print("%30s = %s" % (NAME_EXCLUDE_REL_PATH_LIST,item))
        for item in self.EXCLUDE_ABS_PATH_LIST:
            print("%30s = %s" % (NAME_EXCLUDE_ABS_PATH_LIST,item))
        for item in self.INCLUDE_EXT_LIST:
            print("%30s = %s" % (NAME_INCLUDE_EXT_LIST,item))
        for item in self.EXCLUDE_EXT_LIST:
            print("%30s = %s" % (NAME_EXCLUDE_EXT_LIST,item))
        for item in self.COPY_ALWAYS_FILE_LIST:
            print("%30s = %s" % (NAME_COPY_ALWAYS_FILE_LIST,item))

            

        print("%30s = %i" % (NAME_EXACT_COPY_FLAG,self.exact_copy_flag))
        print("")




        return
############################################################################
    def make_backup(self):
        """ Startet den backup-Vorgang
        """
############################################################################

        #Start
        start_text = time.ctime()

        # backup starten
        self.backup_walk_tree(self.SOURCE_PATH,self.TARGET_PATH)
 
        #Start,Ende
        print("Start: %s" % start_text)
        print("Ende: %s" % time.ctime())

############################################################################
    def backup_walk_tree(self, s_path, t_path):
############################################################################
        '''recursively descend the directory tree rooted at top,
           calling the callback function for each regular file'''

#        print("test",path_liste)
        try:
            s_liste   = os.listdir(s_path)
        except WindowsError:
            print("copybackup.backup_walk_tree.error: os.listdir(\"%s\") not possible" % s_path)
            self.STATUS = NOT_OKAY
            return self.STATUS

        s_liste_norm = []
        slen         = len(s_liste)
        for s_filename in s_liste:
            s_liste_norm.append(os.path.normcase(s_filename))

##            s_path_filename = os.path.join(s_path,s_filename)
##
##            a = os.path.isabs(s_path_filename)
##            b = os.path.isdir(s_path_filename)
##            c = os.path.isfile(s_path_filename)
##            d = os.path.islink(s_path_filename)
##            e = os.path.ismount(s_path_filename)
        nogo_liste = []
        for s_filename in s_liste:

            s_path_filename = os.path.join(s_path,s_filename)

##            if( h.such(s_path_filename,"rec","vs") >= 0 ):
##                a=0
##            else:
##                a=1

            if( os.path.isfile(s_path_filename) ): #File


                (s_body,s_ext) = os.path.splitext(s_filename)
                s_ext          = s_ext[1:]

                # Proof if always to copy
                if( self.proof_always_copy_file_list(s_filename) ):
                    copy_flag = 1
                else:
                    copy_flag = 0

                # Prueft die Ausschlussliste der extensions
                if( copy_flag or self.proof_extension_for_backup(os.path.normcase(s_ext)) ):

                    # Zielpfad pruefen
                    if( not os.path.isdir(t_path) ):
                        try:
                            os.makedirs(t_path)
                        except OSError:
                            print("copybackup.backup_walk_tree.error: os.makedir(\"%s\") not possible" % t_path)
                            self.STATUS = NOT_OKAY
                            return self.STATUS
                    #endif
                    # target-path-filename bestimmen
                    t_path_filename = os.path.join(t_path,s_filename)

                    # Namensl�nge
                    len_name = len(s_filename)

                    self.proof_and_make_copy(s_path_filename,   \
                                             t_path_filename,   \
                                             len_name,copy_flag)
                #endif
            elif( os.path.isdir(s_path_filename) ): #Dir

                try:
                  s_liste   = os.listdir(s_path_filename)
                  flag = 1
                except WindowsError:
                  flag = 0
                  nogo_liste.append(os.path.normcase(s_filename))
                #endtry
                # Prueft die Ausschlussliste 
                if( flag and self.proof_path_for_backup(s_path_filename,0) ):

                  t_path_filename =os.path.join(t_path,s_filename)

                  if not os.path.isdir(t_path_filename):
                    try:
                      os.makedirs(t_path_filename)

                    except OSError:
                      print("copybackup.error: Das Zielverzeichnis %s kann nicht erstellt werden" % t_path_filename)
                      self.STATUS = NOT_OKAY
                      return
                    #endtry
                  #endif


                  if( flag and self.backup_walk_tree(s_path_filename                       \
                                                    ,os.path.join(t_path,s_filename)       \
                                                    ) != OKAY ):
                    return self.STATUS
                  #endif
        #endfor
        if( self.exact_copy_flag ):
            # if( (t_path == "D:\\backup\\container_stick\\PortablePython_3251") or (t_path == "d:\\backup\\container_stick\\portablepython_3251")):
            #     t_liste = []
            # #endif
            # print(t_path)
            if( "portablepython_3251" in s_liste_norm):
                a =0
            #endif
            self.proof_exact_copy( t_path,nogo_liste,s_liste_norm)
        #endif
        return self.STATUS
############################################################################
    def proof_exact_copy(self, t_path,nogo_liste,s_liste_norm):
############################################################################
        try:
            t_liste   = os.listdir(t_path)
        except WindowsError:
            print("copybackup.backup_walk_tree.error: os.listdir(\"%s\") not possible" % t_path)
            self.STATUS = NOT_OKAY
            return self.STATUS
        #endtry
        for t_item in t_liste:

            t_filename      = os.path.normcase(t_item)
            t_path_filename = os.path.join(t_path,t_filename)
            if(   (   (t_filename not in nogo_liste) \
                    and (t_filename not in s_liste_norm) \
                    and (t_filename not in NOT_REMOVE_TARGET_LIST) \
                    )
                or not self.proof_path_for_backup(t_path_filename,1)
                ):

                if( os.path.isfile(t_path_filename) ):
                    print("Remove File: %s" % t_path_filename)
                    try:
                        os.remove(t_path_filename)
                    except WindowsError:
                        print("copybackup.backup_walk_tree.error: os.remove(\"%s\") not possible" % t_path_filename)
                elif(os.path.isdir(t_path_filename) ):
                    print("Remove Dir: %s" % t_path_filename)
                    try:
                        h.remove_dir_all(t_path_filename)
                    except WindowsError:
                        print("copybackup.backup_walk_tree.error: os.remove_dir_all(\"%s\") not possible" % t_path_filename)
                    #endtry
                #endif
            #endif
        #endfor

############################################################################
    def proof_always_copy_file_list(self,s_filename):
        """ Prueft der filename in der Always Copy Liste steht
        """
        for filename in self.COPY_ALWAYS_FILE_LIST:
            if( s_filename == filename ):
                return True
        return False
############################################################################

############################################################################
    def proof_extension_for_backup(self,s_ext):
        """ Prueft die Extension Liste aus der Eingabe, ob backup oder nicht
            Wenn include-Ext-Liste gew�hlt, wird geschaut, ob enthalten
            ansonsten wird exlude-Ext-Liste nachgeschaut zu excludieren
        """
############################################################################
        if( self.INCLUDE_EXT_LIST_SET ):
            for ext in self.INCLUDE_EXT_LIST:
                if( ext == s_ext ):
                    return True
            return False
        else:
            for ext in self.EXCLUDE_EXT_LIST:
                if( ext == s_ext ):
                    return False
            return True
############################################################################
    def proof_path_for_backup(self,s_path,exchange):
        """ Prueft die exclude path Liste aus der Eingabe, ob backup oder nicht
        """
############################################################################

        n_path = os.path.normcase(s_path)


        if(exchange):
            source_path = os.path.normcase(self.SOURCE_PATH)
            target_path = os.path.normcase(self.TARGET_PATH)
            n_path = h.change(n_path,target_path,source_path) 
            n_path = h.change_max(n_path,'\\\\','\\')
        #endif

        for path in self.EXCLUDE_ABS_PATH_LIST:
            if( os.path.normcase(path) == n_path ):
                return False
            #endif
        #endfor

        (body,leave) = os.path.split(n_path)
        for path in self.EXCLUDE_REL_PATH_LIST:
            if( os.path.normcase(path) == leave ):
                return False
            #endif
        #endfor

        return True

############################################################################
    def proof_and_make_copy(self,s_path_filename,t_path_filename,len_name,copy_flag):
############################################################################
        """ 1. Pruefen ob File in target noch nicht vorhanden
            2. Pruefen ob File in target �lter

            Kopieren
        """
############################################################################


        if( not os.path.isfile(t_path_filename)):
            copy_type = 0
            if( copy_flag ):
               copy_text = "AlwaysCopy"
            else:    
                copy_flag = 1
                copy_text = "New"
        else:
            copy_type = 1
            if( copy_flag ):
                copy_text = "AlwaysCopy"
            else:
                copy_text = "OVERWRITE"
            
            # maketime abfragen
            s_mtime = int(os.path.getmtime(s_path_filename))
            t_mtime = int(os.path.getmtime(t_path_filename))

            if( s_mtime > t_mtime ):
                copy_flag = 1
 
        if( copy_flag ):

            if( (copy_type == 1) and (self.USE_BACKUP == 1) ):
              self.make_copy_backup_to_backup_path(t_path_filename)

            if( len_name > 32 ):
                # print "Voricht: Filenamel�nge >32 File <%s>" % s_path_filename
                # print "Vorsicht:len>32"
                # print s_path_filename
                # copy_flag = 0
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
                    except WindowsError:
                        print("warning: (WindowsError) open file %s was not possible" % t_path_filename)
                except IOError:
                    print("warning: (IOError) open file %s was not possible" % s_path_filename)
                except WindowsError:
                    print("warning: (WindowsError) open file %s was not possible" % s_path_filename)


            else:
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
            print("-----------------------------------------------------------")

############################################################################
    def make_copy_backup_to_backup_path(self,t_path_filename):

############################################################################
        """ 1. L�scht Backup zu Beginn
            2. kopiert t_path_filename in Backup


        """
############################################################################
        if( self.BACKUP_PATH_CLEANED == 0 ):
          h.remove_all_in_dir(self.BACKUP_PATH)
          self.BACKUP_PATH_CLEANED = 1

        for t_path in self.TARGET_PATH:
          (okay,b_path_filename) = h.file_exchange_path(t_path_filename,t_path,self.BACKUP_PATH)
          if( okay == h.OK ):
            h.copy_build_path(t_path_filename,b_path_filename)
            break

############################################################################
if __name__ == '__main__':


  dict = {}
  # Quellpfad abfragen
  temp_dir = "D:\\cmd"
  temp_val = temp_dir.split("\\")
  temp_val = temp_val[-1]
  # Quellpfad
  dict[NAME_SOURCE_PATH]  = temp_dir

  # Zielpfad abfragen
  temp_dir = "D:\\temp\\backup"
  # Zielpfad
  dict[NAME_TARGET_PATH]  = temp_dir+"\\"+temp_val
  # Extension
  dict[NAME_EXCLUDE_EXT_LIST]  =['asc','hdf5']

  # relative Pfade
  dict[NAME_EXCLUDE_REL_PATH_LIST]  = ['stick_unzip']

  # absoluten Pfade
  dict[NAME_EXCLUDE_ABS_PATH_LIST]  = []

  dict[NAME_COPY_ALWAYS_FILE_LIST] = ['copy_laufwerk_u_auf_v.py']

  dict[NAME_EXACT_COPY_FLAG]  = 1

  b = copybackup(dict)
  if( b.STATUS == OKAY ):
    b.make_backup()
