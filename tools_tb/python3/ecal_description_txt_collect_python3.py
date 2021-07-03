# -*- coding: cp1252 -*-
import sys
import os
import string
from tkinter import *
from tkinter.constants import *
import tkinter.filedialog



def get_liste_of_subdir_files(Path,liste=[]):
    """ Gibt eine Liste von allen Dateien in Unterverzeichnissen an
        tr�gt Ergebnis in liste ein un gibt sie zur�ck
        liste kann vorggeben werden
        liste = get_liste_of_subdir_files("d:\\abc"):
        oder
        vorgegebene_liste = get_liste_of_subdir_files("d:\\abc",vorgegebene_liste):
    """

    dirlist = os.listdir (Path)
    join = os.path.join
    isdir = os.path.isdir
    for basename in dirlist:
        fullPath = join (Path, basename)
        if isdir (fullPath):
            liste = get_liste_of_subdir_files(fullPath,liste)
        else:
            liste.append(fullPath)
    return liste

def file_split(file_name):
    """
    (path,body,ext) = file_split(file_name)

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
def change(text,muster_alt,muster_neu):
    """
    ersetzt einmal alle muster_alt mit muster_neu im Text
    """
    text = text.replace(muster_alt,muster_neu)
    #liste = text.split(muster_alt)
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
#-------------------------------------------------------
def get_path_name(pp):
  """
  """

  p = os.path.normpath(pp)

  ll = p.split('\\')

  n = len(ll)

  return ll[n-1]
#-------------------------------------------------------
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

#-------------------------------------------------------
def get_parent_path_dirs(pp,name_only=0):
  """
  liste_of_dirs  = get_parent_path_dirs(path)
  liste_of_names = get_parent_path_dirs(path,1)
  dir auf parent path and get dirs
  """
  liste = []
  if( os.path.isdir(pp) ):

    p = os.path.normpath(pp)

    ll = p.split('\\')

    n = len(ll)
    if( n > 1): n -= 1

    parent_dir = ''
    for i in range(n):
      parent_dir += ll[i]
      if( i < n-1 ): parent_dir += '\\'
    #endFor

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
#-------------------------------------------------------
def get_project_pathes(file_liste):

  project_pathes = []
  project_names  = []

  for f in file_liste:

    (t_path,t_body,t_ext) = file_split(f)

    if( (t_ext == 'hdf5') or  (t_ext == 'HDF5') ):
      liste = get_parent_path_dirs(t_path,1)

      if( ('doc' in liste) or ('DOC' in liste) ):

        t_path = get_parent_path(t_path)

        if( t_path not in project_pathes ):
          project_pathes.append(t_path)
          project_names.append(get_path_name(t_path))
        #endIf
      #endIf
    #endIf
  #endFor


  #endfor

  return (project_pathes,project_names)
  
def abfrage_dir(comment=None,start_dir=None):
    """ gui f�r Pfad auszuw�hlen """
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

############################################################################
KMEAS     = "measurement:"
KPATH     = "path:"
KCOM      = "Comment:"
KMATFILE  = "e-struct-matfile"

if __name__ == '__main__':

  ii = len(sys.argv)
  if( ii > 1 ):
    MAIN_PATH = sys.argv[1]
  else:
    MAIN_PATH = "D:\\"

  mdir = str(abfrage_dir("Verzeichnis mit Tacc-Messungen asuw�hlen",MAIN_PATH))

  if( os.path.exists(mdir) ):

    at_flag       = 1                        # all textfile read flag

    liste = mdir.split('\\')
    if( len(liste) > 1 ):
      add_name = "_"+liste[-1]
    else:
      add_name = ""

    out_file_name = os.path.join(mdir,"description"+add_name+".txt")
    csv_file_name = os.path.join(mdir,"description"+add_name+".csv")

    file_liste = get_liste_of_subdir_files(mdir)

    [project_pathes,project_names] = get_project_pathes(file_liste)
    n = len(project_names)

    tt = "search_path: %s" % os.path.abspath(mdir)

    with open(out_file_name,'w') as fout:

        for i in range(len(tt)):
            fout.write("=")
        fout.write("\n")
        fout.write(tt)
        fout.write("\n")
        for i in range(len(tt)):
            fout.write("=")
        fout.write("\n\n")

        with open(csv_file_name,'w') as fcsv:
            fcsv.write("\"%s\";" % tt)
            fcsv.write("\n")
            fcsv.write("\"%s\";" % KMEAS)
            fcsv.write("\"%s\";" % KPATH)
            fcsv.write("\"%s\";" % KCOM)
            fcsv.write("\"%s\";" % KMATFILE)
            fcsv.write("\n")

            for i in range(n):

                tt = "path: %s" % os.path.abspath(project_pathes[i])
                fout.write("\n%s\n" % tt)
                for j in range(len(tt)):
                    fout.write("=")
                fout.write("\n\n")

                doc_file = os.path.join(project_pathes[i],'doc','description.txt')
                try:
                    with open(doc_file,'r') as fin:
                        lines = fin.readlines()
                    #endwith
                except:
                    lines = []

                comment = ''
                for line in lines:
                    comment += line

                mat_file = os.path.join(project_pathes[i],project_names[i]+'_e.mat')

                if( os.path.isfile(mat_file)):
                    mat_file = project_names[i]+'_e.mat'
                else:
                    mat_file = ''
                #endif

                fcsv.write("\"%s\";" % project_names[i])
                fcsv.write("\"%s\";" % project_pathes[i])
                fcsv.write("\" %s \";" % comment)
                fcsv.write("\" %s \";" % mat_file)
                fcsv.write("\n")

                fout.write("============================================================\n")
                fout.write("%s\n" % project_pathes[i])
                fout.write("%s\n" % project_names[i])
                fout.write("%s\n" % comment)
                fout.write("============================================================\n")
            #endofr
        # endwith
    # endwith

    print ("Text-Datei: %s" % out_file_name)
    print ("CSV-Datei : %s" % csv_file_name)





