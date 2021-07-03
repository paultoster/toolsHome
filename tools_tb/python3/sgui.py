# -*- coding: cp1252 -*-
########file test.py##########################################
#!/usr/bin/env python
#
## File ausw�hlen
## full_file_name = eingabe_file(file_types="*",comment="Waehle oder benenne neue Datei",start_dir=None)
## eingabe_file (FileSelectBox) um ein neues File zu generieren
##                             mit folgenden Parameter
## file_types = "*.c *.h"      Filetypen (auch feste namen m�glich "abc.py")
## start_dir  = "d:\\abc"	Anfangspfad
## comment    = "Suche Datei"  Windows Leisten text
# gui - Funktionen
#
# z.B. liste = ["alpha","beta","gamma"] , listeAbfrage = ["Ok","Cancel","�ndern"]
#
# (index,indexAbfrage)      = sgui.abfrage_liste_index_abfrage_index(liste,listeAbfrage):
# (indexListe,indexAbfrage) = sgui.abfrage_liste_indexListe_abfrage_index(liste,listeAbfrage):
# index                     = sgui.abfrage_liste_index(liste)
# indexListe                = sgui.abfrage_liste_indexListe(liste)
#
# z.B. listeAnzeige = ["Materialname","Materialmesseinheit","Materialabrechnungseinheit"]
#
# listeErgebnis = sgui.abfrage_n_eingabezeilen(listeAnzeige)

import tkinter as Tk
# import tkinter.filedialog
from tkinter.filedialog import askopenfilename
import tkinter.messagebox
import os
import sys
import types
import tkinter.tix
import string
import copy
import sstr

tools_path = "D:\\tools_tb\\python" 

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

OK     = 1
NOT_OK = 0
TCL_ALL_EVENTS		= 0

import hfkt     as h
import hfkt_def as hdef

#===============================================================================
#========================== abfrage_liste ======================================
def abfrage_liste_index_abfrage_index(liste,listeAbfrage,title=None):
  obj = abfrage_liste_class(liste,listeAbfrage,title)
  index        = obj.index
  indexAbfrage = obj.indexAbfrage
  del obj
  return (index,indexAbfrage)
def abfrage_liste_indexListe_abfrage_index(liste,listeAbfrage,title=None):
  obj = abfrage_liste_class(liste,listeAbfrage,title)
  indexListe   = obj.indexListe
  indexAbfrage = obj.indexAbfrage
  del obj
  return (indexListe,indexAbfrage)

def abfrage_liste_index(liste,title=None):
  obj = abfrage_liste_class(liste,title=title)
  index        = obj.index
  del obj
  return index
def abfrage_liste_indexListe(liste,title=None):
  obj = abfrage_liste_class(liste,title=title)
  indexListe   = obj.indexListe
  del obj
  return indexListe

class abfrage_liste_class:
  """
    Gui Abfrgae einer Liste, es k�nnen Buttons erstellt werden
    der Abfrage von listeAbfrage (default ['okay','cancel'])
    z.B.
    liste = ["abc","def","ghi",jkl"]
    listeAbfrage = ["cancel","gut"]
    obj   = sgui.abfrage_liste(liste,listeAbfrage)

    R�ckgabe:
    obj.index        erster Index
    obj.indexListe   Liste mit index zu vorgegebenen Liste
    obj.indexAbfrage index der Abfrage Liste

  """
  GUI_GEOMETRY_WIDTH    = 800
  GUI_GEOMETRY_HEIGHT   = 600
  GUI_GEOMETRY_POSX     = 0
  GUI_GEOMETRY_POSY     = 0
  GUI_ICON_FILE         = "SGUI.ico"
  GUI_TITLE             = "Liste"

  GUI_LISTE_ID          = 0

  state                 = hdef.OKAY
  str_liste             = []
  index_liste           = []
  str_auswahl_liste     = []
  index_auswahl_liste   = []
  indexListe            = []
  index                 = -1
  indexAbfrage          = -1
  act_frame_id          = 0
  Gui_rahmen            = [None]
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def __init__(self,liste,listeAbfrage = None,title = None):
    """
    """
    self.state                 = hdef.OKAY
    self.str_liste             = []
    self.index_liste           = []
    self.str_auswahl_liste     = []
    self.index_auswahl_liste   = []
    self.indexListe            = []
    self.abfrage_liste         = [u'okay',u'cancel']
    self.index                 = -1
    self.act_frame_id          = 0
    self.title                 = u"Liste"

    # liste in string-liste wandeln
    index = 0
    for item in liste:
      if( isinstance(item, str) ):
        self.str_liste.append(item)
      elif( isinstance(item, float) ):
        self.str_liste.append("%f" % item)
      elif( isinstance(item, int) ):
        self.str_liste.append("%i" % item)
      #endif
      self.index_liste.append(index)
      index += 1
    #endfor

    # Liste der Abfrage buttons
    if( listeAbfrage ):
      self.abfrage_liste = []
      for item in listeAbfrage:
        if( isinstance(item, str) ):
          self.abfrage_liste.append(item)
        elif( isinstance(item, float) ):
          self.abfrage_liste.append("%f" % item)
        elif( isinstance(item, int) ):
          self.abfrage_liste.append("%i" % item)
        #endif
    #endif

    # Titel:
    if( title and isinstance(title, str)):
      self.title = title
    #endif

    # Auswahlliste wird auf gesamte Liste gesetzt
    self.str_auswahl_liste   = self.str_liste
    self.index_auswahl_liste = self.index_liste


    # TK-Grafik anlegen
    #------------------
    self.root = Tk.Tk()
    self.root.protocol("WM_DELETE_WINDOW", self.exitMenu)
    #geo = str(self.GUI_GEOMETRY_WIDTH)+"x"+str(self.GUI_GEOMETRY_HEIGHT)
    #self.root.geometry(geo)
    self.root.wm_geometry("%dx%d+%d+%d" % (self.GUI_GEOMETRY_WIDTH, self.GUI_GEOMETRY_HEIGHT, self.GUI_GEOMETRY_POSX, self.GUI_GEOMETRY_POSY))

    if( os.path.isfile(self.GUI_ICON_FILE) ):
        self.root.wm_iconbitmap(self.GUI_ICON_FILE)
    self.root.title(self.GUI_TITLE)

    # Gui anlegen
    #--------------
    self.createListGui()

    # Menue anlegen
    #--------------
    # self.createMenu()
    self.makeListGui()
    self.flag_mainloop = True

    self.root.mainloop()

  def __del__(self):
      if(self.flag_mainloop):
        self.root.destroy()
        self.flag_mainloop = False
  def exitMenu(self):
    ''' Beenden der Gui
    '''
    # Vor Beenden Speichern abfragen
    # ans = tkinter.messagebox.askyesno(parent=self.root,title='Sichern', message='Soll Datenbasis gesichert werden')
    # if( ans ): self.base.save_db_file()

    if(self.flag_mainloop):
        self.root.destroy()
        self.flag_mainloop = False


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def createListGui(self):
    ''' Gui f�r Liste
    '''
    self.Gui_rahmen[self.GUI_LISTE_ID] = Tk.LabelFrame(self.root,bd=2,text=self.title,font=('Verdana',10,'bold'))

    gr_entry = Tk.Frame(self.Gui_rahmen[self.GUI_LISTE_ID],relief=Tk.GROOVE, bd=2)
    gr_entry.pack(pady=5)

    # label links oben mit text Filte
    label_a = Tk.Label(gr_entry,text='Filter:',font=('Verdana',10,'bold'))
    label_a.pack(side=Tk.LEFT,pady=1,padx=1)

    # entry StringVar f�r die Eingabe
    self.StringVarFiltText = Tk.StringVar()
    self.StringVarFiltText.set("")
    self.StringVarFiltText.trace("w",self.runDoFilter)

    # entry Aufruf
    entry_a = Tk.Entry(gr_entry,width=(100),textvariable=self.StringVarFiltText)
    entry_a.pack(side=Tk.LEFT,pady=1,padx=1)

##    button_a = Tk.Button(gr_entry,text='do', command=self.runDoFilter)
##    button_a.pack(side=Tk.RIGHT,pady=1,padx=1)

    gr_listbox = Tk.Frame(self.Gui_rahmen[self.GUI_LISTE_ID])
    gr_listbox.pack(expand=1,fill=Tk.BOTH)

    # Scrollbar
    scroll_listbox = Tk.Scrollbar(gr_listbox)
    scroll_listbox.pack(side=Tk.RIGHT,fill=Tk.Y)

    # Listbox
    self.listGui_ListBox = Tk.Listbox(gr_listbox,selectmode=Tk.EXTENDED,yscrollcommand=scroll_listbox.set,font=('Verdana',15,'bold'))
    self.listGui_ListBox.bind("<Double-1>", self.SelectOnDoubleClick)
    self.listGui_ListBox.pack(fill=Tk.BOTH, expand=1)

    scroll_listbox.config(command=self.listGui_ListBox.yview)


    gr_buts = Tk.Frame(self.Gui_rahmen[self.GUI_LISTE_ID],relief=Tk.GROOVE, bd=2)
    gr_buts.pack(fill=Tk.X,pady=5)

    self.Button = []
    for name in self.abfrage_liste:

      b_back = Tk.Button(gr_buts,text=name,command=lambda m=name: self.selectListGui(m))  # lambda m=method: self.populateMethod(m))
      b_back.pack(side=Tk.LEFT,pady=4,padx=2)
      self.Button.append(b_back)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def runDoFilter(self,*dummy):

    tt = self.StringVarFiltText.get()

    self.str_auswahl_liste   = []
    self.index_auswahl_liste = []

    for i in range(0,len(self.str_liste),1):

      ii = self.str_liste[i].find(tt)
      if( ii > -1 ):
        self.str_auswahl_liste.append(self.str_liste[i])
        self.index_auswahl_liste.append(self.index_liste[i])
      #endif
    #endfor

    self.makeListGui()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def makeListGui(self):
    ''' list-Gui f�llen
    '''

    # delete listbox
    n = self.listGui_ListBox.size()
    if( n > 0 ): self.listGui_ListBox.delete(0, n)

    self.listGui_ListBox.pack(expand=1,fill=Tk.BOTH)

    # Gruppenname aktualisieren
    # self.selectListGui()
    # fill listbox

    if( len(self.str_auswahl_liste) > 0 ):
      for item in self.str_auswahl_liste:
        self.listGui_ListBox.insert(Tk.END,item)
    else:
      text = 'keine Liste vorhanden'
      self.listGui_ListBox.insert(Tk.END,text)


    self.setActFrameID(self.GUI_LISTE_ID)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def selectListGui(self,button_name):
    ''' eine Gruppe ausw�hlen �ber selection, wenn nicht dann weiter
        aktuellen Namen verwenden
        Ergebnis wird in self.actual_group_name gespeichert
        R�ckgabewert:
        Wenn self.actual_group_name belegt ist, dann True
        ansonasten False
    '''
    # Nimmt den aktuellen Cursorstellung
    self.indexListe = []

    select = self.listGui_ListBox.curselection()

    if( len(select) > 0  ):
      for i in range(0,len(select),1):
        ii = select[i]
        if( ii < len(self.index_auswahl_liste) ):
          self.indexListe.append(self.index_auswahl_liste[ii])
        #endif
      #endfor
      if( len(self.indexListe) > 0 ):
        self.index = self.indexListe[0]
    #endif
    flag = False
    icount = 0
    for name in self.abfrage_liste:
      if( name == button_name ):
        flag = True
        self.indexAbfrage = icount
        break
      icount += 1
      #endif
    #endfor

    self.exitMenu()

  def SelectOnDoubleClick(self, event):
    ''' eine Gruppe ausw�hlen �ber selection, wenn nicht dann weiter
        aktuellen Namen verwenden
        Ergebnis wird in self.actual_group_name gespeichert
        R�ckgabewert:
        Wenn self.actual_group_name belegt ist, dann True
        ansonasten False
    '''
    # Nimmt den aktuellen Cursorstellung
    self.indexListe = []

    select = self.listGui_ListBox.curselection()

    if( len(select) > 0  ):
      flag_select = True
      for i in range(0,len(select),1):
        ii = select[i]
        if( ii < len(self.index_auswahl_liste) ):
          self.indexListe.append(self.index_auswahl_liste[ii])
        #endif
      #endfor
      if( len(self.indexListe) > 0 ):
        self.index = self.indexListe[0]
    else:
      flag_select = False
    #endif

    #default setzen erster
    self.indexAbfrage = 0

    if( flag_select ): self.exitMenu()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def setActFrameID(self,id):
    ''' Setzt Gui mit ID
    '''
    if( self.act_frame_id == self.GUI_LISTE_ID ):
      self.Gui_rahmen[self.GUI_LISTE_ID].pack_forget()

    self.act_frame_id = id
    if( self.act_frame_id == self.GUI_LISTE_ID ):
      self.Gui_rahmen[self.GUI_LISTE_ID].pack(expand=1,fill=Tk.BOTH)
#========================== abfrage_liste ======================================
#===============================================================================

#===============================================================================
#========================== abfrage_n_eingabezeilen ============================
def abfrage_n_eingabezeilen(liste,vorgabe_liste=None,title=None):
  """
    Gui Abfrage verschiedener Eingaben
    z.B.
    listeAnzeige = ["Materialname","Materialmesseinheit","Materialabrechnungseinheit"]
    listeErgebnis = sgui.abfrage_n_eingabezeilen(listeAnzeige)
  """
  obj = abfrage_n_eingabezeilen_class(liste=liste,vorgabe_liste=vorgabe_liste,title=title)
  liste = obj.eingabeListe
  del obj
  return liste
class abfrage_n_eingabezeilen_class:
  """
    Gui Abfrage verschiedener Eingaben
    z.B.
    liste = ["Materialname","Materialmesseinheit","Materialabrechnungseinheit"]
    obj   = sgui.abfrage_n_eingabezeilen_class(liste)

    R�ckgabe:
    obj.eingabeListe Liste mit Eingabe
  """
  GUI_GEOMETRY_WIDTH    = 800
  GUI_GEOMETRY_HEIGHT   = 600
  GUI_GEOMETRY_POSX     = 0
  GUI_GEOMETRY_POSY     = 0
  GUI_ICON_FILE         = "SGUI.ico"
  GUI_TITLE             = "Eingabe"

  GUI_LISTE_ID          = 0

  state                 = hdef.OKAY
  str_liste             = []
  eingabeListe          = []
  act_frame_id          = 0
  StringVarListe        = []
  Gui_rahmen            = [None]
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def __init__(self,liste,vorgabe_liste=None,title=None):
    """
    """
    self.state                 = hdef.OKAY
    self.str_liste             = []
    self.eingabeListe          = []
    self.act_frame_id          = 0
    self.StringVarListe        = []
    self.title                 = u"Eingabe"

    # liste in string-liste wandeln
    for item in liste:
      if( isinstance(item, str) ):
        self.str_liste.append(item)
      elif( isinstance(item, float) ):
        self.str_liste.append("%f" % item)
      elif( isinstance(item, int) ):
        self.str_liste.append("%i" % item)
      #endif
      self.eingabeListe.append("")
    #endfor

    if( vorgabe_liste ):
      n = min(len(self.eingabeListe),len(vorgabe_liste))
      for i in range(n):
        item = vorgabe_liste[i]
        if( isinstance(item, str) ):
          self.eingabeListe[i] = item
        elif( isinstance(item, float) ):
          self.eingabeListe[i] = "%f" % item
        elif( isinstance(item, int) ):
          self.eingabeListe[i] = "%i" % item
        #endif
      #endfor
    #endif

    # Titel:
    if( title and isinstance(title, str)):
      self.title = title
    #endif


    # TK-Grafik anlegen
    #------------------
    self.root = Tk.Tk()
    self.root.protocol("WM_DELETE_WINDOW", self.exitMenu)
    #geo = str(self.GUI_GEOMETRY_WIDTH)+"x"+str(self.GUI_GEOMETRY_HEIGHT)
    #self.root.geometry(geo)
    self.root.wm_geometry("%dx%d+%d+%d" % (self.GUI_GEOMETRY_WIDTH, self.GUI_GEOMETRY_HEIGHT, self.GUI_GEOMETRY_POSX, self.GUI_GEOMETRY_POSY))
    if( os.path.isfile(self.GUI_ICON_FILE) ):
        self.root.wm_iconbitmap(self.GUI_ICON_FILE)
    self.root.title(self.GUI_TITLE)

    # Gui anlegen
    #--------------
    self.createEingabeGui()

    # Menue anlegen
    #--------------
    # self.createMenu()
    self.makeEingabeGui()
    self.flag_mainloop = True

    self.root.mainloop()
 
  def __del__(self):
    if( self.flag_mainloop ):
        self.root.destroy()
        self.flag_mainloop = False

  def exitMenu(self):
    ''' Beenden der Gui
    '''
    # Vor Beenden Speichern abfragen
    # ans = tkinter.messagebox.askyesno(parent=self.root,title='Sichern', message='Soll Datenbasis gesichert werden')
    # if( ans ): self.base.save_db_file()

    if( self.flag_mainloop ):
        self.root.destroy()
        self.flag_mainloop = False

  def cancelMenu(self):
    ''' Cancel der Gui
    '''
    self.eingabeListe          = []
    self.exitMenu()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def createEingabeGui(self):
    ''' Gui f�r Eingabe
    '''
    self.Gui_rahmen[self.GUI_LISTE_ID] = Tk.LabelFrame(self.root,bd=2,text=self.title,font=('Verdana',10,'bold'))

    gr_canvas = Tk.Frame(self.Gui_rahmen[self.GUI_LISTE_ID])
    gr_canvas.pack(expand=1,fill=Tk.BOTH)

    # Listbox
    self.listGui_Canvas = Tk.Canvas(gr_canvas)
    # Scrollbar
    scroll_canvas = Tk.Scrollbar(gr_canvas,orient="vertical",command=self.listGui_Canvas.yview)
    self.listGui_Canvas.configure(yscrollcommand=scroll_canvas.set)

    scroll_canvas.pack(side=Tk.RIGHT,fill=Tk.Y)
    self.listGui_Canvas.pack(fill=Tk.BOTH, expand=1)

    frame=Tk.Frame(self.listGui_Canvas)
    self.listGui_Canvas.create_window((0,0),window=frame,anchor='nw')
    frame.bind("<Configure>",self.myfunction)

    for i in range(len(self.str_liste)):
      item = self.str_liste[i]
      vorg = self.eingabeListe[i]
      gr_entry = Tk.Frame(frame,relief=Tk.GROOVE, bd=2)
      gr_entry.pack(pady=5,fill=Tk.X)

      # label links oben mit text Filte
      label_a = Tk.Label(gr_entry,text=item+":",font=('Verdana',11,'bold'))
      label_a.pack(side=Tk.LEFT,pady=1,padx=1)

      # entry StringVar f�r die Eingabe
      stringVarFiltText = Tk.StringVar()
      stringVarFiltText.set(vorg)
      self.StringVarListe.append(stringVarFiltText)

      # entry Aufruf
      entry_a = Tk.Entry(gr_entry,width=(100),textvariable=stringVarFiltText)
      entry_a.pack(side=Tk.RIGHT,pady=1,padx=1)

    #endfor

    gr_buts = Tk.Frame(gr_canvas,relief=Tk.GROOVE, bd=2)
    gr_buts.pack(fill=Tk.X,pady=5)

    b_back = Tk.Button(gr_buts,text='Okay',command=self.getEntry)
    b_back.pack(side=Tk.LEFT,pady=4,padx=2)

    b_edit = Tk.Button(gr_buts,text='Cancel', command=self.cancelMenu)
    b_edit.pack(side=Tk.LEFT,pady=4,padx=2)

  def myfunction(self,event):
    self.listGui_Canvas.configure(scrollregion=self.listGui_Canvas.bbox("all"))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def makeEingabeGui(self):
    ''' Eingabe-Gui f�llen
    '''


    self.setActFrameID(self.GUI_LISTE_ID)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def getEntry(self):
    ''' eine Gruppe ausw�hlen �ber selection, wenn nicht dann weiter
        aktuellen Namen verwenden
        Ergebnis wird in self.actual_group_name gespeichert
        R�ckgabewert:
        Wenn self.actual_group_name belegt ist, dann True
        ansonasten False
    '''
    # Nimmt den aktuellen Cursorstellung
    self.eingabeListe = []

    for item in self.StringVarListe:

      tt = item.get()
      self.eingabeListe.append(tt)
    #endfor

    self.exitMenu()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def setActFrameID(self,id):
    ''' Setzt Gui mit ID
    '''
    if( self.act_frame_id == self.GUI_LISTE_ID ):
      self.Gui_rahmen[self.GUI_LISTE_ID].pack_forget()

    self.act_frame_id = id
    if( self.act_frame_id == self.GUI_LISTE_ID ):
      self.Gui_rahmen[self.GUI_LISTE_ID].pack(expand=1,fill=Tk.BOTH)
#========================== abfrage_n_eingabezeilen ============================
#===============================================================================
#===============================================================================
#========================== abfrage_janein ============================
def abfrage_janein(text=None,title=None):
  """
    Gui Abfrage Ja-Nein
    return True  (Ja)
           False (Nein)
  """
  obj = abfrage_janein_class(text=text,title=title)
  flagJa = obj.flagJa
  del obj

  if( flagJa ): return True
  else:         return False

class abfrage_janein_class:
  """
    Gui Abfrage Ja-Nein Frage
    z.B.

  """
  GUI_GEOMETRY_WIDTH    = 800
  GUI_GEOMETRY_HEIGHT   = 600
  GUI_GEOMETRY_POSX     = 0
  GUI_GEOMETRY_POSY     = 0
  GUI_ICON_FILE         = "SGUI.ico"
  GUI_TITLE             = "J/N"

  GUI_LISTE_ID          = 0

  state                 = hdef.OKAY
  act_frame_id          = 0
  Gui_rahmen            = [None]
  flagJa                = False
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def __init__(self,text=None,title=None):
    """
    """
    self.state                 = hdef.OKAY
    self.act_frame_id          = 0
    self.title                 = u"Eingabe"
    self.text                  = u"Ist das Okay?"


    # Text:
    if( text and isinstance(text, str)):
      self.text = text
    #endif

    # Titel:
    if( title and isinstance(title, str)):
      self.title = title
    #endif


    # TK-Grafik anlegen
    #------------------
    self.root = Tk.Tk()
    self.root.protocol("WM_DELETE_WINDOW", self.exitMenu)
    geo = str(self.GUI_GEOMETRY_WIDTH)+"x"+str(self.GUI_GEOMETRY_HEIGHT)
    self.root.geometry(geo)
    if( os.path.isfile(self.GUI_ICON_FILE) ):
        self.root.wm_iconbitmap(self.GUI_ICON_FILE)
    self.root.title(self.GUI_TITLE)

    # Gui anlegen
    #--------------
    self.createJaNeinGui()

    # Menue anlegen
    #--------------
    # self.createMenu()
    self.makeJaNeinGui()
    self.flag_mainloop = True

    self.root.mainloop()
 
  def __del__(self):
    if(self.flag_mainloop):
      self.root.destroy()
      self.flag_mainloop = False

  def exitMenu(self):
    ''' Beenden der Gui
    '''
    # Vor Beenden Speichern abfragen
    # ans = tkinter.messagebox.askyesno(parent=self.root,title='Sichern', message='Soll Datenbasis gesichert werden')
    # if( ans ): self.base.save_db_file()

    if(self.flag_mainloop):
      self.root.destroy()
      self.flag_mainloop = False


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def createJaNeinGui(self):
    ''' Gui f�r Eingabe
    '''
    self.Gui_rahmen[self.GUI_LISTE_ID] = Tk.LabelFrame(self.root,bd=2,text=self.title,font=('Verdana',10,'bold'))


    gr_entry = Tk.Frame(self.Gui_rahmen[self.GUI_LISTE_ID],relief=Tk.GROOVE, bd=2)
    gr_entry.pack(pady=5)

    self.gr_text_a = Tk.Text(gr_entry,width=120,height=16,font=('Verdana',15,'bold'))
    self.gr_text_a.pack(side=Tk.LEFT,pady=1,padx=1)

    gr_buts = Tk.Frame(self.Gui_rahmen[self.GUI_LISTE_ID],relief=Tk.GROOVE, bd=2)
    gr_buts.pack(fill=Tk.X,pady=5)

    b_back = Tk.Button(gr_buts,text='Ja(Yes)',command=self.getEntry)
    b_back.pack(side=Tk.LEFT,pady=4,padx=2)

    b_edit = Tk.Button(gr_buts,text='Nein(No)', command=self.exitMenu)
    b_edit.pack(side=Tk.LEFT,pady=4,padx=2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def makeJaNeinGui(self):
    ''' Eingabe-Gui f�llen
    '''

    self.gr_text_a.insert(Tk.END,self.text)
    self.setActFrameID(self.GUI_LISTE_ID)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def getEntry(self):
    '''
    '''
    # Ist Ja
    self.flagJa = True

    self.exitMenu()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
  def setActFrameID(self,id):
    ''' Setzt Gui mit ID
    '''
    if( self.act_frame_id == self.GUI_LISTE_ID ):
      self.Gui_rahmen[self.GUI_LISTE_ID].pack_forget()

    self.act_frame_id = id
    if( self.act_frame_id == self.GUI_LISTE_ID ):
      self.Gui_rahmen[self.GUI_LISTE_ID].pack(expand=1,fill=Tk.BOTH)
#========================== abfrage_n_eingabezeilen ============================
#===============================================================================
class slistbox1:
    def __init__(self,master,liste,smode="E"):
        self.name = "Listbox"
        self.items = []

        self.toplevel=Toplevel(master)
        self.toplevel.withdraw()

        self.result=StringVar()

        self.listbox = Listbox(self.toplevel,selectmode=smode)
        self.listbox.pack()

##        # liste auf H�he und Breite auswerten
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

        # Listbox f�llen
        for item in liste:
            print("items: %s" % item)
            self.listbox.insert('end',item)



    def get_items_and_close(self):

        # items abfragen
        items = self.listbox.curselection()

        # wegen alter Version transformieren
        try:
            items = map(string.atoi, items)
        except ValueError: pass

        # items an struktur �bergeben
        for i in items:
            self.items.append(int(i))

        # widget l�schen
        self.result.set("ok")
        self.toplevel.destroy()
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

        # Create the tixDirList and the tixLabelEntry widgets on the on the top
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
        if( not file_types or not isinstance(file_types, str)  ):
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
                       , relief=Tk.FLAT  \
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
                , side=tkinter.tix.TOP     \
                )

        top.fselect.pack( expand=1  \
                        , fill=tkinter.tix.BOTH \
                        , padx=4    \
                        , pady=4    \
                        , side=tkinter.tix.LEFT \
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


class slistbox:
    def __init__(self,liste,smode="E"):
        Tk.__init__(self)
        self.name = "Listbox"
        self.items = []

        # liste auf H�he und Breite auswerten
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

        # Listbox f�llen
        for item in liste:
            self.listbox.insert('end',item)


    def get_items_and_close(self, event):

        # items abfragen
        items = self.listbox.curselection()

        # wegen alter Version transformieren
        try:
            items = map(string.atoi, items)
        except ValueError: pass

        # items an struktur �bergeben
        for i in items:
            self.items.append(int(i))

        # widget l�schen
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
                       , relief=tkinter.tix.FLAT  \
                       , bd=20        \
                       )

##        top.label = tkinter.tix.Label(w, padx=20, pady=10, bd=1, relief=tkinter.tix.RAISED,
##		    anchor=tkinter.tix.CENTER, text=comment)
        top.label = tkinter.tix.Label(top, bd=1, relief=tkinter.tix.RAISED,
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
                , side=tkinter.tix.TOP     \
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
        liste       Liste von auszuw�hlend
        smode       "s" singlemode, nur ein item darf ausgew�hlt werden
                    "e" extended mehrere items
        Beispiel
        liste = ["Dieter","Roland","Dirk"]
        items = abfrage_listbox(liste,"s")
        for item in items:
            print "\nName: %s" % liste[item]
    """

    tk = Tk()
    t = slistbox1(tk,liste,smode)

    return t.items

def abfrage_listbox(liste,smode):
    """ Listenabfrage Auswahl eines oder mehrere items aus einer Liste
        Beispiel:
        liste       Liste von auszuw�hlend
        smode       "s" singlemode, nur ein item darf ausgew�hlt werden
                    "e" extended mehrere items
        Beispiel
        liste = ["Dieter","Roland","Dirk"]
        items = abfrage_listbox(liste,"s")
        for item in items:
            print "\nName: %s" % liste[item]
    """

    t = slistbox(liste=liste,smode=smode)
    t.mainloop()
    t.destroy()

    return t.items

def abfrage_liste_scroll(liste,comment=None,cols=70,rows=20,multi=0):
    """ Listenabfrage Auswahl eines items aus einer Liste
        wobei auf dem Bildschirm gescrollt wird (da lange und gro�)
        Wenn kein Wert zur�ckgegeben, dann okay = 0
        Es werden die Werte in einer Liste zur�ckgegeben, wenn multi=1
        Es k�nnen mehrere Werte getrennt mit Komma eingeben werden.
        Es wird nur ein Wert zur�ckgegeben, wenn multi=0 (default)

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
    if(   not h.is_list(liste[0]) \
      and not h.is_tuple(liste[0]) ):

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

        if( isinstance(liste[i][0], str)  ):
            tdum = "%3s" % liste[i][0]
        elif( isinstance(liste[i][0], int) ):
            tdum = "%3i" % liste[i][0]
        elif( isinstance(liste[i][0], float) ):
            tdum = "%3f" % liste[i][0]
        else:
            print("abfrage_liste_scroll.error: liste[i][0] hat nicht das korrekte Format")
            print(liste[i][0])
            return None, 0
        auswahl_liste.append(sstr.elim_ae(tdum,' '))
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
    if(  not h.is_list(liste[0])  \
      and not h.is_tuple(liste[0]) ):

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

                if( isinstance(liste[i][0], str) ):
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
def abfrage_ok_box(text="Ist das okay"):

    root = tkinter.tix.Tk()
    f = SOkCancelBox(root,text)
    f.mainloop()
    f.destroy()
    return f.OK_FLAG

def abfrage_dir(comment=None,start_dir=None):
    """ gui f�r Pfad auszuw�hlen """
    import traceback, tkinter.tix

    global dirlist

    try:
        root=tkinter.tix.Tk()
        dirlist = DirList(root,start_dir,comment)
        dirlist.mainloop()
        if( dirlist.dlist_dir == "" ):
            dirname = None
        else:
            dirname = dirlist.dlist_dir+"\\"
        dirlist.destroy()

        if( not os.path.exists(dirname) and dirname):

            comment = "Soll das Verzeichnis <%s> angelegt werden??" % dirname
            if( abfrage_ok_box(comment) ):
                os.makedirs(dirname)
            else:
                dirname = None


    except:
        t, v, tb = sys.exc_info()
        dirname = None
        text = "Error running the demo script:\n"
        for line in traceback.format_exception(t,v,tb):
            text = text + line + '\n'
            d = tkinter.messagebox.showerror ( 'tkinter.tix Demo Error', text)
    return dirname

def abfrage_sub_dir(comment=None,start_dir=None):
    """ gui f�r Unterpfad von start_dir auszuw�hlen """
    import traceback

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

        dirname1=sstr.change_max(dirname,"\\","/")
        start_dir1=sstr.change_max(start_dir,"\\","/")
        if( (not os.path.exists(dirname))           or \
            (not dirname)                           or \
            (sstr.such(sstr.change_max(string.lower(dirname),"\\","/"),          \
                  sstr.change_max(string.lower(start_dir),"\\","/"),"vs") != 0) ):

            print("Verzeichnis <%s> liegt nicht in der start_dir <%s>" \
                  % (dirname,start_dir))
        else:
            dir_not_found = False


    return dirname

def abfrage_file(file_types="*.*",comment=None,start_dir=None):
    """
    abfrage_file (FileSelectBox) um ein bestehendes Fiele einzuladen mit folgenden Parameter
    file_types = "*.c *.h"      Filetypen (auch feste namen m�glich "abc.py")
    start_dir  = "d:\\abc"	Anfangspfad
    comment    = "Suche Datei"  Windows Leisten text
    """
    # root = tkinter.tix.Tk()
    # f = SFileSelectBox(root,file_types,comment,start_dir)
    # f.mainloop()
    # f.destroy()
    # return f.SELECTED_FILE

    if( not comment ):
        comment = "search a file"

    if( not start_dir):
        start_dir = "D:/"

    ft = ("",file_types)

    root = tkinter.Tk()
    

    name = askopenfilename(parent=root,
                        initialdir=start_dir,
                        filetypes =(ft,("All Files","*.*")),
                        title = comment
                        )

    root.destroy()
    
    return name

def eingabe_file(file_types="*",comment="Waehle oder benenne neue Datei",start_dir=None):
    """
    eingabe_file (FileSelectBox) um ein neues File zu generieren
                                mit folgenden Parameter
    file_types = "*.c *.h"      Filetypen (auch feste namen m�glich "abc.py")
    start_dir  = "d:\\abc"	Anfangspfad
    comment    = "Suche Datei"  Windows Leisten text
    return selected_file or None
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
        R�ckgabe True (ja) oder False (nein)
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
        x = input(frage)
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




if __name__ == '__main__':

    listeAnzeige = ["Materialname","Materialmesseinheit","Materialabrechnungseinheit","Materialname","Materialmesseinheit","Materialabrechnungseinheit","Materialname","Materialmesseinheit","Materialabrechnungseinheit","Materialname","Materialmesseinheit","Materialabrechnungseinheit","Materialname","Materialmesseinheit","Materialabrechnungseinheit"]
    #listeAnzeige = ["Materialname","Materialmesseinheit","Materialabrechnungseinheit"]
    listeErgebnis = abfrage_n_eingabezeilen(listeAnzeige)
    print(listeErgebnis)

##    liste=[]
##    for i in range(0,10,1):
##        liste.append("abcdef "+chr(65+i))
##        # print ("%s" % liste[i])
##
##
##    [index,indexAbfrage] = abfrage_liste_index(liste)
##    print index
##    print indexAbfrage

###################################################################
