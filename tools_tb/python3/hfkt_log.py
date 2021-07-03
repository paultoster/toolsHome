# -*- coding: utf8 -*-
#
# log: Logfile und terminal ausgabe
#
#  class log mit
#  log.write(text="",screen=0/1)       write text to cue / screen
#  log.write_e(text="",screen=0/1)     write with endl
#  log.write_err(text="",screen=0/1)   write as an error
#  log.write_warn(text="",screen=0/1)  write as  warning
#
# verionen:
# 0.0
#
import hfkt_def
import os
import sys

if(hfkt_def.DEFAULT_PYTHON_DIR not in sys.path):
  sys.path.append(hfkt_def.DEFAULT_PYTHON_DIR)
import hfkt as h

class log:
  state            = hfkt_def.OKAY
  logfile_out_flag = False
  log_message      = []
  def __init__(self,log_file=None):
    """ Log-Datei oeffnen
    """

    # Logfile-Name
    #-------------
    if( not log_file ):
      self.log_file         = hfkt_def.DEFAULT_LOG_FILE_NAME
    else:
      self.log_file         = log_file


    (path,body,ext) = h.file_split(self.log_file)

    if(len(path) == 0 ):
      path = os.path.abspath(os.curdir)
      self.log_file = os.path.join(path,self.log_file)

    self.open()
  #-----------------------------------------------------------------------------
  def __del__(self):
    self.close()
  #-----------------------------------------------------------------------------
  def open(self):
    # Log-File öffnen
    #----------------
    try:
      self.fid              = open(self.log_file,"w")
      self.logfile_out_flag = True

    except IOError:
      print("IO-error of opening log_file <%s>" % log_file)
      self.state            = hfkt_def.NOT_OK
      self.logfile_out_flag = False
      self.fid              = 0

  #-----------------------------------------------------------------------------
  def close(self):
    if( self.logfile_out_flag ):
      try:
          self.fid.close()
          self.logfile_out_flag = False
      except IOError:
          print("IO-error of close log_file <%s>" % self.log_file)
  #-----------------------------------------------------------------------------
  def write(self,text,screen=0):

    # log-message in Datei schreiben
    if( self.logfile_out_flag and (self.state == hfkt_def.OK) ):
      self.fid.write(h.to_bytes(text))
    # log-message in Buffer schreiben
    self.log_message.append(text)
    # log-message auf den Bildschirm schreiben
    if( screen ):
      print(text)
  #-----------------------------------------------------------------------------
  def write_e(self,text,screen=0):

    self.write(text+"\n",screen)
  #-----------------------------------------------------------------------------
  def write_err(self,text,screen=0):

    self.write("ERROR: "+text+"\n",screen)
  #-----------------------------------------------------------------------------
  def write_warn(self,text,screen=0):

    self.write("WARNING: "+text+"\n",screen)
  #-----------------------------------------------------------------------------
  def get_next_message(self):
    """
    nächsten Wert des Message-Buffers auslesen
    return Textzeile oder None
    """
    if( len(self.log_message) ):
      t                = self.log_message[0]
      self.log_message = self.log_message[1:]
    else:
      t                = None
    return t
  #-----------------------------------------------------------------------------
