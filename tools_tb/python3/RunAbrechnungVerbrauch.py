#-------------------------------------------------------------------------------
# Name:        RunAbrechnungVerbrauch (Python 3)
# Purpose:
#
# Author:      tftbe1
#
# Created:     02.01.2020
# Copyright:   (c) tftbe1 2020
# Licence:     -
#-------------------------------------------------------------------------------

import sys, os

tools_path = "D:\\tools_tb\\python3" # "D:\\tools\\python"

if( tools_path not in sys.path ):
    sys.path.append(tools_path)

import AbrechnungVerbrauch as av
# Hilfsfunktionen
import hfkt as h
import hfkt_def as hdef



a = av.AV(inifile="modul.ini",debug_flag=1)

if( a.status == hdef.OK ):
  a.run()
a.__del__()