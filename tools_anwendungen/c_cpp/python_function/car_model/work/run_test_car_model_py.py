import sys
import os

pathname = "D:/tools/tools_tb/python3" 
if( pathname not in sys.path ):
     sys.path.append(pathname)


import hfkt as h

import car_model_py as cm


# read parameterfile
(okay,txt) = h.read_ascii("testparameter.txt")

# send parameter file text
cm.set_param(txt)


# initialize
cm.init(0.01)

print( cm.erg )

# initialize
cm.first(2.0)

print( cm.erg )


for i in range(10):
  cm.loop(1.0)
#endfor

print( dir(cm))

print( cm.__doc__ )
print( cm.erg )
print( cm.param )


