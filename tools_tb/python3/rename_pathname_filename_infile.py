# -*- coding: cp1252 -*-
# rename path and filenames and content
#
import os
import hfkt as h



""" old_name_list = ["admotioncontrol"]
new_name_list = ["thomaspaulberthold"]
old_name_list = ["thomaspaulberthold"]
new_name_list = ["admotioncontrolvehicle"]
old_name_list = ["ADMotionControl"]
new_name_list = ["ThomasPaulBerthold"]
old_name_list = ["ThomasPaulBerthold"]
new_name_list = ["ADMotionControlVehicle"]


start_dir_vorschlag="D:\\RTAS_modules\\mod"

old_name_list = ["admotioncontrol"]
new_name_list = ["thomaspaulberthold"]
old_name_list = ["thomaspaulberthold"]
new_name_list = ["admotioncontrolcube"]
old_name_list = ["ADMotionControl"]
new_name_list = ["ThomasPaulBerthold"]
old_name_list = ["ThomasPaulBerthold"]
new_name_list = ["ADMotionControlCubE"]

start_dir_vorschlag="D:\\RTAS_work\\RPECU_B8_HUD_SAI\\linked_build\\app\\tc1793_dana\\prj\\phad2018_rtas_b8"
start_dir_vorschlag="D:\\RTAS_work\\RPECU_CUBE_JP1_PHAD2018\\linked_build\\app\\tricore_1793\\prj\\phad2018_rtas_cube"
start_dir_vorschlag="D:\\RTAS_work\\RPECU_CUBE_1_PHAD2018\\linked_build\\app\\tricore_1793\\prj\\phad2018_rtas_cube"

old_name_list = ["rpecu_b8_ad_tz"]
new_name_list = ["thomaspaulberthold"]
 """


start_dir_vorschlag="D:\\RTAS_modules\\mod"
old_name_list = ["ADMotionControlVehicle"]
new_name_list = ["ADMotionControlTest"]

change_path_name    = 1
change_file_name    = 1
change_file_content = 1

n = min(len(old_name_list),len(new_name_list))

start_dir = h.abfrage_dir(comment="Welches Verzeichnis �ndern",start_dir=start_dir_vorschlag)

for i in range(n):

  old_name = old_name_list[i]
  new_name = new_name_list[i]

  l_old_name = len(old_name)
  l_new_name = len(new_name)


  if( change_path_name ):
      liste = []
      liste = h.get_liste_of_subdirs(start_dir,liste=liste,include_start_dir=1)


      for i in range(len(liste)):
          item = liste[i]
          i0 = h.such(item,old_name,"vs")
          if( i0 >= 0 ):
              new_item = h.change_max(item,old_name,new_name)
              os.rename(item,new_item)
              if( item == start_dir ):
                  start_dir = new_item

              for j in range(len(liste)):
                  item1 = liste[j]
                  i0 = h.such(item1,item,"vs")
                  if( i0 >= 0 ):
                      new_item1 = h.change_max(item1,item,new_item)
                      liste[j] = new_item1


  liste = []
  liste = h.get_liste_of_subdir_files(start_dir,liste=liste)

  for item in liste:

      print(item)

      if( change_file_name ):

          (path,body,ext) = h.file_split(item)

          i0 = h.such(body,old_name,"vs")
          if( i0 >= 0 ):
              new_body = h.change_max(body,old_name,new_name)
              new_item = os.path.join(path,new_body+"."+ext)
              os.rename(item,new_item)
              item = new_item

      if( change_file_content and h.is_textfile(item) ):

          with open(item, 'r') as f:
            lines = f.readlines()
          #endwith

          with open(item, 'w') as f:
            for line in lines:
              i0 = h.such(line,old_name,"vs")
              if( i0 >= 0 ):
                  line = h.change_max(line,old_name,new_name)

              f.write(line)

          #endwith
print("---- Ende ----")



