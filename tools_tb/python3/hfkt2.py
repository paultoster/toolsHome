# -*- coding: utf8 -*-
###################################################################################
# string bearbeitung
###################################################################################
# b = change_item_byte_to_string(a,copyitem=False) 
#
#     if string, list or dictionary has b'name' inside it will be change to 'name'
# 
###################################################################################
# list calculation
###################################################################################
# ll = list_list_sort(ll,index_sort=0,reverse=False)
#
#      ll = [list1,list2,list3]   list in list  => if index_sort = 1 => list2 will be sorted and list1, list3 
#----------------------------------------------------------------------------------
# ll = list_list_elim_compare(ll,delta,index_compare=0,reverse=False)
#      ll = [list1,list2,list3]   list in list  => if index_compare = 1 => list2 will be compared 
#      if difference between list2[i+1] - list2[i] < delta => i+1 is eliminated in list1, list2, list3 (reverse = False) or i is eliminated reverse = True
#----------------------------------------------------------------------------------
# index = list_find_closest_value(list1,value)
#      list1 = [1,2,5,6]   value = 4  => index=2 
###################################################################################
import copy
import hfkt_def as hdef

#############################
#Stringbearbeitung
##################
def change_item_byte_to_string(a,copyitem=False):
#-------------------------------------------------------
  """
  change all bytes int string
  for bytes
      lists
      dictionary
  """
  if( copyitem ):  
    a = copy.copy(a)
  
  if( isinstance(a,bytes)):
    return change_byte_to_string(a)
  elif( isinstance(a,list)):
    return change_list_to_string(a)
  elif( isinstance(a,dict)):
    return change_dict_to_string(a)
  #endif
#-------------------------------------------------------
def change_byte_to_string(a,copyitem=False):
  if( copyitem ):  
    a = copy.copy(a)
  #endif
  if( isinstance(a,bytes)):
    try:
      return a.decode()
    except:
      b = ""
      for t in a:
          b += chr(t)
  else:
    return a
#-------------------------------------------------------
def change_list_to_string(a,copyitem=False):
  if( copyitem ):  
    a = copy.copy(a)
  #endif
  if( isinstance(a,list)):
    for i in range(len(a)):

      if(isinstance(a[i],bytes)):
        a[i]=change_byte_to_string(a[i])
      elif(isinstance(a[i],list)):
        a[i]=change_list_to_string(a[i])
      elif(isinstance(a[i],dict)):
        a[i]=change_dict_to_string(a[i])
      #endif

    #endfor
    return a
  else:
    return a
#-------------------------------------------------------
def change_dict_to_string(a,copyitem=False):
  if( copyitem ):  
    a = copy.copy(a)
  #endif
  if( isinstance(a,dict)):

    for key in a:

      if(isinstance(a[key],bytes)):
        a[key]=change_byte_to_string(a[key])
      elif(isinstance(a[key],list)):
        a[key]=change_list_to_string(a[key])
      elif(isinstance(a[key],dict)):
        a[key]=change_dict_to_string(a[key])
      #endif
    #endif

    keylist = list(a.keys())
    for key in keylist:
      if( isinstance(key,bytes)):
        key1 = change_byte_to_string(key,True)
        a[key1] = a[key]
        del a[key]
    #endfor
    return a
  else:
    return a
#enddef
def list_list_sort(ll,index_sort=0,reverse=False):
  """
  
  ll = list_list_sort(ll,index_sort=0,reverse=False)

  ll = [list1,list2,list3]   list in list  => 
  
  if index_sort = 1 => list2 will be sorted and list1, list3 accordingly
   
  """
  n1 = len(ll)
  while(n1):
    if( isinstance(ll[n1-1],list) ):
      break
    else:
      n1 -= 1
    #endif
  #endwhile

  if( n1 == 0):
    return ll
  #endif

  indexlist = [min(max(0,index_sort),n1-1)]
  nmax      = len(ll[indexlist[0]])

  for i in range(n1):

    if( i != indexlist[0] ):
      if( isinstance(ll[i],list) and len(ll[i]) >= nmax):
        indexlist.append(i)
      #endif
    #endif
  #endfor

  valuelist = copy.copy(indexlist)

  n2 = len(indexlist)

  for i in range(1,nmax):

    for ii in range(n2):
      iii  = indexlist[ii]
      valuelist[ii] = ll[iii][i]
    #endfor

    for j in range(i-1,-1,-1):

      if( reverse == True ):

        if( ll[indexlist[0]][j] < valuelist[0] ):

          for ii in range(n2):
            iii  = indexlist[ii]
            ll[iii][j+1] = ll[iii][j]
          #endfor

        else:

          if( i != j+1 ):
            for ii in range(n2):
              iii  = indexlist[ii]
              ll[iii][j+1] = valuelist[ii]
            #endfor
          #endif
          break

        #endif
      else:
        if( ll[indexlist[0]][j] > valuelist[0] ):

          for ii in range(n2):
            iii  = indexlist[ii]
            ll[iii][j+1] = ll[iii][j]
          #endfor

        else:

          if( i != j+1 ):
            for ii in range(n2):
              iii  = indexlist[ii]
              ll[iii][j+1] = valuelist[ii]
            #endfor
          #endif
          break

        #endif
      #endif
      if( j == 0):
        for ii in range(n2):
          iii  = indexlist[ii]
          ll[iii][0] = valuelist[ii]
        #endfor
      #endif
                      
    #endfor

  #endfor

  return ll

#enddef
def list_list_elim_compare(ll,delta,index_compare=0,reverse=False):
  """
       ll = list_list_elim_compare(ll,delta,index_compare=0,reverse=False)
#      ll = [list1,list2,list3]   list in list  => if index_compare = 1 => list2 will be compared 
#      if difference between list2[i+1] - list2[i] < delta => i+1 is eliminated in list1, list2, list3 (reverse = False) or i is eliminated reverse = True
  """
  n1 = len(ll)
  while(n1):
    if( isinstance(ll[n1-1],list) ):
      break
    else:
      n1 -= 1
    #endif
  #endwhile

  if( n1 == 0):
    return ll
  #endif

  indexlist = [min(max(0,index_compare),n1-1)]
  nmax      = len(ll[indexlist[0]])

  for i in range(n1):

    if( i != indexlist[0] ):
      if( isinstance(ll[i],list) and len(ll[i]) >= nmax):
        indexlist.append(i)
      #endif
    #endif
  #endfor

  n2 = len(indexlist)


  delta = abs(delta)

  i = 1
  while( i < nmax):
  
    if( abs(ll[indexlist[0]][i] - ll[indexlist[0]][i-1]) < delta ):
      
      if( reverse == True ):

        j = i-1

      else:

        j = i

      #endif

      for ii in range(n2):
        iii  = indexlist[ii]
        del(ll[iii][j])
      #endfor

      nmax -= 1

    else:
      i += 1
  #endwhile

  return ll

#enddef
def list_find_closest_value(list1,value):
  """
  index = list_find_closest_value(list1,value)
  e.g. list1 = [1,2,5,6]   value = 4  => index=2 
  """
  index = 0
  delta = abs(value-list1[0])
  for i in range(1,len(list1)):
    d = abs(value-list1[i])
    if( d < delta ):
      index = i
      delta = d
    #endif
  #endfor

  return index
###########################################################################
# testen mit main
###########################################################################
if __name__ == '__main__':

  # a = {b"abc":1,b"sdf":[b"abc",b'sss'],b"www":"fsfs"}

  # b = change_item_byte_to_string(a,True)
  # print(b)

  ll = [[4,3,2,1],[2,1,3,4],[1,1,1,3]]

  ll= list_list_sort(ll,index_sort=1,reverse=False)
  ll= list_list_elim_compare(ll,0.5,index_compare=1,reverse=False)
  lll = copy.copy(ll)
  ll = [[4,3,2,1],[2,1,3,4],[1,1,1,3]]
  ll= list_list_elim_compare(ll,0.5,index_compare=2,reverse=True)
  print(ll)
  
