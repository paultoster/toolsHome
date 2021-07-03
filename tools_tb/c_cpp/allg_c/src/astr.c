/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/*------------------------------------------------------------------------
   1.0     25.02.03   TBert einfache string-funktionen 
  Version  Datum      Wer   Was
-------------------------------------------------------------------------*/
/*************************************************************************
* File:             astr.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            astr_s.c
* Datum:            25.02.03
*************************************************************************
* Kurzbeschreibung: 
*
* Stringbearbeitungsfunktionen: siehe astr_s.h
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "definer.h"
#include "astr.h"
#include "astrs.h"
#include "amem.h"


/*==========================================================================*/
SINT16_T astr_such(const char *txt1, const char *txt2, const char *regel)
{
  return astr_such_i0i1(txt1,txt2,regel,0,strlen(txt1)-1);
}
/*==========================================================================*/
SINT16_T astr_such_quot(char *txt,const char *txtsuch,const char *suchregel,
                        const char *txtquot0, const char *txtquot1,const char *quotregel,
                        UINT8_T inner)
/*
    Sucht string txtsuch in txt nach der suchregel
    suchregel = "vs" sucht      string von txtsuch         vorwaerts   in txt
                "rs" sucht      "                          rueckwaerts "
                "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
                "rn" sucht "                               rueckwaerts "
    wobei er innerhalb oder ausserhalb von quot0 und quot1 sucht entsprechend
    dem status istatus
    quotregel = "i" innerhalb
              = "a" ausserhalb

    istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots
              1   "                             innerhalb     " 

     return(-1)  nicht gefunden ansonsten
     return(i)   Stelle wo absolut gefunden beginnend mit 0
*/
{
  SINT16_T isuch;
  size_t i,j,i0,i1;
  size_t lentxt,lenquot0,lenquot1,lentsuch;
  size_t *ivsuch=NULL;
  BOOL_T found_flag,end_flag;
  
  if( strlen(txt)      == 0 ) return -1;
  if( strlen(txtsuch)  == 0 ) return -1;
  if( strlen(txtquot0) == 0 ) return -1;
  if( strlen(txtquot1) == 0 ) return -1;

  lentxt   = strlen(txt);
  lentsuch = strlen(txtsuch);
  lenquot0 = strlen(txtquot0);
  lenquot1 = strlen(txtquot1);

  amem_stmem_g(&ivsuch,lentxt);


  i = 0;
  while(i<lentxt) {

      if( inner ) { /* innerhalb */
          if( astr_such_i0i1(txt,txtquot1,"vs",i,i+lenquot1-1) >= 0 ) {
              inner = 0;
              for(j=0;j<lenquot1;j++) {
                ivsuch[i] = 0;
                i++;
              }
          } else {
              ivsuch[i] = 0;
              i++;
          }
      } else { /*ausserhalb*/
          if( astr_such_i0i1(txt,txtquot0,"vs",i,i+lenquot0-1) >= 0 ) {
              inner = 1;
              for(j=0;j<lenquot0;j++) {
                ivsuch[i] = 0;
                i++;
              }
          } else {
              ivsuch[i] = 1;
              i++;
          }
      }
  }
  if( *quotregel == 'i' ) { /* innerhalb ivquot invertieren */

      for(i=0;i<lentxt;i++) {
          if( ivsuch[i] )
              ivsuch[i] = 0;
          else
              ivsuch[i] = 1;
      }
  }

  if( *suchregel == 'v' || *suchregel == 'V' ) { /* vorwärst */

        i0 = 0;
        found_flag = 0;
        while( !found_flag && i0 < lentxt ) {
        
            isuch = astr_such_i0i1(txt,txtsuch,suchregel,i0,lentxt-1);
            if( isuch >= 0 ) { /*gefunden */

                i = isuch;
                found_flag = 1;
                for( j=0;j<lentsuch;j++) {
                    i0++;
                    if( !ivsuch[i+j] ) { /* prüfen ob nicht gültig */
                        found_flag = 0;                                                
                    }
                }
            } else {
                i0++;
            }
        }
        amem_stfree_g(&ivsuch);
        if( found_flag )
            return (SINT16_T)i;
        else
            return -1;
  } else { /* rückwärts */

        i1 = (size_t)MAX(0,(int)(lentxt-1));
        found_flag = 0;
        end_flag   = 0;
        while( !found_flag && !end_flag  ) {
        
            isuch = astr_such_i0i1(txt,txtsuch,suchregel,0,i1);
            if( isuch >= 0 ) { /*gefunden */

                i = isuch;
                found_flag = 1;
                for( j=0;j<lentsuch;j++) {
                    i1 = (size_t)MAX(0,(int)(i1-1));
                    if( !ivsuch[i+j] ) { /* prüfen ob nicht gültig */
                        found_flag = 0;
                    }
                }
            } else {
                if( i1 == 0 )
                    end_flag = 1;
                else
                    i1--;
            }
        }
        amem_stfree_g(&ivsuch);
        if( found_flag )
            return (SINT16_T)i;
        else
            return -1;
  }

#if 0
  SINT16_T isuch;
  size_t i,i0=0,i1=0,iact=0,iend;
  size_t lentxt,lenquot0,lenquot1;
  size_t *ivsuch;
  
  if( strlen(txt)      == 0 ) return -1;
  if( strlen(txtsuch)  == 0 ) return -1;
  if( strlen(txtquot0) == 0 ) return -1;
  if( strlen(txtquot1) == 0 ) return -1;

  iend = strlen(txt)- 1;
  if( *quotregel == 'i' )
  {
    while( iact < iend )
    {
      if( istatus == 0 ) /* ausserhalb des gequoteten */
      {
        if( (isuch = astr_such_i0i1(txt,txtquot0,"vs",iact,iend)) < 0 )
           return -1;
        i0 = isuch+strlen(txtquot0);
      }
      else
        istatus = 0;
      
      if( (isuch = astr_such_i0i1(txt,txtquot1,"vs",i0,iend)) < 0 )
         i1 = strlen(txt)-1;
      else
         i1 = isuch-1;
  
      if( (isuch=astr_such_i0i1(txt,txtsuch,suchregel,i0,i1)) >= 0 )
      {
          return isuch;
      }

      iact = i1+1+strlen(txtquot1);
    }
  }
  else
  {
    while( iact < iend )
    {
      if( istatus == 1 )
      {
        if( (isuch = astr_such_i0i1(txt,txtquot1,"vs",iact,iend)) < 0 )
           return -1;
        i0 = isuch+strlen(txtquot1);
      }
      else
        istatus = 1;
      
      if( (isuch = astr_such_i0i1(txt,txtquot0,"vs",i1,iend)) < 0 )
         i1 = strlen(txt)-1;
      else
         i1 = isuch-(int)1;
  
      if( ( isuch=astr_such_i0i1(txt,txtsuch,suchregel,i0,i1) ) >= (int)0 )
      {
         return isuch;
      }

      iact = i1+1+strlen(txtquot0);
    }
  }  
  return -1;    
#endif
}
/*==========================================================================*/
SINT16_T astr_such_i0i1(const char *txt, const char *txtsuch, const char *regel,
                      size_t ianf, size_t iend)
{
  size_t l2;
  size_t    i,j,k;

  if( strlen(txt)     == 0   ) return -1;
  if( strlen(txtsuch) == 0   ) return -1;
  if( strlen(regel)   == 0   ) return -1;
  
  l2 = strlen(txtsuch);  

  if( ianf >  iend           ) return -1;
  if( iend >  strlen(txt)-1  ) iend = strlen(txt)-1;
  if( l2   >  iend+1         ) return -1;

  if( *regel == 'v')
  {
    if( *(regel+1) == 's' )
    {
      for(j=ianf;j<iend-l2+2;j++)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i == 0) return (SINT16_T)j;
      }
    }
    else if( *(regel+1) == 'n' )
    {
      for(j=ianf;j<iend-l2+2;j++)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i != 0) return (SINT16_T)j;
      }
    }
    else
    {
      return -2;
    }
  }
  else if( *regel == 'r')
  {
    if( *(regel+1) == 's' )
    {
      for(j=iend-l2+1;j>=ianf;j--)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i == 0 ) return (SINT16_T)j;

        if( j == ianf )
            break;
      }
    }
    else if( *(regel+1) == 'n' )
    {
      for(j=iend-l2+1;j>=ianf;j--)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i != 0) return (SINT16_T)j;

        if( j == ianf )
            break;
      }
    }
    else
      return -1;
  }
  else
    return -1;
    
  return -1;
}
/*==========================================================================*/
SINT16_T astr_nsuch(const char *txt1, const char *txtsuch, size_t n)
{
  size_t    i,j=0,l=strlen(txtsuch);
  SINT16_T    isuch;

  for(i=0;i<n;i++)
  {
    if( (isuch=astr_such(txt1+j,txtsuch,"vs")) < 0 )
      return isuch;
    j = j+isuch+l;
  }  
  return (SINT16_T)(j-l);
}    
/*==========================================================================*/
size_t astr_count(const char *txt1, const char *txtsuch, size_t i0, size_t i1)
{
  size_t    i=i0,j=0;
  SINT16_T  isuch;
  
  while( (isuch=astr_such(txt1+i,txtsuch,"vs")) >= 0 && i+(size_t)isuch <= i1)
  {
    j++;
    i = i+(size_t)isuch+1;
  }      
  return j;
}    
/*==========================================================================*/
size_t astr_count_zeilen(const char *txt1)
{ 
    return (1+astr_count(txt1,"\n",0,strlen(txt1)-1));
}
/*==========================================================================*/
IERR_T astr_cut(char *txt1, size_t i0, size_t i1)
{
  size_t    i;

  if( strlen(txt1) == 0   ) return NOT_OK;
  if( i0 < 0              ) return NOT_OK;
  if( i0 > strlen(txt1)-1 ) return NOT_OK;
  if( i1 < i0             ) return NOT_OK;
  if( i1 >= strlen(txt1)  ) i1 = strlen(txt1)-1;

  
    i  = i0;
    i1 = i1+1-i0;
    while( (*(txt1+i) = *(txt1+i1+i)) != '\0' )
        i++;

  return 0;
}
/*==========================================================================*/
IERR_T astr_cut_ae(char *txt1,const char *txtcut)
{
  IERR_T ierr;
  if( (ierr = astr_cut_a(txt1,txtcut)) != OK )
      return ierr;
  if( (ierr = astr_cut_e(txt1,txtcut)) != OK )
      return ierr;
  return OK;
}
/*==========================================================================*/
IERR_T astr_cut_a(char *txt1,const char *txtcut)
{
    IERR_T ierr;
    int i = astr_such(txt1,txtcut,"vn");
    if( i > 0 )
        if( (ierr = astr_cut(txt1,0,(size_t)(i-1))) != OK )
            return ierr;
    return OK;
}
/*==========================================================================*/
IERR_T astr_cut_e(char *txt1,const char *txtcut)
{
  IERR_T ierr=OK;
  int i;
  
  if( strlen(txt1) > 0 ) {
      i = astr_such(txt1,txtcut,"rn");

      ierr = astr_cut(txt1,(size_t)i+1,(size_t)MAX((int)strlen(txt1)-1,0));
  }
  return ierr;
}
/*==========================================================================*/
IERR_T astr_cut_comment(char *txt,const char *txtcomment)
{
    SINT16_T i;
  
    if( (i=astr_such(txt,txtcomment,"vs")) >= 0 )
      return astr_cut(txt,(size_t)i,(size_t)(MAX((int)strlen(txt)-1,0)));  
    return OK;
}
/*==========================================================================*/
IERR_T astr_cat(char *txt1,const char *txt2, size_t i0, size_t i1)
{
  size_t l0,l1,l2,l3;
  size_t    i;

  l2 = strlen(txt1);
  l3 = strlen(txt2);

  l0 = i0;
  if( i0 < 0  ) l0 = 0;
  if( i0 > l3-1 ) return NOT_OK;

  l1 = i1;
  if( l1 > l3-1 ) l1 = l3-1;
  if( l1 < l0 ) return NOT_OK;

  for(i=0;i<=l1-l0;i++) 
      *(txt1+l2+i) = *(txt2+l0+i);
  *(txt1+l2+i) = '\0';
  return OK;
}
/*==========================================================================*/
IERR_T astr_cmp(const char *txt1,const char *txt2, size_t i0, size_t i1)
{
  size_t l2,l3;
  size_t    i;

  l2 = strlen(txt1);
  l3 = strlen(txt2);

  if( i0 < 0  ) i0 = 0;
  if( i0 > l3-1 || i0 > l2-1) return NOT_OK;
  if( i1 > l3-1 || i1 > l2-1) return NOT_OK;


  l3 = i1-i0;
  if( l3 < l2-1 )l3 = l2-1;

  for(i=0;i<=l3;i++) {
    if( *(txt1+i) != *(txt2+i0+i) )
      return NOT_OK;
  }
  return OK;
}
/*==========================================================================*/
IERR_T astr_red(char *txt1, size_t i0, size_t i1)
{
  size_t j0,j1;
  size_t    i;

  j0 = 0;
  j1 = strlen(txt1)-1;

  if( i0 < 0  || i1 < i0 || i0 > j1 ) return NOT_OK;

  for(i=0;i<=i1-i0;i++)
    txt1[i] = txt1[i0+i];
/*
  for(i=0;i<=j1-i1;i++) 
    txt1[i+i0] = txt1[i1+1+i];
*/    
  txt1[i1-i0+1] = '\0';
  return OK;
}
/*==========================================================================*/
IERR_T astr_insert(char *txt1,const char *txt2, size_t i0, size_t ninsert)
{
  size_t    i;

  if( ninsert > (int)strlen(txt2)  )ninsert = strlen(txt2);
  if( ninsert < 0 )return (int)-1;
  if( i0 < 0  ) return (int)-1;
  if( i0 > (int)strlen(txt1) )return (int)-1;

  for(i=strlen(txt1);i>=i0;i--)
    *(txt1+i+ninsert) = *(txt1+i);
  for(i=i0;i<=i0+ninsert-(int)1;i++)
    *(txt1+i) = *(txt2+i-i0);
  return (int)0;
}
/*==========================================================================*/
size_t astr_change(char *txt,const char *txtsuch,const char *txtchange)
{
  SINT16_T isuch;
  size_t i=0,ncount=0;
  
  while( (isuch=astr_such_i0i1(txt,txtsuch,"vs",i,strlen(txt)-1)) >= 0 )
  {
    i = isuch;
    astr_cut(txt,i,i+strlen(txtsuch)-1);
    if( *txtchange != '\0' )
      astr_insert(txt,txtchange,i,strlen(txtchange));
    i = i+1-strlen(txtsuch)+strlen(txtchange);
    ncount++;
  }
  
  return ncount;
}
/*==========================================================================*/
size_t astr_change_quot(char *txt, const char *txtsuch, const char *txtchange,
                       const char *txtquot0, const char *txtquot1, const char *regel,
                       UINT8_T istatus)
{
    UINT8_T iistatus = istatus;
    return astr_change_quot1(txt, txtsuch, txtchange, txtquot0, txtquot1, regel, &iistatus);
}  
/*==========================================================================*/
size_t astr_change_quot1(char *txt, const char *txtsuch, const char *txtchange,
                       const char *txtquot0, const char *txtquot1, const char *regel,
                       UINT8_T *pstatus)
{
    size_t iact = 0;
    size_t isuch_end;
    SINT16_T isuch;
    size_t ncount=0;
    UINT8_T istatus = *pstatus;

    if( *regel == 'i' || *regel == 'I') { /* Innerhalb des Quots tauschen */

        while( iact<strlen(txt) ) {
            
            if( istatus == 0 ){
            
                if( (isuch=astr_such_i0i1(txt,txtquot0,"vs",iact,strlen(txt)-1)) >= 0 ) {
                    istatus = 1;
                    iact = isuch + strlen(txtquot0);
                } else {
                    iact = strlen(txt);
                }
            }
            if( istatus == 1){
          
                isuch=astr_such_i0i1(txt,txtquot1,"vs",iact,strlen(txt)-1);
                if( isuch < 0 ){
                    isuch_end = strlen(txt)-1;
                    istatus = 1;
                }else{
                    isuch_end = isuch-1;
                    istatus=0;
                }

                while( ( isuch=astr_such_i0i1(txt,txtsuch,"vs",iact,isuch_end) ) >= 0 )
                {
                    astr_cut(txt,(size_t)isuch,(size_t)isuch+strlen(txtsuch)-1);
                    isuch_end -= strlen(txtsuch);
                    if( *txtchange != '\0' ) {
                        astr_insert(txt,txtchange,(size_t)isuch,strlen(txtchange));
                        isuch_end += strlen(txtchange);
                    }
                    ncount++;
                }
                iact=isuch_end+strlen(txtquot1);
            }
        }
    } else { /* Ausserhalb des Quots tauschen */
        while( iact<strlen(txt) ) {
            
            if( istatus == 1 ){
            
                if( (isuch=astr_such_i0i1(txt,txtquot1,"vs",iact,strlen(txt)-1)) >= 0 ) {
                    istatus = 0;
                    iact = isuch + strlen(txtquot1);
                } else {
                    iact = strlen(txt);
                }
            }
            if( istatus == 0){
          
                isuch=astr_such_i0i1(txt,txtquot0,"vs",iact,strlen(txt)-1);
                if( isuch < 0 ){
                    isuch_end = strlen(txt)-1;
                    istatus = 0;
                }else{
                    isuch_end = isuch-1;
                    istatus=1;
                }

                while( ( isuch=astr_such_i0i1(txt,txtsuch,"vs",iact,isuch_end) ) >= 0 )
                {
                    astr_cut(txt,(size_t)isuch,(size_t)isuch+strlen(txtsuch)-1);
                    isuch_end -= strlen(txtsuch);
                    if( *txtchange != '\0' ) {
                        astr_insert(txt,txtchange,(size_t)isuch,strlen(txtchange));
                        isuch_end += strlen(txtchange);
                    }
                    ncount++;
                }
                iact=isuch_end+strlen(txtquot0);
            }
        }
    }
    *pstatus = istatus;
    return ncount;
}  
/*==========================================================================*/
size_t astr_change_spez1(char *txt,const char *txtkenn,const char *txtsuch,
                         const char *txtchange,const char *regel)
{
  size_t i0=0;
  size_t i1,ik,is;
  size_t ncount=0;
  SINT16_T isuch;
  
  i1 = strlen(txt)-1;
  
  while( i0 <= i1 )
  {
     if( (isuch = astr_such_i0i1(txt,txtkenn,"vs",i0,i1)) < 0 )
       return ncount;
     ik = isuch;
       
     if( *regel == 'r' )
     {
       isuch = astr_such_i0i1(txt,txtsuch,"rn",i0,ik-1);
       if( isuch < 0 ) is=i0;
       else            is=isuch+1;
       
       if( astr_cut(txt,is,ik-1) >= 0 )
       {
         ncount++;
         if( *txtchange != '\0' )
           astr_insert(txt,txtchange,is,strlen(txtchange));
         ik = is + strlen(txtchange);           
       }
     }
     else
     {
       isuch = astr_such_i0i1(txt,txtsuch,"vn",ik+strlen(txtkenn),i1);
       if( isuch < 0 ) is=i1;
       else            is=isuch-1;
       
       if(    astr_cut(txt,ik+strlen(txtkenn),is) >= 0 )
       {
         ncount++;
         if( *txtchange != '\0' )
           astr_insert(txt,txtchange,ik+strlen(txtkenn),strlen(txtchange));
       }
     }
     i0 = ik+strlen(txtkenn);     
  }
  return ncount;
}
/*==========================================================================*/
IERR_T astr_copy(char *txt1, const char *txt2, size_t i0, size_t i1)
{
  size_t    i;

  if( i0 < 0              ) return NOT_OK;
  if( i1 > strlen(txt2)-1 ) i1 = strlen(txt2)-1;
  if( i1 < i0             ) return NOT_OK;

  for(i=0;i<=i1-i0;i++) 
      *(txt1+i) = *(txt2+i0+i);
  *(txt1+i1-i0+1)='\0';
  return OK;
}
/*==========================================================================*/
IERR_T astr_copy_cut(char *txt1, char *txt2, size_t i0, size_t i1) 
{
   IERR_T ierr;
   if( (ierr= astr_copy(txt1, txt2, i0, i1)) != OK)
     return ierr;
   astr_cut(txt2, i0, i1);
   return OK;
}
/*==========================================================================*/
IERR_T astr_copy_quot(char *txt1,const char *txt2,const char *txtquot0,const char *txtquot1)
{
 return astr_copy_nquot(txt1,txt2,txtquot0,txtquot1,1);
}
/*==========================================================================*/
IERR_T astr_copy_nquot(char *txt1,const char *txt2,const char *txtquot0,const char *txtquot1,
                     size_t nquot)
{
  size_t i0=0;
  size_t i,i1;
  SINT16_T isuch;
  
  i1 = strlen(txt2)-1;
  
  if( txtquot0 != NULL )
  {
    for(i=1;i<=nquot;i++)
      if( (isuch=astr_such_i0i1(txt2,txtquot0,"vs",i0,i1)) < 0 )
        return -1;
  }
  i0 = isuch;   
  if( txtquot1 != NULL )
  {
    if( (isuch=astr_such_i0i1(txt2,txtquot1,"vs",i0,i1)) < 0 )
      return -1;
    i1 = isuch-1;
  }

  if( txtquot0 != NULL )i0=i0+strlen(txtquot0);
      
  return astr_copy(txt1,txt2,i0,i1);
}
/*==========================================================================*/
void astr_upper(char *txt1)
{
  size_t    i,j;

  j=strlen(txt1);
  if(j == 0) j=1;

  for(i=0;i<=j-1;i++)
     *(txt1+i) = (char)toupper( (int)*(txt1+i));
}
/*==========================================================================*/
void astr_lower(char *txt1)
{
  size_t    i,j;

  j=strlen(txt1);
  if(j == 0) j=1;

  for(i=0;i<=j-1;i++)
     *(txt1+i) = (char)tolower( (int)*(txt1+i));
}
/*==========================================================================*/
size_t astr_tcount(const char *txt1,const char *txttrenn)
{
  size_t nmax,ntxttrenn,i,i0,i1;
  size_t icount = 0;

  ntxttrenn = strlen(txttrenn);
  nmax = astr_count(txt1,txttrenn,0,strlen(txt1)-1) + 1;

  for(i=1;i<=nmax;i++)
  {
    
    if( i == 1)
      i0 = 0;
    else {
      i0 = (size_t)MAX((SINT16_T)0,astr_nsuch(txt1,txttrenn,i-1))+ntxttrenn;
    }
    if( i == nmax )
      i1 = strlen(txt1)-(int)1;
    else
      i1 = (size_t)MAX((SINT16_T)1,astr_nsuch(txt1,txttrenn,i)) - 1;

    if( i0 <= i1 ) icount++;
  }
  return(icount);
}
/*==========================================================================*/
size_t astr_tcount_quot(const char *txt1,const char *txttrenn,const char *tquot0,const char *tquot1)
{

  size_t i0=0,iact=0,iend;
  SINT16_T i;
  size_t  ncount = 0;
  
  iend = strlen(txt1)- 1;
  while( iact <= iend )
  {

      /* Sucht erstes quot-Zeichen */  
      if( astr_such_i0i1(txt1,tquot0,"vs",iact,iend) == (SINT16_T)iact ) {
          
            /* Sucht zweites Quot-Zeichen */
            i=astr_such_i0i1(txt1,tquot1,"vs",iact+strlen(tquot0),iend);
            if( i < 0 ) /* Kein Ende gefunden, der Rest der zeile wird gequotet */
                iact = iend;
            else /* Setzt iact an das Ende des Quots */
                iact = i+strlen(tquot1);
      }
      /* Sucht nach dem Trennzeichen und zählt darüber */
      if( astr_such_i0i1(txt1,txttrenn,"vs",iact,iend) == (SINT16_T)iact ) {

        if(  (iact != 0) /* nicht am Anfang */
          && (iact != iend-strlen(txttrenn)+1) /* nicht am Ende */
          )
            ncount++;

        iact += strlen(txttrenn);
      } else {

          iact++;
      }
  }
  return ++ncount;
}
/*==========================================================================*/
size_t astr_tstrlen(const char *txt1,const char *txttrenn,size_t istelle)
{
  size_t ntxttrenn,nmax,i0,i1;
  
  ntxttrenn = strlen(txttrenn);
  nmax = astr_count(txt1,txttrenn,0,strlen(txt1)-1)+1;
  if(nmax == 1)
  {
    return strlen(txt1);
  }
  if(istelle > nmax-1)istelle=nmax-1;

  if( istelle == 0)
    i0 = 0;
  else
    i0 = (size_t)MAX((SINT16_T)0,astr_nsuch(txt1,txttrenn,istelle))+ntxttrenn;

  if( istelle == nmax-1 )
    i1 = strlen(txt1)-1;
  else
    i1 = astr_nsuch(txt1,txttrenn,istelle+1) - 1;

  return i1-i0+1;
}
/*==========================================================================*/
size_t astr_tstrlen_quot(const char *txt1,const char *txttrenn,size_t istelle,const char *tquot0,const char *tquot1)
{
  size_t iact,iend,itrenn,nzeichen;
  char count_flag;

  if( istelle >= astr_tcount_quot(txt1,txttrenn,tquot0,tquot1) ) 
      istelle = astr_tcount_quot(txt1,txttrenn,tquot0,tquot1)-1;
  
  iact = 0;
  iend = strlen(txt1)-1;
  nzeichen = 0;
  itrenn = 0;
  while( iact <= iend )
  {
        if( itrenn < istelle )
            count_flag = 0;
        else if( itrenn == istelle )
            count_flag = 1;
        else
            return nzeichen;

        if( astr_such_i0i1(txt1,tquot0,"vs",iact,iend) == (SINT16_T)iact ) {
            iact += strlen(tquot0);
            if(count_flag) nzeichen += strlen(tquot0);
            while( iact <= iend ) {
                if( astr_such_i0i1(txt1,tquot1,"vs",iact,iend) == (SINT16_T)iact ){
                    iact += strlen(tquot1);
                    if(count_flag) nzeichen += strlen(tquot1);
                    break;
                }
                iact++;
                if(count_flag) nzeichen++;
            }
            if( iact > iend ) return nzeichen;
            
        }
        if( astr_such_i0i1(txt1,txttrenn,"vs",iact,iend) == (SINT16_T)iact ) {
            itrenn++;
        } else {
            if(count_flag) nzeichen++;
        }

        iact++;
  }
  return nzeichen;
}
/*==========================================================================*/
size_t astr_tget(char *txt2,const char *txt1,const char *txttrenn,size_t istelle)
{
  size_t ntxttrenn,nmax,i0,i1;
  
  ntxttrenn = strlen(txttrenn);
  nmax      = astr_count(txt1,txttrenn,0,strlen(txt1)-1)+1;
  if(nmax == 1)
  {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(txt2, strlen(txt1) + 1, txt1);
#else
    strcpy(txt2, txt1);
#endif
   
    return 0; /* erste Stelle */
  }
  if(istelle > nmax-1)istelle=nmax-1;

  if( istelle == 0)
    i0 = 0;
  else
    i0 = (size_t)MAX((SINT16_T)0,astr_nsuch(txt1,txttrenn,istelle))+ntxttrenn;

  if( istelle == nmax-1 )
    i1 = strlen(txt1)-1;
  else
    i1 = astr_nsuch(txt1,txttrenn,istelle+1) - 1;

  if( i0 > i1 )
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(txt2, 1, "");
#else
    strcpy(txt2, "");
#endif
  else
      astr_copy(txt2,txt1,i0,i1);
  return istelle;
}
/*==========================================================================*/
size_t astr_tget_quot(char *txt2, const char *txt1,const char *txttrenn,size_t istelle,const char *tquot0,const char *tquot1)
{
  size_t iact,iend,itrenn,i0;
  char count_flag;

  if( istelle >= astr_tcount_quot(txt1,txttrenn,tquot0,tquot1) ) 
      istelle = astr_tcount_quot(txt1,txttrenn,tquot0,tquot1)-1;


  iact = 0;
  iend = strlen(txt1)-1;
  i0   = iend;
  itrenn = 0;
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(txt2, 1, "");
#else
  strcpy(txt2, "");
#endif
  while( iact <= iend )
  {
        if( itrenn < istelle )
            count_flag = 0;
        else if( itrenn == istelle )
            count_flag = 1;
        else
            return i0;

        if( astr_such_i0i1(txt1,tquot0,"vs",iact,iend) == (SINT16_T)iact ) {
            iact += strlen(tquot0);
            while( iact <= iend ) {
                if( astr_such_i0i1(txt1,tquot1,"vs",iact,iend) == (SINT16_T)iact ){
                    iact += strlen(tquot1);
                    break;
                }
                if(count_flag) {
                    astr_cat(txt2,txt1,iact,iact);
                    i0 = MIN(iact,i0);
                }
                iact++;
            }
            if( iact > iend ) return i0;
            
        }
        if( astr_such_i0i1(txt1,txttrenn,"vs",iact,iend) == (SINT16_T)iact ) {
            itrenn++;
        } else {
                if(count_flag) {
                    astr_cat(txt2,txt1,iact,iact);
                    i0 = MIN(iact,i0);
                }
        }

        iact++;
  }
  return i0;
}
/*==========================================================================*/
size_t astr_tget_bse(char *txt2,char *txt1,const char *txttrenn,size_t istelle,
                     UINT8_T bisstringende)
{
  size_t ntxttrenn,nmax,i0,i1;
  
  ntxttrenn = strlen(txttrenn);
  nmax = astr_count(txt1,txttrenn,0,strlen(txt1)-1)+1;
  if(nmax == 1)
  {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(txt2, strlen(txt1)+1, txt1);
#else
    strcpy(txt2, txt1);
#endif
    return 0; /* erste Stelle */
  }
  if(istelle > nmax-1)istelle=nmax-1;

  if( istelle == 0)
    i0 = 0;
  else
    i0 = (size_t)MAX((SINT16_T)0,astr_nsuch(txt1,txttrenn,istelle))+ntxttrenn;


  if( bisstringende == 1 || istelle == nmax-1 )
    i1 = strlen(txt1)-1; 
  else
    i1 = (size_t)MAX((SINT16_T)1,astr_nsuch(txt1,txttrenn,istelle+1)) - 1;

  astr_copy(txt2,txt1,i0,i1);
  return istelle;
}
/*==========================================================================*/
size_t astr_strlen_zeilen(const char *txt1,size_t istelle)
{
    return astr_tstrlen(txt1,"\n",istelle);
}
/*==========================================================================*/
size_t astr_get_zeilen(char *txt2,const char *txt1,size_t istelle)
{
    return astr_tget(txt2,txt1,"\n",istelle);
}
/*==========================================================================*/
size_t astr_tsuch(const char *txt1,const char *txt2,const char *txttrenn)
{
  size_t ntxttrenn,nmax,i,i0,i1;

  ntxttrenn = strlen(txttrenn);
  nmax = astr_count(txt1,txttrenn,0,strlen(txt1)-1);

  if( nmax == 0 )
  {
    if( strcmp(txt1,txt2) == 0)
      return 1;  /* erste Stelle */
    else
      return 0; /* nicht gefunden */
  }

  for( i=1;i<=nmax+1;i++)
  {
    if( i == 1 )
      i0 = 0;
    else
      i0 = astr_nsuch(txt1,txttrenn,i-1)+ntxttrenn;

    if( i == nmax+1 )
      i1 = strlen(txt1)-1;
    else
      i1 = astr_nsuch(txt1,txttrenn,i)-ntxttrenn;

    if( astr_cmp(txt2,txt1,i0,i1) == 0 )
      return i; /* i-te Stelle */
  }
  return 0;  /* nicht gefunden */
}
/*==========================================================================*/
size_t astr_fprintf_text(FILE *handle,char *text,size_t Spaltenbreite,char *wiebuendig)
{
  size_t Laengetext=strlen(text);
  size_t AnzahlLeerzeichenVor,AnzahlTextzeichen,AnzahlLeerzeichenNach;
  size_t i;

  AnzahlTextzeichen = MIN(Laengetext,Spaltenbreite);

  if( *wiebuendig == 'l' || *wiebuendig == 'L'   )
  {
	  AnzahlLeerzeichenVor  = 0;
	  AnzahlLeerzeichenNach = MAX(Spaltenbreite-AnzahlTextzeichen,0);
  }
  else
  {
	  AnzahlLeerzeichenVor  = MAX(Spaltenbreite-AnzahlTextzeichen,0);
 	  AnzahlLeerzeichenNach = 0;
  }

  for(i=0;i<=AnzahlLeerzeichenVor-1;i++)
	  fprintf(handle,"%1s"," ");
  for(i=0;i<=AnzahlTextzeichen-1;i++)
	  fprintf(handle,"%c",text[i]);
  for(i=0;i<=AnzahlLeerzeichenNach-1;i++)
	  fprintf(handle,"%1s"," ");

  astr_cut(text,  0, AnzahlTextzeichen-1);
  return strlen(text);
}
/*==========================================================================*/
void astr_message_init(void)
{
	astr_message_func(1,NULL,NULL);
	return;
}
/*==========================================================================*/
void astr_message_cat(char *txt)
{
	astr_message_func(2,txt,NULL);
	return;
}
/*======================================================================*/
void astr_message_cat_ival( int i) {
    char text[30];
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(text, strlen(text), "%i", i);
#else
    sprintf(text, "%i", i);
#endif
   
    astr_message_cat(text);
    return;
}
/*======================================================================*/
void astr_message_cat_fval( float f) {
    char text[30];
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(text, strlen(text), "%g", f);
#else
    sprintf(text, "%g", f);
#endif
    astr_message_cat(text);
    return ;
}
/*======================================================================*/
void astr_message_cat_dval( double d) {
    char text[30];
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(text, strlen(text), "%g", d);
#else
    sprintf(text, "%g", d);
#endif
    astr_message_cat(text);
    return ;
}
/*==========================================================================*/
void astr_message_cpy(char *txt)
{
	astr_message_init();
	astr_message_cat(txt);
	return;
}
/*======================================================================*/
void astr_message_cpy_ival( int i) {
    char text[30];
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(text, strlen(text), "%i", i);
#else
    sprintf(text, "%i", i);
#endif
    astr_message_cpy(text);
    return ;
}
/*======================================================================*/
void astr_message_cpy_fval( float f) {
    char text[30];
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(text, strlen(text), "%g", f);
#else
    sprintf(text, "%g", f);
#endif
    astr_message_cpy(text);
    return ;
}
/*======================================================================*/
void astr_message_cpy_dval( double d) {
    char text[30];
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(text, strlen(text), "%g", d);
#else
    sprintf(text, "%g", d);
#endif
    astr_message_cpy(text);
    return ;
}
/*==========================================================================*/
void astr_message_ins(char *txt)
{
	astr_message_func(3,txt,NULL);
	return;
}
/*==========================================================================*/
size_t astr_message_count(void)
{

	return astr_message_func(4,NULL,NULL);
}
/*==========================================================================*/
char *astr_message(void)
{
	char *txt=NULL;
	astr_message_func(5,NULL,&txt);
	return txt;
}
/*==========================================================================*/
void astr_message_free(void)
{

	astr_message_func(1,NULL,NULL);
	return ;
}
/*==========================================================================*/
size_t astr_message_func(UINT8_T server, char *txt, char **ppnewmess)
{
	static char   message[4096];
	static int    nmessage = 0;

	switch(server) {
	case 1:
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(message, 1, "");
#else
    strcpy(message, "");
#endif
		nmessage = 0;
		break;
	case 2:
		astr_cat(message,txt,0,MIN(4095-nmessage,(int)strlen(txt)-1));
		nmessage = strlen(message);
		break;
	case 3:
		astr_insert(message,txt,0,MIN(4095-nmessage,(int)strlen(txt)-1)+1);
		nmessage = strlen(message);
		break;
	case 4:
		return nmessage;
	case 5:
		*ppnewmess = &message[0];
		break;
	}
	return 0;  
}
/*==========================================================================*/
IERR_T astr_such_pfe(const char *pinput,const char *pregel) {
    
    char Trennzeichen[10];
    char *p1;
    int i0,i1;
    int found_flag = 0;
    int count = 0;
        
    /* Suche nach Trennzeichen */
    if( astr_such(pinput,"\\","vs") >= 0 ) {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
      strcpy_s(Trennzeichen, 3,"\\");
#else
      strcpy(Trennzeichen, "\\");
#endif
    } else {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
      strcpy_s(Trennzeichen, 2, "/");
#else
      strcpy(Trennzeichen, "/");
#endif
    }
    
    if( astr_such(pregel,"p","vs") >= 0 || astr_such(pregel,"P","vs") >= 0 ) {
        count++;
        i0=astr_such(pinput," ","vn");
        if( (i1=astr_such(pinput,Trennzeichen,"rs")) >= 0 ) {
            if( i1 > i0 )
                found_flag++;
        } else if( strcmp(pinput,".") == 0 ) {
            found_flag++;
        } else if( strcmp(pinput,"..") == 0 ) {
            found_flag++;
        }
        
    }
    if( astr_such(pregel,"f","vs") >= 0 || astr_such(pregel,"F","vs") >= 0 ) {
        
        count++;    
        if( (strcmp(pinput,".")!=0) && (strcmp(pinput,"..")!=0) ) {
            
            if( (i0=astr_such(pinput,Trennzeichen,"rs")) >= 0 )
                p1 = (char *)pinput+i0+1;
            else if( (i0=astr_such(pinput," ","vn"))>=0 )
                p1 = (char *)pinput+i0;
            else
                p1 = (char *)pinput;
            
            
            if( (i1=astr_such(p1,".","vs"))>=0 ) {
                if( i1 > 0 )
                    found_flag++;
            } else if( (i1=astr_such(p1," ","rn"))>=0 ) {
                found_flag++;
            }
        }
    }
    
    if( astr_such(pregel,"e","vs") >= 0 || astr_such(pregel,"E","vs") >= 0 ) {
        
        count++;
        
        if( (strcmp(pinput,".")!=0) && (strcmp(pinput,"..")!=0) ) {
            
            if( (i0=astr_such(pinput,Trennzeichen,"rs")) >= 0 )
                p1 = (char *)pinput+i0+1;
            else if( (i0=astr_such(pinput," ","vn"))>=0 )
                p1 = (char *)pinput+i0;
            else
                p1 = (char *)pinput;
            
            
            if( (i0=astr_such(p1,".","vs"))>=0 ) {
                p1 = p1+i0+1;
                if(strlen(p1)>0)
                    found_flag++;
            }
        }
    }
    
    if( count == found_flag ) return OK;
    else                      return -1;
}
/*==========================================================================*/
#if 0
IERR_T astr_search_path_file_old(char *pinput,char *ppath, char *pfile) {

  char Trennzeichen[10];
  size_t i0,i1;

  strcpy(ppath,"");
  strcpy(pfile,"");
  if( strlen(pinput) == 0 ) {
    return 1;
  }
  if( strcmp(pinput,".") == 0 ) {
    strcpy(ppath,"./");
    return 0;
  }
  if( strcmp(pinput,"..") == 0 ) {
    strcpy(ppath,"../");
    return 0;
  }
  /* Suche nach Trennzeichen */
  if( astr_such(pinput,"/","vs") >= 0 ) {
    strcpy(Trennzeichen,"/");
  } else if( astr_such(pinput,"\\","vs") >= 0 ) {
    strcpy(Trennzeichen,"\\");
  } else {
    strcpy(pfile,pinput);
    return 0;
  }
  /* Pfad raussuchen */
  i0 = (size_t)MAX((SINT16_T)0,astr_such(pinput," ","vn"));
  i1 = (size_t)MAX((SINT16_T)0,astr_such(pinput,Trennzeichen,"rs"));
  astr_copy(ppath,pinput,i0,i1);

  i0 = i1+1;
  i1 = (size_t)MAX((SINT16_T)0,astr_such(pinput," ","rn"));
  astr_copy(pfile,pinput,i0,i1);
  
  return 0;
 }
#endif
/*==========================================================================*/
IERR_T astr_extract_pfe(char *pinput,char *poutput, char *pregel) {

  char Trennzeichen[10];
  size_t i0,i1;
  SINT16_T i;

#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(poutput, 1, "");
#else
  strcpy(poutput, "");
#endif

  /* Suche nach Trennzeichen */
  if( astr_such(pinput,"/","vs") >= 0 ) {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(Trennzeichen, 2, "/");
#else
    strcpy(Trennzeichen, "/");
#endif
  } else {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(Trennzeichen, 3, "\\");
#else
    strcpy(Trennzeichen, "\\");
#endif
  }

  if( astr_such(pregel,"p","vs") >= 0 || astr_such(pregel,"P","vs") >= 0 ) {
    
    i0=(size_t)MAX((SINT16_T)0,astr_such(pinput," ","vn"));
    if( (i=astr_such(pinput,Trennzeichen,"rs")) >= 0 ) {
      astr_copy(poutput,pinput,i0,(size_t)i);
    } else if( strcmp(pinput,".") == 0 ) {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
      strcpy_s(poutput, 3, "./");
#else
      strcpy(poutput, "./");
#endif
      return OK;
    } else if( strcmp(pinput,"..") == 0 ) {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
      strcpy_s(poutput, 4, "../");
#else
      strcpy(poutput, "../");
#endif
      return OK;
    } else {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
      strcpy_s(poutput, 3, "./");
#else
      strcpy(poutput, "./");
#endif
    }

  }
  if( astr_such(pregel,"f","vs") >= 0 || astr_such(pregel,"F","vs") >= 0 ) {

    if( (strcmp(pinput,".")==0) || (strcmp(pinput,"..")==0) ) {
      return NOT_OK;
    }
    if( (i=astr_such(pinput,Trennzeichen,"rs")) >= 0 ) {
      i0=i+1;
      if( i0+1 >= strlen(pinput) )return NOT_OK;
    } else {
      i0=(size_t)MAX((SINT16_T)0,astr_such(pinput," ","vn"));
    }
    if( (i=astr_such(pinput,".","rs")) >= 0 ) {
      i1=i-1;
      if(i1<i0) return NOT_OK;
    } else {
      i1=(size_t)MAX((SINT16_T)0,astr_such(pinput," ","rn"));
    }
    astr_cat(poutput,pinput,i0,i1);
  }

  if( astr_such(pregel,"e","vs") >= 0 || astr_such(pregel,"E","vs") >= 0 ) {

    if( (i=astr_such(pinput,".","rs")) >= 0 ) {
      if( !(astr_such(pregel,"f","vs") >= 0 || astr_such(pregel,"F","vs") >= 0) ) i++;
      if( i+1 >= (SINT16_T)strlen(pinput) )return NOT_OK;
    } else {
      return NOT_OK;
    }
    i=astr_such(pinput," ","rn");
    if( i < (SINT16_T)i0 )return NOT_OK;
    astr_cat(poutput,pinput,i0,(size_t)i);
  }
  
  return OK;
 }
/*==========================================================================*/
UINT8_T astr_exist_extension(char *pinput) {

  SINT16_T i;
  i=astr_such(pinput,".","rs");
  if( (i >= 0) && (i < (SINT16_T)strlen(pinput)-1) )
    return TRUE;
  else
    return FALSE;
}
/*==========================================================================*/
SINT16_T astr_such_t(const char *txt,const char *auswahl){
 
    size_t i;
    char t[2] = "\0";

    if( strlen(auswahl) == 0 )
        return -1;
    
    for(i=0;i<strlen(auswahl);i++) {
        t[0] = auswahl[i];
        t[1] = '\0';
        if( astr_such(txt,t,"vs") >0 ) {
            return (SINT16_T)i;
        }
    }
    return -1;
}

/*==========================================================================*/
/* Sucht das Trennzeichen aus der Auswahl in txt und schreibt es in ptrennz */
/* Wenn keins gefunden, dann erstes aus auswahl                             */
/*==========================================================================*/
IERR_T astr_extract_t(char *ptrennz,const char *txt,const char *auswahl){
 
    UINT8_T i;
    int found = 0;
    char t[2] = "\0";

    if( strlen(auswahl) == 0 ) {
        return NOT_OK;
    }
    
    for(i=0;i<strlen(auswahl);i++) {
        t[0] = auswahl[i];
        t[1] = '\0';
        if( astr_such(txt,t,"vs") >0 ) {
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
          strcpy_s(ptrennz, strlen(t)+1,t);
#else
          strcpy(ptrennz, t);
#endif
            found = 1;
            break;
        }
    }
    if( found == 0 ) {
        t[0] = auswahl[0];
        t[1] = '\0';
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
        strcpy_s(ptrennz, strlen(t) + 1, t);
#else
        strcpy(ptrennz, t);
#endif
    }
    return OK;
}
/*=============================================================================*/
/* Sucht den Suchtext aus der Auswahl in txtlist und gibt Stelle (ab 0) zurück */
/* Wenn nicht übereinstimt -1                                                  */
/*=============================================================================*/
IERR_T astr_match(const char *txtsearch,const char *txtlist,const char *txttrenn,char exact_flag
                 ,char eindeutig_flag,size_t *imatch){
    
    astrs_s *ps_text;
    size_t i,n,nmax,count,index;
    int  n1;
    size_t *pnmax=NULL;
    
    if( strlen(txtsearch) == 0 ) return 1;
    if( strlen(txtlist)   == 0 ) return 1;
    
    n1 = astr_tcount(txtlist,txttrenn);
    if( n1 <= 0 ) 
        return 1;
    else
        n = n1;
    
    
    if( amem_mem_g((void **)&pnmax,n,sizeof(size_t)) )
        return 1;
    
    ps_text=astrs_new();
    
    nmax = 0;
    for(i=0;i<n;i++) {
        astrs_tget(ps_text,txtlist,txttrenn,i);
        if( strcmp(txtsearch,astrs_string(ps_text)) == 0 ) {    
            astrs_delete(ps_text);
            amem_free_g((void **)&pnmax);
            *imatch = i;
            return 0;
        } else {
            
            pnmax[i] = 0;
            while(  (txtsearch[pnmax[i]] != '\0')
                && (ps_text->pBuffer[pnmax[i]] != '\0')
                && (txtsearch[pnmax[i]] == ps_text->pBuffer[pnmax[i]])  
                ) {
                
                
                pnmax[i]++;
                if( pnmax[i] > nmax )
                    nmax = pnmax[i];
            }
        }
    }
    
    
    count = 0;
    for(i=0;i<n;i++) {
        if(  (nmax > 0)
            && (nmax == pnmax[i])
            ) {
            count++;
            index = i;
        }
    }
    
    astrs_delete(ps_text);
    amem_free_g((void **)&pnmax);
    /* Wenn exakt waere, dann schon bei strcmp() gefunden */
    if( exact_flag ) return -1;
    /* keine Uebereinstimmung */
    if( nmax == 0 ) return -1;
    if( eindeutig_flag ) {
        if( count > 1 ) 
            return -1;
        else {
            *imatch = index;
            return 0;
        }
    }
    *imatch = index;
    return 0;
    
}

/*==========================================================================*/
/* Prüft ob string nach maxlength-Zeichen einen Terminator '\0' hat         */
/*==========================================================================*/
UINT8_T astr_proof_string(char *ptxt,UINT16_T maxlength){


    UINT8_T   okay_flag = FALSE;
    UINT16_T  i;

    if( ptxt == NULL )
        return okay_flag;

    for(i=0;i<maxlength;i++) {
        if( ptxt[i] == '\0' ) {
            okay_flag = TRUE;
            break;
        }
    }
    return okay_flag;
}

