/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 2.2 19.11.01 TBert astrm_s und  alle Funktionen damit astrm_..() eingeführt                  */
/* 2.1 12.11.01 TBert astrs_tget, astrs_tget_quot astrs_change..., astrs_cut..., astrs_change...eingefhrt */
/* 2.0 13.08.01 TBert Release VehicleModell 2.0           */
/* Aenderungen 
001 07.03.01 TBert      Anlegen einer neuen Struktur
Ver Datum*** Wer        Was
*/
/*************************************************************************
* File:             astr_a.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
* Version:          1.0
* Datum:            6.03.01
*************************************************************************
* Kurzbeschreibung: 
*
* Stringbearbeitungsfunktionen: siehe astr_a.h
************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "definer.h"
#include "astr.h"
#include "amem.h"
#include "astrs.h"
#include "amsg.h"


#if !defined(TXTCOMMENT)
  #define TXTCOMMENT "#"
#endif

void            astrm_cpy_vpointer(astrm_s *pm,astrv_s *pv);
void            astrv_cpy_mpointer(astrv_s *pv,astrm_s *pm);

static int astrs_err_code=0;
/*=================================================================*/
/* Anlegen einer neuen Datenstruktur. */
/*=================================================================*/
astrs_s *astrs_new(void){

    astrs_s *p=NULL;

    /* Memory für die Struktur p */
    amem_mem_g((void **)&p,1,sizeof(astrs_s));
    if( p == NULL ) {
        amsg_set_status("\nastrs_new_err: no memory for astrs_s p");
        astrs_err_code = ASTRS_NO_MEMORY;
        return p;
    } else {
        p->memsize = 0;
        p->pBuffer = NULL;
    }

    /* characterbuffer von p initialisieren */
    if( astrs_mem(p,1) != 0 )
        return NULL;

    /* p initialisieren (kein Wert) */
    if( astrs_cpy(p,"") != 0 )
        return NULL;

    /* Initialisierter Popinter zurückgeben */
    return p;
}
/*=================================================================*/
/* Löschen einer Datenstruktur. */
/*=================================================================*/
int astrs_delete(astrs_s *p) {

    /* Characterspeicher von p freigeben */
    if( astrs_free(p) != 0 )
        return astrs_err_code;

    /* Memory für die Struktur p freigeben */
    if( amem_free_g((void **)&p) != 0) {
        amsg_set_status("\nastrs_delete_err: error in amem_free");
        astrs_err_code = ASTRS_NO_MEMORY;
        return astrs_err_code;
    }
    return 0;
}
/*=================================================================*/
/* Memory für pBuffer anlegen */
/*=================================================================*/
int astrs_mem(astrs_s *p,size_t n) {

    if( p == NULL ) {
        amsg_set_status("\nastrs_upper_err p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( amem_smem(&(p->pBuffer),&(p->memsize),n+1) != 0 ) {
        amsg_set_status("\nastrs_mem_err: no memory for char pBuffer");
        astrs_err_code = ASTRS_NO_MEMORY;
        return astrs_err_code;
    }
    return 0;
}
/*=================================================================*/
/* Memory für pBuffer freigeben */
/*=================================================================*/
int astrs_free(astrs_s *p) {

    if( amem_sfree(&(p->pBuffer),&(p->memsize)) != 0 ) {
        amsg_set_status("\nastrs_free_err: error in amem_sfree");
        astrs_err_code = ASTRS_FREE_ERR;
        return astrs_err_code;
    }
    if( p->pBuffer == NULL ) {
        /* characterbuffer von p initialisieren */
        if( astrs_mem(p,1) != 0 )
            return 1;

        /* p initialisieren (kein Wert) */
        if( astrs_cpy(p,"") != 0 )
            return 1;
    }
    return 0;
}
/*=================================================================*/
/* Error Code zurueckgeben */
/*=================================================================*/
int astrs_err(void) {
    return astrs_err_code;
}
/*=================================================================*/
/* Error Code zurueckgeben */
/*=================================================================*/
char* astrs_err_text(void){
    if( astrs_err_code != 0 )
        return amsg_get_msg();
    else
        return "";
}


/*=================================================================*/
/* vollstaendigen String kopieren  */
/*=================================================================*/
int astrs_cpy(astrs_s *p,const char *txtcpy) {
    
    if( p == NULL ) {
        amsg_set_status("\nastrs_cpy p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( astrs_mem(p,strlen(txtcpy)+1) != 0 )
        return astrs_err_code;
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(p->pBuffer, strlen(txtcpy)+1,txtcpy);
#else
    strcpy(p->pBuffer, txtcpy);
#endif
    
    return 0;
}
/*=========================================================================*/
/* String aus txt von zwischen txtquot0 und txtquot1 an der iten Stelle    */
/*=========================================================================*/
int astrs_cpy_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1) {
    return astrs_cpy_iquot(p,txt,txtquot0,txtquot1,0); 
}
int astrs_cpy_iquot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1, size_t i_such) {

  size_t i0=0;
  size_t i,i1;
  SINT16_T isuch;
  
  i1 = strlen(txt)-1;
  
  if( txtquot0 != NULL )
  {
    for(i=0;i<=i_such;i++)
      if( (isuch=astr_such_i0i1(txt,txtquot0,"vs",i0,i1)) < 0 )
        return -1;
  }
  i0 = isuch;   
  if( txtquot1 != NULL )
  {
    if( (isuch=astr_such_i0i1(txt,txtquot1,"vs",i0,i1)) < 0 )
      return -1;
    i1 = isuch-1;
  }

  if( txtquot0 != NULL )i0=i0+strlen(txtquot0);
      
  return astrs_cpyi(p,txt,i0,i1);
}
/*=================================================================*/
/* String aus txtcpy von Stelle i0 bis i1 in p-pBuffer kopieren    */
/*=================================================================*/
int astrs_cpyi( astrs_s *p, const char *txtcpy, size_t i0, size_t i1) {

    size_t j;

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( i1 > strlen(txtcpy)-1 ) {
        i1 = strlen(txtcpy)-1;
        amsg_set_status("\nastrs_icat_warning: position i1 too large, i1 is set to strlen(txtcat)-1\n");
        astrs_err_code = ASTRS_WRONG_OFFSET_WARNING;
    }
    if( i0 > i1 ) {
        i0 = i1;
        amsg_set_status("\nastrs_icat_warning: position i0 too large, i1 is set to i1\n");
        astrs_err_code = ASTRS_WRONG_OFFSET_WARNING;
    }
    if( astrs_mem(p,(i1-i0+1)+1) != 0 )
        return astrs_err_code;
    

    for(j=0;j<i1-i0+1;j++)
        p->pBuffer[j] = txtcpy[i0+j];
    
    p->pBuffer[(i1-i0+1)] = '\0';
        
    return 0;
}
/*=================================================================*/
/* vollstaendigen String kopieren  */
/*=================================================================*/
int astrs_scpy(astrs_s *p,const astrs_s *pcpy) {
    
    if( pcpy == NULL || pcpy->pBuffer == NULL) {
        amsg_set_status("\nastrs_scpy pcpy or pcpy->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    return astrs_cpy(p,pcpy->pBuffer);
}
/*=================================================================*/
/* String aus txtcpy von Stelle i0 bis i1 in p-pBuffer kopieren    */
/*=================================================================*/
int astrs_scpyi( astrs_s *p, const astrs_s *pcpy, size_t i0, size_t i1) {
        
    if( pcpy == NULL || pcpy->pBuffer == NULL) {
        amsg_set_status("\nastrs_scpyi pcpy or pcpy->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    return astrs_cpyi(p,pcpy->pBuffer,i0,i1);
}
/*=================================================================*/
/* Integer i in Text kopieren                                      */
/*=================================================================*/
int astrs_cpy_ival(astrs_s *p, const int ival) {


    int iival,n=1;
    char frmt[30];

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( ival >= 0 ) {
        iival = ival;
    } else {
        iival = -ival;
        n++;
    }
    while(iival > 9) {
        iival /= 10;
        n++;
    }
    if( astrs_mem(p,n+1) != 0 )
        return astrs_err_code;
    
    astr_insert(frmt,"%",0,1);
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(frmt,2, "i");
#else
    strcat(frmt,"i");
#endif

#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(p->pBuffer, 1,"");
#else
    strcpy(p->pBuffer,"");
#endif
#if (SYSTEM_COMP == VISUAL_C) && (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(p->pBuffer,strlen(p->pBuffer), frmt, ival);
#else
    sprintf(p->pBuffer,frmt,ival);
#endif
    return 0;
}
/*=================================================================*/
/* Long Integer l in Text kopieren                                      */
/*=================================================================*/
int astrs_cpy_lval(astrs_s *p, const signed long int lval) {


    signed long int llval;
    int             n=1;
    char frmt[30];

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( lval >= 0 ) {
        llval = lval;
    } else {
        llval = -lval;
        n++;
    }
    while(llval > 9) {
        llval /= 10;
        n++;
    }
    if( astrs_mem(p,n+1) != 0 )
        return astrs_err_code;
    
#if (SYSTEM_COMP == VISUAL_C) && (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(frmt, strlen(frmt), "%20i", n);
#else
    sprintf(frmt, "%20i", n);
#endif
    astr_insert(frmt,"%",0,1);
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(frmt, 2, "i");
#else
    strcat(frmt, "i");
#endif

#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(p->pBuffer, 1, "");
#else
    strcpy(p->pBuffer, "");
#endif
#if (SYSTEM_COMP == VISUAL_C) && (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(p->pBuffer,strlen(p->pBuffer), frmt, lval);
#else
    sprintf(p->pBuffer,frmt,lval);
#endif
    return 0;
}
/*=================================================================*/
/* Unsigned Long Integer l in Text kopieren                                      */
/*=================================================================*/
int astrs_cpy_ulval(astrs_s *p, const unsigned long int ulval) {


    unsigned long int ullval;
    int             n=1;
    char frmt[30];

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    ullval = ulval;
    while(ullval > 9) {
        ullval /= 10;
        n++;
    }
    if( astrs_mem(p,n+1) != 0 )
        return astrs_err_code;
    
#if (SYSTEM_COMP == VISUAL_C) && (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(frmt,strlen(frmt),"%20i",n);
#else
    sprintf(frmt, "%20i", n);
#endif
    astr_insert(frmt,"%",0,1);
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(frmt,2,"i");
#else
    strcat(frmt, "i");
#endif
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcpy_s(p->pBuffer,1,"");
#else
    strcpy(p->pBuffer, "");
#endif
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(p->pBuffer,strlen(p->pBuffer),frmt,ulval);
#else
    sprintf(p->pBuffer, frmt, ulval);
#endif
    return 0;
}
/*=================================================================*/
/* Double d in Text kopieren                                      */
/*=================================================================*/
int astrs_cpy_dval(astrs_s *p, const double dval) {

    char t[50];

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(t,strlen(t),"%g",dval);
#else
    sprintf(t, "%g", dval);
#endif
    if( astrs_cpy(p,t) != 0 )
        return astrs_err_code;
    
    return 0;
}
/*=================================================================*/
/* vollstaendigen String anhängen  */
/*=================================================================*/
int astrs_cat(astrs_s *p,const char *txtcat) {
    
    if( p == NULL ) {
        amsg_set_status("\nastrs_cat p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( astrs_mem(p,strlen(txtcat)+strlen(p->pBuffer)+1) != 0 )
        return astrs_err_code;
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(p->pBuffer,strlen(txtcat)+1,txtcat);
#else
    strcat(p->pBuffer, txtcat);
#endif
    return 0;
}
/*=================================================================*/
/* String aus txtcat von Stelle i0 bis i1 an p-pBuffer anhängen    */
/*=================================================================*/
int astrs_cati( astrs_s *p, const char *txtcat, size_t i0, size_t i1) {

    size_t j0,j;

    if( p == NULL ) {
        amsg_set_status("\nastrs_cati p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( i1 > strlen(txtcat)-1 ) {
        i1 = strlen(txtcat)-1;
        amsg_set_status("\nastrs_icat_warning: position i1 too large, i1 is set to strlen(txtcat)-1\n");
        astrs_err_code = ASTRS_WRONG_OFFSET_WARNING;
    }
    if( i0 > i1 ) {
        i0 = i1;
        amsg_set_status("\nastrs_icat_warning: position i0 too large, i1 is set to i1\n");
        astrs_err_code = ASTRS_WRONG_OFFSET_WARNING;
    }
    if( astrs_mem(p,strlen(p->pBuffer)+(i1-i0+1)+1) != 0 )
        return astrs_err_code;
    
    j0 = strlen(p->pBuffer);

    for(j=0;j<i1-i0+1;j++)
        p->pBuffer[j0+j] = txtcat[i0+j];
    
    p->pBuffer[j0+(i1-i0+1)] = '\0';
        
    return 0;
}
/*=================================================================*/
/* vollstaendigen String anhängen  */
/*=================================================================*/
int astrs_scat(astrs_s *p,const astrs_s *pcat) {

    if( pcat == NULL || pcat->pBuffer == NULL) {
        amsg_set_status("\nastrs_scat pcat or pcat->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    return astrs_cat(p,pcat->pBuffer);
}
/*=================================================================*/
/* String aus txtcat von Stelle i0 bis i1 an p-pBuffer anhängen    */
/*=================================================================*/
int astrs_scati( astrs_s *p, const astrs_s *pcat, size_t i0, size_t i1) {

    if( pcat == NULL || pcat->pBuffer == NULL) {
        amsg_set_status("\nastrs_scat pcat or pcat->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    return astrs_cati(p,pcat->pBuffer,i0,i1);
}
/*=================================================================*/
/* Integer i an Text anhängen                                      */
/*=================================================================*/
int astrs_cat_ival(astrs_s *p, const int ival) {


    int iival,n=1;
    char frmt[30];
    astrs_s *p1=astrs_new();

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( ival >= 0 ) {
        iival = ival;
    } else {
        iival = -ival;
        n++;
    }
    while(iival > 9) {
        iival /= 10;
        n++;
    }
    if( astrs_mem(p1,n+1) != 0 )
        return astrs_err_code;
    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(frmt,strlen(frmt),"%20i",n);
#else
    sprintf(frmt, "%20i", n);
#endif
    astr_insert(frmt,"%",0,1);
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(frmt,2,"i");
#else
    strcat(frmt, "i");
#endif

#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(p1->pBuffer,strlen(p1->pBuffer),frmt,ival);
#else
    sprintf(p1->pBuffer, frmt, ival);
#endif

    if( astrs_scat(p,p1) != 0 )
        return astrs_err_code;
    
    astrs_delete(p1);
    

    return 0;
}
/*=================================================================*/
/* Long Integer l an Text anhängen                                      */
/*=================================================================*/
int astrs_cat_lval(astrs_s *p, const signed long int lval) {


    signed long int llval;
    int n=1;
    char frmt[30];
    astrs_s *p1=astrs_new();

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( lval >= 0 ) {
        llval = lval;
    } else {
        llval = -lval;
        n++;
    }
    while(llval > 9) {
        llval /= 10;
        n++;
    }
    if( astrs_mem(p1,n+1) != 0 )
        return astrs_err_code;
    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(frmt,strlen(frmt),"%20i",n);
#else
    sprintf(frmt, "%20i", n);
#endif
    astr_insert(frmt,"%",0,1);
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(frmt,2,"i");
#else
    strcat(frmt, "i");
#endif

#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(p1->pBuffer,strlen(p1->pBuffer),frmt,lval);
#else
    sprintf(p1->pBuffer, frmt, lval);
#endif

    if( astrs_scat(p,p1) != 0 )
        return astrs_err_code;
    
    astrs_delete(p1);
    

    return 0;
}
/*=================================================================*/
/* Unsigned Long Integer l an Text anhängen                                      */
/*=================================================================*/
int astrs_cat_ulval(astrs_s *p, const unsigned long int ulval) {


    unsigned long int ullval;
    int n=1;
    char frmt[30];
    astrs_s *p1=astrs_new();

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    ullval = ulval;
    while(ullval > 9) {
        ullval /= 10;
        n++;
    }
    if( astrs_mem(p1,n+1) != 0 )
        return astrs_err_code;
    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(frmt,strlen(frmt),"%20i",n);
#else
    sprintf(frmt, "%20i", n);
#endif
    astr_insert(frmt,"%",0,1);
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    strcat_s(frmt,2,"i");
#else
    strcat(frmt, "i");
#endif

#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(p1->pBuffer,strlen(p1->pBuffer),frmt,ulval);
#else
    sprintf(p1->pBuffer, frmt, ulval);
#endif
    if( astrs_scat(p,p1) != 0 )
        return astrs_err_code;
    
    astrs_delete(p1);
    

    return 0;
}
/*=================================================================*/
/* Double d an Text anhängen                                       */
/*=================================================================*/
int astrs_cat_dval(astrs_s *p, const double dval) {

    char t[50];

    if( p == NULL ) {
        amsg_set_status("\nastrs_cpyi p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    sprintf_s(t,strlen(t),"%g",dval);
#else
    sprintf(t, "%g", dval);
#endif
    if( astrs_cat(p,t) != 0 )
        return astrs_err_code;
    
    return 0;
}
/*========================================================================*/
/* vollständige Dateiangebae aus Pfad, File und Extension zusammensetzen  */
/* regel = "" oder "pfe"    => path, file und ext benutzen */
/*       = "f"              => file benutzen, wenn kein path und/oder extension da sind,
                               dann path und/oder extension anhängen */
/*       = "pf"             => path und file benutzen, wenn kein extension in file da,
                               dann extension anhaengen */
/*========================================================================*/
int astrs_cat_pfe(astrs_s *p,const char *path,const char *file,const char *ext,const char *regel) {

    int path_force_flag = 0;
    int file_force_flag = 0;
    int ext_force_flag  = 0;
    size_t  l;
    char *pf;

    astrs_s *pdum=astrs_new();
    astrs_s *ppath;
    astrs_s *pfile;
    astrs_s *pext,*pextsearch;

    if( p == NULL ) {
        amsg_set_status("\nastrs_cat_pfe p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }

    /* Regel aufbereiten */
    if( astrs_cpy(pdum,regel) != 0 ) {
        astrs_delete(pdum);
        return astrs_err_code;
    }

    if( astrs_lower(pdum) != 0 ) {
        astrs_delete(pdum);
        return astrs_err_code;
    }

    /* Flags setzen */
    if( strcmp(pdum->pBuffer,"") == 0 ) {
        path_force_flag = 1;
        file_force_flag = 1;
        ext_force_flag  = 1;
    } else {
        if( astr_such(pdum->pBuffer,"p","vs") > 0 )
            path_force_flag = 1;
        if( astr_such(pdum->pBuffer,"f","vs") > 0 )
            file_force_flag = 1;
        if( astr_such(pdum->pBuffer,"e","vs") > 0 )
            ext_force_flag = 1;
    }

    /* Trennzeichen finden */
    if( astr_such_t(path,"/\\") >= 0 ) {
        if( astrs_extract_t(pdum,path,"/\\") )return astrs_err();

    } else if( astr_such_t(file,"/\\") >= 0 ) {
        if( astrs_extract_t(pdum,file,"/\\") )return astrs_err();
    } else {
        if( astrs_cpy(pdum,"/") )return astrs_err();
    }


    /* Regel prüfen */

    if( file_force_flag == 0 ) {
        amsg_set_status("\nastrs_cat_pfe_err Falsche Regel");
        astrs_delete(pdum);
        astrs_err_code = ASTRS_WRONG_RULE;
        return astrs_err_code;
    }


    /* Filename finden */
    pfile = astrs_new();
    if( file_force_flag ) {

        /* Nachschauen, ob filename existiert */
        if( astr_such_pfe(file,"f") < 0) {
            amsg_set_status("\nastrs_cat_pfe_err Es ist kein Filename im Parameter zu erkennen file: <");
            pf = (char *)file;
            amsg_set_status(pf);amsg_set_status(">\n");
            astrs_delete(pdum);
            astrs_delete(pfile);
            astrs_err_code = ASTRS_WRONG_PARAMETER;
            return astrs_err_code;
        }
        /* Filename extrahieren */
        if( astrs_extract_pfe(pfile,file,"f") != 0 ) {
            astrs_delete(pfile);
            astrs_delete(pdum);
            return astrs_err();
        }
    }

    /* Pfadname finden */
    ppath = astrs_new();
    if( path_force_flag ) {

        /* Zuerst nachschauen, ob Pfad in path existiert */
        if( astr_such_pfe(path,"p") >= 0) {
            if( astrs_extract_pfe(ppath,path,"p") != 0 ) {
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann nachschauen, ob Pfad in file existiert */
        } else if( astr_such_pfe(file,"p") >= 0 ) {
            if( astrs_extract_pfe(ppath,file,"p") != 0 ) {
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann default setzen  ./ oder .\ */
        } else {
            if( (astrs_cpy(ppath,".") != 0) ||
                (astrs_scat(ppath,pdum) != 0) ){
                astrs_delete(pdum);
                astrs_delete(pfile);
                astrs_delete(ppath);
                return astrs_err();
            }
        }
    } else {

        /* Zuerst nachschauen, ob Pfad in file existiert */
        if( astr_such_pfe(file,"p") >= 0) {
            if( astrs_extract_pfe(ppath,file,"p") != 0 ) {
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann nachschauen, ob Pfad in path existiert */
        } else if( astr_such_pfe(path,"p") >= 0 ) {
            if( astrs_extract_pfe(ppath,path,"p") != 0 ) {
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann default setzen ./ oder .\ */
        } else {
            if( (astrs_cpy(ppath,".") != 0) ||
                (astrs_scat(ppath,pdum) != 0) ){
                astrs_delete(pdum);
                astrs_delete(pfile);
                astrs_delete(ppath);
                return astrs_err();
            }
        }
    }
    /* Trennzeichen anhängen */
    if( (l=strlen(ppath->pBuffer)) > 0 ) {

        if( ppath->pBuffer[l-1] != pdum->pBuffer[0] )
            astrs_scat(ppath,pdum);
    }

    /* Extensionname finden */
    pext = astrs_new();
    pextsearch = astrs_new();
    if( astr_such(ext,".","vs") < 0 ) {
        if( astrs_cpy(pextsearch,".") != 0 ) {
            astrs_delete(pextsearch);
            astrs_delete(pext);
            astrs_delete(ppath);
            astrs_delete(pfile);
            astrs_delete(pdum);
            return astrs_err();
        }
    }
    if( astrs_cat(pextsearch,ext) != 0 ) {
            astrs_delete(pextsearch);
            astrs_delete(pext);
            astrs_delete(ppath);
            astrs_delete(pfile);
            astrs_delete(pdum);
            return astrs_err();
    }

    if( ext_force_flag ) {

        /* Zuerst nachschauen, ob Pfad in path existiert */
        if( astr_such_pfe(pextsearch->pBuffer,"e") >= 0) {
            if( astrs_extract_pfe(pext,pextsearch->pBuffer,"e") != 0 ) {
                astrs_delete(pextsearch);
                astrs_delete(pext);
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann nachschauen, ob Pfad in file existiert */
        } else if( astr_such_pfe(file,"e") >= 0 ) {
            if( astrs_extract_pfe(pext,file,"e") != 0 ) {
                astrs_delete(pextsearch);
                astrs_delete(pext);
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann default setzen */
        } else {
            if( (astrs_cpy(pext,"") != 0) ){
                astrs_delete(pextsearch);
                astrs_delete(pdum);
                astrs_delete(pext);
                astrs_delete(pfile);
                astrs_delete(ppath);
                return astrs_err();
            }
        }
    } else {

        /* Zuerst nachschauen, ob Extension in file existiert */
        if( astr_such_pfe(file,"e") >= 0) {
            if( astrs_extract_pfe(pext,file,"e") != 0 ) {
                astrs_delete(pextsearch);
                astrs_delete(pext);
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann nachschauen, ob Extension in ext existiert */
        } else if( astr_such_pfe(pextsearch->pBuffer,"e") >= 0 ) {
            if( astrs_extract_pfe(pext,pextsearch->pBuffer,"e") != 0 ) {
                astrs_delete(pextsearch);
                astrs_delete(pext);
                astrs_delete(ppath);
                astrs_delete(pfile);
                astrs_delete(pdum);
                return astrs_err();
            }
        /* dann default setzen */
        } else {
            if( (astrs_cpy(pext,"") != 0) ){
                astrs_delete(pdum);
                astrs_delete(pextsearch);
                astrs_delete(pext);
                astrs_delete(pfile);
                astrs_delete(ppath);
                return astrs_err();
            }
        }
    }
    /* Punkt einfügen, wenn extension vorhanden */
    if( (strlen(pext->pBuffer)>0) && (astr_such(pext->pBuffer,".","vs")<0) ) {
        if( astrs_insert(pext,".",0,1) != 0 ) {
            astrs_delete(pdum);
            astrs_delete(pextsearch);
            astrs_delete(pext);
            astrs_delete(pfile);
            astrs_delete(ppath);
            return astrs_err();
        }
    }

    /* Vollständigen Namen zusammensetzen */
    /* Pfad */
    if( astrs_cpy(p,ppath->pBuffer) != 0 ) {
        astrs_delete(pdum);
        astrs_delete(pextsearch);
        astrs_delete(pext);
        astrs_delete(pfile);
        astrs_delete(ppath);
        return astrs_err();
    }
    /* File */
    if( astrs_cat(p,pfile->pBuffer) != 0 ) {
        astrs_delete(pdum);
        astrs_delete(pextsearch);
        astrs_delete(pext);
        astrs_delete(pfile);
        astrs_delete(ppath);
        return astrs_err();
    }
    /* Extension */
    if( astrs_cat(p,pext->pBuffer) != 0 ) {
        astrs_delete(pdum);
        astrs_delete(pextsearch);
        astrs_delete(pext);
        astrs_delete(pfile);
        astrs_delete(ppath);
        return astrs_err();
    }
    /* Ende gut alles gut */
    astrs_delete(pdum);
    astrs_delete(pextsearch);
    astrs_delete(pext);
    astrs_delete(pfile);
    astrs_delete(ppath);
    return 0;
}
/*==========================================================================*/
/* Verschieben von astrs_s Variable                                     */
/*==========================================================================*/
int astrs_smove(astrs_s *p,astrs_s *pmove) {
    if( p == pmove ) return 0;
    if( astrs_scpy(p,pmove) != 0 )return astrs_err();
    if( astrs_cpy(pmove,"") != 0 ) return astrs_err();
    return 0;
}
/*==========================================================================*/
/* Einfügen von String txtins in die Stelle i0 von p->pBuffer n Zeichen    */
/*==========================================================================*/
int astrs_insert_full(astrs_s *p, const char *txtinsert, size_t i0)
{
    return astrs_insert(p,txtinsert,i0,strlen(txtinsert));
}
int astrs_insert(astrs_s *p, const char *txtinsert, size_t i0, size_t n)
{
    size_t    i;

    if( p == NULL ) {
        amsg_set_status("\nastrs_insert p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( n > strlen(txtinsert) ) {
        n = strlen(txtinsert);
        amsg_set_status("\nastrs_insert_warning: length n too large, n is set to strlen(txtinsert)\n");
        astrs_err_code = ASTRS_WRONG_LENGTH_WARNING;
    }

    if( i0 > strlen(p->pBuffer) ) {
        i0 = strlen(p->pBuffer);
        amsg_set_status("\nastrs_insert_warning: offset i0 too large, io is set to strlen(p->pBuffer)\n");
        astrs_err_code = ASTRS_WRONG_OFFSET_WARNING;
    }

    if( astrs_mem(p,n+strlen(p->pBuffer)+1) != 0 )
        return astrs_err_code;

    i=strlen(p->pBuffer)+1;
    do {
        i--;
        p->pBuffer[i+n] = p->pBuffer[i];
    } while( i != i0 );

    for(i=i0;i<i0+n;i++)
        p->pBuffer[i] = txtinsert[i-i0];

    return 0;
}
/*==========================================================================*/
/* Einfügen von String aus pinsert->pBuffer in die Stelle i0 von p->pBuffer */
/* n Zeichen    */
/*==========================================================================*/
int astrs_sinsert_full(astrs_s *p, const astrs_s *pinsert, size_t i0)
{
    return astrs_sinsert(p,pinsert,i0,astrs_len(pinsert));
}
int astrs_sinsert(astrs_s *p, const astrs_s *pinsert, size_t i0, size_t n)
{
    if( pinsert == NULL || pinsert->pBuffer == NULL) {
        amsg_set_status("\nastrs_sinsert pinsert or pinsert->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    return astrs_insert(p,pinsert->pBuffer,i0,n);
}
/*==========================================================================*/
/* Schneidet aus p->pBuffer raus, von Stelle i0 bis i1                      */
/*==========================================================================*/
int astrs_cut(astrs_s *p, size_t i0, size_t i1)
{
  size_t    i,n;

    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( i0 < 0 ) {
        i0 = 0;
        amsg_set_status("\nastrs_cut_warning: offset i0 is negative, sst to zero\n");
        astrs_err_code = ASTRS_WRONG_OFFSET_WARNING;
    }
    if( i0 > i1 ) {
#if 0
        /* kein Fehler */
        amsg_set_status("\nastrs_cut_error: offset i0 > i1");
        astrs_err_code = ASTRS_WRONG_OFFSET;
        return astrs_err_code;
#endif
        return 0;

    }
    if( i1 > strlen(p->pBuffer)-1 ) {
        i1 = strlen(p->pBuffer)-1;
        amsg_set_status("\nastrs_cut_warning: offset i1 too large, i1 is set to strlen(p->pBuffer)-1\n");
        astrs_err_code = ASTRS_WRONG_LENGTH_WARNING;
    }

    n = strlen(p->pBuffer);
    for(i=0;i<=n-i1-1;i++)
        *(p->pBuffer+i0+i) = *(p->pBuffer+i1+1+i);

    if( astrs_mem(p,strlen(p->pBuffer)+1) != 0 )
        return astrs_err_code;

    return 0;
}
/*==========================================================================*/
/* Schneidet aus p->pBuffer Anfang und Ende mit txtcut raus                 */
/*==========================================================================*/
int astrs_cut_ae(astrs_s *p,const char *txtcut)
{
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
  if( astrs_cut_a(p,txtcut) != 0 )
      return astrs_err_code;
  if( astrs_cut_e(p,txtcut) != 0 )
      return astrs_err_code;
  return 0;
}
/*==========================================================================*/
/* Schneidet aus p->pBuffer Anfang mit txtcut raus                          */
/*==========================================================================*/
int astrs_cut_a(astrs_s *p,const char *txtcut)
{
    int i;
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( (i=astr_such(p->pBuffer,txtcut,"vn")) > 0 ) { 
        if( astrs_cut(p,0,i-1) != 0 )
            return astrs_err_code;
    } else if( i < 0 ) {
        if( astrs_cut(p,0,astrs_len(p)-1) != 0 )
            return astrs_err_code;
    }
    return 0;
}
/*==========================================================================*/
/* Schneidet aus p->pBuffer Ende mit txtcut raus                            */
/*==========================================================================*/
int astrs_cut_e(astrs_s *p,const char *txtcut)
{
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    
    if( astrs_cut(p,astr_such(p->pBuffer,txtcut,"rn")+1,strlen(p->pBuffer)-1) != 0 )
        return astrs_err_code;
    return 0;
}
/*==========================================================================*/
/* Schneidet aus p->pBuffer Comment raus                                    */
/*==========================================================================*/
int astrs_cut_comment(astrs_s *p,const char *txtcomment)
{
  int i;
  
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
  if( (i=astr_such(p->pBuffer,txtcomment,"vs")) >= 0 )
  {
    if( astrs_cut(p,i,strlen(p->pBuffer)-1) != 0 )
       return astrs_err_code;
  }
  
  return 0;
}
/*==========================================================================*/
/* Schneidet aus p->pBuffer Comment aus nicht gequoteten textraus           */
/*==========================================================================*/
int astrs_cut_comment_unquoted(astrs_s *p,const char *txtcomment,const char *txtquot)
{
    int i;
    char *ptext = p->pBuffer;
  
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }

    if( (i=astr_such_quot(ptext,txtcomment,"vs",txtquot,txtquot,"a",0)) >= 0 ) {

        if( astrs_cut(p,i,strlen(p->pBuffer)-1) != 0 )
           return astrs_err_code;
    }
  
    return 0;
}
/*==========================================================================*/
/* Gibt pointer auf p->pBuffer zurück                                       */
/*==========================================================================*/
char *astrs_string(astrs_s *p){
    if( p != NULL ) 
        return p->pBuffer;
    else
        return NULL;
}
/*==========================================================================*/
/* Gibt Länge von p->pBuffer zurück                                         */
/*==========================================================================*/
size_t astrs_memsize(astrs_s *p){
    if( p != NULL ) 
        return p->memsize;
    else
        return 0;
}
/*==========================================================================*/
/* Gibt Länge von p->pBuffer zurück                                         */
/*==========================================================================*/
size_t astrs_len(const astrs_s *p){
    if( p != NULL ) 
        return strlen(p->pBuffer);
    else
        return 0;
}
/*==========================================================================*/
/* Sucht in p->pBuffer die Zeichen txtsuch und ersetzt es mit txtchange     */
/* return ncount   Anzahl der changes                                       */
/*==========================================================================*/
int astrs_change(astrs_s *p,const char *txtsuch,const char *txtchange)
{
  int i=0;
  int ncount=0;
  
  if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
  }
  if( strcmp(txtsuch,txtchange) != 0 && *txtchange != '\0' && *txtsuch != '\0' ) {
  while( (i=astr_such_i0i1(p->pBuffer,txtsuch,"vs",i,strlen(p->pBuffer)-1)) >= 0 )
  {
    if( astrs_cut(p,i,i+strlen(txtsuch)-1) != 0 )
        return astrs_err_code;
    if( *txtchange != '\0' ) {
      if( astrs_insert(p,txtchange,i,strlen(txtchange)) != 0 )
        return astrs_err_code;
    }
    i = i+1-strlen(txtsuch)+strlen(txtchange);
    ncount++;
  }
  }  
  return ncount;
}
/*=============================================================================*/
/*    Sucht in p-> pBuffer die durch txtquot0 und txtquot1 eingeschlossenen    */
/*    String nach txtsuch und ersetzt durch txtchange (wenn txtchange = "\0",  */
/*    dann wird txtsuch rausgeschnitten)                                       */
/*                                                                             */
/*    regel = "i"   Es wird der string innerhalb des Quots behandelt           */
/*            "a"   Es wird der string ausserhalb des Quots beh.               */
/*                                                                             */
/*    istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots         */
/*              1   "                             innerhalb     "              */
/*=============================================================================*/
int astrs_change_quot(astrs_s *p,const char *txtsuch,const char *txtchange,
                      const char *txtquot0, const char *txtquot1,const char *regel,
                      int istatus1) {
    int i0=(int)0;
    int iact = (int)0;
    int istat;
    int i1=(int)0,iend;
    int ncount = (int)0;
    int icount = 0;
    char found_flag;
    
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    
    /* Wenn such und erstzen identisch sind */
    if( strcmp(txtsuch,txtchange) == 0 )
        return ncount;
    
    /* Suchen in Erszen enthalten ist, ergibt endlosschleife */
    if( astr_such(txtchange,txtsuch,"vs") >= 0 ) {
        amsg_set_status("txtsuch in txtchange enthalten");
        astrs_err_code = ASTRS_TXTSUCH_IN_TXTCHANGE;
        return astrs_err_code;
    }
    
    
    do {
        
        iend = MAX((int)strlen(p->pBuffer)-1,0);
        istat = istatus1;
        iact = 0;
        found_flag = 0;
        
        if( icount > 100 )
            return ncount;
        else
            icount++;
        
        if( *regel == 'a' ) { /* ausserhalb des Quots */
            
            while( iact <= iend ) {
                
                if( istat == 1 ) { /* innerhalb des Quots, suche quotende  */
                    
                    /* Quotende suchen */
                    i1 = MAX(iact+(int)strlen(txtquot1)-1,iact);
                    if( astr_such_i0i1(p->pBuffer,txtquot1,"vs",iact,i1) >= 0 ) {
                        istat = 0;
                        iact = i1;
                    }
                } else {
                    /* Quotanfang suchen */
                    i1 = MAX(iact+(int)strlen(txtquot0)-1,iact);
                    if( astr_such_i0i1(p->pBuffer,txtquot0,"vs",iact,i1) >= 0 ) {
                        istat = 1;
                        iact = i1;
                    } else { /* txtsuch suchen */
                        
                        i1 = MAX(iact+(int)strlen(txtsuch)-1,iact);
                        if( astr_such_i0i1(p->pBuffer,txtsuch,"vs",iact,i1)  >= 0 ) {
                            
                            /* txtsuch rausschneiden */
                            astrs_cut(p,iact,i1);
                            /* txtchange einsetzen */
                            if( *txtchange != '\0' ) {
                                if( astrs_insert(p,txtchange,iact,strlen(txtchange)) != 0 )
                                    return astrs_err_code;
                            }
                            iact = iact + strlen(txtchange) - 1;
                            iend = MAX((int)strlen(p->pBuffer)-1,0);
                            ncount++;
                            found_flag = 1;
                        }
                    }
                }
                iact++;
                
            }
            
        } else {  /* innerhalb des Quots tauschen */
            
            while( iact <= iend )
            {
                if( istat == (int)0 ) /* ausserhalb des Quots, suche quotanfang  */
                {
                    /* Quotanfang suchen */
                    i1 = MAX(iact+(int)strlen(txtquot0)-1,iact);
                    if( astr_such_i0i1(p->pBuffer,txtquot0,"vs",iact,i1) >= 0 ) {
                        istat = 1;
                        iact = i1;
                    }
                } else {
                    /* Quotende suchen */
                    i1 = MAX(iact+(int)strlen(txtquot1)-1,iact);
                    if( astr_such_i0i1(p->pBuffer,txtquot1,"vs",iact,i1) >= 0 ) {
                        istat = 0;
                        iact  = i1;
                    } else { /* txtsuch suchen */
                        
                        i1 = MAX(iact+(int)strlen(txtsuch)-1,iact);
                        if( astr_such_i0i1(p->pBuffer,txtsuch,"vs",iact,i1)  >= 0 ) {
                            
                            /* txtsuch rausschneiden */
                            astrs_cut(p,iact,i1);
                            /* txtchange einsetzen */
                            if( *txtchange != '\0' ) {
                                if( astrs_insert(p,txtchange,iact,strlen(txtchange)) != 0 )
                                    return astrs_err_code;
                            }
                            iact = iact + strlen(txtchange) - 1;
                            iend = MAX((int)strlen(p->pBuffer)-1,0);
                            ncount++;
                            found_flag = 1;
                        }
                    }
                }
                iact++;
                
            }
        }    
    } while( !found_flag );
    
    return ncount;    
}
 
#if 0
  
  {
    while( iact < iend )
    {
      if( istatus == (int)0 )
      {
        if( (i0 = astr_such_i0i1(p->pBuffer,txtquot0,"vs",iact,iend)) < (int)0 )
           return ncount;
        i0 = i0+(int)strlen(txtquot0);
      }
      else
        istatus = (int)0;
      
      if( (i1 = astr_such_i0i1(p->pBuffer,txtquot1,"vs",i0,iend)) < (int)0 )
         return ncount;
      i1 = i1-(int)1;
  
      while( ( i=astr_such_i0i1(p->pBuffer,txtsuch,"vs",i0,i1) ) >= (int)0 )
      {
         if( astrs_cut(p,i,i+strlen(txtsuch)-(int)1) != 0 )
             return astrs_err_code;
         if( *txtchange != '\0' ) {
           if( astrs_insert(p,txtchange,i,strlen(txtchange)) != 0 )
             return astrs_err_code;
         }
         i0 = i+1;
         ncount++;
      }

      iact = i1+1+strlen(txtquot1);
    }
  }
    while( iact <= iend )
    {
      if( istatus == (int)1 )
      {
        if( (i0 = astr_such_i0i1(p->pBuffer,txtquot1,"vs",iact,iend)) < (int)0 )
           return ncount;
        i0 = i0+(int)strlen(txtquot1);
      }
      else
        istatus = (int)1;
      
      if( (i1 = astr_such_i0i1(p->pBuffer,txtquot0,"vs",i1,iend)) < (int)0 )
         return ncount;
      i1 = i1-(int)1;
  
      while( ( i=astr_such_i0i1(p->pBuffer,txtsuch,"vs",i0,i1) ) >= (int)0 )
      {
         if( astrs_cut(p,i,i+strlen(txtsuch)-1) != 0 )
             return astrs_err_code;
         if( *txtchange != '\0' ) {
           if( astrs_insert(p,txtchange,i,strlen(txtchange)) != 0 )
             return astrs_err_code;
         }
         i0 = i+1;
         ncount++;
      }

      iact = i1+1+strlen(txtquot0);
    }
  }
#endif
   
/*=============================================================================*/
/*    Sucht in p-> pBuffer die durch txtquot0 und txtquot1 eingeschlossenen    */
/*    String nach txtsuch und ersetzt durch txtchange (wenn txtchange = "\0",  */
/*    dann wird txtsuch rausgeschnitten)                                       */
/*                                                                             */
/*    regel = "i"   Es wird der string innerhalb des Quots behandelt           */
/*            "a"   Es wird der string ausserhalb des Quots beh.               */
/*                                                                             */
/*    istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots         */
/*              1   "                             innerhalb     "              */
/*    Es wird pointer auf istatus übergeben, damit                             */
/*    der letzte Zustannd nach durchforsten des Strings zurückgegeben wird     */
/*=============================================================================*/
int astrs_change_quot1(astrs_s *p,const char *txtsuch,const char *txtchange,
                      const char *txtquot0, const char *txtquot1,const char *regel,
                      int *istatus)
{
  int i0=(int)0;
  int iact = (int)0; 
  int i,i1=(int)0,iend;
  int ncount = (int)0;
  
  if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
  }
  iend = (int)strlen(p->pBuffer)-(int)1;
  if( *regel == 'i' )
  {
    while( iact < iend )
    {
      if( *istatus == (int)0 )
      {
        if( (i0 = astr_such_i0i1(p->pBuffer,txtquot0,"vs",iact,iend)) < (int)0 )
           return ncount;
        i0 = i0+(int)strlen(txtquot0);
      }
      else
        *istatus = (int)0;
      
      if( (i1 = astr_such_i0i1(p->pBuffer,txtquot1,"vs",i0,iend)) < (int)0 )
         return ncount;
      i1 = i1-(int)1;
  
      while( ( i=astr_such_i0i1(p->pBuffer,txtsuch,"vs",i0,i1) ) >= (int)0 )
      {
         if( astrs_cut(p,i,i+strlen(txtsuch)-(int)1) != 0 )
             return astrs_err_code;
         if( *txtchange != '\0' ) {
           if( astrs_insert(p,txtchange,i,strlen(txtchange)) != 0 )
             return astrs_err_code;
         }
         i0 = i+1;
         ncount++;
      }

      iact = i1+1+strlen(txtquot1);
    }
  }
  else
  {
    while( iact < iend )
    {
      if( *istatus == (int)1 )
      {
        if( (i0 = astr_such_i0i1(p->pBuffer,txtquot1,"vs",iact,iend)) < (int)0 )
           return ncount;
        i0 = i0+(int)strlen(txtquot1);
      }
      else
        *istatus = (int)1;
      
      if( (i1 = astr_such_i0i1(p->pBuffer,txtquot0,"vs",i1,iend)) < (int)0 )
         return ncount;
      i1 = i1-(int)1;
  
      while( ( i=astr_such_i0i1(p->pBuffer,txtsuch,"vs",i0,i1) ) >= (int)0 )
      {
         if( astrs_cut(p,i,i+strlen(txtsuch)-1) != 0 )
             return astrs_err_code;
         if( *txtchange != '\0' ) {
           if( astrs_insert(p,txtchange,i,strlen(txtchange)) != 0 )
             return astrs_err_code;
         }
         i0 = i+1;
         ncount++;
      }

      iact = i1+1+strlen(txtquot0);
    }
  }  
  return ncount;    
}
/*==========================================================================*/
/* Sucht in p->pBuffer die Zeichen txtsuch und ersetzt es mit txtchange am  */
/* Anfang und Ende des strings return ncount   Anzahl der changes           */
/*==========================================================================*/
int astrs_change_ae(astrs_s *p,const char *txtsuch,const char *txtchange) {

    int n;
    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_change_ae: p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    n = astrs_change_a(p,txtsuch,txtchange);

    n += astrs_change_e(p,txtsuch,txtchange);

    return n;
}
/*==========================================================================*/
/* Sucht in p->pBuffer die Zeichen txtsuch und ersetzt es mit txtchange am  */
/* Anfang des strings return ncount   Anzahl der changes           */
/*==========================================================================*/
int astrs_change_a(astrs_s *p,const char *txtsuch,const char *txtchange){
  int i;
  int ncount=0;
  
  if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
  }
  if( strcmp(txtsuch,txtchange) != 0 && *txtchange != '\0' && *txtsuch != '\0' ) {
    
      /* Anfang */
    
    while( astr_such(p->pBuffer,txtsuch,"vs") == 0 ) {

        if( astrs_cut(p,0,strlen(txtsuch)-1) != 0 )
            return astrs_err_code;

        ncount++;
    }

    for(i=0;i<ncount;i++)
            if( astrs_insert(p,txtchange,0,strlen(txtchange)) != 0 )
                return astrs_err_code;
            
  }
  
  return ncount;
}
/*==========================================================================*/
/* Sucht in p->pBuffer die Zeichen txtsuch und ersetzt es mit txtchange am  */
/* Ende des strings return ncount   Anzahl der changes           */
/*==========================================================================*/
int astrs_change_e(astrs_s *p,const char *txtsuch,const char *txtchange){
  int i;
  int ncount=0;
  
  if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_cut p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
  }
  if( strcmp(txtsuch,txtchange) != 0 && *txtchange != '\0' && *txtsuch != '\0' ) {
    
      /* Ende */
    while( (astr_such(p->pBuffer,txtsuch,"rs") == ((int)strlen(p->pBuffer)-(int)strlen(txtsuch)-1)) ) {

        if( astrs_cut(p,strlen(p->pBuffer)-strlen(txtsuch)-1,strlen(p->pBuffer)-1) != 0 )
            return astrs_err_code;

        ncount++;
    }

    for(i=0;i<ncount;i++)
            if( astrs_cat(p,txtchange) != 0 )
                return astrs_err_code;
            
  }
  
  return ncount;
}
/*==========================================================================*/
/* Alle Zeichen groß schreiben */
/*==========================================================================*/
int astrs_upper(astrs_s *p) {

    char   *pBuffer;
    size_t is;

    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_upper_err p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    } else {
        pBuffer = p->pBuffer;
    }
    for(is=0;is<strlen(pBuffer);is++)
        pBuffer[is] = (char)toupper( (int)pBuffer[is] );

    return 0;
}
/*==========================================================================*/
/* Alle Zeichen klein schreiben */
/*==========================================================================*/
int astrs_lower(astrs_s *p) {

    char   *pBuffer;
    size_t is;

    if( p == NULL || p->pBuffer == NULL) {
        amsg_set_status("\nastrs_upper_err p or p->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    pBuffer=p->pBuffer;

    for(is=0;is<strlen(p->pBuffer);is++)
        pBuffer[is] = (char)tolower( (int)pBuffer[is] );

    return 0;
}
/*==========================================================================*/
/* Sucht das Trennzeichen aus der Auswahl in txt und schreibt es in ptrennz */
/* Wenn keins gefunden, dann erstes aus auswahl                             */
/*==========================================================================*/
int astrs_extract_t(astrs_s *ptrennz,const char *txt,const char *auswahl){
 
    size_t i;
    int found = 0;
    char t[2] = "\0";

    if( ptrennz == NULL || ptrennz->pBuffer == NULL) {
        amsg_set_status("\nastrs_cat ptrennz or ptrennz->pBuffer not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    if( strlen(auswahl) == 0 ) {
        amsg_set_status("\nastrs_such_trennz_err Kein Character in auswahl; strlen(auswahl) == 0\n");
        astrs_err_code = ASTRS_NO_CHARACTER_IN_PARAMETER;
        return astrs_err_code;
    }
    
    for(i=0;i<strlen(auswahl);i++) {
        t[0] = auswahl[i];
        t[1] = '\0';
        if( astr_such(txt,t,"vs") >0 ) {
            if( astrs_cpy(ptrennz,t) != 0 ) return astrs_err_code;
            found = 1;
            break;
        }
    }
    if( found == 0 ) {
        t[0] = auswahl[0];
        t[1] = '\0';
        if( astrs_cpy(ptrennz,t) != 0 ) return astrs_err_code;
    }
    return 0;
}
/*==============================================================================*/
/* Extrahiert aus txt der Regel Pfad, File und Extension und kopiert es in die  */
/* Struktur p (p->pBuffer). In regel kann stehen:                               */
/* "p"    Pfad, "f" wie filename und "e" wie extension                          */
/* z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn         */
/* vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt*/
/*==============================================================================*/
int astrs_extract_pfe(astrs_s *p,const char *txt,const char *regel) {

    const char *ptxt;
    int itrennz,iext;
    astrs_s *ptrennz=astrs_new();   /* Trennzeichen */

    if( ptrennz == NULL )
        return astrs_err_code;

    if( p == NULL ) {
        amsg_set_status("\nastrs_extract_pfe_err p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }
    /* Zuallesrt den string in p leeren */
    if( astrs_cpy(p,"") != 0 )
        return astrs_err_code;
    
    /* Zuallerst den einfachsten Konstrukt rausfiltern */
    if( strcmp(txt,"")==0 ) {
        return 0;
    }

    /* Dann einfache Pfadkonstrukte rausfiltern */
    if( (strcmp(txt,".")==0)  ||  (strcmp(txt,"..")==0) ) {

        if( (astr_such(regel,"p","vs")>=0) ||
            (astr_such(regel,"P","vs")>=0) ){

            if( astrs_cpy(p,txt) != 0 )
                return astrs_err_code;
        }
        return 0;
    }

    /* Pfadtrennzeichen extrahieren */
    if( astrs_extract_t(ptrennz,txt,"/\\") != 0 )
        return astrs_err_code;

    /* letztes Trennzeichen suchen */
    if( (itrennz=astr_such(txt,ptrennz->pBuffer,"rs"))>=0 ) {

        if( (astr_such(regel,"p","vs")>=0) ||
            (astr_such(regel,"P","vs")>=0) ){

            if( astrs_cati(p,txt,0,(size_t)itrennz) != 0 )
                return astrs_err_code;
        }
        ptxt = &(txt[itrennz+1]);
    } else {

        if( (astr_such(regel,"p","vs")>=0) ||
            (astr_such(regel,"P","vs")>=0) ){

            if( astrs_cat(p,".") != 0 )
                return astrs_err_code;
            if( astrs_cat(p,ptrennz->pBuffer) != 0 )
                return astrs_err_code;
        }
        ptxt = txt;
    }

    /* Jetzt in ptxt extension suchen */
    if( (iext=astr_such(ptxt,".","vs"))>=0 ) {

        if( (astr_such(regel,"f","vs")>=0) ||
            (astr_such(regel,"F","vs")>=0) ){
            
            if( iext>0 ) 
                if( astrs_cati(p,ptxt,0,(size_t)iext-1) != 0 )
                    return astrs_err_code;
        }
        if( (astr_such(regel,"e","vs")>=0) ||
            (astr_such(regel,"E","vs")>=0) ){
            
            if( strlen(ptxt) > 0 )
                if( astrs_cati(p,ptxt,(size_t)iext+1,strlen(ptxt)-1) != 0 )
                    return astrs_err_code;
        }
    } else {

        if( (astr_such(regel,"f","vs")>=0) ||
            (astr_such(regel,"F","vs")>=0) ){
            
            if( strlen(ptxt) > 0 )
                if( astrs_cati(p,ptxt,0,strlen(ptxt)-1) != 0 )
                    return astrs_err_code;
        }
    }


    if( astrs_delete(ptrennz) != 0 )
        return astrs_err_code;

    return 0;
}
/*==============================================================================*/
/* Sucht in Liste txt1 die Stelle istelle mit txttrenn getrennte String         */
/* und gibt ihn in *p->pBuffer zurück  pBuffer wird automatisch verwaltet       */
/* Beispiel txt1 = "abc,cde,wef"; istelle = 1; txttrenn = ","                   */
/* ==> p->pBuffer = "cde"; return 0                                             */
/*==============================================================================*/
int astrs_tget(astrs_s *p,const char *txt1,const char *txttrenn,size_t istelle) {

    size_t n;
    /* Prüfen, ob p initialisiert */
    if( p == NULL ) {
        amsg_set_status("\nastrs_tget_err p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }

    /* Speichergröße bestimmen*/
    n=astr_tstrlen(txt1,txttrenn,istelle);
    if( n < 0 ) {
        amsg_set_status("\nastrs_tget_err error in function astr_tstrlen");
        astrs_err_code = ASTRS_ERROR_IN_STRING_S;
        return astrs_err_code;
    }

    /* Speicher in p->pBuffer bereitstellen */
    if( astrs_mem(p,n+1) != 0 )
        return astrs_err_code;

    if( astr_tget(p->pBuffer,txt1,txttrenn,istelle) < 0 ) {
        amsg_set_status("\nastrs_tget_err error in function astr_tget");
        astrs_err_code = ASTRS_ERROR_IN_STRING_S;
        return astrs_err_code;
    }
    
    return 0;
}
/*==============================================================================*/
/* Sucht in Liste txt1 die Stelle istelle mit txttrenn getrennte String         */
/* und gibt ihn in *p->pBuffer zurück  pBuffer wird automatisch verwaltet.      */
/* Laesst Text innerhalb von txtquot0 und txtquot1 ausser acht.                 */
/* Beispiel txt1 = "abc,\"cd,e"\,wef"; istelle = 1; txttrenn = ",",tquot0="\""  */
/* tquot1="\"" ==> p->pBuffer = "cd,e"; return 0                                */
/*==============================================================================*/
int astrs_tget_quot(astrs_s *p, const char *txt1,const char *txttrenn,size_t istelle,
                     const char *tquot0,const char *tquot1)
{
    size_t n;
    /* Prüfen, ob p initialisiert */
    if( p == NULL ) {
        amsg_set_status("\nastrs_tget_quot_err p not initialised");
        astrs_err_code = ASTRS_P_NOT_INITIALISED;
        return astrs_err_code;
    }

    /* Speichergröße bestimmen*/
    n=astr_tstrlen_quot(txt1,txttrenn,istelle,tquot0,tquot1);
    if( n < 0 ) {
        amsg_set_status("\nastrs_tget_quot_err error in function astr_tstrlen_quot");
        astrs_err_code = ASTRS_ERROR_IN_STRING_S;
        return astrs_err_code;
    }

    /* Speicher in p->pBuffer bereitstellen */
    if( astrs_mem(p,n+1) != 0 )
        return astrs_err_code;

    if( astr_tget_quot(p->pBuffer,txt1,txttrenn,istelle,tquot0,tquot1) < 0 ) {
        amsg_set_status("\nastrs_tget_quot_err error in function astr_tget_quot");
        astrs_err_code = ASTRS_ERROR_IN_STRING_S;
        return astrs_err_code;
    }
    
    return 0;

}
/*==============================================================================*/
/*= extrahiert aus txt gequoteten text in struktur p                           =*/
/*==============================================================================*/
int astrs_get_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1){

    int i0 = astr_such(txt,txtquot0,"vs");
    int i1 = astr_such_i0i1(txt,txtquot1,"vs",MAX(0,i0+(int)strlen(txtquot0)),strlen(txt));

    if( i0 < 0 || i1 < 0 ) {
        amsg_set_status("\nastrs_get_quot_err error in function astrs_get_quot: no quoted text found");
        astrs_err_code = ASTRS_QUOT_NOT_FOUND;
        return astrs_err_code;
    }
    return astrs_cpyi(p,txt,i0+strlen(txtquot0),i1-1);

}
/*==============================================================================*/
/*= Führt path0 und path1 zusammen und schreibt in new_path                    =*/
/*==============================================================================*/
int astrs_match_spath(astrs_s *ps_new_path,astrs_s *ps_path0,astrs_s *ps_path1) {
    return astrs_match_path(ps_new_path,astrs_string(ps_path0),astrs_string(ps_path1));
}
int astrs_match_path(astrs_s *ps_new_path,const char *p_path0,const char *p_path1) {

    int i;

    astrs_s *ps_0   = astrs_new();
    astrs_s *ps_1   = astrs_new();
    astrs_s *ps_dum = astrs_new();

    /* Prüfen, ob ps_new_path initialisiert */
    if( ps_new_path == NULL ) {
        ps_new_path = astrs_new();
    }
    /* Kopiert path0 */
    if( astrs_cpy(ps_new_path,p_path0) != 0 )
        return astrs_err_code;

    /* Sucht nach Begrenzungszeichen */    
    if( astrs_extract_t(ps_0,astrs_string(ps_new_path),"/\\") != 0 )
        return astrs_err_code;

    /* Schneidet am Ende ab */
    astrs_change_e(ps_new_path,astrs_string(ps_0),"");


    /* Zwischenkopieren */
    if( astrs_cpy(ps_dum,p_path1) != 0 )
        return astrs_err_code;

    /* Sucht relativen Pfad .. in path1 am Anfang */
    if( astrs_such(ps_dum,"..","vs") == 0 ) {
        /* Schneidet ab */
        astrs_change_a(ps_dum,"..","");

        /* Sucht Pfad in p_path0 */
        if( (i=astrs_ssuch(ps_new_path,ps_0,"rs")) > 0 )
            astrs_cut(ps_new_path,i,astrs_len(ps_new_path)-1);
        else if( (i=astrs_such(ps_new_path,":","rs")) > 0 )
            astrs_cut(ps_new_path,i+1,astrs_len(ps_new_path)-1);
        else
            astrs_cpy(ps_new_path,"");
    }
    /* Sucht relativen Pfad . in path1 */
    else if(astrs_such(ps_dum,".","vs") == 0 ) {
        /* Schneidet ab */
        astrs_change_a(ps_dum,".","");
    }

    /* Sucht nach Begrenzungszeichen in path1*/
    if( astrs_extract_t(ps_1,astrs_string(ps_dum),"/\\") != 0 )
        return astrs_err_code;

    /* Schneidet am Anfang ab */
    astrs_change_a(ps_dum,astrs_string(ps_1),"");


    /* Fügt Pfade an */
    if( astrs_len(ps_new_path) > 0 )
        if( astrs_scat(ps_new_path,ps_1) != 0 )
            return astrs_err_code;
    if( astrs_scat(ps_new_path,ps_dum) != 0 )
        return astrs_err_code;


    if( (astrs_such(ps_new_path,"/","vs") > 0) && (astrs_such(ps_new_path,"\\","vs") > 0) )
        astrs_change(ps_new_path,"\\","/");

    astrs_delete(ps_0);
    astrs_delete(ps_1);
    astrs_delete(ps_dum);

    return 0;
}

/*==============================================================================*/
/*= Führt path0 und path1 zusammen und schreibt in new_path                    =*/
/*==============================================================================*/
int astrs_match_spath_file(astrs_s *ps_path_file,astrs_s *ps_path,astrs_s *ps_file) {
    return astrs_match_path_file(ps_path,astrs_string(ps_path),astrs_string(ps_file));
}
int astrs_match_path_file(astrs_s *ps_path_file,const char *p_path,const char *p_file) {

    int ierr=OK;

    astrs_s *ps_0   = astrs_new();
    astrs_s *ps_dum = astrs_new();

    /* Prüfen, ob ps_path initialisiert */
    if( ps_path_file == NULL ) {
        ps_path_file = astrs_new();
    }

    /* Extrahiert Pfad von p_file und matched mit p_path und kopiert in ps_path_file*/
    if( astr_such_pfe(p_file,"p") == 0 ) {
        astrs_extract_pfe(ps_dum,p_file,"p") ;
        ierr = astrs_match_path(ps_path_file,p_path,astrs_string(ps_dum));
        if( ierr != OK )
            return ierr;
    /* kopiert p_path in ps_path_file */
    } else {
        astrs_cpy(ps_path_file,p_path);
    }

    /* Sucht nach file und extrahiert */
    if( astr_such_pfe(p_file,"fe") == 0 )
        astrs_extract_pfe(ps_dum,p_file,"fe") ;
    else if( astr_such_pfe(p_file,"f") == 0 )
        astrs_extract_pfe(ps_dum,p_file,"f") ;
    else
        astrs_cpy(ps_dum,"");

    if( astrs_len(ps_dum) > 0 ) {

        /* Sucht nach Begrenzungszeichen */    
        if( astrs_extract_t(ps_0,astrs_string(ps_path_file),"/\\") != 0 )
            return astrs_err_code;

        /* Schneidet am Ende ab */
        astrs_change_e(ps_path_file,astrs_string(ps_0),"");

        /* Fügt an */
        astrs_scat(ps_path_file,ps_0);
        astrs_scat(ps_path_file,ps_dum);
    }

    astrs_delete(ps_0);
    astrs_delete(ps_dum);

    return ierr;
}
/*==============================================================================*/
/*= sucht string ps_such in ps_txt                                             =*/
/*==============================================================================*/
int astrs_ssuch(astrs_s *ps_txt,astrs_s *ps_such,char *regel) {
    return astrs_such(ps_txt,astrs_string(ps_such),regel);
}
int astrs_such(astrs_s *ps_txt,char *p_such,char *regel) {
    return (int)astr_such(astrs_string(ps_txt),p_such,regel);
}
int astrs_such_pfe(astrs_s *ps_input,const char *pregel) {
    return astr_such_pfe(astrs_string(ps_input),pregel);
}
int astrs_such_i0i1(astrs_s *ps_txt, const char *text, const char *regel, size_t i0,size_t i1) {
    return astr_such_i0i1(astrs_string(ps_txt), text, regel,i0,i1);
}

int astrs_such_quot(astrs_s *ps_txt,const char *txtsuch,const char *suchregel,
                    const char *txtquot0, const char *txtquot1,const char *quotregel,
                    UINT8_T istatus) {
    return astr_such_quot(astrs_string(ps_txt),txtsuch,suchregel,
                          txtquot0,txtquot1,quotregel,istatus);
}

/*=============================================================================================*/
/*=============================================================================================*/
/*= astrv_s - Funktionen                                                                 =*/
/*=============================================================================================*/
/*=============================================================================================*/

/*=================================================================*/
/* Anlegen einer neuen Matrix_t-Datenstruktur. */
/*=================================================================*/
astrv_s *astrv_new(void){

    astrv_s *pv=NULL;

    /* Memory für die Struktur pv */
    amem_mem_g((void **)&pv,1,sizeof(astrv_s));
    if( pv == NULL ) {
        amsg_set_status("\nastrv_new_err: no memory for astrv_s pv");
        astrs_err_code = ASTRS_NO_MEMORY;
        return pv;
    } else {
        pv->pp   = NULL;
        pv->nrow = 0;
    }

    /* Initialisierter Popinter zurückgeben */
    return pv;
}
/*=================================================================*/
/* Matrix_t für pv anlegen */
/* pv[row]
/*=================================================================*/
int astrv_make(astrv_s *pv,size_t nrow) {

    int ierr;
    astrm_s m;

    if( pv == NULL ) {
        pv = astrv_new();
        if( pv == NULL ) {
            amsg_set_status("\nastrm_make_err pm not initialised");
            astrs_err_code = ASTRM_PM_NOT_INITIALISED;
            return astrs_err();
        }
        pv->pp   = NULL;
        pv->nrow = 0;
    }
    astrm_cpy_vpointer(&m,pv);

    ierr = astrm_make(&m,nrow,1);

    astrv_cpy_mpointer(pv,&m);

    return ierr;
}
/*=================================================================*/
/* Löschen einer Werte in der Struktur. */
/*=================================================================*/
int astrv_free(astrv_s *pv) {

    size_t irow;
    if( pv == NULL ) return 0;

    if( pv->nrow != 0 ) {
            for(irow=0;irow<pv->nrow;irow++) {

                if( ( astrs_err_code = astrs_delete( *(pv->pp+irow) ) ) != 0 ) {
                    amsg_set_status("\nastrm_delete_err: error when deleting pv->pp[]");
                    return astrs_err_code;
                }
            }
    }
    pv->nrow = 0;
    pv->pp   = NULL;

    return 0;
}
/*=================================================================*/
/* Löschen einer Vektorstruktur. */
/*=================================================================*/
int astrv_delete(astrv_s *pv) {

    if( pv == NULL ) return 0;

    if( (astrs_err_code = astrv_free(pv)) != 0 )
        return astrs_err_code;

    amem_free_g((void **)&pv);

    return 0;
}
/*=================================================================*/
/* Kopiere string in  Vektorstruktur. */
/*=================================================================*/
int astrv_cpy(astrv_s *pv,size_t irow,char *pstring){

    int          ierr;
    astrm_s m;
    if( pv == NULL ) {
        pv = astrv_new();
        if(pv == NULL)return astrs_err();
    }
    astrm_cpy_vpointer(&m,pv);
    ierr = astrm_cpy(&m,irow,0,pstring);
    astrv_cpy_mpointer(pv,&m);

    return ierr;
}
/*=================================================================*/
/* Kopiere Teil string in  Vektorstruktur. */
/*=================================================================*/
int astrv_cpyi(astrv_s *pv,size_t irow,char *pstring,size_t i0, size_t i1) {

    int          ierr;
    astrm_s m;
    if( pv == NULL ) {
        pv = astrv_new();
        if(pv == NULL)return astrs_err();
    }
    astrm_cpy_vpointer(&m,pv);
    ierr = astrm_cpyi(&m,irow,0,pstring,i0,i1);
    astrv_cpy_mpointer(pv,&m);

    return ierr;
}
/*=================================================================*/
/* Kopiere astrs_s in  Vektorstruktur. */
/*=================================================================*/
int astrv_scpy(astrv_s *pv,size_t irow,astrs_s *pquelle){

    int          ierr;
    astrm_s m;
    if( pv == NULL ) {
        pv = astrv_new();
        if(pv == NULL)return astrs_err();
    }
    astrm_cpy_vpointer(&m,pv);
    ierr = astrm_scpy(&m,irow,0,pquelle);
    astrv_cpy_mpointer(pv,&m);

    return ierr;
}
/*=================================================================*/
/* Kopiere Vektor String in  Vektorstruktur. */
/*=================================================================*/
int  astrv_vcpy(astrv_s *pv,size_t irow,
                astrv_s *pvquelle,size_t irowquelle){
    int          ierr;
    astrm_s m,mq;
    if( pv == NULL ) {
        pv = astrv_new();
        if(pv == NULL)return astrs_err();
    }
    astrm_cpy_vpointer(&m,pv);
    astrm_cpy_vpointer(&mq,pvquelle);
    ierr = astrm_mcpy(&m,irow,0,&mq,irowquelle,0);
    astrv_cpy_mpointer(pv,&m);
    astrv_cpy_mpointer(pvquelle,&mq);

    return ierr;

}
/*=================================================================*/
/* Kopiere Matrix_t String in  Vektorstruktur. */
/*=================================================================*/
int astrv_mcpy(astrv_s *pv,size_t irow, astrm_s *pmquelle,size_t irowquelle,size_t icolquelle){
    int          ierr;
    astrm_s m;
    if( pv == NULL ) {
        pv = astrv_new();
        if(pv == NULL)return astrs_err();
    }
    astrm_cpy_vpointer(&m,pv);
    ierr = astrm_mcpy(&m,irow,0,pmquelle,irowquelle,icolquelle);
    astrv_cpy_mpointer(pv,&m);

    return ierr;
}
/*======================================================================================*/
/* Anhängen einer String-Struktur psquelle an die  Vektorstruktur pv. */
/*======================================================================================*/
int astrv_scat(astrv_s *pv, astrs_s *psquelle) {

    if( astrs_len(psquelle) > 0 )
        astrv_cat(pv,astrs_string(psquelle));
    return 0;
}
/*======================================================================================*/
/* Anhängen eines strings quelle an die  Vektorstruktur pv. */
/*======================================================================================*/
int astrv_cat(astrv_s *pv, char *quelle) {
	
	astrv_make(pv,astrv_nrow(pv)+1);

    astrv_cpy(pv,astrv_nrow(pv)+1,quelle);

	return 0;
}

/*======================================================================================*/
/* Anhängen einer der vollständigen Vektor-Struktur pvquelle an die  Vektorstruktur pv. */
/*======================================================================================*/
int astrv_vcat_full(astrv_s *pv, astrv_s *pvquelle) {

	size_t i,nziel,nquelle;

	nziel   = astrv_nrow(pv);
	nquelle = astrv_nrow(pvquelle);
	
	if( nquelle == 0 )
		return 0;

	astrv_make(pv,nziel+nquelle);

	for(i=0;i<nquelle;i++) {

		astrv_cpy(pv,nziel+i,astrv_string(pvquelle,i));
	}

	return 0;
}
/*======================================================================================*/
/* Anhängen einer String-Struktur an einem bestimmten Element */
/*======================================================================================*/
int astrv_cat_str(astrv_s *pv, size_t irow, char *text) {

    size_t n = astrv_nrow(pv);

    if( irow < n ) {

        astrs_s *ps = astrs_new();

        astrs_scpy(ps,astrv_astrs(pv,irow));

        astrs_cat(ps,text);

        astrv_scpy(pv,irow,ps);

        astrs_delete(ps);
    } else {

        astrv_cpy(pv,irow,text);
    }
    return 0;
}

/*====================================================================================*/
/* Fügt an Stelle  irow Text oder die Struktur astrs_sin die Vektorstruktur ein. */
/*====================================================================================*/
int             astrv_sinsert(astrv_s *pv,astrs_s *ps_insert,size_t irow) {

	return astrv_insert(pv,astrs_string(ps_insert),irow);
}
int  astrv_insert(astrv_s *pv,char *p_insert,size_t irow) {


    size_t i,n;

    if(  pv == NULL  ) {
        amsg_set_status("\nastrv_insert_err pv not initialised");
        return ASTRV_PV_NOT_INITIALISED;
    }

	n = astrv_nrow(pv);
	if( irow > n )
		irow = n;

	n++;

    astrv_make(pv,n);

	if( n > 1 )
		for(i=n-1;i>irow;i--)
			astrv_cpy(pv,i,astrv_string(pv,i-1));
		
	astrv_cpy(pv,irow,p_insert);

	return 0;
}

/*=================================================================*/
/* Schneidet irow0 bis irow1 strukturen der Vektorstruktur raus. */
/*=================================================================*/
int astrv_scut_first(astrv_s *pv) {
    return astrv_scut(pv,0,0);
}
int astrv_scut_last(astrv_s *pv) {
    return astrv_scut(pv,pv->nrow-1,pv->nrow-1);
}
int astrv_scut(astrv_s *pv,size_t irow0,size_t irow1){

    size_t i;

    if(  pv == NULL  ) {
        amsg_set_status("\nastrv_scut_err pv not initialised");
        return ASTRV_PV_NOT_INITIALISED;
    }
    if( irow1 < irow0 ) 
        irow1 = irow0;
    if( irow1 > pv->nrow-1 ) {
        amsg_set_status("\nastrv_scut_err nrow of pv too small");
        return ASTRV_WRONG_PARAMETER;
    }

    for(i=0;i<pv->nrow-irow1-1;i++) 
        astrv_vcpy(pv,irow0+i,pv,irow1+i+1);

    astrv_make(pv,pv->nrow-(irow1-irow0+1));

    return 0;
    
}
int astrv_scut_such(astrv_s *pv,char *p_such) {

    size_t i;

    if(  pv == NULL  ) {
        amsg_set_status("\nastrv_scut_such_err pv not initialised");
        return ASTRV_PV_NOT_INITIALISED;
    }

	while( astrv_such(pv,p_such,&i) == TRUE ) {
		astrv_scut(pv,i,i);
	}

	return 0;
}
/*==========================================================================*/
/* Gibt pointer auf pv->pp[irow]->pBuffer zurück                            */
/*==========================================================================*/
char *astrv_string(astrv_s *pv,size_t irow){
    if(  pv == NULL  
      || irow >= pv->nrow )
        return NULL;
    return pv->pp[irow]->pBuffer;
}
/*==========================================================================*/
/* Gibt letzten pointer pv->pp[nrow-1]->pBuffer zurück                            */
/*==========================================================================*/
char *astrv_string_last(astrv_s *pv){
    if(  pv == NULL  
      || pv->nrow == 0)
        return NULL;
    return pv->pp[pv->nrow-1]->pBuffer;
}
/*==========================================================================*/
/* Gibt pointer letzten wert auf pv->pp[nrow]->pBuffer zurück               */
/*==========================================================================*/
char *astrv_last_string(astrv_s *pv) {

    return astrv_string(pv,pv->nrow-1);
}

/*==========================================================================*/
/* Gibt astrs_s-pnter auf pv->pp[irow] zurück                            */
/*==========================================================================*/
astrs_s   *astrv_astrs(astrv_s *pv,size_t irow) {
    if(  pv == NULL  
      || irow >= pv->nrow )
        return NULL;
    return pv->pp[irow];
}

/*==========================================================================*/
/* Gibt Laenge des vektors zurück                                           */
/*==========================================================================*/
size_t          astrv_nrow(astrv_s *pv){
    if( pv == NULL )
        return 0;
    else
        return pv->nrow;
}
/*==========================================================================*/
/* Gibt Laenge des strings eines vektors zurück                                           */
/*==========================================================================*/
size_t  astrv_len(astrv_s *pv,size_t irow) {
    return strlen(astrv_string(pv,irow));
}

/*=================================================================*/
/* Kopiere Pointer MAtrixstruktur. in Vektorstruct */
/*=================================================================*/
void astrv_cpy_mpointer(astrv_s *pv,astrm_s *pm){
    
    if( pm != NULL ) {
        pv->pp    = pm->pp;
        pv->nrow  = pm->nrow * pm->ncol;
    } else {
        pv = NULL;
    }
}
/*=================================================================*/
/* Suchen in struktur pv nach einer bestimmenten Zelle */
/*=================================================================*/
int             astrv_ssuch(astrv_s *pv,astrs_s *ps_such,size_t *p_i_found) {

	return astrv_such_irow(pv,astrs_string(ps_such),p_i_found,0);
}
int             astrv_such(astrv_s *pv,char *p_such,size_t *p_i_found) {

	return astrv_such_irow(pv,p_such,p_i_found,0);
}
int astrv_ssuch_irow(astrv_s *pv,astrs_s *ps_such,size_t *p_i_found,size_t irow){

	return astrv_such_irow(pv,astrs_string(ps_such),p_i_found,irow);
}
int astrv_such_irow(astrv_s *pv,char *p_such,size_t *p_i_found,size_t irow) {

    size_t i;

    if(  pv == NULL ) {
        amsg_set_status("\nastrv_such_irow_err pv not initialised");
        return ASTRV_PV_NOT_INITIALISED;
    }

    if(  (pv->nrow == 0)
	  || (irow >= pv->nrow)
	  )
        return FALSE;
    
    for(i=irow;i<pv->nrow;i++) {
		if( strcmp(pv->pp[i]->pBuffer,p_such) == 0 ) {

			*p_i_found = i;
			return TRUE;
		}
	}
	return FALSE;

}
/*=================================================================*/
/* Zerlegt p_text (Trennzeichen p_split)                           */
/*=================================================================*/
int astrv_split(astrv_s *pv,char *p_text,char *p_split) {

    size_t i,n;
    int    ierr=0;
    astrs_s *ps_text=astrs_new();

    /* Zählt Blöcke */
    n =  astr_tcount(p_text,p_split);

    for(i=0;i<n;i++) {

        if( (ierr=astrs_tget(ps_text,p_text,p_split,i)) != 0 )
            return ierr;

        if( (ierr=astrv_scpy(pv,i,ps_text)) != 0 )
            return ierr;
    }

    astrs_delete(ps_text);
    return 0;
}
/*=================================================================*/
/* Zerlegt p_text (Trennzeichen p_split)                           */
/*=================================================================*/
int astrv_split_quot(astrv_s *pv,char *p_text,char *p_split, char *p_quot0, char *p_quot1) {

    size_t i,n;
    int    ierr=0;
    astrs_s *ps_text=astrs_new();

    /* Zählt Blöcke */
    n =  astr_tcount_quot(p_text,p_split,p_quot0,p_quot1);

    for(i=0;i<n;i++) {

        if( (ierr=astrs_tget_quot(ps_text,p_text,p_split,i,p_quot0,p_quot1)) != 0 )
            return ierr;

        if( (ierr=astrv_scpy(pv,i,ps_text)) != 0 )
            return ierr;
    }

    astrs_delete(ps_text);
    return 0;
}
/*=============================================================================================*/
/*=============================================================================================*/
/*= astrm_s - Funktionen                                                                 =*/
/*=============================================================================================*/
/*=============================================================================================*/

/*=================================================================*/
/* Anlegen einer neuen Matrix_t-Datenstruktur. */
/*=================================================================*/
astrm_s *astrm_new(void){

    astrm_s *pm=NULL;

    /* Memory für die Struktur pm */
    amem_mem_g((void **)&pm,1,sizeof(astrm_s));
    if( pm == NULL ) {
        amsg_set_status("\nastrm_new_err: no memory for astrm_s pm");
        astrs_err_code = ASTRS_NO_MEMORY;
        return pm;
    } else {
        pm->pp   = NULL;
        pm->nrow = 0;
        pm->ncol = 0;
    }

    /* Initialisierter Popinter zurückgeben */
    return pm;
}
/*=================================================================*/
/* Matrix_t für pm anlegen */
/* pm[col][row]
/*=================================================================*/
int astrm_make(astrm_s *pm,size_t nrow,size_t ncol) {

    size_t icol,irow,im,iquel,iziel;

    if( pm == NULL ) {
        pm = astrm_new();
        if( pm == NULL ) {
            amsg_set_status("\nastrm_make_err pm not initialised");
            astrs_err_code = ASTRM_PM_NOT_INITIALISED;
            return astrs_err();
        }
        pm->pp   = NULL;
        pm->nrow = 0;
        pm->ncol = 0;
    }

    /* Keine Veränderung */
    if( nrow == pm->nrow && ncol == pm->ncol ) return 0;

    /* MAtrixx verkleinern */
    /* Spalten weg nehmen */
    if( ncol < pm->ncol ) {
        for(icol=ncol;icol<pm->ncol;icol++) {
            for(irow=0;irow<pm->nrow;irow++) {        
                im = icol*pm->nrow+irow;
                if( astrs_delete( *(pm->pp+im) ) != 0 ) {
                        amsg_set_status("\nastrm_make_err: error when deleting pm->pp+i");
                        return astrs_err();
                }
            }
        }
        if( (astrs_err_code=amem_mem_g((void **)&(pm->pp),pm->nrow*ncol,sizeof(astrs_s *))) != 0 ) {
            amsg_set_status("\nastrm_make_err: error with amem_mem_g reducing columns");
            return astrs_err_code;
        }
        pm->ncol = ncol;
    }
    /* Reihen wegnehmen */
    if( nrow < pm->nrow ) {
        for(icol=1;icol<pm->ncol;icol++) {
            for(irow=0;irow<nrow;irow++) {
                iquel=icol*pm->nrow+irow;
                iziel=icol*nrow+irow;
                if( astrs_scpy(*(pm->pp+iziel),*(pm->pp+iquel)) != 0 ) {
                    amsg_set_status("\nastrm_make_err: error astrs_scpy reducing rows");
                    return astrs_err();
                }
            }
        }
        if( (astrs_err_code=amem_mem_g((void **)&(pm->pp),nrow*pm->ncol,sizeof(astrs_s *))) != 0 ) {
            amsg_set_status("\nastrm_make_err: error with amem_mem_g reducing rows");
            return astrs_err_code;
        }
        pm->nrow = nrow;
    }
                    
    /* Matrix_t vergrößern */
    if( nrow > pm->nrow || ncol > pm->ncol ) {

        /* Speicher anfordern */
        if( amem_mem_g((void **)&(pm->pp),nrow*ncol,sizeof(astrs_s *)) != 0 ) {
            amsg_set_status("\nastrm_make_err: no memory for astrs_s pm->pp expanding rows and columns ");
            astrs_err_code = ASTRS_NO_MEMORY;
            return astrs_err_code;
        }
        /* zusätzlich astrs_s anlegen */
        for(im=pm->nrow*pm->ncol;im<nrow*ncol;im++) {
            if( ( *(pm->pp+im) = astrs_new() ) == NULL ) {
                amsg_set_status("\nastrm_make_err: no memory for astrs_s pm->pp[][] expanding rows and columns");
                astrs_err_code = ASTRS_NO_MEMORY;
                return astrs_err_code;
            }
        }
        /* Werte verschieben, wenn cols vergrößert wurden */
        if( nrow > pm->nrow && pm->ncol > 1) {
            icol     = pm->ncol;
            while( icol-- ) {
                irow     = pm->nrow;
                while( irow-- ) {
                    iquel = icol*pm->nrow+irow;
                    iziel = icol*nrow+irow;
                    if( astrs_smove(*(pm->pp+iziel),*(pm->pp+iquel)) != 0 ) {
                        amsg_set_status("\nastrm_make_err: error astrs_move expanding rows and columns");
                        return astrs_err();
                    }
                }

            }

        }
        pm->nrow = nrow;
        pm->ncol = ncol;
    }
    return 0;
}
/*=================================================================*/
/* Löschen von Werten einer Matrixstruktur. */
/*=================================================================*/
int astrm_free(astrm_s *pm) {

    size_t icol,irow,im;
    if( pm == NULL ) return 0;

    if( pm->nrow * pm->ncol != 0 ) {
        for(icol=0;icol<pm->ncol;icol++) {
            for(irow=0;irow<pm->nrow;irow++) {

                im = icol*pm->nrow+irow;
                if( ( astrs_err_code = astrs_delete( *(pm->pp+im) ) ) != 0 ) {
                    amsg_set_status("\nastrm_delete_err: error when deleting pm->pp[][]");
                    return astrs_err_code;
                }
            }
        }
    }

    return 0;
}
/*=================================================================*/
/* Löschen einer MAtrixstruktur. */
/*=================================================================*/
int astrm_delete(astrm_s *pm) {

    if( pm == NULL ) return 0;

    if( (astrs_err_code = astrm_free(pm)) != 0 )
        return astrs_err_code;

    amem_free_g((void **)&pm);


    return 0;
}
/*=================================================================*/
/* Kopiere string in  MAtrixstruktur. */
/*=================================================================*/
int astrm_cpy(astrm_s *pm,size_t irow, size_t icol,char *pstring){

    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im=icol*nrow+irow;
    if( astrs_cpy(pm->pp[im],pstring) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Kopiere Teilstring in  MAtrixstruktur. */
/*=================================================================*/
int astrm_cpyi(astrm_s *pm,size_t irow, size_t icol,char *pstring,size_t i0, size_t i1) {

    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im=icol*nrow+irow;
    if( astrs_cpyi(pm->pp[im],pstring,i0,i1) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Kopiere astrs_s in  MAtrixstruktur. */
/*=================================================================*/
int astrm_scpy(astrm_s *pm,size_t irow, size_t icol,astrs_s *pquelle){

    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im=icol*nrow+irow;
    if( astrs_scpy(pm->pp[im],pquelle) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Kopiere astrv_s in  MAtrixstruktur. */
/*=================================================================*/
int  astrm_vcpy(astrm_s *pm,size_t icol,astrv_s *pquelle) {

    size_t ncol = MAX(pm->ncol,icol+1);
    size_t nrow = MAX(pm->nrow,astrv_nrow(pquelle));
    size_t im,irow,n;

    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    
    n = astrv_nrow(pquelle);

    for(irow=0;irow<n;irow++) {
        im=icol*nrow+irow;
        if( astrs_scpy(pm->pp[im],astrv_astrs(pquelle,irow)) != 0 ) {
            return astrs_err();
        }
    }
    return 0;
}
/*=================================================================*/
/* Kopiere Matrix_t String in  MAtrixstruktur. */
/*=================================================================*/
int  astrm_mcpy(astrm_s *pm,size_t irow, size_t icol,
                astrm_s *pmquelle,size_t irowquelle, size_t icolquelle){
    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im,iquelle;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im      = icol*nrow+irow;
    iquelle = icolquelle*(pmquelle->nrow)+irowquelle;
    if( astrs_scpy(pm->pp[im],pmquelle->pp[iquelle]) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Hänge string in  MAtrixstruktur an. */
/*=================================================================*/
int astrm_cat(astrm_s *pm,size_t irow, size_t icol,char *pstring){

    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im=icol*nrow+irow;
    if( astrs_cat(pm->pp[im],pstring) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Hänge Teilstring in  MAtrixstruktur an. */
/*=================================================================*/
int astrm_cati(astrm_s *pm,size_t irow, size_t icol,char *pstring,size_t i0, size_t i1) {

    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im=icol*nrow+irow;
    if( astrs_cati(pm->pp[im],pstring,i0,i1) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Hänge astrs_s an  MAtrixstruktur an. */
/*=================================================================*/
int astrm_scat(astrm_s *pm,size_t irow, size_t icol,astrs_s *pquelle){

    size_t nrow = MAX(pm->nrow,irow+1);
    size_t ncol = MAX(pm->ncol,icol+1);
    size_t im;
    if( pm == NULL ) {
        pm = astrm_new();
        if(pm == NULL)return astrs_err();
    }
    if( astrm_make(pm,nrow,ncol) != 0) {
        return astrs_err();
    }
    im=icol*nrow+irow;
    if( astrs_scat(pm->pp[im],pquelle) != 0 ) {
        return astrs_err();
    }
    return 0;
}
/*=================================================================*/
/* Sucht oder hängt Name in Spalte icol der  MAtrixstruktur an. */
/*=================================================================*/
size_t astrm_find_or_scat_row(astrm_s *pm,size_t icol,astrs_s *ps_name) {
    return astrm_find_or_cat_row(pm,icol,astrs_string(ps_name));
}
size_t astrm_find_or_cat_row(astrm_s *pm,size_t icol,char *p_name) {

    char found_flag = 0;
    size_t i;

    if( icol >= pm->ncol )
        icol = pm->ncol-1;
    
    for(i=0;i<astrm_nrow(pm);i++) {

        if( strcmp(astrm_string(pm,i,icol),p_name) == 0 ) {
            /* Wenn gefunden index nehmen */
            found_flag = 1;
            break;
        }
    }
    if( !found_flag ) { /* Wenn nicht gefunden hinzufügen*/
        i = astrm_nrow(pm);
        astrm_cpy(pm,i,icol,p_name);
    } 
    
    return i;
}
/*=================================================================*/
/* Hängt string an Matrix_t in der Apalte icol an         */
/*=================================================================*/
size_t  astrm_scat_row(astrm_s *pm,size_t icol,astrs_s *ps) {
    return astrm_cat_row(pm,icol,astrs_string(ps));
}
size_t astrm_cat_row(astrm_s *pm,size_t icol,char *p_string) {

    if( icol >= pm->ncol )
        icol = pm->ncol-1;

    astrm_cpy(pm,astrm_nrow(pm),icol,p_string);

    return astrm_nrow(pm)-1;
}

/*=================================================================*/
/* Schneidet Zeile raus         */
/*=================================================================*/
int   astrm_cut_irow(astrm_s *pm,size_t irow) {
    size_t ir,ic,imq,imz;
    if( pm == NULL ) return 0;
    if( irow >= pm->nrow ) return 0;

    for(ic=0;ic<pm->ncol;ic++) {
        for(ir=irow+1;ir<pm->nrow;ir++) {

            imq = ic*pm->nrow+ir;
            imz = ic*pm->nrow+ir-1;
            if( astrs_scpy(pm->pp[imz],pm->pp[imq]) != 0 ) 
                return astrs_err();
        }
    }
    if( astrm_make(pm,pm->nrow-1,pm->ncol) != 0 )
        return astrs_err();

    return 0;
}
/*=================================================================*/
/* Schneidet Spalte raus         */
/*=================================================================*/
int  astrm_cut_icol(astrm_s *pm,size_t icol) {
    size_t ir,ic,imq,imz;
    if( pm == NULL ) return 0;
    if( icol >= pm->ncol ) return 0;

    for(ic=icol+1;ic<pm->ncol;ic++) {
        for(ir=0;ir<pm->nrow;ir++) {

            imq = ic*pm->nrow+ir;
            imz = (ic-1)*pm->nrow+ir;
            if( astrs_scpy(pm->pp[imz],pm->pp[imq]) != 0 ) 
                return astrs_err();
        }
    }
    if( astrm_make(pm,pm->nrow,pm->ncol-1) != 0 )
        return astrs_err();
    return 0;
}
/*=================================================================*/
/* Schneidet in jeder Zelle am Anfang und Ende txtcut raus         */
/*=================================================================*/
int astrm_cut_ae(astrm_s *pm,char *txtcut) {

    size_t ir,ic;
    if( pm == NULL ) return 0;
    
    for(ir = 0;ir<pm->nrow;ir++) {
        for(ic = 0;ic<pm->ncol;ic++) {
            astrs_cut_ae(astrm_astrs(pm,ir,ic),txtcut);
        }
    }
    return 0;
}
/*==========================================================================*/
/* Gibt pointer auf pm->pp[nrow*icol+irow]->pBuffer zurück                            */
/*==========================================================================*/
char *astrm_string(astrm_s *pm,size_t irow,size_t icol){
    if(  pm == NULL  
      || irow >= pm->nrow 
      || icol >= pm->ncol)
        return NULL;
    return pm->pp[pm->nrow*icol+irow]->pBuffer;
}
/*==========================================================================*/
/* Gibt pointer auf pm->pp[nrow*icol+irow] zurück                            */
/*==========================================================================*/
astrs_s *astrm_astrs(astrm_s *pm,size_t irow,size_t icol){
    if(  pm == NULL  
      || irow >= pm->nrow 
      || icol >= pm->ncol)
        return NULL;
    return pm->pp[pm->nrow*icol+irow];
}
/*==========================================================================*/
/* Gibt nrow Anzahl Zeilen  zurück                            */
/*==========================================================================*/
size_t          astrm_nrow(astrm_s *pm) {
    if(  pm == NULL )
        return 0;
    return pm->nrow;
}
/*==========================================================================*/
/* Gibt ncol Anzahl Spalten  zurück                            */
/*==========================================================================*/
size_t          astrm_ncol(astrm_s *pm) {
    if(  pm == NULL )
        return 0;
    return pm->ncol;
}

/*==========================================================================*/
/* Gibt Länge des strings pm->pp[nrow*icol+irow]->pBuffer zurück                            */
/*==========================================================================*/
size_tastrm_string(astrm_s *pm,size_t irow,size_t icol){
    if(  pm == NULL  
      || irow >= pm->nrow 
      || icol >= pm->ncol)
        return 0;
    return strlen(pm->pp[pm->nrow*icol+irow]->pBuffer);
}
/*=================================================================*/
/* Kopiere Pointer Vektorstruct in  MAtrixstruktur. */
/*=================================================================*/
void astrm_cpy_vpointer(astrm_s *pm,astrv_s *pv){

    if( pv != NULL ) {
        pm->pp    = pv->pp;
        pm->nrow  = pv->nrow;
        pm->ncol  = 1;
    } else {
        pm = NULL;
    }
}
