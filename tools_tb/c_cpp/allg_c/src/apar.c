/* Aenderungen 
001 02.08.07 TBert      Neu
Ver Datum*** Wer        Was
*/
/*************************************************************************
* File:             apar.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
* Version:          1.0
* Datum:            16.11.04
*************************************************************************
* Kurzbeschreibung: 
*
* Bearbeitet Variablen für Parameterberechnung
************************************************************************/

#include <stdio.h>
#include "apar.h"
#include "anum.h"
#include "amsg.h"

/*===========================================
  Parameter benutzen
============================================*/

// Vektor
//========
STATUS_T par_get_val_vec(par_vec_s *pvec,double dindex,double *pyval) {

    UINT32_T index = (UINT32_T)dindex;

    if( index < pvec->nrows )
        *pyval = pvec->ptr[index];
    else
        *pyval = pvec->ptr[pvec->nrows-1];

    return OK;
}
STATUS_T  par_free_vec(par_vec_s *pvec) {

    if( pvec->name != NULL ) 
        free( pvec->name );
    pvec->name = NULL;

    if( pvec->ptr != NULL ) 
        free( pvec->ptr );
    pvec->ptr = NULL;

    return OK;
}
// Tabelle
//========
STATUS_T  par_set_table(par_tab_s *ptab,char *name
                       ,char *xname,double *p_xvec,UINT32_T nrows_x
                       ,char *yname,double *p_yvec,UINT32_T nrows_y
                       ,char islinear) {

    if( ptab->isproofed )
        par_free_table(ptab);

    ptab->index     = 0;
    ptab->islinear  = islinear;
    ptab->isproofed = 0;

	/* Tabellenname */
	ptab->name = (char *)malloc(sizeof(char)*(strlen(name)+1));
	if( ptab->name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->name, strlen(name) + 1, name);
#else
  strcpy(ptab->name, name);
#endif


    /* X-Vektor Name */
	ptab->xvec.name = (char *)malloc(sizeof(char)*(strlen(xname)+1));
	if( ptab->xvec.name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->xvec.name, strlen(xname) + 1, xname);
#else
  strcpy(ptab->xvec.name, xname);
#endif
					
    /* X-Vektor */
    ptab->xvec.nrows = nrows_x;

	ptab->xvec.ptr = (double *)malloc(sizeof(double)*nrows_x);
	if( ptab->xvec.ptr == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc (double)für Parameterübergabe vector",name);
		return NOT_OK;
	}
	memcpy(ptab->xvec.ptr, p_xvec, sizeof(double)*nrows_x);
    
    /* Y-Vektor Name */
	ptab->yvec.name = (char *)malloc(sizeof(char)*(strlen(yname)+1));
	if( ptab->yvec.name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->yvec.name, strlen(yname) + 1, yname);
#else
  strcpy(ptab->yvec.name, yname);
#endif

    /* Y-Vektor */
    ptab->yvec.nrows = nrows_y;

	ptab->yvec.ptr = (double *)malloc(sizeof(double)*nrows_y);
	if( ptab->yvec.ptr == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc (double)für Parameterübergabe vector",name);
		return NOT_OK;
	}
	memcpy(ptab->yvec.ptr, p_yvec, sizeof(double)*nrows_y);

    return OK;
}
STATUS_T par_check_table(par_tab_s *ptab) {

    double Test;
    int    i;

    //--------------------------------
    // x-vektor gleiche Länge y-Vektor
    //--------------------------------
    if( ptab->xvec.nrows != ptab->yvec.nrows ) {
        amsg_set_error("mex_check_par_table_error: Interpolation mit Tabelle <%s>\n"
                       "x-Vektor <%s>(nrows=%i) und y-Vektor <%s>(nrows=%i) "
                       "nicht gleich lang"
                      ,ptab->name,ptab->xvec.name,ptab->xvec.nrows
                      ,ptab->yvec.name,ptab->yvec.nrows);
        return NOT_OK;
    }
    // Interpolation testen
    if( ptab->xvec.nrows > 1 ) {
        if( (i=anum_dint1dim(1
                            ,ptab->xvec.ptr
                            ,ptab->yvec.ptr
                            ,ptab->xvec.nrows
                            ,0.0
                            ,&Test
                            ,0,ptab->islinear,0)) < 0 ) {
            amsg_set_error("mex_check_par_table_error: Problem Interpolation %s\n"
                           "in anum_dint2dim during init"
                          ,ptab->name);
            if( i == -1 )
                amsg_set_error("\n x-vector <%s> not monoton increasing",ptab->xvec.name);
            return NOT_OK;
        }
    }

    ptab->isproofed = 1;

    return OK;
}
STATUS_T par_get_val_table(par_tab_s *ptab,double xval,double *pyval) {

    if( !ptab->isproofed && (par_check_table(ptab) != OK) )
        return NOT_OK;


    ptab->index = anum_dint1dim(0
                               ,ptab->xvec.ptr
                               ,ptab->yvec.ptr
                               ,ptab->xvec.nrows
                               ,xval,pyval
                               ,ptab->index
                               ,ptab->islinear
                               ,0);
    return OK;
}

STATUS_T  par_set_index_table(par_tab_s *ptab,UINT32_T index) {

    ptab->index = index;

    return OK;
};
STATUS_T  par_get_spzval_table(par_tab_s *ptab,char *pspz,double *value) {

    if( !ptab->isproofed && (par_check_table(ptab) != OK) )
        return NOT_OK;

    if( strcmp(pspz,"xstart") == 0 || strcmp(pspz,"xanf") == 0 ) {

        *value = ptab->xvec.ptr[0];
    } else if( strcmp(pspz,"xend") == 0 ) {

        *value = ptab->xvec.ptr[ptab->xvec.nrows-1];
    } else if( strcmp(pspz,"xmin") == 0 ) {

        UINT32_T i;
        *value = ptab->xvec.ptr[0];
        for(i=1;i<ptab->xvec.nrows;i++)
            *value = MIN(*value,ptab->xvec.ptr[i]);
    } else if( strcmp(pspz,"xmax") == 0 ) {

        UINT32_T i;
        *value = ptab->xvec.ptr[0];
        for(i=1;i<ptab->xvec.nrows;i++)
            *value = MAX(*value,ptab->xvec.ptr[i]);
    } else if( strcmp(pspz,"ystart") == 0 || strcmp(pspz,"yanf") == 0 ) {

        *value = ptab->yvec.ptr[0];
    } else if( strcmp(pspz,"yend") == 0 ) {

        *value = ptab->yvec.ptr[ptab->yvec.nrows-1];
    } else if( strcmp(pspz,"ymin") == 0 ) {

        UINT32_T i;
        *value = ptab->yvec.ptr[0];
        for(i=1;i<ptab->yvec.nrows;i++)
            *value = MIN(*value,ptab->yvec.ptr[i]);
    } else if( strcmp(pspz,"ymax") == 0 ) {

        UINT32_T i;
        *value = ptab->yvec.ptr[0];
        for(i=1;i<ptab->yvec.nrows;i++)
            *value = MAX(*value,ptab->yvec.ptr[i]);
    } else {
        amsg_set_error("par_get_spzval_table_error Parameter pspz <%s> must be xstart,xend,xmin,xmax,ystart,yend,ymin or ymax",pspz);
        return NOT_OK;
     }
    return OK;
}
STATUS_T  par_free_table(par_tab_s *ptab) {

    if( ptab->name != NULL ) 
        free( ptab->name );
    ptab->name = NULL;

    if( ptab->xvec.name != NULL ) 
        free( ptab->xvec.name );
    ptab->xvec.name = NULL;

    if( ptab->xvec.ptr != NULL ) 
        free( ptab->xvec.ptr );
    ptab->xvec.ptr = NULL;

    if( ptab->yvec.name != NULL ) 
        free( ptab->yvec.name );
    ptab->yvec.name = NULL;

    if( ptab->yvec.ptr != NULL ) 
        free( ptab->yvec.ptr );
    ptab->yvec.ptr = NULL;

    ptab->isproofed = 0;

    return OK;
}
// 2D-Tabelle
//===========
STATUS_T  par_set_2d_table(par_2d_tab_s *ptab,char *name
                          ,char *xname,double *p_xvec,UINT32_T nrows_x
                          ,char *yname,double *p_yvec,UINT32_T nrows_y
                          ,char *zname,double *p_zmat,UINT32_T nrows_z
                          ,UINT32_T ncols_z) {

    if( ptab->isproofed )
        par_free_2d_table(ptab);

    ptab->isproofed = 0;

	/* Tabellenname */
	ptab->name = (char *)malloc(sizeof(char)*(strlen(name)+1));
	if( ptab->name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->name, strlen(name) + 1, name);
#else
  strcpy(ptab->name, name);
#endif


    /* X-Vektor Name */
	ptab->xvec.name = (char *)malloc(sizeof(char)*(strlen(xname)+1));
	if( ptab->xvec.name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}

#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->xvec.name, strlen(xname) + 1, xname);
#else
  strcpy(ptab->xvec.name, xname);
#endif
					
    /* X-Vektor */
    ptab->xvec.nrows = nrows_x;

	ptab->xvec.ptr = (double *)malloc(sizeof(double)*nrows_x);
	if( ptab->xvec.ptr == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc (double)für Parameterübergabe vector",name);
		return NOT_OK;
	}
	memcpy(ptab->xvec.ptr, p_xvec, sizeof(double)*nrows_x);
    
    /* Y-Vektor Name */
	ptab->yvec.name = (char *)malloc(sizeof(char)*(strlen(yname)+1));
	if( ptab->yvec.name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->yvec.name, strlen(yname) + 1, yname);
#else
  strcpy(ptab->yvec.name, yname);
#endif

    /* Y-Vektor */
    ptab->yvec.nrows = nrows_y;

	ptab->yvec.ptr = (double *)malloc(sizeof(double)*nrows_y);
	if( ptab->yvec.ptr == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc (double)für Parameterübergabe vector",name);
		return NOT_OK;
	}
	memcpy(ptab->yvec.ptr, p_yvec, sizeof(double)*nrows_y);

    /* Z-Matrix_t Name */
	ptab->zmat.name = (char *)malloc(sizeof(char)*(strlen(zname)+1));
	if( ptab->zmat.name == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
#if  (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  strcpy_s(ptab->zmat.name, strlen(zname) + 1, zname);
#else
  strcpy(ptab->zmat.name, zname);
#endif

    /* Z-Matrix_t */
    ptab->zmat.nrows = nrows_z;
    ptab->zmat.ncols = ncols_z;

	ptab->zmat.ptr = (double *)malloc(sizeof(double)*nrows_z*ncols_z);
	if( ptab->zmat.ptr == NULL ) {
		amsg_set_status("\n\nParameter <%s>: Fehler malloc (double)für Parameterübergabe vector",name);
		return NOT_OK;
	}
	memcpy(ptab->zmat.ptr, p_zmat, sizeof(double)*nrows_z*ncols_z);

    return OK;
}
STATUS_T par_check_2d_table(par_2d_tab_s *ptab) {

    double Test;
    int    i;

    //--------------------------------
    // x-vektor gleiche Länge z-Matrix_t
    //--------------------------------
    if( ptab->xvec.nrows != ptab->zmat.nrows ) {
        amsg_set_error("mex_check_par_2d_table_error: Interpolation mit Tabelle <%s>\n"
                       "x-Vektor <%s>(nrows=%i) und z-Matrix_t <%s>(nrows=%i) "
                       "nicht gleich lang"
                      ,ptab->name,ptab->xvec.name,ptab->xvec.nrows
                      ,ptab->zmat.name,ptab->zmat.nrows);
        return NOT_OK;
    }
    //--------------------------------
    // y-vektor gleiche Länge z-Matrix_t
    //--------------------------------
    if( ptab->yvec.nrows != ptab->zmat.ncols ) {
        amsg_set_error("mex_check_par_2d_table_error: Interpolation mit Tabelle <%s>\n"
                       "y-Vektor <%s>(nrows=%i) und z-Matrix_t <%s>(ncols=%i) "
                       "nicht gleich lang"
                      ,ptab->name,ptab->yvec.name,ptab->yvec.nrows
                      ,ptab->zmat.name,ptab->zmat.ncols);
        return NOT_OK;
    }
    // Interpolation testen
    if( ptab->xvec.nrows > 1 || ptab->yvec.nrows > 1 ) {

        if( (i=anum_dint2dim(ptab->xvec.ptr
                            ,ptab->xvec.nrows
                            ,ptab->yvec.ptr
                            ,ptab->yvec.nrows
                            ,ptab->zmat.ptr
                            ,0.0
                            ,0.0
                            ,&Test
                            ,1)) != 0 ) {
            amsg_set_error("mex_check_par_table_error: Problem Interpolation %s\n"
                           "in anum_dint2dim during init"
                          ,ptab->name);
            amsg_set_error("\n x-vector <%s> or y-vector <%s> is not monoton increasing",ptab->xvec.name,ptab->yvec.name);
            return NOT_OK;
        }
    }

    ptab->isproofed = 1;

    return OK;
}

STATUS_T par_get_val_2d_table(par_2d_tab_s *ptab,double xval,double yval,double *pzval) {

    if( !ptab->isproofed && (par_check_2d_table(ptab) != OK) )
        return NOT_OK;

    anum_dint2dim(ptab->xvec.ptr
                 ,ptab->xvec.nrows
                 ,ptab->yvec.ptr
                 ,ptab->yvec.nrows
                 ,ptab->zmat.ptr
                 ,xval,yval
                 ,pzval
                 ,0);
    return OK;
}

STATUS_T  par_free_2d_table(par_2d_tab_s *ptab) {

    if( ptab->name != NULL ) 
        free( ptab->name );
    ptab->name = NULL;

    if( ptab->xvec.name != NULL ) 
        free( ptab->xvec.name );
    ptab->xvec.name = NULL;

    if( ptab->xvec.ptr != NULL ) 
        free( ptab->xvec.ptr );
    ptab->xvec.ptr = NULL;

    if( ptab->yvec.name != NULL ) 
        free( ptab->yvec.name );
    ptab->yvec.name = NULL;

    if( ptab->yvec.ptr != NULL ) 
        free( ptab->yvec.ptr );
    ptab->yvec.ptr = NULL;

    if( ptab->zmat.name != NULL ) 
        free( ptab->zmat.name );
    ptab->zmat.name = NULL;

    if( ptab->zmat.ptr != NULL ) 
        free( ptab->zmat.ptr );
    ptab->zmat.ptr = NULL;

    ptab->isproofed = 0;

    return OK;
}
STATUS_T  par_get_xmean_2d_table(par_2d_tab_s *ptab,double *pxmean) {

    UINT32_T i;
    *pxmean = 0.0;

    for(i=0;i<ptab->xvec.nrows;i++)
        *pxmean += ptab->xvec.ptr[i];
    if( ptab->xvec.nrows )
        *pxmean /= (double)ptab->xvec.nrows;

    return OK;
}
STATUS_T  par_get_ymean_2d_table(par_2d_tab_s *ptab,double *pymean) {

    UINT32_T i;
    *pymean = 0.0;

    for(i=0;i<ptab->yvec.nrows;i++)
        *pymean += ptab->yvec.ptr[i];
    if( ptab->yvec.nrows )
        *pymean /= (double)ptab->yvec.nrows;

    return OK;
}
// Matrix_t
//========
STATUS_T  par_free_mat(par_mat_s *pmat) {

    if( pmat->name != NULL ) 
        free( pmat->name );
    pmat->name = NULL;

    if( pmat->ptr != NULL ) 
        free( pmat->ptr );
    pmat->ptr = NULL;

    return OK;
}
