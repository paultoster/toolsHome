#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "SlfTab.h"
#include "SlfNum.h"
#include "amsg.h"


// 1D-Tabelle
//===========
status_t  Slf1DTabSet(SSlf1DTab *p1dtab
                     ,char *name, char *comment
                     ,char *xname, char *xunit, char * xcomment
                     ,double *pxval, uint32 nxrows
                     ,char *yname, char *yunit, char * ycomment
                     ,double *pyval, uint32 nyrows
                     ,uint8 islinear) {

    p1dtab->pName     = 0;
    p1dtab->pComment  = 0;
    p1dtab->pXVec     = 0;
    p1dtab->pYVec     = 0;
    p1dtab->Index     = 0;
    p1dtab->IsLinear  = islinear;
    p1dtab->IsChecked = 0;

    // NAme
    if( name ) {
	    p1dtab->pName = (char *)malloc(sizeof(char)*(strlen(name)+1));
      if( !p1dtab->pName ) 
      {
        amsg_set_error("Problem Slf1DTabSet: malloc Name !!!");     
        return NOT_OKAY;
      }
      strncpy(p1dtab->pName,name,strlen(name));
    }

    // Comment
    if( comment ) {
	    p1dtab->pName = (char *)malloc(sizeof(char)*(strlen(comment)+1));
      if( !p1dtab->pName ) {
        amsg_set_error("Problem Slf1DTabSet: malloc Comment !!!"); 
        return NOT_OKAY;
      }
      strncpy(p1dtab->pName,comment,strlen(comment));

    }

    // X-Vektor
    //=========
    p1dtab->pXVec = (SSlfVec *)malloc(sizeof(SSlfVec));

    if( SlfVecSet(p1dtab->pXVec
                 ,xname, xunit, xcomment
                 ,pxval, nxrows) != 0 )
                 return NOT_OKAY;

    // Y-Vektor
    //=========
    p1dtab->pYVec = (SSlfVec *)malloc(sizeof(SSlfVec));

    if( SlfVecSet(p1dtab->pYVec
                 ,yname, yunit, ycomment
                 ,pyval, nyrows) != 0 )
                 return NOT_OKAY;
    				
    
    return OK;
}
status_t Slf1DTabCheck(SSlf1DTab *pd1tab) {

    double Test;
    uint32 i;

    //--------------------------------
    // x-vektor gleiche Länge y-Vektor
    //--------------------------------
    if( GET_NROWS(pd1tab->pXVec) != GET_NROWS(pd1tab->pYVec) ) {

        amsg_set_error("Problem Slf1DTabCheck:Length 1dtable <%s>",pd1tab->pName);
        amsg_set_error(" is different nxrows:<%i>",GET_NROWS(pd1tab->pXVec));
        amsg_set_error(" != nynrows: <%s>\n",GET_NROWS(pd1tab->pYVec));
        
        return NOT_OKAY;
    }
    // Interpolation testen
    if( GET_NROWS(pd1tab->pXVec) > 1 ) {
        if( SlfNumInterp1D(1
                          ,pd1tab->pXVec->pVec
                          ,pd1tab->pYVec->pVec
                          ,GET_NROWS(pd1tab->pXVec)
                          ,0.0
                          ,&Test
                          ,&i,pd1tab->IsLinear,0) != OKAY )
        {

          amsg_set_error("Problem Slf1DTabCheck:Length 1dtable <%s> Interpolation xVector is not moton increasing\n",pd1tab->pName); 
          return NOT_OKAY;
        }
    }

    pd1tab->IsChecked = 1;

    return OK;
}
status_t Slf1DTabGetVal(SSlf1DTab *pd1tab,double xval,double *pyval) {

    if( pd1tab->IsChecked && (Slf1DTabCheck(pd1tab) != OK) )
        return NOT_OKAY;

    SlfNumInterp1D(0
                  ,pd1tab->pXVec->pVec
                  ,pd1tab->pYVec->pVec
                  ,GET_NROWS(pd1tab->pXVec)
                  ,xval,pyval
                  ,&pd1tab->Index
                  ,pd1tab->IsLinear
                  ,0);
    return OKAY;
}
void  Slf1DTabFree(SSlf1DTab *pd1tab) {

    if( pd1tab->pName ) 
        free( pd1tab->pName );
    pd1tab->pName = 0;

    if( pd1tab->pComment ) 
        free( pd1tab->pComment );
    pd1tab->pComment = 0;

    if( pd1tab->pXVec ) {
        SlfVecFree(pd1tab->pXVec);
        free(pd1tab->pXVec);
        pd1tab->pXVec = 0;
    }
    if( pd1tab->pYVec ) {
        SlfVecFree(pd1tab->pYVec);
        free(pd1tab->pYVec);
        pd1tab->pYVec = 0;
    }

    pd1tab->IsChecked = 0;

}
status_t  Slf1DTabGetSpzVal(SSlf1DTab *pd1tab,char *pspz,double *value) {

  if( !pd1tab->IsChecked && (Slf1DTabCheck(pd1tab) != OK) )
        return NOT_OK;

    if( strcmp(pspz,"xstart") == 0 || strcmp(pspz,"xanf") == 0 ) 
    {
      *value = pd1tab->pXVec->pVec[0];
    } 
    else if( strcmp(pspz,"xend") == 0 ) 
    {
      *value = pd1tab->pXVec->pVec[GET_NROWS(pd1tab->pXVec)-1];
    } 
    else if( strcmp(pspz,"xmin") == 0 ) 
    {
        UINT32_T i;
        *value = pd1tab->pXVec->pVec[0];
        for(i=1;i<GET_NROWS(pd1tab->pXVec);i++)
            *value = MIN(*value,pd1tab->pXVec->pVec[i]);
    } 
    else if( strcmp(pspz,"xmax") == 0 ) 
    {

        UINT32_T i;
        *value = pd1tab->pXVec->pVec[0];
        for(i=1;i<GET_NROWS(pd1tab->pXVec);i++)
            *value = MAX(*value,pd1tab->pXVec->pVec[i]);
    } 
    else if( strcmp(pspz,"ystart") == 0 || strcmp(pspz,"yanf") == 0 ) 
    {
        *value = pd1tab->pYVec->pVec[0];
    } 
    else if( strcmp(pspz,"yend") == 0 ) 
    {
        *value = pd1tab->pYVec->pVec[GET_NROWS(pd1tab->pYVec)-1];
    } 
    else if( strcmp(pspz,"ymin") == 0 ) 
    {
        UINT32_T i;
        *value = pd1tab->pYVec->pVec[0];
        for(i=1;i<GET_NROWS(pd1tab->pYVec);i++)
            *value = MIN(*value,pd1tab->pYVec->pVec[i]);

    } 
    else if( strcmp(pspz,"ymax") == 0 ) 
    {

        UINT32_T i;
        *value = pd1tab->pYVec->pVec[0];
        for(i=1;i<GET_NROWS(pd1tab->pYVec);i++)
            *value = MAX(*value,pd1tab->pYVec->pVec[i]);
    } else {
        return NOT_OK;
     }
    return OK;
}
status_t  Slf1DTabSetInd(SSlf1DTab *ptab,uint32 index) 
{

  ptab->Index = index;

  return OK;
};
// Vektor
//===========
status_t  SlfVecSet(SSlfVec *pvec
                   ,char *name, char *unit, char * comment
                   ,double *pval, uint32 nrows) {

    pvec->pName    = 0;
    pvec->pComment = 0;
    pvec->pUnit    = 0;
	  pvec->pVec     = 0;

    // NAme
    if( name ) {
	    pvec->pName = (char *)malloc(sizeof(char)*(strlen(name)+1));
      if( !pvec->pName ) {
        amsg_set_error("Problem SlfVecSet: malloc Name !!!");
        return NOT_OKAY;
      }
      strncpy(pvec->pName,name,strlen(name));
    }

    // Comment
    if( comment ) {
	    pvec->pComment = (char *)malloc(sizeof(char)*(strlen(comment)+1));
        if( !pvec->pComment ) {
          amsg_set_error("Problem SlfVecSet: malloc Comment !!!");   
          return NOT_OKAY;
        }
        strncpy(pvec->pComment,comment,strlen(comment));

    }

    // Unit
    if( unit ) {
	    pvec->pUnit = (char *)malloc(sizeof(char)*(strlen(unit)+1));
      if( !pvec->pUnit ) {
        amsg_set_error("Problem SlfVecSet: malloc unit !!!");
        return NOT_OKAY;
      }
      strncpy(pvec->pUnit,unit,strlen(unit));
    }

    /* Vektor */
	  pvec->pVec = NewVector(nrows);
    if( !pvec->pVec ) 
    {
      amsg_set_error("Problem SlfVecSet: NewVector( pVec ) <%s>\n",pvec->pName);
      return NOT_OKAY;
    }
	  memcpy(pvec->pVec, pval, sizeof(double)*nrows);

    return OKAY;
}
status_t SlfVecGet(SSlfVec *pvec,double dindex,double *pyval)
{
    UINT32_T index = (UINT32_T)dindex;

    if( index < GET_NROWS(pvec) )
        *pyval = pvec->pVec[index];
    else
        *pyval = pvec->pVec[GET_NROWS(pvec)-1];

    return OK;
}
void  SlfVecFree(SSlfVec *pvec) {

    if( pvec->pName ) 
        free( pvec->pName );
    pvec->pName = 0;

    if( pvec->pUnit ) 
        free( pvec->pUnit );
    pvec->pUnit = 0;

    if( pvec->pComment ) 
        free( pvec->pComment );
    pvec->pComment = 0;

    if( pvec->pVec ) 
        FreeVector( pvec->pVec );
    pvec->pVec = 0;
}
// Matrix
//===========
status_t  SlfMatSet(SSlfMat *pmat
                   ,char *name, char *unit, char * comment
                   ,double *pval, uint32 nrows, uint32 ncols) 
{

    pmat->pName    = 0;
    pmat->pComment = 0;
    pmat->pUnit    = 0;
	  pmat->pMat     = 0;

    // NAme
    if( name ) {
	    pmat->pName = (char *)malloc(sizeof(char)*(strlen(name)+1));
      if( !pmat->pName ) {
        amsg_set_error("Problem SlfVecSet: malloc Name !!!");
          
        return NOT_OKAY;
      }
      strncpy(pmat->pName,name,strlen(name));
    }

    // Comment
    if( comment ) {
	    pmat->pComment = (char *)malloc(sizeof(char)*(strlen(comment)+1));
      if( !pmat->pComment ) {
        amsg_set_error("Problem SlfVecSet: malloc Comment !!!");   
        return NOT_OKAY;
      }
      strncpy(pmat->pComment,comment,strlen(comment));

    }

    // Unit
    if( unit ) {
	    pmat->pUnit = (char *)malloc(sizeof(char)*(strlen(unit)+1));
      if( !pmat->pUnit ) {
        amsg_set_error("Problem SlfVecSet: malloc unit !!!");
        return NOT_OKAY;
      }
      strncpy(pmat->pUnit,unit,strlen(unit));
    }

    /* Matrixx */
    pmat->pMat = MatrixCopyD(pval,nrows,ncols,0);
    if( !pmat->pMat ) {
      amsg_set_error("Problem SlfMatSet: malloc pMat <%s>\n",pmat->pName);
      return NOT_OKAY;
    }

    return OKAY;
}
void  SlfMatFree(SSlfMat *pmat) {

    if( pmat->pName ) 
        free( pmat->pName );
    pmat->pName = 0;

    if( pmat->pUnit ) 
        free( pmat->pUnit );
    pmat->pUnit = 0;

    if( pmat->pComment ) 
        free( pmat->pComment );
    pmat->pComment = 0;

    if( pmat->pMat ) 
    {
      FreeMatrix(pmat->pMat);
    }
    pmat->pMat = 0;
}
// 2D-Tabelle
//===========
status_t  Slf2DTabSet(SSlf2DTab *ptab,char *name, char *comment
                     ,char *xname, char *xunit, char *xcomment, double *pxval, uint32 nrows_x
                     ,char *yname, char *yunit, char *ycomment, double *pyval, uint32 nrows_y
                     ,char *zname, char *zunit, char *zcomment, double *pzmat, uint32 nrows_z, uint32 ncols_z)

{

  if( ptab->IsProofed )
      Slf2DTabFree(ptab);

	/* Tabellenname */
	ptab->pName = (char *)malloc(sizeof(char)*(strlen(name)+1));
	if( ptab->pName == NULL ) {
		amsg_set_error("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",name);
		return NOT_OK;
	}
	strcpy(ptab->pName,name);

  /* Tabellenkommentar */
  ptab->pComment= (char *)malloc(sizeof(char)*(strlen(comment)+1));
	if( ptab->pComment == NULL ) {
		amsg_set_error("\n\nParameter <%s>: Fehler malloc für Parameterübergabe",comment);
		return NOT_OK;
	}
	strcpy(ptab->pComment,comment);

  /* X-Vektor */
  if( SlfVecSet(ptab->pXVec,xname,xunit,xcomment,pxval,nrows_x) != OK )
  {
    return NOT_OK;
  }
  /* Y-Vektor */
  if( SlfVecSet(ptab->pYVec,yname,yunit,ycomment,pyval,nrows_y) != OK )
  {
    return NOT_OK;
  }
  /* Z-Matrix */
  if( SlfMatSet(ptab->pZMat,zname,zunit,zcomment,pzmat,nrows_x,nrows_y) != OK )
  {
    return NOT_OK;
  }
  

    return OK;
}
status_t Slf2DTabCheck(SSlf2DTab *ptab) 
{

    double Test;
    int    i;

    //--------------------------------
    // x-vektor gleiche Länge z-Matrix
    //--------------------------------
    if( GET_NROWS(ptab->pXVec) != GET_NROWS(ptab->pZMat) ) {
      amsg_set_error("Slf1DTabCheck_error: Interpolation mit Tabelle <%s>\n"
                     "x-Vektor <%s>(nrows=%i) und z-Matrix <%s>(nrows=%i) "
                     "nicht gleich lang"
                     ,ptab->pName,ptab->pXVec->pName,GET_NROWS(ptab->pXVec)
                     ,ptab->pZMat->pName,GET_NROWS(ptab->pZMat));
      return NOT_OK;
    }
    //--------------------------------
    // y-vektor gleiche Länge z-Matrix
    //--------------------------------
    if( GET_NROWS(ptab->pXVec) != GET_NCOLS(ptab->pZMat) ) {
        amsg_set_error("Slf1DTabCheck_error: Interpolation mit Tabelle <%s>\n"
                       "y-Vektor <%s>(nrows=%i) und z-Matrix <%s>(ncols=%i) "
                       "nicht gleich lang"
                      ,ptab->pName,ptab->pYVec->pName,GET_NROWS(ptab->pYVec)
                      ,ptab->pZMat->pName,GET_NCOLS(ptab->pZMat));
        return NOT_OK;
    }
    // Interpolation testen
    if( GET_NROWS(ptab->pXVec) > 1 || GET_NROWS(ptab->pYVec) > 1 ) {

        if( (i=SlfNumInterp2D(1
                             ,ptab->pXVec->pVec
                             ,GET_NROWS(ptab->pXVec)
                             ,ptab->pXVec->pVec
                             ,GET_NROWS(ptab->pXVec)
                             ,ptab->pZMat->pMat
                             ,0.0
                             ,0.0
                             ,&Test)) != 0 ) {
            amsg_set_error("Slf1DTabCheck_error: Problem Interpolation %s\n"
                           "in anum_dint2dim during init"
                          ,ptab->pName);
            amsg_set_error("\n x-vector <%s> or y-vector <%s> is not monoton increasing",ptab->pXVec->pName,ptab->pYVec->pName);
            return NOT_OK;
        }
    }

    ptab->IsProofed = 1;

    return OK;
}

status_t Slf2DTabGetVal(SSlf2DTab *ptab,double xval,double yval,double *pzval) {

    if( !ptab->IsProofed && (Slf2DTabCheck(ptab) != OK) )
    {
        return NOT_OK;
    }
    SlfNumInterp2D(0
                  ,ptab->pXVec->pVec
                  ,GET_NROWS(ptab->pXVec)
                  ,ptab->pYVec->pVec
                  ,GET_NROWS(ptab->pYVec)
                  ,ptab->pZMat->pMat
                  ,xval,yval
                  ,pzval);
    return OK;
}

status_t  Slf2DTabFree(SSlf2DTab *ptab) {

    if( ptab->pName != NULL ) 
        free( ptab->pName );
    ptab->pName = NULL;

    if( ptab->pComment != NULL ) 
        free( ptab->pComment );
    ptab->pComment = NULL;

    SlfVecFree(ptab->pXVec);
    SlfVecFree(ptab->pYVec);
    SlfMatFree(ptab->pZMat);

    ptab->IsProofed = 0;

    return OK;
}
status_t  Slf2DTabGetSpzVal(SSlf2DTab *ptab,char *pspz,double *pval) {

    UINT32_T i;
    if( strcmp(pspz,"xmean") == 0 || strcmp(pspz,"XMEAN") == 0 ) 
    {
      *pval = 0.0;

      for(i=0;i<GET_NROWS(ptab->pXVec);i++)
      {
        *pval += ptab->pXVec->pVec[i];
      }
      if( GET_NROWS(ptab->pXVec) )
      {
          *pval /= (double)GET_NROWS(ptab->pXVec);
      }
    }
    else if( strcmp(pspz,"ymean") == 0 || strcmp(pspz,"YMEAN") == 0 ) 
    {
      *pval = 0.0;

      for(i=0;i<GET_NROWS(ptab->pYVec);i++)
      {
        *pval += ptab->pYVec->pVec[i];
      }
      if( GET_NROWS(ptab->pYVec) )
      {
          *pval /= (double)GET_NROWS(ptab->pYVec);
      }
    }
    else
    {
      return NOT_OK;
    }
    return OK;
}
