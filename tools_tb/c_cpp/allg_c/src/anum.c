/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      25.02.03   TBert Aufteilung in amem und anum               */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             anum.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            numeric.c
*************************************************************************
* Kurzbeschreibung: 
*
* numerische Funktionen: siehe anum.h
************************************************************************/
/************************************************************************

************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "definer.h"
#include "amem.h"
#include "anum.h"

#ifndef PI 
 #define PI 3.141593f
#endif

/*======================================================================*/
float anum_float_abs(float val)
{
  return (float)fabs((double)val) ;
}
/*======================================================================*/
int anum_fvekmerge(float **pziel,size_t *pmem_ziel,size_t *pn_ziel,
                  float *quelle1,size_t nquelle1,float *quelle2,size_t nquelle2,
                  int icase,float delta)
{
  if( (anum_fvekcpy(pziel,pmem_ziel,quelle1,nquelle1)) < 0 )
    return -1;
  *pn_ziel = nquelle1;
  if( anum_fvekcat(pziel,pmem_ziel,pn_ziel,quelle2,nquelle2) < 0 )
    return -1;
  anum_fveksort_delta(pziel,pmem_ziel,pn_ziel,icase,delta);
  
  return 0;
}
/*======================================================================*/
int anum_fvekcpy(float **pziel,size_t *pmem_ziel,float *quelle,size_t nquelle)
{
  float *vek;
  size_t i;
  
  if(nquelle < 0)return -1;
  
  if( amem_fmem(pziel,pmem_ziel,nquelle) < 0 )
    return -1;
  
  vek = *pziel;
  
  for(i=0;i<nquelle;i++)
    vek[i]=quelle[i];
    
  return 0;
}
/*======================================================================*/
int anum_fvekcat(float **pziel,size_t *pmem_ziel,size_t *nziel,float *quelle, size_t nquelle)
{
  size_t i;
  float *vek;
    
  if( amem_fmem(pziel,pmem_ziel,*nziel+nquelle) < 0 )
    return -1;
    
  vek = *pziel;
  
  for(i=0;i<nquelle;i++)
     vek[*nziel+i]=quelle[i];
     
  *nziel += nquelle;
  return 0;  
}
/*======================================================================*/
void anum_fveksort_delta(float **pvek,size_t *pmem_vek,size_t *n,int icase,float delta)
{
  /* icase = 0 kein Sortieren, kein Wert elininieren
           = 1 aufsteigend Sortieren, kein Wert el.
           = 2 absteigend Sortieren, kein Wert el.
           = 3 kein Sortieren, Werte mit Abstand <delta eliminieren
           = 4 aufsteigend Sortieren, Werte mit Abstand <delta eliminieren
           = 5 absteigend Sortieren, Werte mit Abstand <delta eliminieren
  */
  int i=icase;
  size_t is,js;
  float *vek=*pvek;

  if( i >= 3)i-=3;
  i %= 10;
  
  anum_fveksort(pvek,*n,i);
  
  if( icase > 3 )
  {
    if(delta < 0.0f)delta *=-1.0f;
    for(is=1;is<*n;is++)
    {
      if( (float)fabs((double)(vek[is]-vek[is-1])) < delta )
      {
        *n -= 1;
        for(js=is;js<*n;js++)
          vek[js]=vek[js+1];
      }
    }
  }
  amem_fmem_red(pvek,pmem_vek,*n);
  return;
}
/*===========================================================*/
void anum_fveksort(float **pvek,size_t n,int icase)
{
/* 
icase   = 0 kein Sortieren, kein Wert elinminieren
        = 1 aufsteigend Sortieren
        = 2 absteigend Sortieren
        = 3 Umgekehr sortieren

  icase + 10 Zweiter Vektor bearbeiten nach der Vorschrift für den ersten 
  icase + 20 Zweiter und Dritter Vektoren bearbeiten nach der Vorschrift für den ersten 
    */
    
    int sortflag=1;
    size_t is,nvek=1;
    float *vek0=*pvek,*vek1=*pvek,*vek2=*pvek;
    
    if( icase <= 0 )return;
    
    if( icase >= 10 ){vek1 = *(++pvek);nvek++;}
    if( icase >= 20 ){vek2 = *(++pvek);nvek++;}
    
    icase %= 10;
    
    switch( icase ) {
    case 0:
        break;
    case 1:
    case 2:
        
        while(sortflag == 1)
        {
            sortflag=0;
            for(is=1;is<n;is++)
            {
                if( ( icase == 1 && vek0[is] < vek0[is-1] )
                    || ( icase == 2 && vek0[is] > vek0[is-1] )
                    )
                {
                    sortflag = 1;
                    anum_ftausche(&vek0[is-1],&vek0[is]);
                    if( nvek >= 2 )
                        anum_ftausche(&vek1[is-1],&vek1[is]);
                    if( nvek >= 3 )
                        anum_ftausche(&vek2[is-1],&vek2[is]);
                    
                }
            }
        }
        break;
    case 3:
        {
            float fdum;
            for(is=0;is<n/2;is++) {
                fdum         = vek0[is];
                vek0[is]     = vek0[n-1-is];
                vek0[n-1-is] = fdum;
                if( nvek >= 2 ) {
                    fdum         = vek1[is];
                    vek1[is]     = vek1[n-1-is];
                    vek1[n-1-is] = fdum;
                }
                if( nvek >= 3 ) {
                    fdum         = vek2[is];
                    vek2[is]     = vek2[n-1-is];
                    vek2[n-1-is] = fdum;
                }
            }
        }
    
        break;
        
    }
    return; 
}
/*===========================================================*/
void anum_ucveksort(unsigned char **pvek,size_t n,int icase)
{
/* 
icase   = 0 kein Sortieren, kein Wert elinminieren
        = 1 aufsteigend Sortieren
        = 2 absteigend Sortieren
        = 3 Umgekehr sortieren

  icase + 10 Zweiter Vektor bearbeiten nach der Vorschrift für den ersten 
  icase + 20 Zweiter und Dritter Vektoren bearbeiten nach der Vorschrift für den ersten 
    */
    
    int sortflag=1;
    size_t is,nvek=1;
    unsigned char *vek0=*pvek,*vek1=*pvek,*vek2=*pvek;
    
    if( icase <= 0 )return;
    
    if( icase >= 10 ){vek1 = *(++pvek);nvek++;}
    if( icase >= 20 ){vek2 = *(++pvek);nvek++;}
    
    icase %= 10;
    
    switch( icase ) {
    case 0:
        break;
    case 1:
    case 2:
        
        while(sortflag == 1)
        {
            sortflag=0;
            for(is=1;is<n;is++)
            {
                if(  ( icase == 1 && vek0[is] < vek0[is-1] )
                  || ( icase == 2 && vek0[is] > vek0[is-1] )
                  )
                {
                    sortflag = 1;
                    anum_uctausche(&vek0[is-1],&vek0[is]);
                    if( nvek >= 2 )
                        anum_uctausche(&vek1[is-1],&vek1[is]);
                    if( nvek >= 3 )
                        anum_uctausche(&vek2[is-1],&vek2[is]);
                    
                }
            }
        }
        break;
    case 3:
        {
            unsigned char ucdum;
            for(is=0;is<n/2;is++) {
                ucdum        = vek0[is];
                vek0[is]     = vek0[n-1-is];
                vek0[n-1-is] = ucdum;
                if( nvek >= 2 ) {
                    ucdum        = vek1[is];
                    vek1[is]     = vek1[n-1-is];
                    vek1[n-1-is] = ucdum;
                }
                if( nvek >= 3 ) {
                    ucdum        = vek2[is];
                    vek2[is]     = vek2[n-1-is];
                    vek2[n-1-is] = ucdum;
                }
            }
        }
    
        break;
        
    }
    return; 
}
/*===========================================================*/
int anum_fint1dim(int iflag,float *x,float *y,size_t nxy,float x0,float *y0,
             int istart,int iord,int iabl)
/*******************************************************************
*
*       iflag           Inittialisierung 0/1
*       x[nxy]          x-Vektor, muss monoton steigend sein
*       y[nxy]          y-Vektor
*       x0              x-Wert an dem y-Wert bestimmt wird
*       y0              y-Wert, der zurÏckgegeben wird
*       istart          aktuelle Stuetzstelle der letzten Berechnung
*                       wird als Ausgangswert benutzt, um schneller
*                       den Wertebereich in x zu finden (z.B. wenn x0
*                       fortlaufend steigt.
*       iord            = 0     Konstantwert swischen den Stuetzstellen
*                       = 1     lineare Interpolation
*
*       iabl            = 0,1   i.Ableitung
*
*       return i0       >0 aktuelle Stuetzstelle
*       return -1       Fehler, x-Vektor nicht monoton steigend (nur bei init)
*       return -2       Fehler, iord > 1 kubischer Spline nicht vorhanden (nur bei init)
********************************************************************/
#define SELFEPSILON 1.e-30
{
  float dx=0.0f,fak=0.0f;
  size_t is,i0=(size_t)istart;

  if( x == NULL )return -3;
  if( y == NULL )return -3;
  
  if(iflag == 1) /* Initialisierung */
  {
    for(is=0;is<=nxy-2;is++)
    {
      if( (x[is+1]-x[is]) < (float)SELFEPSILON )
        return -1;
    }
    if(iord > 1)
      return -2;
  }
  
  /* Berechnung */
  
  if( i0 > nxy-1 || i0 < 0 ) i0 = 0;
  
  if( x0 >= x[nxy-1] )
  {
     i0 = nxy-1;
     dx  = x0 - x[nxy-1];
     fak = (y[nxy-1]-y[nxy-2])/(x[nxy-1]-x[nxy-2]);
  }
  else if( x0 >= x[i0] )
  {
    for(is=i0;is<=nxy-2;is++)
      if( x0 < x[is+1] )
      {
        i0 = is;
        dx  = x0-x[is];
        fak = (y[is+1]-y[is])/(x[is+1]-x[is]);
        break;
      }  
        
  }
  else if( x0 < x[0] )
  {
    i0 = 0;
    dx  = x0 - x[0];
    fak = (y[1]-y[0])/(x[1]-x[0]);
  }
  else
  {
    for(is=i0-1;is>=0;is--)
      if( x0 >= x[is] )
      {
        i0 = is;
        dx  = x0 - x[is];
        fak = (y[is+1]-y[is])/(x[is+1]-x[is]);
        break;
      }
  }
  
  if( iord == 1 )
  {
    if( iabl == 0 )
      *y0 = y[i0]+fak*dx;
    else
      *y0 = fak;
  }
  else
  {
    if( iabl == 0 )
      *y0 = y[i0];
    else
      *y0 = 0.0f;
  }
   
   return (size_t)i0;
}
/*===========================================================*/
int anum_dint1dim(int iflag,double *x,double *y,size_t nxy,double x0,double *y0,
             int istart,int iord,int iabl)
/*******************************************************************
*
*       iflag           Inittialisierung 0/1
*       x[nxy]          x-Vektor, muss monoton steigend sein
*       y[nxy]          y-Vektor
*       x0              x-Wert an dem y-Wert bestimmt wird
*       y0              y-Wert, der zurÏckgegeben wird
*       istart          aktuelle Stuetzstelle der letzten Berechnung
*                       wird als Ausgangswert benutzt, um schneller
*                       den Wertebereich in x zu finden (z.B. wenn x0
*                       fortlaufend steigt.
*       iord            = 0     Konstantwert swischen den Stuetzstellen
*                       = 1     lineare Interpolation
*
*       iabl            = 0,1   i.Ableitung
*
*       return i0       >0 aktuelle Stuetzstelle
*       return -1       Fehler, x-Vektor nicht monoton steigend (nur bei init)
*       return -2       Fehler, iord > 1 kubischer Spline nicht vorhanden (nur bei init)
********************************************************************/
{
  double dx=0.0,fak=0.0;
  size_t   i,i0=(size_t)istart;
  
  if( x == NULL )return -3;
  if( y == NULL )return -3;

  if( nxy == 0 ) {
      return -4;
  } else if( nxy == 1 ) {

      if( iabl )
          *y0 = 0.0;
      else
          *y0 = y[0];

      return 0;

  } else {

      if(iflag == 1) /* Initialisierung */
      {
        for(i=0;i<=nxy-2;i++)
        {
          if( (x[i+1]-x[i]) < (double)SELFEPSILON )
            return -1;
        }
        if(iord > 1)
          return -2;
      }
  
      /* Berechnung */
  
      if( i0 > nxy-1 || i0 < 0 ) i0 = 0;
  
      if( x0 >= x[nxy-1] )
      {
         i0 = nxy-1;
         dx  = x0 - x[nxy-1];
         fak = (y[nxy-1]-y[nxy-2])/(x[nxy-1]-x[nxy-2]);
      }
      else if( x0 >= x[i0] )
      {
        for(i=i0;i<=nxy-2;i++)
          if( x0 < x[i+1] )
          {
            i0 = i;
            dx  = x0-x[i];
            fak = (y[i+1]-y[i])/(x[i+1]-x[i]);
            break;
          }  
        
      }
      else if( x0 < x[0] )
      {
        i0 = 0;
        dx  = x0 - x[0];
        fak = (y[1]-y[0])/(x[1]-x[0]);
      }
      else
      {
        for(i=i0-1;i>=0;i--)
          if( x0 >= x[i] )
          {
            i0 = i;
            dx  = x0 - x[i];
            fak = (y[i+1]-y[i])/(x[i+1]-x[i]);
            break;
          }
      }
  
      if( iord == 1 )
      {
        if( iabl == 0 )
          *y0 = y[i0]+fak*dx;
        else
          *y0 = fak;
      }
      else
      {
        if( iabl == 0 )
          *y0 = y[i0];
        else
          *y0 = 0.0f;
      }
   
       return (size_t)i0;
  }
}
/*==============================================================*/
void anum_search_index_3D(size_t *piLeft, size_t *piRght,
                          double u,double *pData,size_t nu)
{
  /* Find the location of current input value in the data table. */
  *piLeft = 0;
  *piRght = nu-1;

  if ( u <= pData[0] )
  {
    /* Less than or equal to the smallest point in the table. */
    *piRght = 0;
  }
  else if ( u >= pData[nu-1] )
  {
    /* Greater than or equal to the largest point in the table. */
    *piLeft = nu-1;
  }
  else
  {
    size_t i;

    /* Do a binary search. */
    while ( ( *piRght - *piLeft ) > 1 )
    {
      /* Get the average of the left and right indices using to Floor rounding. */
      i = (*piLeft + *piRght) >> 1;

      /* Move either the right index or the left index so that */
      /*  LeftDataPoint <= CurrentValue < RightDataPoint */
      if ( u < pData[i] )
      {
        *piRght = i;
      }
      else
      {
        *piLeft = i;
      }
    }
  }
}
/*==============================================================*/
int anum_dint2dim(double *xvec,size_t nx,double *yvec,size_t ny,
                  double *zmat,double x0,double y0,double *z0,
                  char init)
/*******************************************************************
*
*       Interpolation z(nrow,ncol)=f(x(nrow),y(ncol))
*       Der Endwert wird begrenzt
*       xvec[nx]        x-Vektor, monoton steigend
*       yvec[ny]        y-Vektor, monoton steigend
*       zmat[ny][nx]    z-Matrix_t
*       x0              x-Wert an dem z-Wert bestimmt wird
*       y0              y-Wert an dem z-Wert bestimmt wird
*       z0              z-Wert, der zurÏckgegeben wird
*       init            = 1 Überprüft nur Vektorlänge und Änderung von Punkt zu Punkt
*       return 0        okay
*       return 1        Fehler zuwenig Werte
*       return 2        Fehler nicht monoton steigend
********************************************************************/
{
    size_t i;
    size_t irow_index1,irow_index2,icol_index1,icol_index2;
    double ztemp1,ztemp2;
    double colLambda;
    double rowLambda;

    if( init ) {

        if( nx < 1 || ny < 1 )
            return 1;
        for(i=1;i<nx;i++) {

            if( xvec[i]-xvec[i-1] <= (double)SELFEPSILON )
                return 2;
        }
        for(i=1;i<ny;i++) {

            if( yvec[i]-yvec[i-1] <= (double)SELFEPSILON )
                return 3;
        }
        return 0;
    }

    anum_search_index_3D(&irow_index1,&irow_index2,x0,xvec,nx);
    anum_search_index_3D(&icol_index1,&icol_index2,y0,yvec,ny);

    if ( yvec[icol_index1] != yvec[icol_index2] )
    {
      double num;
      double den;

      den = yvec[icol_index2];
      den -= yvec[icol_index1];
      num = y0;
      num -= yvec[icol_index1];
      {
        colLambda = (num/den);
      }
    }
    else
    {
      colLambda = 0;
    }
    if ( xvec[irow_index1] != xvec[irow_index2] )
    {
      double num;
      double den;

      den = xvec[irow_index2];
      den -= xvec[irow_index1];
      num = x0;
      num -= xvec[irow_index1];
      {
        rowLambda = (num/den);
      }
    }
    else
    {
      rowLambda = 0;
    }
    /* Interpolate along column variable
     *    with the row variable locked on the left row
     */
    {
      double yLeft;
      double yRght;
      yLeft = zmat[irow_index1 + nx*icol_index1];
      yRght = zmat[irow_index1 + nx*icol_index2];
      yLeft += colLambda * ( yRght - yLeft );
      ztemp1 = yLeft;
    }
    /* Interpolate along column variable
     *    with the row variable locked on the right row
     */
    {
      double yLeft;
      double yRght;
      yLeft = zmat[irow_index2 + nx*icol_index1];
      yRght = zmat[irow_index2 + nx*icol_index2];
      yLeft += colLambda * ( yRght - yLeft );
      ztemp2 = yLeft;
    }
    /*
     * Interpolate along row variable
     *    with the col variable locked on its interpolated value
     */
    {
      double yLeft;
      double yRght;
      yLeft = ztemp1;
      yRght = ztemp2;
      yLeft += rowLambda * ( yRght - yLeft );
      *z0 = yLeft;
    }
  
    

    return 0;
  
}
/*===========================================================*/
float anum_fatan2(float y,float x)
{
    double dum;
    if( x < 1.e-30 && x > -1.e-30 )
    {
      if( y < 1.e-30 && y > -1.e-30 )
        return 0.0f;
      else if(y > 0.)
        return((float)(PI/(double)2.));
      else
        return(-(float)(PI/(double)2.));
    }
    else if( x > 0. )
    {
      dum = (double)y/(double)x;
      return (float)atan(dum);
/*      return(dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
    }
    else
    {
      dum = (double)y/(double)x;
      if( y >= 0. )
      {
        return (float)PI+(float)atan(dum);
/*        return(PI+dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
      }
      else
      {
        return -(float)PI+(float)atan(dum);
/*        return(-PI+dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
      }
    }
}
/*===========================================================*/
void anum_ftausche(float *val0,float *val1)
{
    float val=*val0;
    *val0=*val1;
    *val1=val;
    return;
}
/*===========================================================*/
void anum_uctausche(unsigned char *val0,unsigned char *val1)
{
    unsigned char val=*val0;
    *val0=*val1;
    *val1=val;
    return;
}
/*===========================================================*/
float anum_fbetrag(float x,float y,float z)
{
  return (float)sqrt( pow((double)x,(double)2)
                     +pow((double)y,(double)2)
                     +pow((double)z,(double)2));
}
/*===========================================================*/
float anum_dbetrag(double x,double y,double z)
{
  return (float)sqrt( pow(x,(double)2)
                +pow(y,(double)2)
                +pow(z,(double)2));
}

