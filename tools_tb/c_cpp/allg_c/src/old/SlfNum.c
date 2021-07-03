#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "Numeric.h"
#include "string.h"


static char     SlfNumErrMess[BUFSIZ];
static status_t SlfNumStatus = 0;
char *pSlfNumErrMess = SlfNumErrMess;

/*===========================================================*/
status_t SlfNumInterp1D(uint8_t iflag
                       ,double *x   
                       ,double *y
                       ,uint32_t nxy
                       ,double x0
                       ,double *y0
                       ,uint32_t *piact
                       ,uint8_t  iord
                       ,uint8_t  iabl)
{
  double dx=0.0,fak=0.0;
  uint32_t i,i0=*piact;
  
  if( nxy == 1 ) {

      if( iabl )
          *y0 = 0.0;
      else
          *y0 = y[0];

      *piact =0;
      return OKAY;

  } else {

#ifdef SLS_NUM_PROOF_FUNCTION
      if(iflag == 1) /* Initialisierung */
      {
        for(i=0;i<nxy-1;i++)
        {
            if( (x[i+1]-x[i]) < (double)EPSILON ) {

#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(SlfNumErrMess,BUFSIZ,"Error SlfNumInterp1D: x-vector is not monoton increasing !!!");
#else
                sprintf(SlfNumErrMess,"Error SlfNumInterp1D: x-vector is not monoton increasing !!!");
#endif
                SlfNumStatus = NOT_OKAY;
                return NOT_OKAY;
            }
        }
      }
#endif  
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
        for(i=i0-1;i>=0 && i<i0;i--)
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
   
      *piact = i0;
      return OKAY;
  }
}
/*==============================================================*/
void SlfNumInterp2D_search_index(size_t *piLeft, size_t *piRght,
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
int SlfNumInterp2D(uint8_t iflag,double *xvec,size_t nx,double *yvec,size_t ny,
                  Matrix_t zmat,double x0,double y0,double *z0)
/*******************************************************************
*
*       Interpolation z(nrow,ncol)=f(x(nrow),y(ncol))
*       Der Endwert wird begrenzt
*       iflag           Inittialisierung 0/1
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
*       return 3        Länge zmat ist kleiner als xvec oder yvec
********************************************************************/
{
    size_t i;
    size_t index_x1,index_x2,index_y1,index_y2;
    double ztemp1,ztemp2;
    double colLambda;
    double rowLambda;
    double ztemp;
    double num;
    double den;

    if( iflag ) {

        if( nx < 1 || ny < 1 )
            return 1;
        if( GET_NROWS(zmat) < ny || GET_NCOLS(zmat) < nx)
            return 3;
        for(i=1;i<nx;i++) {

            if( xvec[i]-xvec[i-1] <= (double)EPSILON )
                return 2;
        }
        for(i=1;i<ny;i++) {

            if( yvec[i]-yvec[i-1] <= (double)EPSILON )
                return 2;
        }
        return 0;
    }

    SlfNumInterp2D_search_index(&index_x1,&index_x2,x0,xvec,nx);
    SlfNumInterp2D_search_index(&index_y1,&index_y2,y0,yvec,ny);

    if ( yvec[index_y1] != yvec[index_y2] )
    {
      den = yvec[index_y2];
      den -= yvec[index_y1];
      num = y0;
      num -= yvec[index_y1];
      {
        colLambda = (num/den);
      }
    }
    else
    {
      colLambda = 0;
    }
    if ( xvec[index_x1] != xvec[index_x2] )
    {

      den = xvec[index_x2];
      den -= xvec[index_x1];
      num = x0;
      num -= xvec[index_x1];
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
      ztemp1 = zmat[index_y1][index_x1];
      ztemp  = zmat[index_y2][index_x1];
      ztemp1 += colLambda * ( ztemp - ztemp1 );
    /* Interpolate along column variable
     *    with the row variable locked on the right row
     */
      ztemp2 = zmat[index_y1][index_x2];
      ztemp  = zmat[index_y2][index_x2];
      ztemp2 += colLambda * ( ztemp - ztemp2 );
    /*
     * Interpolate along row variable
     *    with the col variable locked on its interpolated value
     */
      ztemp1 += rowLambda * ( ztemp2 - ztemp1 );
      *z0 = ztemp1;
  
    

    return 0;
  
}
/*==============================================================*/
void SlfNumStepSet(SSlfNumStep *s,double xa,double xb,double ya,double yb) 
{

    s->xa = MIN(xa,xb);
    s->xb = MAX(xa,xb);
    s->dx = MAX(EPSILON,ABS(xb-xa));
    s->ya = ya;
    s->yb = yb;
    s->dy = yb-ya;
}
void SlfNumStepCalc(SSlfNumStep *s,double x) 
{

    double xx = (x-s->xa)/s->dx;

    s->x  = x;

    if( x <= s->xa ) {

        s->y  = s->ya;
        s->yp = 0.0;
    } else if( x >= s->xb ) {

        s->y  = s->yb;
        s->yp = 0.0;
    } else {
        s->y  = 3. - 2. * xx;
        s->y  = s->ya + s->dy * xx * xx * s->y;

        s->yp = 1. - xx;
        s->yp = 6. * s->dy / s->dx * xx * s->yp;
    }
}
/*===========================================================*/
double SlfNumAbs(double x)
{
  if( x >= 0.0 ) return x;
  else           return x*(-1.0);
}
/*===========================================================*/
sint8_t SlfNumSgn(double x)
{
  if( x >= 0.0 ) return 1;
  else           return -1;
}
/*****************************************************************************
  @fn                   SlfNumLim1(double *pVal, double *pLimUp, double *pLimLow)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      LimUp   upper border
  @param[in]      LimLow  lower border
   
  @return         limited Value between -Lim and Lim
*****************************************************************************/
double SlfNumLim(double val, double limUp, double limLow)
{

  if( val > limUp )
  {
    val = limUp;
  }
  else if( val < limLow )
  {
    val = limLow;
  }
  return val;
}
/*****************************************************************************
  @fn              double SlfNumLim1(double Val, double Lim)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      Lim     upper border, lower border => *(-1.f)
   
  @return         limited Value between -Lim and Lim
*****************************************************************************/

double SlfNumLim1(double val, double lim)
{
  double lim0 = lim * (-1.f);

  if( val > lim )
  {
    val = lim;
  }
  else if( val < lim0 )
  {
    val = lim0;
  }
  return val;
}
/*===========================================================*/
double SlfNumATan2(double y,double x)
{
    double dum;
    if( x < 1.e-30 && x > -1.e-30 )
    {
      if( y < 1.e-30 && y > -1.e-30 )
        return 0.0f;
      else if(y > 0.)
        return((PI/(double)2.));
      else
        return(-(PI/(double)2.));
    }
    else if( x > 0. )
    {
      dum = y/x;
      return atan(dum);
/*      return(dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
    }
    else
    {
      dum = y/x;
      if( y >= 0. )
      {
        return PI+atan(dum);
/*        return(PI+dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
      }
      else
      {
        return -PI+atan(dum);
/*        return(-PI+dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
      }
    }
}
//===============================================
//  m = sqrt(a^2+b^2)
//===============================================
double SlfNumSqrt2(double a, double b) 
{
    return sqrt(a*a+b*b);
}
/***********************************************************************

matrix.c - simple matrix operations

Copyright (C) 1991 Dean Rubine

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License. See ../COPYING for
the full agreement.
**********************************************************************/

//=================================================================
//=================================================================
// Build/Destroy Vector_t/Matrix_t
//=================================================================
//=================================================================
Vector_t NewVector(uarray r)
{
	register struct array_header *a;
	register Vector_t v;

	a = (struct array_header *)
	    SlfNumAllocate(sizeof(struct array_header) + r * sizeof(double), char);

	a->ndims = 1;
	a->nrows = r;
	a->ncols = 1;
  a->link  = 0;
	v = (Vector_t) (a + 1);

#ifdef SLS_NUM_PROOF_FUNCTION

	if(HEADER(v) != (struct array_header *) a ||
	   GET_NDIMS(v) != 1 || GET_NROWS(v) != r || GET_NCOLS(v) != 1) {
#if _MSC_VER > MSC_VER_BIS_VS2005
        
        sprintf_s(SlfNumErrMess,BUFSIZ,"NewRowVector error: v=%p  D:%d,%d  R:%d,%d  C:%d,%d\n", (void *)v,    GET_NDIMS(v), 1,  GET_NROWS(v), r, GET_NCOLS(v), 1);
#else
	    	sprintf(SlfNumErrMess,"NewRowVector error: v=%x H: %x,%x  D:%d,%d  R:%d,%d  C:%d,%d\n", v,  HEADER(v), a,  GET_NDIMS(v), 1,  GET_NROWS(v), r, GET_NCOLS(v), 1);
#endif
        SlfNumStatus = NOT_OKAY;
        return NULL;
	    }
#endif

	return v;
}
Matrix_t NewMatrix(uarray r,uarray c)
{
	register struct array_header *a = (struct array_header *)
	   SlfNumAllocate(sizeof(struct array_header) + r * sizeof(double *), char);
	register uarray i;
	register Matrix_t m;

	m = (Matrix_t) (a + 1);
	for(i = 0; i < r; i++)
		m[i] = SlfNumAllocate(c, double);
	a->ndims = 2;
	a->nrows = r;
	a->ncols = c;
    a->link  = 0;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(HEADER(m) != (struct array_header *) a ||
	   GET_NDIMS(m) != 2 || GET_NROWS(m) != r || GET_NCOLS(m) != c) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"NewCollVector error: v=%p D:%d,%d  R:%d,%d  C:%d,%d\n", (void *)m,    GET_NDIMS(m), 2,  GET_NROWS(m), r, GET_NCOLS(m), c);
#else
	  sprintf(SlfNumErrMess,"NewCollVector error: v=%x H: %x,%x  D:%d,%d  R:%d,%d  C:%d,%d\n", m,  HEADER(m), a,  GET_NDIMS(m), 2,  GET_NROWS(m), r, GET_NCOLS(m), c);
#endif
    SlfNumStatus = NOT_OKAY;
    return NULL;
	}
#endif

  return m;
}
//=====================================================
// Es wird bestehender Vektor, der als Matrix_t benutzt wird
// auf die Matrix_t-Struktur gelinkt, es wird nur noch Speicher 
// double ** allokiert
//=====================================================
Matrix_t  LinkMatrix(double *pVec,uarray r,uarray c)
{
	
    register struct array_header *a = (struct array_header *)
	   SlfNumAllocate(sizeof(struct array_header) + r * sizeof(double *), char);
	register uarray i;
	register Matrix_t m;

	m = (Matrix_t) (a + 1);
	for(i = 0; i < r; i++)
		m[i] = pVec+i*c;

	a->ndims = 2;
	a->nrows = r;
	a->ncols = c;
    a->link  = 1; // Zeigt an, dass pVec auf m gelinkt ist

#ifdef SLS_NUM_PROOF_FUNCTION
	if(HEADER(m) != (struct array_header *) a ||
	   GET_NDIMS(m) != 2 || GET_NROWS(m) != r || GET_NCOLS(m) != c) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"NewCollVector error: v=%p D:%d,%d  R:%d,%d  C:%d,%d\n", (void *)m,  GET_NDIMS(m), 2,  GET_NROWS(m), r, GET_NCOLS(m), c);
#else
	  sprintf(SlfNumErrMess,"NewCollVector error: v=%x H: %x,%x  D:%d,%d  R:%d,%d  C:%d,%d\n", m,  HEADER(m), a,  GET_NDIMS(m), 2,  GET_NROWS(m), r, GET_NCOLS(m), c);
#endif
    SlfNumStatus = NOT_OKAY;
    return NULL;
	}
#endif

    return m;
}

Vector_t VectorCopyD(double *pdval,uarray r)
{
	register struct array_header *a;
	register Vector_t v;

	a = (struct array_header *)
	    SlfNumAllocate(sizeof(struct array_header) + r * sizeof(double), char);

	a->ndims = 1;
	a->nrows = r;
	a->ncols = 1;
    a->link  = 0;
	v = (Vector_t) (a + 1);

	memcpy(v,pdval,sizeof(double)*r);

#ifdef SLS_NUM_PROOF_FUNCTION

	if(HEADER(v) != (struct array_header *) a ||
	   GET_NDIMS(v) != 1 || GET_NROWS(v) != r || GET_NCOLS(v) != 1) {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"NewRowVector error: v=%p  D:%d,%d  R:%d,%d  C:%d,%d\n", (void *)v,  GET_NDIMS(v), 1,  GET_NROWS(v), r, GET_NCOLS(v), 1);
#else
    sprintf(SlfNumErrMess,"NewRowVector error: v=%x H: %x,%x  D:%d,%d  R:%d,%d  C:%d,%d\n", v,  HEADER(v), a,  GET_NDIMS(v), 1,  GET_NROWS(v), r, GET_NCOLS(v), 1);
#endif
            SlfNumStatus = NOT_OKAY;
            return NULL;
	    }
#endif

	return v;
}
Matrix_t MatrixCopyD(double *pdval,uarray r,uarray c,uint8_t type)
{
	register struct array_header *a = (struct array_header *)
	   SlfNumAllocate(sizeof(struct array_header) + r * sizeof(double *), char);
	register uarray i,j;
	register Matrix_t m;

	m = (Matrix_t) (a + 1);
	for(i = 0; i < r; i++)
		m[i] = SlfNumAllocate(c, double);
	a->ndims = 2;
	a->nrows = r;
	a->ncols = c;
    a->link  = 0;

	if( type == DOUBLE_MATRIX_ROW_BEFORE_COL ) {

		for(i = 0; i < r; i++)
			for(j = 0; j < c; j++)
				m[i][j] = pdval[i+j*r];
	} else {
		for(i = 0; i < r; i++)
			for(j = 0; j < c; j++)
				m[i][j] = pdval[i*c+j];
	}
#ifdef SLS_NUM_PROOF_FUNCTION
	if(HEADER(m) != (struct array_header *) a ||
	   GET_NDIMS(m) != 2 || GET_NROWS(m) != r || GET_NCOLS(m) != c) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"NewCollVector error: v=%p  D:%d,%d  R:%d,%d  C:%d,%d\n", (void *)m,  GET_NDIMS(m), 2,  GET_NROWS(m), r, GET_NCOLS(m), c);
#else
    sprintf(SlfNumErrMess,"NewCollVector error: v=%x H: %x,%x  D:%d,%d  R:%d,%d  C:%d,%d\n", m,  HEADER(m), a,  GET_NDIMS(m), 2,  GET_NROWS(m), r, GET_NCOLS(m), c);
#endif   
    SlfNumStatus = NOT_OKAY;
    return NULL;
	}
#endif

    return m;
}
void FreeVector(Vector_t v)
{
  if( v )
  {
	  free(HEADER(v));
  }
  v = 0;
}
void FreeMatrix(Matrix_t m)
{
	register uarray i;

  if( m ) {
	  if( !ISLINKED(m) ) {
      for(i = 0; i < GET_NROWS(m); i++) {
		      free(m[i]);
          m[i] = 0;
      }
    }
  	free(HEADER(m));
    m = 0;
  }
}

Vector_t VectorCopy(register Vector_t v)
{
	register Vector_t r = NewVector(GET_NROWS(v));
	register uarray i;

	for(i = 0; i < GET_NROWS(v); i++)
		r[i] = v[i];
	return r;
}
Matrix_t MatrixCopy(register Matrix_t m)
{
	register Matrix_t r=0;
	register uarray i, j;

  if( m )
  {
    r = NewMatrix(GET_NROWS(m), GET_NCOLS(m));
	  for(i = 0; i < GET_NROWS(m); i++)
		  for(j = 0; j < GET_NCOLS(m); j++)
			  r[i][j] = m[i][j];
  }
	return r;
}

//=================================================================
//=================================================================
// Vector_t/Matrix_t Calculaton
//=================================================================
//=================================================================
void ZeroVector(Vector_t v)
{
	register uarray i;
	for(i = 0; i < GET_NROWS(v); i++) v[i] = 0.0;
}


void ZeroMatrix(Matrix_t m)
{
	register uarray i, j;
	for(i = 0; i < GET_NROWS(m); i++)
		for(j = 0; j < GET_NCOLS(m); j++)
			m[i][j] = 0.0;
}

void FillMatrix(Matrix_t m,double fill)
{
	register uarray i, j;
	for(i = 0; i < GET_NROWS(m); i++)
		for(j = 0; j < GET_NCOLS(m); j++)
			m[i][j] = fill;
}
double InnerProduct(register Vector_t v1,register Vector_t  v2)
{
	double result = 0;
	uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v1) != GET_NROWS(v2)) {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"InnerProduct %d x %d ", GET_NROWS(v1), GET_NROWS(v2));
#else
		sprintf(SlfNumErrMess,"InnerProduct %d x %d ", GET_NROWS(v1), GET_NROWS(v2));
#endif
        SlfNumStatus = NOT_OKAY;
        return 0.0;
	    }
#endif
	
    for(i = 0; i < GET_NROWS(v1); i++)
        result += v1[i] * v2[i];
	return result;
}
/*
Compute result = m1*m2 where
	m1 is a matrix (r1 x c1)
	m2 is a matrix (r2 x c2)
	result is a matrix (r1 x c2)
=================================================================================
=================================================================================
*/
void MatrixMultiply(register Matrix_t m1,register Matrix_t  m2,register Matrix_t  result)
{
	register uarray i, j, k;
	double sum;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NCOLS(m1) != GET_NROWS(m2)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixMultiply: Can't multiply %dx%d and %dx%d matrices",
			GET_NROWS(m1), GET_NCOLS(m1), GET_NROWS(m2), GET_NCOLS(m2));
#else
	  sprintf(SlfNumErrMess,"MatrixMultiply: Can't multiply %dx%d and %dx%d matrices",
			GET_NROWS(m1), GET_NCOLS(m1), GET_NROWS(m2), GET_NCOLS(m2));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

	if(GET_NROWS(result) != GET_NROWS(m1) || GET_NCOLS(result) != GET_NCOLS(m2)) {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixMultiply: %dx%d times %dx%d does not give %dx%d product",
			GET_NROWS(m1), GET_NCOLS(m1), GET_NROWS(m2), GET_NCOLS(m2),
			GET_NROWS(result), GET_NCOLS(result));
#else
		sprintf(SlfNumErrMess,"MatrixMultiply: %dx%d times %dx%d does not give %dx%d product",
			GET_NROWS(m1), GET_NCOLS(m1), GET_NROWS(m2), GET_NCOLS(m2),
			GET_NROWS(result), GET_NCOLS(result));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif
	for(i = 0; i < GET_NROWS(m1); i++)
		for(j = 0; j < GET_NCOLS(m2); j++) {
			sum = 0;
			for(k = 0; k < GET_NCOLS(m1); k++)
				sum += m1[i][k] * m2[k][j];
			result[i][j] = sum;
		}
}
/*
Compute vres = m * v where
	v is a row vector (rv x 1) 
	m is a matrix (r x c)
	vres is a row vector (r x 1) 
*/
void MatrixTimesVector(Matrix_t m,Vector_t v,Vector_t vres)
{
	register uarray i, j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NCOLS(m)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixTimesVector: %dx%d mat time %d vector does not fit",
			GET_NROWS(m), GET_NCOLS(m), GET_NROWS(v));
#else
		sprintf(SlfNumErrMess,"MatrixTimesVector: %dx%d mat time %d vector does not fit",
			GET_NROWS(m), GET_NCOLS(m), GET_NROWS(v));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

	if(GET_NROWS(vres) != GET_NROWS(m)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixTimesVector: %d res-vector = %dx%d mat time %d vector does not fit",
			GET_NROWS(vres),GET_NROWS(m), GET_NCOLS(m), GET_NROWS(v));
#else
		sprintf(SlfNumErrMess,"MatrixTimesVector: %d res-vector = %dx%d mat time %d vector does not fit",
			GET_NROWS(vres),GET_NROWS(m), GET_NCOLS(m), GET_NROWS(v));
#endif
        SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif
	for(i = 0; i < GET_NROWS(m); i++) {
		vres[i] = 0;
		for(j = 0; j < GET_NCOLS(m); j++)
			vres[i] += v[j] * m[i][j];
	}
}	
/*
Compute vres' = v' * m where
	v is a row vector (r x 1) (v' column vector)
	m is a matrix (r x c)
	vres' is a column vector (c x 1) (vres row vector)
*/
void VectorTimesMatrix(Vector_t v,Matrix_t m,Vector_t vres)
{
	register uarray i, j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NROWS(m)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorTimesMatrix: Can't multiply %d vector by %dx%d",
			GET_NROWS(v), GET_NROWS(m), GET_NCOLS(m));
#else
		sprintf(SlfNumErrMess,"VectorTimesMatrix: Can't multiply %d vector by %dx%d",
			GET_NROWS(v), GET_NROWS(m), GET_NCOLS(m));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

	if(GET_NROWS(vres) != GET_NCOLS(m)) {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorTimesMatrix: %d vector times %dx%d mat does not fit in %d product" ,
			GET_NROWS(v), GET_NROWS(m), GET_NCOLS(m), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"VectorTimesMatrix: %d vector times %dx%d mat does not fit in %d product" ,
			GET_NROWS(v), GET_NROWS(m), GET_NCOLS(m), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif
	for(j = 0; j < GET_NCOLS(m); j++) {
		vres[j] = 0;
		for(i = 0; i < GET_NROWS(m); i++)
			vres[j] += v[i] * m[i][j];
	}
}	
void ScalarTimesVector(double s,Vector_t v,Vector_t vres)
{
	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NROWS(vres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"ScalarTimesVector: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"ScalarTimesVector: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif
	for(i = 0; i < GET_NROWS(v); i++)
        vres[i] = s * v[i];
}
void VectorGet(Vector_t v,Vector_t vres)
{
	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NROWS(vres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"ScalarTimesVector: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"ScalarTimesVector: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif
	for(i = 0; i < GET_NROWS(v); i++)
        vres[i] = v[i];
}

void ScalarTimesMatrix(double s,Matrix_t m,Matrix_t mres)
{
	register uarray i, j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(m) != GET_NROWS(mres)  || 
	   GET_NCOLS(m) != GET_NCOLS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"ScalarTimesMatrix: result wrong size (%d!=%d)or(%d!=%d)",
			GET_NROWS(m), GET_NROWS(mres),
			GET_NCOLS(m), GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"ScalarTimesMatrix: result wrong size (%d!=%d)or(%d!=%d)",
			GET_NROWS(m), GET_NROWS(mres),
			GET_NCOLS(m), GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif

	for(i = 0; i < GET_NROWS(m); i++)
		for(j = 0; j < GET_NCOLS(m); j++)
			mres[i][j] = s * m[i][j];
}
/*
 Compute v'mv
 */

double QuadraticForm(register Vector_t v,register Matrix_t  m)
{
	register uarray i, j, n;
	double result = 0;

	n = GET_NROWS(v);

#ifdef SLS_NUM_PROOF_FUNCTION
	if(n != GET_NROWS(m) || n != GET_NCOLS(m)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"QuadraticForm: bad matrix size (%dx%d not %dx%d)",
			GET_NROWS(m), GET_NCOLS(m), n, n);
#else
		sprintf(SlfNumErrMess,"QuadraticForm: bad matrix size (%dx%d not %dx%d)",
			GET_NROWS(m), GET_NCOLS(m), n, n);
#endif
    SlfNumStatus = NOT_OKAY;
    return 0;
	}
#endif
	for(i = 0; i < n; i++)
		for(j = 0; j < n; j++) {

#ifdef PIQ_DEBUG
			printf("%g*%g*%g [%g] %s ",
			m[i][j],v[i],v[j],
			m[i][j] * v[i] * v[j],
			i==n-1&&j==n-1? "=" : "+");
#endif

			result += m[i][j] * v[i] * v[j];
		}
	return result;
}
/* Matrix_t inversion using full pivoting.
 * The standard Gauss-Jordan method is used.
 * The return value is the determinant.
 * The input matrix may be the same as the result matrix
 *
 *	det = InvertMatrix(inputmatrix, resultmatrix);
 *
 * HISTORY
 * 26-Feb-82  David Smith (drs) at Carnegie-Mellon University
 *	Written.
 * Sun Mar 20 19:36:16 EST 1988 - converted to this form by Dean Rubine
 *
 */
int	DebugInvertMatrix = 0;

#define PERMBUFSIZE 200	/* Max mat size */

double InvertMatrix(Matrix_t ym,Matrix_t rm)
{
	register uarray i, j, k;
	double det, biga, recip_biga, hold;
	iarray l[PERMBUFSIZE], m[PERMBUFSIZE];
	register uarray n;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(ym) != GET_NCOLS(ym)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"InvertMatrix: not square");
#else
		sprintf(SlfNumErrMess,"InvertMatrix: not square");
#endif
    SlfNumStatus = NOT_OKAY;
    return 0;
	}
#endif

	n = GET_NROWS(ym);

#ifdef SLS_NUM_PROOF_FUNCTION
	if(n != GET_NROWS(rm) || n != GET_NCOLS(rm)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"InvertMatrix: result wrong size");
#else
		sprintf(SlfNumErrMess,"InvertMatrix: result wrong size");
#endif
    SlfNumStatus = NOT_OKAY;
    return 0;
	}
#endif

	/* Copy ym to rm */
	
	if(ym != rm)
		for(i = 0; i < n; i++)
			for(j = 0; j < n; j++)
				rm[i][j] = ym[i][j];

	/*if(DebugInvertMatrix) PrintMatrix(rm, "Inverting (det=%g)\n", det);*/

    /* Allocate permutation vectors for l and m, with the same origin
       as the matrix. */

#ifdef SLS_NUM_PROOF_FUNCTION
	if (n >= PERMBUFSIZE) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"InvertMatrix: PERMBUFSIZE");
#else
		sprintf(SlfNumErrMess,"InvertMatrix: PERMBUFSIZE");
#endif
    SlfNumStatus = NOT_OKAY;
    return 0;
	}
#endif

	det = 1.0;
	for (k = 0; k < n;  k++) {
		l[k] = k;  m[k] = k;
		biga = rm[k][k];

		/* Find the biggest element in the submatrix */
		for (i = k;  i < n;  i++)
			for (j = k; j < n; j++)
				if (ABS(rm[i][j]) > ABS(biga)) {
					biga = rm[i][j];
					l[k] = i;
					m[k] = j;
				}

//		if(DebugInvertMatrix) 
//			if(biga == 0.0)
//				PrintMatrix(m, "found zero biga = %g\n", biga);

		/* Interchange rows */
		i = l[k];
		if (i > k)
			for (j = 0; j < n; j++) {
				hold = -rm[k][j];
				rm[k][j] = rm[i][j];
				rm[i][j] = hold;
			}

		/* Interchange columns */
		j = m[k];
		if (j > k)
			for (i = 0; i < n; i++) {
				hold = -rm[i][k];
				rm[i][k] = rm[i][j];
				rm[i][j] = hold;
			}

		/* Divide column by minus pivot
		    (value of pivot element is contained in biga). */
		if (biga == 0.0) {
			return 0.0;
		}

		if(DebugInvertMatrix) printf("biga = %g\n", biga);
		recip_biga = 1/biga;
		for (i = 0; i < n; i++)
			if (i != k)
				rm[i][k] *= -recip_biga;

		/* Reduce matrix */
		for (i = 0; i < n; i++)
			if (i != k) {
				hold = rm[i][k];
				for (j = 0; j < n; j++)
					if (j != k)
						rm[i][j] += hold * rm[k][j];
			}

		/* Divide row by pivot */
		for (j = 0; j < n; j++)
			if (j != k)
				rm[k][j] *= recip_biga;

		det *= biga;	/* Product of pivots */
		if(DebugInvertMatrix) printf("det = %g\n", det);
		rm[k][k] = recip_biga;

	}	/* K loop */

	/* Final row & column interchanges */
	for (k = n - 1; k >= 0 && k<n; k--) {
		i = l[k];
		if (i > k)
			for (j = 0; j < n; j++) {
				hold = rm[j][k];
				rm[j][k] = -rm[j][i];
				rm[j][i] = hold;
			}
		j = m[k];
		if (j > k)
			for (i = 0; i < n; i++) {
				hold = rm[k][i];
				rm[k][i] = -rm[j][i];
				rm[j][i] = hold;
			}
	}

	if(DebugInvertMatrix) printf("returning, det = %g\n", det);

	return det;
}

void VectorFactorOffset(double factor,double offset,Vector_t v) {

    ScalarTimesVector(factor,v,v);
    VectorAddScalar(offset,v,v);
}
void VectorAdd(Vector_t va,Vector_t vb, Vector_t vres)
{
	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(va) != GET_NROWS(vb)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorAdd: result wrong size (dim(va)=%d !=dim(vb)=%d )",
			GET_NROWS(va), GET_NROWS(vb));
#else
		sprintf(SlfNumErrMess,"VectorAdd: result wrong size (dim(va)=%d !=dim(vb)=%d )",
			GET_NROWS(va), GET_NROWS(vb));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
  }

	if(GET_NROWS(va) != GET_NROWS(vres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorAdd: result wrong size (dim(va)=%d !=dim(vres)=%d )",
			GET_NROWS(va), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"VectorAdd: result wrong size (dim(va)=%d !=dim(vres)=%d )",
			GET_NROWS(va), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
  }
#endif
	for(i=0;i<GET_NROWS(va);i++) 
		vres[i] = va[i] + vb[i];
}
// vres = v + s
void VectorAddScalar(double s,Vector_t v,Vector_t vres)
{
	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NROWS(vres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorAddScalar: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"VectorAddScalar: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif
	for(i = 0; i < GET_NROWS(v); i++)
        vres[i] =  v[i]+s;
}
// mres = ma + s;
void MatrixAddScalar(double s,Matrix_t ma, Matrix_t mres) {

	register uarray i,j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(  GET_NROWS(mres) != GET_NROWS(ma)
      || GET_NCOLS(mres) != GET_NCOLS(ma)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixAddScalar: result wrong size (dim(mres)=(%dx%d) !=dim(ma)=(%dx%d) )",
			GET_NROWS(mres),GET_NCOLS(mres), GET_NROWS(ma),GET_NCOLS(ma));
#else
		sprintf(SlfNumErrMess,"MatrixAddScalar: result wrong size (dim(mres)=(%dx%d) !=dim(ma)=(%dx%d) )",
			GET_NROWS(mres),GET_NCOLS(mres), GET_NROWS(ma),GET_NCOLS(ma));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif
	for(i=0;i<GET_NROWS(mres);i++) 
    	for(j=0;j<GET_NCOLS(mres);j++) 
		    mres[i][j] = ma[i][j] + s;
} 

// vres = va - vb
//===============
void VectorSub(Vector_t va,Vector_t vb, Vector_t vres)
{
	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(va) != GET_NROWS(vb)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorSub: result wrong size (dim(va)=%d !=dim(vb)=%d )",
			GET_NROWS(va), GET_NROWS(vb));
#else
		sprintf(SlfNumErrMess,"VectorSub: result wrong size (dim(va)=%d !=dim(vb)=%d )",
			GET_NROWS(va), GET_NROWS(vb));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

	if(GET_NROWS(va) != GET_NROWS(vres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorSub: result wrong size (dim(va)=%d !=dim(vres)=%d )",
			GET_NROWS(va), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"VectorSub: result wrong size (dim(va)=%d !=dim(vres)=%d )",
			GET_NROWS(va), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

#endif
	for(i=0;i<GET_NROWS(va);i++) 
		vres[i] = va[i] - vb[i];
}
// vio = vio + s*vadd
//===================
void VectorMultAdd(double s,Vector_t vadd, Vector_t vio) {

	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(vio) != GET_NROWS(vadd)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorMultAdd: result wrong size (dim(vio)=%d !=dim(vadd)=%d )",
			GET_NROWS(vio), GET_NROWS(vadd));
#else
		sprintf(SlfNumErrMess,"VectorMultAdd: result wrong size (dim(vio)=%d !=dim(vadd)=%d )",
			GET_NROWS(vio), GET_NROWS(vadd));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif
	for(i=0;i<GET_NROWS(vadd);i++) 
		vio[i] = vio[i] + s*vadd[i];
}
// vio = vio - s*vsub
//===================
void VectorMultSub(double s,Vector_t vsub, Vector_t vio) {

	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(vio) != GET_NROWS(vsub)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"VectorMultSub: result wrong size (dim(vio)=%d !=dim(vsub)=%d )",
			GET_NROWS(vio), GET_NROWS(vsub));
#else
		sprintf(SlfNumErrMess,"VectorMultSub: result wrong size (dim(vio)=%d !=dim(vsub)=%d )",
			GET_NROWS(vio), GET_NROWS(vsub));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
  }
#endif
	for(i=0;i<GET_NROWS(vsub);i++) 
		vio[i] = vio[i] - s*vsub[i];
}
// mio = mio + s*madd;
void MatrixMultAdd(double s,Matrix_t madd, Matrix_t mio) {

	register uarray i,j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(  GET_NROWS(mio) != GET_NROWS(madd)
      || GET_NCOLS(mio) != GET_NCOLS(madd)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixMultAdd: result wrong size (dim(mio)=(%dx%d) !=dim(madd)=(%dx%d) )",
			GET_NROWS(mio),GET_NCOLS(mio), GET_NROWS(madd),GET_NCOLS(madd));
#else
		sprintf(SlfNumErrMess,"MatrixMultAdd: result wrong size (dim(mio)=(%dx%d) !=dim(madd)=(%dx%d) )",
			GET_NROWS(mio),GET_NCOLS(mio), GET_NROWS(madd),GET_NCOLS(madd));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
 }
#endif
	for(i=0;i<GET_NROWS(mio);i++) 
    	for(j=0;j<GET_NCOLS(mio);j++) 
		    mio[i][j] = mio[i][j] + s*madd[i][j];
}
// mio = mio - s*msub;
void MatrixMultSub(double s,Matrix_t msub, Matrix_t mio) {

	register uarray i,j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(  GET_NROWS(mio) != GET_NROWS(msub)
      || GET_NCOLS(mio) != GET_NCOLS(msub)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixMultSub: result wrong size (dim(mio)=(%dx%d) !=dim(madd)=(%dx%d) )",
			GET_NROWS(mio),GET_NCOLS(mio), GET_NROWS(msub),GET_NCOLS(msub));
#else
		sprintf(SlfNumErrMess,"MatrixMultSub: result wrong size (dim(mio)=(%dx%d) !=dim(madd)=(%dx%d) )",
			GET_NROWS(mio),GET_NCOLS(mio), GET_NROWS(msub),GET_NCOLS(msub));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

#endif
	for(i=0;i<GET_NROWS(mio);i++) 
    	for(j=0;j<GET_NCOLS(mio);j++) 
		    mio[i][j] = mio[i][j] - s*msub[i][j];
} 

void VectorDivideScalar(double s,Vector_t v,Vector_t vres)
{
	register uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NROWS(vres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"ScalarTimesVector: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#else
		sprintf(SlfNumErrMess,"ScalarTimesVector: result wrong size (%d!=%d)",
			GET_NROWS(v), GET_NROWS(vres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif
	for(i = 0; i < GET_NROWS(v); i++)
        vres[i] = v[i]/s;
}

double VectorNorm(Vector_t v) {

    return sqrt(InnerProduct(v,v));
}
void VectorAbs(Vector_t v) {
	register uarray i;

	for(i = 0; i < GET_NROWS(v); i++)
        v[i] = fabs(v[i]);
}

void UnifyVector(Vector_t v, Vector_t e) {

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != GET_NROWS(e)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"UnifyVector: result wrong size v(%d) != e(%d)",
			GET_NROWS(v), GET_NROWS(e));
#else
	  sprintf(SlfNumErrMess,"UnifyVector: result wrong size v(%d) != e(%d)",
			GET_NROWS(v), GET_NROWS(e));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
  }

#endif

    VectorDivideScalar(NOT_ZERO(VectorNorm(v)),v,e);
}

void MakeTildeMatrix(Vector_t v,Matrix_t mres) {

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != 3  ) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(v));
#else
		sprintf(SlfNumErrMess,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(v));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
  }

	if(GET_NROWS(mres) != 3  || 
	   GET_NCOLS(mres) != 3) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTildeMatrix: result wrong size mres(%d,%d) != 3x3",
			GET_NROWS(mres),
			GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"MakeTildeMatrix: result wrong size mres(%d,%d) != 3x3",
			GET_NROWS(mres),
			GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

    mres[0][0] = 0.0;
    mres[1][0] = v[2];
    mres[2][0] = -v[1];

    mres[0][1] = -v[2];
    mres[1][1] = 0.0;
    mres[2][1] = v[0];

    mres[0][2] = v[1];
    mres[1][2] = -v[0];
    mres[2][2] = 0.0;

}
//
// M= rtilde' * rtilde
void MakeTransponeTildeMatrixMultTildeMatrix(Vector_t v,Matrix_t mres) {

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(v) != 3  ) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTransponeTildeMatrixMultTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(v));
#else
		sprintf(SlfNumErrMess,"MakeTransponeTildeMatrixMultTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(v));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
	if(GET_NROWS(mres) != 3  || 
	   GET_NCOLS(mres) != 3) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTransponeTildeMatrixMultTildeMatrix: result wrong size mres(%d,%d) != 3x3",
			GET_NROWS(mres),
			GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"MakeTransponeTildeMatrixMultTildeMatrix: result wrong size mres(%d,%d) != 3x3",
			GET_NROWS(mres),
			GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

    mres[0][0] = v[1]*v[1]+v[2]+v[2];
    mres[1][0] = -v[0]*v[1];
    mres[2][0] = -v[0]*v[2];

    mres[0][1] = mres[1][0];
    mres[1][1] = v[0]*v[0]+v[2]+v[2];
    mres[2][1] = -v[1]*v[2];

    mres[0][2] = mres[2][0];
    mres[1][2] = mres[2][1];
    mres[2][2] = v[0]*v[0]+v[1]*v[1];

}
// mres = v*v'
//============
void MakeVectorMultTransponeVektor(Vector_t va,Vector_t vtotrans, Matrix_t mres) {

    uarray i,j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(va) != GET_NROWS(mres) || GET_NROWS(vtotrans) != GET_NCOLS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeVectorMultTransponeVektor: result wrong size");
#else
		sprintf(SlfNumErrMess,"MakeVectorMultTransponeVektor: result wrong size");
#endif
    SlfNumStatus = NOT_OKAY;
    return;
	}
#endif

    for(i=0;i<GET_NROWS(va);i++)
        for(j=0;j<GET_NROWS(vtotrans);j++)
            mres[i][j] = va[i]*vtotrans[j];
}
void MatrixAdd(Matrix_t ma,Matrix_t mb,Matrix_t mres) {

	register uarray i, j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(ma) != GET_NROWS(mres)  || 
	   GET_NCOLS(ma) != GET_NCOLS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixAdd: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(ma), GET_NCOLS(ma),
			GET_NROWS(mres), GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"MatrixAdd: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(ma), GET_NCOLS(ma),
			GET_NROWS(mres), GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}

	if(GET_NROWS(mb) != GET_NROWS(mres)  || 
	   GET_NCOLS(mb) != GET_NCOLS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixAdd: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(mb), GET_NCOLS(mb),
			GET_NROWS(mres), GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"MatrixAdd: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(mb), GET_NCOLS(mb),
			GET_NROWS(mres), GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

	for(i = 0; i < GET_NROWS(mres); i++)
		for(j = 0; j < GET_NCOLS(mres); j++)
			mres[i][j] = ma[i][j] + mb[i][j];
}
// mres = ma - mb
//===============
void MatrixSub(Matrix_t ma,Matrix_t mb,Matrix_t mres) {

	register uarray i, j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(ma) != GET_NROWS(mres)  || 
	   GET_NCOLS(ma) != GET_NCOLS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixSub: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(ma), GET_NCOLS(ma),
			GET_NROWS(mres), GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"MatrixSub: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(ma), GET_NCOLS(ma),
			GET_NROWS(mres), GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
	if(GET_NROWS(mb) != GET_NROWS(mres)  || 
	   GET_NCOLS(mb) != GET_NCOLS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MatrixSub: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(mb), GET_NCOLS(mb),
			GET_NROWS(mres), GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"MatrixSub: result wrong size ma(%d x %d) != mres(%d x %d)",
			GET_NROWS(mb), GET_NCOLS(mb),
			GET_NROWS(mres), GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif

	for(i = 0; i < GET_NROWS(mres); i++)
		for(j = 0; j < GET_NCOLS(mres); j++)
			mres[i][j] = ma[i][j] - mb[i][j];
}
// M[i+irow][j+icol] = m[i][j]
//============================
void PutMatrixToMatrix(Matrix_t m,Matrix_t M,uarray irow,uarray icol) {

    uarray i, j;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(irow+GET_NROWS(m) > GET_NROWS(M)  || 
	   icol+GET_NCOLS(m) > GET_NCOLS(M)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"PutMatrixToMatrix: size m(%d x %d) doesn't fit to M(%d x %d) at pos irow=%d,icol=%d",
			GET_NROWS(m), GET_NCOLS(m),
			GET_NROWS(M), GET_NCOLS(M),
            irow,icol);
#else
		sprintf(SlfNumErrMess,"PutMatrixToMatrix: size m(%d x %d) doesn't fit to M(%d x %d) at pos irow=%d,icol=%d",
			GET_NROWS(m), GET_NCOLS(m),
			GET_NROWS(M), GET_NCOLS(M),
            irow,icol);
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

	for(i = 0; i < GET_NROWS(m); i++)
		for(j = 0; j < GET_NCOLS(m); j++)
			M[irow+i][icol+j] = m[i][j];


}
void PutVectorToMatrix(Vector_t v, Matrix_t M, uarray irow, uarray icol) {

    uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(irow+GET_NROWS(v) > GET_NROWS(M)  || 
	   icol+1 > GET_NCOLS(M)) {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"PutVectorToMatrix: size v(%d) doesn't fit to M(%d x %d) at pos irow=%d,icol=%d",
			GET_NROWS(v),
			GET_NROWS(M), GET_NCOLS(M),
            irow,icol);
#else
		sprintf(SlfNumErrMess,"PutVectorToMatrix: size v(%d) doesn't fit to M(%d x %d) at pos irow=%d,icol=%l",
			GET_NROWS(v),
			GET_NROWS(M), GET_NCOLS(M),
            irow,icol);
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

	for(i = 0; i < GET_NROWS(v); i++)
		M[irow+i][icol] = v[i];


}
void PutVectorToVector(Vector_t v, Vector_t vres, uarray irow) {

    uarray i;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(irow+GET_NROWS(v) > GET_NROWS(vres) ) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"PutVectorxToVector: size v(%d) doesn't fit to vres(%d) at pos irow=%d",
			GET_NROWS(v),
			GET_NROWS(vres),
            irow);
#else
		sprintf(SlfNumErrMess,"PutVectorxToVector: size v(%d) doesn't fit to vres(%d) at pos irow=%d",
			GET_NROWS(v),
			GET_NROWS(vres),
            irow);
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

	for(i = 0; i < GET_NROWS(v); i++)
		vres[irow+i] = v[i];
}
void PutVectorToVectorMax(Vector_t v, Vector_t vres, uarray nmax) {

    uarray i;
    
    nmax = MIN(GET_NROWS(v),nmax);
    nmax = MIN(GET_NROWS(vres),nmax);


	for(i = 0; i < nmax; i++)
		vres[i] = v[i];
}
void PutMatrixToMatrixMax(Matrix_t m, Matrix_t M, uarray nrowmax, uarray ncolmax) {

    uarray i, j;

    nrowmax = MIN(GET_NROWS(m),nrowmax);
    nrowmax = MIN(GET_NROWS(M),nrowmax);
    ncolmax = MIN(GET_NCOLS(m),ncolmax);
    ncolmax = MIN(GET_NCOLS(M),ncolmax);

	for(i = 0; i < nrowmax; i++)
		for(j = 0; j < ncolmax; j++)
			M[i][j] = m[i][j];


}void TransponeMatrix(Matrix_t m,Matrix_t mres) {

	register uarray i, j;
    Matrix_t          mm;

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(m) != GET_NCOLS(mres)  || 
	   GET_NCOLS(m) != GET_NROWS(mres)) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"TransponeMatrix: result wrong size m(%d x %d) could not be transponed to mres(%d x %d)",
			GET_NROWS(m), GET_NCOLS(m),
			GET_NROWS(mres), GET_NCOLS(mres));
#else
		sprintf(SlfNumErrMess,"TransponeMatrix: result wrong size m(%d x %d) could not be transponed to mres(%d x %d)",
			GET_NROWS(m), GET_NCOLS(m),
			GET_NROWS(mres), GET_NCOLS(mres));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	    }
#endif

    if( m == mres ){

        mm = NewMatrix(GET_NCOLS(m),GET_NROWS(m));

	    for(i = 0; i < GET_NROWS(m); i++)
		    for(j = 0; j < GET_NCOLS(m); j++)
			    mm[j][i] = m[i][j];

	    for(i = 0; i < GET_NROWS(mm); i++)
		    for(j = 0; j < GET_NCOLS(mm); j++)
			    mres[i][j] = mm[i][j];

        FreeMatrix(mm);
    } else {

        for(i = 0; i < GET_NROWS(m); i++)
		    for(j = 0; j < GET_NCOLS(m); j++)
			    mres[j][i] = m[i][j];
    }

}
void VectorCrossProduct(Vector_t va,Vector_t vb, Vector_t vres) {

#ifdef SLS_NUM_PROOF_FUNCTION
	if(GET_NROWS(va) != 3  ) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(va));
#else
		sprintf(SlfNumErrMess,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(va));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
	if(GET_NROWS(vb) != 3  ) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(va));
#else
		sprintf(SlfNumErrMess,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(va));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
	if(GET_NROWS(vres) != 3  ) 
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(va));
#else
		sprintf(SlfNumErrMess,"MakeTildeMatrix: Vektorwrong size v(%d) != 3",
			GET_NROWS(va));
#endif
    SlfNumStatus = NOT_OKAY;
		return;
	}
#endif

    vres[0] = va[1]*vb[2]-va[2]*vb[1];
    vres[1] = va[2]*vb[0]-va[0]*vb[2];
    vres[2] = va[0]*vb[1]-va[1]*vb[0];

}

//=================================================================
//=================================================================
// Memory allocation, Do not call this function directly, 
// use SlfNumAllocate(n, type)
//=================================================================
//=================================================================

void *SlfNumAllocateFunc(uint32_t nitems, uint32_t itemsize, char *tname)
{
	register uint32_t bytes = nitems * itemsize;
	register void *p = malloc(bytes)/*new char[bytes]*/;

#ifdef SLS_NUM_PROOF_FUNCTION
    if(p == NULL) 
    {
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfNumErrMess,BUFSIZ,"Error SlfNumAllocatFunc: Can't get mem for %d %s's (each %d bytes, %d total bytes)",
		nitems, tname, itemsize, bytes);
#else
       sprintf(SlfNumErrMess,"Error SlfNumAllocatFunc: Can't get mem for %d %s's (each %d bytes, %d total bytes)",
		nitems, tname, itemsize, bytes);
    
#endif
    }
#endif
	return p;
}

#if 0
//==============================================================================
// LU-Decomposition by Gaussian elimination
// ========================================
// Input
// a[0 ... n-1][0 ... n-1]
// Output
// a[beta11,beta12, ..., beta1n;
//   alph21,beta22, ..., beta2n;
//   alph31,alph32, ..., beta3n;
//   ...
//   alphn1,alphn2, ..., betann]
//
//  alphij : lower triangular
//  betaij : upper triangular
//
// index[0 ... n-1] enthält die Reihen permutation
// d = +/- 1.0 Zeigt ob gerade Zahl (+1) oder ungerade (-1) von vertauschungen
//
// Rückgabe NO_ERR(0)              : okay
//          MATRIX_IS_SINGULAR(16) : singular
//==============================================================================
error_t ludcmp(Matrix_t a, uint32_t n, uint32_t *indx, double *d) {

	uint32_t i,imax,j,k;
	double big,dum,sum,temp;
	double *vv;

	vv=NewVector(n);
	*d=1.0;
	for (i=0;i<n;i++) {
		big=0.0;
		for (j=0;j<n;j++)
			if ((temp=fabs(a[i][j])) > big) big=temp;
		if (big == 0.0) // singulär
            return MATRIX_IS_SINGULAR;
		vv[i]=1.0/big;
	}
	for (j=0;j<n;j++) {
		for (i=0;i<j;i++) {
			sum=a[i][j];
			for (k=0;k<i;k++) sum -= a[i][k]*a[k][j];
			a[i][j]=sum;
		}
		big=0.0;
		for (i=j;i<n;i++) {
			sum=a[i][j];
			for (k=0;k<j;k++)
				sum -= a[i][k]*a[k][j];
			a[i][j]=sum;
			if ( (dum=vv[i]*fabs(sum)) >= big) {
				big=dum;
				imax=i;
			}
		}
		if (j != imax) {
			for (k=0;k<n;k++) {
				dum=a[imax][k];
				a[imax][k]=a[j][k];
				a[j][k]=dum;
			}
			*d = -(*d);
			vv[imax]=vv[j];
		}
		indx[j]=imax;
		if (a[j][j] == 0.0) a[j][j]=DEPSILON;
		if (j != n-1) {
			dum=1.0/(a[j][j]);
			for (i=j+1;i<n;i++) a[i][j] *= dum;
		}
	}
	FreeVector(vv);

    return NO_ERR;
}

//==============================================================================
// LU-Decomposition by Gaussian elimination of a banded matrix
// with lower bandwidth ml and upper bandwidth mu
// ==================================================
// Input
// a[0 ... n-1][0 ... n-1]
// n   order orginalmatrix
// ml  lower bandwidth of a (diagnonal not counted)
// mu  upper bandwidth of a (diagnonal not counted)
//
// Output
/*     a       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND */
/*                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT. */
/*     ip      INDEX VECTOR OF PIVOT INDICES. */
//     d = +/- 1.0 Zeigt ob gerade Zahl (+1) oder ungerade (-1) von vertauschungen
/*              USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
// index[0 ... n-1] enthält die Reihen permutation
//
// Rückgabe NO_ERR(0)              : okay
//          MATRIX_IS_SINGULAR(16) : singular
/*  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1. */

/*  REFERENCE.. */
/*     THIS IS A MODIFICATION OF */
/*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
/*     C.A.C.M. 15 (1972), P. 274. */
//
//==================================================================================
error_t ludcmpb(Matrix_t a, uint32_t n, uint32_t ml, uint32_t mu, uint32_t *indx, double *d) {

    /* Local variables */
    static uint32_t i,j,k,m;
    static double t;
    static uint32_t md, jk, mm, ju, md1, nm1, kp1, mdl, ijk;

    md = ml + mu + 1;
    md1 = md + 1;

    ju = 0;
    *d = 1.0;
    
    if (ml && n > 1){
    
        if (n >= mu + 2) {

            for (j = mu + 2; j <= n; ++j) {
                for (i = 1; i <= ml; ++i) 
                    a[i-1][j-1] = 0.;
            }
        }
        nm1 = n - 1;
        for (k = 1; k <= nm1; ++k) {
	        kp1 = k + 1;
	        m   = md;
	        mdl = MIN(ml,n-k) + md;
    	    for (i = md1; i <= mdl; ++i) {
	            if( fabs(a[i-1][k-1]) > fabs(a[m-1][k-1]) ) 
                    m = i;
	        
	        }
	        indx[k - 1] = m + k - md;
    	    t = a[m-1][k-1];
            if (m != md) {
	    
                *d *= -1.0;
        	    a[m-1][k-1]  = a[md-1][k-1];
    	        a[md-1][k-1] = t;
            }
    
    	    if (t == 0.)  return MATRIX_IS_SINGULAR;
	    
    	    t = 1. / t;
    	    for (i = md1; i <= mdl; ++i) 
    	        a[i-1][k-1] = -a[i-1][k-1] * t;
	    
            ju  = MIN(MAX(ju,mu+indx[k-1]),n);

    	    mm = md;
            if (ju >= kp1) {
	    
                for (j = kp1; j <= ju; ++j) {
    	            --m;
    	            --mm;
    	            t = a[m-1][j-1];
                    if (m != mm) {
	            
    	                a[m-1][j-1] = a[mm-1][j-1];
    	                a[mm-1][j-1] = t;
                    }
                    if (t != 0.) {
	            
    	                jk = j - k;
    	                for (i = md1; i <= mdl; ++i) {
    		                ijk = i - jk;
    		                a[ijk-1][j-1] = a[ijk-1][j-1] + a[i-1][k-1] * t;
	                    }

                    }                    
    	        }
            }
        }
    }
    k = n;
    if (a[md-1][n-1] == 0.)	
        return MATRIX_IS_SINGULAR;
    
    return NO_ERR;

    ;
}
/*C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A HESSENBERG
C  MATRIX WITH LOWER BANDWIDTH LB
C  INPUT..
C     N = ORDER OF MATRIX A.
C     NDIM = DECLARED DIMENSION OF ARRAY  A .
C     A = MATRIX TO BE TRIANGULARIZED.
C     LB = LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED, LB.GE.1).
C  OUTPUT..
C     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U .
C     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
C     d = +/- 1.0 Zeigt ob gerade Zahl (+1) oder ungerade (-1) von vertauschungen
C              USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C     index[0 ... n-1] enthält die Reihen permutation
C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
C           SINGULAR AT STAGE K.
C  USE  SOLH  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     THIS IS A SLIGHT MODIFICATION OF
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------*/
error_t ludcmph(Matrix_t a,uint32_t n, uint32_t lb, uint32_t *indx, double *d) {
  
    uint32_t nm1,k,i,j,m,na,kp1;
    double t;

    *d = 1.0;
    if(n > 1) {
      
        nm1 = n - 1;
        for(k=1;k<=nm1;k++) {
            kp1 = k + 1;
            m   = k;
            na  = MIN(n,lb+k);
            for(i=kp1;i<=na;i++) {
                if( fabs(a[i-1][k-1]) > fabs(a[m-1][k-1]) ) 
                    m = i;
            }
            indx[k-1] = m;
            t = a[m-1][k-1];
            if( m != k ) {
                *d *= -1.;
                a[m-1][k-1] = a[k-1][k-1];
                a[k-1][k-1] = t;
            }
            if( t == 0.0) return MATRIX_IS_SINGULAR;
            t = 1.0/t;
            for(i=kp1;i<=na;i++) 
                a[i-1][k-1] = -a[i-1][k-1]*t;
            for(j=kp1;j<=n;j++) {
        
                t = a[m-1][j-1];
                a[m-1][j-1] = a[k-1][j-1];
                a[k-1][j-1] = t;
                if( t != 0.0) {
                    for(i=kp1;i<=na;i++)
                        a[i-1][j-1] += a[i-1][k-1]*t;
                }
            }
        }
    }
    k = n;
    if( a[n-1][n-1] == 0.0) 
        return MATRIX_IS_SINGULAR;

    return NO_ERR;
}
/*
C     this subroutine is a translation of the algol procedure elmhes,
C     num. math. 12, 349-368(1968) by martin and wilkinson.
C     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
C
C     given a real general matrix, this subroutine
C     reduces a submatrix situated in rows and columns
C     low through igh to upper hessenberg form by
C     stabilized elementary similarity transformations.
C
C     on input:
C
C      nm must be set to the row dimension of two-dimensional
C        array parameters as declared in the calling program
C        dimension statement;
C
C      n is the order of the matrix;
C
C      low and igh are integers determined by the balancing
C        subroutine  balanc.      if  balanc  has not been used,
C        set low=1, igh=n;
C
C      a contains the input matrix.
C
C     on output:
C
C      a contains the hessenberg matrix.  the multipliers
C        which were used in the reduction are stored in the
C        remaining triangle under the hessenberg matrix;
C
C      int contains information on the rows and columns
C        interchanged in the reduction.
C        only elements low through igh are used.
C
C     questions and comments should be directed to b. s. garbow,
C     applied mathematics division, argonne national laboratory
C
C     ------------------------------------------------------------------
C
*/
void elmhes(uint32_t n,uint32_t low,uint32_t igh,Matrix_t a,uint32_t *iphes) {
    
    uint32_t la,kp1,mm1,mp1,i,m,j;
    double x,y;
        
    la = igh - 1;
    kp1 = low + 1;
    if (la < kp1) return;
    if( GET_NROWS(a) < n ) return;
    
    for(m=kp1;m<=la;m++) {
        
        mm1 = m - 1;
        x   = 0.0;
        i   = m;
        
        for(j=m;j<=igh;j++) {
            
            if(fabs(a[j-1][mm1-1]) > fabs(x) ) {
                x = a[j-1][mm1-1];
                i = j;
            }
        }
        iphes[m-1] = i;
        if( i != m ) {
            //    :::::::::: interchange rows and columns of a ::::::::::
            for(j=mm1;j<=n;j++) {
                y = a[i-1][j-1];
                a[i-1][j-1] = a[m-1][j-1];
                a[m-1][j-1] = y;
            }
            
            for(j=1;j<=igh;j++) {
                y           = a[j-1][i-1];
                a[j-1][i-1] = a[j-1][m-1];
                a[j-1][m-1] = y;
            }
        }
        //    :::::::::: end interchange ::::::::::
        
        if (x != 0.0) {
            
            mp1 = m + 1;
            
            for(i=mp1;i<=igh;i++) {
                
                y = a[i-1][mm1-1];
                if (y != 0.0) {
                    y             = y / x;
                    a[i-1][mm1-1] = y;
                    
                    for(j=m;j<=n;j++) 
                        a[i-1][j-1] -= y * a[m-1][j-1];
                    
                    for(j=1;j<=igh;j++)
                        a[j-1][m-1] += y * a[j-1][i-1];
                    
                }
            }
        }
        
    }
}
#endif
double SlfNumDmaxnorm(sint32_t n, double *v, double *w)
{
    /* System generated locals */
    sint32_t i;
    double ret_val;

/* ----------------------------------------------------------------------- */
/* This function routine computes the weighted max-norm */
/* of the vector of length N contained in the array V, with weights */
/* contained in the array w of length N: */
/*   DMNORM = MAX(i=1,...,N) ABS(V(i))*W(i) */
/* ----------------------------------------------------------------------- */

    ret_val = 0.;
    for(i=0;i<n;i++)
        ret_val = MAX(ret_val,fabs(v[i]*w[i]));
    
    return ret_val;
}

/* DECK DBNORM */
double SlfNumDbnorm(sint32_t n, double *a, sint32_t nra, sint32_t ml, 
	sint32_t mu, double *w)
{
    /* System generated locals */
    sint32_t a_dim1, a_offset, i__1, i__2;
    double ret_val, d__1, d__2;

    /* Local variables */
    static sint32_t i__, j, i1;
    static double an;
    static sint32_t jhi, jlo;
    static double sum;

/* ----------------------------------------------------------------------- */
/* This function computes the norm of a banded N by N matrix, */
/* stored in the array A, that is consistent with the weighted max-norm */
/* on vectors, with weights stored in the array W. */
/* ML and MU are the lower and upper half-bandwidths of the matrix. */
/* NRA is the first dimension of the A array, NRA .ge. ML+MU+1. */
/* In terms of the matrix elements a(i,j), the norm is given by: */
/*   DBNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) ) */
/* ----------------------------------------------------------------------- */
    /* Parameter adjustments */
    --w;
    a_dim1 = nra;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    an = 0.;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i1 = i__ + mu + 1;
/* Computing MAX */
	i__2 = i__ - ml;
	jlo = MAX(i__2,1);
/* Computing MIN */
	i__2 = i__ + mu;
	jhi = MIN(i__2,n);
	i__2 = jhi;
	for (j = jlo; j <= i__2; ++j) {
/* L10: */
	    sum += fabs(a[i1 - j + j * a_dim1]) / w[j];
	}
/* Computing MAX */
	d__1 = an, d__2 = sum * w[i__];
	an = MAX(d__1,d__2);
/* L20: */
    }
    ret_val = an;
    return ret_val;
/* ----------------------- End of Function DBNORM ------------------------ */
} /* dbnorm_ */
//////////////////////////

double SlfNumDfnorm(sint32_t n, double *a, double *w)
{
    /* System generated locals */
    sint32_t a_dim1, a_offset, i__1, i__2;
    double ret_val, d__1, d__2;

    /* Local variables */
    static sint32_t i__, j;
    static double an, sum;

/* ----------------------------------------------------------------------- */
/* This function computes the norm of a full N by N matrix, */
/* stored in the array A, that is consistent with the weighted max-norm */
/* on vectors, with weights stored in the array W: */
/*   DFNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) ) */
/* ----------------------------------------------------------------------- */
    /* Parameter adjustments */
    --w;
    a_dim1 = n;
    a_offset = 1 + a_dim1;
    a -= a_offset;

    /* Function Body */
    an = 0.;
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = n;
	for (j = 1; j <= i__2; ++j) {
/* L10: */
	    sum += fabs(a[i__ + j * a_dim1]) / w[j];
	}
/* Computing MAX */
	d__1 = an, d__2 = sum * w[i__];
	an = MAX(d__1,d__2);
/* L20: */
    }
    ret_val = an;
    return ret_val;
/* ----------------------- End of Function DFNORM ------------------------ */
} /* dfnorm_ */

#if 0
/* Subroutine */ int dewset_org(integer *n, integer *itol, doublereal *rtol, 
	doublereal *atol, doublereal *ycur, doublereal *ewt)
{
    /* System generated locals */
    sint32_t i;
    doublereal d__1;

    /* Local variables */
    static integer i__;

/* ***BEGIN PROLOGUE  DEWSET */
/* ***SUBSIDIARY */
/* ***PURPOSE  Set error weight vector. */
/* ***TYPE      DOUBLE PRECISION (SEWSET-S, DEWSET-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  This subroutine sets the error weight vector EWT according to */
/*      EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N, */
/*  with the subscript on RTOL and/or ATOL possibly replaced by 1 above, */
/*  depending on the value of ITOL. */

/* ***SEE ALSO  DLSODE */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/* ***END PROLOGUE  DEWSET */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  DEWSET */
    /* Parameter adjustments */
    --ewt;
    --ycur;
    --rtol;
    --atol;

    /* Function Body */
    switch (*itol) {
	case 1:  goto L10;
	case 2:  goto L20;
	case 3:  goto L30;
	case 4:  goto L40;
    }
L10:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L15: */
	ewt[i__] = rtol[1] * (d__1 = ycur[i__], abs(d__1)) + atol[1];
    }
    return 0;
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L25: */
	ewt[i__] = rtol[1] * (d__1 = ycur[i__], abs(d__1)) + atol[i__];
    }
    return 0;
L30:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L35: */
	ewt[i__] = rtol[i__] * (d__1 = ycur[i__], abs(d__1)) + atol[1];
    }
    return 0;
L40:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L45: */
	ewt[i__] = rtol[i__] * (d__1 = ycur[i__], abs(d__1)) + atol[i__];
    }
    return 0;
/* ----------------------- END OF SUBROUTINE DEWSET ---------------------- */
} /* dewset_ */
#else
int SlfNumDewset(sint32_t n, sint32_t itol, double *rtol, 
	double *atol, double *ycur, double *ewt)
{
    sint32_t i;

    switch(itol) {
    case 1:
        for(i=0;i<n;i++) 
            ewt[i] = rtol[1] * fabs(ycur[i]) + atol[1];
        break;
    case 2:
        for(i=0;i<n;i++) 
            ewt[i] = rtol[1] * fabs(ycur[i]) + atol[i];
        break;
    case 3:
        for(i=0;i<n;i++) 
            ewt[i] = rtol[i] * fabs(ycur[i]) + atol[1];
        break;
    case 4:
        for(i=0;i<n;i++) 
            ewt[i] = rtol[i] * fabs(ycur[i]) + atol[i];
        break;
    }
    return 0;
}
double SlfNumDumach()
{
    double u;

/* ***BEGIN PROLOGUE  DUMACH */
/* ***PURPOSE  Compute the unit roundoff of the machine. */
/* ***CATEGORY  R1 */
/* ***TYPE      DOUBLE PRECISION (RUMACH-S, DUMACH-D) */
/* ***KEYWORDS  MACHINE CONSTANTS */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */
/* *Usage: */
/*        DOUBLE PRECISION  A, DUMACH */
/*        A = DUMACH() */

/* *Function Return Values: */
/*     A : the unit roundoff of the machine. */

/* *Description: */
/*     The unit roundoff is defined as the smallest positive machine */
/*     number u such that  1.0 + u .ne. 1.0.  This is computed by DUMACH */
/*     in a machine-independent manner. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  DUMSUM */
/* ***REVISION HISTORY  (YYYYMMDD) */
/*   19930216  DATE WRITTEN */
/*   19930818  Added SLATEC-format prologue.  (FNF) */
/*   20030707  Added DUMSUM to force normal storage of COMP.  (ACH) */
/* ***END PROLOGUE  DUMACH */

/* ***FIRST EXECUTABLE STATEMENT  DUMACH */
    u = 1.;
    while(u+1. != 1.) {
        u *= .5;
    }
    return u * 2.;
/* ----------------------- End of Function DUMACH ------------------------ */
} /* dumach_ */

int SlfNumDcfode(sint32_t meth, double *elco, double *tesco)
{
    /* System generated locals */
    sint32_t i__1;

    /* Local variables */
    static sint32_t i__, ib;
    static double pc[12];
    static sint32_t nq;
    static double fnq;
    static sint32_t nqm1, nqp1;
    static double ragq, pint, xpin, fnqm1, agamq, rqfac, tsign, rq1fac;

/* ***BEGIN PROLOGUE  DCFODE */
/* ***SUBSIDIARY */
/* ***PURPOSE  Set ODE integrator coefficients. */
/* ***TYPE      DOUBLE PRECISION (SCFODE-S, DCFODE-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  DCFODE is called by the integrator routine to set coefficients */
/*  needed there.  The coefficients for the current method, as */
/*  given by the value of METH, are set for all orders and saved. */
/*  The maximum order assumed here is 12 if METH = 1 and 5 if METH = 2. */
/*  (A smaller value of the maximum order is also allowed.) */
/*  DCFODE is called once at the beginning of the problem, */
/*  and is not called again unless and until METH is changed. */

/*  The ELCO array contains the basic method coefficients. */
/*  The coefficients el(i), 1 .le. i .le. nq+1, for the method of */
/*  order nq are stored in ELCO(i,nq).  They are given by a genetrating */
/*  polynomial, i.e., */
/*      l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq. */
/*  For the implicit Adams methods, l(x) is given by */
/*      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0. */
/*  For the BDF methods, l(x) is given by */
/*      l(x) = (x+1)*(x+2)* ... *(x+nq)/K, */
/*  where         K = factorial(nq)*(1 + 1/2 + ... + 1/nq). */

/*  The TESCO array contains test constants used for the */
/*  local error test and the selection of step size and/or order. */
/*  At order nq, TESCO(k,nq) is used for the selection of step */
/*  size at order nq - 1 if k = 1, at order nq if k = 2, and at order */
/*  nq + 1 if k = 3. */

/* ***SEE ALSO  DLSODE */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/* ***END PROLOGUE  DCFODE */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  DCFODE */
    /* Parameter adjustments */
    tesco -= 4;
    elco -= 14;

    /* Function Body */
    switch (meth) {
	case 1:  goto L100;
	case 2:  goto L200;
    }

L100:
    elco[14] = 1.;
    elco[15] = 1.;
    tesco[4] = 0.;
    tesco[5] = 2.;
    tesco[7] = 1.;
    tesco[39] = 0.;
    pc[0] = 1.;
    rqfac = 1.;
    for (nq = 2; nq <= 12; ++nq) {
/* ----------------------------------------------------------------------- */
/* The PC array will contain the coefficients of the polynomial */
/*     p(x) = (x+1)*(x+2)*...*(x+nq-1). */
/* Initially, p(x) = 1. */
/* ----------------------------------------------------------------------- */
	rq1fac = rqfac;
	rqfac /= nq;
	nqm1 = nq - 1;
	fnqm1 = (double) nqm1;
	nqp1 = nq + 1;
/* Form coefficients of p(x)*(x+nq-1). ---------------------------------- */
	pc[nq - 1] = 0.;
	i__1 = nqm1;
	for (ib = 1; ib <= i__1; ++ib) {
	    i__ = nqp1 - ib;
/* L110: */
	    pc[i__ - 1] = pc[i__ - 2] + fnqm1 * pc[i__ - 1];
	}
	pc[0] = fnqm1 * pc[0];
/* Compute integral, -1 to 0, of p(x) and x*p(x). ----------------------- */
	pint = pc[0];
	xpin = pc[0] / 2.;
	tsign = 1.;
	i__1 = nq;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    tsign = -tsign;
	    pint += tsign * pc[i__ - 1] / i__;
/* L120: */
	    xpin += tsign * pc[i__ - 1] / (i__ + 1);
	}
/* Store coefficients in ELCO and TESCO. -------------------------------- */
	elco[nq * 13 + 1] = pint * rq1fac;
	elco[nq * 13 + 2] = 1.;
	i__1 = nq;
	for (i__ = 2; i__ <= i__1; ++i__) {
/* L130: */
	    elco[i__ + 1 + nq * 13] = rq1fac * pc[i__ - 1] / i__;
	}
	agamq = rqfac * xpin;
	ragq = 1. / agamq;
	tesco[nq * 3 + 2] = ragq;
	if (nq < 12) {
	    tesco[nqp1 * 3 + 1] = ragq * rqfac / nqp1;
	}
	tesco[nqm1 * 3 + 3] = ragq;
/* L140: */
    }
    return 0;

L200:
    pc[0] = 1.;
    rq1fac = 1.;
    for (nq = 1; nq <= 5; ++nq) {
/* ----------------------------------------------------------------------- */
/* The PC array will contain the coefficients of the polynomial */
/*     p(x) = (x+1)*(x+2)*...*(x+nq). */
/* Initially, p(x) = 1. */
/* ----------------------------------------------------------------------- */
	fnq = (double) nq;
	nqp1 = nq + 1;
/* Form coefficients of p(x)*(x+nq). ------------------------------------ */
	pc[nqp1 - 1] = 0.;
	i__1 = nq;
	for (ib = 1; ib <= i__1; ++ib) {
	    i__ = nq + 2 - ib;
/* L210: */
	    pc[i__ - 1] = pc[i__ - 2] + fnq * pc[i__ - 1];
	}
	pc[0] = fnq * pc[0];
/* Store coefficients in ELCO and TESCO. -------------------------------- */
	i__1 = nqp1;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L220: */
	    elco[i__ + nq * 13] = pc[i__ - 1] / pc[1];
	}
	elco[nq * 13 + 2] = 1.;
	tesco[nq * 3 + 1] = rq1fac;
	tesco[nq * 3 + 2] = nqp1 / elco[nq * 13 + 1];
	tesco[nq * 3 + 3] = (nq + 2) / elco[nq * 13 + 1];
	rq1fac /= fnq;
/* L230: */
    }
    return 0;
/* ----------------------- END OF SUBROUTINE DCFODE ---------------------- */
} /* dcfode_ */

int SlfNumDgbfa(double *abd, sint32_t lda, sint32_t n, 
	            sint32_t ml, sint32_t mu, sint32_t *ipvt, sint32_t *info)
{
    /* System generated locals */
    sint32_t  abd_dim1, abd_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    static sint32_t  i__, j, k, l, m;
    static double t;
    static sint32_t  i0, j0, j1, lm, mm, ju, jz, kp1, nm1;

/* ***BEGIN PROLOGUE  DGBFA */
/* ***PURPOSE  Factor a band matrix using Gaussian elimination. */
/* ***CATEGORY  D2A2 */
/* ***TYPE      DOUBLE PRECISION (SGBFA-S, DGBFA-D, CGBFA-C) */
/* ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     DGBFA factors a double precision band matrix by elimination. */

/*     DGBFA is usually called by DGBCO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */

/*     On Entry */

/*        ABD     DOUBLE PRECISION(LDA, N) */
/*                contains the matrix in band storage.  The columns */
/*                of the matrix are stored in the columns of  ABD  and */
/*                the diagonals of the matrix are stored in rows */
/*                ML+1 through 2*ML+MU+1 of  ABD . */
/*                See the comments below for details. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  ABD . */
/*                LDA must be .GE. 2*ML + MU + 1 . */

/*        N       INTEGER */
/*                the order of the original matrix. */

/*        ML      INTEGER */
/*                number of diagonals below the main diagonal. */
/*                0 .LE. ML .LT.  N . */

/*        MU      INTEGER */
/*                number of diagonals above the main diagonal. */
/*                0 .LE. MU .LT.  N . */
/*                More efficient if  ML .LE. MU . */
/*     On Return */

/*        ABD     an upper triangular matrix in band storage and */
/*                the multipliers which were used to obtain it. */
/*                The factorization can be written  A = L*U  where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that DGBSL will divide by zero if */
/*                     called.  Use  RCOND  in DGBCO for a reliable */
/*                     indication of singularity. */

/*     Band Storage */

/*           If  A  is a band matrix, the following program segment */
/*           will set up the input. */

/*                   ML = (band width below the diagonal) */
/*                   MU = (band width above the diagonal) */
/*                   M = ML + MU + 1 */
/*                   DO 20 J = 1, N */
/*                      I1 = MAX(1, J-MU) */
/*                      I2 = MIN(N, J+ML) */
/*                      DO 10 I = I1, I2 */
/*                         K = I - J + M */
/*                         ABD(K,J) = A(I,J) */
/*                10    CONTINUE */
/*                20 CONTINUE */

/*           This uses rows  ML+1  through  2*ML+MU+1  of  ABD . */
/*           In addition, the first  ML  rows in  ABD  are used for */
/*           elements generated during the triangularization. */
/*           The total number of rows needed in  ABD  is  2*ML+MU+1 . */
/*           The  ML+MU by ML+MU  upper left triangle and the */
/*           ML by ML  lower right triangle are not referenced. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DGBFA */


/* ***FIRST EXECUTABLE STATEMENT  DGBFA */
    /* Parameter adjustments */
    abd_dim1 = lda;
    abd_offset = 1 + abd_dim1;
    abd -= abd_offset;
    --ipvt;

    /* Function Body */
    m = ml + mu + 1;
    info = 0;

/*     ZERO INITIAL FILL-IN COLUMNS */

    j0 = mu + 2;
    j1 = MIN(n,m) - 1;
    if (j1 < j0) {
	goto L30;
    }
    i__1 = j1;
    for (jz = j0; jz <= i__1; ++jz) {
	i0 = m + 1 - jz;
	i__2 = ml;
	for (i__ = i0; i__ <= i__2; ++i__) {
	    abd[i__ + jz * abd_dim1] = 0.;
/* L10: */
	}
/* L20: */
    }
L30:
    jz = j1;
    ju = 0;

/*     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING */

    nm1 = n - 1;
    if (nm1 < 1) {
	goto L130;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        ZERO NEXT FILL-IN COLUMN */

	++jz;
	if (jz > n) {
	    goto L50;
	}
	if (ml < 1) {
	    goto L50;
	}
	i__2 = ml;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    abd[i__ + jz * abd_dim1] = 0.;
/* L40: */
	}
L50:

/*        FIND L = PIVOT INDEX */

/* Computing MIN */
	i__2 = ml, i__3 = n - k;
	lm = MIN(i__2,i__3);
	i__2 = lm + 1;
	l = SlfNumIdamax(i__2, &abd[m + k * abd_dim1], 1) + m - 1;
	ipvt[k] = l + k - m;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	if (abd[l + k * abd_dim1] == 0.) {
	    goto L100;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == m) {
	    goto L60;
	}
	t = abd[l + k * abd_dim1];
	abd[l + k * abd_dim1] = abd[m + k * abd_dim1];
	abd[m + k * abd_dim1] = t;
L60:

/*           COMPUTE MULTIPLIERS */

	t = -1. / abd[m + k * abd_dim1];
	SlfNumDscal(lm, &t, &abd[m + 1 + k * abd_dim1], 1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

/* Computing MIN */
/* Computing MAX */
	i__3 = ju, i__4 = mu + ipvt[k];
	i__2 = MAX(i__3,i__4);
	ju = MIN(i__2,n);
	mm = m;
	if (ju < kp1) {
	    goto L90;
	}
	i__2 = ju;
	for (j = kp1; j <= i__2; ++j) {
	    --l;
	    --mm;
	    t = abd[l + j * abd_dim1];
	    if (l == mm) {
		goto L70;
	    }
	    abd[l + j * abd_dim1] = abd[mm + j * abd_dim1];
	    abd[mm + j * abd_dim1] = t;
L70:
	    SlfNumDaxpy(lm, &t, &abd[m + 1 + k * abd_dim1], 1, &abd[mm + 1 + 
		    j * abd_dim1], 1);
/* L80: */
	}
L90:
	goto L110;
L100:
	*info = k;
L110:
/* L120: */
	;
    }
L130:
    ipvt[n] = n;
    if (abd[m + n * abd_dim1] == 0.) {
	*info = n;
    }
    return 0;
} /* dgbfa_ */
int SlfNumDscal(sint32_t n, double *da, double *dx, sint32_t incx)
{
    /* System generated locals */
    sint32_t i__1;

    /* Local variables */
    static sint32_t i__, m, ix, mp1;

/* ***BEGIN PROLOGUE  DSCAL */
/* ***PURPOSE  Multiply a vector by a constant. */
/* ***CATEGORY  D1A6 */
/* ***TYPE      DOUBLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DA  double precision scale factor */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */

/*     --Output-- */
/*       DX  double precision result (unchanged if N.LE.0) */

/*     Replace double precision DX by double precision DA*DX. */
/*     For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DSCAL */
/* ***FIRST EXECUTABLE STATEMENT  DSCAL */
    /* Parameter adjustments */
    --dx;

    /* Function Body */
    if (n <= 0) {
	return 0;
    }
    if (incx == 1) {
	goto L20;
    }

/*     Code for increment not equal to 1. */

    ix = 1;
    if (incx < 0) {
	ix = (-(n) + 1) * incx + 1;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dx[ix] = *da * dx[ix];
	ix += incx;
/* L10: */
    }
    return 0;

/*     Code for increment equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 5. */

L20:
    m = n % 5;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dx[i__] = *da * dx[i__];
/* L30: */
    }
    if (n < 5) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = n;
    for (i__ = mp1; i__ <= i__1; i__ += 5) {
	dx[i__] = *da * dx[i__];
	dx[i__ + 1] = *da * dx[i__ + 1];
	dx[i__ + 2] = *da * dx[i__ + 2];
	dx[i__ + 3] = *da * dx[i__ + 3];
	dx[i__ + 4] = *da * dx[i__ + 4];
/* L50: */
    }
    return 0;
} /* dscal_ */
int SlfNumDaxpy(sint32_t n, double *da, double *dx, 
	            sint32_t incx, double *dy, sint32_t incy)
{
    /* System generated locals */
    sint32_t i__1, i__2;

    /* Local variables */
    static sint32_t i__, m, ix, iy, ns, mp1;

/* ***BEGIN PROLOGUE  DAXPY */
/* ***PURPOSE  Compute a constant times a vector plus a vector. */
/* ***CATEGORY  D1A7 */
/* ***TYPE      DOUBLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DA  double precision scalar multiplier */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */
/*       DY  double precision vector with N elements */
/*     INCY  storage spacing between elements of DY */

/*     --Output-- */
/*       DY  double precision result (unchanged if N .LE. 0) */

/*     Overwrite double precision DY with double precision DA*DX + DY. */
/*     For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) + */
/*       DY(LY+I*INCY), */
/*     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is */
/*     defined in a similar way using INCY. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920310  Corrected definition of LX in DESCRIPTION.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DAXPY */
/* ***FIRST EXECUTABLE STATEMENT  DAXPY */
    /* Parameter adjustments */
    --dy;
    --dx;

    /* Function Body */
    if (n <= 0 || *da == 0.) {
	return 0;
    }
    if (incx == incy) {
	if ((i__1 = incx - 1) < 0) {
	    goto L5;
	} else if (i__1 == 0) {
	    goto L20;
	} else {
	    goto L60;
	}
    }

/*     Code for unequal or nonpositive increments. */

L5:
    ix = 1;
    iy = 1;
    if (incx < 0) {
	ix = (-(n) + 1) * incx + 1;
    }
    if (incy < 0) {
	iy = (-(n) + 1) * incy + 1;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dy[iy] += *da * dx[ix];
	ix += incx;
	iy += incy;
/* L10: */
    }
    return 0;

/*     Code for both increments equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 4. */

L20:
    m = n % 4;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dy[i__] += *da * dx[i__];
/* L30: */
    }
    if (n < 4) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = n;
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
	dy[i__] += *da * dx[i__];
	dy[i__ + 1] += *da * dx[i__ + 1];
	dy[i__ + 2] += *da * dx[i__ + 2];
	dy[i__ + 3] += *da * dx[i__ + 3];
/* L50: */
    }
    return 0;

/*     Code for equal, positive, non-unit increments. */

L60:
    ns = n * incx;
    i__1 = ns;
    i__2 = incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	dy[i__] = *da * dx[i__] + dy[i__];
/* L70: */
    }
    return 0;
} /* daxpy_ */

sint32_t SlfNumIdamax(sint32_t n, double *dx, sint32_t incx)
{
    /* System generated locals */
    sint32_t ret_val, i__1;

    /* Local variables */
    static sint32_t i__, ix;
    static double dmax__, xmag;

/* ***BEGIN PROLOGUE  IDAMAX */
/* ***PURPOSE  Find the smallest index of that component of a vector */
/*            having the maximum magnitude. */
/* ***CATEGORY  D1A2 */
/* ***TYPE      DOUBLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */

/*     --Output-- */
/*   IDAMAX  smallest index (zero if N .LE. 0) */

/*     Find smallest index of maximum magnitude of double precision DX. */
/*     IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890531  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  IDAMAX */
/* ***FIRST EXECUTABLE STATEMENT  IDAMAX */
    /* Parameter adjustments */
    --dx;

    /* Function Body */
    ret_val = 0;
    if (n <= 0) {
	return ret_val;
    }
    ret_val = 1;
    if (n == 1) {
	return ret_val;
    }

    if (incx == 1) {
	goto L20;
    }

/*     Code for increments not equal to 1. */

    ix = 1;
    if (incx < 0) {
	ix = (-(n) + 1) * incx + 1;
    }
    dmax__ = fabs(dx[ix]);
    ix += incx;
    i__1 = n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = fabs(dx[ix]);
	if (xmag > dmax__) {
	    ret_val = i__;
	    dmax__ = xmag;
	}
	ix += incx;
/* L10: */
    }
    return ret_val;

/*     Code for increments equal to 1. */

L20:
    dmax__ = fabs(dx[1]);
    i__1 = n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = fabs(dx[i__]);
	if (xmag > dmax__) {
	    ret_val = i__;
	    dmax__ = xmag;
	}
/* L30: */
    }
    return ret_val;
} /* idamax_ */

int SlfNumDgefa(double *a, sint32_t lda, sint32_t n, sint32_t *ipvt, sint32_t *info)
{
    /* System generated locals */
    sint32_t a_dim1, a_offset, i__1, i__2, i__3;

    /* Local variables */
    static sint32_t j, k, l;
    static double t;
    static sint32_t kp1, nm1;

/* ***BEGIN PROLOGUE  DGEFA */
/* ***PURPOSE  Factor a matrix using Gaussian elimination. */
/* ***CATEGORY  D2A1 */
/* ***TYPE      DOUBLE PRECISION (SGEFA-S, DGEFA-D, CGEFA-C) */
/* ***KEYWORDS  GENERAL MATRIX, LINEAR ALGEBRA, LINPACK, */
/*             MATRIX FACTORIZATION */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     DGEFA factors a double precision matrix by Gaussian elimination. */

/*     DGEFA is usually called by DGECO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */
/*     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) . */

/*     On Entry */

/*        A       DOUBLE PRECISION(LDA, N) */
/*                the matrix to be factored. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*     On Return */

/*        A       an upper triangular matrix and the multipliers */
/*                which were used to obtain it. */
/*                The factorization can be written  A = L*U  where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that DGESL or DGEDI will divide by zero */
/*                     if called.  Use  RCOND  in DGECO for a reliable */
/*                     indication of singularity. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DGEFA */


/*     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING */

/* ***FIRST EXECUTABLE STATEMENT  DGEFA */
    /* Parameter adjustments */
    a_dim1 = lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;

    /* Function Body */
    *info = 0;
    nm1 = n - 1;
    if (nm1 < 1) {
	goto L70;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        FIND L = PIVOT INDEX */

	i__2 = n - k + 1;
	l = SlfNumIdamax(i__2, &a[k + k * a_dim1], 1) + k - 1;
	ipvt[k] = l;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	if (a[l + k * a_dim1] == 0.) {
	    goto L40;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == k) {
	    goto L10;
	}
	t = a[l + k * a_dim1];
	a[l + k * a_dim1] = a[k + k * a_dim1];
	a[k + k * a_dim1] = t;
L10:

/*           COMPUTE MULTIPLIERS */

	t = -1. / a[k + k * a_dim1];
	i__2 = n - k;
	SlfNumDscal(i__2, &t, &a[k + 1 + k * a_dim1], 1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

	i__2 = n;
	for (j = kp1; j <= i__2; ++j) {
	    t = a[l + j * a_dim1];
	    if (l == k) {
		goto L20;
	    }
	    a[l + j * a_dim1] = a[k + j * a_dim1];
	    a[k + j * a_dim1] = t;
L20:
	    i__3 = n - k;
	    SlfNumDaxpy(i__3, &t, &a[k + 1 + k * a_dim1], 1, &a[k + 1 + j * 
		    a_dim1], 1);
/* L30: */
	}
	goto L50;
L40:
	*info = k;
L50:
/* L60: */
	;
    }
L70:
    ipvt[n] = n;
    if (a[n + n * a_dim1] == 0.) {
	*info = n;
    }
    return 0;
} /* dgefa_ */
int SlfNumDgbsl(double *abd, sint32_t lda, sint32_t n, 
	sint32_t ml, sint32_t mu, sint32_t *ipvt, double *b, sint32_t job)
{
    /* System generated locals */
    sint32_t abd_dim1, abd_offset, i__1, i__2, i__3;

    /* Local variables */
    static sint32_t k, l, m;
    static double t;
    static sint32_t kb, la, lb, lm, nm1;
    extern double ddot_org(sint32_t *, double *, sint32_t *, double *, 
	    sint32_t *);
    extern /* Subroutine */ int daxpy_org(sint32_t *, double *, double *, 
	    sint32_t *, double *, sint32_t *);

/* ***BEGIN PROLOGUE  DGBSL */
/* ***PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using */
/*            the factors computed by DGBCO or DGBFA. */
/* ***CATEGORY  D2A2 */
/* ***TYPE      DOUBLE PRECISION (SGBSL-S, DGBSL-D, CGBSL-C) */
/* ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     DGBSL solves the double precision band system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by DGBCO or DGBFA. */

/*     On Entry */

/*        ABD     DOUBLE PRECISION(LDA, N) */
/*                the output from DGBCO or DGBFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  ABD . */

/*        N       INTEGER */
/*                the order of the original matrix. */

/*        ML      INTEGER */
/*                number of diagonals below the main diagonal. */

/*        MU      INTEGER */
/*                number of diagonals above the main diagonal. */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from DGBCO or DGBFA. */

/*        B       DOUBLE PRECISION(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B , where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically this indicates singularity */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if DGBCO has set RCOND .GT. 0.0 */
/*        or DGBFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z) */
/*           IF (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0) */
/*        10 CONTINUE */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  DAXPY, DDOT */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DGBSL */

/* ***FIRST EXECUTABLE STATEMENT  DGBSL */
    /* Parameter adjustments */
    abd_dim1 = lda;
    abd_offset = 1 + abd_dim1;
    abd -= abd_offset;
    --ipvt;
    --b;

    /* Function Body */
    m = mu + ml + 1;
    nm1 = n - 1;
    if (job != 0) {
	goto L50;
    }

/*        JOB = 0 , SOLVE  A * X = B */
/*        FIRST SOLVE L*Y = B */

    if (ml == 0) {
	goto L30;
    }
    if (nm1 < 1) {
	goto L30;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
/* Computing MIN */
	i__2 = ml, i__3 = n - k;
	lm = MIN(i__2,i__3);
	l = ipvt[k];
	t = b[l];
	if (l == k) {
	    goto L10;
	}
	b[l] = b[k];
	b[k] = t;
L10:
	SlfNumDaxpy(lm, &t, &abd[m + 1 + k * abd_dim1], 1, &b[k + 1], 1);
/* L20: */
    }
L30:

/*        NOW SOLVE  U*X = Y */

    i__1 = n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = n + 1 - kb;
	b[k] /= abd[m + k * abd_dim1];
	lm = MIN(k,m) - 1;
	la = m - lm;
	lb = k - lm;
	t = -b[k];
	SlfNumDaxpy(lm, &t, &abd[la + k * abd_dim1], 1, &b[lb], 1);
/* L40: */
    }
    goto L100;
L50:

/*        JOB = NONZERO, SOLVE  TRANS(A) * X = B */
/*        FIRST SOLVE  TRANS(U)*Y = B */

    i__1 = n;
    for (k = 1; k <= i__1; ++k) {
	lm = MIN(k,m) - 1;
	la = m - lm;
	lb = k - lm;
	t = SlfNumDdot(lm, &abd[la + k * abd_dim1], 1, &b[lb], 1);
	b[k] = (b[k] - t) / abd[m + k * abd_dim1];
/* L60: */
    }

/*        NOW SOLVE TRANS(L)*X = Y */

    if (ml == 0) {
	goto L90;
    }
    if (nm1 < 1) {
	goto L90;
    }
    i__1 = nm1;
    for (kb = 1; kb <= i__1; ++kb) {
	k = n - kb;
/* Computing MIN */
	i__2 = ml, i__3 = n - k;
	lm = MIN(i__2,i__3);
	b[k] += SlfNumDdot(lm, &abd[m + 1 + k * abd_dim1], 1, &b[k + 1], 1);
	l = ipvt[k];
	if (l == k) {
	    goto L70;
	}
	t = b[l];
	b[l] = b[k];
	b[k] = t;
L70:
/* L80: */
	;
    }
L90:
L100:
    return 0;
} /* dgbsl_ */
double SlfNumDdot(sint32_t n, double *dx, sint32_t incx, double *dy, sint32_t incy)
{
    /* System generated locals */
    sint32_t i__1, i__2;
    double ret_val;

    /* Local variables */
    static sint32_t i__, m, ix, iy, ns, mp1;

/* ***BEGIN PROLOGUE  DDOT */
/* ***PURPOSE  Compute the inner product of two vectors. */
/* ***CATEGORY  D1A4 */
/* ***TYPE      DOUBLE PRECISION (SDOT-S, DDOT-D, CDOTU-C) */
/* ***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */
/*       DY  double precision vector with N elements */
/*     INCY  storage spacing between elements of DY */

/*     --Output-- */
/*     DDOT  double precision dot product (zero if N .LE. 0) */

/*     Returns the dot product of double precision DX and DY. */
/*     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY), */
/*     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is */
/*     defined in a similar way using INCY. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920310  Corrected definition of LX in DESCRIPTION.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DDOT */
/* ***FIRST EXECUTABLE STATEMENT  DDOT */
    /* Parameter adjustments */
    --dy;
    --dx;

    /* Function Body */
    ret_val = 0.;
    if (n <= 0) {
	return ret_val;
    }
    if (incx == incy) {
	if ((i__1 = incx - 1) < 0) {
	    goto L5;
	} else if (i__1 == 0) {
	    goto L20;
	} else {
	    goto L60;
	}
    }

/*     Code for unequal or nonpositive increments. */

L5:
    ix = 1;
    iy = 1;
    if (incx < 0) {
	ix = (-(n) + 1) * incx + 1;
    }
    if (incy < 0) {
	iy = (-(n) + 1) * incy + 1;
    }
    i__1 = n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += dx[ix] * dy[iy];
	ix += incx;
	iy += incy;
/* L10: */
    }
    return ret_val;

/*     Code for both increments equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 5. */

L20:
    m = n % 5;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += dx[i__] * dy[i__];
/* L30: */
    }
    if (n < 5) {
	return ret_val;
    }
L40:
    mp1 = m + 1;
    i__1 = n;
    for (i__ = mp1; i__ <= i__1; i__ += 5) {
	ret_val = ret_val + dx[i__] * dy[i__] + dx[i__ + 1] * dy[i__ + 1] + 
		dx[i__ + 2] * dy[i__ + 2] + dx[i__ + 3] * dy[i__ + 3] + dx[
		i__ + 4] * dy[i__ + 4];
/* L50: */
    }
    return ret_val;

/*     Code for equal, positive, non-unit increments. */

L60:
    ns = n * incx;
    i__1 = ns;
    i__2 = incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	ret_val += dx[i__] * dy[i__];
/* L70: */
    }
    return ret_val;
} /* ddot_ */
int SlfNumDgesl(double *a, sint32_t lda, sint32_t n, sint32_t *ipvt, double *b, sint32_t job)
{
    /* System generated locals */
    sint32_t a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static sint32_t k, l;
    static double t;
    static sint32_t kb, nm1;

/* ***BEGIN PROLOGUE  DGESL */
/* ***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the */
/*            factors computed by DGECO or DGEFA. */
/* ***CATEGORY  D2A1 */
/* ***TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C) */
/* ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     DGESL solves the double precision system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by DGECO or DGEFA. */

/*     On Entry */

/*        A       DOUBLE PRECISION(LDA, N) */
/*                the output from DGECO or DGEFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from DGECO or DGEFA. */

/*        B       DOUBLE PRECISION(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B  where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically this indicates singularity */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if DGECO has set RCOND .GT. 0.0 */
/*        or DGEFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL DGECO(A,LDA,N,IPVT,RCOND,Z) */
/*           IF (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL DGESL(A,LDA,N,IPVT,C(1,J),0) */
/*        10 CONTINUE */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  DAXPY, DDOT */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  DGESL */

/* ***FIRST EXECUTABLE STATEMENT  DGESL */
    /* Parameter adjustments */
    a_dim1 = lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;
    --b;

    /* Function Body */
    nm1 = n - 1;
    if (job != 0) {
	goto L50;
    }

/*        JOB = 0 , SOLVE  A * X = B */
/*        FIRST SOLVE  L*Y = B */

    if (nm1 < 1) {
	goto L30;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	l = ipvt[k];
	t = b[l];
	if (l == k) {
	    goto L10;
	}
	b[l] = b[k];
	b[k] = t;
L10:
	i__2 = n - k;
	SlfNumDaxpy(i__2, &t, &a[k + 1 + k * a_dim1], 1, &b[k + 1], 1);
/* L20: */
    }
L30:

/*        NOW SOLVE  U*X = Y */

    i__1 = n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = n + 1 - kb;
	b[k] /= a[k + k * a_dim1];
	t = -b[k];
	i__2 = k - 1;
	SlfNumDaxpy(i__2, &t, &a[k * a_dim1 + 1], 1, &b[1], 1);
/* L40: */
    }
    goto L100;
L50:

/*        JOB = NONZERO, SOLVE  TRANS(A) * X = B */
/*        FIRST SOLVE  TRANS(U)*Y = B */

    i__1 = n;
    for (k = 1; k <= i__1; ++k) {
	i__2 = k - 1;
	t = SlfNumDdot(i__2, &a[k * a_dim1 + 1], 1, &b[1], 1);
	b[k] = (b[k] - t) / a[k + k * a_dim1];
/* L60: */
    }

/*        NOW SOLVE TRANS(L)*X = Y */

    if (nm1 < 1) {
	goto L90;
    }
    i__1 = nm1;
    for (kb = 1; kb <= i__1; ++kb) {
	k = n - kb;
	i__2 = n - k;
	b[k] += SlfNumDdot(i__2, &a[k + 1 + k * a_dim1], 1, &b[k + 1], 1);
	l = ipvt[k];
	if (l == k) {
	    goto L70;
	}
	t = b[l];
	b[l] = b[k];
	b[k] = t;
L70:
/* L80: */
	;
    }
L90:
L100:
    return 0;
} /* dgesl_ */
double SlfNumPowDi(double x, sint32_t n) {

    double pow;
    unsigned long u;

    pow = 1.;

    if(n != 0) {
	    if(n < 0) {
		    n = -n;
		    x = 1/NOT_ZERO(x);
		}
	    for(u = n; ; ) {

            if(u & 01)
			    pow *= x;
		    if(u >>= 1)
			    x *= x;
		    else
			    break;
		}
	}
    return(pow);
}
/*-------------------------------------------------------------------------
void   FilterDigPar(SSlfNumFiltDig *ps, uint8_t n,double *pa, double *pb);
void   FilterDigInit(SSlfNumFiltDig *ps, uint8_t n,double *px, double *py);
double FilterDigCalc(SSlfNumFiltDig *ps, double xact);

Mir der Funktion FilterDigPar werden alle xi und yi null gesetzt
Wenn die Ordnung n angegeben, dann müssen die Parameter n+1 lang sein
n=2  a0=>a[0],a1=>a[1],a2=>a[2] => double a[n+1];

  y    b0 + b1*z^-1 + ... bn*z^-n
  -- = --------------------------
  x    a0 + a1*z^-1 + ... an*z^-n

         1.   nb               na
  y(i) = -- ( sum(x(i-j)*bj) - sum(y(i-j)*aj) )
         a0   j=0              j=1
-------------------------------------------------------------------------*/
//#define SLF_NUM_MAX_FILT_ORDER    5
//typedef struct tag_SSlfNumFiltDig {
//    uint8_t        n;
//    double       x[SLF_NUM_MAX_FILT_ORDER];
//    double       y[SLF_NUM_MAX_FILT_ORDER];
//    double       a[SLF_NUM_MAX_FILT_ORDER];
//    double       b[SLF_NUM_MAX_FILT_ORDER];
//}SSlfNumFiltDig;
void   SlfNumFiltDigPar(SSlfNumFiltDig *ps,double *pa,uint8_t na,double *pb,uint8_t nb)
{
  uint8_t i;
  ps->na = MIN(na,SLF_NUM_MAX_FILT_ORDER);
  for(i=0;i<(ps->na+1);i++)
  {
    ps->a[i] = pa[i];
    ps->y[i] = 0.0;
  }
  ps->a[0] = NOT_ZERO(ps->a[0]); 

  ps->nb = MIN(nb,SLF_NUM_MAX_FILT_ORDER);
  for(i=0;i<(ps->nb+1);i++)
  {
    ps->b[i] = pb[i];
    ps->x[i] = 0.0;
  }
}
void   SlfNumFiltDigInit(SSlfNumFiltDig *ps,double *px, uint8_t nx, double *py, uint8_t ny)
{
  uint8_t i;
  uint8_t n1 = MIN(nx,SLF_NUM_MAX_FILT_ORDER);
  for(i=0;i<(n1+1);i++)
  {
    ps->x[i] = px[i];
  }
  n1 = MIN(ny,SLF_NUM_MAX_FILT_ORDER);
  for(i=0;i<(n1+1);i++)
  {
    ps->y[i] = py[i];
  }
}
double SlfNumFiltDigCalc(SSlfNumFiltDig *ps, double xact)
{
  uint8_t  i;
  for(i=ps->na;i>0;i--)
  {
    ps->y[i]=ps->y[i-1];
  }
  ps->y[0] = 0.0;
  for(i=ps->nb;i>0;i--)
  {
    ps->x[i]=ps->x[i-1];
  }
  ps->x[0] = xact;

  for(i=0;i<(ps->nb+1);i++)
  {  
    ps->y[0] += ps->x[i]*ps->b[i];
  }
  for(i=1;i<(ps->na+1);i++)
  {  
    ps->y[0] -= ps->y[i]*ps->a[i];
  }
  ps->y[0] /= ps->a[0];

  return ps->y[0];
}
/*-------------------------------------------------------------------------
void   SlfNumPt1FiltInit(SSlfNumFiltDig *ps, double t1, double dt)
double FilterDigCalc(SSlfNumFiltDig *ps, double xact);

Init PT1-Filter for digital-Filter use FilterDigCalc() for Calc
-------------------------------------------------------------------------*/
void SlfNumPt1FiltInit(SSlfNumFiltDig *ps, double t1, double dt, double y0)
{
  ps->na = 1;
  ps->nb = 1;
  ps->a[0] = 1.0;
  ps->a[1] = -exp(-dt/t1);
  ps->b[0] = 0.0;
  ps->b[1] = 1.0-exp(-dt/t1);

  ps->y[0] = 0.0;
  ps->y[1] = y0;
  ps->x[0] = 0.0;
  ps->x[1] = y0;
}

#endif