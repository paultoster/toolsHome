#ifndef SLF_NUM_H_INCLUDED
#define SLF_NUM_H_INCLUDED
 
#include "SlfBasic.h"


// Abfrage auf Länegen, Monotonie, etc
// Auch der Error-Text ist damit verknüpft
//========================================
#ifdef _DEBUG
    #ifndef SLS_NUM_PROOF_FUNCTION
        #define SLS_NUM_PROOF_FUNCTION
    #endif
#endif
#ifdef NDEBUG
    #ifdef SLS_NUM_PROOF_FUNCTION
        #undef SLS_NUM_PROOF_FUNCTION
    #endif
#endif
#if !defined NDEBUG && !defined _DEBUG
    #ifndef SLS_NUM_PROOF_FUNCTION
        #define SLS_NUM_PROOF_FUNCTION
    #endif
#endif

#ifdef SLS_NUM_PROOF_FUNCTION
extern char *pSlfNumErrMess;
#endif


typedef double **Matrix_t;
typedef double *Vector_t;
typedef uint32_t uarray;
typedef sint32_t iarray;
#define MAX_UARRAY MAX_UINT32

#define	SlfNumAllocate(n, type)	\
	((type *) SlfNumAllocateFunc(n, sizeof(type), "type"))


#define HEADER(a)	( ((struct array_header *) a) - 1 )

#define	GET_NDIMS(a)	(HEADER(a)->ndims)
#define GET_NROWS(a)	(HEADER(a)->nrows)
#define GET_NCOLS(a)	(HEADER(a)->ncols)
#define ISLINKED(a)     (HEADER(a)->link == 1)
#define	ISVECTOR(a)	    (GET_NDIMS(a) == 1)
#define	ISMATRIX(a)	    (GET_NDIMS(a) == 2)
#define SET_NROWS(a,n)  (HEADER(a)->nrows = (uarray)n)
#define SET_NCOLS(a,n)  (HEADER(a)->ncols = (uarray)n)


/* Note: this structure is prepended at the beginning of a Vector_t, and causes
   the Vector_t data type to be 32-byte aligned, but not 64-byte aligned.
   If this were a problem, filler could be filler[5] (or more) instead. 
   --Sharon Perl, 12/17/98. */

struct array_header {
	uint8_t	ndims;	/* 1 = vector, 2 = matrix */
	uarray	nrows;
	uarray	ncols;
	uint8_t	link;   /* 0: no link, 1: linked to vektor */
};
typedef struct tag_SSlfVec {
    char         *pName;
    char         *pUnit;
    char         *pComment;
    Vector_t     pVec;
}SSlfVec;
typedef struct tag_SSlfMat {
    char         *pName;
    char         *pUnit;
    char         *pComment;
    Matrix_t     pMat;
}SSlfMat;

#ifdef __cplusplus
extern "C" {
#endif
// Build/Destroy Vector_t/Matrix_t
//============================

Vector_t	NewVector(uarray r);
// vec[0 ... r-1]

Matrix_t  NewMatrix(uarray r,uarray c);
// mat[0 ... r-1][0 ... c-1] => mat[Zeile][Spalte]

Matrix_t  LinkMatrix(double *pVec,uarray r,uarray c);
// Links a Matrix_t as onedimensional vector to Matrixstruct
// m[i][j] = pVec[i*c+j]

void FreeVector(Vector_t v);
// destroy vector

void FreeMatrix(Matrix_t m);
// destroy matrix and linked matrix


Vector_t VectorCopy(register Vector_t v);
Matrix_t MatrixCopy(register Matrix_t m);

void VectorGet(Vector_t v,Vector_t vres);

Vector_t VectorCopyD(double *pdval,uarray n);
Matrix_t MatrixCopyD(double *pdval,uarray nrow,uarray ncol,uint8_t type);

// Vector_t/Matrix_t Calculaton
//=========================

void ZeroVector(Vector_t v);
void ZeroMatrix(Matrix_t m);
void FillMatrix(Matrix_t m,double fill);

double InnerProduct(register Vector_t v1,register Vector_t  v2);

// mres = m1 *m2;
void MatrixMultiply(register Matrix_t m1,register Matrix_t  m2,register Matrix_t  result);

// vres = m * v
void MatrixTimesVector(Matrix_t m,Vector_t v,Vector_t vres);

// vres' = v' * m
void VectorTimesMatrix(Vector_t v,Matrix_t m,Vector_t vres);

void ScalarTimesVector(double s,Vector_t v,Vector_t vres);
void ScalarTimesMatrix(double s,Matrix_t m,Matrix_t mres);

double QuadraticForm(register Vector_t v,register Matrix_t  m);

double InvertMatrix(Matrix_t ym,Matrix_t rm);
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

void VectorFactorOffset(double factor,double offset,Vector_t v);
void VectorAdd(Vector_t va,Vector_t vb, Vector_t vres);
void VectorSub(Vector_t va,Vector_t vb, Vector_t vres);
void VectorAddScalar(double s,Vector_t v,Vector_t vres);

void MatrixAddScalar(double s,Matrix_t v,Matrix_t vres);

// vio = vio + s*vadd;
void VectorMultAdd(double s,Vector_t vadd, Vector_t vio);

// vio = vio - s*vsub;
void VectorMultSub(double s,Vector_t vsub, Vector_t vio); 

// mio = mio + s*madd;
void MatrixMultAdd(double s,Matrix_t madd, Matrix_t mio);

// mio = mio - s*msub;
void MatrixMultSub(double s,Matrix_t msub, Matrix_t mio); 

void VectorDivideScalar(double s,Vector_t v,Vector_t vres);

void MakeTildeMatrix(Vector_t v,Matrix_t mres);
void MakeTransponeTildeMatrixMultTildeMatrix(Vector_t v,Matrix_t mres);

void MakeVectorMultTransponeVektor(Vector_t v,Vector_t vtot,Matrix_t mres);

void MatrixAdd(Matrix_t ma,Matrix_t mb,Matrix_t mres);
void MatrixSub(Matrix_t ma,Matrix_t mb,Matrix_t mres);

// Kopiere Vektor/Matrix_t ab der angegebenen Stelle
//
void PutMatrixToMatrix(Matrix_t m, Matrix_t mres, uarray irow, uarray icol);
void PutVectorToMatrix(Vector_t v, Matrix_t mres, uarray irow, uarray icol);
void PutVectorToVector(Vector_t v, Vector_t vres, uarray irow);

void PutVectorToVectorMax(Vector_t v, Vector_t vres, uarray nmax);
void PutMatrixToMatrixMax(Matrix_t m, Matrix_t mres, uarray nrowmax, uarray ncolmax);

void TransponeMatrix(Matrix_t m,Matrix_t mres);

/* normalise Vector_t */
double VectorNorm(Vector_t v);

/* absulte each vector value */
void VectorAbs(Vector_t v);

void UnifyVector(Vector_t v, Vector_t e);

// vres = va x vb
void VectorCrossProduct(Vector_t va,Vector_t vb, Vector_t vres);

// Memory allocation, Do not call this function directly, use SlfNumAllocate(n, type)
void *SlfNumAllocateFunc(uint32_t nitems, uint32_t itemsize, char *tname);

#if 0
error_t ludcmp(Matrix_t a, uint32_t n, uint32_t *indx, double *d);
//==============================================================================
// LU-Decomposition
// =================
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

error_t ludcmpb(Matrix_t a, uint32_t n, uint32_t ml, uint32_t mu, uint32_t *indx, double *d);
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

error_t ludcmph(Matrix_t a,uint32_t n, uint32_t lb, uint32_t *indx, double *d);
//==================================================================================
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

void elmhes(uint32_t n,uint32_t low,uint32_t igh,Matrix_t a,uint32_t *iphes);
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
CC
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
#endif

status_t SlfNumInterp1D(uint8_t iflag
                       ,double *x   
                       ,double *y
                       ,uint32_t nxy
                       ,double x0
                       ,double *y0
                       ,uint32_t *piact
                       ,uint8_t  iord
                       ,uint8_t  iabl);
/*******************************************************************
*
*       iflag           Inittialisierung 0/1
*       x[nxy]          x-Vektor, muss monoton steigend sein
*       y[nxy]          y-Vektor
*       x0              x-Wert an dem y-Wert bestimmt wird
*       y0              y-Wert, der zurueckgegeben wird
*       iact            aktuelle Stuetzstelle der letzten Berechnung
*                       wird als Ausgangswert benutzt, um schneller
*                       den Wertebereich in x zu finden (z.B. wenn x0
*                       fortlaufend steigt. iact wird dann aktuallisiert
*       iord            = 0     Konstantwert swischen den Stuetzstellen
*                       = 1     lineare Interpolation
*
*       iabl            = 0,1   i.Ableitung
*
*       return OKAY;    bei initialisierung wird geprüft
*       return NOT_OKAY Fehler, x-Vektor nicht monoton steigend (nur bei init)
*                       
********************************************************************/

int SlfNumInterp2D(uint8_t iflag,double *xvec,size_t nx,double *yvec,size_t ny,
                  Matrix_t zmat,double x0,double y0,double *z0);
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
*       return 1        Fehler zuwenig Werte == 0
*       return 2        Fehler nicht monoton steigend
*       return 3        Länge zmat ist kleiner als xvec oder yvec
********************************************************************/

#if 0 // Noch nicht fertig
typedef struct SSlfNumSpline3_tag {

    Vector_t xvec;
    Vector_t yvec;
    Vector_t zvec;
	uint32_t n;

} SSlfNumSpline3;

SSlfNumSpline3 *SlfNumSpline3Coef(uint32_t n,double *xvec, double *yvec);
double         SlfNumSpline3(SSlfNumSpline3 *pZ,double x);
void           SlfNumSpline3Free(SSlfNumSpline3 *pZ);
#endif
//===================================================================
// natural kubic spline
//
// bildet Koeffizienten ür Spline-Berechnung
//
// SSlfNumSpline3 *pZ = SlfNumSpline3Coef(uint32_t n,double *xvec, double *yvec);
//
// Berechnung des spline
//
// double y = SlfNumSpline3(SSlfNumSpline3 *pZ,double x);
//
// Löschen der Koeffizienten
//
// viod SlfNumSpline3Free(SSlfNumSpline3 *pZ);
//
//===================================================================
typedef struct SSlfNumStep_tag {

    double xa;
    double xb;
    double ya;
    double yb;
    double dx;
    double dy;
    double x;
    double y;
    double yp;

} SSlfNumStep;

void SlfNumStepSet(SSlfNumStep *s,double xa,double xb,double ya,double yb);
void SlfNumStepCalc(SSlfNumStep *s,double x);
//===================================================================
// Step-Funktion
// 
// x <= xa  : y  = ya
//            yp = 0.0
// x >= xb  : y  = yb
//            yp = 0.0
// ansonsten: y  = ya + (yb-ya)*[(x-xa)/(xb-xa)]^2*{3-2*[(x-xa)/(xb-xa)]}
//            yp = 6*(yb-ya)/(xb-xa)*[(x-xa)/(xb-xa)]*{1-[(x-xa)/(xb-xa)]}
//
//
// 
// SlfStepSet(SSlfNumStep *s,double xa,double xb,double ya,double yb)
// SlfStepCalc(SSlfNumStep *s,double x)
// Ergebnis: s.y und s.yp
//====================================================================

double SlfNumAbs(double x);
//===============================================
//  Abs(x)
//===============================================

sint8_t SlfNumSgn(double x);
//===============================================
//  Signum(x)
//===============================================

double SlfNumLim(double val, double limUp, double limLow);
/*****************************************************************************
  @fn                   SlfNumLim1(double *pVal, double *pLimUp, double *pLimLow)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      LimUp   upper border
  @param[in]      LimLow  lower border
   
  @return         limited Value between -Lim and Lim
*****************************************************************************/

double SlfNumLim1(double val, double lim);
/*****************************************************************************
  @fn              double SlfNumLim1(double Val, double Lim)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      Lim     upper border, lower border => *(-1.f)
   
  @return         limited Value between -Lim and Lim
*****************************************************************************/

double SlfNumATan2(double y,double x);
//===============================================
//  Arcus tangens y/x
//===============================================

double SlfNumSqrt2(double a, double b);
//===============================================
//  m = sqrt(a^2+b^2)
//===============================================
double SlfNumDmaxnorm(sint32_t n, double *v, double *w);
/* ----------------------------------------------------------------------- */
/* This function routine computes the weighted max-norm */
/* of the vector of length N contained in the array V, with weights */
/* contained in the array w of length N: */
/*   DMNORM = MAX(i=1,...,N) ABS(V(i))*W(i) */
/* ----------------------------------------------------------------------- */
int SlfNumDewset(sint32_t n, sint32_t itol, double *rtol, 
	double *atol, double *ycur, double *ewt);
/* ----------------------------------------------------------------------- */
/*  This subroutine sets the error weight vector EWT according to */
/*      EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N, */
/*  with the subscript on RTOL and/or ATOL possibly replaced by 1 above, */
/*  depending on the value of ITOL. */
/* ----------------------------------------------------------------------- */
double SlfNumDumach();
/* ----------------------------------------------------------------------- */
/* *Function Return Values: */
/*     A : the unit roundoff of the machine. */
/* ----------------------------------------------------------------------- */
int SlfNumDcfode(sint32_t meth, double *elco, double *tesco);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
int SlfNumDgbfa(double *abd, sint32_t lda, sint32_t n, 
	            sint32_t ml, sint32_t mu, sint32_t *ipvt, sint32_t *info);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
int SlfNumDscal(sint32_t n, double *da, double *dx, sint32_t incx);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
int SlfNumDaxpy(sint32_t n, double *da, double *dx, 
	            sint32_t incx, double *dy, sint32_t incy);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
sint32_t SlfNumIdamax(sint32_t n, double *dx, sint32_t incx);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
int SlfNumDgefa(double *a, sint32_t lda, sint32_t n, sint32_t *ipvt, sint32_t *info);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
double SlfNumDbnorm(sint32_t n, double *a, sint32_t nra, sint32_t ml, 
	                sint32_t mu, double *w);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
double SlfNumDfnorm(sint32_t n, double *a, double *w);
/* ----------------------------------------------------------------------- */
/* This function computes the norm of a full N by N matrix, */
/* stored in the array A, that is consistent with the weighted max-norm */
/* on vectors, with weights stored in the array W: */
/*   DFNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) ) */
/* ----------------------------------------------------------------------- */
double SlfNumDdot(sint32_t n, double *dx, sint32_t incx, double *dy, sint32_t incy);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
int SlfNumDgbsl(double *abd, sint32_t lda, sint32_t n, 
	sint32_t ml, sint32_t mu, sint32_t *ipvt, double *b, sint32_t job);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
int SlfNumDgesl(double *a, sint32_t lda, sint32_t n, sint32_t *ipvt, double *b, sint32_t job);
/* ----------------------------------------------------------------------- */
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
/* ----------------------------------------------------------------------- */
double SlfNumPowDi(double x, sint32_t n);
/* ----------------------------------------------------------------------- */
// y = x ^ n n: negativ or positiv
/* ----------------------------------------------------------------------- */

/*-------------------------------------------------------------------------
void   FilterDigPar(SSlfNumFiltDig *ps, uint8_t n,double *pa, double *pb);
void   FilterDigInit(SSlfNumFiltDig *ps, uint8_t n,double *px, double *py);
double FilterDigInit(SSlfNumFiltDig *ps, double xact);

Mir der Funktion FilterDigPar werden alle xi und yi null gesetzt

  y    b0 + b1*z^-1 + ... bn*z^-n
  -- = --------------------------
  x    a0 + a1*z^-1 + ... an*z^-n

         1.    n              n
  y(i) = -- ( sum(xi-j*bj) - sum(yi-j*aj) )
         a0   j=0            j=1
-------------------------------------------------------------------------*/
#define SLF_NUM_MAX_FILT_ORDER    5
typedef struct tag_SSlfNumFiltDig {
    uint8_t        na,nb;
    double       x[SLF_NUM_MAX_FILT_ORDER+1];
    double       y[SLF_NUM_MAX_FILT_ORDER+1];
    double       a[SLF_NUM_MAX_FILT_ORDER+1];
    double       b[SLF_NUM_MAX_FILT_ORDER+1];
}SSlfNumFiltDig;
void   SlfNumFiltDigPar(SSlfNumFiltDig *ps,double *pa, uint8_t na, double *pb, uint8_t nb);
void   SlfNumFiltDigInit(SSlfNumFiltDig *ps,double *px, uint8_t nx, double *py, uint8_t ny);
double SlfNumFiltDigCalc(SSlfNumFiltDig *ps, double xact);

void SlfNumPt1FiltInit(SSlfNumFiltDig *ps, double t1, double dt, double y0);
#ifdef __cplusplus
    }
#endif

#endif