/* radau5.f -- translated by f2c (version 20060506).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer nn, nn2, nn3, nn4;
    doublereal xsol, hsol, c2m1, c1m1;
} conra5_;

#define conra5_1 conra5_

struct {
    integer mle, mue, mbjac, mbb, mdiag, mdiff, mbdiag;
} linal_;

#define linal_1 linal_

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__3 = 3;
static doublereal c_b100 = .5;
static doublereal c_b215 = 81.;
static doublereal c_b216 = .33333333333333331;
static doublereal c_b217 = 9.;
static doublereal c_b227 = 1.;
static doublereal c_b325 = .8;
static doublereal c_b389 = .25;

/*<    >*/
/* Subroutine */ int radau5_(integer *n, U_fp fcn, doublereal *x, doublereal *
	y, doublereal *xend, doublereal *h__, doublereal *rtol, doublereal *
	atol, integer *itol, U_fp jac, integer *ijac, integer *mljac, integer 
	*mujac, U_fp mas, integer *imas, integer *mlmas, integer *mumas, U_fp 
	solout, integer *iout, doublereal *work, integer *lwork, integer *
	iwork, integer *liwork, doublereal *rpar, integer *ipar, integer *
	idid)
{
    /* System generated locals */
    integer work_dim1, iwork_dim1, i__1, i__2, i__3, i__4, i__5, i__6, i__7, 
	    i__8, i__9, i__10, i__11, i__12, i__13, i__14, i__15, i__16, 
	    i__17;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_wsle(cilist *), 
	    do_lio(integer *, integer *, char *, ftnlen), e_wsle(void);
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static integer i__, m1, m2, nm1, nit, iee1, ief1, lde1, ief2, ief3, iey0, 
	    iez1, iez2, iez3;
    static doublereal facl;
    static integer ndec, njac;
    static doublereal facr, safe;
    static integer ijob, nfcn;
    static logical pred;
    static doublereal hmax;
    static integer nmax;
    static doublereal thet, expm;
    static integer nsol;
    static doublereal quot;
    static integer iee2i, iee2r, ieip1, ieip2, nind1, nind2, nind3;
    static doublereal quot1, quot2;
    static integer iejac, ldjac;
    static logical jband;
    static integer iecon, iemas, ldmas, ieiph;
    static logical arret;
    static doublereal fnewt;
    static integer nstep;
    static doublereal tolst;
    static integer ldmas2, iescal, naccpt;
    extern /* Subroutine */ int radcor_(integer *, U_fp, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, U_fp, integer *, integer *,
	     integer *, U_fp, integer *, integer *, U_fp, integer *, integer *
	    , integer *, doublereal *, doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, integer *, integer *, logical *, 
	    integer *, integer *, integer *, logical *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, logical *, logical 
	    *, integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, doublereal *, integer *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, doublereal *, integer *);
    static integer nrejct;
    static logical implct;
    static integer istore;
    static logical startn;
    static doublereal uround;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___12 = { 0, 6, 0, 0, 0 };
    static cilist io___15 = { 0, 6, 0, 0, 0 };
    static cilist io___17 = { 0, 6, 0, 0, 0 };
    static cilist io___19 = { 0, 6, 0, 0, 0 };
    static cilist io___24 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, 0, 0 };
    static cilist io___31 = { 0, 6, 0, 0, 0 };
    static cilist io___33 = { 0, 6, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, 0, 0 };
    static cilist io___39 = { 0, 6, 0, 0, 0 };
    static cilist io___43 = { 0, 6, 0, 0, 0 };
    static cilist io___50 = { 0, 6, 0, 0, 0 };
    static cilist io___52 = { 0, 6, 0, 0, 0 };
    static cilist io___68 = { 0, 6, 0, 0, 0 };
    static cilist io___72 = { 0, 6, 0, 0, 0 };


/* ---------------------------------------------------------- */
/*     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC) */
/*     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS */
/*                     M*Y'=F(X,Y). */
/*     THE SYSTEM CAN BE (LINEARLY) IMPLICIT (MASS-MATRIX M .NE. I) */
/*     OR EXPLICIT (M=I). */
/*     THE METHOD USED IS AN IMPLICIT RUNGE-KUTTA METHOD (RADAU IIA) */
/*     OF ORDER 5 WITH STEP SIZE CONTROL AND CONTINUOUS OUTPUT. */
/*     CF. SECTION IV.8 */

/*     AUTHORS: E. HAIRER AND G. WANNER */
/*              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES */
/*              CH-1211 GENEVE 24, SWITZERLAND */
/*              E-MAIL:  Ernst.Hairer@math.unige.ch */
/*                       Gerhard.Wanner@math.unige.ch */

/*     THIS CODE IS PART OF THE BOOK: */
/*         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL */
/*         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS. */
/*         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 14, */
/*         SPRINGER-VERLAG 1991, SECOND EDITION 1996. */

/*     VERSION OF JULY 9, 1996 */
/*     (latest small correction: January 18, 2002) */

/*     INPUT PARAMETERS */
/*     ---------------- */
/*     N           DIMENSION OF THE SYSTEM */

/*     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE */
/*                 VALUE OF F(X,Y): */
/*                    SUBROUTINE FCN(N,X,Y,F,RPAR,IPAR) */
/*                    DOUBLE PRECISION X,Y(N),F(N) */
/*                    F(1)=...   ETC. */
/*                 RPAR, IPAR (SEE BELOW) */

/*     X           INITIAL X-VALUE */

/*     Y(N)        INITIAL VALUES FOR Y */

/*     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE) */

/*     H           INITIAL STEP SIZE GUESS; */
/*                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT, */
/*                 H=1.D0/(NORM OF F'), USUALLY 1.D-3 OR 1.D-5, IS GOOD. */
/*                 THIS CHOICE IS NOT VERY IMPORTANT, THE STEP SIZE IS */
/*                 QUICKLY ADAPTED. (IF H=0.D0, THE CODE PUTS H=1.D-6). */

/*     RTOL,ATOL   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY */
/*                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N. */

/*     ITOL        SWITCH FOR RTOL AND ATOL: */
/*                   ITOL=0: BOTH RTOL AND ATOL ARE SCALARS. */
/*                     THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF */
/*                     Y(I) BELOW RTOL*ABS(Y(I))+ATOL */
/*                   ITOL=1: BOTH RTOL AND ATOL ARE VECTORS. */
/*                     THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW */
/*                     RTOL(I)*ABS(Y(I))+ATOL(I). */

/*     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES */
/*                 THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y */
/*                 (THIS ROUTINE IS ONLY CALLED IF IJAC=1; SUPPLY */
/*                 A DUMMY SUBROUTINE IN THE CASE IJAC=0). */
/*                 FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM */
/*                    SUBROUTINE JAC(N,X,Y,DFY,LDFY,RPAR,IPAR) */
/*                    DOUBLE PRECISION X,Y(N),DFY(LDFY,N) */
/*                    DFY(1,1)= ... */
/*                 LDFY, THE COLUMN-LENGTH OF THE ARRAY, IS */
/*                 FURNISHED BY THE CALLING PROGRAM. */
/*                 IF (MLJAC.EQ.N) THE JACOBIAN IS SUPPOSED TO */
/*                    BE FULL AND THE PARTIAL DERIVATIVES ARE */
/*                    STORED IN DFY AS */
/*                       DFY(I,J) = PARTIAL F(I) / PARTIAL Y(J) */
/*                 ELSE, THE JACOBIAN IS TAKEN AS BANDED AND */
/*                    THE PARTIAL DERIVATIVES ARE STORED */
/*                    DIAGONAL-WISE AS */
/*                       DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J). */

/*     IJAC        SWITCH FOR THE COMPUTATION OF THE JACOBIAN: */
/*                    IJAC=0: JACOBIAN IS COMPUTED INTERNALLY BY FINITE */
/*                       DIFFERENCES, SUBROUTINE "JAC" IS NEVER CALLED. */
/*                    IJAC=1: JACOBIAN IS SUPPLIED BY SUBROUTINE JAC. */

/*     MLJAC       SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN: */
/*                    MLJAC=N: JACOBIAN IS A FULL MATRIX. THE LINEAR */
/*                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION. */
/*                    0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN */
/*                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW */
/*                       THE MAIN DIAGONAL). */

/*     MUJAC       UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON- */
/*                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL). */
/*                 NEED NOT BE DEFINED IF MLJAC=N. */

/*     ----   MAS,IMAS,MLMAS, AND MUMAS HAVE ANALOG MEANINGS      ----- */
/*     ----   FOR THE "MASS MATRIX" (THE MATRIX "M" OF SECTION IV.8): - */

/*     MAS         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE MASS- */
/*                 MATRIX M. */
/*                 IF IMAS=0, THIS MATRIX IS ASSUMED TO BE THE IDENTITY */
/*                 MATRIX AND NEEDS NOT TO BE DEFINED; */
/*                 SUPPLY A DUMMY SUBROUTINE IN THIS CASE. */
/*                 IF IMAS=1, THE SUBROUTINE MAS IS OF THE FORM */
/*                    SUBROUTINE MAS(N,AM,LMAS,RPAR,IPAR) */
/*                    DOUBLE PRECISION AM(LMAS,N) */
/*                    AM(1,1)= .... */
/*                    IF (MLMAS.EQ.N) THE MASS-MATRIX IS STORED */
/*                    AS FULL MATRIX LIKE */
/*                         AM(I,J) = M(I,J) */
/*                    ELSE, THE MATRIX IS TAKEN AS BANDED AND STORED */
/*                    DIAGONAL-WISE AS */
/*                         AM(I-J+MUMAS+1,J) = M(I,J). */

/*     IMAS       GIVES INFORMATION ON THE MASS-MATRIX: */
/*                    IMAS=0: M IS SUPPOSED TO BE THE IDENTITY */
/*                       MATRIX, MAS IS NEVER CALLED. */
/*                    IMAS=1: MASS-MATRIX  IS SUPPLIED. */

/*     MLMAS       SWITCH FOR THE BANDED STRUCTURE OF THE MASS-MATRIX: */
/*                    MLMAS=N: THE FULL MATRIX CASE. THE LINEAR */
/*                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION. */
/*                    0<=MLMAS<N: MLMAS IS THE LOWER BANDWITH OF THE */
/*                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW */
/*                       THE MAIN DIAGONAL). */
/*                 MLMAS IS SUPPOSED TO BE .LE. MLJAC. */

/*     MUMAS       UPPER BANDWITH OF MASS-MATRIX (>= NUMBER OF NON- */
/*                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL). */
/*                 NEED NOT BE DEFINED IF MLMAS=N. */
/*                 MUMAS IS SUPPOSED TO BE .LE. MUJAC. */

/*     SOLOUT      NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE */
/*                 NUMERICAL SOLUTION DURING INTEGRATION. */
/*                 IF IOUT=1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP. */
/*                 SUPPLY A DUMMY SUBROUTINE IF IOUT=0. */
/*                 IT MUST HAVE THE FORM */
/*                    SUBROUTINE SOLOUT (NR,XOLD,X,Y,CONT,LRC,N, */
/*                                       RPAR,IPAR,IRTRN) */
/*                    DOUBLE PRECISION X,Y(N),CONT(LRC) */
/*                    .... */
/*                 SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH */
/*                    GRID-POINT "X" (THEREBY THE INITIAL VALUE IS */
/*                    THE FIRST GRID-POINT). */
/*                 "XOLD" IS THE PRECEEDING GRID-POINT. */
/*                 "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN */
/*                    IS SET <0, RADAU5 RETURNS TO THE CALLING PROGRAM. */

/*          -----  CONTINUOUS OUTPUT: ----- */
/*                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION */
/*                 FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH */
/*                 THE FUNCTION */
/*                        >>>   CONTR5(I,S,CONT,LRC)   <<< */
/*                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH */
/*                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE */
/*                 S SHOULD LIE IN THE INTERVAL [XOLD,X]. */
/*                 DO NOT CHANGE THE ENTRIES OF CONT(LRC), IF THE */
/*                 DENSE OUTPUT FUNCTION IS USED. */

/*     IOUT        SWITCH FOR CALLING THE SUBROUTINE SOLOUT: */
/*                    IOUT=0: SUBROUTINE IS NEVER CALLED */
/*                    IOUT=1: SUBROUTINE IS AVAILABLE FOR OUTPUT. */

/*     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK". */
/*                 WORK(1), WORK(2),.., WORK(20) SERVE AS PARAMETERS */
/*                 FOR THE CODE. FOR STANDARD USE OF THE CODE */
/*                 WORK(1),..,WORK(20) MUST BE SET TO ZERO BEFORE */
/*                 CALLING. SEE BELOW FOR A MORE SOPHISTICATED USE. */
/*                 WORK(21),..,WORK(LWORK) SERVE AS WORKING SPACE */
/*                 FOR ALL VECTORS AND MATRICES. */
/*                 "LWORK" MUST BE AT LEAST */
/*                             N*(LJAC+LMAS+3*LE+12)+20 */
/*                 WHERE */
/*                    LJAC=N              IF MLJAC=N (FULL JACOBIAN) */
/*                    LJAC=MLJAC+MUJAC+1  IF MLJAC<N (BANDED JAC.) */
/*                 AND */
/*                    LMAS=0              IF IMAS=0 */
/*                    LMAS=N              IF IMAS=1 AND MLMAS=N (FULL) */
/*                    LMAS=MLMAS+MUMAS+1  IF MLMAS<N (BANDED MASS-M.) */
/*                 AND */
/*                    LE=N               IF MLJAC=N (FULL JACOBIAN) */
/*                    LE=2*MLJAC+MUJAC+1 IF MLJAC<N (BANDED JAC.) */

/*                 IN THE USUAL CASE WHERE THE JACOBIAN IS FULL AND THE */
/*                 MASS-MATRIX IS THE INDENTITY (IMAS=0), THE MINIMUM */
/*                 STORAGE REQUIREMENT IS */
/*                             LWORK = 4*N*N+12*N+20. */
/*                 IF IWORK(9)=M1>0 THEN "LWORK" MUST BE AT LEAST */
/*                          N*(LJAC+12)+(N-M1)*(LMAS+3*LE)+20 */
/*                 WHERE IN THE DEFINITIONS OF LJAC, LMAS AND LE THE */
/*                 NUMBER N CAN BE REPLACED BY N-M1. */

/*     LWORK       DECLARED LENGTH OF ARRAY "WORK". */

/*     IWORK       INTEGER WORKING SPACE OF LENGTH "LIWORK". */
/*                 IWORK(1),IWORK(2),...,IWORK(20) SERVE AS PARAMETERS */
/*                 FOR THE CODE. FOR STANDARD USE, SET IWORK(1),.., */
/*                 IWORK(20) TO ZERO BEFORE CALLING. */
/*                 IWORK(21),...,IWORK(LIWORK) SERVE AS WORKING AREA. */
/*                 "LIWORK" MUST BE AT LEAST 3*N+20. */

/*     LIWORK      DECLARED LENGTH OF ARRAY "IWORK". */

/*     RPAR, IPAR  REAL AND INTEGER PARAMETERS (OR PARAMETER ARRAYS) WHICH */
/*                 CAN BE USED FOR COMMUNICATION BETWEEN YOUR CALLING */
/*                 PROGRAM AND THE FCN, JAC, MAS, SOLOUT SUBROUTINES. */

/* ---------------------------------------------------------------------- */

/*     SOPHISTICATED SETTING OF PARAMETERS */
/*     ----------------------------------- */
/*              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT WORK */
/*              WELL. THEY MAY BE DEFINED BY SETTING WORK(1),... */
/*              AS WELL AS IWORK(1),... DIFFERENT FROM ZERO. */
/*              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES: */

/*    IWORK(1)  IF IWORK(1).NE.0, THE CODE TRANSFORMS THE JACOBIAN */
/*              MATRIX TO HESSENBERG FORM. THIS IS PARTICULARLY */
/*              ADVANTAGEOUS FOR LARGE SYSTEMS WITH FULL JACOBIAN. */
/*              IT DOES NOT WORK FOR BANDED JACOBIAN (MLJAC<N) */
/*              AND NOT FOR IMPLICIT SYSTEMS (IMAS=1). */

/*    IWORK(2)  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS. */
/*              THE DEFAULT VALUE (FOR IWORK(2)=0) IS 100000. */

/*    IWORK(3)  THE MAXIMUM NUMBER OF NEWTON ITERATIONS FOR THE */
/*              SOLUTION OF THE IMPLICIT SYSTEM IN EACH STEP. */
/*              THE DEFAULT VALUE (FOR IWORK(3)=0) IS 7. */

/*    IWORK(4)  IF IWORK(4).EQ.0 THE EXTRAPOLATED COLLOCATION SOLUTION */
/*              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD. */
/*              IF IWORK(4).NE.0 ZERO STARTING VALUES ARE USED. */
/*              THE LATTER IS RECOMMENDED IF NEWTON'S METHOD HAS */
/*              DIFFICULTIES WITH CONVERGENCE (THIS IS THE CASE WHEN */
/*              NSTEP IS LARGER THAN NACCPT + NREJCT; SEE OUTPUT PARAM.). */
/*              DEFAULT IS IWORK(4)=0. */

/*       THE FOLLOWING 3 PARAMETERS ARE IMPORTANT FOR */
/*       DIFFERENTIAL-ALGEBRAIC SYSTEMS OF INDEX > 1. */
/*       THE FUNCTION-SUBROUTINE SHOULD BE WRITTEN SUCH THAT */
/*       THE INDEX 1,2,3 VARIABLES APPEAR IN THIS ORDER. */
/*       IN ESTIMATING THE ERROR THE INDEX 2 VARIABLES ARE */
/*       MULTIPLIED BY H, THE INDEX 3 VARIABLES BY H**2. */

/*    IWORK(5)  DIMENSION OF THE INDEX 1 VARIABLES (MUST BE > 0). FOR */
/*              ODE'S THIS EQUALS THE DIMENSION OF THE SYSTEM. */
/*              DEFAULT IWORK(5)=N. */

/*    IWORK(6)  DIMENSION OF THE INDEX 2 VARIABLES. DEFAULT IWORK(6)=0. */

/*    IWORK(7)  DIMENSION OF THE INDEX 3 VARIABLES. DEFAULT IWORK(7)=0. */

/*    IWORK(8)  SWITCH FOR STEP SIZE STRATEGY */
/*              IF IWORK(8).EQ.1  MOD. PREDICTIVE CONTROLLER (GUSTAFSSON) */
/*              IF IWORK(8).EQ.2  CLASSICAL STEP SIZE CONTROL */
/*              THE DEFAULT VALUE (FOR IWORK(8)=0) IS IWORK(8)=1. */
/*              THE CHOICE IWORK(8).EQ.1 SEEMS TO PRODUCE SAFER RESULTS; */
/*              FOR SIMPLE PROBLEMS, THE CHOICE IWORK(8).EQ.2 PRODUCES */
/*              OFTEN SLIGHTLY FASTER RUNS */

/*       IF THE DIFFERENTIAL SYSTEM HAS THE SPECIAL STRUCTURE THAT */
/*            Y(I)' = Y(I+M2)   FOR  I=1,...,M1, */
/*       WITH M1 A MULTIPLE OF M2, A SUBSTANTIAL GAIN IN COMPUTERTIME */
/*       CAN BE ACHIEVED BY SETTING THE PARAMETERS IWORK(9) AND IWORK(10). */
/*       E.G., FOR SECOND ORDER SYSTEMS P'=V, V'=G(P,V), WHERE P AND V ARE */
/*       VECTORS OF DIMENSION N/2, ONE HAS TO PUT M1=M2=N/2. */
/*       FOR M1>0 SOME OF THE INPUT PARAMETERS HAVE DIFFERENT MEANINGS: */
/*       - JAC: ONLY THE ELEMENTS OF THE NON-TRIVIAL PART OF THE */
/*              JACOBIAN HAVE TO BE STORED */
/*              IF (MLJAC.EQ.N-M1) THE JACOBIAN IS SUPPOSED TO BE FULL */
/*                 DFY(I,J) = PARTIAL F(I+M1) / PARTIAL Y(J) */
/*                FOR I=1,N-M1 AND J=1,N. */
/*              ELSE, THE JACOBIAN IS BANDED ( M1 = M2 * MM ) */
/*                 DFY(I-J+MUJAC+1,J+K*M2) = PARTIAL F(I+M1) / PARTIAL Y(J+K*M2) */
/*                FOR I=1,MLJAC+MUJAC+1 AND J=1,M2 AND K=0,MM. */
/*       - MLJAC: MLJAC=N-M1: IF THE NON-TRIVIAL PART OF THE JACOBIAN IS FULL */
/*                0<=MLJAC<N-M1: IF THE (MM+1) SUBMATRICES (FOR K=0,MM) */
/*                     PARTIAL F(I+M1) / PARTIAL Y(J+K*M2),  I,J=1,M2 */
/*                    ARE BANDED, MLJAC IS THE MAXIMAL LOWER BANDWIDTH */
/*                    OF THESE MM+1 SUBMATRICES */
/*       - MUJAC: MAXIMAL UPPER BANDWIDTH OF THESE MM+1 SUBMATRICES */
/*                NEED NOT BE DEFINED IF MLJAC=N-M1 */
/*       - MAS: IF IMAS=0 THIS MATRIX IS ASSUMED TO BE THE IDENTITY AND */
/*              NEED NOT BE DEFINED. SUPPLY A DUMMY SUBROUTINE IN THIS CASE. */
/*              IT IS ASSUMED THAT ONLY THE ELEMENTS OF RIGHT LOWER BLOCK OF */
/*              DIMENSION N-M1 DIFFER FROM THAT OF THE IDENTITY MATRIX. */
/*              IF (MLMAS.EQ.N-M1) THIS SUBMATRIX IS SUPPOSED TO BE FULL */
/*                 AM(I,J) = M(I+M1,J+M1)     FOR I=1,N-M1 AND J=1,N-M1. */
/*              ELSE, THE MASS MATRIX IS BANDED */
/*                 AM(I-J+MUMAS+1,J) = M(I+M1,J+M1) */
/*       - MLMAS: MLMAS=N-M1: IF THE NON-TRIVIAL PART OF M IS FULL */
/*                0<=MLMAS<N-M1: LOWER BANDWIDTH OF THE MASS MATRIX */
/*       - MUMAS: UPPER BANDWIDTH OF THE MASS MATRIX */
/*                NEED NOT BE DEFINED IF MLMAS=N-M1 */

/*    IWORK(9)  THE VALUE OF M1.  DEFAULT M1=0. */

/*    IWORK(10) THE VALUE OF M2.  DEFAULT M2=M1. */

/* ---------- */

/*    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16. */

/*    WORK(2)   THE SAFETY FACTOR IN STEP SIZE PREDICTION, */
/*              DEFAULT 0.9D0. */

/*    WORK(3)   DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED; */
/*              INCREASE WORK(3), TO 0.1 SAY, WHEN JACOBIAN EVALUATIONS */
/*              ARE COSTLY. FOR SMALL SYSTEMS WORK(3) SHOULD BE SMALLER */
/*              (0.001D0, SAY). NEGATIV WORK(3) FORCES THE CODE TO */
/*              COMPUTE THE JACOBIAN AFTER EVERY ACCEPTED STEP. */
/*              DEFAULT 0.001D0. */

/*    WORK(4)   STOPPING CRITERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1. */
/*              SMALLER VALUES OF WORK(4) MAKE THE CODE SLOWER, BUT SAFER. */
/*              DEFAULT MIN(0.03D0,RTOL(1)**0.5D0) */

/*    WORK(5) AND WORK(6) : IF WORK(5) < HNEW/HOLD < WORK(6), THEN THE */
/*              STEP SIZE IS NOT CHANGED. THIS SAVES, TOGETHER WITH A */
/*              LARGE WORK(3), LU-DECOMPOSITIONS AND COMPUTING TIME FOR */
/*              LARGE SYSTEMS. FOR SMALL SYSTEMS ONE MAY HAVE */
/*              WORK(5)=1.D0, WORK(6)=1.2D0, FOR LARGE FULL SYSTEMS */
/*              WORK(5)=0.99D0, WORK(6)=2.D0 MIGHT BE GOOD. */
/*              DEFAULTS WORK(5)=1.D0, WORK(6)=1.2D0 . */

/*    WORK(7)   MAXIMAL STEP SIZE, DEFAULT XEND-X. */

/*    WORK(8), WORK(9)   PARAMETERS FOR STEP SIZE SELECTION */
/*              THE NEW STEP SIZE IS CHOSEN SUBJECT TO THE RESTRICTION */
/*                 WORK(8) <= HNEW/HOLD <= WORK(9) */
/*              DEFAULT VALUES: WORK(8)=0.2D0, WORK(9)=8.D0 */

/* ----------------------------------------------------------------------- */

/*     OUTPUT PARAMETERS */
/*     ----------------- */
/*     X           X-VALUE FOR WHICH THE SOLUTION HAS BEEN COMPUTED */
/*                 (AFTER SUCCESSFUL RETURN X=XEND). */

/*     Y(N)        NUMERICAL SOLUTION AT X */

/*     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP */

/*     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN: */
/*                   IDID= 1  COMPUTATION SUCCESSFUL, */
/*                   IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUT) */
/*                   IDID=-1  INPUT IS NOT CONSISTENT, */
/*                   IDID=-2  LARGER NMAX IS NEEDED, */
/*                   IDID=-3  STEP SIZE BECOMES TOO SMALL, */
/*                   IDID=-4  MATRIX IS REPEATEDLY SINGULAR. */

/*   IWORK(14)  NFCN    NUMBER OF FUNCTION EVALUATIONS (THOSE FOR NUMERICAL */
/*                      EVALUATION OF THE JACOBIAN ARE NOT COUNTED) */
/*   IWORK(15)  NJAC    NUMBER OF JACOBIAN EVALUATIONS (EITHER ANALYTICALLY */
/*                      OR NUMERICALLY) */
/*   IWORK(16)  NSTEP   NUMBER OF COMPUTED STEPS */
/*   IWORK(17)  NACCPT  NUMBER OF ACCEPTED STEPS */
/*   IWORK(18)  NREJCT  NUMBER OF REJECTED STEPS (DUE TO ERROR TEST), */
/*                      (STEP REJECTIONS IN THE FIRST STEP ARE NOT COUNTED) */
/*   IWORK(19)  NDEC    NUMBER OF LU-DECOMPOSITIONS OF BOTH MATRICES */
/*   IWORK(20)  NSOL    NUMBER OF FORWARD-BACKWARD SUBSTITUTIONS, OF BOTH */
/*                      SYSTEMS; THE NSTEP FORWARD-BACKWARD SUBSTITUTIONS, */
/*                      NEEDED FOR STEP SIZE SELECTION, ARE NOT COUNTED */
/* ----------------------------------------------------------------------- */
/* *** *** *** *** *** *** *** *** *** *** *** *** *** */
/*          DECLARATIONS */
/* *** *** *** *** *** *** *** *** *** *** *** *** *** */
/*<       IMPLICIT DOUBLE PRECISION (A-H,O-Z) >*/
/*<       DIMENSION Y(N),ATOL(*),RTOL(*),WORK(LWORK),IWORK(LIWORK) >*/
/*<       DIMENSION RPAR(*),IPAR(*) >*/
/*<       LOGICAL IMPLCT,JBAND,ARRET,STARTN,PRED >*/
/*<       EXTERNAL FCN,JAC,MAS,SOLOUT >*/
/* *** *** *** *** *** *** *** */
/*        SETTING THE PARAMETERS */
/* *** *** *** *** *** *** *** */
/*<        NFCN=0 >*/
    /* Parameter adjustments */
    work_dim1 = *lwork;
    iwork_dim1 = *liwork;

    /* Function Body */
    nfcn = 0;
/*<        NJAC=0 >*/
    njac = 0;
/*<        NSTEP=0 >*/
    nstep = 0;
/*<        NACCPT=0 >*/
    naccpt = 0;
/*<        NREJCT=0 >*/
    nrejct = 0;
/*<        NDEC=0 >*/
    ndec = 0;
/*<        NSOL=0 >*/
    nsol = 0;
/*<        ARRET=.FALSE. >*/
    arret = FALSE_;
/* -------- UROUND   SMALLEST NUMBER SATISFYING 1.0D0+UROUND>1.0D0 */
/*<       IF (WORK(1).EQ.0.0D0) THEN >*/
    if (work[(i__1 = 0) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)390)] == 0.) {
/*<          UROUND=1.0D-16 >*/
	uround = 1e-16;
/*<       ELSE >*/
    } else {
/*<          UROUND=WORK(1) >*/
	uround = work[(i__1 = 0) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1,
		 "radau5_", (ftnlen)393)];
/*<          IF (UROUND.LE.1.0D-19.OR.UROUND.GE.1.0D0) THEN >*/
	if (uround <= 1e-19 || uround >= 1.) {
/*<             WRITE(6,*)' COEFFICIENTS HAVE 20 DIGITS, UROUND=',WORK(1) >*/
	    s_wsle(&io___10);
	    do_lio(&c__9, &c__1, " COEFFICIENTS HAVE 20 DIGITS, UROUND=", (
		    ftnlen)37);
	    do_lio(&c__5, &c__1, (char *)&work[(i__1 = 0) < 1 * work_dim1 ? 
		    i__1 : s_rnge("work", i__1, "radau5_", (ftnlen)395)], (
		    ftnlen)sizeof(doublereal));
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<          END IF >*/
	}
/*<       END IF >*/
    }
/* -------- CHECK AND CHANGE THE TOLERANCES */
/*<       EXPM=2.0D0/3.0D0 >*/
    expm = .66666666666666663;
/*<       IF (ITOL.EQ.0) THEN >*/
    if (*itol == 0) {
/*<           IF (ATOL(1).LE.0.D0.OR.RTOL(1).LE.10.D0*UROUND) THEN >*/
	if (atol[0] <= 0. || rtol[0] <= uround * 10.) {
/*<               WRITE (6,*) ' TOLERANCES ARE TOO SMALL' >*/
	    s_wsle(&io___12);
	    do_lio(&c__9, &c__1, " TOLERANCES ARE TOO SMALL", (ftnlen)25);
	    e_wsle();
/*<               ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<           ELSE >*/
	} else {
/*<               QUOT=ATOL(1)/RTOL(1) >*/
	    quot = atol[0] / rtol[0];
/*<               RTOL(1)=0.1D0*RTOL(1)**EXPM >*/
	    rtol[0] = pow_dd(rtol, &expm) * .1;
/*<               ATOL(1)=RTOL(1)*QUOT >*/
	    atol[0] = rtol[0] * quot;
/*<           END IF >*/
	}
/*<       ELSE >*/
    } else {
/*<           DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<           IF (ATOL(I).LE.0.D0.OR.RTOL(I).LE.10.D0*UROUND) THEN >*/
	    if (atol[i__ - 1] <= 0. || rtol[i__ - 1] <= uround * 10.) {
/*<               WRITE (6,*) ' TOLERANCES(',I,') ARE TOO SMALL' >*/
		s_wsle(&io___15);
		do_lio(&c__9, &c__1, " TOLERANCES(", (ftnlen)12);
		do_lio(&c__3, &c__1, (char *)&i__, (ftnlen)sizeof(integer));
		do_lio(&c__9, &c__1, ") ARE TOO SMALL", (ftnlen)15);
		e_wsle();
/*<               ARRET=.TRUE. >*/
		arret = TRUE_;
/*<           ELSE >*/
	    } else {
/*<               QUOT=ATOL(I)/RTOL(I) >*/
		quot = atol[i__ - 1] / rtol[i__ - 1];
/*<               RTOL(I)=0.1D0*RTOL(I)**EXPM >*/
		rtol[i__ - 1] = pow_dd(&rtol[i__ - 1], &expm) * .1;
/*<               ATOL(I)=RTOL(I)*QUOT >*/
		atol[i__ - 1] = rtol[i__ - 1] * quot;
/*<           END IF >*/
	    }
/*<           END DO >*/
	}
/*<       END IF >*/
    }
/* -------- NMAX , THE MAXIMAL NUMBER OF STEPS ----- */
/*<       IF (IWORK(2).EQ.0) THEN >*/
    if (iwork[(i__1 = 1) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)423)] == 0) {
/*<          NMAX=100000 >*/
	nmax = 100000;
/*<       ELSE >*/
    } else {
/*<          NMAX=IWORK(2) >*/
	nmax = iwork[(i__1 = 1) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", 
		i__1, "radau5_", (ftnlen)426)];
/*<          IF (NMAX.LE.0) THEN >*/
	if (nmax <= 0) {
/*<             WRITE(6,*)' WRONG INPUT IWORK(2)=',IWORK(2) >*/
	    s_wsle(&io___17);
	    do_lio(&c__9, &c__1, " WRONG INPUT IWORK(2)=", (ftnlen)22);
	    do_lio(&c__3, &c__1, (char *)&iwork[(i__1 = 1) < 1 * iwork_dim1 ? 
		    i__1 : s_rnge("iwork", i__1, "radau5_", (ftnlen)428)], (
		    ftnlen)sizeof(integer));
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<          END IF >*/
	}
/*<       END IF >*/
    }
/* -------- NIT    MAXIMAL NUMBER OF NEWTON ITERATIONS */
/*<       IF (IWORK(3).EQ.0) THEN >*/
    if (iwork[(i__1 = 2) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)433)] == 0) {
/*<          NIT=7 >*/
	nit = 7;
/*<       ELSE >*/
    } else {
/*<          NIT=IWORK(3) >*/
	nit = iwork[(i__1 = 2) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1,
		 "radau5_", (ftnlen)436)];
/*<          IF (NIT.LE.0) THEN >*/
	if (nit <= 0) {
/*<             WRITE(6,*)' CURIOUS INPUT IWORK(3)=',IWORK(3) >*/
	    s_wsle(&io___19);
	    do_lio(&c__9, &c__1, " CURIOUS INPUT IWORK(3)=", (ftnlen)24);
	    do_lio(&c__3, &c__1, (char *)&iwork[(i__1 = 2) < 1 * iwork_dim1 ? 
		    i__1 : s_rnge("iwork", i__1, "radau5_", (ftnlen)438)], (
		    ftnlen)sizeof(integer));
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<          END IF >*/
	}
/*<       END IF >*/
    }
/* -------- STARTN  SWITCH FOR STARTING VALUES OF NEWTON ITERATIONS */
/*<       IF(IWORK(4).EQ.0)THEN >*/
    if (iwork[(i__1 = 3) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)443)] == 0) {
/*<          STARTN=.FALSE. >*/
	startn = FALSE_;
/*<       ELSE >*/
    } else {
/*<          STARTN=.TRUE. >*/
	startn = TRUE_;
/*<       END IF >*/
    }
/* -------- PARAMETER FOR DIFFERENTIAL-ALGEBRAIC COMPONENTS */
/*<       NIND1=IWORK(5) >*/
    nind1 = iwork[(i__1 = 4) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)449)];
/*<       NIND2=IWORK(6) >*/
    nind2 = iwork[(i__1 = 5) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)450)];
/*<       NIND3=IWORK(7) >*/
    nind3 = iwork[(i__1 = 6) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)451)];
/*<       IF (NIND1.EQ.0) NIND1=N >*/
    if (nind1 == 0) {
	nind1 = *n;
    }
/*<       IF (NIND1+NIND2+NIND3.NE.N) THEN >*/
    if (nind1 + nind2 + nind3 != *n) {
/*<        WRITE(6,*)' CURIOUS INPUT FOR IWORK(5,6,7)=',NIND1,NIND2,NIND3 >*/
	s_wsle(&io___24);
	do_lio(&c__9, &c__1, " CURIOUS INPUT FOR IWORK(5,6,7)=", (ftnlen)32);
	do_lio(&c__3, &c__1, (char *)&nind1, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nind2, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&nind3, (ftnlen)sizeof(integer));
	e_wsle();
/*<        ARRET=.TRUE. >*/
	arret = TRUE_;
/*<       END IF >*/
    }
/* -------- PRED   STEP SIZE CONTROL */
/*<       IF(IWORK(8).LE.1)THEN >*/
    if (iwork[(i__1 = 7) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)458)] <= 1) {
/*<          PRED=.TRUE. >*/
	pred = TRUE_;
/*<       ELSE >*/
    } else {
/*<          PRED=.FALSE. >*/
	pred = FALSE_;
/*<       END IF >*/
    }
/* -------- PARAMETER FOR SECOND ORDER EQUATIONS */
/*<       M1=IWORK(9) >*/
    m1 = iwork[(i__1 = 8) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)464)];
/*<       M2=IWORK(10) >*/
    m2 = iwork[(i__1 = 9) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, 
	    "radau5_", (ftnlen)465)];
/*<       NM1=N-M1 >*/
    nm1 = *n - m1;
/*<       IF (M1.EQ.0) M2=N >*/
    if (m1 == 0) {
	m2 = *n;
    }
/*<       IF (M2.EQ.0) M2=M1 >*/
    if (m2 == 0) {
	m2 = m1;
    }
/*<       IF (M1.LT.0.OR.M2.LT.0.OR.M1+M2.GT.N) THEN >*/
    if (m1 < 0 || m2 < 0 || m1 + m2 > *n) {
/*<        WRITE(6,*)' CURIOUS INPUT FOR IWORK(9,10)=',M1,M2 >*/
	s_wsle(&io___29);
	do_lio(&c__9, &c__1, " CURIOUS INPUT FOR IWORK(9,10)=", (ftnlen)31);
	do_lio(&c__3, &c__1, (char *)&m1, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&m2, (ftnlen)sizeof(integer));
	e_wsle();
/*<        ARRET=.TRUE. >*/
	arret = TRUE_;
/*<       END IF >*/
    }
/* --------- SAFE     SAFETY FACTOR IN STEP SIZE PREDICTION */
/*<       IF (WORK(2).EQ.0.0D0) THEN >*/
    if (work[(i__1 = 1) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)474)] == 0.) {
/*<          SAFE=0.9D0 >*/
	safe = .9;
/*<       ELSE >*/
    } else {
/*<          SAFE=WORK(2) >*/
	safe = work[(i__1 = 1) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, 
		"radau5_", (ftnlen)477)];
/*<          IF (SAFE.LE.0.001D0.OR.SAFE.GE.1.0D0) THEN >*/
	if (safe <= .001 || safe >= 1.) {
/*<             WRITE(6,*)' CURIOUS INPUT FOR WORK(2)=',WORK(2) >*/
	    s_wsle(&io___31);
	    do_lio(&c__9, &c__1, " CURIOUS INPUT FOR WORK(2)=", (ftnlen)27);
	    do_lio(&c__5, &c__1, (char *)&work[(i__1 = 1) < 1 * work_dim1 ? 
		    i__1 : s_rnge("work", i__1, "radau5_", (ftnlen)479)], (
		    ftnlen)sizeof(doublereal));
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<          END IF >*/
	}
/*<       END IF >*/
    }
/* ------ THET     DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED; */
/*<       IF (WORK(3).EQ.0.D0) THEN >*/
    if (work[(i__1 = 2) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)484)] == 0.) {
/*<          THET=0.001D0 >*/
	thet = .001;
/*<       ELSE >*/
    } else {
/*<          THET=WORK(3) >*/
	thet = work[(i__1 = 2) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, 
		"radau5_", (ftnlen)487)];
/*<          IF (THET.GE.1.0D0) THEN >*/
	if (thet >= 1.) {
/*<             WRITE(6,*)' CURIOUS INPUT FOR WORK(3)=',WORK(3) >*/
	    s_wsle(&io___33);
	    do_lio(&c__9, &c__1, " CURIOUS INPUT FOR WORK(3)=", (ftnlen)27);
	    do_lio(&c__5, &c__1, (char *)&work[(i__1 = 2) < 1 * work_dim1 ? 
		    i__1 : s_rnge("work", i__1, "radau5_", (ftnlen)489)], (
		    ftnlen)sizeof(doublereal));
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<          END IF >*/
	}
/*<       END IF >*/
    }
/* --- FNEWT   STOPPING CRITERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1. */
/*<       TOLST=RTOL(1) >*/
    tolst = rtol[0];
/*<       IF (WORK(4).EQ.0.D0) THEN >*/
    if (work[(i__1 = 3) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)495)] == 0.) {
/*<          FNEWT=MAX(10*UROUND/TOLST,MIN(0.03D0,TOLST**0.5D0)) >*/
/* Computing MAX */
/* Computing MIN */
	d__3 = .03, d__4 = pow_dd(&tolst, &c_b100);
	d__1 = uround * 10 / tolst, d__2 = min(d__3,d__4);
	fnewt = max(d__1,d__2);
/*<       ELSE >*/
    } else {
/*<          FNEWT=WORK(4) >*/
	fnewt = work[(i__1 = 3) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, 
		"radau5_", (ftnlen)498)];
/*<          IF (FNEWT.LE.UROUND/TOLST) THEN >*/
	if (fnewt <= uround / tolst) {
/*<             WRITE(6,*)' CURIOUS INPUT FOR WORK(4)=',WORK(4) >*/
	    s_wsle(&io___36);
	    do_lio(&c__9, &c__1, " CURIOUS INPUT FOR WORK(4)=", (ftnlen)27);
	    do_lio(&c__5, &c__1, (char *)&work[(i__1 = 3) < 1 * work_dim1 ? 
		    i__1 : s_rnge("work", i__1, "radau5_", (ftnlen)500)], (
		    ftnlen)sizeof(doublereal));
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<          END IF >*/
	}
/*<       END IF >*/
    }
/* --- QUOT1 AND QUOT2: IF QUOT1 < HNEW/HOLD < QUOT2, STEP SIZE = CONST. */
/*<       IF (WORK(5).EQ.0.D0) THEN >*/
    if (work[(i__1 = 4) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)505)] == 0.) {
/*<          QUOT1=1.D0 >*/
	quot1 = 1.;
/*<       ELSE >*/
    } else {
/*<          QUOT1=WORK(5) >*/
	quot1 = work[(i__1 = 4) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, 
		"radau5_", (ftnlen)508)];
/*<       END IF >*/
    }
/*<       IF (WORK(6).EQ.0.D0) THEN >*/
    if (work[(i__1 = 5) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)510)] == 0.) {
/*<          QUOT2=1.2D0 >*/
	quot2 = 1.2;
/*<       ELSE >*/
    } else {
/*<          QUOT2=WORK(6) >*/
	quot2 = work[(i__1 = 5) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, 
		"radau5_", (ftnlen)513)];
/*<       END IF >*/
    }
/*<       IF (QUOT1.GT.1.0D0.OR.QUOT2.LT.1.0D0) THEN >*/
    if (quot1 > 1. || quot2 < 1.) {
/*<          WRITE(6,*)' CURIOUS INPUT FOR WORK(5,6)=',QUOT1,QUOT2 >*/
	s_wsle(&io___39);
	do_lio(&c__9, &c__1, " CURIOUS INPUT FOR WORK(5,6)=", (ftnlen)29);
	do_lio(&c__5, &c__1, (char *)&quot1, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&quot2, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<          ARRET=.TRUE. >*/
	arret = TRUE_;
/*<       END IF >*/
    }
/* -------- MAXIMAL STEP SIZE */
/*<       IF (WORK(7).EQ.0.D0) THEN >*/
    if (work[(i__1 = 6) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)520)] == 0.) {
/*<          HMAX=XEND-X >*/
	hmax = *xend - *x;
/*<       ELSE >*/
    } else {
/*<          HMAX=WORK(7) >*/
	hmax = work[(i__1 = 6) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, 
		"radau5_", (ftnlen)523)];
/*<       END IF  >*/
    }
/* -------  FACL,FACR     PARAMETERS FOR STEP SIZE SELECTION */
/*<       IF(WORK(8).EQ.0.D0)THEN >*/
    if (work[(i__1 = 7) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)526)] == 0.) {
/*<          FACL=5.D0 >*/
	facl = 5.;
/*<       ELSE >*/
    } else {
/*<          FACL=1.D0/WORK(8) >*/
	facl = 1. / work[(i__1 = 7) < 1 * work_dim1 ? i__1 : s_rnge("work", 
		i__1, "radau5_", (ftnlen)529)];
/*<       END IF >*/
    }
/*<       IF(WORK(9).EQ.0.D0)THEN >*/
    if (work[(i__1 = 8) < 1 * work_dim1 ? i__1 : s_rnge("work", i__1, "radau"
	    "5_", (ftnlen)531)] == 0.) {
/*<          FACR=1.D0/8.0D0 >*/
	facr = .125;
/*<       ELSE >*/
    } else {
/*<          FACR=1.D0/WORK(9) >*/
	facr = 1. / work[(i__1 = 8) < 1 * work_dim1 ? i__1 : s_rnge("work", 
		i__1, "radau5_", (ftnlen)534)];
/*<       END IF >*/
    }
/*<       IF (FACL.LT.1.0D0.OR.FACR.GT.1.0D0) THEN >*/
    if (facl < 1. || facr > 1.) {
/*<             WRITE(6,*)' CURIOUS INPUT WORK(8,9)=',WORK(8),WORK(9) >*/
	s_wsle(&io___43);
	do_lio(&c__9, &c__1, " CURIOUS INPUT WORK(8,9)=", (ftnlen)25);
	do_lio(&c__5, &c__1, (char *)&work[(i__1 = 7) < 1 * work_dim1 ? i__1 :
		 s_rnge("work", i__1, "radau5_", (ftnlen)537)], (ftnlen)
		sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&work[(i__2 = 8) < 1 * work_dim1 ? i__2 :
		 s_rnge("work", i__2, "radau5_", (ftnlen)537)], (ftnlen)
		sizeof(doublereal));
	e_wsle();
/*<             ARRET=.TRUE. >*/
	arret = TRUE_;
/*<          END IF >*/
    }
/* *** *** *** *** *** *** *** *** *** *** *** *** *** */
/*         COMPUTATION OF ARRAY ENTRIES */
/* *** *** *** *** *** *** *** *** *** *** *** *** *** */
/* ---- IMPLICIT, BANDED OR NOT ? */
/*<       IMPLCT=IMAS.NE.0 >*/
    implct = *imas != 0;
/*<       JBAND=MLJAC.LT.NM1 >*/
    jband = *mljac < nm1;
/* -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS --- */
/* -- JACOBIAN  AND  MATRICES E1, E2 */
/*<       IF (JBAND) THEN >*/
    if (jband) {
/*<          LDJAC=MLJAC+MUJAC+1 >*/
	ldjac = *mljac + *mujac + 1;
/*<          LDE1=MLJAC+LDJAC >*/
	lde1 = *mljac + ldjac;
/*<       ELSE >*/
    } else {
/*<          MLJAC=NM1 >*/
	*mljac = nm1;
/*<          MUJAC=NM1 >*/
	*mujac = nm1;
/*<          LDJAC=NM1 >*/
	ldjac = nm1;
/*<          LDE1=NM1 >*/
	lde1 = nm1;
/*<       END IF >*/
    }
/* -- MASS MATRIX */
/*<       IF (IMPLCT) THEN >*/
    if (implct) {
/*<           IF (MLMAS.NE.NM1) THEN >*/
	if (*mlmas != nm1) {
/*<               LDMAS=MLMAS+MUMAS+1 >*/
	    ldmas = *mlmas + *mumas + 1;
/*<               IF (JBAND) THEN >*/
	    if (jband) {
/*<                  IJOB=4 >*/
		ijob = 4;
/*<               ELSE >*/
	    } else {
/*<                  IJOB=3 >*/
		ijob = 3;
/*<               END IF >*/
	    }
/*<           ELSE >*/
	} else {
/*<               MUMAS=NM1 >*/
	    *mumas = nm1;
/*<               LDMAS=NM1 >*/
	    ldmas = nm1;
/*<               IJOB=5 >*/
	    ijob = 5;
/*<           END IF >*/
	}
/* ------ BANDWITH OF "MAS" NOT SMALLER THAN BANDWITH OF "JAC" */
/*<           IF (MLMAS.GT.MLJAC.OR.MUMAS.GT.MUJAC) THEN >*/
	if (*mlmas > *mljac || *mumas > *mujac) {
/*<    >*/
	    s_wsle(&io___50);
	    do_lio(&c__9, &c__1, "BANDWITH OF \"MAS\" NOT SMALLER THAN BANDW"
		    "ITH OF \"JAC\"", (ftnlen)52);
	    e_wsle();
/*<             ARRET=.TRUE. >*/
	    arret = TRUE_;
/*<           END IF >*/
	}
/*<       ELSE >*/
    } else {
/*<           LDMAS=0 >*/
	ldmas = 0;
/*<           IF (JBAND) THEN >*/
	if (jband) {
/*<              IJOB=2 >*/
	    ijob = 2;
/*<           ELSE >*/
	} else {
/*<              IJOB=1 >*/
	    ijob = 1;
/*<              IF (N.GT.2.AND.IWORK(1).NE.0) IJOB=7 >*/
	    if (*n > 2 && iwork[(i__1 = 0) < 1 * iwork_dim1 ? i__1 : s_rnge(
		    "iwork", i__1, "radau5_", (ftnlen)583)] != 0) {
		ijob = 7;
	    }
/*<           END IF >*/
	}
/*<       END IF >*/
    }
/*<       LDMAS2=MAX(1,LDMAS) >*/
    ldmas2 = max(1,ldmas);
/* ------ HESSENBERG OPTION ONLY FOR EXPLICIT EQU. WITH FULL JACOBIAN */
/*<       IF ((IMPLCT.OR.JBAND).AND.IJOB.EQ.7) THEN >*/
    if ((implct || jband) && ijob == 7) {
/*<    >*/
	s_wsle(&io___52);
	do_lio(&c__9, &c__1, " HESSENBERG OPTION ONLY FOR EXPLICIT EQUATIONS"
		" WITH FULL JACOBIAN", (ftnlen)65);
	e_wsle();
/*<          ARRET=.TRUE. >*/
	arret = TRUE_;
/*<       END IF >*/
    }
/* ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK ----- */
/*<       IEZ1=21 >*/
    iez1 = 21;
/*<       IEZ2=IEZ1+N >*/
    iez2 = iez1 + *n;
/*<       IEZ3=IEZ2+N >*/
    iez3 = iez2 + *n;
/*<       IEY0=IEZ3+N >*/
    iey0 = iez3 + *n;
/*<       IESCAL=IEY0+N >*/
    iescal = iey0 + *n;
/*<       IEF1=IESCAL+N >*/
    ief1 = iescal + *n;
/*<       IEF2=IEF1+N >*/
    ief2 = ief1 + *n;
/*<       IEF3=IEF2+N >*/
    ief3 = ief2 + *n;
/*<       IECON=IEF3+N >*/
    iecon = ief3 + *n;
/*<       IEJAC=IECON+4*N >*/
    iejac = iecon + (*n << 2);
/*<       IEMAS=IEJAC+N*LDJAC >*/
    iemas = iejac + *n * ldjac;
/*<       IEE1=IEMAS+NM1*LDMAS >*/
    iee1 = iemas + nm1 * ldmas;
/*<       IEE2R=IEE1+NM1*LDE1 >*/
    iee2r = iee1 + nm1 * lde1;
/*<       IEE2I=IEE2R+NM1*LDE1 >*/
    iee2i = iee2r + nm1 * lde1;
/* ------ TOTAL STORAGE REQUIREMENT ----------- */
/*<       ISTORE=IEE2I+NM1*LDE1-1 >*/
    istore = iee2i + nm1 * lde1 - 1;
/*<       IF(ISTORE.GT.LWORK)THEN >*/
    if (istore > *lwork) {
/*<          WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE >*/
	s_wsle(&io___68);
	do_lio(&c__9, &c__1, " INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=", (
		ftnlen)43);
	do_lio(&c__3, &c__1, (char *)&istore, (ftnlen)sizeof(integer));
	e_wsle();
/*<          ARRET=.TRUE. >*/
	arret = TRUE_;
/*<       END IF >*/
    }
/* ------- ENTRY POINTS FOR INTEGER WORKSPACE ----- */
/*<       IEIP1=21 >*/
    ieip1 = 21;
/*<       IEIP2=IEIP1+NM1 >*/
    ieip2 = ieip1 + nm1;
/*<       IEIPH=IEIP2+NM1 >*/
    ieiph = ieip2 + nm1;
/* --------- TOTAL REQUIREMENT --------------- */
/*<       ISTORE=IEIPH+NM1-1 >*/
    istore = ieiph + nm1 - 1;
/*<       IF (ISTORE.GT.LIWORK) THEN >*/
    if (istore > *liwork) {
/*<          WRITE(6,*)' INSUFF. STORAGE FOR IWORK, MIN. LIWORK=',ISTORE >*/
	s_wsle(&io___72);
	do_lio(&c__9, &c__1, " INSUFF. STORAGE FOR IWORK, MIN. LIWORK=", (
		ftnlen)40);
	do_lio(&c__3, &c__1, (char *)&istore, (ftnlen)sizeof(integer));
	e_wsle();
/*<          ARRET=.TRUE. >*/
	arret = TRUE_;
/*<       END IF >*/
    }
/* ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1 */
/*<       IF (ARRET) THEN >*/
    if (arret) {
/*<          IDID=-1 >*/
	*idid = -1;
/*<          RETURN >*/
	return 0;
/*<       END IF >*/
    }
/* -------- CALL TO CORE INTEGRATOR ------------ */
/*<    >*/
    radcor_(n, (U_fp)fcn, x, y, xend, &hmax, h__, rtol, atol, itol, (U_fp)jac,
	     ijac, mljac, mujac, (U_fp)mas, mlmas, mumas, (U_fp)solout, iout, 
	    idid, &nmax, &uround, &safe, &thet, &fnewt, &quot1, &quot2, &nit, 
	    &ijob, &startn, &nind1, &nind2, &nind3, &pred, &facl, &facr, &m1, 
	    &m2, &nm1, &implct, &jband, &ldjac, &lde1, &ldmas2, &work[(i__1 = 
	    iez1 - 1) < 1 * work_dim1 && 0 <= i__1 ? i__1 : s_rnge("work", 
	    i__1, "radau5_", (ftnlen)630)], &work[(i__2 = iez2 - 1) < 1 * 
	    work_dim1 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "radau5_", (
	    ftnlen)630)], &work[(i__3 = iez3 - 1) < 1 * work_dim1 && 0 <= 
	    i__3 ? i__3 : s_rnge("work", i__3, "radau5_", (ftnlen)630)], &
	    work[(i__4 = iey0 - 1) < 1 * work_dim1 && 0 <= i__4 ? i__4 : 
	    s_rnge("work", i__4, "radau5_", (ftnlen)630)], &work[(i__5 = 
	    iescal - 1) < 1 * work_dim1 && 0 <= i__5 ? i__5 : s_rnge("work", 
	    i__5, "radau5_", (ftnlen)630)], &work[(i__6 = ief1 - 1) < 1 * 
	    work_dim1 && 0 <= i__6 ? i__6 : s_rnge("work", i__6, "radau5_", (
	    ftnlen)630)], &work[(i__7 = ief2 - 1) < 1 * work_dim1 && 0 <= 
	    i__7 ? i__7 : s_rnge("work", i__7, "radau5_", (ftnlen)630)], &
	    work[(i__8 = ief3 - 1) < 1 * work_dim1 && 0 <= i__8 ? i__8 : 
	    s_rnge("work", i__8, "radau5_", (ftnlen)630)], &work[(i__9 = 
	    iejac - 1) < 1 * work_dim1 && 0 <= i__9 ? i__9 : s_rnge("work", 
	    i__9, "radau5_", (ftnlen)630)], &work[(i__10 = iee1 - 1) < 1 * 
	    work_dim1 && 0 <= i__10 ? i__10 : s_rnge("work", i__10, "radau5_",
	     (ftnlen)630)], &work[(i__11 = iee2r - 1) < 1 * work_dim1 && 0 <= 
	    i__11 ? i__11 : s_rnge("work", i__11, "radau5_", (ftnlen)630)], &
	    work[(i__12 = iee2i - 1) < 1 * work_dim1 && 0 <= i__12 ? i__12 : 
	    s_rnge("work", i__12, "radau5_", (ftnlen)630)], &work[(i__13 = 
	    iemas - 1) < 1 * work_dim1 && 0 <= i__13 ? i__13 : s_rnge("work", 
	    i__13, "radau5_", (ftnlen)630)], &iwork[(i__14 = ieip1 - 1) < 1 * 
	    iwork_dim1 && 0 <= i__14 ? i__14 : s_rnge("iwork", i__14, "radau"
	    "5_", (ftnlen)630)], &iwork[(i__15 = ieip2 - 1) < 1 * iwork_dim1 &&
	     0 <= i__15 ? i__15 : s_rnge("iwork", i__15, "radau5_", (ftnlen)
	    630)], &iwork[(i__16 = ieiph - 1) < 1 * iwork_dim1 && 0 <= i__16 ?
	     i__16 : s_rnge("iwork", i__16, "radau5_", (ftnlen)630)], &work[(
	    i__17 = iecon - 1) < 1 * work_dim1 && 0 <= i__17 ? i__17 : s_rnge(
	    "work", i__17, "radau5_", (ftnlen)630)], &nfcn, &njac, &nstep, &
	    naccpt, &nrejct, &ndec, &nsol, rpar, ipar);
/*<       IWORK(14)=NFCN >*/
    iwork[(i__1 = 13) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)639)] = nfcn;
/*<       IWORK(15)=NJAC >*/
    iwork[(i__1 = 14) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)640)] = njac;
/*<       IWORK(16)=NSTEP >*/
    iwork[(i__1 = 15) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)641)] = nstep;
/*<       IWORK(17)=NACCPT >*/
    iwork[(i__1 = 16) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)642)] = naccpt;
/*<       IWORK(18)=NREJCT >*/
    iwork[(i__1 = 17) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)643)] = nrejct;
/*<       IWORK(19)=NDEC >*/
    iwork[(i__1 = 18) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)644)] = ndec;
/*<       IWORK(20)=NSOL >*/
    iwork[(i__1 = 19) < 1 * iwork_dim1 ? i__1 : s_rnge("iwork", i__1, "radau"
	    "5_", (ftnlen)645)] = nsol;
/* -------- RESTORE TOLERANCES */
/*<       EXPM=1.0D0/EXPM >*/
    expm = 1. / expm;
/*<       IF (ITOL.EQ.0) THEN >*/
    if (*itol == 0) {
/*<               QUOT=ATOL(1)/RTOL(1) >*/
	quot = atol[0] / rtol[0];
/*<               RTOL(1)=(10.0D0*RTOL(1))**EXPM >*/
	d__1 = rtol[0] * 10.;
	rtol[0] = pow_dd(&d__1, &expm);
/*<               ATOL(1)=RTOL(1)*QUOT >*/
	atol[0] = rtol[0] * quot;
/*<       ELSE >*/
    } else {
/*<           DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<               QUOT=ATOL(I)/RTOL(I) >*/
	    quot = atol[i__ - 1] / rtol[i__ - 1];
/*<               RTOL(I)=(10.0D0*RTOL(I))**EXPM >*/
	    d__1 = rtol[i__ - 1] * 10.;
	    rtol[i__ - 1] = pow_dd(&d__1, &expm);
/*<               ATOL(I)=RTOL(I)*QUOT >*/
	    atol[i__ - 1] = rtol[i__ - 1] * quot;
/*<           END DO >*/
	}
/*<       END IF >*/
    }
/* ----------- RETURN ----------- */
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* radau5_ */


/*     END OF SUBROUTINE RADAU5 */

/* *********************************************************** */

/*<    >*/
/* Subroutine */ int radcor_(integer *n, S_fp fcn, doublereal *x, doublereal *
	y, doublereal *xend, doublereal *hmax, doublereal *h__, doublereal *
	rtol, doublereal *atol, integer *itol, S_fp jac, integer *ijac, 
	integer *mljac, integer *mujac, S_fp mas, integer *mlmas, integer *
	mumas, S_fp solout, integer *iout, integer *idid, integer *nmax, 
	doublereal *uround, doublereal *safe, doublereal *thet, doublereal *
	fnewt, doublereal *quot1, doublereal *quot2, integer *nit, integer *
	ijob, logical *startn, integer *nind1, integer *nind2, integer *nind3,
	 logical *pred, doublereal *facl, doublereal *facr, integer *m1, 
	integer *m2, integer *nm1, logical *implct, logical *banded, integer *
	ldjac, integer *lde1, integer *ldmas, doublereal *z1, doublereal *z2, 
	doublereal *z3, doublereal *y0, doublereal *scal, doublereal *f1, 
	doublereal *f2, doublereal *f3, doublereal *fjac, doublereal *e1, 
	doublereal *e2r, doublereal *e2i, doublereal *fmas, integer *ip1, 
	integer *ip2, integer *iphes, doublereal *cont, integer *nfcn, 
	integer *njac, integer *nstep, integer *naccpt, integer *nrejct, 
	integer *ndec, integer *nsol, doublereal *rpar, integer *ipar)
{
    /* Format strings */
    static char fmt_979[] = "(\002 EXIT OF RADAU5 AT X=\002,e18.4)";

    /* System generated locals */
    integer y_dim1, z1_dim1, z2_dim1, z3_dim1, y0_dim1, scal_dim1, f1_dim1, 
	    f2_dim1, f3_dim1, fjac_dim1, fjac_dim2, fjac_offset, fmas_dim1, 
	    fmas_offset, cont_dim1, e1_dim1, e1_offset, e2r_dim1, e2r_offset, 
	    e2i_dim1, e2i_offset, i__1, i__2, i__3, i__4, i__5, i__6, i__7;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal), pow_dd(doublereal *, doublereal *), d_sign(
	    doublereal *, doublereal *);
    integer s_rnge(char *, integer, char *, integer);
    double pow_di(doublereal *, integer *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    static integer i__, j, k, l;
    static doublereal a1, a2, c1, c2, a3;
    static integer j1, n2, n3;
    static doublereal u1, ak;
    static integer md;
    static doublereal t11, t12, t13, t21, t22, t23, t31;
    static integer mm;
    static doublereal qt, dd1, dd2, dd3, ak1, ak2, ak3, f1i, f2i, f3i, c1q, 
	    c2q, c3q, z1i, z2i, z3i, sq6, fac, ti11, cno;
    static integer lrc;
    static doublereal ti12, ti13, ti21, ti22, ti23, ti31, ti32, ti33;
    static integer ier;
    static doublereal xph, thq, err, fac1, cfac, hacc, c1mc2, beta;
    static integer lbeg;
    static doublereal alph, hold;
    static integer lend;
    static doublereal delt, hnew;
    static logical last;
    static doublereal hopt, xold;
    static integer newt;
    static doublereal dyno, dyth, quot, hhfac, betan, alphn, denom, theta, 
	    ysafe, hmaxn;
    static integer nsing;
    static logical first;
    static integer irtrn, nrsol, nsolu;
    static doublereal qnewt, xosol, acont3;
    static logical index1, index2, index3, caljac;
    static doublereal faccon;
    extern /* Subroutine */ int decomc_(integer *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, integer *);
    static logical calhes;
    static doublereal erracc;
    static integer mujacj;
    extern /* Subroutine */ int decomr_(integer *, doublereal *, integer *, 
	    doublereal *, integer *, integer *, integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, integer *, logical *, integer *);
    static logical reject;
    static doublereal facgus;
    static integer mujacp;
    extern /* Subroutine */ int estrad_(integer *, doublereal *, integer *, 
	    integer *, integer *, doublereal *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, S_fp, 
	    integer *, doublereal *, doublereal *, integer *, doublereal *, 
	    integer *, integer *, integer *, doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, logical *, logical *, doublereal *, doublereal *, 
	    integer *);
    static doublereal dynold, posneg;
    extern /* Subroutine */ int slvrad_(integer *, doublereal *, integer *, 
	    integer *, integer *, doublereal *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, integer *, integer *);
    static doublereal thqold;

    /* Fortran I/O blocks */
    static cilist io___176 = { 0, 6, 0, fmt_979, 0 };
    static cilist io___177 = { 0, 6, 0, 0, 0 };
    static cilist io___178 = { 0, 6, 0, fmt_979, 0 };
    static cilist io___179 = { 0, 6, 0, 0, 0 };
    static cilist io___180 = { 0, 6, 0, fmt_979, 0 };
    static cilist io___181 = { 0, 6, 0, 0, 0 };
    static cilist io___182 = { 0, 6, 0, fmt_979, 0 };


/* ---------------------------------------------------------- */
/*     CORE INTEGRATOR FOR RADAU5 */
/*     PARAMETERS SAME AS IN RADAU5 WITH WORKSPACE ADDED */
/* ---------------------------------------------------------- */
/*         DECLARATIONS */
/* ---------------------------------------------------------- */
/*<       IMPLICIT DOUBLE PRECISION (A-H,O-Z) >*/
/*<       DIMENSION Y(N),Z1(N),Z2(N),Z3(N),Y0(N),SCAL(N),F1(N),F2(N),F3(N) >*/
/*<       DIMENSION FJAC(LDJAC,N),FMAS(LDMAS,NM1),CONT(4*N) >*/
/*<       DIMENSION E1(LDE1,NM1),E2R(LDE1,NM1),E2I(LDE1,NM1) >*/
/*<       DIMENSION ATOL(*),RTOL(*),RPAR(*),IPAR(*) >*/
/*<       INTEGER IP1(NM1),IP2(NM1),IPHES(NM1) >*/
/*<       COMMON /CONRA5/NN,NN2,NN3,NN4,XSOL,HSOL,C2M1,C1M1 >*/
/*<       COMMON/LINAL/MLE,MUE,MBJAC,MBB,MDIAG,MDIFF,MBDIAG >*/
/*<       LOGICAL REJECT,FIRST,IMPLCT,BANDED,CALJAC,STARTN,CALHES >*/
/*<       LOGICAL INDEX1,INDEX2,INDEX3,LAST,PRED >*/
/*<       EXTERNAL FCN >*/
/* *** *** *** *** *** *** *** */
/*  INITIALISATIONS */
/* *** *** *** *** *** *** *** */
/* --------- DUPLIFY N FOR COMMON BLOCK CONT ----- */
/*<       NN=N >*/
    /* Parameter adjustments */
    cont_dim1 = 4 * *n;
    f3_dim1 = *n;
    f2_dim1 = *n;
    f1_dim1 = *n;
    scal_dim1 = *n;
    y0_dim1 = *n;
    z3_dim1 = *n;
    z2_dim1 = *n;
    z1_dim1 = *n;
    y_dim1 = *n;
    fjac_dim1 = *ldjac;
    fjac_dim2 = *n;
    fjac_offset = 1 + fjac_dim1;
    e2i_dim1 = *lde1;
    e2i_offset = 1 + e2i_dim1;
    e2r_dim1 = *lde1;
    e2r_offset = 1 + e2r_dim1;
    e1_dim1 = *lde1;
    e1_offset = 1 + e1_dim1;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;

    /* Function Body */
    conra5_1.nn = *n;
/*<       NN2=2*N >*/
    conra5_1.nn2 = *n << 1;
/*<       NN3=3*N  >*/
    conra5_1.nn3 = *n * 3;
/*<       LRC=4*N >*/
    lrc = *n << 2;
/* -------- CHECK THE INDEX OF THE PROBLEM ----- */
/*<       INDEX1=NIND1.NE.0 >*/
    index1 = *nind1 != 0;
/*<       INDEX2=NIND2.NE.0 >*/
    index2 = *nind2 != 0;
/*<       INDEX3=NIND3.NE.0 >*/
    index3 = *nind3 != 0;
/* ------- COMPUTE MASS MATRIX FOR IMPLICIT CASE ---------- */
/*<       IF (IMPLCT) CALL MAS(NM1,FMAS,LDMAS,RPAR,IPAR) >*/
    if (*implct) {
	(*mas)(nm1, fmas, ldmas, rpar, ipar);
    }
/* ---------- CONSTANTS --------- */
/*<       SQ6=DSQRT(6.D0) >*/
    sq6 = sqrt(6.);
/*<       C1=(4.D0-SQ6)/10.D0 >*/
    c1 = (4. - sq6) / 10.;
/*<       C2=(4.D0+SQ6)/10.D0 >*/
    c2 = (sq6 + 4.) / 10.;
/*<       C1M1=C1-1.D0 >*/
    conra5_1.c1m1 = c1 - 1.;
/*<       C2M1=C2-1.D0 >*/
    conra5_1.c2m1 = c2 - 1.;
/*<       C1MC2=C1-C2 >*/
    c1mc2 = c1 - c2;
/*<       DD1=-(13.D0+7.D0*SQ6)/3.D0 >*/
    dd1 = -(sq6 * 7. + 13.) / 3.;
/*<       DD2=(-13.D0+7.D0*SQ6)/3.D0 >*/
    dd2 = (sq6 * 7. - 13.) / 3.;
/*<       DD3=-1.D0/3.D0 >*/
    dd3 = -.33333333333333331;
/*<       U1=(6.D0+81.D0**(1.D0/3.D0)-9.D0**(1.D0/3.D0))/30.D0 >*/
    u1 = (pow_dd(&c_b215, &c_b216) + 6. - pow_dd(&c_b217, &c_b216)) / 30.;
/*<       ALPH=(12.D0-81.D0**(1.D0/3.D0)+9.D0**(1.D0/3.D0))/60.D0 >*/
    alph = (12. - pow_dd(&c_b215, &c_b216) + pow_dd(&c_b217, &c_b216)) / 60.;
/*<       BETA=(81.D0**(1.D0/3.D0)+9.D0**(1.D0/3.D0))*DSQRT(3.D0)/60.D0 >*/
    beta = (pow_dd(&c_b215, &c_b216) + pow_dd(&c_b217, &c_b216)) * sqrt(3.) / 
	    60.;
/*<       CNO=ALPH**2+BETA**2 >*/
/* Computing 2nd power */
    d__1 = alph;
/* Computing 2nd power */
    d__2 = beta;
    cno = d__1 * d__1 + d__2 * d__2;
/*<       U1=1.0D0/U1 >*/
    u1 = 1. / u1;
/*<       ALPH=ALPH/CNO >*/
    alph /= cno;
/*<       BETA=BETA/CNO >*/
    beta /= cno;
/*<       T11=9.1232394870892942792D-02 >*/
    t11 = .091232394870892942792;
/*<       T12=-0.14125529502095420843D0 >*/
    t12 = -.14125529502095420843;
/*<       T13=-3.0029194105147424492D-02 >*/
    t13 = -.030029194105147424492;
/*<       T21=0.24171793270710701896D0 >*/
    t21 = .24171793270710701896;
/*<       T22=0.20412935229379993199D0 >*/
    t22 = .20412935229379993199;
/*<       T23=0.38294211275726193779D0 >*/
    t23 = .38294211275726193779;
/*<       T31=0.96604818261509293619D0 >*/
    t31 = .96604818261509293619;
/*<       TI11=4.3255798900631553510D0 >*/
    ti11 = 4.325579890063155351;
/*<       TI12=0.33919925181580986954D0 >*/
    ti12 = .33919925181580986954;
/*<       TI13=0.54177053993587487119D0 >*/
    ti13 = .54177053993587487119;
/*<       TI21=-4.1787185915519047273D0 >*/
    ti21 = -4.1787185915519047273;
/*<       TI22=-0.32768282076106238708D0 >*/
    ti22 = -.32768282076106238708;
/*<       TI23=0.47662355450055045196D0 >*/
    ti23 = .47662355450055045196;
/*<       TI31=-0.50287263494578687595D0 >*/
    ti31 = -.50287263494578687595;
/*<       TI32=2.5719269498556054292D0 >*/
    ti32 = 2.5719269498556054292;
/*<       TI33=-0.59603920482822492497D0 >*/
    ti33 = -.59603920482822492497;
/*<       IF (M1.GT.0) IJOB=IJOB+10 >*/
    if (*m1 > 0) {
	*ijob += 10;
    }
/*<       POSNEG=SIGN(1.D0,XEND-X) >*/
    d__1 = *xend - *x;
    posneg = d_sign(&c_b227, &d__1);
/*<       HMAXN=MIN(ABS(HMAX),ABS(XEND-X))  >*/
/* Computing MIN */
    d__2 = abs(*hmax), d__3 = (d__1 = *xend - *x, abs(d__1));
    hmaxn = min(d__2,d__3);
/*<       IF (ABS(H).LE.10.D0*UROUND) H=1.0D-6 >*/
    if (abs(*h__) <= *uround * 10.) {
	*h__ = 1e-6;
    }
/*<       H=MIN(ABS(H),HMAXN) >*/
/* Computing MIN */
    d__1 = abs(*h__);
    *h__ = min(d__1,hmaxn);
/*<       H=SIGN(H,POSNEG) >*/
    *h__ = d_sign(h__, &posneg);
/*<       HOLD=H >*/
    hold = *h__;
/*<       REJECT=.FALSE. >*/
    reject = FALSE_;
/*<       FIRST=.TRUE. >*/
    first = TRUE_;
/*<       LAST=.FALSE. >*/
    last = FALSE_;
/*<       IF ((X+H*1.0001D0-XEND)*POSNEG.GE.0.D0) THEN >*/
    if ((*x + *h__ * 1.0001 - *xend) * posneg >= 0.) {
/*<          H=XEND-X >*/
	*h__ = *xend - *x;
/*<          LAST=.TRUE. >*/
	last = TRUE_;
/*<       END IF >*/
    }
/*<       HOPT=H >*/
    hopt = *h__;
/*<       FACCON=1.D0 >*/
    faccon = 1.;
/*<       CFAC=SAFE*(1+2*NIT) >*/
    cfac = *safe * ((*nit << 1) + 1);
/*<       NSING=0 >*/
    nsing = 0;
/*<       XOLD=X >*/
    xold = *x;
/*<       IF (IOUT.NE.0) THEN >*/
    if (*iout != 0) {
/*<           IRTRN=1 >*/
	irtrn = 1;
/*<           NRSOL=1 >*/
	nrsol = 1;
/*<           XOSOL=XOLD >*/
	xosol = xold;
/*<           XSOL=X >*/
	conra5_1.xsol = *x;
/*<           DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<              CONT(I)=Y(I) >*/
	    cont[(i__2 = i__ - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("cont", i__2, "radcor_", (ftnlen)763)] = y[(i__3 = 
		    i__ - 1) < 1 * y_dim1 && 0 <= i__3 ? i__3 : s_rnge("y", 
		    i__3, "radcor_", (ftnlen)763)];
/*<           END DO >*/
	}
/*<           NSOLU=N >*/
	nsolu = *n;
/*<           HSOL=HOLD >*/
	conra5_1.hsol = hold;
/*<    >*/
	(*solout)(&nrsol, &xosol, &conra5_1.xsol, y, cont, &lrc, &nsolu, rpar,
		 ipar, &irtrn);
/*<           IF (IRTRN.LT.0) GOTO 179 >*/
	if (irtrn < 0) {
	    goto L179;
	}
/*<       END IF >*/
    }
/*<       MLE=MLJAC >*/
    linal_1.mle = *mljac;
/*<       MUE=MUJAC >*/
    linal_1.mue = *mujac;
/*<       MBJAC=MLJAC+MUJAC+1 >*/
    linal_1.mbjac = *mljac + *mujac + 1;
/*<       MBB=MLMAS+MUMAS+1 >*/
    linal_1.mbb = *mlmas + *mumas + 1;
/*<       MDIAG=MLE+MUE+1 >*/
    linal_1.mdiag = linal_1.mle + linal_1.mue + 1;
/*<       MDIFF=MLE+MUE-MUMAS >*/
    linal_1.mdiff = linal_1.mle + linal_1.mue - *mumas;
/*<       MBDIAG=MUMAS+1 >*/
    linal_1.mbdiag = *mumas + 1;
/*<       N2=2*N >*/
    n2 = *n << 1;
/*<       N3=3*N >*/
    n3 = *n * 3;
/*<       IF (ITOL.EQ.0) THEN >*/
    if (*itol == 0) {
/*<           DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<              SCAL(I)=ATOL(1)+RTOL(1)*ABS(Y(I)) >*/
	    scal[(i__3 = i__ - 1) < 1 * scal_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("scal", i__3, "radcor_", (ftnlen)782)] = atol[0] + 
		    rtol[0] * (d__1 = y[(i__2 = i__ - 1) < 1 * y_dim1 && 0 <= 
		    i__2 ? i__2 : s_rnge("y", i__2, "radcor_", (ftnlen)782)], 
		    abs(d__1));
/*<           END DO >*/
	}
/*<       ELSE >*/
    } else {
/*<           DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<              SCAL(I)=ATOL(I)+RTOL(I)*ABS(Y(I)) >*/
	    scal[(i__3 = i__ - 1) < 1 * scal_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("scal", i__3, "radcor_", (ftnlen)786)] = atol[i__ 
		    - 1] + rtol[i__ - 1] * (d__1 = y[(i__2 = i__ - 1) < 1 * 
		    y_dim1 && 0 <= i__2 ? i__2 : s_rnge("y", i__2, "radcor_", 
		    (ftnlen)786)], abs(d__1));
/*<           END DO >*/
	}
/*<       END IF >*/
    }
/*<       HHFAC=H >*/
    hhfac = *h__;
/*<       CALL FCN(N,X,Y,Y0,RPAR,IPAR) >*/
    (*fcn)(n, x, y, y0, rpar, ipar);
/*<       NFCN=NFCN+1 >*/
    ++(*nfcn);
/* --- BASIC INTEGRATION STEP */
/*<   10  CONTINUE >*/
L10:
/* *** *** *** *** *** *** *** */
/*  COMPUTATION OF THE JACOBIAN */
/* *** *** *** *** *** *** *** */
/*<       NJAC=NJAC+1 >*/
    ++(*njac);
/*<       IF (IJAC.EQ.0) THEN >*/
    if (*ijac == 0) {
/* --- COMPUTE JACOBIAN MATRIX NUMERICALLY */
/*<          IF (BANDED) THEN >*/
	if (*banded) {
/* --- JACOBIAN IS BANDED */
/*<             MUJACP=MUJAC+1 >*/
	    mujacp = *mujac + 1;
/*<             MD=MIN(MBJAC,M2) >*/
	    md = min(linal_1.mbjac,*m2);
/*<             DO MM=1,M1/M2+1 >*/
	    i__1 = *m1 / *m2 + 1;
	    for (mm = 1; mm <= i__1; ++mm) {
/*<                DO K=1,MD >*/
		i__2 = md;
		for (k = 1; k <= i__2; ++k) {
/*<                   J=K+(MM-1)*M2 >*/
		    j = k + (mm - 1) * *m2;
/*<  12               F1(J)=Y(J) >*/
L12:
		    f1[(i__3 = j - 1) < 1 * f1_dim1 && 0 <= i__3 ? i__3 : 
			    s_rnge("f1", i__3, "radcor_", (ftnlen)807)] = y[(
			    i__4 = j - 1) < 1 * y_dim1 && 0 <= i__4 ? i__4 : 
			    s_rnge("y", i__4, "radcor_", (ftnlen)807)];
/*<                   F2(J)=DSQRT(UROUND*MAX(1.D-5,ABS(Y(J)))) >*/
/* Computing MAX */
		    d__2 = 1e-5, d__3 = (d__1 = y[(i__3 = j - 1) < 1 * y_dim1 
			    && 0 <= i__3 ? i__3 : s_rnge("y", i__3, "radcor_",
			     (ftnlen)808)], abs(d__1));
		    f2[(i__4 = j - 1) < 1 * f2_dim1 && 0 <= i__4 ? i__4 : 
			    s_rnge("f2", i__4, "radcor_", (ftnlen)808)] = 
			    sqrt(*uround * max(d__2,d__3));
/*<                   Y(J)=Y(J)+F2(J) >*/
		    y[(i__3 = j - 1) < 1 * y_dim1 && 0 <= i__3 ? i__3 : 
			    s_rnge("y", i__3, "radcor_", (ftnlen)809)] = y[(
			    i__4 = j - 1) < 1 * y_dim1 && 0 <= i__4 ? i__4 : 
			    s_rnge("y", i__4, "radcor_", (ftnlen)809)] + f2[(
			    i__5 = j - 1) < 1 * f2_dim1 && 0 <= i__5 ? i__5 : 
			    s_rnge("f2", i__5, "radcor_", (ftnlen)809)];
/*<                   J=J+MD >*/
		    j += md;
/*<                   IF (J.LE.MM*M2) GOTO 12  >*/
		    if (j <= mm * *m2) {
			goto L12;
		    }
/*<                   CALL FCN(N,X,Y,CONT,RPAR,IPAR) >*/
		    (*fcn)(n, x, y, cont, rpar, ipar);
/*<                   J=K+(MM-1)*M2 >*/
		    j = k + (mm - 1) * *m2;
/*<                   J1=K >*/
		    j1 = k;
/*<                   LBEG=MAX(1,J1-MUJAC)+M1 >*/
/* Computing MAX */
		    i__3 = 1, i__4 = j1 - *mujac;
		    lbeg = max(i__3,i__4) + *m1;
/*<  14               LEND=MIN(M2,J1+MLJAC)+M1 >*/
L14:
/* Computing MIN */
		    i__3 = *m2, i__4 = j1 + *mljac;
		    lend = min(i__3,i__4) + *m1;
/*<                   Y(J)=F1(J) >*/
		    y[(i__3 = j - 1) < 1 * y_dim1 && 0 <= i__3 ? i__3 : 
			    s_rnge("y", i__3, "radcor_", (ftnlen)817)] = f1[(
			    i__4 = j - 1) < 1 * f1_dim1 && 0 <= i__4 ? i__4 : 
			    s_rnge("f1", i__4, "radcor_", (ftnlen)817)];
/*<                   MUJACJ=MUJACP-J1-M1 >*/
		    mujacj = mujacp - j1 - *m1;
/*<                   DO L=LBEG,LEND >*/
		    i__3 = lend;
		    for (l = lbeg; l <= i__3; ++l) {
/*<                      FJAC(L+MUJACJ,J)=(CONT(L)-Y0(L))/F2(J)  >*/
			fjac[(i__4 = l + mujacj + j * fjac_dim1 - fjac_offset)
				 < 1 * fjac_dim1 * fjac_dim2 && 0 <= i__4 ? 
				i__4 : s_rnge("fjac", i__4, "radcor_", (
				ftnlen)820)] = (cont[(i__5 = l - 1) < 1 * 
				cont_dim1 && 0 <= i__5 ? i__5 : s_rnge("cont",
				 i__5, "radcor_", (ftnlen)820)] - y0[(i__6 = 
				l - 1) < 1 * y0_dim1 && 0 <= i__6 ? i__6 : 
				s_rnge("y0", i__6, "radcor_", (ftnlen)820)]) /
				 f2[(i__7 = j - 1) < 1 * f2_dim1 && 0 <= i__7 
				? i__7 : s_rnge("f2", i__7, "radcor_", (
				ftnlen)820)];
/*<                   END DO >*/
		    }
/*<                   J=J+MD >*/
		    j += md;
/*<                   J1=J1+MD >*/
		    j1 += md;
/*<                   LBEG=LEND+1 >*/
		    lbeg = lend + 1;
/*<                   IF (J.LE.MM*M2) GOTO 14 >*/
		    if (j <= mm * *m2) {
			goto L14;
		    }
/*<                END DO >*/
		}
/*<             END DO >*/
	    }
/*<          ELSE >*/
	} else {
/* --- JACOBIAN IS FULL */
/*<             DO I=1,N >*/
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                YSAFE=Y(I) >*/
		ysafe = y[(i__2 = i__ - 1) < 1 * y_dim1 && 0 <= i__2 ? i__2 : 
			s_rnge("y", i__2, "radcor_", (ftnlen)831)];
/*<                DELT=DSQRT(UROUND*MAX(1.D-5,ABS(YSAFE))) >*/
/* Computing MAX */
		d__1 = 1e-5, d__2 = abs(ysafe);
		delt = sqrt(*uround * max(d__1,d__2));
/*<                Y(I)=YSAFE+DELT >*/
		y[(i__2 = i__ - 1) < 1 * y_dim1 && 0 <= i__2 ? i__2 : s_rnge(
			"y", i__2, "radcor_", (ftnlen)833)] = ysafe + delt;
/*<                CALL FCN(N,X,Y,CONT,RPAR,IPAR) >*/
		(*fcn)(n, x, y, cont, rpar, ipar);
/*<                DO J=M1+1,N >*/
		i__2 = *n;
		for (j = *m1 + 1; j <= i__2; ++j) {
/*<                  FJAC(J-M1,I)=(CONT(J)-Y0(J))/DELT >*/
		    fjac[(i__3 = j - *m1 + i__ * fjac_dim1 - fjac_offset) < 1 
			    * fjac_dim1 * fjac_dim2 && 0 <= i__3 ? i__3 : 
			    s_rnge("fjac", i__3, "radcor_", (ftnlen)836)] = (
			    cont[(i__4 = j - 1) < 1 * cont_dim1 && 0 <= i__4 ?
			     i__4 : s_rnge("cont", i__4, "radcor_", (ftnlen)
			    836)] - y0[(i__5 = j - 1) < 1 * y0_dim1 && 0 <= 
			    i__5 ? i__5 : s_rnge("y0", i__5, "radcor_", (
			    ftnlen)836)]) / delt;
/*<                END DO >*/
		}
/*<                Y(I)=YSAFE >*/
		y[(i__2 = i__ - 1) < 1 * y_dim1 && 0 <= i__2 ? i__2 : s_rnge(
			"y", i__2, "radcor_", (ftnlen)838)] = ysafe;
/*<             END DO >*/
	    }
/*<          END IF >*/
	}
/*<       ELSE >*/
    } else {
/* --- COMPUTE JACOBIAN MATRIX ANALYTICALLY */
/*<          CALL JAC(N,X,Y,FJAC,LDJAC,RPAR,IPAR) >*/
	(*jac)(n, x, y, fjac, ldjac, rpar, ipar);
/*<       END IF >*/
    }
/*<       CALJAC=.TRUE. >*/
    caljac = TRUE_;
/*<       CALHES=.TRUE. >*/
    calhes = TRUE_;
/*<   20  CONTINUE >*/
L20:
/* --- COMPUTE THE MATRICES E1 AND E2 AND THEIR DECOMPOSITIONS */
/*<       FAC1=U1/H >*/
    fac1 = u1 / *h__;
/*<       ALPHN=ALPH/H >*/
    alphn = alph / *h__;
/*<       BETAN=BETA/H >*/
    betan = beta / *h__;
/*<    >*/
    decomr_(n, fjac, ldjac, fmas, ldmas, mlmas, mumas, m1, m2, nm1, &fac1, e1,
	     lde1, ip1, &ier, ijob, &calhes, iphes);
/*<       IF (IER.NE.0) GOTO 78 >*/
    if (ier != 0) {
	goto L78;
    }
/*<    >*/
    decomc_(n, fjac, ldjac, fmas, ldmas, mlmas, mumas, m1, m2, nm1, &alphn, &
	    betan, e2r, e2i, lde1, ip2, &ier, ijob);
/*<       IF (IER.NE.0) GOTO 78 >*/
    if (ier != 0) {
	goto L78;
    }
/*<       NDEC=NDEC+1 >*/
    ++(*ndec);
/*<   30  CONTINUE >*/
L30:
/*<       NSTEP=NSTEP+1 >*/
    ++(*nstep);
/*<       IF (NSTEP.GT.NMAX) GOTO 178 >*/
    if (*nstep > *nmax) {
	goto L178;
    }
/*<       IF (0.1D0*ABS(H).LE.ABS(X)*UROUND) GOTO 177 >*/
    if (abs(*h__) * .1 <= abs(*x) * *uround) {
	goto L177;
    }
/*<           IF (INDEX2) THEN >*/
    if (index2) {
/*<              DO I=NIND1+1,NIND1+NIND2 >*/
	i__1 = *nind1 + *nind2;
	for (i__ = *nind1 + 1; i__ <= i__1; ++i__) {
/*<                 SCAL(I)=SCAL(I)/HHFAC >*/
	    scal[(i__2 = i__ - 1) < 1 * scal_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("scal", i__2, "radcor_", (ftnlen)865)] = scal[(
		    i__3 = i__ - 1) < 1 * scal_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("scal", i__3, "radcor_", (ftnlen)865)] / hhfac;
/*<              END DO >*/
	}
/*<           END IF >*/
    }
/*<           IF (INDEX3) THEN >*/
    if (index3) {
/*<              DO I=NIND1+NIND2+1,NIND1+NIND2+NIND3 >*/
	i__1 = *nind1 + *nind2 + *nind3;
	for (i__ = *nind1 + *nind2 + 1; i__ <= i__1; ++i__) {
/*<                 SCAL(I)=SCAL(I)/(HHFAC*HHFAC) >*/
	    scal[(i__2 = i__ - 1) < 1 * scal_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("scal", i__2, "radcor_", (ftnlen)870)] = scal[(
		    i__3 = i__ - 1) < 1 * scal_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("scal", i__3, "radcor_", (ftnlen)870)] / (hhfac * 
		    hhfac);
/*<              END DO >*/
	}
/*<           END IF >*/
    }
/*<       XPH=X+H >*/
    xph = *x + *h__;
/* *** *** *** *** *** *** *** */
/*  STARTING VALUES FOR NEWTON ITERATION */
/* *** *** *** *** *** *** *** */
/*<       IF (FIRST.OR.STARTN) THEN >*/
    if (first || *startn) {
/*<          DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<             Z1(I)=0.D0 >*/
	    z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "z1", i__2, "radcor_", (ftnlen)879)] = 0.;
/*<             Z2(I)=0.D0 >*/
	    z2[(i__2 = i__ - 1) < 1 * z2_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "z2", i__2, "radcor_", (ftnlen)880)] = 0.;
/*<             Z3(I)=0.D0 >*/
	    z3[(i__2 = i__ - 1) < 1 * z3_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "z3", i__2, "radcor_", (ftnlen)881)] = 0.;
/*<             F1(I)=0.D0 >*/
	    f1[(i__2 = i__ - 1) < 1 * f1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "f1", i__2, "radcor_", (ftnlen)882)] = 0.;
/*<             F2(I)=0.D0 >*/
	    f2[(i__2 = i__ - 1) < 1 * f2_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "f2", i__2, "radcor_", (ftnlen)883)] = 0.;
/*<             F3(I)=0.D0 >*/
	    f3[(i__2 = i__ - 1) < 1 * f3_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "f3", i__2, "radcor_", (ftnlen)884)] = 0.;
/*<          END DO >*/
	}
/*<       ELSE >*/
    } else {
/*<          C3Q=H/HOLD >*/
	c3q = *h__ / hold;
/*<          C1Q=C1*C3Q >*/
	c1q = c1 * c3q;
/*<          C2Q=C2*C3Q >*/
	c2q = c2 * c3q;
/*<          DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<             AK1=CONT(I+N) >*/
	    ak1 = cont[(i__2 = i__ + *n - 1) < 1 * cont_dim1 && 0 <= i__2 ? 
		    i__2 : s_rnge("cont", i__2, "radcor_", (ftnlen)891)];
/*<             AK2=CONT(I+N2) >*/
	    ak2 = cont[(i__2 = i__ + n2 - 1) < 1 * cont_dim1 && 0 <= i__2 ? 
		    i__2 : s_rnge("cont", i__2, "radcor_", (ftnlen)892)];
/*<             AK3=CONT(I+N3) >*/
	    ak3 = cont[(i__2 = i__ + n3 - 1) < 1 * cont_dim1 && 0 <= i__2 ? 
		    i__2 : s_rnge("cont", i__2, "radcor_", (ftnlen)893)];
/*<             Z1I=C1Q*(AK1+(C1Q-C2M1)*(AK2+(C1Q-C1M1)*AK3)) >*/
	    z1i = c1q * (ak1 + (c1q - conra5_1.c2m1) * (ak2 + (c1q - 
		    conra5_1.c1m1) * ak3));
/*<             Z2I=C2Q*(AK1+(C2Q-C2M1)*(AK2+(C2Q-C1M1)*AK3)) >*/
	    z2i = c2q * (ak1 + (c2q - conra5_1.c2m1) * (ak2 + (c2q - 
		    conra5_1.c1m1) * ak3));
/*<             Z3I=C3Q*(AK1+(C3Q-C2M1)*(AK2+(C3Q-C1M1)*AK3)) >*/
	    z3i = c3q * (ak1 + (c3q - conra5_1.c2m1) * (ak2 + (c3q - 
		    conra5_1.c1m1) * ak3));
/*<             Z1(I)=Z1I >*/
	    z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "z1", i__2, "radcor_", (ftnlen)897)] = z1i;
/*<             Z2(I)=Z2I >*/
	    z2[(i__2 = i__ - 1) < 1 * z2_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "z2", i__2, "radcor_", (ftnlen)898)] = z2i;
/*<             Z3(I)=Z3I >*/
	    z3[(i__2 = i__ - 1) < 1 * z3_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "z3", i__2, "radcor_", (ftnlen)899)] = z3i;
/*<             F1(I)=TI11*Z1I+TI12*Z2I+TI13*Z3I >*/
	    f1[(i__2 = i__ - 1) < 1 * f1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "f1", i__2, "radcor_", (ftnlen)900)] = ti11 * z1i + ti12 *
		     z2i + ti13 * z3i;
/*<             F2(I)=TI21*Z1I+TI22*Z2I+TI23*Z3I >*/
	    f2[(i__2 = i__ - 1) < 1 * f2_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "f2", i__2, "radcor_", (ftnlen)901)] = ti21 * z1i + ti22 *
		     z2i + ti23 * z3i;
/*<             F3(I)=TI31*Z1I+TI32*Z2I+TI33*Z3I >*/
	    f3[(i__2 = i__ - 1) < 1 * f3_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "f3", i__2, "radcor_", (ftnlen)902)] = ti31 * z1i + ti32 *
		     z2i + ti33 * z3i;
/*<          END DO >*/
	}
/*<       END IF >*/
    }
/* *** *** *** *** *** *** *** */
/*  LOOP FOR THE SIMPLIFIED NEWTON ITERATION */
/* *** *** *** *** *** *** *** */
/*<             NEWT=0 >*/
    newt = 0;
/*<             FACCON=MAX(FACCON,UROUND)**0.8D0 >*/
    d__1 = max(faccon,*uround);
    faccon = pow_dd(&d__1, &c_b325);
/*<             THETA=ABS(THET) >*/
    theta = abs(*thet);
/*<   40        CONTINUE >*/
L40:
/*<             IF (NEWT.GE.NIT) GOTO 78 >*/
    if (newt >= *nit) {
	goto L78;
    }
/* ---     COMPUTE THE RIGHT-HAND SIDE */
/*<             DO I=1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                CONT(I)=Y(I)+Z1(I) >*/
	cont[(i__2 = i__ - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"cont", i__2, "radcor_", (ftnlen)915)] = y[(i__3 = i__ - 1) < 
		1 * y_dim1 && 0 <= i__3 ? i__3 : s_rnge("y", i__3, "radcor_", 
		(ftnlen)915)] + z1[(i__4 = i__ - 1) < 1 * z1_dim1 && 0 <= 
		i__4 ? i__4 : s_rnge("z1", i__4, "radcor_", (ftnlen)915)];
/*<             END DO >*/
    }
/*<             CALL FCN(N,X+C1*H,CONT,Z1,RPAR,IPAR) >*/
    d__1 = *x + c1 * *h__;
    (*fcn)(n, &d__1, cont, z1, rpar, ipar);
/*<             DO I=1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                CONT(I)=Y(I)+Z2(I) >*/
	cont[(i__2 = i__ - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"cont", i__2, "radcor_", (ftnlen)919)] = y[(i__3 = i__ - 1) < 
		1 * y_dim1 && 0 <= i__3 ? i__3 : s_rnge("y", i__3, "radcor_", 
		(ftnlen)919)] + z2[(i__4 = i__ - 1) < 1 * z2_dim1 && 0 <= 
		i__4 ? i__4 : s_rnge("z2", i__4, "radcor_", (ftnlen)919)];
/*<             END DO >*/
    }
/*<             CALL FCN(N,X+C2*H,CONT,Z2,RPAR,IPAR) >*/
    d__1 = *x + c2 * *h__;
    (*fcn)(n, &d__1, cont, z2, rpar, ipar);
/*<             DO I=1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                CONT(I)=Y(I)+Z3(I) >*/
	cont[(i__2 = i__ - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"cont", i__2, "radcor_", (ftnlen)923)] = y[(i__3 = i__ - 1) < 
		1 * y_dim1 && 0 <= i__3 ? i__3 : s_rnge("y", i__3, "radcor_", 
		(ftnlen)923)] + z3[(i__4 = i__ - 1) < 1 * z3_dim1 && 0 <= 
		i__4 ? i__4 : s_rnge("z3", i__4, "radcor_", (ftnlen)923)];
/*<             END DO >*/
    }
/*<             CALL FCN(N,XPH,CONT,Z3,RPAR,IPAR) >*/
    (*fcn)(n, &xph, cont, z3, rpar, ipar);
/*<             NFCN=NFCN+3 >*/
    *nfcn += 3;
/* ---     SOLVE THE LINEAR SYSTEMS */
/*<            DO I=1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<               A1=Z1(I) >*/
	a1 = z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"z1", i__2, "radcor_", (ftnlen)929)];
/*<               A2=Z2(I) >*/
	a2 = z2[(i__2 = i__ - 1) < 1 * z2_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"z2", i__2, "radcor_", (ftnlen)930)];
/*<               A3=Z3(I) >*/
	a3 = z3[(i__2 = i__ - 1) < 1 * z3_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"z3", i__2, "radcor_", (ftnlen)931)];
/*<               Z1(I)=TI11*A1+TI12*A2+TI13*A3 >*/
	z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : s_rnge("z1", 
		i__2, "radcor_", (ftnlen)932)] = ti11 * a1 + ti12 * a2 + ti13 
		* a3;
/*<               Z2(I)=TI21*A1+TI22*A2+TI23*A3 >*/
	z2[(i__2 = i__ - 1) < 1 * z2_dim1 && 0 <= i__2 ? i__2 : s_rnge("z2", 
		i__2, "radcor_", (ftnlen)933)] = ti21 * a1 + ti22 * a2 + ti23 
		* a3;
/*<               Z3(I)=TI31*A1+TI32*A2+TI33*A3 >*/
	z3[(i__2 = i__ - 1) < 1 * z3_dim1 && 0 <= i__2 ? i__2 : s_rnge("z3", 
		i__2, "radcor_", (ftnlen)934)] = ti31 * a1 + ti32 * a2 + ti33 
		* a3;
/*<            END DO >*/
    }
/*<    >*/
    slvrad_(n, fjac, ldjac, mljac, mujac, fmas, ldmas, mlmas, mumas, m1, m2, 
	    nm1, &fac1, &alphn, &betan, e1, e2r, e2i, lde1, z1, z2, z3, f1, 
	    f2, f3, cont, ip1, ip2, iphes, &ier, ijob);
/*<             NSOL=NSOL+1 >*/
    ++(*nsol);
/*<             NEWT=NEWT+1 >*/
    ++newt;
/*<             DYNO=0.D0 >*/
    dyno = 0.;
/*<             DO I=1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                DENOM=SCAL(I) >*/
	denom = scal[(i__2 = i__ - 1) < 1 * scal_dim1 && 0 <= i__2 ? i__2 : 
		s_rnge("scal", i__2, "radcor_", (ftnlen)943)];
/*<    >*/
/* Computing 2nd power */
	d__1 = z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"z1", i__2, "radcor_", (ftnlen)944)] / denom;
/* Computing 2nd power */
	d__2 = z2[(i__3 = i__ - 1) < 1 * z2_dim1 && 0 <= i__3 ? i__3 : s_rnge(
		"z2", i__3, "radcor_", (ftnlen)944)] / denom;
/* Computing 2nd power */
	d__3 = z3[(i__4 = i__ - 1) < 1 * z3_dim1 && 0 <= i__4 ? i__4 : s_rnge(
		"z3", i__4, "radcor_", (ftnlen)944)] / denom;
	dyno = dyno + d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
/*<             END DO >*/
    }
/*<             DYNO=DSQRT(DYNO/N3) >*/
    dyno = sqrt(dyno / n3);
/* ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE */
/*<             IF (NEWT.GT.1.AND.NEWT.LT.NIT) THEN >*/
    if (newt > 1 && newt < *nit) {
/*<                 THQ=DYNO/DYNOLD >*/
	thq = dyno / dynold;
/*<                 IF (NEWT.EQ.2) THEN >*/
	if (newt == 2) {
/*<                    THETA=THQ >*/
	    theta = thq;
/*<                 ELSE >*/
	} else {
/*<                    THETA=SQRT(THQ*THQOLD) >*/
	    theta = sqrt(thq * thqold);
/*<                 END IF >*/
	}
/*<                 THQOLD=THQ >*/
	thqold = thq;
/*<                 IF (THETA.LT.0.99D0) THEN >*/
	if (theta < .99) {
/*<                     FACCON=THETA/(1.0D0-THETA) >*/
	    faccon = theta / (1. - theta);
/*<                     DYTH=FACCON*DYNO*THETA**(NIT-1-NEWT)/FNEWT >*/
	    i__1 = *nit - 1 - newt;
	    dyth = faccon * dyno * pow_di(&theta, &i__1) / *fnewt;
/*<                     IF (DYTH.GE.1.0D0) THEN >*/
	    if (dyth >= 1.) {
/*<                          QNEWT=DMAX1(1.0D-4,DMIN1(20.0D0,DYTH)) >*/
/* Computing MAX */
		d__1 = 1e-4, d__2 = min(20.,dyth);
		qnewt = max(d__1,d__2);
/*<                          HHFAC=.8D0*QNEWT**(-1.0D0/(4.0D0+NIT-1-NEWT)) >*/
		d__1 = -1. / (*nit + 4. - 1 - newt);
		hhfac = pow_dd(&qnewt, &d__1) * .8;
/*<                          H=HHFAC*H >*/
		*h__ = hhfac * *h__;
/*<                          REJECT=.TRUE. >*/
		reject = TRUE_;
/*<                          LAST=.FALSE. >*/
		last = FALSE_;
/*<                          IF (CALJAC) GOTO 20 >*/
		if (caljac) {
		    goto L20;
		}
/*<                          GOTO 10 >*/
		goto L10;
/*<                     END IF >*/
	    }
/*<                 ELSE >*/
	} else {
/*<                     GOTO 78 >*/
	    goto L78;
/*<                 END IF >*/
	}
/*<             END IF >*/
    }
/*<             DYNOLD=MAX(DYNO,UROUND) >*/
    dynold = max(dyno,*uround);
/*<             DO I=1,N >*/
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                F1I=F1(I)+Z1(I) >*/
	f1i = f1[(i__2 = i__ - 1) < 1 * f1_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"f1", i__2, "radcor_", (ftnlen)975)] + z1[(i__3 = i__ - 1) < 
		1 * z1_dim1 && 0 <= i__3 ? i__3 : s_rnge("z1", i__3, "radcor_"
		, (ftnlen)975)];
/*<                F2I=F2(I)+Z2(I) >*/
	f2i = f2[(i__2 = i__ - 1) < 1 * f2_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"f2", i__2, "radcor_", (ftnlen)976)] + z2[(i__3 = i__ - 1) < 
		1 * z2_dim1 && 0 <= i__3 ? i__3 : s_rnge("z2", i__3, "radcor_"
		, (ftnlen)976)];
/*<                F3I=F3(I)+Z3(I) >*/
	f3i = f3[(i__2 = i__ - 1) < 1 * f3_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		"f3", i__2, "radcor_", (ftnlen)977)] + z3[(i__3 = i__ - 1) < 
		1 * z3_dim1 && 0 <= i__3 ? i__3 : s_rnge("z3", i__3, "radcor_"
		, (ftnlen)977)];
/*<                F1(I)=F1I >*/
	f1[(i__2 = i__ - 1) < 1 * f1_dim1 && 0 <= i__2 ? i__2 : s_rnge("f1", 
		i__2, "radcor_", (ftnlen)978)] = f1i;
/*<                F2(I)=F2I >*/
	f2[(i__2 = i__ - 1) < 1 * f2_dim1 && 0 <= i__2 ? i__2 : s_rnge("f2", 
		i__2, "radcor_", (ftnlen)979)] = f2i;
/*<                F3(I)=F3I >*/
	f3[(i__2 = i__ - 1) < 1 * f3_dim1 && 0 <= i__2 ? i__2 : s_rnge("f3", 
		i__2, "radcor_", (ftnlen)980)] = f3i;
/*<                Z1(I)=T11*F1I+T12*F2I+T13*F3I >*/
	z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : s_rnge("z1", 
		i__2, "radcor_", (ftnlen)981)] = t11 * f1i + t12 * f2i + t13 *
		 f3i;
/*<                Z2(I)=T21*F1I+T22*F2I+T23*F3I >*/
	z2[(i__2 = i__ - 1) < 1 * z2_dim1 && 0 <= i__2 ? i__2 : s_rnge("z2", 
		i__2, "radcor_", (ftnlen)982)] = t21 * f1i + t22 * f2i + t23 *
		 f3i;
/*<                Z3(I)=T31*F1I+    F2I >*/
	z3[(i__2 = i__ - 1) < 1 * z3_dim1 && 0 <= i__2 ? i__2 : s_rnge("z3", 
		i__2, "radcor_", (ftnlen)983)] = t31 * f1i + f2i;
/*<             END DO >*/
    }
/*<             IF (FACCON*DYNO.GT.FNEWT) GOTO 40 >*/
    if (faccon * dyno > *fnewt) {
	goto L40;
    }
/* --- ERROR ESTIMATION */
/*<    >*/
    estrad_(n, fjac, ldjac, mljac, mujac, fmas, ldmas, mlmas, mumas, h__, &
	    dd1, &dd2, &dd3, (S_fp)fcn, nfcn, y0, y, ijob, x, m1, m2, nm1, e1,
	     lde1, z1, z2, z3, cont, f1, f2, ip1, iphes, scal, &err, &first, &
	    reject, &fac1, rpar, ipar);
/* --- COMPUTATION OF HNEW */
/* --- WE REQUIRE .2<=HNEW/H<=8. */
/*<       FAC=MIN(SAFE,CFAC/(NEWT+2*NIT)) >*/
/* Computing MIN */
    d__1 = *safe, d__2 = cfac / (newt + (*nit << 1));
    fac = min(d__1,d__2);
/*<       QUOT=MAX(FACR,MIN(FACL,ERR**.25D0/FAC)) >*/
/* Computing MAX */
/* Computing MIN */
    d__3 = *facl, d__4 = pow_dd(&err, &c_b389) / fac;
    d__1 = *facr, d__2 = min(d__3,d__4);
    quot = max(d__1,d__2);
/*<       HNEW=H/QUOT >*/
    hnew = *h__ / quot;
/* *** *** *** *** *** *** *** */
/*  IS THE ERROR SMALL ENOUGH ? */
/* *** *** *** *** *** *** *** */
/*<       IF (ERR.LT.1.D0) THEN >*/
    if (err < 1.) {
/* --- STEP IS ACCEPTED */
/*<          FIRST=.FALSE. >*/
	first = FALSE_;
/*<          NACCPT=NACCPT+1 >*/
	++(*naccpt);
/*<          IF (PRED) THEN >*/
	if (*pred) {
/*       --- PREDICTIVE CONTROLLER OF GUSTAFSSON */
/*<             IF (NACCPT.GT.1) THEN >*/
	    if (*naccpt > 1) {
/*<                FACGUS=(HACC/H)*(ERR**2/ERRACC)**0.25D0/SAFE >*/
/* Computing 2nd power */
		d__2 = err;
		d__1 = d__2 * d__2 / erracc;
		facgus = hacc / *h__ * pow_dd(&d__1, &c_b389) / *safe;
/*<                FACGUS=MAX(FACR,MIN(FACL,FACGUS)) >*/
/* Computing MAX */
		d__1 = *facr, d__2 = min(*facl,facgus);
		facgus = max(d__1,d__2);
/*<                QUOT=MAX(QUOT,FACGUS) >*/
		quot = max(quot,facgus);
/*<                HNEW=H/QUOT >*/
		hnew = *h__ / quot;
/*<             END IF >*/
	    }
/*<             HACC=H >*/
	    hacc = *h__;
/*<             ERRACC=MAX(1.0D-2,ERR) >*/
	    erracc = max(.01,err);
/*<          END IF >*/
	}
/*<          XOLD=X >*/
	xold = *x;
/*<          HOLD=H >*/
	hold = *h__;
/*<          X=XPH  >*/
	*x = xph;
/*<          DO I=1,N >*/
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/*<             Y(I)=Y(I)+Z3(I)   >*/
	    y[(i__2 = i__ - 1) < 1 * y_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "y", i__2, "radcor_", (ftnlen)1018)] = y[(i__3 = i__ - 1) 
		    < 1 * y_dim1 && 0 <= i__3 ? i__3 : s_rnge("y", i__3, 
		    "radcor_", (ftnlen)1018)] + z3[(i__4 = i__ - 1) < 1 * 
		    z3_dim1 && 0 <= i__4 ? i__4 : s_rnge("z3", i__4, "radcor_"
		    , (ftnlen)1018)];
/*<             Z2I=Z2(I) >*/
	    z2i = z2[(i__2 = i__ - 1) < 1 * z2_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("z2", i__2, "radcor_", (ftnlen)1019)];
/*<             Z1I=Z1(I) >*/
	    z1i = z1[(i__2 = i__ - 1) < 1 * z1_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("z1", i__2, "radcor_", (ftnlen)1020)];
/*<             CONT(I+N)=(Z2I-Z3(I))/C2M1 >*/
	    cont[(i__2 = i__ + *n - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("cont", i__2, "radcor_", (ftnlen)1021)] = (z2i - 
		    z3[(i__3 = i__ - 1) < 1 * z3_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("z3", i__3, "radcor_", (ftnlen)1021)]) / 
		    conra5_1.c2m1;
/*<             AK=(Z1I-Z2I)/C1MC2 >*/
	    ak = (z1i - z2i) / c1mc2;
/*<             ACONT3=Z1I/C1 >*/
	    acont3 = z1i / c1;
/*<             ACONT3=(AK-ACONT3)/C2 >*/
	    acont3 = (ak - acont3) / c2;
/*<             CONT(I+N2)=(AK-CONT(I+N))/C1M1 >*/
	    cont[(i__2 = i__ + n2 - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("cont", i__2, "radcor_", (ftnlen)1025)] = (ak - 
		    cont[(i__3 = i__ + *n - 1) < 1 * cont_dim1 && 0 <= i__3 ? 
		    i__3 : s_rnge("cont", i__3, "radcor_", (ftnlen)1025)]) / 
		    conra5_1.c1m1;
/*<             CONT(I+N3)=CONT(I+N2)-ACONT3 >*/
	    cont[(i__2 = i__ + n3 - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("cont", i__2, "radcor_", (ftnlen)1026)] = cont[(
		    i__3 = i__ + n2 - 1) < 1 * cont_dim1 && 0 <= i__3 ? i__3 :
		     s_rnge("cont", i__3, "radcor_", (ftnlen)1026)] - acont3;
/*<          END DO >*/
	}
/*<          IF (ITOL.EQ.0) THEN >*/
	if (*itol == 0) {
/*<              DO I=1,N >*/
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                 SCAL(I)=ATOL(1)+RTOL(1)*ABS(Y(I)) >*/
		scal[(i__3 = i__ - 1) < 1 * scal_dim1 && 0 <= i__3 ? i__3 : 
			s_rnge("scal", i__3, "radcor_", (ftnlen)1030)] = atol[
			0] + rtol[0] * (d__1 = y[(i__2 = i__ - 1) < 1 * 
			y_dim1 && 0 <= i__2 ? i__2 : s_rnge("y", i__2, "radc"
			"or_", (ftnlen)1030)], abs(d__1));
/*<              END DO >*/
	    }
/*<          ELSE >*/
	} else {
/*<              DO I=1,N >*/
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                 SCAL(I)=ATOL(I)+RTOL(I)*ABS(Y(I)) >*/
		scal[(i__3 = i__ - 1) < 1 * scal_dim1 && 0 <= i__3 ? i__3 : 
			s_rnge("scal", i__3, "radcor_", (ftnlen)1034)] = atol[
			i__ - 1] + rtol[i__ - 1] * (d__1 = y[(i__2 = i__ - 1) 
			< 1 * y_dim1 && 0 <= i__2 ? i__2 : s_rnge("y", i__2, 
			"radcor_", (ftnlen)1034)], abs(d__1));
/*<              END DO >*/
	    }
/*<          END IF >*/
	}
/*<          IF (IOUT.NE.0) THEN >*/
	if (*iout != 0) {
/*<              NRSOL=NACCPT+1 >*/
	    nrsol = *naccpt + 1;
/*<              XSOL=X >*/
	    conra5_1.xsol = *x;
/*<              XOSOL=XOLD >*/
	    xosol = xold;
/*<              DO I=1,N >*/
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/*<                 CONT(I)=Y(I) >*/
		cont[(i__2 = i__ - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : 
			s_rnge("cont", i__2, "radcor_", (ftnlen)1042)] = y[(
			i__3 = i__ - 1) < 1 * y_dim1 && 0 <= i__3 ? i__3 : 
			s_rnge("y", i__3, "radcor_", (ftnlen)1042)];
/*<              END DO >*/
	    }
/*<              NSOLU=N >*/
	    nsolu = *n;
/*<              HSOL=HOLD >*/
	    conra5_1.hsol = hold;
/*<    >*/
	    (*solout)(&nrsol, &xosol, &conra5_1.xsol, y, cont, &lrc, &nsolu, 
		    rpar, ipar, &irtrn);
/*<              IF (IRTRN.LT.0) GOTO 179 >*/
	    if (irtrn < 0) {
		goto L179;
	    }
/*<          END IF >*/
	}
/*<          CALJAC=.FALSE. >*/
	caljac = FALSE_;
/*<          IF (LAST) THEN >*/
	if (last) {
/*<             H=HOPT >*/
	    *h__ = hopt;
/*<             IDID=1 >*/
	    *idid = 1;
/*<             RETURN >*/
	    return 0;
/*<          END IF >*/
	}
/*<          CALL FCN(N,X,Y,Y0,RPAR,IPAR) >*/
	(*fcn)(n, x, y, y0, rpar, ipar);
/*<          NFCN=NFCN+1 >*/
	++(*nfcn);
/*<          HNEW=POSNEG*MIN(ABS(HNEW),HMAXN) >*/
/* Computing MIN */
	d__1 = abs(hnew);
	hnew = posneg * min(d__1,hmaxn);
/*<          HOPT=HNEW >*/
	hopt = hnew;
/*<          HOPT=MIN(H,HNEW) >*/
	hopt = min(*h__,hnew);
/*<          IF (REJECT) HNEW=POSNEG*MIN(ABS(HNEW),ABS(H))  >*/
	if (reject) {
/* Computing MIN */
	    d__1 = abs(hnew), d__2 = abs(*h__);
	    hnew = posneg * min(d__1,d__2);
	}
/*<          REJECT=.FALSE. >*/
	reject = FALSE_;
/*<          IF ((X+HNEW/QUOT1-XEND)*POSNEG.GE.0.D0) THEN >*/
	if ((*x + hnew / *quot1 - *xend) * posneg >= 0.) {
/*<             H=XEND-X >*/
	    *h__ = *xend - *x;
/*<             LAST=.TRUE. >*/
	    last = TRUE_;
/*<          ELSE >*/
	} else {
/*<             QT=HNEW/H  >*/
	    qt = hnew / *h__;
/*<             HHFAC=H >*/
	    hhfac = *h__;
/*<             IF (THETA.LE.THET.AND.QT.GE.QUOT1.AND.QT.LE.QUOT2) GOTO 30 >*/
	    if (theta <= *thet && qt >= *quot1 && qt <= *quot2) {
		goto L30;
	    }
/*<             H=HNEW  >*/
	    *h__ = hnew;
/*<          END IF >*/
	}
/*<          HHFAC=H >*/
	hhfac = *h__;
/*<          IF (THETA.LE.THET) GOTO 20 >*/
	if (theta <= *thet) {
	    goto L20;
	}
/*<          GOTO 10 >*/
	goto L10;
/*<       ELSE >*/
    } else {
/* --- STEP IS REJECTED */
/*<          REJECT=.TRUE. >*/
	reject = TRUE_;
/*<          LAST=.FALSE. >*/
	last = FALSE_;
/*<          IF (FIRST) THEN >*/
	if (first) {
/*<              H=H*0.1D0 >*/
	    *h__ *= .1;
/*<              HHFAC=0.1D0 >*/
	    hhfac = .1;
/*<          ELSE  >*/
	} else {
/*<              HHFAC=HNEW/H >*/
	    hhfac = hnew / *h__;
/*<              H=HNEW >*/
	    *h__ = hnew;
/*<          END IF >*/
	}
/*<          IF (NACCPT.GE.1) NREJCT=NREJCT+1 >*/
	if (*naccpt >= 1) {
	    ++(*nrejct);
	}
/*<          IF (CALJAC) GOTO 20 >*/
	if (caljac) {
	    goto L20;
	}
/*<          GOTO 10 >*/
	goto L10;
/*<       END IF >*/
    }
/* --- UNEXPECTED STEP-REJECTION */
/*<   78  CONTINUE >*/
L78:
/*<       IF (IER.NE.0) THEN >*/
    if (ier != 0) {
/*<           NSING=NSING+1 >*/
	++nsing;
/*<           IF (NSING.GE.5) GOTO 176 >*/
	if (nsing >= 5) {
	    goto L176;
	}
/*<       END IF >*/
    }
/*<       H=H*0.5D0  >*/
    *h__ *= .5;
/*<       HHFAC=0.5D0 >*/
    hhfac = .5;
/*<       REJECT=.TRUE. >*/
    reject = TRUE_;
/*<       LAST=.FALSE. >*/
    last = FALSE_;
/*<       IF (CALJAC) GOTO 20 >*/
    if (caljac) {
	goto L20;
    }
/*<       GOTO 10 >*/
    goto L10;
/* --- FAIL EXIT */
/*<  176  CONTINUE >*/
L176:
/*<       WRITE(6,979)X    >*/
    s_wsfe(&io___176);
    do_fio(&c__1, (char *)&(*x), (ftnlen)sizeof(doublereal));
    e_wsfe();
/*<       WRITE(6,*) ' MATRIX IS REPEATEDLY SINGULAR, IER=',IER >*/
    s_wsle(&io___177);
    do_lio(&c__9, &c__1, " MATRIX IS REPEATEDLY SINGULAR, IER=", (ftnlen)36);
    do_lio(&c__3, &c__1, (char *)&ier, (ftnlen)sizeof(integer));
    e_wsle();
/*<       IDID=-4 >*/
    *idid = -4;
/*<       RETURN >*/
    return 0;
/*<  177  CONTINUE >*/
L177:
/*<       WRITE(6,979)X    >*/
    s_wsfe(&io___178);
    do_fio(&c__1, (char *)&(*x), (ftnlen)sizeof(doublereal));
    e_wsfe();
/*<       WRITE(6,*) ' STEP SIZE T0O SMALL, H=',H >*/
    s_wsle(&io___179);
    do_lio(&c__9, &c__1, " STEP SIZE T0O SMALL, H=", (ftnlen)24);
    do_lio(&c__5, &c__1, (char *)&(*h__), (ftnlen)sizeof(doublereal));
    e_wsle();
/*<       IDID=-3 >*/
    *idid = -3;
/*<       RETURN >*/
    return 0;
/*<  178  CONTINUE >*/
L178:
/*<       WRITE(6,979)X    >*/
    s_wsfe(&io___180);
    do_fio(&c__1, (char *)&(*x), (ftnlen)sizeof(doublereal));
    e_wsfe();
/*<       WRITE(6,*) ' MORE THAN NMAX =',NMAX,'STEPS ARE NEEDED'  >*/
    s_wsle(&io___181);
    do_lio(&c__9, &c__1, " MORE THAN NMAX =", (ftnlen)17);
    do_lio(&c__3, &c__1, (char *)&(*nmax), (ftnlen)sizeof(integer));
    do_lio(&c__9, &c__1, "STEPS ARE NEEDED", (ftnlen)16);
    e_wsle();
/*<       IDID=-2 >*/
    *idid = -2;
/*<       RETURN >*/
    return 0;
/* --- EXIT CAUSED BY SOLOUT */
/*<  179  CONTINUE >*/
L179:
/*<       WRITE(6,979)X >*/
    s_wsfe(&io___182);
    do_fio(&c__1, (char *)&(*x), (ftnlen)sizeof(doublereal));
    e_wsfe();
/*<  979  FORMAT(' EXIT OF RADAU5 AT X=',E18.4)  >*/
/*<       IDID=2 >*/
    *idid = 2;
/*<       RETURN >*/
    return 0;
/*<       END >*/
} /* radcor_ */


/*     END OF SUBROUTINE RADCOR */

/* *********************************************************** */

/*<       DOUBLE PRECISION FUNCTION CONTR5(I,X,CONT,LRC)  >*/
doublereal contr5_(integer *i__, doublereal *x, doublereal *cont, integer *
	lrc)
{
    /* System generated locals */
    integer cont_dim1, i__1, i__2, i__3, i__4;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static doublereal s;

/* ---------------------------------------------------------- */
/*     THIS FUNCTION CAN BE USED FOR CONINUOUS OUTPUT. IT PROVIDES AN */
/*     APPROXIMATION TO THE I-TH COMPONENT OF THE SOLUTION AT X. */
/*     IT GIVES THE VALUE OF THE COLLOCATION POLYNOMIAL, DEFINED FOR */
/*     THE LAST SUCCESSFULLY COMPUTED STEP (BY RADAU5). */
/* ---------------------------------------------------------- */
/*<       IMPLICIT DOUBLE PRECISION (A-H,O-Z) >*/
/*<       DIMENSION CONT(LRC) >*/
/*<       COMMON /CONRA5/NN,NN2,NN3,NN4,XSOL,HSOL,C2M1,C1M1 >*/
/*<       S=(X-XSOL)/HSOL >*/
    /* Parameter adjustments */
    cont_dim1 = *lrc;

    /* Function Body */
    s = (*x - conra5_1.xsol) / conra5_1.hsol;
/*<    >*/
    ret_val = cont[(i__1 = *i__ - 1) < 1 * cont_dim1 && 0 <= i__1 ? i__1 : 
	    s_rnge("cont", i__1, "contr5_", (ftnlen)1141)] + s * (cont[(i__2 =
	     *i__ + conra5_1.nn - 1) < 1 * cont_dim1 && 0 <= i__2 ? i__2 : 
	    s_rnge("cont", i__2, "contr5_", (ftnlen)1141)] + (s - 
	    conra5_1.c2m1) * (cont[(i__3 = *i__ + conra5_1.nn2 - 1) < 1 * 
	    cont_dim1 && 0 <= i__3 ? i__3 : s_rnge("cont", i__3, "contr5_", (
	    ftnlen)1141)] + (s - conra5_1.c1m1) * cont[(i__4 = *i__ + 
	    conra5_1.nn3 - 1) < 1 * cont_dim1 && 0 <= i__4 ? i__4 : s_rnge(
	    "cont", i__4, "contr5_", (ftnlen)1141)]));
/*<       RETURN >*/
    return ret_val;
/*<       END >*/
} /* contr5_ */

