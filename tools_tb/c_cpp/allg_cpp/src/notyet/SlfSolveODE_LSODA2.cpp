#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "f2c.h"
#include "SlfSolveODE.h"    
/* Common Block Declarations */


typedef struct tag_struct_dls001_ {
    doublereal rowns[209]
             , ccmax
             , el0
             , h__
             , hmin
             , hmxi
             , hu
             , rc
             , tn
             , uround;
    integer init
          , mxstep
          , mxhnil
          , nhnil
          , nslast
          , nyh
          , iowns[6]
          , icf
          , ierpj
          , iersl
          , jcur
          , jstart
          , kflag
          , l
          , lyh
          , lewt
          , lacor
          , lsavf
          , lwm
          , liwm
          , meth
          , miter
          , maxord
          , maxcor
          , msbp
          , mxncf
          , n
          , nq
          , nst
          , nfe
          , nje
          , nqu;
} struct_dls001_;

typedef struct tag_struct_dls002_ {
    doublereal conit
             , crate
             , el[13]
             , elco[156]	/* was [13][12] */
             , hold
             , rmax
             , tesco[36]	/* was [3][12] */
             , ccmax
             , el0
             , h__
             , hmin
             , hmxi
             , hu
             , rc
             , tn
             , uround;
    integer iownd[6]
          , ialth
          , ipup
          , lmax
          , meo
          , nqnyh
          , nslp
          , icf
          , ierpj
          , iersl
          , jcur
          , jstart
          , kflag
          , l
          , lyh
          , lewt
          , lacor
          , lsavf
          , lwm
          , liwm
          , meth
          , miter
          , maxord
          , maxcor
          , msbp
          , mxncf
          , n
          , nq
          , nst
          , nfe
          , nje
          , nqu;
} struct_dls002_;
typedef struct tag_struct_dls003_ {
    doublereal rowns[209]
             , ccmax
             , el0
             , h__
             , hmin
             , hmxi
             , hu
             , rc
             , tn
             , uround;
    integer iownd[6]
          , iowns[6]
          , icf
          , ierpj
          , iersl
          , jcur
          , jstart
          , kflag
          , l
          , lyh
          , lewt
          , lacor
          , lsavf
          , lwm
          , liwm
          , meth
          , miter
          , maxord
          , maxcor
          , msbp
          , mxncf
          , n
          , nq
          , nst
          , nfe
          , nje
          , nqu;
} struct_dls003_;

typedef union tag_union_dls001{

    struct_dls001_ a;
    struct_dls002_ b;
    struct_dls003_ c;
}union_dls001;

extern union_dls001 dls001_;

#define dls001_1 dls001_.b
#define dls001_2 dls001_.c

typedef struct tag_struct_dlsa01_ {
    doublereal tsw
             , rowns2[20]
             , pdnorm;
    integer insufr
          , insufi
          , ixpr
          , iowns2[2]
          , jtyp
          , mused
          , mxordn
          , mxords;
} struct_dlsa01_;
typedef struct tag_struct_dlsa02_ {
    doublereal rownd2, cm1[12], cm2[5], pdest, pdlast, ratio, pdnorm;
    integer iownd2[3], icount, irflag, jtyp, mused, mxordn, mxords;
} struct_dlsa02_;

typedef union tag_union_dlsa01{

    struct_dlsa01_ a;
    struct_dlsa02_ b;
}union_dlsa01;

extern union_dlsa01 dlsa01_;

#define dlsa01_1 dlsa01_.b

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__30 = 30;
static integer c__51 = 51;
static doublereal c_b20 = 0.;
static integer c__52 = 52;
static integer c__60 = 60;

#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
extern CSlfStr    *pIntegratorLSODA_ErrText;
#else
extern char       *pIntegratorLSODA_errtext;
#endif
//====================================================================================
//
// DSTODA
//
/* ----------------------------------------------------------------------- */
/* DSTODA performs one step of the integration of an initial value */
/* problem for a system of ordinary differential equations. */
/* Note: DSTODA is independent of the value of the iteration method */
/* indicator MITER, when this is .ne. 0, and hence is independent */
/* of the type of chord method used, or the Jacobian structure. */
/* Communication with DSTODA is done with the following variables: */

/* Y      = an array of length .ge. N used as the Y argument in */
/*          all calls to F and JAC. */
/* NEQ    = integer array containing problem size in NEQ(1), and */
/*          passed as the NEQ argument in all calls to F and JAC. */
/* YH     = an NYH by LMAX array containing the dependent variables */
/*          and their approximate scaled derivatives, where */
/*          LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate */
/*          j-th derivative of y(i), scaled by H**j/factorial(j) */
/*          (j = 0,1,...,NQ).  On entry for the first step, the first */
/*          two columns of YH must be set from the initial values. */
/* NYH    = a constant integer .ge. N, the first dimension of YH. */
/* YH1    = a one-dimensional array occupying the same space as YH. */
/* EWT    = an array of length N containing multiplicative weights */
/*          for local error measurements.  Local errors in y(i) are */
/*          compared to 1.0/EWT(i) in various error tests. */
/* SAVF   = an array of working storage, of length N. */
/* ACOR   = a work array of length N, used for the accumulated */
/*          corrections.  On a successful return, ACOR(i) contains */
/*          the estimated one-step local error in y(i). */
/* WM,IWM = real and integer work arrays associated with matrix */
/*          operations in chord iteration (MITER .ne. 0). */
/* PJAC   = name of routine to evaluate and preprocess Jacobian matrix */
/*          and P = I - H*EL0*Jac, if a chord method is being used. */
/*          It also returns an estimate of norm(Jac) in PDNORM. */
/* SLVS   = name of routine to solve linear system in chord iteration. */
/* CCMAX  = maximum relative change in H*EL0 before PJAC is called. */
/* H      = the step size to be attempted on the next step. */
/*          H is altered by the error control algorithm during the */
/*          problem.  H can be either positive or negative, but its */
/*          sign must remain constant throughout the problem. */
/* HMIN   = the minimum absolute value of the step size H to be used. */
/* HMXI   = inverse of the maximum absolute value of H to be used. */
/*          HMXI = 0.0 is allowed and corresponds to an infinite HMAX. */
/*          HMIN and HMXI may be changed at any time, but will not */
/*          take effect until the next change of H is considered. */
/* TN     = the independent variable. TN is updated on each step taken. */
/* JSTART = an integer used for input only, with the following */
/*          values and meanings: */
/*               0  perform the first step. */
/*           .gt.0  take a new step continuing from the last. */
/*              -1  take the next step with a new value of H, */
/*                    N, METH, MITER, and/or matrix parameters. */
/*              -2  take the next step with a new value of H, */
/*                    but with other inputs unchanged. */
/*          On return, JSTART is set to 1 to facilitate continuation. */
/* KFLAG  = a completion code with the following meanings: */
/*               0  the step was succesful. */
/*              -1  the requested error could not be achieved. */
/*              -2  corrector convergence could not be achieved. */
/*              -3  fatal error in PJAC or SLVS. */
/*              -8  fatal error in state-Funktion
/*              -9  fatal error in jacobi-Funktion
/*          A return with KFLAG = -1 or -2 means either */
/*          ABS(H) = HMIN or 10 consecutive failures occurred. */
/*          On a return with KFLAG negative, the values of TN and */
/*          the YH array are as of the beginning of the last */
/*          step, and H is the last step size attempted. */
/* MAXORD = the maximum order of integration method to be allowed. */
/* MAXCOR = the maximum number of corrector iterations allowed. */
/* MSBP   = maximum number of steps between PJAC calls (MITER .gt. 0). */
/* MXNCF  = maximum number of convergence failures allowed. */
/* METH   = current method. */
/*          METH = 1 means Adams method (nonstiff) */
/*          METH = 2 means BDF method (stiff) */
/*          METH may be reset by DSTODA. */
/* MITER  = corrector iteration method. */
/*          MITER = 0 means functional iteration. */
/*          MITER = JT .gt. 0 means a chord iteration corresponding */
/*          to Jacobian type JT.  (The DLSODA/DLSODAR argument JT is */
/*          communicated here as JTYP, but is not used in DSTODA */
/*          except to load MITER following a method switch.) */
/*          MITER may be reset by DSTODA. */
/* N      = the number of first-order differential equations. */
/* ----------------------------------------------------------------------- */
//====================================================================================



int dstoda_(integer *neq, doublereal *y, doublereal *yh, 
	integer *nyh, doublereal *yh1, doublereal *ewt, doublereal *savf, 
	doublereal *acor, doublereal *wm, integer *iwm
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    , CDsModBase *pclass
#else
    , FcnEqDiff state
    , FcnEqJac  jacobi
#endif    
    , S_fp pjac, S_fp slvs
    ,Matrix fjacmat
    )
{
    okay_t ret_ok;
    /* Initialized data */

    static doublereal sm1[12] = { .5,.575,.55,.45,.35,.25,.2,.15,.1,.075,.05,
	    .025 };

    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    static integer i__, j, m;
    static doublereal r__;
    static integer i1, jb;
    static doublereal rh, rm, dm1, dm2;
    static integer lm1, lm2;
    static doublereal rh1, rh2, del, ddn;
    static integer ncf;
    static doublereal pdh, dsm, dup, exm1, exm2;
    static integer nqm1, nqm2;
    static doublereal dcon, delp;
    static integer lm1p1, lm2p1;
    static doublereal exdn, rhdn;
    static integer iret;
    static doublereal told, rhsm;
    static integer newq;
    static doublereal exsm, rhup, rate, exup, rh1it, alpha;
    static integer iredo;
    static doublereal pnorm;
    extern /* Subroutine */ int dcfode_(integer *, doublereal *, doublereal *)
	    ;
    extern doublereal dmnorm_(integer *, doublereal *, doublereal *);

    /* Parameter adjustments */
    --neq;
    --y;
    yh_dim1 = *nyh;
    yh_offset = 1 + yh_dim1;
    yh -= yh_offset;
    --yh1;
    --ewt;
    --savf;
    --acor;
    --wm;
    --iwm;

    /* Function Body */
    dls001_1.kflag = 0;
    told = dls001_1.tn;
    ncf = 0;
    dls001_1.ierpj = 0;
    dls001_1.iersl = 0;
    dls001_1.jcur = 0;
    dls001_1.icf = 0;
    delp = 0.;
    if (dls001_1.jstart > 0) {
	goto L200;
    }
    if (dls001_1.jstart == -1) {
	goto L100;
    }
    if (dls001_1.jstart == -2) {
	goto L160;
    }
/* ----------------------------------------------------------------------- */
/* On the first call, the order is set to 1, and other variables are */
/* initialized.  RMAX is the maximum ratio by which H can be increased */
/* in a single step.  It is initially 1.E4 to compensate for the small */
/* initial H, but then is normally equal to 10.  If a failure */
/* occurs (in corrector convergence or error test), RMAX is set at 2 */
/* for the next increase. */
/* DCFODE is called to get the needed coefficients for both methods. */
/* ----------------------------------------------------------------------- */
    dls001_1.lmax = dls001_1.maxord + 1;
    dls001_1.nq = 1;
    dls001_1.l = 2;
    dls001_1.ialth = 2;
    dls001_1.rmax = 1e4;
    dls001_1.rc = 0.;
    dls001_1.el0 = 1.;
    dls001_1.crate = .7;
    dls001_1.hold = dls001_1.h__;
    dls001_1.nslp = 0;
    dls001_1.ipup = dls001_1.miter;
    iret = 3;
/* Initialize switching parameters.  METH = 1 is assumed initially. ----- */
    dlsa01_1.icount = 20;
    dlsa01_1.irflag = 0;
    dlsa01_1.pdest = 0.;
    dlsa01_1.pdlast = 0.;
    dlsa01_1.ratio = 5.;
    dcfode_(&c__2, dls001_1.elco, dls001_1.tesco);
    for (i__ = 1; i__ <= 5; ++i__) {
/* L10: */
	dlsa01_1.cm2[i__ - 1] = dls001_1.tesco[i__ * 3 - 2] * dls001_1.elco[
		i__ + 1 + i__ * 13 - 14];
    }
    dcfode_(&c__1, dls001_1.elco, dls001_1.tesco);
    for (i__ = 1; i__ <= 12; ++i__) {
/* L20: */
	dlsa01_1.cm1[i__ - 1] = dls001_1.tesco[i__ * 3 - 2] * dls001_1.elco[
		i__ + 1 + i__ * 13 - 14];
    }
    goto L150;
/* ----------------------------------------------------------------------- */
/* The following block handles preliminaries needed when JSTART = -1. */
/* IPUP is set to MITER to force a matrix update. */
/* If an order increase is about to be considered (IALTH = 1), */
/* IALTH is reset to 2 to postpone consideration one more step. */
/* If the caller has changed METH, DCFODE is called to reset */
/* the coefficients of the method. */
/* If H is to be changed, YH must be rescaled. */
/* If H or METH is being changed, IALTH is reset to L = NQ + 1 */
/* to prevent further changes in H for that many steps. */
/* ----------------------------------------------------------------------- */
L100:
    dls001_1.ipup = dls001_1.miter;
    dls001_1.lmax = dls001_1.maxord + 1;
    if (dls001_1.ialth == 1) {
	dls001_1.ialth = 2;
    }
    if (dls001_1.meth == dlsa01_1.mused) {
	goto L160;
    }
    dcfode_(&dls001_1.meth, dls001_1.elco, dls001_1.tesco);
    dls001_1.ialth = dls001_1.l;
    iret = 1;
/* ----------------------------------------------------------------------- */
/* The el vector and related constants are reset */
/* whenever the order NQ is changed, or at the start of the problem. */
/* ----------------------------------------------------------------------- */
L150:
    i__1 = dls001_1.l;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L155: */
	dls001_1.el[i__ - 1] = dls001_1.elco[i__ + dls001_1.nq * 13 - 14];
    }
    dls001_1.nqnyh = dls001_1.nq * *nyh;
    dls001_1.rc = dls001_1.rc * dls001_1.el[0] / dls001_1.el0;
    dls001_1.el0 = dls001_1.el[0];
    dls001_1.conit = .5 / (dls001_1.nq + 2);
    switch (iret) {
	case 1:  goto L160;
	case 2:  goto L170;
	case 3:  goto L200;
    }
/* ----------------------------------------------------------------------- */
/* If H is being changed, the H ratio RH is checked against */
/* RMAX, HMIN, and HMXI, and the YH array rescaled.  IALTH is set to */
/* L = NQ + 1 to prevent a change of H for that many steps, unless */
/* forced by a convergence or error test failure. */
/* ----------------------------------------------------------------------- */
L160:
    if (dls001_1.h__ == dls001_1.hold) {
	goto L200;
    }
    rh = dls001_1.h__ / dls001_1.hold;
    dls001_1.h__ = dls001_1.hold;
    iredo = 3;
    goto L175;
L170:
/* Computing MAX */
    d__1 = rh, d__2 = dls001_1.hmin / abs(dls001_1.h__);
    rh = max(d__1,d__2);
L175:
    rh = min(rh,dls001_1.rmax);
/* Computing MAX */
    d__1 = 1., d__2 = abs(dls001_1.h__) * dls001_1.hmxi * rh;
    rh /= max(d__1,d__2);
/* ----------------------------------------------------------------------- */
/* If METH = 1, also restrict the new step size by the stability region. */
/* If this reduces H, set IRFLAG to 1 so that if there are roundoff */
/* problems later, we can assume that is the cause of the trouble. */
/* ----------------------------------------------------------------------- */
    if (dls001_1.meth == 2) {
	goto L178;
    }
    dlsa01_1.irflag = 0;
/* Computing MAX */
    d__1 = abs(dls001_1.h__) * dlsa01_1.pdlast;
    pdh = max(d__1,1e-6);
    if (rh * pdh * 1.00001 < sm1[dls001_1.nq - 1]) {
	goto L178;
    }
    rh = sm1[dls001_1.nq - 1] / pdh;
    dlsa01_1.irflag = 1;
L178:
    r__ = 1.;
    i__1 = dls001_1.l;
    for (j = 2; j <= i__1; ++j) {
	r__ *= rh;
	i__2 = dls001_1.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L180: */
	    yh[i__ + j * yh_dim1] *= r__;
	}
    }
    dls001_1.h__ *= rh;
    dls001_1.rc *= rh;
    dls001_1.ialth = dls001_1.l;
    if (iredo == 0) {
	goto L690;
    }
/* ----------------------------------------------------------------------- */
/* This section computes the predicted values by effectively */
/* multiplying the YH array by the Pascal triangle matrix. */
/* RC is the ratio of new to old values of the coefficient  H*EL(1). */
/* When RC differs from 1 by more than CCMAX, IPUP is set to MITER */
/* to force PJAC to be called, if a Jacobian is involved. */
/* In any case, PJAC is called at least every MSBP steps. */
/* ----------------------------------------------------------------------- */
L200:
    if ((d__1 = dls001_1.rc - 1., abs(d__1)) > dls001_1.ccmax) {
	dls001_1.ipup = dls001_1.miter;
    }
    if (dls001_1.nst >= dls001_1.nslp + dls001_1.msbp) {
	dls001_1.ipup = dls001_1.miter;
    }
    dls001_1.tn += dls001_1.h__;
    i1 = dls001_1.nqnyh + 1;
    i__2 = dls001_1.nq;
    for (jb = 1; jb <= i__2; ++jb) {
	i1 -= *nyh;
/* DIR$ IVDEP */
	i__1 = dls001_1.nqnyh;
	for (i__ = i1; i__ <= i__1; ++i__) {
/* L210: */
	    yh1[i__] += yh1[i__ + *nyh];
	}
/* L215: */
    }
    pnorm = dmnorm_(&dls001_1.n, &yh1[1], &ewt[1]);
/* ----------------------------------------------------------------------- */
/* Up to MAXCOR corrector iterations are taken.  A convergence test is */
/* made on the RMS-norm of each correction, weighted by the error */
/* weight vector EWT.  The sum of the corrections is accumulated in the */
/* vector ACOR(i).  The YH array is not altered in the corrector loop. */
/* ----------------------------------------------------------------------- */
L220:
    m = 0;
    rate = 0.;
    del = 0.;
    i__2 = dls001_1.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L230: */
	y[i__] = yh[i__ + yh_dim1];
    }
    //(*f)(&neq[1], &dls001_1.tn, &y[1], &savf[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_1.tn, &y[1], &savf[1]);
#else
    ret_ok = state(neq[1], dls001_1.tn, &y[1], &savf[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error lsoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error lsoda_core: error during state-functioncall\n");
                     
#endif
        return 0;
    }
    ++dls001_1.nfe;
    if (dls001_1.ipup <= 0) {
	goto L250;
    }
/* ----------------------------------------------------------------------- */
/* If indicated, the matrix P = I - H*EL(1)*J is reevaluated and */
/* preprocessed before starting the corrector iteration.  IPUP is set */
/* to 0 as an indicator that this has been done. */
/* ----------------------------------------------------------------------- */
    (*pjac)(&neq[1], &y[1], &yh[yh_offset], nyh, &ewt[1], &acor[1], &savf[1], 
	    &wm[1], &iwm[1]        
        //, (S_fp)f, (U_fp)jac
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        , pclass
#else
        , state
        , jacobi
#endif
        , fjacmat
        );
    // Fehler
    if( dls001_1.kflag < 0 )
        return 0;

    dls001_1.ipup = 0;
    dls001_1.rc = 1.;
    dls001_1.nslp = dls001_1.nst;
    dls001_1.crate = .7;
    if (dls001_1.ierpj != 0) {
	goto L430;
    }
L250:
    i__2 = dls001_1.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L260: */
	acor[i__] = 0.;
    }
L270:
    if (dls001_1.miter != 0) {
	goto L350;
    }
/* ----------------------------------------------------------------------- */
/* In the case of functional iteration, update Y directly from */
/* the result of the last function evaluation. */
/* ----------------------------------------------------------------------- */
    i__2 = dls001_1.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	savf[i__] = dls001_1.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)];
/* L290: */
	y[i__] = savf[i__] - acor[i__];
    }
    del = dmnorm_(&dls001_1.n, &y[1], &ewt[1]);
    i__2 = dls001_1.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	y[i__] = yh[i__ + yh_dim1] + dls001_1.el[0] * savf[i__];
/* L300: */
	acor[i__] = savf[i__];
    }
    goto L400;
/* ----------------------------------------------------------------------- */
/* In the case of the chord method, compute the corrector error, */
/* and solve the linear system with that as right-hand side and */
/* P as coefficient matrix. */
/* ----------------------------------------------------------------------- */
L350:
    i__2 = dls001_1.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L360: */
	y[i__] = dls001_1.h__ * savf[i__] - (yh[i__ + (yh_dim1 << 1)] + acor[
		i__]);
    }
    (*slvs)(&wm[1], &iwm[1], &y[1], &savf[1]);
    if (dls001_1.iersl < 0) {
	goto L430;
    }
    if (dls001_1.iersl > 0) {
	goto L410;
    }
    del = dmnorm_(&dls001_1.n, &y[1], &ewt[1]);
    i__2 = dls001_1.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	acor[i__] += y[i__];
/* L380: */
	y[i__] = yh[i__ + yh_dim1] + dls001_1.el[0] * acor[i__];
    }
/* ----------------------------------------------------------------------- */
/* Test for convergence.  If M .gt. 0, an estimate of the convergence */
/* rate constant is stored in CRATE, and this is used in the test. */

/* We first check for a change of iterates that is the size of */
/* roundoff error.  If this occurs, the iteration has converged, and a */
/* new rate estimate is not formed. */
/* In all other cases, force at least two iterations to estimate a */
/* local Lipschitz constant estimate for Adams methods. */
/* On convergence, form PDEST = local maximum Lipschitz constant */
/* estimate.  PDLAST is the most recent nonzero estimate. */
/* ----------------------------------------------------------------------- */
L400:
    if (del <= pnorm * 100. * dls001_1.uround) {
	goto L450;
    }
    if (m == 0 && dls001_1.meth == 1) {
	goto L405;
    }
    if (m == 0) {
	goto L402;
    }
    rm = 1024.;
    if (del <= delp * 1024.) {
	rm = del / delp;
    }
    rate = max(rate,rm);
/* Computing MAX */
    d__1 = dls001_1.crate * .2;
    dls001_1.crate = max(d__1,rm);
L402:
/* Computing MIN */
    d__1 = 1., d__2 = dls001_1.crate * 1.5;
    dcon = del * min(d__1,d__2) / (dls001_1.tesco[dls001_1.nq * 3 - 2] * 
	    dls001_1.conit);
    if (dcon > 1.) {
	goto L405;
    }
/* Computing MAX */
    d__2 = dlsa01_1.pdest, d__3 = rate / (d__1 = dls001_1.h__ * dls001_1.el[0]
	    , abs(d__1));
    dlsa01_1.pdest = max(d__2,d__3);
    if (dlsa01_1.pdest != 0.) {
	dlsa01_1.pdlast = dlsa01_1.pdest;
    }
    goto L450;
L405:
    ++m;
    if (m == dls001_1.maxcor) {
	goto L410;
    }
    if (m >= 2 && del > delp * 2.) {
	goto L410;
    }
    delp = del;
    //(*f)(&neq[1], &dls001_1.tn, &y[1], &savf[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_1.tn, &y[1], &savf[1]);
#else
    ret_ok = state(neq[1], dls001_1.tn, &y[1], &savf[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error lsoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error lsoda_core: error during state-functioncall\n");
                     
#endif
        return 0;
    }
    ++dls001_1.nfe;
    goto L270;
/* ----------------------------------------------------------------------- */
/* The corrector iteration failed to converge. */
/* If MITER .ne. 0 and the Jacobian is out of date, PJAC is called for */
/* the next try.  Otherwise the YH array is retracted to its values */
/* before prediction, and H is reduced, if possible.  If H cannot be */
/* reduced or MXNCF failures have occurred, exit with KFLAG = -2. */
/* ----------------------------------------------------------------------- */
L410:
    if (dls001_1.miter == 0 || dls001_1.jcur == 1) {
	goto L430;
    }
    dls001_1.icf = 1;
    dls001_1.ipup = dls001_1.miter;
    goto L220;
L430:
    dls001_1.icf = 2;
    ++ncf;
    dls001_1.rmax = 2.;
    dls001_1.tn = told;
    i1 = dls001_1.nqnyh + 1;
    i__2 = dls001_1.nq;
    for (jb = 1; jb <= i__2; ++jb) {
	i1 -= *nyh;
/* DIR$ IVDEP */
	i__1 = dls001_1.nqnyh;
	for (i__ = i1; i__ <= i__1; ++i__) {
/* L440: */
	    yh1[i__] -= yh1[i__ + *nyh];
	}
/* L445: */
    }
    if (dls001_1.ierpj < 0 || dls001_1.iersl < 0) {
	goto L680;
    }
    if (abs(dls001_1.h__) <= dls001_1.hmin * 1.00001) {
	goto L670;
    }
    if (ncf == dls001_1.mxncf) {
	goto L670;
    }
    rh = .25;
    dls001_1.ipup = dls001_1.miter;
    iredo = 1;
    goto L170;
/* ----------------------------------------------------------------------- */
/* The corrector has converged.  JCUR is set to 0 */
/* to signal that the Jacobian involved may need updating later. */
/* The local error test is made and control passes to statement 500 */
/* if it fails. */
/* ----------------------------------------------------------------------- */
L450:
    dls001_1.jcur = 0;
    if (m == 0) {
	dsm = del / dls001_1.tesco[dls001_1.nq * 3 - 2];
    }
    if (m > 0) {
	dsm = dmnorm_(&dls001_1.n, &acor[1], &ewt[1]) / dls001_1.tesco[
		dls001_1.nq * 3 - 2];
    }
    if (dsm > 1.) {
	goto L500;
    }
/* ----------------------------------------------------------------------- */
/* After a successful step, update the YH array. */
/* Decrease ICOUNT by 1, and if it is -1, consider switching methods. */
/* If a method switch is made, reset various parameters, */
/* rescale the YH array, and exit.  If there is no switch, */
/* consider changing H if IALTH = 1.  Otherwise decrease IALTH by 1. */
/* If IALTH is then 1 and NQ .lt. MAXORD, then ACOR is saved for */
/* use in a possible order increase on the next step. */
/* If a change in H is considered, an increase or decrease in order */
/* by one is considered also.  A change in H is made only if it is by a */
/* factor of at least 1.1.  If not, IALTH is set to 3 to prevent */
/* testing for that many steps. */
/* ----------------------------------------------------------------------- */
    dls001_1.kflag = 0;
    iredo = 0;
    ++dls001_1.nst;
    dls001_1.hu = dls001_1.h__;
    dls001_1.nqu = dls001_1.nq;
    dlsa01_1.mused = dls001_1.meth;
    i__2 = dls001_1.l;
    for (j = 1; j <= i__2; ++j) {
	i__1 = dls001_1.n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L460: */
	    yh[i__ + j * yh_dim1] += dls001_1.el[j - 1] * acor[i__];
	}
    }
    --dlsa01_1.icount;
    if (dlsa01_1.icount >= 0) {
	goto L488;
    }
    if (dls001_1.meth == 2) {
	goto L480;
    }
/* ----------------------------------------------------------------------- */
/* We are currently using an Adams method.  Consider switching to BDF. */
/* If the current order is greater than 5, assume the problem is */
/* not stiff, and skip this section. */
/* If the Lipschitz constant and error estimate are not polluted */
/* by roundoff, go to 470 and perform the usual test. */
/* Otherwise, switch to the BDF methods if the last step was */
/* restricted to insure stability (irflag = 1), and stay with Adams */
/* method if not.  When switching to BDF with polluted error estimates, */
/* in the absence of other information, double the step size. */

/* When the estimates are OK, we make the usual test by computing */
/* the step size we could have (ideally) used on this step, */
/* with the current (Adams) method, and also that for the BDF. */
/* If NQ .gt. MXORDS, we consider changing to order MXORDS on switching. */
/* Compare the two step sizes to decide whether to switch. */
/* The step size advantage must be at least RATIO = 5 to switch. */
/* ----------------------------------------------------------------------- */
    if (dls001_1.nq > 5) {
	goto L488;
    }
    if (dsm > pnorm * 100. * dls001_1.uround && dlsa01_1.pdest != 0.) {
	goto L470;
    }
    if (dlsa01_1.irflag == 0) {
	goto L488;
    }
    rh2 = 2.;
    nqm2 = min(dls001_1.nq,dlsa01_1.mxords);
    goto L478;
L470:
    exsm = 1. / dls001_1.l;
    rh1 = 1. / (pow(dsm, exsm) * 1.2 + 1.2e-6);
    rh1it = rh1 * 2.;
    pdh = dlsa01_1.pdlast * abs(dls001_1.h__);
    if (pdh * rh1 > 1e-5) {
	rh1it = sm1[dls001_1.nq - 1] / pdh;
    }
    rh1 = min(rh1,rh1it);
    if (dls001_1.nq <= dlsa01_1.mxords) {
	goto L474;
    }
    nqm2 = dlsa01_1.mxords;
    lm2 = dlsa01_1.mxords + 1;
    exm2 = 1. / lm2;
    lm2p1 = lm2 + 1;
    dm2 = dmnorm_(&dls001_1.n, &yh[lm2p1 * yh_dim1 + 1], &ewt[1]) / 
	    dlsa01_1.cm2[dlsa01_1.mxords - 1];
    rh2 = 1. / (pow(dm2, exm2) * 1.2 + 1.2e-6);
    goto L476;
L474:
    dm2 = dsm * (dlsa01_1.cm1[dls001_1.nq - 1] / dlsa01_1.cm2[dls001_1.nq - 1]
	    );
    rh2 = 1. / (pow(dm2, exsm) * 1.2 + 1.2e-6);
    nqm2 = dls001_1.nq;
L476:
    if (rh2 < dlsa01_1.ratio * rh1) {
	goto L488;
    }
/* THE SWITCH TEST PASSED.  RESET RELEVANT QUANTITIES FOR BDF. ---------- */
L478:
    rh = rh2;
    dlsa01_1.icount = 20;
    dls001_1.meth = 2;
    dls001_1.miter = dlsa01_1.jtyp;
    dlsa01_1.pdlast = 0.;
    dls001_1.nq = nqm2;
    dls001_1.l = dls001_1.nq + 1;
    goto L170;
/* ----------------------------------------------------------------------- */
/* We are currently using a BDF method.  Consider switching to Adams. */
/* Compute the step size we could have (ideally) used on this step, */
/* with the current (BDF) method, and also that for the Adams. */
/* If NQ .gt. MXORDN, we consider changing to order MXORDN on switching. */
/* Compare the two step sizes to decide whether to switch. */
/* The step size advantage must be at least 5/RATIO = 1 to switch. */
/* If the step size for Adams would be so small as to cause */
/* roundoff pollution, we stay with BDF. */
/* ----------------------------------------------------------------------- */
L480:
    exsm = 1. / dls001_1.l;
    if (dlsa01_1.mxordn >= dls001_1.nq) {
	goto L484;
    }
    nqm1 = dlsa01_1.mxordn;
    lm1 = dlsa01_1.mxordn + 1;
    exm1 = 1. / lm1;
    lm1p1 = lm1 + 1;
    dm1 = dmnorm_(&dls001_1.n, &yh[lm1p1 * yh_dim1 + 1], &ewt[1]) / 
	    dlsa01_1.cm1[dlsa01_1.mxordn - 1];
    rh1 = 1. / (pow(dm1, exm1) * 1.2 + 1.2e-6);
    goto L486;
L484:
    dm1 = dsm * (dlsa01_1.cm2[dls001_1.nq - 1] / dlsa01_1.cm1[dls001_1.nq - 1]
	    );
    rh1 = 1. / (pow(dm1, exsm) * 1.2 + 1.2e-6);
    nqm1 = dls001_1.nq;
    exm1 = exsm;
L486:
    rh1it = rh1 * 2.;
    pdh = dlsa01_1.pdnorm * abs(dls001_1.h__);
    if (pdh * rh1 > 1e-5) {
	rh1it = sm1[nqm1 - 1] / pdh;
    }
    rh1 = min(rh1,rh1it);
    rh2 = 1. / (pow(dsm, exsm) * 1.2 + 1.2e-6);
    if (rh1 * dlsa01_1.ratio < rh2 * 5.) {
	goto L488;
    }
    alpha = max(.001,rh1);
    dm1 = pow(alpha, exm1) * dm1;
    if (dm1 <= dls001_1.uround * 1e3 * pnorm) {
	goto L488;
    }
/* The switch test passed.  Reset relevant quantities for Adams. -------- */
    rh = rh1;
    dlsa01_1.icount = 20;
    dls001_1.meth = 1;
    dls001_1.miter = 0;
    dlsa01_1.pdlast = 0.;
    dls001_1.nq = nqm1;
    dls001_1.l = dls001_1.nq + 1;
    goto L170;

/* No method switch is being made.  Do the usual step/order selection. -- */
L488:
    --dls001_1.ialth;
    if (dls001_1.ialth == 0) {
	goto L520;
    }
    if (dls001_1.ialth > 1) {
	goto L700;
    }
    if (dls001_1.l == dls001_1.lmax) {
	goto L700;
    }
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L490: */
	yh[i__ + dls001_1.lmax * yh_dim1] = acor[i__];
    }
    goto L700;
/* ----------------------------------------------------------------------- */
/* The error test failed.  KFLAG keeps track of multiple failures. */
/* Restore TN and the YH array to their previous values, and prepare */
/* to try the step again.  Compute the optimum step size for this or */
/* one lower order.  After 2 or more failures, H is forced to decrease */
/* by a factor of 0.2 or less. */
/* ----------------------------------------------------------------------- */
L500:
    --dls001_1.kflag;
    dls001_1.tn = told;
    i1 = dls001_1.nqnyh + 1;
    i__1 = dls001_1.nq;
    for (jb = 1; jb <= i__1; ++jb) {
	i1 -= *nyh;
/* DIR$ IVDEP */
	i__2 = dls001_1.nqnyh;
	for (i__ = i1; i__ <= i__2; ++i__) {
/* L510: */
	    yh1[i__] -= yh1[i__ + *nyh];
	}
/* L515: */
    }
    dls001_1.rmax = 2.;
    if (abs(dls001_1.h__) <= dls001_1.hmin * 1.00001) {
	goto L660;
    }
    if (dls001_1.kflag <= -3) {
	goto L640;
    }
    iredo = 2;
    rhup = 0.;
    goto L540;
/* ----------------------------------------------------------------------- */
/* Regardless of the success or failure of the step, factors */
/* RHDN, RHSM, and RHUP are computed, by which H could be multiplied */
/* at order NQ - 1, order NQ, or order NQ + 1, respectively. */
/* In the case of failure, RHUP = 0.0 to avoid an order increase. */
/* The largest of these is determined and the new order chosen */
/* accordingly.  If the order is to be increased, we compute one */
/* additional scaled derivative. */
/* ----------------------------------------------------------------------- */
L520:
    rhup = 0.;
    if (dls001_1.l == dls001_1.lmax) {
	goto L540;
    }
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L530: */
	savf[i__] = acor[i__] - yh[i__ + dls001_1.lmax * yh_dim1];
    }
    dup = dmnorm_(&dls001_1.n, &savf[1], &ewt[1]) / dls001_1.tesco[
	    dls001_1.nq * 3 - 1];
    exup = 1. / (dls001_1.l + 1);
    rhup = 1. / (pow(dup, exup) * 1.4 + 1.4e-6);
L540:
    exsm = 1. / dls001_1.l;
    rhsm = 1. / (pow(dsm, exsm) * 1.2 + 1.2e-6);
    rhdn = 0.;
    if (dls001_1.nq == 1) {
	goto L550;
    }
    ddn = dmnorm_(&dls001_1.n, &yh[dls001_1.l * yh_dim1 + 1], &ewt[1]) / 
	    dls001_1.tesco[dls001_1.nq * 3 - 3];
    exdn = 1. / dls001_1.nq;
    rhdn = 1. / (pow(ddn, exdn) * 1.3 + 1.3e-6);
/* If METH = 1, limit RH according to the stability region also. -------- */
L550:
    if (dls001_1.meth == 2) {
	goto L560;
    }
/* Computing MAX */
    d__1 = abs(dls001_1.h__) * dlsa01_1.pdlast;
    pdh = max(d__1,1e-6);
    if (dls001_1.l < dls001_1.lmax) {
/* Computing MIN */
	d__1 = rhup, d__2 = sm1[dls001_1.l - 1] / pdh;
	rhup = min(d__1,d__2);
    }
/* Computing MIN */
    d__1 = rhsm, d__2 = sm1[dls001_1.nq - 1] / pdh;
    rhsm = min(d__1,d__2);
    if (dls001_1.nq > 1) {
/* Computing MIN */
	d__1 = rhdn, d__2 = sm1[dls001_1.nq - 2] / pdh;
	rhdn = min(d__1,d__2);
    }
    dlsa01_1.pdest = 0.;
L560:
    if (rhsm >= rhup) {
	goto L570;
    }
    if (rhup > rhdn) {
	goto L590;
    }
    goto L580;
L570:
    if (rhsm < rhdn) {
	goto L580;
    }
    newq = dls001_1.nq;
    rh = rhsm;
    goto L620;
L580:
    newq = dls001_1.nq - 1;
    rh = rhdn;
    if (dls001_1.kflag < 0 && rh > 1.) {
	rh = 1.;
    }
    goto L620;
L590:
    newq = dls001_1.l;
    rh = rhup;
    if (rh < 1.1) {
	goto L610;
    }
    r__ = dls001_1.el[dls001_1.l - 1] / dls001_1.l;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L600: */
	yh[i__ + (newq + 1) * yh_dim1] = acor[i__] * r__;
    }
    goto L630;
L610:
    dls001_1.ialth = 3;
    goto L700;
/* If METH = 1 and H is restricted by stability, bypass 10 percent test. */
L620:
    if (dls001_1.meth == 2) {
	goto L622;
    }
    if (rh * pdh * 1.00001 >= sm1[newq - 1]) {
	goto L625;
    }
L622:
    if (dls001_1.kflag == 0 && rh < 1.1) {
	goto L610;
    }
L625:
    if (dls001_1.kflag <= -2) {
	rh = min(rh,.2);
    }
/* ----------------------------------------------------------------------- */
/* If there is a change of order, reset NQ, L, and the coefficients. */
/* In any case H is reset according to RH and the YH array is rescaled. */
/* Then exit from 690 if the step was OK, or redo the step otherwise. */
/* ----------------------------------------------------------------------- */
    if (newq == dls001_1.nq) {
	goto L170;
    }
L630:
    dls001_1.nq = newq;
    dls001_1.l = dls001_1.nq + 1;
    iret = 2;
    goto L150;
/* ----------------------------------------------------------------------- */
/* Control reaches this section if 3 or more failures have occured. */
/* If 10 failures have occurred, exit with KFLAG = -1. */
/* It is assumed that the derivatives that have accumulated in the */
/* YH array have errors of the wrong order.  Hence the first */
/* derivative is recomputed, and the order is set to 1.  Then */
/* H is reduced by a factor of 10, and the step is retried, */
/* until it succeeds or H reaches HMIN. */
/* ----------------------------------------------------------------------- */
L640:
    if (dls001_1.kflag == -10) {
	goto L660;
    }
    rh = .1;
/* Computing MAX */
    d__1 = dls001_1.hmin / abs(dls001_1.h__);
    rh = max(d__1,rh);
    dls001_1.h__ *= rh;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L645: */
	y[i__] = yh[i__ + yh_dim1];
    }
    //(*f)(&neq[1], &dls001_1.tn, &y[1], &savf[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_1.tn, &y[1], &savf[1]);
#else
    ret_ok = state(neq[1], dls001_1.tn, &y[1], &savf[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error lsoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error lsoda_core: error during state-functioncall\n");
                     
#endif
        return 0;
    }
    ++dls001_1.nfe;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L650: */
	yh[i__ + (yh_dim1 << 1)] = dls001_1.h__ * savf[i__];
    }
    dls001_1.ipup = dls001_1.miter;
    dls001_1.ialth = 5;
    if (dls001_1.nq == 1) {
	goto L200;
    }
    dls001_1.nq = 1;
    dls001_1.l = 2;
    iret = 3;
    goto L150;
/* ----------------------------------------------------------------------- */
/* All returns are made through this section.  H is saved in HOLD */
/* to allow the caller to change H on the next step. */
/* ----------------------------------------------------------------------- */
L660:
    dls001_1.kflag = -1;
    goto L720;
L670:
    dls001_1.kflag = -2;
    goto L720;
L680:
    dls001_1.kflag = -3;
    goto L720;
L690:
    dls001_1.rmax = 10.;
L700:
    r__ = 1. / dls001_1.tesco[dls001_1.nqu * 3 - 2];
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L710: */
	acor[i__] *= r__;
    }
L720:
    dls001_1.hold = dls001_1.h__;
    dls001_1.jstart = 1;
    return 0;
/* ----------------------- End of Subroutine DSTODA ---------------------- */
} /* dstoda_ */
//============================================================================
//
// DPRJA
//
/* ----------------------------------------------------------------------- */
/* DPRJA is called by DSTODA to compute and process the matrix */
/* P = I - H*EL(1)*J , where J is an approximation to the Jacobian. */
/* Here J is computed by the user-supplied routine JAC if */
/* MITER = 1 or 4 or by finite differencing if MITER = 2 or 5. */
/* J, scaled by -H*EL(1), is stored in WM.  Then the norm of J (the */
/* matrix norm consistent with the weighted max-norm on vectors given */
/* by DMNORM) is computed, and J is overwritten by P.  P is then */
/* subjected to LU decomposition in preparation for later solution */
/* of linear systems with P as coefficient matrix.  This is done */
/* by DGEFA if MITER = 1 or 2, and by DGBFA if MITER = 4 or 5. */

/* In addition to variables described previously, communication */
/* with DPRJA uses the following: */
/* Y     = array containing predicted values on entry. */
/* FTEM  = work array of length N (ACOR in DSTODA). */
/* SAVF  = array containing f evaluated at predicted y. */
/* WM    = real work space for matrices.  On output it contains the */
/*         LU decomposition of P. */
/*         Storage of matrix elements starts at WM(3). */
/*         WM also contains the following matrix-related data: */
/*         WM(1) = SQRT(UROUND), used in numerical Jacobian increments. */
/* IWM   = integer work space containing pivot information, starting at */
/*         IWM(21).   IWM also contains the band parameters */
/*         ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5. */
/* EL0   = EL(1) (input). */
/* PDNORM= norm of Jacobian matrix. (Output). */
/* IERPJ = output error flag,  = 0 if no trouble, .gt. 0 if */
/*         P matrix found to be singular. */
/* JCUR  = output flag = 1 to indicate that the Jacobian matrix */
/*         (or approximation) is now current. */
/* This routine also uses the Common variables EL0, H, TN, UROUND, */
/* MITER, N, NFE, and NJE. */
/* ----------------------------------------------------------------------- */
//=============================================================================
/* Subroutine */ int dprja_(integer *neq, doublereal *y, doublereal *yh, 
	integer *nyh, doublereal *ewt, doublereal *ftem, doublereal *savf, 
	doublereal *wm, integer *iwm
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    , CDsModBase *pclass
#else
    , FcnEqDiff state
    , FcnEqJac  jacobi
#endif
    ,Matrix fjacmat
    )
{
    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;

    okay_t ret_ok=OKAY;

    /* Local variables */
    static integer i__, j;
    static doublereal r__;
    static integer i1, i2, j1;
    static doublereal r0;
    static integer ii, jj, ml, mu;
    static doublereal yi, yj, hl0;
    static integer ml3, np1;
    static doublereal fac;
    static integer mba, ier;
    static doublereal con, yjj;
    static integer meb1, lenp;
    static doublereal srur;
    extern /* Subroutine */ int dgbfa_(doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), dgefa_(doublereal *, 
	    integer *, integer *, integer *, integer *);
    static integer mband, meband;
    extern doublereal dbnorm_(integer *, doublereal *, integer *, integer *, 
	    integer *, doublereal *), dfnorm_(integer *, doublereal *, 
	    doublereal *), dmnorm_(integer *, doublereal *, doublereal *);

    /* Parameter adjustments */
    --neq;
    --y;
    yh_dim1 = *nyh;
    yh_offset = 1 + yh_dim1;
    yh -= yh_offset;
    --ewt;
    --ftem;
    --savf;
    --wm;
    --iwm;

    /* Function Body */
    ++dls001_1.nje;
    dls001_1.ierpj = 0;
    dls001_1.jcur = 1;
    hl0 = dls001_1.h__ * dls001_1.el0;
    switch (dls001_1.miter) {
	case 1:  goto L100;
	case 2:  goto L200;
	case 3:  goto L300;
	case 4:  goto L400;
	case 5:  goto L500;
    }
/* If MITER = 1, call JAC and multiply by scalar. ----------------------- */
L100:
    lenp = dls001_1.n * dls001_1.n;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L110: */
	wm[i__ + 2] = 0.;
    }
    //(*jac)(&neq[1], &dls001_1.tn, &y[1], &c__0, &c__0, &wm[3], &dls001_1.n);
    // Jacobi-Funktion
    //================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->jacobi(dls001_1.tn, &y[1], fjacmat, (uint32_t)dls001_1.n);
#else
    ret_ok = jacobi(neq[1], dls001_1.tn, &y[1], fjacmat, (uint32_t)dls001_1.n);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -9;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error dprja_: during jacobi-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error dprja_: error during jacobi-functioncall\n");
                 
#endif
        return 0;
    } else {

        i__1 = dls001_1.n;
        for (j = 1; j <= i__1; ++j) {
	        i__2 = neq[1];
	        for (i__ = 1; i__ <= i__2; ++i__) {
	            wm[i__ + (j-1) * dls001_1.n + 2] = fjacmat[i__-1][j-1];
                //printf("Slf:%g|wm[%i]=%g\n",dls001_1.tn,i__ + (j-1) * dls001_1.n + 2,wm[i__ + (j-1) * dls001_1.n + 2]);
            }
        }
    }

    con = -hl0;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L120: */
	wm[i__ + 2] *= con;
    }
    goto L240;
/* If MITER = 2, make N calls to F to approximate J. -------------------- */
L200:
    fac = dmnorm_(&dls001_1.n, &savf[1], &ewt[1]);
    r0 = abs(dls001_1.h__) * 1e3 * dls001_1.uround * dls001_1.n * fac;
    if (r0 == 0.) {
	r0 = 1.;
    }
    srur = wm[1];
    j1 = 2;
    i__1 = dls001_1.n;
    for (j = 1; j <= i__1; ++j) {
	yj = y[j];
/* Computing MAX */
	d__1 = srur * abs(yj), d__2 = r0 / ewt[j];
	r__ = max(d__1,d__2);
	y[j] += r__;
	fac = -hl0 / r__;
	//(*f)(&neq[1], &dls001_1.tn, &y[1], &ftem[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_1.tn, &y[1], &ftem[1]);
#else
    ret_ok = state(neq[1], dls001_1.tn, &y[1], &ftem[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error dprja_: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error dprja_: error during state-functioncall\n");
                     
#endif
        return 0;
    }

	i__2 = dls001_1.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L220: */
	    wm[i__ + j1] = (ftem[i__] - savf[i__]) * fac;
	}
	y[j] = yj;
	j1 += dls001_1.n;
/* L230: */
    }
    dls001_1.nfe += dls001_1.n;
L240:
/* Compute norm of Jacobian. -------------------------------------------- */
    dlsa01_1.pdnorm = dfnorm_(&dls001_1.n, &wm[3], &ewt[1]) / abs(hl0);
/* Add identity matrix. ------------------------------------------------- */
    j = 3;
    np1 = dls001_1.n + 1;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wm[j] += 1.;
/* L250: */
	j += np1;
    }
/* Do LU decomposition on P. -------------------------------------------- */
    dgefa_(&wm[3], &dls001_1.n, &dls001_1.n, &iwm[21], &ier);
    if (ier != 0) {
	dls001_1.ierpj = 1;
    }
    return 0;
/* Dummy block only, since MITER is never 3 in this routine. ------------ */
L300:
    return 0;
/* If MITER = 4, call JAC and multiply by scalar. ----------------------- */
L400:
    ml = iwm[1];
    mu = iwm[2];
    ml3 = ml + 3;
    mband = ml + mu + 1;
    meband = mband + ml;
    lenp = meband * dls001_1.n;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L410: */
	wm[i__ + 2] = 0.;
    }
    //(*jac)(&neq[1], &dls001_1.tn, &y[1], &ml, &mu, &wm[ml3], &meband);
    // Jacobi-Funktion
    //================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->jacobi(dls001_1.tn, &y[1], fjacmat, (uint32_t)meband);
#else
    ret_ok = jacobi(neq[1], dls001_1.tn, &y[1], fjacmat, (uint32_t)meband);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -9;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error dprja_: during jacobi-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error dprja_: error during jacobi-functioncall\n");
                 
#endif
        return 0;
    } else {

        i__1 = meband;
        for (j = 1; j <= i__1; ++j) {
	        i__2 = neq[1];
	        for (i__ = 1; i__ <= i__2; ++i__) {
	            wm[i__ + (j-1) * meband + 2] = fjacmat[i__-1][j-1];
            }
        }
    }
    con = -hl0;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L420: */
	wm[i__ + 2] *= con;
    }
    goto L570;
/* If MITER = 5, make MBAND calls to F to approximate J. ---------------- */
L500:
    ml = iwm[1];
    mu = iwm[2];
    mband = ml + mu + 1;
    mba = min(mband,dls001_1.n);
    meband = mband + ml;
    meb1 = meband - 1;
    srur = wm[1];
    fac = dmnorm_(&dls001_1.n, &savf[1], &ewt[1]);
    r0 = abs(dls001_1.h__) * 1e3 * dls001_1.uround * dls001_1.n * fac;
    if (r0 == 0.) {
	r0 = 1.;
    }
    i__1 = mba;
    for (j = 1; j <= i__1; ++j) {
	i__2 = dls001_1.n;
	i__3 = mband;
	for (i__ = j; i__3 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__3) {
	    yi = y[i__];
/* Computing MAX */
	    d__1 = srur * abs(yi), d__2 = r0 / ewt[i__];
	    r__ = max(d__1,d__2);
/* L530: */
	    y[i__] += r__;
	}
	//(*f)(&neq[1], &dls001_1.tn, &y[1], &ftem[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_1.tn, &y[1], &ftem[1]);
#else
    ret_ok = state((uint32_t)neq[1], dls001_1.tn, &y[1], &ftem[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        pIntegratorLSODA_ErrText->catFormat("Error dprja_: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        sprintf(pIntegratorLSODA_errtext,"Error dprja_: error during state-functioncall\n");
                     
#endif
        return 0;
    }
	i__3 = dls001_1.n;
	i__2 = mband;
	for (jj = j; i__2 < 0 ? jj >= i__3 : jj <= i__3; jj += i__2) {
	    y[jj] = yh[jj + yh_dim1];
	    yjj = y[jj];
/* Computing MAX */
	    d__1 = srur * abs(yjj), d__2 = r0 / ewt[jj];
	    r__ = max(d__1,d__2);
	    fac = -hl0 / r__;
/* Computing MAX */
	    i__4 = jj - mu;
	    i1 = max(i__4,1);
/* Computing MIN */
	    i__4 = jj + ml;
	    i2 = min(i__4,dls001_1.n);
	    ii = jj * meb1 - ml + 2;
	    i__4 = i2;
	    for (i__ = i1; i__ <= i__4; ++i__) {
/* L540: */
		wm[ii + i__] = (ftem[i__] - savf[i__]) * fac;
	    }
/* L550: */
	}
/* L560: */
    }
    dls001_1.nfe += mba;
L570:
/* Compute norm of Jacobian. -------------------------------------------- */
    dlsa01_1.pdnorm = dbnorm_(&dls001_1.n, &wm[ml + 3], &meband, &ml, &mu, &
	    ewt[1]) / abs(hl0);
/* Add identity matrix. ------------------------------------------------- */
    ii = mband + 2;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wm[ii] += 1.;
/* L580: */
	ii += meband;
    }
/* Do LU decomposition of P. -------------------------------------------- */
    dgbfa_(&wm[3], &meband, &dls001_1.n, &ml, &mu, &iwm[21], &ier);
    if (ier != 0) {
	dls001_1.ierpj = 1;
    }
    return 0;
/* ----------------------- End of Subroutine DPRJA ----------------------- */
} /* dprja_ */

/* DECK DSOLSY */
/* Subroutine */ int dsolsy_(doublereal *wm, integer *iwm, doublereal *x, 
	doublereal *tem)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static doublereal r__, di;
    static integer ml, mu;
    static doublereal hl0, phl0;
    extern /* Subroutine */ int dgbsl_(doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, doublereal *, integer *), dgesl_(
	    doublereal *, integer *, integer *, integer *, doublereal *, 
	    integer *);
    static integer meband;

/* ***BEGIN PROLOGUE  DSOLSY */
/* ***SUBSIDIARY */
/* ***PURPOSE  ODEPACK linear system solver. */
/* ***TYPE      DOUBLE PRECISION (SSOLSY-S, DSOLSY-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  This routine manages the solution of the linear system arising from */
/*  a chord iteration.  It is called if MITER .ne. 0. */
/*  If MITER is 1 or 2, it calls DGESL to accomplish this. */
/*  If MITER = 3 it updates the coefficient h*EL0 in the diagonal */
/*  matrix, and then computes the solution. */
/*  If MITER is 4 or 5, it calls DGBSL. */
/*  Communication with DSOLSY uses the following variables: */
/*  WM    = real work space containing the inverse diagonal matrix if */
/*          MITER = 3 and the LU decomposition of the matrix otherwise. */
/*          Storage of matrix elements starts at WM(3). */
/*          WM also contains the following matrix-related data: */
/*          WM(1) = SQRT(UROUND) (not used here), */
/*          WM(2) = HL0, the previous value of h*EL0, used if MITER = 3. */
/*  IWM   = integer work space containing pivot information, starting at */
/*          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band */
/*          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5. */
/*  X     = the right-hand side vector on input, and the solution vector */
/*          on output, of length N. */
/*  TEM   = vector of work space of length N, not used in this version. */
/*  IERSL = output flag (in COMMON).  IERSL = 0 if no trouble occurred. */
/*          IERSL = 1 if a singular matrix arose with MITER = 3. */
/*  This routine also uses the COMMON variables EL0, H, MITER, and N. */

/* ***SEE ALSO  DLSODE */
/* ***ROUTINES CALLED  DGBSL, DGESL */
/* ***COMMON BLOCKS    DLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010418  Reduced size of Common block /DLS001/. (ACH) */
/*   031105  Restored 'own' variables to Common block /DLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/* ***END PROLOGUE  DSOLSY */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  DSOLSY */
    /* Parameter adjustments */
    --tem;
    --x;
    --iwm;
    --wm;

    /* Function Body */
    dls001_2.iersl = 0;
    switch (dls001_2.miter) {
	case 1:  goto L100;
	case 2:  goto L100;
	case 3:  goto L300;
	case 4:  goto L400;
	case 5:  goto L400;
    }
L100:
    dgesl_(&wm[3], &dls001_2.n, &dls001_2.n, &iwm[21], &x[1], &c__0);
    return 0;

L300:
    phl0 = wm[2];
    hl0 = dls001_2.h__ * dls001_2.el0;
    wm[2] = hl0;
    if (hl0 == phl0) {
	goto L330;
    }
    r__ = hl0 / phl0;
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	di = 1. - r__ * (1. - 1. / wm[i__ + 2]);
	if (abs(di) == 0.) {
	    goto L390;
	}
/* L320: */
	wm[i__ + 2] = 1. / di;
    }
L330:
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L340: */
	x[i__] = wm[i__ + 2] * x[i__];
    }
    return 0;
L390:
    dls001_2.iersl = 1;
    return 0;

L400:
    ml = iwm[1];
    mu = iwm[2];
    meband = (ml << 1) + mu + 1;
    dgbsl_(&wm[3], &meband, &dls001_2.n, &ml, &mu, &iwm[21], &x[1], &c__0);
    return 0;
/* ----------------------- END OF SUBROUTINE DSOLSY ---------------------- */
} /* dsolsy_ */


int dintdy_(doublereal *t, integer *k, doublereal *yh, 
	integer *nyh, doublereal *dky, integer *iflag)
{
    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2;

    /* Builtin functions */
    double pow_di(doublereal *, integer *);
    /* Subroutine */ void s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static doublereal c__;
    static integer i__, j;
    static doublereal r__, s;
    static integer ic, jb, jj;
    static doublereal tp;
    static integer jb2, jj1, jp1;
    static char msg[80];
    extern /* Subroutine */ int xerrwd_(char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, ftnlen);

/* ***BEGIN PROLOGUE  DINTDY */
/* ***SUBSIDIARY */
/* ***PURPOSE  Interpolate solution derivatives. */
/* ***TYPE      DOUBLE PRECISION (SINTDY-S, DINTDY-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  DINTDY computes interpolated values of the K-th derivative of the */
/*  dependent variable vector y, and stores it in DKY.  This routine */
/*  is called within the package with K = 0 and T = TOUT, but may */
/*  also be called by the user for any K up to the current order. */
/*  (See detailed instructions in the usage documentation.) */

/*  The computed values in DKY are gotten by interpolation using the */
/*  Nordsieck history array YH.  This array corresponds uniquely to a */
/*  vector-valued polynomial of degree NQCUR or less, and DKY is set */
/*  to the K-th derivative of this polynomial at T. */
/*  The formula for DKY is: */
/*               q */
/*   DKY(i)  =  sum  c(j,K) * (T - tn)**(j-K) * h**(-j) * YH(i,j+1) */
/*              j=K */
/*  where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, tn = TCUR, h = HCUR. */
/*  The quantities  nq = NQCUR, l = nq+1, N = NEQ, tn, and h are */
/*  communicated by COMMON.  The above sum is done in reverse order. */
/*  IFLAG is returned negative if either K or T is out of bounds. */

/* ***SEE ALSO  DLSODE */
/* ***ROUTINES CALLED  XERRWD */
/* ***COMMON BLOCKS    DLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010418  Reduced size of Common block /DLS001/. (ACH) */
/*   031105  Restored 'own' variables to Common block /DLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/* ***END PROLOGUE  DINTDY */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  DINTDY */
    /* Parameter adjustments */
    yh_dim1 = *nyh;
    yh_offset = 1 + yh_dim1;
    yh -= yh_offset;
    --dky;

    /* Function Body */
    *iflag = 0;
    if (*k < 0 || *k > dls001_2.nq) {
	goto L80;
    }
    tp = dls001_2.tn - dls001_2.hu - dls001_2.uround * 100. * (dls001_2.tn + 
	    dls001_2.hu);
    if ((*t - tp) * (*t - dls001_2.tn) > 0.) {
	goto L90;
    }

    s = (*t - dls001_2.tn) / dls001_2.h__;
    ic = 1;
    if (*k == 0) {
	goto L15;
    }
    jj1 = dls001_2.l - *k;
    i__1 = dls001_2.nq;
    for (jj = jj1; jj <= i__1; ++jj) {
/* L10: */
	ic *= jj;
    }
L15:
    c__ = (doublereal) ic;
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L20: */
	dky[i__] = c__ * yh[i__ + dls001_2.l * yh_dim1];
    }
    if (*k == dls001_2.nq) {
	goto L55;
    }
    jb2 = dls001_2.nq - *k;
    i__1 = jb2;
    for (jb = 1; jb <= i__1; ++jb) {
	j = dls001_2.nq - jb;
	jp1 = j + 1;
	ic = 1;
	if (*k == 0) {
	    goto L35;
	}
	jj1 = jp1 - *k;
	i__2 = j;
	for (jj = jj1; jj <= i__2; ++jj) {
/* L30: */
	    ic *= jj;
	}
L35:
	c__ = (doublereal) ic;
	i__2 = dls001_2.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L40: */
	    dky[i__] = c__ * yh[i__ + jp1 * yh_dim1] + s * dky[i__];
	}
/* L50: */
    }
    if (*k == 0) {
	return 0;
    }
L55:
    i__1 = -(*k);
    r__ = pow_di(&dls001_2.h__, &i__1);
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L60: */
	dky[i__] = r__ * dky[i__];
    }
    return 0;

L80:
    s_copy(msg, "DINTDY-  K (=I1) illegal      ", (ftnlen)80, (ftnlen)30);
    xerrwd_(msg, &c__30, &c__51, &c__0, &c__1, k, &c__0, &c__0, &c_b20, &
	    c_b20, (ftnlen)80);
    *iflag = -1;
    return 0;
L90:
    s_copy(msg, "DINTDY-  T (=R1) illegal      ", (ftnlen)80, (ftnlen)30);
    xerrwd_(msg, &c__30, &c__52, &c__0, &c__0, &c__0, &c__0, &c__1, t, &c_b20,
	     (ftnlen)80);
    s_copy(msg, "      T not in interval TCUR - HU (= R1) to TCUR (=R2)      "
	    , (ftnlen)80, (ftnlen)60);
    xerrwd_(msg, &c__60, &c__52, &c__0, &c__0, &c__0, &c__0, &c__2, &tp, &
	    dls001_2.tn, (ftnlen)80);
    *iflag = -2;
    return 0;
/* ----------------------- END OF SUBROUTINE DINTDY ---------------------- */
} /* dintdy_ */
