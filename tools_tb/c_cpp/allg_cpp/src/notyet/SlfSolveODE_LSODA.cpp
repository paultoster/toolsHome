#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SlfSolveODE.h" 
#include "SlfNum.h"   
#include "SlfStr.h"
#include "f2c.h"
    
namespace slf
{

#define dls001_1 Dsl1.a
#define dlsa01_1 Dsla.a
#define dls001_2 Dsl1.b
#define dlsa01_2 Dsla.b

/* Table of constant values */

static integer cc__60 = 60;
static integer cc__103 = 103;
static integer cc__0 = 0;
static doublereal c_b41 = 0.;
static integer cc__50 = 50;
static integer cc__2 = 2;
static integer cc__104 = 104;
static integer cc__101 = 101;
static integer cc__102 = 102;
static integer cc__1 = 1;
static integer cc__105 = 105;
static integer cc__106 = 106;
static integer cc__107 = 107;
static integer cc__201 = 201;
static integer cc__202 = 202;
static integer cc__203 = 203;
static integer cc__204 = 204;
static integer cc__205 = 205;
static integer cc__30 = 30;
static integer cc__206 = 206;
static integer cc__207 = 207;
static integer cc__3 = 3;
static integer cc__4 = 4;
static integer cc__5 = 5;
static integer cc__6 = 6;
static integer cc__7 = 7;
static integer cc__8 = 8;
static integer cc__9 = 9;
static integer cc__10 = 10;
static integer cc__11 = 11;
static integer cc__12 = 12;
static integer cc__13 = 13;
static integer cc__40 = 40;
static integer cc__14 = 14;
static integer cc__15 = 15;
static integer cc__16 = 16;
static integer cc__17 = 17;
static integer cc__18 = 18;
static integer cc__19 = 19;
static integer cc__20 = 20;
static integer cc__21 = 21;
static integer cc__22 = 22;
static integer cc__23 = 23;
static integer cc__24 = 24;
static integer cc__25 = 25;
static integer cc__26 = 26;
static integer cc__27 = 27;
static integer cc__28 = 28;
static integer cc__29 = 29;
static integer cc__303 = 303;
static integer cc__51 = 51;
static doublereal c_b20 = 0.;
static integer cc__52 = 52;


#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
CStr    *pIntegratorLSODA_ErrText;
#else
char       *pIntegratorLSODA_errtext;
#endif

CSolveODE_LSODA::CSolveODE_LSODA(SIntegratorBaseInp *pinp) {
    
    Status = OKAY;
    Ierr   = NO_ERR;
    defaultInp(pinp);
    

    NstepMax = NstepAccept = 0;
    
    N    = 0;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    pClass = 0;
#else
    Func = 0;
#endif
    Y          = 0;
    FlagYalloc = 0;

    Itoler= 0;
    Atol  = 0.0;
    Rtol  = 0.0;
    VAtol = 0;
    VRtol = 0;

    Hmax = 0.0;
    Nmax = 100000;

    X      = 0.0;
    Posneg = 1.0;
    H      = 0.0;

    FjacMat = 0;


}
CSolveODE_LSODA::~CSolveODE_LSODA() {
    
    reset();
}
void CSolveODE_LSODA::reset(void) {
    
    if( VAtol )    FreeVector(VAtol); VAtol = 0;
    if( VRtol )    FreeVector(VRtol); VRtol = 0;
    
    if( FlagYalloc && Y )   FreeVector(Y); Y = 0;FlagYalloc = 0;
        
    if( FjacMat )  FreeMatrix(FjacMat); FjacMat = 0;
}
//=================================================
// Init-Funktion
//=================================================
okay_t CSolveODE_LSODA::init(SIntegratorBaseInp *pinp) {
    
    //=================
    //Anzahl Zustände N
    //=================
    if( pinp->n >= MAX_SINT32 ) {
        
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_RAD::init: Anzahl der Zustände zu groß, maximal %li möglich",MAX_SINT32-1);
        return Status;
    } else if( pinp->n == 0 ) {
        
        Status = NOT_OKAY;
        ErrText.cat("CSolveODE_RAD::init: Anzahl der Zustände n gleich null");
        return Status;
    } else {
        
        N = (int32_t)pinp->n;
    }

    //=================
    // Funktionspointer
    //=================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    if(pinp->pclass == 0 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_RAD::init: Klassenpointer nicht gesetzt inp.pclass == 0\n");
        return Status;
    }
    pClass = pinp->pclass;
#else
    if(pinp->state == 0 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_RAD::init: Funktionspointer inp.state == 0\n");
        return Status;
    }
    Func   = pinp->state;                  //
#endif
   

    //===============
    // Zustandsvektor
    //===============
    if( pinp->ystate ) {

        if( GET_NROWS(pinp->ystate) != (uint32_t)N ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Zustandsvektor ystate(n) = %i, entspricht nicht vorgegebenen Statelänge N = %i\n", GET_NROWS(pinp->ystate), N);
            return Status;
        }
        Y          = pinp->ystate;
        FlagYalloc = 0;
    } else {

        Y    = NewVector((uint16_t)N);
        FlagYalloc = 1;
        if( !Y) {
             
            Status = NOT_OKAY;
            ErrText.cat("CSolveODE_RAD::init: Speicherproblem mit Zustand Y");
            return Status;
        }
    }
    ZeroVector(Y);

    //==========
    // Toleranz 
    //==========
    if( pinp->itol == 0 ) {//skalar

        Itoler = 0;
        VAtol = NewVector(1);
        VRtol = NewVector(1);

        VAtol[0] = fabs(pinp->atol);
        VRtol[0] = fabs(pinp->rtol);

        if( VAtol[0] == 0.0 ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Skalar absolute Error must be set != 0.0 \n");
            return Status;
        }
        if( VRtol[0] == 0.0 ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Skalar relative Error must be set != 0.0 \n");
            return Status;
        }

    } else { // Vektor

        uint16_t i;

        Itoler = 4;
        if( !pinp->vatol ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Vektor absolute Error must be set\n");
            return Status;
        }
        if( !pinp->vrtol ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Vektor relative Error must be set\n");
            return Status;
        }
        VAtol = VectorCopy(pinp->vatol);
        VRtol = VectorCopy(pinp->vrtol);
        
        VectorAbs(VAtol); // Absolutwert
        VectorAbs(VRtol);

        if( GET_NROWS(VAtol) < (uint32_t)N ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Vektor length:%i absolute Error must be equal N:%i\n"
                             ,GET_NROWS(VAtol)
                             ,N);
            return Status;
        }
        for(i=0;i<N;i++) {

            if( VAtol[i] == 0.0 ) {
                Status = NOT_OKAY;
                ErrText.catFormat("CSolveODE_RAD::init: CVectorD %i absolute Error must be set != 0.0 \n",i);
                return Status;
            }
        }
        if( GET_NROWS(VRtol) < (uint32_t)N ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Vektor length:%i relative Error must be equal N:%i\n"
                             ,GET_NROWS(VRtol)
                             ,N);
            return Status;
        }
        for(i=0;i<N;i++) {
            if( VRtol[i] == 0.0 ) {
                Status = NOT_OKAY;
                ErrText.catFormat("CSolveODE_RAD::init: CVectorD %i relative Error must be set != 0.0 \n",i);
                return Status;
            }
        }

    }


    //==================
    // task
    /* Itask  = an index specifying the task to be performed. */
    /*          Input only.  ITASK has the following values and meanings. */
    /*          1  means normal computation of output values of y(t) at */
    /*             t = TOUT (by overshooting and interpolating). */
    /*          2  means take one step only and return. */
    /*          3  means stop at the first internal mesh point at or */
    /*             beyond t = TOUT and return. */
    /*          4  means normal computation of output values of y(t) at */
    /*             t = TOUT but without overshooting t = TCRIT. */
    /*             TCRIT must be input as RWORK(1).  TCRIT may be equal to */
    /*             or beyond TOUT, but not behind it in the direction of */
    /*             integration.  This option is useful if the problem */
    /*             has a singularity at or beyond t = TCRIT. */
    /*          5  means take one step, without passing TCRIT, and return. */
    /*             TCRIT must be input as RWORK(1). */
    //==================
    Itask = 1;
    

    //===================
    // maximal step size 
    //===================
    Hmax = pinp->hmax;
    Hmax = fabs (Hmax);

    //===================
    // minimal step size 
    //===================
    Hmin = pinp->hmin;
    Hmin = fabs (Hmin);
    

    //===================
    // maximale Iteration
    //===================
    Nmax = pinp->nmax;

    if( Nmax == 0 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_RAD::init: Nmax == 0 \n");
        return Status;
    }
    
    //=================================
    // Wird Jacobimatrix berechnet
    //=================================
    Ijac = (int32_t)pinp->lsoda.ijac;

    if( pinp->lsoda.ijac == MAX_UINT32 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_RAD::init: ijac (Jaocobifunktion verwenden? siehe SlfSolveODE.h) muß beelgt werden \n");
        return Status;
    }



    //========================================
    // Funktionspointer Jacobi, wenn notwendig
    //========================================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 0
    if( Ijac ) { 
        if(pinp->jacobi == 0 ) {
            Status = NOT_OKAY;
            ErrText.catFormat("CSolveODE_RAD::init: Jacobifunktionspointer inp.jacobi == 0\n");
            return Status;
        }
        Jacobi   = pinp->jacobi;                  //
    }
#endif

    //=================================
    // Band der Jacobimatrix
    // mljac lower bandwidth
    // mljac = n  volle Matrix
    // 0<=mljac<n lower bandwidth unteres Band
    // mujac upper bandwidth, if mljac < n 
    //=================================
    if( pinp->lsoda.mljac == 0 )
        Mljac = N;
    else
        Mljac = (int32_t)pinp->lsoda.mljac;

    if( Mljac == MAX_SINT32 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_LSODA::init: mljac (lower bandwidth siehe SlfSolveODE.h) muß beelgt werden \n");
        return Status;
    }
    if( (int32_t)pinp->lsoda.mujac == 0  )
        Mujac = N;
    else
        Mujac = (int32_t)pinp->lsoda.mujac;

    if( Mujac == MAX_SINT32 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("CSolveODE_LSODA::init: mujac (upper bandwidth siehe SlfSolveODE.h) muß beelgt werden \n");
        return Status;
    }

    //==============
    // Jacobi Typ Jt
    /*          JT has the following values and meanings: */
    /*           1 means a user-supplied full (NEQ by NEQ) Jacobian. */
    /*           2 means an internally generated (difference quotient) full */
    /*             Jacobian (using NEQ extra calls to F per df/dy value). */
    /*           4 means a user-supplied banded Jacobian. */
    /*           5 means an internally generated banded Jacobian (using */
    /*             ML+MU+1 extra calls to F per df/dy evaluation). */
    /*          If JT = 1 or 4, the user must supply a Subroutine JAC */
    /*          (the name is arbitrary) as described above under JAC. */
    /*          If JT = 2 or 5, a dummy argument can be used. */
    //==============
    if( Mljac == N ) {
        if( Ijac )
            Jt = 1;
        else
            Jt = 2;
    } else {
        if( Ijac )
            Jt = 4;
        else
            Jt = 5;
    }

    //==================================
    // maximale Ordnung nonstiff / stiff
    //==================================
    Mxordn = pinp->lsoda.mxordn;
    Mxords = pinp->lsoda.mxords;

    if( Mxordn == 0 )
        Mxordn = 12;
    if( Mxordn > 12 )
        Mxordn = 12;

    if( Mxords == 0 )
        Mxords = 5;
    if( Mxords > 5 )
        Mxords = 5;



    //=============
    // Work-Buffers
    //=============
    Liw   = 20+N;
    Iwork = new int32_t[Liw];
    if( !Iwork ) {
        Status = NOT_OKAY;
        ErrText.cat("CSolveODE_LSODA::init: Iwork Speicherproblem");
        return Status;
    }

    Lrw = 20+16*N;
    if( Jt == 1 || Jt == 2 )
        Lrw = MAX(Lrw,22+9*N+N*N);
    else
        Lrw = MAX(Lrw,22+10*N+(2*Mljac+Mujac)*N);

    Rwork = new double[Lrw];
    if( !Rwork ) {
        Status = NOT_OKAY;
        ErrText.cat("CSolveODE_LSODA::init: Rwork Speicherproblem");
        return Status;
    }

    FjacMat = NewMatrix(MIN(N,Mljac+Mujac+1),N);
    ZeroMatrix(FjacMat);

    return Status;
    
}
okay_t CSolveODE_LSODA::calcFirst(double x
                              ,CVectorD ystart
                              ,double hstart
                              ,double posneg/*=1*/
                              ) {

    int32_t i;
    okay_t ret_ok=OKAY;

    // Startpunkt
    //===========
    X = x;
    H = fabs(hstart);

    // in welche Richtung rechnen wir (zeitlich positiv)
    //==================================================
    Posneg =  SIGN(posneg);
    Hmax   *= Posneg;
    H      *= Posneg;

    /* H0      RWORK(5)  the step size to be attempted on the first step. */
    /*                   The default value is determined by the solver. */

    /* HMAX    RWORK(6)  the maximum absolute step size allowed. */
    /*                   The default value is infinite. */

    /* HMIN    RWORK(7)  the minimum absolute step size allowed. */
    /*                   The default value is 0.  (This lower bound is not */
    /*                   enforced on the final step before reaching TCRIT */
    /*                   when ITASK = 4 or 5.) */

    /* IXPR    IWORK(5)  flag to generate extra printing at method switches. */
    /*                   IXPR = 0 means no extra printing (the default). */
    /*                   IXPR = 1 means print data on each switch. */
    /*                   T, H, and NST will be printed on the same logical */
    /*                   unit as used for error messages. */

    /* MXSTEP  IWORK(6)  maximum number of (internally defined) steps */
    /*                   allowed during one call to the solver. */
    /*                   The default value is 500. */

    /* MXHNIL  IWORK(7)  maximum number of messages printed (per problem) */
    /*                   warning that T + H = T on a step (H = step size). */
    /*                   This must be positive to result in a non-default */
    /*                   value.  The default value is 10. */

    /* MXORDN  IWORK(8)  the maximum order to be allowed for the nonstiff */
    /*                   (Adams) method.  the default value is 12. */
    /*                   if MXORDN exceeds the default value, it will */
    /*                   be reduced to the default value. */
    /*                   MXORDN is held constant during the problem. */

    /* MXORDS  IWORK(9)  the maximum order to be allowed for the stiff */
    /*                   (BDF) method.  The default value is 5. */
    /*                   If MXORDS exceeds the default value, it will */
    /*                   be reduced to the default value. */
    /*                   MXORDS is held constant during the problem. */
    /* ----------------------------------------------------------------------- */
    Itoler = 1;
    Iopt   = 1;
    Itask  = 1;
    Rwork[5-1] = H*0;
    Rwork[6-1] = H;
    Rwork[7-1] = Hmin*0;
    Iwork[1-1] = Mljac*0;
    Iwork[2-1] = Mujac*0;
#if DS_DEBUG_MODE&2 // Zustände ausgeben
    Iwork[5-1] = 1*0;
#else
    Iwork[5-1] = 0;
#endif
    Iwork[6-1] = Nmax*0;
    Iwork[7-1] = 1*0;
    Iwork[8-1] = Mxordn*0;
    Iwork[9-1] = Mxords*0;
    
    Nfcn  = 0;
    Nstep = 0;
    Njac  = 0;



    // Initialisierungswerte
    //======================
    if( GET_NROWS(ystart) != (uarray)N ) {

        Status = NOT_OKAY;
        ErrText.catFormat("Initialisierungsvektor ystart(n) = %i, entspricht nicht vorgegebenen Statelänge N = %i\n", GET_NROWS(ystart), N);
        return Status;
    }
    for(i=0;i<N;i++)
        Y[i] = ystart[i];


    // dlsoda initialisieren
    //======================
   Istate = 1;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    pIntegratorLSODA_ErrText = &ErrText;
#else
    #error pIntegratorLSODA_errtext nicht definiert
#endif

    return Status;
}
okay_t CSolveODE_LSODA::calc(double xend) {

    double hmaxn = MIN(ABS(Hmax),ABS(xend-X));

    //Itask = 4;
    //Rwork[1-1] = xend;
    // dlsoda aufrufen
    //================
	dlsoda( &N
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
          , pClass
#else
          , Func
          , Jacobi
#endif
          , FjacMat
          , ErrText
          , Y
          , &X
          , &xend
          , &Itoler
          , VRtol
          , VAtol
          , &Itask
          , &Istate
          , &Iopt
          , Rwork
          , &Lrw
          , Iwork
          , &Liw
          , &Jt);
       

    if( NstepMax < Nstep )
        NstepMax = Iwork[10];
    Iopt  = 0;
    Nfcn  = Iwork[11]-Nfcn;
    Nstep = Iwork[10]-Nstep;
    Njac  = Iwork[12]-Njac;
    Norder = Iwork[13];
    Meth  = (uint8_t)Iwork[18];
    NstepAccept = 0;
    H     = Rwork[11];
    //X     = Rwork[12];
    X     = xend;
       
/* ISTATE = an index used for input and output to specify the */
/*          the state of the calculation. */
/*          On output, ISTATE has the following values and meanings. */
/*           1  means nothing was done; TOUT = T and ISTATE = 1 on input. */
/*           2  means the integration was performed successfully. */
/*          -1  means an excessive amount of work (more than MXSTEP */
/*              steps) was done on this call, before completing the */
/*              requested task, but the integration was otherwise */
/*              successful as far as T.  (MXSTEP is an optional input */
/*              and is normally 500.)  To continue, the user may */
/*              simply reset ISTATE to a value .gt. 1 and call again */
/*              (the excess work step counter will be reset to 0). */
/*              In addition, the user may increase MXSTEP to avoid */
/*              this error return (see below on optional inputs). */
/*          -2  means too much accuracy was requested for the precision */
/*              of the machine being used.  This was detected before */
/*              completing the requested task, but the integration */
/*              was successful as far as T.  To continue, the tolerance */
/*              parameters must be reset, and ISTATE must be set */
/*              to 3.  The optional output TOLSF may be used for this */
/*              purpose.  (Note: If this condition is detected before */
/*              taking any steps, then an illegal input return */
/*              (ISTATE = -3) occurs instead.) */
/*          -3  means illegal input was detected, before taking any */
/*              integration steps.  See written message for details. */
/*              Note:  If the solver detects an infinite loop of calls */
/*              to the solver with illegal input, it will cause */
/*              the run to stop. */
/*          -4  means there were repeated error test failures on */
/*              one attempted step, before completing the requested */
/*              task, but the integration was successful as far as T. */
/*              The problem may have a singularity, or the input */
/*              may be inappropriate. */
/*          -5  means there were repeated convergence test failures on */
/*              one attempted step, before completing the requested */
/*              task, but the integration was successful as far as T. */
/*              This may be caused by an inaccurate Jacobian matrix, */
/*              if one is being used. */
/*          -6  means EWT(i) became zero for some i during the */
/*              integration.  Pure relative error control (ATOL(i)=0.0) */
/*              was requested on a variable which has now vanished. */
/*              The integration was successful as far as T. */
/*          -7  means the length of RWORK and/or IWORK was too small to */
/*              proceed, but the integration was successful as far as T. */
/*              This happens when DLSODA chooses to switch methods */
/*              but LRW and/or LIW is too small for the new method. */
/*          -8  Function-Call State-Fkt not ok */
/*          -9  Function-Call Jacobi-Fkt not ok */
/*          Note:  Since the normal output value of ISTATE is 2, */
/*          it does not need to be reset for normal continuation. */
/*          Also, since a negative input value of ISTATE will be */
/*          regarded as illegal, a negative output value requires the */
/*          user to change it, and possibly other inputs, before */
/*          calling the solver again. */


    if( Istate < 0 ) {

        switch(Istate) {
        case -1:
            Ierr   = NMAX_REACHED;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: nmax_reached");
            break;
        case -2:
            Ierr   = STEPSIZE_TOO_SMALL;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: steppsize_too_small");
            break;
        case -3:
            Ierr   = INPUT_NOT_CONSISTENT;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: input_not_consistent");
            break;
        case -4:
        case -5:
            Ierr   = 0;
            Istate = 2;
            //Status = NOT_OKAY;
            break;
        case -6:
            Ierr   = MATRIX_IS_SINGULAR;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: matrix_is_singular");
            break;
        case -7:
            Ierr   = INPUT_NOT_CONSISTENT;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: input_not_consitent");
            break;
        case -8:
            Ierr   = STATE_NO_SUCCESS;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: state_no_success");
            break;
        case -9:
            Ierr   = JACOBI_NO_SUCCESS;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: jacobi_no_success");
            break;
        default:
            Ierr   = UNKOWN_ERR;
            Status = NOT_OKAY;
            ErrText.cat("IntegratorLSODA error: unkonw error");
        }
    }
    return Status;
}
int CSolveODE_LSODA::dlsoda( integer    *neq
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
                        , CDsModBase *pclass
#else
                        , FcnEqDiff state
                        , FcnEqJac  jacobi
#endif
                        , Matrix    fjacmat
                        , CSlfStr   &errtext
                        , doublereal *y
                        , doublereal *t
                        , doublereal *tout
                        , integer    *itol
                        , doublereal *rtol
                        , doublereal *atol
                        , integer    *itask
                        , integer    *istate
                        , integer    *iopt
                        , doublereal *rwork
                        , integer    *lrw
                        , integer    *iwork
                        , integer    *liw
                        , integer    *jt)
{
    /* Initialized data */

    okay_t ret_ok=OKAY;
    static integer mord[2] = { 12,5 };
    static integer mxstp0 = 500;
    static integer mxhnl0 = 10;

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer i__;
    static doublereal h0;
    static integer i1, i2;
    static doublereal w0;
    static integer ml;
    static doublereal rh;
    static integer mu;
    static doublereal tp;
    static integer lf0;
    static doublereal big;
    static integer kgo;
    static doublereal ayi;
    static char msg[60];
    static doublereal hmx, tol, sum;
    static integer len1, len2;
    static doublereal hmax;
    static logical ihit;
    static doublereal ewti, size;
    static integer len1c, len1n, len1s, iflag;
    static doublereal atoli;
    static integer leniw, lenwm, imxer;
    static doublereal tcrit;
    static integer lenrw;
    static doublereal tdist, rtoli, tolsf, tnext;
    static integer leniwc;
    static integer lenrwc;

/* ----------------------------------------------------------------------- */
/* This is the 12 November 2003 version of */
/* DLSODA: Livermore Solver for Ordinary Differential Equations, with */
/*         Automatic method switching for stiff and nonstiff problems. */

/* This version is in double precision. */

/* DLSODA solves the initial value problem for stiff or nonstiff */
/* systems of first order ODEs, */
/*     dy/dt = f(t,y) ,  or, in component form, */
/*     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ). */

/* This a variant version of the DLSODE package. */
/* It switches automatically between stiff and nonstiff methods. */
/* This means that the user does not have to determine whether the */
/* problem is stiff or not, and the solver will automatically choose the */
/* appropriate method.  It always starts with the nonstiff method. */

/* Authors:       Alan C. Hindmarsh */
/*                Center for Applied Scientific Computing, L-561 */
/*                Lawrence Livermore National Laboratory */
/*                Livermore, CA 94551 */
/* and */
/*                Linda R. Petzold */
/*                Univ. of California at Santa Barbara */
/*                Dept. of Computer Science */
/*                Santa Barbara, CA 93106 */

/* References: */
/* 1.  Alan C. Hindmarsh,  ODEPACK, A Systematized Collection of ODE */
/*     Solvers, in Scientific Computing, R. S. Stepleman et al. (Eds.), */
/*     North-Holland, Amsterdam, 1983, pp. 55-64. */
/* 2.  Linda R. Petzold, Automatic Selection of Methods for Solving */
/*     Stiff and Nonstiff Systems of Ordinary Differential Equations, */
/*     Siam J. Sci. Stat. Comput. 4 (1983), pp. 136-148. */
/* ----------------------------------------------------------------------- */
/* Summary of Usage. */

/* Communication between the user and the DLSODA package, for normal */
/* situations, is summarized here.  This summary describes only a subset */
/* of the full set of options available.  See the full description for */
/* details, including alternative treatment of the Jacobian matrix, */
/* optional inputs and outputs, nonstandard options, and */
/* instructions for special situations.  See also the example */
/* problem (with program and output) following this summary. */

/* A. First provide a subroutine of the form: */
/*               SUBROUTINE F (NEQ, T, Y, YDOT) */
/*               DOUBLE PRECISION T, Y(*), YDOT(*) */
/* which supplies the vector function f by loading YDOT(i) with f(i). */

/* B. Write a main program which calls Subroutine DLSODA once for */
/* each point at which answers are desired.  This should also provide */
/* for possible use of logical unit 6 for output of error messages */
/* by DLSODA.  On the first call to DLSODA, supply arguments as follows: */
/* F      = name of subroutine for right-hand side vector f. */
/*          This name must be declared External in calling program. */
/* NEQ    = number of first order ODEs. */
/* Y      = array of initial values, of length NEQ. */
/* T      = the initial value of the independent variable. */
/* TOUT   = first point where output is desired (.ne. T). */
/* ITOL   = 1 or 2 according as ATOL (below) is a scalar or array. */
/* RTOL   = relative tolerance parameter (scalar). */
/* ATOL   = absolute tolerance parameter (scalar or array). */
/*          the estimated local error in y(i) will be controlled so as */
/*          to be less than */
/*             EWT(i) = RTOL*ABS(Y(i)) + ATOL     if ITOL = 1, or */
/*             EWT(i) = RTOL*ABS(Y(i)) + ATOL(i)  if ITOL = 2. */
/*          Thus the local error test passes if, in each component, */
/*          either the absolute error is less than ATOL (or ATOL(i)), */
/*          or the relative error is less than RTOL. */
/*          Use RTOL = 0.0 for pure absolute error control, and */
/*          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error */
/*          control.  Caution: actual (global) errors may exceed these */
/*          local tolerances, so choose them conservatively. */
/* ITASK  = 1 for normal computation of output values of y at t = TOUT. */
/* ISTATE = integer flag (input and output).  Set ISTATE = 1. */
/* IOPT   = 0 to indicate no optional inputs used. */
/* RWORK  = real work array of length at least: */
/*             22 + NEQ * MAX(16, NEQ + 9). */
/*          See also Paragraph E below. */
/* LRW    = declared length of RWORK (in user's dimension). */
/* IWORK  = integer work array of length at least  20 + NEQ. */
/* LIW    = declared length of IWORK (in user's dimension). */
/* JAC    = name of subroutine for Jacobian matrix. */
/*          Use a dummy name.  See also Paragraph E below. */
/* JT     = Jacobian type indicator.  Set JT = 2. */
/*          See also Paragraph E below. */
/* Note that the main program must declare arrays Y, RWORK, IWORK, */
/* and possibly ATOL. */

/* C. The output from the first call (or any call) is: */
/*      Y = array of computed values of y(t) vector. */
/*      T = corresponding value of independent variable (normally TOUT). */
/* ISTATE = 2  if DLSODA was successful, negative otherwise. */
/*          -1 means excess work done on this call (perhaps wrong JT). */
/*          -2 means excess accuracy requested (tolerances too small). */
/*          -3 means illegal input detected (see printed message). */
/*          -4 means repeated error test failures (check all inputs). */
/*          -5 means repeated convergence failures (perhaps bad Jacobian */
/*             supplied or wrong choice of JT or tolerances). */
/*          -6 means error weight became zero during problem. (Solution */
/*             component i vanished, and ATOL or ATOL(i) = 0.) */
/*          -7 means work space insufficient to finish (see messages). */

/* D. To continue the integration after a successful return, simply */
/* reset TOUT and call DLSODA again.  No other parameters need be reset. */

/* E. Note: If and when DLSODA regards the problem as stiff, and */
/* switches methods accordingly, it must make use of the NEQ by NEQ */
/* Jacobian matrix, J = df/dy.  For the sake of simplicity, the */
/* inputs to DLSODA recommended in Paragraph B above cause DLSODA to */
/* treat J as a full matrix, and to approximate it internally by */
/* difference quotients.  Alternatively, J can be treated as a band */
/* matrix (with great potential reduction in the size of the RWORK */
/* array).  Also, in either the full or banded case, the user can supply */
/* J in closed form, with a routine whose name is passed as the JAC */
/* argument.  These alternatives are described in the paragraphs on */
/* RWORK, JAC, and JT in the full description of the call sequence below. */

/* ----------------------------------------------------------------------- */
/* Example Problem. */

/* The following is a simple example problem, with the coding */
/* needed for its solution by DLSODA.  The problem is from chemical */
/* kinetics, and consists of the following three rate equations: */
/*     dy1/dt = -.04*y1 + 1.e4*y2*y3 */
/*     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2 */
/*     dy3/dt = 3.e7*y2**2 */
/* on the interval from t = 0.0 to t = 4.e10, with initial conditions */
/* y1 = 1.0, y2 = y3 = 0.  The problem is stiff. */

/* The following coding solves this problem with DLSODA, */
/* printing results at t = .4, 4., ..., 4.e10.  It uses */
/* ITOL = 2 and ATOL much smaller for y2 than y1 or y3 because */
/* y2 has much smaller values. */
/* At the end of the run, statistical quantities of interest are */
/* printed (see optional outputs in the full description below). */

/*     EXTERNAL FEX */
/*     DOUBLE PRECISION ATOL, RTOL, RWORK, T, TOUT, Y */
/*     DIMENSION Y(3), ATOL(3), RWORK(70), IWORK(23) */
/*     NEQ = 3 */
/*     Y(1) = 1. */
/*     Y(2) = 0. */
/*     Y(3) = 0. */
/*     T = 0. */
/*     TOUT = .4 */
/*     ITOL = 2 */
/*     RTOL = 1.D-4 */
/*     ATOL(1) = 1.D-6 */
/*     ATOL(2) = 1.D-10 */
/*     ATOL(3) = 1.D-6 */
/*     ITASK = 1 */
/*     ISTATE = 1 */
/*     IOPT = 0 */
/*     LRW = 70 */
/*     LIW = 23 */
/*     JT = 2 */
/*     DO 40 IOUT = 1,12 */
/*       CALL DLSODA(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE, */
/*    1     IOPT,RWORK,LRW,IWORK,LIW,JDUM,JT) */
/*       WRITE(6,20)T,Y(1),Y(2),Y(3) */
/* 20    FORMAT(' At t =',D12.4,'   Y =',3D14.6) */
/*       IF (ISTATE .LT. 0) GO TO 80 */
/* 40    TOUT = TOUT*10. */
/*     WRITE(6,60)IWORK(11),IWORK(12),IWORK(13),IWORK(19),RWORK(15) */
/* 60  FORMAT(/' No. steps =',I4,'  No. f-s =',I4,'  No. J-s =',I4/ */
/*    1   ' Method last used =',I2,'   Last switch was at t =',D12.4) */
/*     STOP */
/* 80  WRITE(6,90)ISTATE */
/* 90  FORMAT(///' Error halt.. ISTATE =',I3) */
/*     STOP */
/*     END */

/*     SUBROUTINE FEX (NEQ, T, Y, YDOT) */
/*     DOUBLE PRECISION T, Y, YDOT */
/*     DIMENSION Y(3), YDOT(3) */
/*     YDOT(1) = -.04*Y(1) + 1.D4*Y(2)*Y(3) */
/*     YDOT(3) = 3.D7*Y(2)*Y(2) */
/*     YDOT(2) = -YDOT(1) - YDOT(3) */
/*     RETURN */
/*     END */

/* The output of this program (on a CDC-7600 in single precision) */
/* is as follows: */

/*   At t =  4.0000e-01   y =  9.851712e-01  3.386380e-05  1.479493e-02 */
/*   At t =  4.0000e+00   Y =  9.055333e-01  2.240655e-05  9.444430e-02 */
/*   At t =  4.0000e+01   Y =  7.158403e-01  9.186334e-06  2.841505e-01 */
/*   At t =  4.0000e+02   Y =  4.505250e-01  3.222964e-06  5.494717e-01 */
/*   At t =  4.0000e+03   Y =  1.831975e-01  8.941774e-07  8.168016e-01 */
/*   At t =  4.0000e+04   Y =  3.898730e-02  1.621940e-07  9.610125e-01 */
/*   At t =  4.0000e+05   Y =  4.936363e-03  1.984221e-08  9.950636e-01 */
/*   At t =  4.0000e+06   Y =  5.161831e-04  2.065786e-09  9.994838e-01 */
/*   At t =  4.0000e+07   Y =  5.179817e-05  2.072032e-10  9.999482e-01 */
/*   At t =  4.0000e+08   Y =  5.283401e-06  2.113371e-11  9.999947e-01 */
/*   At t =  4.0000e+09   Y =  4.659031e-07  1.863613e-12  9.999995e-01 */
/*   At t =  4.0000e+10   Y =  1.404280e-08  5.617126e-14  1.000000e+00 */

/*   No. steps = 361  No. f-s = 693  No. J-s =  64 */
/*   Method last used = 2   Last switch was at t =  6.0092e-03 */
/* ----------------------------------------------------------------------- */
/* Full description of user interface to DLSODA. */

/* The user interface to DLSODA consists of the following parts. */

/* 1.   The call sequence to Subroutine DLSODA, which is a driver */
/*      routine for the solver.  This includes descriptions of both */
/*      the call sequence arguments and of user-supplied routines. */
/*      following these descriptions is a description of */
/*      optional inputs available through the call sequence, and then */
/*      a description of optional outputs (in the work arrays). */

/* 2.   Descriptions of other routines in the DLSODA package that may be */
/*      (optionally) called by the user.  These provide the ability to */
/*      alter error message handling, save and restore the internal */
/*      Common, and obtain specified derivatives of the solution y(t). */

/* 3.   Descriptions of Common blocks to be declared in overlay */
/*      or similar environments, or to be saved when doing an interrupt */
/*      of the problem and continued solution later. */

/* 4.   Description of a subroutine in the DLSODA package, */
/*      which the user may replace with his/her own version, if desired. */
/*      this relates to the measurement of errors. */

/* ----------------------------------------------------------------------- */
/* Part 1.  Call Sequence. */

/* The call sequence parameters used for input only are */
/*     F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, JT, */
/* and those used for both input and output are */
/*     Y, T, ISTATE. */
/* The work arrays RWORK and IWORK are also used for conditional and */
/* optional inputs and optional outputs.  (The term output here refers */
/* to the return from Subroutine DLSODA to the user's calling program.) */

/* The legality of input parameters will be thoroughly checked on the */
/* initial call for the problem, but not checked thereafter unless a */
/* change in input parameters is flagged by ISTATE = 3 on input. */

/* The descriptions of the call arguments are as follows. */

/* F      = the name of the user-supplied subroutine defining the */
/*          ODE system.  The system must be put in the first-order */
/*          form dy/dt = f(t,y), where f is a vector-valued function */
/*          of the scalar t and the vector y.  Subroutine F is to */
/*          compute the function f.  It is to have the form */
/*               SUBROUTINE F (NEQ, T, Y, YDOT) */
/*               DOUBLE PRECISION T, Y(*), YDOT(*) */
/*          where NEQ, T, and Y are input, and the array YDOT = f(t,y) */
/*          is output.  Y and YDOT are arrays of length NEQ. */
/*          Subroutine F should not alter Y(1),...,Y(NEQ). */
/*          F must be declared External in the calling program. */

/*          Subroutine F may access user-defined quantities in */
/*          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array */
/*          (dimensioned in F) and/or Y has length exceeding NEQ(1). */
/*          See the descriptions of NEQ and Y below. */

/*          If quantities computed in the F routine are needed */
/*          externally to DLSODA, an extra call to F should be made */
/*          for this purpose, for consistent and accurate results. */
/*          If only the derivative dy/dt is needed, use DINTDY instead. */

/* NEQ    = the size of the ODE system (number of first order */
/*          ordinary differential equations).  Used only for input. */
/*          NEQ may be decreased, but not increased, during the problem. */
/*          If NEQ is decreased (with ISTATE = 3 on input), the */
/*          remaining components of Y should be left undisturbed, if */
/*          these are to be accessed in F and/or JAC. */

/*          Normally, NEQ is a scalar, and it is generally referred to */
/*          as a scalar in this user interface description.  However, */
/*          NEQ may be an array, with NEQ(1) set to the system size. */
/*          (The DLSODA package accesses only NEQ(1).)  In either case, */
/*          this parameter is passed as the NEQ argument in all calls */
/*          to F and JAC.  Hence, if it is an array, locations */
/*          NEQ(2),... may be used to store other integer data and pass */
/*          it to F and/or JAC.  Subroutines F and/or JAC must include */
/*          NEQ in a Dimension statement in that case. */

/* Y      = a real array for the vector of dependent variables, of */
/*          length NEQ or more.  Used for both input and output on the */
/*          first call (ISTATE = 1), and only for output on other calls. */
/*          On the first call, Y must contain the vector of initial */
/*          values.  On output, Y contains the computed solution vector, */
/*          evaluated at T.  If desired, the Y array may be used */
/*          for other purposes between calls to the solver. */

/*          This array is passed as the Y argument in all calls to */
/*          F and JAC.  Hence its length may exceed NEQ, and locations */
/*          Y(NEQ+1),... may be used to store other real data and */
/*          pass it to F and/or JAC.  (The DLSODA package accesses only */
/*          Y(1),...,Y(NEQ).) */

/* T      = the independent variable.  On input, T is used only on the */
/*          first call, as the initial point of the integration. */
/*          on output, after each call, T is the value at which a */
/*          computed solution Y is evaluated (usually the same as TOUT). */
/*          on an error return, T is the farthest point reached. */

/* TOUT   = the next value of t at which a computed solution is desired. */
/*          Used only for input. */

/*          When starting the problem (ISTATE = 1), TOUT may be equal */
/*          to T for one call, then should .ne. T for the next call. */
/*          For the initial t, an input value of TOUT .ne. T is used */
/*          in order to determine the direction of the integration */
/*          (i.e. the algebraic sign of the step sizes) and the rough */
/*          scale of the problem.  Integration in either direction */
/*          (forward or backward in t) is permitted. */

/*          If ITASK = 2 or 5 (one-step modes), TOUT is ignored after */
/*          the first call (i.e. the first call with TOUT .ne. T). */
/*          Otherwise, TOUT is required on every call. */

/*          If ITASK = 1, 3, or 4, the values of TOUT need not be */
/*          monotone, but a value of TOUT which backs up is limited */
/*          to the current internal T interval, whose endpoints are */
/*          TCUR - HU and TCUR (see optional outputs, below, for */
/*          TCUR and HU). */

/* ITOL   = an indicator for the type of error control.  See */
/*          description below under ATOL.  Used only for input. */

/* RTOL   = a relative error tolerance parameter, either a scalar or */
/*          an array of length NEQ.  See description below under ATOL. */
/*          Input only. */

/* ATOL   = an absolute error tolerance parameter, either a scalar or */
/*          an array of length NEQ.  Input only. */

/*             The input parameters ITOL, RTOL, and ATOL determine */
/*          the error control performed by the solver.  The solver will */
/*          control the vector E = (E(i)) of estimated local errors */
/*          in y, according to an inequality of the form */
/*                      max-norm of ( E(i)/EWT(i) )   .le.   1, */
/*          where EWT = (EWT(i)) is a vector of positive error weights. */
/*          The values of RTOL and ATOL should all be non-negative. */
/*          The following table gives the types (scalar/array) of */
/*          RTOL and ATOL, and the corresponding form of EWT(i). */

/*             ITOL    RTOL       ATOL          EWT(i) */
/*              1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL */
/*              2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i) */
/*              3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL */
/*              4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i) */

/*          When either of these parameters is a scalar, it need not */
/*          be dimensioned in the user's calling program. */

/*          If none of the above choices (with ITOL, RTOL, and ATOL */
/*          fixed throughout the problem) is suitable, more general */
/*          error controls can be obtained by substituting a */
/*          user-supplied routine for the setting of EWT. */
/*          See Part 4 below. */

/*          If global errors are to be estimated by making a repeated */
/*          run on the same problem with smaller tolerances, then all */
/*          components of RTOL and ATOL (i.e. of EWT) should be scaled */
/*          down uniformly. */

/* ITASK  = an index specifying the task to be performed. */
/*          Input only.  ITASK has the following values and meanings. */
/*          1  means normal computation of output values of y(t) at */
/*             t = TOUT (by overshooting and interpolating). */
/*          2  means take one step only and return. */
/*          3  means stop at the first internal mesh point at or */
/*             beyond t = TOUT and return. */
/*          4  means normal computation of output values of y(t) at */
/*             t = TOUT but without overshooting t = TCRIT. */
/*             TCRIT must be input as RWORK(1).  TCRIT may be equal to */
/*             or beyond TOUT, but not behind it in the direction of */
/*             integration.  This option is useful if the problem */
/*             has a singularity at or beyond t = TCRIT. */
/*          5  means take one step, without passing TCRIT, and return. */
/*             TCRIT must be input as RWORK(1). */

/*          Note:  If ITASK = 4 or 5 and the solver reaches TCRIT */
/*          (within roundoff), it will return T = TCRIT (exactly) to */
/*          indicate this (unless ITASK = 4 and TOUT comes before TCRIT, */
/*          in which case answers at t = TOUT are returned first). */

/* ISTATE = an index used for input and output to specify the */
/*          the state of the calculation. */

/*          On input, the values of ISTATE are as follows. */
/*          1  means this is the first call for the problem */
/*             (initializations will be done).  See note below. */
/*          2  means this is not the first call, and the calculation */
/*             is to continue normally, with no change in any input */
/*             parameters except possibly TOUT and ITASK. */
/*             (If ITOL, RTOL, and/or ATOL are changed between calls */
/*             with ISTATE = 2, the new values will be used but not */
/*             tested for legality.) */
/*          3  means this is not the first call, and the */
/*             calculation is to continue normally, but with */
/*             a change in input parameters other than */
/*             TOUT and ITASK.  Changes are allowed in */
/*             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, JT, ML, MU, */
/*             and any optional inputs except H0, MXORDN, and MXORDS. */
/*             (See IWORK description for ML and MU.) */
/*          Note:  A preliminary call with TOUT = T is not counted */
/*          as a first call here, as no initialization or checking of */
/*          input is done.  (Such a call is sometimes useful for the */
/*          purpose of outputting the initial conditions.) */
/*          Thus the first call for which TOUT .ne. T requires */
/*          ISTATE = 1 on input. */

/*          On output, ISTATE has the following values and meanings. */
/*           1  means nothing was done; TOUT = T and ISTATE = 1 on input. */
/*           2  means the integration was performed successfully. */
/*          -1  means an excessive amount of work (more than MXSTEP */
/*              steps) was done on this call, before completing the */
/*              requested task, but the integration was otherwise */
/*              successful as far as T.  (MXSTEP is an optional input */
/*              and is normally 500.)  To continue, the user may */
/*              simply reset ISTATE to a value .gt. 1 and call again */
/*              (the excess work step counter will be reset to 0). */
/*              In addition, the user may increase MXSTEP to avoid */
/*              this error return (see below on optional inputs). */
/*          -2  means too much accuracy was requested for the precision */
/*              of the machine being used.  This was detected before */
/*              completing the requested task, but the integration */
/*              was successful as far as T.  To continue, the tolerance */
/*              parameters must be reset, and ISTATE must be set */
/*              to 3.  The optional output TOLSF may be used for this */
/*              purpose.  (Note: If this condition is detected before */
/*              taking any steps, then an illegal input return */
/*              (ISTATE = -3) occurs instead.) */
/*          -3  means illegal input was detected, before taking any */
/*              integration steps.  See written message for details. */
/*              Note:  If the solver detects an infinite loop of calls */
/*              to the solver with illegal input, it will cause */
/*              the run to stop. */
/*          -4  means there were repeated error test failures on */
/*              one attempted step, before completing the requested */
/*              task, but the integration was successful as far as T. */
/*              The problem may have a singularity, or the input */
/*              may be inappropriate. */
/*          -5  means there were repeated convergence test failures on */
/*              one attempted step, before completing the requested */
/*              task, but the integration was successful as far as T. */
/*              This may be caused by an inaccurate Jacobian matrix, */
/*              if one is being used. */
/*          -6  means EWT(i) became zero for some i during the */
/*              integration.  Pure relative error control (ATOL(i)=0.0) */
/*              was requested on a variable which has now vanished. */
/*              The integration was successful as far as T. */
/*          -7  means the length of RWORK and/or IWORK was too small to */
/*              proceed, but the integration was successful as far as T. */
/*              This happens when DLSODA chooses to switch methods */
/*              but LRW and/or LIW is too small for the new method. */

/*          Note:  Since the normal output value of ISTATE is 2, */
/*          it does not need to be reset for normal continuation. */
/*          Also, since a negative input value of ISTATE will be */
/*          regarded as illegal, a negative output value requires the */
/*          user to change it, and possibly other inputs, before */
/*          calling the solver again. */

/* IOPT   = an integer flag to specify whether or not any optional */
/*          inputs are being used on this call.  Input only. */
/*          The optional inputs are listed separately below. */
/*          IOPT = 0 means no optional inputs are being used. */
/*                   default values will be used in all cases. */
/*          IOPT = 1 means one or more optional inputs are being used. */

/* RWORK  = a real array (double precision) for work space, and (in the */
/*          first 20 words) for conditional and optional inputs and */
/*          optional outputs. */
/*          As DLSODA switches automatically between stiff and nonstiff */
/*          methods, the required length of RWORK can change during the */
/*          problem.  Thus the RWORK array passed to DLSODA can either */
/*          have a static (fixed) length large enough for both methods, */
/*          or have a dynamic (changing) length altered by the calling */
/*          program in response to output from DLSODA. */

/*                       --- Fixed Length Case --- */
/*          If the RWORK length is to be fixed, it should be at least */
/*               MAX (LRN, LRS), */
/*          where LRN and LRS are the RWORK lengths required when the */
/*          current method is nonstiff or stiff, respectively. */

/*          The separate RWORK length requirements LRN and LRS are */
/*          as follows: */
/*          IF NEQ is constant and the maximum method orders have */
/*          their default values, then */
/*             LRN = 20 + 16*NEQ, */
/*             LRS = 22 + 9*NEQ + NEQ**2           if JT = 1 or 2, */
/*             LRS = 22 + 10*NEQ + (2*ML+MU)*NEQ   if JT = 4 or 5. */
/*          Under any other conditions, LRN and LRS are given by: */
/*             LRN = 20 + NYH*(MXORDN+1) + 3*NEQ, */
/*             LRS = 20 + NYH*(MXORDS+1) + 3*NEQ + LMAT, */
/*          where */
/*             NYH    = the initial value of NEQ, */
/*             MXORDN = 12, unless a smaller value is given as an */
/*                      optional input, */
/*             MXORDS = 5, unless a smaller value is given as an */
/*                      optional input, */
/*             LMAT   = length of matrix work space: */
/*             LMAT   = NEQ**2 + 2              if JT = 1 or 2, */
/*             LMAT   = (2*ML + MU + 1)*NEQ + 2 if JT = 4 or 5. */

/*                       --- Dynamic Length Case --- */
/*          If the length of RWORK is to be dynamic, then it should */
/*          be at least LRN or LRS, as defined above, depending on the */
/*          current method.  Initially, it must be at least LRN (since */
/*          DLSODA starts with the nonstiff method).  On any return */
/*          from DLSODA, the optional output MCUR indicates the current */
/*          method.  If MCUR differs from the value it had on the */
/*          previous return, or if there has only been one call to */
/*          DLSODA and MCUR is now 2, then DLSODA has switched */
/*          methods during the last call, and the length of RWORK */
/*          should be reset (to LRN if MCUR = 1, or to LRS if */
/*          MCUR = 2).  (An increase in the RWORK length is required */
/*          if DLSODA returned ISTATE = -7, but not otherwise.) */
/*          After resetting the length, call DLSODA with ISTATE = 3 */
/*          to signal that change. */

/* LRW    = the length of the array RWORK, as declared by the user. */
/*          (This will be checked by the solver.) */

/* IWORK  = an integer array for work space. */
/*          As DLSODA switches automatically between stiff and nonstiff */
/*          methods, the required length of IWORK can change during */
/*          problem, between */
/*             LIS = 20 + NEQ   and   LIN = 20, */
/*          respectively.  Thus the IWORK array passed to DLSODA can */
/*          either have a fixed length of at least 20 + NEQ, or have a */
/*          dynamic length of at least LIN or LIS, depending on the */
/*          current method.  The comments on dynamic length under */
/*          RWORK above apply here.  Initially, this length need */
/*          only be at least LIN = 20. */

/*          The first few words of IWORK are used for conditional and */
/*          optional inputs and optional outputs. */

/*          The following 2 words in IWORK are conditional inputs: */
/*            IWORK(1) = ML     these are the lower and upper */
/*            IWORK(2) = MU     half-bandwidths, respectively, of the */
/*                       banded Jacobian, excluding the main diagonal. */
/*                       The band is defined by the matrix locations */
/*                       (i,j) with i-ML .le. j .le. i+MU.  ML and MU */
/*                       must satisfy  0 .le.  ML,MU  .le. NEQ-1. */
/*                       These are required if JT is 4 or 5, and */
/*                       ignored otherwise.  ML and MU may in fact be */
/*                       the band parameters for a matrix to which */
/*                       df/dy is only approximately equal. */

/* LIW    = the length of the array IWORK, as declared by the user. */
/*          (This will be checked by the solver.) */

/* Note: The base addresses of the work arrays must not be */
/* altered between calls to DLSODA for the same problem. */
/* The contents of the work arrays must not be altered */
/* between calls, except possibly for the conditional and */
/* optional inputs, and except for the last 3*NEQ words of RWORK. */
/* The latter space is used for internal scratch space, and so is */
/* available for use by the user outside DLSODA between calls, if */
/* desired (but not for use by F or JAC). */

/* JAC    = the name of the user-supplied routine to compute the */
/*          Jacobian matrix, df/dy, if JT = 1 or 4.  The JAC routine */
/*          is optional, but if the problem is expected to be stiff much */
/*          of the time, you are encouraged to supply JAC, for the sake */
/*          of efficiency.  (Alternatively, set JT = 2 or 5 to have */
/*          DLSODA compute df/dy internally by difference quotients.) */
/*          If and when DLSODA uses df/dy, it treats this NEQ by NEQ */
/*          matrix either as full (JT = 1 or 2), or as banded (JT = */
/*          4 or 5) with half-bandwidths ML and MU (discussed under */
/*          IWORK above).  In either case, if JT = 1 or 4, the JAC */
/*          routine must compute df/dy as a function of the scalar t */
/*          and the vector y.  It is to have the form */
/*               SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD) */
/*               DOUBLE PRECISION T, Y(*), PD(NROWPD,*) */
/*          where NEQ, T, Y, ML, MU, and NROWPD are input and the array */
/*          PD is to be loaded with partial derivatives (elements of */
/*          the Jacobian matrix) on output.  PD must be given a first */
/*          dimension of NROWPD.  T and Y have the same meaning as in */
/*          Subroutine F. */
/*               In the full matrix case (JT = 1), ML and MU are */
/*          ignored, and the Jacobian is to be loaded into PD in */
/*          columnwise manner, with df(i)/dy(j) loaded into PD(i,j). */
/*               In the band matrix case (JT = 4), the elements */
/*          within the band are to be loaded into PD in columnwise */
/*          manner, with diagonal lines of df/dy loaded into the rows */
/*          of PD.  Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j). */
/*          ML and MU are the half-bandwidth parameters (see IWORK). */
/*          The locations in PD in the two triangular areas which */
/*          correspond to nonexistent matrix elements can be ignored */
/*          or loaded arbitrarily, as they are overwritten by DLSODA. */
/*               JAC need not provide df/dy exactly.  A crude */
/*          approximation (possibly with a smaller bandwidth) will do. */
/*               In either case, PD is preset to zero by the solver, */
/*          so that only the nonzero elements need be loaded by JAC. */
/*          Each call to JAC is preceded by a call to F with the same */
/*          arguments NEQ, T, and Y.  Thus to gain some efficiency, */
/*          intermediate quantities shared by both calculations may be */
/*          saved in a user Common block by F and not recomputed by JAC, */
/*          if desired.  Also, JAC may alter the Y array, if desired. */
/*          JAC must be declared External in the calling program. */
/*               Subroutine JAC may access user-defined quantities in */
/*          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array */
/*          (dimensioned in JAC) and/or Y has length exceeding NEQ(1). */
/*          See the descriptions of NEQ and Y above. */

/* JT     = Jacobian type indicator.  Used only for input. */
/*          JT specifies how the Jacobian matrix df/dy will be */
/*          treated, if and when DLSODA requires this matrix. */
/*          JT has the following values and meanings: */
/*           1 means a user-supplied full (NEQ by NEQ) Jacobian. */
/*           2 means an internally generated (difference quotient) full */
/*             Jacobian (using NEQ extra calls to F per df/dy value). */
/*           4 means a user-supplied banded Jacobian. */
/*           5 means an internally generated banded Jacobian (using */
/*             ML+MU+1 extra calls to F per df/dy evaluation). */
/*          If JT = 1 or 4, the user must supply a Subroutine JAC */
/*          (the name is arbitrary) as described above under JAC. */
/*          If JT = 2 or 5, a dummy argument can be used. */
/* ----------------------------------------------------------------------- */
/* Optional Inputs. */

/* The following is a list of the optional inputs provided for in the */
/* call sequence.  (See also Part 2.)  For each such input variable, */
/* this table lists its name as used in this documentation, its */
/* location in the call sequence, its meaning, and the default value. */
/* The use of any of these inputs requires IOPT = 1, and in that */
/* case all of these inputs are examined.  A value of zero for any */
/* of these optional inputs will cause the default value to be used. */
/* Thus to use a subset of the optional inputs, simply preload */
/* locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and */
/* then set those of interest to nonzero values. */

/* Name    Location      Meaning and Default Value */

/* H0      RWORK(5)  the step size to be attempted on the first step. */
/*                   The default value is determined by the solver. */

/* HMAX    RWORK(6)  the maximum absolute step size allowed. */
/*                   The default value is infinite. */

/* HMIN    RWORK(7)  the minimum absolute step size allowed. */
/*                   The default value is 0.  (This lower bound is not */
/*                   enforced on the final step before reaching TCRIT */
/*                   when ITASK = 4 or 5.) */

/* IXPR    IWORK(5)  flag to generate extra printing at method switches. */
/*                   IXPR = 0 means no extra printing (the default). */
/*                   IXPR = 1 means print data on each switch. */
/*                   T, H, and NST will be printed on the same logical */
/*                   unit as used for error messages. */

/* MXSTEP  IWORK(6)  maximum number of (internally defined) steps */
/*                   allowed during one call to the solver. */
/*                   The default value is 500. */

/* MXHNIL  IWORK(7)  maximum number of messages printed (per problem) */
/*                   warning that T + H = T on a step (H = step size). */
/*                   This must be positive to result in a non-default */
/*                   value.  The default value is 10. */

/* MXORDN  IWORK(8)  the maximum order to be allowed for the nonstiff */
/*                   (Adams) method.  the default value is 12. */
/*                   if MXORDN exceeds the default value, it will */
/*                   be reduced to the default value. */
/*                   MXORDN is held constant during the problem. */

/* MXORDS  IWORK(9)  the maximum order to be allowed for the stiff */
/*                   (BDF) method.  The default value is 5. */
/*                   If MXORDS exceeds the default value, it will */
/*                   be reduced to the default value. */
/*                   MXORDS is held constant during the problem. */
/* ----------------------------------------------------------------------- */
/* Optional Outputs. */

/* As optional additional output from DLSODA, the variables listed */
/* below are quantities related to the performance of DLSODA */
/* which are available to the user.  These are communicated by way of */
/* the work arrays, but also have internal mnemonic names as shown. */
/* except where stated otherwise, all of these outputs are defined */
/* on any successful return from DLSODA, and on any return with */
/* ISTATE = -1, -2, -4, -5, or -6.  On an illegal input return */
/* (ISTATE = -3), they will be unchanged from their existing values */
/* (if any), except possibly for TOLSF, LENRW, and LENIW. */
/* On any error return, outputs relevant to the error will be defined, */
/* as noted below. */

/* Name    Location      Meaning */

/* HU      RWORK(11) the step size in t last used (successfully). */

/* HCUR    RWORK(12) the step size to be attempted on the next step. */

/* TCUR    RWORK(13) the current value of the independent variable */
/*                   which the solver has actually reached, i.e. the */
/*                   current internal mesh point in t.  On output, TCUR */
/*                   will always be at least as far as the argument */
/*                   T, but may be farther (if interpolation was done). */

/* TOLSF   RWORK(14) a tolerance scale factor, greater than 1.0, */
/*                   computed when a request for too much accuracy was */
/*                   detected (ISTATE = -3 if detected at the start of */
/*                   the problem, ISTATE = -2 otherwise).  If ITOL is */
/*                   left unaltered but RTOL and ATOL are uniformly */
/*                   scaled up by a factor of TOLSF for the next call, */
/*                   then the solver is deemed likely to succeed. */
/*                   (The user may also ignore TOLSF and alter the */
/*                   tolerance parameters in any other way appropriate.) */

/* TSW     RWORK(15) the value of t at the time of the last method */
/*                   switch, if any. */

/* NST     IWORK(11) the number of steps taken for the problem so far. */

/* NFE     IWORK(12) the number of f evaluations for the problem so far. */

/* NJE     IWORK(13) the number of Jacobian evaluations (and of matrix */
/*                   LU decompositions) for the problem so far. */

/* NQU     IWORK(14) the method order last used (successfully). */

/* NQCUR   IWORK(15) the order to be attempted on the next step. */

/* IMXER   IWORK(16) the index of the component of largest magnitude in */
/*                   the weighted local error vector ( E(i)/EWT(i) ), */
/*                   on an error return with ISTATE = -4 or -5. */

/* LENRW   IWORK(17) the length of RWORK actually required, assuming */
/*                   that the length of RWORK is to be fixed for the */
/*                   rest of the problem, and that switching may occur. */
/*                   This is defined on normal returns and on an illegal */
/*                   input return for insufficient storage. */

/* LENIW   IWORK(18) the length of IWORK actually required, assuming */
/*                   that the length of IWORK is to be fixed for the */
/*                   rest of the problem, and that switching may occur. */
/*                   This is defined on normal returns and on an illegal */
/*                   input return for insufficient storage. */

/* MUSED   IWORK(19) the method indicator for the last successful step: */
/*                   1 means Adams (nonstiff), 2 means BDF (stiff). */

/* MCUR    IWORK(20) the current method indicator: */
/*                   1 means Adams (nonstiff), 2 means BDF (stiff). */
/*                   This is the method to be attempted */
/*                   on the next step.  Thus it differs from MUSED */
/*                   only if a method switch has just been made. */

/* The following two arrays are segments of the RWORK array which */
/* may also be of interest to the user as optional outputs. */
/* For each array, the table below gives its internal name, */
/* its base address in RWORK, and its description. */

/* Name    Base Address      Description */

/* YH      21             the Nordsieck history array, of size NYH by */
/*                        (NQCUR + 1), where NYH is the initial value */
/*                        of NEQ.  For j = 0,1,...,NQCUR, column j+1 */
/*                        of YH contains HCUR**j/factorial(j) times */
/*                        the j-th derivative of the interpolating */
/*                        polynomial currently representing the solution, */
/*                        evaluated at T = TCUR. */

/* ACOR     LACOR         array of size NEQ used for the accumulated */
/*         (from Common   corrections on each step, scaled on output */
/*           as noted)    to represent the estimated local error in y */
/*                        on the last step.  This is the vector E in */
/*                        the description of the error control.  It is */
/*                        defined only on a successful return from */
/*                        DLSODA.  The base address LACOR is obtained by */
/*                        including in the user's program the */
/*                        following 2 lines: */
/*                           COMMON /DLS001/ RLS(218), ILS(37) */
/*                           LACOR = ILS(22) */

/* ----------------------------------------------------------------------- */
/* Part 2.  Other Routines Callable. */

/* The following are optional calls which the user may make to */
/* gain additional capabilities in conjunction with DLSODA. */
/* (The routines XSETUN and XSETF are designed to conform to the */
/* SLATEC error handling package.) */

/*     Form of Call                  Function */
/*   CALL XSETUN(LUN)          set the logical unit number, LUN, for */
/*                             output of messages from DLSODA, if */
/*                             the default is not desired. */
/*                             The default value of LUN is 6. */

/*   CALL XSETF(MFLAG)         set a flag to control the printing of */
/*                             messages by DLSODA. */
/*                             MFLAG = 0 means do not print. (Danger: */
/*                             This risks losing valuable information.) */
/*                             MFLAG = 1 means print (the default). */

/*                             Either of the above calls may be made at */
/*                             any time and will take effect immediately. */

/*   CALL DSRCMA(RSAV,ISAV,JOB) saves and restores the contents of */
/*                             the internal Common blocks used by */
/*                             DLSODA (see Part 3 below). */
/*                             RSAV must be a real array of length 240 */
/*                             or more, and ISAV must be an integer */
/*                             array of length 46 or more. */
/*                             JOB=1 means save Common into RSAV/ISAV. */
/*                             JOB=2 means restore Common from RSAV/ISAV. */
/*                                DSRCMA is useful if one is */
/*                             interrupting a run and restarting */
/*                             later, or alternating between two or */
/*                             more problems solved with DLSODA. */

/*   CALL DINTDY(,,,,,)        provide derivatives of y, of various */
/*        (see below)          orders, at a specified point t, if */
/*                             desired.  It may be called only after */
/*                             a successful return from DLSODA. */

/* The detailed instructions for using DINTDY are as follows. */
/* The form of the call is: */

/*   CALL DINTDY (T, K, RWORK(21), NYH, DKY, IFLAG) */

/* The input parameters are: */

/* T         = value of independent variable where answers are desired */
/*             (normally the same as the T last returned by DLSODA). */
/*             For valid results, T must lie between TCUR - HU and TCUR. */
/*             (See optional outputs for TCUR and HU.) */
/* K         = integer order of the derivative desired.  K must satisfy */
/*             0 .le. K .le. NQCUR, where NQCUR is the current order */
/*             (see optional outputs).  The capability corresponding */
/*             to K = 0, i.e. computing y(T), is already provided */
/*             by DLSODA directly.  Since NQCUR .ge. 1, the first */
/*             derivative dy/dt is always available with DINTDY. */
/* RWORK(21) = the base address of the history array YH. */
/* NYH       = column length of YH, equal to the initial value of NEQ. */

/* The output parameters are: */

/* DKY       = a real array of length NEQ containing the computed value */
/*             of the K-th derivative of y(t). */
/* IFLAG     = integer flag, returned as 0 if K and T were legal, */
/*             -1 if K was illegal, and -2 if T was illegal. */
/*             On an error return, a message is also written. */
/* ----------------------------------------------------------------------- */
/* Part 3.  Common Blocks. */

/* If DLSODA is to be used in an overlay situation, the user */
/* must declare, in the primary overlay, the variables in: */
/*   (1) the call sequence to DLSODA, and */
/*   (2) the two internal Common blocks */
/*         /DLS001/  of length  255  (218 double precision words */
/*                      followed by 37 integer words), */
/*         /DLSA01/  of length  31    (22 double precision words */
/*                      followed by  9 integer words). */

/* If DLSODA is used on a system in which the contents of internal */
/* Common blocks are not preserved between calls, the user should */
/* declare the above Common blocks in the calling program to insure */
/* that their contents are preserved. */

/* If the solution of a given problem by DLSODA is to be interrupted */
/* and then later continued, such as when restarting an interrupted run */
/* or alternating between two or more problems, the user should save, */
/* following the return from the last DLSODA call prior to the */
/* interruption, the contents of the call sequence variables and the */
/* internal Common blocks, and later restore these values before the */
/* next DLSODA call for that problem.  To save and restore the Common */
/* blocks, use Subroutine DSRCMA (see Part 2 above). */

/* ----------------------------------------------------------------------- */
/* Part 4.  Optionally Replaceable Solver Routines. */

/* Below is a description of a routine in the DLSODA package which */
/* relates to the measurement of errors, and can be */
/* replaced by a user-supplied version, if desired.  However, since such */
/* a replacement may have a major impact on performance, it should be */
/* done only when absolutely necessary, and only with great caution. */
/* (Note: The means by which the package version of a routine is */
/* superseded by the user's version may be system-dependent.) */

/* (a) DEWSET. */
/* The following subroutine is called just before each internal */
/* integration step, and sets the array of error weights, EWT, as */
/* described under ITOL/RTOL/ATOL above: */
/*     Subroutine DEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT) */
/* where NEQ, ITOL, RTOL, and ATOL are as in the DLSODA call sequence, */
/* YCUR contains the current dependent variable vector, and */
/* EWT is the array of weights set by DEWSET. */

/* If the user supplies this subroutine, it must return in EWT(i) */
/* (i = 1,...,NEQ) a positive quantity suitable for comparing errors */
/* in y(i) to.  The EWT array returned by DEWSET is passed to the */
/* DMNORM routine, and also used by DLSODA in the computation */
/* of the optional output IMXER, and the increments for difference */
/* quotient Jacobians. */

/* In the user-supplied version of DEWSET, it may be desirable to use */
/* the current values of derivatives of y.  Derivatives up to order NQ */
/* are available from the history array YH, described above under */
/* optional outputs.  In DEWSET, YH is identical to the YCUR array, */
/* extended to NQ + 1 columns with a column length of NYH and scale */
/* factors of H**j/factorial(j).  On the first call for the problem, */
/* given by NST = 0, NQ is 1 and H is temporarily set to 1.0. */
/* NYH is the initial value of NEQ.  The quantities NQ, H, and NST */
/* can be obtained by including in DEWSET the statements: */
/*     DOUBLE PRECISION RLS */
/*     COMMON /DLS001/ RLS(218),ILS(37) */
/*     NQ = ILS(33) */
/*     NST = ILS(34) */
/*     H = RLS(212) */
/* Thus, for example, the current value of dy/dt can be obtained as */
/* YCUR(NYH+i)/H  (i=1,...,NEQ)  (and the division by H is */
/* unnecessary when NST = 0). */
/* ----------------------------------------------------------------------- */

/* ***REVISION HISTORY  (YYYYMMDD) */
/* 19811102  DATE WRITTEN */
/* 19820126  Fixed bug in tests of work space lengths; */
/*           minor corrections in main prologue and comments. */
/* 19870330  Major update: corrected comments throughout; */
/*           removed TRET from Common; rewrote EWSET with 4 loops; */
/*           fixed t test in INTDY; added Cray directives in STODA; */
/*           in STODA, fixed DELP init. and logic around PJAC call; */
/*           combined routines to save/restore Common; */
/*           passed LEVEL = 0 in error message calls (except run abort). */
/* 19970225  Fixed lines setting JSTART = -2 in Subroutine LSODA. */
/* 20010425  Major update: convert source lines to upper case; */
/*           added *DECK lines; changed from 1 to * in dummy dimensions; */
/*           changed names R1MACH/D1MACH to RUMACH/DUMACH; */
/*           renamed routines for uniqueness across single/double prec.; */
/*           converted intrinsic names to generic form; */
/*           removed ILLIN and NTREP (data loaded) from Common; */
/*           removed all 'own' variables from Common; */
/*           changed error messages to quoted strings; */
/*           replaced XERRWV/XERRWD with 1993 revised version; */
/*           converted prologues, comments, error messages to mixed case; */
/*           numerous corrections to prologues and internal comments. */
/* 20010507  Converted single precision source to double precision. */
/* 20010613  Revised excess accuracy test (to match rest of ODEPACK). */
/* 20010808  Fixed bug in DPRJA (matrix in DBNORM call). */
/* 20020502  Corrected declarations in descriptions of user routines. */
/* 20031105  Restored 'own' variables to Common blocks, to enable */
/*           interrupt/restart feature. */
/* 20031112  Added SAVE statements for data-loaded constants. */

/* ----------------------------------------------------------------------- */
/* Other routines in the DLSODA package. */

/* In addition to Subroutine DLSODA, the DLSODA package includes the */
/* following subroutines and function routines: */
/*  DINTDY   computes an interpolated value of the y vector at t = TOUT. */
/*  DSTODA   is the core integrator, which does one step of the */
/*           integration and the associated error control. */
/*  DCFODE   sets all method coefficients and test constants. */
/*  DPRJA    computes and preprocesses the Jacobian matrix J = df/dy */
/*           and the Newton iteration matrix P = I - h*l0*J. */
/*  DSOLSY   manages solution of linear system in chord iteration. */
/*  DEWSET   sets the error weight vector EWT before each step. */
/*  DMNORM   computes the weighted max-norm of a vector. */
/*  DFNORM   computes the norm of a full matrix consistent with the */
/*           weighted max-norm on vectors. */
/*  DBNORM   computes the norm of a band matrix consistent with the */
/*           weighted max-norm on vectors. */
/*  DSRCMA   is a user-callable routine to save and restore */
/*           the contents of the internal Common blocks. */
/*  DGEFA and DGESL   are routines from LINPACK for solving full */
/*           systems of linear algebraic equations. */
/*  DGBFA and DGBSL   are routines from LINPACK for solving banded */
/*           linear systems. */
/*  DUMACH   computes the unit roundoff in a machine-independent manner. */
/*  XERRWD, XSETUN, XSETF, IXSAV, and IUMACH  handle the printing of all */
/*           error messages and warnings.  XERRWD is machine-dependent. */
/* Note:  DMNORM, DFNORM, DBNORM, DUMACH, IXSAV, and IUMACH are */
/* function routines.  All the others are subroutines. */

/* ----------------------------------------------------------------------- */
/* ----------------------------------------------------------------------- */
/* The following two internal Common blocks contain */
/* (a) variables which are local to any subroutine but whose values must */
/*     be preserved between calls to the routine ("own" variables), and */
/* (b) variables which are communicated between subroutines. */
/* The block DLS001 is declared in subroutines DLSODA, DINTDY, DSTODA, */
/* DPRJA, and DSOLSY. */
/* The block DLSA01 is declared in subroutines DLSODA, DSTODA, and DPRJA. */
/* Groups of variables are replaced by dummy arrays in the Common */
/* declarations in routines where those variables are not used. */
/* ----------------------------------------------------------------------- */


    /* Parameter adjustments */
    --neq;
    --y;
    --rtol;
    --atol;
    --rwork;
    --iwork;

    /* Function Body */
/* ----------------------------------------------------------------------- */
/* Block A. */
/* This code block is executed on every call. */
/* It tests ISTATE and ITASK for legality and branches appropriately. */
/* If ISTATE .gt. 1 but the flag INIT shows that initialization has */
/* not yet been done, an error return occurs. */
/* If ISTATE = 1 and TOUT = T, return immediately. */
/* ----------------------------------------------------------------------- */
    if (*istate < 1 || *istate > 3) {
	goto L601;
    }
    if (*itask < 1 || *itask > 5) {
	goto L602;
    }
    if (*istate == 1) {
	goto L10;
    }
    if (dls001_1.init == 0) {
	goto L603;
    }
    if (*istate == 2) {
	goto L200;
    }
    goto L20;
L10:
    dls001_1.init = 0;
    if (*tout == *t) {
	return 0;
    }
/* ----------------------------------------------------------------------- */
/* Block B. */
/* The next code block is executed for the initial call (ISTATE = 1), */
/* or for a continuation call with parameter changes (ISTATE = 3). */
/* It contains checking of all inputs and various initializations. */

/* First check legality of the non-optional inputs NEQ, ITOL, IOPT, */
/* JT, ML, and MU. */
/* ----------------------------------------------------------------------- */
L20:
    if (neq[1] <= 0) {
	goto L604;
    }
    if (*istate == 1) {
	goto L25;
    }
    if (neq[1] > dls001_1.n) {
	goto L605;
    }
L25:
    dls001_1.n = neq[1];
    if (*itol < 1 || *itol > 4) {
	goto L606;
    }
    if (*iopt < 0 || *iopt > 1) {
	goto L607;
    }
    if (*jt == 3 || *jt < 1 || *jt > 5) {
	goto L608;
    }
    dlsa01_1.jtyp = *jt;
    if (*jt <= 2) {
	goto L30;
    }
    ml = iwork[1];
    mu = iwork[2];
    if (ml < 0 || ml >= dls001_1.n) {
	goto L609;
    }
    if (mu < 0 || mu >= dls001_1.n) {
	goto L610;
    }
L30:
/* Next process and check the optional inputs. -------------------------- */
    if (*iopt == 1) {
	goto L40;
    }
    dlsa01_1.ixpr = 0;
    dls001_1.mxstep = mxstp0;
    dls001_1.mxhnil = mxhnl0;
    dls001_1.hmxi = 0.;
    dls001_1.hmin = 0.;
    if (*istate != 1) {
	goto L60;
    }
    h0 = 0.;
    dlsa01_1.mxordn = mord[0];
    dlsa01_1.mxords = mord[1];
    goto L60;
L40:
    dlsa01_1.ixpr = iwork[5];
    if (dlsa01_1.ixpr < 0 || dlsa01_1.ixpr > 1) {
	goto L611;
    }
    dls001_1.mxstep = iwork[6];
    if (dls001_1.mxstep < 0) {
	goto L612;
    }
    if (dls001_1.mxstep == 0) {
	dls001_1.mxstep = mxstp0;
    }
    dls001_1.mxhnil = iwork[7];
    if (dls001_1.mxhnil < 0) {
	goto L613;
    }
    if (dls001_1.mxhnil == 0) {
	dls001_1.mxhnil = mxhnl0;
    }
    if (*istate != 1) {
	goto L50;
    }
    h0 = rwork[5];
    dlsa01_1.mxordn = iwork[8];
    if (dlsa01_1.mxordn < 0) {
	goto L628;
    }
    if (dlsa01_1.mxordn == 0) {
	dlsa01_1.mxordn = 100;
    }
    dlsa01_1.mxordn = min(dlsa01_1.mxordn,mord[0]);
    dlsa01_1.mxords = iwork[9];
    if (dlsa01_1.mxords < 0) {
	goto L629;
    }
    if (dlsa01_1.mxords == 0) {
	dlsa01_1.mxords = 100;
    }
    dlsa01_1.mxords = min(dlsa01_1.mxords,mord[1]);
    if ((*tout - *t) * h0 < 0.) {
	goto L614;
    }
L50:
    hmax = rwork[6];
    if (hmax < 0.) {
	goto L615;
    }
    dls001_1.hmxi = 0.;
    if (hmax > 0.) {
	dls001_1.hmxi = 1. / hmax;
    }
    dls001_1.hmin = rwork[7];
    if (dls001_1.hmin < 0.) {
	goto L616;
    }
/* ----------------------------------------------------------------------- */
/* Set work array pointers and check lengths LRW and LIW. */
/* If ISTATE = 1, METH is initialized to 1 here to facilitate the */
/* checking of work space lengths. */
/* Pointers to segments of RWORK and IWORK are named by prefixing L to */
/* the name of the segment.  E.g., the segment YH starts at RWORK(LYH). */
/* Segments of RWORK (in order) are denoted  YH, WM, EWT, SAVF, ACOR. */
/* If the lengths provided are insufficient for the current method, */
/* an error return occurs.  This is treated as illegal input on the */
/* first call, but as a problem interruption with ISTATE = -7 on a */
/* continuation call.  If the lengths are sufficient for the current */
/* method but not for both methods, a warning message is sent. */
/* ----------------------------------------------------------------------- */
L60:
    if (*istate == 1) {
	dls001_1.meth = 1;
    }
    if (*istate == 1) {
	dls001_1.nyh = dls001_1.n;
    }
    dls001_1.lyh = 21;
    len1n = (dlsa01_1.mxordn + 1) * dls001_1.nyh + 20;
    len1s = (dlsa01_1.mxords + 1) * dls001_1.nyh + 20;
    dls001_1.lwm = len1s + 1;
    if (*jt <= 2) {
	lenwm = dls001_1.n * dls001_1.n + 2;
    }
    if (*jt >= 4) {
	lenwm = ((ml << 1) + mu + 1) * dls001_1.n + 2;
    }
    len1s += lenwm;
    len1c = len1n;
    if (dls001_1.meth == 2) {
	len1c = len1s;
    }
    len1 = max(len1n,len1s);
    len2 = dls001_1.n * 3;
    lenrw = len1 + len2;
    lenrwc = len1c + len2;
    iwork[17] = lenrw;
    dls001_1.liwm = 1;
    leniw = dls001_1.n + 20;
    leniwc = 20;
    if (dls001_1.meth == 2) {
	leniwc = leniw;
    }
    iwork[18] = leniw;
    if (*istate == 1 && *lrw < lenrwc) {
	goto L617;
    }
    if (*istate == 1 && *liw < leniwc) {
	goto L618;
    }
    if (*istate == 3 && *lrw < lenrwc) {
	goto L550;
    }
    if (*istate == 3 && *liw < leniwc) {
	goto L555;
    }
    dls001_1.lewt = len1 + 1;
    dlsa01_1.insufr = 0;
    if (*lrw >= lenrw) {
	goto L65;
    }
    dlsa01_1.insufr = 2;
    dls001_1.lewt = len1c + 1;
    scopy(msg, "DLSODA-  Warning.. RWORK length is sufficient for now, but  "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__103, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      may not be later.  Integration will proceed anyway.   "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__103, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      Length needed is LENRW = I1, while LRW = I2.", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__103, &cc__0, &cc__2, &lenrw, lrw, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
L65:
    dls001_1.lsavf = dls001_1.lewt + dls001_1.n;
    dls001_1.lacor = dls001_1.lsavf + dls001_1.n;
    dlsa01_1.insufi = 0;
    if (*liw >= leniw) {
	goto L70;
    }
    dlsa01_1.insufi = 2;
    scopy(msg, "DLSODA-  Warning.. IWORK length is sufficient for now, but  "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__104, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      may not be later.  Integration will proceed anyway.   "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__104, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      Length needed is LENIW = I1, while LIW = I2.", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__104, &cc__0, &cc__2, &leniw, liw, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
L70:
/* Check RTOL and ATOL for legality. ------------------------------------ */
    rtoli = rtol[1];
    atoli = atol[1];
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*itol >= 3) {
	    rtoli = rtol[i__];
	}
	if (*itol == 2 || *itol == 4) {
	    atoli = atol[i__];
	}
	if (rtoli < 0.) {
	    goto L619;
	}
	if (atoli < 0.) {
	    goto L620;
	}
/* L75: */
    }
    if (*istate == 1) {
	goto L100;
    }
/* If ISTATE = 3, set flag to signal parameter changes to DSTODA. ------- */
    dls001_1.jstart = -1;
    if (dls001_1.n == dls001_1.nyh) {
	goto L200;
    }
/* NEQ was reduced.  Zero part of YH to avoid undefined references. ----- */
    i1 = dls001_1.lyh + dls001_1.l * dls001_1.nyh;
    i2 = dls001_1.lyh + (dls001_1.maxord + 1) * dls001_1.nyh - 1;
    if (i1 > i2) {
	goto L200;
    }
    i__1 = i2;
    for (i__ = i1; i__ <= i__1; ++i__) {
/* L95: */
	rwork[i__] = 0.;
    }
    goto L200;
/* ----------------------------------------------------------------------- */
/* Block C. */
/* The next block is for the initial call only (ISTATE = 1). */
/* It contains all remaining initializations, the initial call to F, */
/* and the calculation of the initial step size. */
/* The error weights in EWT are inverted after being loaded. */
/* ----------------------------------------------------------------------- */
L100:
    dls001_1.uround = SlfNumDumach();
    dls001_1.tn = *t;
    dlsa01_1.tsw = *t;
    dls001_1.maxord = dlsa01_1.mxordn;
    if (*itask != 4 && *itask != 5) {
	goto L110;
    }
    tcrit = rwork[1];
    if ((tcrit - *tout) * (*tout - *t) < 0.) {
	goto L625;
    }
    if (h0 != 0. && (*t + h0 - tcrit) * h0 > 0.) {
	h0 = tcrit - *t;
    }
L110:
    dls001_1.jstart = 0;
    dls001_1.nhnil = 0;
    dls001_1.nst = 0;
    dls001_1.nje = 0;
    dls001_1.nslast = 0;
    dls001_1.hu = 0.;
    dls001_1.nqu = 0;
    dlsa01_1.mused = 0;
    dls001_1.miter = 0;
    dls001_1.ccmax = .3;
    dls001_1.maxcor = 3;
    dls001_1.msbp = 20;
    dls001_1.mxncf = 10;
/* Initial call to F.  (LF0 points to YH(*,2).) ------------------------- */
    lf0 = dls001_1.lyh + dls001_1.nyh;
    //(*f)(&neq[1], t, &y[1], &rwork[lf0]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(*t, &y[1], &rwork[lf0]);
#else
    ret_ok = state(neq[1], *t, &y[1], &rwork[lf0]);
#endif
    if( ret_ok != OKAY ) {
        *istate = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error SlfTntLSODA::lsoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error SlfTntLSODA::lsoda: error during state-functioncall\n");
                         
#endif
        return 0;
    }
    dls001_1.nfe = 1;
/* Load the initial value vector in YH. --------------------------------- */
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L115: */
	rwork[i__ + dls001_1.lyh - 1] = y[i__];
    }
/* Load and invert the EWT array.  (H is temporarily set to 1.0.) ------- */
    dls001_1.nq = 1;
    dls001_1.h__ = 1.;
    SlfNumDewset(dls001_1.n, *itol, &rtol[1], &atol[1], &rwork[dls001_1.lyh], &
	    rwork[dls001_1.lewt]);
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (rwork[i__ + dls001_1.lewt - 1] <= 0.) {
	    goto L621;
	}
/* L120: */
	rwork[i__ + dls001_1.lewt - 1] = 1. / rwork[i__ + dls001_1.lewt - 1];
    }
/* ----------------------------------------------------------------------- */
/* The coding below computes the step size, H0, to be attempted on the */
/* first step, unless the user has supplied a value for this. */
/* First check that TOUT - T differs significantly from zero. */
/* A scalar tolerance quantity TOL is computed, as MAX(RTOL(i)) */
/* if this is positive, or MAX(ATOL(i)/ABS(Y(i))) otherwise, adjusted */
/* so as to be between 100*UROUND and 1.0E-3. */
/* Then the computed value H0 is given by: */

/*   H0**(-2)  =  1./(TOL * w0**2)  +  TOL * (norm(F))**2 */

/* where   w0     = MAX ( ABS(T), ABS(TOUT) ), */
/*         F      = the initial value of the vector f(t,y), and */
/*         norm() = the weighted vector norm used throughout, given by */
/*                  the DMNORM function routine, and weighted by the */
/*                  tolerances initially loaded into the EWT array. */
/* The sign of H0 is inferred from the initial values of TOUT and T. */
/* ABS(H0) is made .le. ABS(TOUT-T) in any case. */
/* ----------------------------------------------------------------------- */
    if (h0 != 0.) {
	goto L180;
    }
    tdist = (d__1 = *tout - *t, abs(d__1));
/* Computing MAX */
    d__1 = abs(*t), d__2 = abs(*tout);
    w0 = max(d__1,d__2);
    if (tdist < dls001_1.uround * 2. * w0) {
	goto L622;
    }
    tol = rtol[1];
    if (*itol <= 2) {
	goto L140;
    }
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L130: */
/* Computing MAX */
	d__1 = tol, d__2 = rtol[i__];
	tol = max(d__1,d__2);
    }
L140:
    if (tol > 0.) {
	goto L160;
    }
    atoli = atol[1];
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*itol == 2 || *itol == 4) {
	    atoli = atol[i__];
	}
	ayi = (d__1 = y[i__], abs(d__1));
	if (ayi != 0.) {
/* Computing MAX */
	    d__1 = tol, d__2 = atoli / ayi;
	    tol = max(d__1,d__2);
	}
/* L150: */
    }
L160:
/* Computing MAX */
    d__1 = tol, d__2 = dls001_1.uround * 100.;
    tol = max(d__1,d__2);
    tol = min(tol,.001);
    sum = SlfNumDmaxnorm(dls001_1.n, &rwork[lf0], &rwork[dls001_1.lewt]);
/* Computing 2nd power */
    d__1 = sum;
    sum = 1. / (tol * w0 * w0) + tol * (d__1 * d__1);
    h0 = 1. / sqrt(sum);
    h0 = min(h0,tdist);
    d__1 = *tout - *t;
    h0 = fabs(h0)*SIGN(d__1);
/* Adjust H0 if necessary to meet HMAX bound. --------------------------- */
L180:
    rh = abs(h0) * dls001_1.hmxi;
    if (rh > 1.) {
	h0 /= rh;
    }
/* Load H with H0 and scale YH(*,2) by H0. ------------------------------ */
    dls001_1.h__ = h0;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L190: */
	rwork[i__ + lf0 - 1] = h0 * rwork[i__ + lf0 - 1];
    }
    goto L270;
/* ----------------------------------------------------------------------- */
/* Block D. */
/* The next code block is for continuation calls only (ISTATE = 2 or 3) */
/* and is to check stop conditions before taking a step. */
/* ----------------------------------------------------------------------- */
L200:
    dls001_1.nslast = dls001_1.nst;
    switch (*itask) {
	case 1:  goto L210;
	case 2:  goto L250;
	case 3:  goto L220;
	case 4:  goto L230;
	case 5:  goto L240;
    }
L210:
    if ((dls001_1.tn - *tout) * dls001_1.h__ < 0.) {
	goto L250;
    }
    dintdy(tout, &cc__0, &rwork[dls001_1.lyh], &dls001_1.nyh, &y[1], &iflag);
    if (iflag != 0) {
	goto L627;
    }
    *t = *tout;
    goto L420;
L220:
    tp = dls001_1.tn - dls001_1.hu * (dls001_1.uround * 100. + 1.);
    if ((tp - *tout) * dls001_1.h__ > 0.) {
	goto L623;
    }
    if ((dls001_1.tn - *tout) * dls001_1.h__ < 0.) {
	goto L250;
    }
    *t = dls001_1.tn;
    goto L400;
L230:
    tcrit = rwork[1];
    if ((dls001_1.tn - tcrit) * dls001_1.h__ > 0.) {
	goto L624;
    }
    if ((tcrit - *tout) * dls001_1.h__ < 0.) {
	goto L625;
    }
    if ((dls001_1.tn - *tout) * dls001_1.h__ < 0.) {
	goto L245;
    }
    dintdy(tout, &cc__0, &rwork[dls001_1.lyh], &dls001_1.nyh, &y[1], &iflag);
    if (iflag != 0) {
	goto L627;
    }
    *t = *tout;
    goto L420;
L240:
    tcrit = rwork[1];
    if ((dls001_1.tn - tcrit) * dls001_1.h__ > 0.) {
	goto L624;
    }
L245:
    hmx = abs(dls001_1.tn) + abs(dls001_1.h__);
    ihit = (d__1 = dls001_1.tn - tcrit, abs(d__1)) <= dls001_1.uround * 100. *
	     hmx;
    if (ihit) {
	*t = tcrit;
    }
    if (ihit) {
	goto L400;
    }
    tnext = dls001_1.tn + dls001_1.h__ * (dls001_1.uround * 4. + 1.);
    if ((tnext - tcrit) * dls001_1.h__ <= 0.) {
	goto L250;
    }
    dls001_1.h__ = (tcrit - dls001_1.tn) * (1. - dls001_1.uround * 4.);
    if (*istate == 2 && dls001_1.jstart >= 0) {
	dls001_1.jstart = -2;
    }
/* ----------------------------------------------------------------------- */
/* Block E. */
/* The next block is normally executed for all calls and contains */
/* the call to the one-step core integrator DSTODA. */

/* This is a looping point for the integration steps. */

/* First check for too many steps being taken, update EWT (if not at */
/* start of problem), check for too much accuracy being requested, and */
/* check for H below the roundoff level in T. */
/* ----------------------------------------------------------------------- */
L250:
    if (dls001_1.meth == dlsa01_1.mused) {
	goto L255;
    }
    if (dlsa01_1.insufr == 1) {
	goto L550;
    }
    if (dlsa01_1.insufi == 1) {
	goto L555;
    }
L255:
    if (dls001_1.nst - dls001_1.nslast >= dls001_1.mxstep) {
	goto L500;
    }
    SlfNumDewset(dls001_1.n, *itol, &rtol[1], &atol[1], &rwork[dls001_1.lyh], &
	    rwork[dls001_1.lewt]);
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (rwork[i__ + dls001_1.lewt - 1] <= 0.) {
	    goto L510;
	}
/* L260: */
	rwork[i__ + dls001_1.lewt - 1] = 1. / rwork[i__ + dls001_1.lewt - 1];
    }
L270:
    tolsf = dls001_1.uround * SlfNumDmaxnorm(dls001_1.n, &rwork[dls001_1.lyh], &
	    rwork[dls001_1.lewt]);
    if (tolsf <= 1.) {
	goto L280;
    }
    tolsf *= 2.;
    if (dls001_1.nst == 0) {
	goto L626;
    }
    goto L520;
L280:
    if (dls001_1.tn + dls001_1.h__ != dls001_1.tn) {
	goto L290;
    }
    ++dls001_1.nhnil;
    if (dls001_1.nhnil > dls001_1.mxhnil) {
	goto L290;
    }
    scopy(msg, "DLSODA-  Warning..Internal T (=R1) and H (=R2) are", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__101, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      such that in the machine, T + H = T on the next step  "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__101, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "     (H = step size). Solver will continue anyway.", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__101, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &
	    dls001_1.tn, &dls001_1.h__, (ftnlen)60);
    if (dls001_1.nhnil < dls001_1.mxhnil) {
	goto L290;
    }
    scopy(msg, "DLSODA-  Above warning has been issued I1 times.  ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__102, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "     It will not be issued again for this problem.", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__102, &cc__0, &cc__1, &dls001_1.mxhnil, &cc__0, &
	    cc__0, &c_b41, &c_b41, (ftnlen)60);
L290:
/* ----------------------------------------------------------------------- */
/*   CALL DSTODA(NEQ,Y,YH,NYH,YH,EWT,SAVF,ACOR,WM,IWM,F,JAC,DPRJA,DSOLSY) */
/* ----------------------------------------------------------------------- */
    dstoda(&neq[1], &y[1], &rwork[dls001_1.lyh], &dls001_1.nyh, &rwork[
	    dls001_1.lyh], &rwork[dls001_1.lewt], &rwork[dls001_1.lsavf], &
	    rwork[dls001_1.lacor], &rwork[dls001_1.lwm], &iwork[dls001_1.liwm]
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        , pclass
#else
        , state
        , jacobi
#endif
        , fjacmat
        , errtext);
    if( dls001_1.kflag == -8 || dls001_1.kflag == -9 ) {
        *istate = dls001_1.kflag;
        return 0;
    }
    kgo = 1 - dls001_1.kflag;
    switch (kgo) {
	case 1:  goto L300;
	case 2:  goto L530;
	case 3:  goto L540;
    }
/* ----------------------------------------------------------------------- */
/* Block F. */
/* The following block handles the case of a successful return from the */
/* core integrator (KFLAG = 0). */
/* If a method switch was just made, record TSW, reset MAXORD, */
/* set JSTART to -1 to signal DSTODA to complete the switch, */
/* and do extra printing of data if IXPR = 1. */
/* Then, in any case, check for stop conditions. */
/* ----------------------------------------------------------------------- */
L300:
    dls001_1.init = 1;
    if (dls001_1.meth == dlsa01_1.mused) {
	goto L310;
    }
    dlsa01_1.tsw = dls001_1.tn;
    dls001_1.maxord = dlsa01_1.mxordn;
    if (dls001_1.meth == 2) {
	dls001_1.maxord = dlsa01_1.mxords;
    }
    if (dls001_1.meth == 2) {
	rwork[dls001_1.lwm] = sqrt(dls001_1.uround);
    }
    dlsa01_1.insufr = min(dlsa01_1.insufr,1);
    dlsa01_1.insufi = min(dlsa01_1.insufi,1);
    dls001_1.jstart = -1;
    if (dlsa01_1.ixpr == 0) {
	goto L310;
    }
    if (dls001_1.meth == 2) {
	scopy(msg, "DLSODA- A switch to the BDF (stiff) method has occurred\
     ", (ftnlen)60, (ftnlen)60);
	xerrwd(msg, &cc__60, &cc__105, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &
		c_b41, &c_b41, (ftnlen)60);
    }
    if (dls001_1.meth == 1) {
	scopy(msg, "DLSODA- A switch to the Adams (nonstiff) method has occ\
urred", (ftnlen)60, (ftnlen)60);
	xerrwd(msg, &cc__60, &cc__106, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &
		c_b41, &c_b41, (ftnlen)60);
    }
    scopy(msg, "     at T = R1,  tentative step size H = R2,  step NST = I1 "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__107, &cc__0, &cc__1, &dls001_1.nst, &cc__0, &cc__2, &
	    dls001_1.tn, &dls001_1.h__, (ftnlen)60);
L310:
    switch (*itask) {
	case 1:  goto L320;
	case 2:  goto L400;
	case 3:  goto L330;
	case 4:  goto L340;
	case 5:  goto L350;
    }
/* ITASK = 1.  If TOUT has been reached, interpolate. ------------------- */
L320:
    if ((dls001_1.tn - *tout) * dls001_1.h__ < 0.) {
	goto L250;
    }
    dintdy(tout, &cc__0, &rwork[dls001_1.lyh], &dls001_1.nyh, &y[1], &iflag);
    *t = *tout;
    goto L420;
/* ITASK = 3.  Jump to exit if TOUT was reached. ------------------------ */
L330:
    if ((dls001_1.tn - *tout) * dls001_1.h__ >= 0.) {
	goto L400;
    }
    goto L250;
/* ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary. */
L340:
    if ((dls001_1.tn - *tout) * dls001_1.h__ < 0.) {
	goto L345;
    }
    dintdy(tout, &cc__0, &rwork[dls001_1.lyh], &dls001_1.nyh, &y[1], &iflag);
    *t = *tout;
    goto L420;
L345:
    hmx = abs(dls001_1.tn) + abs(dls001_1.h__);
    ihit = (d__1 = dls001_1.tn - tcrit, abs(d__1)) <= dls001_1.uround * 100. *
	     hmx;
    if (ihit) {
	goto L400;
    }
    tnext = dls001_1.tn + dls001_1.h__ * (dls001_1.uround * 4. + 1.);
    if ((tnext - tcrit) * dls001_1.h__ <= 0.) {
	goto L250;
    }
    dls001_1.h__ = (tcrit - dls001_1.tn) * (1. - dls001_1.uround * 4.);
    if (dls001_1.jstart >= 0) {
	dls001_1.jstart = -2;
    }
    goto L250;
/* ITASK = 5.  See if TCRIT was reached and jump to exit. --------------- */
L350:
    hmx = abs(dls001_1.tn) + abs(dls001_1.h__);
    ihit = (d__1 = dls001_1.tn - tcrit, abs(d__1)) <= dls001_1.uround * 100. *
	     hmx;
/* ----------------------------------------------------------------------- */
/* Block G. */
/* The following block handles all successful returns from DLSODA. */
/* If ITASK .ne. 1, Y is loaded from YH and T is set accordingly. */
/* ISTATE is set to 2, and the optional outputs are loaded into the */
/* work arrays before returning. */
/* ----------------------------------------------------------------------- */
L400:
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L410: */
	y[i__] = rwork[i__ + dls001_1.lyh - 1];
    }
    *t = dls001_1.tn;
    if (*itask != 4 && *itask != 5) {
	goto L420;
    }
    if (ihit) {
	*t = tcrit;
    }
L420:
    *istate = 2;
    rwork[11] = dls001_1.hu;
    rwork[12] = dls001_1.h__;
    rwork[13] = dls001_1.tn;
    rwork[15] = dlsa01_1.tsw;
    iwork[11] = dls001_1.nst;
    iwork[12] = dls001_1.nfe;
    iwork[13] = dls001_1.nje;
    iwork[14] = dls001_1.nqu;
    iwork[15] = dls001_1.nq;
    iwork[19] = dlsa01_1.mused;
    iwork[20] = dls001_1.meth;
    return 0;
/* ----------------------------------------------------------------------- */
/* Block H. */
/* The following block handles all unsuccessful returns other than */
/* those for illegal input.  First the error message routine is called. */
/* If there was an error test or convergence test failure, IMXER is set. */
/* Then Y is loaded from YH and T is set to TN. */
/* The optional outputs are loaded into the work arrays before returning. */
/* ----------------------------------------------------------------------- */
/* The maximum number of steps was taken before reaching TOUT. ---------- */
L500:
    scopy(msg, "DLSODA-  At current T (=R1), MXSTEP (=I1) steps   ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__201, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      taken on this call before reaching TOUT     ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__201, &cc__0, &cc__1, &dls001_1.mxstep, &cc__0, &
	    cc__1, &dls001_1.tn, &c_b41, (ftnlen)60);
    *istate = -1;
    goto L580;
/* EWT(i) .le. 0.0 for some i (not at start of problem). ---------------- */
L510:
    ewti = rwork[dls001_1.lewt + i__ - 1];
    scopy(msg, "DLSODA-  At T (=R1), EWT(I1) has become R2 .le. 0.", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__202, &cc__0, &cc__1, &i__, &cc__0, &cc__2, &
	    dls001_1.tn, &ewti, (ftnlen)60);
    *istate = -6;
    goto L580;
/* Too much accuracy requested for machine precision. ------------------- */
L520:
    scopy(msg, "DLSODA-  At T (=R1), too much accuracy requested  ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__203, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      for precision of machine..  See TOLSF (=R2) ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__203, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &
	    dls001_1.tn, &tolsf, (ftnlen)60);
    rwork[14] = tolsf;
    *istate = -2;
    goto L580;
/* KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. ----- */
L530:
    scopy(msg, "DLSODA-  At T(=R1) and step size H(=R2), the error", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__204, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      test failed repeatedly or with ABS(H) = HMIN", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__204, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &
	    dls001_1.tn, &dls001_1.h__, (ftnlen)60);
    *istate = -4;
    goto L560;
/* KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ---- */
L540:
    scopy(msg, "DLSODA-  At T (=R1) and step size H (=R2), the    ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__205, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      corrector convergence failed repeatedly     ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__205, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      or with ABS(H) = HMIN   ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__205, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &
	    dls001_1.tn, &dls001_1.h__, (ftnlen)60);
    *istate = -5;
    goto L560;
/* RWORK length too small to proceed. ----------------------------------- */
L550:
    scopy(msg, "DLSODA-  At current T(=R1), RWORK length too small", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__206, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      to proceed.  The integration was otherwise successful."
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__206, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, &
	    dls001_1.tn, &c_b41, (ftnlen)60);
    *istate = -7;
    goto L580;
/* IWORK length too small to proceed. ----------------------------------- */
L555:
    scopy(msg, "DLSODA-  At current T(=R1), IWORK length too small", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__207, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      to proceed.  The integration was otherwise successful."
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__207, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, &
	    dls001_1.tn, &c_b41, (ftnlen)60);
    *istate = -7;
    goto L580;
/* Compute IMXER if relevant. ------------------------------------------- */
L560:
    big = 0.;
    imxer = 1;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	size = (d__1 = rwork[i__ + dls001_1.lacor - 1] * rwork[i__ + 
		dls001_1.lewt - 1], abs(d__1));
	if (big >= size) {
	    goto L570;
	}
	big = size;
	imxer = i__;
L570:
	;
    }
    iwork[16] = imxer;
/* Set Y vector, T, and optional outputs. ------------------------------- */
L580:
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L590: */
	y[i__] = rwork[i__ + dls001_1.lyh - 1];
    }
    *t = dls001_1.tn;
    rwork[11] = dls001_1.hu;
    rwork[12] = dls001_1.h__;
    rwork[13] = dls001_1.tn;
    rwork[15] = dlsa01_1.tsw;
    iwork[11] = dls001_1.nst;
    iwork[12] = dls001_1.nfe;
    iwork[13] = dls001_1.nje;
    iwork[14] = dls001_1.nqu;
    iwork[15] = dls001_1.nq;
    iwork[19] = dlsa01_1.mused;
    iwork[20] = dls001_1.meth;
    return 0;
/* ----------------------------------------------------------------------- */
/* Block I. */
/* The following block handles all error returns due to illegal input */
/* (ISTATE = -3), as detected before calling the core integrator. */
/* First the error message routine is called.  If the illegal input */
/* is a negative ISTATE, the run is aborted (apparent infinite loop). */
/* ----------------------------------------------------------------------- */
L601:
    scopy(msg, "DLSODA-  ISTATE (=I1) illegal.", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__1, &cc__0, &cc__1, istate, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    if (*istate < 0) {
	goto L800;
    }
    goto L700;
L602:
    scopy(msg, "DLSODA-  ITASK (=I1) illegal. ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__2, &cc__0, &cc__1, itask, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L603:
    scopy(msg, "DLSODA-  ISTATE .gt. 1 but DLSODA not initialized.", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__3, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L604:
    scopy(msg, "DLSODA-  NEQ (=I1) .lt. 1     ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__4, &cc__0, &cc__1, &neq[1], &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L605:
    scopy(msg, "DLSODA-  ISTATE = 3 and NEQ increased (I1 to I2). ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__5, &cc__0, &cc__2, &dls001_1.n, &neq[1], &cc__0, &
	    c_b41, &c_b41, (ftnlen)60);
    goto L700;
L606:
    scopy(msg, "DLSODA-  ITOL (=I1) illegal.  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__6, &cc__0, &cc__1, itol, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L607:
    scopy(msg, "DLSODA-  IOPT (=I1) illegal.  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__7, &cc__0, &cc__1, iopt, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L608:
    scopy(msg, "DLSODA-  JT (=I1) illegal.    ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__8, &cc__0, &cc__1, jt, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L609:
    scopy(msg, "DLSODA-  ML (=I1) illegal: .lt.0 or .ge.NEQ (=I2) ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__9, &cc__0, &cc__2, &ml, &neq[1], &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L610:
    scopy(msg, "DLSODA-  MU (=I1) illegal: .lt.0 or .ge.NEQ (=I2) ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__10, &cc__0, &cc__2, &mu, &neq[1], &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L611:
    scopy(msg, "DLSODA-  IXPR (=I1) illegal.  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__11, &cc__0, &cc__1, &dlsa01_1.ixpr, &cc__0, &cc__0, &
	    c_b41, &c_b41, (ftnlen)60);
    goto L700;
L612:
    scopy(msg, "DLSODA-  MXSTEP (=I1) .lt. 0  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__12, &cc__0, &cc__1, &dls001_1.mxstep, &cc__0, &cc__0,
	     &c_b41, &c_b41, (ftnlen)60);
    goto L700;
L613:
    scopy(msg, "DLSODA-  MXHNIL (=I1) .lt. 0  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__13, &cc__0, &cc__1, &dls001_1.mxhnil, &cc__0, &cc__0,
	     &c_b41, &c_b41, (ftnlen)60);
    goto L700;
L614:
    scopy(msg, "DLSODA-  TOUT (=R1) behind T (=R2)      ", (ftnlen)60, (
	    ftnlen)40);
    xerrwd(msg, &cc__40, &cc__14, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, tout, t, (
	    ftnlen)60);
    scopy(msg, "      Integration direction is given by H0 (=R1)  ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__14, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, &h0, &
	    c_b41, (ftnlen)60);
    goto L700;
L615:
    scopy(msg, "DLSODA-  HMAX (=R1) .lt. 0.0  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__15, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, &hmax, &
	    c_b41, (ftnlen)60);
    goto L700;
L616:
    scopy(msg, "DLSODA-  HMIN (=R1) .lt. 0.0  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__16, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, &
	    dls001_1.hmin, &c_b41, (ftnlen)60);
    goto L700;
L617:
    scopy(msg, "DLSODA-  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)"
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__17, &cc__0, &cc__2, &lenrw, lrw, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L618:
    scopy(msg, "DLSODA-  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)"
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__18, &cc__0, &cc__2, &leniw, liw, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    goto L700;
L619:
    scopy(msg, "DLSODA-  RTOL(I1) is R1 .lt. 0.0        ", (ftnlen)60, (
	    ftnlen)40);
    xerrwd(msg, &cc__40, &cc__19, &cc__0, &cc__1, &i__, &cc__0, &cc__1, &rtoli, &
	    c_b41, (ftnlen)60);
    goto L700;
L620:
    scopy(msg, "DLSODA-  ATOL(I1) is R1 .lt. 0.0        ", (ftnlen)60, (
	    ftnlen)40);
    xerrwd(msg, &cc__40, &cc__20, &cc__0, &cc__1, &i__, &cc__0, &cc__1, &atoli, &
	    c_b41, (ftnlen)60);
    goto L700;
L621:
    ewti = rwork[dls001_1.lewt + i__ - 1];
    scopy(msg, "DLSODA-  EWT(I1) is R1 .le. 0.0         ", (ftnlen)60, (
	    ftnlen)40);
    xerrwd(msg, &cc__40, &cc__21, &cc__0, &cc__1, &i__, &cc__0, &cc__1, &ewti, &
	    c_b41, (ftnlen)60);
    goto L700;
L622:
    scopy(msg, "DLSODA-  TOUT(=R1) too close to T(=R2) to start integration."
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__22, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, tout, t, (
	    ftnlen)60);
    goto L700;
L623:
    scopy(msg, "DLSODA-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__23, &cc__0, &cc__1, itask, &cc__0, &cc__2, tout, &tp,
	     (ftnlen)60);
    goto L700;
L624:
    scopy(msg, "DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__24, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &tcrit, &
	    dls001_1.tn, (ftnlen)60);
    goto L700;
L625:
    scopy(msg, "DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__25, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &tcrit, 
	    tout, (ftnlen)60);
    goto L700;
L626:
    scopy(msg, "DLSODA-  At start of problem, too much accuracy   ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__26, &cc__0, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    scopy(msg, "      requested for precision of machine..  See TOLSF (=R1) "
	    , (ftnlen)60, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__26, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, &tolsf, &
	    c_b41, (ftnlen)60);
    rwork[14] = tolsf;
    goto L700;
L627:
    scopy(msg, "DLSODA-  Trouble in DINTDY.  ITASK = I1, TOUT = R1", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__27, &cc__0, &cc__1, itask, &cc__0, &cc__1, tout, &
	    c_b41, (ftnlen)60);
    goto L700;
L628:
    scopy(msg, "DLSODA-  MXORDN (=I1) .lt. 0  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__28, &cc__0, &cc__1, &dlsa01_1.mxordn, &cc__0, &cc__0,
	     &c_b41, &c_b41, (ftnlen)60);
    goto L700;
L629:
    scopy(msg, "DLSODA-  MXORDS (=I1) .lt. 0  ", (ftnlen)60, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__29, &cc__0, &cc__1, &dlsa01_1.mxords, &cc__0, &cc__0,
	     &c_b41, &c_b41, (ftnlen)60);

L700:
    *istate = -3;
    return 0;

L800:
    scopy(msg, "DLSODA-  Run aborted.. apparent infinite loop.    ", (ftnlen)
	    60, (ftnlen)50);
    xerrwd(msg, &cc__50, &cc__303, &cc__2, &cc__0, &cc__0, &cc__0, &cc__0, &c_b41, &
	    c_b41, (ftnlen)60);
    return 0;
/* ----------------------- End of Subroutine DLSODA ---------------------- */
} /* dlsoda_ */
//===========================================================================
//===========================================================================
//
// dstoda
//===========================================================================
//===========================================================================
int CSolveODE_LSODA::dstoda( integer    *neq
                        , doublereal *y
                        , doublereal *yh
                        , integer    *nyh
                        , doublereal *yh1
                        , doublereal *ewt
                        , doublereal *savf
                        , doublereal *acor
                        , doublereal *wm
                        , integer    *iwm
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
                        , CDsModBase *pclass
#else
                        , FcnEqDiff state
                        , FcnEqJac  jacobi
#endif
                        , Matrix    fjacmat
                        , CSlfStr   &errtext)
{
    /* Initialized data */

    static doublereal sm1[12] = { .5,.575,.55,.45,.35,.25,.2,.15,.1,.075,.05,
	    .025 };

    okay_t ret_ok=OKAY;

    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */

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
	    ;

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
/* dprja  = name of routine to evaluate and preprocess Jacobian matrix */
/*          and P = I - H*EL0*Jac, if a chord method is being used. */
/*          It also returns an estimate of norm(Jac) in PDNORM. */
/* dsolsy = name of routine to solve linear system in chord iteration. */
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
/*              -3  fatal error in PJAC(dprja) or SLVS(dsolsy). */
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
    dls001_2.kflag = 0;
    told = dls001_2.tn;
    ncf = 0;
    dls001_2.ierpj = 0;
    dls001_2.iersl = 0;
    dls001_2.jcur = 0;
    dls001_2.icf = 0;
    delp = 0.;
    if (dls001_2.jstart > 0) {
	goto L200;
    }
    if (dls001_2.jstart == -1) {
	goto L100;
    }
    if (dls001_2.jstart == -2) {
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
    dls001_2.lmax = dls001_2.maxord + 1;
    dls001_2.nq = 1;
    dls001_2.l = 2;
    dls001_2.ialth = 2;
    dls001_2.rmax = 1e4;
    dls001_2.rc = 0.;
    dls001_2.el0 = 1.;
    dls001_2.crate = .7;
    dls001_2.hold = dls001_2.h__;
    dls001_2.nslp = 0;
    dls001_2.ipup = dls001_2.miter;
    iret = 3;
/* Initialize switching parameters.  METH = 1 is assumed initially. ----- */
    dlsa01_2.icount = 20;
    dlsa01_2.irflag = 0;
    dlsa01_2.pdest = 0.;
    dlsa01_2.pdlast = 0.;
    dlsa01_2.ratio = 5.;
    SlfNumDcfode(cc__2, dls001_2.elco, dls001_2.tesco);
    for (i__ = 1; i__ <= 5; ++i__) {
/* L10: */
	dlsa01_2.cm2[i__ - 1] = dls001_2.tesco[i__ * 3 - 2] * dls001_2.elco[
		i__ + 1 + i__ * 13 - 14];
    }
    SlfNumDcfode(cc__1, dls001_2.elco, dls001_2.tesco);
    for (i__ = 1; i__ <= 12; ++i__) {
/* L20: */
	dlsa01_2.cm1[i__ - 1] = dls001_2.tesco[i__ * 3 - 2] * dls001_2.elco[
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
    dls001_2.ipup = dls001_2.miter;
    dls001_2.lmax = dls001_2.maxord + 1;
    if (dls001_2.ialth == 1) {
	dls001_2.ialth = 2;
    }
    if (dls001_2.meth == dlsa01_2.mused) {
	goto L160;
    }
    SlfNumDcfode(dls001_2.meth, dls001_2.elco, dls001_2.tesco);
    dls001_2.ialth = dls001_2.l;
    iret = 1;
/* ----------------------------------------------------------------------- */
/* The el vector and related constants are reset */
/* whenever the order NQ is changed, or at the start of the problem. */
/* ----------------------------------------------------------------------- */
L150:
    i__1 = dls001_2.l;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L155: */
	dls001_2.el[i__ - 1] = dls001_2.elco[i__ + dls001_2.nq * 13 - 14];
    }
    dls001_2.nqnyh = dls001_2.nq * *nyh;
    dls001_2.rc = dls001_2.rc * dls001_2.el[0] / dls001_2.el0;
    dls001_2.el0 = dls001_2.el[0];
    dls001_2.conit = .5 / (dls001_2.nq + 2);
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
    if (dls001_2.h__ == dls001_2.hold) {
	goto L200;
    }
    rh = dls001_2.h__ / dls001_2.hold;
    dls001_2.h__ = dls001_2.hold;
    iredo = 3;
    goto L175;
L170:
/* Computing MAX */
    d__1 = rh, d__2 = dls001_2.hmin / abs(dls001_2.h__);
    rh = max(d__1,d__2);
L175:
    rh = min(rh,dls001_2.rmax);
/* Computing MAX */
    d__1 = 1., d__2 = abs(dls001_2.h__) * dls001_2.hmxi * rh;
    rh /= max(d__1,d__2);
/* ----------------------------------------------------------------------- */
/* If METH = 1, also restrict the new step size by the stability region. */
/* If this reduces H, set IRFLAG to 1 so that if there are roundoff */
/* problems later, we can assume that is the cause of the trouble. */
/* ----------------------------------------------------------------------- */
    if (dls001_2.meth == 2) {
	goto L178;
    }
    dlsa01_2.irflag = 0;
/* Computing MAX */
    d__1 = abs(dls001_2.h__) * dlsa01_2.pdlast;
    pdh = max(d__1,1e-6);
    if (rh * pdh * 1.00001 < sm1[dls001_2.nq - 1]) {
	goto L178;
    }
    rh = sm1[dls001_2.nq - 1] / pdh;
    dlsa01_2.irflag = 1;
L178:
    r__ = 1.;
    i__1 = dls001_2.l;
    for (j = 2; j <= i__1; ++j) {
	r__ *= rh;
	i__2 = dls001_2.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L180: */
	    yh[i__ + j * yh_dim1] *= r__;
	}
    }
    dls001_2.h__ *= rh;
    dls001_2.rc *= rh;
    dls001_2.ialth = dls001_2.l;
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
    if ((d__1 = dls001_2.rc - 1., abs(d__1)) > dls001_2.ccmax) {
	dls001_2.ipup = dls001_2.miter;
    }
    if (dls001_2.nst >= dls001_2.nslp + dls001_2.msbp) {
	dls001_2.ipup = dls001_2.miter;
    }
    dls001_2.tn += dls001_2.h__;
    i1 = dls001_2.nqnyh + 1;
    i__2 = dls001_2.nq;
    for (jb = 1; jb <= i__2; ++jb) {
	i1 -= *nyh;
/* DIR$ IVDEP */
	i__1 = dls001_2.nqnyh;
	for (i__ = i1; i__ <= i__1; ++i__) {
/* L210: */
	    yh1[i__] += yh1[i__ + *nyh];
	}
/* L215: */
    }
    pnorm = SlfNumDmaxnorm(dls001_2.n, &yh1[1], &ewt[1]);
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
    i__2 = dls001_2.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L230: */
	y[i__] = yh[i__ + yh_dim1];
    }
    //(*f)(&neq[1], &dls001_2.tn, &y[1], &savf[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_2.tn, &y[1], &savf[1]);
#else
    ret_ok = state(neq[1], dls001_2.tn, &y[1], &savf[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_2.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error SlfTntLSODA::dstoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error SlfTntLSODA::dstoda: error during state-functioncall\n");
                         
#endif
        return 0;
    }
    ++dls001_2.nfe;
    if (dls001_2.ipup <= 0) {
	goto L250;
    }
/* ----------------------------------------------------------------------- */
/* If indicated, the matrix P = I - H*EL(1)*J is reevaluated and */
/* preprocessed before starting the corrector iteration.  IPUP is set */
/* to 0 as an indicator that this has been done. */
/* ----------------------------------------------------------------------- */
    dprja(&neq[1], &y[1], &yh[yh_offset], nyh, &ewt[1], &acor[1], &savf[1], 
	    &wm[1], &iwm[1]
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        , pclass
#else
        , state
        , jacobi
#endif
        , fjacmat
        , errtext);

    // Error state/jacobi
    if( dls001_2.kflag == -8 || dls001_2.kflag == -9 )
        return 0;
    dls001_2.ipup = 0;
    dls001_2.rc = 1.;
    dls001_2.nslp = dls001_2.nst;
    dls001_2.crate = .7;
    if (dls001_2.ierpj != 0) {
	goto L430;
    }
L250:
    i__2 = dls001_2.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L260: */
	acor[i__] = 0.;
    }
L270:
    if (dls001_2.miter != 0) {
	goto L350;
    }
/* ----------------------------------------------------------------------- */
/* In the case of functional iteration, update Y directly from */
/* the result of the last function evaluation. */
/* ----------------------------------------------------------------------- */
    i__2 = dls001_2.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	savf[i__] = dls001_2.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)];
/* L290: */
	y[i__] = savf[i__] - acor[i__];
    }
    del = SlfNumDmaxnorm(dls001_2.n, &y[1], &ewt[1]);
    i__2 = dls001_2.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	y[i__] = yh[i__ + yh_dim1] + dls001_2.el[0] * savf[i__];
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
    i__2 = dls001_2.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L360: */
	y[i__] = dls001_2.h__ * savf[i__] - (yh[i__ + (yh_dim1 << 1)] + acor[
		i__]);
    }
    dsolsy(&wm[1], &iwm[1], &y[1], &savf[1]);
    if (dls001_2.iersl < 0) {
	goto L430;
    }
    if (dls001_2.iersl > 0) {
	goto L410;
    }
    del = SlfNumDmaxnorm(dls001_2.n, &y[1], &ewt[1]);
    i__2 = dls001_2.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	acor[i__] += y[i__];
/* L380: */
	y[i__] = yh[i__ + yh_dim1] + dls001_2.el[0] * acor[i__];
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
    if (del <= pnorm * 100. * dls001_2.uround) {
	goto L450;
    }
    if (m == 0 && dls001_2.meth == 1) {
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
    d__1 = dls001_2.crate * .2;
    dls001_2.crate = max(d__1,rm);
L402:
/* Computing MIN */
    d__1 = 1., d__2 = dls001_2.crate * 1.5;
    dcon = del * min(d__1,d__2) / (dls001_2.tesco[dls001_2.nq * 3 - 2] * 
	    dls001_2.conit);
    if (dcon > 1.) {
	goto L405;
    }
/* Computing MAX */
    d__2 = dlsa01_2.pdest, d__3 = rate / (d__1 = dls001_2.h__ * dls001_2.el[0]
	    , abs(d__1));
    dlsa01_2.pdest = max(d__2,d__3);
    if (dlsa01_2.pdest != 0.) {
	dlsa01_2.pdlast = dlsa01_2.pdest;
    }
    goto L450;
L405:
    ++m;
    if (m == dls001_2.maxcor) {
	goto L410;
    }
    if (m >= 2 && del > delp * 2.) {
	goto L410;
    }
    delp = del;
    //(*f)(&neq[1], &dls001_2.tn, &y[1], &savf[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_2.tn, &y[1], &savf[1]);
#else
    ret_ok = state(neq[1], dls001_2.tn, &y[1], &savf[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_2.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error SlfTntLSODA::dstoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error SlfTntLSODA::dstoda: error during state-functioncall\n");
                         
#endif
        return 0;
    }
    ++dls001_2.nfe;
    goto L270;
/* ----------------------------------------------------------------------- */
/* The corrector iteration failed to converge. */
/* If MITER .ne. 0 and the Jacobian is out of date, PJAC is called for */
/* the next try.  Otherwise the YH array is retracted to its values */
/* before prediction, and H is reduced, if possible.  If H cannot be */
/* reduced or MXNCF failures have occurred, exit with KFLAG = -2. */
/* ----------------------------------------------------------------------- */
L410:
    if (dls001_2.miter == 0 || dls001_2.jcur == 1) {
	goto L430;
    }
    dls001_2.icf = 1;
    dls001_2.ipup = dls001_2.miter;
    goto L220;
L430:
    dls001_2.icf = 2;
    ++ncf;
    dls001_2.rmax = 2.;
    dls001_2.tn = told;
    i1 = dls001_2.nqnyh + 1;
    i__2 = dls001_2.nq;
    for (jb = 1; jb <= i__2; ++jb) {
	i1 -= *nyh;
/* DIR$ IVDEP */
	i__1 = dls001_2.nqnyh;
	for (i__ = i1; i__ <= i__1; ++i__) {
/* L440: */
	    yh1[i__] -= yh1[i__ + *nyh];
	}
/* L445: */
    }
    if (dls001_2.ierpj < 0 || dls001_2.iersl < 0) {
	goto L680;
    }
    if (abs(dls001_2.h__) <= dls001_2.hmin * 1.00001) {
	goto L670;
    }
    if (ncf == dls001_2.mxncf) {
	goto L670;
    }
    rh = .25;
    dls001_2.ipup = dls001_2.miter;
    iredo = 1;
    goto L170;
/* ----------------------------------------------------------------------- */
/* The corrector has converged.  JCUR is set to 0 */
/* to signal that the Jacobian involved may need updating later. */
/* The local error test is made and control passes to statement 500 */
/* if it fails. */
/* ----------------------------------------------------------------------- */
L450:
    dls001_2.jcur = 0;
    if (m == 0) {
	dsm = del / dls001_2.tesco[dls001_2.nq * 3 - 2];
    }
    if (m > 0) {
	dsm = SlfNumDmaxnorm(dls001_2.n, &acor[1], &ewt[1]) / dls001_2.tesco[
		dls001_2.nq * 3 - 2];
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
    dls001_2.kflag = 0;
    iredo = 0;
    ++dls001_2.nst;
    dls001_2.hu = dls001_2.h__;
    dls001_2.nqu = dls001_2.nq;
    dlsa01_2.mused = dls001_2.meth;
    i__2 = dls001_2.l;
    for (j = 1; j <= i__2; ++j) {
	i__1 = dls001_2.n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L460: */
	    yh[i__ + j * yh_dim1] += dls001_2.el[j - 1] * acor[i__];
	}
    }
    --dlsa01_2.icount;
    if (dlsa01_2.icount >= 0) {
	goto L488;
    }
    if (dls001_2.meth == 2) {
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
    if (dls001_2.nq > 5) {
	goto L488;
    }
    if (dsm > pnorm * 100. * dls001_2.uround && dlsa01_2.pdest != 0.) {
	goto L470;
    }
    if (dlsa01_2.irflag == 0) {
	goto L488;
    }
    rh2 = 2.;
    nqm2 = min(dls001_2.nq,dlsa01_2.mxords);
    goto L478;
L470:
    exsm = 1. / dls001_2.l;
    rh1 = 1. / (pow(dsm, exsm) * 1.2 + 1.2e-6);
    rh1it = rh1 * 2.;
    pdh = dlsa01_2.pdlast * abs(dls001_2.h__);
    if (pdh * rh1 > 1e-5) {
	rh1it = sm1[dls001_2.nq - 1] / pdh;
    }
    rh1 = min(rh1,rh1it);
    if (dls001_2.nq <= dlsa01_2.mxords) {
	goto L474;
    }
    nqm2 = dlsa01_2.mxords;
    lm2 = dlsa01_2.mxords + 1;
    exm2 = 1. / lm2;
    lm2p1 = lm2 + 1;
    dm2 = SlfNumDmaxnorm(dls001_2.n, &yh[lm2p1 * yh_dim1 + 1], &ewt[1]) / 
	    dlsa01_2.cm2[dlsa01_2.mxords - 1];
    rh2 = 1. / (pow(dm2, exm2) * 1.2 + 1.2e-6);
    goto L476;
L474:
    dm2 = dsm * (dlsa01_2.cm1[dls001_2.nq - 1] / dlsa01_2.cm2[dls001_2.nq - 1]
	    );
    rh2 = 1. / (pow(dm2, exsm) * 1.2 + 1.2e-6);
    nqm2 = dls001_2.nq;
L476:
    if (rh2 < dlsa01_2.ratio * rh1) {
	goto L488;
    }
/* THE SWITCH TEST PASSED.  RESET RELEVANT QUANTITIES FOR BDF. ---------- */
L478:
    rh = rh2;
    dlsa01_2.icount = 20;
    dls001_2.meth = 2;
    dls001_2.miter = dlsa01_2.jtyp;
    dlsa01_2.pdlast = 0.;
    dls001_2.nq = nqm2;
    dls001_2.l = dls001_2.nq + 1;
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
    exsm = 1. / dls001_2.l;
    if (dlsa01_2.mxordn >= dls001_2.nq) {
	goto L484;
    }
    nqm1 = dlsa01_2.mxordn;
    lm1 = dlsa01_2.mxordn + 1;
    exm1 = 1. / lm1;
    lm1p1 = lm1 + 1;
    dm1 = SlfNumDmaxnorm(dls001_2.n, &yh[lm1p1 * yh_dim1 + 1], &ewt[1]) / 
	    dlsa01_2.cm1[dlsa01_2.mxordn - 1];
    rh1 = 1. / (pow(dm1, exm1) * 1.2 + 1.2e-6);
    goto L486;
L484:
    dm1 = dsm * (dlsa01_2.cm2[dls001_2.nq - 1] / dlsa01_2.cm1[dls001_2.nq - 1]
	    );
    rh1 = 1. / (pow(dm1, exsm) * 1.2 + 1.2e-6);
    nqm1 = dls001_2.nq;
    exm1 = exsm;
L486:
    rh1it = rh1 * 2.;
    pdh = dlsa01_2.pdnorm * abs(dls001_2.h__);
    if (pdh * rh1 > 1e-5) {
	rh1it = sm1[nqm1 - 1] / pdh;
    }
    rh1 = min(rh1,rh1it);
    rh2 = 1. / (pow(dsm, exsm) * 1.2 + 1.2e-6);
    if (rh1 * dlsa01_2.ratio < rh2 * 5.) {
	goto L488;
    }
    alpha = max(.001,rh1);
    dm1 = pow(alpha, exm1) * dm1;
    if (dm1 <= dls001_2.uround * 1e3 * pnorm) {
	goto L488;
    }
/* The switch test passed.  Reset relevant quantities for Adams. -------- */
    rh = rh1;
    dlsa01_2.icount = 20;
    dls001_2.meth = 1;
    dls001_2.miter = 0;
    dlsa01_2.pdlast = 0.;
    dls001_2.nq = nqm1;
    dls001_2.l = dls001_2.nq + 1;
    goto L170;

/* No method switch is being made.  Do the usual step/order selection. -- */
L488:
    --dls001_2.ialth;
    if (dls001_2.ialth == 0) {
	goto L520;
    }
    if (dls001_2.ialth > 1) {
	goto L700;
    }
    if (dls001_2.l == dls001_2.lmax) {
	goto L700;
    }
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L490: */
	yh[i__ + dls001_2.lmax * yh_dim1] = acor[i__];
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
    --dls001_2.kflag;
    dls001_2.tn = told;
    i1 = dls001_2.nqnyh + 1;
    i__1 = dls001_2.nq;
    for (jb = 1; jb <= i__1; ++jb) {
	i1 -= *nyh;
/* DIR$ IVDEP */
	i__2 = dls001_2.nqnyh;
	for (i__ = i1; i__ <= i__2; ++i__) {
/* L510: */
	    yh1[i__] -= yh1[i__ + *nyh];
	}
/* L515: */
    }
    dls001_2.rmax = 2.;
    if (abs(dls001_2.h__) <= dls001_2.hmin * 1.00001) {
	goto L660;
    }
    if (dls001_2.kflag <= -3) {
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
    if (dls001_2.l == dls001_2.lmax) {
	goto L540;
    }
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L530: */
	savf[i__] = acor[i__] - yh[i__ + dls001_2.lmax * yh_dim1];
    }
    dup = SlfNumDmaxnorm(dls001_2.n, &savf[1], &ewt[1]) / dls001_2.tesco[
	    dls001_2.nq * 3 - 1];
    exup = 1. / (dls001_2.l + 1);
    rhup = 1. / (pow(dup, exup) * 1.4 + 1.4e-6);
L540:
    exsm = 1. / dls001_2.l;
    rhsm = 1. / (pow(dsm, exsm) * 1.2 + 1.2e-6);
    rhdn = 0.;
    if (dls001_2.nq == 1) {
	goto L550;
    }
    ddn = SlfNumDmaxnorm(dls001_2.n, &yh[dls001_2.l * yh_dim1 + 1], &ewt[1]) / 
	    dls001_2.tesco[dls001_2.nq * 3 - 3];
    exdn = 1. / dls001_2.nq;
    rhdn = 1. / (pow(ddn, exdn) * 1.3 + 1.3e-6);
/* If METH = 1, limit RH according to the stability region also. -------- */
L550:
    if (dls001_2.meth == 2) {
	goto L560;
    }
/* Computing MAX */
    d__1 = abs(dls001_2.h__) * dlsa01_2.pdlast;
    pdh = max(d__1,1e-6);
    if (dls001_2.l < dls001_2.lmax) {
/* Computing MIN */
	d__1 = rhup, d__2 = sm1[dls001_2.l - 1] / pdh;
	rhup = min(d__1,d__2);
    }
/* Computing MIN */
    d__1 = rhsm, d__2 = sm1[dls001_2.nq - 1] / pdh;
    rhsm = min(d__1,d__2);
    if (dls001_2.nq > 1) {
/* Computing MIN */
	d__1 = rhdn, d__2 = sm1[dls001_2.nq - 2] / pdh;
	rhdn = min(d__1,d__2);
    }
    dlsa01_2.pdest = 0.;
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
    newq = dls001_2.nq;
    rh = rhsm;
    goto L620;
L580:
    newq = dls001_2.nq - 1;
    rh = rhdn;
    if (dls001_2.kflag < 0 && rh > 1.) {
	rh = 1.;
    }
    goto L620;
L590:
    newq = dls001_2.l;
    rh = rhup;
    if (rh < 1.1) {
	goto L610;
    }
    r__ = dls001_2.el[dls001_2.l - 1] / dls001_2.l;
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L600: */
	yh[i__ + (newq + 1) * yh_dim1] = acor[i__] * r__;
    }
    goto L630;
L610:
    dls001_2.ialth = 3;
    goto L700;
/* If METH = 1 and H is restricted by stability, bypass 10 percent test. */
L620:
    if (dls001_2.meth == 2) {
	goto L622;
    }
    if (rh * pdh * 1.00001 >= sm1[newq - 1]) {
	goto L625;
    }
L622:
    if (dls001_2.kflag == 0 && rh < 1.1) {
	goto L610;
    }
L625:
    if (dls001_2.kflag <= -2) {
	rh = min(rh,.2);
    }
/* ----------------------------------------------------------------------- */
/* If there is a change of order, reset NQ, L, and the coefficients. */
/* In any case H is reset according to RH and the YH array is rescaled. */
/* Then exit from 690 if the step was OK, or redo the step otherwise. */
/* ----------------------------------------------------------------------- */
    if (newq == dls001_2.nq) {
	goto L170;
    }
L630:
    dls001_2.nq = newq;
    dls001_2.l = dls001_2.nq + 1;
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
    if (dls001_2.kflag == -10) {
	goto L660;
    }
    rh = .1;
/* Computing MAX */
    d__1 = dls001_2.hmin / abs(dls001_2.h__);
    rh = max(d__1,rh);
    dls001_2.h__ *= rh;
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L645: */
	y[i__] = yh[i__ + yh_dim1];
    }
    //(*f)(&neq[1], &dls001_2.tn, &y[1], &savf[1]);
    // State-Funktions-Aufruf
    //=======================
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->state(dls001_2.tn, &y[1], &savf[1]);
#else
    ret_ok = state(neq[1], dls001_2.tn, &y[1], &savf[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_2.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error SlfTntLSODA::dstoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error SlfTntLSODA::dstoda: error during state-functioncall\n");
                         
#endif
        return 0;
    }
    ++dls001_2.nfe;
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L650: */
	yh[i__ + (yh_dim1 << 1)] = dls001_2.h__ * savf[i__];
    }
    dls001_2.ipup = dls001_2.miter;
    dls001_2.ialth = 5;
    if (dls001_2.nq == 1) {
	goto L200;
    }
    dls001_2.nq = 1;
    dls001_2.l = 2;
    iret = 3;
    goto L150;
/* ----------------------------------------------------------------------- */
/* All returns are made through this section.  H is saved in HOLD */
/* to allow the caller to change H on the next step. */
/* ----------------------------------------------------------------------- */
L660:
    dls001_2.kflag = -1;
    goto L720;
L670:
    dls001_2.kflag = -2;
    goto L720;
L680:
    dls001_2.kflag = -3;
    goto L720;
L690:
    dls001_2.rmax = 10.;
L700:
    r__ = 1. / dls001_2.tesco[dls001_2.nqu * 3 - 2];
    i__1 = dls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L710: */
	acor[i__] *= r__;
    }
L720:
    dls001_2.hold = dls001_2.h__;
    dls001_2.jstart = 1;
    return 0;
/* ----------------------- End of Subroutine DSTODA ---------------------- */
} /* dstoda_ */
//===========================================================================
//===========================================================================
//
// dprja
//
//===========================================================================
//===========================================================================
    int CSolveODE_LSODA::dprja( integer    *neq
                           , doublereal *y
                           , doublereal *yh
                           , integer    *nyh
                           , doublereal *ewt
                           , doublereal *ftem
                           , doublereal *savf
                           , doublereal *wm
                           , integer    *iwm
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
                           , CDsModBase *pclass
#else
                           , FcnEqDiff state
                           , FcnEqJac  jacobi
#endif
                           , Matrix    fjacmat
                           , CSlfStr   &errtext)
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
    static integer mband, meband;

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
    //(*jac)(&neq[1], &dls001_1.tn, &y[1], &cc__0, &cc__0, &wm[3], &dls001_1.n);
    // Jacobi-Aufruf
    //==============
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->jacobi(dls001_1.tn, &y[1], fjacmat, dls001_1.n);
#else
    ret_ok = jacobi(neq[1], dls001_1.tn, &y[1], fjacmat, dls001_1.n);
#endif
    if( ret_ok != OKAY ) {
        dls001_2.kflag  = -9;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error CSolveODE_LSODA::dprja: during jacobi-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error CSolveODE_LSODA::dprja: error during jacobi-functioncall\n");
                 
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
    //printf("dprja:%g|wm[%i]=%g\n",dls001_1.tn,i__ + 2,wm[i__ + 2]);
	wm[i__ + 2] *= con;
    }
    goto L240;
/* If MITER = 2, make N calls to F to approximate J. -------------------- */
L200:
    fac = SlfNumDmaxnorm(dls001_1.n, &savf[1], &ewt[1]);
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
        dls001_2.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error SlfTntLSODA::dstoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error SlfTntLSODA::dstoda: error during state-functioncall\n");
                         
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
    dlsa01_1.pdnorm = SlfNumDfnorm(dls001_1.n, &wm[3], &ewt[1]) / abs(hl0);
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
    SlfNumDgefa(&wm[3], dls001_1.n, dls001_1.n, &iwm[21], &ier);
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
    // Jacobi-Aufruf
    //==============
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
    ret_ok = pclass->jacobi(dls001_1.tn, &y[1], fjacmat, meband);
#else
    ret_ok = jacobi(neq[1], dls001_1.tn, &y[1], fjacmat, meband);
#endif
    if( ret_ok != OKAY ) {
        dls001_2.kflag  = -9;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error CSolveODE_LSODA::dprja: during jacobi-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error CSolveODE_LSODA::dprja: error during jacobi-functioncall\n");
                 
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
    fac = SlfNumDmaxnorm(dls001_1.n, &savf[1], &ewt[1]);
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
    ret_ok = state(neq[1], dls001_1.tn, &y[1], &ftem[1]);
#endif
    if( ret_ok != OKAY ) {
        dls001_1.kflag  = -8;
#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        errtext.catFormat("Error SlfTntLSODA::dstoda: during state-functioncall of <%s> \n%s\n"
                         ,pclass->getName()
                         ,pclass->getErrText()
                         );
#else
        errtext.cat("Error SlfTntLSODA::dstoda: error during state-functioncall\n");
                         
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
    dlsa01_1.pdnorm = SlfNumDbnorm(dls001_1.n, &wm[ml + 3], meband, ml, mu, &
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
    SlfNumDgbfa(&wm[3], meband, dls001_1.n, ml, mu, &iwm[21], &ier);
    if (ier != 0) {
	dls001_1.ierpj = 1;
    }
    return 0;
/* ----------------------- End of Subroutine DPRJA ----------------------- */
} /* dprja_ */
//===========================================================================
//===========================================================================
//
// dolsy
//
//===========================================================================
//===========================================================================
int CSolveODE_LSODA::dsolsy( doublereal *wm
                        , integer    *iwm
                        , doublereal *x
                        , doublereal *tem)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static doublereal r__, di;
    static integer ml, mu;
    static doublereal hl0, phl0;
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
    dls001_1.iersl = 0;
    switch (dls001_1.miter) {
	case 1:  goto L100;
	case 2:  goto L100;
	case 3:  goto L300;
	case 4:  goto L400;
	case 5:  goto L400;
    }
L100:
    SlfNumDgesl(&wm[3], dls001_1.n, dls001_1.n, &iwm[21], &x[1], 0);
    return 0;

L300:
    phl0 = wm[2];
    hl0 = dls001_1.h__ * dls001_1.el0;
    wm[2] = hl0;
    if (hl0 == phl0) {
	goto L330;
    }
    r__ = hl0 / phl0;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	di = 1. - r__ * (1. - 1. / wm[i__ + 2]);
	if (abs(di) == 0.) {
	    goto L390;
	}
/* L320: */
	wm[i__ + 2] = 1. / di;
    }
L330:
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L340: */
	x[i__] = wm[i__ + 2] * x[i__];
    }
    return 0;
L390:
    dls001_1.iersl = 1;
    return 0;

L400:
    ml = iwm[1];
    mu = iwm[2];
    meband = (ml << 1) + mu + 1;
    SlfNumDgbsl(&wm[3], meband, dls001_1.n, ml, mu, &iwm[21], &x[1], 0);
    return 0;
/* ----------------------- END OF SUBROUTINE DSOLSY ---------------------- */
} /* dsolsy_ */
//===========================================================================
//===========================================================================
//
// dolsy
//
//===========================================================================
//===========================================================================
int CSolveODE_LSODA::dintdy( doublereal *t
                        , integer    *k
                        , doublereal *yh
                        , integer    *nyh
                        , doublereal *dky
                        , integer *iflag)
{
    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2;

    /* Builtin functions */

    /* Local variables */
    static doublereal cc__;
    static integer i__, j;
    static doublereal r__, s;
    static integer ic, jb, jj;
    static doublereal tp;
    static integer jb2, jj1, jp1;
    static char msg[80];

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
    if (*k < 0 || *k > dls001_1.nq) {
	goto L80;
    }
    tp = dls001_1.tn - dls001_1.hu - dls001_1.uround * 100. * (dls001_1.tn + 
	    dls001_1.hu);
    if ((*t - tp) * (*t - dls001_1.tn) > 0.) {
	goto L90;
    }

    s = (*t - dls001_1.tn) / dls001_1.h__;
    ic = 1;
    if (*k == 0) {
	goto L15;
    }
    jj1 = dls001_1.l - *k;
    i__1 = dls001_1.nq;
    for (jj = jj1; jj <= i__1; ++jj) {
/* L10: */
	ic *= jj;
    }
L15:
    cc__ = (doublereal) ic;
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L20: */
	dky[i__] = cc__ * yh[i__ + dls001_1.l * yh_dim1];
    }
    if (*k == dls001_1.nq) {
	goto L55;
    }
    jb2 = dls001_1.nq - *k;
    i__1 = jb2;
    for (jb = 1; jb <= i__1; ++jb) {
	j = dls001_1.nq - jb;
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
	cc__ = (doublereal) ic;
	i__2 = dls001_1.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L40: */
	    dky[i__] = cc__ * yh[i__ + jp1 * yh_dim1] + s * dky[i__];
	}
/* L50: */
    }
    if (*k == 0) {
	return 0;
    }
L55:
    i__1 = -(*k);
    r__ = SlfNumPowDi(dls001_1.h__, i__1);
    i__1 = dls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L60: */
	dky[i__] = r__ * dky[i__];
    }
    return 0;

L80:
    scopy(msg, "DINTDY-  K (=I1) illegal      ", (ftnlen)80, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__51, &cc__0, &cc__1, k, &cc__0, &cc__0, &c_b20, &
	    c_b20, (ftnlen)80);
    *iflag = -1;
    return 0;
L90:
    scopy(msg, "DINTDY-  T (=R1) illegal      ", (ftnlen)80, (ftnlen)30);
    xerrwd(msg, &cc__30, &cc__52, &cc__0, &cc__0, &cc__0, &cc__0, &cc__1, t, &c_b20,
	     (ftnlen)80);
    scopy(msg, "      T not in interval TCUR - HU (= R1) to TCUR (=R2)      "
	    , (ftnlen)80, (ftnlen)60);
    xerrwd(msg, &cc__60, &cc__52, &cc__0, &cc__0, &cc__0, &cc__0, &cc__2, &tp, &
	    dls001_1.tn, (ftnlen)80);
    *iflag = -2;
    return 0;
/* ----------------------- END OF SUBROUTINE DINTDY ---------------------- */
} /* dintdy_ */
int CSolveODE_LSODA::xerrwd( char       *msg
                        , integer    *nmes
                        , integer    *nerr
                        , integer    *level
                        , integer    *ni
                        , integer    *i1
                        , integer    *i2
                        , integer    *nr
                        , doublereal *r1
                        , doublereal *r2
                        , ftnlen msg_len)
{


/* ***BEGIN PROLOGUE  XERRWD */
/* ***SUBSIDIARY */
/* ***PURPOSE  Write error message with values. */
/* ***CATEGORY  R3C */
/* ***TYPE      DOUBLE PRECISION (XERRWV-S, XERRWD-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV, */
/*  as given here, constitute a simplified version of the SLATEC error */
/*  handling package. */

/*  All arguments are input arguments. */

/*  MSG    = The message (character array). */
/*  NMES   = The length of MSG (number of characters). */
/*  NERR   = The error number (not used). */
/*  LEVEL  = The error level.. */
/*           0 or 1 means recoverable (control returns to caller). */
/*           2 means fatal (run is aborted--see note below). */
/*  NI     = Number of integers (0, 1, or 2) to be printed with message. */
/*  I1,I2  = Integers to be printed, depending on NI. */
/*  NR     = Number of reals (0, 1, or 2) to be printed with message. */
/*  R1,R2  = Reals to be printed, depending on NR. */

/*  Note..  this routine is machine-dependent and specialized for use */
/*  in limited context, in the following ways.. */
/*  1. The argument MSG is assumed to be of type CHARACTER, and */
/*     the message is printed with a format of (1X,A). */
/*  2. The message is assumed to take only one line. */
/*     Multi-line messages are generated by repeated calls. */
/*  3. If LEVEL = 2, control passes to the statement   STOP */
/*     to abort the run.  This statement may be machine-dependent. */
/*  4. R1 and R2 are assumed to be in double precision and are printed */
/*     in D21.13 format. */

/* ***ROUTINES CALLED  IXSAV */
/* ***REVISION HISTORY  (YYMMDD) */
/*   920831  DATE WRITTEN */
/*   921118  Replaced MFLGSV/LUNSAV by IXSAV. (ACH) */
/*   930329  Modified prologue to SLATEC format. (FNF) */
/*   930407  Changed MSG from CHARACTER*1 array to variable. (FNF) */
/*   930922  Minor cosmetic change. (FNF) */
/* ***END PROLOGUE  XERRWD */

/* *Internal Notes: */

/* For a different default logical unit number, IXSAV (or a subsidiary */
/* routine that it calls) will need to be modified. */
/* For a different run-abort command, change the statement following */
/* statement 100 at the end. */
/* ----------------------------------------------------------------------- */
/* Subroutines called by XERRWD.. None */
/* Function routine called by XERRWD.. IXSAV */
/* ----------------------------------------------------------------------- */
/* **End */

/*  Declare arguments. */


/*  Declare local variables. */


/*  Get logical unit number and message print flag. */

/* ***FIRST EXECUTABLE STATEMENT  XERRWD */

/*  Write the message. */

    printf("%s\n",msg);
    if( *ni  == 1 )
        printf("(6x,\002In above message,  I1 =\002,i10)",*i1);
    if( *ni == 2 )
        printf("(6x,\002In above message,  I1 =\002,i10,3x,\002I\2 =\002,i10)",i1,i2);
    if( *nr == 1 )
        printf("(6x,\002In above message,  R1 =\002,d21.13)",*r1);
    if( *nr == 2 )
        printf("(6x,\002In above,  R1 =\002,d21.13,3x,\002R2 \2 =\002,d21.13)",*r1,*r2);


/*  Abort the run if LEVEL = 2. */
/* ----------------------- End of Subroutine XERRWD ---------------------- */
    if( *level != 2 )
        return 0;
    else
        exit(1);

    return 0;
} /* xerrwd_ */
void CSolveODE_LSODA::scopy(register char *a, register char *b, int32_t la, int32_t lb)
{
	register char *aend, *bend;

	aend = a + la;

	if(la <= lb)
		while(a < aend)
			*a++ = *b++;
	else {
		bend = b + lb;
		while(b < bend)
			*a++ = *b++;
		while(a < aend)
			*a++ = ' ';
    }
}
}