// DEF_LSODA      livermore solver ordinary diffequ atomatic method switching stiff/nonstiff
//
#ifndef SLF_SOLVER_ODE_LSODA_H_INCLUDED
#define SLF_SOLVER_ODE_LSODA_H_INCLUDED
namespace slf
{
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
  /*         =Rwork[10] */
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
  /*         Iwork[10] */
  /* NFE     IWORK(12) the number of f evaluations for the problem so far. */
  /*         Iwork[11] */
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
  class CSolveODE_LSODA : public CSolveODEBase {
  public:

      CSolveODE_LSODA(SIntegratorBaseInp *pinp);
      ~CSolveODE_LSODA();


      okay_t init(SIntegratorBaseInp *pinp);
      okay_t calcFirst(double x       // Startzeit, x-Wert
                       ,CVectorD ystart  // Startzust?nde
                       ,double hstart  // Startschrittweite (kann 0.0 sein)
                       ,double posneg=1. // Zeitachse/x-Achse positiv (1.) oder negativ (-1.)
                       );
      okay_t calc(double xend);     // Rechne bis xend
    
      void reset(void);               // L?schen von Speicher und zur?cksetzen der Variablen

      EErrNo get_ierr(void){return Ierr;}
  private:


      EErrNo Ierr;

      int32_t  N;
      int32_t  Itoler;
      int32_t  Itask;
      int32_t  Istate;
      int32_t  Iopt;
      int32_t  Ijac;
      int32_t  Mljac;
      int32_t  Mujac;
      int32_t  Jt;
      int32_t  Mxordn;
      int32_t  Mxords;
      int32_t  Liw;
      int32_t  Lrw;

      int32_t  *Iwork;
      double  *Rwork;

      double    Hmin;
      CMatrixD  FjacMat;

      uint32_t Naccpt,Nrejct,Ndec,Nsol;

      UIntegratorLSODA_1 Dsl1;
      UIntegratorLSODA_a Dsla;

      // Aufruf Integrator
      //==================
      int dlsoda( int32_t    *neq
  #if SLF_SOLVER_ODE_USE_DSMODBASE == 1
                , CModelBase *pclass
  #else
                , FcnEqDiff state
                , FcnEqJac  jacobi
  #endif
                , CMatrixD    fjacmat
                , CStr        &errtext
                , double *y
                , double *t
                , double *tout
                , int32_t    *itol
                , double *rtol
                , double *atol
                , int32_t    *itask
                , int32_t    *istate
                , int32_t    *iopt
                , double *rwork
                , int32_t    *lrw
                , int32_t    *iwork
                , int32_t    *liw
                , int32_t    *jt);

      int dstoda( int32_t    *neq
                , double *y
                , double *yh
                , int32_t    *nyh
                , double *yh1
                , double *ewt
                , double *savf
                , double *acor
                , double *wm
                , int32_t    *iwm
  #if SLF_SOLVER_ODE_USE_DSMODBASE == 1
                , CModelBase *pclass
  #else
                , FcnEqDiff state
                , FcnEqJac  jacobi
  #endif
                , CMatrixD fjacmat
                , CStr     &errtext);

      int dsolsy( double *wm
                , int32_t    *iwm
                , double *x
                , double *tem);

      int dprja( int32_t    *neq
               , double *y
               , double *yh
               , int32_t    *nyh
               , double *ewt
               , double *ftem
               , double *savf
               , double *wm
               , int32_t    *iwm
  #if SLF_SOLVER_ODE_USE_DSMODBASE == 1
               , CModelBase *pclass
  #else
               , FcnEqDiff state
               , FcnEqJac  jacobi
  #endif
               , CMatrixD  fjacmat
               , CStr      &errtext);

      int dintdy( double *t
                , int32_t    *k
                , double *yh
                , int32_t    *nyh
                , double *dky
                , int32_t *iflag);

      int xerrwd( char      *msg
                , int32_t    *nmes
                , int32_t    *nerr
                , int32_t    *level
                , int32_t    *ni
                , int32_t    *i1
                , int32_t    *i2
                , int32_t    *nr
                , double    *r1
                , double    *r2
                , int32_t    msg_len);
      void scopy( register char *a
                , register char *b
                , int32_t la
                , int32_t lb);


  };

}
#endif