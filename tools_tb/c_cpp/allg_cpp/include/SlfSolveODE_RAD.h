// DEF_RADAU      impliziter Runge-Kutta, Radau
//
#ifndef SLF_SOLVER_ODE_RAD_H_INCLUDED
#define SLF_SOLVER_ODE_RAD_H_INCLUDED

#include "SlfSolveODE.h"
#include "SlfSolveODE_RAD_int.h"

namespace slf
{

  /* implizites Runge-Kutta-Verfahren nach Radau
     ===========================================
  This code computes the numerical solution of a stiff (or differential algebraic)
  system of first order ordinary differential equations

                           M*y'=f(x,y).

  The system can be (linearly) implicit (mass-matrix M != I) or explicit (M = I).
  the method used is an implicit Runge-Kutta method (RADAU IIA) of order 5 with
  step size control and continuous output. (See Section IV.8 of Hairer and Wanner).

  C     AUTHORS: E. HAIRER AND G. WANNER
  C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
  C              CH-1211 GENEVE 24, SWITZERLAND 
  C              E-MAIL:  Ernst.Hairer@math.unige.ch
  C                       Gerhard.Wanner@math.unige.ch
  C     
  C     FOR A DESCRIPTION OF THE RELATED CODE RADAU5 SEE THE BOOK:
  C         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
  C         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
  C         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 14,
  C         SPRINGER-VERLAG 1991, SECOND EDITION 1996.
  C      
  C     PRELIMINARY VERSION OF APRIL 23, 1998
  C     (latest small correction: January 18, 2002)

  Input Parameter mit input-strucre:
  ----------------------------------

  
  ----------------  
  inp.n         (must)(uint32_t) DIMENSION OF THE SYSTEM 

  #if SLF_SOLVER_ODE_USE_DSMODBASE == 1
  inp.pclass    (must)(CDsModBase) 
                bei Klassenbenutzung wird Pointer mit Objektklasse CDSModBase übergeben werden
              
                okay_t pClass->state(const double &t,const CVectorD &x, CVectorD &xp); vorhanden sein xp = dx/dt(t,x)

                      n = GET_NROWS(xp)
                      for(i=0;i<n;i++)
                          xp[i] = dx/dt(t,x)

                wenn inp.RAD.ijac == 1: Fkt muß vorhanden sein

                      okay_t pClass->jacobi(const double &t, const CVectorD &x, CMatrixD dxp_dx)
                    
                        n       = GET_NROWS(x) = GET_N>ROWS(dxp_dx)
                        ldxp_dx = GET_NCOLS(dxp_dx)

                wenn inp.n == ldxp_dx alles belegt

                      for(i=0;i<ldxp_dx;i++)
                          for(j=0;j<n;i++)
                             dxp_dx[i][j] = dxp[i] / dx[j]

                wenn ldxp_dx < inp.n Band

                      for(i=0;i<ldxp_dx;i++)
                          for(j=0;j<n;i++)
                             ldxp_dx[i-j+mujac+1][j] = dxp[i] / dx[j]

                wenn inp.imas == 1
                      okay_t pClass->mass(Matrix am);
                      
                        lmas    = GET_NROWS(am)
                        n       = GET_NCOLS(am)

                Wenn lmas == n (inp.mlmas == inp.n ) voll belegt

                      for(i=0;i<n;i++)
                          for(j=0;j<lmas;i++)
                             am[i][j] = M[i][j]
                    
                Wenn lmas < n (inp.mlmas < inp.n ) voll belegt

                      for(i=0;i<n;i++)
                          for(j=0;j<lmas;i++)
                             am(i-j+mumas+1,j) = M[i,j].

  #else
                 entsprechend:
                 okay_t state(uint32_t n, double x, double *y, double *f);
                 okay_t jacobi(uint32_t n, double x, double *y, double **dfdy,uint32_t ldfy);
                 okay_t mass(uint32_t n, double **am, uint32_t lmas);
  #endif



  inp.ystate     (default=0)(CVectorD)INITIAL VALUES FOR Y, kann intern erzeugt

  inp.itol       (must)(uint8_t) = 0: skalare Toleranz/Fehlerschranke
                                    THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF
                                    Y(I) BELOW RTOL*ABS(Y(I))+ATOL
                               = 1; vektorielle Toleranz/Fehlerschranke (pro Zustand)
                                    THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW
                                    RTOL(I)*ABS(Y(I))+ATOL(I).
  inp.vatol      (must,itol=1)(CVectorD) vektorieller absoluter Fehler (Länge inp.n) 
  inp.vrtol      (must,itol=1)(CVectorD) vektorieller relativer Fehler (Länge inp.n) 
  inp.atol       (must,itol=0)(double) skalare absoluter Fehler (Länge inp.n) 
  inp.rtol       (must,itol=0)(double) skalare relativer Fehler (Länge inp.n)

  inp.hmax       (default=0.0)(double)                (WORK(7))
                  maximale Schrittweite,
                  // wenn 0.0, dann xend-x
                  MAXIMAL STEP SIZE, DEFAULT XEND-X.

  inp.nmax       (default=100000)()           (IWORK(2))      
                  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS.
                  THE DEFAULT VALUE (FOR NMAX=0) IS 100000.

  inp.rad.ijac   (must)(uint8_t) SWITCH FOR THE COMPUTATION OF THE JACOBIAN:
                 IJAC=0: JACOBIAN IS COMPUTED INTERNALLY BY FINITE
                 DIFFERENCES, SUBROUTINE "JAC" IS NEVER CALLED.
                 IJAC=1: JACOBIAN IS SUPPLIED BY SUBROUTINE JAC.

  inp.rad.mljac  (must)(uint32_t) SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
                   MLJAC=N: JACOBIAN IS A FULL MATRIX. THE LINEAR
                      ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
                   0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN 
                      MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
                      THE MAIN DIAGONAL).

  inp.rad.mujac (must,default,wenn mljac=n)(uint32_t) 
                UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON-
                ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
                NEED NOT BE DEFINED IF MLJAC=N.

    ----   MAS,IMAS,MLMAS, AND MUMAS HAVE ANALOG MEANINGS      -----
    ----   FOR THE "MASS MATRIX" (THE MATRIX "M" OF SECTION IV.8): -


  inp.rad.imas  (default=0)(uint8_t) (GIVES INFORMATION ON THE MASS-MATRIX:
                   IMAS=0: M IS SUPPOSED TO BE THE IDENTITY
                      MATRIX, MAS IS NEVER CALLED.
                   IMAS=1: MASS-MATRIX  IS SUPPLIED.

  inp.rad.mlmas (default=0)(uint8_t) Wenn == 0 wird dann intern auf min(mljac,n) gesetzt
                   SWITCH FOR THE BANDED STRUCTURE OF THE MASS-MATRIX:
                   MLMAS=N: THE FULL MATRIX CASE. THE LINEAR
                      ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
                   0<=MLMAS<N: MLMAS IS THE LOWER BANDWITH OF THE
                      MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
                      THE MAIN DIAGONAL).
                MLMAS IS SUPPOSED TO BE .LE. MLJAC.

  inp.rad.mumas (default=0)(uint8_t) Wenn == 0 wird dann intern auf min(mujac,n) gesetzt
                UPPER BANDWITH OF MASS-MATRIX (>= NUMBER OF NON-
                ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
                NEED NOT BE DEFINED IF MLMAS=N.
                MUMAS IS SUPPOSED TO BE .LE. MUJAC.

        
  inp.rad.ihesse  (default=0)(unit8)              (IWORK(1)) 
             IF IHESSE.NE.0, THE CODE TRANSFORMS THE JACOBIAN
             MATRIX TO HESSENBERG FORM. THIS IS PARTICULARLY
             ADVANTAGEOUS FOR LARGE SYSTEMS WITH FULL JACOBIAN.
             IT DOES NOT WORK FOR BANDED JACOBIAN (MLJAC<N)
             AND NOT FOR IMPLICIT SYSTEMS (IMAS=1).

  inp.rad.nit   (default=7)(uint32_t)              (IWORK(3))  
             THE MAXIMUM NUMBER OF NEWTON ITERATIONS FOR THE
             SOLUTION OF THE IMPLICIT SYSTEM IN EACH STEP
             IWORK(3)+(NS-3)*2.5. DEFAULT VALUE IS 7.
             NS IS THE NUMBER OF STAGES (SEE IWORK(11)).

  inp.rad.startn (default=0)(bool)               (IWORK(4))  
             IF STARTN == false THE EXTRAPOLATED COLLOCATION SOLUTION
             IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
             IF STARTN == true ZERO STARTING VALUES ARE USED.
             THE LATTER IS RECOMMENDED IF NEWTON'S METHOD HAS
             DIFFICULTIES WITH CONVERGENCE (THIS IS THE CASE WHEN
             NSTEP IS LARGER THAN NACCPT + NREJCT; SEE OUTPUT PARAM.).
             DEFAULT IS STARTN=false

          THE FOLLOWING 3 PARAMETERS ARE IMPORTANT FOR
          DIFFERENTIAL-ALGEBRAIC SYSTEMS OF INDEX > 1.
          THE FUNCTION-SUBROUTINE SHOULD BE WRITTEN SUCH THAT
          THE INDEX 1,2,3 VARIABLES APPEAR IN THIS ORDER. 
          IN ESTIMATING THE ERROR THE INDEX 2 VARIABLES ARE
          MULTIPLIED BY H, THE INDEX 3 VARIABLES BY H**2.

  inp.rad.nind1  (default=0)(uint32_t)         (IWORK(5))
             DIMENSION OF THE INDEX 1 VARIABLES (MUST BE > 0). FOR 
             ODE'S THIS EQUALS THE DIMENSION OF THE SYSTEM.
             DEFAULT nind1=N.

  inp.rad.nind2  (default=0)(uint32_t)         (IWORK(6))
             DIMENSION OF THE INDEX 2 VARIABLES. DEFAULT IWORK(6)=0.

  inp.rad.nind3  (default=0)(uint32_t)         (IWORK(7))
             DIMENSION OF THE INDEX 3 VARIABLES. DEFAULT IWORK(7)=0.

  inp.rad.pred   (default=1)(uint8_t)           (IWORK(8))
             SWITCH FOR STEP SIZE STRATEGY
             IF IWORK(8).EQ.1  MOD. PREDICTIVE CONTROLLER (GUSTAFSSON)
             IF IWORK(8).EQ.2  CLASSICAL STEP SIZE CONTROL
             THE DEFAULT VALUE  IWORK(8)=1.
             THE CHOICE IWORK(8).EQ.1 SEEMS TO PRODUCE SAFER RESULTS;
             FOR SIMPLE PROBLEMS, THE CHOICE IWORK(8).EQ.2 PRODUCES
             OFTEN SLIGHTLY FASTER RUNS

  inp.rad.m1  (default=0)(uint32_t)             (IWORK(9))
             THE VALUE OF M1.  DEFAULT M1=0. see blow

  inp.rad.m2  (default=0)(uint32_t)             (IWORK(10))
             THE VALUE OF M2.  DEFAULT M2=M1.

                  IF THE DIFFERENTIAL SYSTEM HAS THE SPECIAL STRUCTURE THAT
                       Y(I)' = Y(I+M2)   FOR  I=1,...,M1,
                  WITH M1 A MULTIPLE OF M2, A SUBSTANTIAL GAIN IN COMPUTERTIME
                  CAN BE ACHIEVED BY SETTING THE PARAMETERS IWORK(9) AND IWORK(10).
                  E.G., FOR SECOND ORDER SYSTEMS P'=V, V'=G(P,V), WHERE P AND V ARE 
                  VECTORS OF DIMENSION N/2, ONE HAS TO PUT M1=M2=N/2.
                  FOR M1>0 SOME OF THE INPUT PARAMETERS HAVE DIFFERENT MEANINGS:
                  - JAC: ONLY THE ELEMENTS OF THE NON-TRIVIAL PART OF THE
                         JACOBIAN HAVE TO BE STORED
                         IF (MLJAC.EQ.N-M1) THE JACOBIAN IS SUPPOSED TO BE FULL
                            DFY(I,J) = PARTIAL F(I+M1) / PARTIAL Y(J)
                           FOR I=1,N-M1 AND J=1,N.
                         ELSE, THE JACOBIAN IS BANDED ( M1 = M2 * MM )
                            DFY(I-J+MUJAC+1,J+K*M2) = PARTIAL F(I+M1) / PARTIAL Y(J+K*M2)
                           FOR I=1,MLJAC+MUJAC+1 AND J=1,M2 AND K=0,MM.
                  - MLJAC: MLJAC=N-M1: IF THE NON-TRIVIAL PART OF THE JACOBIAN IS FULL
                           0<=MLJAC<N-M1: IF THE (MM+1) SUBMATRICES (FOR K=0,MM)
                                PARTIAL F(I+M1) / PARTIAL Y(J+K*M2),  I,J=1,M2
                               ARE BANDED, MLJAC IS THE MAXIMAL LOWER BANDWIDTH
                               OF THESE MM+1 SUBMATRICES
                  - MUJAC: MAXIMAL UPPER BANDWIDTH OF THESE MM+1 SUBMATRICES
                           NEED NOT BE DEFINED IF MLJAC=N-M1
                  - MAS: IF IMAS=0 THIS MATRIX IS ASSUMED TO BE THE IDENTITY AND
                         NEED NOT BE DEFINED. SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
                         IT IS ASSUMED THAT ONLY THE ELEMENTS OF RIGHT LOWER BLOCK OF
                         DIMENSION N-M1 DIFFER FROM THAT OF THE IDENTITY MATRIX.
                         IF (MLMAS.EQ.N-M1) THIS SUBMATRIX IS SUPPOSED TO BE FULL
                            AM(I,J) = M(I+M1,J+M1)     FOR I=1,N-M1 AND J=1,N-M1.
                         ELSE, THE MASS MATRIX IS BANDED
                            AM(I-J+MUMAS+1,J) = M(I+M1,J+M1)
                  - MLMAS: MLMAS=N-M1: IF THE NON-TRIVIAL PART OF M IS FULL
                           0<=MLMAS<N-M1: LOWER BANDWIDTH OF THE MASS MATRIX
                  - MUMAS: UPPER BANDWIDTH OF THE MASS MATRIX
                           NEED NOT BE DEFINED IF MLMAS=N-M1

  inp.rad.nsmin   (default=3)(uint8_t)          (IWORK(11))
              NSMIN, MINIMAL NUMBER OF STAGES NS (ORDER 2*NS-1)
             POSSIBLE VALUES ARE 1,3,5,7. DEFAULT NS=3.

  inp.rad.nsmax   (default=7)(uint8_t)          (IWORK(12))
             NSMAX, MAXIMAL NUMBER OF STAGES NS.  
             POSSIBLE VALUES ARE 1,3,5,7. DEFAULT NS=7.
  inp.rad.nsus   (default=3)(uint8_t)          (IWORK(13))
             VALUE OF NS FOR THE FIRST STEP (DEFAULT VALUE: NSMIN)

  inp.rad.uround   (default=1.E-16)(double)   (WORK(1))
             UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16.

  inp.rad.safe     (default=0.9)(double)      (WORK(2))
             THE SAFETY FACTOR IN STEP SIZE PREDICTION,
             DEFAULT 0.9D0.

  inp.rad.thet     (default=0.001)(double)    (WORK(3))
             DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
             INCREASE WORK(3), TO 0.1 SAY, WHEN JACOBIAN EVALUATIONS
             ARE COSTLY. FOR SMALL SYSTEMS WORK(3) SHOULD BE SMALLER 
             (0.001D0, SAY). NEGATIV WORK(3) FORCES THE CODE TO
             COMPUTE THE JACOBIAN AFTER EVERY ACCEPTED STEP.     
             DEFAULT 0.001D0.

  inp.rad.quot1    (default=1.0)(double)       (WORK(5))
  inp.rad.quot2    (default=1.2)(double)       (WORK(6))

             IF WORK(5) < HNEW/HOLD < WORK(6), THEN THE
             STEP SIZE IS NOT CHANGED. THIS SAVES, TOGETHER WITH A
             LARGE WORK(3), LU-DECOMPOSITIONS AND COMPUTING TIME FOR
             LARGE SYSTEMS. FOR SMALL SYSTEMS ONE MAY HAVE
             WORK(5)=1.D0, WORK(6)=1.2D0, FOR LARGE FULL SYSTEMS
             WORK(5)=0.99D0, WORK(6)=2.D0 MIGHT BE GOOD.
             DEFAULTS WORK(5)=1.D0, WORK(6)=1.2D0 .

  inp.rad.fac1     (default=0.2)(double)       (WORK(8))
  inp.rad.fac2     (default=10)(double)        (WORK(9))
             PARAMETERS FOR STEP SIZE SELECTION
             THE NEW STEP SIZE IS CHOSEN SUBJECT TO THE RESTRICTION
                WORK(8) <= HNEW/HOLD <= WORK(9)
             DEFAULT VALUES: WORK(8)=0.2D0, WORK(9)=8.D0

  inp.rad.vitu     (default=0.002)(double)       (WORK(10))
             ORDER IS INCREASED IF THE CONTRACTIVITY FACTOR IS
             SMALL THAN WORK(10), DEFAULT VALUE IS 0.002

  inp.rad.vitd     (default=0.8)(double)         (WORK(11))
             ORDER IS DECREASED IF THE CONTRACTIVITY FACTOR IS
             LARGER THAN WORK(11), DEFAULT VALUE IS 0.8

  inp.rad.hhou     (default=1.2)(double)         (WORK(12))
  inp.rad.hhod     (default=0.8)(double)         (WORK(13))
             ORDER IS DECREASED ONLY IF THE STEPSIZE
             RATIO SATISFIES  WORK(13).LE.HNEW/H.LE.WORK(12),
             DEFAULT VALUES ARE 1.2 AND 0.8
  Output

       get_ierr()    REPORTS ON SUCCESSFULNESS UPON RETURN:
                     = 0  COMPUTATION SUCCESSFUL,
                     = INPUT_NOT_CONSISTENT  INPUT IS NOT CONSISTENT,
                     = NMAX_REACHED  LARGER NMAX IS NEEDED,
                     = STEPSIZE_TOO_SMALL  STEP SIZE BECOMES TOO SMALL,
                     = MATRIX_IS_SINGULAR  MATRIX IS REPEATEDLY SINGULAR.
                     = STATE_NO_SUCCESS  Functioncall state was not successful
                     = JACOBI_NO_SUCCESS  Functioncall jacobi was not successful
                     = MASS_NO_SUCCESS  Functioncall mass was not successful

  ======================================================================================*/
struct CIntegratorRADInp : public CIntegratorBaseInp // Parameter Gear implizit Euler and partial 
{
  bool     ijac;            // (true)  false/true Jakobimatrix zu berechnen
  uint32_t mljac;           // (0)  <= n low banded matrix Jakobi
  uint32_t mujac;           // (0)  <= n upper banded matrix Jakobi
  bool     imas;            // (false)  false/true Massenmatrix berechnen
  uint32_t mlmas;           // (0)  <= n low banded matrix Mass
  uint32_t mumas;           // (0)  <= n upper banded matrix Mass
  bool     ihesse;          // (false)  Soll Hesseform berechnet werden
  uint32_t nit;             // (7)  max Iteration Newton
  bool     startn;          // (false) IF STARTN == false THE EXTRAPOLATED COLLOCATION SOLUTION
                            //              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
                            //              IF STARTN == true ZERO STARTING VALUES ARE USED. 
  uint32_t nind1;           // (0)  Anzahl Index 1 algeraische Variabeln
  uint32_t nind2;           // (0)  Anzahl Index 2 algeraische Variabeln
  uint32_t nind3;           // (0)  Anzahl Index 3 algeraische Variabeln
  uint8_t  pred;            // (1)  1/2 Schrittweitensteuerung Gustafson/standard
  uint32_t m1;              // (0)  Anzahl erste Ableitung Y(I)' = Y(I+M2)   FOR  I=1,...,M1
  uint32_t m2;              // (0)  Anzahl zweite Ableitung m2 sollte m1
  uint8_t  nsmin;           // (3)  NSMIN, MINIMAL NUMBER OF STAGES NS (3) (ORDER 2*NS-1)
  uint8_t  nsmax;           // (7)  NSMAX, MAXIMAL NUMBER OF STAGES NS (7)
  uint8_t  nsus;            // (3)  VALUE OF NS FOR THE FIRST STEP (DEFAULT VALUE: NSMIN)
  double   uround;          // (1.0e-16)  Rundung
  double   safe;            // (0.8)      Sicherheitsfaktor
  double   thet;            // (0.001)    Kosten für Jakobimatrix z.B. 0.1, wenn teuer
  double   quot1;           // (1.0)      H wird nicht geändert, wenn:
  double   quot2;           // (1.2)      quot1 < HNEW/HOLD < quot2
  double   fac1;            // (0.2)      parameters for step size selection
  double   fac2;            // (10.)      fac1 <= hnew/hold <= fac2
  double   vitu;            // (0.002)    ORDER IS INCREASED IF THE CONTRACTIVITY FACTOR IS SMALL
  double   vitd;            // (0.8)      ORDER IS DECREASED IF THE CONTRACTIVITY FACTOR IS LARGER
  double   hhou;            // (1.2)      ORDER IS DECREASED ONLY IF THE STEPSIZE
  double   hhod;            // (0.8)      RATIO SATISFIES  hhou<=HNEW/H<=hhod

  CIntegratorRADInp() :CIntegratorBaseInp()
    , ijac(true)
    , mljac(0)
    , mujac(0)
    , imas(false)
    , mlmas(0)
    , mumas(0)
    , ihesse(false)
    , nit(7)
    , startn(false)
    , nind1(0)
    , nind2(0)
    , nind3(0)
    , pred(1)
    , m1(0)
    , m2(0)
    , nsmin(3)
    , nsmax(7)
    , nsus(3)
    , uround(1.0e-16)
    , safe(0.8)
    , thet(0.001)
    , quot1(1.0)
    , quot2(1.2)
    , fac1(0.2)
    , fac2(10.)
    , vitu(0.002)
    , vitd(0.8)
    , hhou(1.2)
    , hhod(0.8)
  {}

};


  class CSolveODE_RAD : public CSolveODEBase 
  {
  public:

      CSolveODE_RAD();
      ~CSolveODE_RAD();


      okay_t init(CIntegratorRADInp &inp);
      okay_t calcFirst(double &x       // Startzeit, x-Wert
                       ,CVectorD &ystart  // Startzustände
                       ,double &hstart  // Startschrittweite (kann 0.0 sein)
                       ,double posneg=1. // Zeitachse/x-Achse positiv (1.) oder negativ (-1.)
                       );
      okay_t calc(double xend);     // Rechne bis xend
    
      void reset(void);               // Löschen von Speicher und zurücksetzen der Variablen

      EErrNo get_ierr(void){return Ierr;}
  private:

    //bool     ijac;            // (true)  false/true Jakobimatrix zu berechnen
    //uint32_t mljac;           // (0)  <= n low banded matrix Jakobi
    //uint32_t mujac;           // (0)  <= n upper banded matrix Jakobi
    //uint8_t  imas;            // (0)  0/1 Massenmatrix berechnen
    //uint32_t mlmas;           // (0)  <= n low banded matrix Mass
    //uint32_t mumas;           // (0)  <= n upper banded matrix Mass
    //bool     ihesse;          // (0)  Soll Hesseform berechnet werden
    //uint32_t nit;             // (7)  max Iteration Newton
    //bool     startn;          // (false) IF STARTN == false THE EXTRAPOLATED COLLOCATION SOLUTION
    //                          //              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
    //                          //              IF STARTN == true ZERO STARTING VALUES ARE USED. 
    //uint32_t nind1;           // (0)  Anzahl Index 1 algeraische Variabeln
    //uint32_t nind2;           // (0)  Anzahl Index 2 algeraische Variabeln
    //uint32_t nind3;           // (0)  Anzahl Index 3 algeraische Variabeln
    //uint8_t  pred;            // (1)  1/2 Schrittweitensteuerung Gustafson/standard
    //uint32_t m1;              // (0)  Anzahl erste Ableitung Y(I)' = Y(I+M2)   FOR  I=1,...,M1
    //uint32_t m2;              // (0)  Anzahl zweite Ableitung m2 sollte m1
    //uint8_t  nsmin;           // (3)  NSMIN, MINIMAL NUMBER OF STAGES NS (3) (ORDER 2*NS-1)
    //uint8_t  nsmax;           // (7)  NSMAX, MAXIMAL NUMBER OF STAGES NS (7)
    //uint8_t  nsus;            // (3)  VALUE OF NS FOR THE FIRST STEP (DEFAULT VALUE: NSMIN)
    //double   uround;          // (1.0e-16)  Rundung
    //double   safe;            // (0.8)      Sicherheitsfaktor
    //double   thet;            // (0.001)    Kosten für Jakobimatrix z.B. 0.1, wenn teuer
    //double   quot1;           // (1.0)      H wird nicht geändert, wenn:
    //double   quot2;           // (1.2)      quot1 < HNEW/HOLD < quot2
    //double   fac1;            // (0.2)      parameters for step size selection
    //double   fac2;            // (10.)      fac1 <= hnew/hold <= fac2
    //double   vitu;            // (0.002)    ORDER IS INCREASED IF THE CONTRACTIVITY FACTOR IS SMALL
    //double   vitd;            // (0.8)      ORDER IS DECREASED IF THE CONTRACTIVITY FACTOR IS LARGER
    //double   hhou;            // (1.2)      ORDER IS DECREASED ONLY IF THE STEPSIZE
    //double   hhod;            // (0.8)      RATIO SATISFIES  hhou<=HNEW/H<=hhod

      EErrNo Ierr;
      uint32_t N;                       // Anzahl der Zustände

      uint8_t Itol;                     // Auswahl Toleranz skalar(0) oder Vektor(1)
      uint8_t Ns;                       // Stufe des Verfahrens
      uint32_t Nsmin;                    // minmale zu verwendende Stufe
      uint32_t Nsmax;                    // maxmale zu verwendende Stufe
      uint32_t NNsmax;                  // ist N*Nsmax
      uint32_t Nm1Nsmax;                // ist Nm1*Nsmax
      uint32_t Nmee;                    // Nm1*(Nsmax-1), Spaltengröße EE2
      bool    Ijac;                     // Soll Jacobi berechnet werden
      uint32_t Mljac;                    // lower bandwidth Jacobi-Matrix
      uint32_t Mujac;                    // upper bandwidth Jacobi-Matrix
      uint32_t Mlmas;                    // lower bandwidth Mass-Matrix
      uint32_t Mumas;                    // upper bandwidth Mass-Matrix
      uint8_t  Iout;                     // Soll eine Ausgabefunktion benutzt werden (nicht vorgesehen = 0)
      int32_t Idid;                     // Rückgabewert Fehler wenn < 0
      uint32_t Nit;                      // Maximale Iteration Newton
      uint8_t Ijob;                     // Typ der Berechnung wir in init berechnet
      bool    Startn;                   // Startmethode IF STARTN == false THE EXTRAPOLATED COLLOCATION SOLUTION
                                        //              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
                                        //              IF STARTN == true ZERO STARTING VALUES ARE USED.
      uint32_t Nind1, Nind2, Nind3;     // Anzahl Index1, Index2, Index3 Variablen
      uint8_t  Pred;                     // (1)  1/2 step strategy Gustafson/standard
      uint32_t M1;                      // Anzahl erste Ordnung Gleichungen (default:0)
      uint32_t M2;                      // Anzahl zweite Ordnung Gleichungen (default:0)
      uint32_t Nm1;                     // N - M1, wenn M1=0, dann Nm1=N
      bool     Implct;                   // Implizit, d.h Massenmatrix wird berechnet
      bool     Banded;                   // Jacobimatrix is banded
      uint32_t Ldjac;                    // Rowdimension Fjac
      uint32_t Lde1;                     // Rowdimension E1,EE2
      uint32_t Ldmas;                    // Rowdimension Fmas
      CVectorD Zz;                       //  Vektor
      CVectorD Ff;                       //
      CVectorD Y0;                       //
      CVectorD Scal;                     //
      CVectorD Fjac;                     // Jacobimatrix 
      CMatrixD FjacMat;                  // als Matrix definiert, enthält pointer von pFjac
      CVectorD Fmas;                     // Massenmatrix 
      CMatrixD FmasMat;                  // als Matrix definiert, enthält pointer von pFmas
      CVectorD Cont;                     // Vektor
      CVectorD E1,Ee2;                   // Matrix als Vektor
      CVectorI Ip1;                      // Integervektor
      CVectorI Ip2;                      // Integervektor
      CVectorI Iphes;                    // Integervektor

      uint32_t Naccpt,Nrejct,Ndec,Nsol;

      double Thet;
      double Quot1, Quot2;
      double Facl, Facr;
      double Vitu, Vitd;
      double Hhou, Hhod;

      bool   Imas;
      bool   Ihesse;

      sradau_weight rad_weight;
      sradau_linal  rad_linal;
      sradau_coe3   rad_coe3;
      sradau_coe5   rad_coe5;
      sradau_coe7   rad_coe7;

      int radau_core(uint32_t &n
        , CModelBase *pobj
        , double &x
        , CVectorD  &y
        , double    &xend
        , double    &hmax
        , double    &h__
        , CVectorD  &rtol
        , CVectorD  &atol
        , uint8_t &itol
        , uint8_t &ns
        , bool    ijac
        , uint32_t &mljac
        , uint32_t &mujac
        , uint32_t &mlmas
        , uint32_t &mumas
        , S_fp solout
        , uint8_t &iout
        , int32_t &idid
        , uint32_t &nmax
        , double &uround
        , double &safe
        , double &thet
        , double &quot1
        , double &quot2
        , uint32_t &nit1
        , uint8_t &ijob
        , bool    startn
        , uint32_t &nind1
        , uint32_t &nind2
        , uint32_t &nind3
        , uint8_t &pred
        , double &facl
        , double &facr
        , uint32_t &m1
        , uint32_t &m2
        , uint32_t &nm1
        , uint32_t &nsmin
        , uint32_t &nsmax
        , uint32_t &nnms
        , uint32_t &nm1ns
        , uint32_t &nmee
        , bool     implct
        , bool     banded
        , uint32_t &ldjac
        , uint32_t &lde1
        , uint32_t &ldmas
        , double   &zz
        , CVectorD  &y0
        , CVectorD  &scal
        , CVectorD  &ff
        , CMatrixD  &fjac
        , CMatrixD  &fjacmat
        , CMatrixD  &e1
        , CMatrixD  &ee2
        , CMatrixD  &fmas
        , CMatrixD  &fmasmat
        , CVectorD  &cont
        , CVectorI  &ip1
        , CVectorI  &ip2
        , CVectorI  &iphes
        , double    &vitu
        , double    &vitd
        , double    &hhou
        , double    &hhod
        , uint32_t &nfcn
        , uint32_t &njac
        , uint32_t &nstep
        , uint32_t &naccpt
        , uint32_t &nrejct
        , uint32_t &ndec
        , uint32_t &nsol
        , CStr &errtext);

      int coertv(const uint32_t &nsmax);
    
      int coercv(uint8_t &ns, double *c__, double *dd,
        double &u1, double *alph, double *beta);
    
      double radau_contra(int32_t *i, double *x, double *cont, int32_t *lrc);
    
      int estrad(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, double *h__, double *dd1, 
	  double *dd2, double *dd3
  #if SLF_SOLVER_ODE_USE_DSMODBASE == 1
      , CModelBase *pclass
  #else
      , FcnEqDiff state
  #endif
      , int32_t *nfcn, double *y0, double *y, int32_t *ijob, double *x, int32_t *m1, 
	  int32_t *m2, int32_t *nm1, double *e1, int32_t *lde1, double *
	  z1, double *z2, double *z3, double *cont, double *f1, 
	  double *f2, int32_t *ip1, int32_t *iphes, double *scal, 
	  double *err, int32_t *first, int32_t *reject, double *fac1
      ,int32_t *idid, CStr &errtext);

      int estrav(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, double *h__, double *dd
  #if SLF_SOLVER_ODE_USE_DSMODBASE == 1
      , CModelBase *pclass
  #else
      , FcnEqDiff state
  #endif
      , int32_t *nfcn, double *y0, double *y, int32_t *ijob, 
	  double *x, int32_t *m1, int32_t *m2, int32_t *nm1, int32_t *ns, 
	  int32_t *nns, double *e1, int32_t *lde1, double *zz, 
	  double *cont, double *ff, int32_t *ip1, int32_t *iphes, 
	  double *scal, double *err, int32_t *first, int32_t *reject, 
	  double *fac1, int32_t *idid, CStr &errtext);

      int decomr(int32_t *n, double *fjac, int32_t *ldjac, 
	  double *fmas, int32_t *ldmas, int32_t *mlmas, int32_t *mumas, 
	  int32_t *m1, int32_t *m2, int32_t *nm1, double *fac1, double *
	  e1, int32_t *lde1, int32_t *ip1, int32_t *ier, int32_t *ijob, uint8_t 
	  *calhes, int32_t *iphes);
    
      int decomc(int32_t *n, double *fjac, int32_t *ldjac, 
	  double *fmas, int32_t *ldmas, int32_t *mlmas, int32_t *mumas, 
	  int32_t *m1, int32_t *m2, int32_t *nm1, double *alphn, double 
	  *betan, double *e2r, double *e2i, int32_t *lde1, int32_t *ip2,
	   int32_t *ier, int32_t *ijob);

      int slvrar(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	  nm1, double *fac1, double *e1, int32_t *lde1, double *z1, 
	  double *f1, int32_t *ip1, int32_t *iphes, int32_t *ier, int32_t *
	  ijob);

      int slvrai(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	  nm1, double *alphn, double *betan, double *e2r, 
	  double *e2i, int32_t *lde1, double *z2, double *z3, 
	  double *f2, double *f3, double *cont, int32_t *ip2, 
	  int32_t *iphes, int32_t *ier, int32_t *ijob);

      int slvrad(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	  nm1, double *fac1, double *alphn, double *betan, 
	  double *e1, double *e2r, double *e2i, int32_t *lde1, 
	  double *z1, double *z2, double *z3, double *f1, 
	  double *f2, double *f3, double *cont, int32_t *ip1, 
	  int32_t *ip2, int32_t *iphes, int32_t *ier, int32_t *ijob);

      int slvrod(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	  nm1, double *fac1, double *e, int32_t *lde, int32_t *ip, 
	  double *dy, double *ak, double *fx, double *ynew, 
	  double *hd, int32_t *ijob, int32_t *stage1);

      int slvseu(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	  nm1, double *fac1, double *e, int32_t *lde, int32_t *ip, 
	  int32_t *iphes, double *del, int32_t *ijob);

  #if 0
    uint8_t   Ijob;
  
    uint32_t  Nsus;
  
  
    uint32_t Nmee;
    //uint32_t Iezz,Iey0,Iescal,Ieff,Iecon,Iejac,Iemas,Iee1,Iee;

    //int32_t Ieip1,Ieip2,Ieiph;

  

    uint32_t Nn;
    uint32_t Nscon;
    double Xsol;
    double Hsol;
    double Alphn[INTEGRATOR_NSDIM];
    double Betan[INTEGRATOR_NSDIM];

    uint32_t Mle, Mue, Mbjac, Mbb, Mdiag, Mdiff, Mbdiag;

    double C1[2];
    double Alph1[1];
    double Beta1[1];
    double Ddd1[1];

    double T3[3][3];
    double Ti3[3][3];
    double C3[4];
    double Alph3[3];
    double Beta3[3];
    double Ddd3[3];

    double T5[5][5];
    double Ti5[5][5];
    double C5[6];
    double Alph5[5];
    double Beta5[5];
    double Ddd5[5];

    double T7[7][7];
    double Ti7[7][7];
    double C7[8];
    double Alph7[7];
    double Beta7[7];
    double Ddd7[7];

    double U1[INTEGRATOR_NSDIM];

    uint8_t Reject, First, Caljac, Startn, Calhes;
    uint8_t Last, Change, Unexp, Unexn, Variab;

    double Expo, Sq6, C31, C32, C31m1, C32m1, C31mc2, Dd1, Dd2, Dd3;
    double St9;
    uint32_t N2, N3, N4, N5, N6, Lrc, Nns;
    uint32_t Newt;
    uint8_t  Ikeep, Ichan;
    EErrNo Ierr;

    double Theta, Thetat;

    double Xold, Hold, Hopt, Hquot, Faccon;
    uint32_t Nsing;
    double Expmns,Hhfac;

  #endif
  };

}
#endif