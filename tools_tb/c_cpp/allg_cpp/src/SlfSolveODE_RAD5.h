// DEF_RADAU      impliziter Runge-Kutta, Radau
//
#ifndef SLF_SOLVER_ODE_RAD_H_INCLUDED
#define SLF_SOLVER_ODE_RAD_H_INCLUDED

#include "SlfSolveODE.h"
//#include "SlfSolveODE_RAD_int.h"

namespace slf
{

  /*====================================================================================
  ======================================================================================*/
struct CIntegratorRAD5Inp : public CIntegratorBaseInp // Parameter Gear implizit Euler and partial 
{
  uint8_t  ijac;            // (0)  0/1 Jakobimatrix zu berechnen
  size_t mljac;           // (0)  <= n low banded matrix Jakobi
  size_t mujac;           // (0)  <= n upper banded matrix Jakobi
  bool     imas;            // (false)  false/true Massenmatrix berechnen
  size_t mlmas;           // (0)  <= n low banded matrix Mass
  size_t mumas;           // (0)  <= n upper banded matrix Mass
  bool     ihesse;          // (false)  Soll Hesseform berechnet werden
  size_t nit;             // (7)  max Iteration Newton
  bool     startn;          // (false) IF STARTN == false THE EXTRAPOLATED COLLOCATION SOLUTION
                            //              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
                            //              IF STARTN == true ZERO STARTING VALUES ARE USED. 
  size_t nind1;           // (0)  Anzahl Index 1 algeraische Variabeln
  size_t nind2;           // (0)  Anzahl Index 2 algeraische Variabeln
  size_t nind3;           // (0)  Anzahl Index 3 algeraische Variabeln
  bool     pred;            // (true)  1/2 Schrittweitensteuerung Gustafson/standard
  size_t m1;              // (0)  Anzahl erste Ableitung Y(I)' = Y(I+M2)   FOR  I=1,...,M1
  size_t m2;              // (0)  Anzahl zweite Ableitung m2 sollte m1
  //uint8_t  nsmin;           // (3)  NSMIN, MINIMAL NUMBER OF STAGES NS (3) (ORDER 2*NS-1)
  //uint8_t  nsmax;           // (7)  NSMAX, MAXIMAL NUMBER OF STAGES NS (7)
  //uint8_t  nsus;            // (3)  VALUE OF NS FOR THE FIRST STEP (DEFAULT VALUE: NSMIN)
  double   uround;          // (1.0e-16)  Rundung
  double   safe;            // (0.8)      Sicherheitsfaktor
  double   thet;            // (0.001)    Kosten für Jakobimatrix z.B. 0.1, wenn teuer
  double   fnewt;           // (0.0)      stopping criterion for Newton's method, usually chosen < 1
  double   quot1;           // (1.0)      H wird nicht geändert, wenn:
  double   quot2;           // (1.2)      quot1 < HNEW/HOLD < quot2
  double   fac1;            // (0.2)      parameters for step size selection
  double   fac2;            // (10.)      fac1 <= hnew/hold <= fac2
  double   vitu;            // (0.002)    ORDER IS INCREASED IF THE CONTRACTIVITY FACTOR IS SMALL
  double   vitd;            // (0.8)      ORDER IS DECREASED IF THE CONTRACTIVITY FACTOR IS LARGER
  double   hhou;            // (1.2)      ORDER IS DECREASED ONLY IF THE STEPSIZE
  double   hhod;            // (0.8)      RATIO SATISFIES  hhou<=HNEW/H<=hhod
  uint8_t  iout;            // (0)        1: write output to log
  double   hinit;           // (1.e-7)    initial step size

  CIntegratorRAD5Inp() :CIntegratorBaseInp()
    , ijac(0)
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
    , pred(true)
    , m1(0)
    , m2(0)
    //, nsmin(3)
    //, nsmax(7)
    //, nsus(3)
    , uround(1.0e-16)
    , safe(0.9)
    , thet(0.001)
    , fnewt(0.0)
    , quot1(1.0)
    , quot2(1.2)
    , fac1(0.2)
    , fac2(8.)
    , vitu(0.002)
    , vitd(0.8)
    , hhou(1.2)
    , hhod(0.8)
    , iout(false)
    , hinit(1.e-7)
  {}

};


  class CSolveODE_RAD5 : public CSolveODEBase 
  {
  public:

      CSolveODE_RAD5();
      ~CSolveODE_RAD5();


      okay_t init(CIntegratorRAD5Inp &inp);
      okay_t calcFirst(double &x       // Startzeit, x-Wert
                       ,CVectorD &ystart  // Startzustände
                       ,double &hstart  // Startschrittweite (kann 0.0 sein)
                       ,double posneg=1. // Zeitachse/x-Achse positiv (1.) oder negativ (-1.)
                       );
      okay_t calc(double xend);     // Rechne bis xend
    
      void reset(void);               // Löschen von Speicher und zurücksetzen der Variablen

      EErrNo get_ierr(void){return Ierr;}
  private:

      EErrNo Ierr;
      size_t N;                       // Anzahl der Zustände

      uint8_t Itol;                     // Auswahl Toleranz skalar(0) oder Vektor(1)
      //uint8_t Ns;                       // Stufe des Verfahrens
      //size_t Nsmin;                    // minmale zu verwendende Stufe
      //size_t Nsmax;                    // maxmale zu verwendende Stufe
      //size_t NNsmax;                  // ist N*Nsmax
      //size_t Nm1Nsmax;                // ist Nm1*Nsmax
      //size_t Nmee;                    // Nm1*(Nsmax-1), Spaltengröße EE2
      bool    Ijac;                     // Soll Jacobi berechnet werden
      size_t Mljac;                    // lower bandwidth Jacobi-Matrix
      size_t Mujac;                    // upper bandwidth Jacobi-Matrix
      size_t Mlmas;                    // lower bandwidth Mass-Matrix
      size_t Mumas;                    // upper bandwidth Mass-Matrix
      uint8_t  Iout;                     // Soll eine Ausgabefunktion benutzt werden (nicht vorgesehen = 0)
                                         // =0 keine Ausgabe
                                         // =1 Endausgabe
                                         // =2 Endausgabe + Zwischenergebnisse
      int32_t Idid;                      // Rückgabewert Fehler wenn < 0
      size_t Nit;                      // Maximale Iteration Newton
      uint8_t Ijob;                     // Typ der Berechnung wir in init berechnet
      bool    Startn;                   // Startmethode IF STARTN == false THE EXTRAPOLATED COLLOCATION SOLUTION
                                        //              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
                                        //              IF STARTN == true ZERO STARTING VALUES ARE USED.
      size_t Nind1, Nind2, Nind3;     // Anzahl Index1, Index2, Index3 Variablen
      bool   Pred;                      // true  1/2 step strategy Gustafson/standard
      size_t M1;                      // Anzahl erste Ordnung Gleichungen (default:0)
      size_t M2;                      // Anzahl zweite Ordnung Gleichungen (default:0)
      size_t Nm1;                     // N - M1, wenn M1=0, dann Nm1=N
      bool     Implct;                   // Implizit, d.h Massenmatrix wird berechnet
      bool     Banded;                   // Jacobimatrix is banded
      size_t Ldjac;                    // Rowdimension Fjac
      size_t Lde1;                     // Rowdimension E1,EE2
      size_t Ldmas;                    // Rowdimension Fmas

      CVectorD Z1,Z2,Z3;                 //  Vektor
      CVectorD Y0;                       //
      CVectorD Scal;                     //
      CVectorD F1, F2, F3;               //  Vektor

      CMatrixD Fjac;                     // Jacobimatrix 
      CMatrixD FjacMat;                  // als Matrix definiert, enthält pointer von pFjac
      CMatrixD Fmas;                     // Massenmatrix 
      CMatrixD FmasMat;                  // als Matrix definiert, enthält pointer von pFmas
      CVectorD Cont;                     // Vektor
      CMatrixD E1,E2i, E2r;                    // Matrix als Vektor
      CVectorI Ip1;                      // Integervektor
      CVectorI Ip2;                      // Integervektor
      CVectorI Iphes;                    // Integervektor

      size_t Naccpt,Nrejct,Ndec,Nsol;

      double Thet;
      double Fnewt;
      double Quot1, Quot2;
      double Facl, Facr;
      double Vitu, Vitd;
      double Hhou, Hhod;

      bool   Imas;
      bool   Ihesse;

      bool   Reject;
      bool   First;
      bool   Caljac;
      bool   Calhesse;

      size_t Mle;
      size_t Mue;
      size_t Mbjac;
      size_t Mbb;
      size_t Mdiag;
      size_t Mdiff;
      size_t Mbdiag;


      double Err;

      void Integrate(double &xend);
      int CoreIntegrator(void);
      void ComputeJacobian(void);
      double ContinuousOutput(size_t i);
      size_t DecompReal(void);
      size_t DecompComplex(void);
      size_t LinearSolve(void);
      size_t ErrorEstimate(void);
      size_t SolutionOutput(void);
      //int32_t radau5_
      //  (int32_t *n                // Anzahl Zustaände
      //  , CModelBase *pobj
      //  //, U_fp fcn
      //  , double *x                // Initial value x-value
      //  , double *y                // initial value for y
      //  , double *xend             // final x-value
      //  , double *h__              // initial step size
      //  , double *rtol             // relative and absolute error
      //  , double *atol
      //  , int32_t *itol            // =0 scalar =1 vector error y(i) <= rtol(i)*abs(y(i))+atol(i) 
      //  //, U_fp jac
      //  , int32_t *ijac            // =0 Jacobian internal calculated =1 jacobian supplied by pObj->Jacobi()
      //  , int32_t *mljac           // 0<mljac<n lower bandwidth of jabobian =n full matrix
      //  , int32_t *mujac           // upper bandwidth jacobian
      //  //, U_fp mas         
      //  , int32_t *imas            // =0 identy matrix mass never called, =1 mass matrix supplied
      //  , int32_t *mlmas           // 0<mlmas<n lower bandwidth of mass matrix calculated =n full matrix
      //  , int32_t *mumas           // upper bandwidth mass matrix
      //  //, U_fp solout
      //  , int32_t *iout            // =0 no call of output routine
      //  , double *work             // array of working space work[0] ... work[20] must be zero, length of vector lwork must be at least n*(ljac+lmas+3*le+12)+20
      //  , int32_t *lwork           // = n*(ljac+lmas+3*le+12)+20 seee above
      //  , int32_t *iwork           // integer work space of length liwork
      //  , int32_t *liwork          // = 3*n+20.
      //  //, double *rpar
      //  //, int32_t *ipar
      //  , int32_t *idid)           //     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
      //                             //     IDID= 1  COMPUTATION SUCCESSFUL, 
      //                             //     IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUT) 
      //                             //     IDID=-1  INPUT IS NOT CONSISTENT, 
      //                             //     IDID=-2  LARGER NMAX IS NEEDED, 
      //                             //     IDID=-3  STEP SIZE BECOMES TOO SMALL, 
      //                             //     IDID=-4  MATRIX IS REPEATEDLY SINGULAR. 



  };

}
#endif