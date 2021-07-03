//
// verschiedene Integrationsverfahren als Klassendefinition
//
// DEF_PIEULER    partial implicit euler (Teilimpliziter Euler)
// DEF_PGEARS     partial implicit Gears-Mehrschrittverfahren (Teilimpliziter Gears)
// DEF_IEULER     implicit Euler
// DEF_GEARS      implizites Gears-Mehrschrittverfahren
// DEF_DOPRI45    expliziter Runge-Kutta, Dormand & Prince 4(5) Ordnung
// DEF_RADAU      impliziter Runge-Kutta, Radau
// DEF_LSODA      livermore solver ordinary diffequ atomatic method switching stiff/nonstiff
//
#ifndef SLF_SOLVER_ODE_H_INCLUDED
#define SLF_SOLVER_ODE_H_INCLUDED

 
#include "SlfStr.h"
#include "SlfNum.h"
#include "SlfMessage.h"
#include "SlfModelBase.h"

namespace slf
{ 
    typedef okay_t (*FcnEqDiff)(const double &tact, const CVectorD &x, CVectorD &xp);
    typedef okay_t (*FcnEqJac)(const double &tact, const CVectorD &x, CMatrixD &dxp_dx, uint32_t ldsp_dx);
    typedef okay_t (*FcnEqMas)(CMatrixD &am, uint32_t lmas);
}

#define INTEGRATOR_KMAX         10
#define INTEGRATOR_NSDIM         7

namespace slf
{
  //typedef void (*Func_fp)(const int32_t &n, const double &tact, const CVectorD &x, CVectorD &xp);
  //typedef void (*Jac_fp)(const int32_t &n, const double &tact, const CVectorD &x, int32_t &ml, int32_t &mu, CMatrixD &dxp_dx, uint32_t ldsp_dx);
  // dummy
  //typedef int (*S_fp)(...);

  struct SIntegratorLSODAInp  // Parameter livermore solver ordinary diffequ atomatic
  {                                    // method switching stiff/nonstiff

    uint32_t ijac;            // (1)  0/1  Jakobimatrix berechnen
    uint32_t mljac;           // (0)  <= n low banded matrix
    uint32_t mujac;           // (0)  <= n upper banded matrix
    uint32_t mxordn;          // (12) maximalen Ordnung non stiff (max = 12)
    uint32_t mxords;          // (5)  maximalen Ordnung siff (max=5)
  };

  struct SIntegratorRADInp      // Parameter expliziter RungeKutta
  {
    uint32_t ijac;            // (1)  0/1 Jakobimatrix zu berechnen
    uint32_t mljac;           // (0)  <= n low banded matrix Jakobi
    uint32_t mujac;           // (0)  <= n upper banded matrix Jakobi
    uint8_t  imas;            // (0)  0/1 Massenmatrix berechnen
    uint32_t mlmas;           // (0)  <= n low banded matrix Mass
    uint32_t mumas;           // (0)  <= n upper banded matrix Mass
    uint8_t  ihesse;          // (0)  Soll Hesseform berechnet werden
    uint32_t nit;             // (7)  max Iteration Newton
    uint8_t  startn;          // (0) 
    uint32_t nind1;           // (0)  Anzahl Index 1 algeraische Variabeln
    uint32_t nind2;           // (0)  Anzahl Index 2 algeraische Variabeln
    uint32_t nind3;           // (0)  Anzahl Index 3 algeraische Variabeln
    uint8_t  pred;            // (1)  1/2 Schrittweitensteuerung Gustafson/standard
    uint32_t m1;              // (0)  Anzahl erste Ableitung Y(I)' = Y(I+M2)   FOR  I=1,...,M1
    uint32_t m2;              // (0)  Anzahl zweite Ableitung m1 sollte m2
    uint8_t  nsmin;           // (3)  NSMIN, MINIMAL NUMBER OF STAGES NS (3) (ORDER 2*NS-1)
    uint8_t  nsmax;           // (7)  NSMAX, MAXIMAL NUMBER OF STAGES NS (7)
    uint8_t  nsus;            // (3)  VALUE OF NS FOR THE FIRST STEP (DEFAULT VALUE: NSMIN)
    double uround;          // (1.0e-16)  Rundung
    double safe;            // (0.8)      Sicherheitsfaktor
    double thet;            // (0.001)    Kosten für Jakobimatrix z.B. 0.1, wenn teuer
    double quot1;           // (1.0)      H wird nicht geändert, wenn:
    double quot2;           // (1.2)      quot1 < HNEW/HOLD < quot2
    double fac1;            // (0.2)      parameters for step size selection
    double fac2;            // (10.)      fac1 <= hnew/hold <= fac2
    double vitu;            // (0.002)    ORDER IS INCREASED IF THE CONTRACTIVITY FACTOR IS SMALL
    double vitd;            // (0.8)      ORDER IS DECREASED IF THE CONTRACTIVITY FACTOR IS LARGER
    double hhou;            // (1.2)      ORDER IS DECREASED ONLY IF THE STEPSIZE
    double hhod;            // (0.8)      RATIO SATISFIES  hhou<=HNEW/H<=hhod
  };

  class CIntegratorBaseInp
  {
  public:
    //--------------------------allgemeine Parameter
    uint32_t        n;          // (?)    Anzahl Zustände
   
    CModelBase      *pobj;      //        class object with obj.StateDer(const double &tact, const CVectorD &x, CVectorD &xp)
                                //                          input:    tact time in sec
                                //                                    x    state-Vector 
                                //                          output:   xp   state derivation-Vector
                                //
    //FcnEqJac        jacobi;     // (?)    Funktion zur Berechnung der Jacobimatrix
    //FcnEqMas        mass;       // (?)    Funktion zur Berechnung der Massenmatrix
    uint8_t         itol;        // (0)    itol=0, skalar err(y[i]) < rtol*abs(y[i])+atol
                                 //        itol=1, vector err(y[i]) < vrtol[i]*abs(y[i])+vatol[i]
    double          rtol;        // (?)    relative  Toleranz itol = 0
    double          atol;        // (?)    absolute Toleranz
    CVectorD        vrtol;       // (?)    relative  Vektor Toleranz itol = 1
    CVectorD        vatol;       // (?)    absolute Vektor Toleranz
    double          hmax;        // (0.0)  maximal step size default=xend-x
    double          hmin;        // (0.0)  kleinste Schrittweite defoult 0.
    uint32_t        nmax;        // 100000 maximal number of allowed steps, wenn 0, default 100000

    CIntegratorBaseInp()
      :n(0)
      , pobj(0)
      //,jacobi(0)
      //,mass(0)
      ,itol(0)
      ,rtol(0.001)
      ,atol(0.001)
      ,vrtol()
      ,vatol()
      ,hmax(0.0)
      ,hmin(0.0)
      ,nmax(100000) {}
    //---------------------------spezifische Parameter
    //SIntegratorERKInp   erk;
    //SIntegratorRADInp   rad;
    //SIntegratorLSODAInp lsoda;
    //SIntegratorGEARInp  gear;
  };

  // Basismodell 
  //========================================================================
  class CSolveODEBase
  {
  protected:
    CMessage       mMessage;

    CModelBase     *PObj;

    uint32_t       N;                // Anzahl Zustände
    uint32_t        Nmax;             // maximal zulässige Anzahl von steps

    CVectorD       Y;                // akueller  Zustndsvektor
    double         H, H1;            // aktuelle Schrittweite und Hilfsgröße
    double         Hold;             // alte Schritweite
    double         Hmax;             // maximal step size default=xend-x

    double         Uround;           // Rundunggröße
    double         Posneg;           // Richtungsangabe

    CVectorD       VAtol;           // absolute Toleranz für alle Zustände
    CVectorD       VRtol;           // relative Toleranz
    double         Atol;            // absolute Toleranz über alle konstant (skaler)
    double         Rtol;            // relative Toleranz
    uint8_t        Itoler;          // Itoler=0, skalar 
                                    // Itoler=1, vector 
    uint8_t        Meth;             // Methode siehe Funktion
    uint32_t       Norder;           // Ordnung

    uint32_t       Nfcn;             // Anzahl Funktionsaufrufe
    uint32_t       Nstep;            // aktuelle Stepzahl
    uint32_t       NstepMax;         // maximale Anzahl Steps pro Aufruf
    uint32_t       NstepAccept;      // Anzahl angenommener Steps dabei
    uint32_t       Njac;             // Anzahl Jacobiaufrufe
    double         X;                // aktueller X-Wert
    double         Xend;             // Endwert X
    double         Xold;
    double         Xd;
    double         Dx;
    double         Safe;             // Sicherheitsfaktor Zeitschrittberechnung
    double         Fac1;             // parameters for step size selection, wenn 0.0, dann default fac1=0.2 and fac2=10.0
    double         Fac2;             // fac1 <= hnew/hold <= fac2.
    double         Alphn;
    double         Betan;
  public:
    const CMessage &Message;

    inline CSolveODEBase() 
      :mMessage("SlfSolveODE")
      ,Message(mMessage)
      ,PObj(0)
      ,N(0)
      ,Nmax(100000)
      ,Y()
      ,H(0.)
      ,H1(0.)
      ,Hmax(0.)
      , Hold(0.)
      , Uround(2.3E-16)
      , Posneg(1.)
      , VAtol()
      , VRtol()
      , Atol(0.)
      , Rtol(0.)
      , Itoler(0)
      , Meth(1)
      , Norder(0)
      , Nfcn(0)
      , Nstep(0)
      , NstepMax(0)
      , NstepAccept(0)
      , Njac(0)
      , X(0.0)
      , Xend(0.0)
      , Xold(0.0)
      , Xd(0.0)
      , Dx(0.001)
      , Safe(0.9)
      , Fac1(0.2)
      , Fac2(10.0)
      , Alphn(0.0)
      , Betan(0.0)
    { }
    virtual ~CSolveODEBase() { }

    virtual okay_t init(CIntegratorBaseInp &inp) { return OK; } /* Prototype init */

    virtual okay_t calcFirst( double x       // Startzeit, x-Wert
                            , CVectorD ystart  // Startzustände
                            , double hstart  // Startschrittweite (kann 0.0 sein)
                            , double posneg = 1. // Zeitachse/x-Achse positiv (1.) oder negativ (-1.)
                            )
    {
      return OK;
    } /* Prototype  */

    virtual okay_t calc(double xend) { return OK; } /* Prototype calc */
    virtual void reset(void){ mMessage.Clear(); }  // Löschen von Speicher und zurücksetzen der Variablen

    uint32_t nfcnRead(void){ return Nfcn; }
    uint32_t nstepRead(void){ return Nstep; }
    uint32_t naccptRead(void){ return NstepAccept; }
    double hRead (void){ return H; }
    double xRead (void){ return X; }
    uint32_t norderRead(void){ return Norder; }
    uint32_t methRead(void){ return Meth; }
    uint32_t njacRead(void){ return Njac; }

    okay_t   getOkay(void) { return mMessage.IsOkay(); }

    //// setzt alle Input-Werte auf null bzw. default
    ////=============================================
    //void SetDefaultInp(SIntegratorBaseInp *pinp)
    //{


    //  pinp->rad.ijac = 1;
    //  pinp->rad.mljac = 0;
    //  pinp->rad.mujac = 0;
    //  pinp->rad.imas = 0;
    //  pinp->rad.mlmas = 0;
    //  pinp->rad.mumas = 0;

    //  pinp->rad.ihesse = 0;
    //  pinp->rad.nit = 7;
    //  pinp->rad.startn = 0;
    //  pinp->rad.nind1 = 0;
    //  pinp->rad.nind2 = 0;
    //  pinp->rad.nind3 = 0;
    //  pinp->rad.pred = 1;
    //  pinp->rad.m1 = 0;
    //  pinp->rad.m2 = 0;
    //  pinp->rad.nsmin = 3;
    //  pinp->rad.nsmax = 7;
    //  pinp->rad.nsus = 3;

    //  pinp->rad.uround = 1.0e-16;
    //  pinp->rad.safe = 0.9;
    //  pinp->rad.thet = 0.001;
    //  pinp->rad.quot1 = 1.0;
    //  pinp->rad.quot2 = 1.2;
    //  pinp->rad.fac1 = 0.2;
    //  pinp->rad.fac2 = 10.;
    //  pinp->rad.vitu = 0.002;
    //  pinp->rad.vitd = 0.8;
    //  pinp->rad.hhou = 1.2;
    //  pinp->rad.hhod = 0.8;

    //  pinp->lsoda.ijac = 1;
    //  pinp->lsoda.mljac = 0;
    //  pinp->lsoda.mujac = 0;
    //  pinp->lsoda.mxordn = 12;
    //  pinp->lsoda.mxords = 5;

    //  pinp->gear.meth = 1;
    //  pinp->gear.nit = 20;
    //  pinp->gear.njacmax = 5;
    //  pinp->gear.ijac = 1;

    //}

  };
}
#endif