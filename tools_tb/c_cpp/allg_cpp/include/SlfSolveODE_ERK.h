// DEF_DOPRI45    expliziter Runge-Kutta, Dormand & Prince 4(5) Ordnung
//
#ifndef SLF_SOLVER_ODE_ERK_H_INCLUDED
#define SLF_SOLVER_ODE_ERK_H_INCLUDED

#include "SlfSolveODE.h"

namespace slf
{

  /* explizites Runge-Kutta-Verfahren
     =================================

     Beispiel Implementierung
 
     // Inputstruktur erstellen
     //
     SIntegratorBaseInp inp;

     // Objektklasse ERK erstellem und Initialisieren von inp, d.h genullt
     //
     pIntFunc = new CSolveODE_ERK(&inp);

     // allgemeinen Inputparameter
     inp.n       = 2;         // (must)(uint32_t) Anzahl der Zustände
     inp.ystate  = vstate;    // (defualt)(CVectorD) Zustandsvektor Vector vstate
                              // Wenn null bzw. nicht gesetzt, dann wird intern ein Zustandsvektor erstellt
                              // über get-Funktion bekommt man die Zustände
     inp.itol    = 1;         // (must)(uint8_t) = 0: skalare Toleranz/Fehlerschranke
                              //        = 1; vektorielle Toleranz/Fehlerschranke (pro Zustand)
     inp.vatol   = VError;    // (must,itol=1)(Vector) vektorieller absoluter Fehler (Länge inp.n) 
     inp.vrtol   = VError;    // (must,itol=1)(Vector) vektorieller relativer Fehler (Länge inp.n) 
     inp.atol    = 1.e-7;     // (must,itol=0)(double) skalare absoluter Fehler (Länge inp.n) 
     inp.rtol    = 1.e-7;     // (must,itol=0)(double) skalare relativer Fehler (Länge inp.n)
     inp.hmax    = 0.0        // (default) maximale Schrittweite,
                              // wenn 0.0, dann xend-x
     inp.nmax    = 100000     // (default=100000) maximale Anzahl von Schritten
                              // wenn nicht besetzt

     // runge-kutta-spezifische Inputparameter
     inp.erk.meth   = 1;   // (must)   Methode =1: Dormand-Prince 4(5) mit Schrittweitensteuerung
     inp.erk.nstiff = -1;  // (default=-1)Wenn < 0, dann keine Abfrage ob System zu steif
                              // Test for stiffness is activated when the current step 
                              // number is a multiple of nstiff
     inp.erk.uround = 0.0; // (default=2.3E-16) Rundungsfehler
     inp.erk.safe   = 0.0; // (default=0.9) Sicherheitfaktor < 1.0 zur Schrittweitensteuerung
     inp.erk.fac1   = 0.2; // (default=0.2) parameters for step size selection
     inp.erk.fac2   = 10.; // (default=10.) parameters for step size selection
                              // grenzt Änderung ein fac1 <= hnew/hold <= fac2
     inp.erk.beta   = 0.02;// (default=0.04) for stabilized step size control

  ======================================================================================*/
  class CIntegratorERKInp  : public CIntegratorBaseInp    // Parameter expliziter RungeKutta
  {
  public:
    uint32_t        n;
    uint8_t meth;             // (1)        switch for the choice of the coefficients 1:dopri45
    int32_t nstiff;           // (-1)       test ob system steif ist, -1 kein test
    double uround;            // (2.3e-16)  rounding unit, wenn 0.0, dann default 2.3E-16
    double safe;              // (0.9)      safety factor, wenn 0.0, dann default 0.9
    double fac1;              // (0.2)      parameters for step size selection, wenn 0.0, dann default fac1=0.2 and fac2=10.0
    double fac2;              // (10.)      fac1 <= hnew/hold <= fac2.
    double beta;              // (0.04)     for stabilized step size control, wenn 0.0, dann default beta=0.04

    CIntegratorERKInp() :CIntegratorBaseInp()
      , meth(1)
      , nstiff(-1)
      , uround(2.3e-16)
      , safe(0.9)
      , fac1(0.2)
      , fac2(10.)
      , beta(0.04) {}
  };

  class CSolveODE_ERK : public CSolveODEBase 
  {
  private:

    CVectorD    YY1[INTEGRATOR_KMAX];  // Zwischengröße y
    CVectorD    YY2;               // Zwischengröße y
    CVectorD    K[INTEGRATOR_KMAX]; // K-Werte Runge-Kutta
    double      dNf, dNy, Sqr, Sk;  // Fehlerabschätzung
    double      Der2, Der12;      // Zwischen größen Ableitung

    double      Beta;             // for stabilized step size control, wenn 0.0, dann default beta=0.04
    double      Facold;
    double      Facc1;
    double      Facc2;
    double      Expo1;

    int32_t     Nstiff;           // > 0 Test auf Steifheit wenn Nstiff ein mehrfaches von Naccept

#if 0
    uint32_t    *pIndir;          // bei dense output
    double    *pRcont1;
    double    *pRcont2;
    double    *pRcont3;
    double    *pRcont4;
    double    *pRcont5;
#endif

    uint32_t    Iord;             // höchste Ordnung
    uint32_t    Nstufen;          // Stufigkeit
    CVectorD    C;                // Koeffizienten
    CVectorD    D;                // 
    CVectorD    E;                // 
    CMatrixD    A;                // Koeffizientenmatrix
  public:

    CSolveODE_ERK() 
      :CSolveODEBase() 
      ,YY2()
      ,A()
      ,E()
      ,C()
      ,D()
    {

      for (uint8_t i = 0; i < INTEGRATOR_KMAX; i++) {
        K[i].clear();
        YY1[i].clear();
      }

    }
    ~CSolveODE_ERK() {

      reset();
    }
    void reset(void) {

      VAtol.clear();
      VRtol.clear();

      for (uint8_t i = 0; i < INTEGRATOR_KMAX; i++) {
        K[i].clear();
        YY1[i].clear();
      }
      YY2.clear();
      Y.clear();
      C.clear();
      A.Clear();
      D.clear();
      E.clear();
    }


      okay_t init(CIntegratorERKInp &inp);
      okay_t calcFirst(const double &x         // Startzeit, x-Wert
                      ,const CVectorD &ystart  // Startzustände
                      ,const double &hstart    // Startschrittweite (kann 0.0 sein)
                      ,const double posneg=1.  // Zeitachse/x-Achse positiv (1.) oder negativ (-1.)
                       );
      okay_t calc(double &xend);     // Rechne bis xend
      const CVectorD &getState(void) { return Y; }
    

  private:

      // Erste Schrittweite
      //===================
      double  hinit(double &x, CVectorD &y,double &posneg);

  };
}
#endif