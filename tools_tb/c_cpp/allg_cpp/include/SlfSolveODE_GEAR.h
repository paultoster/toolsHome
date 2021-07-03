// DEF_GEARS      implizites Gears-Mehrschrittverfahren
//
#ifndef SLF_SOLVER_ODE_GEAR_H_INCLUDED
#define SLF_SOLVER_ODE_GEAR_H_INCLUDED
#include "SlfSolveODE.h"

namespace slf
{

  // einfache implizite Verfahren ohne Schrittweitensteuerung
  //=========================================================
  //
  // inp.n            (?) Anzahl Zust‰nde
  // inp.pclass       (?) Klassenobjekt zur Berechnung der Ableitung und jacobi
  // inp.ytstate      (0) Zustandsvektor (wenn =0, muﬂ wird Zustand intern gbildet
  // inp.itol         (0) itol=0, skalar err(y[i]) < atol
  //                      itol=1, vector err(y[i]) < atol[i]
  // inp.atol;        (?) absolute Toleranz itol = 0
  // inp.vatol        (?) absolute Vektor Toleranz itol = 1
  // inp.hmax         (0.0) maximal step size default=xend-x
  //
  // inp.gear.meth = (1): teilimplizit Euler
  //                  2:   teilimplizit Gear
  //                  3:   implizit Euler
  //                  4:   implizit Gear
  // inp.gear.nit    (20) maximale Iteration pro Schritt
  // inp.gear.njac   (5) Berechnung Jakobimatrix nach wievielten Schritt 
  //                      
  // inp.gear.ijac   (1)  Berechnung der Jakobimatrix

  // Inputstruktur
  //==============
  struct CIntegratorGEARInp : public CIntegratorBaseInp // Parameter Gear implizit Euler and partial 
  {
    uint8_t  meth;            // (1)        switch for the choice of the coefficients 
                              //            1:paritial impl. Euler 2 : Paratial Gear
                              //            3:implicit Euler       4 : Gear
    uint32_t njacmax;         // (5)        after how many iteration to calculate Jacobi
    uint32_t nit;             // (20)       max iteration per time step
    uint8_t  ijac;            // (1)        1:calculation jacobi analytically (wih function)
                              //            0: numerically
    CIntegratorGEARInp() :CIntegratorBaseInp()
      , meth(1)
      , njacmax(5)
      , nit(20)
      , ijac(1)
    {}

  };

  class CSolveODE_GEAR : public CSolveODEBase {
  private:


    EErrNo Ierr;

    uint32_t  Nsteps;

    uint32_t  Nit;
    uint32_t  Njacmax;

    uint8_t   Ijac;

    CMatrixD  FjacMat;
    CVectorD  VStaDer;
    CVectorD  VStaDer0;
    CVectorD  VSta0;
    CVectorD  VStam1;
    CVectorD  VStam2;

    CVectorD  Vec1;
    CVectorD  Vec;
    CVectorD  Delta;

    CMatrixD InvMat;
    CMatrixD InvMatOld;

    CVectorD  B;

    double Afac;

    double Hact;
  public:

    CSolveODE_GEAR()
      :CSolveODEBase()
      , Ierr(NO_ERR)
      , FjacMat()
      , VStaDer()
      , VStaDer0()
      , VSta0()
      , VStam1()
      , VStam2()
      ,Vec1()
      ,Vec()
      ,Delta()
      ,InvMat()
      ,InvMatOld()
      ,B()
    {
    }
    CSolveODE_GEAR::~CSolveODE_GEAR()
    {
    }
    void reset(void)
    {
      Ierr = NO_ERR;

      N = 0;
      PObj = 0;

      Y.clear();
      FjacMat.Clear();

      VStaDer.clear();
      VStaDer0.clear();
      VSta0.clear();
      VStam1.clear();
      VStam2.clear();

      Vec1.clear();
      Vec.clear();
      Delta.clear();

      InvMat.Clear();
      InvMatOld.Clear();

      B.clear();
      Nit = 10;

      X = 0.0;
      Posneg = 1.0;
      H = 0.0;
      Hact = 0.0;

      Uround = 2.3E-16;

    }


      okay_t init(CIntegratorGEARInp *pinp);
      
      okay_t calcFirst( const double &x       // Startzeit, x-Wert
                      , const CVectorD &ystart // Startzust‰nde
                      , const double &hstart  // Startschrittweite (kann 0.0 sein)
                      , double posneg=1. // Zeitachse/x-Achse positiv (1.) oder negativ (-1.)
                      );
      okay_t calc(double xend);     // Rechne bis xend
    

      EErrNo get_ierr(void){return Ierr;}
  private:

      okay_t calcInverseMaterix(void);

  };
}
#endif