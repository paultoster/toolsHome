#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SlfSolveODE_RAD5.h"      
#include "SlfNum.h"
#include "SlfNumDecSol.h"
//#include <iostream>
namespace slf
{
  /* Common Block Declarations */

//  struct {
//    int32_t nn, nn2, nn3, nn4;
//    double xsol, hsol, c2m1, c1m1;
//  } conra5_;
//
//#define conra5_1 conra5_
//
//  struct {
//    int32_t Mle, mue, mbjac, mbb, mdiag, mdiff, mbdiag;
//  } linal_;
//
//#define linal_1 linal_
//
//    
//  /* Table of constant values */
//
//  static int32_t c__9 = 9;
//  static int32_t c__1 = 1;
//  static int32_t c__5 = 5;
//  static int32_t c__3 = 3;
//  static double c_b100 = .5;
//  static double c_b215 = 81.;
//  static double c_b216 = .33333333333333331;
//  static double c_b217 = 9.;
//  static double c_b227 = 1.;
//  static double c_b325 = .8;
//  static double c_b389 = .25;



  CSolveODE_RAD5::CSolveODE_RAD5(void) {
    
      Ierr   = NO_ERR;    

      NstepMax = NstepAccept = 0;
    
      N    = 0;
      Y    = 0;

      Itoler= 0;
      VAtol.clear();
      VRtol.clear();

      Hmax = 0.0;
      Nmax = 100000;

      X      = 0.0;
      Posneg = 1.0;
      H      = 0.0;


      Reject = false;
      First = true;
      Caljac = true;
      Calhesse = true;
      Err = 0.;
      Iout = 0;

      reset();
  }
  CSolveODE_RAD5::~CSolveODE_RAD5() {
    
      reset();
  }
  void CSolveODE_RAD5::reset(void) {
    
    VAtol.clear();
    VRtol.clear();
    Y.clear();
    Y0.clear();
    Scal.clear();
    Fjac.Clear();
    Fmas.Clear();
    FjacMat.Clear();
    FmasMat.Clear();
    Cont.clear();
    E1.Clear();
    E2i.Clear();
    E2r.Clear();

    Ip1.clear();
    Ip2.clear();
    Iphes.clear();        
  }
  //=================================================
  // Init-Funktion
  //=================================================
  okay_t CSolveODE_RAD5::init(CIntegratorRAD5Inp &inp) 
  {  
    CStr errtext;
    //=================
    //Anzahl Zustände N
    //=================
      if( inp.n >= MAX_SINT32 ) {
        
          errtext.catFormat("CSolveODE_RAD::init: Anzahl der Zustände zu groß, maximal %li möglich", MAX_SINT32 - 1);
          mMessage.SetErr(NMAX_REACHED, errtext.c_str());
          return mMessage.IsOkay();
      } 
      else if( inp.n == 0 ) 
      {
          mMessage.SetErr(N_IS_ZERO,"CSolveODE_RAD::init: Anzahl der Zustände n gleich null");
          return mMessage.IsOkay();
      } 
      else 
      {
        
          N = (size_t)inp.n;
      }

      // Funktionspointer
      //=================
      if (inp.pobj == 0) {
        mMessage.SetErr(FUNCTION_POINTER_NOT_SET, "Funktionspointer inp.state == 0\n");
        return mMessage.IsOkay();
      }
      PObj = inp.pobj;                  //



      //===============
      // Zustandsvektor
      //===============
      Y.resize(N);
      Y.FillZero();

      //if (itoler == 0) {
      //  if ((atoler[0] <= 0.0) || (rtoler[0] <= 10.0*uround)) {
      //    std::cout << " tolerances are too small" << std::endl;
      //    throw - 1;
      //  }
      //  else {
      //    double quot = atoler[0] / rtoler[0];
      //    rtoler[0] = 0.1*pow(rtoler[0], 2.0 / 3.0);
      //    atoler[0] = rtoler[0] * quot;
      //  }
      //}
      //else {
      //  for (i = 0; i < n; i++) {
      //    if ((atoler[i] <= 0.0) || (rtoler[i] <= 10.0*uround)) {
      //      std::cout << " tolerances('" << i << ") are too small" << std::endl;
      //      throw - 1;
      //    }
      //    else {
      //      double quot = atoler[i] / rtoler[i];
      //      rtoler[i] = 0.1*pow(rtoler[i], 2.0 / 3.0);
      //      atoler[i] = rtoler[i] * quot;
      //    }
      //  }
      //}
      //==========
      // Toleranz 
      //==========
      if( inp.itol == 0 ) {//skalar

          Itoler = 0;
          VAtol.resize(1);
          VRtol.resize(1);

          VAtol[0] = fabs(inp.atol);
          VRtol[0] = fabs(inp.rtol);

          if (VAtol[0] <= 0.0) {
            mMessage.SetErr(VALUE_IS_ZERO, "Skalar absolute Error atol must be set > 0.0 \n");
            return mMessage.IsOkay();
          }
          if (VRtol[0] <= 0.0) {
            mMessage.SetErr(VALUE_IS_ZERO, "Skalar relative Error rtol must be set > 0.0 \n");
            return mMessage.IsOkay();
          }


      } else { // Vektor

          size_t i;

          Itoler = 1;
          if (inp.vatol.size() == 0)
          {
            mMessage.SetErr(VECTOR_IS_ZERO, "Vektor absolute Error must be set\n");
            return mMessage.IsOkay();
          }
          if (inp.vrtol.size() == 0)
          {
            mMessage.SetErr(VECTOR_IS_ZERO, "Vektor relative Error must be set\n");
            return mMessage.IsOkay();
          }
          VAtol.Copy(inp.vatol);
          VRtol.Copy(inp.vrtol);

          VAtol.MakeAbs(); // Absolutwert
          VRtol.MakeAbs();

          if( VAtol.size() < (std::size_t)N ) {
            errtext.catFormat("Vektor length:%i absolute Error must be equal N:%i\n"
              , VAtol.size(), N);
            mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
            return mMessage.IsOkay();
          }

          for(i=0;i<N;i++) {

              if( VAtol[i] <= 0.0 ) {
                errtext.catFormat("CVectorD %i absolute Error must be set > 0.0 \n", i);
                mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
                return mMessage.IsOkay();
              }
          }
          if( VRtol.size() < (std::size_t)N ) {
              errtext.catFormat("CSolveODE_RAD::init: Vektor length:%i relative Error must be equal N:%i\n"
                               ,VRtol.size()
                               ,N);
              mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
              return mMessage.IsOkay();
          }
          for(i=0;i<N;i++) {
              if( VRtol[i] <= 0.0 ) {
                  errtext.catFormat("CSolveODE_RAD::init: CVectorD %i relative Error must be set > 0.0 \n",i);
                  mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
                  return mMessage.IsOkay();
              }
          }

      }
    
      //===================
      // maximal step size 
      //===================
      Hmax = inp.hmax;
      Hmax = fabs (Hmax);

      // Abfrage siehe Uraound
    

      //===================
      // maximale Iteration
      //===================
      Nmax = inp.nmax;

      if( Nmax == 0 ) 
      {

        errtext.catFormat("CSolveODE_RAD::init: Nmax == 0 \n");
        mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
        return mMessage.IsOkay();
      }
    
      //=================================
      // meth, methode
      //=================================
      Meth = 1; // im Moment nur eine Methode
    
      //=================================
      // Wird Jacobimatrix analytisch berechnet
      //=================================
      Ijac = (inp.ijac != 0);



      //=================================
      // Band der Jacobimatrix
      // Mljac lower bandwidth
      // Mljac = n  volle Matrix
      // 0<=Mljac<n lower bandwidth unteres Band
      // Mujac upper bandwidth, if Mljac < n 
      //=================================
      if( inp.mljac == 0 )
          Mljac = N;
      else
          Mljac = (size_t)inp.mljac;

      if( inp.mujac == 0  )
          Mujac = N;
      else
          Mujac = (size_t)inp.mujac;


      //=================================
      // Wird Massenmatrix berechnet
      //=================================
      Imas = inp.imas;

      //========================================
      // Funktionspointer Mass, wenn notwendig
      //========================================

      //=================================
      // Band der Jacobimatrix
      // Mljac lower bandwidth
      // Mljac = n  volle Matrix
      // 0<=Mljac<n lower bandwidth unteres Band
      // Mujac upper bandwidth, if Mljac < n 
      //=================================
      Mlmas = inp.mumas;

      if( Mlmas == 0 )
          Mlmas = SLF_MIN(Mljac,N);

      Mumas = inp.mumas;

      if( Mumas == 0 )
          Mumas = SLF_MIN(Mujac,N);
     
      //================================
      // Umwandlung in Hessenberg-Matrix
      //================================
      Ihesse = inp.ihesse;
    
      //================================
      // maximale Newtoniteration
      //================================
      Nit = inp.nit;

      if( Nit == 0 ) {
          errtext.catFormat("CSolveODE_RAD::init: Nit == 0 \n");
          mMessage.SetErr(N_IS_ZERO, errtext.c_str());
          return mMessage.IsOkay();
      }

      //================
      // Startmethode
      //================
      Startn = inp.startn;

      //===================================
      // Differentialalgebraische Variablen
      // und Index1  ... Index3 festlegen
      //===================================
      Nind1 = inp.nind1; //Index1 Variable
      Nind2 = inp.nind2; //Index1 Variable
      Nind3 = inp.nind3; //Index1 Variable

      if( Nind1 == 0 )
          Nind1 = N;

      if( Nind1+Nind2+Nind3 != N ) {
          
          errtext.catFormat("CSolveODE_RAD::init: Nind1+Nind2+Nind3 != N=%i \n",N);
          mMessage.SetErr(UNKOWN_ERR, errtext.c_str());
          return mMessage.IsOkay();
      }

      //===================
      // switch for step size
      // strategy
      //===================
      Pred = inp.pred;


      //============================
      // spezielles Differntialsystem
      // Y[i]' = Y[i+M2]   FOR  i=1,...,M1,
      //============================
      M1 = inp.m1;
      M2 = inp.m2;

      Nm1 = N-M1;

      if( M1 == 0 ) M2 = N;
      if( M2 == 0 ) M2 = M1;

      if( M1+M2 > N ) {
          errtext.catFormat("CSolveODE_RAD::init: M1+M2 > N =%i \n",N);
          mMessage.SetErr(UNKOWN_ERR, errtext.c_str());
          return mMessage.IsOkay();
      }


      //==============================
      // minimale//maximale/start Stufenzahl
      //==============================
      //Nsmin = inp.nsmin;
      //Nsmax = inp.nsmax;
      //Ns    = inp.nsus;

      //if( Nsmin == 0 )
      //    Nsmin = 3;
      //if( Nsmin >= 2 )
      //    Nsmin = SLF_MAX(3,Nsmin);
      //if( Nsmin >= 4 )
      //    Nsmin = SLF_MAX(5,Nsmin);
      //if( Nsmin >= 6 )
      //    Nsmin = 7;

      //if( Nsmax == 0 )
      //    Nsmax = 7;

      //Nsmax = SLF_MIN(7,Nsmax);

      //if( Nsmax <= 6 )
      //    Nsmax = SLF_MIN(5,Nsmax);
      //if( Nsmax <= 4 )
      //    Nsmax = SLF_MIN(3,Nsmax);
      //if( Nsmax <= 2 )
      //    Nsmax = 1;

      //if( Ns == 0 ) 
      //    Ns = Nsmin;

      //=====================================================
      // uround, smallest number satisfying 1.0+uround > 1.0
      //=====================================================
      Uround = inp.uround;
    
      if (Uround <= 1.0E-19 )
      {
          errtext.catFormat("CSolveODE_RAD::init: Which machine do you have ? Your uround was : %.16e\r\n", Uround);
          mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
      }
      else if (Uround >= 1.0)
      {
        errtext.catFormat("CSolveODE_RAD::init: Which machine do you have ? Your uround was : %.16e\r\n", Uround);
        mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
        return mMessage.IsOkay();

      }
    
      // Hmax abprüfen
      if( Hmax <= Uround ) {

          Hmax = 1e30; // wird später mit einer Minfkt auf xend-x gekürzt
          //Status = NOT_OKAY;
          //ErrText.catFormat("CSolveODE_RAD::init: Hmax <= Uround \n");
          //return Status;
      }

      //===============================
      // Toleranz mit Uround überprüfen 
      //===============================
      if( Itoler == 0 ) {//skalar

        if(VRtol[0] <= Uround*10. ) 
        {
            errtext.catFormat("CSolveODE_RAD::init: Skalar relative Error must be set > 10*Uround (Rtol(VRtol[0]):%g, Uround:%g) \n", VRtol[0],Uround);
            mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
            return mMessage.IsOkay();
        }
          
        double quot = VAtol[0] / VRtol[0];
        VRtol[0] = 0.1*pow(VRtol[0], 2.0 / 3.0);
        VAtol[0] = VRtol[0] * quot;
        

      } else { // Vektor

          size_t i;


          for(i=0;i<N;i++) {
              if( VRtol[i] <= Uround*10. ) {
                  errtext.catFormat("CSolveODE_RAD::init: CVectorD %i relative Error must be set > 10*Uround (VRtol[i]:%g,Uround:%g) \n",i, VRtol[i],Uround);
                  mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
                  return mMessage.IsOkay();
              }
              double quot = VAtol[i] / VRtol[i];
              VRtol[i] = 0.1*pow(VRtol[i], 2.0 / 3.0);
              VAtol[i] = VRtol[i] * quot;
          }

      }

      //==============
      // safety factor
      //==============
      Safe = inp.safe;
    
      if (Safe <= 1.0E-3) 
      {
          errtext.catFormat("CSolveODE_RAD::init: Safe muß zwischen 1.0E-4 und 1.0 liegen safe = %g",Safe);
          mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
      }
      else if (Safe >= 1.0)
      {
        errtext.catFormat("CSolveODE_RAD::init: Safe muß zwischen 1.0E-4 und 1.0 liegen safe = %g", Safe);
        mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
        return mMessage.IsOkay();
      }


      //===========================================================
      // stopping criterion for Newton's method, usually chosen < 1
      //===========================================================
      Fnewt = inp.fnewt;
      if (Fnewt == 0.0) Fnewt = SLF_MAX((10.0*Uround / VRtol[0]), SLF_MIN(0.03, sqrt(VRtol[0])));
      if (Fnewt <= Uround / VRtol[0])
      {
        errtext.catFormat("CSolveODE_RAD::init: curious input for fnewt = %g", Fnewt);
        return mMessage.IsOkay();
      }

    
      //==========================
      // Stepchange factor
      // Quot1 < Hnew/Hold < Quot2
      //==========================
      Quot1 = inp.quot1;
      Quot2 = inp.quot2;
    
      if( Quot1 > 1.0 ) {
          errtext.catFormat("CSolveODE_RAD::init: Quot1 muß kleinergleich 1.0 sein Quot1 = %g",Quot1);
          mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
          return mMessage.IsOkay();
      }
      if( Quot2 < 1.0 ) {
          errtext.catFormat("CSolveODE_RAD::init: Quot2 muß größergleich 1.0 sein Quot2 = %g",Quot1);
          mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
      }

      //=========================
      // cost factor Jacobimatrix
      //=========================
      Thet = inp.thet;
      if (Thet >= 1.0)
      {
        errtext.catFormat("CSolveODE_RAD::init: Thet muß kleiner 1.0 liegen thet = %g", Thet);
        mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
        return mMessage.IsOkay();
      }

      //===============================================
      // Fac1, Fac2, parameters for step size selection
      // Fac1 <= Hnew/Hold <= Fac2
      //===============================================
      Fac1 = inp.fac1;
      Fac2 = inp.fac2;
      Facl  = 1.0 / Fac1;
      Facr  = 1.0 / Fac2;
    
      if( Facl < 1.0 ) {
          errtext.catFormat("CSolveODE_RAD::init: Facl = 1/Fac1 muß größergleich 1.0 sein Fac1 = %g",Fac1);
          mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
      }
      if( Facr > 1.0 ) {
          errtext.catFormat("CSolveODE_RAD::init: Facr = 1/Fac2 muß kleinergleich 1.0 sein Fac1 = %g",Fac1);
          mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
          return mMessage.IsOkay();
      }

      //=======================================
      // contravtivity factor increase/decrease
      //=======================================
      Vitu = inp.vitu;
      Vitd = inp.vitd;

      //==========================
      // Order is decreased if
      // Hhod <= Hnew/Hold <= Hhou
      //==========================
      Hhou = inp.hhou;
      Hhod = inp.hhod;


      //=============
      // Massenmatrix
      //=============
      Implct = Imas; 
      
      //=============
      // Jacobimatrix
      //=============
      Banded = (Mljac < Nm1);

      if( Banded ) 
      {
          Ldjac = Mljac+Mujac+1;
          Lde1  = Mljac+Ldjac;
      } else {
          Mljac = Nm1;
          Mujac = Nm1;
          Ldjac = Nm1;
          Lde1  = Nm1;
      }


      if( Implct ) 
      {
        if( Mlmas != Nm1 ) 
        {
          Ldmas = Mlmas + Mumas+1;
          if( Banded ) Ijob = 4;
          else         Ijob = 3;
        } 
        else 
        {
          Mumas = Nm1;
          Ldmas = Nm1;
          Ijob  = 5;
        }

        if( Mlmas > Mljac || Mumas > Mujac ) {
            errtext.catFormat("CSolveODE_RAD::init: bandwidth of mass not snmaller than bandwidth of Jacobian\n");
            mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
            return mMessage.IsOkay();
        }
      } 
      else 
      {
        Ldmas = 0;
        if( Banded ) 
          Ijob = 2;
        else 
        {
           Ijob = 1;
           if( N > 2 && Ihesse )
             Ijob = 7;
        }
      }
      Ldmas = SLF_MAX(1,Ldmas);
    
      if( (Implct || Banded) && Ijob == 7 ) {
          errtext.catFormat("CSolveODE_RAD::init: Hessebergoptions only for explicit equations with full Jacobian\n");
          mMessage.SetErr(UNKOWN_ERR, errtext.c_str());
          return mMessage.IsOkay();
      }


      //===========================================
      //NNsmax   = N*Nsmax;
      //Nm1Nsmax = Nm1*Nsmax;
      //Nmee     = (Nsmax-1)*Nm1;


      //=============
      // Work-Buffers
      //=============
      //z1 = new double[n];
      //z2 = new double[n];
      //z3 = new double[n];
      //y0 = new double[n];
      //scal = new double[n];
      //f1 = new double[n];
      //f2 = new double[n];
      //f3 = new double[n];
      //cont = new double[4 * n];
      //ip1 = new int[nm1];
      //ip2 = new int[nm1];
      //iphes = new int[n];
      Z1.resize(N);
      Z2.resize(N);
      Z3.resize(N);
      Y0.resize(N);
      Scal.resize(N);
      F1.resize(N);
      F2.resize(N);
      F3.resize(N);

      Fjac.SetNdim(Ldjac,N);
      Fmas.SetNdim(Ldmas, Nm1);

      // Matrix für die Berechnung in den Funktionen
      FjacMat.SetNdim(Ldjac,N);
      FmasMat.SetNdim(Ldmas,Nm1);

      //Cont.resize(NNsmax+N);
      Cont.resize(4 * N);

      E1.SetNdim(Lde1,Nm1);
      E2i.SetNdim(Lde1, Nm1);
      E2r.SetNdim(Lde1, Nm1);

      Ip1.resize(Nm1);
      Ip2.resize(Nm1);
      Iphes.resize(Nm1);

      // Ausgabe solout
      //===============
      Iout = inp.iout;

      // Startwert Idid
      //===============
      Idid = 1;

      Reject = false;
      First = true;
      Caljac = true;
      Calhesse = true;
      Err = 0.;

      // Define constants used in linear algebra routines
      Mle = Mljac;
      Mue = Mujac;
      Mbjac = Mljac + Mujac + 1;
      Mbb = Mlmas + Mumas + 1;
      Mdiag = Mle + Mue;
      Mdiff = Mle + Mue - Mumas;
      Mbdiag = Mumas + 1;

      Hnext = Hmax;

      return OKAY;
    
  }
  okay_t CSolveODE_RAD5::calcFirst(double &x
                                ,CVectorD &ystart
                                ,double &hstart
                                ,double posneg/*=1*/
                                ) 
  {

      size_t i;
      CStr errtext;

      // Startpunkt
      //===========
      X = x;

      // Startschrittweite
      //==================
      H = fabs(hstart);
      if (H < 10.0*Uround) H = 1.0e-6;
      // in welche Richtung rechnen wir (zeitlich positiv)
      //==================================================
      Posneg =  SLF_SIGN(posneg,double);
      Hmax   *= Posneg;
      H      *= Posneg;
      Hnext = H;


      // Initialisierungswerte
      //======================
      if( ystart.size() != N ) {

          errtext.catFormat("Initialisierungsvektor ystart(n) = %i, entspricht nicht vorgegebenen Statelänge N = %i\n", ystart.GetNdim(), N);
          mMessage.SetErr(VECTOR_LENGTH, errtext.c_str());
          return mMessage.IsOkay();
      }
      for (i = 0; i < N; i++)
        Y[i] = ystart[i];

      First = true;

      return OKAY;
  }
  okay_t CSolveODE_RAD5::calc(double xend) 
  {
    CStr errtext;

    double hmaxn = SLF_MIN(SLF_ABS(Hmax,double),SLF_ABS((xend-X),double));

    Nfcn=Njac=Nstep=Naccpt=Nrejct=Ndec=Nsol=0;

    Reject = false;
    //First = true;


       
      if( NstepMax < Nstep )
          NstepMax = Nstep;

    
      Integrate(xend);

  /*
  C     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
  C                   IDID= 1  COMPUTATION SUCCESSFUL,
  C                   IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUT)
  C                   IDID=-1  INPUT IS NOT CONSISTENT,
  C                   IDID=-2  LARGER NMAX IS NEEDED,
  C                   IDID=-3  STEP SIZE BECOMES TOO SMALL,
  C                   IDID=-4  MATRIX IS REPEATEDLY SINGULAR.
  C                   IDID=-5  Failure in state-Calculation.
  C                   IDID=-6  Failure in jacobi-Calculation.
  C                   IDID=-7  Failure in mas-Calculation.

  =>                  Ierr    = 0  COMPUTATION SUCCESSFUL,
                              = INPUT_NOT_CONSISTENT  INPUT IS NOT CONSISTENT,
                              = NMAX_REACHED  LARGER NMAX IS NEEDED,
                              = STEPSIZE_TOO_SMALL  STEP SIZE BECOMES TOO SMALL,
                              = MATRIX_IS_SINGULAR  MATRIX IS REPEATEDLY SINGULAR.
                              = STATE_NO_SUCCESS  Functioncall state was not successful
                              = JACOBI_NO_SUCCESS  Functioncall jacobi was not successful
                              = MASS_NO_SUCCESS  Functioncall mass was not successful
  */

      if (Idid < 0) 
      {
        CStr txt;
        txt.catFormat("exit of RADAU5 at x = %f\n", X);
        mMessage.SetLog(txt);
      }

      Ierr = NO_ERR;
      if( Idid == -1 ) {
          
        Ierr   = INPUT_NOT_CONSISTENT;

        mMessage.SetErr(INPUT_NOT_CONSISTENT, "INPUT IS NOT CONSISTENT");
        return mMessage.IsOkay();

      } else if( Idid == -2 ) {
          Ierr   = NMAX_REACHED;
          mMessage.SetErr(NMAX_REACHED, "LARGER NMAX IS NEEDED");
          return mMessage.IsOkay();
      } else if( Idid == -3 ) {
          Ierr   = STEPSIZE_TOO_SMALL;
          mMessage.SetErr(STEPSIZE_TOO_SMALL, "STEP SIZE BECOMES TOO SMALL");
          return mMessage.IsOkay();
      } else if( Idid == -4 ) {
          Ierr   = MATRIX_IS_SINGULAR;
          mMessage.SetErr(MATRIX_IS_SINGULAR, "STEP SIZE BECOMES TOO SMALL");
          return mMessage.IsOkay();
      } else if( Idid == -5 ) {
          Ierr   = STATE_NO_SUCCESS;
          mMessage.SetErr(STATE_NO_SUCCESS, "Failure in state-Calculation");
          return mMessage.IsOkay();
      } else if( Idid == -6 ) {
          Ierr   = JACOBI_NO_SUCCESS;
          mMessage.SetErr(JACOBI_NO_SUCCESS, "Failure in jacobi-Calculation");
          return mMessage.IsOkay();
      } else if( Idid == -7 ) {
          Ierr   = MASS_NO_SUCCESS;
          mMessage.SetErr(MASS_NO_SUCCESS, "Failure in mas-Calculation");
          return mMessage.IsOkay();
      }

      return OKAY;
  }
  //=======================================================================================
  // Integrate()
  //=======================================================================================
  void CSolveODE_RAD5::Integrate(double &xend)
  {
    size_t i;
    Xend = xend;
    // set step length from last step or if first calc take H from calcFirst
    H = Hnext;

    Idid = CoreIntegrator();

    //if (Idid < 0) {
    //  std::cout << " Computation failed " << std::endl;
    //  return;
    //}

    // restore tolerances
    if (Itoler == 0) {
      double quot = VAtol[0] / VRtol[0];
      VRtol[0] = pow(10.0*VRtol[0], 1.5);
      VAtol[0] = VRtol[0] * quot;
    }
    else {
      for (i = 0; i < N; i++) {
        double quot = VAtol[i] / VRtol[i];
        VRtol[i] = pow(10.0*VRtol[i], 1.5);
        VAtol[i] = VRtol[i] * quot;
      }
    }

    // print final solution
    if (Iout > 0) 
    {
      CStr text;
      
      text.catFormat("StepF %i: t = %11.9f  y =  ", Naccpt, xend);
      for (i = 0; i < N; i++)
        text.catFormat("%11.9f    ", Y[i]);
      text.catFormat("\n");
      //std::cout << text << std::endl;
      mMessage.SetLog(text);
    }

    return;

  } // Integrate

    // core integrator for radau5

    // return value of CoreIntegrator
    //  1  computation successful,
    //  2  comput. successful (interrupted by SolutionOutput)
    // -1  error in linear algebra routines,
    // -2  larger nmax is needed,
    // -3  step size becomes too small,
    // -4  matrix is repeatedly singular.
    // -5  stateDerivative function nio
    // -6  jacobian function nio
    // -7  mass function nio
    
  int CSolveODE_RAD5::CoreIntegrator(void)
  {
    size_t i;
    okay_t ret_ok;

    // constants
    const double t11 = 9.1232394870892942792e-02;
    const double t12 = -0.14125529502095420843;
    const double t13 = -3.0029194105147424492e-02;
    const double t21 = 0.24171793270710701896;
    const double t22 = 0.20412935229379993199;
    const double t23 = 0.38294211275726193779;
    const double t31 = 0.96604818261509293619;
    const double ti11 = 4.3255798900631553510;
    const double ti12 = 0.33919925181580986954;
    const double ti13 = 0.54177053993587487119;
    const double ti21 = -4.1787185915519047273;
    const double ti22 = -0.32768282076106238708;
    const double ti23 = 0.47662355450055045196;
    const double ti31 = -0.50287263494578687595;
    const double ti32 = 2.5719269498556054292;
    const double ti33 = -0.59603920482822492497;

    const double sq6 = sqrt(6.0);
    const double c1 = (4.0 - sq6) / 10.0;
    const double c2 = (4.0 + sq6) / 10.0;
    const double c1m1 = c1 - 1.0;
    const double c2m1 = c2 - 1.0;
    const double c1mc2 = c1 - c2;
    const double u1 = 1.0 / ((6.0 + pow(81.0, 1.0 / 3.0) - pow(9.0, 1.0 / 3.0)) / 30.0);
    double alph = (12.0 - pow(81.0, 1.0 / 3.0) + pow(9.0, 1.0 / 3.0)) / 60.0;
    double beta = (pow(81.0, 1.0 / 3.0) + pow(9.0, 1.0 / 3.0))*sqrt(3.0) / 60.0;
    const double cno = alph*alph + beta*beta;

    alph = alph / cno;
    beta = beta / cno;

    const double posneg = _copysign(1.0, Xend - X);
    const double hmaxn = SLF_MIN(fabs(Hmax), fabs(Xend - X));
    const double cfac = Safe*(1 + 2 * Nit);

    // compute mass matrix for implicit case
    if (Implct)
    {
      ret_ok = PObj->Mass(Fmas);
      if (ret_ok != OKAY)
      {
        CStr errtext;
        errtext.cat("Error CSolveODE_RAD5::Error in Mass()\n");
        errtext.cat(PObj->GetErrText());
        mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
 
        return -7;
      }
    }
    H = SLF_MIN(fabs(H), hmaxn);
    H = _copysign(H, posneg);
    Hold = H;

    bool last = false;

    if ((X + H*1.0001 - Xend)*posneg >= 0.0) 
    {
      H = Xend - X;
      last = true;
    }

    double hopt = H;
    double faccon = 1.0;


    if (Itoler == 0) 
    {
      for (i = 0; i < N; i++)
        Scal[i] = VAtol[0] + VRtol[0] * fabs(Y[i]);
    }
    else 
    {
      for (i = 0; i < N; i++)
        Scal[i] = VAtol[i] + VRtol[i] * fabs(Y[i]);
    }

    ret_ok = PObj->StateDer(X, Y, Y0);

    Nfcn++;

    if (ret_ok != OKAY)
    {
      CStr errtext;
      errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
      errtext.cat(PObj->GetErrText());
      mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
      return -5;
    }

    double hhfac = H;
    double hacc, erracc, thqold;
    size_t nsing = 0,ier = 0;

    // basic integration step
    ComputeJacobian();
    bool loop = true;
    while (loop) 
    {
      loop = false;
      // compute the matrices E1 and e2 and their decompositions
      Fac1 = u1 / H;
      Alphn = alph / H;
      Betan = beta / H;

      ier = DecompReal();

      if (ier != 0) 
      {
        if (ier == SIZE_MAX) return -1;
        nsing++;
        if (nsing >= 5) 
        {
          CStr txt;
          txt.catFormat("exit of RADAU5 at x = %f\n", X);
          txt.catFormat(" matrix is repeatedly singular, ier= %i", ier);
          mMessage.SetLog(txt);
          return -4;
        }
        H *= 0.5;

        hhfac = 0.5;
        Reject = true;
        last = false;
        if (!Caljac) ComputeJacobian();
        loop = true;
        continue;
      }

      ier = DecompComplex();

      if (ier != 0) 
      {
        if (ier == -1) return(-1);
        nsing++;
        if (nsing >= 5) 
        {
          CStr txt;
          txt.catFormat("exit of RADAU5 at x = %f\n", X);
          txt.catFormat(" matrix is repeatedly singular, ier= %i", ier);
          mMessage.SetLog(txt);
          return -4;
        }
        H *= 0.5;
        hhfac = 0.5;
        Reject = true;
        last = false;
        if (!Caljac) ComputeJacobian();
        loop = true;
        continue;
      }
      Ndec++;

      while (true) 
      {
        Nstep++;
        if (Nstep >= Nmax) 
        {
          CStr txt;
          txt.catFormat("exit of RADAU5 at X = %f\n", X);
          txt.catFormat(" more than Nmax =  %i steps are needed", Nmax);
          mMessage.SetLog(txt);
          return -2;
        }

        if (0.1*fabs(H) <= fabs(X)*Uround) 
        {
          CStr txt;
          txt.catFormat("exit of RADAU5 at X = %f\n", X);
          txt.catFormat(" step size too small, H = %f", H);
          mMessage.SetLog(txt);
          return -3;
        }

        // check the index of the problem
        if (Nind2 != 0)  // is index 2
        {
          for (i = Nind1; i < Nind1 + Nind2; i++)
            Scal[i] = Scal[i] / hhfac;
        }

        if (Nind3 != 0) // is index 3
        {
          for (i = Nind1 + Nind2; i < Nind1 + Nind2 + Nind3; i++)
            Scal[i] = Scal[i] / (hhfac*hhfac);
        }

        double xph = X + H;

        //  starting values for Newton iteration
        if (First || Startn) 
        {
          for (i = 0; i < N; i++)
            Z1[i] = Z2[i] = Z3[i] = F1[i] = F2[i] = F3[i] = 0.0;
        }
        else 
        {
          double c3q = H / Hold;
          double c1q = c1*c3q;
          double c2q = c2*c3q;
          double ak1, ak2, ak3;
          for (i = 0; i < N; i++) 
          {
            ak1 = Cont[i + N];
            ak2 = Cont[i + 2 * N];
            ak3 = Cont[i + 3 * N];
            Z1[i] = c1q*(ak1 + (c1q - c2m1)*(ak2 + (c1q - c1m1)*ak3));
            Z2[i] = c2q*(ak1 + (c2q - c2m1)*(ak2 + (c2q - c1m1)*ak3));
            Z3[i] = c3q*(ak1 + (c3q - c2m1)*(ak2 + (c3q - c1m1)*ak3));
            F1[i] = ti11*Z1[i] + ti12*Z2[i] + ti13*Z3[i];
            F2[i] = ti21*Z1[i] + ti22*Z2[i] + ti23*Z3[i];
            F3[i] = ti31*Z1[i] + ti32*Z2[i] + ti33*Z3[i];
          }
        }

        //  loop for the simplified Newton iteration
        size_t newt = 0;
        faccon = pow(SLF_MAX(faccon, Uround), 0.8);
        double theta = fabs(Thet);
        double dyno, dynold;

        while (true) 
        {
          if (newt >= Nit) 
          {
            if (ier != 0) 
            {
              nsing++;
              if (nsing >= 5) 
              {
                CStr txt;
                txt.catFormat("exit of RADAU5 at x = %f\n", X);
                txt.catFormat(" matrix is repeatedly singular, ier= %i", ier);
                mMessage.SetLog(txt);
                return -4;
              }
            }
            H *= 0.5;
            hhfac = 0.5;
            Reject = true;
            last = false;
            if (!Caljac) ComputeJacobian();
            loop = true;
            break;
          }
          // compute the right-hand side
          for (i = 0; i < N; i++) Cont[i] = Y[i] + Z1[i];
          
          ret_ok = PObj->StateDer(X + c1*H, Cont, Z1);
          
          if (ret_ok != OKAY)
          {
            CStr errtext;
            errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
            errtext.cat(PObj->GetErrText());
            mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
            return -5;
          }

          for (i = 0; i < N; i++) Cont[i] = Y[i] + Z2[i];

          ret_ok = PObj->StateDer(X + c2*H, Cont, Z2);
          if (ret_ok != OKAY)
          {
            CStr errtext;
            errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
            errtext.cat(PObj->GetErrText());
            mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
            return -5;
          }

          for (i = 0; i < N; i++) Cont[i] = Y[i] + Z3[i];

          ret_ok = PObj->StateDer(xph, Cont, Z3);
          if (ret_ok != OKAY)
          {
            CStr errtext;
            errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
            errtext.cat(PObj->GetErrText());
            mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
            return -5;
          }

          Nfcn += 3;

          // solve the linear systems
          for (i = 0; i < N; i++) {
            double a1 = Z1[i];
            double a2 = Z2[i];
            double a3 = Z3[i];
            Z1[i] = ti11*a1 + ti12*a2 + ti13*a3;
            Z2[i] = ti21*a1 + ti22*a2 + ti23*a3;
            Z3[i] = ti31*a1 + ti32*a2 + ti33*a3;
          }
          ier = LinearSolve();
          if (ier == SIZE_MAX) return -1;
          Nsol++;
          newt++;
          dyno = 0.0;
          double denom;
          for (i = 0; i < N; i++) {
            denom = Scal[i];
            dyno = dyno + pow(Z1[i] / denom, 2) + pow(Z2[i] / denom, 2) +
              pow(Z3[i] / denom, 2);
          }
          dyno = sqrt(dyno / (3 * N));
          // bad convergence or number of iterations to large
          if ((newt > 1) && (newt < Nit)) {
            double thq = dyno / dynold;
            if (newt == 2) theta = thq;
            else theta = sqrt(thq*thqold);
            thqold = thq;
            if (theta < 0.99) 
            {
              faccon = theta / (1.0 - theta);
              double dyth = faccon*dyno*pow(theta, Nit - 1 - newt) / Fnewt;
              if (dyth >= 1.0) {
                double qnewt = SLF_MAX(1.0e-4, SLF_MIN(20.0, dyth));
                hhfac = 0.8*pow(qnewt, -1.0 / (4.0 + Nit - 1 - newt));
                H *= hhfac;
                Reject = true;
                last = false;
                if (Caljac) ComputeJacobian();
                loop = true;
                break;
              }
            }
            else 
            {
              if (ier != 0) {
                nsing++;
                if (nsing >= 5) {
                  CStr txt;
                  txt.catFormat("exit of RADAU5 at x = %f\n", X);
                  txt.catFormat(" matrix is repeatedly singular, ier= %i", ier);
                  mMessage.SetLog(txt);
                  return -4;
                }
              }
              H *= 0.5;
              hhfac = 0.5;
              Reject = true;
              last = false;
              if (!Caljac) ComputeJacobian();
              loop = true;
              break;
            }
          }
          dynold = SLF_MAX(dyno, Uround);
          for (i = 0; i < N; i++) {
            F1[i] = F1[i] + Z1[i];
            F2[i] = F2[i] + Z2[i];
            F3[i] = F3[i] + Z3[i];
            Z1[i] = t11*F1[i] + t12*F2[i] + t13*F3[i];
            Z2[i] = t21*F1[i] + t22*F2[i] + t23*F3[i];
            Z3[i] = t31*F1[i] + F2[i];
          }
          if (faccon*dyno <= Fnewt) break;
        }

        if (loop) break;

        // error estimation
        Err = 0.0;
        ier = ErrorEstimate();
        if (ier == SIZE_MAX) return -1;

        // computation of hnew -- require 0.2 <= hnew/h <= 8.
        double fac = SLF_MIN(Safe, cfac / (newt + 2 * Nit));
        double quot = SLF_MAX(Facr, SLF_MIN(Facl, pow(Err, 0.25) / fac));
        double hnew = H / quot;

        //  is the error small enough ?
        if (Err < 1.0) {
          // step is accepted
          First = false;
          Naccpt++;

          if (Iout > 1) {
            CStr text;
            text.catFormat("Step- %i|%i: t = %10.8f h = %10.8f y = ", Naccpt,Nstep, X,Hold);

            for (size_t iii = 0; iii < N; iii++)
            {
              text.catFormat("%10.8f    ", Y[iii]);
            }
            //text.catFormat("\n");
            mMessage.SetLog(text);
            //std::cout << text << std::endl;
          }

          if (Pred) 
          {
            // predictive controller of Gustafsson
            if (Naccpt > 1) {
              double facgus = (hacc / (H))*pow(Err*Err / erracc, 0.25) / Safe;
              facgus = SLF_MAX(Facr, SLF_MIN(Facl, facgus));
              quot = SLF_MAX(quot, facgus);
              hnew = H / quot;
            }
            hacc = H;
            erracc = SLF_MAX(1.0e-2, Err);
          }
          Xold = X;
          Hold = H;
          X = xph;
          double ak, acont3;
          for (i = 0; i < N; i++) {
            Y[i] = Y[i] + Z3[i];
            Cont[i + N] = (Z2[i] - Z3[i]) / c2m1;
            ak = (Z1[i] - Z2[i]) / c1mc2;
            acont3 = Z1[i] / c1;
            acont3 = (ak - acont3) / c2;
            Cont[i + 2 * N] = (ak - Cont[i + N]) / c1m1;
            Cont[i + 3 * N] = Cont[i + 2 * N] - acont3;
          }
          if (Itoler == 0) 
          {
            for (i = 0; i < N; i++) Scal[i] = VAtol[0] + VRtol[0] * fabs(Y[i]);
          }
          else 
          {
            for (i = 0; i < N; i++) Scal[i] = VAtol[i] + VRtol[i] * fabs(Y[i]);
          }

          if (Iout > 0) 
          {
            for (i = 0; i < N; i++) Cont[i] = Y[i];
            size_t irtrn = 1;
            irtrn = SolutionOutput();
            if (irtrn != 0) 
            {
              CStr txt;
              txt.catFormat("exit of RADAU5 at x = %f\n", X);
              mMessage.SetLog(txt);
              return 2;
            }
          }
          Caljac = false;
          if (last) 
          {
            H = hopt;
            return 1;
          }

          ret_ok = PObj->StateDer(X, Y, Y0);
          Nfcn++;
          if (ret_ok != OKAY)
          {
            CStr errtext;
            errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
            errtext.cat(PObj->GetErrText());
            mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
            return -5;
          }
          hnew = posneg*SLF_MIN(fabs(hnew), hmaxn);
          hopt = SLF_MIN(H, hnew);
          if (Reject) hnew = posneg*SLF_MIN(fabs(hnew), fabs(H));
          Reject = false;
          if ((X + hnew / Quot1 - Xend)*posneg >= 0.0) 
          {
            Hnext = X+H-Xend;
            H = Xend - X;
            last = true;
          }
          else 
          {
            double qt = hnew / (H);
            hhfac = H;
            if ((theta <= Thet) && (qt >= Quot1) && (qt <= Quot2)) continue;
            H = hnew;
            Hnext = hnew;
          }
          hhfac = H;
          if (theta > Thet) ComputeJacobian();
          loop = true;
        }
        else 
        {
          // step is rejected
          Reject = true;
          last = false;
          if (First) {
            H *= 0.1;
            hhfac = 0.1;
          }
          else 
          {
            hhfac = hnew / (H);
            H = hnew;
          }
          if (Naccpt >= 1) Nrejct++;
          if (!Caljac) ComputeJacobian();
          loop = true;
        }
        break;
      }
    }

    return 1;
  }  // CoreIntegrator

  void CSolveODE_RAD5::ComputeJacobian()
  {
    Njac++;
    if (Ijac == 0) 
    {
      // compute Jacobian matrix numerically
      size_t mm1, k, l;
      if (Banded) 
      {
        // Jacobian is banded
        size_t mujacp = Mujac + 1;
        size_t md = SLF_MIN(Ldjac, M2);
        for (mm1 = 0; mm1 < M1 / M2 + 1; mm1++) {
          for (k = 0; k < md; k++) {
            size_t j = k + mm1*M2;
            while (true) 
            {
              F1[j] = Y[j];
              F2[j] = sqrt(Uround*SLF_MAX(1.0e-5, fabs(Y[j])));
              Y[j] = Y[j] + F2[j];
              j += md;
              if (j > (mm1 + 1)*M2 - 1) break;
            }
            okay_t ret_ok = PObj->StateDer(X, Y, Cont);
            if (ret_ok != OKAY)
            {
              CStr errtext;
              errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
              errtext.cat(PObj->GetErrText());
              mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
              return;
            }
            j = k + mm1*M2;
            size_t j1 = k;
            size_t lbeg = M1;
            if (j1 > Mujac)lbeg += j1 - Mujac;            
            size_t lend, mujacj;
            while (true) 
            {
              lend = SLF_MIN(M2, j1 + Mljac) + M1;
              Y[j] = F1[j];
              mujacj = mujacp - j1 - M1 - 1;
              for (l = lbeg; l <= lend; l++) 
              {
                Fjac[l + mujacj][j] = (Cont[l] - Y0[l]) / F2[j];
              }
              j += md;
              j1 += md;
              lbeg = lend + 1;
              if (j >= (mm1 + 1)*M2) break;
            }
          }
        }
      }
      else 
      {
        // Jacobian is full
        double delt, ysafe;
        size_t i, j;
        for (i = 0; i < N; i++) 
        {
          ysafe = Y[i];
          delt = sqrt(Uround*SLF_MAX(1.0e-5, fabs(ysafe)));
          Y[i] = ysafe + delt;
          okay_t ret_ok = PObj->StateDer(X, Y, Cont);
          if (ret_ok != OKAY)
          {
            CStr errtext;
            errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
            errtext.cat(PObj->GetErrText());
            mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
            return;
          }
          for (j = M1; j < N; j++)
            Fjac[j - M1][i] = (Cont[j] - Y0[j]) / delt;
          Y[i] = ysafe;
        }
      }
    }
    else 
    {
      // compute Jacobian matrix analytically
      okay_t ret_ok = PObj->Jacobian(X, Y, Fjac, Ldjac);
      if (ret_ok != OKAY)
      {
        CStr errtext;
        errtext.cat("Error CSolveODE_RAD5::Error in Jacobi()\n");
        errtext.cat(PObj->GetErrText());
        mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
        return;
      }
    }

    Caljac = true;
    Calhesse = true;

    return;
  }

  double CSolveODE_RAD5::ContinuousOutput(size_t i)
  {

    // ----------------------------------------------------------
    //     This function can be used for continuous output. It provides an
    //     approximation to the i-th component of the solution at xd.
    //     It gives the value of the collocation polynomial, defined for
    //     the last successfully computed step.
    // ----------------------------------------------------------

    double s, sq6, c1, c2, c2m1, c1m1;

    sq6 = sqrt(6.0);
    c1 = (4.0 - sq6) / 10.0;
    c2 = (4.0 + sq6) / 10.0;
    c1m1 = c1 - 1.0;
    c2m1 = c2 - 1.0;

    s = (Xd - X) / Hold;

    return (Cont[i] + s*(Cont[i + N] + (s - c2m1)*(Cont[i + 2 * N] +
      (s - c1m1)*Cont[i + 3 * N])));

  }


  size_t CSolveODE_RAD5::DecompReal(void)
  {
    size_t mm, ier, j, i, k;
    double sum;

    switch (Ijob)
    {
    case (1):
      // mass = identity, Jacobian a full matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < N; i++) 
        {
          E1[i][j] = -Fjac[i][j];
        }
        E1[j][j] += Fac1;
      }
      ier = dec(N, E1, Ip1);
      break;

    case (2):
      // mass = identity, Jacobian a banded matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < Ldjac; i++) 
        {
          E1[i + Mle][j] = -Fjac[i][j];
        }
        E1[Mdiag][j] += Fac1;
      }
      ier = decb(N, E1, Mle, Mue, Ip1);
      break;

    case (3):
      // mass is a banded matrix, Jacobian a full matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < N; i++)
          E1[i][j] = -Fjac[i][j];
        for (i = SLF_MAX(0, j - Mumas); i < SLF_MIN(N, j + Mlmas + 1); i++)
          E1[i][j] += Fac1*Fmas[i - j + Mbdiag - 1][j];
      }
      ier = dec(N, E1, Ip1);
      break;

    case (4):
      // mass is a banded matrix, Jacobian a banded matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < Ldjac; i++)
          E1[i + Mle][j] = -Fjac[i][j];
        for (i = 0; i < Mbb; i++)
          E1[i + Mdiff][j] += Fac1*Fmas[i][j];
      }
      ier = decb(N, E1, Mle, Mue, Ip1);
      break;

    case (5):
      // mass is a full matrix, Jacobian a full matrix
      for (j = 0; j < N; j++)
        for (i = 0; i < N; i++)
          E1[i][j] = Fmas[i][j] * Fac1 - Fjac[i][j];
      ier = dec(N, E1, Ip1);
      break;

    case (6):
      // mass is a full matrix, Jacobian a banded matrix
      // This option is not provided
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    case (7):
      // mass = identity, Jacobian a full matrix, Hessenberg-option
      if (Calhesse) elmhes(N, 0, N, Fjac, Iphes);
      Calhesse = false;
      for (j = 0; j < N - 1; j++) E1[j + 1][j] = -Fjac[j + 1][j];
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i <= j; i++) E1[i][j] = -Fjac[i][j];
        E1[j][j] += Fac1;
      }
      ier = dech(N, E1, 1, Ip1);
      break;

    case (11):
      // mass = identity, Jacobian a full matrix, second order
      for (j = 0; j < Nm1; j++) {
        for (i = 0; i < Nm1; i++) {
          E1[i][j] = -Fjac[i][j + M1];
        }
        E1[j][j] += Fac1;
      }
      break;

    case (12):
      // mass = identity, Jacobian a banded matrix, second order
      for (j = 0; j < Nm1; j++) {
        for (i = 0; i < Ldjac; i++)
          E1[i + Mle][j] = -Fjac[i][j + M1];
        E1[Mdiag][j] += Fac1;
      }
      break;

    case (13):
      // mass is a banded matrix, Jacobian a full matrix, second order
      for (j = 0; j < Nm1; j++) {
        for (i = 0; i < Nm1; i++)
          E1[i][j] = -Fjac[i][j + M1];
        for (i = SLF_MAX(0, j - Mumas); i < SLF_MIN(N, j + Mumas + 1); i++)
          E1[i][j] += Fac1*Fmas[i - j + Mbdiag - 1][j];
      }
      break;

    case (14):
      // mass is a banded matrix, Jacobian a banded matrix, second order
      for (j = 0; j < Nm1; j++) {
        for (i = 0; i < Ldjac; i++)
          E1[i + Mle][j] = -Fjac[i][j + M1];
        for (i = 0; i < Mbb; i++)
          E1[i + Mdiff][j] += Fac1*Fmas[i][j];
      }
      break;

    case (15):
      // mass is a full matrix, Jacobian a full matrix, second order
      for (j = 0; j < Nm1; j++)
        for (i = 0; i < Nm1; i++)
          E1[i][j] = Fmas[i][j] * Fac1 - Fjac[i][j + M1];
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);
    }

    switch (Ijob)
    {
    case (1):
    case (2):
    case (3):
    case (4):
    case (5):
    case (7):
      break;

    case (11):
    case (13):
    case (15):
      mm = M1 / M2;
      for (j = 0; j < M2; j++) {
        for (i = 0; i < Nm1; i++) {
          sum = 0.0;
          for (k = 0; k < mm; k++)
            sum = (sum + Fjac[i][j + k*M2]) / Fac1;
          E1[i][j] -= sum;
        }
      }
      ier = dec(Nm1, E1, Ip1);
      break;

    case (12):
    case (14):
      mm = M1 / M2;
      for (j = 0; j < M2; j++) {
        for (i = 0; i < Ldjac; i++) {
          sum = 0.0;
          for (k = 0; k < mm; k++)
            sum = (sum + Fjac[i][j + k*M2]) / Fac1;
          E1[i + Mle][j] -= sum;
        }
      }
      ier = decb(Nm1, E1, Mle, Mue, Ip1);
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);
    }

    return (ier);

  } // DecompReal


  size_t CSolveODE_RAD5::DecompComplex(void)
  {
    size_t ier = 0;
    size_t mm, j, i, k;
    double bb, ffma, abno, alp, bet, sumr, sumi, sums;

    switch (Ijob) {
    case (1):
      // mass = identity, Jacobian a full matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < N; i++) 
        {
          E2r[i][j] = -Fjac[i][j];
          E2i[i][j] = 0.0;
        }
        E2r[j][j] += Alphn;
        E2i[j][j] = Betan;
      }
      ier = decc(N, E2r, E2i, Ip2);
      break;

    case (2):
      // mass = identiy, Jacobian a banded matrix
      for (j = 0; j < N; j++) {
        for (i = 0; i < Ldjac; i++) {
          E2r[i + Mle][j] = -Fjac[i][j];
          E2i[i + Mle][j] = 0.0;
        }
        E2r[Mdiag][j] += Alphn;
        E2i[Mdiag][j] = Betan;
      }
      ier = decbc(N, E2r, E2i, Mle, Mue, Ip2);
      break;

    case (3):
      // mass is a banded matrix, Jacobian a full matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < N; i++) 
        {
          E2r[i][j] = -Fjac[i][j];
          E2i[i][j] = 0.0;
        }
      }
      for (j = 0; j < N; j++) 
      {
        for (i = SLF_MAX(0, j - Mumas); i < SLF_MIN(N, j + Mumas + 1); i++) 
        {
          bb = Fmas[i - j + Mbdiag - 1][j];
          E2r[i][j] += Alphn*bb;
          E2i[i][j] = Betan*bb;
        }
      }
      ier = decc(N, E2r, E2i, Ip2);
      break;

    case (4):
      // mass is a banded matrix, Jacobian a banded matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < Ldjac; i++) 
        {
          E2r[i + Mle][j] = -Fjac[i][j];
          E2i[i + Mle][j] = 0.0;
        }
        for (i = SLF_MAX(0, Mumas - j); i < SLF_MIN(Mbb, Mumas - j + N); i++) 
        {
          bb = Fmas[i][j];
          E2r[i + Mdiff][j] += Alphn*bb;
          E2i[i + Mdiff][j] = Betan*bb;
        }
      }
      ier = decbc(N, E2r, E2i, Mle, Mue, Ip2);
      break;

    case (5):
      // mass is a full matrix, Jacobian a full matrix
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i < N; i++) 
        {
          bb = Fmas[i][j];
          E2r[i][j] = Alphn*bb - Fjac[i][j];
          E2i[i][j] = Betan*bb;
        }
      }
      ier = decc(N, E2r, E2i, Ip2);
      break;

    case (6):
      // mass is a full matrix, Jacobian a banded matrix
      // This option is not provided
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    case (7):
      // mass = identity, Jacobian a full matrix, Hessenberg-option
      for (j = 0; j < N - 1; j++) 
      {
        E2r[j + 1][j] = -Fjac[j + 1][j];
        E2i[j + 1][j] = 0.0;
      }
      for (j = 0; j < N; j++) 
      {
        for (i = 0; i <= j; i++) 
        {
          E2i[i][j] = 0.0;
          E2r[i][j] = -Fjac[i][j];
        }
        E2r[j][j] += Alphn;
        E2i[j][j] = Betan;
      }
      ier = dechc(N, E2r, E2i, 1, Ip2);
      break;

    case (11):
      // mass = identity, Jacobian a full matrix, second order
      for (j = 0; j < Nm1; j++) 
      {
        for (i = 0; i < Nm1; i++) 
        {
          E2r[i][j] = -Fjac[i][j + M1];
          E2i[i][j] = 0.0;
        }
        E2r[j][j] += Alphn;
        E2i[j][j] = Betan;
      }
      break;

    case (12):
      // mass = identity, Jacobian a banded matrix, second order
      for (j = 0; j < Nm1; j++) 
      {
        for (i = 0; i < Ldjac; i++) 
        {
          E2r[i + Mle][j] = -Fjac[i][j + M1];
          E2i[i + Mle][j] = 0.0;
        }
        E2r[Mdiag][j] += Alphn;
        E2i[Mdiag][j] += Betan;
      }
      break;

    case (13):
      // mass is a banded matrix, Jacobian a full matrix, second order
      for (j = 0; j < Nm1; j++) 
      {
        for (i = 0; i < Nm1; i++) 
        {
          E2r[i][j] = -Fjac[i][j + M1];
          E2i[i][j] = 0.0;
        }
        for (i = SLF_MAX(0, j - Mumas); i < SLF_MIN(Nm1, j + Mumas + 1); i++) 
        {
          ffma = Fmas[i - j + Mbdiag - 1][j];
          E2r[j][j] += Alphn*ffma;
          E2i[j][j] += Betan*ffma;
        }
      }
      break;

    case (14):
      // mass is a banded matrix, Jacobian a banded matrix, second order
      for (j = 0; j < Nm1; j++) 
      {
        for (i = 0; i < Ldjac; i++) 
        {
          E2r[i + Mle][j] = -Fjac[i][j + M1];
          E2i[i + Mle][j] = 0.0;
        }
        for (i = 0; i < Mbb; i++) 
        {
          ffma = Fmas[i][j];
          E2r[i + Mdiff][j] += Alphn*ffma;
          E2i[i + Mdiff][j] += Betan*ffma;
        }
      }
      break;

    case (15):
      // mass is a full matrix, Jacobian a full matrix, second order
      for (j = 0; j < Nm1; j++) 
      {
        for (i = 0; i < Nm1; i++) 
        {
          E2r[i][j] = Alphn*Fmas[i][j] - Fjac[i][j + M1];
          E2i[i][j] = Betan*Fmas[i][j];
        }
      }
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    }

    switch (Ijob) {
    case (1):
    case (2):
    case (3):
    case (4):
    case (5):
    case (7):
      break;

    case (11):
    case (13):
    case (15):
      mm = M1 / M2;
      abno = Alphn*Alphn + Betan*Betan;
      alp = Alphn / abno;
      bet = Betan / abno;
      for (j = 0; j < M2; j++) 
      {
        for (i = 0; i < Nm1; i++) 
        {
          sumr = sumi = 0.0;
          for (k = 0; k < mm; k++) 
          {
            sums = sumr + Fjac[i][j + k*M2];
            sumr = sums*alp + sumi*bet;
            sumi = sumi*alp - sums*bet;
          }
          E2r[i][j] -= sumr;
          E2i[i][j] -= sumi;
        }
      }
      ier = decc(Nm1, E2r, E2i, Ip2);
      break;

    case (12):
    case (14):
      mm = M1 / M2;
      abno = Alphn*Alphn + Betan*Betan;
      alp = Alphn / abno;
      bet = Betan / abno;
      for (j = 0; j < M2; j++) 
      {
        for (i = 0; i < Ldjac; i++) 
        {
          sumr = sumi = 0.0;
          for (k = 0; k < mm; k++) 
          {
            sums = sumr + Fjac[i][j + k*M2];
            sumr = sums*alp + sumi*bet;
            sumi = sumi*alp - sums*bet;
          }
          E2r[i + Mle][j] -= sumr;
          E2i[i + Mle][j] -= sumi;
        }
      }
      ier = decbc(Nm1, E2r, E2i, Mle, Mue, Ip2);
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);
    }

    return (ier);

  } // DecompComplex


  size_t CSolveODE_RAD5::LinearSolve(void)
  {
    size_t ier = 0;
    size_t mm, mp, mp1, ii, jkm, mpi, i, j, mm1, k;
    double abno, bb, e1imp, s1, s2, s3, sum1, sum2, sum3, sumh;
    double ffja, z2i, z3i, zsafe;

    switch (Ijob) {
    case (1):
      // mass = identity, Jacobian a full matrix
      for (i = 0; i < N; i++) 
      {
        s2 = -F2[i];
        s3 = -F3[i];
        Z1[i] -= F1[i] * Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      sol(N, E1, Z1, Ip1);
      solc(N, E2r, E2i, Z2, Z3, Ip2);
      break;

    case (2):
      // mass = identity, Jacobian a banded matrix
      for (i = 0; i < N; i++) 
      {
        s2 = -F2[i];
        s3 = -F3[i];
        Z1[i] -= F1[i] * Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      solb(N, E1, Mle, Mue, Z1, Ip1);
      solbc(N, E2r, E2i, Mle, Mue, Z2, Z3, Ip2);
      break;

    case (3):
      // mass is a banded matrix, Jacobian a full matrix
      for (i = 0; i < N; i++) 
      {
        s1 = s2 = s3 = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(N, i + Mumas + 1); j++) 
        {
          bb = Fmas[i - j + Mbdiag - 1][j];
          s1 -= bb*F1[j];
          s2 -= bb*F2[j];
          s3 -= bb*F3[j];
        }
        Z1[i] += s1*Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      sol(N, E1, Z1, Ip1);
      solc(N, E2r, E2i, Z2, Z3, Ip2);
      break;

    case (4):
      // mass is a banded matrix, Jacobian a banded matrix
      for (i = 0; i < N; i++) 
      {
        s1 = s2 = s3 = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(N, i + Mumas + 1); j++) 
        {
          bb = Fmas[i - j + Mbdiag - 1][j];
          s1 -= bb*F1[j];
          s2 -= bb*F2[j];
          s3 -= bb*F3[j];
        }
        Z1[i] += s1*Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      solb(N, E1, Mle, Mue, Z1, Ip1);
      solbc(N, E2r, E2i, Mle, Mue, Z2, Z3, Ip2);
      break;

    case (5):
      // mass is a full matrix, Jacobian a full matrix
      for (i = 0; i < N; i++) 
      {
        s1 = s2 = s3 = 0.0;
        for (j = 0; j < N; j++) 
        {
          bb = Fmas[i][j];
          s1 -= bb*F1[j];
          s2 -= bb*F2[j];
          s3 -= bb*F3[j];
        }
        Z1[i] += s1*Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      sol(N, E1, Z1, Ip1);
      solc(N, E2r, E2i, Z2, Z3, Ip2);
      break;

    case (6):
      // mass is a full matrix, Jacobian a banded matrix
      // This option is not provided
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    case (7):
      // mass = identity, Jacobian a full matrix, Hessenberg-option
      for (i = 0; i < N; i++) 
      {
        s2 = -F2[i];
        s3 = -F3[i];
        Z1[i] -= F1[i] * Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      for (mm1 = N - 3; mm1 >= 0; mm1--) 
      {
        mp = N - mm1 - 2;
        mp1 = mp - 1;
        ii = (size_t)Iphes[mp];
        if (ii != mp) 
        {
          zsafe = Z1[mp];
          Z1[mp] = Z1[ii];
          Z1[ii] = zsafe;
          zsafe = Z2[mp];
          Z2[mp] = Z2[ii];
          Z2[ii] = zsafe;
          zsafe = Z3[mp];
          Z3[mp] = Z3[ii];
          Z3[ii] = zsafe;
        }
        for (i = mp + 1; i < N; i++) 
        {
          e1imp = Fjac[i][mp1];
          Z1[i] -= e1imp*Z1[mp];
          Z2[i] -= e1imp*Z2[mp];
          Z3[i] -= e1imp*Z3[mp];
        }
      }
      solh(N, E1, 1, Z1, Ip1);
      solhc(N, E2r, E2i, 1, Z2, Z3, Ip2);
      for (mm1 = 0; mm1 < N - 2; mm1++) 
      {
        mp = N - mm1 - 2;
        mp1 = mp - 1;
        for (i = mp; i < N; i++) 
        {
          e1imp = Fjac[i][mp1];
          Z1[i] += e1imp*Z1[mp];
          Z2[i] += e1imp*Z2[mp];
          Z3[i] += e1imp*Z3[mp];
        }
        ii = (size_t)Iphes[mp];
        if (ii != mp) {
          zsafe = Z1[mp];
          Z1[mp] = Z1[ii];
          Z1[ii] = zsafe;
          zsafe = Z2[mp];
          Z2[mp] = Z2[ii];
          Z2[ii] = zsafe;
          zsafe = Z3[mp];
          Z3[mp] = Z3[ii];
          Z3[ii] = zsafe;
        }
      }
      break;

    case (11):
      // mass = identity, Jacobian a full matrix, second order
    case (12):
      // ---  b = identity, Jacobian a banded matrix, second order
      for (i = 0; i < N; i++) 
      {
        s2 = -F2[i];
        s3 = -F3[i];
        Z1[i] -= F1[i] * Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      break;

    case (13):
      // mass is a banded matrix, Jacobian a full matrix, second order
    case (14):
      // mass is a banded matrix, Jacobian a banded matrix, second order
      for (i = 0; i < M1; i++) 
      {
        s2 = -F2[i];
        s3 = -F3[i];
        Z1[i] -= F1[i] * Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      for (i = 0; i < Nm1; i++) 
      {
        s1 = s2 = s3 = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(Nm1, i + Mumas + 1); j++) 
        {
          bb = Fmas[i - j + Mbdiag - 1][j];
          s1 -= bb*F1[j + M1];
          s2 -= bb*F2[j + M1];
          s3 -= bb*F3[j + M1];
        }
        Z1[i + M1] += s1*Fac1;
        Z2[i + M1] = Z2[i + M1] + s2*Alphn - s3*Betan;
        Z3[i + M1] = Z3[i + M1] + s3*Alphn + s2*Betan;
      }
      break;

    case (15):
      // mass is a full matrix, Jacobian a full matrix, second order
      for (i = 0; i < M1; i++) 
      {
        s2 = -F2[i];
        s3 = -F3[i];
        Z1[i] -= F1[i] * Fac1;
        Z2[i] = Z2[i] + s2*Alphn - s3*Betan;
        Z3[i] = Z3[i] + s3*Alphn + s2*Betan;
      }
      for (i = 0; i < Nm1; i++) 
      {
        s1 = s2 = s3 = 0.0;
        for (j = 0; j < Nm1; j++) 
        {
          bb = Fmas[i][j];
          s1 -= bb*F1[j + M1];
          s2 -= bb*F2[j + M1];
          s3 -= bb*F3[j + M1];
        }
        Z1[i + M1] += s1*Fac1;
        Z2[i + M1] = Z2[i + M1] + s2*Alphn - s3*Betan;
        Z3[i + M1] = Z3[i + M1] + s3*Alphn + s2*Betan;
      }
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);
    }

    switch (Ijob) {
    case (1):
    case (2):
    case (3):
    case (4):
    case (5):
    case (7):
      break;

    case (11):
    case (13):
    case (15):
      abno = Alphn*Alphn + Betan*Betan;
      mm = M1 / M2;
      for (j = 0; j < M2; j++) 
      {
        sum1 = sum2 = sum3 = 0.0;
        for (k = mm - 1; k >= 0; k--) 
        {
          jkm = j + k*M2;
          sum1 = (Z1[jkm] + sum1) / Fac1;
          sumh = (Z2[jkm] + sum2) / abno;
          sum3 = (Z3[jkm] + sum3) / abno;
          sum2 = sumh*Alphn + sum3*Betan;
          sum3 = sum3*Alphn - sumh*Betan;
          for (i = 0; i < Nm1; i++) 
          {
            Z1[i + M1] += Fjac[i][jkm] * sum1;
            Z2[i + M1] += Fjac[i][jkm] * sum2;
            Z3[i + M1] += Fjac[i][jkm] * sum3;
          }
        }
      }

      sol(Nm1, E1, &Z1[M1], Ip1);
      solc(Nm1, E2r, E2i, &Z2[M1], &Z3[M1], Ip2);
      break;

    case (12):
    case (14):
      abno = Alphn*Alphn + Betan*Betan;
      mm = M1 / M2;
      for (j = 0; j < M2; j++) {
        sum1 = sum2 = sum3 = 0.0;
        for (k = mm - 1; k >= 0; k--) {
          jkm = j + k*M2;
          sum1 = (Z1[jkm] + sum1) / Fac1;
          sumh = (Z2[jkm] + sum2) / abno;
          sum3 = (Z3[jkm] + sum3) / abno;
          sum2 = sumh*Alphn + sum3*Betan;
          sum3 = sum3*Alphn - sumh*Betan;
          for (i = SLF_MAX(0, j - Mujac); i < SLF_MIN(Nm1, j + Mljac + 1); i++) {
            ffja = Fjac[i + Mujac - j][jkm];
            Z1[i + M1] += ffja*sum1;
            Z2[i + M1] += ffja*sum2;
            Z3[i + M1] += ffja*sum3;
          }
        }
      }
      solb(Nm1, E1, Mle, Mue, &Z1[M1], Ip1);
      solbc(Nm1, E2r, E2i, Mle, Mue, &Z2[M1], &Z3[M1], Ip2);
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);
    }

    switch (Ijob) {
    case (1):
    case (2):
    case (3):
    case (4):
    case (5):
    case (7):
      break;

    case (11):
    case (12):
    case (13):
    case (14):
    case (15):
      for (i = M1 - 1; i >= 0; i--) {
        mpi = M2 + i;
        Z1[i] = (Z1[i] + Z1[mpi]) / Fac1;
        z2i = Z2[i] + Z2[mpi];
        z3i = Z3[i] + Z3[mpi];
        Z3[i] = (z3i*Alphn - z2i*Betan) / abno;
        Z2[i] = (z2i*Alphn + z3i*Betan) / abno;
      }
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    }

    return (ier);

  } // LinearSolve


  size_t CSolveODE_RAD5::ErrorEstimate(void)
  {
    size_t ier = 0;
    size_t mm, ii, mp, i, j, mm1, k;
    double sum, zsafe;

    double hee1 = -(13.0 + 7.0*sqrt(6.0)) / (3.0*H);
    double hee2 = (-13.0 + 7.0*sqrt(6.0)) / (3.0*H);
    double hee3 = -1.0 / (3.0*H);

    switch (Ijob)
    {
    case (1):
      // mass = identity, Jacobian a full matrix
      for (i = 0; i < N; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      sol(N, E1, Cont, Ip1);
      break;

    case (2):
      // mass = identity, Jacobian a banded matrix
      for (i = 0; i < N; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      solb(N, E1, Mle, Mue, Cont, Ip1);
      break;

    case (3):
      // mass is a banded matrix, Jacobian a full matrix
      for (i = 0; i < N; i++)
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
      for (i = 0; i < N; i++) {
        sum = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(N, i + Mumas + 1); j++)
          sum += Fmas[i - j + Mbdiag - 1][j] * F1[j];
        F2[i] = sum;
        Cont[i] = sum + Y0[i];
      }
      sol(N, E1, Cont, Ip1);
      break;

    case (4):
      // mass is a banded matrix, Jacobian a banded matrix
      for (i = 0; i < N; i++)
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
      for (i = 0; i < N; i++) {
        sum = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(N, i + Mumas + 1); j++)
          sum = sum + Fmas[i - j + Mbdiag - 1][j] * F1[j];
        F2[i] = sum;
        Cont[i] = sum + Y0[i];
      }
      solb(N, E1, Mle, Mue, Cont, Ip1);
      break;

    case (5):
      // mass is a full matrix, Jacobian a full matrix
      for (i = 0; i < N; i++)
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
      for (i = 0; i < N; i++) {
        sum = 0.0;
        for (j = 0; j < N; j++)
          sum += Fmas[j][i] * F1[j];
        F2[i] = sum;
        Cont[i] = sum + Y0[i];
      }
      sol(N, E1, Cont, Ip1);
      break;

    case (6):
      // mass is a full matrix, Jacobian a banded matrix
      // this option is not provided
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    case (7):
      // mass = identity, Jacobian a full matrix, Hessenberg-option
      for (i = 0; i < N; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      for (mm1 = N - 3; mm1 >= 0; mm1--) {
        mp = N - mm1 - 2;
        ii = Iphes[mp];
        if (ii != mp) {
          zsafe = Cont[mp];
          Cont[mp] = Cont[ii];
          Cont[ii] = zsafe;
        }
        for (i = mp; i < N; i++)
          Cont[i] -= Fjac[i][mp - 1] * Cont[mp];
      }
      solh(N, E1, 1, Cont, Ip1);
      for (mm1 = 0; mm1 < N - 2; mm1++) {
        mp = N - mm1 - 2;
        for (i = mp; i < N; i++)
          Cont[i] += Fjac[i][mp - 1] * Cont[mp];
        ii = Iphes[mp];
        if (ii != mp) {
          zsafe = Cont[mp];
          Cont[mp] = Cont[ii];
          Cont[ii] = zsafe;
        }
      }
      break;

    case (11):
      // mass = identity, Jacobian a full matrix, second order
      for (i = 0; i < N; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      break;

    case (12):
      // mass = identity, Jacobian a banded matrix, second order
      for (i = 0; i < N; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      break;

    case (13):
      // mass is a banded matrix, Jacobian a full matrix, second order
      for (i = 0; i < M1; i++) {
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      for (i = M1; i < N; i++)
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
      for (i = 0; i < Nm1; i++) {
        sum = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(Nm1, i + Mumas + 1); j++)
          sum += Fmas[i - j + Mbdiag - 1][j] * F1[j + M1];
        F2[i + M1] = sum;
        Cont[i + M1] = sum + Y0[i + M1];
      }
      break;

    case (14):
      // mass is a banded matrix, Jacobian a banded matrix, second order
      for (i = 0; i < M1; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      for (i = M1; i < N; i++)
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
      for (i = 0; i < Nm1; i++) {
        sum = 0.0;
        for (j = SLF_MAX(0, i - Mumas); j < SLF_MIN(Nm1, i + Mumas + 1); j++)
          sum += Fmas[i - j + Mbdiag - 1][j] * F1[j + M1];
        F2[i + M1] = sum;
        Cont[i + M1] = sum + Y0[i + M1];
      }
      break;

    case (15):
      // mass is a banded matrix, Jacobian a full matrix, second order
      for (i = 0; i < M1; i++) {
        F2[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
        Cont[i] = F2[i] + Y0[i];
      }
      for (i = M1; i < N; i++)
        F1[i] = hee1*Z1[i] + hee2*Z2[i] + hee3*Z3[i];
      for (i = 0; i < Nm1; i++) {
        sum = 0.0;
        for (j = 0; j < Nm1; j++)
          sum += Fmas[j][i] * F1[j + M1];
        F2[i + M1] = sum;
        Cont[i + M1] = sum + Y0[i + M1];
      }
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    }

    switch (Ijob)
    {
    case (1):
    case (2):
    case (3):
    case (4):
    case (5):
    case (7):
      break;

    case (11):
    case (13):
    case (15):
      mm = M1 / M2;
      for (j = 0; j < M2; j++) {
        sum = 0.0;
        for (k = mm - 1; k >= 0; k--) {
          sum = (Cont[j + k*M2] + sum) / Fac1;
          for (i = 0; i < Nm1; i++)
            Cont[i + M1] += Fjac[i][j + k*M2] * sum;
        }
      }
      sol(Nm1, E1, &Cont[M1], Ip1);
      for (i = M1 - 1; i >= 0; i--)
        Cont[i] = (Cont[i] + Cont[M2 + i]) / Fac1;
      break;

    case (12):
    case (14):
      mm = M1 / M2;
      for (j = 0; j < M2; j++) {
        sum = 0.0;
        for (k = mm - 1; k >= 0; k--) {
          sum = (Cont[j + k*M2] + sum) / Fac1;
          for (i = SLF_MAX(0, j - Mujac); i < SLF_MIN(Nm1, j + Mljac); i++)
            Cont[i + M1] += Fjac[i + Mujac - j][j + k*M2] * sum;
        }
      }
      solb(Nm1, E1, Mle, Mue, &Cont[M1], Ip1);
      for (i = M1 - 1; i >= 0; i--)
        Cont[i] = (Cont[i] + Cont[M2 + i]) / Fac1;
      break;
    default:
      {
        CStr txt;
        txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
        mMessage.SetLog(txt);
      }
      ier = SIZE_MAX;
      return (ier);

    }

    Err = 0.0;
    for (i = 0; i < N; i++)
      Err += pow(Cont[i] / Scal[i], 2);
    Err = SLF_MAX(sqrt(Err / N), 1.0e-10);

    if (Err < 1.0) return (ier);

    if (First || Reject) 
    {
      for (i = 0; i < N; i++) Cont[i] = Y[i] + Cont[i];
      okay_t ret_ok = PObj->StateDer(X, Cont, F1);
      if (ret_ok != OKAY)
      {
        CStr errtext;
        errtext.cat("Error CSolveODE_RAD5::Error in StateDer()\n");
        errtext.cat(PObj->GetErrText());
        mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
        ier = SIZE_MAX;
        return (ier);
      }

      Nfcn++;
      for (i = 0; i < N; i++) Cont[i] = F1[i] + F2[i];

      switch (Ijob)
      {
      case (1):
      case (3):
      case (5):
        // full matrix option
        sol(N, E1, Cont, Ip1);
        break;

      case (2):
      case (4):
        // banded matrix option
        solb(N, E1, Mle, Mue, Cont, Ip1);
        break;

      case (7):
        //Hessenberg matrix option
        // mass = identity, Jacobian a full matrix, Hessenberg-option
        for (mm1 = N - 3; mm1 >= 0; mm1--) {
          mp = N - mm1 - 2;
          ii = Iphes[mp];
          if (ii != mp) {
            zsafe = Cont[mp];
            Cont[mp] = Cont[ii];
            Cont[ii] = zsafe;
          }
          for (i = mp; i < N; i++)
            Cont[i] -= Fjac[i][mp - 1] * Cont[mp];
        }
        solh(N, E1, 1, Cont, Ip1);
        for (mm1 = 0; mm1 < N - 2; mm1++) {
          mp = N - mm1 - 2;
          for (i = mp; i < N; i++)
            Cont[i] += Fjac[i][mp - 1] * Cont[mp];
          ii = Iphes[mp];
          if (ii != mp) {
            zsafe = Cont[mp];
            Cont[mp] = Cont[ii];
            Cont[ii] = zsafe;
          }
        }
        break;

      case (11):
      case (13):
      case (15):
        // Full matrix option, second order
        for (j = 0; j < M2; j++) {
          sum = 0.0;
          for (k = mm - 1; k >= 0; k--) {
            sum = (Cont[j + k*M2] + sum) / Fac1;
            for (i = 0; i < Nm1; i++)
              Cont[i + M1] += Fjac[i][j + k*M2] * sum;
          }
        }
        sol(Nm1, E1, &Cont[M1], Ip1);
        for (i = M1 - 1; i >= 0; i--)
          Cont[i] = (Cont[i] + Cont[M2 + i]) / Fac1;
        break;

      case (12):
      case (14):
        // Banded matrix option, second order
        for (j = 0; j < M2; j++) {
          sum = 0.0;
          for (k = mm - 1; k >= 0; k--) {
            sum = (Cont[j + k*M2] + sum) / Fac1;
            for (i = SLF_MAX(0, j - Mujac); i < SLF_MIN(Nm1, j + Mljac); i++)
              Cont[i + M1] += Fjac[i + Mujac - j][j + k*M2] * sum;
          }
        }
        solb(Nm1, E1, Mle, Mue, &Cont[M1], Ip1);
        for (i = M1 - 1; i >= 0; i--)
          Cont[i] = (Cont[i] + Cont[M2 + i]) / Fac1;
        break;
      default:
        {
          CStr txt;
          txt.catFormat("Not a value Ijob. \tijob = %i\n", Ijob);
          mMessage.SetLog(txt);
        }
        ier = SIZE_MAX;
        return (ier);
      }

      Err = 0.0;
      for (i = 0; i < N; i++)
        Err += pow(Cont[i] / Scal[i], 2);
      Err = SLF_MAX(sqrt(Err / N), 1.0e-10);
    }

    return (ier);

  } // ErrorEstimate

  // Function that controls the output of the results.
  // Modify this routine according to your needs
  size_t CSolveODE_RAD5::SolutionOutput(void)
  {
    CStr text;

    if (Naccpt == 0) Xd = Xold;
    
    while (Xd < X) 
    {
      if ((Xold <= Xd) && (X >= Xd)) 
      {
        text.catFormat("StepI %i: t = %5.2f y = ", Naccpt, Xd);

        for (size_t i = 0; i < N; i++)
        {
          text.catFormat("%10.8f    ", ContinuousOutput(i));
        }
        text.catFormat("\n");
        mMessage.SetLog(text);
        //std::cout << text << std::endl;
        Xd += Dx;
      }
    }

    return 0;

  }  // SolutionOutput

} //namespace slf
