#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SlfSolveODE_RAD.h"      
#include "f2c.h"
#include "SlfNum.h"

namespace slf
{

  /* radau.f -- translated by f2c (version 20060506).
  You must link the resulting object file with libf2c:
  on Microsoft Windows system, link with libf2c.lib;
  on Linux or Unix systems, link with .../path/to/libf2c.a -lm
  or, if you install libf2c.a in a standard place, with -lf2c -lm
  -- in that order, at the end of the command line, as in
  cc *.o -lf2c -lm
  Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		  http://www.netlib.org/f2c/libf2c.zip
  */
            
      /* Table of constant values */
    
      static int32_t c__9 = 9;
      static int32_t c__1 = 1;
      static int32_t c__3 = 3;
      static int32_t c__5 = 5;
      static double c_b90 = 1.;
      static double c_b100 = .8;
      static double c_b140 = 9.;
      static double c_b141 = .33333333333333331;
    
    


  CSolveODE_RAD::CSolveODE_RAD(void) {
    
      Ierr   = NO_ERR;    

      NstepMax = NstepAccept = 0;
    
      N    = 0;
      Y    = 0;

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
      
      reset();
  }
  CSolveODE_RAD::~CSolveODE_RAD() {
    
      reset();
  }
  void CSolveODE_RAD::reset(void) {
    
    VAtol.clear();
    VRtol.clear();
    Y.clear();
    Y0.clear();
    Scal.clear();
    Zz.clear();
    Ff.clear();
    Fjac.clear();
    Fmas.clear();
    FjacMat.Clear();
    FmasMat.Clear();
    Cont.clear();
    E1.clear();
    Ee2.clear();
    
    Ip1.clear();
    Ip2.clear();
    Iphes.clear();        
  }
  //=================================================
  // Init-Funktion
  //=================================================
  okay_t CSolveODE_RAD::init(CIntegratorRADInp &inp) 
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
        
          N = (uint32_t)inp.n;
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

      //==========
      // Toleranz 
      //==========
      if( inp.itol == 0 ) {//skalar

          Itoler = 0;
          VAtol.resize(1);
          VRtol.resize(1);

          VAtol[0] = fabs(inp.atol);
          VRtol[0] = fabs(inp.rtol);

          if (VAtol[0] == 0.0) {
            mMessage.SetErr(VALUE_IS_ZERO, "Skalar absolute Error must be set != 0.0 \n");
            return mMessage.IsOkay();
          }
          if (VRtol[0] == 0.0) {
            mMessage.SetErr(VALUE_IS_ZERO, "Skalar relative Error must be set != 0.0 \n");
            return mMessage.IsOkay();
          }


      } else { // Vektor

          uint32_t i;

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

              if( VAtol[i] == 0.0 ) {
                errtext.catFormat("CVectorD %i absolute Error must be set != 0.0 \n", i);
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
              if( VRtol[i] == 0.0 ) {
                  errtext.catFormat("CSolveODE_RAD::init: CVectorD %i relative Error must be set != 0.0 \n",i);
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

      if( Nmax == 0 ) {

        errtext.catFormat("CSolveODE_RAD::init: Nmax == 0 \n");
        mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
        return mMessage.IsOkay();
      }
    
      //=================================
      // meth, methode
      //=================================
      Meth = 1; // im Moment nur eine Methode
    
      //=================================
      // Wird Jacobimatrix berechnet
      //=================================
      Ijac = inp.ijac;



      //=================================
      // Band der Jacobimatrix
      // mljac lower bandwidth
      // mljac = n  volle Matrix
      // 0<=mljac<n lower bandwidth unteres Band
      // mujac upper bandwidth, if mljac < n 
      //=================================
      if( inp.mljac == 0 )
          Mljac = N;
      else
          Mljac = (uint32_t)inp.mljac;

      if( inp.mujac == 0  )
          Mujac = N;
      else
          Mujac = (uint32_t)inp.mujac;


      //=================================
      // Wird Massenmatrix berechnet
      //=================================
      Imas = inp.imas;

      //========================================
      // Funktionspointer Mass, wenn notwendig
      //========================================

      //=================================
      // Band der Jacobimatrix
      // mljac lower bandwidth
      // mljac = n  volle Matrix
      // 0<=mljac<n lower bandwidth unteres Band
      // mujac upper bandwidth, if mljac < n 
      //=================================
      Mlmas = inp.mlmas;

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
      Nit = (int32_t)inp.nit;

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

      if( Pred <= 1 )
          Pred = 1;
      else 
          Pred = 0;

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
      Nsmin = inp.nsmin;
      Nsmax = inp.nsmax;
      Ns    = inp.nsus;

      if( Nsmin == 0 )
          Nsmin = 3;
      if( Nsmin >= 2 )
          Nsmin = SLF_MAX(3,Nsmin);
      if( Nsmin >= 4 )
          Nsmin = SLF_MAX(5,Nsmin);
      if( Nsmin >= 6 )
          Nsmin = 7;

      if( Nsmax == 0 )
          Nsmax = 7;

      Nsmax = SLF_MIN(7,Nsmax);

      if( Nsmax <= 6 )
          Nsmax = SLF_MIN(5,Nsmax);
      if( Nsmax <= 4 )
          Nsmax = SLF_MIN(3,Nsmax);
      if( Nsmax <= 2 )
          Nsmax = 1;

      if( Ns == 0 ) 
          Ns = Nsmin;

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

          if( Rtol <= Uround*10. ) {
              errtext.catFormat("CSolveODE_RAD::init: Skalar relative Error must be set > 10*Uround=%g \n",Uround);
              mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
              return mMessage.IsOkay();
          }

      } else { // Vektor

          uint32_t i;


          for(i=0;i<N;i++) {
              if( VRtol[i] <= Uround*10. ) {
                  errtext.catFormat("CSolveODE_RAD::init: CVectorD %i relative Error must be set > 10*Uround:%g \n",i,Uround);
                  mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
                  return mMessage.IsOkay();
              }
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

      //=========================
      // cost factor Jacobimatrix
      //=========================
      Thet = inp.thet;
      if( Thet >= 1.0 )
      {
          errtext.catFormat("CSolveODE_RAD::init: Thet muß kleiner 1.0 liegen thet = %g",Thet);
          mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
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
      // Jacobimatrix
      //=============
      if( Mljac < Nm1 )
          Banded = 1;
      else
          Banded = 0;

      if( Banded ) {

          Ldjac = Mljac+Mujac+1;
          Lde1  = Mljac+Ldjac;
      } else {
          Mljac = Nm1;
          Mujac = Nm1;
          Ldjac = Nm1;
          Lde1  = Nm1;
      }

      //=============
      // Massenmatrix
      //=============
      if( Imas )
          Implct = true;
      else
          Implct = false;

      if( Implct ) {

          if( Mlmas != Nm1 ) {
              Ldmas = Mlmas + Mumas+1;
              if( Banded )
                  Ijob = 4;
              else
                  Ijob = 3;
          } else {

              Ldmas = Nm1;
              Ijob  = 5;
          }

          if( Mlmas > Mljac || Mumas > Mujac ) {
              errtext.catFormat("CSolveODE_RAD::init: bandwidth of mass not snmaller than bandwidth of Jacobian\n");
              mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
              return mMessage.IsOkay();
          }
      } else {

          Ldmas = 0;
          if( Banded ) 
              Ijob = 2;
          else {
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
      NNsmax   = N*Nsmax;
      Nm1Nsmax = Nm1*Nsmax;
      Nmee     = (Nsmax-1)*Nm1;


      //=============
      // Work-Buffers
      //=============
      Y0.resize(N);
      Scal.resize(N);

      Zz.resize(NNsmax);
      Ff.resize(NNsmax);

      Fjac.resize(Ldjac*N);
      Fmas.resize(Ldmas*Nm1);

      // Matrix für die Berechnung in den Funktionen
      FjacMat.SetNdim(Ldjac,N);
      FmasMat.SetNdim(Ldmas,Nm1);

      Cont.resize(NNsmax+N);

      E1.resize(Lde1*Nm1);
      Ee2.resize(Lde1*Nmee);

      Ip1.resize(Nm1);
      Ip2.resize(Nmee/2);
      Iphes.resize(Nm1);

      // Ausgabe solout
      //===============
      Iout = 0; // Keine Ausgabe

      // Startwert Idid
      //===============
      Idid = 1;


      return OKAY;
    
  }
  okay_t CSolveODE_RAD::calcFirst(double &x
                                ,CVectorD &ystart
                                ,double &hstart
                                ,double posneg/*=1*/
                                ) 
  {

      uint32_t i;
      CStr errtext;

      // Startpunkt
      //===========
      X = x;
      H = fabs(hstart);

      // in welche Richtung rechnen wir (zeitlich positiv)
      //==================================================
      Posneg =  SLF_SIGN(posneg,double);
      Hmax   *= Posneg;
      H      *= Posneg;


      // Initialisierungswerte
      //======================
      if( ystart.size() != N ) {

          errtext.catFormat("Initialisierungsvektor ystart(n) = %i, entspricht nicht vorgegebenen Statelänge N = %i\n", ystart.GetNdim(), N);
          mMessage.SetErr(VECTOR_LENGTH, errtext.c_str());
          return mMessage.IsOkay();
      }
      for (i = 0; i < N; i++)
        Y[i] = ystart[i];

      return OKAY;
  }
  okay_t CSolveODE_RAD::calc(double xend) 
  {
    CStr errtext;

    double hmaxn = SLF_MIN(SLF_ABS(Hmax,double),SLF_ABS((xend-X),double));

    Nfcn=Njac=Nstep=Naccpt=Nrejct=Ndec=Nsol=0;

    radau_core( N
              , PObj
              , X
              , Y
              , xend
              , hmaxn
              , H
              , VRtol
              , VAtol
              , Itoler
              , Ns
              , Ijac
              , Mljac
              , Mujac
              , Mlmas
              , Mumas
              , 0
              , Iout
              , Idid
              , Nmax
              , Uround
              , Safe
              , Thet
              , Quot1
              , Quot2
              , Nit
              , Ijob
              , Startn
              , Nind1
              , Nind2
              , Nind3
              , Pred
              , Facl
              , Facr
              , M1
              , M2
              , Nm1
              , Nsmin
              , Nsmax
              , NNsmax
              , Nm1Nsmax
              , Nmee
              , Implct
              , Banded
              , Ldjac
              , Lde1
              , Ldmas
              , Zz
              , Y0
              , Scal
              , Ff
              , Fjac
              , FjacMat
              , E1
              , Ee2
              , Fmas
              , FmasMat
              , Cont
              , Ip1
              , Ip2
              , Iphes
              , Vitu
              , Vitd
              , Hhou
              , Hhod
              , Nfcn
              , Njac
              , Nstep
              , Naccpt
              , Nrejct
              , Ndec
              , Nsol
              , errtext);

       
      if( NstepMax < Nstep )
          NstepMax = Nstep;
    

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
  int CSolveODE_RAD::radau_core(uint32_t &n
    , CModelBase *pobj
    , double &x
    , CVectorD  &y
    , double &xend
    , double &hmax
    , double &h__
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
    , CStr &errtext)
{
    /* Format strings */
    static char fmt_979[] = "(\002 EXIT OF RADAU AT X=\002,e18.4)";
        
    /* System generated locals */
    uint32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e1_dim1, 
        e1_offset, ee2_dim1, ee2_offset, i__1, i__2, i__3, i__4;
    double d__1, d__2, d__3, d__4, d__5;
        
    /* Builtin functions */
    /*
    double sqrt(double), d_sign(double *, double *), pow_dd(
        double *, double *), pow_di(double *, int32_t *);
    int32_t s_wsfe(cilist *), do_fio(int32_t *, char *, ftnlen), e_wsfe(), 
        s_wsle(cilist *), do_lio(int32_t *, int32_t *, char *, ftnlen), 
        e_wsle();
    */

    /* Local variables */
    static uint32_t i__, j, k, l;
    static double a1, a2, a3;
    static uint32_t j1, n2, n3, n4, n5, n6;
    static double u1, c31, dd[7], c32, ak;
    static uint32_t md, in, mm;
    static double qt, dd1, dd2, dd3, ak1, ak2, ak3, f1i, f2i, f3i, c1q, 
        c2q, c3q, sq6;
    static uint32_t nns, lrc;
    static double fac1;
    static uint32_t ier, iad;
    static double xph, z1i, z2i, c31m1, c32m1, z3i;
    static uint32_t nit;
    static double thq;
    static uint32_t in2;
    static double err, ccq, val, z4i, z5i, z6i, beta[7], alph[7];
    static uint8_t last;
    static double expo, c31mc2, hold, hopt, xold, rtol1, atol1;
    static uint32_t lbeg, lend;
    static double delt;
    static uint32_t newt;
    static double hhfac, dyno, dyth;
    static uint32_t ichan, iadd;
    static double fact, betan[7], z7i, fac, quot;
    static uint32_t ikeep;
    static double hnew, hacc, alphn[7], denom, theta, ysafe, hmaxn;
    static uint32_t nsing;
    static double expmi, fnewt;
    static bool first;
    static uint32_t nsnew;
    static bool unexn, unexp;
    static uint32_t irtrn, nrsol, nsolu;
    static double hquot, xosol, qnewt, quott, acont3;
    static bool index1, index2, index3, caljac, change;
    static double faccon;
    static bool calhes;
    static double erracc;
    static bool variab;
    static uint32_t mujacj;
    static double facgus;
    static bool reject;
    static uint32_t mujacp;
    static double thetat, dynold, posneg;
    static double thqold;
    static double expmns;
        
    /* Fortran I/O blocks */
    static cilist io___195 = { 0, 6, 0, fmt_979, 0 };
    static cilist io___196 = { 0, 6, 0, 0, 0 };
    static cilist io___197 = { 0, 6, 0, fmt_979, 0 };
    static cilist io___198 = { 0, 6, 0, 0, 0 };
    static cilist io___199 = { 0, 6, 0, fmt_979, 0 };
    static cilist io___200 = { 0, 6, 0, 0, 0 };
    static cilist io___201 = { 0, 6, 0, fmt_979, 0 };

    //error_t reterr;
    okay_t ret_ok;
        
    /* ---------------------------------------------------------- */
    /*     CORE INTEGRATOR FOR RADAU */
    /*     PARAMETERS SAME AS IN RADAU WITH WORKSPACE ADDED */
    /* ---------------------------------------------------------- */
    /*         DECLARATIONS */
    /* ---------------------------------------------------------- */
    /* --- THIS PARAMETER HAS TO BE CHANGED IF NUMBER OF STAGES IS >=7 */
    /* *** *** *** *** *** *** *** */
    /*  INITIALISATIONS */
    /* *** *** *** *** *** *** *** */
    /* -------- CHECK THE INDEX OF THE PROBLEM ----- */
    /* Parameter adjustments */

    //fjac_dim1 = *ldjac;
    //fjac_offset = 1 + fjac_dim1;
    //fjac -= fjac_offset;
    //ee2_dim1 = *lde1;
    //ee2_offset = 1 + ee2_dim1;
    //ee2 -= ee2_offset;
    //e1_dim1 = *lde1;
    //e1_offset = 1 + e1_dim1;
    //e1 -= e1_offset;
    //fmas_dim1 = *ldmas;
    //fmas_offset = 1 + fmas_dim1;
    //fmas -= fmas_offset;
        
    
    /* Function Body */
    index1 = nind1 != 0;
    index2 = nind2 != 0;
    index3 = nind3 != 0;

    /* ------- COMPUTE MASS MATRIX FOR IMPLICIT CASE ---------- */
    if (implct) 
    {
          ret_ok = pobj->Mass(ldmas, nm1,  fmasmat);

          if (ret_ok != OKAY) 
          {
            idid = -7;
            errtext.catFormat( "Error CSolveODE_RAD::radau_core: during mass-functioncall of <%s> \n%s\n"
                              , pobj->GetName(), pobj->GetErrText());
            return NOT_OKAY;

          }
          else
          {

            i__1 = ldmas;
            //for (j = 1; j <= i__1; ++j)
            //{
            //  i__2 = *nm1;
            //  for (i__ = 1; i__ <= i__2; ++i__)
            //  {
            //    fmas[i__ + j * fmas_dim1] = fmasmat[i__ - 1][j - 1];
            //  }
            //}
            for (j = 0; j < i__1; ++j)
            {
              i__2 = nm1;
              for (i__ = 0; i__ < i__2; ++i__)
              {
                fmas[i__][j] = fmasmat[i__][j];
              }
            }
          }
          //(*mas)(nm1, &fmas[fmas_offset], ldmas, &rpar[1], &ipar[1]);
      }

      variab = nsmin < nsmax;
      
      /* ---------- CONSTANTS --------- */
      expo = 1. / (ns + 1.);
      sq6 = sqrt(6.);
      c31 = (4. - sq6) / 10.;
      c32 = (sq6 + 4.) / 10.;
      c31m1 = c31 - 1.;
      c32m1 = c32 - 1.;
      c31mc2 = c31 - c32;
      dd1 = -(sq6 * 7. + 13.) / 3.;
      dd2 = (sq6 * 7. - 13.) / 3.;
      dd3 = -.33333333333333331;
      n2 = n << 1;
      n3 = n * 3;
      n4 = n << 2;
      n5 = n * 5;
      n6 = n * 6;
      unexp = false;
      unexn = false;
      change = false;
      ikeep = 0;
      ichan = 0;
      theta = 0.;
      thetat = 0.;
      rad_weight.nn = n;
      nns = n * ns;
      rad_weight.ns = ns;
      lrc = nns + n;
      coertv(nsmax);
      coercv(ns, rad_weight.c__, dd, u1, alph, beta);
        if (m1 > 0) 
        {
            ijob += 10;
        }
        d__1 = xend - x;
        posneg = SLF_SIGN(d__1,double);
        /* Computing MIN */
        d__2 = SLF_ABS(hmax, double);
        d__3 = SLF_ABS(d__1,double);
        hmaxn = SLF_MIN(d__2,d__3);
        if (SLF_ABS(h__,double) <= uround * 10.)
        {
            h__ = 1e-6;
        }
        /* Computing MIN */
        h__ = SLF_MIN(fabs(h__),hmaxn);
        h__ *= posneg;
        hold = h__;
        reject = false;
        first = true;
        last = false;
        if( ((x + h__ * 1.0001 - xend) * posneg) >= 0.) 
        {
            h__ = xend - x;
            last = true;
        }
        hopt = h__;
        faccon = 1.;
        nsing = 0;
        xold = x;
        if (iout != 0) 
        {
            irtrn = 1;
            nrsol = 1;
            xosol = xold;
            rad_weight.xsol = x;
           
            for (i__ = 0; i__ < n; ++i__) 
            {
                cont[i__] = y[i__];
            }
            nsolu = n;
            rad_weight.hsol = hold;
            (*solout)(&nrsol, &xosol, &rad_weight.xsol, &y[0], &cont[0], &lrc, &nsolu, 0, 0, &irtrn);
            
            if (irtrn < 0) 
            {
                goto L179;
            }
        }
        rad_linal.mle = mljac;
        rad_linal.mue = mujac;
        rad_linal.mbjac = mljac + mujac + 1;
        rad_linal.mbb = mlmas + mumas + 1;
        rad_linal.mdiag = rad_linal.mle + rad_linal.mue + 1;
        rad_linal.mdiff = rad_linal.mle + rad_linal.mue - mumas;
        rad_linal.mbdiag = mumas + 1;
        expmns = (ns + 1.) / (ns * 2.);
        quott = atol[0] / rtol[0];
        if (itol == 0) {
            rtol1 = pow(rtol[0], expmns) * .1;
            atol1 = rtol1 * quott;
            
            for (i__ = 0; i__ < n; ++i__) 
            {
                scal[i__] = atol1 + rtol1 * SLF_ABS(y[i__],double);
            }
        } else 
        {
            
            for (i__ = 0; i__ < n; ++i__) 
            {
                quott = atol[i__] / rtol[i__];
                rtol1 = pow(rtol[i__], expmns) * .1;
                atol1 = rtol1 * quott;
                scal[i__] = atol1 + rtol1 * SLF_ABS(y[i__],double);
            }
        }
        hhfac = h__;

        // State-Funktions-Aufruf
        //=======================
        ret_ok = pobj->StateDer(n,x, &y[0], &y0[0]);

        if( ret_ok != OKAY ) 
        {
            idid = -5;
            errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                              , pobj->GetName()
                              , pobj->GetErrText()
                              );
            return 0;
        }

        //(*fcn)(n, x, &y[1], &y0[1], &rpar[1], &ipar[1]);
        ++nfcn;
        /* --- BASIC INTEGRATION STEP */
L10:
        /* *** *** *** *** *** *** *** */
        /*  COMPUTATION OF THE JACOBIAN */
        /* *** *** *** *** *** *** *** */
        ++njac;
        if (ijac == 0) 
        {
            /* --- COMPUTE JACOBIAN MATRIX NUMERICALLY */
            if (banded) 
            {
                /* --- JACOBIAN IS BANDED */
                mujacp = mujac + 1;
                md = SLF_MIN(rad_linal.mbjac,m2);
                i__1 = m1 / m2 + 1;
                for (mm = 0; mm < i__1; ++mm) 
                {
                    
                    for (k = 0; k < md; ++k) 
                    {
                        j = k + mm * m2;
L12:
                        ff[j] = y[j];
                        /* Computing MAX */
                        d__2 = 1e-5, d__3 = SLF_ABS(y[j],double);
                        ff[j + n ] = sqrt(uround * SLF_MAX(d__2,d__3));
                        y[j] += ff[j + n];
                        j += md;
                        if (j < mm * m2) {
                            goto L12;
                        }
 
                        // State-Funktions-Aufruf
                        //=======================
                        ret_ok = pobj->StateDer(n,x, &y[0], &cont[0]);

                        if (ret_ok != OKAY)
                        {
                          idid = -5;
                          errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                            , pobj->GetName()
                            , pobj->GetErrText()
                          );
                          return 0;
                        }
  
                        //(*fcn)(n, x, &y[1], &cont[1], &rpar[1], &ipar[1]);
                        j = k + mm * m2;
                        j1 = k;
                        /* Computing MAX */
                        i__3 = 1, i__4 = j1 - mujac;
                        lbeg = SLF_MAX(i__3,i__4) + m1;
L14:
                        /* Computing MIN */
                        i__3 = m2, i__4 = j1 + mljac;
                        lend = SLF_MIN(i__3,i__4) + m1;
                        y[j] = ff[j];
                        mujacj = mujacp - j1 - m1;
                        
                        for (l = lbeg-1; l < lend; ++l) 
                        {
                            fjac[l + mujacj + (j * fjac_dim1) + 1] = (cont[0] - y0[0]) /ff[j + n ];
                        }
                        j += md;
                        j1 += md;
                        lbeg = lend + 1;
                        if (j < mm * m2) 
                        {
                            goto L14;
                        }
                    }
                }
            } else {
                /* --- JACOBIAN IS FULL */
                
                for (i__ = 0; i__ < n; ++i__) 
                {
                    ysafe = y[i__];
                    /* Computing MAX */
                    d__1 = 1e-5, d__2 = SLF_ABS(ysafe,double);
                    delt = sqrt(uround * SLF_MAX(d__1,d__2));
                    y[i__] = ysafe + delt;

                    // State-Funktions-Aufruf
                    //=======================
                    ret_ok = pobj->StateDer(n,x, &y[0], &cont[0]);

                    if (ret_ok != OKAY)
                    {
                      idid = -5;
                      errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                       , pobj->GetName()
                                       , pobj->GetErrText()
                                       );
                      return 0;
                    }
                    
                    //(*fcn)(n, x, &y[1], &cont[1], &rpar[1], &ipar[1]);
                    //i__2 = *n;
                    //for (j = *m1 + 1; j <= i__2; ++j) {
                    //  fjac[j - *m1 + i__ * fjac_dim1] = (cont[j - 1] - y0[j - 1]) /
                    //    delt;
                    //}
                    
                    for (j = m1 ; j < n; ++j) {
                      fjac[j - m1 + i__ * fjac_dim1] = (cont[j] - y0[j]) /delt;
                    }
                    y[i__] = ysafe;
                }
            }
        } else {
              /* --- COMPUTE JACOBIAN MATRIX ANALYTICALLY */
              // Jacobi-Aufruf
              //==============
 
              ret_ok = pobj->Jacobi(n,x, &y[0], fjacmat, ldjac);
 
              if( ret_ok != OKAY ) 
              {
                  idid = -6;
                  errtext.catFormat("Error CSolveODE_RAD::radau_core: during jacobi-functioncall of <%s> \n%s\n"
                                   ,pobj->GetName()
                                   ,pobj->GetErrText()
                                   );
                  return 0;
              } else 
              {

                for (j = 0; j < ldjac; ++j) 
                {
	                
	                for (i__ = 0; i__ < n; ++i__) 
                  {
	                    fjac[(i__+1) + (j+1) * fjac_dim1] = fjacmat[i__][j];
                  }
                }

              }
	

              //(*jac)(n, x, &y[1], &fjac[fjac_offset], ldjac, &rpar[1], &ipar[1]);
          }
        caljac = true;
        calhes = true;
L20:
        /* --- CHANGE THE ORDER HERE IF NECESSARY */
        if (variab) 
        {
            nsnew = ns;
            ++ichan;
            hquot = h__ / hold;
            /* Computing MIN */
            /* Computing MAX */
            d__3 = theta, d__4 = thetat * .5;
            d__1 = 10., d__2 = SLF_MAX(d__3,d__4);
            thetat = SLF_MIN(d__1,d__2);
            if (newt > 1 && thetat <= vitu && hquot < hhou && hquot > hhod) 
            {
                /* Computing MIN */
                nsnew = SLF_MIN(nsmax, ns + 2);
            }
            if (thetat >= vitd || unexp) 
            {
                /* Computing MAX */
                nsnew = SLF_MAX(nsmin, ns - 2);
            }
            if (ichan >= 1 && unexn) 
            {
                /* Computing MAX */
                nsnew = SLF_MAX(nsmin,ns-2);
            }
            if (ichan <= 10) 
            {
                nsnew = SLF_MIN(ns,nsnew);
            }
            change = ns != nsnew;
            unexn = false;
            unexp = false;
            if (change) 
            {
                ns = nsnew;
                ichan = 1;
                nns = n * ns;
                rad_weight.ns = ns;
                lrc = nns + n;
                coercv(ns, rad_weight.c__, dd, u1, alph, beta);
                expo = 1. / (ns + 1.);
                expmns = (ns + 1.) / (ns * 2.);
                rtol1 = pow(rtol[0], expmns) * .1;
                atol1 = rtol1 * quott;
                if (itol == 0) 
                {
                    for (i__ = 0; i__ < n; ++i__) 
                    {
                        scal[i__] = atol1 + rtol1 * SLF_ABS(y[i__],double);
                    }
                } 
                else 
                {
                    for (i__ = 0; i__ < n; ++i__) 
                    {
                        quott = atol[i__] / rtol[i__];
                        rtol1 = pow(rtol[i__], expmns) * .1;
                        atol1 = rtol1 * quott;
                        scal[i__] = atol1 + rtol1 * SLF_ABS(y[i__],double);
                    }
                }
            }
        }
//#### => 6.11
        /* --- COMPUTE THE MATRICES E1 AND E2 AND THEIR DECOMPOSITIONS */
        fac1 = u1 / h__;
        decomr(n, &fjac[fjac_offset], ldjac, &fmas[fmas_offset], ldmas, mlmas, 
            mumas, m1, m2, nm1, &fac1, &e1[e1_offset], lde1, &ip1[0], &ier, 
            ijob, &calhes, &iphes[0]);
        if (ier != 0) {
            goto L78;
        }
        i__1 = (*ns - 1) / 2;
        for (k = 1; k <= i__1; ++k) {
            alphn[k - 1] = alph[k - 1] / *h__;
            betan[k - 1] = beta[k - 1] / *h__;
            iad = ((k - 1) << 1) * *nm1 + 1;
            decomc(n, &fjac[fjac_offset], ldjac, &fmas[fmas_offset], ldmas, 
                mlmas, mumas, m1, m2, nm1, &alphn[k - 1], &betan[k - 1], &ee2[
                iad * ee2_dim1 + 1], &ee2[(iad + *nm1) * ee2_dim1 + 1], lde1, 
                &ip2[(k - 1) * *nm1], &ier, ijob);
            if (ier != 0) {
                goto L78;
            }
        }
        ++(*ndec);
L30:
        if (variab && ikeep == 1) {
            ++ichan;
            ikeep = 0;
            if (ichan >= 10 && *ns < *nsmax) {
                goto L20;
            }
        }
        ++(*nstep);
        if (*nstep > *nmax) {
            goto L178;
        }
        if (SLF_ABS(*h__,double) * .1 <= SLF_ABS(*x,double) * *uround) {
            goto L177;
        }
        if (index2) {
            i__1 = *nind1 + *nind2;
            for (i__ = *nind1 + 1; i__ <= i__1; ++i__) {
                scal[i__-1] /= hhfac;
            }
        }
        if (index3) {
            i__1 = *nind1 + *nind2 + *nind3;
            for (i__ = *nind1 + *nind2 + 1; i__ <= i__1; ++i__) {
                scal[i__-1] /= hhfac * hhfac;
            }
        }
        xph = *x + *h__;
        /* *** *** *** *** *** *** *** */
        /* *** *** *** *** *** *** *** */
        if (*ns == 3) {
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
            if (first || *startn || change) {
                i__1 = nns;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    zz[i__-1] = 0.;
                    ff[i__-1] = 0.;
                }
            } else {
                hquot = *h__ / hold;
                c3q = hquot;
                c1q = c31 * c3q;
                c2q = c32 * c3q;
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    ak1 = cont[i__ + *n - 1];
                    ak2 = cont[i__ + n2 - 1];
                    ak3 = cont[i__ + n3 - 1];
                    z1i = c1q * (ak1 + (c1q - c32m1) * (ak2 + (c1q - c31m1) * ak3)
                        );
                    z2i = c2q * (ak1 + (c2q - c32m1) * (ak2 + (c2q - c31m1) * ak3)
                        );
                    z3i = c3q * (ak1 + (c3q - c32m1) * (ak2 + (c3q - c31m1) * ak3)
                        );
                    zz[i__-1] = z1i;
                    zz[i__ + *n - 1] = z2i;
                    zz[i__ + n2 - 1] = z3i;
                    ff[i__-1] = rad_coe3.ti311 * z1i + rad_coe3.ti312 * z2i + 
                        rad_coe3.ti313 * z3i;
                    ff[i__ + *n - 1] = rad_coe3.ti321 * z1i + rad_coe3.ti322 * z2i + 
                        rad_coe3.ti323 * z3i;
                    ff[i__ + n2 - 1] = rad_coe3.ti331 * z1i + rad_coe3.ti332 * z2i + 
                        rad_coe3.ti333 * z3i;
                }
            }
            /* *** *** *** *** *** *** *** */
            /*  LOOP FOR THE SIMPLIFIED NEWTON ITERATION */
            /* *** *** *** *** *** *** *** */
            newt = 0;
            nit = *nit1;
            expmi = 1. / expmns;
            /* Computing MAX */
            /* Computing MIN */
            d__5 = expmi - 1.;
            d__3 = .03, d__4 = pow(rtol1, d__5);
            d__1 = *uround * 10 / rtol1, d__2 = min(d__3,d__4);
            fnewt = max(d__1,d__2);
            d__1 = max(faccon,*uround);
            faccon = pow(d__1, c_b100);
            theta = abs(*thet);
L40:
            if (newt >= nit) {
                goto L78;
            }
            /* ---     COMPUTE THE RIGHT-HAND SIDE */
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                cont[i__-1] = y[i__-1] + zz[i__-1];
            }
            d__1 = *x + c31 * *h__;
              // State-Funktions-Aufruf
              //=======================
              ret_ok = PObj->StateDer(d__1, cont, zz);

              if( ret_ok != OKAY ) {
                  *idid = -5;
                  errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                   , PObj->getName()
                                   , PObj->getErrText()
                                   );
                  return 0;
              }
            //(*fcn)(n, &d__1, &cont[1], &zz[1], &rpar[1], &ipar[1]);
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                cont[i__-1] = y[i__-1] + zz[i__ + *n - 1];
            }
            d__1 = *x + c32 * *h__;
              // State-Funktions-Aufruf
              //=======================
              ret_ok = PObj->StateDer(d__1, cont, zz[*n]);

              if( ret_ok != OKAY ) {
                  *idid = -5;
                  errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                   , PObj->getName()
                                   , PObj->getErrText()
                                   );
                  return 0;
              }
            //(*fcn)(n, &d__1, &cont[1], &zz[*n + 1], &rpar[1], &ipar[1]);
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                cont[i__-1] = y[i__-1] + zz[i__ + n2 - 1];
            }
              // State-Funktions-Aufruf
              //=======================
              ret_ok = PObj->StateDer(xph, cont, zz[n2]);

              if( ret_ok != OKAY ) {
                  *idid = -5;
                  errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                   , PObj->getName()
                                   , PObj->getErrText()
                                   );
                  return 0;
              }
            //(*fcn)(n, &xph, &cont[1], &zz[n2 + 1], &rpar[1], &ipar[1]);
            *nfcn += 3;
            /* ---     SOLVE THE LINEAR SYSTEMS */
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                a1 = zz[i__-1];
                a2 = zz[i__ + *n - 1];
                a3 = zz[i__ + n2 - 1];
                zz[i__-1] = rad_coe3.ti311 * a1 + rad_coe3.ti312 * a2 + rad_coe3.ti313 * 
                    a3;
                zz[i__ + *n - 1] = rad_coe3.ti321 * a1 + rad_coe3.ti322 * a2 + 
                    rad_coe3.ti323 * a3;
                zz[i__ + n2 - 1] = rad_coe3.ti331 * a1 + rad_coe3.ti332 * a2 + 
                    rad_coe3.ti333 * a3;
            }
            slvrad(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[fmas_offset]
                , ldmas, mlmas, mumas, m1, m2, nm1, &fac1, alphn, betan, &e1[
                e1_offset], &ee2[ee2_offset], &ee2[(*nm1 + 1) * ee2_dim1 + 1],
                lde1, &zz[0], &zz[*n], &zz[n2], &ff[0], &ff[*n], 
                &ff[n2], &cont[0], &ip1[0], &ip2[0], &iphes[0], &ier, 
                ijob);
            ++(*nsol);
            ++newt;
            dyno = 0.;
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                denom = scal[i__-1];
                /* Computing 2nd power */
                d__1 = zz[i__-1] / denom;
                /* Computing 2nd power */
                d__2 = zz[i__ + *n - 1] / denom;
                /* Computing 2nd power */
                d__3 = zz[i__ + n2 - 1] / denom;
                dyno = dyno + d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
            }
            dyno = sqrt(dyno / nns);
            /* ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE */
            if (newt > 1 && newt < nit) {
                thq = dyno / dynold;
                if (newt == 2) {
                    theta = thq;
                } else {
                    theta = sqrt(thq * thqold);
                }
                thqold = thq;
                if (theta < .99) {
                    faccon = theta / (1. - theta);
                    i__1 = nit - 1 - newt;
                    dyth = faccon * dyno * pow(theta, i__1) / fnewt;
                    if (dyth >= 1.) {
                        /* Computing MAX */
                        d__1 = 1e-4, d__2 = min(20.,dyth);
                        qnewt = max(d__1,d__2);
                        d__1 = -1. / (nit + 4. - 1 - newt);
                        hhfac = pow(qnewt, d__1) * .8;
                        *h__ = hhfac * *h__;
                        reject = true;
                        last = false;
                        if (hhfac <= .5) {
                            unexn = true;
                        }
                        if (caljac) {
                            goto L20;
                        }
                        goto L10;
                    }
                } else {
                    goto L78;
                }
            }
            dynold = max(dyno,*uround);
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                in = i__ + *n;
                in2 = i__ + n2;
                f1i = ff[i__-1] + zz[i__-1];
                f2i = ff[in-1] + zz[in-1];
                f3i = ff[in2-1] + zz[in2-1];
                ff[i__-1] = f1i;
                ff[in-1] = f2i;
                ff[in2-1] = f3i;
                zz[i__-1] = rad_coe3.t311 * f1i + rad_coe3.t312 * f2i + rad_coe3.t313 * 
                    f3i;
                zz[in-1] = rad_coe3.t321 * f1i + rad_coe3.t322 * f2i + rad_coe3.t323 * 
                    f3i;
                zz[in2-1] = rad_coe3.t331 * f1i + f2i;
            }
            if (faccon * dyno > fnewt) {
                goto L40;
            }
              /* --- ERROR ESTIMATION */
            estrad(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[fmas_offset]
                , ldmas, mlmas, mumas, h__, &dd1, &dd2, &dd3
                  , PObj
                  , nfcn,
                , nfcn,
                &y0[0], &y[0], ijob, x, m1, m2, nm1, &e1[e1_offset], lde1,
                &zz[0], &zz[*n], &zz[n2], &cont[0], &ff[0], &ff[*n]
                , &ip1[0], &iphes[0], &scal[0], &err, &first, &reject, &fac1, idid
                , errtext);

            if( *idid < 0 ) {
                return 0;
            }
            /*       --- COMPUTE FINITE DIFFERENCES FOR DENSE OUTPUT */
            if (err < 1.) {
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    y[i__-1] += zz[i__ + n2 - 1];
                    z2i = zz[i__ + *n - 1];
                    z1i = zz[i__ - 1];
                    cont[i__ + *n - 1] = (z2i - zz[i__ + n2 - 1]) / c32m1;
                    ak = (z1i - z2i) / c31mc2;
                    acont3 = z1i / c31;
                    acont3 = (ak - acont3) / c32;
                    cont[i__ + n2 - 1] = (ak - cont[i__ + *n - 1]) / c31m1;
                    cont[i__ + n3 - 1] = cont[i__ + n2 - 1] - acont3;
                }
            }
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
    } else {
        if (*ns == 5) {
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
            if (first || *startn || change) {
                i__1 = nns;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    zz[i__-1] = 0.;
                    ff[i__-1] = 0.;
                }
            } else {
                hquot = *h__ / hold;
                i__1 = *ns;
                for (k = 1; k <= i__1; ++k) {
                    ccq = rad_weight.c__[k] * hquot;
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        val = cont[i__ + *ns * *n - 1];
                        for (l = *ns - 1; l >= 1; --l) {
                            val = cont[i__ + l * *n - 1] + (ccq - rad_weight.c__[*
                                ns - l] + 1.) * val;
                        }
                        zz[i__ + (k - 1) * *n - 1] = ccq * val;
                    }
                }
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    z1i = zz[i__-1];
                    z2i = zz[i__ + *n -1];
                    z3i = zz[i__ + n2 - 1];
                    z4i = zz[i__ + n3 - 1];
                    z5i = zz[i__ + n4 - 1];
                    ff[i__-1] = rad_coe5.ti511 * z1i + rad_coe5.ti512 * z2i + 
                        rad_coe5.ti513 * z3i + rad_coe5.ti514 * z4i + 
                        rad_coe5.ti515 * z5i;
                    ff[i__ + *n - 1] = rad_coe5.ti521 * z1i + rad_coe5.ti522 * z2i + 
                        rad_coe5.ti523 * z3i + rad_coe5.ti524 * z4i + 
                        rad_coe5.ti525 * z5i;
                    ff[i__ + n2 - 1] = rad_coe5.ti531 * z1i + rad_coe5.ti532 * z2i + 
                        rad_coe5.ti533 * z3i + rad_coe5.ti534 * z4i + 
                        rad_coe5.ti535 * z5i;
                    ff[i__ + n3 - 1] = rad_coe5.ti541 * z1i + rad_coe5.ti542 * z2i + 
                        rad_coe5.ti543 * z3i + rad_coe5.ti544 * z4i + 
                        rad_coe5.ti545 * z5i;
                    ff[i__ + n4 - 1] = rad_coe5.ti551 * z1i + rad_coe5.ti552 * z2i + 
                        rad_coe5.ti553 * z3i + rad_coe5.ti554 * z4i + 
                        rad_coe5.ti555 * z5i;
                }
            }
            /* *** *** *** *** *** *** *** */
            /*  LOOP FOR THE SIMPLIFIED NEWTON ITERATION */
            /* *** *** *** *** *** *** *** */
            newt = 0;
            nit = *nit1 + 5;
            expmi = 1. / expmns;
            /* Computing MAX */
            /* Computing MIN */
            d__5 = expmi - 1.;
            d__3 = .03, d__4 = pow(rtol1, d__5);
            d__1 = *uround * 10 / rtol1, d__2 = min(d__3,d__4);
            fnewt = max(d__1,d__2);
            d__1 = max(faccon,*uround);
            faccon = pow(d__1, c_b100);
            theta = abs(*thet);
L140:
            if (newt >= nit) {
                goto L78;
            }
            /* ---     COMPUTE THE RIGHT-HAND SIDE */
            i__1 = *ns - 1;
            for (k = 0; k <= i__1; ++k) {
                iadd = k * *n;
                i__2 = *n;
                for (i__ = 1; i__ <= i__2; ++i__) {
                    cont[i__-1] = y[i__-1] + zz[iadd + i__-1];
                }
                d__1 = *x + rad_weight.c__[k + 1] * *h__;
                  // State-Funktions-Aufruf
                  //=======================
                  ret_ok = PObj->StateDer(d__1, cont, &zz[iadd]);

                  if( ret_ok != OKAY ) {
                      *idid = -5;
                      errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                       , PObj->getName()
                                       , PObj->getErrText()
                                       );
                      return 0;
                  }
                  //(*fcn)(n, &d__1, &cont[1], &zz[iadd + 1], &rpar[1], &ipar[1]);
              }
            *nfcn += *ns;
            /* ---     SOLVE THE LINEAR SYSTEMS */
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                z1i = zz[i__-1];
                z2i = zz[i__ + *n - 1];
                z3i = zz[i__ + n2 - 1];
                z4i = zz[i__ + n3 - 1];
                z5i = zz[i__ + n4 - 1];
                zz[i__-1] = rad_coe5.ti511 * z1i + rad_coe5.ti512 * z2i + 
                    rad_coe5.ti513 * z3i + rad_coe5.ti514 * z4i + 
                    rad_coe5.ti515 * z5i;
                zz[i__ + *n - 1] = rad_coe5.ti521 * z1i + rad_coe5.ti522 * z2i + 
                    rad_coe5.ti523 * z3i + rad_coe5.ti524 * z4i + 
                    rad_coe5.ti525 * z5i;
                zz[i__ + n2 -1] = rad_coe5.ti531 * z1i + rad_coe5.ti532 * z2i + 
                    rad_coe5.ti533 * z3i + rad_coe5.ti534 * z4i + 
                    rad_coe5.ti535 * z5i;
                zz[i__ + n3 - 1] = rad_coe5.ti541 * z1i + rad_coe5.ti542 * z2i + 
                    rad_coe5.ti543 * z3i + rad_coe5.ti544 * z4i + 
                    rad_coe5.ti545 * z5i;
                zz[i__ + n4 - 1] = rad_coe5.ti551 * z1i + rad_coe5.ti552 * z2i + 
                    rad_coe5.ti553 * z3i + rad_coe5.ti554 * z4i + 
                    rad_coe5.ti555 * z5i;
            }
            slvrar(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                fmas_offset], ldmas, mlmas, mumas, m1, m2, nm1, &fac1, &
                e1[e1_offset], lde1, &zz[0], &ff[0], &ip1[0], &iphes[0], &
                ier, ijob);
            for (k = 1; k <= 2; ++k) {
                iad = ((k - 1) << 1) * *nm1 + 1;
                slvrai(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                    fmas_offset], ldmas, mlmas, mumas, m1, m2, nm1, &
                    alphn[k - 1], &betan[k - 1], &ee2[iad * ee2_dim1 + 1],
                    &ee2[(iad + *nm1) * ee2_dim1 + 1], lde1, &zz[((k << 
                    1) - 1) * *n], &zz[(k << 1) * *n], &ff[((k << 1) - 1) * *n ]
                    , &ff[(k << 1) * *n ], &cont[0], &
                    ip2[(k - 1) * *nm1], &iphes[0], &ier, ijob);
            }
            ++(*nsol);
            ++newt;
            dyno = 0.;
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                denom = scal[i__-1];
                i__2 = *ns - 1;
                for (k = 0; k <= i__2; ++k) {
                    /* Computing 2nd power */
                    d__1 = zz[i__ + k * *n - 1] / denom;
                    dyno += d__1 * d__1;
                }
            }
            dyno = sqrt(dyno / nns);
            /* ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE */
            if (newt > 1 && newt < nit) {
                thq = dyno / dynold;
                if (newt == 2) {
                    theta = thq;
                } else {
                    theta = sqrt(thq * thqold);
                }
                thqold = thq;
                if (theta < .99) {
                    faccon = theta / (1. - theta);
                    i__1 = nit - 1 - newt;
                    dyth = faccon * dyno * pow(theta, i__1) / fnewt;
                    if (dyth >= 1.) {
                        /* Computing MAX */
                        d__1 = 1e-4, d__2 = min(20.,dyth);
                        qnewt = max(d__1,d__2);
                        d__1 = -1. / (nit + 4. - 1 - newt);
                        hhfac = pow(qnewt, d__1) * .8;
                        *h__ = hhfac * *h__;
                        reject = true;
                        last = false;
                        if (hhfac <= .5) {
                            unexn = true;
                        }
                        if (caljac) {
                            goto L20;
                        }
                        goto L10;
                    }
                } else {
                    goto L78;
                }
            }
            dynold = max(dyno,*uround);
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                z1i = ff[i__-1] + zz[i__ - 1];
                z2i = ff[i__ + *n - 1] + zz[i__ + *n - 1];
                z3i = ff[i__ + n2 - 1] + zz[i__ + n2 - 1];
                z4i = ff[i__ + n3 - 1] + zz[i__ + n3 - 1];
                z5i = ff[i__ + n4 - 1] + zz[i__ + n4 - 1];
                ff[i__ - 1] = z1i;
                ff[i__ + *n - 1] = z2i;
                ff[i__ + n2 -1 ] = z3i;
                ff[i__ + n3 -1 ] = z4i;
                ff[i__ + n4 - 1] = z5i;
                zz[i__-1] = rad_coe5.t511 * z1i + rad_coe5.t512 * z2i + rad_coe5.t513 
                    * z3i + rad_coe5.t514 * z4i + rad_coe5.t515 * z5i;
                zz[i__ + *n - 1] = rad_coe5.t521 * z1i + rad_coe5.t522 * z2i + 
                    rad_coe5.t523 * z3i + rad_coe5.t524 * z4i + rad_coe5.t525 * 
                    z5i;
                zz[i__ + n2 - 1] = rad_coe5.t531 * z1i + rad_coe5.t532 * z2i + 
                    rad_coe5.t533 * z3i + rad_coe5.t534 * z4i + rad_coe5.t535 * 
                    z5i;
                zz[i__ + n3 - 1] = rad_coe5.t541 * z1i + rad_coe5.t542 * z2i + 
                    rad_coe5.t543 * z3i + rad_coe5.t544 * z4i + rad_coe5.t545 * 
                    z5i;
                zz[i__ + n4 - 1] = rad_coe5.t551 * z1i + z2i + z4i;
            }
            if (faccon * dyno > fnewt) {
                goto L140;
            }
            /* --- ERROR ESTIMATION */
            estrav(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                fmas_offset], ldmas, mlmas, mumas, h__, dd
                  , PObj
                ,nfcn, &y0[0], &y[0], ijob, x, m1, m2, nm1, ns, &nns, &e1[
                e1_offset], lde1, &zz[0], &cont[0], &ff[0], &ip1[0], &
                iphes[0], &scal[0], &err, &first, &reject, &fac1, idid, errtext);
            if( *idid < 0 )
                return 0;
            /*       --- COMPUTE FINITE DIFFERENCES FOR DENSE OUTPUT */
            if (err < 1.) {
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    y[i__-1] += zz[i__ + n4 - 1];
                    cont[i__ + n5 - 1] = zz[i__-1] / rad_weight.c__[1];
                }
                i__1 = *ns - 1;
                for (k = 1; k <= i__1; ++k) {
                    fact = 1. / (rad_weight.c__[*ns - k] - rad_weight.c__[*ns - k 
                        + 1]);
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        cont[i__ + k * *n - 1] = (zz[i__ + (*ns - k - 1) * *n - 1] - 
                            zz[i__ + (*ns - k) * *n - 1]) * fact;
                    }
                }
                i__1 = *ns;
                for (j = 2; j <= i__1; ++j) {
                    i__2 = j;
                    for (k = *ns; k >= i__2; --k) {
                        fact = 1. / (rad_weight.c__[*ns - k] - rad_weight.c__[*ns 
                            - k + j]);
                        i__3 = *n;
                        for (i__ = 1; i__ <= i__3; ++i__) {
                            cont[i__ + k * *n - 1] = 
                              (cont[i__ + k * *n - 1] - cont[i__ + (k - 1) * *n - 1]) * fact;
                        }
                    }
                }
            }
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
    } else {
        if (*ns == 7) {
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
            if (first || *startn || change) {
                i__1 = nns;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    zz[i__-1] = 0.;
                    ff[i__-1] = 0.;
                }
            } else {
                hquot = *h__ / hold;
                i__1 = *ns;
                for (k = 1; k <= i__1; ++k) {
                    ccq = rad_weight.c__[k] * hquot;
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        val = cont[i__ + *ns * *n - 1];
                        for (l = *ns - 1; l >= 1; --l) {
                            val = cont[i__ + l * *n - 1] + (ccq - 
                                rad_weight.c__[*ns - l] + 1.) * val;
                        }
                        zz[i__ + (k - 1) * *n - 1] = ccq * val;
                    }
                }
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    z1i = zz[i__-1];
                    z2i = zz[i__ + *n - 1];
                    z3i = zz[i__ + n2 - 1];
                    z4i = zz[i__ + n3 - 1];
                    z5i = zz[i__ + n4 - 1];
                    z6i = zz[i__ + n5 - 1];
                    z7i = zz[i__ + n6 - 1];
                    ff[i__-1] = rad_coe7.ti711 * z1i + rad_coe7.ti712 * z2i + 
                        rad_coe7.ti713 * z3i + rad_coe7.ti714 * z4i + 
                        rad_coe7.ti715 * z5i + rad_coe7.ti716 * z6i + 
                        rad_coe7.ti717 * z7i;
                    ff[i__ + *n - 1] = rad_coe7.ti721 * z1i + rad_coe7.ti722 * 
                        z2i + rad_coe7.ti723 * z3i + rad_coe7.ti724 * z4i 
                        + rad_coe7.ti725 * z5i + rad_coe7.ti726 * z6i + 
                        rad_coe7.ti727 * z7i;
                    ff[i__ + n2 - 1] = rad_coe7.ti731 * z1i + rad_coe7.ti732 * 
                        z2i + rad_coe7.ti733 * z3i + rad_coe7.ti734 * z4i 
                        + rad_coe7.ti735 * z5i + rad_coe7.ti736 * z6i + 
                        rad_coe7.ti737 * z7i;
                    ff[i__ + n3 - 1] = rad_coe7.ti741 * z1i + rad_coe7.ti742 * 
                        z2i + rad_coe7.ti743 * z3i + rad_coe7.ti744 * z4i 
                        + rad_coe7.ti745 * z5i + rad_coe7.ti746 * z6i + 
                        rad_coe7.ti747 * z7i;
                    ff[i__ + n4 - 1] = rad_coe7.ti751 * z1i + rad_coe7.ti752 * 
                        z2i + rad_coe7.ti753 * z3i + rad_coe7.ti754 * z4i 
                        + rad_coe7.ti755 * z5i + rad_coe7.ti756 * z6i + 
                        rad_coe7.ti757 * z7i;
                    ff[i__ + n5 - 1] = rad_coe7.ti761 * z1i + rad_coe7.ti762 * 
                        z2i + rad_coe7.ti763 * z3i + rad_coe7.ti764 * z4i 
                        + rad_coe7.ti765 * z5i + rad_coe7.ti766 * z6i + 
                        rad_coe7.ti767 * z7i;
                    ff[i__ + n6 - 1] = rad_coe7.ti771 * z1i + rad_coe7.ti772 * 
                        z2i + rad_coe7.ti773 * z3i + rad_coe7.ti774 * z4i 
                        + rad_coe7.ti775 * z5i + rad_coe7.ti776 * z6i + 
                        rad_coe7.ti777 * z7i;
                }
            }
            /* *** *** *** *** *** *** *** */
            /*  LOOP FOR THE SIMPLIFIED NEWTON ITERATION */
            /* *** *** *** *** *** *** *** */
            newt = 0;
            nit = *nit1 + 10;
            expmi = 1. / expmns;
            /* Computing MAX */
            /* Computing MIN */
            d__5 = expmi - 1.;
            d__3 = .03, d__4 = pow(rtol1, d__5);
            d__1 = *uround * 10 / rtol1, d__2 = min(d__3,d__4);
            fnewt = max(d__1,d__2);
            d__1 = max(faccon,*uround);
            faccon = pow(d__1, c_b100);
            theta = abs(*thet);
L240:
            if (newt >= nit) {
                goto L78;
            }
            /* ---     COMPUTE THE RIGHT-HAND SIDE */
            i__1 = *ns - 1;
            for (k = 0; k <= i__1; ++k) {
                iadd = k * *n;
                i__2 = *n;
                for (i__ = 1; i__ <= i__2; ++i__) {
                    cont[i__-1] = y[i__-1] + zz[iadd + i__ -1];
                }
                d__1 = *x + rad_weight.c__[k + 1] * *h__;
                  // State-Funktions-Aufruf
                  //=======================
                  ret_ok = PObj->StateDer(d__1, cont, &zz[iadd]);
                  if( ret_ok != OKAY ) {
                      *idid = -5;
                      errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                       , PObj->getName()
                                       , PObj->getErrText()
                                       );
                      return 0;
                  }
                  //(*fcn)(n, &d__1, &cont[1], &zz[iadd + 1], 0, 0);
              }
            *nfcn += *ns;
            /* ---     SOLVE THE LINEAR SYSTEMS */
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                z1i = zz[i__-1];
                z2i = zz[i__ + *n - 1];
                z3i = zz[i__ + n2 - 1];
                z4i = zz[i__ + n3 - 1];
                z5i = zz[i__ + n4 - 1];
                z6i = zz[i__ + n5 - 1];
                z7i = zz[i__ + n6 - 1];
                zz[i__-1] = rad_coe7.ti711 * z1i + rad_coe7.ti712 * z2i + 
                    rad_coe7.ti713 * z3i + rad_coe7.ti714 * z4i + 
                    rad_coe7.ti715 * z5i + rad_coe7.ti716 * z6i + 
                    rad_coe7.ti717 * z7i;
                zz[i__ + *n - 1] = rad_coe7.ti721 * z1i + rad_coe7.ti722 * z2i + 
                    rad_coe7.ti723 * z3i + rad_coe7.ti724 * z4i + 
                    rad_coe7.ti725 * z5i + rad_coe7.ti726 * z6i + 
                    rad_coe7.ti727 * z7i;
                zz[i__ + n2 - 1] = rad_coe7.ti731 * z1i + rad_coe7.ti732 * z2i + 
                    rad_coe7.ti733 * z3i + rad_coe7.ti734 * z4i + 
                    rad_coe7.ti735 * z5i + rad_coe7.ti736 * z6i + 
                    rad_coe7.ti737 * z7i;
                zz[i__ + n3 - 1] = rad_coe7.ti741 * z1i + rad_coe7.ti742 * z2i + 
                    rad_coe7.ti743 * z3i + rad_coe7.ti744 * z4i + 
                    rad_coe7.ti745 * z5i + rad_coe7.ti746 * z6i + 
                    rad_coe7.ti747 * z7i;
                zz[i__ + n4 - 1] = rad_coe7.ti751 * z1i + rad_coe7.ti752 * z2i + 
                    rad_coe7.ti753 * z3i + rad_coe7.ti754 * z4i + 
                    rad_coe7.ti755 * z5i + rad_coe7.ti756 * z6i + 
                    rad_coe7.ti757 * z7i;
                zz[i__ + n5 - 1] = rad_coe7.ti761 * z1i + rad_coe7.ti762 * z2i + 
                    rad_coe7.ti763 * z3i + rad_coe7.ti764 * z4i + 
                    rad_coe7.ti765 * z5i + rad_coe7.ti766 * z6i + 
                    rad_coe7.ti767 * z7i;
                zz[i__ + n6 - 1] = rad_coe7.ti771 * z1i + rad_coe7.ti772 * z2i + 
                    rad_coe7.ti773 * z3i + rad_coe7.ti774 * z4i + 
                    rad_coe7.ti775 * z5i + rad_coe7.ti776 * z6i + 
                    rad_coe7.ti777 * z7i;
            }
            slvrar(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                fmas_offset], ldmas, mlmas, mumas, m1, m2, nm1, &fac1,
                &e1[e1_offset], lde1, &zz[0], &ff[0], &ip1[0], &
                iphes[0], &ier, ijob);
            for (k = 1; k <= 3; ++k) {
                iad = ((k - 1) << 1) * *nm1 + 1;
                slvrai(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                    fmas_offset], ldmas, mlmas, mumas, m1, m2, nm1, &
                    alphn[k - 1], &betan[k - 1], &ee2[iad * ee2_dim1 
                    + 1], &ee2[(iad + *nm1) * ee2_dim1 + 1], lde1, &
                    zz[((k << 1) - 1) * *n], &zz[(k << 1) * *n]
                    , &ff[((k << 1) - 1) * *n], &ff[(k << 1) * *
                    n], &cont[0], &ip2[(k - 1) * *nm1], &
                    iphes[0], &ier, ijob);
            }
            ++(*nsol);
            ++newt;
            dyno = 0.;
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                denom = scal[i__-1];
                i__2 = *ns - 1;
                for (k = 0; k <= i__2; ++k) {
                    /* Computing 2nd power */
                    d__1 = zz[i__ + k * *n - 1] / denom;
                    dyno += d__1 * d__1;
                }
            }
            dyno = sqrt(dyno / nns);
            /* ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE */
            if (newt > 1 && newt < nit) {
                thq = dyno / dynold;
                if (newt == 2) {
                    theta = thq;
                } else {
                    theta = sqrt(thq * thqold);
                }
                thqold = thq;
                if (theta < .99) {
                    faccon = theta / (1. - theta);
                    i__1 = nit - 1 - newt;
                    dyth = faccon * dyno * pow(theta, i__1) / fnewt;
                    if (dyth >= 1.) {
                        /* Computing MAX */
                        d__1 = 1e-4, d__2 = min(20.,dyth);
                        qnewt = max(d__1,d__2);
                        d__1 = -1. / (nit + 4. - 1 - newt);
                        hhfac = pow(qnewt, d__1) * .8;
                        *h__ = hhfac * *h__;
                        reject = true;
                        last = false;
                        if (hhfac <= .5) {
                            unexn = true;
                        }
                        if (caljac) {
                            goto L20;
                        }
                        goto L10;
                    }
                } else {
                    goto L78;
                }
            }
            dynold = max(dyno,*uround);
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                z1i = ff[i__-1] + zz[i__-1];
                z2i = ff[i__ + *n-1] + zz[i__ + *n - 1];
                z3i = ff[i__ + n2-1] + zz[i__ + n2 - 1];
                z4i = ff[i__ + n3-1] + zz[i__ + n3 - 1];
                z5i = ff[i__ + n4-1] + zz[i__ + n4 - 1];
                z6i = ff[i__ + n5-1] + zz[i__ + n5 - 1];
                z7i = ff[i__ + n6-1] + zz[i__ + n6 - 1];
                ff[i__- 1] = z1i;
                ff[i__ + *n - 1] = z2i;
                ff[i__ + n2 - 1] = z3i;
                ff[i__ + n3 - 1] = z4i;
                ff[i__ + n4 - 1] = z5i;
                ff[i__ + n5 - 1] = z6i;
                ff[i__ + n6 - 1] = z7i;
                zz[i__-1] = rad_coe7.t711 * z1i + rad_coe7.t712 * z2i + 
                    rad_coe7.t713 * z3i + rad_coe7.t714 * z4i + 
                    rad_coe7.t715 * z5i + rad_coe7.t716 * z6i + 
                    rad_coe7.t717 * z7i;
                zz[i__ + *n - 1] = rad_coe7.t721 * z1i + rad_coe7.t722 * z2i + 
                    rad_coe7.t723 * z3i + rad_coe7.t724 * z4i + 
                    rad_coe7.t725 * z5i + rad_coe7.t726 * z6i + 
                    rad_coe7.t727 * z7i;
                zz[i__ + n2 - 1] = rad_coe7.t731 * z1i + rad_coe7.t732 * z2i + 
                    rad_coe7.t733 * z3i + rad_coe7.t734 * z4i + 
                    rad_coe7.t735 * z5i + rad_coe7.t736 * z6i + 
                    rad_coe7.t737 * z7i;
                zz[i__ + n3 - 1] = rad_coe7.t741 * z1i + rad_coe7.t742 * z2i + 
                    rad_coe7.t743 * z3i + rad_coe7.t744 * z4i + 
                    rad_coe7.t745 * z5i + rad_coe7.t746 * z6i + 
                    rad_coe7.t747 * z7i;
                zz[i__ + n4 - 1] = rad_coe7.t751 * z1i + rad_coe7.t752 * z2i + 
                    rad_coe7.t753 * z3i + rad_coe7.t754 * z4i + 
                    rad_coe7.t755 * z5i + rad_coe7.t756 * z6i + 
                    rad_coe7.t757 * z7i;
                zz[i__ + n5 - 1] = rad_coe7.t761 * z1i + rad_coe7.t762 * z2i + 
                    rad_coe7.t763 * z3i + rad_coe7.t764 * z4i + 
                    rad_coe7.t765 * z5i + rad_coe7.t766 * z6i + 
                    rad_coe7.t767 * z7i;
                zz[i__ + n6 - 1] = rad_coe7.t771 * z1i + z2i + z4i + z6i;
            }
            if (faccon * dyno > fnewt) {
                goto L240;
            }
            /* --- ERROR ESTIMATION */
            estrav(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                fmas_offset], ldmas, mlmas, mumas, h__, dd
                  , PObj
                ,nfcn, &y0[0], &y[0], ijob, x, m1, m2, nm1, ns, &nns, 
                &e1[e1_offset], lde1, &zz[0], &cont[0], &ff[0], &ip1[0]
                , &iphes[0], &scal[0], &err, &first, &reject, &fac1
                , idid, errtext);

            /*       --- COMPUTE FINITE DIFFERENCES FOR DENSE OUTPUT */
            if (err < 1.) {
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    y[i__-1] += zz[i__ + (*ns - 1) * *n - 1];
                    cont[i__ + *ns * *n - 1] = zz[i__-1] / rad_weight.c__[1];
                }
                i__1 = *ns - 1;
                for (k = 1; k <= i__1; ++k) {
                    fact = 1. / (rad_weight.c__[*ns - k] - rad_weight.c__[*ns 
                        - k + 1]);
                    i__2 = *n;
                    for (i__ = 1; i__ <= i__2; ++i__) {
                        cont[i__ + k * *n - 1] = (zz[i__ + (*ns - k - 1) * *n - 1]
                            - zz[i__ + (*ns - k) * *n - 1]) * fact;
                    }
                }
                i__1 = *ns;
                for (j = 2; j <= i__1; ++j) {
                    i__2 = j;
                    for (k = *ns; k >= i__2; --k) {
                        fact = 1. / (rad_weight.c__[*ns - k] - rad_weight.c__[
                            *ns - k + j]);
                        i__3 = *n;
                        for (i__ = 1; i__ <= i__3; ++i__) {
                            cont[i__ + k * *n - 1] = (cont[i__ + k * *n - 1] - 
                                cont[i__ + (k - 1) * *n - 1]) * fact;
                        }
                    }
                }
            }
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
        } else {
            /* ASE       (NS.EQ.1) */
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
            if (first || *startn || change) {
                i__1 = *ns;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    zz[i__-1] = 0.;
                    ff[i__-1] = 0.;
                }
            } else {
                hquot = *h__ / hold;
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    z1i = hquot * cont[i__ + *n - 1];
                    zz[i__-1] = z1i;
                    ff[i__-1] = z1i;
                }
            }
            /* *** *** *** *** *** *** *** */
            /*  LOOP FOR THE SIMPLIFIED NEWTON ITERATION */
            /* *** *** *** *** *** *** *** */
            newt = 0;
            nit = *nit1 - 3;
            expmi = 1. / expmns;
            /* Computing MAX */
            d__1 = *uround * 10 / rtol1;
            fnewt = max(d__1,.03);
            d__1 = max(faccon,*uround);
            faccon = pow(d__1, c_b100);
            theta = abs(*thet);
L440:
            if (newt >= nit) {
                goto L78;
            }
            /* ---     COMPUTE THE RIGHT-HAND SIDE */
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                cont[i__-1] = y[i__-1] + zz[i__-1];
            }
              // State-Funktions-Aufruf
              //=======================
              ret_ok = PObj->StateDer(xph, cont, &zz[0]);

              if( ret_ok != OKAY ) {
                  *idid = -5;
                  errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                                   , PObj->getName()
                                   , PObj->getErrText()
                                   );
                  return 0;
              }

            //(*fcn)(n, &xph, &cont[1], &zz[1], 0, 0);
            ++(*nfcn);
            /* ---     SOLVE THE LINEAR SYSTEMS */
            slvrar(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                fmas_offset], ldmas, mlmas, mumas, m1, m2, nm1, &fac1,
                &e1[e1_offset], lde1, &zz[0], &ff[0], &ip1[0], &
                iphes[0], &ier, ijob);
            ++(*nsol);
            ++newt;
            dyno = 0.;
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                denom = scal[i__-1];
                /* Computing 2nd power */
                d__1 = zz[i__-1] / denom;
                dyno += d__1 * d__1;
            }
            dyno = sqrt(dyno / nns);
            /* ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE */
            if (newt > 1 && newt < nit) {
                thq = dyno / dynold;
                if (newt == 2) {
                    theta = thq;
                } else {
                    theta = sqrt(thq * thqold);
                }
                thqold = thq;
                if (theta < .99) {
                    faccon = theta / (1. - theta);
                    i__1 = nit - 1 - newt;
                    dyth = faccon * dyno * pow(theta, i__1) / fnewt;
                    if (dyth >= 1.) {
                        /* Computing MAX */
                        d__1 = 1e-4, d__2 = min(20.,dyth);
                        qnewt = max(d__1,d__2);
                        d__1 = -1. / (nit + 4. - 1 - newt);
                        hhfac = pow(qnewt, d__1) * .8;
                        *h__ = hhfac * *h__;
                        reject = true;
                        last = false;
                        if (hhfac <= .5) {
                            unexn = true;
                        }
                        if (caljac) {
                            goto L20;
                        }
                        goto L10;
                    }
                } else {
                    goto L78;
                }
            }
            dynold = max(dyno,*uround);
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                f1i = ff[i__-1] + zz[i__-1];
                ff[i__-1] = f1i;
                zz[i__-1] = f1i;
            }
            if (faccon * dyno > fnewt) {
                goto L440;
            }
            /* --- ERROR ESTIMATION */
            estrav(n, &fjac[fjac_offset], ldjac, mljac, mujac, &fmas[
                fmas_offset], ldmas, mlmas, mumas, h__, dd
                  , PObj
                ,nfcn, &y0[0], &y[0], ijob, x, m1, m2, nm1, ns, &nns, 
                &e1[e1_offset], lde1, &zz[0], &cont[0], &ff[0], &ip1[0]
                , &iphes[0], &scal[0], &err, &first, &reject, &fac1
                , idid, errtext);
            if( *idid < 0 )
                return 0;
            /*       --- COMPUTE FINITE DIFFERENCES FOR DENSE OUTPUT */
            if (err < 1.) {
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    y[i__-1] += zz[i__-1];
                    cont[i__ + *n - 1] = zz[i__-1];
                }
            }
            /* *** *** *** *** *** *** *** */
            /* *** *** *** *** *** *** *** */
        }
    }
    }
    /* *** *** *** *** *** *** *** */
    /* *** *** *** *** *** *** *** */
    /* --- COMPUTATION OF HNEW */
    /* --- WE REQUIRE .2<=HNEW/H<=8. */
    /* Computing MIN */
    d__1 = *safe, d__2 = ((nit << 1) + 1) * *safe / (newt + (nit << 1));
    fac = min(d__1,d__2);
    /* Computing MAX */
    /* Computing MIN */
    d__3 = *facl, d__4 = pow(err, expo) / fac;
    d__1 = *facr, d__2 = min(d__3,d__4);
    quot = max(d__1,d__2);
    hnew = *h__ / quot;
    /* *** *** *** *** *** *** *** */
    /*  IS THE ERROR SMALL ENOUGH ? */
    /* *** *** *** *** *** *** *** */
    if (err < 1.) {
        /* --- STEP IS ACCEPTED */
        first = false;
        ++(*naccpt);
        if (*pred && ! change) {
            /*       --- PREDICTIVE CONTROLLER OF GUSTAFSSON */
            if (*naccpt > 1) {
                /* Computing 2nd power */
                d__2 = err;
                d__1 = d__2 * d__2 / erracc;
                facgus = hacc / *h__ * pow(d__1, expo) / *safe;
                /* Computing MAX */
                d__1 = *facr, d__2 = min(*facl,facgus);
                facgus = max(d__1,d__2);
                quot = max(quot,facgus);
                hnew = *h__ / quot;
            }
            hacc = *h__;
            erracc = max(.01,err);
        }
        xold = *x;
        hold = *h__;
        *x = xph;
        /*       --- UPDATE SCALING */
        if (*itol == 0) {
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                scal[i__-1] = atol1 + rtol1 * (d__1 = y[i__-1], abs(d__1));
            }
        } else {
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                quott = atol[i__-1] / rtol[i__-1];
                rtol1 = pow(rtol[i__-1], expmns) * .1;
                atol1 = rtol1 * quott;
                scal[i__-1] = atol1 + rtol1 * (d__1 = y[i__-1], abs(d__1));
            }
        }
        if (*iout != 0) {
            nrsol = *naccpt + 1;
            rad_weight.xsol = *x;
            xosol = xold;
            i__1 = *n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                cont[i__-1] = y[i__-1];
            }
            nsolu = *n;
            rad_weight.hsol = hold;
            (*solout)(&nrsol, &xosol, &rad_weight.xsol, &y[0], &cont[0], &lrc, &
                nsolu, 0, 0, &irtrn);
            if (irtrn < 0) {
                goto L179;
            }
        }
        caljac = false;
        if (last) {
            *h__ = hopt;
            *idid = 1;
            return 0;
        }
          // State-Funktions-Aufruf
          //=======================
          ret_ok = PObj->StateDer(*x, y, y0);
          
          if( ret_ok != OKAY ) {
              *idid = -5;
              errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
                               , PObj->getName()
                               , PObj->getErrText()
                               );
              return 0;
          }
        //(*fcn)(n, x, &y[1], &y0[1], 0, 0);
        ++(*nfcn);
        /* Computing MIN */
        d__1 = abs(hnew);
        hnew = posneg * min(d__1,hmaxn);
        hopt = hnew;
        hopt = min(*h__,hnew);
        if (reject) {
            /* Computing MIN */
            d__1 = abs(hnew), d__2 = abs(*h__);
            hnew = posneg * min(d__1,d__2);
        }
        reject = false;
        if ((*x + hnew / *quot1 - *xend) * posneg >= 0.) {
            *h__ = *xend - *x;
            last = true;
        } else {
            qt = hnew / *h__;
            hhfac = *h__;
            if (theta <= *thet && qt >= *quot1 && qt <= *quot2) {
                ikeep = 1;
                goto L30;
            }
            *h__ = hnew;
        }
        hhfac = *h__;
        if (theta <= *thet) {
            goto L20;
        }
        goto L10;
    } else {
        /* --- STEP IS REJECTED */
        reject = true;
        last = false;
        if (first) {
            *h__ *= .1;
            hhfac = .1;
        } else {
            hhfac = hnew / *h__;
            *h__ = hnew;
        }
        if (*naccpt >= 1) {
            ++(*nrejct);
        }
        if (caljac) {
            goto L20;
        }
        goto L10;
    }
    /* --- UNEXPECTED STEP-REJECTION */
L78:
    unexp = true;
    if (ier != 0) {
        ++nsing;
        if (nsing >= 5) {
            goto L176;
        }
    }
    *h__ *= .5;
    hhfac = .5;
    reject = true;
    last = false;
    if (caljac) {
        goto L20;
    }
    goto L10;
    /* --- FAIL EXIT */
L176:
    errtext.catFormat("Error CIntegratorRAD::radau_core: X = %g, Matrix is repeatedley singular, ierr=%li\n"
                     ,*x,ier);
    *idid = -4;
    return 0;
L177:
    errtext.catFormat("Error CIntegratorRAD::radau_core: X = %g, STEP SIZE T0O SMALL, H = %g\n"
                     ,*x,*h__);
    *idid = -3;
    return 0;
L178:
    errtext.catFormat("Error CIntegratorRAD::radau_core: X = %g, MORE THAN NMAX = %li STEPS ARE NEEDED\n"
                     ,*x,*nmax);
    *idid = -2;
    return 0;
    /* --- EXIT CAUSED BY SOLOUT */
L179:
    /*
    s_wsfe(&io___201);
    do_fio(&c__1, (char *)&(*x), (ftnlen)sizeof(double));
    e_wsfe();
    */
    *idid = 2;
    return 0;
} /* radau_core */


/*     END OF SUBROUTINE RADCOV */

/* *********************************************************** */

int CIntegratorRAD::coertv(const uint32_t &nsmax)
{
    /* --- */
    rad_coe3.t311 = .09123239487089294279155;
    rad_coe3.t312 = -.141255295020954208428;
    rad_coe3.t313 = -.03002919410514742449186;
    rad_coe3.t321 = .2417179327071070189575;
    rad_coe3.t322 = .204129352293799931996;
    rad_coe3.t323 = .3829421127572619377954;
    rad_coe3.t331 = .9660481826150929361906;
    rad_coe3.ti311 = 4.325579890063155351024;
    rad_coe3.ti312 = .3391992518158098695428;
    rad_coe3.ti313 = .5417705399358748711865;
    rad_coe3.ti321 = -4.178718591551904727346;
    rad_coe3.ti322 = -.3276828207610623870825;
    rad_coe3.ti323 = .4766235545005504519601;
    rad_coe3.ti331 = -.5028726349457868759512;
    rad_coe3.ti332 = 2.571926949855605429187;
    rad_coe3.ti333 = -.5960392048282249249688;
    if (nsmax <= 3) {
        return 0;
    }
    rad_coe5.t511 = -.01251758622050104589014;
    rad_coe5.t512 = -.01024204781790882707009;
    rad_coe5.t513 = .04767387729029572386318;
    rad_coe5.t514 = -.01147851525522951470794;
    rad_coe5.t515 = -.01401985889287541028108;
    rad_coe5.t521 = -.001491670151895382429004;
    rad_coe5.t522 = .05017286451737105816299;
    rad_coe5.t523 = -.09433181918161143698066;
    rad_coe5.t524 = -.007668830749180162885157;
    rad_coe5.t525 = .02470857842651852681253;
    rad_coe5.t531 = -.07298187638808714862266;
    rad_coe5.t532 = -.2305395340434179467214;
    rad_coe5.t533 = .1027030453801258997922;
    rad_coe5.t534 = .01939846399882895091122;
    rad_coe5.t535 = .08180035370375117083639;
    rad_coe5.t541 = -.3800914400035681041264;
    rad_coe5.t542 = .3778939022488612495439;
    rad_coe5.t543 = .4667441303324943592896;
    rad_coe5.t544 = .4076011712801990666217;
    rad_coe5.t545 = .1996824278868025259365;
    rad_coe5.t551 = -.9219789736812104884883;
    rad_coe5.ti511 = -30.04156772154440162771;
    rad_coe5.ti512 = -13.86510785627141316518;
    rad_coe5.ti513 = -3.480002774795185561828;
    rad_coe5.ti514 = 1.032008797825263422771;
    rad_coe5.ti515 = -.8043030450739899174753;
    rad_coe5.ti521 = 5.344186437834911598895;
    rad_coe5.ti522 = 4.593615567759161004454;
    rad_coe5.ti523 = -3.036360323459424298646;
    rad_coe5.ti524 = 1.05066019023145886386;
    rad_coe5.ti525 = -.2727786118642962705386;
    rad_coe5.ti531 = 3.748059807439804860051;
    rad_coe5.ti532 = -3.984965736343884667252;
    rad_coe5.ti533 = -1.044415641608018792942;
    rad_coe5.ti534 = 1.184098568137948487231;
    rad_coe5.ti535 = -.4499177701567803688988;
    rad_coe5.ti541 = -33.04188021351900000806;
    rad_coe5.ti542 = -17.37695347906356701945;
    rad_coe5.ti543 = -.1721290632540055611515;
    rad_coe5.ti544 = -.09916977798254264258817;
    rad_coe5.ti545 = .5312281158383066671849;
    rad_coe5.ti551 = -8.6114439798752919777;
    rad_coe5.ti552 = 9.699991409528808231336;
    rad_coe5.ti553 = 1.914728639696874284851;
    rad_coe5.ti554 = 2.418692006084940026427;
    rad_coe5.ti555 = -1.047463487935337418694;
    if (nsmax <= 5) {
        return 0;
    }
    rad_coe7.t711 = -.002153754627310526422828;
    rad_coe7.t712 = .02156755135132077338691;
    rad_coe7.t713 = .008783567925144144407326;
    rad_coe7.t714 = -.004055161452331023898198;
    rad_coe7.t715 = .004427232753268285479678;
    rad_coe7.t716 = -.001238646187952874056377;
    rad_coe7.t717 = -.002760617480543852499548;
    rad_coe7.t721 = .001600025077880428526831;
    rad_coe7.t722 = -.03813164813441154669442;
    rad_coe7.t723 = -.02152556059400687552385;
    rad_coe7.t724 = .008415568276559589237177;
    rad_coe7.t725 = -.004031949570224549492304;
    rad_coe7.t726 = -6.666635339396338181761e-5;
    rad_coe7.t727 = .003185474825166209848748;
    rad_coe7.t731 = -.00405910730194768309165;
    rad_coe7.t732 = .05739650893938171539757;
    rad_coe7.t733 = .05885052920842679105612;
    rad_coe7.t734 = -.008560431061603432060177;
    rad_coe7.t735 = -.006923212665023908924141;
    rad_coe7.t736 = -.002352180982943338340535;
    rad_coe7.t737 = 4.169077725297562691409e-4;
    rad_coe7.t741 = -.01575048807937684420346;
    rad_coe7.t742 = -.03821469359696835048464;
    rad_coe7.t743 = -.1657368112729438512412;
    rad_coe7.t744 = -.03737124230238445741907;
    rad_coe7.t745 = .008239007298507719404499;
    rad_coe7.t746 = .003115071152346175252726;
    rad_coe7.t747 = .02511660491343882192836;
    rad_coe7.t751 = -.1129776610242208076086;
    rad_coe7.t752 = -.2491742124652636863308;
    rad_coe7.t753 = .2735633057986623212132;
    rad_coe7.t754 = .005366761379181770094279;
    rad_coe7.t755 = .1932111161012620144312;
    rad_coe7.t756 = .1017177324817151468081;
    rad_coe7.t757 = .09504502035604622821039;
    rad_coe7.t761 = -.4583810431839315010281;
    rad_coe7.t762 = .5315846490836284292051;
    rad_coe7.t763 = .4863228366175728940567;
    rad_coe7.t764 = .5265742264584492629141;
    rad_coe7.t765 = .2755343949896258141929;
    rad_coe7.t766 = .5217519452747652852946;
    rad_coe7.t767 = .1280719446355438944141;
    rad_coe7.t771 = -.8813915783538183763135;
    rad_coe7.ti711 = -258.1319263199822292761;
    rad_coe7.ti712 = -189.073763081398508952;
    rad_coe7.ti713 = -49.08731481793013119445;
    rad_coe7.ti714 = -4.110647469661428418112;
    rad_coe7.ti715 = -4.053447889315563304175;
    rad_coe7.ti716 = 3.112755366607346076554;
    rad_coe7.ti717 = -1.646774913558444650169;
    rad_coe7.ti721 = -3.007390169451292131731;
    rad_coe7.ti722 = -11.01586607876577132911;
    rad_coe7.ti723 = 1.487799456131656281486;
    rad_coe7.ti724 = 2.130388159559282459432;
    rad_coe7.ti725 = -1.816141086817565624822;
    rad_coe7.ti726 = 1.134325587895161100083;
    rad_coe7.ti727 = -.414699045943303531993;
    rad_coe7.ti731 = -8.441963188321084681757;
    rad_coe7.ti732 = -.6505252740575150028169;
    rad_coe7.ti733 = 6.940670730369876478804;
    rad_coe7.ti734 = -3.205047525597898431565;
    rad_coe7.ti735 = 1.071280943546478589783;
    rad_coe7.ti736 = -.354850749121622187973;
    rad_coe7.ti737 = .09198549132786554154409;
    rad_coe7.ti741 = 74.67833223502269977153;
    rad_coe7.ti742 = 87.40858897990081640204;
    rad_coe7.ti743 = 4.024158737379997877014;
    rad_coe7.ti744 = -3.714806315158364186639;
    rad_coe7.ti745 = -3.430093985982317350741;
    rad_coe7.ti746 = 2.696604809765312378853;
    rad_coe7.ti747 = -.9386927436075461933568;
    rad_coe7.ti751 = 58.35652885190657724237;
    rad_coe7.ti752 = -10.06877395780018096325;
    rad_coe7.ti753 = -30.36638884256667120811;
    rad_coe7.ti754 = -1.020020865184865985027;
    rad_coe7.ti755 = -.1124175003784249621267;
    rad_coe7.ti756 = 1.8906408310003776228;
    rad_coe7.ti757 = -.9716486393831482282172;
    rad_coe7.ti761 = -299.1862480282520966786;
    rad_coe7.ti762 = -243.0407453687447911819;
    rad_coe7.ti763 = -48.77710407803786921219;
    rad_coe7.ti764 = -2.03867190574193440528;
    rad_coe7.ti765 = 1.673560239861084944268;
    rad_coe7.ti766 = -1.087374032057106164456;
    rad_coe7.ti767 = .9019382492960993738427;
    rad_coe7.ti771 = -93.07650289743530591157;
    rad_coe7.ti772 = 23.88163105628114427703;
    rad_coe7.ti773 = 39.2788807308138438271;
    rad_coe7.ti774 = 14.38891568549108006988;
    rad_coe7.ti775 = -3.510438399399361221087;
    rad_coe7.ti776 = 4.863284885566180701215;
    rad_coe7.ti777 = -2.2464827295912399164;
    return 0;
} /* coertv_ */


/*     END OF SUBROUTINE COERTV */

/* *********************************************************** */

int CIntegratorRAD::coercv(uint32_t &ns, double *c__, double *dd, 
double &u1, double *alph, double *beta)
{
    /* System generated locals */
    double d__1, d__2;
    
    /* Builtin functions */
    //double sqrt(double), pow_dd(double *, double *);
    
    /* Local variables */
    static double sq6, st9, bet, alp, cno;
    
    /* Parameter adjustments */
    --beta;
    --alph;
    --dd;
    
    /* Function Body */
    c__[0] = 0.;
    c__[ns] = 1.;
    switch (ns) 
    {
    case 1:  goto L1;
    case 2:  goto L11;
    case 3:  goto L3;
    case 4:  goto L11;
    case 5:  goto L5;
    case 6:  goto L11;
    case 7:  goto L7;
    }
L11:
    return 0;
L1:
    c__[1] = 1.;
    u1 = 1.;
    dd[1] = -1.;
    return 0;
L3:
    sq6 = sqrt(6.);
    c__[1] = (4. - sq6) / 10.;
    c__[2] = (sq6 + 4.) / 10.;
    st9 = pow(c_b140, c_b141);
    u1 = (st9 * (st9 - 1) + 6.) / 30.;
    alp = (12. - st9 * (st9 - 1)) / 60.;
    bet = st9 * (st9 + 1) * sqrt(3.) / 60.;
    /* Computing 2nd power */
    d__1 = alp;
    /* Computing 2nd power */
    d__2 = bet;
    cno = d__1 * d__1 + d__2 * d__2;
    u1 = 1. / u1;
    alph[1] = alp / cno;
    beta[1] = bet / cno;
    return 0;
L5:
    c__[1] = .05710419611451768219312;
    c__[2] = .27684301363812382768;
    c__[3] = .5835904323689168200567;
    c__[4] = .8602401356562194478479;
    dd[1] = -27.78093394406463730479;
    dd[2] = 3.641478498049213152712;
    dd[3] = -1.252547721169118720491;
    dd[4] = .5920031671845428725662;
    dd[5] = -.2;
    u1 = 6.286704751729276645173;
    alph[1] = 3.655694325463572258243;
    beta[1] = 6.543736899360077294021;
    alph[2] = 5.70095329867178941917;
    beta[2] = 3.210265600308549888425;
    return 0;
L7:
    c__[1] = .02931642715978489197205;
    c__[2] = .14807859966848429185;
    c__[3] = .3369846902811542990971;
    c__[4] = .5586715187715501320814;
    c__[5] = .7692338620300545009169;
    c__[6] = .9269456713197411148519;
    dd[1] = -54.37443689412861451458;
    dd[2] = 7.000024004259186512041;
    dd[3] = -2.355661091987557192256;
    dd[4] = 1.132289066106134386384;
    dd[5] = -.6468913267673587118673;
    dd[6] = .3875333853753523774248;
    dd[7] = -.1428571428571428571429;
    u1 = 8.936832788405216337302;
    alph[1] = 4.378693561506806002523;
    beta[1] = 10.16969328379501162732;
    alph[2] = 7.141055219187640105775;
    beta[2] = 6.623045922639275970621;
    alph[3] = 8.511834825102945723051;
    beta[3] = 3.281013624325058830036;
    return 0;
} /* coercv_ */


/*     END OF SUBROUTINE COERCV */

/* *********************************************************** */

double CIntegratorRAD::radau_contra(int32_t *i__, double *x, double *cont, int32_t *lrc)
{
    /* System generated locals */
    double ret_val;
    
    /* Local variables */
    static int32_t k;
    static double s;
    
    /* ---------------------------------------------------------- */
    /*     THIS FUNCTION CAN BE USED FOR CONINUOUS OUTPUT. IT PROVIDES AN */
    /*     APPROXIMATION TO THE I-TH COMPONENT OF THE SOLUTION AT X. */
    /*     IT GIVES THE VALUE OF THE COLLOCATION POLYNOMIAL, DEFINED FOR */
    /*     THE LAST SUCCESSFULLY COMPUTED STEP (BY RADAU). */
    /* ---------------------------------------------------------- */
    /* Parameter adjustments */
    --cont;
    
    /* Function Body */
    s = (*x - rad_weight.xsol) / rad_weight.hsol + 1.;
    ret_val = cont[*i__ + rad_weight.ns * rad_weight.nn];
    for (k = rad_weight.ns - 1; k >= 0; --k) {
        ret_val = cont[*i__ + k * rad_weight.nn] + (s - rad_weight.c__[
            rad_weight.ns - k]) * ret_val;
    }
    return ret_val;
} /* radau_contra */
int CIntegratorRAD::estrad(int32_t *n, double *fjac, int32_t *ldjac, 
	int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	int32_t *mlmas, int32_t *mumas, double *h__, double *dd1, 
	double *dd2, double *dd3
    , CDsModBase *PObj
    , int32_t *nfcn, double 
	*y0, double *y, int32_t *ijob, double *x, int32_t *m1, 
	int32_t *m2, int32_t *nm1, double *e1, int32_t *lde1, double *
	z1, double *z2, double *z3, double *cont, double *f1, 
	double *f2, int32_t *ip1, int32_t *iphes, double *scal, 
	double *err, uint8_t *first, uint8_t *reject, double *fac1
    ,int32_t *idid, CSlfStr &errtext)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e1_dim1, 
	    e1_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    double d__1;

    error_t reterr;
    /* Builtin functions */
    double sqrt(double);

    /* Local variables */
    static int32_t i__, j, k, mm, mp, im1;
    extern /* Subroutine */ int radau_sol(int32_t *, int32_t *, double *, 
	    double *, int32_t *);
    static double sum, hee1, hee2, hee3, sum1;
    extern /* Subroutine */ int radau_solb(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *, double *, int32_t *), radau_solh(int32_t *, 
	    int32_t *, double *, int32_t *, double *, int32_t *);
    static double zsafe;

    /* Parameter adjustments */
    --scal;
    --iphes;
    --f2;
    --f1;
    --cont;
    --z3;
    --z2;
    --z1;
    --y;
    --y0;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip1;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e1_dim1 = *lde1;
    e1_offset = 1 + e1_dim1;
    e1 -= e1_offset;

    /* Function Body */
    hee1 = *dd1 / *h__;
    hee2 = *dd2 / *h__;
    hee3 = *dd3 / *h__;
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L14;
	case 15:  goto L15;
    }

L1:
/* ------  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
    radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
    goto L77;

L11:
/* ------  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
L48:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    sum1 = (cont[j + k * *m2] + sum1) / *fac1;
	    i__2 = *nm1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		im1 = i__ + *m1;
		cont[im1] += fjac[i__ + (j + k * *m2) * fjac_dim1] * sum1;
	    }
	}
    }
    radau_sol(nm1, lde1, &e1[e1_offset], &cont[*m1 + 1], &ip1[1]);
    for (i__ = *m1; i__ >= 1; --i__) {
	cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
    }
    goto L77;

L2:
/* ------  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[1], &ip1[
	    1]);
    goto L77;

L12:
/* ------  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
L45:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    sum1 = (cont[j + k * *m2] + sum1) / *fac1;
/* Computing MAX */
	    i__2 = 1, i__3 = j - *mujac;
/* Computing MIN */
	    i__5 = *nm1, i__6 = j + *mljac;
	    i__4 = min(i__5,i__6);
	    for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		im1 = i__ + *m1;
		cont[im1] += fjac[i__ + *mujac + 1 - j + (j + k * *m2) * 
			fjac_dim1] * sum1;
	    }
	}
    }
    radau_solb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[*m1 + 
	    1], &ip1[1]);
    for (i__ = *m1; i__ >= 1; --i__) {
	cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
    }
    goto L77;

L3:
/* ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f1[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j];
	}
	f2[i__] = sum;
	cont[i__] = sum + y0[i__];
    }
    radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
    goto L77;

L13:
/* ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
    i__1 = *n;
    for (i__ = *m1 + 1; i__ <= i__1; ++i__) {
	f1[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__3 = 1, i__4 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *nm1, i__6 = i__ + *mumas;
	i__2 = min(i__5,i__6);
	for (j = max(i__3,i__4); j <= i__2; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j + *
		    m1];
	}
	im1 = i__ + *m1;
	f2[im1] = sum;
	cont[im1] = sum + y0[im1];
    }
    goto L48;

L4:
/* ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f1[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__2 = 1, i__3 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__4 = min(i__5,i__6);
	for (j = max(i__2,i__3); j <= i__4; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j];
	}
	f2[i__] = sum;
	cont[i__] = sum + y0[i__];
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[1], &ip1[
	    1]);
    goto L77;

L14:
/* ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
    i__1 = *n;
    for (i__ = *m1 + 1; i__ <= i__1; ++i__) {
	f1[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *nm1, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j + *
		    m1];
	}
	im1 = i__ + *m1;
	f2[im1] = sum;
	cont[im1] = sum + y0[im1];
    }
    goto L45;

L5:
/* ------  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f1[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *n;
	for (j = 1; j <= i__3; ++j) {
	    sum += fmas[i__ + j * fmas_dim1] * f1[j];
	}
	f2[i__] = sum;
	cont[i__] = sum + y0[i__];
    }
    radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
    goto L77;

L15:
/* ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
    i__1 = *n;
    for (i__ = *m1 + 1; i__ <= i__1; ++i__) {
	f1[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *nm1;
	for (j = 1; j <= i__3; ++j) {
	    sum += fmas[i__ + j * fmas_dim1] * f1[j + *m1];
	}
	im1 = i__ + *m1;
	f2[im1] = sum;
	cont[im1] = sum + y0[im1];
    }
    goto L48;

L6:
/* ------  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ------  THIS OPTION IS NOT PROVIDED */
    return 0;

L7:
/* ------  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	f2[i__] = hee1 * z1[i__] + hee2 * z2[i__] + hee3 * z3[i__];
	cont[i__] = f2[i__] + y0[i__];
    }
    for (mm = *n - 2; mm >= 1; --mm) {
	mp = *n - mm;
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L310;
	}
	zsafe = cont[mp];
	cont[mp] = cont[i__];
	cont[i__] = zsafe;
L310:
	i__1 = *n;
	for (i__ = mp + 1; i__ <= i__1; ++i__) {
	    cont[i__] -= fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	}
    }
    radau_solh(n, lde1, &e1[e1_offset], &c__1, &cont[1], &ip1[1]);
    i__1 = *n - 2;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *n - mm;
	i__3 = *n;
	for (i__ = mp + 1; i__ <= i__3; ++i__) {
	    cont[i__] += fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	}
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L440;
	}
	zsafe = cont[mp];
	cont[mp] = cont[i__];
	cont[i__] = zsafe;
L440:
	;
    }

/* -------------------------------------- */

L77:
    *err = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	d__1 = cont[i__] / scal[i__];
	*err += d__1 * d__1;
    }
/* Computing MAX */
    d__1 = sqrt(*err / *n);
    *err = max(d__1,1e-10);

    if (*err < 1.) {
	return 0;
    }
    if (*first || *reject) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    cont[i__] = y[i__] + cont[i__];
	}
    // State-Funktions-Aufruf
    //=======================
#if INTEGRATOR_USE_DSMODBASE == 1
    reterr = PObj->StateDer(*x, &cont[1], &f1[1]);
#else
    reterr = func(*n, *x, &cont[1], &f1[1]);
#endif
    if( reterr ) {
        *idid = -5;
#if INTEGRATOR_USE_DSMODBASE == 1
        errtext.catFormat("Error radau_core: during state-functioncall of <%s> \n%s\n"
                         , PObj->getName()
                         , PObj->getErrText()
                         );
#else
        errtext.cat("Error radau_core: error during state-functioncall\n");
                     
#endif
        return 0;
    }
	//(*fcn)(n, x, &cont[1], &f1[1], &rpar[1], &ipar[1]);
	++(*nfcn);
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    cont[i__] = f1[i__] + f2[i__];
	}
	switch (*ijob) {
	    case 1:  goto L31;
	    case 2:  goto L32;
	    case 3:  goto L31;
	    case 4:  goto L32;
	    case 5:  goto L31;
	    case 6:  goto L32;
	    case 7:  goto L33;
	    case 8:  goto L55;
	    case 9:  goto L55;
	    case 10:  goto L55;
	    case 11:  goto L41;
	    case 12:  goto L42;
	    case 13:  goto L41;
	    case 14:  goto L42;
	    case 15:  goto L41;
	}
/* ------ FULL MATRIX OPTION */
L31:
	radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
	goto L88;
/* ------ FULL MATRIX OPTION, SECOND ORDER */
L41:
	i__1 = *m2;
	for (j = 1; j <= i__1; ++j) {
	    sum1 = 0.;
	    for (k = mm - 1; k >= 0; --k) {
		sum1 = (cont[j + k * *m2] + sum1) / *fac1;
		i__3 = *nm1;
		for (i__ = 1; i__ <= i__3; ++i__) {
		    im1 = i__ + *m1;
		    cont[im1] += fjac[i__ + (j + k * *m2) * fjac_dim1] * sum1;
		}
	    }
	}
	radau_sol(nm1, lde1, &e1[e1_offset], &cont[*m1 + 1], &ip1[1]);
	for (i__ = *m1; i__ >= 1; --i__) {
	    cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
	}
	goto L88;
/* ------ BANDED MATRIX OPTION */
L32:
	radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[1], &
		ip1[1]);
	goto L88;
/* ------ BANDED MATRIX OPTION, SECOND ORDER */
L42:
	i__1 = *m2;
	for (j = 1; j <= i__1; ++j) {
	    sum1 = 0.;
	    for (k = mm - 1; k >= 0; --k) {
		sum1 = (cont[j + k * *m2] + sum1) / *fac1;
/* Computing MAX */
		i__3 = 1, i__4 = j - *mujac;
/* Computing MIN */
		i__5 = *nm1, i__6 = j + *mljac;
		i__2 = min(i__5,i__6);
		for (i__ = max(i__3,i__4); i__ <= i__2; ++i__) {
		    im1 = i__ + *m1;
		    cont[im1] += fjac[i__ + *mujac + 1 - j + (j + k * *m2) * 
			    fjac_dim1] * sum1;
		}
	    }
	}
	radau_solb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[*
		m1 + 1], &ip1[1]);
	for (i__ = *m1; i__ >= 1; --i__) {
	    cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
	}
	goto L88;
/* ------ HESSENBERG MATRIX OPTION */
L33:
	for (mm = *n - 2; mm >= 1; --mm) {
	    mp = *n - mm;
	    i__ = iphes[mp];
	    if (i__ == mp) {
		goto L510;
	    }
	    zsafe = cont[mp];
	    cont[mp] = cont[i__];
	    cont[i__] = zsafe;
L510:
	    i__1 = *n;
	    for (i__ = mp + 1; i__ <= i__1; ++i__) {
		cont[i__] -= fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	    }
	}
	radau_solh(n, lde1, &e1[e1_offset], &c__1, &cont[1], &ip1[1]);
	i__1 = *n - 2;
	for (mm = 1; mm <= i__1; ++mm) {
	    mp = *n - mm;
	    i__2 = *n;
	    for (i__ = mp + 1; i__ <= i__2; ++i__) {
		cont[i__] += fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	    }
	    i__ = iphes[mp];
	    if (i__ == mp) {
		goto L640;
	    }
	    zsafe = cont[mp];
	    cont[mp] = cont[i__];
	    cont[i__] = zsafe;
L640:
	    ;
	}
/* ----------------------------------- */
L88:
	*err = 0.;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	    d__1 = cont[i__] / scal[i__];
	    *err += d__1 * d__1;
	}
/* Computing MAX */
	d__1 = sqrt(*err / *n);
	*err = max(d__1,1e-10);
    }
    return 0;
/* ----------------------------------------------------------- */
L55:
    return 0;
} /* estrad_ */


/*     END OF SUBROUTINE ESTRAD */

/* *********************************************************** */

int CIntegratorRAD::estrav(int32_t *n, double *fjac, int32_t *ldjac, 
	int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	int32_t *mlmas, int32_t *mumas, double *h__, double *dd
#if INTEGRATOR_USE_DSMODBASE == 1
    , CDsModBase *PObj
#else
    , FcnEqDiff func
#endif
    , int32_t *nfcn, double *y0, double *y, int32_t *ijob, 
	double *x, int32_t *m1, int32_t *m2, int32_t *nm1, int32_t *ns, 
	int32_t *nns, double *e1, int32_t *lde1, double *zz, 
	double *cont, double *ff, int32_t *ip1, int32_t *iphes, 
	double *scal, double *err, uint8_t *first, uint8_t *reject, 
	double *fac1, int32_t *idid, CSlfStr &errtext)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e1_dim1, 
	    e1_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    double d__1;

    error_t reterr;

    /* Builtin functions */
    double sqrt(double);

    /* Local variables */
    static int32_t i__, j, k, mm, mp, im1;
    extern /* Subroutine */ int radau_sol(int32_t *, int32_t *, double *, 
	    double *, int32_t *);
    static double sum, sum1;
    extern /* Subroutine */ int radau_solb(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *, double *, int32_t *), radau_solh(int32_t *, 
	    int32_t *, double *, int32_t *, double *, int32_t *);
    static double zsafe;

    /* Parameter adjustments */
    --scal;
    --iphes;
    --cont;
    --y;
    --y0;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip1;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    --dd;
    --ff;
    --zz;
    e1_dim1 = *lde1;
    e1_offset = 1 + e1_dim1;
    e1 -= e1_offset;

    /* Function Body */
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L14;
	case 15:  goto L15;
    }

L1:
/* ------  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = *ns;
	for (k = 1; k <= i__2; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
    radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
    goto L77;

L11:
/* ------  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = *ns;
	for (k = 1; k <= i__2; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
L48:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    sum1 = (cont[j + k * *m2] + sum1) / *fac1;
	    i__2 = *nm1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		im1 = i__ + *m1;
		cont[im1] += fjac[i__ + (j + k * *m2) * fjac_dim1] * sum1;
	    }
	}
    }
    radau_sol(nm1, lde1, &e1[e1_offset], &cont[*m1 + 1], &ip1[1]);
    for (i__ = *m1; i__ >= 1; --i__) {
	cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
    }
    goto L77;

L2:
/* ------  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = *ns;
	for (k = 1; k <= i__2; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[1], &ip1[
	    1]);
    goto L77;

L12:
/* ------  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = *ns;
	for (k = 1; k <= i__2; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
L45:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    sum1 = (cont[j + k * *m2] + sum1) / *fac1;
/* Computing MAX */
	    i__2 = 1, i__3 = j - *mujac;
/* Computing MIN */
	    i__5 = *nm1, i__6 = j + *mljac;
	    i__4 = min(i__5,i__6);
	    for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		im1 = i__ + *m1;
		cont[im1] += fjac[i__ + *mujac + 1 - j + (j + k * *m2) * 
			fjac_dim1] * sum1;
	    }
	}
    }
    radau_solb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[*m1 + 
	    1], &ip1[1]);
    for (i__ = *m1; i__ >= 1; --i__) {
	cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
    }
    goto L77;

L3:
/* ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__4 = *ns;
	for (k = 1; k <= i__4; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__] = sum / *h__;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ff[j];
	}
	ff[i__ + *n] = sum;
	cont[i__] = sum + y0[i__];
    }
    radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
    goto L77;

L13:
/* ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *ns;
	for (k = 1; k <= i__3; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
    i__1 = *n;
    for (i__ = *m1 + 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *ns;
	for (k = 1; k <= i__3; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__] = sum / *h__;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__3 = 1, i__4 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *nm1, i__6 = i__ + *mumas;
	i__2 = min(i__5,i__6);
	for (j = max(i__3,i__4); j <= i__2; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ff[j + *
		    m1];
	}
	im1 = i__ + *m1;
	ff[im1 + *n] = sum;
	cont[im1] = sum + y0[im1];
    }
    goto L48;

L4:
/* ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = *ns;
	for (k = 1; k <= i__2; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__] = sum / *h__;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__2 = 1, i__3 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__4 = min(i__5,i__6);
	for (j = max(i__2,i__3); j <= i__4; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ff[j];
	}
	ff[i__ + *n] = sum;
	cont[i__] = sum + y0[i__];
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[1], &ip1[
	    1]);
    goto L77;

L14:
/* ------  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__4 = *ns;
	for (k = 1; k <= i__4; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
    i__1 = *n;
    for (i__ = *m1 + 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__4 = *ns;
	for (k = 1; k <= i__4; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__] = sum / *h__;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *nm1, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ff[j + *
		    m1];
	}
	im1 = i__ + *m1;
	ff[im1 + *n] = sum;
	cont[im1] = sum + y0[im1];
    }
    goto L45;

L5:
/* ------  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *ns;
	for (k = 1; k <= i__3; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__] = sum / *h__;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *n;
	for (j = 1; j <= i__3; ++j) {
	    sum += fmas[i__ + j * fmas_dim1] * ff[j];
	}
	ff[i__ + *n] = sum;
	cont[i__] = sum + y0[i__];
    }
    radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
    goto L77;

L15:
/* ------  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *ns;
	for (k = 1; k <= i__3; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
    i__1 = *n;
    for (i__ = *m1 + 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *ns;
	for (k = 1; k <= i__3; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__] = sum / *h__;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *nm1;
	for (j = 1; j <= i__3; ++j) {
	    sum += fmas[i__ + j * fmas_dim1] * ff[j + *m1];
	}
	im1 = i__ + *m1;
	ff[im1 + *n] = sum;
	cont[im1] = sum + y0[im1];
    }
    goto L48;

L6:
/* ------  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ------  THIS OPTION IS NOT PROVIDED */
    return 0;

L7:
/* ------  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__3 = *ns;
	for (k = 1; k <= i__3; ++k) {
	    sum += dd[k] * zz[i__ + (k - 1) * *n];
	}
	ff[i__ + *n] = sum / *h__;
	cont[i__] = ff[i__ + *n] + y0[i__];
    }
    for (mm = *n - 2; mm >= 1; --mm) {
	mp = *n - mm;
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L310;
	}
	zsafe = cont[mp];
	cont[mp] = cont[i__];
	cont[i__] = zsafe;
L310:
	i__1 = *n;
	for (i__ = mp + 1; i__ <= i__1; ++i__) {
	    cont[i__] -= fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	}
    }
    radau_solh(n, lde1, &e1[e1_offset], &c__1, &cont[1], &ip1[1]);
    i__1 = *n - 2;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *n - mm;
	i__3 = *n;
	for (i__ = mp + 1; i__ <= i__3; ++i__) {
	    cont[i__] += fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	}
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L440;
	}
	zsafe = cont[mp];
	cont[mp] = cont[i__];
	cont[i__] = zsafe;
L440:
	;
    }

/* -------------------------------------- */

L77:
    *err = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	d__1 = cont[i__] / scal[i__];
	*err += d__1 * d__1;
    }
/* Computing MAX */
    d__1 = sqrt(*err / *n);
    *err = max(d__1,1e-10);

    if (*err < 1.) {
	return 0;
    }
    if (*first || *reject) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    cont[i__] = y[i__] + cont[i__];
	}
    // State-Funktions-Aufruf
    //=======================
#if INTEGRATOR_USE_DSMODBASE == 1
    reterr = PObj->StateDer(*x, &cont[1], &ff[1]);
#else
    reterr = func(*n, *x, &cont[1], &ff[1]);
#endif
    if( reterr ) {
        *idid = -5;
#if INTEGRATOR_USE_DSMODBASE == 1
        errtext.catFormat("Error radau_core: during state-functioncall of <%s> \n%s\n"
                         , PObj->getName()
                         , PObj->getErrText()
                         );
#else
        errtext.cat("Error radau_core: error during state-functioncall\n");
                     
#endif
        return 0;
    }
	//(*fcn)(n, x, &cont[1], &ff[1], &rpar[1], &ipar[1]);
	++(*nfcn);
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    cont[i__] = ff[i__] + ff[i__ + *n];
	}
	switch (*ijob) {
	    case 1:  goto L31;
	    case 2:  goto L32;
	    case 3:  goto L31;
	    case 4:  goto L32;
	    case 5:  goto L31;
	    case 6:  goto L32;
	    case 7:  goto L33;
	    case 8:  goto L55;
	    case 9:  goto L55;
	    case 10:  goto L55;
	    case 11:  goto L41;
	    case 12:  goto L42;
	    case 13:  goto L41;
	    case 14:  goto L42;
	    case 15:  goto L41;
	}
/* ------ FULL MATRIX OPTION */
L31:
	radau_sol(n, lde1, &e1[e1_offset], &cont[1], &ip1[1]);
	goto L88;
/* ------ FULL MATRIX OPTION, SECOND ORDER */
L41:
	i__1 = *m2;
	for (j = 1; j <= i__1; ++j) {
	    sum1 = 0.;
	    for (k = mm - 1; k >= 0; --k) {
		sum1 = (cont[j + k * *m2] + sum1) / *fac1;
		i__3 = *nm1;
		for (i__ = 1; i__ <= i__3; ++i__) {
		    im1 = i__ + *m1;
		    cont[im1] += fjac[i__ + (j + k * *m2) * fjac_dim1] * sum1;
		}
	    }
	}
	radau_sol(nm1, lde1, &e1[e1_offset], &cont[*m1 + 1], &ip1[1]);
	for (i__ = *m1; i__ >= 1; --i__) {
	    cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
	}
	goto L88;
/* ------ BANDED MATRIX OPTION */
L32:
	radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[1], &
		ip1[1]);
	goto L88;
/* ------ BANDED MATRIX OPTION, SECOND ORDER */
L42:
	i__1 = *m2;
	for (j = 1; j <= i__1; ++j) {
	    sum1 = 0.;
	    for (k = mm - 1; k >= 0; --k) {
		sum1 = (cont[j + k * *m2] + sum1) / *fac1;
/* Computing MAX */
		i__3 = 1, i__4 = j - *mujac;
/* Computing MIN */
		i__5 = *nm1, i__6 = j + *mljac;
		i__2 = min(i__5,i__6);
		for (i__ = max(i__3,i__4); i__ <= i__2; ++i__) {
		    im1 = i__ + *m1;
		    cont[im1] += fjac[i__ + *mujac + 1 - j + (j + k * *m2) * 
			    fjac_dim1] * sum1;
		}
	    }
	}
	radau_solb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &cont[*
		m1 + 1], &ip1[1]);
	for (i__ = *m1; i__ >= 1; --i__) {
	    cont[i__] = (cont[i__] + cont[*m2 + i__]) / *fac1;
	}
	goto L88;
/* ------ HESSENBERG MATRIX OPTION */
L33:
	for (mm = *n - 2; mm >= 1; --mm) {
	    mp = *n - mm;
	    i__ = iphes[mp];
	    if (i__ == mp) {
		goto L510;
	    }
	    zsafe = cont[mp];
	    cont[mp] = cont[i__];
	    cont[i__] = zsafe;
L510:
	    i__1 = *n;
	    for (i__ = mp + 1; i__ <= i__1; ++i__) {
		cont[i__] -= fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	    }
	}
	radau_solh(n, lde1, &e1[e1_offset], &c__1, &cont[1], &ip1[1]);
	i__1 = *n - 2;
	for (mm = 1; mm <= i__1; ++mm) {
	    mp = *n - mm;
	    i__2 = *n;
	    for (i__ = mp + 1; i__ <= i__2; ++i__) {
		cont[i__] += fjac[i__ + (mp - 1) * fjac_dim1] * cont[mp];
	    }
	    i__ = iphes[mp];
	    if (i__ == mp) {
		goto L640;
	    }
	    zsafe = cont[mp];
	    cont[mp] = cont[i__];
	    cont[i__] = zsafe;
L640:
	    ;
	}
/* ----------------------------------- */
L88:
	*err = 0.;
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
	    d__1 = cont[i__] / scal[i__];
	    *err += d__1 * d__1;
	}
/* Computing MAX */
	d__1 = sqrt(*err / *n);
	*err = max(d__1,1e-10);
    }
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* estrav_ */


/*     END OF SUBROUTINE ESTRAV */
/* ****************************************** */
/*     VERSION OF SEPTEMBER 18, 1995 */
/* ****************************************** */

    int CIntegratorRAD::decomr(int32_t *n, double *fjac, int32_t *ldjac, 
	double *fmas, int32_t *ldmas, int32_t *mlmas, int32_t *mumas, 
	int32_t *m1, int32_t *m2, int32_t *nm1, double *fac1, double *
	e1, int32_t *lde1, int32_t *ip1, int32_t *ier, int32_t *ijob, uint8_t 
	*calhes, int32_t *iphes)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e1_dim1, 
	    e1_offset, i__1, i__2, i__3, i__4, i__5, i__6;

    /* Local variables */
    static int32_t i__, j, k, j1, ib, mm, jm1;
    extern /* Subroutine */ int radau_dec(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *);
    static double sum;
    extern /* Subroutine */ int radau_decb(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *, int32_t *, int32_t *), radau_dech(int32_t *, 
	    int32_t *, double *, int32_t *, int32_t *, int32_t *), 
	    radau_elmhes(int32_t *, int32_t *, int32_t *, int32_t *, double *, 
	    int32_t *);


    /* Parameter adjustments */
    --iphes;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip1;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e1_dim1 = *lde1;
    e1_offset = 1 + e1_dim1;
    e1 -= e1_offset;

    /* Function Body */
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L14;
	case 15:  goto L15;
    }

/* ----------------------------------------------------------- */

L1:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e1[i__ + j * e1_dim1] = -fjac[i__ + j * fjac_dim1];
	}
	e1[j + j * e1_dim1] += *fac1;
    }
    radau_dec(n, lde1, &e1[e1_offset], &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L11:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__2 = *nm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e1[i__ + j * e1_dim1] = -fjac[i__ + jm1 * fjac_dim1];
	}
	e1[j + j * e1_dim1] += *fac1;
    }
L45:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *nm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sum = 0.;
	    i__3 = mm - 1;
	    for (k = 0; k <= i__3; ++k) {
		sum = (sum + fjac[i__ + (j + k * *m2) * fjac_dim1]) / *fac1;
	    }
	    e1[i__ + j * e1_dim1] -= sum;
	}
    }
    radau_dec(nm1, lde1, &e1[e1_offset], &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L2:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e1[i__ + rad_linal.mle + j * e1_dim1] = -fjac[i__ + j * fjac_dim1];
	}
	e1[rad_linal.mdiag + j * e1_dim1] += *fac1;
    }
    radau_decb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L12:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e1[i__ + rad_linal.mle + j * e1_dim1] = -fjac[i__ + jm1 * fjac_dim1]
		    ;
	}
	e1[rad_linal.mdiag + j * e1_dim1] += *fac1;
    }
L46:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sum = 0.;
	    i__3 = mm - 1;
	    for (k = 0; k <= i__3; ++k) {
		sum = (sum + fjac[i__ + (j + k * *m2) * fjac_dim1]) / *fac1;
	    }
	    e1[i__ + rad_linal.mle + j * e1_dim1] -= sum;
	}
    }
    radau_decb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &ip1[1], ier)
	    ;
    return 0;

/* ----------------------------------------------------------- */

L3:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e1[i__ + j * e1_dim1] = -fjac[i__ + j * fjac_dim1];
	}
/* Computing MAX */
	i__2 = 1, i__3 = j - *mumas;
/* Computing MIN */
	i__5 = *n, i__6 = j + *mlmas;
	i__4 = min(i__5,i__6);
	for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
	    e1[i__ + j * e1_dim1] += *fac1 * fmas[i__ - j + rad_linal.mbdiag + 
		    j * fmas_dim1];
	}
    }
    radau_dec(n, lde1, &e1[e1_offset], &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L13:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__4 = *nm1;
	for (i__ = 1; i__ <= i__4; ++i__) {
	    e1[i__ + j * e1_dim1] = -fjac[i__ + jm1 * fjac_dim1];
	}
/* Computing MAX */
	i__4 = 1, i__2 = j - *mumas;
/* Computing MIN */
	i__5 = *nm1, i__6 = j + *mlmas;
	i__3 = min(i__5,i__6);
	for (i__ = max(i__4,i__2); i__ <= i__3; ++i__) {
	    e1[i__ + j * e1_dim1] += *fac1 * fmas[i__ - j + rad_linal.mbdiag + 
		    j * fmas_dim1];
	}
    }
    goto L45;

/* ----------------------------------------------------------- */

L4:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__3 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    e1[i__ + rad_linal.mle + j * e1_dim1] = -fjac[i__ + j * fjac_dim1];
	}
	i__3 = rad_linal.mbb;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    ib = i__ + rad_linal.mdiff;
	    e1[ib + j * e1_dim1] += *fac1 * fmas[i__ + j * fmas_dim1];
	}
    }
    radau_decb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L14:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__3 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    e1[i__ + rad_linal.mle + j * e1_dim1] = -fjac[i__ + jm1 * fjac_dim1]
		    ;
	}
	i__3 = rad_linal.mbb;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    ib = i__ + rad_linal.mdiff;
	    e1[ib + j * e1_dim1] += *fac1 * fmas[i__ + j * fmas_dim1];
	}
    }
    goto L46;

/* ----------------------------------------------------------- */

L5:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__3 = *n;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    e1[i__ + j * e1_dim1] = fmas[i__ + j * fmas_dim1] * *fac1 - fjac[
		    i__ + j * fjac_dim1];
	}
    }
    radau_dec(n, lde1, &e1[e1_offset], &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L15:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__3 = *nm1;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    e1[i__ + j * e1_dim1] = fmas[i__ + j * fmas_dim1] * *fac1 - fjac[
		    i__ + jm1 * fjac_dim1];
	}
    }
    goto L45;

/* ----------------------------------------------------------- */

L6:
/* ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ---  THIS OPTION IS NOT PROVIDED */
    return 0;

/* ----------------------------------------------------------- */

L7:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    if (*calhes) {
	radau_elmhes(ldjac, n, &c__1, n, &fjac[fjac_offset], &iphes[1]);
    }
    *calhes = false;
    i__1 = *n - 1;
    for (j = 1; j <= i__1; ++j) {
	j1 = j + 1;
	e1[j1 + j * e1_dim1] = -fjac[j1 + j * fjac_dim1];
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__3 = j;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    e1[i__ + j * e1_dim1] = -fjac[i__ + j * fjac_dim1];
	}
	e1[j + j * e1_dim1] += *fac1;
    }
    radau_dech(n, lde1, &e1[e1_offset], &c__1, &ip1[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* decomr_ */


/*     END OF SUBROUTINE DECOMR */

/* *********************************************************** */

int CIntegratorRAD::decomc(int32_t *n, double *fjac, int32_t *ldjac, 
	double *fmas, int32_t *ldmas, int32_t *mlmas, int32_t *mumas, 
	int32_t *m1, int32_t *m2, int32_t *nm1, double *alphn, double 
	*betan, double *e2r, double *e2i, int32_t *lde1, int32_t *ip2,
	 int32_t *ier, int32_t *ijob)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e2r_dim1, 
	    e2r_offset, e2i_dim1, e2i_offset, i__1, i__2, i__3, i__4, i__5, 
	    i__6;
    double d__1, d__2;

    /* Local variables */
    static int32_t i__, j, k, j1;
    static double bb;
    static int32_t ib, mm, jm1;
    static double bet, alp;
    extern /* Subroutine */ int radau_decc(int32_t *, int32_t *, double *, 
	    double *, int32_t *, int32_t *);
    static double ffma, abno;
    static int32_t imle;
    static double sumi, sumr, sums;
    extern /* Subroutine */ int radau_decbc(int32_t *, int32_t *, double *, 
	    double *, int32_t *, int32_t *, int32_t *, int32_t *), radau_dechc(
	    int32_t *, int32_t *, double *, double *, int32_t *, 
	    int32_t *, int32_t *);


    /* Parameter adjustments */
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip2;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e2i_dim1 = *lde1;
    e2i_offset = 1 + e2i_dim1;
    e2i -= e2i_offset;
    e2r_dim1 = *lde1;
    e2r_offset = 1 + e2r_dim1;
    e2r -= e2r_offset;

    /* Function Body */
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L14;
	case 15:  goto L15;
    }

/* ----------------------------------------------------------- */

L1:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2r[i__ + j * e2r_dim1] = -fjac[i__ + j * fjac_dim1];
	    e2i[i__ + j * e2i_dim1] = 0.;
	}
	e2r[j + j * e2r_dim1] += *alphn;
	e2i[j + j * e2i_dim1] = *betan;
    }
    radau_decc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L11:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__2 = *nm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2r[i__ + j * e2r_dim1] = -fjac[i__ + jm1 * fjac_dim1];
	    e2i[i__ + j * e2i_dim1] = 0.;
	}
	e2r[j + j * e2r_dim1] += *alphn;
	e2i[j + j * e2i_dim1] = *betan;
    }
L45:
    mm = *m1 / *m2;
/* Computing 2nd power */
    d__1 = *alphn;
/* Computing 2nd power */
    d__2 = *betan;
    abno = d__1 * d__1 + d__2 * d__2;
    alp = *alphn / abno;
    bet = *betan / abno;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *nm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sumr = 0.;
	    sumi = 0.;
	    i__3 = mm - 1;
	    for (k = 0; k <= i__3; ++k) {
		sums = sumr + fjac[i__ + (j + k * *m2) * fjac_dim1];
		sumr = sums * alp + sumi * bet;
		sumi = sumi * alp - sums * bet;
	    }
	    e2r[i__ + j * e2r_dim1] -= sumr;
	    e2i[i__ + j * e2i_dim1] -= sumi;
	}
    }
    radau_decc(nm1, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L2:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    imle = i__ + rad_linal.mle;
	    e2r[imle + j * e2r_dim1] = -fjac[i__ + j * fjac_dim1];
	    e2i[imle + j * e2i_dim1] = 0.;
	}
	e2r[rad_linal.mdiag + j * e2r_dim1] += *alphn;
	e2i[rad_linal.mdiag + j * e2i_dim1] = *betan;
    }
    radau_decbc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L12:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2r[i__ + rad_linal.mle + j * e2r_dim1] = -fjac[i__ + jm1 * 
		    fjac_dim1];
	    e2i[i__ + rad_linal.mle + j * e2i_dim1] = 0.;
	}
	e2r[rad_linal.mdiag + j * e2r_dim1] += *alphn;
	e2i[rad_linal.mdiag + j * e2i_dim1] += *betan;
    }
L46:
    mm = *m1 / *m2;
/* Computing 2nd power */
    d__1 = *alphn;
/* Computing 2nd power */
    d__2 = *betan;
    abno = d__1 * d__1 + d__2 * d__2;
    alp = *alphn / abno;
    bet = *betan / abno;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    sumr = 0.;
	    sumi = 0.;
	    i__3 = mm - 1;
	    for (k = 0; k <= i__3; ++k) {
		sums = sumr + fjac[i__ + (j + k * *m2) * fjac_dim1];
		sumr = sums * alp + sumi * bet;
		sumi = sumi * alp - sums * bet;
	    }
	    imle = i__ + rad_linal.mle;
	    e2r[imle + j * e2r_dim1] -= sumr;
	    e2i[imle + j * e2i_dim1] -= sumi;
	}
    }
    radau_decbc(nm1, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L3:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2r[i__ + j * e2r_dim1] = -fjac[i__ + j * fjac_dim1];
	    e2i[i__ + j * e2i_dim1] = 0.;
	}
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	i__2 = 1, i__3 = j - *mumas;
/* Computing MIN */
	i__5 = *n, i__6 = j + *mlmas;
	i__4 = min(i__5,i__6);
	for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    e2r[i__ + j * e2r_dim1] += *alphn * bb;
	    e2i[i__ + j * e2i_dim1] = *betan * bb;
	}
    }
    radau_decc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L13:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__4 = *nm1;
	for (i__ = 1; i__ <= i__4; ++i__) {
	    e2r[i__ + j * e2r_dim1] = -fjac[i__ + jm1 * fjac_dim1];
	    e2i[i__ + j * e2i_dim1] = 0.;
	}
/* Computing MAX */
	i__4 = 1, i__2 = j - *mumas;
/* Computing MIN */
	i__5 = *nm1, i__6 = j + *mlmas;
	i__3 = min(i__5,i__6);
	for (i__ = max(i__4,i__2); i__ <= i__3; ++i__) {
	    ffma = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    e2r[i__ + j * e2r_dim1] += *alphn * ffma;
	    e2i[i__ + j * e2i_dim1] += *betan * ffma;
	}
    }
    goto L45;

/* ----------------------------------------------------------- */

L4:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__3 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    imle = i__ + rad_linal.mle;
	    e2r[imle + j * e2r_dim1] = -fjac[i__ + j * fjac_dim1];
	    e2i[imle + j * e2i_dim1] = 0.;
	}
/* Computing MAX */
	i__3 = 1, i__4 = *mumas + 2 - j;
/* Computing MIN */
	i__5 = rad_linal.mbb, i__6 = *mumas + 1 - j + *n;
	i__2 = min(i__5,i__6);
	for (i__ = max(i__3,i__4); i__ <= i__2; ++i__) {
	    ib = i__ + rad_linal.mdiff;
	    bb = fmas[i__ + j * fmas_dim1];
	    e2r[ib + j * e2r_dim1] += *alphn * bb;
	    e2i[ib + j * e2i_dim1] = *betan * bb;
	}
    }
    radau_decbc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L14:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__2 = rad_linal.mbjac;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2r[i__ + rad_linal.mle + j * e2r_dim1] = -fjac[i__ + jm1 * 
		    fjac_dim1];
	    e2i[i__ + rad_linal.mle + j * e2i_dim1] = 0.;
	}
	i__2 = rad_linal.mbb;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    ib = i__ + rad_linal.mdiff;
	    ffma = fmas[i__ + j * fmas_dim1];
	    e2r[ib + j * e2r_dim1] += *alphn * ffma;
	    e2i[ib + j * e2i_dim1] += *betan * ffma;
	}
    }
    goto L46;

/* ----------------------------------------------------------- */

L5:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    bb = fmas[i__ + j * fmas_dim1];
	    e2r[i__ + j * e2r_dim1] = bb * *alphn - fjac[i__ + j * fjac_dim1];
	    e2i[i__ + j * e2i_dim1] = bb * *betan;
	}
    }
    radau_decc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L15:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *nm1;
    for (j = 1; j <= i__1; ++j) {
	jm1 = j + *m1;
	i__2 = *nm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2r[i__ + j * e2r_dim1] = *alphn * fmas[i__ + j * fmas_dim1] - 
		    fjac[i__ + jm1 * fjac_dim1];
	    e2i[i__ + j * e2i_dim1] = *betan * fmas[i__ + j * fmas_dim1];
	}
    }
    goto L45;

/* ----------------------------------------------------------- */

L6:
/* ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ---  THIS OPTION IS NOT PROVIDED */
    return 0;

/* ----------------------------------------------------------- */

L7:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    i__1 = *n - 1;
    for (j = 1; j <= i__1; ++j) {
	j1 = j + 1;
	e2r[j1 + j * e2r_dim1] = -fjac[j1 + j * fjac_dim1];
	e2i[j1 + j * e2i_dim1] = 0.;
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    e2i[i__ + j * e2i_dim1] = 0.;
	    e2r[i__ + j * e2r_dim1] = -fjac[i__ + j * fjac_dim1];
	}
	e2r[j + j * e2r_dim1] += *alphn;
	e2i[j + j * e2i_dim1] = *betan;
    }
    radau_dechc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &c__1, &ip2[1], ier);
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* decomc */


/*     END OF SUBROUTINE DECOMC */

/* *********************************************************** */

int CIntegratorRAD::slvrar(int32_t *n, double *fjac, int32_t *ldjac, 
	int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	nm1, double *fac1, double *e1, int32_t *lde1, double *z1, 
	double *f1, int32_t *ip1, int32_t *iphes, int32_t *ier, int32_t *
	ijob)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e1_dim1, 
	    e1_offset, i__1, i__2, i__3, i__4, i__5, i__6;

    /* Local variables */
    static int32_t i__, j, k;
    static double s1;
    static int32_t mm, mp, im1, mp1, jkm;
    extern /* Subroutine */ int radau_sol(int32_t *, int32_t *, double *, 
	    double *, int32_t *);
    static double sum1;
    extern /* Subroutine */ int radau_solb(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *, double *, int32_t *), radau_solh(int32_t *, 
	    int32_t *, double *, int32_t *, double *, int32_t *);
    static double zsafe;


    /* Parameter adjustments */
    --iphes;
    --f1;
    --z1;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip1;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e1_dim1 = *lde1;
    e1_offset = 1 + e1_dim1;
    e1 -= e1_offset;

    /* Function Body */
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L13;
	case 15:  goto L15;
    }

/* ----------------------------------------------------------- */

L1:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
    radau_sol(n, lde1, &e1[e1_offset], &z1[1], &ip1[1]);
    return 0;

/* ----------------------------------------------------------- */

L11:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
L48:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sum1 = (z1[jkm] + sum1) / *fac1;
	    i__2 = *nm1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		im1 = i__ + *m1;
		z1[im1] += fjac[i__ + jkm * fjac_dim1] * sum1;
	    }
	}
    }
    radau_sol(nm1, lde1, &e1[e1_offset], &z1[*m1 + 1], &ip1[1]);
L49:
    for (i__ = *m1; i__ >= 1; --i__) {
	z1[i__] = (z1[i__] + z1[*m2 + i__]) / *fac1;
    }
    return 0;

/* ----------------------------------------------------------- */

L2:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &z1[1], &ip1[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L12:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
L45:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sum1 = (z1[jkm] + sum1) / *fac1;
/* Computing MAX */
	    i__2 = 1, i__3 = j - *mujac;
/* Computing MIN */
	    i__5 = *nm1, i__6 = j + *mljac;
	    i__4 = min(i__5,i__6);
	    for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		im1 = i__ + *m1;
		z1[im1] += fjac[i__ + *mujac + 1 - j + jkm * fjac_dim1] * 
			sum1;
	    }
	}
    }
    radau_solb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &z1[*m1 + 1],
	     &ip1[1]);
    goto L49;

/* ----------------------------------------------------------- */

L3:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s1 = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    s1 -= fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j];
	}
	z1[i__] += s1 * *fac1;
    }
    radau_sol(n, lde1, &e1[e1_offset], &z1[1], &ip1[1]);
    return 0;

/* ----------------------------------------------------------- */

L13:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ + *m1;
	s1 = 0.;
/* Computing MAX */
	i__3 = 1, i__4 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *nm1, i__6 = i__ + *mumas;
	i__2 = min(i__5,i__6);
	for (j = max(i__3,i__4); j <= i__2; ++j) {
	    s1 -= fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j + *m1]
		    ;
	}
	z1[im1] += s1 * *fac1;
    }
    if (*ijob == 14) {
	goto L45;
    }
    goto L48;

/* ----------------------------------------------------------- */

L4:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s1 = 0.;
/* Computing MAX */
	i__2 = 1, i__3 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__4 = min(i__5,i__6);
	for (j = max(i__2,i__3); j <= i__4; ++j) {
	    s1 -= fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * f1[j];
	}
	z1[i__] += s1 * *fac1;
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &z1[1], &ip1[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L5:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s1 = 0.;
	i__4 = *n;
	for (j = 1; j <= i__4; ++j) {
	    s1 -= fmas[i__ + j * fmas_dim1] * f1[j];
	}
	z1[i__] += s1 * *fac1;
    }
    radau_sol(n, lde1, &e1[e1_offset], &z1[1], &ip1[1]);
    return 0;

/* ----------------------------------------------------------- */

L15:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ + *m1;
	s1 = 0.;
	i__4 = *nm1;
	for (j = 1; j <= i__4; ++j) {
	    s1 -= fmas[i__ + j * fmas_dim1] * f1[j + *m1];
	}
	z1[im1] += s1 * *fac1;
    }
    goto L48;

/* ----------------------------------------------------------- */

L6:
/* ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ---  THIS OPTION IS NOT PROVIDED */
    return 0;

/* ----------------------------------------------------------- */

L7:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	z1[i__] -= f1[i__] * *fac1;
    }
    for (mm = *n - 2; mm >= 1; --mm) {
	mp = *n - mm;
	mp1 = mp - 1;
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L746;
	}
	zsafe = z1[mp];
	z1[mp] = z1[i__];
	z1[i__] = zsafe;
L746:
	i__1 = *n;
	for (i__ = mp + 1; i__ <= i__1; ++i__) {
	    z1[i__] -= fjac[i__ + mp1 * fjac_dim1] * z1[mp];
	}
    }
    radau_solh(n, lde1, &e1[e1_offset], &c__1, &z1[1], &ip1[1]);
    i__1 = *n - 2;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *n - mm;
	mp1 = mp - 1;
	i__4 = *n;
	for (i__ = mp + 1; i__ <= i__4; ++i__) {
	    z1[i__] += fjac[i__ + mp1 * fjac_dim1] * z1[mp];
	}
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L750;
	}
	zsafe = z1[mp];
	z1[mp] = z1[i__];
	z1[i__] = zsafe;
L750:
	;
    }
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* slvrar_ */


/*     END OF SUBROUTINE SLVRAR */

/* *********************************************************** */

int CIntegratorRAD::slvrai(int32_t *n, double *fjac, int32_t *ldjac, 
	int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	nm1, double *alphn, double *betan, double *e2r, 
	double *e2i, int32_t *lde1, double *z2, double *z3, 
	double *f2, double *f3, double *cont, int32_t *ip2, 
	int32_t *iphes, int32_t *ier, int32_t *ijob)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e2r_dim1, 
	    e2r_offset, e2i_dim1, e2i_offset, i__1, i__2, i__3, i__4, i__5, 
	    i__6;
    double d__1, d__2;

    /* Local variables */
    static int32_t i__, j, k;
    static double s2, s3, bb;
    static int32_t mm, mp, im1, jm1, mp1;
    static double z2i, z3i;
    static int32_t jkm, mpi;
    static double sum2, sum3, abno;
    extern /* Subroutine */ int radau_solc(int32_t *, int32_t *, double *, 
	    double *, double *, double *, int32_t *);
    static int32_t iimu;
    static double sumh, e1imp;
    extern /* Subroutine */ int radau_solbc(int32_t *, int32_t *, double *, 
	    double *, int32_t *, int32_t *, double *, double *, 
	    int32_t *);
    static double zsafe;
    extern /* Subroutine */ int radau_solhc(int32_t *, int32_t *, double *, 
	    double *, int32_t *, double *, double *, int32_t *);


    /* Parameter adjustments */
    --iphes;
    --f3;
    --f2;
    --z3;
    --z2;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip2;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e2i_dim1 = *lde1;
    e2i_offset = 1 + e2i_dim1;
    e2i -= e2i_offset;
    e2r_dim1 = *lde1;
    e2r_offset = 1 + e2r_dim1;
    e2r -= e2r_offset;

    /* Function Body */
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L13;
	case 15:  goto L15;
    }

/* ----------------------------------------------------------- */

L1:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[1], &z3[1], &ip2[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L11:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
L48:
/* Computing 2nd power */
    d__1 = *alphn;
/* Computing 2nd power */
    d__2 = *betan;
    abno = d__1 * d__1 + d__2 * d__2;
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum2 = 0.;
	sum3 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sumh = (z2[jkm] + sum2) / abno;
	    sum3 = (z3[jkm] + sum3) / abno;
	    sum2 = sumh * *alphn + sum3 * *betan;
	    sum3 = sum3 * *alphn - sumh * *betan;
	    i__2 = *nm1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		im1 = i__ + *m1;
		z2[im1] += fjac[i__ + jkm * fjac_dim1] * sum2;
		z3[im1] += fjac[i__ + jkm * fjac_dim1] * sum3;
	    }
	}
    }
    radau_solc(nm1, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[*m1 + 1], &z3[*
	    m1 + 1], &ip2[1]);
L49:
    for (i__ = *m1; i__ >= 1; --i__) {
	mpi = *m2 + i__;
	z2i = z2[i__] + z2[mpi];
	z3i = z3[i__] + z3[mpi];
	z3[i__] = (z3i * *alphn - z2i * *betan) / abno;
	z2[i__] = (z2i * *alphn + z3i * *betan) / abno;
    }
    return 0;

/* ----------------------------------------------------------- */

L2:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solbc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &z2[1], &z3[1], &ip2[1]);
    return 0;

/* ----------------------------------------------------------- */

L12:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
L45:
/* Computing 2nd power */
    d__1 = *alphn;
/* Computing 2nd power */
    d__2 = *betan;
    abno = d__1 * d__1 + d__2 * d__2;
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum2 = 0.;
	sum3 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sumh = (z2[jkm] + sum2) / abno;
	    sum3 = (z3[jkm] + sum3) / abno;
	    sum2 = sumh * *alphn + sum3 * *betan;
	    sum3 = sum3 * *alphn - sumh * *betan;
/* Computing MAX */
	    i__2 = 1, i__3 = j - *mujac;
/* Computing MIN */
	    i__5 = *nm1, i__6 = j + *mljac;
	    i__4 = min(i__5,i__6);
	    for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		im1 = i__ + *m1;
		iimu = i__ + *mujac + 1 - j;
		z2[im1] += fjac[iimu + jkm * fjac_dim1] * sum2;
		z3[im1] += fjac[iimu + jkm * fjac_dim1] * sum3;
	    }
	}
    }
    radau_solbc(nm1, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &z2[*m1 + 1], &z3[*m1 + 1], &ip2[1]);
    goto L49;

/* ----------------------------------------------------------- */

L3:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = 0.;
	s3 = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    s2 -= bb * f2[j];
	    s3 -= bb * f3[j];
	}
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[1], &z3[1], &ip2[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L13:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ + *m1;
	s2 = 0.;
	s3 = 0.;
/* Computing MAX */
	i__3 = 1, i__4 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *nm1, i__6 = i__ + *mumas;
	i__2 = min(i__5,i__6);
	for (j = max(i__3,i__4); j <= i__2; ++j) {
	    jm1 = j + *m1;
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    s2 -= bb * f2[jm1];
	    s3 -= bb * f3[jm1];
	}
	z2[im1] = z2[im1] + s2 * *alphn - s3 * *betan;
	z3[im1] = z3[im1] + s3 * *alphn + s2 * *betan;
    }
    if (*ijob == 14) {
	goto L45;
    }
    goto L48;

/* ----------------------------------------------------------- */

L4:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = 0.;
	s3 = 0.;
/* Computing MAX */
	i__2 = 1, i__3 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__4 = min(i__5,i__6);
	for (j = max(i__2,i__3); j <= i__4; ++j) {
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    s2 -= bb * f2[j];
	    s3 -= bb * f3[j];
	}
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solbc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &z2[1], &z3[1], &ip2[1]);
    return 0;

/* ----------------------------------------------------------- */

L5:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = 0.;
	s3 = 0.;
	i__4 = *n;
	for (j = 1; j <= i__4; ++j) {
	    bb = fmas[i__ + j * fmas_dim1];
	    s2 -= bb * f2[j];
	    s3 -= bb * f3[j];
	}
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[1], &z3[1], &ip2[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L15:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ + *m1;
	s2 = 0.;
	s3 = 0.;
	i__4 = *nm1;
	for (j = 1; j <= i__4; ++j) {
	    jm1 = j + *m1;
	    bb = fmas[i__ + j * fmas_dim1];
	    s2 -= bb * f2[jm1];
	    s3 -= bb * f3[jm1];
	}
	z2[im1] = z2[im1] + s2 * *alphn - s3 * *betan;
	z3[im1] = z3[im1] + s3 * *alphn + s2 * *betan;
    }
    goto L48;

/* ----------------------------------------------------------- */

L6:
/* ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ---  THIS OPTION IS NOT PROVIDED */
    return 0;

/* ----------------------------------------------------------- */

L7:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    for (mm = *n - 2; mm >= 1; --mm) {
	mp = *n - mm;
	mp1 = mp - 1;
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L746;
	}
	zsafe = z2[mp];
	z2[mp] = z2[i__];
	z2[i__] = zsafe;
	zsafe = z3[mp];
	z3[mp] = z3[i__];
	z3[i__] = zsafe;
L746:
	i__1 = *n;
	for (i__ = mp + 1; i__ <= i__1; ++i__) {
	    e1imp = fjac[i__ + mp1 * fjac_dim1];
	    z2[i__] -= e1imp * z2[mp];
	    z3[i__] -= e1imp * z3[mp];
	}
    }
    radau_solhc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &c__1, &z2[1], &z3[1],
	     &ip2[1]);
    i__1 = *n - 2;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *n - mm;
	mp1 = mp - 1;
	i__4 = *n;
	for (i__ = mp + 1; i__ <= i__4; ++i__) {
	    e1imp = fjac[i__ + mp1 * fjac_dim1];
	    z2[i__] += e1imp * z2[mp];
	    z3[i__] += e1imp * z3[mp];
	}
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L750;
	}
	zsafe = z2[mp];
	z2[mp] = z2[i__];
	z2[i__] = zsafe;
	zsafe = z3[mp];
	z3[mp] = z3[i__];
	z3[i__] = zsafe;
L750:
	;
    }
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* slvrai_ */


/*     END OF SUBROUTINE SLVRAI */

/* *********************************************************** */

int CIntegratorRAD::slvrad(int32_t *n, double *fjac, int32_t *ldjac, 
	int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	nm1, double *fac1, double *alphn, double *betan, 
	double *e1, double *e2r, double *e2i, int32_t *lde1, 
	double *z1, double *z2, double *z3, double *f1, 
	double *f2, double *f3, double *cont, int32_t *ip1, 
	int32_t *ip2, int32_t *iphes, int32_t *ier, int32_t *ijob)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e1_dim1, 
	    e1_offset, e2r_dim1, e2r_offset, e2i_dim1, e2i_offset, i__1, i__2,
	     i__3, i__4, i__5, i__6;
    double d__1, d__2;

    /* Local variables */
    static int32_t i__, j, k;
    static double s1, s2, s3, bb;
    static int32_t mm, mp, j1b, j2b, im1, jm1, mp1;
    static double z2i, z3i;
    static int32_t jkm, mpi;
    extern /* Subroutine */ int radau_sol(int32_t *, int32_t *, double *, 
	    double *, int32_t *);
    static double sum1, sum2, sum3, ffja, abno;
    extern /* Subroutine */ int radau_solb(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *, double *, int32_t *), radau_solc(int32_t *, 
	    int32_t *, double *, double *, double *, double *,
	     int32_t *), radau_solh(int32_t *, int32_t *, double *, int32_t *, 
	    double *, int32_t *);
    static double sumh, e1imp;
    extern /* Subroutine */ int radau_solbc(int32_t *, int32_t *, double *, 
	    double *, int32_t *, int32_t *, double *, double *, 
	    int32_t *);
    static double zsafe;
    extern /* Subroutine */ int radau_solhc(int32_t *, int32_t *, double *, 
	    double *, int32_t *, double *, double *, int32_t *);


    /* Parameter adjustments */
    --iphes;
    --f3;
    --f2;
    --f1;
    --z3;
    --z2;
    --z1;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip2;
    --ip1;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e2i_dim1 = *lde1;
    e2i_offset = 1 + e2i_dim1;
    e2i -= e2i_offset;
    e2r_dim1 = *lde1;
    e2r_offset = 1 + e2r_dim1;
    e2r -= e2r_offset;
    e1_dim1 = *lde1;
    e1_offset = 1 + e1_dim1;
    e1 -= e1_offset;

    /* Function Body */
    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L7;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L13;
	case 15:  goto L15;
    }

/* ----------------------------------------------------------- */

L1:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_sol(n, lde1, &e1[e1_offset], &z1[1], &ip1[1]);
    radau_solc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[1], &z3[1], &ip2[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L11:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
L48:
/* Computing 2nd power */
    d__1 = *alphn;
/* Computing 2nd power */
    d__2 = *betan;
    abno = d__1 * d__1 + d__2 * d__2;
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	sum2 = 0.;
	sum3 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sum1 = (z1[jkm] + sum1) / *fac1;
	    sumh = (z2[jkm] + sum2) / abno;
	    sum3 = (z3[jkm] + sum3) / abno;
	    sum2 = sumh * *alphn + sum3 * *betan;
	    sum3 = sum3 * *alphn - sumh * *betan;
	    i__2 = *nm1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		im1 = i__ + *m1;
		z1[im1] += fjac[i__ + jkm * fjac_dim1] * sum1;
		z2[im1] += fjac[i__ + jkm * fjac_dim1] * sum2;
		z3[im1] += fjac[i__ + jkm * fjac_dim1] * sum3;
	    }
	}
    }
    radau_sol(nm1, lde1, &e1[e1_offset], &z1[*m1 + 1], &ip1[1]);
    radau_solc(nm1, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[*m1 + 1], &z3[*
	    m1 + 1], &ip2[1]);
L49:
    for (i__ = *m1; i__ >= 1; --i__) {
	mpi = *m2 + i__;
	z1[i__] = (z1[i__] + z1[mpi]) / *fac1;
	z2i = z2[i__] + z2[mpi];
	z3i = z3[i__] + z3[mpi];
	z3[i__] = (z3i * *alphn - z2i * *betan) / abno;
	z2[i__] = (z2i * *alphn + z3i * *betan) / abno;
    }
    return 0;

/* ----------------------------------------------------------- */

L2:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &z1[1], &ip1[1]
	    );
    radau_solbc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &z2[1], &z3[1], &ip2[1]);
    return 0;

/* ----------------------------------------------------------- */

L12:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
L45:
/* Computing 2nd power */
    d__1 = *alphn;
/* Computing 2nd power */
    d__2 = *betan;
    abno = d__1 * d__1 + d__2 * d__2;
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum1 = 0.;
	sum2 = 0.;
	sum3 = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sum1 = (z1[jkm] + sum1) / *fac1;
	    sumh = (z2[jkm] + sum2) / abno;
	    sum3 = (z3[jkm] + sum3) / abno;
	    sum2 = sumh * *alphn + sum3 * *betan;
	    sum3 = sum3 * *alphn - sumh * *betan;
/* Computing MAX */
	    i__2 = 1, i__3 = j - *mujac;
/* Computing MIN */
	    i__5 = *nm1, i__6 = j + *mljac;
	    i__4 = min(i__5,i__6);
	    for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		im1 = i__ + *m1;
		ffja = fjac[i__ + *mujac + 1 - j + jkm * fjac_dim1];
		z1[im1] += ffja * sum1;
		z2[im1] += ffja * sum2;
		z3[im1] += ffja * sum3;
	    }
	}
    }
    radau_solb(nm1, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &z1[*m1 + 1],
	     &ip1[1]);
    radau_solbc(nm1, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &z2[*m1 + 1], &z3[*m1 + 1], &ip2[1]);
    goto L49;

/* ----------------------------------------------------------- */

L3:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s1 = 0.;
	s2 = 0.;
	s3 = 0.;
/* Computing MAX */
	i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__3 = min(i__5,i__6);
	for (j = max(i__4,i__2); j <= i__3; ++j) {
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    s1 -= bb * f1[j];
	    s2 -= bb * f2[j];
	    s3 -= bb * f3[j];
	}
	z1[i__] += s1 * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_sol(n, lde1, &e1[e1_offset], &z1[1], &ip1[1]);
    radau_solc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[1], &z3[1], &ip2[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L13:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ + *m1;
	s1 = 0.;
	s2 = 0.;
	s3 = 0.;
/* Computing MAX */
	i__3 = 1, i__4 = i__ - *mlmas;
	j1b = max(i__3,i__4);
/* Computing MIN */
	i__3 = *nm1, i__4 = i__ + *mumas;
	j2b = min(i__3,i__4);
	i__3 = j2b;
	for (j = j1b; j <= i__3; ++j) {
	    jm1 = j + *m1;
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    s1 -= bb * f1[jm1];
	    s2 -= bb * f2[jm1];
	    s3 -= bb * f3[jm1];
	}
	z1[im1] += s1 * *fac1;
	z2[im1] = z2[im1] + s2 * *alphn - s3 * *betan;
	z3[im1] = z3[im1] + s3 * *alphn + s2 * *betan;
    }
    if (*ijob == 14) {
	goto L45;
    }
    goto L48;

/* ----------------------------------------------------------- */

L4:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s1 = 0.;
	s2 = 0.;
	s3 = 0.;
/* Computing MAX */
	i__3 = 1, i__4 = i__ - *mlmas;
/* Computing MIN */
	i__5 = *n, i__6 = i__ + *mumas;
	i__2 = min(i__5,i__6);
	for (j = max(i__3,i__4); j <= i__2; ++j) {
	    bb = fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1];
	    s1 -= bb * f1[j];
	    s2 -= bb * f2[j];
	    s3 -= bb * f3[j];
	}
	z1[i__] += s1 * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_solb(n, lde1, &e1[e1_offset], &rad_linal.mle, &rad_linal.mue, &z1[1], &ip1[1]
	    );
    radau_solbc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &rad_linal.mle, &
	    rad_linal.mue, &z2[1], &z3[1], &ip2[1]);
    return 0;

/* ----------------------------------------------------------- */

L5:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s1 = 0.;
	s2 = 0.;
	s3 = 0.;
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    bb = fmas[i__ + j * fmas_dim1];
	    s1 -= bb * f1[j];
	    s2 -= bb * f2[j];
	    s3 -= bb * f3[j];
	}
	z1[i__] += s1 * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    radau_sol(n, lde1, &e1[e1_offset], &z1[1], &ip1[1]);
    radau_solc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &z2[1], &z3[1], &ip2[1]
	    );
    return 0;

/* ----------------------------------------------------------- */

L15:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    i__1 = *m1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    i__1 = *nm1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	im1 = i__ + *m1;
	s1 = 0.;
	s2 = 0.;
	s3 = 0.;
	i__2 = *nm1;
	for (j = 1; j <= i__2; ++j) {
	    jm1 = j + *m1;
	    bb = fmas[i__ + j * fmas_dim1];
	    s1 -= bb * f1[jm1];
	    s2 -= bb * f2[jm1];
	    s3 -= bb * f3[jm1];
	}
	z1[im1] += s1 * *fac1;
	z2[im1] = z2[im1] + s2 * *alphn - s3 * *betan;
	z3[im1] = z3[im1] + s3 * *alphn + s2 * *betan;
    }
    goto L48;

/* ----------------------------------------------------------- */

L6:
/* ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ---  THIS OPTION IS NOT PROVIDED */
    return 0;

/* ----------------------------------------------------------- */

L7:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, HESSENBERG-OPTION */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s2 = -f2[i__];
	s3 = -f3[i__];
	z1[i__] -= f1[i__] * *fac1;
	z2[i__] = z2[i__] + s2 * *alphn - s3 * *betan;
	z3[i__] = z3[i__] + s3 * *alphn + s2 * *betan;
    }
    for (mm = *n - 2; mm >= 1; --mm) {
	mp = *n - mm;
	mp1 = mp - 1;
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L746;
	}
	zsafe = z1[mp];
	z1[mp] = z1[i__];
	z1[i__] = zsafe;
	zsafe = z2[mp];
	z2[mp] = z2[i__];
	z2[i__] = zsafe;
	zsafe = z3[mp];
	z3[mp] = z3[i__];
	z3[i__] = zsafe;
L746:
	i__1 = *n;
	for (i__ = mp + 1; i__ <= i__1; ++i__) {
	    e1imp = fjac[i__ + mp1 * fjac_dim1];
	    z1[i__] -= e1imp * z1[mp];
	    z2[i__] -= e1imp * z2[mp];
	    z3[i__] -= e1imp * z3[mp];
	}
    }
    radau_solh(n, lde1, &e1[e1_offset], &c__1, &z1[1], &ip1[1]);
    radau_solhc(n, lde1, &e2r[e2r_offset], &e2i[e2i_offset], &c__1, &z2[1], &z3[1],
	     &ip2[1]);
    i__1 = *n - 2;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *n - mm;
	mp1 = mp - 1;
	i__2 = *n;
	for (i__ = mp + 1; i__ <= i__2; ++i__) {
	    e1imp = fjac[i__ + mp1 * fjac_dim1];
	    z1[i__] += e1imp * z1[mp];
	    z2[i__] += e1imp * z2[mp];
	    z3[i__] += e1imp * z3[mp];
	}
	i__ = iphes[mp];
	if (i__ == mp) {
	    goto L750;
	}
	zsafe = z1[mp];
	z1[mp] = z1[i__];
	z1[i__] = zsafe;
	zsafe = z2[mp];
	z2[mp] = z2[i__];
	z2[i__] = zsafe;
	zsafe = z3[mp];
	z3[mp] = z3[i__];
	z3[i__] = zsafe;
L750:
	;
    }
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* slvrad_ */


/*     END OF SUBROUTINE SLVRAD */

/* *********************************************************** */


/* *********************************************************** */

int CIntegratorRAD::slvrod(int32_t *n, double *fjac, int32_t *ldjac, 
	int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	nm1, double *fac1, double *e, int32_t *lde, int32_t *ip, 
	double *dy, double *ak, double *fx, double *ynew, 
	double *hd, int32_t *ijob, uint8_t *stage1)
{
    /* System generated locals */
    int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e_dim1, e_offset, 
	    i__1, i__2, i__3, i__4, i__5, i__6;

    /* Local variables */
    static int32_t i__, j, k, mm, im1, jkm;
    extern /* Subroutine */ int radau_sol(int32_t *, int32_t *, double *, 
	    double *, int32_t *);
    static double sum;
    extern /* Subroutine */ int radau_solb(int32_t *, int32_t *, double *, 
	    int32_t *, int32_t *, double *, int32_t *);


    /* Parameter adjustments */
    --ynew;
    --fx;
    --ak;
    --dy;
    fjac_dim1 = *ldjac;
    fjac_offset = 1 + fjac_dim1;
    fjac -= fjac_offset;
    --ip;
    fmas_dim1 = *ldmas;
    fmas_offset = 1 + fmas_dim1;
    fmas -= fmas_offset;
    e_dim1 = *lde;
    e_offset = 1 + e_dim1;
    e -= e_offset;

    /* Function Body */
    if (*hd == 0.) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] = dy[i__];
	}
    } else {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] = dy[i__] + *hd * fx[i__];
	}
    }

    switch (*ijob) {
	case 1:  goto L1;
	case 2:  goto L2;
	case 3:  goto L3;
	case 4:  goto L4;
	case 5:  goto L5;
	case 6:  goto L6;
	case 7:  goto L55;
	case 8:  goto L55;
	case 9:  goto L55;
	case 10:  goto L55;
	case 11:  goto L11;
	case 12:  goto L12;
	case 13:  goto L13;
	case 14:  goto L13;
	case 15:  goto L15;
    }

/* ----------------------------------------------------------- */

L1:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] += ynew[i__];
	}
    }
    radau_sol(n, lde, &e[e_offset], &ak[1], &ip[1]);
    return 0;

/* ----------------------------------------------------------- */

L11:
/* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] += ynew[i__];
	}
    }
L48:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sum = (ak[jkm] + sum) / *fac1;
	    i__2 = *nm1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		im1 = i__ + *m1;
		ak[im1] += fjac[i__ + jkm * fjac_dim1] * sum;
	    }
	}
    }
    radau_sol(nm1, lde, &e[e_offset], &ak[*m1 + 1], &ip[1]);
    for (i__ = *m1; i__ >= 1; --i__) {
	ak[i__] = (ak[i__] + ak[*m2 + i__]) / *fac1;
    }
    return 0;

/* ----------------------------------------------------------- */

L2:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] += ynew[i__];
	}
    }
    radau_solb(n, lde, &e[e_offset], &rad_linal.mle, &rad_linal.mue, &ak[1], &ip[1]);
    return 0;

/* ----------------------------------------------------------- */

L12:
/* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] += ynew[i__];
	}
    }
L45:
    mm = *m1 / *m2;
    i__1 = *m2;
    for (j = 1; j <= i__1; ++j) {
	sum = 0.;
	for (k = mm - 1; k >= 0; --k) {
	    jkm = j + k * *m2;
	    sum = (ak[jkm] + sum) / *fac1;
/* Computing MAX */
	    i__2 = 1, i__3 = j - *mujac;
/* Computing MIN */
	    i__5 = *nm1, i__6 = j + *mljac;
	    i__4 = min(i__5,i__6);
	    for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		im1 = i__ + *m1;
		ak[im1] += fjac[i__ + *mujac + 1 - j + jkm * fjac_dim1] * sum;
	    }
	}
    }
    radau_solb(nm1, lde, &e[e_offset], &rad_linal.mle, &rad_linal.mue, &ak[*m1 + 1], &
	    ip[1]);
    for (i__ = *m1; i__ >= 1; --i__) {
	ak[i__] = (ak[i__] + ak[*m2 + i__]) / *fac1;
    }
    return 0;

/* ----------------------------------------------------------- */

L3:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
/* Computing MAX */
	    i__4 = 1, i__2 = i__ - *mlmas;
/* Computing MIN */
	    i__5 = *n, i__6 = i__ + *mumas;
	    i__3 = min(i__5,i__6);
	    for (j = max(i__4,i__2); j <= i__3; ++j) {
		sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ynew[
			j];
	    }
	    ak[i__] += sum;
	}
    }
    radau_sol(n, lde, &e[e_offset], &ak[1], &ip[1]);
    return 0;

/* ----------------------------------------------------------- */

L13:
/* ---  B IS A BANDED MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    if (*stage1) {
	i__1 = *m1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] += ynew[i__];
	}
	i__1 = *nm1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
/* Computing MAX */
	    i__3 = 1, i__4 = i__ - *mlmas;
/* Computing MIN */
	    i__5 = *nm1, i__6 = i__ + *mumas;
	    i__2 = min(i__5,i__6);
	    for (j = max(i__3,i__4); j <= i__2; ++j) {
		sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ynew[
			j + *m1];
	    }
	    im1 = i__ + *m1;
	    ak[im1] += sum;
	}
    }
    if (*ijob == 14) {
	goto L45;
    }
    goto L48;

/* ----------------------------------------------------------- */

L4:
/* ---  B IS A BANDED MATRIX, JACOBIAN A BANDED MATRIX */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
/* Computing MAX */
	    i__2 = 1, i__3 = i__ - *mlmas;
/* Computing MIN */
	    i__5 = *n, i__6 = i__ + *mumas;
	    i__4 = min(i__5,i__6);
	    for (j = max(i__2,i__3); j <= i__4; ++j) {
		sum += fmas[i__ - j + rad_linal.mbdiag + j * fmas_dim1] * ynew[
			j];
	    }
	    ak[i__] += sum;
	}
    }
    radau_solb(n, lde, &e[e_offset], &rad_linal.mle, &rad_linal.mue, &ak[1], &ip[1]);
    return 0;

/* ----------------------------------------------------------- */

L5:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
	    i__4 = *n;
	    for (j = 1; j <= i__4; ++j) {
		sum += fmas[i__ + j * fmas_dim1] * ynew[j];
	    }
	    ak[i__] += sum;
	}
    }
    radau_sol(n, lde, &e[e_offset], &ak[1], &ip[1]);
    return 0;

/* ----------------------------------------------------------- */

L15:
/* ---  B IS A FULL MATRIX, JACOBIAN A FULL MATRIX, SECOND ORDER */
    if (*stage1) {
	i__1 = *m1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ak[i__] += ynew[i__];
	}
	i__1 = *nm1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
	    i__4 = *nm1;
	    for (j = 1; j <= i__4; ++j) {
		sum += fmas[i__ + j * fmas_dim1] * ynew[j + *m1];
	    }
	    im1 = i__ + *m1;
	    ak[im1] += sum;
	}
    }
    goto L48;

/* ----------------------------------------------------------- */

L6:
/* ---  B IS A FULL MATRIX, JACOBIAN A BANDED MATRIX */
/* ---  THIS OPTION IS NOT PROVIDED */
    if (*stage1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    sum = 0.;
	    i__4 = *n;
	    for (j = 1; j <= i__4; ++j) {
/* L623: */
		sum += fmas[i__ + j * fmas_dim1] * ynew[j];
	    }
/* L624: */
	    ak[i__] += sum;
	}
	radau_solb(n, lde, &e[e_offset], &rad_linal.mle, &rad_linal.mue, &ak[1], &ip[1]
		);
    }
    return 0;

/* ----------------------------------------------------------- */

L55:
    return 0;
} /* slvrod_ */


/*     END OF SUBROUTINE SLVROD */


  /* *********************************************************** */

  int CIntegratorRAD::slvseu(int32_t *n, double *fjac, int32_t *ldjac, 
	  int32_t *mljac, int32_t *mujac, double *fmas, int32_t *ldmas, 
	  int32_t *mlmas, int32_t *mumas, int32_t *m1, int32_t *m2, int32_t *
	  nm1, double *fac1, double *e, int32_t *lde, int32_t *ip, 
	  int32_t *iphes, double *del, int32_t *ijob)
  {
      /* System generated locals */
      int32_t fjac_dim1, fjac_offset, fmas_dim1, fmas_offset, e_dim1, e_offset, 
	      i__1, i__2, i__3, i__4, i__5, i__6;

      /* Local variables */
      static int32_t i__, j, k, mm, mp, im1, mp1, jkm, mmm;
      extern /* Subroutine */ int radau_sol(int32_t *, int32_t *, double *, 
	      double *, int32_t *);
      static double sum;
      extern /* Subroutine */ int radau_solb(int32_t *, int32_t *, double *, 
	      int32_t *, int32_t *, double *, int32_t *), radau_solh(int32_t *, 
	      int32_t *, double *, int32_t *, double *, int32_t *);
      static double zsafe;


      /* Parameter adjustments */
      --del;
      --iphes;
      fjac_dim1 = *ldjac;
      fjac_offset = 1 + fjac_dim1;
      fjac -= fjac_offset;
      --ip;
      fmas_dim1 = *ldmas;
      fmas_offset = 1 + fmas_dim1;
      fmas -= fmas_offset;
      e_dim1 = *lde;
      e_offset = 1 + e_dim1;
      e -= e_offset;

      /* Function Body */
      switch (*ijob) {
	  case 1:  goto L1;
	  case 2:  goto L2;
	  case 3:  goto L1;
	  case 4:  goto L2;
	  case 5:  goto L1;
	  case 6:  goto L55;
	  case 7:  goto L7;
	  case 8:  goto L55;
	  case 9:  goto L55;
	  case 10:  goto L55;
	  case 11:  goto L11;
	  case 12:  goto L12;
	  case 13:  goto L11;
	  case 14:  goto L12;
	  case 15:  goto L11;
      }

  /* ----------------------------------------------------------- */

  L1:
  /* ---  B=IDENTITY, JACOBIAN A FULL MATRIX */
      radau_sol(n, lde, &e[e_offset], &del[1], &ip[1]);
      return 0;

  /* ----------------------------------------------------------- */

  L11:
  /* ---  B=IDENTITY, JACOBIAN A FULL MATRIX, SECOND ORDER */
      mm = *m1 / *m2;
      i__1 = *m2;
      for (j = 1; j <= i__1; ++j) {
	  sum = 0.;
	  for (k = mm - 1; k >= 0; --k) {
	      jkm = j + k * *m2;
	      sum = (del[jkm] + sum) / *fac1;
	      i__2 = *nm1;
	      for (i__ = 1; i__ <= i__2; ++i__) {
		  im1 = i__ + *m1;
		  del[im1] += fjac[i__ + jkm * fjac_dim1] * sum;
	      }
	  }
      }
      radau_sol(nm1, lde, &e[e_offset], &del[*m1 + 1], &ip[1]);
      for (i__ = *m1; i__ >= 1; --i__) {
	  del[i__] = (del[i__] + del[*m2 + i__]) / *fac1;
      }
      return 0;

  /* ----------------------------------------------------------- */

  L2:
  /* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX */
      radau_solb(n, lde, &e[e_offset], &rad_linal.mle, &rad_linal.mue, &del[1], &ip[1]);
      return 0;

  /* ----------------------------------------------------------- */

  L12:
  /* ---  B=IDENTITY, JACOBIAN A BANDED MATRIX, SECOND ORDER */
      mm = *m1 / *m2;
      i__1 = *m2;
      for (j = 1; j <= i__1; ++j) {
	  sum = 0.;
	  for (k = mm - 1; k >= 0; --k) {
	      jkm = j + k * *m2;
	      sum = (del[jkm] + sum) / *fac1;
  /* Computing MAX */
	      i__2 = 1, i__3 = j - *mujac;
  /* Computing MIN */
	      i__5 = *nm1, i__6 = j + *mljac;
	      i__4 = min(i__5,i__6);
	      for (i__ = max(i__2,i__3); i__ <= i__4; ++i__) {
		  im1 = i__ + *m1;
		  del[im1] += fjac[i__ + *mujac + 1 - j + jkm * fjac_dim1] * 
			  sum;
	      }
	  }
      }
      radau_solb(nm1, lde, &e[e_offset], &rad_linal.mle, &rad_linal.mue, &del[*m1 + 1], &
	      ip[1]);
      for (i__ = *m1; i__ >= 1; --i__) {
	  del[i__] = (del[i__] + del[*m2 + i__]) / *fac1;
      }
      return 0;

  /* ----------------------------------------------------------- */

  L7:
  /* ---  HESSENBERG OPTION */
      for (mmm = *n - 2; mmm >= 1; --mmm) {
	  mp = *n - mmm;
	  mp1 = mp - 1;
	  i__ = iphes[mp];
	  if (i__ == mp) {
	      goto L110;
	  }
	  zsafe = del[mp];
	  del[mp] = del[i__];
	  del[i__] = zsafe;
  L110:
	  i__1 = *n;
	  for (i__ = mp + 1; i__ <= i__1; ++i__) {
	      del[i__] -= fjac[i__ + mp1 * fjac_dim1] * del[mp];
	  }
      }
      radau_solh(n, lde, &e[e_offset], &c__1, &del[1], &ip[1]);
      i__1 = *n - 2;
      for (mmm = 1; mmm <= i__1; ++mmm) {
	  mp = *n - mmm;
	  mp1 = mp - 1;
	  i__4 = *n;
	  for (i__ = mp + 1; i__ <= i__4; ++i__) {
	      del[i__] += fjac[i__ + mp1 * fjac_dim1] * del[mp];
	  }
	  i__ = iphes[mp];
	  if (i__ == mp) {
	      goto L240;
	  }
	  zsafe = del[mp];
	  del[mp] = del[i__];
	  del[i__] = zsafe;
  L240:
	  ;
      }
      return 0;

  /* ----------------------------------------------------------- */

  L55:
      return 0;
  } /* slvseu_ */

  /* Subroutine */ int radau_dec(int32_t *n, int32_t *ndim, double *a, int32_t *
	  ip, int32_t *ier)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2, i__3;
      double d__1, d__2;

      /* Local variables */
      static int32_t i__, j, k, m;
      static double t;
      static int32_t nm1, kp1;

  /* VERSION REAL DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION. */
  /*  INPUT.. */
  /*     N = ORDER OF MATRIX. */
  /*     NDIM = DECLARED DIMENSION OF ARRAY  A . */
  /*     A = MATRIX TO BE TRIANGULARIZED. */
  /*  OUTPUT.. */
  /*     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U . */
  /*     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L. */
  /*     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW. */
  /*     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O . */
  /*     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE */
  /*           SINGULAR AT STAGE K. */
  /*  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
  /*  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N). */
  /*  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO. */

  /*  REFERENCE.. */
  /*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
  /*     C.A.C.M. 15 (1972), P. 274. */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      a_dim1 = *ndim;
      a_offset = 1 + a_dim1;
      a -= a_offset;

      /* Function Body */
      *ier = 0;
      ip[*n] = 1;
      if (*n == 1) {
	  goto L70;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = k;
	  i__2 = *n;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      if ((d__1 = a[i__ + k * a_dim1], abs(d__1)) > (d__2 = a[m + k * 
		      a_dim1], abs(d__2))) {
		  m = i__;
	      }
  /* L10: */
	  }
	  ip[k] = m;
	  t = a[m + k * a_dim1];
	  if (m == k) {
	      goto L20;
	  }
	  ip[*n] = -ip[*n];
	  a[m + k * a_dim1] = a[k + k * a_dim1];
	  a[k + k * a_dim1] = t;
  L20:
	  if (t == 0.) {
	      goto L80;
	  }
	  t = 1. / t;
	  i__2 = *n;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
  /* L30: */
	      a[i__ + k * a_dim1] = -a[i__ + k * a_dim1] * t;
	  }
	  i__2 = *n;
	  for (j = kp1; j <= i__2; ++j) {
	      t = a[m + j * a_dim1];
	      a[m + j * a_dim1] = a[k + j * a_dim1];
	      a[k + j * a_dim1] = t;
	      if (t == 0.) {
		  goto L45;
	      }
	      i__3 = *n;
	      for (i__ = kp1; i__ <= i__3; ++i__) {
  /* L40: */
		  a[i__ + j * a_dim1] += a[i__ + k * a_dim1] * t;
	      }
  L45:
  /* L50: */
	      ;
	  }
  /* L60: */
      }
  L70:
      k = *n;
      if (a[*n + *n * a_dim1] == 0.) {
	  goto L80;
      }
      return 0;
  L80:
      *ier = k;
      ip[*n] = 0;
      return 0;
  /* ----------------------- END OF SUBROUTINE DEC ------------------------- */
  } /* radau_dec */



  /* Subroutine */ int radau_sol(int32_t *n, int32_t *ndim, double *a, 
	  double *b, int32_t *ip)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2;

      /* Local variables */
      static int32_t i__, k, m;
      static double t;
      static int32_t kb, km1, nm1, kp1;

  /* VERSION REAL DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  SOLUTION OF LINEAR SYSTEM, A*X = B . */
  /*  INPUT.. */
  /*    N = ORDER OF MATRIX. */
  /*    NDIM = DECLARED DIMENSION OF ARRAY  A . */
  /*    A = TRIANGULARIZED MATRIX OBTAINED FROM DEC. */
  /*    B = RIGHT HAND SIDE VECTOR. */
  /*    IP = PIVOT VECTOR OBTAINED FROM DEC. */
  /*  DO NOT USE IF DEC HAS SET IER .NE. 0. */
  /*  OUTPUT.. */
  /*    B = SOLUTION VECTOR, X . */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      --b;
      a_dim1 = *ndim;
      a_offset = 1 + a_dim1;
      a -= a_offset;

      /* Function Body */
      if (*n == 1) {
	  goto L50;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = ip[k];
	  t = b[m];
	  b[m] = b[k];
	  b[k] = t;
	  i__2 = *n;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
  /* L10: */
	      b[i__] += a[i__ + k * a_dim1] * t;
	  }
  /* L20: */
      }
      i__1 = nm1;
      for (kb = 1; kb <= i__1; ++kb) {
	  km1 = *n - kb;
	  k = km1 + 1;
	  b[k] /= a[k + k * a_dim1];
	  t = -b[k];
	  i__2 = km1;
	  for (i__ = 1; i__ <= i__2; ++i__) {
  /* L30: */
	      b[i__] += a[i__ + k * a_dim1] * t;
	  }
  /* L40: */
      }
  L50:
      b[1] /= a[a_dim1 + 1];
      return 0;
  /* ----------------------- END OF SUBROUTINE SOL ------------------------- */
  } /* radau_sol */



  /* Subroutine */ int radau_dech(int32_t *n, int32_t *ndim, double *a, int32_t *
	  lb, int32_t *ip, int32_t *ier)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2, i__3;
      double d__1, d__2;

      /* Local variables */
      static int32_t i__, j, k, m;
      static double t;
      static int32_t na, nm1, kp1;

  /* VERSION REAL DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A HESSENBERG */
  /*  MATRIX WITH LOWER BANDWIDTH LB */
  /*  INPUT.. */
  /*     N = ORDER OF MATRIX A. */
  /*     NDIM = DECLARED DIMENSION OF ARRAY  A . */
  /*     A = MATRIX TO BE TRIANGULARIZED. */
  /*     LB = LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED, LB.GE.1). */
  /*  OUTPUT.. */
  /*     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U . */
  /*     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L. */
  /*     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW. */
  /*     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O . */
  /*     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE */
  /*           SINGULAR AT STAGE K. */
  /*  USE  SOLH  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
  /*  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N). */
  /*  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO. */

  /*  REFERENCE.. */
  /*     THIS IS A SLIGHT MODIFICATION OF */
  /*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
  /*     C.A.C.M. 15 (1972), P. 274. */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      a_dim1 = *ndim;
      a_offset = 1 + a_dim1;
      a -= a_offset;

      /* Function Body */
      *ier = 0;
      ip[*n] = 1;
      if (*n == 1) {
	  goto L70;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = k;
  /* Computing MIN */
	  i__2 = *n, i__3 = *lb + k;
	  na = min(i__2,i__3);
	  i__2 = na;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      if ((d__1 = a[i__ + k * a_dim1], abs(d__1)) > (d__2 = a[m + k * 
		      a_dim1], abs(d__2))) {
		  m = i__;
	      }
  /* L10: */
	  }
	  ip[k] = m;
	  t = a[m + k * a_dim1];
	  if (m == k) {
	      goto L20;
	  }
	  ip[*n] = -ip[*n];
	  a[m + k * a_dim1] = a[k + k * a_dim1];
	  a[k + k * a_dim1] = t;
  L20:
	  if (t == 0.) {
	      goto L80;
	  }
	  t = 1. / t;
	  i__2 = na;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
  /* L30: */
	      a[i__ + k * a_dim1] = -a[i__ + k * a_dim1] * t;
	  }
	  i__2 = *n;
	  for (j = kp1; j <= i__2; ++j) {
	      t = a[m + j * a_dim1];
	      a[m + j * a_dim1] = a[k + j * a_dim1];
	      a[k + j * a_dim1] = t;
	      if (t == 0.) {
		  goto L45;
	      }
	      i__3 = na;
	      for (i__ = kp1; i__ <= i__3; ++i__) {
  /* L40: */
		  a[i__ + j * a_dim1] += a[i__ + k * a_dim1] * t;
	      }
  L45:
  /* L50: */
	      ;
	  }
  /* L60: */
      }
  L70:
      k = *n;
      if (a[*n + *n * a_dim1] == 0.) {
	  goto L80;
      }
      return 0;
  L80:
      *ier = k;
      ip[*n] = 0;
      return 0;
  /* ----------------------- END OF SUBROUTINE DECH ------------------------ */
  } /* radau_dech */



  /* Subroutine */ int radau_solh(int32_t *n, int32_t *ndim, double *a, int32_t *
	  lb, double *b, int32_t *ip)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2, i__3;

      /* Local variables */
      static int32_t i__, k, m;
      static double t;
      static int32_t kb, na, km1, nm1, kp1;

  /* VERSION REAL DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  SOLUTION OF LINEAR SYSTEM, A*X = B . */
  /*  INPUT.. */
  /*    N = ORDER OF MATRIX A. */
  /*    NDIM = DECLARED DIMENSION OF ARRAY  A . */
  /*    A = TRIANGULARIZED MATRIX OBTAINED FROM DECH. */
  /*    LB = LOWER BANDWIDTH OF A. */
  /*    B = RIGHT HAND SIDE VECTOR. */
  /*    IP = PIVOT VECTOR OBTAINED FROM DEC. */
  /*  DO NOT USE IF DECH HAS SET IER .NE. 0. */
  /*  OUTPUT.. */
  /*    B = SOLUTION VECTOR, X . */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      --b;
      a_dim1 = *ndim;
      a_offset = 1 + a_dim1;
      a -= a_offset;

      /* Function Body */
      if (*n == 1) {
	  goto L50;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = ip[k];
	  t = b[m];
	  b[m] = b[k];
	  b[k] = t;
  /* Computing MIN */
	  i__2 = *n, i__3 = *lb + k;
	  na = min(i__2,i__3);
	  i__2 = na;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
  /* L10: */
	      b[i__] += a[i__ + k * a_dim1] * t;
	  }
  /* L20: */
      }
      i__1 = nm1;
      for (kb = 1; kb <= i__1; ++kb) {
	  km1 = *n - kb;
	  k = km1 + 1;
	  b[k] /= a[k + k * a_dim1];
	  t = -b[k];
	  i__2 = km1;
	  for (i__ = 1; i__ <= i__2; ++i__) {
  /* L30: */
	      b[i__] += a[i__ + k * a_dim1] * t;
	  }
  /* L40: */
      }
  L50:
      b[1] /= a[a_dim1 + 1];
      return 0;
  /* ----------------------- END OF SUBROUTINE SOLH ------------------------ */
  } /* radau_solh */


  /* Subroutine */ int radau_decc(int32_t *n, int32_t *ndim, double *ar, 
	  double *ai, int32_t *ip, int32_t *ier)
  {
      /* System generated locals */
      int32_t ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
      double d__1, d__2, d__3, d__4;

      /* Local variables */
      static int32_t i__, j, k, m;
      static double ti, tr;
      static int32_t nm1, kp1;
      static double den, prodi, prodr;

  /* VERSION COMPLEX DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION */
  /*  ------ MODIFICATION FOR COMPLEX MATRICES -------- */
  /*  INPUT.. */
  /*     N = ORDER OF MATRIX. */
  /*     NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI . */
  /*     (AR, AI) = MATRIX TO BE TRIANGULARIZED. */
  /*  OUTPUT.. */
  /*     AR(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; REAL PART. */
  /*     AI(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; IMAGINARY PART. */
  /*     AR(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L. */
  /*                                                    REAL PART. */
  /*     AI(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L. */
  /*                                                    IMAGINARY PART. */
  /*     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW. */
  /*     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O . */
  /*     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE */
  /*           SINGULAR AT STAGE K. */
  /*  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
  /*  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO. */

  /*  REFERENCE.. */
  /*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
  /*     C.A.C.M. 15 (1972), P. 274. */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      ai_dim1 = *ndim;
      ai_offset = 1 + ai_dim1;
      ai -= ai_offset;
      ar_dim1 = *ndim;
      ar_offset = 1 + ar_dim1;
      ar -= ar_offset;

      /* Function Body */
      *ier = 0;
      ip[*n] = 1;
      if (*n == 1) {
	  goto L70;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = k;
	  i__2 = *n;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      if ((d__1 = ar[i__ + k * ar_dim1], abs(d__1)) + (d__2 = ai[i__ + 
		      k * ai_dim1], abs(d__2)) > (d__3 = ar[m + k * ar_dim1], 
		      abs(d__3)) + (d__4 = ai[m + k * ai_dim1], abs(d__4))) {
		  m = i__;
	      }
  /* L10: */
	  }
	  ip[k] = m;
	  tr = ar[m + k * ar_dim1];
	  ti = ai[m + k * ai_dim1];
	  if (m == k) {
	      goto L20;
	  }
	  ip[*n] = -ip[*n];
	  ar[m + k * ar_dim1] = ar[k + k * ar_dim1];
	  ai[m + k * ai_dim1] = ai[k + k * ai_dim1];
	  ar[k + k * ar_dim1] = tr;
	  ai[k + k * ai_dim1] = ti;
  L20:
	  if (abs(tr) + abs(ti) == 0.) {
	      goto L80;
	  }
	  den = tr * tr + ti * ti;
	  tr /= den;
	  ti = -ti / den;
	  i__2 = *n;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      ar[i__ + k * ar_dim1] = -prodr;
	      ai[i__ + k * ai_dim1] = -prodi;
  /* L30: */
	  }
	  i__2 = *n;
	  for (j = kp1; j <= i__2; ++j) {
	      tr = ar[m + j * ar_dim1];
	      ti = ai[m + j * ai_dim1];
	      ar[m + j * ar_dim1] = ar[k + j * ar_dim1];
	      ai[m + j * ai_dim1] = ai[k + j * ai_dim1];
	      ar[k + j * ar_dim1] = tr;
	      ai[k + j * ai_dim1] = ti;
	      if (abs(tr) + abs(ti) == 0.) {
		  goto L48;
	      }
	      if (ti == 0.) {
		  i__3 = *n;
		  for (i__ = kp1; i__ <= i__3; ++i__) {
		      prodr = ar[i__ + k * ar_dim1] * tr;
		      prodi = ai[i__ + k * ai_dim1] * tr;
		      ar[i__ + j * ar_dim1] += prodr;
		      ai[i__ + j * ai_dim1] += prodi;
  /* L40: */
		  }
		  goto L48;
	      }
	      if (tr == 0.) {
		  i__3 = *n;
		  for (i__ = kp1; i__ <= i__3; ++i__) {
		      prodr = -ai[i__ + k * ai_dim1] * ti;
		      prodi = ar[i__ + k * ar_dim1] * ti;
		      ar[i__ + j * ar_dim1] += prodr;
		      ai[i__ + j * ai_dim1] += prodi;
  /* L45: */
		  }
		  goto L48;
	      }
	      i__3 = *n;
	      for (i__ = kp1; i__ <= i__3; ++i__) {
		  prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * 
			  ti;
		  prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * 
			  ti;
		  ar[i__ + j * ar_dim1] += prodr;
		  ai[i__ + j * ai_dim1] += prodi;
  /* L47: */
	      }
  L48:
  /* L50: */
	      ;
	  }
  /* L60: */
      }
  L70:
      k = *n;
      if ((d__1 = ar[*n + *n * ar_dim1], abs(d__1)) + (d__2 = ai[*n + *n * 
	      ai_dim1], abs(d__2)) == 0.) {
	  goto L80;
      }
      return 0;
  L80:
      *ier = k;
      ip[*n] = 0;
      return 0;
  /* ----------------------- END OF SUBROUTINE DECC ------------------------ */
  } /* radau_decc */



  int radau_solc(int32_t *n, int32_t *ndim, double *ar, double *ai, double *br, double *bi, int32_t *ip)
  {
      /* System generated locals */
      int32_t ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;

      /* Local variables */
      static int32_t i__, k, m, kb;
      static double ti, tr;
      static int32_t km1, nm1, kp1;
      static double den, prodi, prodr;

  /* VERSION COMPLEX DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  SOLUTION OF LINEAR SYSTEM, A*X = B . */
  /*  INPUT.. */
  /*    N = ORDER OF MATRIX. */
  /*    NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI. */
  /*    (AR,AI) = TRIANGULARIZED MATRIX OBTAINED FROM DEC. */
  /*    (BR,BI) = RIGHT HAND SIDE VECTOR. */
  /*    IP = PIVOT VECTOR OBTAINED FROM DEC. */
  /*  DO NOT USE IF DEC HAS SET IER .NE. 0. */
  /*  OUTPUT.. */
  /*    (BR,BI) = SOLUTION VECTOR, X . */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      --bi;
      --br;
      ai_dim1 = *ndim;
      ai_offset = 1 + ai_dim1;
      ai -= ai_offset;
      ar_dim1 = *ndim;
      ar_offset = 1 + ar_dim1;
      ar -= ar_offset;

      /* Function Body */
      if (*n == 1) {
	  goto L50;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = ip[k];
	  tr = br[m];
	  ti = bi[m];
	  br[m] = br[k];
	  bi[m] = bi[k];
	  br[k] = tr;
	  bi[k] = ti;
	  i__2 = *n;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      br[i__] += prodr;
	      bi[i__] += prodi;
  /* L10: */
	  }
  /* L20: */
      }
      i__1 = nm1;
      for (kb = 1; kb <= i__1; ++kb) {
	  km1 = *n - kb;
	  k = km1 + 1;
	  den = ar[k + k * ar_dim1] * ar[k + k * ar_dim1] + ai[k + k * ai_dim1] 
		  * ai[k + k * ai_dim1];
	  prodr = br[k] * ar[k + k * ar_dim1] + bi[k] * ai[k + k * ai_dim1];
	  prodi = bi[k] * ar[k + k * ar_dim1] - br[k] * ai[k + k * ai_dim1];
	  br[k] = prodr / den;
	  bi[k] = prodi / den;
	  tr = -br[k];
	  ti = -bi[k];
	  i__2 = km1;
	  for (i__ = 1; i__ <= i__2; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      br[i__] += prodr;
	      bi[i__] += prodi;
  /* L30: */
	  }
  /* L40: */
      }
  L50:
      den = ar[ar_dim1 + 1] * ar[ar_dim1 + 1] + ai[ai_dim1 + 1] * ai[ai_dim1 + 
	      1];
      prodr = br[1] * ar[ar_dim1 + 1] + bi[1] * ai[ai_dim1 + 1];
      prodi = bi[1] * ar[ar_dim1 + 1] - br[1] * ai[ai_dim1 + 1];
      br[1] = prodr / den;
      bi[1] = prodi / den;
      return 0;
  /* ----------------------- END OF SUBROUTINE SOLC ------------------------ */
  } /* radau_solc */



  int radau_dechc(int32_t *n, int32_t *ndim, double *ar, double *ai, int32_t *lb, int32_t *ip, int32_t *ier)
  {
      /* System generated locals */
      int32_t ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
      double d__1, d__2, d__3, d__4;

      /* Local variables */
      static int32_t i__, j, k, m, na;
      static double ti, tr;
      static int32_t nm1, kp1;
      static double den, prodi, prodr;

  /* VERSION COMPLEX DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION */
  /*  ------ MODIFICATION FOR COMPLEX MATRICES -------- */
  /*  INPUT.. */
  /*     N = ORDER OF MATRIX. */
  /*     NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI . */
  /*     (AR, AI) = MATRIX TO BE TRIANGULARIZED. */
  /*  OUTPUT.. */
  /*     AR(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; REAL PART. */
  /*     AI(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U ; IMAGINARY PART. */
  /*     AR(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L. */
  /*                                                    REAL PART. */
  /*     AI(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L. */
  /*                                                    IMAGINARY PART. */
  /*     LB = LOWER BANDWIDTH OF A (DIAGONAL NOT COUNTED), LB.GE.1. */
  /*     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW. */
  /*     IP(N) = (-1)**(NUMBER OF INTERCHANGES) OR O . */
  /*     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE */
  /*           SINGULAR AT STAGE K. */
  /*  USE  SOL  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
  /*  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO. */

  /*  REFERENCE.. */
  /*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
  /*     C.A.C.M. 15 (1972), P. 274. */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      ai_dim1 = *ndim;
      ai_offset = 1 + ai_dim1;
      ai -= ai_offset;
      ar_dim1 = *ndim;
      ar_offset = 1 + ar_dim1;
      ar -= ar_offset;

      /* Function Body */
      *ier = 0;
      ip[*n] = 1;
      if (*lb == 0) {
	  goto L70;
      }
      if (*n == 1) {
	  goto L70;
      }
      nm1 = *n - 1;
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = k;
  /* Computing MIN */
	  i__2 = *n, i__3 = *lb + k;
	  na = min(i__2,i__3);
	  i__2 = na;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      if ((d__1 = ar[i__ + k * ar_dim1], abs(d__1)) + (d__2 = ai[i__ + 
		      k * ai_dim1], abs(d__2)) > (d__3 = ar[m + k * ar_dim1], 
		      abs(d__3)) + (d__4 = ai[m + k * ai_dim1], abs(d__4))) {
		  m = i__;
	      }
  /* L10: */
	  }
	  ip[k] = m;
	  tr = ar[m + k * ar_dim1];
	  ti = ai[m + k * ai_dim1];
	  if (m == k) {
	      goto L20;
	  }
	  ip[*n] = -ip[*n];
	  ar[m + k * ar_dim1] = ar[k + k * ar_dim1];
	  ai[m + k * ai_dim1] = ai[k + k * ai_dim1];
	  ar[k + k * ar_dim1] = tr;
	  ai[k + k * ai_dim1] = ti;
  L20:
	  if (abs(tr) + abs(ti) == 0.) {
	      goto L80;
	  }
	  den = tr * tr + ti * ti;
	  tr /= den;
	  ti = -ti / den;
	  i__2 = na;
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      ar[i__ + k * ar_dim1] = -prodr;
	      ai[i__ + k * ai_dim1] = -prodi;
  /* L30: */
	  }
	  i__2 = *n;
	  for (j = kp1; j <= i__2; ++j) {
	      tr = ar[m + j * ar_dim1];
	      ti = ai[m + j * ai_dim1];
	      ar[m + j * ar_dim1] = ar[k + j * ar_dim1];
	      ai[m + j * ai_dim1] = ai[k + j * ai_dim1];
	      ar[k + j * ar_dim1] = tr;
	      ai[k + j * ai_dim1] = ti;
	      if (abs(tr) + abs(ti) == 0.) {
		  goto L48;
	      }
	      if (ti == 0.) {
		  i__3 = na;
		  for (i__ = kp1; i__ <= i__3; ++i__) {
		      prodr = ar[i__ + k * ar_dim1] * tr;
		      prodi = ai[i__ + k * ai_dim1] * tr;
		      ar[i__ + j * ar_dim1] += prodr;
		      ai[i__ + j * ai_dim1] += prodi;
  /* L40: */
		  }
		  goto L48;
	      }
	      if (tr == 0.) {
		  i__3 = na;
		  for (i__ = kp1; i__ <= i__3; ++i__) {
		      prodr = -ai[i__ + k * ai_dim1] * ti;
		      prodi = ar[i__ + k * ar_dim1] * ti;
		      ar[i__ + j * ar_dim1] += prodr;
		      ai[i__ + j * ai_dim1] += prodi;
  /* L45: */
		  }
		  goto L48;
	      }
	      i__3 = na;
	      for (i__ = kp1; i__ <= i__3; ++i__) {
		  prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * 
			  ti;
		  prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * 
			  ti;
		  ar[i__ + j * ar_dim1] += prodr;
		  ai[i__ + j * ai_dim1] += prodi;
  /* L47: */
	      }
  L48:
  /* L50: */
	      ;
	  }
  /* L60: */
      }
  L70:
      k = *n;
      if ((d__1 = ar[*n + *n * ar_dim1], abs(d__1)) + (d__2 = ai[*n + *n * 
	      ai_dim1], abs(d__2)) == 0.) {
	  goto L80;
      }
      return 0;
  L80:
      *ier = k;
      ip[*n] = 0;
      return 0;
  /* ----------------------- END OF SUBROUTINE DECHC ----------------------- */
  } /* radau_dechc */



  int radau_solhc(int32_t *n, int32_t *ndim, double *ar, double *ai, int32_t *lb, double *br, double *bi, int32_t *ip)
  {
      /* System generated locals */
      int32_t ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3, i__4;

      /* Local variables */
      static int32_t i__, k, m, kb;
      static double ti, tr;
      static int32_t km1, nm1, kp1;
      static double den, prodi, prodr;

  /* VERSION COMPLEX DOUBLE PRECISION */
  /* ----------------------------------------------------------------------- */
  /*  SOLUTION OF LINEAR SYSTEM, A*X = B . */
  /*  INPUT.. */
  /*    N = ORDER OF MATRIX. */
  /*    NDIM = DECLARED DIMENSION OF ARRAYS  AR AND AI. */
  /*    (AR,AI) = TRIANGULARIZED MATRIX OBTAINED FROM DEC. */
  /*    (BR,BI) = RIGHT HAND SIDE VECTOR. */
  /*    LB = LOWER BANDWIDTH OF A. */
  /*    IP = PIVOT VECTOR OBTAINED FROM DEC. */
  /*  DO NOT USE IF DEC HAS SET IER .NE. 0. */
  /*  OUTPUT.. */
  /*    (BR,BI) = SOLUTION VECTOR, X . */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      --bi;
      --br;
      ai_dim1 = *ndim;
      ai_offset = 1 + ai_dim1;
      ai -= ai_offset;
      ar_dim1 = *ndim;
      ar_offset = 1 + ar_dim1;
      ar -= ar_offset;

      /* Function Body */
      if (*n == 1) {
	  goto L50;
      }
      nm1 = *n - 1;
      if (*lb == 0) {
	  goto L25;
      }
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  kp1 = k + 1;
	  m = ip[k];
	  tr = br[m];
	  ti = bi[m];
	  br[m] = br[k];
	  bi[m] = bi[k];
	  br[k] = tr;
	  bi[k] = ti;
  /* Computing MIN */
	  i__3 = *n, i__4 = *lb + k;
	  i__2 = min(i__3,i__4);
	  for (i__ = kp1; i__ <= i__2; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      br[i__] += prodr;
	      bi[i__] += prodi;
  /* L10: */
	  }
  /* L20: */
      }
  L25:
      i__1 = nm1;
      for (kb = 1; kb <= i__1; ++kb) {
	  km1 = *n - kb;
	  k = km1 + 1;
	  den = ar[k + k * ar_dim1] * ar[k + k * ar_dim1] + ai[k + k * ai_dim1] 
		  * ai[k + k * ai_dim1];
	  prodr = br[k] * ar[k + k * ar_dim1] + bi[k] * ai[k + k * ai_dim1];
	  prodi = bi[k] * ar[k + k * ar_dim1] - br[k] * ai[k + k * ai_dim1];
	  br[k] = prodr / den;
	  bi[k] = prodi / den;
	  tr = -br[k];
	  ti = -bi[k];
	  i__2 = km1;
	  for (i__ = 1; i__ <= i__2; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      br[i__] += prodr;
	      bi[i__] += prodi;
  /* L30: */
	  }
  /* L40: */
      }
  L50:
      den = ar[ar_dim1 + 1] * ar[ar_dim1 + 1] + ai[ai_dim1 + 1] * ai[ai_dim1 + 
	      1];
      prodr = br[1] * ar[ar_dim1 + 1] + bi[1] * ai[ai_dim1 + 1];
      prodi = bi[1] * ar[ar_dim1 + 1] - br[1] * ai[ai_dim1 + 1];
      br[1] = prodr / den;
      bi[1] = prodi / den;
      return 0;
  /* ----------------------- END OF SUBROUTINE SOLHC ----------------------- */
  } /* radau_solhc */


  int radau_decb(int32_t *n, int32_t *ndim, double *a, int32_t *ml, int32_t *mu, int32_t *ip, int32_t *ier)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2, i__3, i__4;
      double d__1, d__2;

      /* Local variables */
      static int32_t i__, j, k, m;
      static double t;
      static int32_t md, jk, mm, ju, md1, nm1, kp1, mdl, ijk;

  /* ----------------------------------------------------------------------- */
  /*  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED */
  /*  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU */
  /*  INPUT.. */
  /*     N       ORDER OF THE ORIGINAL MATRIX A. */
  /*     NDIM    DECLARED DIMENSION OF ARRAY  A. */
  /*     A       CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS */
  /*                OF THE MATRIX ARE STORED IN THE COLUMNS OF  A  AND */
  /*                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS */
  /*                ML+1 THROUGH 2*ML+MU+1 OF  A. */
  /*     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*  OUTPUT.. */
  /*     A       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND */
  /*                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT. */
  /*     IP      INDEX VECTOR OF PIVOT INDICES. */
  /*     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O . */
  /*     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE */
  /*                SINGULAR AT STAGE K. */
  /*  USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
  /*  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1. */
  /*  IF IP(N)=O, A IS SINGULAR, SOLB WILL DIVIDE BY ZERO. */

  /*  REFERENCE.. */
  /*     THIS IS A MODIFICATION OF */
  /*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
  /*     C.A.C.M. 15 (1972), P. 274. */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      a_dim1 = *ndim;
      a_offset = 1 + a_dim1;
      a -= a_offset;

      /* Function Body */
      *ier = 0;
      ip[*n] = 1;
      md = *ml + *mu + 1;
      md1 = md + 1;
      ju = 0;
      if (*ml == 0) {
	  goto L70;
      }
      if (*n == 1) {
	  goto L70;
      }
      if (*n < *mu + 2) {
	  goto L7;
      }
      i__1 = *n;
      for (j = *mu + 2; j <= i__1; ++j) {
	  i__2 = *ml;
	  for (i__ = 1; i__ <= i__2; ++i__) {
  /* L5: */
	      a[i__ + j * a_dim1] = 0.;
	  }
      }
  L7:
      nm1 = *n - 1;
      i__2 = nm1;
      for (k = 1; k <= i__2; ++k) {
	  kp1 = k + 1;
	  m = md;
  /* Computing MIN */
	  i__1 = *ml, i__3 = *n - k;
	  mdl = min(i__1,i__3) + md;
	  i__1 = mdl;
	  for (i__ = md1; i__ <= i__1; ++i__) {
	      if ((d__1 = a[i__ + k * a_dim1], abs(d__1)) > (d__2 = a[m + k * 
		      a_dim1], abs(d__2))) {
		  m = i__;
	      }
  /* L10: */
	  }
	  ip[k] = m + k - md;
	  t = a[m + k * a_dim1];
	  if (m == md) {
	      goto L20;
	  }
	  ip[*n] = -ip[*n];
	  a[m + k * a_dim1] = a[md + k * a_dim1];
	  a[md + k * a_dim1] = t;
  L20:
	  if (t == 0.) {
	      goto L80;
	  }
	  t = 1. / t;
	  i__1 = mdl;
	  for (i__ = md1; i__ <= i__1; ++i__) {
  /* L30: */
	      a[i__ + k * a_dim1] = -a[i__ + k * a_dim1] * t;
	  }
  /* Computing MIN */
  /* Computing MAX */
	  i__3 = ju, i__4 = *mu + ip[k];
	  i__1 = max(i__3,i__4);
	  ju = min(i__1,*n);
	  mm = md;
	  if (ju < kp1) {
	      goto L55;
	  }
	  i__1 = ju;
	  for (j = kp1; j <= i__1; ++j) {
	      --m;
	      --mm;
	      t = a[m + j * a_dim1];
	      if (m == mm) {
		  goto L35;
	      }
	      a[m + j * a_dim1] = a[mm + j * a_dim1];
	      a[mm + j * a_dim1] = t;
  L35:
	      if (t == 0.) {
		  goto L45;
	      }
	      jk = j - k;
	      i__3 = mdl;
	      for (i__ = md1; i__ <= i__3; ++i__) {
		  ijk = i__ - jk;
  /* L40: */
		  a[ijk + j * a_dim1] += a[i__ + k * a_dim1] * t;
	      }
  L45:
  /* L50: */
	      ;
	  }
  L55:
  /* L60: */
	  ;
      }
  L70:
      k = *n;
      if (a[md + *n * a_dim1] == 0.) {
	  goto L80;
      }
      return 0;
  L80:
      *ier = k;
      ip[*n] = 0;
      return 0;
  /* ----------------------- END OF SUBROUTINE DECB ------------------------ */
  } /* radau_decb */


  
  int radau_solb(int32_t *n, int32_t *ndim, double *a, int32_t *ml, int32_t *mu, double *b, int32_t *ip)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2, i__3;

      /* Local variables */
      static int32_t i__, k, m;
      static double t;
      static int32_t kb, md, lm, md1, nm1, imd, kmd, mdl, mdm;

  /* ----------------------------------------------------------------------- */
  /*  SOLUTION OF LINEAR SYSTEM, A*X = B . */
  /*  INPUT.. */
  /*    N      ORDER OF MATRIX A. */
  /*    NDIM   DECLARED DIMENSION OF ARRAY  A . */
  /*    A      TRIANGULARIZED MATRIX OBTAINED FROM DECB. */
  /*    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*    B      RIGHT HAND SIDE VECTOR. */
  /*    IP     PIVOT VECTOR OBTAINED FROM DECB. */
  /*  DO NOT USE IF DECB HAS SET IER .NE. 0. */
  /*  OUTPUT.. */
  /*    B      SOLUTION VECTOR, X . */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      --b;
      a_dim1 = *ndim;
      a_offset = 1 + a_dim1;
      a -= a_offset;

      /* Function Body */
      md = *ml + *mu + 1;
      md1 = md + 1;
      mdm = md - 1;
      nm1 = *n - 1;
      if (*ml == 0) {
	  goto L25;
      }
      if (*n == 1) {
	  goto L50;
      }
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  m = ip[k];
	  t = b[m];
	  b[m] = b[k];
	  b[k] = t;
  /* Computing MIN */
	  i__2 = *ml, i__3 = *n - k;
	  mdl = min(i__2,i__3) + md;
	  i__2 = mdl;
	  for (i__ = md1; i__ <= i__2; ++i__) {
	      imd = i__ + k - md;
  /* L10: */
	      b[imd] += a[i__ + k * a_dim1] * t;
	  }
  /* L20: */
      }
  L25:
      i__1 = nm1;
      for (kb = 1; kb <= i__1; ++kb) {
	  k = *n + 1 - kb;
	  b[k] /= a[md + k * a_dim1];
	  t = -b[k];
	  kmd = md - k;
  /* Computing MAX */
	  i__2 = 1, i__3 = kmd + 1;
	  lm = max(i__2,i__3);
	  i__2 = mdm;
	  for (i__ = lm; i__ <= i__2; ++i__) {
	      imd = i__ - kmd;
  /* L30: */
	      b[imd] += a[i__ + k * a_dim1] * t;
	  }
  /* L40: */
      }
  L50:
      b[1] /= a[md + a_dim1];
      return 0;
  /* ----------------------- END OF SUBROUTINE SOLB ------------------------ */
  } /* radau_solb */


  int radau_decbc(int32_t *n, int32_t *ndim, double *ar, double *ai, int32_t *ml, int32_t *mu, int32_t *ip, int32_t *ier)
  {
      /* System generated locals */
      int32_t ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3, i__4;
      double d__1, d__2, d__3, d__4;

      /* Local variables */
      static int32_t i__, j, k, m, md, jk, mm;
      static double ti;
      static int32_t ju;
      static double tr;
      static int32_t md1, nm1, kp1;
      static double den;
      static int32_t mdl, ijk;
      static double prodi, prodr;

  /* ----------------------------------------------------------------------- */
  /*  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A BANDED COMPLEX */
  /*  MATRIX WITH LOWER BANDWIDTH ML AND UPPER BANDWIDTH MU */
  /*  INPUT.. */
  /*     N       ORDER OF THE ORIGINAL MATRIX A. */
  /*     NDIM    DECLARED DIMENSION OF ARRAY  A. */
  /*     AR, AI     CONTAINS THE MATRIX IN BAND STORAGE.   THE COLUMNS */
  /*                OF THE MATRIX ARE STORED IN THE COLUMNS OF  AR (REAL */
  /*                PART) AND AI (IMAGINARY PART)  AND */
  /*                THE DIAGONALS OF THE MATRIX ARE STORED IN ROWS */
  /*                ML+1 THROUGH 2*ML+MU+1 OF  AR AND AI. */
  /*     ML      LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*     MU      UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*  OUTPUT.. */
  /*     AR, AI  AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND */
  /*                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT. */
  /*     IP      INDEX VECTOR OF PIVOT INDICES. */
  /*     IP(N)   (-1)**(NUMBER OF INTERCHANGES) OR O . */
  /*     IER     = 0 IF MATRIX A IS NONSINGULAR, OR  = K IF FOUND TO BE */
  /*                SINGULAR AT STAGE K. */
  /*  USE  SOLBC  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
  /*  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1. */
  /*  IF IP(N)=O, A IS SINGULAR, SOLBC WILL DIVIDE BY ZERO. */

  /*  REFERENCE.. */
  /*     THIS IS A MODIFICATION OF */
  /*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
  /*     C.A.C.M. 15 (1972), P. 274. */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      ai_dim1 = *ndim;
      ai_offset = 1 + ai_dim1;
      ai -= ai_offset;
      ar_dim1 = *ndim;
      ar_offset = 1 + ar_dim1;
      ar -= ar_offset;

      /* Function Body */
      *ier = 0;
      ip[*n] = 1;
      md = *ml + *mu + 1;
      md1 = md + 1;
      ju = 0;
      if (*ml == 0) {
	  goto L70;
      }
      if (*n == 1) {
	  goto L70;
      }
      if (*n < *mu + 2) {
	  goto L7;
      }
      i__1 = *n;
      for (j = *mu + 2; j <= i__1; ++j) {
	  i__2 = *ml;
	  for (i__ = 1; i__ <= i__2; ++i__) {
	      ar[i__ + j * ar_dim1] = 0.;
	      ai[i__ + j * ai_dim1] = 0.;
  /* L5: */
	  }
      }
  L7:
      nm1 = *n - 1;
      i__2 = nm1;
      for (k = 1; k <= i__2; ++k) {
	  kp1 = k + 1;
	  m = md;
  /* Computing MIN */
	  i__1 = *ml, i__3 = *n - k;
	  mdl = min(i__1,i__3) + md;
	  i__1 = mdl;
	  for (i__ = md1; i__ <= i__1; ++i__) {
	      if ((d__1 = ar[i__ + k * ar_dim1], abs(d__1)) + (d__2 = ai[i__ + 
		      k * ai_dim1], abs(d__2)) > (d__3 = ar[m + k * ar_dim1], 
		      abs(d__3)) + (d__4 = ai[m + k * ai_dim1], abs(d__4))) {
		  m = i__;
	      }
  /* L10: */
	  }
	  ip[k] = m + k - md;
	  tr = ar[m + k * ar_dim1];
	  ti = ai[m + k * ai_dim1];
	  if (m == md) {
	      goto L20;
	  }
	  ip[*n] = -ip[*n];
	  ar[m + k * ar_dim1] = ar[md + k * ar_dim1];
	  ai[m + k * ai_dim1] = ai[md + k * ai_dim1];
	  ar[md + k * ar_dim1] = tr;
	  ai[md + k * ai_dim1] = ti;
  L20:
	  if (abs(tr) + abs(ti) == 0.) {
	      goto L80;
	  }
	  den = tr * tr + ti * ti;
	  tr /= den;
	  ti = -ti / den;
	  i__1 = mdl;
	  for (i__ = md1; i__ <= i__1; ++i__) {
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      ar[i__ + k * ar_dim1] = -prodr;
	      ai[i__ + k * ai_dim1] = -prodi;
  /* L30: */
	  }
  /* Computing MIN */
  /* Computing MAX */
	  i__3 = ju, i__4 = *mu + ip[k];
	  i__1 = max(i__3,i__4);
	  ju = min(i__1,*n);
	  mm = md;
	  if (ju < kp1) {
	      goto L55;
	  }
	  i__1 = ju;
	  for (j = kp1; j <= i__1; ++j) {
	      --m;
	      --mm;
	      tr = ar[m + j * ar_dim1];
	      ti = ai[m + j * ai_dim1];
	      if (m == mm) {
		  goto L35;
	      }
	      ar[m + j * ar_dim1] = ar[mm + j * ar_dim1];
	      ai[m + j * ai_dim1] = ai[mm + j * ai_dim1];
	      ar[mm + j * ar_dim1] = tr;
	      ai[mm + j * ai_dim1] = ti;
  L35:
	      if (abs(tr) + abs(ti) == 0.) {
		  goto L48;
	      }
	      jk = j - k;
	      if (ti == 0.) {
		  i__3 = mdl;
		  for (i__ = md1; i__ <= i__3; ++i__) {
		      ijk = i__ - jk;
		      prodr = ar[i__ + k * ar_dim1] * tr;
		      prodi = ai[i__ + k * ai_dim1] * tr;
		      ar[ijk + j * ar_dim1] += prodr;
		      ai[ijk + j * ai_dim1] += prodi;
  /* L40: */
		  }
		  goto L48;
	      }
	      if (tr == 0.) {
		  i__3 = mdl;
		  for (i__ = md1; i__ <= i__3; ++i__) {
		      ijk = i__ - jk;
		      prodr = -ai[i__ + k * ai_dim1] * ti;
		      prodi = ar[i__ + k * ar_dim1] * ti;
		      ar[ijk + j * ar_dim1] += prodr;
		      ai[ijk + j * ai_dim1] += prodi;
  /* L45: */
		  }
		  goto L48;
	      }
	      i__3 = mdl;
	      for (i__ = md1; i__ <= i__3; ++i__) {
		  ijk = i__ - jk;
		  prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * 
			  ti;
		  prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * 
			  ti;
		  ar[ijk + j * ar_dim1] += prodr;
		  ai[ijk + j * ai_dim1] += prodi;
  /* L47: */
	      }
  L48:
  /* L50: */
	      ;
	  }
  L55:
  /* L60: */
	  ;
      }
  L70:
      k = *n;
      if ((d__1 = ar[md + *n * ar_dim1], abs(d__1)) + (d__2 = ai[md + *n * 
	      ai_dim1], abs(d__2)) == 0.) {
	  goto L80;
      }
      return 0;
  L80:
      *ier = k;
      ip[*n] = 0;
      return 0;
  /* ----------------------- END OF SUBROUTINE DECBC ------------------------ */
  } /* radau_decbc */



  int radau_solbc(int32_t *n, int32_t *ndim, double *ar, double *ai, int32_t *ml, int32_t *mu, double *br, double *bi, int32_t *ip)
  {
      /* System generated locals */
      int32_t ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;

      /* Local variables */
      static int32_t i__, k, m, kb, md, lm;
      static double ti, tr;
      static int32_t md1, nm1;
      static double den;
      static int32_t imd, kmd, mdl, mdm;
      static double prodi, prodr;

  /* ----------------------------------------------------------------------- */
  /*  SOLUTION OF LINEAR SYSTEM, A*X = B , */
  /*                  VERSION BANDED AND COMPLEX-DOUBLE PRECISION. */
  /*  INPUT.. */
  /*    N      ORDER OF MATRIX A. */
  /*    NDIM   DECLARED DIMENSION OF ARRAY  A . */
  /*    AR, AI TRIANGULARIZED MATRIX OBTAINED FROM DECB (REAL AND IMAG. PART). */
  /*    ML     LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*    MU     UPPER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED). */
  /*    BR, BI RIGHT HAND SIDE VECTOR (REAL AND IMAG. PART). */
  /*    IP     PIVOT VECTOR OBTAINED FROM DECBC. */
  /*  DO NOT USE IF DECB HAS SET IER .NE. 0. */
  /*  OUTPUT.. */
  /*    BR, BI SOLUTION VECTOR, X (REAL AND IMAG. PART). */
  /* ----------------------------------------------------------------------- */
      /* Parameter adjustments */
      --ip;
      --bi;
      --br;
      ai_dim1 = *ndim;
      ai_offset = 1 + ai_dim1;
      ai -= ai_offset;
      ar_dim1 = *ndim;
      ar_offset = 1 + ar_dim1;
      ar -= ar_offset;

      /* Function Body */
      md = *ml + *mu + 1;
      md1 = md + 1;
      mdm = md - 1;
      nm1 = *n - 1;
      if (*ml == 0) {
	  goto L25;
      }
      if (*n == 1) {
	  goto L50;
      }
      i__1 = nm1;
      for (k = 1; k <= i__1; ++k) {
	  m = ip[k];
	  tr = br[m];
	  ti = bi[m];
	  br[m] = br[k];
	  bi[m] = bi[k];
	  br[k] = tr;
	  bi[k] = ti;
  /* Computing MIN */
	  i__2 = *ml, i__3 = *n - k;
	  mdl = min(i__2,i__3) + md;
	  i__2 = mdl;
	  for (i__ = md1; i__ <= i__2; ++i__) {
	      imd = i__ + k - md;
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      br[imd] += prodr;
	      bi[imd] += prodi;
  /* L10: */
	  }
  /* L20: */
      }
  L25:
      i__1 = nm1;
      for (kb = 1; kb <= i__1; ++kb) {
	  k = *n + 1 - kb;
	  den = ar[md + k * ar_dim1] * ar[md + k * ar_dim1] + ai[md + k * 
		  ai_dim1] * ai[md + k * ai_dim1];
	  prodr = br[k] * ar[md + k * ar_dim1] + bi[k] * ai[md + k * ai_dim1];
	  prodi = bi[k] * ar[md + k * ar_dim1] - br[k] * ai[md + k * ai_dim1];
	  br[k] = prodr / den;
	  bi[k] = prodi / den;
	  tr = -br[k];
	  ti = -bi[k];
	  kmd = md - k;
  /* Computing MAX */
	  i__2 = 1, i__3 = kmd + 1;
	  lm = max(i__2,i__3);
	  i__2 = mdm;
	  for (i__ = lm; i__ <= i__2; ++i__) {
	      imd = i__ - kmd;
	      prodr = ar[i__ + k * ar_dim1] * tr - ai[i__ + k * ai_dim1] * ti;
	      prodi = ai[i__ + k * ai_dim1] * tr + ar[i__ + k * ar_dim1] * ti;
	      br[imd] += prodr;
	      bi[imd] += prodi;
  /* L30: */
	  }
  /* L40: */
      }
      den = ar[md + ar_dim1] * ar[md + ar_dim1] + ai[md + ai_dim1] * ai[md + 
	      ai_dim1];
      prodr = br[1] * ar[md + ar_dim1] + bi[1] * ai[md + ai_dim1];
      prodi = bi[1] * ar[md + ar_dim1] - br[1] * ai[md + ai_dim1];
      br[1] = prodr / den;
      bi[1] = prodi / den;
  L50:
      return 0;
  /* ----------------------- END OF SUBROUTINE SOLBC ------------------------ */
  } /* radau_solbc */



  int radau_elmhes(int32_t *nm, int32_t *n, int32_t *low, int32_t *igh, double *a, int32_t *int__)
  {
      /* System generated locals */
      int32_t a_dim1, a_offset, i__1, i__2, i__3;
      double d__1;

      /* Local variables */
      static int32_t i__, j, m;
      static double x, y;
      static int32_t la, mm1, kp1, mp1;



  /*     this subroutine is a translation of the algol procedure elmhes, */
  /*     num. math. 12, 349-368(1968) by martin and wilkinson. */
  /*     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971). */

  /*     given a real general matrix, this subroutine */
  /*     reduces a submatrix situated in rows and columns */
  /*     low through igh to upper hessenberg form by */
  /*     stabilized elementary similarity transformations. */

  /*     on input: */

  /*      nm must be set to the row dimension of two-dimensional */
  /*        array parameters as declared in the calling program */
  /*        dimension statement; */

  /*      n is the order of the matrix; */

  /*      low and igh are int32_ts determined by the balancing */
  /*        subroutine  balanc.      if  balanc  has not been used, */
  /*        set low=1, igh=n; */

  /*      a contains the input matrix. */

  /*     on output: */

  /*      a contains the hessenberg matrix.  the multipliers */
  /*        which were used in the reduction are stored in the */
  /*        remaining triangle under the hessenberg matrix; */

  /*      int contains information on the rows and columns */
  /*        interchanged in the reduction. */
  /*        only elements low through igh are used. */

  /*     questions and comments should be directed to b. s. garbow, */
  /*     applied mathematics division, argonne national laboratory */

  /*     ------------------------------------------------------------------ */

      /* Parameter adjustments */
      a_dim1 = *nm;
      a_offset = 1 + a_dim1;
      a -= a_offset;
      --int__;

      /* Function Body */
      la = *igh - 1;
      kp1 = *low + 1;
      if (la < kp1) {
	  goto L200;
      }

      i__1 = la;
      for (m = kp1; m <= i__1; ++m) {
	  mm1 = m - 1;
	  x = 0.;
	  i__ = m;

	  i__2 = *igh;
	  for (j = m; j <= i__2; ++j) {
	      if ((d__1 = a[j + mm1 * a_dim1], abs(d__1)) <= abs(x)) {
		  goto L100;
	      }
	      x = a[j + mm1 * a_dim1];
	      i__ = j;
  L100:
	      ;
	  }

	  int__[m] = i__;
	  if (i__ == m) {
	      goto L130;
	  }
  /*    :::::::::: interchange rows and columns of a :::::::::: */
	  i__2 = *n;
	  for (j = mm1; j <= i__2; ++j) {
	      y = a[i__ + j * a_dim1];
	      a[i__ + j * a_dim1] = a[m + j * a_dim1];
	      a[m + j * a_dim1] = y;
  /* L110: */
	  }

	  i__2 = *igh;
	  for (j = 1; j <= i__2; ++j) {
	      y = a[j + i__ * a_dim1];
	      a[j + i__ * a_dim1] = a[j + m * a_dim1];
	      a[j + m * a_dim1] = y;
  /* L120: */
	  }
  /*    :::::::::: end interchange :::::::::: */
  L130:
	  if (x == 0.) {
	      goto L180;
	  }
	  mp1 = m + 1;

	  i__2 = *igh;
	  for (i__ = mp1; i__ <= i__2; ++i__) {
	      y = a[i__ + mm1 * a_dim1];
	      if (y == 0.) {
		  goto L160;
	      }
	      y /= x;
	      a[i__ + mm1 * a_dim1] = y;

	      i__3 = *n;
	      for (j = m; j <= i__3; ++j) {
  /* L140: */
		  a[i__ + j * a_dim1] -= y * a[m + j * a_dim1];
	      }

	      i__3 = *igh;
	      for (j = 1; j <= i__3; ++j) {
  /* L150: */
		  a[j + m * a_dim1] += y * a[j + i__ * a_dim1];
	      }

  L160:
	      ;
	  }

  L180:
	  ;
      }

  L200:
      return 0;
  /*    :::::::::: last card of elmhes :::::::::: */
  } /* radau_elmhes */

} //namespace slf
