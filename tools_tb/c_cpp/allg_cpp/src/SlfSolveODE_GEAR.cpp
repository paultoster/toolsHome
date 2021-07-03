#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SlfSolveODE_GEAR.h"    

namespace slf
{


  //=================================================
  // Init-Funktion
  //=================================================
  okay_t CSolveODE_GEAR::init(CIntegratorGEARInp *pinp)
  {
    size_t i;

    //=================
    //Anzahl Zustände N
    //=================
    if (pinp->n >= MAX_UINT16) 
    {
      CStr errtext;
      errtext.catFormat("Anzahl der Zustände zu groß, maximal %i möglich", MAX_UINT16 - 1);
      mMessage.SetErr(NMAX_REACHED, errtext.c_str());
      return mMessage.IsOkay();
    }
    else if (pinp->n == 0) 
    {
      mMessage.SetErr(N_IS_ZERO, "Anzahl der Zustände n gleich null");
      return mMessage.IsOkay();
    }
    else 
    {
      N = (size_t)pinp->n;
    }

    // Funktionspointer
    //=================
    if (pinp->pobj == 0) {
      mMessage.SetErr(FUNCTION_POINTER_NOT_SET, "Funktionspointer inp.state == 0\n");
      return mMessage.IsOkay();
    }
    PObj = pinp->pobj;                  //


    //===============
    // Zustandsvektor
    //===============
    Y.resize(N);
    Y.FillZero();

    //===========================================
    // Toleranz (Es wird nur die erste verwendet)
    //===========================================
    VAtol.resize(N);

    if (pinp->itol == 0) 
    {//skalar
      for (i = 0; i < N; i++) 
      {
        VAtol[i] = fabs(pinp->atol);
        if (VAtol[i] == 0.0) {
          mMessage.SetErr(VALUE_IS_ZERO, "Skalar absolute Error must be set != 0.0 \n");
          return mMessage.IsOkay();
        }
      }

    }
    else  // Vektor
    {
      Itoler = 1;
      if (pinp->vatol.size() == 0)
      {
        mMessage.SetErr(VECTOR_IS_ZERO, "Vektor absolute Error must be set\n");
        return mMessage.IsOkay();
      }
      VAtol.Copy(pinp->vatol);
      VAtol.MakeAbs();

      if (VAtol.size() < N) 
      {
        CStr errtext;
        errtext.catFormat("Vektor length:%i absolute Error must be equal N:%i\n"
          , VAtol.size(), N);
        mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
        return mMessage.IsOkay();
      }

      for (i = 0; i < N; i++) 
      {
        if (VAtol[i] == 0.0) 
        {
          CStr errtext;
          errtext.catFormat("CVectorD %i absolute Error must be set != 0.0 \n", i);
          mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
        }
      }
    }

    /*===================*/
    /* maximal step size */
    /*===================*/
    Hmax = pinp->hmax;
    Hmax = fabs (Hmax);

    if (Hmax <= Uround) 
    {
      Hmax = 1e30; // wird später mit einer Minfkt auf xend-x gekürzt
      //Status = NOT_OKAY;
      //ErrText.catFormat("CSolveODE_RAD::init: Hmax <= Uround \n");
      //return Status;
    }

    //=================================
    // meth, methode
    //=================================
    Meth = SLF_MAX(1, SLF_MIN(4, pinp->meth)); // 1: teilimplEuler, 2:teilimplizitGear, 3:implizitEuler, 3:implizitGear

    //=================================
    // Wird Jacobimatrix berechnet
    //=================================
    Ijac = pinp->ijac;


    //====================
    // maximale Iteration 
    //====================
    Nit = (int32_t)pinp->nit;


    if (Meth < 3) Nit = 1;

    if (Nit == 0) 
    {
      mMessage.SetErr(UNKOWN_ERR, "CSolveODE_GEAR::init: Nit == 0 ");
      return mMessage.IsOkay();
    }

    //===========================================================
    // nach wieviel Berechnung soll Jacobimatrix berechnet werden
    //===========================================================
    Njacmax = (int32_t)pinp->njacmax;

    if (Njacmax == 0) 
    {
      mMessage.SetErr(UNKOWN_ERR, "CSolveODE_GEAR::init: Njacmax == 0 \n");
      return mMessage.IsOkay();
    }
    
    // 1: teilimplEuler, 2:teilimplizitGear, 3:implizitEuler, 3:implizitGear
    if (Meth == 1)
      Meth = 3;
    else if (Meth == 2)
      Meth = 4;

    // Faktor zur Berechnung Inverse
    if (Meth == 3) // Euler
      Afac = 1.;
    else // Gear
      Afac = 6. / 11.;

    // Matrix für die Berechnung in den Funktionen
    FjacMat.SetNdim(N,N); FjacMat.FillZero();
    VStam1.resize(N); VStam1.FillZero();
    VStam2.resize(N); VStam2.FillZero();
    VSta0.resize(N); VSta0.FillZero();
    VStaDer.resize(N); VStaDer.FillZero();
    VStaDer0.resize(N); VStaDer0.FillZero();

    Delta.resize(N); Delta.FillZero();
    Vec.resize(N); Vec.FillZero();
    Vec1.resize(N); Vec1.FillZero();

    InvMat.SetNdim(N, N); InvMat.FillZero();
    InvMatOld.SetNdim(N, N); InvMatOld.FillZero();

    B.resize(N); B.FillZero();

    return mMessage.IsOkay();
  }
  okay_t CSolveODE_GEAR::calcFirst(const double &x
                                  , const CVectorD &ystart
                                  , const double &hstart
                                  , double posneg/*=1*/
                                  ) 
  {
    size_t i;
    okay_t ret_ok = OKAY;

    // Startpunkt
    //===========
    X = x;
    H = fabs(hstart);
    Nsteps = 0;

    // in welche Richtung rechnen wir (zeitlich positiv)
    //==================================================
    Posneg = SLF_SIGN(posneg,double);
    H *= Posneg;


    // Initialisierungswerte
    //======================
    if (ystart.size() != N) 
    {
      CStr errtext;
      errtext.catFormat("Initialisierungsvektor ystart(n) = %i, entspricht nicht vorgegebenen Statelänge N = %i\n", ystart.size(), N);
      mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
      return mMessage.IsOkay();

    }
    for (i = 0; i < N; i++)
      Y[i] = ystart[i];


    return mMessage.IsOkay();
  }
  okay_t CSolveODE_GEAR::calc(double xend) {

    okay_t ret_ok;

    uint8_t  iterflag;
    size_t nit;
    size_t njac;
    uint8_t  runflag = 1;
    size_t j;

    Hact = SLF_MIN(SLF_ABS(Hmax,double), SLF_ABS((xend - X),double));
    Hact = SLF_MIN(Hact, H);

    Njac = 0;
    Nfcn = 0;
    Nstep = 0;


    do {

      iterflag = 1;
      njac = Njacmax + 1;
      nit = 0;

      // Zeit setzen
      //============
      if ((xend - X - Hact)*Posneg < Uround) {
        Hact = xend - X;
        X = xend;
        runflag = 0;
      }
      else {
        X += Hact;
      }

      // alten Zustand merken
      //=====================
      PutVectorToVectorD(VStam1, VStam2, 0);
      PutVectorToVectorD(VSta0, VStam1, 0);
      PutVectorToVectorD(Y, VSta0, 0);
      PutVectorToVectorD(VStaDer, VStaDer0, 0);


      // Erste Schätzung: Verfahren nach Heun
      //=====================================
      // x(t+h) = x(t)+h*f(t,x(t)
      ScalarTimesVectorD(Hact, VStaDer0, B);
      VectorAddD(B, VSta0, Y);

      // State-Funktions-Aufruf
      // f(t+h,x(t)+h*f(t,x(t)) bilden
      //-----------------------
      ret_ok = PObj->StateDer(X, Y, VStaDer);
      Nfcn++;
      if (ret_ok != OKAY) {
        CStr errtext;
        errtext.catFormat("Error CSolveODE_GEAR::calc: during state-functioncall of <%s>"
          , PObj->GetName());
        mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
        const CMessage *message = PObj->GetMessagePointer();
        mMessage.Collect(message);

        return mMessage.IsOkay();
      }

      // PHI = 0.5*h*(f(t,x(t))+f(t+h,x(t)+h*f(t,x(t)))
      VectorAddD(VStaDer0, VStaDer, B);
      ScalarTimesVectorD(0.5*Hact, B, B);

      // x(t+h) = x(t) + PHI
      VectorAddD(B, VSta0, Y);


      while (iterflag && nit < Nit) // Newton iteration
      {
        Nstep++;
        nit++;
        njac++;

        if (njac > Njacmax) 
        {
          njac = 0;
          if (calcInverseMaterix() != OKAY)
          {
            return mMessage.IsOkay();
          }
        }

        // B=a*f + ...
        //=============
        ScalarTimesVectorD(Hact*Afac, VStaDer, B);

        if (Meth == 4) 
        {
          // ... + [2/11*x(t-2h)-9/11*x(t-h)+7/11*x(t)]
          //===================================================
          ScalarTimesVectorD(2. / 11., VStam2, Vec);
          VectorAddD(B, Vec, B);

          ScalarTimesVectorD(9. / 11., VStam1, Vec);
          VectorSubD(B, Vec, B);

          ScalarTimesVectorD(7. / 11., VSta0, Vec);
          VectorAddD(B, Vec, B);

        }

        // delta = [E-h*J]^-1*(a*f -xn(t+h) +x(t))
        //========================================
        VectorAddD(B, VSta0, B);
        VectorSubD(B, Y, B);

        MatrixTimesVectorD(InvMat, B, Delta);


        // neuer Wert xn+1(t+h) = xn(t+h) + delta
    //=======================================
        VectorAddD(Y, Delta, Y);

        iterflag = 0;
        for (j = 0; j < N; j++) {

          if (fabs(Delta[j]) > VAtol[j]) {

            iterflag = 1;
            break;
          }
        }

        // Ableitung bilden
        //=================
        // f(t+h,x(t+h)) bilden
        //-----------------------
        ret_ok = PObj->StateDer(X, Y, VStaDer);
        Nfcn++;
        if (ret_ok != OKAY) {
          CStr errtext;
          errtext.catFormat("Error CSolveODE_GEAR::calc: during state-functioncall of <%s>"
            , PObj->GetName());
          mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
          const CMessage *message = PObj->GetMessagePointer();
          mMessage.Collect(message);
          return mMessage.IsOkay();
        }

      }

    } while (runflag);

    return mMessage.IsOkay();
  }
  okay_t CSolveODE_GEAR::calcInverseMaterix(void) {

    okay_t ret_ok;
    size_t  j;
    double  det;

    if (Ijac)
    {
      // Jacobi-Aufruf
      //==============
      ret_ok = PObj->Jacobian(X, Y, FjacMat, N);
      Njac++;
      if (ret_ok != OKAY) 
      {
        CStr errtext;
        errtext.catFormat("Error CSolveODE_Gear::dprja: during jacobi-functioncall of <%s>"
          , PObj->GetName());
        mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
        const CMessage *message = PObj->GetMessagePointer();
        mMessage.Collect(message);

        return mMessage.IsOkay();
      }
    } else { // Ijac == 0
      /* --- JACOBIAN IS FULL */

      VStaDer0 = VStaDer;

      double ysafe,delt;
      for (std::size_t i = 0; i < N; ++i)
      {
        ysafe = Y[i];
        /* Computing MAX */
        delt = sqrt(Uround * max(1e-5, abs(ysafe)));
        Y[i] = ysafe + delt;

        // State-Funktions-Aufruf
        //=======================
        ret_ok = PObj->StateDer(X, Y, VStaDer);
        if (ret_ok != OKAY) {
          CStr errtext;
          errtext.catFormat("Error CSolveODE_GEAR::calc: buildng jacobian during state-functioncall of <%s>"
            , PObj->GetName());
          mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
          const CMessage *message = PObj->GetMessagePointer();
          mMessage.Collect(message);

          return mMessage.IsOkay();
        }

        //#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        //        ret_ok = pclass->state(*x, &y[1], &cont[1]);
        //#else
        //        ret_ok = state(*n, *x, &y[1], &cont[1]);
        //#endif
        //        if (ret_ok != OKAY) {
        //          *idid = -5;
        //#if SLF_SOLVER_ODE_USE_DSMODBASE == 1
        //          errtext.catFormat("Error CSolveODE_RAD::radau_core: during state-functioncall of <%s> \n%s\n"
        //            , pclass->getName()
        //            , pclass->getErrText()
        //          );
        //#else
        //          errtext.cat("Error CSolveODE_RAD::radau_core: error during state-functioncall\n");
        //
        //#endif
                //  return 0;
                //}

                //(*fcn)(n, x, &y[1], &cont[1], &rpar[1], &ipar[1]);
        for (std::size_t j = 0; j < N; ++j)
        {
          FjacMat[j][i] = (VStaDer[j] - VStaDer0[j]) / delt;
        }
        Y[i] = ysafe;
      }
    }

    // Jacobi * (-h*a)
    ScalarTimesMatrixD(Hact*Afac*(-1.), FjacMat, FjacMat);

    // + E
    for (j = 0; j < N; j++)
      FjacMat[j][j] += 1.0;

    det = MatrixInvertD(FjacMat, InvMat);


    if (fabs(det) < SLF_EPSILON) {

      PutMatrixToMatrixD(InvMatOld, InvMat, N, N);
      CStr errtext;
      errtext.catFormat("Error CSolveODE_GEAR::calcInverseMaterix: In Modell <%s> ist (E-h*Jacobi)^-1 singulär !!!!\n"
        , PObj->GetName());
      mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
      
    }
    else 
    {
      PutMatrixToMatrixD(InvMat, InvMatOld, N, N);
    }


    return mMessage.IsOkay();
  }
}