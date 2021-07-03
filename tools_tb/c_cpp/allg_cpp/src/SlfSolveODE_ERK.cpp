#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SlfSolveODE_ERK.h"

namespace slf
{

  okay_t CSolveODE_ERK::init(CIntegratorERKInp &inp) {

    //Anzahl Zustände N
    //=================
    if (inp.n >= MAX_UINT16) {

      CStr errtext;
      errtext.catFormat("Anzahl der Zustände zu groß, maximal %i möglich", MAX_UINT16 - 1);
      mMessage.SetErr(NMAX_REACHED, errtext.c_str());
      return mMessage.IsOkay();
    }
    else if (inp.n == 0) {

      mMessage.SetErr(N_IS_ZERO, "Anzahl der Zustände n gleich null");
      return mMessage.IsOkay();
    }
    else {

      N = inp.n;
    }

    // Funktionspointer
    //=================
    if (inp.pobj == 0) {
      mMessage.SetErr(FUNCTION_POINTER_NOT_SET, "Funktionspointer inp.state == 0\n");
      return mMessage.IsOkay();
    }
    PObj = inp.pobj;                  //

    // Zustandsvektor
    //===============
    Y.resize(N);
    Y.FillZero();

    // Toleranz 
    //==========
    if (inp.itol == 0) {//skalar

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

    }
    else  // Vektor
    {
      uint16_t i;

      Itoler = 1;
      if( inp.vatol.size() == 0 ) 
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

      VAtol.MakeAbs();
      VRtol.MakeAbs();

      if (VAtol.size() < N) {
        CStr errtext;
        errtext.catFormat("Vektor length:%i absolute Error must be equal N:%i\n"
          , VAtol.size(), N);
        mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
        return mMessage.IsOkay();
      }

      for (i = 0; i < N; i++) {

        if (VAtol[i] == 0.0) {
          CStr errtext;
          errtext.catFormat("CVectorD %i absolute Error must be set != 0.0 \n", i);
          mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
        }
      }
      if (VRtol.size() < N) {
        CStr errtext;
        errtext.catFormat("Vektor length:%i relative Error must be equal N:%i\n"
          , VAtol.size(), N);
        mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
        return mMessage.IsOkay();
      }
      for (i = 0; i < N; i++) {

        if (VRtol[i] == 0.0) {
          CStr errtext;
          errtext.catFormat("Vector %i relative Error must be set != 0.0 \n", i);
          mMessage.SetErr(VECTOR_TO_SMALL, errtext.c_str());
          return mMessage.IsOkay();
        }
      }

    }

    /* uround, smallest number satisfying 1.0+uround > 1.0 */
    /*=====================================================*/
    Uround = inp.uround;

    if ((Uround <= 1.0E-35) )
    {
      CStr errtext;
      errtext.catFormat("Which machine do you have ? Your uround was : %.16e\r\n", Uround);
      mMessage.SetErr(VALUE_TO_SMALL, errtext.c_str());
      return mMessage.IsOkay();
    }
    if ((Uround >= 1.0))
    {
      CStr errtext;
      errtext.catFormat("uround too big : %.16e\r\n", Uround);
      mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
      return mMessage.IsOkay();
    }

    /* maximal step size */
    /*===================*/
    Hmax = inp.hmax;
    Hmax = fabs (Hmax);

    if (Hmax <= Uround) {
      Hmax = 1e30; // wird später mit einer Minfkt auf xend-x gekürzt
      //Status = NOT_OKAY;
      //ErrText.catFormat("CSolveODE_RAD::init: Hmax <= Uround \n");
      //return Status;
    }

    // maximale Iteration
    //===================
    Nmax = inp.nmax;


    // meth, coefficients of the method
    //=================================
    Meth = SLF_MIN(1, SLF_MAX(1, inp.meth)); // im Moment nur eine Methode

    // nstiff, parameter for stiffness detection
    //========================================== 
    Nstiff = inp.nstiff;

    if (Nstiff < 0)
      Nstiff = (int32_t)Nmax + 10;

    // safety factor
    //==============
    Safe = inp.safe;

    if ((Safe >= 1.0) || (Safe <= 1.0E-4))
    {
      CStr errtext;
      errtext.catFormat("safe muß zwischen 1.0E-4 und 1.0 liegen safe = %g", Safe);
      mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
      return mMessage.IsOkay();
    }

    /* fac1, fac2, parameters for step size selection */
    /*================================================*/
    Fac1 = inp.fac1;
    Fac2 = inp.fac2;

    Facold = 1.0E-4;
    Facc1 = 1.0 / Fac1;
    Facc2 = 1.0 / Fac2;

    /* beta for step control stabilization */
    if (inp.beta != 0.0)
      Beta = inp.beta;
    if (Beta < 0.0)
      Beta = 0.0;
    else if (Beta > 0.2)
    {
      CStr errtext;
      errtext.catFormat("Curious input for beta : beta = %.16e\r\n", inp.beta);
      mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
      return mMessage.IsOkay();
    }
    Expo1 = 0.2 - Beta * 0.75;

    /* Koeffizienten festlegen */
    /*=========================*/
    switch (Meth)
    {
    case 1: // dormand-prince 4(5)

      Nstufen = 7;
      Iord = 5;

      C.resize(Nstufen);
      A.SetNdim(Nstufen, Nstufen);
      E.resize(Nstufen);
      D.resize(Nstufen);

      C[0] = 0.0;
      C[1] = 0.2;
      C[2] = 0.3;
      C[3] = 0.8;
      C[4] = 8.0 / 9.0;
      C[5] = 1.0;
      C[6] = 1.0;

      A[0][0] = 0.0;
      A[1][0] = 0.2;
      A[2][0] = 3.0 / 40.0;
      A[3][0] = 44.0 / 45.0;
      A[4][0] = 19372.0 / 6561.0;
      A[5][0] = 9017.0 / 3168.0;
      A[6][0] = 35.0 / 384.0;

      A[0][1] = A[1][1] = 0.0;
      A[2][1] = 9.0 / 40.0;
      A[3][1] = -56.0 / 15.0;
      A[4][1] = -25360.0 / 2187.0;
      A[5][1] = -355.0 / 33.0;
      A[6][1] = 0.0;

      A[0][2] = A[1][2] = A[2][2] = 0.0;
      A[3][2] = 32.0 / 9.0;
      A[4][2] = 64448.0 / 6561.0;
      A[5][2] = 46732.0 / 5247.0;
      A[6][2] = 500.0 / 1113.0;

      A[0][3] = A[1][3] = A[2][3] = A[3][3] = 0.0;
      A[4][3] = -212.0 / 729.0;
      A[5][3] = 49.0 / 176.0;
      A[6][3] = 125.0 / 192.0;

      A[0][4] = A[1][4] = A[2][4] = A[3][4] = A[4][4] = 0.0;
      A[5][4] = -5103.0 / 18656.0;
      A[6][4] = -2187.0 / 6784.0;

      A[0][5] = A[1][5] = A[2][5] = A[3][5] = A[4][5] = A[5][5] = 0.0;
      A[6][5] = 11.0 / 84.0;

      E[0] = 71.0 / 57600.0;
      E[1] = 0.0;
      E[2] = -71.0 / 16695.0;
      E[3] = 71.0 / 1920.0;
      E[4] = -17253.0 / 339200.0;
      E[5] = 22.0 / 525.0;
      E[6] = -1.0 / 40.0;

      D[0] = -12715105075.0 / 11282082432.0;
      D[1] = 0.0;
      D[2] = 87487479700.0 / 32700410799.0;
      D[3] = -10690763975.0 / 1880347072.0;
      D[4] = 701980252875.0 / 199316789632.0;
      D[5] = -1453857185.0 / 822651844.0;
      D[6] = 69997945.0 / 29380423.0;

      break;
    }

    //=================
    // Speicher anlegen
    //=================
    if (Nstufen > INTEGRATOR_KMAX) {
      CStr errtext;
      errtext.catFormat("level: %i too big, increase define Integrator_max\n", Nstufen);
      mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
      return mMessage.IsOkay();
    }


    // Hilfsvektoren, K-Faktoren
    //==========================
    YY2.resize(N);
    for (uint8_t i = 0; i < Nstufen; i++) {

      K[i].resize(N);
      YY1[i].resize(N);
      K[i].FillZero();
      YY1[i].FillZero();
    }
    YY2.FillZero();

    return mMessage.IsOkay();

  }
  okay_t CSolveODE_ERK::calcFirst(const double &x, const CVectorD &ystart,const double &hstart, double posneg/*=1*/) 
  {
    uint32_t i;
    okay_t ret_ok = OKAY;

    // Startpunkt
    //===========
    X = x;

    // in welche Richtung rechnen wir (zeitlich positiv)
    //==================================================
    Posneg = SLF_SIGN(posneg,double);


    // Initialisierungswerte
    //======================
    if (ystart.size() != N) {
      CStr errtext;
      errtext.catFormat("Initialisierungsvektor ystart(n) = %i, entspricht nicht vorgegebenen Statelänge N = %i\n", ystart.size(), N);
      mMessage.SetErr(VALUE_TO_BIG, errtext.c_str());
      return mMessage.IsOkay();
    }
    for (i = 0; i < N; i++)
      Y[i] = ystart[i];

    //erste Schrittweite
    //==================
    if (hstart == 0.0) {
      H = hinit(X, Y, Posneg);
      if (mMessage.IsOkay() != OKAY)
        return mMessage.IsOkay();
    }
    else {
      H = fabs(hstart) * SLF_SIGN(Posneg,double);
    }

    // der erste Runge-Kutta-Faktor
    //=============================
    ret_ok = PObj->StateDer(X, Y, K[0]);

    ++Nfcn;

    if (ret_ok != OKAY) 
    {
      CStr errtext;
      errtext.cat("Error CSolveODE_ERK::Error in StateDer()\n");
      errtext.cat(PObj->GetErrText());
      mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
      return mMessage.IsOkay();
    }

    return mMessage.IsOkay();
  }

  okay_t CSolveODE_ERK::calc(double &xend) 
  {
    double hmaxn = SLF_MIN(SLF_ABS(Hmax,double), SLF_ABS((xend - X),double));
    uint8_t  last = 0;
    double hlamb = 0.0;
    uint16_t iasti = 0;
    uint16_t nonsti = 0;
    uint8_t  reject = 0;
    uint32_t nrejct = 0;           // Anzahl verworfener Steps
    double sk, sqr, err, fac11, fac;
    double hnew, stnum, stden;
    uint32_t i, j, k;
    okay_t ret_ok = OKAY;

    mMessage.Clear();

    Nstep = 0;
    NstepAccept = 0;
    Nfcn = 0;

    /* basic integration step */
    //==========================
    while (1)
    {

      // maximale Anzahl steps
      if (Nstep > Nmax)
      {
        CStr errtext;
        errtext.catFormat("Exit at X = %20.16e, h = %20.16e more than nmax = %li are needed\r\n", X, H, Nmax);
        mMessage.SetErr(NMAX_REACHED, errtext.c_str());
        return mMessage.IsOkay();
      }

      // zu kleine Schrittweite
      if (0.1 * fabs(H) <= fabs(X) * Uround)
      {
        CStr errtext;
        errtext.catFormat("Exit at X = %20.16e, step size too small H = %20.16e\r\n", X, H);
        mMessage.SetErr(STEPSIZE_TOO_SMALL, errtext.c_str());
        return mMessage.IsOkay();
      }

      // letzter Zeitschritt
      if ((X + 1.01*H - xend) * Posneg > 0.0)
      {
        H = xend - X;
        last = 1;
      }

      Nstep++;

      /* the first 6 stages */
      for (j = 1; j < Nstufen; j++) 
      {
        for (i = 0; i < N; i++)  
        {
          YY1[j][i] = 0.0;
          for (k = 0; k < j; k++)
            YY1[j][i] += A[j][k] * K[k][i];
          YY1[j][i] *= H;
          YY1[j][i] += Y[i];
        }

        ret_ok = PObj->StateDer( X + C[j] * H, YY1[j], K[j]);
        if (ret_ok != OKAY)
        {
          CStr errtext;
          errtext.cat("Error CSolveODE_ERK::hinit: error during state-functioncall\n");
          mMessage.SetErr(FUNCTION_ERR, errtext.c_str());
          return mMessage.IsOkay();
      }

        ++Nfcn;
      }

      for (i = 0; i < N; i++) 
      {
        YY2[i] = 0.0;
        for (j = 0; j < Nstufen; j++)
          YY2[i] += E[j] * K[j][i];
        YY2[i] *= H;
      }
#if 0
      if (iout == 2)
        if (nrds == n)
          for (i = 0; i < n; i++)
          {
            rcont5[i] = H * (d1*k1[i] + d3*k3[i] + d4*k4[i] + d5*k5[i] + d6*k6[i] + d7*k2[i]);
          }
        else
          for (j = 0; j < nrds; j++)
          {
            i = icont[j];
            rcont5[j] = H * (d1*k1[i] + d3*k3[i] + d4*k4[i] + d5*k5[i] + d6*k6[i] + d7*k2[i]);
          }
#endif

      /* error estimation */
      err = 0.0;
      if (!Itoler) {
        for (i = 0; i < N; i++) {
          sk = VAtol[0] + VRtol[0] * SLF_MAX(fabs(Y[i]), fabs(YY1[Nstufen - 1][i]));
          sqr = YY2[i] / sk;
          err += sqr*sqr;
        }
      }
      else {
        for (i = 0; i < N; i++) {
          sk = VAtol[i] + VRtol[i] * SLF_MAX(fabs(Y[i]), fabs(YY1[Nstufen - 1][i]));
          sqr = YY2[i] / sk;
          err += sqr*sqr;
        }
      }
      err = sqrt (err / (double)N);

      /* computation of hnew */
      fac11 = pow (err, Expo1);
      /* Lund-stabilization */
      fac = fac11 / pow(Facold, Beta);
      /* we require fac1 <= hnew/H <= fac2 */
      fac = SLF_MAX(Facc2, SLF_MIN(Facc1, fac / Safe));
      hnew = H / fac;

      if (err <= 1.0)
      {
        /* step accepted */
        Facold = SLF_MAX(err, 1.0E-4);
        NstepAccept++;

        /* stiffness detection */
        if (!(NstepAccept % Nstiff) || (iasti > 0)) {

          stnum = 0.0;
          stden = 0.0;
          for (i = 0; i < N; i++)
          {
            sqr = K[Nstufen - 1][i] - K[Nstufen - 2][i];
            stnum += sqr*sqr;
            sqr = YY1[Nstufen - 1][i] - YY1[Nstufen - 2][i];
            stden += sqr*sqr;
          }
          if (stden > 0.0)
            hlamb = H * sqrt (stnum / stden);
          if (hlamb > 3.25)
          {
            nonsti = 0;
            iasti++;
            if (iasti == 15)
            {
              CStr text;
              text.catFormat("The problem seems to become stiff at X = %20.16e\r\n", X);
              mMessage.SetLog(text.c_str());
            }
          }
          else
          {
            nonsti++;
            if (nonsti == 6)
              iasti = 0;
          }
        }
#if 0
        if (iout == 2) {
          if (nrds == n) {
            for (i = 0; i < n; i++) {
              yd0 = y[i];
              ydiff = yy1[i] - yd0;
              bspl = H * k1[i] - ydiff;
              rcont1[i] = y[i];
              rcont2[i] = ydiff;
              rcont3[i] = bspl;
              rcont4[i] = -H * k2[i] + ydiff - bspl;
            }
          }
          else {
            for (j = 0; j < nrds; j++) {
              i = icont[j];
              yd0 = y[i];
              ydiff = yy1[i] - yd0;
              bspl = H * k1[i] - ydiff;
              rcont1[j] = y[i];
              rcont2[j] = ydiff;
              rcont3[j] = bspl;
              rcont4[j] = -H * k2[i] + ydiff - bspl;
            }
          }
        }
#endif
        // nächster Zeitschritt
        //=====================
        K[0].Copy(K[Nstufen - 1]);
        Y.Copy(YY1[Nstufen - 1]);

        X += H;
#if 0
        if (iout)
        {
          hout = H;
          xout = X;
          solout (naccpt + 1, xold, X, y, n, &irtrn);
          if (irtrn < 0)
          {
            if (fileout)
              fprintf (fileout, "Exit of dopri5 at X = %.16e\r\n", X);
            return 2;
          }
        }
#endif
        /* normal exit */
        if (last)
        {
          H = hnew;
          if (NstepMax < Nstep) {

            NstepMax = Nstep;

          }
          return mMessage.IsOkay();
        }

        if (fabs(hnew) > hmaxn)
          hnew = Posneg * hmaxn;
        if (reject)
          hnew = Posneg * SLF_MIN(fabs(hnew), fabs(H));

        reject = 0;
      }
      else
      {
        /* step rejected */
        hnew = H / SLF_MIN(Facc1, fac11 / Safe);
        reject = 1;
        if (NstepAccept >= 1)
          nrejct = nrejct + 1;
        last = 0;
      }

      H = hnew;
    }
    return mMessage.IsOkay();

  }
  double  CSolveODE_ERK::hinit(double &x, CVectorD &y, double &posneg) 
  {

    double   dnf, dny, sk, h, h1, der2, der12, sqr;
    CVectorD f0 = K[0];
    CVectorD f1 = K[1];
    CVectorD yy1 = K[2];
    unsigned i;
    okay_t  ret_ok = OKAY;

    dnf = 0.0;
    dny = 0.0;

    if (!Itoler) {
      for (i = 0; i < N; i++) {
        sk = VAtol[0] + VRtol[0] * fabs(y[i]);
        sqr = f0[i] / SLF_NOT_ZERO(sk,double);
        dnf += sqr*sqr;
        sqr = y[i] / sk;
        dny += sqr*sqr;
      }
    }
    else {
      for (i = 0; i < N; i++) {
        sk = VAtol[i] + VRtol[i] * fabs(y[i]);
        sqr = f0[i] / sk;
        dnf += sqr*sqr;
        sqr = y[i] / sk;
        dny += sqr*sqr;
      }
    }

    if ((dnf <= 1.0E-10) || (dny <= 1.0E-10))
      h = 1.0E-6;
    else
      h = sqrt (dny / dnf) * 0.01;

    h = SLF_MIN(h, Hmax);
    h = h*SLF_SIGN(posneg,double);

    /* perform an explicit Euler step */
    for (i = 0; i < N; i++)
      yy1[i] = y[i] + h * f0[i];
    ret_ok = PObj->StateDer( x + h, yy1, f1);

    ++Nfcn;
    if (ret_ok != OKAY) {
      CStr errtext;
      errtext.cat("Error CSolveODE_ERK::hinit: error during state-functioncall\n");
      mMessage.SetErr(FUNCTION_ERR, errtext.c_str());

      return 0.0;
    }

    /* estimate the second derivative of the solution */
    der2 = 0.0;
    if (!Itoler) {
      for (i = 0; i < N; i++)
      {
        sk = VAtol[0] + VRtol[0] * fabs(y[i]);
        sqr = (f1[i] - f0[i]) / sk;
        der2 += sqr*sqr;
      }
    }
    else {
      for (i = 0; i < N; i++) {
        sk = VAtol[i] + VRtol[i] * fabs(y[i]);
        sqr = (f1[i] - f0[i]) / sk;
        der2 += sqr*sqr;
      }
      der2 = sqrt (der2) / h;

      /* step size is computed such that h**iord * max_d(norm(f0),norm(der2)) = 0.01 */
      der12 = SLF_MAX(fabs(der2), sqrt(dnf));
      if (der12 <= 1.0E-15)
        h1 = SLF_MAX(1.0E-6, fabs(h)*1.0E-3);
      else
        h1 = pow(0.01 / der12, 1.0 / (double)Iord);
      h = SLF_MIN(100.0 * h, SLF_MIN(h1, Hmax));
    }
    return h*SLF_SIGN(posneg,double);
  }

}