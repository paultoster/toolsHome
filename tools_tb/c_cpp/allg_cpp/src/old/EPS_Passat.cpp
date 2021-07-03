///////////////////////////////////////////////////////////////////////////
// Project:
// Continental 2011
///////////////////////////////////////////////////////////////////////////


#include "EPS_Passat.h"
#include "string.h"

// special includes

//#include "Filter.h"
//#include "SigMan.h"


TEPS_Passat EPS_Passat;

static void EpsFuncCalc(TEpsFunc *pef);
static void EpsFuncInit(TEpsFunc *pef);
static void EpsMotorCalc(TEpsMotor *pef,double *pdt);
static void EpsMotorInit(TEpsMotor *pef);


int Init_EPS_Passat(double *pxRack,double *pxpRack,double *pxppRack)
{
  /*==========================================*/
  /* EPS Function Assist Torque Calculation   */
  /*==========================================*/

  /* Zahnstangenweg, Geschw., Beschl. */
  EPS_Passat.EpsMotor.Inp.pxSteRack   = pxRack;
  EPS_Passat.EpsMotor.Inp.pxpSteRack  = pxpRack;
  EPS_Passat.EpsMotor.Inp.pxppSteRack = pxppRack;

  /* Init */
  EpsFuncInit(&(EPS_Passat.EpsFunc));


  /* Inpupointer für EPsFunc calculated Steer wheel angle */
  EPS_Passat.EpsFunc.Inp.pphiSteColCalc = &(EPS_Passat.EpsMotor.Out.phiSteColCalc);

  /* Inpupointer für EPsFunc EPS Motor angualr speed */
  EPS_Passat.EpsFunc.Inp.phipSteEPS = &(EPS_Passat.EpsMotor.Out.phipSteEps);

  /* Inputpointer vehicle velocity */
  EPS_Passat.EpsFunc.Inp.pvVeh = &(EPS_Passat.vVeh);

  /* External Input */
  EPS_Passat.QP6D0.QP_Alpha_Beta_Interaction = EPS_FUNC_ALPHA_BETA_BEFOR;
  EPS_Passat.QP6D0.QP_AngelControl           = 0;
  EPS_Passat.QP6D0.QP_Weighting_Factor_alpha = EPS_FUNC_ALPHA_BETA_MAX;
  EPS_Passat.QP6D0.QP_Weighting_Factor_beta  = EPS_FUNC_ALPHA_BETA_MAX;

  /* Inputpointer external Input */
  EPS_Passat.EpsFunc.Inp.pQP6D0              = &(EPS_Passat.QP6D0);

  /*==========================================*/
  /* EPS Function Assist Torque Calculation   */
  /*==========================================*/

  /* Init */
  EpsMotorInit(&(EPS_Passat.EpsMotor));


  /* Inputpointer Reference Torque */
  EPS_Passat.EpsMotor.Inp.pTSteEpsRef = &(EPS_Passat.EpsFunc.Out.TSteEpsRef);

  return 0;
}

void EPS_Passat_Calc (double *pdt,double *pTSteColTwist,double *pvel,struct TQP6D0 *pqp6d0,double *passist_frc) 
{
  // Calculate PowerAssist EPS if Trq-Calc and function is set
  // and LoopTime is reached

  /* Torsion Bar Torque */
  EPS_Passat.EpsFunc.Inp.pTSteColTwist = pTSteColTwist;

  /* vehicle velocity */
  EPS_Passat.vVeh = *pvel;

  /* external Input EPS_Passat.QP6D0  */
  memcpy(&(EPS_Passat.QP6D0),pqp6d0,sizeof(struct TQP6D0));
      
  /*==========================================*/
  /* EPS Function Assist Torque Calculation   */
  /*==========================================*/
  EpsFuncCalc(&(EPS_Passat.EpsFunc));
  /*==========================================*/
  /* EPS Function Assist Torque Calculation   */
  /*==========================================*/
  EpsMotorCalc(&(EPS_Passat.EpsMotor),pdt);

  *passist_frc = EPS_Passat.EpsMotor.Out.FSteRackAssist;
      
}
#if 0
static void ContiSteer_Calc_Get_QP6D0(void)
{

#if CONTI_STEER_USE_CONTIGUARD == 1
  EPS_Passat.QP6D0.QP_Alpha_Beta_Interaction = (unsigned char)(CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Alpha_Beta_Interaction());
  EPS_Passat.QP6D0.QP_AngelControl           = (unsigned char)(CChnnls::Access().ManipChnnls().QP_6D0.GetQP_AngelControl());
  EPS_Passat.QP6D0.QP_Req_MotorTorque        = CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Req_MotorTorque();
  if( CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Req_MotorTorque_Sign() )
  {
    EPS_Passat.QP6D0.QP_Req_MotorTorque *= (-1.0f);
  }
  EPS_Passat.QP6D0.QP_Req_SteerAngle         = (unsigned char)(CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Req_SteerAngle());
  if( CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Req_SteerAngle_Sign() )
  {
    EPS_Passat.QP6D0.QP_Req_SteerAngle *= (-1.0f);
  }
  EPS_Passat.QP6D0.QP_Req_SteerTorque        = CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Req_SteerTorque();
  if( CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Req_SteerTorque_Sign() )
  {
    EPS_Passat.QP6D0.QP_Req_SteerTorque *= (-1.0f);
  }
  EPS_Passat.QP6D0.QP_ReqSteerControl        = (unsigned char)(CChnnls::Access().ManipChnnls().QP_6D0.GetQP_ReqSteerControl());
  EPS_Passat.QP6D0.QP_Weighting_Factor_alpha = (unsigned char)(CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Weighting_Factor_alpha());
  EPS_Passat.QP6D0.QP_Weighting_Factor_beta  = (unsigned char)(CChnnls::Access().ManipChnnls().QP_6D0.GetQP_Weighting_Factor_beta());
#else
  EPS_Passat.QP6D0.QP_Alpha_Beta_Interaction = Conti_Steer_In_EPS_Alpha_Beta_Interaction;
  EPS_Passat.QP6D0.QP_AngelControl           = Conti_Steer_In_EPS_AngelControl;
  EPS_Passat.QP6D0.QP_Req_MotorTorque        = Conti_Steer_In_EPS_Req_MotorTorque;
  EPS_Passat.QP6D0.QP_Req_SteerAngle         = Conti_Steer_In_EPS_Req_SteerAngle;
  EPS_Passat.QP6D0.QP_Req_SteerTorque        = Conti_Steer_In_EPS_Req_SteerTorque;
  EPS_Passat.QP6D0.QP_ReqSteerControl        = Conti_Steer_In_EPS_ReqSteerControl;
  EPS_Passat.QP6D0.QP_Weighting_Factor_alpha = Conti_Steer_In_EPS_Weighting_Factor_alpha;
  EPS_Passat.QP6D0.QP_Weighting_Factor_beta  = Conti_Steer_In_EPS_Weighting_Factor_beta;
#endif
}
#endif
static void EpsFuncInit(TEpsFunc *pef)
{

  /* Parameter definition */
  pef->Par.TSteColInpMax         = 8.0;

  pef->Par.phiSteEpsBasFadVec[0] =   0.0*EPS_FUNC_DEG2RAD;
  pef->Par.phiSteEpsBasFadVec[1] = 280.0*EPS_FUNC_DEG2RAD;
  pef->Par.phiSteEpsBasFadVec[2] = 380.0*EPS_FUNC_DEG2RAD;
  pef->Par.phiSteEpsBasFadVec[3] = 480.0*EPS_FUNC_DEG2RAD;
  pef->Par.phiSteEpsBasFadVec[4] = 600.0*EPS_FUNC_DEG2RAD;

  pef->Par.fSteEpsBasFadVec[0]   = 0.00;
  pef->Par.fSteEpsBasFadVec[1]   = 0.00;
  pef->Par.fSteEpsBasFadVec[2]   = 0.25;
  pef->Par.fSteEpsBasFadVec[3]   = 1.00;
  pef->Par.fSteEpsBasFadVec[4]   = 1.00;

  pef->Par.phiSteEpsBasFadMax    = 600.0 * EPS_FUNC_DEG2RAD;
  pef->Par.phiSteEpsBasFadShift  = 0.0;

  pef->Par.bSteEpsBasFadFilt1Vec[0] =  0.0;
  pef->Par.bSteEpsBasFadFilt1Vec[1] =  0.851882;
  pef->Par.bSteEpsBasFadFilt1Vec[2] = -1.657106;
  pef->Par.bSteEpsBasFadFilt1Vec[3] =  0.805811;

  pef->Par.aSteEpsBasFadFilt1Vec[0] =  1.0;
  pef->Par.aSteEpsBasFadFilt1Vec[1] = -2.309106;
  pef->Par.aSteEpsBasFadFilt1Vec[2] =  1.739250;
  pef->Par.aSteEpsBasFadFilt1Vec[3] = -0.429557;

  pef->Par.bSteEpsBasFadFilt2Vec[0] =  0.0;
  pef->Par.bSteEpsBasFadFilt2Vec[1] =  0.481687992170300;
  pef->Par.bSteEpsBasFadFilt2Vec[2] = -1.411902886284927;
  pef->Par.bSteEpsBasFadFilt2Vec[3] =  1.379496935298227;
  pef->Par.bSteEpsBasFadFilt2Vec[4] = -0.449276318134646;

  pef->Par.aSteEpsBasFadFilt2Vec[0] =  1.000000000000000;
  pef->Par.aSteEpsBasFadFilt2Vec[1] = -3.300142525496584;
  pef->Par.aSteEpsBasFadFilt2Vec[2] =  4.027662276603544;
  pef->Par.aSteEpsBasFadFilt2Vec[3] = -2.153222715043856;
  pef->Par.aSteEpsBasFadFilt2Vec[4] =  0.425708686985850;

  /* altenative Filter */
  pef->Par.useTSteColFadFiltAlt         = 1;
  pef->Par.T1SteColFadFiltAlt       = 0.03;

 
  pef->Par.TSteEpsBasInpVec[0]      = 0.0;
  pef->Par.TSteEpsBasInpVec[1]      = 0.2;
  pef->Par.TSteEpsBasInpVec[2]      = 0.8;
  pef->Par.TSteEpsBasInpVec[3]      = 1.6;
  pef->Par.TSteEpsBasInpVec[4]      = 2.6;
  pef->Par.TSteEpsBasInpVec[5]      = 3.6;
  pef->Par.TSteEpsBasInpVec[6]      = 4.6;
  pef->Par.TSteEpsBasInpVec[7]      = 5.6;
  pef->Par.TSteEpsBasInpVec[8]      = 6.6;
  pef->Par.TSteEpsBasInpVec[9]      = 8.0;

  pef->Par.vSteEpsBasVec[0]         =   0.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsBasVec[1]         =   5.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsBasVec[2]         =  10.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsBasVec[3]         =  20.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsBasVec[4]         =  50.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsBasVec[5]         = 100.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsBasVec[6]         = 250.0 * EPS_FUNC_KMPH2MPS;

  double TSteEpsBasAssistMap[EPS_FUNC_N_V_STE_EPS_BAS_ASSIST][EPS_FUNC_N_T_STE_EPS_BAS_ASSIST]=
  {{0.0, 0.120, 0.4800, 0.960, 1.5600, 2.4800, 4.1100, 4.800, 4.93, 5.0000}
  ,{0.0, 0.035, 0.3400, 0.715, 1.2600, 2.0350, 3.2250, 4.080, 4.69, 5.0000}
  ,{0.0, 0.010, 0.2000, 0.470, 0.9600, 1.5900, 2.3400, 3.360, 4.45, 5.0000}
  ,{0.0, 0.010, 0.1825, 0.430, 0.8675, 1.4275, 2.1125, 3.035, 4.05, 4.8375}
  ,{0.0, 0.010, 0.1300, 0.310, 0.5900, 0.9400, 1.4300, 2.060, 2.85, 4.3500}
  ,{0.0, 0.010, 0.0700, 0.180, 0.3400, 0.5300, 0.8000, 1.170, 1.70, 3.0600}
  ,{0.0, 0.010, 0.0400, 0.100, 0.1900, 0.3200, 0.5200, 0.800, 1.14, 2.4500}
  };
  memcpy(pef->Par.TSteEpsBasAssistMap,TSteEpsBasAssistMap,sizeof(TSteEpsBasAssistMap));

  pef->Par.TSteEpsRefMax = 5.0;
  pef->Par.fSteEpsBasTor = 0.80; /* aus messung 30 km/h, 1 Nm : 0.62 */

  pef->Par.dSteEpsAcdDamp = 0.1 / EPS_FUNC_DEG2RAD;
  pef->Par.aSteEpsAcd     = 4.0686e-4;

  pef->Par.vSteEpsAcdVec[0] =   0.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[1] =  20.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[2] =  40.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[3] =  80.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[4] = 100.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[5] = 120.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[6] = 160.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[7] = 200.0 * EPS_FUNC_KMPH2MPS;
  pef->Par.vSteEpsAcdVec[8] = 250.0 * EPS_FUNC_KMPH2MPS;

  pef->Par.fSteEpsAcdVec[0] =   0.00;
  pef->Par.fSteEpsAcdVec[1] =   0.00;
  pef->Par.fSteEpsAcdVec[2] =   0.10;
  pef->Par.fSteEpsAcdVec[3] =   0.25;
  pef->Par.fSteEpsAcdVec[4] =   0.25;
  pef->Par.fSteEpsAcdVec[5] =   0.25;
  pef->Par.fSteEpsAcdVec[6] =   0.25;
  pef->Par.fSteEpsAcdVec[7] =   0.25;
  pef->Par.fSteEpsAcdVec[8] =   0.25;


  /* set up tables */
  pef->fSteEpsBasFadTab.set(pef->Par.phiSteEpsBasFadVec
                           ,EPS_FUNC_N_PHI_STE_EPS_BAS_FAD_VEC
                           ,pef->Par.fSteEpsBasFadVec
                           ,EPS_FUNC_N_PHI_STE_EPS_BAS_FAD_VEC
                           ,1);

  {
    uint32_t i,j;
    Matrix_t z = NewMatrix(EPS_FUNC_N_V_STE_EPS_BAS_ASSIST,EPS_FUNC_N_T_STE_EPS_BAS_ASSIST);

    for(i=0;i<EPS_FUNC_N_V_STE_EPS_BAS_ASSIST;i++)
       for(j=0;j<EPS_FUNC_N_T_STE_EPS_BAS_ASSIST;j++)
          z[i][j] = pef->Par.TSteEpsBasAssistMap[i][j];

    pef->TSteEpsBasAssistMap.set(pef->Par.TSteEpsBasInpVec
                                ,EPS_FUNC_N_T_STE_EPS_BAS_ASSIST
                                ,pef->Par.vSteEpsBasVec
                                ,EPS_FUNC_N_V_STE_EPS_BAS_ASSIST
                                ,z
                                ,1);
    FreeMatrix(z);
  }
  pef->fSteEpsAcdVelTab.set(pef->Par.vSteEpsAcdVec
                           ,EPS_FUNC_N_F_STE_EPS_ACD_VEC
                           ,pef->Par.fSteEpsAcdVec
                           ,EPS_FUNC_N_F_STE_EPS_ACD_VEC
                           ,1);


  /* set up Filter1 Handwheel Angle Fading */
  SlfNumFiltDigPar(&(pef->TSteColFadFiltStruct1)
                  ,pef->Par.aSteEpsBasFadFilt1Vec
                  ,EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT1-1
                  ,pef->Par.bSteEpsBasFadFilt1Vec
                  ,EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT1-1);
  /* set up Filter2 Handwheel Angle Fading */
  SlfNumFiltDigPar(&(pef->TSteColFadFiltStruct2)
                  ,pef->Par.aSteEpsBasFadFilt2Vec
                  ,EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT2-1
                  ,pef->Par.bSteEpsBasFadFilt2Vec
                  ,EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT2-1);

  if( pef->Par.useTSteColFadFiltAlt )
  {
    SlfNumPt1FiltInit(&(pef->TSteColFadFiltAltStruct),pef->Par.T1SteColFadFiltAlt, EPS_FUNC_LOOP_TIME, 0.0);
  }
}
static void EpsFuncCalc(TEpsFunc *pef)
{

  /* Signal Processing */
  /*===================*/
  /* Torque Input Steering Column with external Input befor EPSFunc*/
  if(  (pef->Inp.pQP6D0->QP_ReqSteerControl == EPS_FUNC_STEERING_CONTROL)
    && (pef->Inp.pQP6D0->QP_Alpha_Beta_Interaction == EPS_FUNC_ALPHA_BETA_BEFOR)
    )
  {
    pef->Int.TSteColInp = *(pef->Inp.pTSteColTwist) * ((double)pef->Inp.pQP6D0->QP_Weighting_Factor_alpha/(double)EPS_FUNC_ALPHA_BETA_MAX)
                        + pef->Inp.pQP6D0->QP_Req_SteerTorque * ((double)pef->Inp.pQP6D0->QP_Weighting_Factor_beta/(double)EPS_FUNC_ALPHA_BETA_MAX);
  }
  /* Torque Input Steering Column without external Input */
  else
  {
    pef->Int.TSteColInp = *(pef->Inp.pTSteColTwist);
  }

  /* Base Assistance (BAS)  */
  /*========================*/
  /* Handwheel Angle Fading */
  /*------------------------*/
  pef->Int.phiSteEpsFad = SlfNumLim(SlfNumAbs( *(pef->Inp.phipSteEPS)
                                             + SlfNumSgn(*(pef->Inp.phipSteEPS))*pef->Par.phiSteEpsBasFadShift
                                             )
                                   ,pef->Par.phiSteEpsBasFadMax
                                   ,0.0
                                   );
  pef->fSteEpsBasFadTab.get(pef->Int.phiSteEpsFad,&(pef->Int.fSteEpsBasFad)); 

  /* Handwheel Angle Fading Filter or alternative Filter */
  /*-----------------------------------------------------*/
  if( pef->Par.useTSteColFadFiltAlt )
  {
    pef->Int.TSteEpsBasPc = SlfNumFiltDigCalc(&(pef->TSteColFadFiltAltStruct),pef->Int.TSteColInp);
  }
  else
  {
    pef->Int.TSteColFadFilt1 = SlfNumFiltDigCalc(&(pef->TSteColFadFiltStruct1),pef->Int.TSteColInp);
    pef->Int.TSteColFadFilt2 = SlfNumFiltDigCalc(&(pef->TSteColFadFiltStruct2),pef->Int.TSteColInp);

    /* Phase Compensator Torque      */
    /*-------------------------------*/
    pef->Int.TSteEpsBasPc = (1.0-pef->Int.fSteEpsBasFad) * pef->Int.TSteColFadFilt1
                          + pef->Int.fSteEpsBasFad * pef->Int.TSteColFadFilt2;
  }

  /* Assistance Map */
  /*----------------*/
  pef->TSteEpsBasAssistMap.get(SlfNumAbs(pef->Int.TSteEpsBasPc)
                              ,SlfNumAbs(*(pef->Inp.pvVeh))
                              ,&(pef->Int.TSteEpsBasAssist));
  //pef->Int.TSteEpsBasAssist = 0.0;

  pef->Int.TSteEpsBasRef = SlfNumLim1(pef->Int.TSteEpsBasAssist,pef->Par.TSteEpsRefMax)
                         * SlfNumSgn(pef->Int.TSteEpsBasPc)
                         * pef->Par.fSteEpsBasTor;

  /* Active Damping (ACD)   */
  /*========================*/
  /* Base damping force     */
  /*------------------------*/
  pef->Int.TSteEpsAcdDamp = *(pef->Inp.phipSteEPS) * pef->Par.dSteEpsAcdDamp * pef->Par.aSteEpsAcd;

  /* velocity dependency */
  pef->fSteEpsAcdVelTab.get(SlfNumAbs(*(pef->Inp.pvVeh)),&(pef->Int.fSteEpsAcdVel));


  /* Reference Torque */
  pef->Int.TSteEpsAcdRef = SlfNumLim1(pef->Int.TSteEpsAcdDamp * (-1.0) * pef->Int.fSteEpsAcdVel
                                     ,pef->Par.TSteEpsRefMax);

  /* arbitriertes Moment    */
  /*========================*/
  pef->Out.TSteEpsRef = pef->Int.TSteEpsBasRef
                      + pef->Int.TSteEpsAcdRef;
  if(  (pef->Inp.pQP6D0->QP_ReqSteerControl == EPS_FUNC_STEERING_CONTROL)
    && (pef->Inp.pQP6D0->QP_Alpha_Beta_Interaction == EPS_FUNC_ALPHA_BETA_AFTER)
    )
  {
    pef->Out.TSteEpsRef += pef->Inp.pQP6D0->QP_Req_MotorTorque;
  }
  /* Min/Max */
  pef->Out.TSteEpsRef = SlfNumLim1(pef->Out.TSteEpsRef,pef->Par.TSteEpsRefMax);


}
static void EpsMotorInit(TEpsMotor *pem)
{
  /* Internal variable  */
  pem->Int.dt = 0.0;

  /* Parameter definition */
  pem->Par.rSteEps              = 3.63162e-4*0.996526427;     /* m        Radius Pinion EPSMotor                */
  pem->Par.rStePin              = 8.498682975e-3;     /* m        Radius Pinion Steer Wheel pinion side */
  pem->Par.dSteEps              = 0.0054;
  pem->Par.JSteEps              = 2.4159e-4 * 0.0;
  pem->Par.T1SteEps             = 0.015;

  //pem->Par.FSteRackAssistScaleFac0 = 0.2;
  //pem->Par.FSteRackAssistScaleFac1 = 0.5/1.2;
  pem->Par.FSteRackAssistScaleFac0 = 1.0;
  pem->Par.FSteRackAssistScaleFac1 = 0.0;
}
static void EpsMotorCalc(TEpsMotor *pem, double *pdt)
{
  double factor;

  /* kenamtic e-motor */
  /*------------------*/
  pem->Out.phiSteEps     = *(pem->Inp.pxSteRack) / pem->Par.rSteEps;  
  pem->Out.phipSteEps    = *(pem->Inp.pxpSteRack) / pem->Par.rSteEps;  
  pem->Out.phippSteEps   = *(pem->Inp.pxppSteRack) / pem->Par.rSteEps;

  pem->Out.phiSteColCalc = *(pem->Inp.pxSteRack) * pem->Par.rStePin;


  /* E-Motor Dynamic */
  /*-----------------*/
  if( pem->Int.dt != *pdt )
  {
    SlfNumPt1FiltInit(&(pem->TSteEpsFiltStruct),pem->Par.T1SteEps, *pdt, *(pem->Inp.pTSteEpsRef));
    pem->Int.dt = *pdt;
  }

  pem->Out.TSteEps = SlfNumFiltDigCalc(&(pem->TSteEpsFiltStruct),*(pem->Inp.pTSteEpsRef));


  /* Interface force to rack */

  pem->Out.FSteRackAssist = pem->Out.TSteEps
                          - pem->Out.phippSteEps * pem->Par.JSteEps
                          - pem->Out.phipSteEps * pem->Par.dSteEps;

  pem->Out.FSteRackAssist /= pem->Par.rSteEps;

  if( *(pem->Inp.pxSteRack) < 0. )
  {
    factor = pem->Par.FSteRackAssistScaleFac0-pem->Par.FSteRackAssistScaleFac1 * pem->Out.TSteEps;
  }
  else
  {
    factor = pem->Par.FSteRackAssistScaleFac0+pem->Par.FSteRackAssistScaleFac1 * pem->Out.TSteEps;
  }

  pem->Out.FSteRackAssist *= factor;
}
