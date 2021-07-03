///////////////////////////////////////////////////////////////////////////
// Project: c-Interface EPS_Passat
// Continental 2015
///////////////////////////////////////////////////////////////////////////

#include "EPS_Passat.h"
#include "EPS_Passat_Interface.h"

double phiStePin_Eps_Passat;
double TSteColTwist_Eps_Passat;
double velVeh_Eps_Passat;
struct TQP6D0 qp6d0_Eps_Passat;
double TEpsTorqueCol_Eps_Passat;
double xRack_Eps_Passat;
double xpRack_Eps_Passat;
double xppRack_Eps_Passat;


void EPS_Passat_Interface_Init(void)
{
  Init_EPS_Passat(&xRack_Eps_Passat,&xpRack_Eps_Passat,&xppRack_Eps_Passat);

  xpRack_Eps_Passat  = 0.0;
  xppRack_Eps_Passat = 0.0;

  qp6d0_Eps_Passat.QP_Alpha_Beta_Interaction = 0;
  qp6d0_Eps_Passat.QP_AngelControl           = 0;
  qp6d0_Eps_Passat.QP_CRC                    = 0;
  qp6d0_Eps_Passat.QP_ReqSteerControl        = 0;
  qp6d0_Eps_Passat.QP_Req_MotorTorque        = 0.0;
  qp6d0_Eps_Passat.QP_Req_SteerAngle         = 0.0;
  qp6d0_Eps_Passat.QP_Req_SteerTorque        = 0.0;
  qp6d0_Eps_Passat.QP_Weighting_Factor_alpha = 128;
  qp6d0_Eps_Passat.QP_Weighting_Factor_beta  = 128;


}
void EPS_Passat_Interface_Loop(void)
{
  double assist_frc;
  static double dt = 0.001;

  xRack_Eps_Passat = phiStePin_Eps_Passat * EPS_Passat.EpsMotor.Par.rStePin;

  EPS_Passat_Calc (&dt,&TSteColTwist_Eps_Passat,&velVeh_Eps_Passat,&qp6d0_Eps_Passat,&assist_frc); 

  TEpsTorqueCol_Eps_Passat = assist_frc * EPS_Passat.EpsMotor.Par.rStePin;
}

