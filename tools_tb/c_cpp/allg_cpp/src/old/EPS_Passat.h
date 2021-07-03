///////////////////////////////////////////////////////////////////////////
// Project: Generic CarMaker c-Interface
// Continental 2011
///////////////////////////////////////////////////////////////////////////

#ifndef EPS_PASSAT_H
#define EPS_PASSAT_H

//#include "utilities/defs.h"
#include "SlfTab.h"

#define EPS_FUNC_DEG2RAD                   (0.017453292)
#define EPS_FUNC_KMPH2MPS                  (1.0/3.6)
#define EPS_FUNC_N_PHI_STE_EPS_BAS_FAD_VEC (5)
#define EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT1 (4)
#define EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT2 (5)
#define EPS_FUNC_N_T_STE_EPS_BAS_ASSIST    (10)
#define EPS_FUNC_N_V_STE_EPS_BAS_ASSIST    (7)
#define EPS_FUNC_N_F_STE_EPS_ACD_VEC       (9)
#define EPS_FUNC_LOOP_TIME        (0.001)
#define EPS_FUNC_STEERING_CONTROL (1)
#define EPS_FUNC_ALPHA_BETA_BEFOR (1)
#define EPS_FUNC_ALPHA_BETA_AFTER (0)
#define EPS_FUNC_ALPHA_BETA_MAX   (128)




struct TQP6D0
{
  unsigned char        QP_CRC;                    /*   */
  unsigned char        QP_Zaehler;                /*   */
  unsigned char        QP_ReqSteerControl;        /*  Requested ECU Status, 0: Steering Mode, 1: Steering Actuator Mode */
  unsigned char        QP_Alpha_Beta_Interaction; /* Requested factor interaction, 0: after EPS, 1: before EPS  */
  unsigned char        QP_AngelControl;           /* Requested angle control 0:Off, 1:On  */
  unsigned char        QP_Weighting_Factor_alpha; /* EPS weighting factor alpha for Steering Torque Input/EPS Function Output 0 ... 1.0 */
  unsigned char        QP_Weighting_Factor_beta;  /*  EPS weighting factor beta for Requested Steering Torque/Requested Motor Torque 0 ... 1.0 */
  float                QP_Req_SteerTorque;        /* Nm   Requested steering torque lsb 0.01 Nm */
  float                QP_Req_MotorTorque;        /* Nm  Requested motor torque lsb 0.01 Nm */
  float                QP_Req_SteerAngle;         /* °  */

};

typedef struct
{
  /* Eingang */
  struct
  {
    double *pTSteColTwist;              /* rad      torsion torque */
    double *pphiSteColCalc;             /* rad      calculated Steering angle from Motorrotation */
    double *phipSteEPS;                 /* rad/s    motor angular speed */
    double *pvVeh;                      /* m/s      vehicle velocity */
    struct TQP6D0 *pQP6D0;                     /*          extern Input */
  } Inp;
  struct
  {
    double    TSteColInp;                  /* Nm       Steerwheel Torque Input */
    double    phiSteEpsFad;                /* rad      fading steer wheel angle */
    double    fSteEpsBasFad;               /* -        fading factor sterangle dependend */
    double    TSteColFadFilt1;             /* Nm       Filter torque Input for Handwheel Angle Fading */
    double    TSteColFadFilt2;             /* Nm       Filter torque Input for Handwheel Angle Fading */
    double    TSteEpsBasPc;                /* Nm       Phase Compensator Torque */
    double    TSteEpsBasAssist;            /* Nm       assistance Torque from Map */
    double    TSteEpsBasRef;               /* Nm       Reference base asssictanec torque from TSteEpsBasAssist */
    double    TSteEpsAcdDamp;              /* Nm       base daming torque */
    double    fSteEpsAcdVel;               /* -        ACD factor velocity dependend */
    double    TSteEpsAcdRef;               /* Nm       reference Torque ACD Function */
  } Int;
  struct
  {
    double    TSteEpsRef;                  /* Nm       Arbitrated reference Torque */
  } Out;
  struct 
  {
    double TSteColInpMax;               /* Nm       maximum torque at column */
    double phiSteEpsBasFadVec[EPS_FUNC_N_PHI_STE_EPS_BAS_FAD_VEC];
                                        /* rad      Handwheel Angle Fading SteerWheelAngle */
    double fSteEpsBasFadVec[EPS_FUNC_N_PHI_STE_EPS_BAS_FAD_VEC];
                                        /* rad      Handwheel Angle Fading SteerWheelAngle */
    double phiSteEpsBasFadMax;          /* rad      maximum fade angle */
    double phiSteEpsBasFadShift;        /* rad      sift of fading angle */

    double bSteEpsBasFadFilt1Vec[EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT1];
                                        /*          filter factors for in Input Fade Filter1 */
    double aSteEpsBasFadFilt1Vec[EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT1];
                                        /*          filter factors for in output Fade Filter1 */
    double bSteEpsBasFadFilt2Vec[EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT2];
                                        /*          filter factors for in Input Fade Filter2 */
    double aSteEpsBasFadFilt2Vec[EPS_FUNC_N_B_STE_EPS_BAS_FAD_FILT2];
                                        /*          filter factors for in output Fade Filter2 */
    double         T1SteColFadFiltAlt;    /*        filter time connstant alternative Filter  */
    unsigned char  useTSteColFadFiltAlt;  /*        switch alternative Filter */

    double TSteEpsBasInpVec[EPS_FUNC_N_T_STE_EPS_BAS_ASSIST];
                                        /* Nm       Input-vector Steerdemand  */
    double vSteEpsBasVec[EPS_FUNC_N_V_STE_EPS_BAS_ASSIST];
                                        /* m/s      Input-vector vehicle velocity Column */
    double TSteEpsBasAssistMap[EPS_FUNC_N_V_STE_EPS_BAS_ASSIST][EPS_FUNC_N_T_STE_EPS_BAS_ASSIST];
                                        /* Nm       Input-vector Steerdemand  */
    double TSteEpsRefMax;               /* Nm       maximum Motor Torque */
    double fSteEpsBasTor;               /* -        motor torque scaling */

    double dSteEpsAcdDamp;              /* Nms/rad  damping Factor active Damping */
    double aSteEpsAcd;                  /* -        scaling factor */

    double vSteEpsAcdVec[EPS_FUNC_N_F_STE_EPS_ACD_VEC];
                                        /* m/s      Velocity Vector_t velocity dependend factor */
    double fSteEpsAcdVec[EPS_FUNC_N_F_STE_EPS_ACD_VEC];
                                        /* m/s      Factor Vector_t velocity dependend factor */
  } Par;
  
  CSlf1DTab fSteEpsBasFadTab;            /*          1d-table factor Handwheel Angle Fading */
  CSlf2DTab TSteEpsBasAssistMap;         /*          2d-table for assist function */
  CSlf1DTab fSteEpsAcdVelTab;            /*          1d-table velocity depended factor ACD */


  SSlfNumFiltDig TSteColFadFiltStruct1;  /*          Filterstruktur für Handwheel Angle Fade */
  SSlfNumFiltDig TSteColFadFiltStruct2;  /*          Filterstruktur für Handwheel Angle Fade */

  SSlfNumFiltDig TSteColFadFiltAltStruct;  /*        Filterstruktur für alternativen Filter */
} TEpsFunc;

typedef struct
{
  /* Eingang */
  struct
  {
    double *pxSteRack;                  /* m        rack displacement */
    double *pxpSteRack;                 /* m/s      rack velocity */
    double *pxppSteRack;                /* m/s/s    rack acceleration */
    double *pTSteEpsRef;                /* Nm       reference Motor Torque */
  } Inp;
  struct
  {
    double dt;                          /* -        loop time */
  } Int;
  /* Ausgang */
  struct
  {
    double phiSteEps;                   /* rad      EPS Motor Angle */
    double phipSteEps;                  /* rad/s    EPS Motor Angular Velocity */
    double phippSteEps;                 /* rad/s/s  EPS Motor Angular Acceleration */
    double phiSteColCalc;               /* rad      calculated steer wheel angle from e-motor */
    double TSteEps;                     /* Nm       EPS Motor-Torquq */
    double FSteRackAssist;              /* N        Rack Input Force from EPS E-Motor */
  } Out;
  /* Parameter */
  struct
  {
    double rSteEps;                     /* m        Radius Pinion EPSMotor  */
    double rStePin;                     /* m        Radius Pinion Steer Wheel pinion side */
    double T1SteEps;                    /* s        time constant EPS Motor electrical */
    double JSteEps;                     /* kgm^2    moment of inertia EPS Motor */
    double dSteEps;                     /* Nms/rad  damping coefficient Eps Motor */
    double FSteRackAssistScaleFac0;     /* -        scaling factor for Rackforce constant part */
    double FSteRackAssistScaleFac1;     /* -        scaling factor for Rackforce rack displacement dependend*/
  } Par;

  SSlfNumFiltDig TSteEpsFiltStruct;    /*          Filterstruktur PT1-Verhalten E-Motor */
} TEpsMotor;
//typedef  struct {
//    tDDGetFunc Get;
//    void *ptr;
//    double val;
//} TTSteColTwistDataDict;

typedef struct
{
//  TTSteColTwistDataDict TSteColTwistDataDict;
  double                vVeh;
  struct TQP6D0         QP6D0;
  TEpsFunc              EpsFunc;
  TEpsMotor             EpsMotor;
  
} TEPS_Passat;

extern TEPS_Passat EPS_Passat;


int  Init_EPS_Passat(double *pxRack,double *pxpRack,double *pxppRack);
void EPS_Passat_Calc (double *pdt,double *pTSteColTwist,double *pvel,struct TQP6D0 *pqp6d0,double *passist_frc);

#endif	// EPS_PASSAT_H

