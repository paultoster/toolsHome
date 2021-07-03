///////////////////////////////////////////////////////////////////////////
// Project: c-Interface EPS_Passat
// Continental 2015
///////////////////////////////////////////////////////////////////////////

#ifndef EPS_PASSAT_INTERFACE_H
#define EPS_PASSAT_INTERFACE_H

#include "EPS_Passat.h"


extern double phiStePin_Eps_Passat;
extern double xRack_Eps_Passat;
extern double xpRack_Eps_Passat;
extern double xppRack_Eps_Passat;
extern double TSteColTwist_Eps_Passat;
extern double velVeh_Eps_Passat;
extern struct TQP6D0 qp6d0_Eps_Passat;
extern double TEpsTorqueCol_Eps_Passat;

void EPS_Passat_Interface_Init(void);
void EPS_Passat_Interface_Loop(void);

#endif