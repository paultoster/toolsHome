
#include <math.h>
#include "mod_tire_static_calc.h"



okay_t  CModTire::Init(const double &dtin)
{
  return true;
} 
okay_t  CModTire::First(const double &t0)
{
  mMessage.Clear();


  return OKAY;
} 
okay_t  CModTire::Prestate(const double &tact)
{ 
  
  //TwoTrackModel.PreState(tact);
  return NOT_OK; 
}

okay_t  CModTire::Output(const double &t0, const slf::CVectorD &x)
{
  mMessage.Clear();


  return OKAY;
}
okay_t  CModTire::StateDer(const double &t, const slf::CVectorD &x, slf::CVectorD &xp)
{
  
  
  //TwoTrackModel.state.x_0B0      = x[0];
  //TwoTrackModel.state.y_0B0      = x[1];
  //TwoTrackModel.state.psi_0B0    = x[2];
  //TwoTrackModel.state.vx_0BB     = x[3];
  //TwoTrackModel.state.vy_0BB     = x[4];
  //TwoTrackModel.state.omegaz_0BB = x[5];
  //TwoTrackModel.state.Omega_wh[MOD_TWO_TRACK_IND_WHEEL_FL] = x[6];
  //TwoTrackModel.state.Omega_wh[MOD_TWO_TRACK_IND_WHEEL_FR] = x[7];
  //TwoTrackModel.state.Omega_wh[MOD_TWO_TRACK_IND_WHEEL_RL] = x[8];
  //TwoTrackModel.state.Omega_wh[MOD_TWO_TRACK_IND_WHEEL_RR] = x[9];
  
  return OKAY;
}
