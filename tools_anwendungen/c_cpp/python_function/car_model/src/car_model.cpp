#include <math.h>
#include "car_model.h"



okay_t  CCarModel::Init(const double &dtin)
{
  massVeh = 2000.;
  s0 = 0.;
  v0 = 0.;
  dt = dtin;
  return true;
}
okay_t  CCarModel::First(const double &t0)
{
  mMessage.Clear();
  accVeh = 0.;
  velVeh = v0;
  distVeh = s0;

  return OKAY;
}
okay_t  CCarModel::Output(const double &t0, const slf::CVectorD &x)
{
  mMessage.Clear();

  accVeh = FAntrieb / massVeh;
  velVeh = x[0];
  distVeh = x[1];

  return OKAY;
}
okay_t  CCarModel::StateDer(const double &t, const slf::CVectorD &x, slf::CVectorD &xp)
{
  xp[0] = accVeh;
  xp[1] = x[0];

  return OKAY;
}
