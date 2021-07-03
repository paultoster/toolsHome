
#ifndef CAR_MODEL_HANDLING_H_FILE
#define CAR_MODEL_HANDLING_H_FILE

#include "SlfMessage.h"
#include "SlfLog.h"
#include "SlfPar.h"
#include "SlfModCtrl.h"
#include "car_model_def.h"

class CCarModelHandling
{
public:
  struct SInput
  {
    double x;
    SInput()
      :x(0.0)
    {}
  };
  struct SOutput
  {
    double y;
    SOutput()
      :y(0.0)
    {}
  };
  struct SParam
  {
    double dt_sim;
    SParam()
      :dt_sim(0.001)
    {}
  };

  SInput input;
  SOutput output;
  SParam param;
  slf::CMessage message;
  slf::CLog logging;
  slf::CPar par;
  slf::CMod mod;

  CCarModelHandling()
    :input()
    , output()
    , param()
    , message("car_model")
    , logging()
    , par("par_car_model", logging)
    , mod(logging)
  {}

  /* read parameter from string */
  okay_t SetParamByString(char *str);
  /* read parameter from file */
  okay_t SetParamByFileName(char *str);

  /* set parameter */
  void SetParDtSim(double dt) { param.dt_sim = dt; }

  /* get parameter */
  double GetParDtSim(void) { return param.dt_sim; }

  okay_t Init(double &dt);
  okay_t First(double &y0);
  okay_t Loop(double &x);
};

#endif