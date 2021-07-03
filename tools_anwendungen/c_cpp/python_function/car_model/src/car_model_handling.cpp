
#include "car_model_handling.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string>
#include <iostream>

#include "SlfSys.h"


okay_t CCarModelHandling::SetParamByString(char *str)
{
  slf::CStr s(str);

  if (!par.readStrInNewParSet(s))
  {
    message.SetUnRegErr(CAR_MODEL_ERROR_NR_PARAMETR_STRING_NOT_READ, "CAR_MODEL_ERROR_NR_PARAMETR_STRING_NOT_READ");
    return  NOT_OKAY;
  }

  return OKAY;

}
okay_t CCarModelHandling::SetParamByFileName(char *filename)
{

  if (!par.readFileInNewParSet(filename))
  {
    message.SetUnRegErr(CAR_MODEL_ERROR_NR_PARAMETR_FILE_NOT_READ, "CAR_MODEL_ERROR_NR_PARAMETR_FILE_NOT_READ");
    return  NOT_OKAY;
  }

  return OKAY;
}
okay_t CCarModelHandling::Init(double &dt)
{
  param.dt_sim = dt;

  std::cout << "param.dt:" << param.dt_sim << std::endl;

  // Model Definitions
  //if (!mod.registerModul(vehicle, dt))
  //{
  //  std::cout << "See LogFile : " << LogText << std::endl;
  //  exit(101);
  //}

  return message.IsOkay();
}
okay_t CCarModelHandling::First(double &y0)
{
  
  output.y = y0;

  std::cout << "output.y0:" << output.y << std::endl;
  
  return message.IsOkay();
}
okay_t CCarModelHandling::Loop(double &x)
{
  input.x = x;

  output.y += input.x;

  std::cout << "output.y:" << output.y << std::endl;

  return message.IsOkay();
}