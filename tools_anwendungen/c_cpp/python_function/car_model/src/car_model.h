// CAR_modEL.h
//
// einfaches Fahrzeugmodell
//
//
#ifndef CAR_MODEL_H_INCLUDED
#define CAR_MODEL_H_INCLUDED


#include "SlfModCtrl.h"
#include "SlfNum.h"



// Klasse 
class CCarModel : public slf::CModelBase
{

public:


  // Destruktor
  //=================================================================
  CCarModel(const char *name) :slf::CModelBase(name){}
  ~CCarModel() {}

  // ParameterDefinition ============================================
  //=================================================================
  slf::CParVarDefCollect DefineParameter(void)
  {
    slf::CParVarDefCollect par;

    //double s0;
    //double massVeh;

    par.SetMainGroup(mModelName);
    par.SetParDef(v0, "", "v0", "m/s", "", "velocity");
    par.SetParDef(s0, "", "s0", "m", "0.", "distance");
    par.SetParDef(massVeh, "", "massVeh", "kg", "0", "mass vehicle");

    return par;
  }
  // InputDefinition ===================================================
  //=================================================================
  slf::CVarDefCollect DefineInput(void)
  {
    slf::CVarDefCollect inp;

    inp.SetVarDef(FAntrieb, "FAntrieb", "Nm", "Transmission Force");

    return inp;
  }
  // OutputDefinition ===================================================
  //=================================================================
  slf::CVarDefCollect CCarModel::DefineOutput(void)
  {
    slf::CVarDefCollect out;
    out.SetVarDef(accVeh, "accVeh", "m/s/s", "Acceleration Vehicle");
    out.SetVarDef(velVeh, "velVeh", "m/s", "Velocity Vehicle");
    out.SetVarDef(distVeh, "sVeh", "m", "distance Vehicle");
    return out;
  }
  // StateDefinition ===================================================
  //=================================================================
  slf::CStateDef   CCarModel::DefineState(void)
  {
    nstate = 2;
    bool useprestatfkt = false;
    bool usejacobifkt = false;
    slf::CStateDef state(slf::DEF_DOPRI45, nstate, useprestatfkt, usejacobifkt);
    //state.AddStateName("velo", "m/s");
    //state.AddStateName("dist", "m");

    return state;
  }
  // Init ===============================================================
  okay_t  Init(const double &dt);
  // First ===============================================================
  okay_t  First(const double &t);
  // Output ==============================================================
  okay_t  Output(const double &t, const slf::CVectorD &x);
  // State ==============================================================
  okay_t  StateDer(const double &t, const slf::CVectorD &x, slf::CVectorD &xp);

protected:


  // ist schon definiert in ModBase
  //    okay_t          Status;
  //	  CSlfStr           ErrText;               // Fehlertext
  //    CSlfStr           LogText;               // fürs Logfile gedacht

  /* Input */
  double  FAntrieb;

  /* Output */
  double accVeh;
  double velVeh;
  double distVeh;

  // State
  double vel;
  double dist;
  //Derivative
  double velp;
  double distp;

  // StateVectorLength
  std::size_t    nstate;

  //Parameter State
  double v0;
  double s0;
  double massVeh;

  // intern
  double dt;

};

#endif
