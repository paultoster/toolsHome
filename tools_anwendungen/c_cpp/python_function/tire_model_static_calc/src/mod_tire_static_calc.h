// mod_simple.h
//
// einfaches Bewegungsmodell
//
//
#ifndef MOD_TWO_TRACK_H_INCLUDED
#define MOD_TWO_TRACK_H_INCLUDED


#include "SlfModCtrl.h"
#include "SlfNum.h"



// Klasse 
class  CModTire : public slf::CModelBase
{

public:


  // Destruktor
  //=================================================================
   CModTire(const char *name) : slf::CModelBase(name){}
  ~ CModTire() {}

  // ParameterDefinition ============================================
  //=================================================================
  slf::CParVarDefCollect DefineParameter(void)
  {
    slf::CParVarDefCollect p;

    p.SetMainGroup(mModelName);
    //            variable,             subgroup, varname,                   unit,  deafult,   comment
    //p.SetParDef(TwoTrackModel.par.lf_veh, "", "lf_veh", "m", "0.0", "long distance vehicle from COG to front axle");
    //p.SetParDef(TwoTrackModel.par.lr_veh, "", "lr_veh", "m", "0.0", "long distance vehicle from COG to rear axle");
    //p.SetParDef(TwoTrackModel.par.bfl_veh, "", "bfl_veh", "m", "0.0", "lat distance from  COG to front left wheel");
    //p.SetParDef(TwoTrackModel.par.bfr_veh, "", "bfr_veh", "m", "0.0", "lat distance from  COG to front right wheel");
    //p.SetParDef(TwoTrackModel.par.brl_veh, "", "brl_veh", "m", "0.0", "lat distance from  COG to rear left wheel");
    //p.SetParDef(TwoTrackModel.par.brr_veh, "", "brr_veh", "m", "0.0", "lat distance from  COG to rear right wheel");
    //p.SetParDef(TwoTrackModel.par.hcog_veh, "", "hcog_veh", "m", "0.0", " height of COG from bottom");
    //
    //p.SetParDef(TwoTrackModel.par.cx_w_veh, "", "cx_w_veh", "-", "0.0", "resistance factor");
    //p.SetParDef(TwoTrackModel.par.Ax_veh, "", "Ax_veh", "m*m", "0.0", "projected area in x-Direction vehicle");
    //p.SetParDef(TwoTrackModel.par.rho_air, "", "rho_air", "kg/m/m/m", "0.0", "density air");
    //
    //p.SetParDef(TwoTrackModel.par.m_veh, "", "m_veh", "kg", "0.0", "vehicle mass");
    //p.SetParDef(TwoTrackModel.par.thetaz_veh, "", "thetaz_veh", "kg*m*m", "0.0", "moment of inertia around vertical axis in COG");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].L0, "tire_fl", "L0", "m", "0.0", "Contact patch length");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].B0, "tire_fl", "B0", "m", "0.0", "width tire");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].R0, "tire_fl", "R0", "m", "0.0", "free radius tyre");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].RD, "tire_fl", "RD", "m", "0.0", "dynamic roll radiuse");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].froll, "tire_fl", "froll", "-", "0.0", "rolling resistance");
    //
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].hsri.Calpha, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of lateral characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].hsri.Cs, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of long. characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].hsri.f0, "tire_fl.hsri", "f0", "N", "0.0", "potential fricional connection if slide_velocity=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FL].hsri.k, "tire_fl.hsri", "k", "s/m", "0.0", "fricional connection decrease if forward velocity increase");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].L0, "tire_fr", "L0", "m", "0.0", "Contact patch");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].B0, "tire_fl", "B0", "m", "0.0", "width tire");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].R0, "tire_fl", "R0", "m", "0.0", "free radius tyre");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].RD, "tire_fl", "RD", "m", "0.0", "dynamic roll radiuse");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].froll, "tire_fl", "froll", "-", "0.0", "rolling resistance");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].hsri.Calpha, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of lateral characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].hsri.Cs, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of long. characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].hsri.f0, "tire_fl.hsri", "f0", "N", "0.0", "potential fricional connection if slide_velocity=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_FR].hsri.k, "tire_fl.hsri", "k", "s/m", "0.0", "fricional connection decrease if forward velocity increase");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].L0, "tire_rl", "L0", "m", "0.0", "Contact patch");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].B0, "tire_fl", "B0", "m", "0.0", "width tire");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].R0, "tire_fl", "R0", "m", "0.0", "free radius tyre");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].RD, "tire_fl", "RD", "m", "0.0", "dynamic roll radiuse");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].froll, "tire_fl", "froll", "-", "0.0", "rolling resistance");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].hsri.Calpha, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of lateral characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].hsri.Cs, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of long. characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].hsri.f0, "tire_fl.hsri", "f0", "N", "0.0", "potential fricional connection if slide_velocity=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RL].hsri.k, "tire_fl.hsri", "k", "s/m", "0.0", "fricional connection decrease if forward velocity increase");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].L0, "tire_rr", "L0", "m", "0.0", "Contact patch");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].B0, "tire_fl", "B0", "m", "0.0", "width tire");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].R0, "tire_fl", "R0", "m", "0.0", "free radius tyre");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].RD, "tire_fl", "RD", "m", "0.0", "dynamic roll radiuse");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].froll, "tire_fl", "froll", "-", "0.0", "rolling resistance");

    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].hsri.Calpha, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of lateral characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].hsri.Cs, "tire_fl.hsri", "Calpha", "N", "0.0", "beginning gradient of long. characteristcs if slip=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].hsri.f0, "tire_fl.hsri", "f0", "N", "0.0", "potential fricional connection if slide_velocity=0");
    //p.SetParDef(TwoTrackModel.par.tire[MOD_TWO_TRACK_IND_WHEEL_RR].hsri.k, "tire_fl.hsri", "k", "s/m", "0.0", "fricional connection decrease if forward velocity increase");
    //p.SetParDef(TwoTrackModel.par.trans_type, "trans", "type", "", "TRANS_FA", "transmission type TRANS_FA, TRAN_RA, TRANS_FA_RA");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_engine, "trans.fa", "theta_engine", "kg*m/s/s", "0.0", "moment of inertia combustion  engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_clutch_in, "trans.fa", "theta_clutch_in", "kg*m/s/s", "0.0", "moment of inertia clutch input side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_clutch_out, "trans.fa", "theta_clutch_out", "kg*m/s/s", "0.0", "moment of inertia clutch output side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_gearbox_in, "trans.fa", "theta_gearbox_in", "kg*m/s/s", "0.0", "moment of inertia gearbox input side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_gearbox_in, "trans.fa", "theta_gearbox_in", "kg*m/s/s", "0.0", "moment of inertia gearbox input side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_clutch_out, "trans.fa", "theta_clutch_out", "kg*m/s/s", "0.0", "moment of inertia clutch output side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].theta_diff_in, "trans.fa", "theta_diff_in", "kg*m/s/s", "0.0", "moment of imertia differential gear input-side from Motor seen");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].omega_engine_idle, "trans.fa", "omega_engine_idle", "rad/s", "80.0", "idle rotation angle speed combustion  engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].omega_combustion, "trans.fa", "omega_combustion", "rad/s", "[104.7,1047.2]", "Vector rotation angle speed combustion engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].T_combustion_max, "trans.fa", "T_combustion_max", "Nm", "[500.,500.]", "Vector Maximum Torques alpha_gas=1.0 combustion engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].T_combustion_min, "trans.fa", "T_combustion_min", "Nm", "[-10,-50]", "Vector Minimum Torques alpha_gas=0.0 combustion  engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].gear_ratio_gearbox, "trans.fa", "gear_ratio_gearbox", "-", "[4.0,3.0,2.0,1.5,1.,0.8]", "Vector gear box ratio for first to max gear");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].d0_gearbox, "trans.fa", "d0_gearbox", "Nm", "0.0", "Resistance constnat part in gearbox output sid to differential");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].d1_gearbox, "trans.fa", "d1_gearbox", "Nm/s", "0.0", "Resistance proportional rotation angle speed in gearbox output sid to differential");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_FA].gear_ratio_diff, "trans.fa", "gear_ratio_diff", "-", "1.0", "gear ratio differential");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_engine, "trans.ra", "theta_engine", "kg*m/s/s", "0.0", "moment of inertia combustion  engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_clutch_in, "trans.ra", "theta_clutch_in", "kg*m/s/s", "0.0", "moment of inertia clutch input side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_clutch_out, "trans.ra", "theta_clutch_out", "kg*m/s/s", "0.0", "moment of inertia clutch output side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_gearbox_in, "trans.ra", "theta_gearbox_in", "kg*m/s/s", "0.0", "moment of inertia gearbox input side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_gearbox_in, "trans.ra", "theta_gearbox_in", "kg*m/s/s", "0.0", "moment of inertia gearbox input side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_clutch_out, "trans.ra", "theta_clutch_out", "kg*m/s/s", "0.0", "moment of inertia clutch output side on engine side");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].theta_diff_in, "trans.ra", "theta_diff_in", "kg*m/s/s", "0.0", "moment of imertia differential gear input-side from Motor seen");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].omega_engine_idle, "trans.ra", "omega_engine_idle", "rad/s", "80.0", "idle rotation angle speed combustion  engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].omega_combustion, "trans.ra", "omega_combustion", "rad/s", "[104.7,1047.2]", "Vector rotation angle speed combustion engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].T_combustion_max, "trans.ra", "T_combustion_max", "Nm", "[500.,500.]", "Vector Maximum Torques alpha_gas=1.0 combustion engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].T_combustion_min, "trans.ra", "T_combustion_min", "Nm", "[-10,-50]", "Vector Minimum Torques alpha_gas=0.0 combustion  engine");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].gear_ratio_gearbox, "trans.ra", "gear_ratio_gearbox", "-", "[4.0,3.0,2.0,1.5,1.,0.8]", "Vector gear box ratio for first to max gear");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].d0_gearbox, "trans.ra", "d0_gearbox", "Nm", "0.0", "Resistance constnat part in gearbox output sid to differential");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].d1_gearbox, "trans.ra", "d1_gearbox", "Nm/s", "0.0", "Resistance proportional rotation angle speed in gearbox output sid to differential");
    //p.SetParDef(TwoTrackModel.par.trans[VEHICLE_IND_TRANS_RA].gear_ratio_diff, "trans.ra", "gear_ratio_diff", "-", "1.0", "gear ratio differential");

    //p.SetParDef(TwoTrackModel.par.brake.decel_max, "brake", "decel_max", "m/s/s", "9.81", "maximum deceleration a design parameter");
    //p.SetParDef(TwoTrackModel.par.brake.d_slope_brake, "brake", "d_slope_brake", "-", "5.", "ratio omega dynamic part brake torque");
    //p.SetParDef(TwoTrackModel.par.brake.phi_brake_fa, "brake", "phi_brake_fa", "-", "0.7", "brake torque distribution front/rear portion fronrt axle");

    return p;
  }
  // InputDefinition ===================================================
  //=================================================================
  slf::CVarDefCollect DefineInput(void)
  {
    slf::CVarDefCollect inp;

    //inp.SetVarDef(FAntrieb, "FAntrieb", "Nm", "Transmission Force");

    return inp;
  }  
  // OutputDefinition ===================================================
  //=================================================================
  slf::CVarDefCollect DefineOutput(void)
  {
    slf::CVarDefCollect out;
    //out.SetVarDef(accVeh, "accVeh", "m/s/s", "Acceleration Vehicle");
    //out.SetVarDef(velVeh, "velVeh", "m/s", "Velocity Vehicle");
    //out.SetVarDef(distVeh, "sVeh", "m", "distance Vehicle");
    return out;
  }   
  // StateDefinition ===================================================
  //=================================================================
  slf::CStateDef   DefineState(void)
  {
    nstate = 0;

    slf::CStateDef state( slf::DEF_STEP // type of integration
                        , nstate           // number of states
                        , false            // use pre state function
                        , false);          // use jacobian function

    //state.SetStateName(0, "x_0B0",      "m");      // 
    //state.SetStateName(1, "y_0B0",      "m");      // 
    //state.SetStateName(2, "psi_0B0",    "rad");    // 
    //state.SetStateName(3, "vx_0BB",     "m/s");    // 
    //state.SetStateName(4, "vy_0BB",     "m/s");    // 
    //state.SetStateName(5, "omegaz_0BB", "rad/s");  // 
    //state.SetStateName(6, "Omega_wh_fl", "rad/s"); // 
    //state.SetStateName(7, "Omega_wh_fr", "rad/s"); // 
    //state.SetStateName(8, "Omega_wh_rl", "rad/s"); // 
    //state.SetStateName(9, "Omega_wh_rr", "rad/s"); // 

    //state.SetErr(0.0001, 0.0001);             // ser err tolerance rel, abs for all

    return state;
  }   
  // Init ===============================================================
  okay_t  Init(const double &dt);
  // First ===============================================================
  okay_t  First(const double &t);
  // Prestate ============================================================
  okay_t  Prestate(const double &tact);
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

    // StateVectorLength
    std::size_t    nstate;


    // intern
    double dt;

};

#endif
