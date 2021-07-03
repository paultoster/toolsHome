// run_mod.h
//
// einfaches Bewegungsmodell
//
//
#ifndef RUN_MOD_H_INCLUDED
#define RUN_MOD_H_INCLUDED



#include "SlfNum.h"
#include "SlfModCtrl.h"
#include "SlfPar.h"



// Klasse 
class CRunMod
{
private:
  slf::CLog    log;
  slf::CMod    mod;
  slf::CPar    par;

public:


  // Construktor/Destruktor
  //=================================================================
  CRunMod(): log(),mod(log),par("par1", log){}
  ~CRunMod() {}

  okay_t Init(const char *paramstring);
  okay_t Calc(const double &val);

  const char *GetLogText(void){ return log.getLogText(); }
  void resetLogText(void){ log.resetLogFileValues(); }

};

#endif
