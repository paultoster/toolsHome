// DsModCtrl.h
//
// Steuerung der Modelle 
//
#include "SlfBase.h"
#include "SlfStr.h"
#include "SlfStrV.h"
#include "SlfStrM.h"
#include "SlfNum.h"
#include "SlfLog.h"
#include "SlfMessage.h"
#include "SlfModVar.h"
#include "SlfParVar.h"
#include "SlfParVarVal.h"
#include "SlfModState.h"
#include "SlfModStruct.h"
#include "SlfModelBase.h"

#ifndef DS_MODCTRL_H_INCLUDED
#define DS_MODCTRL_H_INCLUDED

#define INDEX_EXT_INPUT   0
#define INDEX_EXT_OUTPUT  1
#define INDEX_MODUL_FIRST 2

namespace slf
{

  // 
  class CMod
  {
  private:
    CLog                     *pLog;
    CMessage                 mMessage;
    std::vector<SModStruct>  mModVec;
    std::size_t              mNMod;                // number of models in vector (including external input and output)
    timestamp_t              mDtTS;                // time step of general output
    timestamp_t              mDtTSCalc;            // calculation time step
    timestamp_t              mTSCalc;              // calculation time

  public:
    const CMessage &Message;

    CVarDefCollect ExtInpDef;
    CVarDefCollect ExtOutDef;
    CMod(CLog &log) :pLog(&log), mMessage("ModelHandling"), Message(mMessage), ExtInpDef(), ExtOutDef() { reset(); }
    CMod() :pLog(0), mMessage("ModelHandling"), Message(mMessage){ reset(); }
    ~CMod(){}

    // Model registrieren
    // Übergabe Instanz und Schrittweite
    // Für n-Modelle wird der Aufruf n-mal ausgeführt
    // return OKAY oder NOT_OKAY
    okay_t registerModul(CModelBase &model,const double dt);
    
    // printParameterTemplate all parameter and states registered in registerIOPSOfModuls() are printed in a template file
    okay_t printParameterTemplate(const char *filename);

    // set Parameter in Models
    okay_t setParameter(const CParVarValCollect &pvvc);

    // initilize
    okay_t init(double dt);

    // first calculation t=0
    okay_t first(void);

    // step calculation
    okay_t step(void);
  private:

    // Module über Aufruf registrieren
    okay_t registerIOPSOfModuls(std::size_t &index);
    okay_t registerStatesAdditionalToParam(std::size_t &index);
    okay_t registerIntegrationERKAdditionalToParam(std::size_t &index);
    okay_t registerIntegrationGEARAdditionalToParam(std::size_t &index);
    okay_t registerIntegrationRAD5AdditionalToParam(std::size_t &index);

    // Externer Input aus ExtInpDef registrieren
    okay_t registerExtInp(void);

    // Externer Output aus ExtOutDef registrieren
    okay_t registerExtOut(void);

    // build io handling
    okay_t buildModulsIO(void);

    // check parameter if set or will be set with default
    okay_t checkParameter(void);

    // build loop time and state of moduls
    void buildModulsState(void);

    // log to message IO
    void logModulIO(void);

    // search for Outputname
    bool searchInOutputs(const CStr &name, std::size_t indexnot, std::size_t &index, std::size_t &i);

    // set all inputs of modul with index
    void SetModInput(std::size_t index);

    // calculate Integration
    void CalcIntegrationFirst(std::size_t index);
    void CalcIntegrationStep(std::size_t index);


    void reset(void)
    {
      mModVec.clear();
      // first model in modVec is Input
      {
        SModStruct mod("ExtInput");
        mModVec.push_back(mod);
      }
      {
        SModStruct mod("ExtOutput");
        mModVec.push_back(mod);
      }
      mNMod = 2;
    }

  };
} // namespace slf

#endif