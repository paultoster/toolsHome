

#ifndef SFL_BASIC_CLASS_H_INCLUDED
#define SFL_BASIC_CLASS_H_INCLUDED


#ifdef __cplusplus
#include "SlfStr.h"
class CSlfBasic {
public:

    inline CSlfBasic()         {Status=OKAY;}
    virtual ~CSlfBasic()       { }

    uint32_t   getLenErrText(void)  {return ErrText.getLen();}
    const char *getErrText(void)    {return ErrText.c_str();}
    void       resetErrText(void)   {ErrText.clear();}

    uint32_t   getLenLogText(void)  {return LogText.getLen();}
    const char *getLogText(void)    {return LogText.c_str();}
    void       resetLogText(void)   {LogText.clear();}

    bool     isStatusOkay(void)     {if(Status==OKAY)return true;else return false;}
    bool     isStatusNotOkay(void)  {if(Status!=OKAY)return true;else return false;}

protected:
  status_t   Status;
  slf::CStr    ErrText;
  slf::CStr    LogText;

};
#endif

#endif