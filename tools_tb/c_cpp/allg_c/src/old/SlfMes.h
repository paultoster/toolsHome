#ifndef SLF_MES_H_INCLUDED
#define SLF_MES_H_INCLUDED

#include <string.h>
#include "SlfBasic.h"
#include "SlfStr.h"

#define SLF_PMES_DEFAULT_LENGTH 25

class CSlfMes {
public:
    CSlfMes();
    void set(const char *text);
    //void set(const std::string &str_text);
    void set(const CSlfStr &str_text);
    void set(const sint32 ival);
    //void set(const sint64 ival);
    void setEndl(const char *text);
    //void set_endl(const std::string &str_text);
    void setEndl(const CSlfStr &str_text);
    //void setEndl(const sint64 ival);
    void setEndl(const sint32 ival);
    status_t setFormat(char *p_format,...);
    bool exist();
    char *get();
    void prompt();
    void clear();

//bool is_mes();

protected:
    struct SFormatListe {

      char           *ptype;
      uint16         width;
      CSlfStr        text;
      CSlfStr        format;
      SFormatListe   *pnext;
    };

    CSlfStr mes_text;
    bool set_flag;
    status_t setFormatListe(char *pformat,SFormatListe **pformat_liste,
                              uint16 *nformat_liste);

};


#endif