#ifndef SLF_LOG_FILE_H_INCLUDED
#define SLF_LOG_FILE_H_INCLUDED
//
#include <stdio.h>
#include "SlfStr.h"
 
class CSlfLogFile {
public:

    CSlfLogFile();
    CSlfLogFile(const char *file_name);
    ~CSlfLogFile();

    status_t open();
    status_t open(slf::CStr &file_name);
    status_t open(const char *file_name);
    status_t opennew();
    status_t opennew(slf::CStr &file_name);
    status_t opennew(const char *file_name);

    status_t writeEnd(slf::CStr &text);  // write with carrige return at end
    status_t writeEnd(const char *text);     // write with carrige return at end

    status_t writeLine(const char *text,uint16_t l); //// write characters l-times with carrige return at end

    status_t write(slf::CStr &text);  // write with out carrige return at end
    status_t write(const char *text);     // write with out carrige return at end

    const char * getLogFileName(void) {return FileName.c_str();}
    const char * getLogFileFullName(void) {return FullFileName.c_str();}
    bool   isOpen(void) {return FlagFileOpen;}
    bool   hasValue(void) {return FlagValueWritten;}
    FILE   *getFid(void){return Fid;}

    status_t close();
    status_t closeFinal();
protected:

    status_t Status;
    slf::CStr FileName;
	slf::CStr FullFileName;
    bool    FlagFileOpen;
    bool    FlagValueWritten;
    FILE    *Fid;
};

#endif