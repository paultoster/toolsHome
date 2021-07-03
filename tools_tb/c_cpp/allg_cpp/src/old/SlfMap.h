// SlfMap.h
// liest ein ascii-Mapfile ein
// und legt eine Vektorstruktur an mit Adresse und Name
#ifndef SLF_MAP
#define SLF_MAP
//
 
#include <stdio.h>
#include <stdlib.h>
#include "SlfStr.h"


struct SSlfMapReadLine {

    slf::CStr                     Tline;
    uint32                      Zeile;
    slf::CStr                     ParFile;
    FILE                        *Fid;
	char                        ret;
	uint8                       ReadFlag;
};

class CSlfMap {

public:
    CSlfMap();
    ~CSlfMap();

    status_t read(char *par_file);

    uint32   getLenErrText(void) {return ErrText.getLen();}
    char *   getErrText(void) {return ErrText.c_str();}
    void     resetErrText(void) {ErrText.clear();}

private:

    status_t        Status;
    slf::CStr         ErrText;


	char readLine(slf::CStr &tline   // Zeilentext
                 ,SSlfMapReadLine *pstruct    // Hilfsstruktur
                 );

};

#endif