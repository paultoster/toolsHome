#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfMap.h"

#define CIN_LENGTH_JSJS 253

//=================================================================    
//=================================================================    
// Konstruktor CDsParFile
//=================================================================    
//=================================================================    
CSlfMap::CSlfMap() {

    Status = OKAY;

}
//=================================================================    
//=================================================================    
// Destruktor CDsParFile
//=================================================================    
//=================================================================    
CSlfMap::~CSlfMap() {


}

//=================================================================    
//=================================================================    
// read: Datei lesen
//=================================================================    
//=================================================================    
status_t CSlfMap::read(char *par_file) {

    
    slf::CStr          tline;
    SSlfMapReadLine  rstruct;
	slf::CStrV         vtline;
	
	slf::CStrV         vadress;
	slf::CStrV         vname;

	uint8            stat=0;

    // Parameterfile öffnen
    //=====================
    if( !SlfSysExistFile(par_file) ) {
        Status = NOT_OKAY;
        ErrText.catFormat("Error in CSlfMap::read(); Parameterfile <%s> kann nicht gefunden werden\n"
                         ,par_file);
        return Status;
    }

    rstruct.Fid = fopen(par_file,"r");

    if( rstruct.Fid == 0 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("Error in CSlfMap::read(); Mapfile <%s> kann nicht gelesen werden\n"
                         ,par_file);

        return Status;
    }


    rstruct.Zeile    = 0;
    rstruct.ParFile  = par_file;
	rstruct.ret      = 0;
    rstruct.ReadFlag = 1;

    while(rstruct.ReadFlag) {

        
        // nächste Zeile einlesen
        //=======================
        if( readLine(tline,&rstruct) == EOF )
            rstruct.ReadFlag = 0;

		if( Status != OKAY )
			return Status;

        rstruct.Tline = tline;

		tline.elimAnfEndC();

		if( tline.getLen() > 0 ) {

			switch(stat) {

			case 0: // Anfang suchen 

				// Anfang suchen
				if( tline.find("GLOBAL SYMBOLS:") != SLF_STR_NPOS )
					stat = 1;
				break;

			case 1:

				// Ende suchen
				// Anfang suchen
				if( tline.find("GLOBAL SYMBOLS:") != SLF_STR_NPOS )
					rstruct.ReadFlag = 0;

				// Zeile zerlegen
				SlfStrVSplit( vtline, tline.c_str()," ");

				if( vtline.getNrows() > 2 )
					stat = 10;

				vadress.append(vtline.get_str(0));
				vname.append(vtline.get_str(1));
				break;
			}
		}        

    }

    fclose(rstruct.Fid);


    return Status;
}
//=================================================================    
//=================================================================    
// readLine nächste Zeile auslesen
//=================================================================    
//=================================================================    
char CSlfMap::readLine(slf::CStr &tline           // Zeilentext
                         ,SSlfMapReadLine *pstruct     // Hilfsstruct
                         ) {     


    char   cin[CIN_LENGTH_JSJS+2];
    uint16 i = 0;


	// Nächste Zeile einlesen
	//=======================
	i = 0;

	while( 1 ) {

		cin[i]=fgetc(pstruct->Fid);
		if( cin[i] == EOF ) {
			cin[i] = '\0';
			tline.cat(cin);
			pstruct->Zeile += 1;
			pstruct->ret = EOF;
			break;

		} else if( cin[i] == '\n' ) {

			cin[i] = '\0';
			tline.cat(cin);
			pstruct->Zeile += 1;
			break;

		} else if( i == CIN_LENGTH_JSJS ) {

			cin[i+1] = '\0';
			tline.cat(cin);
			i = 0;
			break;
		} else if( cin[i] == '\t' ) {

			cin[i] = ' ';
			i++;

		} else {

			i++;

		}
	}

    return pstruct->ret;
}
