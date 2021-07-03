#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfParGet.h"


//=================================================================    
//=================================================================    
// Konstruktor
//=================================================================    
//=================================================================    
CSlfParGet::CSlfParGet() {

    Status   = OKAY;

    pLogFile = 0;

}
CSlfParGet::CSlfParGet(CSlfLogFile *plogfile) {

    Status   = OKAY;

    pLogFile = plogfile;

}
//=================================================================    
//=================================================================    
// Destruktor 
//=================================================================    
//=================================================================    
CSlfParGet::~CSlfParGet() {

}
