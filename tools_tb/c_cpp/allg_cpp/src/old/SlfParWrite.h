// SlfParWrite.h
// Klasse zum Schreiben der Parameter
// 
// Beschreibung siehe SlfParRead.h

#ifndef SLF_PAR_WRITE
#define SLF_PAR_WRITE

 
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include "SlfStr.h"

#include "SlfParDef.h"
#include "SlfParData.h"



// class definition --------------------------------
class CSlfParWrite : public CSlfBasic
{

public:
  CSlfParWrite();
  ~CSlfParWrite();

  status_t writeParFile(char *par_file,CSlfParData *pdata);
private:

  std::ofstream     fs;         // Filestream
  CSlfParData      *pData;       // Datarnpointer

  status_t printGroup(slf::CStrV &strv);
  status_t printVar(slf::CStrV &strv);
};

#endif