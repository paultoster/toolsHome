//
// HlpSys.h
//
//*************************************************************************
// File:             HlpSys.h        
// Name:             Thomas Berthold/CTZS/Continental TEVES AG & CO. oHG
// Date:             16.08.2015
// Change:           16.08.2015 Overtake from SlfSys.h/SlfSys.cpp        
// 
//*************************************************************************
// Kurzbeschreibung: 
//
// System Funktionen: 
//*************************************************************************
#ifndef HLP_SYS_H
#define HLP_SYS_H


#include <string>
#include <vector>

namespace Hlp
{

	//        actual path
	//---------------------------------
	void  SysGetActPath(std::string &actpath);
	//
	//        exist file
	//---------------------------------
	bool  SysExistFile(char *file);
	bool  SysExistFile(std::string &file);
	//
	//        exist path
	//---------------------------------
	bool  SysExistPath(char *path);
	bool  SysExistPath(std::string &path);
}
#endif