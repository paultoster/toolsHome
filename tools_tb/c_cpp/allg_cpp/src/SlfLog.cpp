//
// HlpLog.cpp
//
//*************************************************************************
// File:             HlpLog.cpp      
// Name:             Thomas Berthold/CTZS/Continental TEVES AG & CO. oHG
// Date:             16.08.2015
// Change:           16.08.2015 Overtake from SlfSys.h/SlfSys.cpp        
// 
//*************************************************************************
// Kurzbeschreibung: 
//
// System Funktionen: 
//*************************************************************************
#include "SlfLog.h"
#include <sstream>

namespace slf
{
  void CLog::openFile()
  {
    if( !CLog::OfsLog.is_open() )
    {
      OfsLog.open(CLog::FullLogFileName,std::ofstream::out);
      if( FlagInfoPrintToScreen )
      {
        if( !OfsLog.is_open() )
        {
          std::cout << "Error opening file:" << CLog::FullLogFileName;
          throw "Logfile konnte nicht geöffnet werden";
        }
        else
        {
          std::cout << "Logfile: open  <" << FullLogFileName << ">"<<std::endl;
        }
      }
    }

  }
  void  CLog::write(const char *text)
  {
    SLog << text;

    if(FlagLogFileWriting    ) openFile();
    if( OfsLog.is_open()     ) OfsLog << text;
    
    if( FlagLogPrintToScreen ) std::cout << text;
    FlagLogValueWritten = true;
  }
  void  CLog::write(const std::string &text)
  {
    CLog::write(text.c_str());
  }
  void CLog::write(char letter,std::size_t n) // write letter n-times
  {
    std::string text;
    for(std::size_t i=0;i<n;++i) text += letter;
    CLog::write(text.c_str());
  }
  void CLog::write(std::size_t val)
  {
    std::stringstream out;
    out << val;
    CLog::write(out.str().c_str());
  }
  void CLog::write(int val)
  {
    std::stringstream out;
    out << val;
    CLog::write(out.str().c_str());
  }
  void CLog::write(float val)
  {
    std::stringstream out;
    out << val;
    CLog::write(out.str().c_str());
  }
  void CLog::write(double val)
  {
    std::stringstream out;
    out << val;
    CLog::write(out.str().c_str());
  }
  void CLog::writeEndL()
  {
    SLog << std::endl;

    if (FlagLogFileWriting) openFile();
    if( OfsLog.is_open()     ) 
    {
      OfsLog << std::endl;
    }
    if( FlagLogPrintToScreen ) std::cout << std::endl;
  }
  void CLog::writeEndL(const char *text)
  {
    CLog::write(text);
    CLog::writeEndL();
  }
  void CLog::writeEndL(const std::string &text)
  {
    CLog::write(text);
    CLog::writeEndL();
  }
  void CLog::writeEndL(char letter,std::size_t n)
  {
    CLog::write(letter,n);
    CLog::writeEndL();
  }
  void CLog::writeEndL(std::size_t val)
  {
    CLog::write(val);
    CLog::writeEndL();
  }
  void CLog::writeEndL(int val)
  {
    CLog::write(val);
    CLog::writeEndL();
  }
  void CLog::writeEndL(float val)
  {
    CLog::write(val);
    CLog::writeEndL();
  }
  void CLog::writeEndL(double val)
  {
    CLog::write(val);
    CLog::writeEndL();
  }
  void  CLog::writeMessage(const CMessage &mess)
  {
    std::size_t i0 = 0;
    for (std::size_t i=0; i < mess.LogVec.size(); ++i)
    {
      if ((i == 0) || (i == i0))
      {
        std::stringstream out;
        out << "Logid : " << mess.LogVec[i].id << "=============================================================================";
        writeEndL(out.str());
      }
      i0 = i;
      writeEndL(mess.LogVec[i].logText);
    }
  
    for (std::size_t i=0; i < mess.ErrVec.size(); ++i)
    {
      if( mess.ErrVec[i].newFlag )
      { 
        std::stringstream out;
        out << "Errid : " << mess.ErrVec[i].id << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
        writeEndL(out.str());
        
        std::stringstream out1;
        out1 << "ErrNum : " << mess.ErrVec[i].errNo;
        writeEndL(out1.str());
        writeEndL(mess.ErrVec[i].errText);
      }
    }
  }
}