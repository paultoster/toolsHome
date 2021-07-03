//
// SlfLog.h
//
// Help Slf LogFile Handling
// 
// Anlegen mit        slf::CLog Log("dateiname.log");            Das gleichzeitige printen an den Bild schirmm ist ausgeschaltet
// oder               slf::CLog Log("dateiname.log",true/false); Damit wird das gleichzeitige printen an den Bildschirm an oder ausgeschaltet
//
//                    Log.setInfoPrintToScreen(false);           Damit können die Infos am Bildschirm unterdrückt werden
//                    Log.setLogPrintToScreen(false);            Damit können auch alle Logs am Bildschirm unterdrückt werden
//                    Log.write(Text);                           Schreiben eines Textes ohne carriage return
//                    Log.writeEndl();                           Schreiben eines carrige return
//                    Log.writeEndl(Text);                       Schreiben eines Textes mit carriage return
//                    Log.writeMessage(const CMessage &Messagge);
//                    Log.close();                               Schliessen der Datei


#ifndef SLF_LOG_H_INCLUDED
#define SLF_LOG_H_INCLUDED

#include <string>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include "SlfStr.h"
#include "SlfSys.h"
#include "SlfMessage.h"
namespace slf
{
  //====================================================================================
  //====================================================================================
  // Log Klasse
  //====================================================================================
  //====================================================================================
  class CLog
  {
  private:
    CStr          LogFileName;
	  CStr          FullLogFileName;
    bool          FlagInfoPrintToScreen;
    bool          FlagLogPrintToScreen;
    bool          FlagLogValueWritten;
    bool          FlagLogFileWriting;

    std::ofstream OfsLog;
    std::stringstream SLog;

  public:
    CLog(void)
    {
      FlagLogFileWriting = false;
      FlagLogPrintToScreen = false;
      FlagInfoPrintToScreen = true;
      FlagLogValueWritten = false;

    }
    CLog(const char *filename)
    {
	    LogFileName           = filename;
      SysGetActPath(FullLogFileName);
      FullLogFileName       += filename;
      FlagLogFileWriting = true;
      FlagLogPrintToScreen  = false;
      FlagInfoPrintToScreen = true;
      FlagLogValueWritten   = false;

    }
    CLog(const char *filename, bool printtoscreen)
    {
	    LogFileName           = filename;
	    SysGetActPath(FullLogFileName);
      FullLogFileName       += filename;
      FlagLogFileWriting = true;
      FlagLogPrintToScreen  = printtoscreen;
      FlagInfoPrintToScreen = true;
      FlagLogValueWritten   = false;

    }
    CLog(bool printtoscreen)
    {
      FlagLogFileWriting = false;
      FlagLogPrintToScreen = printtoscreen;
      FlagInfoPrintToScreen = true;
      FlagLogValueWritten = false;

    }
    ~CLog()
    {
      closeFile();
    }
    void        closeFile() 
    {
      if( OfsLog.is_open() )
      {
        OfsLog.close();
        if( FlagInfoPrintToScreen )
        {
          std::cout << "Logfile: close <"<<FullLogFileName.c_str()<<">"<<std::endl;
        }
      }
    }
    const char *getLogFileName(void) { if (FlagLogFileWriting) return LogFileName.c_str(); else return ""; }
    const char *getLogFileFullName(void) { if (FlagLogFileWriting) return FullLogFileName.c_str(); else return "";}
    const char *getLogText(void) { if (FlagLogValueWritten) return SLog.str().c_str(); else return ""; }
    bool        isLogFileOpen(void) {return OfsLog.is_open();}
    bool        hasLogFileValues(void) {return FlagLogValueWritten;}
    void        resetLogFileValues(void) { SLog.clear(); FlagLogValueWritten = false;}
    void        setLogFileName(const char *logfilename) {
      if (!OfsLog.is_open()){
        LogFileName = logfilename; SysGetActPath(FullLogFileName); FullLogFileName += logfilename; FlagLogFileWriting=true;}}
    void        setLogPrintToScreen(bool flag) {FlagLogPrintToScreen = flag;}
    void        setInfoPrintToScreen(bool flag) {FlagInfoPrintToScreen = flag;}

    void        write(const char *text);
    void        write(const std::string &text);
    void        write(char letter,std::size_t n); // write letter n-times
    void        write(std::size_t val);
    void        write(int val);
    void        write(float val);
    void        write(double val);
    void        writeEndL();                      // with carrige return
    void        writeEndL(const char *text);
    void        writeEndL(const std::string &text);
    void        writeEndL(char letter,std::size_t n);
    void        writeEndL(std::size_t val);
    void        writeEndL(int val);
    void        writeEndL(float val);
    void        writeEndL(double val);
    void        writeMessage(const CMessage &mess);

  private:
    void        openFile();

  };

}

#endif