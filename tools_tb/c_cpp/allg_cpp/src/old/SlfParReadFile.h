// SlfParRead.h
// Klasse zum Handeln der Parameterfiles
//
// Mit 
// #include datei.ext
// wird eine eingebettete Parameterdatei eingefügt

#ifndef SLF_PAR_READ_FILE
#define SLF_PAR_READ_FILE
//
 
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <deque>
#include "SlfBasicClass.h"
#include "SlfStr.h"
#include "SlfParDef.h"


class CSlfParReadFile : public CSlfBasic
{

public:

  enum EParReadFile {
    PAR_READ_FILE_OKAY,
    PAR_READ_FILE_END,
    PAR_READ_FILE_ERR
  };

  CSlfParReadFile();
  ~CSlfParReadFile();

  // Öffnet Parameterdatei
  status_t open(char *par_file);
  // Schliessen Parameterdatei
  status_t close(void);
  // Wirft n Zeichen weg
  status_t throwaway(uint16_t n);
  status_t throwaway_until_eol(void);
  // Liest die nächsten n-Zeichen aus Parameterdatei
  char     getChar(void);
  uint16_t get(char *pval, uint16_t n=1);
  uint16_t get(slf::CStr &string, uint16_t n=1);
  // Sucht bis char c oder string psuch
  // Schreibt den gefundenen string
  // Wenn gefunden, *pflag == true
  uint16_t get_until(slf::CStr &string, char c,bool *pflag);
  uint16_t get_until(slf::CStr &string, char *psuch,bool *pflag);
  // Sucht bis char c oder string psuch bis zum Ende der Zeile
  // Schreibt den gefundenen string
  // Wenn gefunden, *pflag == true
  uint16_t get_until_eol(slf::CStr &string, char c,bool *pflag);
  uint16_t get_until_eol(slf::CStr &string, char *psuch,bool *pflag);
  uint16_t get_until_eol(slf::CStr &string, char **ppsuch,uint16_t n,bool *pflag);
  // Zeigt die nächsten ite-Zeichen in Parameterdatei
  char     peekChar(uint16_t ipos=0);
  //uint16_t peek(char *pval, uint16_t n=1);
  // Schiebt den Text-string zurück in buffer
  status_t putChar(char c);
  status_t put(char *pval);
  status_t put(slf::CStr &string);
  // Status Einlesen
  EParReadFile getReadStatus(void){return ReadStatus;}
  // Sagt wieviele Zeiichen bis zur gesuchten Zeichenfolge
  EParReadFile length_until(char *psuch,uint16_t *plength);
  // Sucht bis einer der Zeichen aus pisuch bzw. csuch
  // gefunden bzw. nicht mehr gefunden
  // return 0 bedeutet das nächste Zeichen ist oder ist nicht das gesuchte 
  //          bzw. eins der gesuchten zeichen
  // _eol bedeutet das bis zum Ende der Zeile gesucht wird
  uint16_t search_until(char **ppisuch,uint16_t n);
  uint16_t search_until_eol(char **ppisuch,uint16_t n,bool *pflag);
  uint16_t search_until(char *pisuch);
  uint16_t search_until_eol(char *pisuch,bool *pflag);
  uint16_t search_until(char csuch);
  uint16_t search_until_eol(char csuch,bool *pflag);
  uint16_t search_until_not(char **pisuch,uint16_t n);
  uint16_t search_until_not_eol(char **pisuch,uint16_t n,bool *pflag);
  uint16_t search_until_not(char *pisuch);
  uint16_t search_until_not_eol(char *pisuch,bool *pflag);
  uint16_t search_until_not(char csuch);
  uint16_t search_until_not_eol(char csuch,bool *pflag);

  uint16_t search_throw_until(char **ppisuch,uint16_t n);
  uint16_t search_throw_until(char *pisuch);
  uint16_t search_throw_until(char csuch);
  uint16_t search_throw_until_not(char **pisuch,uint16_t n);
  uint16_t search_throw_until_not(char *pisuch);
  uint16_t search_throw_until_not(char csuch);

  const char * getFileName(void);
  void getZeilenInfo(slf::CStr &src);

private:

  struct SParReadFile {
    uint32_t          iZeile;     // Zeile beginnend bei 1
    slf::CStr           ParFile;    // Parametername
    std::ifstream     fs;         // Filestream
    SParReadFile      *pNext;
  };

  SParReadFile *pF;              // pointer rekursiver Struktur
  uint8_t      nF;               // Anzahl der aktuellen geschachtellten Parameterdateien
                                 // nF = 1 => erstes Parameterfile geöffnet

  EParReadFile ReadStatus;

  std::deque<char> buffer;

  EParReadFile read_in_buffer(uint16_t nmin); // Read into buffer minimum nmin Characters
  EParReadFile read_next_line(slf::CStr &line); // Read next line from actual file
  void         set_err_mess(void);            // set Message if error while reading occurs
  status_t     check_for_include(slf::CStr &line,bool &includeflag); // check and handle include - File
  status_t     open_inlude(const char *file_name);     // open and hanle include file
  bool         check_end_of_include(void);       // check at of File if go back to parent file
};
#endif