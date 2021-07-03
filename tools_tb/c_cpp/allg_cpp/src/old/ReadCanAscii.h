/******************************************************************************
 * @file  ReadCanAscii.h
 *
 * @author  Thomas Berthold
 * @date    22/2/2014
 *
 * @brief read a Can-Ascii file 
 *
 * @subversion_tags (not part of doxygen)
 *   $LastChangedBy: berthold $
 *   $LastChangedRevision: 38987 $
 *   $LastChangedDate: 2014-02-06 15:49:32 +0100 (Do, 06 Feb 2014) $
 *   $URL: http://frd2ahjg/svn/tze/Departments/EnvironmentPerception/Components/ArbiDev2PathTask/src/Application/ArbiDev2PathMain.cpp $
******************************************************************************/
#ifndef readcanascii_h_included
#define readcanascii_h_included

#include "identline.h"

typedef std::vector<std::string> StrVecT;
typedef std::list<std::string>   StrListT;
struct SIdCh
{
  unsigned char    channel;
  size_t           id;
};
typedef std::vector<struct SIdCh> CVecIdCh;

struct SRecBuf
{
  double           time;
  unsigned char    channel;
  size_t           id;
  unsigned char    receive;
  unsigned char    len;
  unsigned char    data[8];
};
typedef std::list<struct SRecBuf> CRecBufListT;

typedef std::list<std::string>    CStringListT;
typedef std::list<double>         CDoubleListT;

struct SDbcBuf
{
  bool           MesRead;
  bool           IsMultiplexer;       // is Multiplexsignal of ID
  int            MultiplexIndex;     // -1 : no multiplex Signal 0,... Multiplexnumber
  std::string    Name;
  std::string    Unit;
  size_t         id;
  unsigned char  BitStart;
  unsigned char  BitLen;
  int            Sign;
  int            Format;
  double         Scale;
  double         Offset;
  CDoubleListT   TimeList;
  CDoubleListT   VecList;
};
typedef std::list<struct SDbcBuf> CDbcBufListT;



bool ReadDbcFile(const char* dbc_filename, const char* sigformat, CStringListT      *pSigNameList, CDbcBufListT      *pDbcBufList);
/*-----------------------------------------------------------------------------------------------------------------------*/
/* bool ReadDbcFile()                                                                                                  */
/* Ascii-Datei einlesen                                                                                                  */
/* std::string asc_file_name            Dateiname Acsii-CAN-Daten                                                        */
/* CVecIdCh                             Vektor mit Identifier und channel                                                */
/* CRecBufListT *pRecBufList            Buffer Liste mit den eingelesenen Werten                                         */
/* Rückgabe:                                                                                                             */
/* tend                                 Endzeitpunkt                                                                     */
/*-----------------------------------------------------------------------------------------------------------------------*/
double ReadDatFile(std::string asc_file_name,CVecIdCh *pVecIdCh,CRecBufListT *pRecBufList);
/*-----------------------------------------------------------------------------------------------------------------------*/
/* double ReadDatFile()                                                                                                  */
/* Ascii-Datei einlesen                                                                                                  */
/* std::string asc_file_name            Dateiname Acsii-CAN-Daten                                                        */
/* unsigned char channel                Channelnummer der eingelsen werden soll, wenn alle, dann =0 setzen               */
/* size_t *pident                       Array mit Identifier                                                             */
/* size_t nident                        Anzahl der Identifier                                                            */
/* CRecBufListT *pRecBufList            Buffer Liste mit den eingelesenen Werten                                         */
/* Rückgabe:                                                                                                             */
/* tend                                 Endzeitpunkt                                                                     */
/*-----------------------------------------------------------------------------------------------------------------------*/
double ReadDatFile(std::string asc_file_name,unsigned char channel,size_t *pident,size_t nident,CRecBufListT *pRecBufList);

bool SetDbcValues(CRecBufListT *pRecBufList,CDbcBufListT      *pDbcBufList);

#endif