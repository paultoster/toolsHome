/******************************************************************************
 * @file  ReadCanAscii2.h
 *
 * @author  Thomas Berthold
 * @date    06/10/2014
 *
 * @brief read a Can-Ascii file 
 *
 * @subversion_tags (not part of doxygen)
 *   $LastChangedBy: berthold $
 *   $LastChangedRevision: 38987 $
 *   $LastChangedDate: 2014-02-06 15:49:32 +0100 (Do, 06 Feb 2014) $
 *   $URL: http://frd2ahjg/svn/tze/Departments/EnvironmentPerception/Components/ArbiDev2PathTask/src/Application/ArbiDev2PathMain.cpp $
******************************************************************************/
#ifndef readcanascii2_h_included
#define readcanascii2_h_included


#include "SlfBasic.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct RecBuf2_tag 
{
  double           time;
  uint8_t          channel;
  size_t           id;
  uint8_t          receive;
  uint8_t          len;
  uint8_t          data[8];
  struct RecBuf2_tag      *pNext;
} RecBuf2_t;

typedef struct Rec2Ids_tag
{
  size_t  id;
  uint8_t channel;
} Rec2Ids_t;

status_t readCanAscii2_read(FILE *fp, RecBuf2_t **pprecBuf, uint32_t *precBufCount, Rec2Ids_t *precIds, size_t nIds, char *errtext, size_t lerrtext);
void readCanAscii2_delete(RecBuf2_t *precBuf);

#ifdef __cplusplus
}
#endif

#endif
