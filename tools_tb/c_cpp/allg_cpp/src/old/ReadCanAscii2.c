/******************************************************************************
 * @file  ReadCanAscii2.c
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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "ReadCanAscii2.h"

char *ReadCanAscii2StrTokR (char *s, const char *delim, char **save_ptr);

#define READ_CAN_ASCII2_ZERO       0
#define READ_CAN_ASCII2_DEC        10
#define READ_CAN_ASCII2_HEX        16

status_t readCanAscii2_read(FILE *fp, RecBuf2_t **pprecBuf, uint32_t *precBufCount, Rec2Ids_t *precIds, size_t nIds, char *errtext, size_t lerrtext)
{
  char  buf[100];
  char  *cp;
  uint8_t type = READ_CAN_ASCII2_ZERO;
  uint8_t flag;
  size_t  i;
  RecBuf2_t *precact = *pprecBuf;

  /* Zeilen einlesen */
  /*-----------------*/
  while(1) 
  {
    char *buf2;
    char *bp;

    bp = fgets(buf,sizeof(buf),fp);
    if( feof(fp) || (bp == NULL) ) break;
    
    if ((cp = strstr(buf, "\n\r")) != NULL) 
    {       
      *cp = '\0';
    }
    
    /* Erster Token */
    cp = ReadCanAscii2StrTokR (buf," ", &buf2);
    
    if( strcmp(cp,"date") == 0 ) 
    {
      ;
    } 
    else if( strcmp(cp,"base") == 0 ) 
    {
      cp = ReadCanAscii2StrTokR(NULL," ", &buf2);

      if(      strcmp(cp,"dec") == 0 ) type = READ_CAN_ASCII2_DEC;
      else if( strcmp(cp,"hex") == 0 ) type = READ_CAN_ASCII2_HEX;
      else                             sprintf_s(errtext,lerrtext,"readCanAscii2_read_error: unbekannter type %s\n",cp);
      continue;
    } 
    else if( (strcmp(cp,"internal") == 0) || (strcmp(cp,"Begin") == 0) || (strcmp(cp,"Start") == 0) )
    {
      continue;
    } 
    else 
    {

      RecBuf2_t rb;
      char   *id_str;
      char   *rx;
      char   *d;
      //int     i;
      char   *tp;
      char   *time_lasts;
      int    tsec;
      unsigned int tnsec;

      if(type == READ_CAN_ASCII2_ZERO) 
      {
        sprintf_s(errtext,lerrtext,"readCanAscii2_read_error: fehlender type \n");
        return NOT_OK;
      }

      /* Jetzt geht es los */
      rb.pNext = 0;

      /* Zeit */
      tp = ReadCanAscii2StrTokR(cp, ".", &time_lasts); if(tp == NULL) continue;

      tsec = atoi(tp);

      tnsec = 0;
      tp = ReadCanAscii2StrTokR(NULL, " ", &time_lasts); if(tp == NULL) continue;
      {
        //int i;
        for(i = 0; i < 9; i++) 
        {
          tnsec *= 10;
          if( isdigit(*tp) ) 
          {
            tnsec += (*tp) - '0';
            tp++;
          }
        }
      }
      rb.time = (double)tsec + (double)tnsec*1.e-9;

      /* remove newline */
      cp = ReadCanAscii2StrTokR(NULL, "\n", &buf2); if(cp == NULL) continue;

      /* get channel number */
      cp = ReadCanAscii2StrTokR(cp, " ", &buf2); if(cp == NULL) continue;
      rb.channel = atoi(cp);

      /* ident */
      cp = ReadCanAscii2StrTokR(NULL, " ", &buf2); if(cp == NULL) continue;
      id_str = cp;

      cp = ReadCanAscii2StrTokR(NULL, " ", &buf2); if(cp == NULL) continue;
      rx = cp;
      if( !((rx[0] == 'R') || (rx[0] == 'T')) || (rx[1] != 'x')) continue;

      cp = ReadCanAscii2StrTokR(NULL, " ", &buf2); if(cp == NULL) continue;
      d = cp;

      /* DLC */
      cp = ReadCanAscii2StrTokR(NULL, " ", &buf2); if(cp == NULL) continue;
      rb.len = atoi(cp);

      /* data */
      for(i = 0; i < rb.len; i++) {
        cp = ReadCanAscii2StrTokR(NULL, " ", &buf2); if(cp == NULL) break;
        rb.data[i] = (unsigned char)strtoul(cp,NULL,type);
      }
      {
        int len = strlen(id_str);
        if(len == 0) break;

        switch( id_str[len-1] ) 
        {
	      case 'h':
          { /* hex */
            char *cp;
	          char *id_lasts;

            ReadCanAscii2StrTokR(id_str,"_", &id_lasts);
            cp = ReadCanAscii2StrTokR(NULL, "h", &id_lasts);

            /* hex  */
            rb.id = (unsigned int)strtol(cp,NULL,16);
          }
          break;
        case 'x':
          rb.id = (unsigned int)strtol(id_str,NULL,type);
	    
          /* remove  */
          rb.id &= ~0xFF;
          break;
	      default:
          /* mode */
          rb.id = (size_t)strtol(id_str,NULL,type);
	        break;
        }
      }
      /* In die Liste kopieren */
      if( nIds == 0 )
      {
        flag = 1;  /* alle Daten einordnen */
      }
      else
      {
        flag = 0;
        for(i=0;i<nIds;++i)
        {
          if( (precIds[i].id == rb.id) && (precIds[i].channel == rb.channel) )
          {
            flag = 1;
            break;
          }
        }
      }
      if( flag )
      {
        RecBuf2_t *prb;
        prb = (RecBuf2_t *)malloc( sizeof(RecBuf2_t) );
        if( prb )
        {
          memcpy(prb,&rb,sizeof(RecBuf2_t));

          if( !*pprecBuf )
          {
            *pprecBuf          = prb;
            (*pprecBuf)->pNext = 0;
            *precBufCount      = 1;
			precact            = prb;
          }
          else
          {
			prb->pNext     = 0;
            precact->pNext = prb;
            precact        = prb;
            ++(*precBufCount);
          }
        }
      }
    }
  }

  return OKAY;
}
char *ReadCanAscii2StrTokR (char *s, const char *delim, char **save_ptr)
{
  char *token;

  if (s == NULL) s = *save_ptr;

  s += strspn (s, delim);
  if (*s == '\0')
  {
    *save_ptr = s;
    return NULL;
  }

  token = s;
  s = strpbrk (token, delim);
  if (s == NULL)
  {
    *save_ptr = strchr (token, '\0');
  }
  else
  {
    *s = '\0';
    *save_ptr = s + 1;
  }
  return token;
}
void readCanAscii2_delete(RecBuf2_t *precBuf)
{
  RecBuf2_t *p;
  while( precBuf )
  {
    p = precBuf->pNext;
    free((void *)precBuf);
    precBuf = p;
  }
}
