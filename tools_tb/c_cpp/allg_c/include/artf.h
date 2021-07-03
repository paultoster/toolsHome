/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      05.03.03   TBert aus simSys                                */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             artf.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            simSys.c
*************************************************************************
* Kurzbeschreibung: 
*
* read text file Funktionen
************************************************************************/
/************************************************************************

artf_s *artf_new(void);
-------------------------------------------------------------------------
- Struktur löschen                                                      -
-------------------------------------------------------------------------
void artf_delete(artf_s *p);
-------------------------------------------------------------------------
- datei zum Lesen öffnet                                                -
-------------------------------------------------------------------------
IERR_T artf_open(artf_s *p,CHAR *input_file);
-------------------------------------------------------------------------
- datei zum Lesen öffnet                                                -
-------------------------------------------------------------------------
IERR_T artf_close(artf_s *p);
-------------------------------------------------------------------------
- nächste Zeile einlesen                                                -
-------------------------------------------------------------------------
IERR_T artf_read_line(artf_s *p,astrs_s *ps_text);

  return 0, wenn Zeile eingelesen
         END_OF_FILE_ERR, wenn DAteie zuende
         FILE_NOT_OPENED_ERR, wenn Datei nicht geöffnet
         STRUCT_NOT_DEFINED_ERR, wenn p nicht initialisiert;
************************************************************************/
#ifndef artf_h_included

#define artf_h_included

#include "definer.h"
#include "astrs.h"


typedef
struct tag_artf_s {
    astrs_s        *ps_name;  // Dateiname
    FILE                *p_fid;      // Dateizeiger
    UINT16_T            izeile;     // aktuelle  Zeile
    UINT16_T            icol;       // aktuelles Zeichen in Zeile
    IERR_T              ierr;
    struct tag_artf_s   *p_next;

} artf_s;


#ifdef __cplusplus
  extern "C" {
#endif

artf_s *artf_new(void);
void artf_delete(artf_s *p);
IERR_T artf_open(artf_s *p,CHAR *input_file);
IERR_T artf_close(artf_s *p);
IERR_T artf_read_line(artf_s *p,astrs_s *ps_text);


#ifdef __cplusplus
  }
#endif

#endif