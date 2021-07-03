/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      05.03.03   TBert aus simSys                                */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             artf.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            simSys.c
*************************************************************************
* Kurzbeschreibung: 
*
* read text file Funktionen
************************************************************************/
/************************************************************************

************************************************************************/

#include "artf.h"
#include "astr.h"
#include "astrs.h"
#include "amem.h"
#include "amsg.h"

/*-------------------------------------------------------------------------*/
/*- Neue Struktur anlegen                                                 -*/
/*-------------------------------------------------------------------------*/
artf_s *artf_new(void) {

    artf_s *p=NULL;

    /* Memory für die Struktur p */
    amem_mem_g(&p,1,sizeof(artf_s));
    if( p == NULL ) {
        amsg_set_status("\nartf_new_err(1): no memory for artf_s p");
        return p;
    } else {
        p->izeile = 0;
        p->icol   = 0;
        p->ierr   = OK;
        p->p_fid  = NULL;
        p->ps_name= astrs_new();
        p->p_next = NULL; 
    }

    /* Initialisierter Popinter zurückgeben */
    return p;
}
/*-------------------------------------------------------------------------*/
/*- Struktur löschen                                                       -*/
/*-------------------------------------------------------------------------*/
void artf_delete(artf_s *p) {


    if( p->ps_name != NULL )
        astrs_delete(p->ps_name);

    if( p->p_fid != NULL )
        artf_close(p);

    amem_free_g(&p);
}

/*-------------------------------------------------------------------------*/
/*- datei zum Lesen öffnet                                                -*/
/*-------------------------------------------------------------------------*/

IERR_T artf_open(artf_s *p,CHAR *input_file) {

#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
  fopen_s(&(p->p_fid), input_file, "r");
  if (p->p_fid == NULL) {
#else
  if ((p->p_fid = fopen(input_file, "r")) == NULL) {
#endif
  

        astrs_s *ps_err=astrs_new();
        astrs_cat(ps_err,"artf_opopen(1): Datei <");
        astrs_cat(ps_err,input_file);
        astrs_cat(ps_err,"> kann nicht geöffnet werden");
        amsg_set_status(astrs_string(ps_err));
        astrs_delete(ps_err);
        p->ierr = UNABLE_TO_OPEN_FILE_ERR;
        return p->ierr;
    }
    if( astrs_cpy(p->ps_name,input_file) != 0 )
        return NOT_OK;

    return p->ierr;
}
/*-------------------------------------------------------------------------*/
/*- datei schliessen                                                      -*/
/*-------------------------------------------------------------------------*/

IERR_T artf_close(artf_s *p) {

    if( p->p_fid != NULL ) {

        fclose(p->p_fid);
        p->p_fid = NULL;

    }
    astrs_cpy(p->ps_name,"");

    return p->ierr;
}
/*-------------------------------------------------------------------------*/
/*- nächste Zeile einlesen                                                -*/
/*-------------------------------------------------------------------------*/

IERR_T artf_read_line(artf_s *p,astrs_s *ps_text) {

    char cin[2];

    cin[1] = '\0';

    if( ps_text == NULL )
        ps_text = astrs_new();

    astrs_cpy(ps_text,"");

    if( p == NULL ) {
        astrs_s *ps_err=astrs_new();
        astrs_cat(ps_err,"artf_read_line(1): ");
        astrs_cat(ps_err,STRUCT_NOT_DEFINED_TEXT);
        amsg_set_status(astrs_string(ps_err));
        astrs_delete(ps_err);
        p->ierr = STRUCT_NOT_DEFINED_ERR;
        return p->ierr;
    }

    if( p->p_fid == NULL ) {
        astrs_s *ps_err=astrs_new();
        astrs_cat(ps_err,"artf_read_line(1): ");
        astrs_cat(ps_err,FILE_NOT_OPENED_TEXT);
        amsg_set_status(astrs_string(ps_err));
        astrs_delete(ps_err);
        p->ierr = FILE_NOT_OPENED_ERR;
        return p->ierr;
    }

    while( (cin[0]=fgetc(p->p_fid)) != EOF ) {

        if( cin[0] == '\n' ) {
            p->izeile++;
            p->icol = 0;
            break;
        } else {
            astrs_cat(ps_text,cin);
            p->icol++;
        }
    }

    if( (cin[0] == EOF) && (astrs_len(ps_text) == 0) ) {
/*
        astrs_s *ps_err=astrs_new();
        astrs_cat(ps_err,"artf_read_line(2): ");
        astrs_cat(ps_err,END_OF_FILE_TEXT);
        amsg_set_status(astrs_string(ps_err));
        astrs_delete(ps_err);
*/
        p->ierr = END_OF_FILE_ERR;
        return p->ierr;
 
    }

    return p->ierr;
}