/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      19.08.04   TBert aus simSys                                */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             aasap2.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            keine
*************************************************************************
* Kurzbeschreibung: 
*
* Liest asap2-File ein. und gibt Struktur mit eingelesenen Daten zurück
* A
************************************************************************/
/************************************************************************

************************************************************************/

#include "aasap2.h"
#include "astr.h"
#include "astrs.h"
#include "amem.h"
#include "asys.h"
#include "artf.h"


static artf_s *ps_file_aasap2   = NULL;
#if 0
static astrv_s *pv_list_aasap2  = NULL;
static astrs_s *ps_token_aasap2_1 = NULL;
#endif
static char *p_token_aasap2_1     = NULL;
static char *p_token_aasap2_2     = NULL;

//#define AASAP2_COPY(txt,lentxt) strncpy(txt,p_token_aasap2_1,MIN(strlen(p_token_aasap2_1),lentxt+1))
#define AASAP2_COPY(txt,lentxt) if(strlen(p_token_aasap2_1)<lentxt)strncpy(txt,p_token_aasap2_1,strlen(p_token_aasap2_1)+1);else{strncpy(txt,p_token_aasap2_1,lentxt);*(txt+lentxt)='\0';}
#define AASAP2_DEBUG 0


#include "aasap2_functions.c"


/*-------------------------------------------------------------------------*/
/*- Neue Struktur anlegen                                                 -*/
/*-------------------------------------------------------------------------*/
aasap2_s *aasap2_new(void) {

    aasap2_s *p=NULL;

    /* Message handling initialize don not force init and only display*/
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
    amsg_init("aasap2",FALSE,FALSE,"",TRUE);
#endif

    /* Memory für die Struktur p */
    amem_mem_g((void **)&p,1,sizeof(aasap2_s));
    if( p == NULL ) {
        amsg_set_status("\naasap2_new_err: no memory for aasap2_s p");
        return p;
    } else {
        p->ps_file_name = astrs_new();
        p->ps_project   = NULL;
        p->ps_val       = NULL;
    }

    /* Initialisierter Popinter zurückgeben */
    return p;
}
/*-------------------------------------------------------------------------*/
/*- Struktur löschen                                                       -*/
/*-------------------------------------------------------------------------*/
void aasap2_delete(aasap2_s *p) {



    if( p->ps_file_name )
        astrs_delete(p->ps_file_name);

    if( p->ps_project != NULL )
        aasap2_delete_project(p->ps_project);

    if( p->ps_val != NULL )
        aasap2_delete_val(p->ps_val);

    amem_free_g((void **)&p);

    amsg_done("aasap2");

}

/*-------------------------------------------------------------------------*/
/*- read asap-File                                                        -*/
/*-------------------------------------------------------------------------*/
STATUS_T aasap2_read(aasap2_s *p,CHAR *p_input_file) {


    artf_s *ps_file = artf_new();

    /* existiert das input-File */
    if( !asys_exist_file(p_input_file) ) {
        
        amsg_set_status("ERROR in <aasap2_read>: File %s does not exist",p_input_file);
        return NOT_OK;
    }

    /* File öffnen */
    if( artf_open(ps_file,p_input_file) != OK ) {
        
        amsg_set_status("ERROR with <artf_open> in <aasap2_read>");
        artf_close(ps_file);
        artf_delete(ps_file);
        return NOT_OK;
    }

    /* Name übergeben */
    astrs_scpy(p->ps_file_name,ps_file->ps_name);

    /* an globale Variable übergeben */
    ps_file_aasap2 = ps_file;

    /* Projekt einlesen */
    if( aasap2_read_start(p) != OK ) {

        artf_close(ps_file);
        artf_delete(ps_file);
        ps_file_aasap2 = NULL;
        return NOT_OK;
    }

    /* File schliessen */
    artf_close(ps_file);
    artf_delete(ps_file);

    /* globale Variablen zurücksetzen */
    ps_file_aasap2 = NULL;

    return OK;
}
/*-------------------------------------------------------------------------*/
/*- set values                                                            -*/
/*-------------------------------------------------------------------------*/
STATUS_T aasap2_val(aasap2_s *p) {

    aasap2_modul_s *ps_modul = NULL;

    if(  p->ps_project           == NULL 
      || p->ps_project->ps_modul == NULL 
      ) {
        amsg_set_status("ERROR in <aasap2_val>: no project, or no modul found");
        return NOT_OK;
    }

    ps_modul = p->ps_project->ps_modul;

    while( ps_modul != NULL ) {

        if( aasap2_val_modul(ps_modul,p) != OK ) {
            amsg_set_status("ERROR with <aasap2_val_modul> in <aasap2_val>");
            return NOT_OK;
        }

        ps_modul = ps_modul->ps_next;
    }


    return OK;
}