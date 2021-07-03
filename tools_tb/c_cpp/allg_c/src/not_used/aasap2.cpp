/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      19.08.04   TBert                                           */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             aasap2.cpp        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            keine
*************************************************************************
* Kurzbeschreibung: 
*
* Liest asap2-File ein. und gibt Struktur mit eingelesenen Daten zurück
* 
************************************************************************/
/************************************************************************

************************************************************************/

#include "aasap2.h"
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
    #include "amsg.h"
#endif



/*-------------------------------------------------------------------------*/
/*- Konstruktor                                                           -*/
/*-------------------------------------------------------------------------*/
aasap2_c::aasap2_c(void) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
    /* Message handling initialize don not force init and only display*/
    amsg_init("aasap2",FALSE,FALSE,"",TRUE);
#endif
    /* Init Filename-Structure */
    file_name = 0;

    /* Project-Strukture */
    ps_project = new (aasap2_project_s);

    ps_project->name[0]    = '\0';
    ps_project->comment[0] = '\0';
    ps_project->ps_header  = NULL;
    ps_project->ps_modul   = NULL;

    /* Val-Strukture */
    ps_val = NULL;

    /* read_next Variablen */
    n_buffer = 0;
    n_zeile = 0;
    n_index = 0;
    i_index = 0;


    read_status = AASAP2_SEARCH_NEXT_TOKEN;

    start_done_flag = 0;
    end_done_flag   = 0;

#if AASAP2_DEBUG

    out_fid = NULL;
    n_block = 0;
#endif

}
/*-------------------------------------------------------------------------*/
/*- Destruktor                                                           -*/
/*-------------------------------------------------------------------------*/
aasap2_c::~aasap2_c(void) {

    /* Val-Strukture */
    if( ps_val != NULL )
        delete_val(ps_val);

    /* Project-Strukture */
    if( ps_project != NULL )
        delete_project(ps_project);

	if( file_name )
		delete []file_name;

    delete ps_project;

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
    /* Message handling */
    amsg_done("aasap2");
#endif
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::delete_val(aasap2_val_s *ps_val) {

    aasap2_val_s *ps;

    while( ps_val != NULL ) {

        ps = ps_val->ps_next;

        delete ps_val;

        ps_val = ps;
    }

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::delete_project(aasap2_project_s *ps_project) {


    if( ps_project->ps_modul != NULL ) 
             delete_modul(ps_project->ps_modul);

    if( ps_project->ps_header != NULL )
        delete_projectheader(ps_project->ps_header);

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::delete_projectheader(aasap2_projectheader_s *ps_header) {

    delete ps_header;

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::delete_modul(aasap2_modul_s *ps_modul) {

    aasap2_modul_s *ps;

    while( ps_modul != NULL ) {

        if( ps_modul->ps_characteristic != NULL )
            delete_characteristic(ps_modul->ps_characteristic);

        if( ps_modul->ps_conversion != NULL )
            delete_conversion(ps_modul->ps_conversion);

        ps = ps_modul->ps_next;

        delete ps_modul;

        ps_modul = ps;

    }

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::delete_characteristic(aasap2_characteristic_s *ps_char) {

    aasap2_characteristic_s *ps;

    while( ps_char != NULL ) {

        ps = ps_char->ps_next;

        delete ps_char;

        ps_char = ps;
    }

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::delete_conversion(aasap2_conversion_s *ps_conv) {

    aasap2_conversion_s *ps;

    while( ps_conv != NULL ) {

        ps = ps_conv->ps_next;

        delete ps_conv;

        ps_conv = ps;
    }

    return;
}