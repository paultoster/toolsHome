/* $JustDate::  2.03.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      03.03.03   TBert neu                                       */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             amsg.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            simMess.c
*************************************************************************
* Kurzbeschreibung: 
*
* Ausgabe von info- und status-messages:
*************************************************************************
STATUS_T amsg_init(char *p_indent_name,BOOL_T force_init_flag,
                   BOOL_T write_to_file_flag,char *file_name,
                   BOOL_T write_to_display_flag)

    init function
    input:
    p_indent_name           string      Kennzeichnungsname
    force_init_flag         TRUE/FALSE  Es wird aus dieser Funktion auf jeden Fall initialisert,
                                        wenn nicht schon mit diesen ident-Name geschehen
    write_to_file_flag      TRUE/FALSE  Soll msg in File geschrieben werden
    file_name               string      Message-file-name
    write_to_display_flag   TRUE/FALSE  Soll msg an display geschrieben werden

    init internal states
    if write_to_file_flag is set, file named file_name is opened to write
    all messages
    returns OK (=0) if okay and NOT_OK (=1) if not okay

STATUS_T amsg_done(char *p_ident_name);

    free internal states 
    close file if opened
    Wenn egal welcher ident_name dann amsg_done("") verwenden
    
STATUS_T amsg_set_status(char *p_format,...);

    set status-message (short message, error, ...)
    status-message will be copied into a message-string and log-file (new line is added)
    (get message-string with get function)
    p_format is used same as with printf

STATUS_T amsg_set_error(char *p_format,...);

    set error-message (short message, error, ...)
    error-message will be copied into a message-string and log-file (new line is added)
    (get message-string with get function)
    p_format is used same as with printf
    and
    status_error will be set to TRUE, as lang as amsg_res_msg() will aufgerufen

STATUS_T amsg_set_info(char *p_format,...);

    sets info-message (descriptions, ...)
    info-message will be copied into log-file (new line is added)
    p_format is used same as with printf

BOOL amsg_has_msg(void);

    amsg has set a message if true (1)

CHAR *amsg_get_msg(void);

    get status-message for example from master-programm, to send it to specified output-display

STATUS_T amsg_get_error_status(void);

    get error-status for example from master-programm

STATUS_T amsg_res_msg(void);

    resets status-message, use after displaying status-message


************************************************************************/
/************************************************************************

************************************************************************/
#ifndef amsg_h_included

#define amsg_h_included


#include "definer.h"


#ifdef __cplusplus
  extern "C" {
#endif

/*======================================================================================================*/
STATUS_T amsg_init(char *p_indent_name,BOOL_T force_init_flag,
                   BOOL_T write_to_file_flag,char *file_name,
                   BOOL_T write_to_display_flag);
/*======================================================================================================*/
STATUS_T amsg_done(char *p_indent_name);
/*======================================================================================================*/
STATUS_T amsg_set_status( char *p_format,...);
STATUS_T amsg_set_error( char *p_format,...);
STATUS_T amsg_set_info(char *p_format,...);
/*======================================================================================================*/
CHAR *amsg_get_msg(void);
BOOL amsg_has_msg(void);
BOOL_T amsg_get_error_status(void);
/*======================================================================================================*/
STATUS_T amsg_res_msg(void);
/*======================================================================================================*/
#ifdef __cplusplus
  }
#endif

#endif