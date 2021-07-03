/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      19.08.04   TBert                                           */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             aasap2_val.c        
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
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
    #include "amsg.h"
#endif


/*-------------------------------------------------------------------------*/
/*- set values                                                            -*/
/*-------------------------------------------------------------------------*/
STATUS_T     aasap2_c::get_val(aasap2_val_s **pps_val) {

    aasap2_modul_s *ps_modul = NULL;

    if(  ps_project           == NULL 
      || ps_project->ps_modul == NULL 
      ) {
        AASAP2_PRINT("\nERROR in <aasap2_c::get_val>: no project, or no modul found");
        return NOT_OK;
    }

    ps_modul = ps_project->ps_modul;

    while( ps_modul != NULL ) {

        if( get_val_modul(ps_modul,pps_val) != OK ) {
            AASAP2_PRINT("\nERROR with <aasap2_c::get_val_modul> in <aasap2_c::get_val>");
            return NOT_OK;
        }

        ps_modul = ps_modul->ps_next;
    }


    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::get_val_modul(aasap2_modul_s *ps_modul,aasap2_val_s **pps_val) {

    aasap2_characteristic_s *ps_charac = NULL;

    ps_charac = ps_modul->ps_characteristic;

    while( ps_charac != NULL ) {


        if( get_val_characteristic(ps_charac,ps_modul->ps_conversion,pps_val) != OK ) {

                AASAP2_PRINT("\nERROR with <aasap2_c::get_val_characteristic> in <aasap2_c::get_val_modul>");
                return NOT_OK;
        }

        ps_charac = ps_charac->ps_next;
    }
    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::get_val_characteristic(aasap2_characteristic_s *ps_charac,
                                          aasap2_conversion_s *ps_conv,
                                          aasap2_val_s **pps_val) {

    aasap2_val_s        *ps_v = NULL;
    /* keywords conversion types */
    char *key_words[] = {"TAB_INTP","TAB_NOINTP","TAB_VERB","RAT_FUNC"
                        ,"FORM"};

    UINT8_T n_key_words = sizeof( key_words) / sizeof(char *);

   /* Value-Struct anlegen */
    ps_v = new (aasap2_val_s);

   /* Werte übergeben */
   ps_v->p_name         = ps_charac->name;
   ps_v->p_comment      = ps_charac->comment;
   ps_v->char_type      = ps_charac->type;
   ps_v->asap1b_adress  = ps_charac->asap1b_address;
   ps_v->bit_mask       = ps_charac->bit_mask;
   ps_v->p_format       = ps_charac->format;

   /* Val-type */
   if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_UBYTE) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_UBYTE;
   else if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_UWORD) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_UWORD;
   else if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_ULONG) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_ULONG;
   else if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_SBYTE) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_SBYTE;
   else if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_SWORD) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_SWORD;
   else if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_SLONG) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_SLONG;
   else if( strcmp(ps_charac->deposit,AASAP2_DEPOSIT_FLOAT32_IEEE) == 0 )
       ps_v->val_type = AASAP2_DEF_VAL_FLOAT32_IEEE;
   else {
            AASAP2_PRINT("\nERROR in <aasap2_c::get_val_characteristic>: deposit %s not recognized"
                           ,ps_charac->deposit);
            print_characteristic(ps_charac);
            return NOT_OK;
   }


   /* Konvertierung */
   while( ps_conv != NULL ) {
   
       if( strcmp(ps_charac->conversion,ps_conv->name) == 0 )
           break;

       ps_conv = ps_conv->ps_next;
   }

   if( ps_conv != NULL ) {

       if( ps_conv->type == AASAP2_CONV_TYPE_RAT_FUNC ) {

           ps_v->p_coeffs = ps_conv->coeffs;
           ps_v->p_unit   = ps_conv->unit;
       } else {

            AASAP2_PRINT("\nERROR in <aasap2_c::get_val_characteristic>: conversion (COMPU_METHOD) type : %s not programmed"
                           ,key_words[MIN(n_key_words-1,ps_conv->type)]);
            print_characteristic(ps_charac);
            return NOT_OK;
       }
   } else {
        AASAP2_PRINT("\nERROR in <aasap2_c::get_val_characteristic>: conversion name : %s in COMPU_METHODs not found"
                       ,ps_charac->conversion);
        print_characteristic(ps_charac);
        return NOT_OK;
   }


   
   if( *pps_val != NULL ) 
       ps_v->ps_next = *pps_val;
   else
       ps_v->ps_next = NULL;
    
   *pps_val = ps_v;

   return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_c::print_characteristic(aasap2_characteristic_s *ps_charac) {


    AASAP2_PRINT("\nCHARACTRISTIC-Value:");
    AASAP2_PRINT("\n%10s : %s","name",ps_charac->name);
    AASAP2_PRINT("\n%10s : %s","comment",ps_charac->comment);
    AASAP2_PRINT("\n%10s : %i","type",ps_charac->type);
    AASAP2_PRINT("\n%10s : %lx","address",ps_charac->address);
    AASAP2_PRINT("\n%10s : %s","deposit",ps_charac->deposit);
    AASAP2_PRINT("\n%10s : %f","maxdiff",ps_charac->maxdiff);
    AASAP2_PRINT("\n%10s : %s","conversion",ps_charac->conversion);
    AASAP2_PRINT("\n%10s : %f","lower_limit",ps_charac->lower_limit);
    AASAP2_PRINT("\n%10s : %f","upper_limit",ps_charac->upper_limit);
    AASAP2_PRINT("\n%10s : %lx","asap1b_address",ps_charac->asap1b_address);
    AASAP2_PRINT("\n%10s : %s","format",ps_charac->format);
    AASAP2_PRINT("\n%10s : %lx","bit_mask",ps_charac->bit_mask);

    return;
}
