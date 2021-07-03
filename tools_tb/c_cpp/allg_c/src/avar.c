/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* Aenderungen 
001 07.03.01 TBert      Anlegen einer neuen Struktur
Ver Datum*** Wer        Was
*/
/*************************************************************************
* File:             avar.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
* Version:          1.0
* Datum:            16.11.04
*************************************************************************
* Kurzbeschreibung: 
*
* Bearbeitet Variablen (var-Funktionen) und Vektoren (vec-Funktionen)
************************************************************************/

#define AVAR_MSG 1     /* 0: keine mesage ausgabe */
                       /* 1: mit amsg-Funktionen  */
                       /* 2: mit printf           */

#include <stdio.h>
#include "avar.h"
#include "anum.h"

#if AVAR_MSG == 1
    #include "amsg.h"
#elif AVAR_MSG == 2
    #define amsg_set_status printf
#else
    #undef AVAR_MSG
#endif

static STATUS_T avar_stat=OK;

/*===========================================
  legt neue Struktur an
============================================*/
avar_s *avar_new(UINT8_T type) {

    avar_s *ps=NULL;

    /* Memory für die Struktur p */
    ps = malloc(sizeof(avar_s));

    if( ps == NULL ) {
#ifdef AVAR_MSG
        amsg_set_status("\navar_new_err: no memory for avec_s ps");
#endif
        avar_stat = NOT_OK;
        return ps;
    } else {
        ps->ps_name = astrs_new();
        ps->ps_unit = astrs_new();

        ps->ps_next  = NULL;

        ps->type     = type;

        ps->p_d      = NULL;
        ps->p_f      = NULL;
        ps->p_i32    = NULL;
        ps->p_ui32   = NULL;
        ps->p_i16    = NULL;
        ps->p_ui16   = NULL;
        ps->p_i16    = NULL;
        ps->p_ui8    = NULL;
        ps->p_i8     = NULL;
        ps->p_string = NULL;

        switch(type) {
        case DEF_double:
            ps->p_d = malloc(sizeof(double));
            *ps->p_d = 0.0;
            break;
        case DEF_float:
            ps->p_f = malloc(sizeof(float));
            *ps->p_f = 0.0f;
            break;
        case DEF_signed_long:
            ps->p_i32 = malloc(sizeof(SINT32_T));
            *ps->p_i32 = 0L;
            break;
        case DEF_unsigned_long:
            ps->p_ui32 = malloc(sizeof(UINT32_T));
            *ps->p_ui32 = 0L;
            break;
        case DEF_signed_short:
            ps->p_i16 = malloc(sizeof(SINT16_T));
            *ps->p_i16 = 0;
            break;
        case DEF_unsigned_short:
            ps->p_ui16 = malloc(sizeof(UINT16_T));
            *ps->p_ui16 = 0;
            break;
        case DEF_signed_char:
            ps->p_i8 = malloc(sizeof(SINT8_T));
            *ps->p_i8 = 0;
            break;
        case DEF_unsigned_char:
            ps->p_ui8 = malloc(sizeof(UINT8_T));
            *ps->p_ui8 = 0;
            break;
        case DEF_string:
            ps->p_string = astrs_new();
            astrs_cpy(ps->p_string,"");
            break;
        default:
#ifdef AVAR_MSG
            amsg_set_status("\navar_new_err: type is not okay type = %i",type);
#endif
            avar_stat = NOT_OK;
            return ps;
        }

//        ps->dvar    = 0.0;
    }

    /* Initialisierter Popinter zurückgeben */
    return ps;
}
/*===========================================
  hängt neue Struktur an
============================================*/
avar_s *avar_add_new(avar_s *ps_mast,UINT8_T type) {

    avar_s *ps=NULL;

    /* Memory für die Struktur p */
    ps = malloc(sizeof(avar_s));

    if( ps == NULL ) {
#ifdef AVAR_MSG
        amsg_set_status("\navar_new_err: no memory for avec_s ps");
#endif
        avar_stat = NOT_OK;
        return ps;
    } else {
        ps->ps_name = astrs_new();
        ps->ps_unit = astrs_new();

        ps->ps_next  = NULL;
        while(ps_mast->ps_next != NULL)
            ps_mast = ps_mast->ps_next;

        ps_mast->ps_next = ps;

        ps->type     = type;

        ps->p_d      = NULL;
        ps->p_f      = NULL;
        ps->p_i32    = NULL;
        ps->p_ui32   = NULL;
        ps->p_i16    = NULL;
        ps->p_ui16   = NULL;
        ps->p_i16    = NULL;
        ps->p_ui8    = NULL;
        ps->p_i8     = NULL;
        ps->p_string = NULL;

        switch(type) {
        case DEF_double:
            ps->p_d = malloc(sizeof(double));
            *ps->p_d = 0.0;
            break;
        case DEF_float:
            ps->p_f = malloc(sizeof(float));
            *ps->p_f = 0.0f;
            break;
        case DEF_signed_long:
            ps->p_i32 = malloc(sizeof(SINT32_T));
            *ps->p_i32 = 0L;
            break;
        case DEF_unsigned_long:
            ps->p_ui32 = malloc(sizeof(UINT32_T));
            *ps->p_ui32 = 0L;
            break;
        case DEF_signed_short:
            ps->p_i16 = malloc(sizeof(SINT16_T));
            *ps->p_i16 = 0;
            break;
        case DEF_unsigned_short:
            ps->p_ui16 = malloc(sizeof(UINT16_T));
            *ps->p_ui16 = 0;
            break;
        case DEF_signed_char:
            ps->p_i8 = malloc(sizeof(SINT8_T));
            *ps->p_i8 = 0;
            break;
        case DEF_unsigned_char:
            ps->p_ui8 = malloc(sizeof(UINT8_T));
            *ps->p_ui8 = 0;
            break;
        case DEF_string:
            ps->p_string = astrs_new();
            astrs_cpy(ps->p_string,"");
            break;
        default:
#ifdef AVAR_MSG
            amsg_set_status("\navar_new_err: type is not okay type = %i",type);
#endif
            avar_stat = NOT_OK;
            return ps;
        }

//        ps->dvar    = 0.0;
    }

    /* Initialisierter Popinter zurückgeben */
    return ps;
}
/*===========================================
  löscht Struktur
============================================*/
STATUS_T  avar_delete(avar_s *ps){

    avar_s *ps1 = ps;
    do {
        if( ps->ps_name != NULL )
            astrs_delete(ps->ps_name);

        if( ps->ps_unit != NULL )
            astrs_delete(ps->ps_unit);


        if( ps->p_d != NULL )
            free(ps->p_d);
        if( ps->p_f != NULL )
            free(ps->p_f);
        if( ps->p_i32 != NULL )
            free(ps->p_i32);
        if( ps->p_ui32 != NULL )
            free(ps->p_ui32);
        if( ps->p_i16 != NULL )
            free(ps->p_i16);
        if( ps->p_ui16 != NULL )
            free(ps->p_ui16);
        if( ps->p_i8 != NULL )
            free(ps->p_i8);
        if( ps->p_ui8 != NULL )
            free(ps->p_ui8);
        if( ps->p_string != NULL )
            astrs_delete(ps->p_string);

        ps1 = ps->ps_next;

        free(ps);
        ps = ps1;
    } while(ps != NULL);

    return avar_stat;
}
/*===========================================
  Namen der Variable setzen
============================================*/
STATUS_T  avar_set_name(avar_s *ps,char *p_name) {

    if( ps->ps_name == NULL ) 
        ps->ps_name = astrs_new();

    astrs_cpy(ps->ps_name,p_name);

    return OK;
}
/*===========================================
  Einheit der Variable setzen
============================================*/
STATUS_T  avar_set_unit(avar_s *ps,char *p_unit) {

    if( ps->ps_unit == NULL ) 
        ps->ps_unit = astrs_new();

    astrs_cpy(ps->ps_unit,p_unit);

    return OK;
}
#if 0
/*===========================================
  Value der Variable setzen
============================================*/
STATUS_T  avar_set_val(avar_s *ps,UINT8_T type,...){

    if( ps->
    ps_var->dvar = dval;
    return OK;
}
/*===========================================
  Value der Variable holen
============================================*/
double  avar_dval(avar_s *ps_var) {

    return ps_var->dvar;
}
/*===========================================
  Pointer der Variable 
============================================*/
double  *avar_p(avar_s* ps) {

    return &(ps->dvar);
}
#endif
/*===========================================
  Name der Variable 
============================================*/
char           *avar_name(avar_s *ps) {

    return astrs_string(ps->ps_name);
}
/*===========================================
  Unit der Variable 
============================================*/
char           *avar_unit(avar_s *ps) {

    return astrs_string(ps->ps_unit);
}




/*===========================================
  legt neue Vektor-Struktur an
============================================*/
avec_s *avec_new(void) {

    avec_s *ps=NULL;

    /* Memory für die Struktur p */
    ps = malloc(sizeof(avec_s));

    if( ps == NULL ) {
#ifdef AVAR_MSG
        amsg_set_status("\navec_new_err: no memory for avec_s ps");
#endif
        avar_stat = NOT_OK;
        return ps;
    } else {
        ps->ps_name = NULL;
        ps->ps_unit = NULL;
        ps->dvec    = NULL;
        ps->len     = 0;
        ps->p_next  = NULL;
        ps->n_item  = 0;
    }

    /* Initialisierter Popinter zurückgeben */
    return ps;
}
/*===========================================
  legt neuen Vektor an
============================================*/
avec_s *avec_add_new(avec_s *ps) {

    avec_s *ps_new=NULL;

    if( ps->n_item == 0 ) { /* Die erste Struktur wurde noch nicht belegt */

        ps->n_item = 1;
        ps_new = ps;

    } else { /* neue Struktur bilden und anhängen */

        /* Memory für die Struktur p */
        ps_new = malloc(sizeof(avec_s));

        if( ps_new == NULL ) {
#ifdef AVAR_MSG
            amsg_set_status("\navec_add_new_err: no memory for avec_s ps");
#endif
            avar_stat = NOT_OK;
            return ps;
        } else {

            while( ps->p_next != NULL )
                ps = ps->p_next;

            ps->p_next = ps_new;

            ps_new->ps_name = NULL;
            ps_new->ps_unit = NULL;
            ps_new->dvec    = NULL;
            ps_new->len     = 0;
            ps_new->mem_len = 0;
            ps_new->p_next  = NULL;
            ps_new->n_item  = ps->n_item + 1;
        }
    }

    /* Initialisierter Popinter zurückgeben */
    return ps_new;
}
/*===========================================
  löscht Vektor-Struktur
============================================*/
STATUS_T  avec_delete(avec_s *ps) {

    avec_s *ps_next;
    while(ps != NULL ) {

        avec_free(ps);

        if( ps->ps_name != NULL )
            astrs_delete(ps->ps_name);

        if( ps->ps_unit != NULL )
            astrs_delete(ps->ps_unit);

        ps_next = ps->p_next;

        free(ps);

        ps = ps_next;

    } 
    
    return avar_stat;
}
/*===========================================
  löscht Vektor in Struktur
============================================*/
STATUS_T avec_free(avec_s *ps){

    if( ps->dvec != NULL ) {
        free(ps->dvec);
        ps->dvec = NULL;
        ps->len  = 0;
    }
    return avar_stat;
}
/*===========================================
  Namen der Vektor setzen
============================================*/
STATUS_T  avec_set_name(avec_s *ps,char *p_name) {

    if( ps->ps_name == NULL ) 
        ps->ps_name = astrs_new();

    astrs_cpy(ps->ps_name,p_name);

    return OK;
}
/*===========================================
  Einheit der Vektor setzen
============================================*/
STATUS_T  avec_set_unit(avec_s *ps,char *p_unit) {

    if( ps->ps_unit == NULL ) 
        ps->ps_unit = astrs_new();

    astrs_cpy(ps->ps_unit,p_unit);

    return OK;
}
/*===========================================
  Memory des Vektors setzen
============================================*/
double *avec_set_mem(avec_s *ps_vec,UINT16_T mem_len){

    double *p_double = NULL;

    if( ps_vec != NULL ) {

        if( mem_len <= MAX_UINT16/sizeof(double) ) {

            p_double     = realloc((void *)&ps_vec->dvec,sizeof(double)*(UINT16_T)mem_len);
            if( p_double == NULL ) {
#ifdef AVAR_MSG
                amsg_set_status("\navec_set_mem_err: no memory for double ps->dvec");
#endif
                avar_stat = NOT_OK;
            } else {

                ps_vec->dvec    = p_double;
                ps_vec->mem_len = mem_len;
                ps_vec->len = mem_len;
            }

        } else {
#ifdef AVAR_MSG
            amsg_set_status("\navec_set_mem_err: mem_len > MAX_UINT16/sizeof(double) ");
#endif
            avar_stat = NOT_OK;
        }
    } else {
        avar_stat = NOT_OK;
    }

    return p_double;
}

/*===========================================
  Länge des Vektors setzen
============================================*/
double  *avec_set_len(avec_s *ps_vec,UINT16_T len){

    if( ps_vec != NULL ) {

        if( len > ps_vec->mem_len ) {

            return avec_set_mem(ps_vec,len);
        } else {

            ps_vec->len = len;
            return ps_vec->dvec;
        }
    } else {
        return NULL;
    }
}
/*===========================================
  maximale Länge aller Vektoren suchen
============================================*/
UINT16_T  avec_max_len_in_list(avec_s *ps_vec) {

    UINT16_T max_len = 0;

    if( ps_vec != NULL ) {

        max_len = ps_vec->len;

        while( ps_vec->p_next != NULL ) {

            ps_vec = ps_vec->p_next;

            max_len = MAX(max_len,ps_vec->len);
        }
    }
    return max_len;
}
    
/*===========================================
  Wert setzen
============================================*/

STATUS_T  avec_set_val(avec_s *ps_vec,double dval,UINT16_T i_pos) {

    if( ps_vec == NULL ) {

        avar_stat = NOT_OK;
#ifdef AVAR_MSG
                amsg_set_status("\navec_set_val_err: structre ps_vec not initialized ");
#endif
        return avar_stat;

    } else {

        if( i_pos >= ps_vec->mem_len )

            if( avec_set_mem(ps_vec,(UINT16_T)(i_pos+1)) == NULL )
                return NOT_OK;

        ps_vec->dvec[i_pos] = dval;
    }
    return OK;
}

