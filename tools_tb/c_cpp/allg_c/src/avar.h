/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* Aenderungen 
Ver Datum*** Wer        Was
*/
/*************************************************************************
* File:             avar.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
* Version:          1.0
* Datum:            16.11.04
*************************************************************************
* Kurzbeschreibung: 

Struktur:
=========

typedef
struct tag_avar_s {
    astrs_s  *ps_name;
    astrs_s  *ps_unit;
    double   *dvar;
    UINT32_T len;
} avar_s;

avar_s         *avec_new(void);
===============================

  legt neue Struktur an

STATUS_T        avec_delete(avec_s *ps);
========================================

  Löscht Struktur


STATUS_T        avec_free(avec_s *ps);
======================================

  Löscht Vektor in struktur

************************************************************************/
/* includes */

#ifndef AVAR_INCLUDE
#define AVAR_INCLUDE

#include "definer.h"
#include "astrs.h"

/* Defines */

#ifndef DEF_double
    #define  DEF_double         1
#endif
#ifndef DEF_float
    #define  DEF_float          3
#endif
#ifndef DEF_signed_long
    #define  DEF_signed_long    5
#endif
#ifndef DEF_unsigned_long
    #define  DEF_unsigned_long  7
#endif
#ifndef DEF_unsigned_short
    #define  DEF_unsigned_short 9
#endif
#ifndef DEF_signed_short
    #define  DEF_signed_short   11
#endif
#ifndef DEF_signed_char
    #define  DEF_signed_char    13
#endif
#ifndef DEF_unsigned_char
    #define  DEF_unsigned_char  15
#endif
#ifndef DEF_string
    #define  DEF_string         17
#endif

#ifndef DEF_signed_int
    #define  DEF_signed_int     DEF_signed_long
#endif
#ifndef DEF_unsigned_int
    #define  DEF_unsigned_int   DEF_unsigned_long
#endif

/* Strukturdefinition */
typedef
struct tag_avar_s {
    UINT8_T  type;
    astrs_s  *ps_name;
    astrs_s  *ps_unit;
    double   *p_d;
    float    *p_f;
    SINT32_T *p_i32;
    UINT32_T *p_ui32;
    SINT16_T *p_i16;
    UINT16_T *p_ui16;
    SINT8_T  *p_i8;
    UINT8_T  *p_ui8;
    astrs_s  *p_string;
    struct tag_avar_s *ps_next;
} avar_s;

typedef
struct tag_avec_s {
    astrs_s           *ps_name;
    astrs_s           *ps_unit;
    double            *dvec;
    UINT16_T          mem_len;
    UINT16_T          len;
    struct tag_avec_s *p_next;
    UINT16_T          n_item;
} avec_s;



#ifdef  __cplusplus
extern "C" {
#endif

avar_s         *avar_new(UINT8_T type);
STATUS_T        avar_delete(avar_s *ps);
STATUS_T        avar_set_name(avar_s *ps_var,char *p_name);
STATUS_T        avar_set_unit(avar_s *ps_var,char *p_unit);
STATUS_T        avar_set_dval(avar_s *ps_var,double val);
double          avar_dval(avar_s *ps_var);
void           *avar_p(avar_s* ps);
char           *avar_name(avar_s *ps);
char           *avar_unit(avar_s *ps);



avec_s         *avec_new(void);
avec_s         *avec_add_new(avec_s *ps_vec);
STATUS_T        avec_delete(avec_s *ps_vec);
STATUS_T        avec_free(avec_s *ps_vec);
STATUS_T        avec_set_name(avec_s *ps_vec,char *p_name);
STATUS_T        avec_set_unit(avec_s *ps_vec,char *p_unit);
double         *avec_set_mem(avec_s *ps_vec,UINT16_T mem_len);
double         *avec_set_len(avec_s *ps_vec,UINT16_T len);
STATUS_T        avec_set_val(avec_s *ps_vec,double dval,UINT16_T i_pos);

UINT16_T        avec_max_len_in_list(avec_s *ps_vec);

#ifdef  __cplusplus
}
#endif

#endif

/*==========================================================================*/
