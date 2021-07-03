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

Vektor:

  par_vec_s vek;

  vek.name      (char *)        Name
  vek.ptr       (double *)      Pointer des Vektors
  vek.nrows     (UINT32_T)      Länge des Vektors

Vektor:

  par_mat_s mat;

  mat.name      (char *)        Name
  mat.ptr       (double *)      Pointer des Vektors
  mat.nrows     (UINT32_T)      Länge des y-Vektors
  mat.ncols     (UINT32_T)      Länge des x-Vektors

Tabelle:

  par_zab_s tab

  tab.name      (char *)        Name
  tab.xvec      (par_vec_s)     x-Vektor
  tab.yvec      (par_vec_s)     y-Vektor
  tab.islinear  (char)          1: linear 0: step
  tab.index     (UINT32_T)      aktueller Index


  Vektorfunktionen
  ==================

  STATUS_T  par_get_val_vec(par_vec_s *pvec,double dindex,double *pyval);
  ----------------------------------------------------------------------------
  Gibt Wert von Index zurück

  STATUS_T  par_free_vec(par_vec_s *pvec);
  ----------------------------------------
  Löscht Vektor und setzt alles auf null

  Tabellenfunktionen
  ==================

  STATUS_T  par_check_table(par_tab_s *ptab);
  -------------------------------------------
  Checkt Tabelle auf Länge und Monotonie
  Rückgabewert OK oder NOT_OK

  STATUS_T  par_get_val_table(par_tab_s *ptab,double xval,double *pyval);
  ----------------------------------------------------------------------------
  Gibt Wert zurück *pyval = f(xval)

  STATUS_T  par_set_index_table(par_tab_s *ptab,UINT32_T index);
  -------------------------------------------------------------------
  Setzt den Index auf 0 zurück

  STATUS_T  par_get_spzval_table(par_tab_s *ptab,char *pspz,double *pyval);
  ------------------------------------------------------------------------------
  Gibt Spezielle Werte in *pyval zurück:
  char *pspz
  "xstart"        erster Wert X-Vektor
  "xend"          letzter Wert X-Vektor
  "xmin"          Minimum (=erster Wert) X-Vektor
  "xmax"          Maximum (=letzter Wert) X-Vektor
  "ystart"        erster Wert Y-Vektor
  "yend"          letzter Wert Y-Vektor
  "ymin"          Minimum (=erster Wert) Y-Vektor
  "ymax"          Maximum (=letzter Wert) Y-Vektor

    
  STATUS_T  par_free_table(par_tab_s *ptab);
  ------------------------------------------
  Löscht TAbelle und setzt alles auf null


************************************************************************/
/* includes */

#ifndef apar_h_included
#define apar_h_included

#include "definer.h"

enum par_type_t {
  PAR_void,
  PAR_double,
  PAR_float,
  PAR_signed_long,
  PAR_unsigned_long,
  PAR_signed_short,
  PAR_unsigned_short,
  PAR_signed_char,
  PAR_unsigned_char,
  PAR_string,
  PAR_table,
  PAR_2D_table
};


/* Struktur für einen Parameter-Vektor */
typedef
struct tag_par_vec_s {
	char     *name;
	double   *ptr;
	UINT32_T nrows;
}par_vec_s;

/* Struktur für eine Matrix_t */
typedef
struct tag_par_mat_s {
	char        *name;
	double      *ptr;
	UINT32_T    nrows;
	UINT32_T    ncols;
} par_mat_s;


/* Struktur für eine 1D-Tabelle */
typedef
struct tag_par_tab_s {

    par_vec_s      xvec;
    par_vec_s      yvec;
	char           islinear;
    char           *name;
    char           isproofed;
    UINT32_T       index;
} par_tab_s;
/* Struktur für eine 2D-Tabelle */
typedef
struct tag_par_2d_tab_s {

    par_vec_s      xvec;
    par_vec_s      yvec;
    par_mat_s      zmat;
    char           *name;
    char           isproofed;
} par_2d_tab_s;

#ifdef  __cplusplus
extern "C" {
#endif

STATUS_T  par_get_val_vec(par_vec_s *pvec,double dindex,double *pyval);
STATUS_T  par_free_vec(par_vec_s *pvec);


STATUS_T  par_set_table(par_tab_s *ptab,char *name
                       ,char *xname,double *xvec,UINT32_T nrows_x
                       ,char *yname,double *yvec,UINT32_T nrows_y
                       ,char islinear);
STATUS_T  par_set_index_table(par_tab_s *ptab,UINT32_T index);
STATUS_T  par_check_table(par_tab_s *ptab);
STATUS_T  par_get_val_table(par_tab_s *ptab,double xval,double *pyval);
STATUS_T  par_get_spzval_table(par_tab_s *ptab,char *pspz,double *value);
STATUS_T  par_free_table(par_tab_s *ptab);

STATUS_T  par_set_2d_table(par_2d_tab_s *ptab,char *name
                          ,char *xname,double *xvec,UINT32_T nrows_x
                          ,char *yname,double *yvec,UINT32_T nrows_y
                          ,char *zname,double *zvec,UINT32_T nrows_z
                          ,UINT32_T ncols_z);
STATUS_T  par_check_2d_table(par_2d_tab_s *ptab);
STATUS_T  par_get_val_2d_table(par_2d_tab_s *ptab,double xval,double yval,double *pzval);
STATUS_T  par_free_2d_table(par_2d_tab_s *ptab);
STATUS_T  par_get_xmean_2d_table(par_2d_tab_s *ptab,double *pxmean);
STATUS_T  par_get_ymean_2d_table(par_2d_tab_s *ptab,double *pxmean);

STATUS_T  par_free_mat(par_mat_s *pmat);

#ifdef  __cplusplus
}
#endif

#endif

/*==========================================================================*/
