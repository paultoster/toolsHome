/* $JustDate::  2.03.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      25.02.03   TBert Aufteilung in amem und anum               */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/*************************************************************************
* File:             anum.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            numeric.h
*************************************************************************
* Kurzbeschreibung: 
*
* numerische Funktionen: 

===========================================================================
float anum_float_abs(float v);

Berechnet den Absolutwert eines float-Wertes
===========================================================================
int anum_fvekmerge(float **pziel,int *pmem_ziel,int *pn_ziel,
                  float *quelle1,int nquelle1,float *quelle2,int nquelle2,
                  int icase,float delta);
===========================================================================
int anum_fvekcpy(float **pziel,int *pmem_ziel,float *pquelle,int nquelle);
===========================================================================
int anum_fvekcat(float **pziel,int *pmem_ziel,int *nziel,float *quelle, int nquelle);
===========================================================================
void anum_fveksort_delta(float **pvek,int *pmem_vek,int *n,int icase,float delta);
     icase = 0 kein Sortieren, kein Wert elininieren
           = 1 aufsteigend Sortieren, kein Wert el.
           = 2 absteigend Sortieren, kein Wert el.
           = 3 kein Sortieren, Werte mit Abstand <delta eliminieren
           = 4 aufsteigend Sortieren, Werte mit Abstand <delta eliminieren
           = 5 absteigend Sortieren, Werte mit Abstand <delta eliminieren

===========================================================================
void anum_fveksort(float **pvek,int n,int icase);
     icase = 1 aufsteigend Sortieren
           = 2 absteigend Sortieren
           = 3 reverse sorting uc[0 ... n] = uc[n ... 0];

  icase + 10 Zweiter Vektor bearbeiten nach der Vorschrift für den ersten 
  icase + 20 Zweiter und Dritter Vektoren bearbeiten nach der Vorschrift für den ersten 

===========================================================================
void anum_ucveksort(float **pvek,int n,int icase);
     icase = 1 aufsteigend Sortieren
           = 2 absteigend Sortieren
           = 3 reverse sorting uc[0 ... n] = uc[n ... 0];

  icase + 10 Zweiter Vektor bearbeiten nach der Vorschrift für den ersten 
  icase + 20 Zweiter und Dritter Vektoren bearbeiten nach der Vorschrift für den ersten 
 
===========================================================================
int anum_fint1dim(int iflag,float *x,float *y,int nxy,float x0,float *y0,
             int i0,int iord,int iabl);

int anum_dint1dim(int iflag,double *x,double *y,int nxy,double x0,double *y0,
             int i0,int iord,int iabl);
*
*       iflag           Inittialisierung 0/1
*       x[nxy]          x-Vektor, muss monoton steigend sein
*       y[nxy]          y-Vektor
*       x0              x-Wert an dem y-Wert bestimmt wird
*       y0              y-Wert, der zurÏckgegeben wird
*       i0              aktuelle Stuetzstelle der letzten Berechnung
*                       wird als Ausgangswert benutzt, um schneller
*                       den Wertebereich in x zu finden (z.B. wenn x0
*                       fortlaufend steigt.
*       iord            = 0     Konstantwert swischen den Stuetzstellen
*                       = 1     lineare Interpolation
*
*       iabl            = 0,1   i.Ableitung
*
*       return i0       >0 aktuelle Stuetzstelle
*       return -1       Fehler, x-Vektor nicht monoton steigend (nur bei init)
*       return -2       Fehler, iord > 1 kubischer Spline nicht vorhanden (nur bei init)
===========================================================================
int anum_dint2dim(double *xvec,size_t nx,double *yvec,size_t ny,
                  double *zmat,double x0,double y0,double *z0,
                  char init);
*
*       Interpolation z(nrow,ncol)=f(x(nrow),y(ncol))
*       Der Endwert wird begrenzt
*       xvec[nx]        x-Vektor, monoton steigend
*       yvec[ny]        y-Vektor, monoton steigend
*       zmat[ny][nx]    z-Matrix_t
*       x0              x-Wert an dem z-Wert bestimmt wird
*       y0              y-Wert an dem z-Wert bestimmt wird
*       z0              z-Wert, der zurÏckgegeben wird
*       init            = 1 Überprüft nur Vektorlänge und Änderung von Punkt zu Punkt
*       return 0        okay
*       return 1        Fehler zuwenig Werte
*       return 2        Fehler nicht monoton steigend
===========================================================================
float anum_fatan2(float y,float x);
==================================

  Arcus tangens

void anum_ftausche(float *val0,float *val1);
===========================================
void anum_uctausche(unsigned char *val0,unsigned char *val1);
===========================================================

  Vertauscht die Werte 

===========================================================================
float anum_fbetrag(float x,float y,float z);
===========================================================================
float anum_dbetrag(double x,double y,double z);

===========================================================================

************************************************************************/
#ifndef numeric_h_included

#define numeric_h_included

#ifdef __cplusplus
  extern "C" {
#endif

float anum_float_abs(float v);

int anum_fvekmerge(float **pziel,size_t *pmem_ziel,size_t *pn_ziel,
                  float *quelle1,size_t nquelle1,float *quelle2,size_t nquelle2,
                  int icase,float delta);

int anum_fvekcpy(float **pziel,size_t *pmem_ziel,float *pquelle,size_t nquelle);
int anum_fvekcat(float **pziel,size_t *pmem_ziel,size_t *nziel,float *quelle, size_t nquelle);
void anum_fveksort_delta(float **pvek,size_t *pmem_vek,size_t *n,int icase,float delta);
void anum_fveksort(float **pvek,size_t n,int icase);
void anum_ucveksort(unsigned char **pvek,size_t n,int icase);

int anum_fint1dim(int iflag,float *x,float *y,size_t nxy,float x0,float *y0,
             int istart,int iord,int iabl);
int anum_dint1dim(int iflag,double *x,double *y,size_t nxy,double x0,double *y0,
             int istart,int iord,int iabl);
int anum_dint2dim(double *xvec,size_t nx,double *yvec,size_t ny,
                  double *zmat,double x0,double y0,double *z0,
                  char init);

float anum_fatan2(float y,float x);

void anum_ftausche(float *val0,float *val1);
void anum_uctausche(unsigned char *val0,unsigned char *val1);

float anum_fbetrag(float x,float y,float z);
float anum_dbetrag(double x,double y,double z);
int anum_dstep(double x, double x0, double h0, double x1, double h1,
              int iord, double *dval);
#ifdef __cplusplus
  }
#endif

#endif