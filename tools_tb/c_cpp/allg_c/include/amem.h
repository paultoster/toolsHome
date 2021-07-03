/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      25.02.03   TBert Aufteilung in amem und anum               */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/*************************************************************************
* File:             amem.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
*************************************************************************
* Kurzbeschreibung: 
*
* memory Funktionen: 
===========================================================================
int amem_smem(char **pp,size_t *pmem,size_t n);
int amem_imem(int **pp,size_t *pmem,size_t n);
int amem_stmem(size_t **pp,size_t *pmem,size_t n);
int amem_fmem(float **pp,size_t *pmem,size_t n);
int amem_dmem(double **pp,size_t *pmem,size_t n);
int amem_mem(void **pp,size_t *pmem,size_t n,size_t size_struct);

Stellt Speicher bereit n float-Werte.
Wenn *pp = NULL wird neuer Speicher bereitgestellt, Speichergröße steht in
*pmem. Wenn nicht geügend Speicher vorhanden n > *pmem und *pp != NULL, wird 
Speicher vergößert.

Rueckgabewert = 0, ansonsten Fehler mit der Funktion realloc

Beispiel:

float *vektor=NULL;
size_t   nvektor;

if( amem_fmem(&vektor,&nvektor,500) != 0 )
{
  string_message_cat("Funktion_error: FEhler in amem_fmem");
  return -1;
}
===========================================================================
int amem_smem_g(char **pp,size_t n);
int amem_imem_g(int **pp,size_t n);
int amem_stmem_g(size_t **pp,size_t n);
int amem_fmem_g(float **pp,size_t n);
int amem_dmem_g(double **pp,size_t n);
int amem_mem_g(void **pp,size_t n,size_t size_struct);

  Gleiche Funktion wie oben z.B. amem_smem(), allociert aber genau den
  geforderten Speicher.

===========================================================================
int amem_smem_red(char **pp,size_t *pmem,size_t n);
int amem_imem_red(int **pp,size_t *pmem,size_t n);
int amem_stmem_red(size_t **pp,size_t *pmem,size_t n);
int amem_fmem_red(float **pp,size_t *pmem,size_t n);
int amem_dmem_red(double **pp,size_t *pmem,size_t n);
int amem_mem_red(void **pp,size_t *pmem,size_t n,size_t size_struct);

  Reduziert Speicher
  amem_smem_red() benutzen, wenn vorher mit amem_smem() zu großen
  Speicher allociert wurde.
===========================================================================
int amem_sfree(char **pp,size_t *pmem);
int amem_ifree(int **pp,size_t *pmem);
int amem_stfree(size_t **pp,size_t *pmem);
int amem_ffree(float **pp,size_t *pmem);
int amem_dfree(double **pp,size_t *pmem);
int amem_free(void **pp,size_t *pmem);

  Gibt Speicher frei
  amem_sfree() benutzen, wenn vorher mit amem_smem()
  Speicher allociert wurde.
===========================================================================
int amem_sfree_g(char **pp);
int amem_ifree_g(int **pp);
int amem_stfree_g(size_t **pp);
int amem_ffree_g(float **pp);
int amem_dfree_g(double **pp);
int amem_free_g(void **pp);

  Gibt Speicher frei
  amem_sfree() benutzen, wenn vorher mit amem_smem()
  Speicher allociert wurde.
===========================================================================
************************************************************************/
#ifndef amem_h_included

#define amem_h_included

#ifdef __cplusplus
  extern "C" {
#endif

int amem_smem(char **pp,size_t *pmem,size_t n);
int amem_imem(int **pp,size_t *pmem,size_t n);
int amem_stmem(size_t **pp,size_t *pmem,size_t n);
int amem_fmem(float **pp,size_t *pmem,size_t n);
int amem_dmem(double **pp,size_t *pmem,size_t n);
int amem_mem(void **pp,size_t *pmem,size_t n,size_t size_struct);

int amem_smem_g(char **pp,size_t n);
int amem_imem_g(int **pp,size_t n);
int amem_stmem_g(size_t **pp,size_t n);
int amem_fmem_g(float **pp,size_t n);
int amem_dmem_g(double **pp,size_t n);
int amem_mem_g(void **pp,size_t n,size_t size_struct);

int amem_smem_red(char **pp,size_t *pmem,size_t n);
int amem_imem_red(int **pp,size_t *pmem,size_t n);
int amem_stmem_red(size_t **pp,size_t *pmem,size_t n);
int amem_fmem_red(float **pp,size_t *pmem,size_t n);
int amem_dmem_red(double **pp,size_t *pmem,size_t n);
int amem_mem_red(void **pp,size_t *pmem,size_t n,size_t size_struct);

int amem_sfree(char **pp,size_t *pmem);
int amem_ifree(int **pp,size_t *pmem);
int amem_stfree(size_t **pp,size_t *pmem);
int amem_ffree(float **pp,size_t *pmem);
int amem_dfree(double **pp,size_t *pmem);
int amem_free(void **pp,size_t *pmem);

int amem_sfree_g(char **pp);
int amem_ifree_g(int **pp);
int amem_stfree_g(size_t **pp);
int amem_ffree_g(float **pp);
int amem_dfree_g(double **pp);
int amem_free_g(void **pp);


#ifdef __cplusplus
  }
#endif

#endif