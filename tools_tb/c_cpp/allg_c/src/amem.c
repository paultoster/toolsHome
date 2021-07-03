/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      25.02.03   TBert Aufteilung in amem und anum               */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             amem.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            numeric.c
*************************************************************************
* Kurzbeschreibung: 
*
* memory Funktionen: siehe amem.h
************************************************************************/
/************************************************************************

************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "definer.h"
#include "astr.h"
#include "amem.h"


void *amem_pointer;
int amem_voidmem(size_t *pmem,size_t n,size_t size,int genau_flag);
int amem_voidmem_red(size_t *pmem,size_t n,size_t size);
int amem_voidfree(size_t *pmem);

/*======================================================================*/
int amem_smem(char **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(pmem,n,sizeof(char),0);
    *pp = (char *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_smem_g(char **pp,size_t n)
{
    int    i = 0;
    size_t mem=0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(&mem,n,sizeof(char),1);
    *pp = (char *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_smem_red(char **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem_red(pmem,n,sizeof(char));
    *pp = (char *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_sfree(char **pp,size_t *pmem)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(pmem);
    *pp = (char *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_sfree_g(char **pp)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(&mem);
    *pp = (char *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_imem(int **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(pmem,n,sizeof(int),0);
    *pp = (int *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_imem_g(int **pp,size_t n)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(&mem,n,sizeof(int),1);
    *pp = (int *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_imem_red(int **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem_red(pmem,n,sizeof(int));
    *pp = (int *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_ifree(int **pp,size_t *pmem)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(pmem);
    *pp = (int *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_ifree_g(int **pp)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(&mem);
    *pp = (int *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_stmem(size_t **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(pmem,n,sizeof(size_t),0);
    *pp = (size_t *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_stmem_g(size_t **pp,size_t n)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(&mem,n,sizeof(size_t),1);
    *pp = (size_t *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_stmem_red(size_t **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem_red(pmem,n,sizeof(size_t));
    *pp = (size_t *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_stfree(size_t **pp,size_t *pmem)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(pmem);
    *pp = (size_t *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_stfree_g(size_t **pp)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(&mem);
    *pp = (size_t *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_fmem(float **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(pmem,n,sizeof(float),0);
    *pp = (float *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_fmem_g(float **pp,size_t n)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(&mem,n,sizeof(float),1);
    *pp = (float *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_fmem_red(float **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem_red(pmem,n,sizeof(float));
    *pp = (float *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_ffree(float **pp,size_t *pmem)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(pmem);
    *pp = (float *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_ffree_g(float **pp)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(&mem);
    *pp = (float *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_dmem(double **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(pmem,n,sizeof(double),0);
    *pp = (double *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_dmem_g(double **pp,size_t n)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(&mem,n,sizeof(double),1);
    *pp = (double *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_dmem_red(double **pp,size_t *pmem,size_t n)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem_red(pmem,n,sizeof(double));
    *pp = (double *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_dfree(double **pp,size_t *pmem)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(pmem);
    *pp = (double *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_dfree_g(double **pp)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(&mem);
    *pp = (double *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_mem(void **pp,size_t *pmem,size_t n,size_t size_struct)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(pmem,n,size_struct,0);
    *pp = (void *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_mem_g(void **pp,size_t n,size_t size_struct)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidmem(&mem,n,size_struct,1);
    *pp = (void *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_mem_red(void **pp,size_t *pmem,size_t n,size_t size_struct)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidmem_red(pmem,n,size_struct);
    *pp = (void *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_free(void **pp,size_t *pmem)
{
    int i = 0;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(pmem);
    *pp = (void *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_free_g(void **pp)
{
    int i = 0;
    size_t mem;
    amem_pointer = (void *)*pp;
    i = amem_voidfree(&mem);
    *pp = (void *)amem_pointer;
    return i;
}
/*======================================================================*/
int amem_voidmem(size_t *pmem,size_t n,size_t size,int genau_flag) {

    if( (n > *pmem && genau_flag != 1) || 
        (amem_pointer == NULL     ) ||
        (genau_flag == 1             ) ){

        if( genau_flag ) {
            *pmem = n;
        } else {
            *pmem = 16;
            while( *pmem < n ) {
                if( *pmem >= 128 )
                    *pmem += 128;
                else
                    *pmem *= 2;
            }
        }
        if(  (( amem_pointer=realloc(amem_pointer,*pmem * size) ) == NULL )
          && (( *pmem * size ) != 0 )
          ) {
            return (int)-1;
        }
    }
    return 0;
}
/*======================================================================*/
int amem_voidmem_red(size_t *pmem,size_t n,size_t size) {

    if( n < *pmem && amem_pointer != NULL ) {

        *pmem = 16;
        while( *pmem < n ) {
            if( *pmem >= 128 )
                *pmem += 128;
            else
                *pmem *= 2;
        }       
        if(  (( amem_pointer=realloc(amem_pointer,*pmem * size) ) == NULL )
          && (( *pmem * size ) != 0 )
          ) {
            amem_pointer = NULL;
            return -1;
        }
    }

    return 0;
}
/*======================================================================*/
int amem_voidfree(size_t *pmem) {

  if( amem_pointer != NULL ) {
    free(amem_pointer);
    amem_pointer= NULL;
    /*
    amem_pointer=realloc(amem_pointer,0);
    */
    *pmem = 0;

  }
  return 0;
}
