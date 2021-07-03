// SlfMatRW.h
//===========
//
// Funktionen zum schreiben und lesen von Matlab Daten
//
// status_t SlfMatRWSetMatrix(const char *filename, SlfMatRWMatrix *pMatrix, uint32_t nMatrix);
//
// Beispiel
//
// #include "SlfMatRW.h"
// #include "Numeric.h"
// #include <stdio.h>
//
// float data1[10] = {0.0f,1.1f,2.2f,3.3f,4.4f,5.5f,6.6f,7.7f,8.8f,9.9f};
// double data2[5] = {100.,200.,300.,400.,500.};
// Vector_t vdata3 = NewVector(10);
// Matrix_t mdata4 = NewMatrix(5,1);
//
// SlfMatRWMatrix m[4];
//	
// m[0].Name    = "abc";
// m[0].VarType = DEF_ARR_FLOAT;
// m[0].pData   = (void *)data1;
// m[0].NRows   = 10;
// m[0].NCols   = 1;
// 
// m[1].Name    = "def";
// m[1].VarType = DEF_ARR_DOUBLE;
// m[1].pData   = (void *)data2;
// m[1].NRows   = 5;
// m[1].NCols   = 1;
//
// for(uint32_t irow=0;irow<10;irow++) vdata3[irow] = data1[irow];
//
// m[2].Name    = "ghi";
// m[2].VarType = DEF_VEC;
// m[2].pData   = (void *)vdata3;
// m[2].NRows   = GET_NROWS(vdata3);
// m[2].NCols   = 1;
//
// for(uint32_t irow=0;irow<5;irow++) mdata4[irow][0] = data2[irow];
//
// m[3].Name    = "jkl";
// m[3].VarType = DEF_MAT;
// m[3].pData   = (void *)mdata4;
// m[3].NRows   = GET_NROWS(mdata4);
// m[3].NCols   = GET_NCOLS(mdata4);
//
// if( SlfMatRWSetMatrix("testmatBuild.mat",m,4) != OKAY )
// {
//   printf(SlfMatRWErrText);
// }
//
#ifndef SLF_MATRW_H_INCLUDED
#define SLF_MATRW_H_INCLUDED


#include <stdio.h>
#include "SlfBasic.h"

typedef
struct tag_SlfMatRWMatrix {

  enum EVarType  VarType;
  char           *Name;
	void           *pData;
  uint32_t       NRows;
  uint32_t       NCols;
  bool           okay;
} SlfMatRWMatrix;


status_t SlfMatRWSetMatrix(const char *filename, SlfMatRWMatrix *pMatrix, uint32_t nMatrix);
status_t SlfMatRWGetMatrix(const char *filename, SlfMatRWMatrix *pMatrix, uint32_t nMatrix);

#define SLF_MATRW_ERR_TEXT_SIZE 255
extern char SlfMatRWErrText[SLF_MATRW_ERR_TEXT_SIZE];

#endif
