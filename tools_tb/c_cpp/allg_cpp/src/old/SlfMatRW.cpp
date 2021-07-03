// SlfMat.cpp
//===========
//
// nachgeildete Matlabfunktionen
//
//#define _CRT_SECURE_NO_DEPRECATE 
#ifndef _CRT_SECURE_NO_WARNINGS
  #define _CRT_SECURE_NO_WARNINGS
#endif
#define SLFBASIC_VAR_TYPE_STR
#include "SlfMatRW.h"
#include "SlfMat.h"
#include "Numeric.h"
#include <string.h>

void SlfMatRWWriteErrText(char *addtext);
char SlfMatRWErrText[SLF_MATRW_ERR_TEXT_SIZE];

//===============================================================================
//===============================================================================
// Schnittstellenfunktionen
//===============================================================================
//===============================================================================
// SlfSetMatrix, Schreiben einer Matlabdatei
//==========================================
status_t SlfMatRWSetMatrix(const char *filename, SlfMatRWMatrix *pMatrix, uint32_t nMatrix)
{
  SlfMATFile *pMatFile;
  SlfMATData *pdat;
  uint32_t   i;
  double     *pd;

  SlfMatRWErrText[0] = '\0';

  if( nMatrix == 0 )
  {
    sprintf(SlfMatRWErrText,"SlfMatRWSetMatrix: No Data nMatrix == 0\n");
    return NOT_OKAY;
  }

  /* Matdatei anlegen */
  pMatFile = SlfMatOpen(filename,"w");
  if( !pMatFile )
  {
    sprintf(SlfMatRWErrText,"SlfMatRWSetMatrix: problem SlfMatOpen with file:<%s>\n",filename);
    SlfMatRWWriteErrText(SlfMatErrText);
    return NOT_OKAY;
  }

  /* Daten bearbeiten */
  for(i=0;i<nMatrix;i++)
  {
    pMatrix[i].okay = false;
    if( (pMatrix[i].NRows*pMatrix[i].NCols) > 0 )
    {
      switch(pMatrix[i].VarType)
      {
      case DEF_ARR_FLOAT:
        {
          float *pf = (float *)pMatrix[i].pData;
          pd = new double[pMatrix[i].NRows*pMatrix[i].NCols];
          char tt[255];
          for(uint32_t idim=0;idim<pMatrix[i].NRows*pMatrix[i].NCols;idim++)
          {
            sprintf(tt,"%f",pf[idim]);
            pd[idim] = atof(tt);
  //          pd[idim] = pf[idim];
          }
        }
        break;
      case DEF_ARR_DOUBLE:
        pd = (double *)pMatrix[i].pData;
        break;
      case DEF_VEC:
        pd = (Vector_t)pMatrix[i].pData;
        break;
      case DEF_MAT:
        {
          Matrix_t pm = (Matrix_t)pMatrix[i].pData;

          if( GET_NROWS(pm) < pMatrix[i].NRows )pMatrix[i].NRows = GET_NROWS(pm);
          if( GET_NCOLS(pm) < pMatrix[i].NCols )pMatrix[i].NCols = GET_NCOLS(pm);

          pd = new double[pMatrix[i].NRows*pMatrix[i].NCols];

          for(uint32_t irow=0;irow<pMatrix[i].NRows;irow++)
          {
            for(uint32_t icol=0;icol<pMatrix[i].NCols;icol++)
            {
              pd[irow+pMatrix[i].NRows*icol] = pm[irow][icol];
            }
          }
        }
        break;
      default:
        sprintf(SlfMatRWErrText,"SlfMatRWSetMatrix: wrong type:<%s>\n",VarTypeStr[pMatrix[i].VarType]);
        return NOT_OKAY;
      }

      if( pMatrix[i].NRows > MAX_SINT32 )
      {
        sprintf(SlfMatRWErrText,"SlfMatRWSetMatrix: pMatrix[%i].NRows > MAX_SINT32\n",pMatrix[i].NRows);
        SlfMatClose(pMatFile);
        switch(pMatrix[i].VarType)
        {
        case DEF_MAT:
        case DEF_ARR_FLOAT:
          delete []pd;
          break;
        }
        return NOT_OKAY;
      }
      if( pMatrix[i].NCols > MAX_SINT32 )
      {
        sprintf(SlfMatRWErrText,"SlfMatRWSetMatrix: pMatrix[%i].NCols > MAX_SINT32\n",pMatrix[i].NCols);
        SlfMatClose(pMatFile);
        switch(pMatrix[i].VarType)
        {
        case DEF_MAT:
        case DEF_ARR_FLOAT:
          delete []pd;
          break;
        }
        return NOT_OKAY;
      }

      pdat = SlfMxCreateDoubleMatrix((int)pMatrix[i].NRows,(int)pMatrix[i].NCols,mxREAL);
      if( !pdat )
      {
        sprintf(SlfMatRWErrText,"SlfMatOpen:problem with SlfMxCreateDoubleMatrix pMatrix[%i]\n",i);
        SlfMatRWWriteErrText(SlfMatErrText);
        SlfMatClose(pMatFile);
        switch(pMatrix[i].VarType)
        {
        case DEF_MAT:
        case DEF_ARR_FLOAT:
          delete []pd;
          break;
        }
        return NOT_OKAY;
      }
      memcpy((void *)(SlfMxGetPr(pdat)), (void *)pd, sizeof(double)*(size_t)(pMatrix[i].NRows*pMatrix[i].NCols));

      if( SlfMatPutVariable(pMatFile,pMatrix[i].Name,pdat) != 0 )
      {
        sprintf(SlfMatRWErrText,"SlfMatRWSetMatrix:  SlfMatPutVariable(pMatrix[%i]\n",i);
        SlfMatRWWriteErrText(SlfMatErrText);
        SlfMatClose(pMatFile);
        SlfMxDestroyArray(pdat);
        switch(pMatrix[i].VarType)
        {
        case DEF_MAT:
        case DEF_ARR_FLOAT:
          delete []pd;
          break;
        }
        return NOT_OKAY;
      }

      SlfMxDestroyArray(pdat);

      pMatrix[i].okay = true;

      switch(pMatrix[i].VarType)
      {
      case DEF_MAT:
      case DEF_ARR_FLOAT:
        delete []pd;
        break;
      }
    }
  }
  SlfMatClose(pMatFile);
  return OKAY;
}
//===============================================================================
// SlfGetMatrix, Lesen einer Matlabdatei
//==========================================
status_t SlfMatRWGetMatrix(const char *filename, SlfMatRWMatrix *pMatrix, uint32_t nMatrix)
{
  SlfMATFile *pMatFile;
  uint32_t   i;
	int nliste;
	char **ppliste;
	SlfMATData *pdat;

  SlfMatRWErrText[0] = '\0';

  /* Matdatei anlegen */
  pMatFile = SlfMatOpen(filename,"r");
  if( !pMatFile )
  {
    sprintf(SlfMatRWErrText,"SlfMatRWGetMatrix: problem SlfMatOpen with file:<%s>\n",filename);
    SlfMatRWWriteErrText(SlfMatErrText);
    return NOT_OKAY;
  }

	ppliste = SlfMatGetDir(pMatFile, &nliste);

  /* Daten bearbeiten */
  for(i=0;i<nMatrix;i++)
  {
    pMatrix[i].okay  = false;
    /* Daten suchen */
		for(int iliste=0;iliste<nliste;iliste++) 
    {

			pdat = SlfMatGetVariable(pMatFile, ppliste[iliste]);
			
      if( SlfMxIsDouble(pdat) )
      {
        if( strcmp(pMatrix[i].Name,SlfMxGetName(pdat)) == 0 )
        {
          pMatrix[i].NRows = SlfMxGetM(pdat);
          pMatrix[i].NCols = SlfMxGetN(pdat);

          switch(pMatrix[i].VarType)
          {
          case DEF_ARR_FLOAT:
            {
              float *pf = new float[pMatrix[i].NRows*pMatrix[i].NCols];
              double *p = SlfMxGetPr(pdat);
              for(uint32_t idim=0;idim<pMatrix[i].NRows*pMatrix[i].NCols;idim++)
              {
                pf[idim] = (float)p[idim];
              }
              pMatrix[i].pData = (void *)pf;
              pMatrix[i].okay  = true;
            }
            break;
          case DEF_ARR_DOUBLE:
            {
              double *pd = new double[pMatrix[i].NRows*pMatrix[i].NCols];
              double *p = SlfMxGetPr(pdat);
              for(uint32_t idim=0;idim<pMatrix[i].NRows*pMatrix[i].NCols;idim++)
              {
                pd[idim] = p[idim];
              }
              pMatrix[i].pData = (void *)pd;
              pMatrix[i].okay  = true;
            }
            break;
          case DEF_VEC:
            {
              Vector_t pv = NewVector(pMatrix[i].NRows*pMatrix[i].NCols);
              double *p = SlfMxGetPr(pdat);
              for(uint32_t idim=0;idim<pMatrix[i].NRows*pMatrix[i].NCols;idim++)
              {
                pv[idim] = p[idim];
              }
              pMatrix[i].pData = (void *)pv;
              pMatrix[i].okay  = true;
            }
            break;
          case DEF_MAT:
            {
              Matrix_t pm = NewMatrix(pMatrix[i].NRows,pMatrix[i].NCols);
              double *p = SlfMxGetPr(pdat);
              for(uint32_t irow=0;irow<pMatrix[i].NRows;irow++)
              {
                for(uint32_t icol=0;icol<pMatrix[i].NCols;icol++)
                {
                  pm[irow][icol] = p[irow+pMatrix[i].NRows*icol];
                }
              }
              pMatrix[i].pData = (void *)pm;
              pMatrix[i].okay  = true;
            }
              break;
          default:
            sprintf(SlfMatRWErrText,"SlfMatRWGetMatrix: wrong type:<%s>\n",VarTypeStr[pMatrix[i].VarType]);
            return NOT_OKAY;
          }
        }        
      }
      if( pMatrix[i].okay ) break;
    }
  }
  return OKAY;
}


void SlfMatRWWriteErrText(char *addtext)
{
  size_t i = 0;
  size_t n = strlen(SlfMatRWErrText);


  while(i < (SLF_MATRW_ERR_TEXT_SIZE-n-1))
  {
     if( addtext[i] == '\0' ) break;
     SlfMatRWErrText[n+i] = addtext[i];
     ++i;
  }
  SlfMatRWErrText[n+i] = '\0';
}