#ifndef SLF_FKT_H_INCLUDED
#define SLF_FKT_H_INCLUDED

#define SLF_FKT_USE_STD_STRING 0

//
// okay_t SlfFktUnitConv(const slf::CStr &u_from,const slf::CStr &u_to
//                        ,double *pfak,double *poffset
//                        ,slf::CStr &ErrText);
// okay_t SlfFktUnitConv(const char *u_from,const char *u_to
//                        ,double *pfak,double *poffset
//                        ,slf::CStr &ErrText);
//
// Einheiten konvertieren von u_from zu u_to mit der Vorschrift
// Wenn nicht gefunden, dann kann ErrText beschrieben werden
//
// val_to = val_from*fak + offset;
// Beispiel:
// u_from = "mm"; u_to = "m"; => fak = 0.001; offset = 0.0;
#ifdef WIN32
#include "windows.h"
#endif
#if SLF_FKT_USE_STD_STRING == 0
  #include "SlfStr.h"
  #include "SlfStrV.h"
  #include "SlfStrM.h"
#else 
  #include "SlfBasic.h"
  #include "Numeric.h"
  #include <string>
  #include <sstream>
#endif
#include "SlfModVar.h"

namespace slf
{

char *SlfFktUnitSI(const char *u_find,CStr &ErrText);

template<typename Tin, typename Tout, typename T>
void SlfFktConvertSingle(const Tin &val_from, Tout &val_to, T &factor, T &offset)
{
  val_to = (Tout)val_from * (Tout)factor + (Tout)offset;
}

okay_t SlfFktUnitConv(const char *u_from
  , const char *u_to
  , double &fak
  , double &offset
  , CStr &ErrText);


//===========================================================================================================================================
//===========================================================================================================================================
//===========================================================================================================================================
template<typename T>
void SlfFktConvertSingle(double *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(double *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (double)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(float *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (float)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int64_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (int64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint64_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (uint64_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int32_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (int32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint32_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (uint32_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int16_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (int16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint16_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (uint16_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(int8_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (int8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, double *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, float *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, int64_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, uint64_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, int32_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, uint32_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, int16_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, uint16_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, int8_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
template<typename T>
void SlfFktConvertSingle(uint8_t *pval_from, uint8_t *pval_to, T &factor, T &offset){ *pval_from = (uint8_t)(*pval_from * factor + offset); }
//===========================================================================================================================================
//===========================================================================================================================================
//===========================================================================================================================================
inline
void SlfFktConvertSingle(double *pval_from, double *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, float *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, int64_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, uint64_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, int32_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, uint32_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, int16_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, uint16_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, int8_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(double *pval_from, uint8_t *pval_to){ *pval_from = (double)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, double *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, float *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, int64_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, uint64_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, int32_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, uint32_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, int16_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, uint16_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, int8_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(float *pval_from, uint8_t *pval_to){ *pval_from = (float)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, double *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, float *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, int64_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, uint64_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, int32_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, uint32_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, int16_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, uint16_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, int8_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int64_t *pval_from, uint8_t *pval_to){ *pval_from = (int64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, double *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, float *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, int64_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, uint64_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, int32_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, uint32_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, int16_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, uint16_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, int8_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint64_t *pval_from, uint8_t *pval_to){ *pval_from = (uint64_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, double *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, float *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, int64_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, uint64_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, int32_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, uint32_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, int16_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, uint16_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, int8_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int32_t *pval_from, uint8_t *pval_to){ *pval_from = (int32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, double *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, float *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, int64_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, uint64_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, int32_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, uint32_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, int16_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, uint16_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, int8_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint32_t *pval_from, uint8_t *pval_to){ *pval_from = (uint32_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, double *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, float *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, int64_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, uint64_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, int32_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, uint32_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, int16_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, uint16_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, int8_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int16_t *pval_from, uint8_t *pval_to){ *pval_from = (int16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, double *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, float *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, int64_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, uint64_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, int32_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, uint32_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, int16_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, uint16_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, int8_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint16_t *pval_from, uint8_t *pval_to){ *pval_from = (uint16_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, double *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, float *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, int64_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, uint64_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, int32_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, uint32_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, int16_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, uint16_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, int8_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(int8_t *pval_from, uint8_t *pval_to){ *pval_from = (int8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, double *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, float *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, int64_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, uint64_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, int32_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, uint32_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, int16_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, uint16_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, int8_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
inline
void SlfFktConvertSingle(uint8_t *pval_from, uint8_t *pval_to){ *pval_from = (uint8_t)(*pval_from); }
//===========================================================================================================================================
//===========================================================================================================================================
//===========================================================================================================================================

bool SlfFktIsStringToNum(const char *pstring);
okay_t SlfFktConvertStringToDouble(const char *pstring, double *pval);
okay_t SlfFktConvertStringToDouble(const CStr &str, double &val);
okay_t SlfFktWriteAsciiToFile(const CStrV &strv, const char *filename);

#if 0

void SlfFktConvertArray(void *pval_from, EVarType type_from, uint32_t n_from
                        ,void *pval_to,   EVarType type_to, uint32_t n_to
                        ,double factor,   double offset
                        );
void SlfFktConvertSingleToVector(void *pval_from, EVarType type_from
                                ,void *pval_to,   EVarType type_to
                                ,uint32_t ivec
                                ,double factor,   double offset);
#if SLF_FKT_USE_STD_STRING == 0
void SlfFktConvertString(void *pval_from, EVarType type_from, uint32_t n_from
                        ,void *pval_to,   EVarType type_to, uint32_t n_to
                        );

okay_t SlfFktConvertStringToDoubleVec(const char *pstring,double **ppval,uint32_t *pnvec);
okay_t SlfFktConvertStringToDoubleMat(const char *pstring,Matrix_t *pmat);
#endif
#if SLF_FKT_USE_STD_STRING == 0
okay_t SlfFktConvertStringToStringVec(const char *pstring,slf::CStrV *pvstr,uint32_t *pnvstr);
#endif
#ifdef WIN32
double SlfFktGetMilliSeconds();
double SlfFktGetMilliSeconds(LONGLONG *ptimefreq_ll);
#endif

okay_t SlfFktCanBufferWrite(double *pdval, double *pbuffer
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset);
okay_t SlfFktCanBufferWrite(double *pdval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset);
okay_t SlfFktCanBufferWrite(sint32_t intval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel);
okay_t SlfFktCanBufferRead(double *pdval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset);
okay_t SlfFktCanBufferRead(int *pival, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel);
// Berechnet Anzahl der Schleifen bei differierender Loop-Zeiten
// positiv wenn maste_loop_time > function_loop_time
// neagativ wenn <
int SlfFktCalcNLoop(double maste_loop_time,double function_loop_time);

// Prüft freien Speicher, Angabe in kbyte
// type = "b" => byte
//      = "k" => kilobyte
//      = "m" => megabyte
//      = "g" => gigabyte
double SlfFktCalcFreeMemory(char *type);
#endif
}
#endif
