#ifndef SLF_FKT_H_INCLUDED
#define SLF_FKT_H_INCLUDED

#define SLF_FKT_USE_STD_STRING 0

//
// status_t SlfFktUnitConv(const slf::CStr &u_from,const slf::CStr &u_to
//                        ,double *pfak,double *poffset
//                        ,slf::CStr &ErrText);
// status_t SlfFktUnitConv(const char *u_from,const char *u_to
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
#if SLF_FKT_USE_STD_STRING == 0
status_t SlfFktUnitConv(const slf::CStr &u_from,const slf::CStr &u_to,
                        double *pfak,double *poffset,slf::CStr &ErrText);
status_t SlfFktUnitConv(const char *u_from,const char *u_to,
                        double *pfak,double *poffset,slf::CStr &ErrText);
char *SlfFktUnitSI(const char *u_find,slf::CStr &ErrText);
#else
status_t SlfFktUnitConv(const std::string &u_from,const std::string &u_to,
                        double *pfak,double *poffset,std::string &ErrText);
status_t SlfFktUnitConv(const char *u_from,const char *u_to,
                        double *pfak,double *poffset,std::string &ErrText);
char *SlfFktUnitSI(const char *u_find,std::string &ErrText);
#endif

void SlfFktConvertSingle(void *pval_from, EVarType type_from
                        ,void *pval_to,   EVarType type_to
                        ,double factor,   double offset);
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
bool SlfFktIsStringToNum(const char *pstring);
status_t SlfFktConvertStringToDouble(const char *pstring,double *pval);

status_t SlfFktConvertStringToDoubleVec(const char *pstring,double **ppval,uint32_t *pnvec);
status_t SlfFktConvertStringToDoubleMat(const char *pstring,Matrix_t *pmat);
#endif
#if SLF_FKT_USE_STD_STRING == 0
status_t SlfFktConvertStringToStringVec(const char *pstring,slf::CStrV *pvstr,uint32_t *pnvstr);
#endif
#ifdef WIN32
double SlfFktGetMilliSeconds();
double SlfFktGetMilliSeconds(LONGLONG *ptimefreq_ll);
#endif

status_t SlfFktCanBufferWrite(double *pdval, double *pbuffer
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset);
status_t SlfFktCanBufferWrite(double *pdval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset);
status_t SlfFktCanBufferWrite(sint32_t intval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel);
status_t SlfFktCanBufferRead(double *pdval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset);
status_t SlfFktCanBufferRead(int *pival, uint8_t *buffer, uint8_t lbuf
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
