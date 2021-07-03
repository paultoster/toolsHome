//
#ifndef SLF_TAB_H_INCLUDED
#define SLF_TAB_H_INCLUDED

#define SLF_TAB_USE_STD_STRING          0
#define SLF_TAB_USE_C_CODE_FUNCTION     0

 
#include "SlfBasic.h"
#include "Numeric.h"
#ifdef __cplusplus
  #if SLF_TAB_USE_STD_STRING == 0
    #include "SlfStr.h"
  #else
    #include <string>
    #include <sstream>
  #endif
#endif



typedef struct tag_SSlf1DTab {

    char         *pName;
    char         *pComment;

    SSlfVec      *pXVec;
    SSlfVec      *pYVec;

    uint8_t        IsLinear;
    uint32_t       Index;
    uint8_t        IsChecked;
}SSlf1DTab;
typedef struct tag_SSlf2DTab {

    char         *pName;
    char         *pComment;

    SSlfVec      *pXVec;
    SSlfVec      *pYVec;
    SSlfMat      *pZMat;

    uint8_t        IsLinear;
    uint8_t        IsProofed;
    uint32_t       XIndex;
    uint32_t       YIndex;
}SSlf2DTab;
#ifdef __cplusplus
extern "C" {
#endif

status_t  SlfVecSet(SSlfVec *pvec
                   ,char *name, char *unit, char *comment
                   ,double *pval, uint32_t nrows);
status_t  SlfVecGet(SSlfVec *pvec,double dindex,double *pyval);
void      SlfVecFree(SSlfVec *pvec);

status_t  SlfMatSet(SSlfMat *pmat
                   ,char *name, char *unit, char *comment
                   ,double *pval, uint32_t nrows, uint32_t ncols);
void      SlfMatFree(SSlfMat *pmat);

status_t  Slf1DTabSet(SSlf1DTab *p1dtab
                     ,char *name, char *comment
                     ,char *xname, char *xunit, char * xcomment
                     ,double *xval, uint32_t nxrows
                     ,char *yname, char *yunit, char * ycomment
                     ,double *yval, uint32_t nyrows
                     ,uint8_t islinear);

status_t  Slf1DTabCheck(SSlf1DTab *p1dtab);
status_t  Slf1DTabGetVal(SSlf1DTab *pd1tab,double xval,double *pyval);
void      Slf1DTabFree(SSlf1DTab *p1dtab);

status_t  Slf1DTabGetSpzVal(SSlf1DTab *pd1tab,char *pspz,double *value);
status_t  Slf1DTabSetInd(SSlf1DTab *ptab,uint32_t index);

status_t  Slf2DTabSet(SSlf2DTab *ptab,char *name, char *comment
                     ,char *xname, char *xunit, char *xcomment, double *pxval, uint32_t nrows_x
                     ,char *yname, char *yunit, char *ycomment, double *p_yvec, uint32_t nrows_y
                     ,char *zname, char *zunit, char *zcomment, double *p_zmat, uint32_t nrows_z, uint32_t ncols_z);
status_t Slf2DTabCheck(SSlf2DTab *ptab);
status_t Slf2DTabGetVal(SSlf2DTab *ptab,double xval,double yval,double *pzval);
status_t  Slf2DTabFree(SSlf2DTab *ptab);
status_t  Slf2DTabGetSpzVal(SSlf2DTab *ptab,char *pspz,double *pval);
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

class CSlf1DTab {
public:

    CSlf1DTab();
    ~CSlf1DTab();

    status_t set(CSlf1DTab *p1dtab);

    status_t set(char *name, char *comment
                ,char *xname, char *xunit, char * xcomment
                ,double *pxval, uint32_t nxrows
                ,char *yname, char *yunit, char * ycomment
                ,double *pyval, uint32_t nyrows
                ,uint8_t islinear);

    status_t set(double *pxval, uint32_t nxrows
                ,double *pyval, uint32_t nyrows
                ,uint8_t islinear);

    status_t convXVec(char *xunit);
    status_t convYVec(char *yunit);
    status_t convXVec(double factor, double offset);
    status_t convYVec(double factor, double offset);

    void get(double xval,double *pyval, uint32_t *pindex);
    void get(double xval,double *pyval);



    void reset(void);

    status_t       getStatus(void) {return Status;}
#if SLF_TAB_USE_STD_STRING == 0
    uint32_t         getLenErrText(void) {return ErrText.getLen();}
#else
    uint32_t         getLenErrText(void) {return ErrText.length();}
#endif
    const char *   getErrText(void) {return ErrText.c_str();}
    void           resetErrText(void) {ErrText.clear();}

    double       *pXVec;
    double       *pYVec;

    uint32_t       NRows;  
    uint8_t        Order;
#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr      Name;
    slf::CStr      Comment;

    slf::CStr      XName;
    slf::CStr      XUnit;
    slf::CStr      XComment;

    slf::CStr      YName;
    slf::CStr      YUnit;
    slf::CStr      YComment;
#else
    std::string      Name;
    std::string      Comment;

    std::string      XName;
    std::string      XUnit;
    std::string      XComment;

    std::string      YName;
    std::string      YUnit;
    std::string      YComment;
#endif

private:

    status_t Status;
#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr ErrText;
#else
    std::string ErrText;
#endif
    
    uint32_t        Index;

};
class CSlf2DTab {
public:

    CSlf2DTab();
    ~CSlf2DTab();

    status_t set(CSlf2DTab *p2dtab);

    status_t set(char *name, char *comment
                ,char *xname, char *xunit, char * xcomment
                ,double *pxval, uint32_t nxrows
                ,char *yname, char *yunit, char * ycomment
                ,double *pyval, uint32_t nyrows
                ,char *zname, char *zunit, char * zcomment
                ,Matrix_t zmat,uint8_t islinear);



   status_t set(double *pxval, uint32_t nxrows
                ,double *pyval, uint32_t nyrows
                ,Matrix_t zmat, uint8_t order);


    status_t convXVec(char *xunit);
    status_t convYVec(char *yunit);
    status_t convZMat(char *zunit);
    status_t convXVec(double factor, double offset);
    status_t convYVec(double factor, double offset);
    status_t convZMat(double factor, double offset);

    void get(double xval,double yval,double *pzval);



    void reset(void);

    status_t       getStatus(void) {return Status;}
#if SLF_TAB_USE_STD_STRING == 0
    uint32_t         getLenErrText(void) {return ErrText.getLen();}
#else
    uint32_t         getLenErrText(void) {return ErrText.length();}
#endif
    const char *   getErrText(void) {return ErrText.c_str();}
    void           resetErrText(void) {ErrText.clear();}

    double       *pXVec;
    uint32_t       XNRows;  
    double       *pYVec;
    uint32_t       YNRows;  
    Matrix_t       ZMat;
    uint8_t        Order;
#if SLF_TAB_USE_STD_STRING == 0

    slf::CStr      Name;
    slf::CStr      Comment;

    slf::CStr      XName;
    slf::CStr      XUnit;
    slf::CStr      XComment;

    slf::CStr      YName;
    slf::CStr      YUnit;
    slf::CStr      YComment;


    slf::CStr      ZName;
    slf::CStr      ZUnit;
    slf::CStr      ZComment;
#else
    std::string      Name;
    std::string      Comment;

    std::string      XName;
    std::string      XUnit;
    std::string      XComment;

    std::string      YName;
    std::string      YUnit;
    std::string      YComment;


    std::string      ZName;
    std::string      ZUnit;
    std::string      ZComment;
#endif

private:

    status_t Status;
#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr ErrText;
#else
    std::string ErrText;
#endif
    

};
#endif

#if 0
status_t  SlfVecSet(SSlfVec *pvec
                   ,char *name, char *unit, char *comment
                   ,double *pval, uint32_t nrows
                   ,slf::CStr *perrtext=0);
void      SlfVecFree(SSlfVec *pvec);

status_t  Slf1DTabSet(SSlf1DTab *p1dtab
                     ,char *name, char *comment
                     ,char *xname, char *xunit, char * xcomment
                     ,double *xval, uint32_t nxrows
                     ,char *yname, char *yunit, char * ycomment
                     ,double *yval, uint32_t nyrows
                     ,uint8_t islinear
                     ,slf::CStr *perrtext=0);

status_t  Slf1DTabCheck(SSlf1DTab *p1dtab,slf::CStr *perrtext=0);
status_t  Slf1DTabGetVal(SSlf1DTab *pd1tab,double xval,double *pyval);
void      Slf1DTabFree(SSlf1DTab *p1dtab);

#endif

#endif