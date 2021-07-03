#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "SlfTab.h"
#include "Numeric.h"
#include "SlfFkt.h"

#if SLF_TAB_USE_C_CODE_FUNCTION > 0
#ifdef __cplusplus
extern "C" {
#endif
#include "SlfTabC.c"
#ifdef __cplusplus
}
#endif
#endif



CSlf1DTab::CSlf1DTab() {

    Status = OKAY;

    pXVec = 0;
    pYVec = 0;

    NRows = 0;
    
    Order = 1; //Default
    Index    = 0;

}
CSlf1DTab::~CSlf1DTab() {

    reset();
}
status_t CSlf1DTab::set(CSlf1DTab *p1dtab) {

    Name     = p1dtab->Name;
    Comment  = p1dtab->Comment;
    NRows    = p1dtab->NRows;

    if( NRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
      ErrText.catFormat("CSlf1DTab::set: In Tabelle <%s> ist Länge Vektor "
                        "gleich null"
                        ,Name.c_str());
#else
      std::stringstream out;
      out << "CSlf1DTab::set: In Tabelle <" << Name.c_str() << "> ist Länge Vektor gleich null";
      ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }

    // x-Vektor anlegen
    //=================
    XName    = p1dtab->XName;
    XUnit    = p1dtab->XUnit;
    XComment = p1dtab->XComment;

    if( pXVec )
        delete []pXVec;

    pXVec    = new double[NRows];

    memcpy(pXVec, p1dtab->pXVec, sizeof(double)*NRows);

    // y-Vektor anlegen
    //=================
    YName    = p1dtab->YName;
    YUnit    = p1dtab->YUnit;
    YComment = p1dtab->YComment;


    if( pYVec )
        delete []pYVec;

    pYVec    = new double[NRows];

    memcpy(pYVec, p1dtab->pYVec, sizeof(double)*NRows);


    Order = p1dtab->Order;

    return Status;
}
status_t CSlf1DTab::set(char *name, char *comment
                       ,char *xname, char *xunit, char * xcomment
                       ,double *pxval, uint32_t nxrows
                       ,char *yname, char *yunit, char * ycomment
                       ,double *pyval, uint32_t nyrows
                       ,uint8_t order) {


    uint32_t i;
    double Test;

    Name     = name;
    if( comment ) Comment  = comment;


    // Länge der Vektoren
    //===================
    NRows    = MIN(nyrows,nxrows);

    if( NRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf1DTab::set: In Tabelle <%s> ist Länge Vektor "
                          "gleich null(nxrows=%g oder nyrows=%g)"
                          ,name
                          ,nxrows,nyrows);
#else
      std::stringstream out;
      out << "CSlf1DTab::set: In Tabelle <" << name << "> ist Länge Vektor gleich null(nxrows=" << nxrows << " oder nyrows=" << nyrows << ")";
      ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }

    // x-Vektor anlegen
    //=================
    XName    = xname;
    if( xunit    ) XUnit    = xunit;
    if( xcomment ) XComment = xcomment;

    if( pXVec )
        delete []pXVec;

    pXVec    = new double[NRows];

    memcpy(pXVec, pxval, sizeof(double)*NRows);

    // y-Vektor anlegen
    //=================
    YName    = yname;
    if( yunit    ) YUnit    = yunit;
    if( ycomment ) YComment = ycomment;

    if( pYVec )
        delete []pYVec;

    pYVec    = new double[NRows];
    memcpy(pYVec, pyval, sizeof(double)*NRows);


    Order = order;

    i = 0;
    // Interpolation testen
    if( NumericInterp1D(1
                      ,pXVec
                      ,pYVec
                      ,NRows
                      ,0.0
                      ,&Test
                      ,&i,Order,0) != OKAY ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf1DTab::set: Problem NumericInterp1D: Tabelle <%s> Interpolation\n"
                               ,Name.c_str());
        
        ErrText.catFormat("xVector <%s> is not moton increasing\n",XName.c_str());
#else
      std::stringstream out;
      out << "CSlf1DTab::set: Problem NumericInterp1D: Tabelle <" << Name.c_str() << "> Interpolation";
      out << "xVector <" << XName.c_str() << "> is not moton increasing\n";
      ErrText = out.str();
#endif
        Status = NOT_OKAY;
    }

    return Status;
}
status_t CSlf1DTab::set(double *pxval, uint32_t nxrows
                       ,double *pyval, uint32_t nyrows
                       ,uint8_t order) {


    uint32_t i;
    double Test;

    Name     = "1dtable";
    Comment  = "";


    // Länge der Vektoren
    //===================
    NRows    = MIN(nyrows,nxrows);

    if( NRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf1DTab::set: In Tabelle <%s> ist Länge Vektor "
                          "gleich null(nxrows=%g oder nyrows=%g)"
                          ,Name.c_str()
                          ,nxrows,nyrows);
#else
      std::stringstream out;
      out << "CSlf1DTab::set: In Tabelle <" << Name.c_str() << " ist Länge Vektor gleich null(nxrows=" << nxrows <<  "oder nyrows=" << nyrows;
      ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }

    // x-Vektor anlegen
    //=================
    XName    = "xvec";

    if( pXVec )
        delete []pXVec;

    pXVec    = new double[NRows];

    memcpy(pXVec, pxval, sizeof(double)*NRows);

    // y-Vektor anlegen
    //=================
    YName    = "yvec";

    if( pYVec )
        delete []pYVec;

    pYVec    = new double[NRows];
    memcpy(pYVec, pyval, sizeof(double)*NRows);


    Order = order;

    i = 0;
    // Interpolation testen
    if( NumericInterp1D(1
                      ,pXVec
                      ,pYVec
                      ,NRows
                      ,0.0
                      ,&Test
                      ,&i,Order,0) != OKAY ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf1DTab::set: Problem NumericInterp1D: Tabelle <%s> Interpolation\n"
                               ,Name.c_str());
        
        ErrText.catFormat("xVector <%s> is not moton increasing\n",XName.c_str());
#else
      std::stringstream out;
      out << "CSlf1DTab::set: Problem NumericInterp1D: Tabelle <" << Name.c_str() << "> Interpolation";
      out << "xVector <" << XName.c_str() << "> is not moton increasing\n";
      ErrText = out.str();
#endif
        Status = NOT_OKAY;
    }

    return Status;
}
status_t CSlf1DTab::convXVec(char *xunit) {

#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr newunit = xunit;
#else
  std::string newunit = xunit;
#endif
    double factor,offset;

    if( pXVec && (XUnit != xunit) ) {


        if( (Status = SlfFktUnitConv(XUnit /*=>*/
                                    ,newunit
                                    ,&factor
                                    ,&offset
                                    ,ErrText
                                    ))          != OKAY )  {
#if SLF_TAB_USE_STD_STRING == 0
            ErrText.catFormat("\nCSlf1DTab::convXVec Problem mit Einheiten\n");
            ErrText.catFormat("Die Tabellle <%s> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n"
                             ,Name.c_str());
            ErrText.catFormat("aktuelle Einheit Tabelle [%s]\n"
                             ,XUnit.c_str());
            ErrText.catFormat("neue Einheit Tabelle [%s]\n"
                             ,newunit.c_str());
#else
      std::stringstream out;
      out << "\nCSlf1DTab::convXVec Problem mit Einheiten\nDie Tabellle <" << Name.c_str() << "> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n";
          out << "aktuelle Einheit Tabelle [" << XUnit.c_str() << "]\n";
          out << "neue Einheit Tabelle [" << newunit.c_str() << "]\n";
      ErrText = out.str();
#endif
            Status = NOT_OKAY;
            return Status;
        }
        for(uint32_t i=0;i<NRows;i++)

            pXVec[i] = pXVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf1DTab::convYVec(char *yunit) {

#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr newunit = yunit;
#else
  std::string newunit = yunit;
#endif
    double factor,offset;

    if( pYVec && (YUnit != yunit) ) {


        if( (Status = SlfFktUnitConv(YUnit /*=>*/
                                    ,newunit
                                    ,&factor
                                    ,&offset
                                    ,ErrText
                                    ))          != OKAY )  {
#if SLF_TAB_USE_STD_STRING == 0
            ErrText.catFormat("\nCSlf1DTab::convYVec Problem mit Einheiten\n");
            ErrText.catFormat("Die Tabellle <%s> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n"
                             ,Name.c_str());
            ErrText.catFormat("aktuelle Einheit Tabelle [%s]\n"
                             ,YUnit.c_str());
            ErrText.catFormat("neue Einheit Tabelle [%s]\n"
                             ,newunit.c_str());
#else
          std::stringstream out;
          out << "\nCSlf1DTab::convXVec Problem mit Einheiten\nDie Tabellle <" << Name.c_str() << "> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n";
          out << "aktuelle Einheit Tabelle [" << YUnit.c_str() << "]\n";
          out << "neue Einheit Tabelle [" << newunit.c_str() << "]\n";
          ErrText = out.str();
#endif
            Status = NOT_OKAY;
            return Status;
        }
        for(uint32_t i=0;i<NRows;i++)

            pYVec[i] = pYVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf1DTab::convXVec(double factor, double offset) {


    if( pXVec && (fabs(factor-1.0) > EPSILON || fabs(offset-0.0) > EPSILON) ) {

        for(uint32_t i=0;i<NRows;i++)

            pXVec[i] = pXVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf1DTab::convYVec(double factor, double offset) {

    if( pYVec && (fabs(factor-1.0) > EPSILON || fabs(offset-0.0) > EPSILON) ) {

        for(uint32_t i=0;i<NRows;i++)

            pYVec[i] = pYVec[i]*factor+offset;
    }
    
    return Status;
}
void CSlf1DTab::get(double xval,double *pyval, uint32_t *pindex) {

    NumericInterp1D(0
                  ,pXVec
                  ,pYVec
                  ,NRows
                  ,xval,pyval
                  ,pindex
                  ,Order
                  ,0);

    Index = *pindex;


}
void CSlf1DTab::get(double xval,double *pyval) {

    NumericInterp1D(0
                  ,pXVec
                  ,pYVec
                  ,NRows
                  ,xval,pyval
                  ,&Index
                  ,Order
                  ,0);

}
void CSlf1DTab::reset(void) {

    if( pXVec )
        delete []pXVec;
    pXVec = 0;

    if( pYVec )
        delete []pYVec;
    pYVec = 0;

    NRows = 0;
    
    Order = 1; //Default
    Index    = 0;
}
//======================================
//======================================
//======================================
//2D-TAbelle
//======================================
//======================================
//======================================
CSlf2DTab::CSlf2DTab() {

    Status = OKAY;

    pXVec = 0;
    pYVec = 0;
    ZMat  = 0;

    XNRows = 0;
    YNRows = 0;
    
    Order = 1; //Default

}
CSlf2DTab::~CSlf2DTab() {

    reset();
}
status_t CSlf2DTab::set(CSlf2DTab *p2dtab) {

    uint32_t i,j;

    Name     = p2dtab->Name;
    Comment  = p2dtab->Comment;

    // x-Vektor anlegen
    //=================
    XName    = p2dtab->XName;
    XUnit    = p2dtab->XUnit;
    XComment = p2dtab->XComment;
    XNRows   = MIN(p2dtab->XNRows,GET_NCOLS(p2dtab->ZMat));

    if( XNRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: In Tabelle <%s> ist Länge x-Vektor "
                          "gleich null"
                          ,Name.c_str());
#else
        std::stringstream out;
        out << "CSlf2DTab::set: In Tabelle <" << Name.c_str() << "> ist Länge x-Vektor gleich null\n";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }


    if( pXVec )
        delete []pXVec;

    pXVec    = new double[XNRows];

    memcpy(pXVec, p2dtab->pXVec, sizeof(double)*XNRows);

    // y-Vektor anlegen
    //=================
    YName    = p2dtab->YName;
    YUnit    = p2dtab->YUnit;
    YComment = p2dtab->YComment;
    YNRows   = MIN(p2dtab->YNRows,GET_NROWS(p2dtab->ZMat));

    if( YNRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: In Tabelle <%s> ist Länge y-Vektor "
                          "gleich null"
                          ,Name.c_str());
#else
        std::stringstream out;
        out << "CSlf2DTab::set: In Tabelle <" << Name.c_str() << "> ist Länge y-Vektor gleich null\n";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }


    if( pYVec )
        delete []pYVec;

    pYVec    = new double[YNRows];

    memcpy(pYVec, p2dtab->pYVec, sizeof(double)*YNRows);

    // z-Matrix anlegen
    //=================
    ZName    = p2dtab->ZName;
    ZUnit    = p2dtab->ZUnit;
    ZComment = p2dtab->ZComment;


    if( ZMat )
        FreeMatrix(ZMat);

    ZMat    = NewMatrix(YNRows,XNRows);

    for(i=0;i<YNRows;i++)
        for(j=0;j<XNRows;j++)
            ZMat[i][j] = p2dtab->ZMat[i][j];

    Order = p2dtab->Order;

    return Status;
}
status_t CSlf2DTab::set(char *name, char *comment
                       ,char *xname, char *xunit, char * xcomment
                       ,double *pxval, uint32_t nxrows
                       ,char *yname, char *yunit, char * ycomment
                       ,double *pyval, uint32_t nyrows
                       ,char *zname, char *zunit, char * zcomment
                       ,Matrix_t zmat
                       ,uint8_t order) {


    uint32_t i,j;
    int    ii;
    double Test;

    Name     = name;
    if( comment ) Comment  = comment;


    // x-Vektor anlegen
    //=================
    XName    = xname;
    if( xunit    ) XUnit    = xunit;
    if( xcomment ) XComment = xcomment;

    // Länge der X-Vektoren
    //===================
    XNRows    = MIN(nxrows,GET_NCOLS(zmat));

    if( XNRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: In Tabelle <%s> ist Länge x-Vektor "
                          "gleich null(nxrows=%i oder nyrows=%i oder zmat(%i,%i))"
                          ,name
                          ,nxrows,nyrows,GET_NROWS(zmat),GET_NCOLS(zmat));
#else
      std::stringstream out;
      out << "CSlf2DTab::set: In Tabelle <" << name << "> ist Länge x-Vektor gleich null(nxrows=" << nxrows;
      out << " oder nyrows=" << nyrows << " oder zmat(" << GET_NROWS(zmat) << "," << GET_NCOLS(zmat) << ")";
      ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }


    if( pXVec )
        delete []pXVec;

    pXVec    = new double[XNRows];

    memcpy(pXVec, pxval, sizeof(double)*XNRows);

    // y-Vektor anlegen
    //=================
    YName    = yname;
    if( yunit    ) YUnit    = yunit;
    if( ycomment ) YComment = ycomment;

    // Länge der X-Vektoren
    //===================
    YNRows    = MIN(nyrows,GET_NROWS(zmat));

    if( YNRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: In Tabelle <%s> ist Länge y-Vektor "
                          "gleich null(nxrows=%i oder nyrows=%i oder zmat(%i,%i))"
                          ,name
                          ,nxrows,nyrows,GET_NROWS(zmat),GET_NCOLS(zmat));
#else
        std::stringstream out;
        out << "CSlf2DTab::set: In Tabelle <" << name << "> ist Länge y-Vektor gleich null(nxrows=" << nxrows;
        out << " oder nyrows=" << nyrows << " oder zmat(" << GET_NROWS(zmat) << "," << GET_NCOLS(zmat) << ")";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }

    if( pYVec )
        delete []pYVec;

    pYVec    = new double[YNRows];
    memcpy(pYVec, pyval, sizeof(double)*YNRows);

    // z-Matrix anlegen
    //=================
    ZName    = zname;
    if( zunit    ) ZUnit    = zunit;
    if( zcomment ) ZComment = zcomment;

    if( ZMat )
        FreeMatrix(ZMat);

    ZMat    = NewMatrix(YNRows,XNRows);

    for(i=0;i<YNRows;i++)
        for(j=0;j<XNRows;j++)
            ZMat[i][j] = zmat[i][j];

    Order = order;

    // Interpolation testen
    ii = NumericInterp2D(1
                       ,pXVec
                       ,XNRows
                       ,pYVec
                       ,YNRows
                       ,ZMat
                       ,0.0
                       ,0.0
                       ,&Test);
    if( ii ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: Problem NumericInterp2D: Tabelle <%s> Interpolation\n"
                               ,Name.c_str());
        if( ii == 2 )
            ErrText.catFormat("xVector <%s> or yVector <%s> is not moton increasing\n",XName.c_str(),YName.c_str());
        else if(ii == 3 )
            ErrText.catFormat("zMatrix <%s> is too small\n",ZName.c_str());
#else
        std::stringstream out;
        out << "CSlf2DTab::set: Problem NumericInterp2D: Tabelle <" << Name.c_str() << "> Interpolation\n";
        if( ii == 2 )
            out << "xVector <" << XName.c_str() << "> or yVector <" << YName.c_str() << "> is not moton increasing\n";
        else if(ii == 3 )
            out << "zMatrix <" << ZName.c_str() << "> is too small\n";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
    }

    return Status;
}
status_t CSlf2DTab::set(double *pxval, uint32_t nxrows
                       ,double *pyval, uint32_t nyrows
                       ,Matrix_t zmat
                       ,uint8_t order) {


    uint32_t i,j;
    int ii;
    double Test;

    Name     = "2dtable";
    Comment  = "";


    // Länge der x-Vektoren
    //===================
    XNRows    = MIN(nxrows,GET_NCOLS(zmat));

    if( XNRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: In Tabelle <%s> ist Länge x-Vektor "
                          "gleich null(nxrows=%i oder zmat(:,nx=%i))"
                          ,Name.c_str()
                          ,nxrows,GET_NCOLS(zmat));
#else
        std::stringstream out;
        out << "CSlf2DTab::set: In Tabelle <" << Name.c_str() << "> ist Länge x-Vektor gleich null";
        out << "(nxrows="<< nxrows <<" oder zmat(:,nx=" << GET_NCOLS(zmat) << "))";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }

    // x-Vektor anlegen
    //=================
    XName    = "xvec";

    if( pXVec )
        delete []pXVec;

    pXVec    = new double[XNRows];

    memcpy(pXVec, pxval, sizeof(double)*XNRows);

    // Länge der y-Vektoren
    //===================
    YNRows    = MIN(nyrows,GET_NROWS(zmat));

    if( YNRows  == 0 ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: In Tabelle <%s> ist Länge y-Vektor "
                          "gleich null(nyrows=%i oder zmat(ny=%i,:))"
                          ,Name.c_str()
                          ,nyrows,GET_NROWS(zmat));
#else
        std::stringstream out;
        out << "CSlf2DTab::set: In Tabelle <" << Name.c_str() << "> ist Länge y-Vektor gleich null";
        out << "(nyrows="<< nyrows <<" oder zmat(ny=" << GET_NROWS(zmat) << ",:))";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
        return Status;
    }

    // y-Vektor anlegen
    //=================
    YName    = "yvec";

    if( pYVec )
        delete []pYVec;

    pYVec    = new double[YNRows];
    memcpy(pYVec, pyval, sizeof(double)*YNRows);


    Order = order;

    // z-Matrix anlegen
    //=================
    ZName    = "zmat";

    if( ZMat )
        FreeMatrix(ZMat);

    ZMat    = NewMatrix(YNRows,XNRows);

    for(i=0;i<YNRows;i++)
        for(j=0;j<XNRows;j++)
            ZMat[i][j] = zmat[i][j];

    Order = order;

    // Interpolation testen
    ii = NumericInterp2D(1
                       ,pXVec
                       ,XNRows
                       ,pYVec
                       ,YNRows
                       ,ZMat
                       ,0.0
                       ,0.0
                       ,&Test);
    if( ii ) {
#if SLF_TAB_USE_STD_STRING == 0
        ErrText.catFormat("CSlf2DTab::set: Problem NumericInterp2D: Tabelle <%s> Interpolation\n"
                               ,Name.c_str());
        
        if( ii == 2 )
            ErrText.catFormat("xVector <%s> or yVector <%s> is not moton increasing\n",XName.c_str(),YName.c_str());
        else if(ii == 3 )
            ErrText.catFormat("zMatrix <%s> is too small\n",ZName.c_str());
#else
        std::stringstream out;
        out << "CSlf2DTab::set: Problem NumericInterp2D: Tabelle <" << Name.c_str() << "> Interpolation\n";
        if( ii == 2 )
            out << "xVector <" << XName.c_str() << "> or yVector <" << YName.c_str() << "> is not moton increasing\n";
        else if(ii == 3 )
            out << "zMatrix <" << ZName.c_str() << "> is too small\n";
        ErrText = out.str();
#endif
        Status = NOT_OKAY;
    }

    return Status;
}
status_t CSlf2DTab::convXVec(char *xunit) {

#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr newunit = xunit;
#else
  std::string newunit = xunit;
#endif
    double factor,offset;

    if( pXVec && (XUnit != xunit) ) {


        if( (Status = SlfFktUnitConv(XUnit /*=>*/
                                    ,newunit
                                    ,&factor
                                    ,&offset
                                    ,ErrText
                                    ))          != OKAY )  {
#if SLF_TAB_USE_STD_STRING == 0
            ErrText.catFormat("\nCSlf2DTab::convXVec Problem mit Einheiten\n");
            ErrText.catFormat("Die Tabellle <%s> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n"
                             ,Name.c_str());
            ErrText.catFormat("aktuelle Einheit Tabelle [%s]\n"
                             ,XUnit.c_str());
            ErrText.catFormat("neue Einheit Tabelle [%s]\n"
                             ,newunit.c_str());
#else
        std::stringstream out;
        out << "\nCSlf2DTab::convXVec Problem mit Einheiten\n";
        out << "Die Tabellle <" << Name.c_str() << "> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n";
        out << "aktuelle Einheit Tabelle [" << XUnit.c_str() << "]\n";
        out << "neue Einheit Tabelle [" << newunit.c_str() << "]\n";
        ErrText = out.str();
#endif
            Status = NOT_OKAY;
            return Status;
        }
        for(uint32_t i=0;i<XNRows;i++)

            pXVec[i] = pXVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf2DTab::convYVec(char *yunit) {

#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr newunit = yunit;
#else
    std::string newunit = yunit;
#endif
    double factor,offset;

    if( pYVec && (YUnit != yunit) ) {


        if( (Status = SlfFktUnitConv(YUnit /*=>*/
                                    ,newunit
                                    ,&factor
                                    ,&offset
                                    ,ErrText
                                    ))          != OKAY )  {
#if SLF_TAB_USE_STD_STRING == 0
            ErrText.catFormat("\nCSlf2DTab::convYVec Problem mit Einheiten\n");
            ErrText.catFormat("Die Tabellle <%s> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n"
                             ,Name.c_str());
            ErrText.catFormat("aktuelle Einheit Tabelle [%s]\n"
                             ,YUnit.c_str());
            ErrText.catFormat("neue Einheit Tabelle [%s]\n"
                             ,newunit.c_str());
#else
            std::stringstream out;
            out << "\nCSlf2DTab::convXVec Problem mit Einheiten\n";
            out << "Die Tabellle <" << Name.c_str() << "> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n";
            out << "aktuelle Einheit Tabelle [" << YUnit.c_str() << "]\n";
            out << "neue Einheit Tabelle [" << newunit.c_str() << "]\n";
            ErrText = out.str();
#endif
            Status = NOT_OKAY;
            return Status;
        }
        for(uint32_t i=0;i<YNRows;i++)

            pYVec[i] = pYVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf2DTab::convZMat(char *zunit) {

#if SLF_TAB_USE_STD_STRING == 0
    slf::CStr newunit = zunit;
#else
    std::string newunit = zunit;
#endif
    double factor,offset;

    if( ZMat && (ZUnit != zunit) ) {


        if( (Status = SlfFktUnitConv(ZUnit /*=>*/
                                    ,newunit
                                    ,&factor
                                    ,&offset
                                    ,ErrText
                                    ))          != OKAY )  {
#if SLF_TAB_USE_STD_STRING == 0
            ErrText.catFormat("\nCSlf2DTab::convZMat Problem mit Einheiten\n");
            ErrText.catFormat("Die Tabellle <%s> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n"
                             ,Name.c_str());
            ErrText.catFormat("aktuelle Einheit Tabelle [%s]\n"
                             ,ZUnit.c_str());
            ErrText.catFormat("neue Einheit Tabelle [%s]\n"
                             ,newunit.c_str());
#else
            std::stringstream out;
            out << "\nCSlf2DTab::convXVec Problem mit Einheiten\n";
            out << "Die Tabellle <" << Name.c_str() << "> hat nicht die passende Einheit zu der angeforderten Einheit !!!\n";
            out << "aktuelle Einheit Tabelle [" << ZUnit.c_str() << "]\n";
            out << "neue Einheit Tabelle [" << newunit.c_str() << "]\n";
            ErrText = out.str();
#endif
            Status = NOT_OKAY;
            return Status;
        }
        for(uint32_t i=0;i<YNRows;i++)
            for(uint32_t j=0;j<XNRows;j++)

            ZMat[i][j] = ZMat[i][j]*factor+offset;
    }
    
    return Status;
}
status_t CSlf2DTab::convXVec(double factor, double offset) {


    if( pXVec && (fabs(factor-1.0) > EPSILON || fabs(offset-0.0) > EPSILON) ) {

        for(uint32_t i=0;i<XNRows;i++)

            pXVec[i] = pXVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf2DTab::convYVec(double factor, double offset) {

    if( pYVec && (fabs(factor-1.0) > EPSILON || fabs(offset-0.0) > EPSILON) ) {

        for(uint32_t i=0;i<YNRows;i++)

            pYVec[i] = pYVec[i]*factor+offset;
    }
    
    return Status;
}
status_t CSlf2DTab::convZMat(double factor, double offset) {

    if( ZMat && (fabs(factor-1.0) > EPSILON || fabs(offset-0.0) > EPSILON) ) {

        for(uint32_t i=0;i<YNRows;i++)
            for(uint32_t j=0;i<XNRows;j++)

                ZMat[i][j] = ZMat[i][j]*factor+offset;
    }
    
    return Status;
}
void CSlf2DTab::get(double xval,double yval,double *pzval) {

    NumericInterp2D(0
                  ,pXVec
                  ,XNRows
                  ,pYVec
                  ,YNRows
                  ,ZMat
                  ,xval,yval,pzval);

}
void CSlf2DTab::reset(void) {

    if( pXVec )
        delete []pXVec;
    pXVec = 0;

    if( pYVec )
        delete []pYVec;
    pYVec = 0;

    YNRows = 0;

    if( ZMat )
        FreeMatrix(ZMat);
    ZMat = 0;
    
    Order = 1; //Default
}




#if 0
// Vektor
//===========
status_t  SlfVecSet(SSlfVec *pvec
                   ,char *name, char *unit, char * comment
                   ,double *pval, uint32_t nrows
                   ,slf::CStr *perrtext /*=0*/) {

    pvec->pName    = 0;
    pvec->pComment = 0;
    pvec->pUnit    = 0;
    pvec->NRows    = 0;
	pvec->pVec     = 0;

    // NAme
    if( name ) {
	    pvec->pName = (char *)malloc(sizeof(char)*(SlfStrLen(name)+1));
        if( !pvec->pName ) {
            if( perrtext )
                perrtext->cat("Problem SlfVecSet: malloc Name !!!");
            return NOT_OKAY;
        }
        SlfStrCpy(pvec->pName,SlfStrLen(name),name);
    }

    // Comment
    if( comment ) {
	    pvec->pComment = (char *)malloc(sizeof(char)*(SlfStrLen(comment)+1));
        if( !pvec->pComment ) {
            if( perrtext )
                perrtext->cat("Problem SlfVecSet: malloc Comment !!!");
            return NOT_OKAY;
        }
        SlfStrCpy(pvec->pComment,SlfStrLen(comment),comment);

    }

    // Unit
    if( unit ) {
	    pvec->pUnit = (char *)malloc(sizeof(char)*(SlfStrLen(unit)+1));
        if( !pvec->pUnit ) {
            if( perrtext )
                perrtext->cat("Problem SlfVecSet: malloc unit !!!");
            return NOT_OKAY;
        }
        SlfStrCpy(pvec->pUnit,SlfStrLen(unit),unit);

    }

    /* Vektor */
    pvec->NRows = nrows;

	pvec->pVec = (double *)malloc(sizeof(double)*nrows);
    if( !pvec->pVec ) {
        if( perrtext )
            perrtext->catFormat("Problem SlfVecSet: malloc pVec <%s> !!!"
                               ,pvec->pName);
        return NOT_OKAY;
    }
	memcpy(pvec->pVec, pval, sizeof(double)*nrows);

    return OKAY;
}
void  SlfVecFree(SSlfVec *pvec) {

    if( pvec->pName ) 
        free( pvec->pName );
    pvec->pName = 0;

    if( pvec->pUnit ) 
        free( pvec->pUnit );
    pvec->pUnit = 0;

    if( pvec->pComment ) 
        free( pvec->pComment );
    pvec->pComment = 0;

    pvec->NRows = 0;

    if( pvec->pVec ) 
        free( pvec->pVec );
    pvec->pVec = 0;
}
// 1D-Tabelle
//===========
status_t  Slf1DTabSet(SSlf1DTab *p1dtab
                     ,char *name, char *comment
                     ,char *xname, char *xunit, char * xcomment
                     ,double *pxval, uint32_t nxrows
                     ,char *yname, char *yunit, char * ycomment
                     ,double *pyval, uint32_t nyrows
                     ,uint8_t islinear
                     ,slf::CStr *perrtext /*=0*/) {

    p1dtab->pName     = 0;
    p1dtab->pComment  = 0;
    p1dtab->pXVec     = 0;
    p1dtab->pYVec     = 0;
    p1dtab->Index     = 0;
    p1dtab->IsLinear  = islinear;
    p1dtab->IsChecked = 0;

    // NAme
    if( name ) {
	    p1dtab->pName = (char *)malloc(sizeof(char)*(SlfStrLen(name)+1));
        if( !p1dtab->pName ) {
            if( perrtext )
                perrtext->cat("Problem Slf1DTabSet: malloc Name !!!");
            return NOT_OKAY;
        }
        SlfStrCpy(p1dtab->pName,SlfStrLen(name),name);
    }

    // Comment
    if( comment ) {
	    p1dtab->pName = (char *)malloc(sizeof(char)*(SlfStrLen(comment)+1));
        if( !p1dtab->pName ) {
            if( perrtext )
                perrtext->cat("Problem Slf1DTabSet: malloc Comment !!!");
            return NOT_OKAY;
        }
        SlfStrCpy(p1dtab->pName,SlfStrLen(comment),comment);

    }

    // X-Vektor
    //=========
    p1dtab->pXVec = (SSlfVec *)malloc(sizeof(SSlfVec));

    if( SlfVecSet(p1dtab->pXVec
                 ,xname, xunit, xcomment
                 ,pxval, nxrows
                 ,perrtext) != 0 )
                 return NOT_OKAY;

    // Y-Vektor
    //=========
    p1dtab->pYVec = (SSlfVec *)malloc(sizeof(SSlfVec));

    if( SlfVecSet(p1dtab->pYVec
                 ,yname, yunit, ycomment
                 ,pyval, nyrows
                 ,perrtext) != 0 )
                 return NOT_OKAY;
    				
    
    return OK;
}
status_t Slf1DTabCheck(SSlf1DTab *pd1tab,slf::CStr *perrtext /*=0*/) {

    double Test;
    uint32_t i;

    //--------------------------------
    // x-vektor gleiche Länge y-Vektor
    //--------------------------------
    if( pd1tab->pXVec->NRows != pd1tab->pYVec->NRows ) {

        if( perrtext )
            perrtext->catFormat("Problem Slf1DTabCheck:Length 1dtable <%s> is different "
                                "nxrows:<%i> != nynrows: <%s>\n"
                               ,pd1tab->pName
                               ,pd1tab->pXVec->NRows
                               ,pd1tab->pYVec->NRows
                               );
        return NOT_OKAY;
    }
    // Interpolation testen
    if( pd1tab->pXVec->NRows > 1 ) {
        if( NumericInterp1D(1
                          ,pd1tab->pXVec->pVec
                          ,pd1tab->pYVec->pVec
                          ,pd1tab->pXVec->NRows
                          ,0.0
                          ,&Test
                          ,&i,pd1tab->IsLinear,0) != OKAY ) {

            if( perrtext ) 
                perrtext->catFormat("Problem Slf1DTabCheck(1dtable): <%s> Interpolation\n"
                                   ,pd1tab->pName);
            
                perrtext->cat("xVector is not moton increasing\n");
            return NOT_OKAY;
        }
    }

    pd1tab->IsChecked = 1;

    return OK;
}
status_t Slf1DTabGetVal(SSlf1DTab *pd1tab,double xval,double *pyval) {

    if( pd1tab->IsChecked && (Slf1DTabCheck(pd1tab) != OK) )
        return NOT_OKAY;

    NumericInterp1D(0
                  ,pd1tab->pXVec->pVec
                  ,pd1tab->pYVec->pVec
                  ,pd1tab->pXVec->NRows
                  ,xval,pyval
                  ,&pd1tab->Index
                  ,pd1tab->IsLinear
                  ,0);
    return OKAY;
}
void  Slf1DTabFree(SSlf1DTab *pd1tab) {

    if( pd1tab->pName ) 
        free( pd1tab->pName );
    pd1tab->pName = 0;

    if( pd1tab->pComment ) 
        free( pd1tab->pComment );
    pd1tab->pComment = 0;

    if( pd1tab->pXVec ) {
        SlfVecFree(pd1tab->pXVec);
        free(pd1tab->pXVec);
        pd1tab->pXVec = 0;
    }
    if( pd1tab->pYVec ) {
        SlfVecFree(pd1tab->pYVec);
        free(pd1tab->pYVec);
        pd1tab->pYVec = 0;
    }

    pd1tab->IsChecked = 0;

}

#endif