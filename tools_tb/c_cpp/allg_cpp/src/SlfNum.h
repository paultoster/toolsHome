#ifndef SLF_NUM_H_INCLUDED
#define SLF_NUM_H_INCLUDED
 
#include "SlfBase.h"
#include "SlfStrV.h"
#include <vector>
#include <sstream>

//========================================================================================================
//
// slf::CVectorD dvec;  // double vector
// slf::CVectorF fvec;  // float vector
// slf::CVectorI ivec;  // int32 vector
// slf::CVectorU uvec;  // uint32_t vector
//
// Funktionen: alle std::vector und
// CVectorD dvec(20);    // legt n=20 an
// n = dvec.GetNdim();      // r?ckgabe ndim
// dvec.FillZero();         // f?llt mit Nullen auf
// dvec.FilleValue(1.);     // f?llt alle mit Einsen auf
// dvec.SetUnit("m/s");     // unit
// dvec.SetName("velx");    // name
// dvec.SetComment("Geschwindigkeit in x-Richtung");    // comment
// dvec.Convert(2.0,0.0)    // alle Werte mit Faktor 2   vec[i] = vec[i]*factor + offset;
// 
//----------------------------------------------------------------------------------------------------------
//
// slf::CMatrixD dmat;  // double matrix
// slf::CMatrixF fmat;  // float matrix
// slf::CMatrixI imat;  // int32 matrix
// slf::CMatrixU umat;  // uint32_t matrix
// Funktionen:
// slf::CMatrixD dmat(30,10);   // nrow =30,ncol=10
// nrow = dmat.GetNrow();
// ncol = dmat.GetNcol();
// dmat.SetNrow(31);
// dmat.SetNcol(11);
// dmat.SetNdim(31,11);
// dmat.FillZero();         // f?llt mit Nullen auf
// dmat.FilleValue(1.);     // f?llt alle mit Einsen auf
// dmatConvert(2.0,0.0);    // alle Werte mit Faktor 2   mat[i][j] = mat[i][j]*factor + offset;
//
//========================================================================================================
namespace slf
{

  //==============================================================================================================================
  // Vector D,F,U,I Class (double,float,uint32_t,int32_t)
  //==============================================================================================================================
  template<typename T> class CVector : public std::vector<T>
  {
  private:
    std::size_t ndim;
    slf::CStr   unit;
    slf::CStr   name;
    slf::CStr   comment;
  public:
    const std::size_t &Ndim;
    const slf::CStr   &Unit;
    const slf::CStr   &Name;
    const slf::CStr   &Comment;

    CVector(const std::size_t n):Ndim(ndim),Unit(unit),Name(name),Comment(comment) { this->resize(n); ndim = n; }
    CVector(void) :Ndim(ndim), Unit(unit), Name(name), Comment(comment) { ndim = 0; }
    CVector &operator=(const CVector& vec)
    {
      this->resize(vec.size()); std::copy (vec.begin(), vec.end(), this->begin());
      return *this;
    }

    std::size_t GetNdim(void)                   { return this->size(); }
    void        FillZero(void)                  { std::fill(this->begin(), this->end(), T(0.)); }
    void        FillValue(const T &val)         { std::fill(this->begin(), this->end(), val); }
    void        SetNdim(const std::size_t n)    { this->resize(ndim);  ndim = n; }
    void        SetUnit(const char *u)          { unit = u; }
    void        SetName(const char *na)         { name = na; }
    void        SetComment(const char *com)     { comment = com; }
    void        Copy(const CVector<T> &vec)     { this->resize(vec.size()); std::copy (vec.begin(), vec.end(), this->begin());}
    void        MakeAbs(void)                   { for (std::vector<T>::iterator iter=this->begin(); iter != this->end(); ++iter) if( *iter < (T)(0.)) *iter *= (T)(-1.); }
    void        Convert(double &factor, double &offset) 
    {
      if( (SLF_ABS((factor - T(1.0)),T) > EPSILON) || (SLF_ABS((offset - T(0.0)),T) > EPSILON)) 
      {
        for (std::vector<T>::iterator it = this->begin(); it != this->end(); ++it)
        {
          *it *= (T)factor;
          *it += (T)offset;
        }
      }
    }

  };
  template<typename T> class C3DVector : public CVector<T>
  {
  public:
    C3DVector(void) { SetNdim(3);  FillZero(); }
  };

  typedef CVector< double    > CVectorD;
  typedef CVector< float     > CVectorF;
#ifdef _WIN64
  typedef CVector< uint64_t  > CVectorU;
  typedef CVector< int64_t   > CVectorI;
#else
  typedef CVector< uint32_t  > CVectorU;
  typedef CVector< int32_t   > CVectorI;
#endif
  typedef CVector< uint64_t  > CVectorU64;
  typedef CVector< int64_t   > CVectorI64;
  typedef CVector< uint32_t  > CVectorU32;
  typedef CVector< int32_t   > CVectorI32;
  typedef CVector< uint16_t  > CVectorU16;
  typedef CVector< int16_t   > CVectorI16;
  typedef CVector< uint8_t  > CVectorU8;
  typedef CVector< int8_t   > CVectorI8;
  typedef CVector< EErrNo    > CVectorE;
  
  typedef C3DVector< double    > C3DVectorD;
  typedef C3DVector< float     > C3DVectorF;
  //==============================================================================================================================
  // Matrix D,F,U,I Class (double,float,uint32_t,int32_t)
  //==============================================================================================================================
  template<typename T> class CMatrix
  {
  private:
    std::size_t nrow, ncol;
    std::vector<CVector<T>> mat;
    slf::CStr   unit;
    slf::CStr   name;
    slf::CStr   comment;

  public:
    const std::size_t &Nrow, &Ncol;
    const slf::CStr   &Unit;
    const slf::CStr   &Name;
    const slf::CStr   &Comment;

    CMatrix(std::size_t mrow, std::size_t mcol):Nrow(nrow),Ncol(ncol), Unit(unit), Name(name), Comment(comment) { mat.resize(mrow); for (std::size_t i = 0; i < mrow; ++i) mat[i].resize(mcol); nrow = mrow; ncol = mcol; }
    CMatrix(void) :Nrow(nrow), Ncol(ncol), Unit(unit), Name(name), Comment(comment)                             { nrow = 0; ncol = 0; mat.resize(nrow); }
    void Clear(void) {
      nrow = 0; ncol = 0; mat.resize(nrow); unit.clear(); name.clear(); comment.clear();}
    std::size_t GetNdim()  const { return nrow*ncol; }
    std::size_t GetNrow()  const { return nrow; }
    std::size_t GetNcol()  const { return ncol; }
    void SetNrow(const std::size_t mrow) { mat.resize(mrow);  for (std::size_t i = nrow; i < mrow; ++i) mat[i].resize(ncol); nrow = mrow; }
    void SetNcol(const std::size_t mcol) { for (std::size_t i = 0; i < nrow; ++i) mat[i].resize(mcol); ncol = mcol; }
    void SetNdim(const std::size_t mrow, const std::size_t mcol) { mat.resize(mrow);  for (std::size_t i = 0; i < mrow; ++i) mat[i].resize(mcol); nrow = mrow; ncol = mcol; }
    void FillZero(void)                                          { for (std::size_t i = 0; i < nrow; ++i) std::fill(mat[i].begin(), mat[i].end(), T(0.)); }
    void FillValue(T val)                                        { for (std::size_t i = 0; i < nrow; ++i) std::fill(mat[i].begin(), mat[i].end(), val); }
    void SetValue(const T &val, const std::size_t irow, const std::size_t icol)
    {
      if (irow >= nrow) SetNrow(irow + 1);
      if (icol >= ncol) SetNcol(icol + 1);
      mat[irow][icol] = val;
    }
    void SetUnit(const char *u)                                  { unit = u; }
    void SetName(const char *na)                                 { name = na; }
    void SetComment(const char *com)                             { comment = com; }
    void Convert(double &factor, double &offset)
                                                                  {
                                                                    if ((SLF_ABS((factor - T(1.0)), T) > EPSILON) || (SLF_ABS((offset - T(0.0)), T) > EPSILON))
                                                                    {
                                                                      for (std::vector<T>::iterator it = mat.begin(); it != mat.end(); ++it)
                                                                      {
                                                                        for (std::vector<T>::iterator it2 = it->begin(); it2 != it->end(); ++it2)
                                                                        {
                                                                          *it2 *= (T)factor;
                                                                          *it2 += (T)offset;
                                                                        }
                                                                      }
                                                                    }
                                                                  }
    CVector<T>& operator[] (std::size_t i)                    { return this->mat[i]; }
    const CVector<T>& operator[] (std::size_t i) const        { return this->mat[i]; }
    CMatrix<T>& operator=(const CMatrix<T>& mat_t)
                                                                { 
                                                                  this->SetNrow(mat_t.GetNrow()); 
                                                                  this->SetNcol(mat_t.GetNcol()); 
                                                                  for (std::size_t i = 0; i < nrow; ++i)
                                                                  {
                                                                    for (std::size_t j = 0; j < ncol; ++j)
                                                                    {
                                                                      this->mat[i][j] = mat_t[i][j];
                                                                    }
                                                                  }
                                                                  return *this;
                                                                }
    void Copy(const CMatrix<T> &mat_t)     {
                                            this->SetNrow(mat_t.GetNrow());
                                            this->SetNcol(mat_t.GetNcol());
                                            for (std::size_t i = 0; i < nrow; ++i)
                                            {
                                              for (std::size_t j = 0; j < ncol; ++j)
                                              {
                                                this->mat[i][j] = mat_t[i][j];
                                              }
                                            }
                                          }

    void Transpone(void) 
                                                                {
                                                                  CMatrix<T> mat_t(ncol, nrow); for (std::size_t i = 0; i < nrow; ++i) { for (std::size_t j = 0; j < ncol; ++j) mat_t[j][i] = mat[i][j]; }
                                                                  this->SetNrow(mat_t.GetNrow()); 
                                                                  this->SetNcol(mat_t.GetNcol()); 
                                                                  for (std::size_t i = 0; i < nrow; ++i) { for (std::size_t j = 0; j < ncol; ++j) mat[i][j] = mat_t[i][j]; }
                                                                }
  };
  template<typename T> class C3DMatrix : public CMatrix<T>
  {
  public:
    C3DMatrix() :CMatrix(3, 3) {
      FillZero(); T val = T(1.); SetValue(val, 0, 0); SetValue(val, 1, 1); SetValue(val, 2, 2);
    }
  };
  typedef CMatrix< double   > CMatrixD;
  typedef CMatrix< float    > CMatrixF;
#ifdef _WIN64
  typedef CMatrix< uint64_t > CMatrixU;
  typedef CMatrix< int64_t  > CMatrixI;
#else
  typedef CMatrix< uint32_t > CMatrixU;
  typedef CMatrix< int32_t  > CMatrixI;
#endif
  typedef CMatrix< uint64_t > CMatrixU64;
  typedef CMatrix< int64_t  > CMatrixI64;
  typedef CMatrix< uint32_t > CMatrixU32;
  typedef CMatrix< int32_t  > CMatrixI32;
  typedef CMatrix< uint16_t > CMatrixU16;
  typedef CMatrix< int16_t  > CMatrixI16;
  typedef CMatrix< uint8_t > CMatrixU8;
  typedef CMatrix< int8_t  > CMatrixI8;
  
  typedef C3DMatrix< double   > C3DMatrixD;
  typedef C3DMatrix< float    > C3DMatrixF;

  //==============================================================================================================================
  // CTable1D D,F,U,I Class (double,float,uint32_t,int32_t) one dimensional table
  //==============================================================================================================================
  //---------------------------------------------------------------------------------------
  // Interp1D(init,)
  // Interpolation 1D y = f(x)
  //      
  // iflag           Inittialisierung 0 / 1 wenn iflag gestezt keine Berechnung nur Pr?fen
  // xvec            x - Vektor, muss monoton steigend sein
  // yvec            y - Vektor
  // x              x - Wert an dem y - Wert bestimmt wird
  // y0              y - Wert, der zurueckgegeben wird
  // iact            aktuelle Stuetzstelle der letzten Berechnung
  //                 wird als Ausgangswert benutzt, um schneller
  //                 den Wertebereich in x zu finden (z.B.wenn x0
  //                 fortlaufend steigt.iact wird dann aktuallisiert
  //  iord = 0       Konstantwert swischen den Stuetzstellen
  //       = 1       lineare Interpolation
  //
  //  iabl = 0, 1    Ableitung
  //
  //       return OKAY;    bei initialisierung wird gepr?ft
  //       return NOT_OKAY Fehler, x - Vektor nicht monoton steigend (nur bei init)
  //---------------------------------------------------------------------------------------
  template<typename T>
  okay_t Interp1D(bool iflag
    , const CVector<T> &xvec, const CVector<T> &yvec, std::size_t nxy
    , T x0, T &y0
    , std::size_t &iact, uint8_t  iord, uint8_t  iabl)
  {
    T dx = T(0.0), fak = T(0.0);
    std::size_t i, i0 = iact;


    if (iflag == 1) /* Initialisierung */
    {
      if (nxy == 1)return OKAY;
      for (i = 0; i < nxy - 1; i++)
      {
        if ((xvec[i + 1] - xvec[i]) < (T)SLF_EPSILON)
        {
          std::stringstream s;
          s << "Interp1D: x-vector is not monoton increasing";

          SlfNumErrMessVector.append(s.str());
          SlfNumErrNumVector.push_back(VECTOR_NOT_MONOTON_INCREASING);
          return NOT_OKAY;
        }
      }
      return OKAY;
    }
    if (nxy == 1) {

      if (iabl)
        y0 = T(0.0);
      else
        y0 = yvec[0];

      iact = 0;
      return OKAY;

    }
    else
    {

      /* Berechnung */
      if (i0 > nxy - 1 || i0 < 0) i0 = 0;

      if (x0 >= xvec[nxy - 1])
      {
        i0 = nxy - 1;
        dx = x0 - xvec[nxy - 1];
        fak = (yvec[nxy - 1] - yvec[nxy - 2]) / (xvec[nxy - 1] - xvec[nxy - 2]);
      }
      else if (x0 >= xvec[i0])
      {
        for (i = i0; i <= nxy - 2; i++)
          if (x0 < xvec[i + 1])
          {
            i0 = i;
            dx = x0 - xvec[i];
            fak = (yvec[i + 1] - yvec[i]) / (xvec[i + 1] - xvec[i]);
            break;
          }

      }
      else if (x0 < xvec[0])
      {
        i0 = 0;
        dx = x0 - xvec[0];
        fak = (yvec[1] - yvec[0]) / (xvec[1] - xvec[0]);
      }
      else
      {
        for (i = i0 - 1; i >= 0 && i < i0; i--)
          if (x0 >= xvec[i])
          {
            i0 = i;
            dx = x0 - xvec[i];
            fak = (yvec[i + 1] - yvec[i]) / (xvec[i + 1] - xvec[i]);
            break;
          }
      }

      if (iord == 1)
      {
        if (iabl == 0)
          y0 = yvec[i0] + fak*dx;
        else
          y0 = fak;
      }
      else
      {
        if (iabl == 0)
          y0 = yvec[i0];
        else
          y0 = T(0.0);
      }

      iact = i0;
      return OKAY;
    }

  }
#define Interp1DD Interp1D<double>

  template<typename T> class CTable1D
  {
  private:
    std::size_t    ndim;
    CStr           name;
    CStr           comment;
    CVector<T>  xVec;
    CVector<T>  yVec;
    uint8_t        order;
    std::size_t    index;
    bool           isChecked;

  public:
    const std::size_t    &Ndim;
    const CStr           &Name;
    const CStr           &Comment;
    const CVector<T>     &XVec;
    const CVector<T>     &YVec;
    const uint8_t        &Order;
    const std::size_t    &Index;
    const bool           &IsChecked;

    CTable1D(const CVector<T> &xvec, const CVector<T> &yvec, bool islin) 
      :Ndim(ndim), Name(name), Comment(comment), XVec(xVec), YVec(yVec), Order(order), Index(index),IsChecked(isChecked)
    {
      index = 0; isChecked = false;
      
      SetXYvec(xvec,yvec, islin);
    }
    CTable1D(void) :Ndim(ndim), Name(name), Comment(comment), XVec(xVec), YVec(yVec), Order(order), Index(index), IsChecked(isChecked)
    { 
      ndim = 0; order = 1; index = 0; isChecked = false;
    }
    CTable1D<T>& operator=(const CTable1D<T>& tab)
    {
      ndim = tab.Ndim;
      name = tab.Name;
      comment = tab.Comment;
      xVec = tab.XVec;
      yVec = tab.YVec;
      order = tab.Order;
      index = tab.Index;
      isChecked = tab.IsChecked;
    }
    okay_t SetXYvec(const CVector<T> &xvec, const CVector<T> &yvec, bool islin)
    {
      if (islin) order = 1;
      else       order = 2;
      ndim = SLF_MIN(xvec.size(), yvec.size());
      xVec = xvec;
      yVec = yvec;
      Xvec.resize(ndim);
      Yvec.resize(ndim);
      T y;
      if (Interp1D(true, xVec, yVec, ndim, T(0.0), y, index, order, 0) == OKAY)
      {
        isChecked = true;
        return OKAY;
      }
      else
      {
        isChecked = flase;
        return NOT_OKAY;
      }
    }
    void Set(const CTable1D<T> &tab)
    {
      ndim           = tab.Ndim;
      name           = tab.Name;
      comment        = tab.Comment;
      xVec           = tab.XVec;
      yVec           = tab.YVec;
      order          = tab.Order;
      index          = tab.Index;
      isChecked      = tab.IsChecked;
    }
    void ConvertXvec(double &factor, double &offset) { xVec.Convert(factor, offset); }
    void ConvertYvec(double &factor, double &offset) { yVec.Convert(factor, offset); }
    void GetValue(const T &xval, T &yval)
    { 
      Interp1D(0
        , xVec
        , yVec
        , ndim
        , xval, yval
        , index
        , order
        , 0);

    }


  };
  typedef CTable1D< double   > CTable1DD;
  typedef CTable1D< float    > CTable1DF;
  typedef CTable1D< uint32_t > CTable1DU;
  typedef CTable1D< int32_t  > CTable1DI;

  //==============================================================================================================================
  // CTable2D D,F,U,I Class (double,float,uint32_t,int32_t) one dimensional table
  //==============================================================================================================================

  //---------------------------------------------------------------------------------------
  // Interp2D(iflag,xvec,nx,yvec,ny,zmat,x0,y0,z0)
  // Interpolation 2D z = f(x,y)
  //
  //      Interpolation z(nrow,ncol)=f(x(nrow),y(ncol))
  //      Der Endwert wird begrenzt
  //      iflag           Inittialisierung 0/1, wenn iflag gesetzt, dann keine Berechnung
  //      xvec[nx]        x-Vektor, monoton steigend
  //      yvec[ny]        y-Vektor, monoton steigend
  //      zmat[ny][nx]    z-Matrix_t
  //      x0              x-Wert an dem z-Wert bestimmt wird
  //      y0              y-Wert an dem z-Wert bestimmt wird
  //      z0              z-Wert, der zur?ckgegeben wird
  //      return OKAY;    bei initialisierung wird gepr?ft
  //      return NOT_OKAY Fehler, x - Vektor nicht monoton steigend,y - Vektor nicht monoton steigend (nur bei init)
  //---------------------------------------------------------------------------------------
  template<typename T>
  okay_t Interp2D(const bool iflag, const CVector<T> &xvec, const std::size_t nx, const CVector<T> &yvec, const std::size_t ny
    , const CMatrix<T> &zmat, const T &x0, const T &y0, T &z0)
  {
    size_t i;
    size_t index_x1, index_x2, index_y1, index_y2;
    double ztemp1, ztemp2;
    double colLambda;
    double rowLambda;
    double ztemp;
    double num;
    double den;

    if (iflag) {

      if (nx < 1 || ny < 1)
      {
        std::stringstream s;
        s << "Interp2D: x-vector or y-vector to small";

        SlfNumErrMessVector.append(s.str());
        SlfNumErrNumVector.push_back(VECTOR_TO_SMALL);
        return NOT_OKAY;
      }
      else if (zmat.Nrow < ny || zmat.Ncol < nx)
      {
        std::stringstream s;
        s << "Interp2D: matrix-dimension to small!!";

        SlfNumErrMessVector.append(s.str());
        SlfNumErrNumVector.push_back(MATRIX_SIZE_WRONG);
        return NOT_OKAY;
      }
      else
      {

        for (i = 1; i < nx; i++)
        {
          if (xvec[i] - xvec[i - 1] <= (T)EPSILON)
          {
            std::stringstream s;
            s << "Interp2D: x-vector is not monoton increasing";

            SlfNumErrMessVector.append(s.str());
            SlfNumErrNumVector.push_back(VECTOR_NOT_MONOTON_INCREASING);
            return NOT_OKAY;
          }
        }
        for (i = 1; i < ny; i++)
        {
          if (yvec[i] - yvec[i - 1] <= (T)EPSILON)
          {
            std::stringstream s;
            s << "Interp2D: y-vector is not monoton increasing";

            SlfNumErrMessVector.append(s.str());
            SlfNumErrNumVector.push_back(VECTOR_NOT_MONOTON_INCREASING);
            return NOT_OKAY;
          }
        }
      }
      return OKAY;
    }

    Interp2D_search_index<T>(index_x1, index_x2, x0, xvec, nx);
    Interp2D_search_index<T>(index_y1, index_y2, y0, yvec, ny);

    if (yvec[index_y1] != yvec[index_y2])
    {
      den = yvec[index_y2];
      den -= yvec[index_y1];
      num = y0;
      num -= yvec[index_y1];
      {
        colLambda = (num / den);
      }
    }
    else
    {
      colLambda = 0;
    }
    if (xvec[index_x1] != xvec[index_x2])
    {

      den = xvec[index_x2];
      den -= xvec[index_x1];
      num = x0;
      num -= xvec[index_x1];
      {
        rowLambda = (num / den);
      }
    }
    else
    {
      rowLambda = 0;
    }
    /* Interpolate along column variable
    *    with the row variable locked on the left row
    */
    ztemp1 = zmat[index_y1][index_x1];
    ztemp = zmat[index_y2][index_x1];
    ztemp1 += colLambda * (ztemp - ztemp1);
    /* Interpolate along column variable
    *    with the row variable locked on the right row
    */
    ztemp2 = zmat[index_y1][index_x2];
    ztemp = zmat[index_y2][index_x2];
    ztemp2 += colLambda * (ztemp - ztemp2);
    /*
    * Interpolate along row variable
    *    with the col variable locked on its interpolated value
    */
    ztemp1 += rowLambda * (ztemp2 - ztemp1);
    *z0 = ztemp1;

    return 0;

  }
  #define Interp2DD Interp2D<double>

  template<typename T>
  void Interp2D_search_index(std::size_t &iLeft, std::size_t &iRght
    , const T &u, const CVector<T> &uvec, const std::size_t nu)
  {
    /* Find the location of current input value in the data table. */
    iLeft = 0;
    iRght = nu - 1;

    if (u <= uvec[0])
    {
      /* Less than or equal to the smallest point in the table. */
      iRght = 0;
    }
    else if (u >= uvec[nu - 1])
    {
      /* Greater than or equal to the largest point in the table. */
      iLeft = nu - 1;
    }
    else
    {
      std::size_t i;

      /* Do a binary search. */
      while ((iRght - iLeft) > 1)
      {
        /* Get the average of the left and right indices using to Floor rounding. */
        i = (iLeft + iRght) >> 1;

        /* Move either the right index or the left index so that */
        /*  LeftDataPoint <= CurrentValue < RightDataPoint */
        if (u < uvec[i])
        {
          iRght = i;
        }
        else
        {
          iLeft = i;
        }
      }
    }
  }
  #define Interp2D_search_indexD Interp2D_search_index<double>

  template<typename T> class CTable2D
  {
  private:
    std::size_t    nrow;
    std::size_t    ncol;
    CStr           name;
    CStr           comment;
    CVector<T>  xVec;
    CVector<T>  yVec;
    CMatrix<T>  zMat;
    bool           isChecked;

  public:
    const std::size_t    &Nrow;
    const std::size_t    &Ncol;
    const CStr           &Name;
    const CStr           &Comment;
    const CVector<T>  &XVec;
    const CVector<T>  &YVec;
    const CMatrix<T>  &ZMat;
    const bool           &IsChecked;

    CTable2D(const CVector<T> &xvec, const CVector<T> &yvec, const CMatrix<T> &zmat)
      :Nrow(nrow), Ncol(ncol), Name(name), Comment(comment), XVec(xVec), YVec(yVec), ZMat(zMat), IsChecked(isChecked)
    {
      isChecked = false;

      SetXYZ(xvec, yvec, zmat);
    }
    CTable2D(void) :Nrow(nrow), Ncol(ncol), Name(name), Comment(comment), XVec(xVec), YVec(yVec), ZMat(zMat), IsChecked(isChecked)
    {
      nrow = 0; ncol = 1; /* indexX = 0; indexY = 0;*/ isChecked = false;
    }
    CTable2D<T>& operator=(const CTable2D<T>& tab)
    {
      nrow = tab.Nrow;
      ncol = tab.Ncol;
      name = tab.Name;
      comment = tab.Comment;
      xVec = tab.XVec;
      yVec = tab.YVec;
      zMat = tab.ZMat;
      isChecked = tab.IsChecked;
    }
    okay_t SetXYZ(const CVector<T> &xvec, const CVector<T> &yvec, const CMatrix<T> &zmat)
    {
      nrow = xvec.size();
      ncol = yvec.size();
      xVec = xvec;
      yVec = yvec;
      zMat = zmat;
      T z;
      if (Interp2D(true, xVec, nrow, yVec, ncol, zMat, T(0.0), T(0.0),z) == OKAY)
      {
        isChecked = true;
        return OKAY;
      }
      else
      {
        isChecked = flase;
        return NOT_OKAY;
      }
    }
    void Set(const CTable2D<T> &tab)
    {
      nrow = tab.Nrow;
      ncol = tab.Ncol;
      name = tab.Name;
      comment = tab.Comment;
      xVec = tab.XVec;
      yVec = tab.YVec;
      zMat = tab.ZMat;
      isChecked = tab.IsChecked;
    }
    void ConvertXvec(double &factor, double &offset) { xVec.Convert(factor, offset); }
    void ConvertYvec(double &factor, double &offset) { yVec.Convert(factor, offset); }
    void ConvertZmat(double &factor, double &offset) { zMat.Convert(factor, offset); }
    void GetValue(const T &xval,const T &yval, T &zval)
    {
      Interp2D(0
        , xVec
        , nrow
        , yVec
        , ncol
        , zMat
        , xval, yval
        , zval);

    }


  };
  typedef CTable2D< double   > CTable2DD;
  typedef CTable2D< float    > CTable2DF;
  typedef CTable2D< uint32_t > CTable2DU;
  typedef CTable2D< int32_t  > CTable2DI;



  //=======================================================================================
  // Functions
  //---------------------------------------------------------------------------------------
  // InnerProduct(vec1,vec2)
  // inner product of two vector (minimum length)
  //---------------------------------------------------------------------------------------
  // InnerProduct(vec1,vec2)
  // inner product of two vector (minimum length)
  //---------------------------------------------------------------------------------------
  template<typename T> T InnerProduct(const CVector<T> &vec1, const CVector<T> &vec2)
  {
    T result = T(0.);
    std::size_t i, n = SLF_MIN(vec1.Ndim, vec2.Ndim);
    for (i = 0; i < n; ++i)
    {
      result += vec1[i] * vec2[i];
    }
    return result;
  }
  #define InnerProductD InnerProduct<double>
  


  /*============================================================================================================================*/
  /*============================================================================================================================*/
  /*============================================================================================================================*/
  /*============================================================================================================================*/
  /*============================================================================================================================*/
  template<typename T>
  void Abs(T &x)
  {
    if (x < (T)0)  x *= (T)(-1);
  }
  #define AbsD Abs<double>

  template<typename T>
  void VectorAbs(CVector<T> &v)
  {
    for (std::size_t i = 0; i < v.size(); i++)
    {
      SlfNumAbs<T>(v[i]);
    }
  }
  #define VectorAbsD VectorAbs<double>

  template<typename T>
  void VectorFactorOffset(const double &factor, const double &offset, CVector<T> &v)
  {
    ScalarTimesVector<T>(factor, v, v);
    VectorAddScalar<T>(offset, v, v);
  }
  #define VectorFactorOffsetD VectorFactorOffset<double>

  template<typename T>
  void VectorAdd(const CVector<T> &va, const CVector<T> &vb, CVector<T> &vres)
  {
    std::size_t i;
    std::size_t n;

    n = SLF_MIN(va.size(), vb.size());
    if (vres.size() < n)
    {
      vres.resize(n);
    }
    for (i = 0; i < n; i++)
    {
      vres[i] = va[i] + vb[i];
    }
  }
  #define VectorAddD VectorAdd<double>
  
  template<typename T>
  void VectorSub(const CVector<T> &va, const CVector<T> &vb, CVector<T> &vres)
  {
    std::size_t i;
    std::size_t n;

    n = SLF_MIN(va.size(), vb.size());
    if (vres.size() < n)
    {
      vres.resize(n);
    }
    for (i = 0; i < n; i++)
    {
      vres[i] = va[i] - vb[i];
    }
  }
  #define VectorSubD VectorSub<double>

  template<typename T>
  void VectorAddScalar(const double &s, const CVector<T> &v, CVector<T> &vres)
  {
    std::size_t i;
    if (vres.size() < v.size())
    {
      vres.resize(v.size());
    }
    for (i = 0; i < v.size(); i++)
    {
      vres[i] = v[i] + s;
    }
  }
  #define VectorAddScalarD VectorAddScalar<double>

  // vres = vres + s*v
  //===================
  template<typename T>
  void VectorMultAdd(const double &s, const CVector<T> &v, CVector<T> &vres)
  {
    std::size_t i;
    if (vres.size() < v.size())
    {
      vres.resize(v.size());
    }
    for (i = 0; i < v.size(); i++)
    {
      vres[i] += v[i] * s;
    }
  }
  #define VectorMultAddD VectorMultAdd<double>

  // vres = vres - s*v
  //===================
  template<typename T>
  void VectorMultSub(const double &s, const CVector<T> &v, CVector<T> &vres)
  {
    std::size_t i;
    if (vres.size() < v.size())
    {
      vres.resize(v.size());
    }
    for (i = 0; i < v.size(); i++)
    {
      vres[i] -= v[i] * s;
    }
  }
  #define VectorMultSubD VectorMultSub<double>

  template<typename T>
  void PutVectorToVector(const CVector<T> &v, CVector<T> &vres, std::size_t irow)
  {
    std::size_t i;

    if (vres.size() < (irow + v.size()))
    {
      vres.resize(v.size() + irow);
    }
    for (i = 0; i < v.size(); i++)
    {
      vres[irow + i] = v[i];
    }
  }
  #define PutVectorToVectorD PutVectorToVector<double>

  template<typename T> void PutVectorToVectorMax(const CVector<T> &v, CVector<T> &vres, std::size_t nmax)
  {

    std::size_t i;

    nmax = SLF_MIN(v.size(), nmax);
    nmax = SLF_MIN(vres.size(), nmax);


    for (i = 0; i < nmax; i++)
      vres[i] = v[i];

  }
  #define PutVectorToVectorMaxD PutVectorToVectorMax<double>

  template<typename T>
  void ScalarTimesVector(const double &s, const CVector<T> &v, CVector<T> &vres)
  {
    std::size_t i;

    if (vres.size() < v.size())
    {
      vres.resize(v.size());
    }
    for (i = 0; i < v.size(); i++)
    {
      vres[i] = s * v[i];
    }
  }
  #define ScalarTimesVectorD ScalarTimesVector<double>

  template<typename T> void VectorCrossProduct(const CVector<T> &va, const CVector<T> &vb, CVector<T> &vres)
  {
    if (vres.size() < 3) vres.resize(3);
    if ((va.size() > 2) && (va.size() > 2))
    {
      vres[0] = va[1] * vb[2] - va[2] * vb[1];
      vres[1] = va[2] * vb[0] - va[0] * vb[2];
      vres[2] = va[0] * vb[1] - va[1] * vb[0];
    }
  }
  #define VectorCrossProductD VectorCrossProduct<double>

  template<typename T>
  void UnifyVector(const CVector<T> &v, CVector<T> &e)
  {

    if (e.size() < v.size())
    {
      e.resize(v.size());
    }
    VectorDivideScalar<T>(SLF_NOT_ZERO(VectorNorm<T>(v), T), v, e);
  }
  #define UnifyVectorD UnifyVector<double>

  template<typename T>
  void VectorDivideScalar(T s, const CVector<T> &v, CVector<T> &vres)
  {
    std::size_t i;

    if (vres.size() < v.size())
    {
      vres.resize(v.size());
    }
    for (i = 0; i < v.size(); i++)
      vres[i] = v[i] / s;
  }
  #define VectorDivideScalarD VectorDivideScalar<double>

  template<typename T>
  T VectorNorm(const CVector<T> &v)
  {
    return sqrt(InnerProduct(v, v));
  }
  #define VectorNormD VectorNorm<double>

  template<typename T>
  void ScalarTimesVector(const double &s, const CVector<T> &v, CVector<T> &vres);
  #define ScalarTimesVectorD ScalarTimesVector<double>



  /*==============================================================*/
  /*==============================================================*/
  /*==============================================================*/
  /*==============================================================*/
  /*===========================================================*/

  template<typename T>
  void ScalarTimesMatrix(const double &s, const CMatrix<T> &m, CMatrix<T> &mres)
  {
    if ((mres.GetNrow() < m.GetNrow()) && (mres.GetNcol() < m.GetNcol()))
    {
      mres.SetNdim(m.GetNrow(), m.GetNcol());
    }
    for (std::size_t i = 0; i < m.GetNrow(); i++)
    {
      for (std::size_t j = 0; j < m.GetNcol(); j++)
      {
        mres[i][j] = s * m[i][j];
      }
    }
  }
  #define ScalarTimesMatrixD ScalarTimesMatrix<double>
  
  //---------------------------------------------------------------------------------------
  // MatrixMultiply(m1,m2,mr)
  // Matrix Multiplicaton mr = m1*m2
  // mr will resized
  //---------------------------------------------------------------------------------------
  #define MatrixMultiplyD MatrixMultiply<double>
  template<typename T> okay_t MatrixMultiply(const CMatrix<T> &m1, const CMatrix<T> &m2, CMatrix<T> &mr)
  {
    if (m1.Ncol != m2.Nrow)
    {
      std::stringstream s;
      s << "MatrixMultiply: Can't multiply m1(" << m1.Nrow << " x " << m1.Ncol << ") and m2(" << m2.Nrow << " x " << m2.Ncol << ") matrices";

      SlfNumErrMessVector.append(s.str());
      SlfNumErrNumVector.push_back(MATRIX_SIZE_WRONG);
      return false;
    }
    mr.SetNdim(m1.Nrow, m2.Ncol);

    for (std::size_t i = 0; i < m1.Nrow; i++)
    {
      for (std::size_t j = 0; j < m2.Ncol; j++)
      {
        T sum = 0;
        for (std::size_t k = 0; k < m1.Ncol; k++)
        {
          sum += m1[i][k] * m2[k][j];
        }
        mr[i][j] = sum;
      }
    }
    return true;
  }
  // Kopiere Vektor/Matrix_t ab der angegebenen Stelle
  //
  // M[i+irow][j+icol] = m[i][j]
  //============================
  #define PutMatrixToMatrixD PutMatrixToMatrix<double>
  template<typename T> void PutMatrixToMatrix(const CMatrix<T> &m, CMatrix<T> &mres, std::size_t irow, std::size_t icol)
  {
    if ((mres.GetNrow() < (irow + m.GetNrow())) || (mres.GetNcol() < (irow + m.GetNcol())))
    {
      mres.SetNdim((m.GetNrow() + irow), (m.GetNcol() + icol));
    }
    for (std::size_t i = 0; i < m.GetNrow(); i++)
    {
      for (std::size_t j = 0; j < m.GetNcol(); j++)
      {
        mres[irow + i][icol + j] = m[i][j];
      }
    }

  }
  // M[irow + i][icol] = v[i]
  //=========================
  template<typename T>
  void PutVectorToMatrix(const CVector<T> &v, CMatrix<T> &mres, std::size_t irow, std::size_t icol);
  #define PutVectorToMatrixD PutVectorToMatrix<double>
  template<typename T> void PutVectorToMatrix(const CVector<T> &v, CMatrix<T> &mres, std::size_t irow, std::size_t icol)
  {
    if (mres.GetNrow() < (irow + v.size()))
    {
      mres.SetNdim((v.size() + irow), mres.GetNcol());
    }
    for (std::size_t i = 0; i < v.size(); i++)
    {
      mres[irow + i][icol] = v[i];
    }
  }
  #define PutMatrixToMatrixMaxD PutMatrixToMatrixMax<double>
  template<typename T> void PutMatrixToMatrixMax(const CMatrix<T> &m, CMatrix<T> &mres, std::size_t nrowmax, std::size_t ncolmax)
  {
    std::size_t i, j;

    nrowmax = SLF_MIN(m.GetNrow(), nrowmax);
    nrowmax = SLF_MIN(mres.GetNrow(), nrowmax);
    ncolmax = SLF_MIN(m.GetNcol(), ncolmax);
    ncolmax = SLF_MIN(mres.GetNcol(), ncolmax);

    for (i = 0; i < nrowmax; i++)
      for (j = 0; j < ncolmax; j++)
        mres[i][j] = m[i][j];
  }
  
  template<typename T>
  void TransponeMatrix(const CMatrix<T> &m, CMatrix<T> &mres);
  #define TransponeMatrixD TransponeMatrix<double>

  template<typename T> void TransponeMatrix(const CMatrix<T> &m, CMatrix<T> &mres)
  {
    mres = m;
    std::size_t i, j;

    for (i = 0; i < m.GetNrow(); i++)
    {
      for (j = 0; j < m.GetNcol(); j++)
      {
        mres[j][i] = m[i][j];
      }
    }
  }
  /*
  Compute vres = m * v where
  v is a row vector (rv x 1)
  m is a matrix (r x c)
  vres is a row vector (r x 1)
  */
  template<typename T>
  void MatrixTimesVector(const CMatrix<T> &m, const CVector<T> &v, CVector<T> &vres);
  #define MatrixTimesVectorD MatrixTimesVector<double>
  
  template<typename T>
  void MatrixTimesVector(const CMatrix<T> &m, const CVector<T> &v, CVector<T> &vres)
  {
    std::size_t i, j;

    if (m.GetNcol() == v.size())
    {
      if (vres.size() < m.GetNrow())
      {
        vres.resize(m.GetNrow());
      }
    }
    for (i = 0; i < m.GetNrow(); i++)
    {
      vres[i] = 0;
      for (j = 0; j < m.GetNcol(); j++)
      {
        vres[i] += v[j] * m[i][j];
      }
    }
  }

  //---------------------------------------------------------------------------------------
  // MatrixInvert(m,mr)
  // Matrix Inversion mr = m^-1
  // mr will resized
  //---------------------------------------------------------------------------------------
  template<typename T> T MatrixInvert(const CMatrix<T> &m, CMatrix<T> &mr);
  template<typename T> T MatrixInvert(CMatrix<T> &mr);
  #define MatrixInvertD MatrixInvert<double>

  //==========================
  // det = MatrixInvert(m,mr);
  //
  //==========================
  template<typename T> T MatrixInvert(const CMatrix<T> &m, CMatrix<T> &mr)
  {
    mr = m;
    return MatrixInvert<T>(mr);
  }
  //==========================
  // det = MatrixInvert(mr);
  //
  //==========================
  template<typename T> T MatrixInvert(CMatrix<T> &mr)
  {
    std::size_t i, j, k, n;
    T det, biga, recip_biga, hold;
    std::vector<std::size_t> lvec, mvec;

    // proof 
    if (mr.Nrow != mr.Ncol)
    {
      std::stringstream s;
      s << "MatrixInvert: Can't inver m.Nrow=" << mr.Nrow << " != mr.Ncol=" << mr.Ncol;

      SlfNumErrMessVector.append(s.str());
      SlfNumErrNumVector.push_back(MATRIX_NOT_SQUARED);
      return T(0.);
    }

    n = mr.Nrow;
    lvec.resize(n);
    mvec.resize(n);

    det = T(1.0);
    for (k = 0; k < n; k++)
    {
      lvec[k] = k;  mvec[k] = k;
      biga = mr[k][k];

      /* Find the biggest element in the submatrix */
      for (i = k; i < n; i++)
      {
        for (j = k; j < n; j++)
        {
          if (SLF_ABS(mr[i][j], T) > SLF_ABS(biga, T))
          {
            biga = mr[i][j];
            lvec[k] = i;
            mvec[k] = j;
          }
        }
      }

      /* Interchange rows */
      i = lvec[k];
      if (i > k)
        for (j = 0; j < n; j++) {
          hold = -mr[k][j];
          mr[k][j] = mr[i][j];
          mr[i][j] = hold;
        }

      /* Interchange columns */
      j = mvec[k];
      if (j > k)
        for (i = 0; i < n; i++) {
          hold = -mr[i][k];
          mr[i][k] = mr[i][j];
          mr[i][j] = hold;
        }

      /* Divide column by minus pivot
      (value of pivot element is contained in biga). */
      if (biga == T(0.0)) {
        return T(0.0);
      }

      recip_biga = T(1.) / biga;
      for (i = 0; i < n; i++)
      {
        if (i != k)
        {
          mr[i][k] *= -recip_biga;
        }
      }

      /* Reduce matrix */
      for (i = 0; i < n; i++)
      {
        if (i != k)
        {
          hold = mr[i][k];
          for (j = 0; j < n; j++)
          {
            if (j != k)
            {
              mr[i][j] += hold * mr[k][j];
            }
          }
        }
      }

      /* Divide row by pivot */
      for (j = 0; j < n; j++)
      {
        if (j != k)
        {
          mr[k][j] *= recip_biga;
        }
      }
      det *= biga;	/* Product of pivots */
      mr[k][k] = recip_biga;

    }	/* K loop */

      /* Final row & column interchanges */
    for (k = n - 1; k >= 0 && k < n; k--)
    {
      i = lvec[k];
      if (i > k)
      {
        for (j = 0; j < n; j++)
        {
          hold = mr[j][k];
          mr[j][k] = -mr[j][i];
          mr[j][i] = hold;
        }
      }
      j = mvec[k];
      if (j > k)
      {
        for (i = 0; i < n; i++)
        {
          hold = mr[k][i];
          mr[k][i] = -mr[j][i];
          mr[j][i] = hold;
        }
      }
    }

    return det;
  }

  //===============================================
  //  Abs(x)
  //===============================================
  template<typename T>
  T Abs(const T &x);
  template<typename T>
  T Abs(const T &x)
  {
    if (x >= (T)0.0) return x;
    else             return x*((T)(-1.0));
  }

  //===============================================
  //  Signum(x)
  //===============================================
  template<typename T>
  sint8_t Sgn(const T &x);
  #define SgnD Sgn<double>
  template<typename T>
  sint8_t Sgn(const T &x)
  {
    if (x >= (T)0.0) return 1;
    else             return -1;
  }


  /*****************************************************************************
  @fn                   SlfNumLim1(double *pVal, double *pLimUp, double *pLimLow)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      LimUp   upper border
  @param[in]      LimLow  lower border

  @return         limited Value between -Lim and Lim
  *****************************************************************************/
  template<typename T>
  T Lim(const T &val, const T &limUp, const T &limLow);
  #define LimD Lim<double>
  template<typename T>
  T Lim(const T &val, const T &limUp, const T &limLow)
  {
    T rval;
    if (val > limUp)
    {
      rval = limUp;
    }
    else if (val < limLow)
    {
      rval = limLow;
    }
    else
    {
      rval = val;
    }
    return rval;
  }

  /*****************************************************************************
  @fn              double SlfNumLim1(double Val, double Lim)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      Lim     upper border, lower border => *(-1.f)

  @return         limited Value between -Lim and Lim
  *****************************************************************************/
  template<typename T>
  T Lim1(const T &val, const T &lim);
  #define Lim1D Lim1<double>
  template<typename T>
  T Lim1(const T &val, const T &lim)
  {
    T lim0 = lim * (T(-1.));
    T rval;
    if (val > lim)
    {
      rval = lim;
    }
    else if (val < lim0)
    {
      rval = lim0;
    }
    else
    {
      rval = val;
    }
    return val;
  }

  //===============================================
  //  Arcus tangens y/x
  //===============================================
  template<typename T>
  T ATan2(const T &y, const T &x);
  #define ATan2D ATan2<double>

  template<typename T>
  T ATan2(const T &y, const T &x)
  {
    T dum;
    if ((x < T(1.e-30)) && (x > T(-1.e-30)))
    {
      if ((y < T(1.e-30) && (y > T(-1.e-30)))
        return T(0.0);
      else if (y > T(0.))
        return(T)(SLF_PI / 2.));
      else
        return(T)(-(SLF_PI / 2.));
    }
    else if (x > T(0.))
    {
      dum = y / x;
      return T(std::atan(dum));
      /*      return(dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
    }
    else
    {
      dum = y / x;
      if (y >= 0.)
      {
        return T(SLF_PI + std::atan(dum));
        /*        return(PI+dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
      }
      else
      {
        return T(-SLF_PI + atan(dum));
        /*        return(-PI+dum*(1+dum*dum*(-0.33333+dum*dum/5.))); */
      }
    }
  }

  //===============================================
  //  m = sqrt(a^2+b^2)
  //===============================================
  //===============================================
  //  m = sqrt(a^2+b^2)
  //===============================================
  template<typename T>
  T Sqrt2(const T &a, const T &b);
  #define Sqrt2D Sqrt2<double>
  template<typename T>
  T Sqrt2(const T &a, const T &b)
  {
    return T(std::sqrt(a*a + b*b));
  }
  

  //=======================================================================================
  // extern definitions
  //=======================================================================================
  extern CStrV       SlfN1umErrMessVector;
  extern CVectorE    SlfNumErrNumVector;
  extern CStrV       SlfNumErrMessVector;


} // namespace slf


#if 0
// vres = m * v
void MatrixTimesVector(Matrix_t m,Vector_t v,Vector_t vres);

// vres' = v' * m
void VectorTimesMatrix(Vector_t v,Matrix_t m,Vector_t vres);


double QuadraticForm(register Vector_t v,register Matrix_t  m);


/* Matrix_t inversion using full pivoting.
 * The standard Gauss-Jordan method is used.
 * The return value is the determinant.
 * The input matrix may be the same as the result matrix
 *
 *	det = InvertMatrix(inputmatrix, resultmatrix);
 *
 * HISTORY
 * 26-Feb-82  David Smith (drs) at Carnegie-Mellon University
 *	Written.
 * Sun Mar 20 19:36:16 EST 1988 - converted to this form by Dean Rubine
 *
 */


void MatrixAddScalar(double s,Matrix_t v,Matrix_t vres);

// vio = vio + s*vadd;
void VectorMultAdd(double s,Vector_t vadd, Vector_t vio);

// vio = vio - s*vsub;
void VectorMultSub(double s,Vector_t vsub, Vector_t vio); 

// mio = mio + s*madd;
void MatrixMultAdd(double s,Matrix_t madd, Matrix_t mio);

// mio = mio - s*msub;
void MatrixMultSub(double s,Matrix_t msub, Matrix_t mio); 

void VectorDivideScalar(double s,Vector_t v,Vector_t vres);

void MakeTildeMatrix(Vector_t v,Matrix_t mres);
void MakeTransponeTildeMatrixMultTildeMatrix(Vector_t v,Matrix_t mres);

void MakeVectorMultTransponeVektor(Vector_t v,Vector_t vtot,Matrix_t mres);

void MatrixAdd(Matrix_t ma,Matrix_t mb,Matrix_t mres);
void MatrixSub(Matrix_t ma,Matrix_t mb,Matrix_t mres);


/* normalise Vector_t */
double VectorNorm(Vector_t v);

/* absulte each vector value */
void VectorAbs(Vector_t v);

void UnifyVector(Vector_t v, Vector_t e);

// vres = va x vb
void VectorCrossProduct(Vector_t va,Vector_t vb, Vector_t vres);

// Memory allocation, Do not call this function directly, use SlfNumAllocate(n, type)
void *SlfNumAllocateFunc(uint32_t nitems, uint32_t itemsize, char *tname);

#if 0
okay_t ludcmp(Matrix_t a, uint32_t n, uint32_t *indx, double *d);
//==============================================================================
// LU-Decomposition
// =================
// Input
// a[0 ... n-1][0 ... n-1]
// Output
// a[beta11,beta12, ..., beta1n;
//   alph21,beta22, ..., beta2n;
//   alph31,alph32, ..., beta3n;
//   ...
//   alphn1,alphn2, ..., betann]
//
//  alphij : lower triangular
//  betaij : upper triangular
//
// index[0 ... n-1] enth?lt die Reihen permutation
// d = +/- 1.0 Zeigt ob gerade Zahl (+1) oder ungerade (-1) von vertauschungen
//
// R?ckgabe NO_ERR(0)              : okay
//          MATRIX_IS_SINGULAR(16) : singular
//==============================================================================

okay_t ludcmpb(Matrix_t a, uint32_t n, uint32_t ml, uint32_t mu, uint32_t *indx, double *d);
//==============================================================================
// LU-Decomposition by Gaussian elimination of a banded matrix
// with lower bandwidth ml and upper bandwidth mu
// ==================================================
// Input
// a[0 ... n-1][0 ... n-1]
// n   order orginalmatrix
// ml  lower bandwidth of a (diagnonal not counted)
// mu  upper bandwidth of a (diagnonal not counted)
//
// Output
/*     a       AN UPPER TRIANGULAR MATRIX IN BAND STORAGE AND */
/*                THE MULTIPLIERS WHICH WERE USED TO OBTAIN IT. */
/*     ip      INDEX VECTOR OF PIVOT INDICES. */
//     d = +/- 1.0 Zeigt ob gerade Zahl (+1) oder ungerade (-1) von vertauschungen
/*              USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM. */
// index[0 ... n-1] enth?lt die Reihen permutation
//
// R?ckgabe NO_ERR(0)              : okay
//          MATRIX_IS_SINGULAR(16) : singular
/*  DETERM(A) = IP(N)*A(MD,1)*A(MD,2)*...*A(MD,N)  WITH MD=ML+MU+1. */

/*  REFERENCE.. */
/*     THIS IS A MODIFICATION OF */
/*     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER, */
/*     C.A.C.M. 15 (1972), P. 274. */
//
//==================================================================================

okay_t ludcmph(Matrix_t a,uint32_t n, uint32_t lb, uint32_t *indx, double *d);
//==================================================================================
/*C  MATRIX TRIANGULARIZATION BY GAUSSIAN ELIMINATION OF A HESSENBERG
C  MATRIX WITH LOWER BANDWIDTH LB
C  INPUT..
C     N = ORDER OF MATRIX A.
C     NDIM = DECLARED DIMENSION OF ARRAY  A .
C     A = MATRIX TO BE TRIANGULARIZED.
C     LB = LOWER BANDWIDTH OF A (DIAGONAL IS NOT COUNTED, LB.GE.1).
C  OUTPUT..
C     A(I,J), I.LE.J = UPPER TRIANGULAR FACTOR, U .
C     A(I,J), I.GT.J = MULTIPLIERS = LOWER TRIANGULAR FACTOR, I - L.
C     IP(K), K.LT.N = INDEX OF K-TH PIVOT ROW.
C     d = +/- 1.0 Zeigt ob gerade Zahl (+1) oder ungerade (-1) von vertauschungen
C              USE  SOLB  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C     index[0 ... n-1] enth?lt die Reihen permutation
C     IER = 0 IF MATRIX A IS NONSINGULAR, OR K IF FOUND TO BE
C           SINGULAR AT STAGE K.
C  USE  SOLH  TO OBTAIN SOLUTION OF LINEAR SYSTEM.
C  DETERM(A) = IP(N)*A(1,1)*A(2,2)*...*A(N,N).
C  IF IP(N)=O, A IS SINGULAR, SOL WILL DIVIDE BY ZERO.
C
C  REFERENCE..
C     THIS IS A SLIGHT MODIFICATION OF
C     C. B. MOLER, ALGORITHM 423, LINEAR EQUATION SOLVER,
C     C.A.C.M. 15 (1972), P. 274.
C-----------------------------------------------------------------------*/

void elmhes(uint32_t n,uint32_t low,uint32_t igh,Matrix_t a,uint32_t *iphes);
/*
C     this subroutine is a translation of the algol procedure elmhes,
C     num. math. 12, 349-368(1968) by martin and wilkinson.
C     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
C
C     given a real general matrix, this subroutine
C     reduces a submatrix situated in rows and columns
C     low through igh to upper hessenberg form by
C     stabilized elementary similarity transformations.
C
C     on input:
CC
C      n is the order of the matrix;
C
C      low and igh are integers determined by the balancing
C        subroutine  balanc.      if  balanc  has not been used,
C        set low=1, igh=n;
C
C      a contains the input matrix.
C
C     on output:
C
C      a contains the hessenberg matrix.  the multipliers
C        which were used in the reduction are stored in the
C        remaining triangle under the hessenberg matrix;
C
C      int contains information on the rows and columns
C        interchanged in the reduction.
C        only elements low through igh are used.
C
C     questions and comments should be directed to b. s. garbow,
C     applied mathematics division, argonne national laboratory
C
C     ------------------------------------------------------------------
C
*/
#endif


int SlfNumInterp2D(uint8_t iflag,double *xvec,size_t nx,double *yvec,size_t ny,
                  Matrix_t zmat,double x0,double y0,double *z0);
/*******************************************************************
*
*       Interpolation z(nrow,ncol)=f(x(nrow),y(ncol))
*       Der Endwert wird begrenzt
*       iflag           Inittialisierung 0/1
*       xvec[nx]        x-Vektor, monoton steigend
*       yvec[ny]        y-Vektor, monoton steigend
*       zmat[ny][nx]    z-Matrix_t
*       x0              x-Wert an dem z-Wert bestimmt wird
*       y0              y-Wert an dem z-Wert bestimmt wird
*       z0              z-Wert, der zur?ckgegeben wird
*       init            = 1 ?berpr?ft nur Vektorl?nge und ?nderung von Punkt zu Punkt
*       return 0        okay
*       return 1        Fehler zuwenig Werte == 0
*       return 2        Fehler nicht monoton steigend
*       return 3        L?nge zmat ist kleiner als xvec oder yvec
********************************************************************/

#if 0 // Noch nicht fertig
typedef struct SSlfNumSpline3_tag {

    Vector_t xvec;
    Vector_t yvec;
    Vector_t zvec;
	uint32_t n;

} SSlfNumSpline3;

SSlfNumSpline3 *SlfNumSpline3Coef(uint32_t n,double *xvec, double *yvec);
double         SlfNumSpline3(SSlfNumSpline3 *pZ,double x);
void           SlfNumSpline3Free(SSlfNumSpline3 *pZ);
#endif
//===================================================================
// natural kubic spline
//
// bildet Koeffizienten ?r Spline-Berechnung
//
// SSlfNumSpline3 *pZ = SlfNumSpline3Coef(uint32_t n,double *xvec, double *yvec);
//
// Berechnung des spline
//
// double y = SlfNumSpline3(SSlfNumSpline3 *pZ,double x);
//
// L?schen der Koeffizienten
//
// viod SlfNumSpline3Free(SSlfNumSpline3 *pZ);
//
//===================================================================
typedef struct SSlfNumStep_tag {

    double xa;
    double xb;
    double ya;
    double yb;
    double dx;
    double dy;
    double x;
    double y;
    double yp;

} SSlfNumStep;

void SlfNumStepSet(SSlfNumStep *s,double xa,double xb,double ya,double yb);
void SlfNumStepCalc(SSlfNumStep *s,double x);
//===================================================================
// Step-Funktion
// 
// x <= xa  : y  = ya
//            yp = 0.0
// x >= xb  : y  = yb
//            yp = 0.0
// ansonsten: y  = ya + (yb-ya)*[(x-xa)/(xb-xa)]^2*{3-2*[(x-xa)/(xb-xa)]}
//            yp = 6*(yb-ya)/(xb-xa)*[(x-xa)/(xb-xa)]*{1-[(x-xa)/(xb-xa)]}
//
//
// 
// SlfStepSet(SSlfNumStep *s,double xa,double xb,double ya,double yb)
// SlfStepCalc(SSlfNumStep *s,double x)
// Ergebnis: s.y und s.yp
//====================================================================

double SlfNumAbs(double x);
//===============================================
//  Abs(x)
//===============================================

sint8_t SlfNumSgn(double x);
//===============================================
//  Signum(x)
//===============================================

double SlfNumLim(double val, double limUp, double limLow);
/*****************************************************************************
  @fn                   SlfNumLim1(double *pVal, double *pLimUp, double *pLimLow)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      LimUp   upper border
  @param[in]      LimLow  lower border
   
  @return         limited Value between -Lim and Lim
*****************************************************************************/

double SlfNumLim1(double val, double lim);
/*****************************************************************************
  @fn              double SlfNumLim1(double Val, double Lim)
  @description     limits Value "val" between Lim1 and Lim2

  @param[in]      Val     value to limit
  @param[in]      Lim     upper border, lower border => *(-1.f)
   
  @return         limited Value between -Lim and Lim
*****************************************************************************/

double SlfNumATan2(double y,double x);
//===============================================
//  Arcus tangens y/x
//===============================================

double SlfNumSqrt2(double a, double b);
//===============================================
//  m = sqrt(a^2+b^2)
//===============================================
double SlfNumDmaxnorm(sint32_t n, double *v, double *w);
/* ----------------------------------------------------------------------- */
/* This function routine computes the weighted max-norm */
/* of the vector of length N contained in the array V, with weights */
/* contained in the array w of length N: */
/*   DMNORM = MAX(i=1,...,N) ABS(V(i))*W(i) */
/* ----------------------------------------------------------------------- */
int SlfNumDewset(sint32_t n, sint32_t itol, double *rtol, 
	double *atol, double *ycur, double *ewt);
/* ----------------------------------------------------------------------- */
/*  This subroutine sets the error weight vector EWT according to */
/*      EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N, */
/*  with the subscript on RTOL and/or ATOL possibly replaced by 1 above, */
/*  depending on the value of ITOL. */
/* ----------------------------------------------------------------------- */
double SlfNumDumach();
/* ----------------------------------------------------------------------- */
/* *Function Return Values: */
/*     A : the unit roundoff of the machine. */
/* ----------------------------------------------------------------------- */
int SlfNumDcfode(sint32_t meth, double *elco, double *tesco);
/* ----------------------------------------------------------------------- */
/*  DCFODE is called by the integrator routine to set coefficients */
/*  needed there.  The coefficients for the current method, as */
/*  given by the value of METH, are set for all orders and saved. */
/*  The maximum order assumed here is 12 if METH = 1 and 5 if METH = 2. */
/*  (A smaller value of the maximum order is also allowed.) */
/*  DCFODE is called once at the beginning of the problem, */
/*  and is not called again unless and until METH is changed. */

/*  The ELCO array contains the basic method coefficients. */
/*  The coefficients el(i), 1 .le. i .le. nq+1, for the method of */
/*  order nq are stored in ELCO(i,nq).  They are given by a genetrating */
/*  polynomial, i.e., */
/*      l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq. */
/*  For the implicit Adams methods, l(x) is given by */
/*      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0. */
/*  For the BDF methods, l(x) is given by */
/*      l(x) = (x+1)*(x+2)* ... *(x+nq)/K, */
/*  where         K = factorial(nq)*(1 + 1/2 + ... + 1/nq). */

/*  The TESCO array contains test constants used for the */
/*  local error test and the selection of step size and/or order. */
/*  At order nq, TESCO(k,nq) is used for the selection of step */
/*  size at order nq - 1 if k = 1, at order nq if k = 2, and at order */
/*  nq + 1 if k = 3. */
/* ----------------------------------------------------------------------- */
int SlfNumDgbfa(double *abd, sint32_t lda, sint32_t n, 
	            sint32_t ml, sint32_t mu, sint32_t *ipvt, sint32_t *info);
/* ----------------------------------------------------------------------- */
/*     DGBFA factors a double precision band matrix by elimination. */

/*     DGBFA is usually called by DGBCO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */

/*     On Entry */

/*        ABD     DOUBLE PRECISION(LDA, N) */
/*                contains the matrix in band storage.  The columns */
/*                of the matrix are stored in the columns of  ABD  and */
/*                the diagonals of the matrix are stored in rows */
/*                ML+1 through 2*ML+MU+1 of  ABD . */
/*                See the comments below for details. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  ABD . */
/*                LDA must be .GE. 2*ML + MU + 1 . */

/*        N       INTEGER */
/*                the order of the original matrix. */

/*        ML      INTEGER */
/*                number of diagonals below the main diagonal. */
/*                0 .LE. ML .LT.  N . */

/*        MU      INTEGER */
/*                number of diagonals above the main diagonal. */
/*                0 .LE. MU .LT.  N . */
/*                More efficient if  ML .LE. MU . */
/*     On Return */

/*        ABD     an upper triangular matrix in band storage and */
/*                the multipliers which were used to obtain it. */
/*                The factorization can be written  A = L*U  where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that DGBSL will divide by zero if */
/*                     called.  Use  RCOND  in DGBCO for a reliable */
/*                     indication of singularity. */

/*     Band Storage */

/*           If  A  is a band matrix, the following program segment */
/*           will set up the input. */

/*                   ML = (band width below the diagonal) */
/*                   MU = (band width above the diagonal) */
/*                   M = ML + MU + 1 */
/*                   DO 20 J = 1, N */
/*                      I1 = MAX(1, J-MU) */
/*                      I2 = MIN(N, J+ML) */
/*                      DO 10 I = I1, I2 */
/*                         K = I - J + M */
/*                         ABD(K,J) = A(I,J) */
/*                10    CONTINUE */
/*                20 CONTINUE */

/*           This uses rows  ML+1  through  2*ML+MU+1  of  ABD . */
/*           In addition, the first  ML  rows in  ABD  are used for */
/*           elements generated during the triangularization. */
/*           The total number of rows needed in  ABD  is  2*ML+MU+1 . */
/*           The  ML+MU by ML+MU  upper left triangle and the */
/*           ML by ML  lower right triangle are not referenced. */
/* ----------------------------------------------------------------------- */
int SlfNumDscal(sint32_t n, double *da, double *dx, sint32_t incx);
/* ----------------------------------------------------------------------- */
/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DA  double precision scale factor */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */

/*     --Output-- */
/*       DX  double precision result (unchanged if N.LE.0) */

/*     Replace double precision DX by double precision DA*DX. */
/*     For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */
/* ----------------------------------------------------------------------- */
int SlfNumDaxpy(sint32_t n, double *da, double *dx, 
	            sint32_t incx, double *dy, sint32_t incy);
/* ----------------------------------------------------------------------- */
/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DA  double precision scalar multiplier */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */
/*       DY  double precision vector with N elements */
/*     INCY  storage spacing between elements of DY */

/*     --Output-- */
/*       DY  double precision result (unchanged if N .LE. 0) */

/*     Overwrite double precision DY with double precision DA*DX + DY. */
/*     For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) + */
/*       DY(LY+I*INCY), */
/*     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is */
/*     defined in a similar way using INCY. */
/* ----------------------------------------------------------------------- */
sint32_t SlfNumIdamax(sint32_t n, double *dx, sint32_t incx);
/* ----------------------------------------------------------------------- */
/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */

/*     --Output-- */
/*   IDAMAX  smallest index (zero if N .LE. 0) */

/*     Find smallest index of maximum magnitude of double precision DX. */
/*     IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */
/* ----------------------------------------------------------------------- */
int SlfNumDgefa(double *a, sint32_t lda, sint32_t n, sint32_t *ipvt, sint32_t *info);
/* ----------------------------------------------------------------------- */
/*     DGEFA factors a double precision matrix by Gaussian elimination. */

/*     DGEFA is usually called by DGECO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */
/*     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) . */

/*     On Entry */

/*        A       DOUBLE PRECISION(LDA, N) */
/*                the matrix to be factored. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*     On Return */

/*        A       an upper triangular matrix and the multipliers */
/*                which were used to obtain it. */
/*                The factorization can be written  A = L*U  where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that DGESL or DGEDI will divide by zero */
/*                     if called.  Use  RCOND  in DGECO for a reliable */
/*                     indication of singularity. */
/* ----------------------------------------------------------------------- */
double SlfNumDbnorm(sint32_t n, double *a, sint32_t nra, sint32_t ml, 
	                sint32_t mu, double *w);
/* ----------------------------------------------------------------------- */
/*     DGEFA factors a double precision matrix by Gaussian elimination. */

/*     DGEFA is usually called by DGECO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */
/*     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) . */

/*     On Entry */

/*        A       DOUBLE PRECISION(LDA, N) */
/*                the matrix to be factored. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*     On Return */

/*        A       an upper triangular matrix and the multipliers */
/*                which were used to obtain it. */
/*                The factorization can be written  A = L*U  where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that DGESL or DGEDI will divide by zero */
/*                     if called.  Use  RCOND  in DGECO for a reliable */
/*                     indication of singularity. */
/* ----------------------------------------------------------------------- */
double SlfNumDfnorm(sint32_t n, double *a, double *w);
/* ----------------------------------------------------------------------- */
/* This function computes the norm of a full N by N matrix, */
/* stored in the array A, that is consistent with the weighted max-norm */
/* on vectors, with weights stored in the array W: */
/*   DFNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) ) */
/* ----------------------------------------------------------------------- */
double SlfNumDdot(sint32_t n, double *dx, sint32_t incx, double *dy, sint32_t incy);
/* ----------------------------------------------------------------------- */
/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       DX  double precision vector with N elements */
/*     INCX  storage spacing between elements of DX */
/*       DY  double precision vector with N elements */
/*     INCY  storage spacing between elements of DY */

/*     --Output-- */
/*     DDOT  double precision dot product (zero if N .LE. 0) */

/*     Returns the dot product of double precision DX and DY. */
/*     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY), */
/*     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is */
/*     defined in a similar way using INCY. */
/* ----------------------------------------------------------------------- */
int SlfNumDgbsl(double *abd, sint32_t lda, sint32_t n, 
	sint32_t ml, sint32_t mu, sint32_t *ipvt, double *b, sint32_t job);
/* ----------------------------------------------------------------------- */
/*     DGBSL solves the double precision band system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by DGBCO or DGBFA. */

/*     On Entry */

/*        ABD     DOUBLE PRECISION(LDA, N) */
/*                the output from DGBCO or DGBFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  ABD . */

/*        N       INTEGER */
/*                the order of the original matrix. */

/*        ML      INTEGER */
/*                number of diagonals below the main diagonal. */

/*        MU      INTEGER */
/*                number of diagonals above the main diagonal. */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from DGBCO or DGBFA. */

/*        B       DOUBLE PRECISION(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B , where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically this indicates singularity */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if DGBCO has set RCOND .GT. 0.0 */
/*        or DGBFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z) */
/*           IF (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0) */
/*        10 CONTINUE */
/* ----------------------------------------------------------------------- */
int SlfNumDgesl(double *a, sint32_t lda, sint32_t n, sint32_t *ipvt, double *b, sint32_t job);
/* ----------------------------------------------------------------------- */
/*     DGESL solves the double precision system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by DGECO or DGEFA. */

/*     On Entry */

/*        A       DOUBLE PRECISION(LDA, N) */
/*                the output from DGECO or DGEFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from DGECO or DGEFA. */

/*        B       DOUBLE PRECISION(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B  where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically this indicates singularity */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if DGECO has set RCOND .GT. 0.0 */
/*        or DGEFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL DGECO(A,LDA,N,IPVT,RCOND,Z) */
/*           IF (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL DGESL(A,LDA,N,IPVT,C(1,J),0) */
/*        10 CONTINUE */
/* ----------------------------------------------------------------------- */
double SlfNumPowDi(double x, sint32_t n);
/* ----------------------------------------------------------------------- */
// y = x ^ n n: negativ or positiv
/* ----------------------------------------------------------------------- */

/*-------------------------------------------------------------------------
void   FilterDigPar(SSlfNumFiltDig *ps, uint8_t n,double *pa, double *pb);
void   FilterDigInit(SSlfNumFiltDig *ps, uint8_t n,double *px, double *py);
double FilterDigInit(SSlfNumFiltDig *ps, double xact);

Mir der Funktion FilterDigPar werden alle xi und yi null gesetzt

  y    b0 + b1*z^-1 + ... bn*z^-n
  -- = --------------------------
  x    a0 + a1*z^-1 + ... an*z^-n

         1.    n              n
  y(i) = -- ( sum(xi-j*bj) - sum(yi-j*aj) )
         a0   j=0            j=1
-------------------------------------------------------------------------*/
#define SLF_NUM_MAX_FILT_ORDER    5
typedef struct tag_SSlfNumFiltDig {
    uint8_t        na,nb;
    double       x[SLF_NUM_MAX_FILT_ORDER+1];
    double       y[SLF_NUM_MAX_FILT_ORDER+1];
    double       a[SLF_NUM_MAX_FILT_ORDER+1];
    double       b[SLF_NUM_MAX_FILT_ORDER+1];
}SSlfNumFiltDig;
void   SlfNumFiltDigPar(SSlfNumFiltDig *ps,double *pa, uint8_t na, double *pb, uint8_t nb);
void   SlfNumFiltDigInit(SSlfNumFiltDig *ps,double *px, uint8_t nx, double *py, uint8_t ny);
double SlfNumFiltDigCalc(SSlfNumFiltDig *ps, double xact);

void SlfNumPt1FiltInit(SSlfNumFiltDig *ps, double t1, double dt, double y0);
#ifdef __cplusplus
    }
#endif
#endif
#endif