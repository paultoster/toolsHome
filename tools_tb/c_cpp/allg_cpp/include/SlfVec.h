#ifndef SLF_VEC_H_INCLUDED
#define SLF_VEC_H_INCLUDED
 
#include <vector>
#include "SlfBase.h"

namespace slf
{

  class CVectorD : public std::vector<double>
  {
  private:
    std::size_t len;
    
  public:
    CVectorD(const std::size_t n) { this->resize(n); len = n; }
    CVectorD(void) {}
    std::size_t GetNdim() {return this->size();}
  };

  class CMatrixD
  {
  private:
    std::size_t nrow,ncol;
    std::vector<CVectorD> mat;

  public:
    CMatrixD(std::size_t mrow, std::size_t mcol) { mat.resize(mrow); for (std::size_t i = 0; i < mrow; ++i) mat[i].resize(mcol); nrow = mrow; ncol = mcol; }
    CMatrixD(void) { nrow = 0; ncol = 0; mat.resize(nrow);}
    std::size_t GetNdim() const { return nrow*ncol; }
    std::size_t GetNrow() const { return nrow; }
    std::size_t GetNcol() const { return ncol; }
    void SetNrow(const std::size_t mrow) { mat.resize(mrow);  for (std::size_t i = nrow; i < mrow; ++i) mat[i].resize(ncol); nrow = mrow; }
    void SetNcol(const std::size_t mcol) { for (std::size_t i = 0; i < nrow; ++i) mat[i].resize(mcol); ncol = mcol; }
    void SetNdim(const std::size_t mrow,const std::size_t mcol) { mat.resize(mrow);  for (std::size_t i = 0; i < mrow; ++i) mat[i].resize(mcol); nrow = mrow; ncol = mcol; }
    CVectorD& operator[] (std::size_t i) { return mat[i]; }
    CMatrixD& operator=(CMatrixD& mat_t) { this->SetNrow(mat_t.GetNrow()); this->SetNcol(mat_t.GetNcol()); for (std::size_t i = 0; i < nrow; ++i) { for (std::size_t j = 0; j < ncol; ++j) mat[i][j] = mat_t[i][j]; } return *this; }

    void transpone(void) { CMatrixD mat_t(ncol, nrow); for (std::size_t i = 0; i < nrow; ++i) { for (std::size_t j = 0; j < ncol; ++j) mat_t[j][i] = mat[i][j]; }
                           this->SetNrow(mat_t.GetNrow()); this->SetNcol(mat_t.GetNcol()); for (std::size_t i = 0; i < nrow; ++i) { for (std::size_t j = 0; j < ncol; ++j) mat[i][j] = mat_t[i][j]; }
                         }
  };
}

#endif //SLF_VEC_H_INCLUDED