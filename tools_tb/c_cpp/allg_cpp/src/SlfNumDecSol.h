/***************************************************************************

                                decsol.h
                             -------------------
    28.11.20 Thomas Berthold
    modified for C++ into namespace slf
                             
    modified for C by:   : Blake Ashby
    last modified        : Nov 15, 2002
    email                : bmashby@stanford.edu

Helper routines for StiffIntegratorT class. Modified from original Fortran
code provided by:

	E. Hairer & G. Wanner
	Universite de Geneve, dept. de Mathematiques
	CH-1211 GENEVE 4, SWITZERLAND
	E-mail : HAIRER@DIVSUN.UNIGE.CH, WANNER@DIVSUN.UNIGE.CH

 ***************************************************************************/

#ifndef _SLF_NUM_DECSOL_H_
#define _SLF_NUM_DECSOL_H_

#include "SlfNum.h"

namespace slf
{
  template<class T>
  inline const T& decsol_max(const T& a, const T& b)
  {
    return a > b ? a : b;
  }

  template<class T>
  inline const T& decsol_min(const T& a, const T& b)
  {
    return a < b ? a : b;
  }

  // Matrix Triangularization by Gaussian Elimination
  size_t dec(const size_t n, CMatrixD &A, CVectorI &ip);

  // Solution of linear system A*x = b
  void sol(const size_t n, CMatrixD &A, double *b, CVectorI &ip);
  void sol(const size_t n, CMatrixD &A, CVectorD &b, CVectorI &ip);
  // Matrix Triangularization by Gaussian Elimination of a Hessenberg
  // matrix with lower bandwidth lb
  size_t dech(const size_t n, CMatrixD &A, size_t lb, CVectorI &ip);

  // Solution of linear system A*x = b -- Hessenberg matrix
  void solh(const size_t n, CMatrixD &A, size_t lb, CVectorD &b, CVectorI &ip);

  // Matrix Triangularization by Gaussian Elimination for complex matrices
  size_t decc(const size_t n, CMatrixD &AR, CMatrixD &AI, CVectorI &ip);

  // Solution of linear system A*x = b -- complex matrices
  void solc(const size_t n, CMatrixD &AR, CMatrixD &AI, CVectorD &br, CVectorD &bi, CVectorI &ip);
  void solc(const size_t n, CMatrixD &AR, CMatrixD &AI, double *br, double *bi, CVectorI &ip);

  // Matrix Triangularization by Gaussian Elimination -- Hessenberg, complex
  // matrices
  size_t dechc(const size_t n, CMatrixD &AR, CMatrixD &AI, size_t lb, CVectorI &ip);

  // Solution of linear system A*x = b -- Hessenberg, complex matrices
  void solhc(const size_t n, CMatrixD &AR, CMatrixD &AI, size_t lb,
    CVectorD &br, CVectorD &bi, CVectorI &ip);

  //Matrix Triangularization by Gaussian Elimination -- banded matrix
  size_t decb(const size_t n, CMatrixD &A, size_t ml, size_t mu, CVectorI &ip);

  // Solution of linear system A*x = b -- banded matrix
  void solb(const size_t n, CMatrixD &A, size_t ml, size_t mu, CVectorD &b, CVectorI &ip);
  void solb(const size_t n, CMatrixD &A, size_t ml, size_t mu, double *b, CVectorI &ip);

  //Matrix Triangularization by Gaussian Elimination -- banded, complex matrices
  size_t decbc(const size_t n, CMatrixD &AR, CMatrixD &AI, size_t ml, size_t mu, CVectorI &ip);

  // Solution of linear system A*x = b -- banded, complex matrices
  void solbc(const size_t n, CMatrixD &AR, CMatrixD &AI, size_t ml, size_t mu,
    CVectorD &br, CVectorD &bi, CVectorI &ip);
  void solbc(const size_t n, CMatrixD &AR, CMatrixD &AI, size_t ml, size_t mu,
    double *br, double *bi, CVectorI &ip);

  // reduces a submatrix to upper Hessenberg form
  void elmhes(const size_t n, size_t low, size_t igh, CMatrixD &A, CVectorI &inter);

} // namespace slf

#endif /* _DECSOL_H_ */
