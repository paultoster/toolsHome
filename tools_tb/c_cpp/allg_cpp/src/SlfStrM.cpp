#include "SlfStrM.h"
#include "SlfStrV.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <ctype.h>

namespace slf
{

  //===========================================================
  // Matrix_t-String-Klasse
  //===========================================================
  CStrM::CStrM():NRow(0), NCol(0), Len(0), MatStr(){}
  CStrM::CStrM(const char* p)
  {
    make(1, 1);
    MatStr[0][0] = p;
  }
  CStrM::CStrM(CStr &str)
  {
    make(1, 1);
    MatStr[0][0] = str;
  }
  CStrM::CStrM(std::size_t nrow, std::size_t ncol)
  {
    make(nrow, ncol);
    empty();
  }
  CStrM::~CStrM()
  {
  }
  void CStrM::clear() {

    MatStr.resize(0);
    NRow = 0;
    NCol = 0;
    Len  = 0;
  }
  void CStrM::empty()
  {
    for (std::size_t i = 0; i < NRow; ++i)
      for (std::size_t j = 0; j < NCol; ++j)
        MatStr[i][j] = "";
  }
  okay_t CStrM::make(std::size_t nrow, std::size_t ncol) 
  {
    MatStr.resize(nrow);
    for (std::size_t i = 0; i < nrow; ++i)
    {
      MatStr[i].resize(ncol);
    }
    NRow = nrow;
    NCol = ncol;
    Len = nrow*ncol;

    return OKAY;
  }
  okay_t CStrM::cutRow(std::size_t irow) {


    if (irow >= NRow)
      return OKAY;

    for (std::size_t ic = 0; ic < NCol; ic++) {

      for (std::size_t ir = irow + 1; ir < NRow; ir++) {

        MatStr[ir - 1][ic] = MatStr[ir][ic];
      }
    }
    return make(NRow - 1, NCol);
  }

  okay_t CStrM::cutCol(std::size_t icol) {


    if (icol >= NCol)
      return OKAY;

    for (std::size_t ic = icol + 1; ic < NCol; ic++) {

      for (std::size_t ir = 0; ir < NRow; ir++) {

        MatStr[ir][ic - 1] = MatStr[ir][ic];
      }
    }
    return make(NRow, NCol - 1);

    return OKAY;
  }
  okay_t CStrM::find(const CStr &s, std::size_t *pirow, std::size_t *picol) {

    return find(s.c_str(), pirow, picol);
  }
  okay_t CStrM::find(const char* pstr, std::size_t *pirow, std::size_t *picol) {


    if (*pirow >= NRow || *picol >= NCol)
      return NOT_OKAY;

    for (std::size_t i = 0; i < NRow; i++) {

      for (std::size_t j = 0; j < NCol; j++) {

        if (MatStr[i][j].compareText(pstr)) {
          *pirow = i;
          *picol = j;
          return OKAY;
        }
      }
    }
    return NOT_OKAY;
  }
  void CStrM::cat(const CStr &s, std::size_t irow, std::size_t icol) {
    cat(s.c_str(), irow, icol);
  }
  void CStrM::cat(const char *str, std::size_t irow, std::size_t icol) {

    if (irow >= NRow || icol >= NCol) {
      make(SLF_MAX(irow + 1, NRow), SLF_MAX(icol + 1, NCol));
    }

    MatStr[irow][icol].append(str);
  }
  void CStrM::cpy(const CStr &s, std::size_t irow, std::size_t icol) {
    cat(s.c_str(), irow, icol);
  }
  void CStrM::cpy(const char *str, std::size_t irow, std::size_t icol) {

    if (irow >= NRow || icol >= NCol) {
      make(SLF_MAX(irow + 1, NRow), SLF_MAX(icol + 1, NCol));
    }

    MatStr[irow][icol] = str;
  }
  const char *CStrM::get_str(std::size_t irow, std::size_t icol) const {

    if (irow >= NRow || icol >= NCol)
      return 0;
    else
      return MatStr[irow][icol].c_str();
  }
  const CStr *CStrM::get(std::size_t irow, std::size_t icol) const {

    if (irow >= NRow || icol >= NCol)
      return 0;
    else
      return &MatStr[irow][icol];
  }
  CStr CStrM::getval(std::size_t irow, std::size_t icol) const
  {
    CStr str;
    if( (irow < NRow) && (icol < NCol))
    {
    str = MatStr[irow][icol];
    }
      return str;
  }
  CStrV CStrM::getCol(std::size_t icol) const
  {
    CStrV vec;

    if (icol >= NCol) icol = NCol;
    if (icol)
    {
      --icol;

      for (std::size_t i = 0; i < NRow; ++i)
      {
        vec.append(MatStr[i][icol]);
      }
    }
    return vec;
  }
  CStrV CStrM::getRow(std::size_t irow) const
  {
    CStrV vec;
    if (irow >= NRow) irow = NRow;
    if (irow)
    {
      --irow;

      for (std::size_t i = 0; i < NCol; ++i)
      {
        vec.append(MatStr[irow][i]);
      }
    }
    return vec;
  }
  //
  // build vector
  // icoltype = true:
  // +         +
  // | 1  2  3 |
  // | 4  5  6 |
  // | 7  8  9 |
  // +         +
  // icoltype = false:
  // +         +
  // | 1  4  7 |
  // | 2  5  8 |
  // | 3  6  9 |
  // +         +
  CStrV CStrM::buildVector(bool icoltype) const
  {
    CStrV vec;
    if (icoltype)
    {
      for (std::size_t icol = 0; icol < NCol; ++icol)
      {
        for (std::size_t i = 0; i < NRow; ++i)
        {
          vec.append(MatStr[i][icol]);
        }
      }
    }
    else
    {
      for (std::size_t irow = 0; irow < NRow; ++irow)
      {
        for (std::size_t i = 0; i < NCol; ++i)
        {
          vec.append(MatStr[irow][i]);
        }
      }
    }
    return vec;
  }
  void  CStrM::setValue(const std::size_t irow, const std::size_t icol, const CStr &str)
  {
    if (irow >= NRow) make(irow + 1, NCol);
    if (icol >= NCol) make(NRow, icol + 1);

    MatStr[irow][icol] = str;
  }
  void CStrM::transpone(void) {

    if (NRow > 1 || NCol > 1) {

      CStrM mat_str(NCol, NRow);
      for (std::size_t i = 0; i < NRow; ++i)
        for (std::size_t j = 0; j < NCol; ++j)
          mat_str.setValue(j,i,MatStr[i][j]);

      *this = mat_str;
    }
  }
  CStrM& CStrM::operator=(const CStrM& sm)
  {
    clear();

    if (sm.getNrows() > 0 && sm.getNcols() > 0) {

      for (std::size_t i = 0; i < sm.getNrows(); i++) {
        for (std::size_t j = 0; j < sm.getNcols(); j++) {
          cat(sm.get_str(i, j), i, j);
        }
      }
    }
    return *this;                 // return *this
  }
} // namespace