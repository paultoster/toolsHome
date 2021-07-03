#ifndef SLF_PRINT_H_INCLUDED
#define SLF_PRINT_H_INCLUDED

#include "SlfStr.h"
#include "SlfStrV.h"
#include "SlfStrM.h"
#include "SlfParDef.h"
#include <sstream>
#include <type_traits>
namespace slf
{

  //template<typename T>
  //CStr SlfPrintStringSingle(const T &val);
  template<typename T>
  CStr SlfPrintStringSingle(const T &val)
  {
    std::stringstream s;

    if (std::is_same<T, uint8_t>::value)
    {
      uint16_t v = (uint16_t)val;
      s << v;
    }
    else if (std::is_same<T, int8_t>::value)
    {
      int16_t v = (int16_t)val;
      s << v;
    }
    else
    {
      s << val;
    }

    return s.str();
  }

  //template<typename T>
  //CStr SlfPrintStringArray(const T *pvec, std::size_t ndim);
  template<typename T>
  CStr SlfPrintStringArray(T *pvec, std::size_t ndim)
  {
    std::stringstream s;

    s << SLFPAR_VEC_START;
    for (std::size_t i = 0; i < ndim; ++i)
    {
      if (std::is_same<T, uint8_t>::value)
      {
        uint16_t v = (uint16_t)pvec[i];
        s << v;
      }
      else if (std::is_same<T, int8_t>::value)
      {
        int16_t v = (int16_t)pvec[i];
        s << v;
      }
      else
      {
        s << pvec[i];
      }
      if (i + 1 == ndim)
      {
        s << SLFPAR_VEC_END;
      }
      else
      {
        s << SLFPAR_VEC_ROW_DELIM;
      }

    }
    return s.str();
  }

  //template<typename T>
  //CStr SlfPrintStringArrayString(const T *pvec, std::size_t ndim);
  template<typename T>
  CStr SlfPrintStringArrayString(const T *pvec, std::size_t ndim)
  {
    std::stringstream s;

    s << SLFPAR_VEC_START;
    for (std::size_t i = 0; i < ndim; ++i)
    {
      s << SLFPAR_QUOT_TEXT << pvec[i] << SLFPAR_QUOT_TEXT;
      if (i + 1 == ndim)
      {
        s << SLFPAR_VEC_END;
      }
      else
      {
        s << SLFPAR_VEC_ROW_DELIM;
      }

    }
    return s.str();

  }

  //template<typename T>
  //CStr SlfPrintStringVector(const T &vec);
  template<typename T>
  CStr SlfPrintStringVector(const T &vec)
  {
    std::stringstream s;

    if (vec.size())
    {
      s << SLFPAR_VEC_START;
      for (std::size_t i = 0; i < vec.size(); ++i)
      {
        if (std::is_same<T, uint8_t>::value)
        {
          uint16_t v = (uint16_t)vec[i];
          s << v;
        }
        else if (std::is_same<T, int8_t>::value)
        {
          int16_t v = (int16_t)vec[i];
          s << v;
        }
        else
        {
          s << vec[i];
        }
        if (i + 1 == vec.size())
        {
          s << SLFPAR_VEC_END;
        }
        else
        {
          s << SLFPAR_VEC_ROW_DELIM;
        }

      }
    }
    return s.str();

  }

  //template<typename T>
  //CStr SlfPrintStringMatrix(const T &mat);
  template<typename T>
  CStr SlfPrintStringMatrix(const T &mat)
  {
    std::stringstream s;

    if ((mat.GetNrow() > 1) || ((mat.GetNrow() == 1) && (mat.GetNcol() == 1)))
    {
      s << SLFPAR_VEC_START;
    }
    for (std::size_t i = 0; i < mat.GetNrow(); ++i)
    {
      if (mat.GetNcol() > 1)
      {
        s << SLFPAR_VEC_START;
      }
      for (std::size_t j = 0; j < mat.GetNcol(); ++j)
      {
        s << mat[i][j];
        if (j + 1 <= mat.GetNcol())
        {
          s << SLFPAR_VEC_COL_DELIM;
        }
      }
      if (mat.GetNcol() > 1)
      {
        s << SLFPAR_VEC_END;
      }
      if (i + 1 == mat.GetNrow())
      {
        if ((mat.GetNrow() > 1) || ((mat.GetNrow() == 1) && (mat.GetNcol() == 1)))
        {
          s << SLFPAR_VEC_END;
        }
      }
      else
      {
        s << SLFPAR_VEC_ROW_DELIM;
      }
    }
    return s.str();

  }

  CStr SlfPrintStringStrV(const CStrV &strv);
  CStr SlfPrintStringStrM(const CStrM &strm);
}
#endif