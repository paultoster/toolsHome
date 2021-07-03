#include "SlfPrint.h"

namespace slf
{


  CStr SlfPrintStringStrV(const CStrV &strv)
  {
    std::stringstream s;

    s << SLFPAR_VEC_START;
    for (std::size_t i = 0; i < strv.getNrows(); ++i)
    {
      s << SLFPAR_QUOT_TEXT << strv[i] << SLFPAR_QUOT_TEXT;
      if (i + 1 == strv.getNrows())
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

  CStr SlfPrintStringStrM(const CStrM &strm)
  {
    std::stringstream s;

    if ((strm.getNrows() > 1) || ((strm.getNrows() == 1) && (strm.getNcols() == 1)))
    {
      s << SLFPAR_VEC_START;
    }
    for (std::size_t i = 0; i < strm.getNrows(); ++i)
    {
      if (strm.getNcols() > 1)
      {
        s << SLFPAR_VEC_START;
      }
      for (std::size_t j = 0; j < strm.getNcols(); ++j)
      {
        s << SLFPAR_QUOT_TEXT << strm.get_str(i, j) << SLFPAR_QUOT_TEXT;

        if (j + 1 <= strm.getNcols())
        {
          s << SLFPAR_VEC_COL_DELIM;
        }
      }
      if (strm.getNcols() > 1)
      {
        s << SLFPAR_VEC_END;
      }
      if (i + 1 == strm.getNrows())
      {
        if ((strm.getNrows() > 1) || ((strm.getNrows() == 1) && (strm.getNcols() == 1)))
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
}
