
#include "SlfPar.h"
#include "SlfSys.h"
#include "SlfFkt.h"
#include <sstream>
#include <iomanip>
#include "SlfStrRegEx.h"

namespace slf
{
  void CPar::createNewParSet(void)
  {
    mIInputParVec = mNInputParVec;
    ++mNInputParVec;
    mInputParVec.resize(mNInputParVec);
  }
  void CPar::deleteLastParSet(void)
  {
    if (mIInputParVec)
    {
      --mIInputParVec;
      mInputParVec.resize(mNInputParVec);
    }
  }

  void WriteParameterStructure(const CParVarDefCollect &pardefcollect, CStrV &strvec)
  {
    std::size_t nmax = 0;
    std::size_t nmaxunit = 0;
    for (std::size_t index = 0; index < pardefcollect.Size(); ++index)
    {
      CStr str = SlfStrVReSplit(pardefcollect.GetGroup(index), SLFPAR_GROUP_DELIM);

      std::size_t n = str.size();
      std::size_t m = pardefcollect.GetUnit(index).size();

      if (n) ++n;

      n += pardefcollect.GetName(index).size();

      if (n > nmax)
      {
        nmax = n;
      }
      if (m > nmaxunit)
      {
        nmaxunit = m;
      }
    }
 
    for (std::size_t index = 0; index < pardefcollect.Size(); ++index)
    {
      CStr tt = SlfStrVReSplit(pardefcollect.GetGroup(index), SLFPAR_GROUP_DELIM);
      
      // group and name
      //---------------
      if (tt.size()) tt.append(SLFPAR_GROUP_DELIM);

      tt.append(pardefcollect.GetName(index));

      std::stringstream s;

      s << std::left << std::setw(nmax) << tt;

      // unit
      //-----
      if (nmaxunit) // is their any unit
      {
        s << " ";

        tt = pardefcollect.GetUnit(index);
        if (tt.size())
        {
          tt.insertText(SLFPAR_UNIT_START, 0);
          tt.append(SLFPAR_UNIT_END);
        }
        s << std::left << std::setw(nmaxunit+strlen(SLFPAR_UNIT_START)+strlen(SLFPAR_UNIT_END)) << tt;
      }

      // equal
      //------
      s << " " << SLFPAR_EQUAL_SIGN << " "; 

      // default value if
      //-----------------
      if (pardefcollect.GetDefault(index).size())
      {
        s << pardefcollect.GetDefault(index);
      }

      s << SLFPAR_VAL_END;

      // comment
      if (pardefcollect.GetComment(index).size())
      {
        s << " " << SLFPAR_COMMMENT << " " << pardefcollect.GetComment(index);
      }

      // copy line into string vector
      strvec.append(s.str());



    }
  }
  void SetParameterFromVarValCollect(const CParVarValCollect &pvvc, CParVarDef &pardef)
  {
    std::size_t isearch = std::string::npos;
    if (pvvc.Search(pardef.name, pardef.pargroupvec, isearch))
    {
      pardef.GetValue(pvvc.Get(isearch));
      ++pardef.value_is_set;
    }
  }


  // inspect defaultvalue to set this value to pardef
  okay_t SetParameterFromDefault(const CStr &defaultvalue, CParVarDef &pardef, CStr &texterr)
  {
    if (defaultvalue.size() == 0)
    {
      texterr.catFormat("Error SetParameterFromDefault:defaultValue: is empty");
      return NOT_OKAY;
    }

    CStrV textvec;    // Contains all text values which are substituded in inputvec with #xxxxx (is a number to corresponding vector index)
    CStrV inputvec;   // string value from default must be put into a string-vector for reading

    // add default value string
    inputvec.append(defaultvalue);

    // prepare input structure
    if (PrepareStructure(SLFPAR_COMMMENT,inputvec, textvec, texterr) != OKAY)
    {
      return NOT_OK;
    }

    // read Variables first as text
    CStrM varvalues;
    bool flag_is_text;
    if (ReadVariableValues(inputvec, 0, 0, textvec, varvalues, flag_is_text, texterr) != OKAY)
    {
      return NOT_OK;
    }

    if (flag_is_text)
    {
      pardef.SetParValue(varvalues);
    }
    else
    {
      CMatrixD mat;
      // Conevrt to numeric
      ConvertVariableValues(varvalues, mat);

      pardef.SetParValue(mat);
    }

    return OKAY;
  }
} // namespace slf