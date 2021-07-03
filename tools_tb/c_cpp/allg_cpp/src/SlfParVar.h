// DsModVar.h
//
// Variablen der Modelle 
//
#include "SlfModVar.h"
#include <memory>
#include <vector>

#ifndef SLF_PARVAR_H_INCLUDED
#define SLF_PARVAR_H_INCLUDED

namespace slf
{

  //===============================================================================================
  // struct definition of name,unit,comment, type and pointer to specific variable
  //===============================================================================================
  class CParVarDef : public CVarDef
  {
  public:
    CStrV         pargroupvec;  // first item is maingroup all other subgroup
    CStr          defaultvalue;   // defaultvalue value
    uint8_t       value_is_set;

    CParVarDef() :CVarDef(), pargroupvec(), defaultvalue(), value_is_set(0){}
    CParVarDef(const CParVarDef &p) 
    { 
      *this = p; 
     // CVarDef::operator=(p);
    }
    bool ValueIsSet(void) 
    {
      return (value_is_set != 0);
    }
    bool     SetParValue(const CStrM &strm)
    {
      if (SetValue(strm))
      {
        ++value_is_set;
        return true;
      }
      return false;
    }
    bool     SetParValue(const CMatrixD &mat)
    {
      if (SetValue(mat))
      {
        ++value_is_set;
        return true;
      }
      return false;
    }



    CParVarDef& operator=(const CParVarDef& v);
  };
 
  //===============================================================================================
  // class definition of handling Variable Vector List
  //===============================================================================================
  class CParVarDefCollect
  {
  private:
    std::vector<CParVarDef> parvarDefVec;
    CStr                    maingroup;
  public:
    CParVarDefCollect() :parvarDefVec() , maingroup(""){}

    CParVarDefCollect &operator=(const CParVarDefCollect& v)
    {
      
      parvarDefVec.clear();
      for (std::vector<CParVarDef>::const_iterator it = v.parvarDefVec.begin(); it != v.parvarDefVec.end(); ++it)
      {
        parvarDefVec.push_back(*it);
      }
      return *this;
    }

    void Clear(void) { parvarDefVec.clear(); }
    std::size_t Size(void) const { return parvarDefVec.size(); }
    CParVarDef &Get(std::size_t index) { return parvarDefVec[index];    }
    const CStr &GetName(std::size_t index) const { return parvarDefVec[index].name; }
    const CStrV &GetGroup(std::size_t index) const { return parvarDefVec[index].pargroupvec; }
    const CStr &GetUnit(std::size_t index) const { return parvarDefVec[index].unit; }
    const CStr &GetDefault(std::size_t index) const { return parvarDefVec[index].defaultvalue; }
    const CStr &GetComment(std::size_t index) const { return parvarDefVec[index].comment; }
    const CStr GetType(std::size_t index) {return parvarDefVec[index].GetType();}

    void Add(const CParVarDef &pvd)
    {
       parvarDefVec.push_back(pvd);
    }
    void Append(CParVarDefCollect &pvdc)
    {
      for (std::size_t i = 0; i < pvdc.Size(); ++i)
      {
        parvarDefVec.push_back(pvdc.Get(i));
      }
    }

    void SetValueIsSet(const std::size_t index) {
      if (index < parvarDefVec.size()) parvarDefVec[index].value_is_set++;
    }
    bool CheckValue(const std::size_t index)
    {
      if (index < parvarDefVec.size())
      {
        return (parvarDefVec[index].value_is_set != 0);
      }
      return false;
    }
    void SetMainGroup(const CStr &mg) { maingroup = mg; }
    void SetMainGroup(const std::string &mg) { maingroup = mg;  }
    void SetMainGroup(const char *mg) { maingroup = mg;}

    void SetParDef(double &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pdouble = &var; v.ndim = 0; v.type = VAR_DEF_DOUBLE; v.group = VAR_DEF_GROUP_SINGLE;v.pargroupvec = build_group(subgroup);v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v);}
    void SetParDef(float &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pfloat = &var; v.ndim = 0; v.type = VAR_DEF_FLOAT; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int64_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint64 = &var; v.ndim = 0; v.type = VAR_DEF_INT64; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint64_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint64 = &var; v.ndim = 0; v.type = VAR_DEF_UINT64; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int32_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint32 = &var; v.ndim = 0; v.type = VAR_DEF_INT32; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint32_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint32 = &var; v.ndim = 0; v.type = VAR_DEF_UINT32; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int16_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint16 = &var; v.ndim = 0; v.type = VAR_DEF_INT16; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint16_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint16 = &var; v.ndim = 0; v.type = VAR_DEF_UINT16; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int8_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint8 = &var; v.ndim = 0; v.type = VAR_DEF_INT8; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint8_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint8 = &var; v.ndim = 0; v.type = VAR_DEF_UINT8; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(std::string &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pstring = &var; v.ndim = 0; v.type = VAR_DEF_STRING; v.group = VAR_DEF_GROUP_STRING; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CStr &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfstring = &var; v.ndim = 0; v.type = VAR_DEF_SLFSTRING; v.group = VAR_DEF_GROUP_STRING; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(char *var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pchar = var; v.ndim = 0; v.type = VAR_DEF_CHAR; v.group = VAR_DEF_GROUP_STRING; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }

    void SetParDef(double &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pdouble = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_DOUBLE; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(float &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pfloat = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_FLOAT; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int64_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint64 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT64; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint64_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint64 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT64; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int32_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint32 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT32; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint32_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint32 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT32; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int16_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint16 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT16; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint16_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint16 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT16; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(int8_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pint8 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT8; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(uint8_t &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.puint8 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT8; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(std::string &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pstring = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_STRING; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CStr &var, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfstring = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_SLFSTRING; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }

    void SetParDef(CVectorD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfvecd = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECD; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CVectorF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfvecf = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECF; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CVectorI &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfveci = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECI; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CVectorU &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfvecu = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECU; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CMatrixD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfmatd = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATD; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CMatrixF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfmatf = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATF; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CMatrixI &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfmati = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATI; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CMatrixU &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pslfmatu = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATU; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CTable1DD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.ptab1dd = &var; v.ndim = 0; v.type = VAR_DEF_TAB1DD; v.group = VAR_DEF_GROUP_TABLE1D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CTable1DF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.ptab1df = &var; v.ndim = 0; v.type = VAR_DEF_TAB1DF; v.group = VAR_DEF_GROUP_TABLE1D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CTable2DD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.ptab2dd = &var; v.ndim = 0; v.type = VAR_DEF_TAB2DD; v.group = VAR_DEF_GROUP_TABLE2D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CTable2DF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.ptab2df = &var; v.ndim = 0; v.type = VAR_DEF_TAB2DF; v.group = VAR_DEF_GROUP_TABLE2D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CStrV &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pstrv = &var; v.ndim = 0; v.type = VAR_DEF_STRINGV; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }
    void SetParDef(CStrM &var, const char *subgroup, const char *name, const char *unit, const char *defaultval="", const char *comment=""){ CParVarDef v; v.pstrm = &var; v.ndim = 0; v.type = VAR_DEF_STRINGM; v.group = VAR_DEF_GROUP_STRING_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); }

  private:
    CStrV build_group(const char *subgroup)
    {
      CStrV pgv;

      // split with "." if their are mor then 1 subgroup
      if (strlen(subgroup))
      {
        SlfStrVSplit(pgv, subgroup, ".");
      }
      // insert in front maingroup if exist
      if (maingroup.size())
      {
        pgv.insert(maingroup,0);
      }
      return pgv;
    }
  };  
} // namespace slf

#endif