// DsModVar.h
//
// Variablen der Modelle 
//
#include "SlfParVar.h"
#include <memory>
#include <vector>

#ifndef SLF_PARVARVAL_H_INCLUDED
#define SLF_PARVARVAL_H_INCLUDED

namespace slf
{

  //-------------------------------------------------------------------------
  // Store Input Value
  //-------------------------------------------------------------------------
  class CParVarVal
  {
  public:

    CParVarVal() :type(VAR_DEF_NULL),ndim(0){}
    ~CParVarVal()
    {
    }
    EVarType      type;


    std::size_t   ndim;
    std::shared_ptr<double> pdouble;
    std::shared_ptr<float>    pfloat;
    std::shared_ptr<int64_t>  pint64;
    std::shared_ptr<uint64_t> puint64;
    std::shared_ptr<int32_t>  pint32;
    std::shared_ptr<uint32_t> puint32;
    std::shared_ptr<int16_t>  pint16;
    std::shared_ptr<uint16_t> puint16;
    std::shared_ptr<int8_t>   pint8;
    std::shared_ptr<uint8_t>  puint8;

    std::shared_ptr<CVectorD> pslfvecd;
    std::shared_ptr<CVectorF> pslfvecf;
    std::shared_ptr<CVectorI> pslfveci;
    std::shared_ptr<CVectorI64> pslfveci64;
    std::shared_ptr<CVectorU> pslfvecu;
    std::shared_ptr<CVectorU64> pslfvecu64;
    std::shared_ptr<CMatrixD> pslfmatd;
    std::shared_ptr<CMatrixF> pslfmatf;
    std::shared_ptr<CMatrixI> pslfmati;
    std::shared_ptr<CMatrixU> pslfmatu;
    std::shared_ptr<CTable1DD> pslftab1dd;
    std::shared_ptr<CTable1DF> pslftab1df;
    std::shared_ptr<CTable2DD> pslftab2dd;
    std::shared_ptr<CTable2DF> pslftab2df;
    std::shared_ptr<std::string> pstr;
    std::shared_ptr<CStr> pslfstr;
    std::shared_ptr<CStrV> pslfstrv;
    std::shared_ptr<CStrM> pslfstrm;

      
          
    double   *SetVal(double  &var) { pdouble = std::make_shared<double>(); *pdouble = var; type = VAR_DEF_DOUBLE; return pdouble.get(); }
    float    *SetVal(float   &var) { pfloat = std::make_shared<float>(); *pfloat = var; type = VAR_DEF_FLOAT; return pfloat.get(); }
    int64_t  *SetVal(int64_t  &var) { pint64 = std::make_shared<int64_t>(); *pint64 = var; type = VAR_DEF_INT64; return pint64.get(); }
    uint64_t *SetVal(uint64_t  &var) { puint64 = std::make_shared<uint64_t>(); *puint64 = var; type = VAR_DEF_UINT64; return puint64.get(); }
    int32_t  *SetVal(int32_t  &var) { pint32 = std::make_shared<int32_t>(); *pint32 = var; type = VAR_DEF_INT32; return pint32.get(); }
    uint32_t *SetVal(uint32_t  &var) { puint32 = std::make_shared<uint32_t>(); *puint32 = var; type = VAR_DEF_UINT32; return puint32.get(); }
    int16_t  *SetVal(int16_t  &var) { pint16 = std::make_shared<int16_t>(); *pint16 = var; type = VAR_DEF_INT16; return pint16.get(); }
    uint16_t *SetVal(uint16_t  &var) { puint16 = std::make_shared<uint16_t>(); *puint16 = var; type = VAR_DEF_UINT16; return puint16.get(); }
    int8_t   *SetVal(int8_t  &var) { pint8 = std::make_shared<int8_t>(); *pint8 = var; type = VAR_DEF_INT8; return pint8.get(); }
    uint8_t  *SetVal(uint8_t  &var) { puint8 = std::make_shared<uint8_t>(); *puint8 = var; type = VAR_DEF_UINT8; return puint8.get(); }
    CVectorD *SetVal(CVectorD  &var) { pslfvecd = std::make_shared<CVectorD>(); *pslfvecd = var; type = VAR_DEF_SLFVECD; return pslfvecd.get(); }
    CVectorF *SetVal(CVectorF  &var) { pslfvecf = std::make_shared<CVectorF>(); *pslfvecf = var; type = VAR_DEF_SLFVECF; return pslfvecf.get(); }
    CVectorI *SetVal(CVectorI  &var) { pslfveci = std::make_shared<CVectorI>(); *pslfveci = var; type = VAR_DEF_SLFVECI; return pslfveci.get(); }
    CVectorU *SetVal(CVectorU  &var) { pslfvecu = std::make_shared<CVectorU>(); *pslfvecu = var; type = VAR_DEF_SLFVECU; return pslfvecu.get(); }
    CMatrixD *SetVal(CMatrixD  &var) { pslfmatd = std::make_shared<CMatrixD>(); *pslfmatd = var; type = VAR_DEF_SLFMATD; return pslfmatd.get(); }
    CMatrixF *SetVal(CMatrixF  &var) { pslfmatf = std::make_shared<CMatrixF>(); *pslfmatf = var; type = VAR_DEF_SLFMATF; return pslfmatf.get(); }
    CMatrixI *SetVal(CMatrixI  &var) { pslfmati = std::make_shared<CMatrixI>(); *pslfmati = var; type = VAR_DEF_SLFMATI; return pslfmati.get(); }
    CMatrixU *SetVal(CMatrixU  &var) { pslfmatu = std::make_shared<CMatrixU>(); *pslfmatu = var; type = VAR_DEF_SLFMATU; return pslfmatu.get(); }
    CTable1DD *SetVal(CTable1DD  &var) { pslftab1dd = std::make_shared<CTable1DD>(); *pslftab1dd = var; type = VAR_DEF_TAB1DD; return pslftab1dd.get(); }
    CTable1DF *SetVal(CTable1DF  &var) { pslftab1df = std::make_shared<CTable1DF>(); *pslftab1df = var; type = VAR_DEF_TAB1DF; return pslftab1df.get(); }
    CTable2DD *SetVal(CTable2DD  &var) { pslftab2dd = std::make_shared<CTable2DD>(); *pslftab2dd = var; type = VAR_DEF_TAB2DD; return pslftab2dd.get(); }
    CTable2DF *SetVal(CTable2DF  &var) { pslftab2df = std::make_shared<CTable2DF>(); *pslftab2df = var; type = VAR_DEF_TAB2DF; return pslftab2df.get(); }
    std::string *SetVal(std::string  &var) { pstr = std::make_shared<std::string>(); *pstr = var; type = VAR_DEF_STRING; return pstr.get(); }
    CStr        *SetVal(CStr  &var) { pslfstr = std::make_shared<CStr>(); *pslfstr = var; type = VAR_DEF_SLFSTRING; return pslfstr.get(); }
    CStrV       *SetVal(CStrV &var) { pslfstrv = std::make_shared<CStrV>(); *pslfstrv = var; type = VAR_DEF_STRINGV; return pslfstrv.get(); }
    CStrM       *SetVal(CStrM &var) { pslfstrm = std::make_shared<CStrM>(); *pslfstrm = var; type = VAR_DEF_STRINGM; return pslfstrm.get(); }

    CStr  *SetVal(char *pvar) { pslfstr = std::make_shared<CStr>(); *pslfstr = pvar; type = VAR_DEF_CHAR; return pslfstr.get(); }


    CVectorD *SetVal(double *pvar, std::size_t ndim){ pslfvecd = std::make_shared<CVectorD>(); pslfvecd->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfvecd->push_back(pvar[i]); type = VAR_DEF_SLFVECD; return pslfvecd.get(); }
    CVectorF *SetVal(float *pvar, std::size_t ndim){ pslfvecf = std::make_shared<CVectorF>(); pslfvecf->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfvecf->push_back(pvar[i]); type = VAR_DEF_SLFVECF; return pslfvecf.get(); }
    CVectorI64 *SetVal(int64_t *pvar, std::size_t ndim){ pslfveci64 = std::make_shared<CVectorI64>(); pslfveci64->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfveci64->push_back(pvar[i]); type = VAR_DEF_SLFVECI64; return pslfveci64.get(); }
    CVectorU64 *SetVal(uint64_t *pvar, std::size_t ndim){ pslfvecu64 = std::make_shared<CVectorU64>(); pslfvecu64->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfvecu64->push_back(pvar[i]); type = VAR_DEF_SLFVECU64; return pslfvecu64.get(); }
    CVectorI   *SetVal(int32_t *pvar, std::size_t ndim){ pslfveci = std::make_shared<CVectorI>(); pslfveci->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfveci->push_back(pvar[i]); type = VAR_DEF_SLFVECI; return pslfveci.get(); }
    CVectorU   *SetVal(uint32_t *pvar, std::size_t ndim){ pslfvecu = std::make_shared<CVectorU>(); pslfvecu->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfvecu->push_back(pvar[i]); type = VAR_DEF_SLFVECU; return pslfvecu.get(); }
    CVectorI   *SetVal(int16_t *pvar, std::size_t ndim){ pslfveci = std::make_shared<CVectorI>(); pslfveci->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfveci->push_back(pvar[i]); type = VAR_DEF_SLFVECI; return pslfveci.get(); }
    CVectorU   *SetVal(uint16_t *pvar, std::size_t ndim){ pslfvecu = std::make_shared<CVectorU>(); pslfvecu->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfvecu->push_back(pvar[i]); type = VAR_DEF_SLFVECU; return pslfvecu.get(); }
    CVectorI   *SetVal(int8_t *pvar, std::size_t ndim){ pslfveci = std::make_shared<CVectorI>(); pslfveci->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfveci->push_back(pvar[i]); type = VAR_DEF_SLFVECI; return pslfveci.get(); }
    CVectorU   *SetVal(uint8_t *pvar, std::size_t ndim){ pslfvecu = std::make_shared<CVectorU>(); pslfvecu->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfvecu->push_back(pvar[i]); type = VAR_DEF_SLFVECU; return pslfvecu.get(); }
    CStrV      *SetVal(std::string *pvar, std::size_t ndim){ pslfstrv = std::make_shared<CStrV>(); pslfvecu->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfstrv->append(pvar[i].c_str()); type = VAR_DEF_STRINGV; return pslfstrv.get(); }
    CStrV      *SetVal(CStr *pvar, std::size_t ndim){ pslfstrv = std::make_shared<CStrV>(); pslfvecu->resize(ndim); for (std::size_t i = 0; i<ndim; ++i)pslfstrv->append(pvar[i].c_str()); type = VAR_DEF_STRINGV; return pslfstrv.get(); }

 
 
  };

  // Set Values from inpt
  class CParVarValCollect
  {
  private:
    std::vector<CParVarDef> parvarDefVec;
    std::vector<CParVarVal> parvarValVec;
  public:
    CParVarValCollect() {}
    ~CParVarValCollect() 
    {
#if 0
      char a;
      a = 0;
#endif
    }

    void Clear(void) { parvarDefVec.clear(); parvarValVec.clear();}
    std::size_t Size(void) const { return parvarDefVec.size(); }
    const CParVarDef &Get(std::size_t index) const { return parvarDefVec[index]; }

    // search for name and group return true and give back isearch
    bool Search(const CStr &name, const CStrV &group, std::size_t &isearch) const;
    

    


    void SetParVal(double &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pdouble = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_DOUBLE; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(float &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pfloat = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_FLOAT; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int64_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pint64 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_INT64; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint64_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.puint64 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_UINT64; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int32_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pint32 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_INT32; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint32_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.puint32 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_UINT32; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int16_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pint16 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_INT16; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint16_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.puint16 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_UINT16; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int8_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pint8 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_INT8; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint8_t &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.puint8 = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_UINT8; v.group = VAR_DEF_GROUP_SINGLE; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(std::string &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pstring = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_STRING; v.group = VAR_DEF_GROUP_STRING; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CStr &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfstring = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFSTRING; v.group = VAR_DEF_GROUP_STRING; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(char *var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfstring = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFSTRING; v.group = VAR_DEF_GROUP_STRING; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }

    void SetParVal(double *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecd = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECD; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(float *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecf = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECF; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int64_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfveci64 = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECI64; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint64_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecu64 = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECU64; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int32_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfveci = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECI; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint32_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecu = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECU; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int16_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfveci = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECI; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint16_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecu = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECU; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(int8_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfveci = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECI; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(uint8_t *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecu = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_SLFVECU; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(std::string *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pstrv = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_STRINGV; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CStr *pvar, std::size_t ndim, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pstrv = val.SetVal(pvar,ndim); v.ndim = ndim; v.type = VAR_DEF_STRINGV; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }

    void SetParVal(CVectorD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecd = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFVECD; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CVectorF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecf = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFVECF; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CVectorI &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfveci = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFVECI; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CVectorU &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfvecu = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFVECU; v.group = VAR_DEF_GROUP_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CMatrixD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfmatd = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFMATD; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CMatrixF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfmatf = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFMATF; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CMatrixI &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfmati = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFMATI; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CMatrixU &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pslfmatu = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_SLFMATU; v.group = VAR_DEF_GROUP_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CTable1DD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.ptab1dd = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_TAB1DD; v.group = VAR_DEF_GROUP_TABLE1D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CTable1DF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.ptab1df = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_TAB1DF; v.group = VAR_DEF_GROUP_TABLE1D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CTable2DD &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.ptab2dd = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_TAB2DD; v.group = VAR_DEF_GROUP_TABLE2D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CTable2DF &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.ptab2df = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_TAB2DF; v.group = VAR_DEF_GROUP_TABLE2D; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CStrV &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pstrv = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_STRINGV; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }
    void SetParVal(CStrM &var, const char *subgroup, const char *name, const char *unit, const char *defaultval = "", const char *comment = ""){ CParVarVal val; CParVarDef v; v.pstrm = val.SetVal(var); v.ndim = 0; v.type = VAR_DEF_STRINGM; v.group = VAR_DEF_GROUP_STRING_MATRIX; v.pargroupvec = build_group(subgroup); v.name = name; v.unit = unit; v.defaultvalue = defaultval; v.comment = comment; parvarDefVec.push_back(v); parvarValVec.push_back(val); }

  private:
    CStrV build_group(const char *subgroup)
    {
      CStrV pgv;

      // split with "." if their are mor then 1 subgroup
      if (strlen(subgroup))
      {
        SlfStrVSplit(pgv, subgroup, ".");
      }
      return pgv;
    }
  };
  
} // namespace slf

#endif