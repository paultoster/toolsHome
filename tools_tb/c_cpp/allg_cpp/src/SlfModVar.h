// DsModVar.h
//
// Variablen der Modelle 
//
#include "SlfBase.h"
#include "SlfStr.h"
#include "SlfStrV.h"
#include "SlfStrM.h"
#include "SlfNum.h"
#include "SlfMessage.h"

#ifndef DS_MODVAR_H_INCLUDED
#define DS_MODVAR_H_INCLUDED

namespace slf
{
  // Var Types
  enum EVarType {
    VAR_DEF_NULL,               // nicht belegt
    VAR_DEF_DOUBLE,             // Einzelwert double,
    VAR_DEF_FLOAT,              // float, ...
    VAR_DEF_INT64,
    VAR_DEF_UINT64,
    VAR_DEF_INT32,
    VAR_DEF_UINT32,
    VAR_DEF_INT16,
    VAR_DEF_UINT16,
    VAR_DEF_INT8,
    VAR_DEF_UINT8,
    VAR_DEF_STRING,             // Einzelwert String-Klasse std::string
    VAR_DEF_SLFSTRING,          // Einzelwert String-Klasse slf::CStr
    VAR_DEF_CHAR,               // String aus charater mit '\0' beendet z.B. matlab eingabe

    VAR_DEF_ARR_DOUBLE,         // Array mit double-Werten, 
    VAR_DEF_ARR_FLOAT,          // Länge muß übergerdneter Struct definiert sein
    VAR_DEF_ARR_INT64,
    VAR_DEF_ARR_UINT64,
    VAR_DEF_ARR_INT32,
    VAR_DEF_ARR_UINT32,
    VAR_DEF_ARR_INT16,
    VAR_DEF_ARR_UINT16,
    VAR_DEF_ARR_INT8,
    VAR_DEF_ARR_UINT8,
    VAR_DEF_ARR_STRING,         // Array String-Klasse std::string
    VAR_DEF_ARR_SLFSTRING,      // Array String-Klasse slf::CStr

    VAR_DEF_SLFVECD,          // Vektor-Struktur mit slf::CSlfVec<double>
    VAR_DEF_SLFVECF,          // Vektor-Struktur mit slf::CSlfVec<float>
    VAR_DEF_SLFVECI,          // Vektor-Struktur mit slf::CSlfVec<int32_t>
    VAR_DEF_SLFVECI64,          // Vektor-Struktur mit slf::CSlfVec<int64_t>
    VAR_DEF_SLFVECU,          // Vektor-Struktur mit slf::CSlfVec<uint32_t>
    VAR_DEF_SLFVECU64,          // Vektor-Struktur mit slf::CSlfVec<uint64_t>
    VAR_DEF_SLFMATD,          // Matrix_t-Struktur mit slf::CSlfMat<double>
    VAR_DEF_SLFMATF,          // Matrix_t-Struktur mit slf::CSlfMat<float>
    VAR_DEF_SLFMATI,          // Matrix_t-Struktur mit slf::CSlfMat<int32_t>
    VAR_DEF_SLFMATU,          // Matrix_t-Struktur mit slf::CSlfMat<uitnt32_t>
    VAR_DEF_TAB1DD,          // einfache Tabellen-Klasse CSfl1DTab<double> y = f(x)
    VAR_DEF_TAB1DF,          // einfache Tabellen-Klasse CSfl1DTab<float> y = f(x)
    VAR_DEF_TAB2DD,          // zweidim. Tabellen-Klasse  CSfl2DTab<double> z = f(x,y)
    VAR_DEF_TAB2DF,          // zweidim. Tabellen-Klasse  CSfl2DTab<float> z = f(x,y)
    VAR_DEF_STRINGV,         // String-Klasse slf::CStrV (Vektor-Klasse)
    VAR_DEF_STRINGM,         // String-Matrix_t-Klasse slf::CStrM

  };
  enum EVarTypeGroup
  { VAR_DEF_GROUP_NULL
  , VAR_DEF_GROUP_SINGLE
  , VAR_DEF_GROUP_VECTOR
  , VAR_DEF_GROUP_MATRIX
  , VAR_DEF_GROUP_TABLE1D
  , VAR_DEF_GROUP_TABLE2D
  , VAR_DEF_GROUP_STRING
  , VAR_DEF_GROUP_STRING_VECTOR
  , VAR_DEF_GROUP_STRING_MATRIX
  };
  enum ECalcStatus
  {
    NO_CALC
    , DEFAULT_FAC_OFFSET
    , FAC_OFFSET_SET
  };

  //===============================================================================================
  // struct definition of name,unit,comment, type and pointer to specific variable
  //===============================================================================================
  class CVarDef
  {
  public:
    CStr          name;
    CStr          unit;
    CStr          comment;
    EVarType      type;
    EVarTypeGroup group;
    double        factor;               // factor to calculate out from in
    double        offset;               // Offset to calculate out from in
    ECalcStatus   status;               // no factor/offset; factor is 1.0 and offset 0.0 or factor is set
    CMessage      message;

    std::size_t   ndim;
    double        *pdouble;
    float         *pfloat;
    int64_t       *pint64;
    uint64_t      *puint64;
    int32_t       *pint32;
    uint32_t      *puint32;
    int16_t       *pint16;
    uint16_t      *puint16;
    int8_t        *pint8;
    uint8_t       *puint8;
    std::string   *pstring;
    CStr          *pslfstring;
    char          *pchar;

    CVectorD *pslfvecd;
    CVectorF *pslfvecf;
    CVectorI *pslfveci;
    CVectorI64 *pslfveci64;
    CVectorU *pslfvecu;
    CVectorU64 *pslfvecu64;
    CMatrixD *pslfmatd;
    CMatrixF *pslfmatf;
    CMatrixI *pslfmati;
    CMatrixU *pslfmatu;
    CTable1DD   *ptab1dd;
    CTable1DF   *ptab1df;
    CTable2DD   *ptab2dd;
    CTable2DF   *ptab2df;
    CStrV       *pstrv;
    CStrM       *pstrm;

    static const char *TextEVarType[];
    static const std::size_t TextEVarTypeN;
    static const double Epsilon;              // resolution to proof for factor==1 and offset==0

    CVarDef() :name(), unit(), comment(), type(VAR_DEF_NULL), group(VAR_DEF_GROUP_NULL), factor(0.), offset(0.), status(NO_CALC), message("VarDef")
      , ndim(0), pdouble(0), pfloat(0), pint64(0), puint64(0), pint32(0), puint32(0), pint16(0), puint16(0), pint8(0), puint8(0)
      , pstring(0), pslfstring(0), pchar(0), pslfvecd(0), pslfvecf(0), pslfveci(0), pslfvecu(0), pslfmatd(0), pslfmatf(0), pslfmati(0), pslfmatu(0)
      , ptab1dd(0), ptab1df(0), ptab2dd(0), ptab2df(0), pstrv(0), pstrm(0) {}
    const char *GetUnit(void) { return unit.c_str(); }
    CVarDef& operator=(const CVarDef& v);
    okay_t   calcFrom(const CVarDef &from, const CStr &fromModulName, const CStr &toModulName);
    void     GetValue(const CVarDef& v);
    const char *GetType(void);
    bool     SetValue(const CStrM &strm);
    bool     SetValue(const CMatrixD &mat);
    CStr     PrintValue(void);
  private:
    // take value from v
    void GetSingleValue(const CVarDef& v);
    void GetSingleValue(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueDouble(const CVarDef& v);
    void GetSingleValueDouble(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueFloat(const CVarDef& v);
    void GetSingleValueFloat(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueInt64(const CVarDef& v);
    void GetSingleValueInt64(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueUint64(const CVarDef& v);
    void GetSingleValueUint64(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueInt32(const CVarDef& v);
    void GetSingleValueInt32(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueUint32(const CVarDef& v);
    void GetSingleValueUint32(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueInt16(const CVarDef& v);
    void GetSingleValueInt16(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueUint16(const CVarDef& v);
    void GetSingleValueUint16(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueInt8(const CVarDef& v);
    void GetSingleValueInt8(const CVarDef& v, const double &factor, const double &offset);
    void GetSingleValueUint8(const CVarDef& v);
    void GetSingleValueUint8(const CVarDef& v, const double &factor, const double &offset);
  };
  //class CVarDefCalc
  //{
  //public:
  //  enum ECalcStatus
  //  { NO_CALC
  //  , DEFAULT_FAC_OFFSET
  //  , FAC_OFFSET_SET
  //  };
  //private:
  //  CVarDef     mOut;                  // out set from in out = in * factor +offset
  //  CVarDef     mIn;   
  //  double      mFactor;               // factor to calculate out from in
  //  double      mOffset;               // Offset to calculate out from in
  //  ECalcStatus mStatus;               // no factor/offset; factor is 1.0 and offset 0.0 or factor is set
  //  double      mEpsilon;              // resolution to proof for factor==1 and offset==0
  //  CMessage    mMessage;
  //public:
  //  const CMessage &Message;
  //  CVarDefCalc() :mOut(), mIn(), mFactor(1.0), mOffset(0.0), mStatus(NO_CALC),mEpsilon(1.e-6), mMessage("VarDefCalc"), Message(mMessage) {}
  //  okay_t      SetInOut(const CVarDef &in,const CStr &inModulName, const CVarDef &out, const CStr &outModulName) { mIn = in; mOut = out; return Proof(inModulName, outModulName); }
  //  double      GetFactor(void) {return mFactor;}
  //  double      GetOffset(void) {return mOffset;}
  //  ECalcStatus GetStatus(void) {return mStatus;}
  //  void        CalcInFromOut(void);
  //private:
  //  okay_t Proof(const CStr &inModulName, const CStr &outModulName);
  //};

  //===============================================================================================
  // class definition of handling Variable Vector List
  //===============================================================================================
  class CVarDefCollect
  {
  private:
    std::vector<CVarDef> varDefVec;
  public:
    CVarDefCollect() :varDefVec() {}

    CVarDefCollect &operator=(const CVarDefCollect& v)
    {
      
      varDefVec.clear();
      for (std::vector<CVarDef>::const_iterator it = v.varDefVec.begin(); it != v.varDefVec.end(); ++it)
      {
        varDefVec.push_back(*it);
      }
      return *this;
    }

    void Clear(void) { varDefVec.clear(); }
    std::size_t Size(void) { return varDefVec.size(); }
    CVarDef &Get(std::size_t index) { return varDefVec[index];    }
    void SetVarDef(double &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pdouble = &var; v.ndim = 0; v.type = VAR_DEF_DOUBLE; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(float &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pfloat = &var; v.ndim = 0; v.type = VAR_DEF_FLOAT; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int64_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint64 = &var; v.ndim = 0; v.type = VAR_DEF_INT64; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint64_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint64 = &var; v.ndim = 0; v.type = VAR_DEF_UINT64; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int32_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint32 = &var; v.ndim = 0; v.type = VAR_DEF_INT32; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint32_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint32 = &var; v.ndim = 0; v.type = VAR_DEF_UINT32; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int16_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint16 = &var; v.ndim = 0; v.type = VAR_DEF_INT16; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint16_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint16 = &var; v.ndim = 0; v.type = VAR_DEF_UINT16; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int8_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint8 = &var; v.ndim = 0; v.type = VAR_DEF_INT8; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint8_t &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint8 = &var; v.ndim = 0; v.type = VAR_DEF_UINT8; v.group = VAR_DEF_GROUP_SINGLE; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(std::string &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pstring = &var; v.ndim = 0; v.type = VAR_DEF_STRING; v.group = VAR_DEF_GROUP_STRING; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CStr &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfstring = &var; v.ndim = 0; v.type = VAR_DEF_SLFSTRING; v.group = VAR_DEF_GROUP_STRING; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(char *var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pchar = var; v.ndim = 0; v.type = VAR_DEF_CHAR; v.group = VAR_DEF_GROUP_STRING; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }

    void SetVarDef(double &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pdouble = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_DOUBLE; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(float &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pfloat = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_FLOAT; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int64_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint64 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT64; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint64_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint64 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT64; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int32_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint32 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT32; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint32_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint32 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT32; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int16_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint16 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT16; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint16_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint16 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT16; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(int8_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pint8 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_INT8; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(uint8_t &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.puint8 = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_UINT8; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(std::string &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pstring = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_STRING; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CStr &var, std::size_t ndim, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfstring = &var; v.ndim = ndim; v.type = VAR_DEF_ARR_SLFSTRING; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }

    void SetVarDef(CVectorD &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfvecd = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECD; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CVectorF &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfvecf = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECF; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CVectorI &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfveci = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECI; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CVectorU &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfvecu = &var; v.ndim = 0; v.type = VAR_DEF_SLFVECU; v.group = VAR_DEF_GROUP_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CMatrixD &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfmatd = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATD; v.group = VAR_DEF_GROUP_MATRIX; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CMatrixF &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfmatf = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATF; v.group = VAR_DEF_GROUP_MATRIX; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CMatrixI &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfmati = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATI; v.group = VAR_DEF_GROUP_MATRIX; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CMatrixU &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pslfmatu = &var; v.ndim = 0; v.type = VAR_DEF_SLFMATU; v.group = VAR_DEF_GROUP_MATRIX; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CTable1DD &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.ptab1dd = &var; v.ndim = 0; v.type = VAR_DEF_TAB1DD; v.group = VAR_DEF_GROUP_TABLE1D; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CTable1DF &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.ptab1df = &var; v.ndim = 0; v.type = VAR_DEF_TAB1DF; v.group = VAR_DEF_GROUP_TABLE1D; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CTable2DD &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.ptab2dd = &var; v.ndim = 0; v.type = VAR_DEF_TAB2DD; v.group = VAR_DEF_GROUP_TABLE2D; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CTable2DF &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.ptab2df = &var; v.ndim = 0; v.type = VAR_DEF_TAB2DF; v.group = VAR_DEF_GROUP_TABLE2D; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CStrV &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pstrv = &var; v.ndim = 0; v.type = VAR_DEF_STRINGV; v.group = VAR_DEF_GROUP_STRING_VECTOR; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }
    void SetVarDef(CStrM &var, const char *name, const char *unit, const char *comment){ CVarDef v; v.pstrm = &var; v.ndim = 0; v.type = VAR_DEF_STRINGM; v.group = VAR_DEF_GROUP_STRING_MATRIX; v.name = name; v.unit = unit; v.comment = comment; varDefVec.push_back(v); }

    
  };

} // namespace slf

#endif