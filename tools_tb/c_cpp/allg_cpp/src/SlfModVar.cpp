// DsModVar.h
//
// Variablen der Modelle 
//
#include "SlfModVar.h"
#include "SlfFkt.h"
#include "SlfPrint.h"

namespace slf
{

  // declaration in SlfModVar.h
  const char *CVarDef::TextEVarType[] =
  { "VAR_DEF_NULL",
    "VAR_DEF_DOUBLE",
    "VAR_DEF_FLOAT",
    "VAR_DEF_INT64",
    "VAR_DEF_UINT64",
    "VAR_DEF_INT32",
    "VAR_DEF_UINT32",
    "VAR_DEF_INT16",
    "VAR_DEF_UINT16",
    "VAR_DEF_INT8",
    "VAR_DEF_UINT8",
    "VAR_DEF_STRING",
    "VAR_DEF_SLFSTRING",
    "VAR_DEF_CHAR",     
    "VAR_DEF_ARR_DOUBLE",
    "VAR_DEF_ARR_FLOAT", 
    "VAR_DEF_ARR_INT64",
    "VAR_DEF_ARR_UINT64",
    "VAR_DEF_ARR_INT32",
    "VAR_DEF_ARR_UINT32",
    "VAR_DEF_ARR_INT16",
    "VAR_DEF_ARR_UINT16",
    "VAR_DEF_ARR_INT8",
    "VAR_DEF_ARR_UINT8",
    "VAR_DEF_ARR_STRING",     
    "VAR_DEF_ARR_SLFSTRING", 
    "VAR_DEF_SLFVECD",         
    "VAR_DEF_SLFVECF",         
    "VAR_DEF_SLFVECI",         
    "VAR_DEF_SLFVECU",         
    "VAR_DEF_SLFMATD",         
    "VAR_DEF_SLFMATF",         
    "VAR_DEF_SLFMATI",         
    "VAR_DEF_SLFMATU",         
    "VAR_DEF_TAB1DD",          
    "VAR_DEF_TAB1DF",          
    "VAR_DEF_TAB2DD",          
    "VAR_DEF_TAB2DF",          
    "VAR_DEF_STRINGV",         
    "VAR_DEF_STRINGM"         
  };
  const std::size_t CVarDef::TextEVarTypeN = (sizeof(TextEVarType)/sizeof(char *));

  const double CVarDef::Epsilon = 1.e-6;

  CVarDef& CVarDef::operator=(const CVarDef& v)
  {
    name = v.name;
    unit = v.unit;
    comment = v.comment;
    type = v.type;
    group = v.group;
    ndim = v.ndim;

    switch (type)
    {
    case VAR_DEF_NULL:
    default:
      break;
    case VAR_DEF_DOUBLE:
    case VAR_DEF_ARR_DOUBLE:
      pdouble = v.pdouble;
      break;
    case VAR_DEF_FLOAT:
    case VAR_DEF_ARR_FLOAT:
      pfloat = v.pfloat;
      break;
    case VAR_DEF_INT64:
    case VAR_DEF_ARR_INT64:
      pint64 = v.pint64;
      break;
    case VAR_DEF_UINT64:
    case VAR_DEF_ARR_UINT64:
      puint64 = v.puint64;
      break;
    case VAR_DEF_INT32:
    case VAR_DEF_ARR_INT32:
      pint32 = v.pint32;
      break;
    case VAR_DEF_UINT32:
    case VAR_DEF_ARR_UINT32:
      puint32 = v.puint32;
      break;
    case VAR_DEF_INT16:
    case VAR_DEF_ARR_INT16:
      pint16 = v.pint16;
      break;
    case VAR_DEF_UINT16:
    case VAR_DEF_ARR_UINT16:
      puint16 = v.puint16;
      break;
    case VAR_DEF_INT8:
    case VAR_DEF_ARR_INT8:
      pint8 = v.pint8;
      break;
    case VAR_DEF_UINT8:
    case VAR_DEF_ARR_UINT8:
      puint8 = v.puint8;
      break;
    case VAR_DEF_STRING:
    case VAR_DEF_ARR_STRING:
      pstring = v.pstring;
      break;
    case VAR_DEF_SLFSTRING:
    case VAR_DEF_ARR_SLFSTRING:
      pslfstring = v.pslfstring;
      break;
    case VAR_DEF_CHAR:
      pchar = v.pchar;
      break;
    case VAR_DEF_SLFVECD:
      pslfvecd = v.pslfvecd;
      break;
    case VAR_DEF_SLFVECF:
      pslfvecf = v.pslfvecf;
      break;
    case VAR_DEF_SLFVECI:
      pslfveci = v.pslfveci;
      break;
    case VAR_DEF_SLFVECU:
      pslfvecu = v.pslfvecu;
      break;
    case VAR_DEF_SLFMATD:
      pslfmatd = v.pslfmatd;
      break;
    case VAR_DEF_SLFMATF:
      pslfmatf = v.pslfmatf;
      break;
    case VAR_DEF_SLFMATI:
      pslfmati = v.pslfmati;
      break;
    case VAR_DEF_SLFMATU:
      pslfmatu = v.pslfmatu;
      break;
    case VAR_DEF_TAB1DD:
      ptab1dd = v.ptab1dd;
      break;
    case VAR_DEF_TAB1DF:
      ptab1df = v.ptab1df;
      break;
    case VAR_DEF_TAB2DD:
      ptab2dd = v.ptab2dd;
      break;
    case VAR_DEF_TAB2DF:
      ptab2df = v.ptab2df;
      break;
    case VAR_DEF_STRINGV:
      pstrv = v.pstrv;
      break;
    case VAR_DEF_STRINGM:
      pstrm = v.pstrm;
      break;
    }
    return *this;
  }
  okay_t CVarDef::calcFrom(const CVarDef &from, const CStr &fromModulName, const CStr &toModulName)
  {
    CStr errText;
    if (!SlfFktUnitConv(unit.c_str(), from.unit.c_str(), factor, offset, errText))
    {
      std::stringstream ss;
      ss << "No Unit Convert" << std::endl;
      ss << "From Modul[" << fromModulName.c_str() << "] Variable <" << from.name << "> unit: " << from.unit.c_str() << std::endl;
      ss << "To   Modul[" << toModulName.c_str() << "] Variable <" << name << "> unit: " << unit.c_str() << std::endl;
      ss << errText.c_str();
      message.SetErr(NO_FACTOR_OFFSET_FOUND, ss.str().c_str());
    }
    else
    {
      if ((std::abs(factor - 1.0) < Epsilon) && (std::abs(offset) < Epsilon)) status = DEFAULT_FAC_OFFSET;
      else                                                                    status = FAC_OFFSET_SET;

      if (from.group != group)
      {
        std::stringstream ss;
        ss << "Type for setting a connection is wrong";
        ss << "From Modul[" << fromModulName.c_str() << "] Variable <" << from.name << "> has type" << CVarDef::TextEVarType[SLF_MIN((std::size_t)(from.type), CVarDef::TextEVarTypeN - 1)] << std::endl;
        ss << "To   Modul[" << toModulName.c_str() << "] Variable <" << name << "> has type" << CVarDef::TextEVarType[SLF_MIN((std::size_t)(type), CVarDef::TextEVarTypeN - 1)] << std::endl;
        message.SetErr(INPUT_NOT_FOUND, ss.str().c_str());
        status = NO_CALC;
      }
      if (from.group != VAR_DEF_GROUP_SINGLE)
      {
        std::stringstream ss;
        ss << "Type for setting a connection is not programmed";
        ss << "From Modul[" << fromModulName.c_str() << "] Variable <" << from.name << "> has type" << CVarDef::TextEVarType[SLF_MIN((std::size_t)(from.type), CVarDef::TextEVarTypeN - 1)] << std::endl;
        ss << "To   Modul[" << toModulName.c_str() << "] Variable <" << name << "> has type" << CVarDef::TextEVarType[SLF_MIN((std::size_t)(type), CVarDef::TextEVarTypeN - 1)] << std::endl;
        message.SetErr(UNKOWN_ERR, ss.str().c_str());
        status = NO_CALC;

      }
    }
    return message.IsOkay();

  }
  void CVarDef::GetValue(const CVarDef& v)
  {
    switch (group)
    {
    case VAR_DEF_GROUP_NULL:
      break;
    case VAR_DEF_GROUP_SINGLE:
      if( status == FAC_OFFSET_SET ) GetSingleValue(v,factor,offset);
      else                           GetSingleValue(v);
      break;
    default:
      break;
    }
  }
  const char *CVarDef::GetType(void)
  {
    switch (type)
    {
    default: case VAR_DEF_NULL: return "VAR_DEF_NULL"; break;               // nicht belegt
    case VAR_DEF_DOUBLE:        return "VAR_DEF_DOUBLE"; break;
    case VAR_DEF_FLOAT:        return "VAR_DEF_FLOAT"; break;
    case VAR_DEF_INT64:        return "VAR_DEF_INT64"; break;
    case VAR_DEF_UINT64:        return "VAR_DEF_UINT64"; break;
    case VAR_DEF_INT32:        return "VAR_DEF_INT32"; break;
    case VAR_DEF_UINT32:        return "VAR_DEF_UINT32"; break;
    case VAR_DEF_INT16:        return "VAR_DEF_INT16"; break;
    case VAR_DEF_UINT16:        return "VAR_DEF_UINT16"; break;
    case VAR_DEF_INT8:        return "VAR_DEF_INT8"; break;
    case VAR_DEF_UINT8:        return "VAR_DEF_UINT8"; break;

    case VAR_DEF_STRING:        return "VAR_DEF_STRING"; break;
    case VAR_DEF_SLFSTRING:        return "VAR_DEF_SLFSTRING"; break;
    case VAR_DEF_CHAR:        return "VAR_DEF_CHAR"; break;

    case VAR_DEF_ARR_DOUBLE:        return "VAR_DEF_ARR_DOUBLE"; break;
    case VAR_DEF_ARR_FLOAT:        return "VAR_DEF_ARR_FLOAT"; break;
    case VAR_DEF_ARR_INT64:        return "VAR_DEF_ARR_INT64"; break;
    case VAR_DEF_ARR_UINT64:        return "VAR_DEF_ARR_UINT64"; break;
    case VAR_DEF_ARR_INT32:        return "VAR_DEF_ARR_INT32"; break;
    case VAR_DEF_ARR_UINT32:        return "VAR_DEF_ARR_UINT32"; break;
    case VAR_DEF_ARR_INT16:        return "VAR_DEF_ARR_INT16"; break;
    case VAR_DEF_ARR_UINT16:        return "VAR_DEF_ARR_UINT16"; break;
    case VAR_DEF_ARR_INT8:        return "VAR_DEF_ARR_UINT16"; break;
    case VAR_DEF_ARR_UINT8:        return "VAR_DEF_ARR_UINT16"; break;
    case VAR_DEF_ARR_STRING:        return "VAR_DEF_ARR_STRING"; break;
    case VAR_DEF_ARR_SLFSTRING:        return "VAR_DEF_ARR_SLFSTRING"; break;

    case VAR_DEF_SLFVECD:        return "VAR_DEF_SLFVECD"; break;
    case VAR_DEF_SLFVECF:        return "VAR_DEF_SLFVECF"; break;
    case VAR_DEF_SLFVECI:        return "VAR_DEF_SLFVECI"; break;
    case VAR_DEF_SLFVECI64:        return "VAR_DEF_SLFVECI64"; break;
    case VAR_DEF_SLFVECU:        return "VAR_DEF_SLFVECU"; break;
    case VAR_DEF_SLFVECU64:        return "VAR_DEF_SLFVECU64"; break;
    case VAR_DEF_SLFMATD:        return "VAR_DEF_SLFMATD"; break;
    case VAR_DEF_SLFMATF:        return "VAR_DEF_SLFMATF"; break;
    case VAR_DEF_SLFMATI:        return "VAR_DEF_SLFMATI"; break;
    case VAR_DEF_SLFMATU:        return "VAR_DEF_SLFMATU"; break;
    case VAR_DEF_TAB1DD:        return "VAR_DEF_TAB1DD"; break;
    case VAR_DEF_TAB1DF:        return "VAR_DEF_TAB1DF"; break;
    case VAR_DEF_TAB2DD:        return "VAR_DEF_TAB2DD"; break;
    case VAR_DEF_TAB2DF:        return "VAR_DEF_TAB2DF"; break;
    case VAR_DEF_STRINGV:        return "VAR_DEF_STRINGV"; break;
    case VAR_DEF_STRINGM:        return "VAR_DEF_STRINGM"; break;
    }
  }
  void CVarDef::GetSingleValue(const CVarDef& v)
  {
    switch (type)
    {
    case VAR_DEF_DOUBLE:
      GetSingleValueDouble(v);
      break;
    case VAR_DEF_FLOAT:
      GetSingleValueFloat(v);
      break;
    case VAR_DEF_INT64:
      GetSingleValueInt64(v);
      break;
    case VAR_DEF_UINT64:
      GetSingleValueUint64(v);
      break;
    case VAR_DEF_INT32:
      GetSingleValueInt32(v);
      break;
    case VAR_DEF_UINT32:
      GetSingleValueUint32(v);
      break;
    case VAR_DEF_INT16:
      GetSingleValueInt16(v);
      break;
    case VAR_DEF_UINT16:
      GetSingleValueUint16(v);
      break;
    case VAR_DEF_INT8:
      GetSingleValueInt8(v);
      break;
    case VAR_DEF_UINT8:
      GetSingleValueUint8(v);
      break;
    }
  }
  void CVarDef::GetSingleValue(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (type)
    {
    case VAR_DEF_DOUBLE:
      GetSingleValueDouble(v,factor,offset);
      break;
    case VAR_DEF_FLOAT:
      GetSingleValueFloat(v, factor, offset);
      break;
    case VAR_DEF_INT64:
      GetSingleValueInt64(v, factor, offset);
      break;
    case VAR_DEF_UINT64:
      GetSingleValueUint64(v, factor, offset);
      break;
    case VAR_DEF_INT32:
      GetSingleValueInt32(v, factor, offset);
      break;
    case VAR_DEF_UINT32:
      GetSingleValueUint32(v, factor, offset);
      break;
    case VAR_DEF_INT16:
      GetSingleValueInt16(v, factor, offset);
      break;
    case VAR_DEF_UINT16:
      GetSingleValueUint16(v, factor, offset);
      break;
    case VAR_DEF_INT8:
      GetSingleValueInt8(v, factor, offset);
      break;
    case VAR_DEF_UINT8:
      GetSingleValueUint8(v, factor, offset);
      break;
    }
  }
  //=======================================================================
  //=======================================================================
  void CVarDef::GetSingleValueDouble(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pdouble = (double)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *pdouble = (double)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *pdouble = (double)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *pdouble = (double)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *pdouble = (double)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *pdouble = (double)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *pdouble = (double)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *pdouble = (double)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *pdouble = (double)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *pdouble = (double)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueFloat(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pfloat = (float)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *pfloat = (float)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *pfloat = (float)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *pfloat = (float)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *pfloat = (float)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *pfloat = (float)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *pfloat = (float)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *pfloat = (float)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *pfloat = (float)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *pfloat = (float)(*v.puint8);
      break;
    }

  }
  void CVarDef::GetSingleValueInt64(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint64 = (int64_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *pint64 = (int64_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *pint64 = (int64_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *pint64 = (int64_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *pint64 = (int64_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *pint64 = (int64_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *pint64 = (int64_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *pint64 = (int64_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *pint64 = (int64_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *pint64 = (int64_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueUint64(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint64 = (uint64_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *puint64 = (uint64_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *puint64 = (uint64_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *puint64 = (uint64_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *puint64 = (uint64_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *puint64 = (uint64_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *puint64 = (uint64_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *puint64 = (uint64_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *puint64 = (uint64_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *puint64 = (uint64_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueInt32(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint32 = (int32_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *pint32 = (int32_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *pint32 = (int32_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *pint32 = (int32_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *pint32 = (int32_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *pint32 = (int32_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *pint32 = (int32_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *pint32 = (int32_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *pint32 = (int32_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *pint32 = (int32_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueUint32(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint32 = (uint32_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *puint32 = (uint32_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *puint32 = (uint32_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *puint32 = (uint32_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *puint32 = (uint32_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *puint32 = (uint32_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *puint32 = (uint32_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *puint32 = (uint32_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *puint32 = (uint32_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *puint32 = (uint32_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueInt16(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint16 = (int16_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *pint16 = (int16_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *pint16 = (int16_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *pint16 = (int16_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *pint16 = (int16_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *pint16 = (int16_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *pint16 = (int16_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *pint16 = (int16_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *pint16 = (int16_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *pint16 = (int16_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueUint16(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint16 = (uint16_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *puint16 = (uint16_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *puint16 = (uint16_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *puint16 = (uint16_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *puint16 = (uint16_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *puint16 = (uint16_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *puint16 = (uint16_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *puint16 = (uint16_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *puint16 = (uint16_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *puint16 = (uint16_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueInt8(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint8 = (int8_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *pint8 = (int8_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *pint8 = (int8_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *pint8 = (int8_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *pint8 = (int8_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *pint8 = (int8_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *pint8 = (int8_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *pint8 = (int8_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *pint8 = (int8_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *pint8 = (int8_t)(*v.puint8);
      break;
    }
  }
  void CVarDef::GetSingleValueUint8(const CVarDef& v)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint8 = (uint8_t)(*v.pdouble);
      break;
    case VAR_DEF_FLOAT:
      *puint8 = (uint8_t)(*v.pfloat);
      break;
    case VAR_DEF_INT64:
      *puint8 = (uint8_t)(*v.pint64);
      break;
    case VAR_DEF_UINT64:
      *puint8 = (uint8_t)(*v.puint64);
      break;
    case VAR_DEF_INT32:
      *puint8 = (uint8_t)(*v.pint32);
      break;
    case VAR_DEF_UINT32:
      *puint8 = (uint8_t)(*v.puint32);
      break;
    case VAR_DEF_INT16:
      *puint8 = (uint8_t)(*v.pint16);
      break;
    case VAR_DEF_UINT16:
      *puint8 = (uint8_t)(*v.puint16);
      break;
    case VAR_DEF_INT8:
      *puint8 = (uint8_t)(*v.pint8);
      break;
    case VAR_DEF_UINT8:
      *puint8 = (uint8_t)(*v.puint8);
      break;
    }
  }
  //=======================================================================
  //=======================================================================
  void CVarDef::GetSingleValueDouble(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pdouble = (double)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *pdouble = (double)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *pdouble = (double)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *pdouble = (double)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *pdouble = (double)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *pdouble = (double)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *pdouble = (double)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *pdouble = (double)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *pdouble = (double)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *pdouble = (double)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueFloat(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pfloat = (float)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *pfloat = (float)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *pfloat = (float)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *pfloat = (float)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *pfloat = (float)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *pfloat = (float)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *pfloat = (float)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *pfloat = (float)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *pfloat = (float)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *pfloat = (float)(*v.puint8 * factor + offset);
      break;
    }

  }
  void CVarDef::GetSingleValueInt64(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint64 = (int64_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *pint64 = (int64_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *pint64 = (int64_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *pint64 = (int64_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *pint64 = (int64_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *pint64 = (int64_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *pint64 = (int64_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *pint64 = (int64_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *pint64 = (int64_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *pint64 = (int64_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueUint64(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint64 = (uint64_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *puint64 = (uint64_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *puint64 = (uint64_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *puint64 = (uint64_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *puint64 = (uint64_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *puint64 = (uint64_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *puint64 = (uint64_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *puint64 = (uint64_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *puint64 = (uint64_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *puint64 = (uint64_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueInt32(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint32 = (int32_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *pint32 = (int32_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *pint32 = (int32_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *pint32 = (int32_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *pint32 = (int32_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *pint32 = (int32_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *pint32 = (int32_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *pint32 = (int32_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *pint32 = (int32_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *pint32 = (int32_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueUint32(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint32 = (uint32_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *puint32 = (uint32_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *puint32 = (uint32_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *puint32 = (uint32_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *puint32 = (uint32_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *puint32 = (uint32_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *puint32 = (uint32_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *puint32 = (uint32_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *puint32 = (uint32_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *puint32 = (uint32_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueInt16(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint16 = (int16_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *pint16 = (int16_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *pint16 = (int16_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *pint16 = (int16_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *pint16 = (int16_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *pint16 = (int16_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *pint16 = (int16_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *pint16 = (int16_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *pint16 = (int16_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *pint16 = (int16_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueUint16(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint16 = (uint16_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *puint16 = (uint16_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *puint16 = (uint16_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *puint16 = (uint16_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *puint16 = (uint16_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *puint16 = (uint16_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *puint16 = (uint16_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *puint16 = (uint16_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *puint16 = (uint16_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *puint16 = (uint16_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueInt8(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *pint8 = (int8_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *pint8 = (int8_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *pint8 = (int8_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *pint8 = (int8_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *pint8 = (int8_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *pint8 = (int8_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *pint8 = (int8_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *pint8 = (int8_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *pint8 = (int8_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *pint8 = (int8_t)(*v.puint8 * factor + offset);
      break;
    }
  }
  void CVarDef::GetSingleValueUint8(const CVarDef& v, const double &factor, const double &offset)
  {
    switch (v.type)
    {
    case VAR_DEF_DOUBLE:
      *puint8 = (uint8_t)(*v.pdouble * factor + offset);
      break;
    case VAR_DEF_FLOAT:
      *puint8 = (uint8_t)(*v.pfloat * factor + offset);
      break;
    case VAR_DEF_INT64:
      *puint8 = (uint8_t)(*v.pint64 * factor + offset);
      break;
    case VAR_DEF_UINT64:
      *puint8 = (uint8_t)(*v.puint64 * factor + offset);
      break;
    case VAR_DEF_INT32:
      *puint8 = (uint8_t)(*v.pint32 * factor + offset);
      break;
    case VAR_DEF_UINT32:
      *puint8 = (uint8_t)(*v.puint32 * factor + offset);
      break;
    case VAR_DEF_INT16:
      *puint8 = (uint8_t)(*v.pint16 * factor + offset);
      break;
    case VAR_DEF_UINT16:
      *puint8 = (uint8_t)(*v.puint16 * factor + offset);
      break;
    case VAR_DEF_INT8:
      *puint8 = (uint8_t)(*v.pint8 * factor + offset);
      break;
    case VAR_DEF_UINT8:
      *puint8 = (uint8_t)(*v.puint8 * factor + offset);
      break;
    }
  }

  bool CVarDef::SetValue(const CStrM &strm)
  {
    switch (type)
    {
    case VAR_DEF_STRING:             // Einzelwert String-Klasse std::string
    case VAR_DEF_SLFSTRING:          // Einzelwert String-Klasse slf::CStr
      if ((strm.getNrows() > 0) && (strm.getNcols() > 0))
      {
        *pstring = strm.get_str(0, 0);
        return true;
      }
      break;
    case VAR_DEF_ARR_STRING:
    case VAR_DEF_ARR_SLFSTRING:
      if ((strm.getNrows() > 0) && (strm.getNcols() == 1))
      {

        for (std::size_t i = 0; (i < strm.getNrows()) && (i < ndim); ++i)
        {
          pstring[i] = strm.get_str(i, 0);
        }
        return true;
      }
      else if ((strm.getNrows() == 1) && (strm.getNcols() > 0))
      {

        for (std::size_t i = 0; (i < strm.getNcols()) && (i < ndim); ++i)
        {
          pstring[i] = strm.get_str(0, i);
        }
        return true;
      }
      else
      {
        std::size_t index = 0;
        for (std::size_t i = 0; i < strm.getNrows(); ++i)
        {
          for (std::size_t j = 0; j < strm.getNcols(); ++j)
          {
            if (index < ndim)
            {
              pstring[index] = strm.get_str(i, j);
              ++index;
            }
            else
            {
              return true;
            }
          }
        }
        return true;
      }
      break;
    case VAR_DEF_STRINGV:
      if ((strm.getNrows() > 0) && (strm.getNcols() == 1))
      {

        for (std::size_t i = 0; i < strm.getNrows(); ++i)
        {
          pstrv[i] = strm.get_str(i, 0);
        }
        return true;
      }
      else if ((strm.getNrows() == 1) && (strm.getNcols() > 0))
      {

        for (std::size_t i = 0; i < strm.getNcols(); ++i)
        {
          pstrv[i] = strm.get_str(0, i);
        }
        return true;
      }
      else
      {
        std::size_t index = 0;
        for (std::size_t i = 0; i < strm.getNrows(); ++i)
        {
          for (std::size_t j = 0; j < strm.getNcols(); ++j)
          {
            pstrv[index] = strm.get_str(i, j);
            ++index;
          }
        }
        return true;
      }
      break;
    case VAR_DEF_STRINGM:

      *pstrm = strm;
      return true;
      break;

    }
    return false;

  }
  bool    CVarDef::SetValue(const CMatrixD &mat)
  {
    switch (type)
    {
    case VAR_DEF_DOUBLE:
      if (mat.GetNdim() > 0)
      {
        *pdouble = mat[0][0];
        return true;
      }
      break;
    case VAR_DEF_FLOAT:
      if (mat.GetNdim() > 0)
      {
        *pfloat = (float)mat[0][0];
        return true;
      }
      break;
    case VAR_DEF_INT64:
      if (mat.GetNdim() > 0)
      {
        *pint64 = (int64_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_UINT64:
      if (mat.GetNdim() > 0)
      {
        *puint64 = (uint64_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_INT32:
      if (mat.GetNdim() > 0)
      {
        *pint32 = (int32_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_UINT32:
      if (mat.GetNdim() > 0)
      {
        *puint32 = (uint32_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_INT16:
      if (mat.GetNdim() > 0)
      {
        *pint16 = (int16_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_UINT16:
      if (mat.GetNdim() > 0)
      {
        *puint16 = (uint16_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_INT8:
      if (mat.GetNdim() > 0)
      {
        *pint8 = (int8_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
    case VAR_DEF_UINT8:
      if (mat.GetNdim() > 0)
      {
        *puint8 = (uint8_t)(mat[0][0] + 0.5);
        return true;
      }
      break;
      //VAR_DEF_STRING,             // Einzelwert String-Klasse std::string
      //VAR_DEF_SLFSTRING,          // Einzelwert String-Klasse slf::CStr
      //VAR_DEF_CHAR,               // String aus charater mit '\0' beendet z.B. matlab eingabe
    case VAR_DEF_ARR_DOUBLE:
    {
      std::size_t i;

      if( (mat.GetNrow() > 0) && (mat.GetNcol()==1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pdouble[i] = mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pdouble[i] = mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_FLOAT:
    {
      std::size_t i;

      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pfloat[i] = (float)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pfloat[i] = (float)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;

    case VAR_DEF_ARR_INT64:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pint64[i] = (int64_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pint64[i] = (int64_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_UINT64:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          puint64[i] = (uint64_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          puint64[i] = (uint64_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_INT32:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pint32[i] = (int32_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pint32[i] = (int32_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_UINT32:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          puint32[i] = (uint32_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          puint32[i] = (uint32_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_INT16:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pint16[i] = (int16_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pint16[i] = (int16_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_UINT16:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          puint16[i] = (uint16_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          puint16[i] = (uint16_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_INT8:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pint8[i] = (int8_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pint8[i] = (int8_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    case VAR_DEF_ARR_UINT8:
    {
      std::size_t i;
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          puint8[i] = (uint8_t)mat[i][0];
        }
        ndim = i;
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          puint8[i] = (uint8_t)mat[0][i];
        }
        ndim = i;
        return true;
      }
    }
    break;
    //VAR_DEF_ARR_STRING,         // Array String-Klasse std::string
    //VAR_DEF_ARR_SLFSTRING,      // Array String-Klasse slf::CStr
    case VAR_DEF_SLFVECD:
    {
      std::size_t i;
      pslfvecd->clear();
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pslfvecd->push_back(mat[i][0]);
        }
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pslfvecd->push_back(mat[0][i]);
        }
        return true;
      }
    }
    break;
    case VAR_DEF_SLFVECF:
    {
      std::size_t i;
      pslfvecd->clear();
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pslfvecf->push_back((float)mat[i][0]);
        }
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pslfvecf->push_back((float)mat[0][i]);
        }
        return true;
      }
    }
    break;
    case VAR_DEF_SLFVECI:
    {
      std::size_t i;
      pslfvecd->clear();
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pslfveci->push_back((int32_t)(mat[i][0] + 0.5));
        }
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pslfveci->push_back((int32_t)(mat[0][i] + 0.5));
        }
        return true;
      }
    }
    break;
    case VAR_DEF_SLFVECI64:
    {
      std::size_t i;
      pslfvecd->clear();
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pslfveci64->push_back((int64_t)(mat[i][0] + 0.5));
        }
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pslfveci64->push_back((int64_t)(mat[0][i] + 0.5));
        }
        return true;
      }
    }
    break;
    case VAR_DEF_SLFVECU:
    {
      std::size_t i;
      pslfvecd->clear();
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pslfvecu->push_back((uint32_t)(mat[i][0] + 0.5));
        }
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pslfvecu->push_back((uint32_t)(mat[0][i] + 0.5));
        }
        return true;
      }
    }
    break;
    case VAR_DEF_SLFVECU64:
    {
      std::size_t i;
      pslfvecd->clear();
      if ((mat.GetNrow() > 0) && (mat.GetNcol() == 1))
      {
        for (i = 0; (i < mat.GetNrow()) && (i < ndim); ++i)
        {
          pslfvecu64->push_back((uint64_t)(mat[i][0] + 0.5));
        }
        return true;
      }
      else if ((mat.GetNcol() > 0) && (mat.GetNrow() == 1))
      {
        for (i = 0; (i < mat.GetNcol()) && (i < ndim); ++i)
        {
          pslfvecu64->push_back((uint64_t)(mat[0][i] + 0.5));
        }
        return true;
      }
    }
    break;

    //,          // Vektor-Struktur mit slf::CSlfVec<double>
    case VAR_DEF_SLFMATD:

      *pslfmatd = mat;
      return true;
      break;
    case VAR_DEF_SLFMATF:
    {
      std::size_t i, j;
      pslfmatf->SetNdim(mat.GetNrow(), mat.GetNcol());

      for (i = 0; i < mat.GetNrow(); ++i)
      {
        for (j = 0; j < mat.GetNcol(); ++j)
        {
          pslfmatf->SetValue((float)(mat[i][0]), i, j);
        }
      }
      return true;
    }
    break;
    case VAR_DEF_SLFMATI:
    {
      std::size_t i, j;
      pslfmati->SetNdim(mat.GetNrow(), mat.GetNcol());
      for (i = 0; i < mat.GetNrow(); ++i)
      {
        for (j = 0; j < mat.GetNcol(); ++j)
        {
          pslfmati[i][j] = (int)mat[i][0];
        }
      }
      return true;
    }
    break;
    case VAR_DEF_SLFMATU:
    {
      std::size_t i, j;
      pslfmatu->SetNdim(mat.GetNrow(), mat.GetNcol());

      for (i = 0; i < mat.GetNrow(); ++i)
      {
        for (j = 0; j < mat.GetNcol(); ++j)
        {
          pslfmatu[i][j] = (unsigned int)mat[i][0];
        }
      }
      return true;
    }
    break;
    //VAR_DEF_TAB1DD,          // einfache Tabellen-Klasse CSfl1DTab<double> y = f(x)
    //VAR_DEF_TAB1DF,          // einfache Tabellen-Klasse CSfl1DTab<float> y = f(x)
    //VAR_DEF_TAB2DD,          // zweidim. Tabellen-Klasse  CSfl2DTab<double> z = f(x,y)
    //VAR_DEF_TAB2DF,          // zweidim. Tabellen-Klasse  CSfl2DTab<float> z = f(x,y)
    //VAR_DEF_STRINGV,         // String-Klasse slf::CStrV (Vektor-Klasse)
    //VAR_DEF_STRINGM,         // String-Matrix_t-Klasse slf::CStrM
    }
    return false;
  }

  CStr CVarDef::PrintValue(void)
  {
    CStr str;
    switch (type)
    {
    case VAR_DEF_DOUBLE: str = SlfPrintStringSingle<double>(*pdouble); break;
    case VAR_DEF_FLOAT:  str = SlfPrintStringSingle<float>(*pfloat); break;
    case VAR_DEF_INT64:  str = SlfPrintStringSingle<int64_t>(*pint64); break;
    case VAR_DEF_UINT64: str = SlfPrintStringSingle<uint64_t>(*puint64); break;
    case VAR_DEF_INT32:  str = SlfPrintStringSingle<int32_t>(*pint32); break;
    case VAR_DEF_UINT32: str = SlfPrintStringSingle<uint32_t>(*puint32); break;
    case VAR_DEF_INT16:  str = SlfPrintStringSingle<int16_t>(*pint16); break;
    case VAR_DEF_UINT16: str = SlfPrintStringSingle<uint16_t>(*puint16); break;
    case VAR_DEF_INT8:   str = SlfPrintStringSingle<int8_t>(*pint8); break;
    case VAR_DEF_UINT8:  str = SlfPrintStringSingle<uint8_t>(*puint8); break;
    case VAR_DEF_STRING: str = *pstring; break;
    case VAR_DEF_SLFSTRING: str = *pslfstring; break;
    case VAR_DEF_CHAR:      str = pchar; break;
    case VAR_DEF_ARR_DOUBLE: str = SlfPrintStringArray<double>(pdouble, ndim); break;
    case VAR_DEF_ARR_FLOAT: str = SlfPrintStringArray<float>(pfloat, ndim); break;
    case VAR_DEF_ARR_INT64: str = SlfPrintStringArray<int64_t>(pint64, ndim); break;
    case VAR_DEF_ARR_UINT64: str = SlfPrintStringArray<uint64_t>(puint64, ndim); break;
    case VAR_DEF_ARR_INT32: str = SlfPrintStringArray<int32_t>(pint32, ndim); break;
    case VAR_DEF_ARR_UINT32: str = SlfPrintStringArray<uint32_t>(puint32, ndim); break;
    case VAR_DEF_ARR_INT16: str = SlfPrintStringArray<int16_t>(pint16, ndim); break;
    case VAR_DEF_ARR_UINT16: str = SlfPrintStringArray<uint16_t>(puint16, ndim); break;
    case VAR_DEF_ARR_INT8: str = SlfPrintStringArray<int8_t>(pint8, ndim); break;
    case VAR_DEF_ARR_UINT8: str = SlfPrintStringArray<uint8_t>(puint8, ndim); break;
    case VAR_DEF_ARR_STRING: str = SlfPrintStringArrayString<std::string>(pstring, ndim); break;
    case VAR_DEF_ARR_SLFSTRING: str = SlfPrintStringArrayString<CStr>(pslfstring, ndim); break;
    case VAR_DEF_SLFVECD: str = SlfPrintStringVector<CVectorD>(*pslfvecd); break;
    case VAR_DEF_SLFVECF: str = SlfPrintStringVector<CVectorF>(*pslfvecf); break;
    case VAR_DEF_SLFVECI: str = SlfPrintStringVector<CVectorI>(*pslfveci); break;
    case VAR_DEF_SLFVECI64: str = SlfPrintStringVector<CVectorI64>(*pslfveci64); break;
    case VAR_DEF_SLFVECU: str = SlfPrintStringVector<CVectorU>(*pslfvecu); break;
    case VAR_DEF_SLFVECU64: str = SlfPrintStringVector<CVectorU64>(*pslfvecu64); break;
    case VAR_DEF_SLFMATD: str = SlfPrintStringMatrix<CMatrixD>(*pslfmatd); break;
    case VAR_DEF_SLFMATF: str = SlfPrintStringMatrix<CMatrixF>(*pslfmatf); break;
    case VAR_DEF_SLFMATI: str = SlfPrintStringMatrix<CMatrixI>(*pslfmati); break;
    case VAR_DEF_SLFMATU: str = SlfPrintStringMatrix<CMatrixU>(*pslfmatu); break;
    case VAR_DEF_STRINGV: str = SlfPrintStringStrV(*pstrv); break;
    case VAR_DEF_STRINGM: str = SlfPrintStringStrM(*pstrm); break;
    //VAR_DEF_TAB1DD,          // einfache Tabellen-Klasse CSfl1DTab<double> y = f(x)
    //VAR_DEF_TAB1DF,          // einfache Tabellen-Klasse CSfl1DTab<float> y = f(x)
    //VAR_DEF_TAB2DD,          // zweidim. Tabellen-Klasse  CSfl2DTab<double> z = f(x,y)
    //VAR_DEF_TAB2DF,          // zweidim. Tabellen-Klasse  CSfl2DTab<float> z = f(x,y)
    }
    return str;

  }


} // namespace slf
