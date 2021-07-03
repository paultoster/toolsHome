


#include "SlfStrV.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <ctype.h>

namespace slf
{
  //===========================================================
  // Vektor-String-Klasse
  //===========================================================
  CStrV::CStrV() {

    StrVec.clear();
    NRow = 0;
  }
  CStrV::CStrV(const char* p)
  {
    NRow = 1;
    StrVec.resize(1);

    StrVec[0] = p;
  }
  CStrV::CStrV(const CStr &str)
  {

    NRow = 1;
    StrVec.resize(1);

    StrVec[0] = str;
  }
  CStrV::CStrV(const CStrV &strv)
  {

    NRow = strv.getNrows();
    if (NRow > 0) {
      StrVec.resize(NRow);

      for (std::size_t i = 0; i < NRow; i++)
        StrVec[i] = strv.get_str(i);
    }
    else 
    {
      StrVec.clear();
    }
  }
  CStrV::~CStrV() 
  {
  }
  void CStrV::clear() 
  {
    StrVec.clear();
    NRow = 0;
  }
  okay_t CStrV::make(std::size_t nrow) 
  {
    StrVec.resize(nrow);

    for (std::size_t i = NRow; i < nrow; ++i)
      StrVec[i] = "";

    NRow = nrow;

    return OKAY;
  }
  // make one string
  CStr CStrV::make_str(const CStrV &strv, const std::size_t &irow0, const std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1) const
  {
    CStr str;

    for (std::size_t i = irow0; (i <= irow1) && (i < NRow); ++i)
    {
      if ((i == irow0) && (istr0<StrVec[i].size()))
      {
        if (i == irow1)
        {
          for (std::size_t j = istr0; (j <= istr1) && (j < StrVec[i].size()); ++j)
          {
            str.cat(StrVec[i].getChar(j));
          }

        }
        else
        {
          const char *p = StrVec[i].getPointer(istr0);
          str.cat(p);
        }
      }
      else if (i == irow1)
      {
        for (std::size_t j = 0; (j <= istr1) && (j < StrVec[i].size()); ++j)
        {
          str.cat(StrVec[i].getChar(j));
        }
      }
      else
      {
        str.cat(StrVec[i]);
      }
    }
    return str;
  }

  okay_t CStrV::cutRow(std::size_t irow) {


    if (irow >= NRow)
      return OKAY;

    for (std::size_t ir = irow + 1; ir < NRow; ir++) {

      StrVec[ir - 1] = StrVec[ir];
    }
    return make(NRow - 1);
  }

  uint8_t CStrV::find(const CStr &s, std::size_t *pirow) const
  {

    return find(s.c_str(), pirow);
  }
  bool_t CStrV::find(const char* pstr,std::size_t *pirow) const
  {
    //    if( *pirow >= NRow )
    //        return NOT_OKAY;

    for (std::size_t i = 0; i < NRow; i++) {

      if (StrVec[i].compareText(pstr))
      {
        *pirow = i;
        return TRUE;
      }

    }
    return FALSE;
  }
  bool_t CStrV::findFrom(const char* pstr, std::size_t &irow, std::size_t &istr)
  {
    std::size_t pos = istr;
    for (std::size_t i = irow; i < NRow; i++)
    {
      pos = StrVec[i].findText(pstr, pos);
      if ( pos != std::string::npos )
      {
        irow = i;
        istr = pos;
        return TRUE;
      }
      pos = 0;
    }
    return FALSE;
  }
  bool_t CStrV::findFrom(const CStr &str, std::size_t &irow, std::size_t &istr)
  {
    return findFrom(str.c_str(), irow, istr);
  }
  // find first of strvector with strings from irow at position istr, 
  // if found return TRUE with new irow and istr
  bool_t CStrV::findFrom(const CStrV &sv, std::size_t &indsv, std::size_t &irow, std::size_t &istr) const
  {
    std::size_t pos,posstart = istr;
    std::size_t index = std::string::npos;
    for (std::size_t i = irow; i < NRow; i++)
    {
      std::size_t posmin = std::string::npos;
      for (std::size_t j = 0; j < sv.size(); ++j)
      {
        pos = StrVec[i].findText(sv[j], posstart);
        if ((pos != std::string::npos) && (pos < posmin))
        {
          index  = j;
          posmin = pos;
        }
      }
      if (posmin != std::string::npos)
      {
        irow = i;
        istr = posmin;
        indsv = index;
        return TRUE;
      }
      
      posstart = 0;
      
    }

    return FALSE;
  }
  // find pstr from irow0 at psition istr0 to row1 and istr1 if found return TRUE with new irow0 and istr0
  bool_t CStrV::findFromTo(const char* pstr, std::size_t &irow0, std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1)
  {
    std::size_t istr = istr0;
    for (std::size_t i = irow0;(i<=irow1) && (i < NRow); i++)
    {
      istr = StrVec[i].findText(pstr, istr);

      if (i == irow1)
      {
        if (istr <= istr1)
        {
          irow0 = i;
          istr0 = istr;
          return TRUE;
        }
      }
      else
      {
        if (istr != std::string::npos)
        {
          irow0 = i;
          istr0 = istr;
          return TRUE;
        }
      }
      istr = 0;
    }
    return FALSE;
  }
  bool_t CStrV::findFromTo(const CStr &str, std::size_t &irow0,std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1)
  {
    return findFromTo(str.c_str(), irow0, istr0, irow1, istr1);
  }
  bool_t CStrV::findFromTo(const CStrV &sv, std::size_t &indsv, std::size_t &irow0, std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1)
  {
    if (findFrom(sv, indsv, irow0, istr0))
    {
      if (irow0 < irow1)
      {
        return true;
      }
      else if (irow0 == irow1)
      {
        if (istr0 <= istr1)
        {
          return true;
        }
      }
    }
    return false;
  }

  bool_t CStrV::findAll(const char* pstr, std::vector<SIrowIstr> &svec)
  {
    std::size_t irow = 0;
    std::size_t istr = 0;
    svec.clear();
    bool_t flag = FALSE;
    while(1)
    {
      if (findFrom(pstr, irow, istr) == TRUE)
      {
        SIrowIstr s;
        s.irow = irow;
        s.istr = istr;
        svec.push_back(s);
        ++istr;
        flag = TRUE;
      }
      else
      {
        break;
      }
    }
    return flag;
  }
  //-------------------------------------------------------------------------------------
  // find ending symbol endsymbol
  // taking into acount, that if a startsymbol occurs an additional endsymbol is needed
  // that means all nested part should be taken into account
  bool CStrV::findEndingSymbol(std::size_t &irow, std::size_t &istr, const char endsymbol, const char startsymbol) const
  {
    std::size_t ifoundcounter = 0;
    std::size_t istr0=istr;
    for (std::size_t i = irow ; i < NRow; ++i)
    {
      for (std::size_t j = istr0 ; j < StrVec[i].size(); ++j)
      {
        if (StrVec[i].getChar(j) == endsymbol)
        {
          if (ifoundcounter)
          {
            --ifoundcounter;
          }
          else
          {
            irow = i;
            istr = j;
            return true;
          }
        }
        else if (StrVec[i].getChar(j) == startsymbol)
        {
          ++ifoundcounter;
        }
      }
      istr0 = 0;
    }
    return false;
  }

  uint8_t CStrV::exist(const CStr &s) {

    return exist(s.c_str());
  }
  uint8_t CStrV::exist(const char* pstr) {


    for (std::size_t i = 0; i < NRow; i++) {
      if (StrVec[i].compareText(pstr)) {
        return 1;
      }

    }
    return 0;
  }
  uint8_t CStrV::exist_last(const CStr &s) {

    return exist_last(s.c_str());
  }
  uint8_t CStrV::exist_last(const char* pstr) {

    if (NRow)
      if (StrVec[NRow - 1].compareText(pstr))
        return 1;
    return 0;
  }
  bool CStrV::isoverend(std::size_t irow, std::size_t istr) const
  {
    if ((irow >= NRow)) return true;
    if (irow == (NRow - 1))
    {
      if (istr >= StrVec[irow].size()) return true;
    }
    return false;
  }
  //-----------------------------------------------------------------------------------
  // get from position ivec and istr search next word beginning with alpha and afterwords alphanumerics
  // or if iytpe != NULL and pstr != NULL next of one of characters of pstr
  // return true if word extra character found and position of next character behind word or character and word
  // if isvalue is set, only numbers like 10 or .10 or 1e-3 are searched
  // e.g.
  // CstrV strv; strv.append(" test:abcd"); strv.append("efg :abc123 = 10");size_t ivec=0 size_t istr=0; CStr word; size_t itype;
  // strv.getNextWord(ivec,istr,word) => word = "test" ivec=0 istr=5
  // strv.getNextWord(istr,word,&itype) => word = "abc" ivec=0 istr=10, itype = npos
  // strv.getNextWord(istr,word,&itype) => word = "efg" ivec=1 istr=3, itype = npos
  // strv.getNextWord(istr,word,itype,"=[:]") => word = ":" ivec = 1 istr=5, itype = 2
  // strv.getNextWord(istr,word,itype,"=[:]",true) => word = "123" ivec = 1 istr=11, itype = npos
  //------------------------------------------------------------------------------------
  bool CStrV::getNextWord(std::size_t &ivec, std::size_t &istr, CStr &word, std::size_t *pitype /*= NULL*/, const char *pstr /* = NULL*/, bool isvalue /* = false */) const
  {
    if (ivec < NRow)
    {
      std::size_t j = istr;
      for (std::size_t i = ivec; i < NRow; ++i)
      {
        if (i != ivec)
        {
          j = 0;
        }
        if (StrVec[i].getNextWord(j, word, pitype, pstr,isvalue))
        {
          istr = j;
          ivec = i;
          return true;
        }
      }

    }
    return false;
  }

  void CStrV::append(CStrV &sv) {


    for (std::size_t i = 0; i < sv.getNrows(); i++)
      append(sv.get_str(i));

  }

  void CStrV::append(const CStr &s) {
    append(s.c_str());
  }
  void CStrV::append(const char *str) {

    make(NRow + 1);

    StrVec[NRow - 1] = str;
  }
  void CStrV::delete_last(void) {

    if (NRow > 0)
      make(NRow - 1);
  }
  void CStrV::cat(const CStr &s, std::size_t irow) {
    cat(s.c_str(), irow);
  }
  void CStrV::cat(const char *str, std::size_t irow) {

    if (irow >= NRow) {
      make(irow + 1);
    }

    StrVec[irow].append(str);
  }
  void CStrV::cpy(const CStr &s, std::size_t irow) {
    cat(s.c_str(), irow);
  }
  void CStrV::cpy(const char *str, std::size_t irow) {

    if (irow >= NRow) {
      make(irow + 1);
    }

    StrVec[irow] = str;
  }
  void CStrV::cpy_to(CStrV &s, const std::size_t irowstart, const std::size_t istrstart, const std::size_t irowend, const std::size_t istrend)
  {
    std::size_t istr0 = istrstart;
    std::size_t istr1 = istrend;

    for (std::size_t i = irowstart; (i <= irowend) && (i <NRow); ++i)
    {
      const char *p = StrVec[i].getPointer(istr0);

      if (istr0 >= StrVec[i].size())
      {
        s.append("");
      }
      else
      {
        if (i == irowend)
        {
          CStr t;
          for (std::size_t j = istr0; (j <= istr1) && (j < StrVec[i].size()); ++j)
          {
            t.add(StrVec[i].getChar(j));
          }
          s.append(t);
        }
        else
        {
          s.append(p);
        }
      }
      istr0 = 0;
    }
  }

  void CStrV::insert(const CStr &s, std::size_t irow) {
    insert(s.c_str(), irow);
  }
  void CStrV::insert(const char *str, std::size_t irow) {

    std::size_t i;
    make(NRow + 1);
    if (irow < NRow)
    {
      for (i = NRow - 1; i > irow; i--)
      {
        StrVec[i] = StrVec[i - 1];
      }
      StrVec[irow] = str;
    }
  }
  void CStrV::insert_in_row(const CStr &s, std::size_t irow, std::size_t istart, std::size_t leng)
  {
    insert_in_row(s.c_str(), irow, istart, leng);
  }
  void CStrV::insert_in_row(const char *str, std::size_t irow, std::size_t istart, std::size_t leng)
  {
    if (irow < NRow)
    {
      CStr s = str;
      if (s.size() > leng) s.cut(leng, std::string::npos);
      StrVec[irow].insert(istart, s);
    }
  }

  void CStrV::cut_in_row(std::size_t irow, std::size_t istart, std::size_t leng)
  {
    if (irow < NRow)
    {
      StrVec[irow].cut(istart, leng);
    }
  }
  void CStrV::set(const CStr &str, const std::size_t irow)
  {
    if (irow >= NRow) make(irow + 1);

    StrVec[irow] = str;
  }
  void CStrV::set(const char *pstr, const std::size_t irow)
  {
    if (irow >= NRow) make(irow + 1);

    StrVec[irow] = pstr;
  }
  const char *CStrV::get_str(std::size_t irow) const {

    if (irow >= NRow)
      return 0;
    else
      return StrVec[irow].c_str();
  }
  const CStr *CStrV::get(std::size_t irow) const {

    if (irow >= NRow)
      return 0;
    else
      return &StrVec[irow];
  }
  const char *CStrV::get_last_str(void) {

    if (NRow == 0)
    {
      return 0;
    }
    else
    {
      return StrVec[NRow - 1].c_str();
    }
  }
  CStr *CStrV::get_last(void) {

    if (NRow == 0)
    {
      return 0;
    }
    else
    {
      return &StrVec[NRow - 1];
    }
  }
  CStr CStrV::get_and_cut_in_row(std::size_t irow, std::size_t istart, std::size_t leng) 
  {
    CStr str;
    if (irow < NRow)
    {
      str = SlfStrExtract(StrVec[irow], istart, leng);
      StrVec[irow].cut(istart, leng);
    }
    return str;
  }
  CStrV& CStrV::operator=(const CStrV& sv)
  {
    clear();

    if (sv.getNrows() > 0) {

      for (std::size_t i = 0; i < sv.getNrows(); i++) {
        append(sv.get_str(i));
      }
    }
    return *this;                 // return *this
  }
  bool CStrV::compare(const CStrV &sv) const
  {
    if (sv.getNrows() != NRow) return false;

    for (std::size_t i = 0; i < sv.getNrows(); i++) {

      if (!StrVec[i].compareText(sv.get_str(i)))
        return false;
    }

    return true;
  }
  bool CStrV::operator==(CStrV &sv) const
  {
    if (sv.getNrows() != NRow) return false;

    for (std::size_t i = 0; i < sv.getNrows(); i++) {

      if (!StrVec[i].compareText(sv.get_str(i)))
        return false;
    }

    return true;
  }
  bool CStrV::operator!=(CStrV &sv) const
  {
    if (sv.getNrows() != NRow) return true;

    for (std::size_t i = 0; i < sv.getNrows(); i++) {

      if (!StrVec[i].compareText(sv.get_str(i)))
        return true;
    }

    return false;
  }
  uint8_t CStrV::isAnyIdentical(void) {

    for (std::size_t i = 0; i < NRow; i++) {

      for (std::size_t j = 0; j < NRow; j++) {

        if (i != j && StrVec[i].compareText(StrVec[j]))
          return 1;
      }
    }

    return 0;
  }

  //String-Vektor-Funktionen =================
  //==========================================
  std::size_t SlfStrVSplit(CStrV &strv, const CStr& str, const char *tsplit)
  {
    return SlfStrVSplit(strv, str.c_str(), tsplit);
  }
  std::size_t SlfStrVSplit(CStrV &strv, const char*t, const char *tsplit/*=" "*/) {

    std::size_t i0 = 0;
    std::size_t i1;
    std::size_t len = SlfStrLen(t);
    std::size_t lents = SlfStrLen(tsplit);

    CStr str;

    strv.clear();

    while (SlfStrFind(t, tsplit, "vs", i0) == 0)
      i0 += lents;

    while (i0 < len) {

      if ((i1 = SlfStrFind(t, tsplit, "vs", i0)) != SLF_STR_NPOS) {

        SlfStrCpy(str, t, i0, i1 - i0);

        strv.append(str);

        i0 = i1 + lents;
      }
      else {

        if (i0 < len) {
          SlfStrCpy(str, t, i0, len - i0);
          strv.append(str);
        }
        i0 = len;
      }
    }

    return strv.getNrows();
  }
  std::size_t SlfStrVSplit(CStrV &strv, const char*t, const char *tsplit, const char *quot0, const char *quot1) {

    std::size_t i0 = 0;
    std::size_t i1;
    std::size_t len = SlfStrLen(t);
    std::size_t lents = SlfStrLen(tsplit);

    CStr str;

    strv.clear();


    while (SlfStrFind(t, tsplit, "vs", quot0, quot1, "a", 0, i0) == 0)
      i0 += lents;

    while (i0 < len) {

      if ((i1 = SlfStrFind(t, tsplit, "vs", quot0, quot1, "a", 0, i0)) != SLF_STR_NPOS) {

        SlfStrCpy(str, t, i0, i1 - i0);

        strv.append(str);

        i0 = i1 + lents;
      }
      else {

        if (i0 < len) {
          SlfStrCpy(str, t, i0, len - i0);
          strv.append(str);
        }
        i0 = len;
      }
    }

    return strv.getNrows();
  }
  CStr SlfStrVReSplit(const CStrV &strv, const char *tsplit)
  {
    CStr str;

    for (std::size_t i = 0; i < strv.size(); ++i)
    {
      str.append(strv[i]);

      if (i + 1 < strv.size())
      {
        str.append(tsplit);
      }
    }
    return str;
  }
  CStr SlfStrVReSplit(const std::vector<CStr> &strv, const char *tsplit)
  {
    CStr str;

    for (std::size_t i = 0; i < strv.size(); ++i)
    {
      str.append(strv[i]);

      if (i + 1 < strv.size())
      {
        str.append(tsplit);
      }
    }
    return str;
  }
} // namespace