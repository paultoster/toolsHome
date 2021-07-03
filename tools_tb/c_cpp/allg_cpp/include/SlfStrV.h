// SlfStrh
#ifndef SLF_STR_V_H_INCLUDED
#define SLF_STR_V_H_INCLUDED

#include <string>
#include <vector>
//#include <string.h>
#include "SlfBase.h"
#include "SlfStr.h"
namespace slf
{
  struct SIrowIstr
  {
    std::size_t irow;
    std::size_t istr;
  };

// Vektor-String-Klasse 
  class CStrV
  {
  public:
   CStrV();
   CStrV(const char* p);
   CStrV(const CStr &s);
   CStrV(const CStrV &strv);

    ~CStrV();

    std::size_t getNrows() const { return NRow; }

    std::size_t size() const { return NRow; }

    void clear();

    bool compare(const CStrV &strv) const;
    okay_t make(std::size_t nrow);

    // make one string
    CStr make_str(const CStrV &strv, const std::size_t &irow0, const std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1) const;
    okay_t cutRow(std::size_t irow);

    // return 1 if found or exist irow is index of vector
    // return 0 if not found
    bool_t find(const char* pstr, std::size_t *pirow) const; // find pstr or s in hole in Vector
    bool_t find(const CStr &s, std::size_t *pirow) const;
    // find pstr from irow at psition istr, if found return TRUE with new irow and istr
    bool_t findFrom(const char* pstr, std::size_t &irow, std::size_t &istr);
    bool_t findFrom(const CStr &str, std::size_t &irow, std::size_t &istr);
    // find first of strvector sv with strings from irow at position istr, 
    bool_t findFrom(const CStrV &sv, std::size_t &indsv, std::size_t &irow, std::size_t &istr) const; 
    // find pstr from irow0 at psition istr0 to row1 and istr1 if found return TRUE with new irow0 and istr0
    bool_t findFromTo(const char* pstr, std::size_t &irow0, std::size_t &istr0,const std::size_t &irow1,const std::size_t &istr1);
    bool_t findFromTo(const CStr &str, std::size_t &irow0, std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1);
    bool_t findFromTo(const CStrV &sv, std::size_t &indsv, std::size_t &irow0, std::size_t &istr0, const std::size_t &irow1, const std::size_t &istr1);
      // if found return TRUE with new irow and istr
    bool_t findAll(const char* pstr, std::vector<SIrowIstr> &svec); // find pstr all over, if found return TRUE with new struct-vector irow and istr

    //-------------------------------------------------------------------------------------
    // find ending symbol endsymbol
    // taking into acount, that if a startsymbol occurs an additional endsymbol is needed
    // that means all nested part should be taken into account
    bool findEndingSymbol(std::size_t &irow,std::size_t &istr, const char endsymbol, const char startsymbol) const;

    uint8_t exist(const char* pstr);
    uint8_t exist(const CStr &s);
    uint8_t exist_last(const char* pstr);
    uint8_t exist_last(const CStr &s);

    bool isoverend(std::size_t irow, std::size_t istr) const;

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
    bool getNextWord(std::size_t &ivec,std::size_t &istr, CStr &word, std::size_t *itype = NULL, const char *pstr = NULL, bool isvalue = false) const;

    void append(const char *str);
    void append(const CStr &s);
    void append(CStrV &sv);
    // Löscht das letzte Element raus
    void delete_last(void);
    void cat(const char *str, std::size_t irow);
    void cat(const CStr &s, std::size_t irow);

    void cpy(const char *str, std::size_t irow);
    void cpy(const CStr &s, std::size_t irow);

    // copy to s from member
    void cpy_to(CStrV &s, const std::size_t irowstart, const std::size_t istrstart, const std::size_t irowend, const std::size_t istrend);


    void insert(const char *str, std::size_t irow);
    void insert(const CStr &s, std::size_t irow);
    void insert_in_row(const char *str, std::size_t irow, std::size_t istart, std::size_t leng);
    void insert_in_row(const CStr &s, std::size_t irow, std::size_t istart, std::size_t leng);
    void cut_in_row(std::size_t irow, std::size_t istart, std::size_t leng);

    void set(const CStr &str,const std::size_t irow);
    void set(const char *pstr,const std::size_t irow);

    std::size_t lengthOf(std::size_t irow) const { if (irow < NRow) return StrVec[irow].size(); else return 0; }

    const char *get_str(std::size_t irow) const;
    const CStr *get(std::size_t irow) const;
    const char *get_last_str(void);
   CStr *get_last(void);
   CStr  get_and_cut_in_row(std::size_t irow, std::size_t istart, std::size_t leng) ;

   const CStr& operator[] (std::size_t i) const { if (i >= NRow) i = NRow - 1; return StrVec[i]; }


   CStrV& operator=(const CStrV& sv);
    bool operator==(CStrV& sv) const;
    bool operator!=(CStrV& sv) const;

    uint8_t isAnyIdentical(void);
    bool isEmpty(void) const { return NRow == 0; }

  private:
    std::size_t         NRow;
    std::vector<CStr>   StrVec;
  };

  //String-Vektor-Funktionen =================
  //==========================================
  // Splittet t mit dem Splitstring tsplit (default Leerzeichen)
  // und legt sie in strv ab Rückgabe Anzahl der Splitteile
  //
  std::size_t SlfStrVSplit(CStrV &strv, const char*t, const char *tsplit);
  std::size_t SlfStrVSplit(CStrV &strv, const CStr& str, const char *tsplit);
  std::size_t SlfStrVSplit(CStrV &strv, const char*t, const char *tsplit, const char *quot0, const char *quot1);
  // Macht split rückgängig, setzt strv zusammen mit tsplit und gibt string aus
  //
  CStr SlfStrVReSplit(const CStrV &strv, const char *tsplit);
  CStr SlfStrVReSplit(const std::vector<CStr> &strv, const char *tsplit);

} // namespace
#endif