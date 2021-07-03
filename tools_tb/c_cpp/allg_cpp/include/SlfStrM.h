// SlfStrM.h
#ifndef SLF_STR_M_H_INCLUDED
#define SLF_STR_M_H_INCLUDED

#include <string>
//#include <string.h>
#include "SlfBase.h"
#include "SlfStr.h"
#include "SlfStrV.h"


namespace slf
{
  // Matrix_t-String-Klasse 
  class CStrM
  {
  public:
    CStrM();
    CStrM(const char* p);
    CStrM(CStr &s);
    CStrM(std::size_t nrow, std::size_t ncol);
    ~CStrM();

    std::size_t getNrows() const { return NRow; }
    std::size_t getNcols() const { return NCol; }

    std::size_t size() const { return NRow*NCol; }

    void clear();
    void empty();

    okay_t make(std::size_t nrow, std::size_t ncol);
    okay_t cutRow(std::size_t irow);
    okay_t cutCol(std::size_t icol);

    okay_t find(const char* pstr, std::size_t *pirow, std::size_t *picol);
    okay_t find(const CStr &s, std::size_t *pirow, std::size_t *picol);

    void cat(const char *str, std::size_t irow, std::size_t icol);
    void cat(const CStr &s, std::size_t irow, std::size_t icol);

    void cpy(const char *str, std::size_t irow, std::size_t icol);
    void cpy(const CStr &s, std::size_t irow, std::size_t icol);

    const char *get_str(std::size_t irow, std::size_t icol) const;
    const CStr *get(std::size_t irow, std::size_t icol) const;
    
    CStr getval(std::size_t irow, std::size_t icol) const;
    CStrV getCol(std::size_t icol) const;
    CStrV getRow(std::size_t irow) const;
    CStrV buildVector(bool icoltype) const;
    void  setValue(const std::size_t irow,const std::size_t icol,const CStr &str);
    void transpone(void);

    CStrM& operator=(const CStrM& sm);

  private:
    std::size_t    NRow, NCol;
    std::size_t    Len;
    std::vector<std::vector<CStr>> MatStr;
  };

} // namespace
#endif
