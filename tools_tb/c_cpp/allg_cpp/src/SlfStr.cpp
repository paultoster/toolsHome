//===================================================================
// noch zu erledigen:
// - Die Funktion compare wird bisher mit bool true zurückgegeben,
//   jetzt mit == 0 zu vegleichen, d.h zu ändern in:
//SlfParData.cpp        	D:\cprog\Allg_C\source                  	35 KB   	27.02.2012 08:42  	12.08.2012 14:22  	C++ Source  	  
//SlfParRead.cpp        	D:\cprog\Allg_C\source                  	34 KB   	27.02.2012 08:42  	12.08.2012 14:22  	C++ Source  	  
//SlfStr.cpp            	D:\cprog\Allg_C\source                  	89 KB   	13.02.2012 21:45  	10.01.2009 14:22  	C++ Source  	  
//DsModSonst.cpp        	D:\cprog\DSModels\source                	30 KB   	14.07.2010 16:08  	14.12.2009 20:51  	C++ Source  	  
//DsParMex.cpp          	D:\cprog\DSModels\source                	41 KB   	06.01.2012 16:57  	10.01.2009 14:19  	C++ Source  	  
//IntegratorLSODA.cpp   	D:\cprog\DSModels\source                	176 KB  	02.10.2009 15:34  	14.12.2009 20:52  	C++ Source  	  
//IntegratorLSODA2.cpp  	D:\cprog\DSModels\source                	60 KB   	02.10.2009 15:34  	14.12.2009 20:52  	C++ Source  	  
// - replace muss ersetzt werden
//   SlfStr.cpp            	D:\cprog\Allg_C\source                           	89 KB   	13.02.2012 21:45  	10.01.2009 14:22  	C++ Source  	                                       
//IntegratorLSODA.cpp   	D:\cprog\DSModels\source                         	176 KB  	02.10.2009 15:34  	14.12.2009 20:52  	C++ Source  	                                       
//IntegratorLSODA3.cpp  	D:\cprog\DSModels\source                         	49 KB   	29.11.2008 15:01  	14.12.2009 20:52  	C++ Source  	                                       
//IniFile.cpp           	D:\cprog\FzgSim\source                           	19 KB   	01.08.2012 22:17  	01.08.2012 22:07  	C++ Source  	                                       
//
// - insert => insertText()
//SlfParData.cpp           	D:\cprog\Allg_C\source                          	35 KB  	27.02.2012 08:42  	12.08.2012 14:22  	C++ Source  	  
//SlfStr.cpp               	D:\cprog\Allg_C\source                          	89 KB  	13.02.2012 21:45  	10.01.2009 14:22  	C++ Source  	  
//DsCtrlIO_mod.cpp         	D:\cprog\DSModels\source\not_used               	61 KB  	05.11.2008 13:52  	14.12.2009 20:52  	C++ Source  	  
//
// - substr
//SlfStr.cpp           	D:\cprog\Allg_C\source                   	89 KB  	13.02.2012 21:45  	10.01.2009 14:22  	C++ Source  	  
//SlfMes.cpp           	D:\cprog\Allg_C\source\old               	14 KB  	10.12.2011 21:56  	10.01.2009 14:22  	C++ Source  	  
//spar.cpp             	D:\cprog\baustelle\simu1\old\source\sim  	15 KB  	19.11.2004 10:43  	12.08.2012 14:23  	C++ Source  	  
//IniFile.cpp          	D:\cprog\FzgSim\source                   	19 KB  	01.08.2012 22:17  	01.08.2012 22:07  	C++ Source  	  
//mexReadCanAscii.cpp  	D:\cprog\mexReadCanAscii                 	25 KB  	13.12.2010 13:58  	12.08.2012 14:25  	C++ Source  	  

#include "SlfStr.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <ctype.h>
#include <sstream>

namespace slf
{ 

//===================================================================


#define SLF_FORMAT_DEFAULT_LENGTH 25

static int maxExponent = 511;	/* Largest possible base 10 exponent.  Any
				 * exponent larger than this will already
				 * produce underflow or overflow, so there's
				 * no need to worry about additional digits.
				 */
static double powersOf10[] = {	/* Table giving binary powers of 10.  Entry */
    10.,			/* is 10^2^i.  Used to convert decimal */
    100.,			/* exponents into floating-point numbers. */
    1.0e4,
    1.0e8,
    1.0e16,
    1.0e32,
    1.0e64,
    1.0e128,
    1.0e256
};


struct SSlfStrFormatListe {

  char           *ptype;
  std::size_t         width;
  CStr        text;
  CStr        format;
  SSlfStrFormatListe   *pnext;
};

okay_t SlfStrSetFormatListe(char *pformat
                             ,SSlfStrFormatListe **ppformat_liste
                             ,std::size_t *n_format_liste);
void SlfStrFindQuoted(const char *txt      // Text der zu durchkämmen
                     ,const char *txtq0    // Start-Quot
                     ,const char *txtq1    // End-Quot
                     ,const char *suchregel// "i": innerhalb quot mit einsen belegen
                                           // "a": außerhalb quot mit -"-
                     ,std::size_t pos0          // Start, wo gesucht wird
                     ,std::size_t len0          // Länge, des Stücks zum Suchen
                     ,uint8_t  *pmarker      // VEktor mit länge len0
                     ,char   istatus       // 0:startet ausserhalb quot
                                           // 1:startet innerhalb quot
                     );

#if 0
CStr::CStr()
: len(0), str(0), npos(MAX_UINT32)
{}
#endif
CStr::CStr() {
  npos = std::string::npos;
}
CStr::CStr(const CStr& s)
{
	this->assign(s.c_str());
  npos = std::string::npos;
}
CStr::CStr(const std::string& s)
{
	this->assign(s.c_str());
  npos = std::string::npos;
}
CStr::CStr(const char* p) 
{
	this->assign(p);
  npos = std::string::npos;
}
CStr::CStr(std::size_t l, char fillChar)
{
  this->resize(l);
  SlfCharSet(this->c_str(), fillChar, l);
  npos = std::string::npos;
}

CStr::CStr(std::size_t l)
{
  std::stringstream s;
  s << l;
	this->assign(s.str());
  npos = std::string::npos;
}


CStr::~CStr()
{
}
CStr& CStr::operator=(const CStr& s)
{
  if (this == &s) return *this; // "a1 = a1"

	this->assign(s);
  //this->len = (std::size_t)this->size();
  return *this;                 // return *this
}
CStr& CStr::operator=(const std::string& s)
{
  if (this == &s) return *this; // "a1 = a1"

	this->assign(s);
  //this->len = (std::size_t)this->size();
  return *this;                 // return *this
}
CStr& CStr::operator=(const char *p)
{
	this->assign(p);
  //this->len = (std::size_t)this->size();
  return *this;
}
CStr& CStr::operator+=(const char *pcat)
{
	this->append(pcat);
  //this->len = (std::size_t)this->size();
  return *this;
}
CStr& CStr::operator+=(const CStr &s)
{
	this->append(s);
  //this->len = (std::size_t)this->size();
  return *this;
}
CStr& CStr::operator+=(const std::string &s)
{
	this->append(s);
  //this->len = (std::size_t)this->size();
  return *this;
}
CStr&  CStr::add(const char u)
{
  char   t[2];

	t[0] = u;
	t[1] = '\0';

	this->append(t);
  //this->len = (std::size_t)this->size();
  return *this;
}
char CStr::getChar(std::size_t pos) const {

	const char *pc     = this->c_str();
	std::size_t len = (std::size_t)this->size();
	if( len == 0 )
	{
		return 0;
	}
  else if( pos+1 > len )
	{
		return pc[len-1];
	}
	else
	{
		return pc[pos];
	}
}
CStr  CStr::getString(std::size_t pos, std::size_t len) const
{
  const char *pc = this->c_str();
  std::size_t l  = (std::size_t)this->size();
  CStr retstr;
  if (pos < l)
  {
    if (pos + len > l)
    {
      len = l - pos;
    }
    for (l = pos; l < pos + len; ++l)
    {
      retstr.add(pc[l]);
    }
  }
  return retstr;
}
const char *CStr::getPointer(std::size_t pos) const
{
  std::size_t len = (std::size_t)this->size();
  const char *pstr = this->c_str();
  if (len == 0)
  {
    const char *pc = NULL;
    return pc;
  }
  else if (pos + 1 > len)
  {
    const char *pc = &pstr[len - 1];
    return pc;
  }
  else
  {
    const char *pc = &pstr[pos];
    return pc;
  }

}
okay_t CStr::insertText(char *textins,std::size_t pos0) {
    return CStr::insertText(textins,pos0,SlfStrLen(textins));
}
okay_t CStr::insertText(char *textins,std::size_t pos0, std::size_t len0) {

    char  *str1;
		const char *str = this->c_str();
    std::size_t i;

    if( pos0 >= this->size() )
        pos0 = this->size();

    if( len0 > SlfStrLen(textins) )
        len0 = SlfStrLen(textins);

    str1 = new char[this->size() +len0+1];
    
    for(i=0;i<pos0;i++)
        str1[i] = str[i];

    for(i=0;i<len0;i++)
        str1[pos0+i] = textins[i];

    for(i=pos0;i<this->size();i++)
        str1[len0+i] = str[i];

    str1[this->size() +len0] = '\0';

		this->assign(str1);

		delete []str1;

    return OKAY;
}
CStr& CStr::cut(std::size_t pos0) 
{
  return cut(pos0, std::string::npos);
}
CStr &CStr::cut(std::size_t pos0, std::size_t len0) {
  
	CStr &s = *this;

  if (pos0 >= s.size()) return s;

  if (pos0 + len0 >= s.size())
  {
    len0 = s.size() - pos0;
  }
	s.replace(pos0,len0,"");
  return s;
}
CStr& CStr::replaceText(const CStr &s, std::size_t pos0, std::size_t len0) {

	return CStr::replaceText(s.c_str(),pos0,len0);
}
CStr& CStr::replaceText(const char *pstr, std::size_t pos0, std::size_t len0) {
  std::size_t len1;

  // pos0 geht über die Länge des Strings hinaus
  if( pos0 > this->size())
    return *this;
  if( pos0+len0 > this->size())
      len0 = this->size() -pos0;
  
  // Erster Teil aus String, der bestehen bleibt 
  len1 = pos0;  
  // Zweiter Teil aus dem einzufügenden String
  len1 += SlfStrLen(pstr);
  // Dritter Teil aus dem Rest des Strings
  len1 += this->size() -pos0-len0;

  char *p = new char[len1+1];
  
  // Erster Teil
  std::size_t i=0;
  while(i<pos0){
    p[i] = *(this->c_str()+i);
    i++;
  };
  p[i] = (char)0;
  // Zweiter Teil
  SlfStrCat(p,len1+1,pstr);
  // Dritter Teil
  if( pos0+len0 < this->size())
	  SlfStrCat(p,len1+1,this->c_str()+pos0+len0);

  this->assign(p);

  return *this;

}
okay_t CStr::change(char *textsuch,char *textchange) {

  return SlfStrChange(*this,textsuch,textchange);
}
okay_t CStr::change(char *textsuch, char *textchange, std::size_t pos0, std::size_t len0)
{
  return SlfStrChange(*this, textsuch, textchange,pos0,len0);
}
okay_t CStr::elimAnf(const char *elim_string/*=" "*/) {
  return SlfStrElimAnf(*this,elim_string);
}
okay_t CStr::elimEnd(const char *elim_string/*=" "*/) {
  return SlfStrElimEnd(*this,elim_string);
}
okay_t CStr::elimAnfEnd(const char *elim_string/*=" "*/) {
  return SlfStrElimAnfEnd(*this,elim_string);
}
okay_t CStr::elimAnfC(const char *elim_string/*=" "*/) {
  return SlfStrElimAnfC(*this,elim_string);
}
okay_t CStr::elimEndC(const char *elim_string/*=" "*/) {
  return SlfStrElimEndC(*this,elim_string);
}
okay_t CStr::elimAnfEndC(const char *elim_string/*=" "*/) {
  return SlfStrElimAnfEndC(*this,elim_string);
}
okay_t CStr::elim(const char *elim_string/*=" "*/) {
  return SlfStrElim(*this,elim_string);
}
void CStr::cat(const CStr &s) {

    this->append(s);
}
void CStr::cat(const char *pstr) {

    this->append(pstr);
}
void CStr::cat(const char t) {

  this->append(1,t);
}
void CStr::cat(const char *pstr,std::size_t pos0)             {cat(pstr,pos0,SlfStrLen(pstr));}
void CStr::cat(const char *pstr,std::size_t pos0,std::size_t len0) 
{

  CStr s = pstr;
  cat(s, pos0, len0);
}
void CStr::cat(const CStr &s, std::size_t pos0)             { cat(s, pos0, s.size()); }
void CStr::cat(const CStr &s, std::size_t pos0, std::size_t len0)
{ 
  if (s.size() < pos0) return; // ist leer oder nicht im Bereich: return

  if (pos0 + len0 - 1 > s.size())
    len0 = (s.size() + 1) - pos0;

  this->append(s, pos0, len0);
}
okay_t CStr::catFormat(char *pformat,...) {
    
    SSlfStrFormatListe *pformat_liste=0;
    SSlfStrFormatListe *p_f_l,*p_f_l1;
    std::size_t      n_format_liste;
    CStr  format_text;
    
    va_list             marker;
    int                 ivalue;
    double              dvalue;
    int                 *p_ivalue;
    void                *p_void;
    char *              p_string;
    long int            livalue;
    long unsigned int   luivalue;
    short int           sivalue;
    short unsigned int  suivalue;
        
    if( SlfStrSetFormatListe(pformat,&pformat_liste,&n_format_liste) == NOT_OKAY ) {
        printf("Error in <CStr::catFormat> aufgetreten\n");
        return NOT_OKAY;
    }

    /* Variable Liste initialisieren */
    va_start(marker,pformat);

    p_f_l = pformat_liste;
    while(p_f_l != 0) {
    
        /* nicht formatierten Text zuerst anhängen */
        this->append(p_f_l->text);    
    
        /* formatierte Eingabe format_text erzeugen */
        /* altes löschen */
        format_text.clear();
    
        /* formatierte Eingabe übergeben */
        if( p_f_l->ptype != NULL ) {
        
            if( (strcmp(p_f_l->ptype,"c") == 0) ) {
            
                ivalue = va_arg( marker, int );
            
                if( ivalue > 31 ) {
                  
                  std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                  //sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                  sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#else
                  sprintf(p,p_f_l->format.c_str(),ivalue);
#endif
                  format_text.append(p);
                  delete  [] p;
                
                }
            } else if(  (strcmp(p_f_l->ptype,"d") == 0) 
                     || (strcmp(p_f_l->ptype,"i") == 0)
                     || (strcmp(p_f_l->ptype,"o") == 0) 
                     || (strcmp(p_f_l->ptype,"u") == 0) 
                     || (strcmp(p_f_l->ptype,"x") == 0) 
                     || (strcmp(p_f_l->ptype,"X") == 0) 
                     ) {

                ivalue = va_arg( marker, int );

                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#else
                sprintf(p,p_f_l->format.c_str(),ivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"e") == 0) 
                     || (strcmp(p_f_l->ptype,"E") == 0) 
                     || (strcmp(p_f_l->ptype,"f") == 0) 
                     || (strcmp(p_f_l->ptype,"g") == 0) 
                     || (strcmp(p_f_l->ptype,"G") == 0) 
                     ) {
                dvalue = va_arg( marker, double );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),dvalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),dvalue);
#else
                sprintf(p,p_f_l->format.c_str(),dvalue);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if(  (strcmp(p_f_l->ptype,"n") == 0) 
                ) {
                p_ivalue = va_arg( marker, int* );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_ivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),p_ivalue);
#else
                sprintf(p,p_f_l->format.c_str(),p_ivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"p") == 0) 
                ) {
                p_void = va_arg( marker, void* );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_void);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),p_void);
#else
                sprintf(p,p_f_l->format.c_str(),p_void);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if(  (strcmp(p_f_l->ptype,"s") == 0)
                ) {
                p_string = va_arg( marker, char* );
            
                if( p_string != NULL ) {
                      std::size_t  ll = SLF_MAX(
                                       (std::size_t)SlfStrLen(p_string)
                                      ,p_f_l->width
                                      )+1;
                      char *p = new char[ll];
                    //sprintf_s(p,ll,p_f_l->format.c_str(),p_string);
#if _MSC_VER > MSC_VER_BIS_VS2005
                     sprintf_s(p,ll,p_f_l->format.c_str(),p_string);
#else
                     sprintf(p,p_f_l->format.c_str(),p_string);
#endif
                    format_text.append(p);
                    delete  [] p;
                }
                        
            } else if( (strcmp(p_f_l->ptype,"lu") == 0) ) {
            
                luivalue = va_arg( marker, long unsigned int );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),luivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),luivalue);
#else
                sprintf(p,p_f_l->format.c_str(),luivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"ld") == 0) 
                || (strcmp(p_f_l->ptype,"li") == 0) 
                || (strcmp(p_f_l->ptype,"lo") == 0) 
                || (strcmp(p_f_l->ptype,"lx") == 0) 
                || (strcmp(p_f_l->ptype,"lX") == 0) 
                ) {
                livalue = va_arg( marker, long int );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),livalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),livalue);
#else
                sprintf(p,p_f_l->format.c_str(),livalue);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if( (strcmp(p_f_l->ptype,"hu") == 0) ) {
            
                suivalue = va_arg( marker, short unsigned int );

                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),suivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),suivalue);
#else
                sprintf(p,p_f_l->format.c_str(),suivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            
            } else if(  (strcmp(p_f_l->ptype,"hd") == 0) 
                || (strcmp(p_f_l->ptype,"hi") == 0) 
                || (strcmp(p_f_l->ptype,"ho") == 0) 
                || (strcmp(p_f_l->ptype,"hx") == 0) 
                || (strcmp(p_f_l->ptype,"hX") == 0) 
                ) {
                sivalue = va_arg( marker, short int );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),sivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),sivalue);
#else
                sprintf(p,p_f_l->format.c_str(),sivalue);
#endif
                format_text.append(p);
                delete  [] p;
                        
            }
        }
        /* ps_text anhängen */
        this->append(format_text);

        /* nächstes Element verkettete Liste */
        p_f_l = p_f_l->pnext;
    }
    /* variable Liste beenden */
    va_end(marker);

    /* Speicher freigeben */
    p_f_l = pformat_liste;
    while(p_f_l != 0) {

      
        /* nächstes Element verkettete Liste */
        p_f_l1 = p_f_l->pnext;

        /* Struktur löschen */
        delete p_f_l;

        p_f_l = p_f_l1;


    }
    return OKAY;
}

std::size_t CStr::findText(const char c) {

  return CStr::findText(c,0);
}

std::size_t CStr::findText(const char c, std::size_t pos) {

  std::size_t i;

  if( pos >= this->size())
    return CStr::npos;

  for(i=pos;i<this->size();i++) {
    if( *(this->c_str()+i) == c )
        return i;
  }
  return CStr::npos;
}
std::size_t CStr::findText(const char* pstr) {

  return CStr::findText(pstr,0, this->size());
}

std::size_t CStr::findText(const char* pstr, std::size_t pos) 
{
  std::size_t ll = this->size();

  if (ll > pos) ll -= pos;
  return CStr::findText(pstr, pos, ll);

  //char   *p1 = new char[SlfStrLen(pstr)+1];
  //char   *p2;
  //std::size_t i0,i;

  //len = this->size();

  //if(  (pos >= len)
  //  || (SlfStrLen(pstr) > len - pos)
  //  ){
  //  delete [] p1;
  //  return CStr::npos;
  //}

  //SlfStrCpy(p1,SlfStrLen(pstr)+1,pstr);
  //p2 = p1;
  //i0 = pos;
  //for(i=pos;i<len;i++) {
  //  if( *(this->c_str()+i) == *p1 ) {
  //    p1++;
  //    if( *p1 == (char)0 ){
  //      delete [] p2;
  //      return i0;
  //    }
  //  } else {
  //    i0 =i+1;
  //    p1 = p2;
  //  }
  //}
  //delete [] p2;
  //return CStr::npos;
}
std::size_t CStr::findText(const char* pstr, std::size_t pos0, std::size_t len0) {

    return SlfStrFind(this->c_str(),pstr,"vs",pos0,len0);
}

std::size_t CStr::findText(const CStr &s) const {

  return CStr::findText(s.c_str(),0,this->size());
}
std::size_t CStr::findText(const CStr &s, std::size_t pos) const {

  std::size_t ll = this->size();

  if (ll > pos) ll -= pos;
  return CStr::findText(s.c_str(),pos, ll);
}
std::size_t CStr::findText(const CStr &s, std::size_t pos0, std::size_t len0) const {

    return SlfStrFind(this->c_str(), s.c_str(), "vs", pos0, len0);
    //CStr::findText(s.c_str(),pos0,len0);
}

std::size_t CStr::findText(const char *txtsuch
                          ,const char *suchregel
                          ,const char *txtquot0
                          ,const char *txtquot1
                          ,const char *quotregel
                          ,char istatus) {
    return SlfStrFind(this->c_str(),txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
}
std::size_t CStr::findText(const CStr &s
                          ,const char *suchregel
                          ,const char *txtquot0
                          ,const char *txtquot1
                          ,const char *quotregel
                          ,char istatus) {

    return SlfStrFind(this->c_str(),s.c_str(),suchregel,txtquot0,txtquot1,quotregel,istatus);
}
std::size_t CStr::findText(CStr &s
                          ,const char *txtquot0
                          ,const char *txtquot1
                          ,const char *suchregel) {

	return SlfStrFindText(s,this->c_str(),txtquot0,txtquot1,suchregel);

}
std::size_t CStr::findNot(const char c) {

  return CStr::findNot(c,0);
}

std::size_t CStr::findNot(const char c, std::size_t pos) {

  std::size_t i;
	const char *str = this->c_str();

  if( pos >= this->size())
    return CStr::npos;

  for(i=pos;i<this->size();i++) {
    if( *(str+i) != c )
        return i;
  }
  return CStr::npos;
}
std::size_t CStr::findNot(const char* pstr) {

  return CStr::findNot(pstr,0);
}

std::size_t CStr::findNot(const char* pstr, std::size_t pos) {

  char   *p1 = new char[SlfStrLen(pstr)+1];
  std::size_t i0,i,j;
	const char *str = this->c_str();

  if(  (pos >= this->size())
    || (SlfStrLen(pstr) > this->size() - pos)
    ){
    delete [] p1;
    return CStr::npos;
  }

  SlfStrCpy(p1,SlfStrLen(pstr)+1,pstr);
  i0 = pos;
  for(i=pos;i<this->size();i++) {
    for(j=0;j<SlfStrLen(p1);j++) {
      if( *(str+i) != *(p1+j) ) {
        delete [] p1;
        return i0;
      }
    }
    i0 =i+1;
  }
  delete [] p1;
  return CStr::npos;
}
std::size_t CStr::findNot(const CStr &s) {

  return CStr::findNot(s.c_str(),0);
}
std::size_t CStr::findNot(const CStr &s, std::size_t pos) {

  return CStr::findNot(s.c_str(),pos);
}
//-----------------------------------------------------------------------------------
// get from position istr search next word beginning with alpha and afterwords alphanumerics
// or if iytpe != NULL and pstr != NULL next of one of characters of pstr
// return true if word extra character found and position of next character behind word or character and word
// if isvalue is set, only numbers like 10 or .10 or 1e-3 are searched
// e.g.
// Cstr str = "  abc123 = efg : re10; size_t istr=0; CStr word; size_t itype;
// str.getNextWord(istr,word) => word = "abc123" istr=8
// str.getNextWord(istr,word,&itype) => word = "efg" istr=14, itype = npos
// str.getNextWord(istr,word,itype,"=[:]") => word = ":" istr=16, itype = 2
// str.getNextWord(istr,word,itype,"=[:]",true) => word = "10" istr=21, itype = npos
//------------------------------------------------------------------------------------
bool CStr::getNextWord(std::size_t &istr, CStr &word, std::size_t *pitype /*= NULL*/, const char *ptypes /*= NULL*/, bool isvalue /* = false */) const
{
  std::size_t len = (std::size_t)this->size();
  const char *pstr = this->c_str();
  std::size_t i=0;
  word.clear();

  // if position istr is behind end of string return false
  if (istr >= len) return false;

  bool flag_word = false;
  bool flag_found = false;
  for (i = istr; i < len;++i)
  {
    if (isalpha(pstr[i]))
    {
      flag_word = true;
      flag_found = true;
      if (pitype != NULL) *pitype = std::string::npos;
      word.add(pstr[i]);

      break;
    }
    else if (ptypes != NULL)
    {
      for (std::size_t j = 0; j < strlen(ptypes); ++j)
      {
        if (ptypes[j] == pstr[i])
        {
          if (pitype != NULL) *pitype = j;
          flag_found = true;
          word.add(pstr[i]);
          goto set;
        }
      }
    }
  }
set:
  if (flag_found)
  {
    if (flag_word)
    {
      while((++i < len) && isalnum(pstr[i]) )
      {
        word.add(pstr[i]);
      }
      istr = i;
    }
    else
    {
      istr = i + 1;
    }
    return true;
  }
  
  return false;
}

CStr& CStr::formatLeft(std::size_t lformat, char * fil/*=" "*/) {

  if( SlfStrLen(fil) > 0 ) {
    while(lformat>this->size())
      this->append(fil);
  }
  if(this->size() > lformat )
    this->cut(lformat, this->size() -lformat);

  return *this;

}
uint8_t CStr::isAlphNum(void) {

	const char *str = this->c_str();
  for(std::size_t i=0;i<this->size();i++) {
      if( !isalnum(str[i]) && !(str[i]=='_') ) // within the ranges A - Z, a - z, or 0 - 9
          return 0;
  }
  return 1;
}	
bool CStr::compareText(const CStr &s) const 
{
  return SlfStrCompare(this->c_str(),0,(std::size_t)this->size(),s.c_str(),0,s.size());
}
bool CStr::compareText(const char *pstr) const
{
  return SlfStrCompare(this->c_str(),0,(std::size_t)this->size(),pstr,0,SlfStrLen(pstr));
}
bool CStr::compareText(const CStr &s,std::size_t pos1, std::size_t len1)
{
  return SlfStrCompare(this->c_str(),pos1,len1,s.c_str(),0,(std::size_t)s.size());
}
bool CStr::compareText(const char *pstr,std::size_t pos1, std::size_t len1)
{
  return SlfStrCompare(this->c_str(),pos1,len1,pstr,0,SlfStrLen(pstr));
}
//==============================================================
// Operatorfunktionen
//==============================================================
CStr& operator+(CStr& a, const CStr& b)
{
  a.append(b);
  return a;
}
CStr& operator+(CStr& a, const std::string& b)
{
  a.append(b);
  return a;
}
CStr operator+(const CStr &a,const CStr& b)
{
  CStr c = a;
	c.append(b);
	return c;
}
CStr operator+(const CStr &a, const char * b)
{
  CStr c = a;
  c.append(b);
  return c;
}
CStr operator+(const CStr &a,const std::string& b)
{
  CStr c = a;
  c.append(b);
  return c;
}
bool operator==(const CStr &a,const CStr &b)
{
	
	if (a.size() != b.size()                  ) return false;
  if( a.size() == 0        && a.size() != 0 ) return false; // Damit wird null abgefangen
  if( a.size() != 0        && b.size() == 0 ) return false; // Damit wird null abgefangen
  if( a.size() == 0        && b.size() == 0 ) return true; // Damit wird null abgefangen
  return (strcmp(a.c_str(), b.c_str())==0); 
}
bool operator==(const CStr &a,const std::string &b)
{
	
	if (a.size() != b.size()                  ) return false;
  if( a.size() == 0        && b.size() != 0 ) return false; // Damit wird null abgefangen
  if( a.size() != 0        && b.size() == 0 ) return false; // Damit wird null abgefangen
  if( a.size() == 0        && b.size() == 0 ) return true; // Damit wird null abgefangen
  return (strcmp(a.c_str(), b.c_str())==0); 
}
bool operator==(const std::string &b,const CStr &a)
{
	
	if (a.size() != b.size()                  ) return false;
  if( a.size() == 0        && b.size() != 0 ) return false; // Damit wird null abgefangen
  if( a.size() != 0        && b.size() == 0 ) return false; // Damit wird null abgefangen
  if( a.size() == 0        && b.size() == 0 ) return true; // Damit wird null abgefangen
  return (strcmp(a.c_str(), b.c_str())==0); 
}
bool operator==(const CStr &a,const char *pstr)
{

    if( a.size()    != SlfStrLen(pstr)                      ) return false;
    if( a.size()    == 0            && SlfStrLen(pstr) != 0 ) return false; // Damit wird null abgefangen
    if( a.size()    != 0            && SlfStrLen(pstr) == 0 ) return false; // Damit wird null abgefangen
    if( a.size()    == 0            && SlfStrLen(pstr) == 0 ) return true; // Damit wird null abgefangen
    return (strcmp(pstr,a.c_str())==0);
}
bool operator!=(const CStr &a,const CStr &b)
{
	
	if (a.size() != b.size()                  ) return true;
  if( a.size() == 0        && a.size() != 0 ) return true; // Damit wird null abgefangen
  if( a.size() != 0        && b.size() == 0 ) return true; // Damit wird null abgefangen
  if( a.size() == 0        && b.size() == 0 ) return false; // Damit wird null abgefangen
  return (strcmp(a.c_str(), b.c_str())!=0); 
}
bool operator!=(const CStr &a,const std::string &b)
{
	
	if (a.size() != b.size()                  ) return true;
  if( a.size() == 0        && b.size() != 0 ) return true; // Damit wird null abgefangen
  if( a.size() != 0        && b.size() == 0 ) return true; // Damit wird null abgefangen
  if( a.size() == 0        && b.size() == 0 ) return false; // Damit wird null abgefangen
  return (strcmp(a.c_str(), b.c_str())!=0); 
}
bool operator!=(const std::string &b,const CStr &a)
{
	
	if (a.size() != b.size()                  ) return true;
  if( a.size() == 0        && b.size() != 0 ) return true; // Damit wird null abgefangen
  if( a.size() != 0        && b.size() == 0 ) return true; // Damit wird null abgefangen
  if( a.size() == 0        && b.size() == 0 ) return false; // Damit wird null abgefangen
  return (strcmp(a.c_str(), b.c_str())!=0); 
}
bool operator!=(const CStr &a,const char *pstr)
{

    if( a.size()    != SlfStrLen(pstr)                      ) return true;
    if( a.size()    == 0            && SlfStrLen(pstr) != 0 ) return true; // Damit wird null abgefangen
    if( a.size()    != 0            && SlfStrLen(pstr) == 0 ) return true; // Damit wird null abgefangen
    if( a.size()    == 0            && SlfStrLen(pstr) == 0 ) return false; // Damit wird null abgefangen
    return (strcmp(pstr,a.c_str())!=0);
}
//===================
// weiter Funktionen
//===================
std::size_t SlfStrLen (const char *s)
{
  std::size_t n = 0;
  while (s != NULL && *s++ != '\0')
    {
      ++n;
    }
  return n;
}
std::size_t SlfStrLen (CStr &s)
{
	return SlfStrLen(s.c_str());
}
//Sonstige-Funktionen ======================
//==========================================
char* SlfCharCpy (char *dest, const char *src, std::size_t len)
{
  if (dest < src)
    {
      const char *firsts = src;
      char *firstd = dest;
      while (len--)
	      *firstd++ = *firsts++;
    }
  else
    {
      const char *lasts = (const char *)src + (len-1);
      char *lastd = (char *)dest + (len-1);
      while (len--)
        *lastd-- = *lasts--;
    }
  return dest;
}
void SlfCharSet (const char * dest, char c, std::size_t len)
{
  unsigned char *ptr = (unsigned char*)dest;
  while (len-- > 0)
    *ptr++ = c;
}
bool   SlfCharIsAlphNum(char c)
{
  if( ((c>64) && (c<91)) || ((c>96) && (c<123)) ) return true;
  else                                            return false;
}
//------------------------------------------------
// check type of character:
// type = 'a'   => ´check if c is alpha
// type = 'd'   =>             is digit
// type = 'n'   =>             is alphanumeric
// any other type => check if c == type
//--------------------------------------------------
bool   SlfCharIsType(const char c,const char type)
{
  if (type == 'a')
  {
    return (isalpha(c) != 0);
  }
  else if (type == 'd')
  {
    return (isdigit(c) != 0);
  }
  else if (type == 'n')
  {
    return (isalnum(c) != 0);
  }
  else if (type == c)
  {
    return true;
  }
  return false;
}
bool   SlfStrIsAlphNum(const char *src)
{
  for(std::size_t i=0;i<SlfStrLen(src);i++)
  {
    if( !SlfCharIsAlphNum(*(src+i)) )
    {
      return false;
    }
  }
  return true;
}
bool   SlfStrIsAlphNum(CStr &s)
{
  return SlfStrIsAlphNum(s.c_str());
}
//==================================================================
// Find-Funktionen
//==================================================================
std::size_t SlfStrFind(const CStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ) {

    return SlfStrFind(str.c_str()
                     ,txtsuch
                     ,suchregel
                     ,txtquot0
                     ,txtquot1
                     ,quotregel
                     ,istatus
                     ,0
                     ,str.getLen()
                     );
}
std::size_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ) {

    return SlfStrFind(txt
                     ,txtsuch
                     ,suchregel
                     ,txtquot0
                     ,txtquot1
                     ,quotregel
                     ,istatus
                     ,0
                     ,SlfStrLen(txt)
                     );
}
std::size_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,std::size_t pos0) {
    
    return SlfStrFind(txt
                     ,txtsuch
                     ,suchregel
                     ,txtquot0
                     ,txtquot1
                     ,quotregel
                     ,istatus
                     ,pos0
                     ,SlfStrLen(txt)-pos0
                     );
}
std::size_t SlfStrFind(const CStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,std::size_t pos0
                 ,std::size_t len0
                 ) {

    return SlfStrFind(str.c_str()
                     ,txtsuch
                     ,suchregel
                     ,txtquot0
                     ,txtquot1
                     ,quotregel
                     ,istatus
                     ,pos0
                     ,len0
                     );
}
std::size_t SlfStrFind(const CStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,std::size_t pos0) {
    
    return SlfStrFind(str.c_str()
                     ,txtsuch
                     ,suchregel
                     ,txtquot0
                     ,txtquot1
                     ,quotregel
                     ,istatus
                     ,pos0
                     ,str.getLen()-pos0
                     );
}
/*---------------------------------------------------------------------------
Sucht string txtsuch in txt zwischen pos0 und len0 nach der suchregel
suchregel = "vs" sucht      string von txtsuch         vorwaerts   in txt
"rs" sucht      "                          rueckwaerts "
"vn" sucht wenn string nicht mehr auftritt vorwaerts   "
"rn" sucht "                               rueckwaerts "
wobei er innerhalb oder ausserhalb von quot0 und quot1 sucht entsprechend
dem status istatus
quotregel = "i" innerhalb
= "a" ausserhalb

istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots
1   "                             innerhalb     "

return(npos)  nicht gefunden ansonsten
return(i)             Stelle wo absolut gefunden beginnend mit 0
----------------------------------------------------------------------------*/
std::size_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,std::size_t pos0
                 ,std::size_t len0
                 ) {

  std::size_t ind,i,j;
  std::size_t lentxt,lentsuch;
  uint8_t  *ivsuch=NULL;
  char run_flag,found_flag,sflag;
  
	if( SlfStrLen(txt)      == 0 ) return (std::size_t)std::string::npos;
  if( SlfStrLen(txtsuch)  == 0 ) return (std::size_t)std::string::npos;
  if( SlfStrLen(txtquot0) == 0 ) return (std::size_t)std::string::npos;
  if( SlfStrLen(txtquot1) == 0 ) return (std::size_t)std::string::npos;

  lentxt   = SLF_MIN(len0,SlfStrLen(txt+pos0));
  lentsuch = SlfStrLen(txtsuch);

	ivsuch = new uint8_t[lentxt];

  //ivsuch = (uint8_t *)malloc(lentxt*sizeof(uint8_t));

  SlfStrFindQuoted(txt,txtquot0,txtquot1
                  ,quotregel,pos0,len0
                  ,ivsuch,istatus);

  if( *(suchregel+1) == 's' || *(suchregel+1) == 'S' )
      sflag = 1;
  else
      sflag = 0;
  
  if( *suchregel == 'v' || *suchregel == 'V' ) { /* vorwärst */


      ind = 0;
      while(ind < lentxt ) {

              found_flag = 1;
              for(i=0;i<lentsuch;i++) {

                  if( (ind+i<lentxt)
                    && ivsuch[ind+i]
                    && (  (sflag  && (*(txt+pos0+ind+i) == *(txtsuch+i)))
                       || (!sflag && (*(txt+pos0+ind+i) != *(txtsuch+i)))
                       )
                    ) {

                      if( !sflag )
                          break;
                  } else {
                      found_flag = 0;
                      break;
                  }
              }
              if( found_flag ) {

                  free(ivsuch);
                  if( sflag )
                    return ind+pos0;
                  else
                    return ind+pos0;
              }
              ind++;
      }
  } else { /* rückwärts */

      ind = lentxt-1;
      run_flag = 1;
      while( run_flag ) {

              found_flag = 1;
              for(i=0;i<lentsuch;i++) {

                  j = lentsuch-1-i;


                  if( (ind>=i) // ind-i >=0
                    && ivsuch[ind-i]
                    && (  (sflag  && (*(txt+pos0+ind-i) == *(txtsuch+j)))
                       || (!sflag && (*(txt+pos0+ind-i) != *(txtsuch+j)))
                       )
                    ) {
                        
                      if( !sflag )
                          break;
                  } else {

                      found_flag = 0;
                      break;
                  }

              }
              if( found_flag ) {

                  free(ivsuch);
                  if( sflag )
                    return ind+pos0+1-lentsuch; // ind+pos0-(lentsuch-1)
                  else
                    return ind+pos0;
              }
              if( ind == 0 )
                  run_flag = 0;
              else
                  ind--;
      }
  }

  //free(ivsuch);
  delete[] ivsuch;      
  return SLF_STR_NPOS;
}
void SlfStrFindQuoted(const char *txt      // Text der zu durchkämmen
                     ,const char *txtq0    // Start-Quot
                     ,const char *txtq1    // End-Quot
                     ,const char *quotregel// "i": innerhalb quot mit einsen belegen
                                           // "a": außerhalb quot mit -"-
                     ,std::size_t pos0          // Start, wo gesucht wird
                     ,std::size_t len0          // Länge, des Stücks zum Suchen
                     ,uint8_t  *pmarker      // VEktor mit länge len0
                     ,char   istatus       // 0:startet ausserhalb quot
                                           // 1:startet innerhalb quot
                     ) {

    
  std::size_t ltxt = SLF_MIN(len0,SlfStrLen(txt+pos0));
  std::size_t lq0  = SlfStrLen(txtq0);
  std::size_t lq1  = SlfStrLen(txtq1);
                     
  std::size_t ind    = 0;
  std::size_t i;
  uint8_t  flag;

  // Ich suche die Stellen innerhalb des quots
  while(ind < ltxt) {

      if( istatus ) { // Ich bin innerhalb des quots

        // Ich suche das nächste Ende des Quots
          flag = 1;
          for(i=0;i<lq1;i++) {
              
            if(  (ind+i < ltxt)
              && (*(txt+pos0+ind+i) == *(txtq1+i)) ) { // übereinstimmend
                
                pmarker[ind+i] = 2;
            
            } else { // nicht übereinstimmend
                
                flag = 0;
                break;
            
            }  
          }
          if( flag ) { // Ende gefunden

              istatus  = 0;     // Wieder ausserhalb suchen
              ind     += lq1-1; // ind auf das Ende des Quots setzen

          } else { // noch nicht gefunden

              pmarker[ind] = 1; //innerhalb
          }

      } else { // Ich bin ausserhalb des quots
          
          // Ich suche den nächsten Anfang
          flag = 1;
          for(i=0;i<lq0;i++) {
              
              if(  (ind+i < ltxt)
                && (*(txt+pos0+ind+i) == *(txtq0+i))
                ) {

                    pmarker[ind+i] = 2;
              } else {

                    flag = 0;
                    break;    
              }
          }
          if( flag ) { // Anfang gefunden

              istatus  = 1;     // Wieder innerhalb suchen
              ind     += lq0-1; // ind auf das Ende des Quots setzen
              
          } else { // noch nicht gefunden

              pmarker[ind] = 0; // ausserhalb
          }

      }
      // Nächste Stelle  
      ind++;
  }

  if( *quotregel == 'a' || *quotregel == 'A' ) {
      
      for(i=0;i<ltxt;i++) {

          if( pmarker[i] == 1 )
              pmarker[i] = 0;
          else if( pmarker[i] == 0 )
              pmarker[i] = 1;
          else
              pmarker[i] = 0;
      }
  } else { 
      for(i=0;i<ltxt;i++) {

          if( pmarker[i] == 2 )
              pmarker[i] = 0;
      }
  }
}
/*==========================================================================*/
std::size_t SlfStrFind(CStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ) {
    return SlfStrFind(str.c_str(),txtsuch,regel,0,(std::size_t)str.size());
}
std::size_t SlfStrFind(CStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ,std::size_t pos0
                 ) {
    return SlfStrFind(str.c_str(),txtsuch,regel,pos0,str.getLen()-pos0);
}
std::size_t SlfStrFind(CStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ,std::size_t pos0
                 ,std::size_t len0
                 ) {
    return SlfStrFind(str.c_str(),txtsuch,regel,pos0,len0);
}
std::size_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ) {
    return SlfStrFind(txt,txtsuch,regel,0,SlfStrLen(txt));
}
std::size_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ,std::size_t pos0
                 ) {
    return SlfStrFind(txt,txtsuch,regel,pos0,SlfStrLen(txt)-pos0);
}

std::size_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ,std::size_t pos0
                 ,std::size_t len0
                 ) {
  std::size_t l2;
  std::size_t    i,j,k;
  std::size_t    pos1;
  
  if( pos0+len0 > 0 )
      pos1 = pos0+len0-1;
  else
      pos1 = 0;

  if( SlfStrLen(txt)     == 0   ) return SLF_STR_NPOS;
  if( SlfStrLen(txtsuch) == 0   ) return SLF_STR_NPOS;
  if( SlfStrLen(regel)   == 0   ) return SLF_STR_NPOS;
  
  l2 = SlfStrLen(txtsuch);  

  if( pos0 >  pos1           ) return SLF_STR_NPOS;
  if( pos1 >  SlfStrLen(txt)-1  ) pos1 = SlfStrLen(txt)-1;
  if( l2   >  pos1+1         ) return SLF_STR_NPOS;

  if( *regel == 'v' || *regel == 'V')
  {
    if( *(regel+1) == 's' || *(regel+1) == 'S')
    {
      for(j=pos0;j<pos1-l2+2;j++)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i == 0) return j;
      }
    }
    else if( *(regel+1) == 'n' || *(regel+1) == 'N' )
    {
      for(j=pos0;j<pos1-l2+2;j++)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i != 0) return j;
      }
    }
    else
    {
      return SLF_STR_NPOS;
    }
  }
  else if( *regel == 'r' || *regel == 'R')
  {
    if( *(regel+1) == 's' || *(regel+1) == 'S' )
    {
      for(j=pos1-l2+1;j>=pos0;j--)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i == 0 ) return j;

        if( j == pos0 )
            break;
      }
    }
    else if( *(regel+1) == 'n' || *(regel+1) == 'N' )
    {
      for(j=pos1-l2+1;j>=pos0;j--)
      {
        i = l2;
        for(k=0;k<l2;k++)
        {
          if( *(txt+j+k) == *(txtsuch+k) ) 
              i = i-1;
        }
        if( i != 0) return j;

        if( j == pos0 )
            break;
      }
    }
    else
      return SLF_STR_NPOS;
  }
    
  return SLF_STR_NPOS;
}
//---------------------------------------------
//
// sucht in txt nach quotes txtquot0, txtquot1 und gibt inhalt in s zurück
// suchregel = "vs","vn","rs","rn" vorwaerts suchen/nicht suchen rückwaerts sucjen7nicht
//
// return 0; nichts gefunden
// return 1; quots gefunden, gibt Inhalt aus s zwischen quota zurück
// return 2; Nur Anfangsquot gefunden gibt alles dahinter zurück
//
uint8_t SlfStrFindText(CStr &s
                    ,const char *txt
                    ,const char *txtquot0
                    ,const char *txtquot1
                    ,const char *suchregel) {

    std::size_t i0;
    std::size_t i1;

    if( SlfStrLen(txtquot0) == 0 ) 

        i0 = 0;

    else if( (i0=SlfStrFind(txt,txtquot0,suchregel)) == SLF_STR_NPOS)

        return 0;


    i0 = i0+SlfStrLen(txtquot0);


    if(  (i1=SlfStrFind(txt,txtquot1,suchregel,i0)) != SLF_STR_NPOS) {
          
            
        SlfStrCpy(s,txt,i0,i1-i0);

        return 1;

    } else {

        SlfStrCpy(s,txt,i0,SlfStrLen(txt)-i0);

        return 2;
            
    }


    return 0;
}
okay_t SlfStrElimAnf(CStr &text,const char *elim_char/*=" "*/) 
{
	okay_t status=OKAY;
  CStr  elim_string(elim_char);
  std::size_t   i=0;
  std::size_t   l0 = elim_string.size();

  for(i=0;i<text.size();i+=l0) {
    if( text.compare(i, l0,elim_string) != 0 ) { // Wenn das erste mal nicht gefunden
                                                 // reduzieren
      text.replace(0, i,"");
      break;
    }
  }

  return OKAY;
}
okay_t SlfStrElimEnd(CStr &text,const char *elim_char/*=" "*/) {

  CStr elim_string(elim_char);
  std::size_t i,i2;
  std::size_t l0 = elim_string.size();
  std::size_t l1 = text.size();
  std::size_t l2;
  bool flag;
  
  if( l1 >= l0 ) {
    i    = l1-l0;
		i2   = std::string::npos;
    l2   = 0;
    flag = true;
    while( flag ) {
      if( text.compare(i,l0,elim_string) != 0 ) {
        flag = false;
      } else {
        i2 =  i;
        l2 += l0;
        if( i < l0 )
          flag = false;
        else
          i -= l0;
      }
    }
    if( l2 != 0 ) text.replace(i2,l2,"");
  }
  return OKAY;
}
okay_t SlfStrElimAnfEnd(CStr &text,const char *elim_char/*=" "*/) {
  okay_t status=OKAY;
  status = SlfStrElimAnf(text,elim_char);
  if( status == OKAY )
    status = SlfStrElimEnd(text,elim_char);
  return status;
}
okay_t SlfStrElimAnfC(CStr &text,const char *elim_char/*="\t "*/) {

  CStr elim_string(elim_char);
  std::size_t  i;
  bool    search_flag = true;

  while( search_flag && text.size() > 0 ) {
    search_flag = false;
    for(i=0;i<elim_string.size();i++) {

      if( text[0] == elim_string[i] ) {
        text.cut(0,1);
        search_flag = true;
        break;
      }
    }
  }
  return OKAY;
}
okay_t SlfStrElimEndC(CStr &text,const char *elim_char/*=" "*/) {

  CStr elim_string(elim_char);
  std::size_t  i;
  bool    search_flag = true;

  while( search_flag && text.size() > 0 ) {
    search_flag = false;
    for(i=0;i<elim_string.size();i++) {

      if( text[text.size()-1] == elim_string[i] ) {
        text.cut((std::size_t)text.size()-1,1);
        search_flag = true;
        break;
      }
    }
  }
  return OKAY;
}
okay_t SlfStrElimAnfEndC(CStr &text,const char *elim_char/*=" "*/) {
  okay_t status=OKAY;
  status = SlfStrElimAnfC(text,elim_char);
  if( status == OKAY )
    status = SlfStrElimEndC(text,elim_char);
  return status;
}
okay_t SlfStrElim(CStr &text,const char *elim_string/*=" "*/) {

    std::size_t i0;

		while( (i0=SlfStrFind(text,elim_string,"vs")) != std::string::npos )
        text.cut(i0,SlfStrLen(elim_string));
    return OKAY;
}
//=============================================================
// String Cpy Funktionen
//=============================================================
const char * SlfStrCpy (char * dest, std::size_t len, const char *  src)
{
  char *d = dest;
  int c;
  while ((c = *src++) != '\0' && len > 0)
    {
      *d++ = c;
      len--;
    }
  *d = '\0';
  return dest;
}
const char * SlfStrCpy (char * dest, std::size_t len, const char *  src, std::size_t pos0, std::size_t len0)
{
  char *d = dest;
  int c;
  
  if( pos0 < SlfStrLen(src) )  {

    if( pos0+len0 > SlfStrLen(src) ) 
      len0 = SlfStrLen(src) - pos0;

    src += pos0; 
    while ((c = *src++) != '\0' && len > 1 && len0 > 0)
      {
        *d++ = c;
        len--;
        len0--;
      }
  }
  *d = '\0';

  return dest;
}
const char * SlfStrCpy (char * dest, std::size_t len, CStr  &ssrc, std::size_t pos0, std::size_t len0) {

  return SlfStrCpy(dest,len,ssrc.c_str(),pos0,len0);
}
const char * SlfStrCpy (CStr  &sdest, CStr  &ssrc, std::size_t pos0, std::size_t len0) {

  char *p = new char[len0+1];
  SlfStrCpy(p,len0+1,ssrc,pos0,len0);
  sdest = p;
  delete []p;

  return sdest.c_str();
}
const char * SlfStrCpy (CStr  &sdest, const char *  src, std::size_t pos0, std::size_t len0) {
  char *p = new char[len0+1];
  SlfStrCpy(p,len0+1,src,pos0,len0);
  sdest = p;
  delete []p;

  return sdest.c_str();
}
const char * SlfStrFindQuotCpy(CStr  &sdest, CStr  &ssrc,char *txtquot0, char *txtquot1) {

    std::size_t i0,i1;

    i0 = SlfStrFind(ssrc, txtquot0,"vs");
    if( i0 == SLF_STR_NPOS)
        return NULL;

    i0 += SlfStrLen(txtquot0);
        
    i1 = SlfStrFind(ssrc, txtquot1,"vs",i0);
    
    if( i1 == SLF_STR_NPOS)
        return NULL;
    
    if( i1 > 0 )
        i1 -= 1;

    if( i1 > i0 ) {
        SlfStrCpy(sdest,ssrc, i0, i1-i0+1);        
        return sdest.c_str();
    }

    return NULL;

}
/*======================================================================================================*/
okay_t SlfStrCatFormat(CStr &text, char *pformat,...) {
    
    SSlfStrFormatListe *pformat_liste=0;
    SSlfStrFormatListe *p_f_l,*p_f_l1;
    std::size_t      n_format_liste;
    CStr  format_text;
    
    va_list             marker;
    int                 ivalue;
    double              dvalue;
    int                 *p_ivalue;
    void                *p_void;
    char *              p_string;
    long int            livalue;
    long unsigned int   luivalue;
    short int           sivalue;
    short unsigned int  suivalue;
        
    if( SlfStrSetFormatListe(pformat,&pformat_liste,&n_format_liste) == NOT_OKAY ) {
        printf("Error in <SlfStrCatFormat> aufgetreten\n");
        return NOT_OKAY;
    }

    /* Variable Liste initialisieren */
    va_start(marker,pformat);

    p_f_l = pformat_liste;
    while(p_f_l != 0) {
    
        /* nicht formatierten Text zuerst anhängen */
        text.append(p_f_l->text);    
    
        /* formatierte Eingabe format_text erzeugen */
        /* altes löschen */
        format_text.clear();
    
        /* formatierte Eingabe übergeben */
        if( p_f_l->ptype != NULL ) {
        
            if( (strcmp(p_f_l->ptype,"c") == 0) ) {
            
                ivalue = va_arg( marker, int );
            
                if( ivalue > 31 ) {
                  
                  std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                  //sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                  sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#else
                  sprintf(p,p_f_l->format.c_str(),ivalue);
#endif
                  format_text.append(p);
                  delete  [] p;
                
                }
            } else if(  (strcmp(p_f_l->ptype,"d") == 0) 
                     || (strcmp(p_f_l->ptype,"i") == 0)
                     || (strcmp(p_f_l->ptype,"o") == 0) 
                     || (strcmp(p_f_l->ptype,"u") == 0) 
                     || (strcmp(p_f_l->ptype,"x") == 0) 
                     || (strcmp(p_f_l->ptype,"X") == 0) 
                     ) {

                ivalue = va_arg( marker, int );

                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
#else
                sprintf(p,p_f_l->format.c_str(),ivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"e") == 0) 
                     || (strcmp(p_f_l->ptype,"E") == 0) 
                     || (strcmp(p_f_l->ptype,"f") == 0) 
                     || (strcmp(p_f_l->ptype,"g") == 0) 
                     || (strcmp(p_f_l->ptype,"G") == 0) 
                     ) {
                dvalue = va_arg( marker, double );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),dvalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),dvalue);
#else
                sprintf(p,p_f_l->format.c_str(),dvalue);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if(  (strcmp(p_f_l->ptype,"n") == 0) 
                ) {
                p_ivalue = va_arg( marker, int* );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_ivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),p_ivalue);
#else
                sprintf(p,p_f_l->format.c_str(),p_ivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"p") == 0) 
                ) {
                p_void = va_arg( marker, void* );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_void);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),p_void);
#else
                sprintf(p,p_f_l->format.c_str(),p_void);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if(  (strcmp(p_f_l->ptype,"s") == 0)
                ) {
                p_string = va_arg( marker, char* );
            
                  std::size_t  ll = SLF_MAX((std::size_t)SlfStrLen(p_string),p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_string);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),p_string);
#else
                sprintf(p,p_f_l->format.c_str(),p_string);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if( (strcmp(p_f_l->ptype,"lu") == 0) ) {
            
                luivalue = va_arg( marker, long unsigned int );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),luivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),luivalue);
#else
                sprintf(p,p_f_l->format.c_str(),luivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"ld") == 0) 
                || (strcmp(p_f_l->ptype,"li") == 0) 
                || (strcmp(p_f_l->ptype,"lo") == 0) 
                || (strcmp(p_f_l->ptype,"lx") == 0) 
                || (strcmp(p_f_l->ptype,"lX") == 0) 
                ) {
                livalue = va_arg( marker, long int );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),livalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),livalue);
#else
                sprintf(p,p_f_l->format.c_str(),livalue);
#endif
                format_text.append(p);
                delete  [] p;
                        
            } else if( (strcmp(p_f_l->ptype,"hu") == 0) ) {
            
                suivalue = va_arg( marker, short unsigned int );

                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),suivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),suivalue);
#else
                sprintf(p,p_f_l->format.c_str(),suivalue);
#endif
                format_text.append(p);
                delete  [] p;
            
            
            } else if(  (strcmp(p_f_l->ptype,"hd") == 0) 
                || (strcmp(p_f_l->ptype,"hi") == 0) 
                || (strcmp(p_f_l->ptype,"ho") == 0) 
                || (strcmp(p_f_l->ptype,"hx") == 0) 
                || (strcmp(p_f_l->ptype,"hX") == 0) 
                ) {
                sivalue = va_arg( marker, short int );
            
                std::size_t  ll = SLF_MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),sivalue);
#if _MSC_VER > MSC_VER_BIS_VS2005
                sprintf_s(p,ll,p_f_l->format.c_str(),sivalue);
#else
                sprintf(p,p_f_l->format.c_str(),sivalue);
#endif
                format_text.append(p);
                delete  [] p;
                        
            }
        }
        /* ps_text anhängen */
        text.append(format_text);

        /* nächstes Element verkettete Liste */
        p_f_l = p_f_l->pnext;
    }
    /* variable Liste beenden */
    va_end(marker);

    /* Speicher freigeben */
    p_f_l = pformat_liste;
    while(p_f_l != 0) {

      
        /* nächstes Element verkettete Liste */
        p_f_l1 = p_f_l->pnext;

        /* Struktur löschen */
        delete p_f_l;

        p_f_l = p_f_l1;


    }
    return OKAY;
}
okay_t SlfStrSetFormatListe(char *pformat
                             ,SSlfStrFormatListe **ppformat_liste
                             ,std::size_t *n_format_liste) {
    
    std::size_t n;
    std::size_t i,itype;
    std::size_t i1,i2,ifound;
    SSlfStrFormatListe *pformat_liste=0;
    CStr format;
    CStr text;
    char *format_typ[]={"c","d","i","u","o","x","X","f","e","E","g","G","p","n","s",
        "ld","li","lo","lx","lX","lu","hd","hi","ho","hx","hX","hu"};
    
    /* kopiert der format-text in die Struktur */
    format = pformat;
    
    *n_format_liste = 0;
    
    while( format.size() > 0 ) { /* sucht nach Formatzeichen */
        
        /* Anzahl Argumate hochzählen */
        (*n_format_liste)++;
        n = *n_format_liste;
        
        /* Anlegen einer Formatstruktur */
        pformat_liste = new SSlfStrFormatListe;

        if( *ppformat_liste == 0 )
          *ppformat_liste = pformat_liste;
        else {
          SSlfStrFormatListe *pP = *ppformat_liste;
          while(pP->pnext != 0 )
            pP = pP->pnext;
          pP->pnext = pformat_liste;
        }        
        pformat_liste->pnext = 0;
        
        i1 = (std::size_t)format.find("%");
        i2 = (std::size_t)format.find("%%");;
        
        if( i1 == format.npos ) { /* keine Formatierung */
                
                pformat_liste->text   = format;
                pformat_liste->format = "";
                pformat_liste->ptype = NULL;
                pformat_liste->width  = 0;
                
                format = "";
        } else if( i1 == i2 ) { /* "%%" gefunden */
                                          
                SlfStrCpy(pformat_liste->text,format,0,i2+1);
                pformat_liste->format = "";
                pformat_liste->ptype = NULL;
                pformat_liste->width  = 0;
                
                format.cut(0,i2+2);                
            
        } else { /* Formatstring gefunden */
            
            
            /* Text vor Formatzeichen rauskopieren */

            //pformat_liste->text = format.substr(0,i1);
            SlfStrCpy (pformat_liste->text, format, 0, i1);
            pformat_liste->format = "";
            pformat_liste->ptype = NULL;
            pformat_liste->width  = 0;
            
            /* Text vor Formatzeichen rausschneiden*/
            format.cut(0,i1);
            
            ifound = format.npos;
            for(i=0;i<15;i++) { /* Alle Formattypen durchlaufen */
                
              i2 = (std::size_t)format.find(format_typ[i]);
              if(  (i2 != format.npos)
                && (i2 < ifound) /* Den am nächsten liegenden Formattyp raussuchen */
                ) {
                    ifound = i2; /* Stelle im Text */
                    itype  = i;
                }
            }
            
            if( ifound != format.npos ) { /* formattyp gefunden */
                                
              /*  Formatanweisung übergeben */
              
              //pformat_liste->format = format.substr(0,ifound+SlfStrLen(format_typ[itype]));
              SlfStrCpy (pformat_liste->format, format, 0, ifound+SlfStrLen(format_typ[itype]));
                
              /* Formattyp */
              pformat_liste->ptype = format_typ[itype];
                
              /* width */
              text = pformat_liste->format;
              text.elimAnfEnd(" ");
              text.elimAnf("%");
              text.elimAnf("-");
              text.elimAnf("+");
              text.elimAnf("0");
              text.elimEnd(format_typ[itype]);

              i1=(std::size_t)text.find(".");
              if( i1 !=  std::string::npos ) text.cut(i1,text.getLen()-i1);
              if( text.size() > 0 )
                pformat_liste->width = (std::size_t)atol(text.c_str());                
              else
                pformat_liste->width = SLF_FORMAT_DEFAULT_LENGTH;

              /* format für nächste while loop bereinigen */
              format.cut(0,ifound+SlfStrLen(format_typ[itype]));
                
            } else {
                /* Fehler: Format nicht vorhanden */
            }
                        
        }
                
    }
        
    return OKAY;
}
/*==============================================================================*/
/* Extrahiert aus name der Regel Pfad, File und Extension und kopiert es in die  */
/* Stringobjekt s. In regel kann stehen:                               */
/* "p"    Pfad, "f" wie filename und "e" wie extension                          */
/* z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn         */
/* vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt*/
/*==============================================================================*/
okay_t SlfStrExtractPfe(CStr &s,const char *name,const char *regel) {

    const char *ptxt;
    std::size_t itrennz,iext;
    CStr trennz;   /* Trennzeichen */

    /* Zuallesrt den string in p leeren */
	s = "";
    
    /* Zuallerst den einfachsten Konstrukt rausfiltern */
    if( SlfStrCompare(name,"") ) {
        return NOT_OKAY;
    }

    /* Dann einfache Pfadkonstrukte rausfiltern */
    if( SlfStrCompare(name,".")  ||  SlfStrCompare(name,"..") ) {

        if(  (SlfStrLen(regel) == 1)
	      && (	(SlfStrFind(regel,"p","vs")!= SLF_STR_NPOS)
             || (SlfStrFind(regel,"P","vs")!= SLF_STR_NPOS)
			 )
		  ) {
			s = name;
			return OKAY;
		}
    }

    /* Pfadtrennzeichen extrahieren */
	if( SlfStrFind(name,"\\","vs") != SLF_STR_NPOS)
		trennz = "\\";
	else if( SlfStrFind(name,"/","vs") != SLF_STR_NPOS)
		trennz = "/";
	else
		trennz = "\\";

    /* letztes Trennzeichen suchen */
	if( (itrennz=SlfStrFind(name,trennz.c_str(),"rs"))!= SLF_STR_NPOS) {

        if( (SlfStrFind(regel,"p","vs")!= SLF_STR_NPOS) ||
            (SlfStrFind(regel,"P","vs")!= SLF_STR_NPOS) ){

			// anhängen einschliesslich Trennzeichen
			s.cat(name,0,itrennz+trennz.getLen());
	
        }
        ptxt = &(name[itrennz+1]);
    } else {

		//kein .\ setzen
        //if( (SlfStrFind(regel,"p","vs")!=npos) ||
        //    (SlfStrFind(regel,"P","vs")!=npos) ){

		//	s.cat(".");
		//	s.cat(trennz);
        //}
        ptxt = name;
    }

    /* Jetzt in ptxt extension suchen */
    if( (iext=SlfStrFind(ptxt,".","vs"))!= SLF_STR_NPOS) {

        if( (SlfStrFind(regel,"f","vs")!= SLF_STR_NPOS) ||
            (SlfStrFind(regel,"F","vs")!= SLF_STR_NPOS) ){

            if( (SlfStrFind(regel,"e","vs")!= SLF_STR_NPOS) ||
                (SlfStrFind(regel,"E","vs")!= SLF_STR_NPOS) ){

				s.cat(ptxt);
			} else if( iext>0 ) 
				s.cat(ptxt,0,iext);
        } else 
        if( (SlfStrFind(regel,"e","vs")!= SLF_STR_NPOS) ||
            (SlfStrFind(regel,"E","vs")!= SLF_STR_NPOS) ){
            
            if( SlfStrLen(ptxt) > iext )
				s.cat(ptxt,iext+1,SlfStrLen(ptxt)-iext);
        }
    } else {

        if( (SlfStrFind(regel,"f","vs")!= SLF_STR_NPOS) ||
            (SlfStrFind(regel,"F","vs")!= SLF_STR_NPOS) ){

            s.cat(ptxt);

        }
    }

    return OKAY;
}
CStr SlfStrExtract(const CStr &s, std::size_t i0, std::size_t len)
{
  CStr sout;
  std::size_t n = i0+len;

  if (n < s.size())
  {
    sout = s;
    if (i0 > 0) sout.cut(0, i0);
    sout.cut(len, std::string::npos);
  }
  return sout;
}
bool SlfStrCompare(const CStr &s0,const CStr &s1) 
{
  return SlfStrCompare(s0.c_str(), 0, s0.getLen()
                     ,s1.c_str(), 0, s1.getLen());
}
bool SlfStrCompare(const char *str0,const char *str1) 
{
  return SlfStrCompare(str0, 0, SlfStrLen(str0), str1, 0, SlfStrLen(str1));
}
bool  SlfStrCompare(const CStr *ps0, const char* p1)
{
  return SlfStrCompare(ps0->c_str(), p1);
}
bool SlfStrCompare(const CStr &s0, std::size_t pos0, std::size_t len0
                ,const CStr &s1, std::size_t pos1, std::size_t len1) {
  return SlfStrCompare(s0.c_str(), pos0, len0, s1.c_str(), pos1, len1);
}
bool SlfStrCompare(const char *str0, std::size_t pos0, std::size_t len0
                ,const char *str1, std::size_t pos1, std::size_t len1) {

  int c0, c1;
  std::size_t n=0;
  
  len0 = SLF_MIN(SlfStrLen(str0),len0); 
  len1 = SLF_MIN(SlfStrLen(str1),len1);

  if( len0 < len1 ) return false;
  if( len0 > len1 ) return false;
  
  str0 = str0+pos0;
  str1 = str1+pos1;
  do
    {
      /* Use "unsigned char" to make the implementation 8-bit clean */
      c0 = *((unsigned char *)(str0++));
      c1 = *((unsigned char *)(str1++));
      n++;
      if (c0 != c1)
        {
          return ((c0 - c1)==0);
        }
    }
  while (c1 != 0 && n < len0);
  return true;
}
std::size_t SlfStrCompareCount(const CStr &s0,const CStr &s1) {
  return SlfStrCompareCount(s0.c_str(), 0, s0.getLen()
                     ,s1.c_str(), 0, s1.getLen());
}
std::size_t SlfStrCompareCount(const char *str0,const char *str1) {
  return SlfStrCompareCount(str0, 0, SlfStrLen(str0), str1, 0, SlfStrLen(str1));
}
std::size_t SlfStrCompareCount(const CStr &s0, std::size_t pos0, std::size_t len0
                ,const CStr &s1, std::size_t pos1, std::size_t len1) {
  return SlfStrCompareCount(s0.c_str(), pos0, len0, s1.c_str(), pos1, len1);
}
std::size_t SlfStrCompareCount(const char *str0, std::size_t pos0, std::size_t len0
                         ,const char *str1, std::size_t pos1, std::size_t len1) {
  int c0, c1;
  std::size_t n=0;
  
  len0 = SLF_MIN(SlfStrLen(str0),len0); 
  len1 = SLF_MIN(SlfStrLen(str1),len1);
  
  str0 = str0+pos0;
  str1 = str1+pos1;
  do {
      /* Use "unsigned char" to make the implementation 8-bit clean */
      c0 = *((unsigned char *)(str0++));
      c1 = *((unsigned char *)(str1++));
      if (c0 != c1)
          return n;
      ++n;

  } while (c1 != 0 && n < len0);
  return n;
}

const char * SlfStrCat (char * dest, std::size_t len, const char * src)
{
  char *d = dest;
  int c;
  std::size_t len0=0;
  while (*d != '\0')
    {
      ++d;
      ++len0;
    }
  while ((c = *src++) != '\0' && len > 0 )
    {
      *d++ = c;
      len--;
    }
  *d = '\0';
  return dest;
}
EErrNo SlfStr2Num(const char *p, double *pdval , char **pp_stop/*=NULL*/) {
  char *p1;
  EErrNo err = NO_ERR;
  //*pdval = strtod( p, &p1 );
  *pdval = SlfStrToD( p, &p1 );
  //*pdval = atof(p);
  if( p == p1 ) {
    err = NO_VALUE_CONVERTED;
  } else if( errno == ERANGE ) {
    err = OVER_UNDER_FLOW;
  }
  if( pp_stop != NULL )
    *pp_stop = p1;

  return err;
}
EErrNo SlfStr2Num(const char *p, uint8_t *puival , char **pp_stop/*=NULL*/) {
  double dval;
  EErrNo err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_UINT8 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MAX_UINT8;
    } else if( dval < (double)MIN_UINT8 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MIN_UINT8;
    } else {
      *puival = (uint8_t)dval;
    }
  }
  return err;
}
EErrNo SlfStr2Num(const char *p, sint8_t *pival , char **pp_stop/*=NULL*/) {
  double dval;
  EErrNo err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_SINT8 ) {
      err   = OVER_UNDER_FLOW;
      *pival = MAX_SINT8;
    } else if( dval < (double)MIN_SINT8 ) {
      err   = OVER_UNDER_FLOW;
      *pival = MIN_SINT8;
    } else {
      *pival = (sint8_t)dval;
    }
  }
  return err;
}
EErrNo SlfStr2Num(const char *p, uint16_t *puival , char **pp_stop/*=NULL*/) {
  double dval;
  EErrNo err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_UINT16 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MAX_UINT16;
    } else if( dval < (double)MIN_UINT16 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MIN_UINT16;
    } else {
      *puival = (uint16_t)dval;
    }
  }
  return err;
}
EErrNo SlfStr2Num(const char *p, sint16_t *pival , char **pp_stop/*=NULL*/) {
  double dval;
  EErrNo err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_SINT16 ) {
      err   = OVER_UNDER_FLOW;
      *pival = MAX_SINT16;
    } else if( dval < (double)MIN_SINT16 ) {
      err   = OVER_UNDER_FLOW;
      *pival = MIN_SINT16;
    } else {
      *pival = (sint16_t)dval;
    }
  }
  return err;
}
EErrNo SlfStr2Num(const char *p, std::size_t *puival , char **pp_stop/*=NULL*/) {
  double dval;
  EErrNo err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_UINT32 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MAX_UINT32;
    } else if( dval < (double)MIN_UINT32 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MIN_UINT32;
    } else {
      *puival = (std::size_t)dval;
    }
  }
  return err;
}
EErrNo SlfStr2Num(const char *p, sint32_t *pival , char **pp_stop/*=NULL*/) {
  double dval;
  EErrNo err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_SINT32 ) {
      err   = OVER_UNDER_FLOW;
      *pival = MAX_SINT32;
    } else if( dval < (double)MIN_SINT32 ) {
      err   = OVER_UNDER_FLOW;
      *pival = MIN_SINT32;
    } else {
      *pival = (sint32_t)dval;
    }
  }
  return err;
}
/*
 *----------------------------------------------------------------------
 *
 * SlfStrToD --
 *
 *	This procedure converts a floating-point number from an ASCII
 *	decimal representation to internal double-precision format.
 *
 * Results:
 *	The return value is the double-precision floating-point
 *	representation of the characters in string.  If endPtr isn't
 *	NULL, then *endPtr is filled in with the address of the
 *	next character after the last one that was part of the
 *	floating-point number.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

double
SlfStrToD(const char *string, char **endPtr)
    		     /* string;A decimal ASCII floating-point number,
				 * optionally preceded by white space.
				 * Must have form "-I.FE-X", where I is the
				 * integer part of the mantissa, F is the
				 * fractional part of the mantissa, and X
				 * is the exponent.  Either of the signs
				 * may be "+", "-", or omitted.  Either I
				 * or F may be omitted, or both.  The decimal
				 * point isn't necessary unless F is present.
				 * The "E" may actually be an "e".  E and X
				 * may both be omitted (but not just one).
				 */
             	/* endPtr;	If non-NULL, store terminating character's
				 * address here. */
{
    int sign, expSign = FALSE;
    double fraction, dblExp, *d;
    register const char *p;
    register int c;
    int exp = 0;		/* Exponent read from "EX" field. */
    int fracExp = 0;		/* Exponent that derives from the fractional
				 * part.  Under normal circumstatnces, it is
				 * the negative of the number of digits in F.
				 * However, if I is very long, the last digits
				 * of I get dropped (otherwise a long I with a
				 * large negative exponent could cause an
				 * unnecessary overflow on I alone).  In this
				 * case, fracExp is incremented one for each
				 * dropped digit. */
    int mantSize;		/* Number of digits in mantissa. */
    int decPt;			/* Number of mantissa digits BEFORE decimal
				 * point. */
    const char *pExp;		/* Temporarily holds location of exponent
				 * in string. */

    /*
     * Strip off leading blanks and check for a sign.
     */

    p = string;
    while (isspace(*p)) {
	p += 1;
    }
    if (*p == '-') {
	sign = TRUE;
	p += 1;
    } else {
	if (*p == '+') {
	    p += 1;
	}
	sign = FALSE;
    }

    /*
     * Count the number of digits in the mantissa (including the decimal
     * point), and also locate the decimal point.
     */

    decPt = -1;
    for (mantSize = 0; ; mantSize += 1)
    {
	c = *p;
	if (!isdigit(c)) {
	    if ((c != '.') || (decPt >= 0)) {
		break;
	    }
	    decPt = mantSize;
	}
	p += 1;
    }

    /*
     * Now suck up the digits in the mantissa.  Use two integers to
     * collect 9 digits each (this is faster than using floating-point).
     * If the mantissa has more than 18 digits, ignore the extras, since
     * they can't affect the value anyway.
     */

    pExp  = p;
    p -= mantSize;
    if (decPt < 0) {
	decPt = mantSize;
    } else {
	mantSize -= 1;			/* One of the digits was the point. */
    }
    if (mantSize > 18) {
	fracExp = decPt - 18;
	mantSize = 18;
    } else {
	fracExp = decPt - mantSize;
    }
    if (mantSize == 0) {
	fraction = 0.0;
	p = string;
	goto done;
    } else {
	int frac1, frac2;
	frac1 = 0;
	for ( ; mantSize > 9; mantSize -= 1)
	{
	    c = *p;
	    p += 1;
	    if (c == '.') {
		c = *p;
		p += 1;
	    }
	    frac1 = 10*frac1 + (c - '0');
	}
	frac2 = 0;
	for (; mantSize > 0; mantSize -= 1)
	{
	    c = *p;
	    p += 1;
	    if (c == '.') {
		c = *p;
		p += 1;
	    }
	    frac2 = 10*frac2 + (c - '0');
	}
	fraction = (1.0e9 * frac1) + frac2;
    }

    /*
     * Skim off the exponent.
     */

    p = pExp;
    if ((*p == 'E') || (*p == 'e')) {
	p += 1;
	if (*p == '-') {
	    expSign = TRUE;
	    p += 1;
	} else {
	    if (*p == '+') {
		p += 1;
	    }
	    expSign = FALSE;
	}
	while (isdigit(*p)) {
	    exp = exp * 10 + (*p - '0');
	    p += 1;
	}
    }
    if (expSign) {
	exp = fracExp - exp;
    } else {
	exp = fracExp + exp;
    }

    /*
     * Generate a floating-point number that represents the exponent.
     * Do this by processing the exponent one bit at a time to combine
     * many powers of 2 of 10. Then combine the exponent with the
     * fraction.
     */

    if (exp < 0) {
	expSign = TRUE;
	exp = -exp;
    } else {
	expSign = FALSE;
    }
    if (exp > maxExponent) {
	exp = maxExponent;
	errno = ERANGE;
    }
    dblExp = 1.0;
    for (d = powersOf10; exp != 0; exp >>= 1, d += 1) {
	if (exp & 01) {
	    dblExp *= *d;
	}
    }
    if (expSign) {
	fraction /= dblExp;
    } else {
	fraction *= dblExp;
    }

done:
    if (endPtr != NULL) {
	*endPtr = (char *) p;
    }

    if (sign) {
	return -fraction;
    }
    return fraction;
}

okay_t SlfStrChange(CStr &text,char *textsuch,char *textchange) {

    std::size_t i=0;
    if(  strcmp(textsuch,textchange) != 0 
      && *textsuch != '\0' ) {

        while( (i = (std::size_t)text.find(textsuch,i)) != std::string::npos) {

            text.cut(i,SlfStrLen(textsuch));
            text.insertText(textchange,i,SlfStrLen(textchange));

            i = i+1-SlfStrLen(textsuch)+SlfStrLen(textchange);
        }
    }
    return OKAY;
}
okay_t SlfStrChange(CStr &text, char *textsuch, char *textchange, std::size_t pos0, std::size_t len0)
{
  std::size_t i  = pos0;
  std::size_t i1 = pos0 + len0;

  if (strcmp(textsuch, textchange) != 0
    && *textsuch != '\0') {

    std::size_t lsuch = SlfStrLen(textsuch);
    std::size_t lchange = SlfStrLen(textchange);

    while (((i = (std::size_t)text.find(textsuch, i)) != std::string::npos) && (i<i1))
    {
      text.cut(i, lsuch);
      text.insertText(textchange, i, lchange);

      i = i + 1 + lchange - lsuch;
      i1 += lchange;
      i1 -= lsuch;
    }
  }
  return OKAY;

}
#if 0
//char& CStr::operator[](std::size_t elem)
//{
// 
//  if(elem == 0 && len == 0) {
//    str  = new char[1];
//    *str = 0;
//    return *str;
//  } else {
//    if((elem>=0) && (elem<len))
//      return str[elem];
//    else
//      return str[0];
//  }
//}

//const char& CStr::operator[](std::size_t elem) const
//{
//  assert((elem>=0) && (elem<len));
//  return str[elem];
//}


CStr CStr::operator()(std::size_t pos, std::size_t cnt) const
{
  char*    ps = new char[cnt+1];
  std::size_t   i;

  //assert((pos>=0) && (cnt>=0) && (pos+cnt<=len));
  if( pos >= len ) {
    ps[0] = (char)0;
  } else {
    if( pos+cnt > len )
      cnt = len - pos;
  
    for (i=pos; i<pos+cnt; ++i) 
      ps[i-pos] = str[i];

    ps[cnt] = (char)0;
  }
  CStr retstr(ps);
  delete [] ps;
  return retstr;  // es muss ein "neuer" String zurueckgegeben werden
}


CStr operator+(const CStr &s1, const CStr &s2)
{
  CStr result = s1;

  result += s2; 
  return result;
}

bool CStr::operator<(const CStr& s) const
{ 
  if (s.len == 0) return false;
  if (len == 0) return true;
  return (strcmp(str,s.str)<0); 
}


CStr::operator char* () const
{
  return str;
}

//istream& operator>>(istream &io, string &s)
//{
//  io >> statInBuf;  // Zuerst in stat. Buffer einlesen
//  s = statInBuf;    // Dann nach s kopieren 
//  return io;
//}

//ostream& operator<<(ostream& os, const string& s)
//{
//  if (s.str!=0) 
//    return os << s.str;
//  else 
//    return os;
//}
void CStr::clear() {

    if( len > 0 ) {

        delete [] str; 
        str = 0;
        len = 0;
    }
}



//=========================================================================
//=========================================================================
#if 0
CStr CStr::substr(std::size_t pos0, std::size_t len0) {

  

  if( pos0 < len && len != 0 ) {
    len0 = SLF_MIN(len-pos0,len0);
    char  *p = new char[len0+1];
    std::size_t i = 0;
    while(i < len0) { 
      p[i] = str[pos0+i];
      i++;
    }
    p[i] = (char)0;
 
    //sstr=p;
    CStr sstr(p);
    delete [] p;
    return sstr;
  } else {
    CStr sstr="";
    return sstr;
  }

  
}
#endif
//=========================================================================
//=========================================================================
/*======================================================================================================*/



  
//===========================================================
// Stringbearbeitungs-Funktionen
//===========================================================
//



#endif
} // namespace