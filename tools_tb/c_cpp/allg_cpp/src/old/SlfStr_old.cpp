#include "SlfStr.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdarg.h>
#include <ctype.h>

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
  uint32_t         width;
  CSlfStr        text;
  CSlfStr        format;
  SSlfStrFormatListe   *pnext;
};

status_t SlfStrSetFormatListe(char *pformat
                             ,SSlfStrFormatListe **ppformat_liste
                             ,uint32_t *n_format_liste);
void SlfStrFindQuoted(const char *txt      // Text der zu durchkämmen
                     ,const char *txtq0    // Start-Quot
                     ,const char *txtq1    // End-Quot
                     ,const char *suchregel// "i": innerhalb quot mit einsen belegen
                                           // "a": außerhalb quot mit -"-
                     ,uint32_t pos0          // Start, wo gesucht wird
                     ,uint32_t len0          // Länge, des Stücks zum Suchen
                     ,uint8_t  *pmarker      // VEktor mit länge len0
                     ,char   istatus       // 0:startet ausserhalb quot
                                           // 1:startet innerhalb quot
                     );
Vector_t *pVecDebugInterim;

#if 0
CSlfStr::CSlfStr()
: len(0), str(0), npos(MAX_UINT32)
{}
#endif
CSlfStr::CSlfStr() {

    len = 0;
    str = 0;
    npos = MAX_UINT32;
}


CSlfStr::CSlfStr(const char* p) 
{
  if (p == 0) { 
    len  = 0; 
    str  = 0;
  } else {
    len = (uint32_t)SlfStrLen(p);
    str = new char[len+1];
    SlfCharCpy(str, p, len+1);
  }
  npos = MAX_UINT32;
}
CSlfStr::CSlfStr(int l, char fillChar)
 : len(l)
{
  if (l == 0) { 
    str = 0;
  } else {
    str = new char[len+1];
    SlfCharSet(str, fillChar,  len);
    str[len] = (char)0;
  }
  npos = MAX_UINT32;
}

CSlfStr::CSlfStr(int l)
{
  char    t[30];

  //
#if     _MSC_VER > 1200  // ab Visual2005
  _itoa_s(l, t, 30, 10);
#else
  _itoa(l, t, 10);
#endif
  
  len = SlfStrLen(t);
  str = new char[len+1];
  SlfCharCpy(str, t, len+1);
  npos = MAX_UINT32;
}


CSlfStr::~CSlfStr()
{
  delete [] str; 
  str = 0;
  len = 0;
}

CSlfStr& CSlfStr::operator=(const CSlfStr& s)
{
  if (this == &s) return *this; // "a1 = a1"

  delete [] str; str = 0;       // alte Inhalte loeschen

  len = s.len;                  // neu allokieren und kopieren
  if (s.str != 0) { 
    str = new char[len+1];
    SlfCharCpy(str, s.str, len+1);
  }
  return *this;                 // return *this
}
//CSlfStr& CSlfStr::operator=(const std::string& s)
//{
//  //if (this == &s) return *this; // "a1 = a1"
//
//  delete [] str; str = 0;       // alte Inhalte loeschen
//
//  len = (uint32_t)s.size();   // neu allokieren und kopieren
//  if (len > 0) { 
//    str = new char[len+1];
//    SlfCharCpy(str, s.c_str(), len+1);
//  }
//  return *this;                 // return *this
//}
CSlfStr& CSlfStr::operator=(const char *p)
{
  delete [] str; 
  str = 0;
  if (p == 0) { 
    len = 0; 
  } else {
    len = SlfStrLen(p);
    str = new char[len + 1];
    SlfCharCpy(str, p, len+1);
  }
  return *this;
}
//char& CSlfStr::operator[](uint32_t elem)
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

//const char& CSlfStr::operator[](uint32_t elem) const
//{
//  assert((elem>=0) && (elem<len));
//  return str[elem];
//}


CSlfStr CSlfStr::operator()(uint32_t pos, uint32_t cnt) const
{
  char*    ps = new char[cnt+1];
  uint32_t   i;

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
  CSlfStr retstr(ps);
  delete [] ps;
  return retstr;  // es muss ein "neuer" String zurueckgegeben werden
}

CSlfStr& CSlfStr::operator+=(const char *pcat)
{
  char*   p;

  if (SlfStrLen(pcat) == 0) return *this; // Wenn s leer ist: return *this
  if (len==0) {
    len = SlfStrLen(pcat);                // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,pcat);
  } else {
    len += SlfStrLen(pcat);               // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,str);              // Strings kopieren
    SlfStrCat(p,len+1,pcat);             
    delete [] str;              // Alten buffer loeschen 
  }
  str = p;                      // neuen buffer zuweisen 
  return *this;
}
CSlfStr& CSlfStr::operator+=(const CSlfStr &s)
{
  char*   p;

  if (s.len == 0) return *this; // Wenn s leer ist: return *this
  if (len==0) {
    len = s.len;                // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,s.str);
  } else {
    len += s.len;               // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,str);              // Strings kopieren
    SlfStrCat(p,len+1,s.str);             
    delete [] str;              // Alten buffer loeschen 
  }
  str = p;                      // neuen buffer zuweisen 
  return *this;
}
//CSlfStr& CSlfStr::operator+=(const std::string &s)
//{
//  char*   p;
//
//  if (s.size() == 0) return *this; // Wenn s leer ist: return *this
//  if (len==0) {
//    len = (uint32_t)s.size();                // neue Laenge festlegen
//    p = new char[len+1];        // neuen Buffer allokieren
//    SlfStrCpyS(p,len+1,s.c_str());
//  } else {
//    len += (uint32_t)s.size();               // neue Laenge festlegen
//    p = new char[len+1];        // neuen Buffer allokieren
//    SlfStrCpyS(p,len+1,str);              // Strings kopieren
//    SlfStrCatS(p,len+1,s.c_str());             
//    delete [] str;              // Alten buffer loeschen 
//  }
//  str = p;                      // neuen buffer zuweisen 
//  return *this;
//}
void CSlfStr::append(const CSlfStr &s)
{
  char*   p;

  if (s.len == 0) return; // Wenn s leer ist: return *this
  if (len==0) {
    len = s.len;                // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,s.str);
  } else {
    p = new char[len+s.len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len,str);              // Strings kopieren
    SlfStrCat(p,s.len,s.str);             
    delete [] str;              // Alten buffer loeschen 
    len += s.len;               // neue Laenge festlegen
  }
  str = p;                      // neuen buffer zuweisen 
}
//void CSlfStr::append(const std::string &s)
//{
//  char*   p;
//
//  if (s.size() == 0) return; // Wenn s leer ist: return *this
//  if (len==0) {
//    len = (uint32_t)s.size();                // neue Laenge festlegen
//    p = new char[len+1];        // neuen Buffer allokieren
//    SlfStrCpyS(p,len+1,s.c_str());
//  } else {
//    len += (uint32_t)s.size();               // neue Laenge festlegen
//    p = new char[len+1];        // neuen Buffer allokieren
//    SlfStrCpyS(p,len+1,str);              // Strings kopieren
//    SlfStrCatS(p,len+1,s.c_str());             
//    delete [] str;              // Alten buffer loeschen 
//  }
//  str = p;                      // neuen buffer zuweisen 
//}
void CSlfStr::append(const char *pstr)
{
  char*   p;

  if (SlfStrLen(pstr) == 0) return; // Wenn s leer ist: return *this
  if (len==0) {
    len = SlfStrLen(pstr);                // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,pstr);
  } else {
    len += SlfStrLen(pstr);               // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,str);              // Strings kopieren
    SlfStrCat(p,len+1,pstr);             
    delete [] str;              // Alten buffer loeschen 
  }
  str = p;                      // neuen buffer zuweisen 
}
void CSlfStr::append(const char u)
{
  char*   p;

  //if (u < 32 ) return; // Kein lesbares Zeichen
  if (len==0) {
    p = new char[2];        // neuen Buffer allokieren
    len  = 1;
    p[0] = u;
    p[1] = '\0';
  } else {
    len += 1;               // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
    SlfStrCpy(p,len+1,str);              // Strings kopieren
    p[len-1] = u;
    p[len]   = '\0';
    delete [] str;              // Alten buffer loeschen 
  }
  str = p;                      // neuen buffer zuweisen 
}

CSlfStr operator+(const CSlfStr &s1, const CSlfStr &s2)
{
  CSlfStr result = s1;

  result += s2; 
  return result;
}

bool CSlfStr::operator==(const CSlfStr &s) const
{
  if (len != s.len) return false;
  if( len == 0 && s.len != 0 ) return false; // Damit wird null abgefangen
  if( len != 0 && s.len == 0 ) return false; // Damit wird null abgefangen
  if( len == 0 && s.len == 0 ) return true; // Damit wird null abgefangen
  return (strcmp(str, s.str)==0); 
}
bool CSlfStr::operator==(char *pstr) const
{

    if( SlfStrLen(pstr) != len ) return false;
    if( len == 0 && SlfStrLen(pstr) != 0 ) return false; // Damit wird null abgefangen
    if( len != 0 && SlfStrLen(pstr) == 0 ) return false; // Damit wird null abgefangen
    if( len == 0 && SlfStrLen(pstr) == 0 ) return true; // Damit wird null abgefangen
    return (strcmp(pstr,str)==0);
}
bool CSlfStr::operator==(const char *pstr) const
{

    if( SlfStrLen(pstr) != len ) return false;
    if( len == 0 && SlfStrLen(pstr) != 0 ) return false; // Damit wird null abgefangen
    if( len != 0 && SlfStrLen(pstr) == 0 ) return false; // Damit wird null abgefangen
    if( len == 0 && SlfStrLen(pstr) == 0 ) return true; // Damit wird null abgefangen
    return (strcmp(pstr,str)==0);
}
bool CSlfStr::operator!=(const CSlfStr &s) const
{
  if (len != s.len) return true;
  if( len == 0 && s.len != 0 ) return true; // Damit wird null abgefangen
  if( len != 0 && s.len == 0 ) return true; // Damit wird null abgefangen
  if( len == 0 && s.len == 0 ) return false; // Damit wird null abgefangen
  return (strcmp(str, s.str)!=0); 
}
bool CSlfStr::operator!=(char *pstr) const
{

    if( SlfStrLen(pstr) != len ) return true;
    if( len == 0 && SlfStrLen(pstr) != 0 ) return true; // Damit wird null abgefangen
    if( len != 0 && SlfStrLen(pstr) == 0 ) return true; // Damit wird null abgefangen
    if( len == 0 && SlfStrLen(pstr) == 0 ) return false; // Damit wird null abgefangen
    return (strcmp(pstr,str)!=0);
}
bool CSlfStr::operator<(const CSlfStr& s) const
{ 
  if (s.len == 0) return false;
  if (len == 0) return true;
  return (strcmp(str,s.str)<0); 
}
bool CSlfStr::compare(const CSlfStr &s)
{
  return SlfStrCompare(this->str,0,this->len,s.str,0,s.len);
}
bool CSlfStr::compare(const char *pstr)
{
  return SlfStrCompare(this->str,0,this->len,pstr,0,SlfStrLen(pstr));
}
bool CSlfStr::compare(const CSlfStr &s,uint32_t pos1, uint32_t len1)
{
  return SlfStrCompare(this->str,pos1,len1,s.str,0,s.size());
}
bool CSlfStr::compare(const char *pstr,uint32_t pos1, uint32_t len1)
{
  return SlfStrCompare(this->str,pos1,len1,pstr,0,SlfStrLen(pstr));
}


CSlfStr::operator char* () const
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
char CSlfStr::getChar(uint32_t pos) {

  if( len == 0 )
    return 0;
  if( pos+1 > len )
    return *(str+len-1);
  
  return *(str+pos);
}
void CSlfStr::clear() {

    if( len > 0 ) {

        delete [] str; 
        str = 0;
        len = 0;
    }
}
CSlfStr &CSlfStr::cut(uint32_t pos0) {

  return CSlfStr::replace("",pos0,len-pos0);
}
CSlfStr &CSlfStr::cut(uint32_t pos0, uint32_t len0) {

  return CSlfStr::replace("",pos0,len0);
}

CSlfStr& CSlfStr::replace(const CSlfStr &s, uint32_t pos0, uint32_t len0) {

	return CSlfStr::replace(s.c_str(),pos0,len0);
  //uint32_t len1;

  //if( pos0+1 <= len )
  //  len1 =  pos0+1;
  //else
  //  len1 = len;
  //
  //len1 += s.size();

  //if( pos0+len0 <= len )
  //  len1 += len-pos0-len0;
  //
  //char *p = new char[len1+1];
  //
  //uint32_t i=0;
  //while(i<pos0){
  //  p[i] = str[i];
  //  i++;
  //};
  //p[i] = (char)0;
  //SlfStrCatS(p,len1+1,s.c_str());
  //if( pos0+len0 <= len )
  //  SlfStrCatS(p,len1+1,str+pos0+len0);

  //if( len > 0 ) {
  //  delete []str;
  //}
  //str = p;
  //len = len1;
  //  
  //return *this;

}
CSlfStr& CSlfStr::replace(const char *pstr, uint32_t pos0, uint32_t len0) {
  uint32_t len1;

  // pos0 geht über die Länge des Strings hinaus
  if( pos0 > len )
    return *this;
  if( pos0+len0 > len )
      len0 = len-pos0;
  
  // Erster Teil aus String, der bestehen bleibt 
  len1 = pos0;  
  // Zweiter Teil aus dem einzufügenden String
  len1 += SlfStrLen(pstr);
  // Dritter Teil aus dem Rest des Strings
  len1 += len-pos0-len0;

  char *p = new char[len1+1];
  
  // Erster Teil
  uint32_t i=0;
  while(i<pos0){
    p[i] = str[i];
    i++;
  };
  p[i] = (char)0;
  // Zweiter Teil
  SlfStrCat(p,len1+1,pstr);
  // Dritter Teil
  if( pos0+len0 < len )
	  SlfStrCat(p,len1+1,str+pos0+len0);

  if( len > 0 ) {
    delete []str;
  }
  str = p;
  len = len1;
    
  return *this;

}
status_t CSlfStr::elimAnf(const char *elim_string/*=" "*/) {
  return SlfStrElimAnf(*this,elim_string);
}
status_t CSlfStr::elimEnd(const char *elim_string/*=" "*/) {
  return SlfStrElimEnd(*this,elim_string);
}
status_t CSlfStr::elimAnfEnd(const char *elim_string/*=" "*/) {
  return SlfStrElimAnfEnd(*this,elim_string);
}
status_t CSlfStr::elimAnfC(const char *elim_string/*=" "*/) {
  return SlfStrElimAnfC(*this,elim_string);
}
status_t CSlfStr::elimEndC(const char *elim_string/*=" "*/) {
  return SlfStrElimEndC(*this,elim_string);
}
status_t CSlfStr::elimAnfEndC(const char *elim_string/*=" "*/) {
  return SlfStrElimAnfEndC(*this,elim_string);
}
status_t CSlfStr::elim(const char *elim_string/*=" "*/) {
  return SlfStrElim(*this,elim_string);
}
uint32_t CSlfStr::find(const char c) {

  return CSlfStr::find(c,0);
}

uint32_t CSlfStr::find(const char c, uint32_t pos) {

  uint32_t i;

  if( pos >= len )
    return CSlfStr::npos;

  for(i=pos;i<len;i++) {
    if( *(str+i) == c )
        return i;
  }
  return CSlfStr::npos;
}
uint32_t CSlfStr::find(const char* pstr) {

  return CSlfStr::find(pstr,0);
}

uint32_t CSlfStr::find(const char* pstr, uint32_t pos) {

  char   *p1 = new char[SlfStrLen(pstr)+1];
  char   *p2;
  uint32_t i0,i;

  if(  (pos >= len)
    || (SlfStrLen(pstr) > len - pos)
    ){
    delete [] p1;
    return CSlfStr::npos;
  }

  SlfStrCpy(p1,SlfStrLen(pstr)+1,pstr);
  p2 = p1;
  i0 = pos;
  for(i=pos;i<len;i++) {
    if( *(str+i) == *p1 ) {
      p1++;
      if( *p1 == (char)0 ){
        delete [] p2;
        return i0;
      }
    } else {
      i0 =i+1;
      p1 = p2;
    }
  }
  delete [] p2;
  return CSlfStr::npos;
}
uint32_t CSlfStr::find(const char* pstr, uint32_t pos0, uint32_t len0) {

    return SlfStrFind(str,pstr,"vs",pos0,len0);
}

uint32_t CSlfStr::find(const CSlfStr &s) {

  return CSlfStr::find(s.c_str(),0);
}
uint32_t CSlfStr::find(const CSlfStr &s, uint32_t pos) {

  return CSlfStr::find(s.c_str(),pos);
}
uint32_t CSlfStr::find(const CSlfStr &s, uint32_t pos0, uint32_t len0) {

    return SlfStrFind(str,s.c_str(),"vs",pos0,len0);
}

uint32_t CSlfStr::find(const char *txtsuch
                    ,const char *suchregel
                    ,const char *txtquot0
                    ,const char *txtquot1
                    ,const char *quotregel
                    ,char istatus) {
    return SlfStrFind(str,txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
}
uint32_t CSlfStr::find(const CSlfStr &s
                    ,const char *suchregel
                    ,const char *txtquot0
                    ,const char *txtquot1
                    ,const char *quotregel
                    ,char istatus) {

    return SlfStrFind(str,s.c_str(),suchregel,txtquot0,txtquot1,quotregel,istatus);
}

uint8_t CSlfStr::findText(CSlfStr &s
                       ,const char *txtquot0
                       ,const char *txtquot1
                       ,const char *suchregel) {

    return SlfStrFindText(s,str,txtquot0,txtquot1,suchregel);

}



uint32_t CSlfStr::findNot(const char c) {

  return CSlfStr::findNot(c,0);
}

uint32_t CSlfStr::findNot(const char c, uint32_t pos) {

  uint32_t i;

  if( pos >= len )
    return CSlfStr::npos;

  for(i=pos;i<len;i++) {
    if( *(str+i) != c )
        return i;
  }
  return CSlfStr::npos;
}
uint32_t CSlfStr::findNot(const char* pstr) {

  return CSlfStr::findNot(pstr,0);
}

uint32_t CSlfStr::findNot(const char* pstr, uint32_t pos) {

  char   *p1 = new char[SlfStrLen(pstr)+1];
  uint32_t i0,i,j;

  if(  (pos >= len)
    || (SlfStrLen(pstr) > len - pos)
    ){
    delete [] p1;
    return CSlfStr::npos;
  }

  SlfStrCpy(p1,SlfStrLen(pstr)+1,pstr);
  i0 = pos;
  for(i=pos;i<len;i++) {
    for(j=0;j<SlfStrLen(p1);j++) {
      if( *(str+i) != *(p1+j) ) {
        delete [] p1;
        return i0;
      }
    }
    i0 =i+1;
  }
  delete [] p1;
  return CSlfStr::npos;
}
uint32_t CSlfStr::findNot(const CSlfStr &s) {

  return CSlfStr::findNot(s.c_str(),0);
}
uint32_t CSlfStr::findNot(const CSlfStr &s, uint32_t pos) {

  return CSlfStr::findNot(s.c_str(),pos);
}

//=========================================================================
//=========================================================================
#if 0
CSlfStr CSlfStr::substr(uint32_t pos0, uint32_t len0) {

  

  if( pos0 < len && len != 0 ) {
    len0 = MIN(len-pos0,len0);
    char  *p = new char[len0+1];
    uint32_t i = 0;
    while(i < len0) { 
      p[i] = str[pos0+i];
      i++;
    }
    p[i] = (char)0;
 
    //sstr=p;
    CSlfStr sstr(p);
    delete [] p;
    return sstr;
  } else {
    CSlfStr sstr="";
    return sstr;
  }

  
}
#endif
//=========================================================================
//=========================================================================
CSlfStr& CSlfStr::formatLeft(uint32_t lformat, char * fil/*=" "*/) {

  if( SlfStrLen(fil) > 0 ) {
    while(lformat>len)
      this->append(fil);
  }
  if( len > lformat )
    this->cut(lformat,len-lformat);

  return *this;

}
/*======================================================================================================*/

void CSlfStr::cat(const CSlfStr &s) {

    this->append(s);
}
void CSlfStr::cat(const char *pstr) {

    this->append(pstr);
}
void CSlfStr::cat(const char *pstr,uint32_t pos0)             {cat(pstr,pos0,SlfStrLen(pstr));}
void CSlfStr::cat(const CSlfStr &s,uint32_t pos0)             {cat(s.c_str(),pos0,s.getLen());}
void CSlfStr::cat(const CSlfStr &s,uint32_t pos0,uint32_t len0) {cat(s.c_str(),pos0,len0);}
void CSlfStr::cat(const char *pstr,uint32_t pos0,uint32_t len0) {

  char*   p;
  uint32_t  i;

  if( SlfStrLen(pstr) < pos0) return; // ist leer oder nicht im Bereich: return

  if( pos0+len0-1 > SlfStrLen(pstr)  )
	len0 = (SlfStrLen(pstr)+1)-pos0;
      
	len += len0;                // neue Laenge festlegen
    p = new char[len+1];        // neuen Buffer allokieren
	for(i=0;i<SlfStrLen(str);i++)
		p[i] = str[i];
	for(i=0;i<len0;i++)
		p[SlfStrLen(str)+i] = pstr[pos0+i];
	p[i] = '\0';
	if( str )
		delete [] str;              // Alten buffer loeschen 
  
    str = p;                      // neuen buffer zuweisen 

}
status_t CSlfStr::catFormat(char *pformat,...) {
    
    SSlfStrFormatListe *pformat_liste=0;
    SSlfStrFormatListe *p_f_l,*p_f_l1;
    uint32_t      n_format_liste;
    CSlfStr  format_text;
    
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
        printf("Error in <CSlfStr::catFormat> aufgetreten\n");
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
                  
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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

                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
                      uint32_t  ll = MAX(
                                       (uint32_t)SlfStrLen(p_string)
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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

                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
status_t CSlfStr::change(char *textsuch,char *textchange) {

        return SlfStrChange(*this,textsuch,textchange);
}
status_t CSlfStr::insert(char *textins,uint32_t pos0) {
    return insert(textins,pos0,SlfStrLen(textins));
}
status_t CSlfStr::insert(char *textins,uint32_t pos0, uint32_t len0) {

    char  *str1;
    uint32_t i;

    if( pos0 >= len )
        pos0 = len;

    if( len0 > SlfStrLen(textins) )
        len0 = SlfStrLen(textins);

    str1 = new char[len+len0+1];
    
    for(i=0;i<pos0;i++)
        str1[i] = str[i];

    for(i=0;i<len0;i++)
        str1[pos0+i] = textins[i];

    for(i=pos0;i<len;i++)
        str1[len0+i] = str[i];

    str1[len+len0] = '\0';

    delete []str;

    len += len0;
    str =  str1;

    return OKAY;
}
uint8_t CSlfStr::isAlphNum(void) {

    for(uint32_t i=0;i<len;i++) {
        if( !isalnum(str[i]) && !(str[i]=='_') ) // within the ranges A - Z, a - z, or 0 - 9
            return 0;
    }
    return 1;
}
//===========================================================
// Vektor-String-Klasse
//===========================================================
CSlfStrV::CSlfStrV() {
    
    pStr = 0;
    NRow = 0;
}
CSlfStrV::CSlfStrV(const char* p) 
{
    NRow      = 1;
    pStr = new CSlfStr[NRow];

    pStr[0] = p;
}
CSlfStrV::CSlfStrV(CSlfStr &str) 
{

    NRow = 1;
    pStr = new CSlfStr[NRow];

    pStr[0] = str;
}
CSlfStrV::CSlfStrV(CSlfStrV &strv) 
{

    NRow = strv.getNrows();
    if( NRow > 0 ) {
        pStr = new CSlfStr[NRow];

        for(uint32_t i=0;i<NRow;i++)
            pStr[i] = strv.get_str(i);
    } else {
        pStr = 0;
    }
}
CSlfStrV::~CSlfStrV() {

    if( pStr != 0)
        delete []pStr;
}
void CSlfStrV::clear() {

    if( pStr != 0)
        delete []pStr;

    pStr = 0;
    NRow = 0;
}
status_t CSlfStrV::make(uint32_t nrow) {

    if( pStr == 0) {

        NRow = nrow;
        if( NRow > 0 )
            pStr = new CSlfStr[NRow];
    } else if( nrow == 0 ) {

        delete []pStr;

        pStr = 0;
        NRow = 0;
    } else {

        CSlfStr *pStr1 = new CSlfStr[nrow];
            
            for(uint32_t irow=0;irow<nrow;irow++) {

                if( irow < NRow )
                    
                    pStr1[irow] = pStr[irow];
                else
                    pStr1[irow] = "";
            }
        

        delete []pStr;

        pStr = pStr1;
        NRow = nrow;

    }
    return OKAY;
}
status_t CSlfStrV::cutRow(uint32_t irow) {


    if( irow >= NRow ) 
        return OKAY;

        for(uint32_t ir=irow+1;ir<NRow;ir++) {

            pStr[ir-1] = pStr[ir];
        }
    return make(NRow-1);
}

uint8_t CSlfStrV::find(const CSlfStr &s,uint32_t *pirow) {

    return find(s.c_str(),pirow);
}
uint8_t CSlfStrV::find(const char* pstr,uint32_t *pirow) {

    
//    if( *pirow >= NRow )
//        return NOT_OKAY;

    for(uint32_t i=0;i<NRow;i++) {


            if( pStr[i].compare(pstr) ) {
                *pirow = i;
                return 1;
            }
        
    }
    return 0;
}
uint8_t CSlfStrV::exist(const CSlfStr &s) {

    return exist(s.c_str());
}
uint8_t CSlfStrV::exist(const char* pstr) {

    
    for(uint32_t i=0;i<NRow;i++) {
            if( pStr[i].compare(pstr) ) {
                return 1;
            }
        
    }
    return 0;
}
uint8_t CSlfStrV::exist_last(const CSlfStr &s) {

    return exist_last(s.c_str());
}
uint8_t CSlfStrV::exist_last(const char* pstr) {

    if( NRow )
        if( pStr[NRow-1].compare(pstr) ) 
            return 1;
    return 0;
}
void CSlfStrV::append(CSlfStrV &sv){


    for(uint32_t i=0;i<sv.getNrows();i++)
        append(sv.get_str(i));
    
}

void CSlfStrV::append(const CSlfStr &s) {
    append(s.c_str());
}
void CSlfStrV::append(const char *str) {

    make(NRow+1);

    pStr[NRow-1] = str;
}
void CSlfStrV::delete_last(void) {

	if( NRow > 0 ) 
		make(NRow-1);
}
void CSlfStrV::cat(const CSlfStr &s, uint32_t irow) {
    cat(s.c_str(),irow);
}
void CSlfStrV::cat(const char *str, uint32_t irow) {

    if( irow >= NRow ) {
        make(irow+1);
    }

    pStr[irow].append(str);
}
void CSlfStrV::cpy(const CSlfStr &s, uint32_t irow) {
    cat(s.c_str(),irow);
}
void CSlfStrV::cpy(const char *str, uint32_t irow) {

    if( irow >= NRow ) {
        make(irow+1);
    }

    pStr[irow] = str;
}
void CSlfStrV::insert(const CSlfStr &s, uint32_t irow) {
    insert(s.c_str(),irow);
}
void CSlfStrV::insert(const char *str, uint32_t irow) {

  uint32_t i;
  make(NRow+1);
  if( irow < NRow )
  {
    for(i=NRow-1;i>irow;i--)
    {
      pStr[i] = pStr[i-1];
    }
    pStr[irow] = str;
  }
}
char *CSlfStrV::get_str(uint32_t irow) {

    if( irow >= NRow )
        return 0;
    else
        return pStr[irow].c_str();
}
CSlfStr *CSlfStrV::get(uint32_t irow) {

    if( irow >= NRow )
        return 0;
    else
        return &pStr[irow];
}
char *CSlfStrV::get_last_str(void) {

  if( NRow == 0 )
  {
    return 0;
  }
  else
  {
    return pStr[NRow-1].c_str();
  }
}
CSlfStr *CSlfStrV::get_last(void) {

  if( NRow == 0 )
  {
    return 0;
  }
  else
  {
    return &pStr[NRow-1];
  }
}
CSlfStrV& CSlfStrV::operator=(CSlfStrV& sv)
{
  clear();
  
  if( sv.getNrows() > 0 ) {

      for(uint32_t i=0;i<sv.getNrows();i++) {
          append(sv.get_str(i));
      }
  }
  return *this;                 // return *this
}
bool CSlfStrV::compare(CSlfStrV &sv)
{
  if(sv.getNrows() != NRow) return false;

  for(uint32_t i=0;i<sv.getNrows();i++) {
      
      if( !pStr[i].compare(sv.get_str(i)) )
          return false;
  }

  return true;   
}
bool CSlfStrV::operator==(CSlfStrV &sv) const
{
  if(sv.getNrows() != NRow) return false;

  for(uint32_t i=0;i<sv.getNrows();i++) {
      
      if( !pStr[i].compare(sv.get_str(i)) )
          return false;
  }

  return true;   
}
bool CSlfStrV::operator!=(CSlfStrV &sv) const
{
  if(sv.getNrows() != NRow) return true;

  for(uint32_t i=0;i<sv.getNrows();i++) {
      
      if( !pStr[i].compare(sv.get_str(i)) )
          return true;
  }

  return false;   
}
uint8_t CSlfStrV::isAnyIdentical(void) {

  for(uint32_t i=0;i<NRow;i++) {
      
	  for(uint32_t j=0;j<NRow;j++) {

		  if( i != j && pStr[i].compare(pStr[j]) )
			  return 1;
	  }
  }

  return 0;   
}


  
//===========================================================
// Matrix_t-String-Klasse
//===========================================================
CSlfStrM::CSlfStrM() {
    
    pStr = 0;
    NRow = 0;
    NCol = 0;
}
CSlfStrM::CSlfStrM(const char* p) 
{
    Len = 1;
    pStr = new CSlfStr[Len];

    pStr[0,0] = p;
    NRow      = 1;
    NCol      = 1;
}
CSlfStrM::CSlfStrM(CSlfStr &str) 
{

    Len = 1;
    pStr = new CSlfStr[Len];

    pStr[0,0] = str;
    NRow      = 1;
    NCol      = 1;
}
CSlfStrM::~CSlfStrM() {

    if( pStr != 0)
        delete []pStr;
}
void CSlfStrM::clear() {

    if( pStr != 0)
        delete []pStr;

    pStr = 0;
    NRow = 0;
    NCol = 0;
    Len  = 0;
}
status_t CSlfStrM::make(uint32_t nrow, uint32_t ncol) {

    if( pStr == 0) {

        Len = nrow*ncol;
        if( Len > 0 )
            pStr = new CSlfStr[Len];
        NRow      = nrow;
        NCol      = ncol;
    } else if( ncol == 0 || nrow == 0 ) {

        delete []pStr;

        Len  = 0;
        pStr = 0;
        NRow = 0;
        NCol = 0;
    } else {

        uint32_t Len1 = nrow * ncol;
        CSlfStr *pStr1 = new CSlfStr[Len1];
            
        for(uint32_t icol=0;icol<ncol;icol++) {

            for(uint32_t irow=0;irow<nrow;irow++) {

                if( icol < NCol && irow < NRow )
                    
                    pStr1[icol*nrow+irow] = pStr[icol*NRow+irow];
                else
                    pStr1[icol*nrow+irow] = "";
            }
        }

        delete []pStr;

        Len  = nrow*ncol;
        pStr = pStr1;
        NRow = nrow;
        NCol = ncol;

    }
    return OKAY;
}
status_t CSlfStrM::cutRow(uint32_t irow) {


    if( irow >= NRow ) 
        return OKAY;

    for(uint32_t ic=0;ic<NCol;ic++) {

        for(uint32_t ir=irow+1;ir<NRow;ir++) {

            pStr[ic*NRow+ir-1] = pStr[ic*NRow+ir];
        }
    }
    return make(NRow-1,NCol);
}

status_t CSlfStrM::cutCol(uint32_t icol) {


    if( icol >= NCol ) 
        return OKAY;

    for(uint32_t ic=icol+1;ic<NCol;ic++) {
        
        for(uint32_t ir=0;ir<NRow;ir++) {

            pStr[(ic-1)*NRow+ir] = pStr[ic*NRow+ir];
        }
    }
    return make(NRow,NCol-1);

    return OKAY;
}
status_t CSlfStrM::find(const CSlfStr &s,uint32_t *pirow, uint32_t *picol) {

    return find(s.c_str(),pirow,picol);
}
status_t CSlfStrM::find(const char* pstr,uint32_t *pirow, uint32_t *picol) {

    
    if( *pirow >= NRow || *picol >= NCol )
        return NOT_OKAY;

    for(uint32_t i=0;i<NRow;i++) {

        for(uint32_t j=0;j<NCol;j++) {

            if( pStr[j*NRow+i].compare(pstr) ) {
                *pirow = i;
                *picol = j;
                return OKAY;
            }
        }
    }
    return NOT_OKAY;
}
void CSlfStrM::cat(const CSlfStr &s, uint32_t irow, uint32_t icol) {
    cat(s.c_str(),irow,icol);
}
void CSlfStrM::cat(const char *str, uint32_t irow, uint32_t icol) {

    if( irow >= NRow || icol >= NCol ) {
        make(MAX(irow+1,NRow),MAX(icol+1,NCol));
    }

    pStr[icol*NRow+irow].append(str);
}
void CSlfStrM::cpy(const CSlfStr &s, uint32_t irow, uint32_t icol) {
    cat(s.c_str(),irow,icol);
}
void CSlfStrM::cpy(const char *str, uint32_t irow, uint32_t icol) {

    if( irow >= NRow || icol >= NCol ) {
        make(MAX(irow+1,NRow),MAX(icol+1,NCol));
    }

    pStr[icol*NRow+irow] = str;
}
char *CSlfStrM::get_str(uint32_t irow, uint32_t icol) {

    if( irow >= NRow || icol >= NCol )
        return 0;
    else
        return pStr[icol*NRow+irow].c_str();
}
CSlfStr *CSlfStrM::get(uint32_t irow, uint32_t icol) {

    if( irow >= NRow || icol >= NCol )
        return 0;
    else
        return &pStr[icol*NRow+irow];
}
void CSlfStrM::transpone(void) {

    if( NRow > 1 || NCol > 1 ) {

        uint32_t Len1 = NRow * NCol;
        uint32_t nrow = NRow;
        uint32_t ncol = NCol;
        CSlfStr *pStr1 = new CSlfStr[Len1];
            
        for(uint32_t icol=0;icol<ncol;icol++) {

            for(uint32_t irow=0;irow<nrow;irow++) {
                
                pStr1[irow*ncol+icol] = pStr[icol*nrow+irow];
            }
        }

        delete []pStr;

        Len  = Len1;
        pStr = pStr1;
        NRow = ncol;
        NCol = nrow;
    }
}
CSlfStrM& CSlfStrM::operator=(CSlfStrM& sm)
{
  clear();
  
  if( sm.getNrows() > 0 && sm.getNcols() > 0 ) {

      for(uint32_t i=0;i<sm.getNrows();i++) {
          for(uint32_t j=0;j<sm.getNcols();j++) {
            cat(sm.get_str(i,j),i,j);
          }
      }
  }
  return *this;                 // return *this
}
//===========================================================
// Stringbearbeitungs-Funktionen
//===========================================================
//
bool SlfStrCompare(const CSlfStr &s0,const CSlfStr &s1) {
  return SlfStrCompare(s0.c_str(), 0, s0.getLen()
                     ,s1.c_str(), 0, s1.getLen());
}
bool SlfStrCompare(const char *str0,const char *str1) {
  return SlfStrCompare(str0, 0, SlfStrLen(str0), str1, 0, SlfStrLen(str1));
}
bool SlfStrCompare(const CSlfStr &s0, uint32_t pos0, uint32_t len0
                ,const CSlfStr &s1, uint32_t pos1, uint32_t len1) {
  return SlfStrCompare(s0.c_str(), pos0, len0, s1.c_str(), pos1, len1);
}
bool SlfStrCompare(const char *str0, uint32_t pos0, uint32_t len0
                ,const char *str1, uint32_t pos1, uint32_t len1) {

  int c0, c1;
  uint32_t n=0;
  
  len0 = MIN(SlfStrLen(str0),len0); 
  len1 = MIN(SlfStrLen(str1),len1);

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
uint32_t SlfStrCompareCount(const CSlfStr &s0,const CSlfStr &s1) {
  return SlfStrCompareCount(s0.c_str(), 0, s0.getLen()
                     ,s1.c_str(), 0, s1.getLen());
}
uint32_t SlfStrCompareCount(const char *str0,const char *str1) {
  return SlfStrCompareCount(str0, 0, SlfStrLen(str0), str1, 0, SlfStrLen(str1));
}
uint32_t SlfStrCompareCount(const CSlfStr &s0, uint32_t pos0, uint32_t len0
                ,const CSlfStr &s1, uint32_t pos1, uint32_t len1) {
  return SlfStrCompareCount(s0.c_str(), pos0, len0, s1.c_str(), pos1, len1);
}
uint32_t SlfStrCompareCount(const char *str0, uint32_t pos0, uint32_t len0
                         ,const char *str1, uint32_t pos1, uint32_t len1) {
  int c0, c1;
  uint32_t n=0;
  
  len0 = MIN(SlfStrLen(str0),len0); 
  len1 = MIN(SlfStrLen(str1),len1);
  
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


status_t SlfStrElimAnf(CSlfStr &text,const char *elim_char/*=" "*/) {
	status_t status=OKAY;

  CSlfStr elim_string(elim_char);
  uint32_t i=0;
  uint32_t l0 = elim_string.size();

  for(i=0;i<text.size();i+=l0) {
    if( !text.compare(elim_string,i, l0) ) { // Wenn das erste mal nicht gefunden
                                                 // reduzieren
      text.replace("",0, i);
      break;
    }
  }

  return OKAY;
}
status_t SlfStrElimEnd(CSlfStr &text,const char *elim_char/*=" "*/) {

  CSlfStr elim_string(elim_char);
  uint32_t i,i2;
  uint32_t l0 = elim_string.size();
  uint32_t l1 = text.size();
  uint32_t l2;
  bool flag;
  
  if( l1 >= l0 ) {
    i    = l1-l0;
    i2   = SLF_STR_NPOS;
    l2   = 0;
    flag = true;
    while( flag ) {
      if( !text.compare(elim_string,i,l0) ) {
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
    text.replace("",i2,l2);
  }
  return OKAY;
}
status_t SlfStrElimAnfEnd(CSlfStr &text,const char *elim_char/*=" "*/) {
  status_t status=OKAY;
  status = SlfStrElimAnf(text,elim_char);
  if( status == OKAY )
    status = SlfStrElimEnd(text,elim_char);
  return status;
}
status_t SlfStrElimAnfC(CSlfStr &text,const char *elim_char/*="\t "*/) {

  CSlfStr    elim_string(elim_char);
  uint32_t i;
  bool           search_flag = true;

  while( search_flag && text.size() > 0 ) {
    search_flag = false;
    for(i=0;i<elim_string.size();i++) {

      if( text[(uint32_t)0] == elim_string[i] ) {
        text.cut(0,1);
        search_flag = true;
        break;
      }
    }
  }
  return OKAY;
}
status_t SlfStrElimEndC(CSlfStr &text,const char *elim_char/*=" "*/) {

  CSlfStr    elim_string(elim_char);
  uint32_t i;
  bool           search_flag = true;

  while( search_flag && text.size() > 0 ) {
    search_flag = false;
    for(i=0;i<elim_string.size();i++) {

      if( text[text.size()-1] == elim_string[i] ) {
        text.cut(text.size()-1,1);
        search_flag = true;
        break;
      }
    }
  }
  return OKAY;
}
status_t SlfStrElimAnfEndC(CSlfStr &text,const char *elim_char/*=" "*/) {
  status_t status=OKAY;
  status = SlfStrElimAnfC(text,elim_char);
  if( status == OKAY )
    status = SlfStrElimEndC(text,elim_char);
  return status;
}
status_t SlfStrElim(CSlfStr &text,const char *elim_string/*=" "*/) {

    uint32_t i0;

    while( (i0=SlfStrFind(text,elim_string,"vs")) != SLF_STR_NPOS )
        text.cut(i0,SlfStrLen(elim_string));
    return OKAY;
}
uint32_t SlfStrLen (const char *s)
{
  uint32_t n = 0;
  while (s != NULL && *s++ != '\0')
    {
      ++n;
    }
  return n;
}
uint32_t SlfStrLen (CSlfStr &s) {

    return s.getLen();
}

char * SlfStrCpy (char * dest, uint32_t len, const char *  src)
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
char * SlfStrCpy (char * dest, uint32_t len, const char *  src, uint32_t pos0, uint32_t len0)
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
char * SlfStrCpy (char * dest, uint32_t len, CSlfStr  &ssrc, uint32_t pos0, uint32_t len0) {

  return SlfStrCpy(dest,len,ssrc.c_str(),pos0,len0);
}
char * SlfStrCpy (CSlfStr  &sdest, CSlfStr  &ssrc, uint32_t pos0, uint32_t len0) {

  char *p = new char[len0+1];
  SlfStrCpy(p,len0+1,ssrc,pos0,len0);
  sdest = p;
  delete []p;

  return sdest.c_str();
}
char * SlfStrCpy (CSlfStr  &sdest, const char *  src, uint32_t pos0, uint32_t len0) {
  char *p = new char[len0+1];
  SlfStrCpy(p,len0+1,src,pos0,len0);
  sdest = p;
  delete []p;

  return sdest.c_str();
}
char * SlfStrFindQuotCpy(CSlfStr  &sdest, CSlfStr  &ssrc,char *txtquot0, char *txtquot1) {

    uint32_t i0,i1;

    i0 = SlfStrFind(ssrc, txtquot0,"vs");
    if( i0 == SLF_STR_NPOS )
        return NULL;

    i0 += SlfStrLen(txtquot0);
        
    i1 = SlfStrFind(ssrc, txtquot1,"vs",i0);
    
    if( i1 == SLF_STR_NPOS )
        return NULL;
    
    if( i1 > 0 )
        i1 -= 1;

    if( i1 > i0 ) {
        SlfStrCpy(sdest,ssrc, i0, i1-i0+1);        
        return sdest.c_str();
    }

    return NULL;

}
char * SlfStrCat (char * dest, uint32_t len, const char * src)
{
  char *d = dest;
  int c;
  uint32_t len0=0;
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
error_t SlfStr2Num(const char *p, double *pdval , char **pp_stop/*=NULL*/) {
  char *p1;
  error_t err = NO_ERR;
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
error_t SlfStr2Num(const char *p, uint8_t *puival , char **pp_stop/*=NULL*/) {
  double dval;
  error_t err = SlfStr2Num(p,&dval,pp_stop);
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
error_t SlfStr2Num(const char *p, sint8_t *pival , char **pp_stop/*=NULL*/) {
  double dval;
  error_t err = SlfStr2Num(p,&dval,pp_stop);
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
error_t SlfStr2Num(const char *p, uint16_t *puival , char **pp_stop/*=NULL*/) {
  double dval;
  error_t err = SlfStr2Num(p,&dval,pp_stop);
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
error_t SlfStr2Num(const char *p, sint16_t *pival , char **pp_stop/*=NULL*/) {
  double dval;
  error_t err = SlfStr2Num(p,&dval,pp_stop);
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
error_t SlfStr2Num(const char *p, uint32_t *puival , char **pp_stop/*=NULL*/) {
  double dval;
  error_t err = SlfStr2Num(p,&dval,pp_stop);
  if( err == NO_ERR ) {
    dval = fabs(dval+0.5);
    if( dval > (double)MAX_UINT32 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MAX_UINT32;
    } else if( dval < (double)MIN_UINT32 ) {
      err   = OVER_UNDER_FLOW;
      *puival = MIN_UINT32;
    } else {
      *puival = (uint32_t)dval;
    }
  }
  return err;
}
error_t SlfStr2Num(const char *p, sint32_t *pival , char **pp_stop/*=NULL*/) {
  double dval;
  error_t err = SlfStr2Num(p,&dval,pp_stop);
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
/*======================================================================================================*/
status_t SlfStrCatFormat(CSlfStr &text, char *pformat,...) {
    
    SSlfStrFormatListe *pformat_liste=0;
    SSlfStrFormatListe *p_f_l,*p_f_l1;
    uint32_t      n_format_liste;
    CSlfStr  format_text;
    
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
                  
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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

                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  uint32_t  ll = MAX((uint32_t)SlfStrLen(p_string),p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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

                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
            
                  int  ll = MAX(SLF_FORMAT_DEFAULT_LENGTH,p_f_l->width)+1;
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
status_t SlfStrSetFormatListe(char *pformat
                             ,SSlfStrFormatListe **ppformat_liste
                             ,uint32_t *n_format_liste) {
    
    uint32_t n;
    uint32_t i,itype;
    uint32_t i1,i2,ifound;
    SSlfStrFormatListe *pformat_liste=0;
    CSlfStr format;
    CSlfStr text;
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
        
        i1 = format.find("%");
        i2 = format.find("%%");;
        
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
                
              i2 = format.find(format_typ[i]);
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

              i1=text.find(".");
              text.cut(i1,text.getLen()-i1);
              if( text.size() > 0 )
                pformat_liste->width = (uint32_t)atol(text.c_str());                
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

status_t SlfStrChange(CSlfStr &text,char *textsuch,char *textchange) {

    uint32_t i=0;
    if(  strcmp(textsuch,textchange) != 0 
      && *textsuch != '\0' ) {

        while( i = text.find(textsuch,i) ) {

            text.cut(i,SlfStrLen(textsuch));
            text.insert(textchange,i,SlfStrLen(textchange));

            i = i+1-SlfStrLen(textsuch)+SlfStrLen(textchange);
        }
    }
    return OKAY;
}
uint32_t SlfStrFind(CSlfStr &str
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
uint32_t SlfStrFind(const char *txt
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
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0) {
    
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
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0
                 ,uint32_t len0
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
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0) {
    
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

uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0
                 ,uint32_t len0
                 ) {
/*
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

     return(SLF_STR_NPOS)  nicht gefunden ansonsten
     return(i)             Stelle wo absolut gefunden beginnend mit 0
*/

  uint32_t ind,i,j;
  uint32_t lentxt,lentsuch;
  uint8_t  *ivsuch=NULL;
  char run_flag,found_flag,sflag;
  
  if( SlfStrLen(txt)      == 0 ) return SLF_STR_NPOS;
  if( SlfStrLen(txtsuch)  == 0 ) return SLF_STR_NPOS;
  if( SlfStrLen(txtquot0) == 0 ) return SLF_STR_NPOS;
  if( SlfStrLen(txtquot1) == 0 ) return SLF_STR_NPOS;

  lentxt   = MIN(len0,SlfStrLen(txt+pos0));
  lentsuch = SlfStrLen(txtsuch);

  ivsuch = (uint8_t *)malloc(lentxt*sizeof(uint8_t));

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

  free(ivsuch);
        
  return SLF_STR_NPOS;
}
void SlfStrFindQuoted(const char *txt      // Text der zu durchkämmen
                     ,const char *txtq0    // Start-Quot
                     ,const char *txtq1    // End-Quot
                     ,const char *quotregel// "i": innerhalb quot mit einsen belegen
                                           // "a": außerhalb quot mit -"-
                     ,uint32_t pos0          // Start, wo gesucht wird
                     ,uint32_t len0          // Länge, des Stücks zum Suchen
                     ,uint8_t  *pmarker      // VEktor mit länge len0
                     ,char   istatus       // 0:startet ausserhalb quot
                                           // 1:startet innerhalb quot
                     ) {

    
  uint32_t ltxt = MIN(len0,SlfStrLen(txt+pos0));
  uint32_t lq0  = SlfStrLen(txtq0);
  uint32_t lq1  = SlfStrLen(txtq1);
                     
  uint32_t ind    = 0;
  uint32_t i;
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
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ) {
    return SlfStrFind(str.c_str(),txtsuch,regel,0,str.size());
}
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0
                 ) {
    return SlfStrFind(str.c_str(),txtsuch,regel,pos0,str.getLen()-pos0);
}
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0
                 ,uint32_t len0
                 ) {
    return SlfStrFind(str.c_str(),txtsuch,regel,pos0,len0);
}
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ) {
    return SlfStrFind(txt,txtsuch,regel,0,SlfStrLen(txt));
}
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0
                 ) {
    return SlfStrFind(txt,txtsuch,regel,pos0,SlfStrLen(txt)-pos0);
}

uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0
                 ,uint32_t len0
                 ) {
  uint32_t l2;
  uint32_t    i,j,k;
  uint32_t    pos1;
  
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
uint8_t SlfStrFindText(CSlfStr &s
                    ,const char *txt
                    ,const char *txtquot0
                    ,const char *txtquot1
                    ,const char *suchregel) {

    uint32_t i0;
    uint32_t i1;

    if( SlfStrLen(txtquot0) == 0 ) 

        i0 = 0;

    else if( (i0=SlfStrFind(txt,txtquot0,suchregel)) == SLF_STR_NPOS )

        return 0;


    i0 = i0+SlfStrLen(txtquot0);


    if(  (i1=SlfStrFind(txt,txtquot1,suchregel,i0)) != SLF_STR_NPOS ) {
          
            
        SlfStrCpy(s,txt,i0,i1-i0);

        return 1;

    } else {

        SlfStrCpy(s,txt,i0,SlfStrLen(txt)-i0);

        return 2;
            
    }


    return 0;
}

//String-Vektor-Funktionen =================
//==========================================
uint32_t SlfStrVSplit( CSlfStrV &strv, const char*t, const char *tsplit/*=" "*/){

    uint32_t i0 = 0;
    uint32_t i1;
    uint32_t len = SlfStrLen(t);
    uint32_t lents = SlfStrLen(tsplit);

    CSlfStr str;

    strv.clear();

    while( SlfStrFind(t,tsplit,"vs",i0) == 0 )
        i0 += lents;

    while( i0<len ){

        if( (i1 = SlfStrFind(t,tsplit,"vs",i0)) != SLF_STR_NPOS ) {

            SlfStrCpy(str, t, i0, i1-i0);
        
            strv.append(str);

            i0 = i1 + lents;
        } else {

            if( i0 < len ) {
                SlfStrCpy(str, t, i0, len-i0);
                strv.append(str);
            }
            i0 = len;
        }
    }

    return strv.getNrows();
}
uint32_t SlfStrVSplit( CSlfStrV &strv, const char*t, const char *tsplit,const char *quot0,const char *quot1) {

    uint32_t i0 = 0;
    uint32_t i1;
    uint32_t len = SlfStrLen(t);
    uint32_t lents = SlfStrLen(tsplit);

    CSlfStr str;

    strv.clear();


    while( SlfStrFind(t,tsplit,"vs",quot0,quot1,"a",0,i0) == 0 )
        i0 += lents;

    while( i0<len ){

        if( (i1 = SlfStrFind(t,tsplit,"vs",quot0,quot1,"a",0,i0)) != SLF_STR_NPOS ) {

            SlfStrCpy(str, t, i0, i1-i0);
        
            strv.append(str);

            i0 = i1 + lents;
        } else {

            if( i0 < len ) {
                SlfStrCpy(str, t, i0, len-i0);
                strv.append(str);
            }
            i0 = len;
        }
    }

    return strv.getNrows();
}

//Sonstige-Funktionen ======================
//==========================================
char* SlfCharCpy (char *dest, const char *src, uint32_t len)
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
char * SlfCharSet (char * dest, char c, uint32_t len)
{
  unsigned char *ptr = (unsigned char*)dest;
  while (len-- > 0)
    *ptr++ = c;
  return dest;
}
bool   SlfCharIsAlphNum(char c)
{
  if( ((c>64) && (c<91)) || ((c>96) && (c<123)) ) return true;
  else                                            return false;
}
bool   SlfStrIsAlphNum(char *src)
{
  for(uint32_t i=0;i<SlfStrLen(src);i++)
  {
    if( !SlfCharIsAlphNum(*(src+i)) )
    {
      return false;
    }
  }
  return true;
}
bool   SlfStrIsAlphNum(CSlfStr &s)
{
  return SlfStrIsAlphNum(s.c_str());
}

/*==============================================================================*/
/* Extrahiert aus name der Regel Pfad, File und Extension und kopiert es in die  */
/* Stringobjekt s. In regel kann stehen:                               */
/* "p"    Pfad, "f" wie filename und "e" wie extension                          */
/* z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn         */
/* vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt*/
/*==============================================================================*/
status_t SlfStrExtractPfe(CSlfStr &s,const char *name,const char *regel) {

    const char *ptxt;
    uint32_t itrennz,iext;
    CSlfStr trennz;   /* Trennzeichen */

    /* Zuallesrt den string in p leeren */
	s = "";
    
    /* Zuallerst den einfachsten Konstrukt rausfiltern */
    if( SlfStrCompare(name,"") ) {
        return NOT_OKAY;
    }

    /* Dann einfache Pfadkonstrukte rausfiltern */
    if( SlfStrCompare(name,".")  ||  SlfStrCompare(name,"..") ) {

        if(  (SlfStrLen(regel) == 1)
	      && (	(SlfStrFind(regel,"p","vs")!=SLF_STR_NPOS)
             || (SlfStrFind(regel,"P","vs")!=SLF_STR_NPOS) 
			 )
		  ) {
			s = name;
			return OKAY;
		}
    }

    /* Pfadtrennzeichen extrahieren */
	if( SlfStrFind(name,"\\","vs") != SLF_STR_NPOS )
		trennz = "\\";
	else if( SlfStrFind(name,"/","vs") != SLF_STR_NPOS )
		trennz = "/";
	else
		trennz = "\\";

    /* letztes Trennzeichen suchen */
    if( (itrennz=SlfStrFind(name,trennz,"rs"))!=SLF_STR_NPOS ) {

        if( (SlfStrFind(regel,"p","vs")!=SLF_STR_NPOS) ||
            (SlfStrFind(regel,"P","vs")!=SLF_STR_NPOS) ){

			// anhängen einschliesslich Trennzeichen
			s.cat(name,0,itrennz+trennz.getLen());
	
        }
        ptxt = &(name[itrennz+1]);
    } else {

		//kein .\ setzen
        //if( (SlfStrFind(regel,"p","vs")!=SLF_STR_NPOS) ||
        //    (SlfStrFind(regel,"P","vs")!=SLF_STR_NPOS) ){

		//	s.cat(".");
		//	s.cat(trennz);
        //}
        ptxt = name;
    }

    /* Jetzt in ptxt extension suchen */
    if( (iext=SlfStrFind(ptxt,".","vs"))!=SLF_STR_NPOS ) {

        if( (SlfStrFind(regel,"f","vs")!=SLF_STR_NPOS) ||
            (SlfStrFind(regel,"F","vs")!=SLF_STR_NPOS) ){

            if( (SlfStrFind(regel,"e","vs")!=SLF_STR_NPOS) ||
                (SlfStrFind(regel,"E","vs")!=SLF_STR_NPOS) ){

				s.cat(ptxt);
			} else if( iext>0 ) 
				s.cat(ptxt,0,iext);
        } else 
        if( (SlfStrFind(regel,"e","vs")!=SLF_STR_NPOS) ||
            (SlfStrFind(regel,"E","vs")!=SLF_STR_NPOS) ){
            
            if( SlfStrLen(ptxt) > iext )
				s.cat(ptxt,iext+1,SlfStrLen(ptxt)-iext);
        }
    } else {

        if( (SlfStrFind(regel,"f","vs")!=SLF_STR_NPOS) ||
            (SlfStrFind(regel,"F","vs")!=SLF_STR_NPOS) ){

            s.cat(ptxt);

        }
    }

    return OKAY;
}
