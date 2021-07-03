//
// StringHelp.h
//
// Funktionen auf die std::string-Klasse
#ifndef HLP_STRING_H
#define HLP_STRING_H


#include <string>
#include <vector>

namespace Hlp
{
  extern const std::size_t npos;
  // StringFind einfache Suche
  //----------------------------------------------------------------------------------------------------
  // suchregel   = "vs", "rs", "vn", "rn"
  // txtsuch zu suchenden Text
  // po0     Offset im zu suchenden Text
  // len0    Länge  -"-
  std::size_t StringFind(std::string &str, const char *txtsuch, const char *suchregel);
  std::size_t StringFind(const char *txt, const char *txtsuch, const char *suchregel);
  // Suche mit Anfang bis Ende
  std::size_t StringFind(std::string &str, std::size_t pos0, const char *txtsuch, const char *suchregel);
  std::size_t StringFind(const char *txt, std::size_t pos0, const char *txtsuch, const char *suchregel);
  // Suche mit Anfang und Länge
  std::size_t StringFind(std::string &str, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *suchregel);
  std::size_t StringFind(const char *txt, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *suchregel);

  std::size_t StringFindCount(std::string &str,const char *txtsuch);
  std::size_t StringFindCount(const char *txt,const char *txtsuch);
  std::size_t StringFindCount(std::string &str, std::size_t pos0, const char *txtsuch);
  std::size_t StringFindCount(const char *txt, std::size_t pos0, const char *txtsuch);
  std::size_t StringFindCount(std::string &str, std::size_t pos0, std::size_t len0, const char *txtsuch);
  std::size_t StringFindCount(const char *txt, std::size_t pos0, std::size_t len0, const char *txtsuch);


  // StringFindQuot
  //------------------------------------------------------------------------------------------------------------
  //
  // Sucht Text im Quot nach suchregel
  // txtquot0   vordere quot für eingeschlossenen Text   [abc] oder "abc"
  // txtquot1   hinterer quot für eingeschlossenen Text
  // Rückgabe 0 , wenn nicht gefunden    => kein Text in s zurückgegeben
  //          1 , wenn vollständig gefunden  => Text in s zurückgegeben
  //          2 , wenn Anfang gefunden aber kein Ende => Text in s zurückgegeben
  unsigned char StrFindQuotText(std::string &str, const char *txtsuch, const char *txtquot0, const char *txtquot1, const char *suchregel);

  // Suche mit Quot
  // Sucht string txtsuch in txt zwischen pos0 und len0 nach der suchregel
  // suchregel = "vs" sucht      string von txtsuch         vorwaerts   in txt
  //             "rs" sucht      "                          rueckwaerts "
  //             "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
  //             "rn" sucht "                               rueckwaerts "
  // wobei er innerhalb oder ausserhalb von quot0 und quot1 sucht entsprechend
  // dem status istatus
  // quotregel = "i" innerhalb
  //           = "a" ausserhalb
  // istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots
  //           1   "                             innerhalb     " 
  // return(npos)  nicht gefunden ansonsten
  // return(i)             Stelle wo absolut gefunden beginnend mit 0
  std::size_t StringFindQuot(std::string &str, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus);
  std::size_t StringFindQuot(const char *txt, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus);
  std::size_t StringFindQuot(std::string &str, std::size_t pos0, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus);
  std::size_t StringFindQuot(const char *txt, std::size_t pos0, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus);
  std::size_t StringFindQuot(std::string &str, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus);
  std::size_t StringFindQuot(const char *txt, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus);

  // StringGet-Functions
  // get quoted string find first from position strpos0
  std::string StringGetQuotedText(const char *txt, const char *txtquot0, const char *txtquot1);
  std::string StringGetQuotedText(std::string &str, const char *txtquot0, const char *txtquot1);
  std::string StringGetQuotedText(const char *txt, std::size_t strpos0,const char *txtquot0, const char *txtquot1);
  std::string StringGetQuotedText(std::string &str, std::size_t strpos0,const char *txtquot0, const char *txtquot1);



  //Sonstige-Funktionen ======================
  //==========================================
  char*  CharCpy (char *dest, const char *src, std::size_t len);
  char*  CharSet (char *dest, char c, std::size_t len);
  bool   CharIsAlphNum(char c);
  bool   StringIsAlphNum(const char *src);
  bool   StringIsAlphNum(std::string &str);

  // Zerlegt name in Pfad(p),body Filename(f),Extension Filename(e)
  // und schreibt in str
  //In regel kann stehen:                          
  // "p"    Pfad, "f" wie filename und/oder  "e" wie extension                        
  //
  // z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn       
  // vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt
  //
  // Rückgabewert    true, wenn oky, in s steht das Ergebnis
  //                 false, wenn nicht okay 
  bool StringExtractPfe(std::string &str,const char *name,const char *regel);

  // StringElim
  void                     StringElimAnfEnd(std::string &text,std::string &elim);
  void                     StringElimAnfEnd(std::string &text,const char *pelim);
  void                     StringElimAnfEnd(char *ptext,const char *pelim);
  void                     StringElimAnf(std::string &text,std::string &elim);
  void                     StringElimAnf(std::string &text,const char *pelim);
  void                     StringElimAnf(char *ptext,const char *pelim);
  void                     StringElimEnd(std::string &text,std::string &elim);
  void                     StringElimEnd(std::string &text,const char *pelim);
  void                     StringElimEnd(char *ptext,const char *pelim);

  void                     StringElimNonAlphaNumAnf(std::string &text);
  void                     StringElimNonAlphaNumEnd(std::string &text);
  void                     StringElimNonAlphaNumAnfEnd(std::string &text);
  void                     StringElimNonAlphaNumAnf(char *ptext);
  void                     StringElimNonAlphaNumEnd(char *ptext);
  void                     StringElimNonAlphaNumAnfEnd(char *ptext);
  // StringCpy
  //------------------------------------------------------------------------------
  const char * StringCpy (char * dest, std::size_t len, const char *  src);
  const char * StringCpy (char * dest, std::size_t len, const char *  src, std::size_t pos0, std::size_t len0);
  const char * StringCpy (char * dest, std::size_t len, std::string  &ssrc, std::size_t pos0, std::size_t len0);
  const char * StringCpy (std::string  &sdest, std::string  &ssrc, std::size_t pos0, std::size_t len0);
  const char * StringCpy (std::string  &sdest, const char *  src, std::size_t pos0, std::size_t len0) ;
  // StrintCat
  //------------------------------------------------------------------------------
  const char * StringCat (char * dest, std::size_t len, const char * src);
  const char * StringCat (char * dest, std::size_t len, const char *  src, std::size_t pos0, std::size_t len0);
  const char * StringCat (char * dest, std::size_t len, std::string  &ssrc, std::size_t pos0, std::size_t len0);
  const char * StringCat (std::string  &sdest, std::string  &ssrc, std::size_t pos0, std::size_t len0);
  const char * StringCat (std::string  &sdest, const char *  src, std::size_t pos0, std::size_t len0) ;
  const char * StringCat (std::string  &sdest, const char *  src) ;

  // StringCompare
  //------------------------------------------------------------------------------
  bool StringCompare(std::string &s0,std::string &s1);
  bool StringCompare(const char *str0,const char *str1);
  bool StringCompare(const std::string &s0, std::size_t pos0, std::size_t len0 ,const std::string &s1, std::size_t pos1, std::size_t len1);
  bool StringCompare(const char *str0, std::size_t pos0, std::size_t len0, const char *str1, std::size_t pos1, std::size_t len1);
  
  // StringInsert
  //------------------------------------------------------------------------------
  void StringInsert(std::string &str,std::size_t pos0str,std::string &sinsert);
  

  // StringReplace
  //------------------------------------------------------------------------------
  void StringReplace(std::string &str0, std::string &str, std::size_t pos0, std::size_t len0);
  void StringReplace(std::string &str0, const char *pstr, std::size_t pos0, std::size_t len0);

  // StringChange
  //-------------------------------------------------------------------------------
  void StringChange(std::string &str, std::string &such, std::string &ersetz);
  void StringChange(std::string &str, const char *psuch, const char *persetz);

  // StringSplit
  // text wird mit einem beliebigen Trenn-string zerlegt in ein Vektor von Strings
  //------------------------------------------------------------------------------
  std::vector<std::string> StringSplit(std::string &text,std::string &delim);
  std::vector<std::string> StringSplit(std::string &text,const char *pdelim);
  std::vector<std::string> StringSplit(const char *ptext,const char *pdelim);

  // StringConcate
  std::string   StringConcate(std::vector<std::string> &strvec);
  std::string   StringConcate(std::vector<std::string> &strvec,const char *pdelim);
  std::string   StringConcate(std::vector<std::string> &strvec,std::string &delim);


  // StringVec
  //------------------------------------------------------------------------------
  // Is stringVector strvec0 in strvec1 to find from startposition pos1 in strvec1
  bool IsStringVec0InStringVec1(std::vector<std::string> &strvec0, std::vector<std::string> &strvec1, std::size_t pos1=0);
  // number of identies of strvec0 in strvec1 from position pos1 
  std::size_t NumOfIdentStringItemsInStrinVec(std::vector<std::string> &strvec0,std::vector<std::string> &strvec1, std::size_t pos1=0);
  // Is string to find in strvec
  bool IsStringInStringVec(std::string &str,std::vector<std::string> &strvec);
  // how many times is string in vector
  std::size_t NumOfStringInStrinVec(std::string &str,std::vector<std::string> &strvec);
  // index for icount = 0 ... NumOfStringInStrinVec()
  std::size_t IndexOfStringInStrinVec(std::string &str,std::vector<std::string> &strvec,std::size_t icount); 

  
}
#endif