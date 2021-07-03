//==========================================
//string-Klasse ============================
//==========================================
//Konstruktoren:
//CStr Text1;
//CStr Text2(Text1);
//CStr Text3("abc");
//CStr Text4(10,'a');
//CStr Text5(20);
//
//[]-Operator:
//size_t i=0;       Text2[i] = 's';
//const size_t j=1; Text2[j] = 't';
//char c;           c        = Text2[i];
//
//Vorsicht: 
//- Text2[5] funktioniert nicht
//- Programm bricht mit assert() ab, wenn Speicherplatz nicht vorhanden
//
//Zuweisung:
//std::string text = "relativ";
//Text1 = "das Wort" // char *
//Text1 = text;      // std::string
//Text2 = Text1;     // CStr
//
//Anhängen:
//Text1 += " hallo";  // char *
//Text1 += text;      // std::string
//Text1 += Text1;     // CStr
//Text1.append(" hallo");  // char *
//Text1.append(text);      // std::string
//Text1.append(Text1);     // CStr
//
//Länge:
//size_t l  = Text1.getLen();
//size_t l1 = Text1.size();
//Poitner:
//char *p; p = Text1. c_str();
//Löschen:
//Text1.clear();
//
//Vergleichen:
//if( Text1 == Text2 ) ...;         // bool true
//if( Text1 == "abc" ) ...;         // bool true
//if( Text1.compare(Text2) ) ...;   // bool true
//if( Text1.compare("abc") ) ...;   // bool true
//
//Finden:
//std::size_t i;
//i=Text1.findText('f');          //Suche const char in Text1
//i=Text1.findText('f', 10);      //Suche const char ab Position 10 in Text1 
//######################################################################################################################################################################################################################################################################################################################################################################################################################################---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#--------------------------#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#--------------------------------
//i=Text1.findText("fahren");     //Suche const char* in Text1
//i=Text1.findText("fahren",10);  //Suche const char* ab Position 10 in Text1
//i=Text1.findText(Text2);        //Suche const CStr &s in Text1
//i=Text1.findText(Text2,10);     //Suche const CStr &s ab Position 10 in Text1
//if( i != CStr::npos ) ...;   //gefunden, Rückgabeposition immer absolut
//
//i=Text1.findText("Suchtext",["vs","rs","vn","rn"],"[","]",["i","a"],[0,1]);
//i=Text1.findText(Text2,["vs","rs","vn","rn"],"[","]",["i","a"],[0,1]);
// vs: vorwärts suchen
// rs: Rückwärts suchen
// vn: vorwärts nicht suchen
// rn: rückwärts nicht suchen
// i:  innerhalb des quots suchen
// a: ausserhalb des quots suchen
// 0: erstes Zeichen ist ausserhalb des quots
// 1: erstes Zeichen ist innerhlab des Quots
// quot hier: [    ]
//
//Ersetzen:
//Text1.replace("abc", 0, 2); // Ersetzt in Text1 an Stelle 0 bis 1 mit "abc"
//Text1.replace(Text2, 0, 2); // Ersetzt in Text1 an Stelle 0 bis 1 mit Text2
//
//Teil-string:
//slf::Text3 = Text1.substr(0,2);    // Übergibt den Teiltring pos=0, len=2
//
//String-Bearbeitung
//==================
//okay_t str_elim_a(CStr &text,const char *elim_string=" ");
//okay_t str_elim_e(CStr &text,const char *elim_string=" ");
//okay_t str_elim_ae(CStr &text,const char *elim_string=" ");
//-----------------------------------------------------------------------------------
//Eliminiert am Anfang bzw. Ende bzw. Anfang und Ende den string elim_string
//In/Out:
//text          Zu bearbeitender Text als string-Klasse
//elim_string   zu eliminierender String default " "
//status        OKAY oder NOT_OKAY aus basic.h
//
//okay_t str_elim_a_c(CStr &text,const char *elim_string="\t ");
//okay_t str_elim_e_c(CStr &text,const char *elim_string="\t ");
//okay_t str_elim_ae_c(CStr &text,const char *elim_string="\t ");
//-----------------------------------------------------------------------------------
//Eliminiert am Anfang bzw. Ende bzw. Anfang und Ende die Zeichen aus elim_string
//sofern wenn einer auftritt
//
//In/Out:
//text          Zu bearbeitender Text als string-Klasse
//elim_string   zu eliminierender String default "\t ", d.h das Zeichen '\t' und ' '
//              werden eliminiert
//status        OKAY oder NOT_OKAY aus basic.h
//
//
//std::size_t SlfStrFind(const char *txt,const char *txtsuch
//                 ,const char *suchregel,const char *txtquot0
//                 ,const char *txtquot1,const char *quotregel
//                 ,char istatus,std::size_t pos0
//                 ,std::size_t len0);
//
//    Sucht string txtsuch in txt zwischen pos0 und len0 nach der suchregel
//    suchregel = "vs" sucht      string von txtsuch         vorwaerts   in txt
//                "rs" sucht      "                          rueckwaerts "
//                "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
//                "rn" sucht "                               rueckwaerts "
//    wobei er innerhalb oder ausserhalb von quot0 und quot1 sucht entsprechend
//    dem status istatus
//    quotregel = "i" innerhalb
//              = "a" ausserhalb
//
//    istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots
//              1   "                             innerhalb     " 
//
//     return(SLF_STR_NPOS)  nicht gefunden ansonsten
//     return(i)             Stelle wo absolut gefunden beginnend mit 0



#ifndef SLF_STR_H_INCLUDED
#define SLF_STR_H_INCLUDED

#include <string>
#include "SlfBase.h"

namespace slf
{

#define SLF_STR_NPOS  (std::string::npos)

  class CStr : public std::string
  {
  public:
    std::size_t npos;
    CStr();
    CStr(const CStr& s);
    CStr(const std::string& s);
    CStr(const char* p);
    CStr(std::size_t l, char fillChar);
    CStr(std::size_t l);
    virtual ~CStr();

    CStr& operator=(const char* p);
    CStr& operator=(const CStr& s);
    CStr& operator=(const std::string& s);
    CStr& operator+=(const char* p);
    CStr& operator+=(const CStr& s);
    CStr& operator+=(const std::string& s);

    CStr& add(const char u);
    std::size_t getLen() const { return (std::size_t)this->size(); }
    char  getChar(std::size_t pos) const;
    CStr  getString(std::size_t pos, std::size_t len) const;
    const char *getPointer(std::size_t pos) const;

    CStr& cut(std::size_t pos0, std::size_t len0) ;
    CStr& cut(std::size_t pos0) ;
    okay_t insertText(char *textins, std::size_t pos0);
    okay_t insertText(char *textins, std::size_t pos0, std::size_t len0);
    CStr& replaceText(const CStr &s, std::size_t pos0, std::size_t len0);
    CStr& replaceText(const char* pstr, std::size_t pos0, std::size_t len0);
    okay_t change(char *textsuch, char *textchange);
    okay_t change(char *textsuch, char *textchange, std::size_t pos0, std::size_t len0);

    okay_t elimAnf(const char *elim_string = " ");
    okay_t elimEnd(const char *elim_string = " ");
    okay_t elimAnfEnd(const char *elim_string = " ");
    okay_t elimAnfC(const char *elim_string = "\t ");
    okay_t elimEndC(const char *elim_string = "\t ");
    okay_t elimAnfEndC(const char *elim_string = "\t ");
    okay_t elim(const char *elim_string = " ");

    void cat(const char *str);
    void cat(const char t);
    void cat(const CStr &s);
    void cat(const char *str, std::size_t pos0);
    void cat(const char *str, std::size_t pos0, std::size_t len0);
    void cat(const CStr &s, std::size_t pos0);
    void cat(const CStr &s, std::size_t pos0, std::size_t len0);

    okay_t catFormat(char *pformat, ...);
    void   erase(void) { this->clear(); }

    std::size_t findText(const char c);
    std::size_t findText(const char c, std::size_t pos);

    std::size_t findText(const char* pstr);
    std::size_t findText(const char* pstr, std::size_t pos0);
    std::size_t findText(const char* pstr, std::size_t pos0, std::size_t len0);
    std::size_t findText(const CStr &s) const;
    std::size_t findText(const CStr &s, std::size_t pos0) const;
    std::size_t findText(const CStr &s, std::size_t pos0, std::size_t len0) const;

    // gequotted
    /*---------------------------------------------------------------------------
    Sucht nach txtsuch oder s in oder ausserhalb quotes txtquot0,txtquot1 
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
    ----------------------------------------------------------------------------*/
    std::size_t findText(const char *txtsuch
      , const char *suchregel
      , const char *txtquot0
      , const char *txtquot1
      , const char *quotregel
      , char istatus);
    std::size_t findText(const CStr &s
      , const char *suchregel
      , const char *txtquot0
      , const char *txtquot1
      , const char *quotregel
      , char istatus);
    //---------------------------------------------
    //
    // sucht in Klasse nach quotes txtquot0, txtquot1 und gibt inhalt in s zurück
    // suchregel = "vs","vn","rs","rn" vorwaerts suchen/nicht suchen rückwaerts sucjen7nicht
    //
    // return 0; nichts gefunden
    // return 1; quots gefunden, gibt Inhalt aus s zwischen quota zurück
    // return 2; Nur Anfangsquot gefunden gibt alles dahinter zurück
    std::size_t findText(CStr &s
      , const char *txtquot0
      , const char *txtquot1
      , const char *suchregel);

    std::size_t findNot(const char c);
    std::size_t findNot(const char c, std::size_t pos);
    std::size_t findNot(const char* pstr);
    std::size_t findNot(const char* pstr, std::size_t pos);
    std::size_t findNot(const CStr &s);
    std::size_t findNot(const CStr &s, std::size_t pos);


    //-----------------------------------------------------------------------------------
    // get from position istr search next word beginning with alpha and afterwords alphanumerics
    // or if iytpe != NULL and pstr != NULL additionally next of one of characters of pstr
    // return true if alphanumeric word or extra character is found and 
    // return position of next character behind word or character 
    // if isvalue is set, only numbers like 10 or .10 or 1e-3 are searched
    // e.g.
    // Cstr str = "  abc123 = efg : re10"; size_t istr=0; CStr word; size_t itype;
    // str.getNextWord(istr,word) => word = "abc123" istr=8
    // str.getNextWord(istr,word,&itype) => word = "efg" istr=14, itype = npos
    // str.getNextWord(istr,word,itype,"=[:]") => word = ":" istr=16, itype = 2
    // str.getNextWord(istr,word,itype,"=[:]",true) => word = "10" istr=21, itype = npos
    //------------------------------------------------------------------------------------
    bool getNextWord(std::size_t &istr, CStr &word, std::size_t *itype = NULL,  const char *pstr = NULL, bool isvalue = false) const;

    CStr &formatLeft(std::size_t length, char * fil = " ");
    uint8_t isAlphNum(void);
    bool    isEmpty(void) { return(this->size() == 0); }

    bool compareText(const CStr &s) const;
    bool compareText(const char *pstr) const;
    bool compareText(const CStr &s, std::size_t pos1, std::size_t len1);
    bool compareText(const char *pstr, std::size_t pos1, std::size_t len1);

  private:
    //std::size_t    len;
  };
  //Operator-Funktionen ======================
  //==========================================
  CStr& operator+(CStr& a, const CStr& b);
  CStr& operator+(CStr& a, const std::string& b);
  CStr operator+(const CStr &a, const CStr& b);
  CStr operator+(const CStr &a, const char * b);
  CStr operator+(const CStr &a, const std::string& b);
  bool operator==(const CStr& a, const CStr& b);
  bool operator==(const CStr& a, const std::string& b);
  bool operator==(const std::string& b, const CStr& a);
  bool operator==(const CStr& a, const char* b);
  bool operator!=(const CStr& a, const CStr& b);
  bool operator!=(const CStr& a, const std::string& b);
  bool operator!=(const std::string& b, const CStr& a);
  bool operator!=(const CStr& a, const char* b);

  //Sonstige-Funktionen ======================
  //==========================================
  std::size_t SlfStrLen(const char *s);
  std::size_t SlfStrLen(CStr &s);
  char*  SlfCharCpy(char *dest, const char *src, std::size_t len);
  void   SlfCharSet(const char *dest, char c, std::size_t len);
  bool   SlfCharIsAlphNum(char c);
  bool   SlfStrIsAlphNum(const char *src);
  bool   SlfStrIsAlphNum(CStr &s);
  bool   SlfCharIsType(const char c, const char type); // check type='a','d','n',any other => alpha,digit,alphanumeric, c==type

  // einfache Suche
  std::size_t SlfStrFind(const CStr &str
    , const char *txtsuch
    , const char *regel);
  std::size_t SlfStrFind(const char *txt
    , const char *txtsuch
    , const char *regel);
  // Suche mit Anfang bis Ende
  std::size_t SlfStrFind(const CStr &str
    , const char *txtsuch
    , const char *regel
    , std::size_t pos0);
  std::size_t SlfStrFind(const char *txt
    , const char *txtsuch
    , const char *regel
    , std::size_t pos0);
  // Suche mit Anfang und Länge
  std::size_t SlfStrFind(const CStr &str
    , const char *txtsuch
    , const char *regel
    , std::size_t pos0
    , std::size_t len0);
  std::size_t SlfStrFind(const char *txt
    , const char *txtsuch
    , const char *regel
    , std::size_t pos0
    , std::size_t len0);
  // Suche mit Quot
  std::size_t SlfStrFind(const CStr &str
    , const char *txtsuch
    , const char *suchregel
    , const char *txtquot0
    , const char *txtquot1
    , const char *quotregel
    , char istatus);
  std::size_t SlfStrFind(const char *txt
    , const char *txtsuch
    , const char *suchregel
    , const char *txtquot0
    , const char *txtquot1
    , const char *quotregel
    , char istatus);
  std::size_t SlfStrFind(const CStr &str
    , const char *txtsuch
    , const char *suchregel
    , const char *txtquot0
    , const char *txtquot1
    , const char *quotregel
    , char istatus
    , std::size_t pos0
    , std::size_t len0);

  std::size_t SlfStrFind(const CStr &str
    , const char *txtsuch
    , const char *suchregel
    , const char *txtquot0
    , const char *txtquot1
    , const char *quotregel
    , char istatus
    , std::size_t pos0);

  std::size_t SlfStrFind(const char *txt
    , const char *txtsuch
    , const char *suchregel
    , const char *txtquot0
    , const char *txtquot1
    , const char *quotregel
    , char istatus
    , std::size_t pos0
    , std::size_t len0);

  std::size_t SlfStrFind(const char *txt
    , const char *txtsuch
    , const char *suchregel
    , const char *txtquot0
    , const char *txtquot1
    , const char *quotregel
    , char istatus
    , std::size_t pos0);
  // Sucht Text im Quot nach suchregel
  // Rückgabe 0 , wenn nicht gefunden    => kein Text in s zurückgegeben
  //          1 , wenn vollständig gefunden  => Text in s zurückgegeben
  //          2 , wenn Anfang gefunden aber kein Ende => Text in s zurückgegeben

  uint8_t SlfStrFindText(CStr &s
    , const char *txt
    , const char *txtquot0
    , const char *txtquot1
    , const char *suchregel);

  okay_t SlfStrElimAnf(CStr &text, const char *elim_string = " ");
  okay_t SlfStrElimEnd(CStr &text, const char *elim_string = " ");
  okay_t SlfStrElimAnfEnd(CStr &text, const char *elim_string = " ");
  okay_t SlfStrElimAnfC(CStr &text, const char *elim_string = "\t ");
  okay_t SlfStrElimEndC(CStr &text, const char *elim_string = "\t ");
  okay_t SlfStrElimAnfEndC(CStr &text, const char *elim_string = "\t ");
  okay_t SlfStrElim(CStr &text, const char *elim_string = " "); // über alles

  const char * SlfStrCpy(char * dest, std::size_t len, const char *  src);
  const char * SlfStrCat(char * dest, std::size_t len, const char * src);
  const char * SlfStrCpy(char * dest, std::size_t len, const char *  src, std::size_t pos0, std::size_t len0);
  const char * SlfStrCpy(char * dest, std::size_t len, CStr  &ssrc, std::size_t pos0, std::size_t len0);
  const char * SlfStrCpy(CStr  &sdest, CStr  &ssrc, std::size_t pos0, std::size_t len0);
  const char * SlfStrCpy(CStr  &sdest, const char *  src, std::size_t pos0, std::size_t len0);
  const char * SlfStrFindQuotCpy(CStr  &sdest, CStr  &ssrc, char *txtquot0, char *txtquot1);

  // schriebt in einen String
  okay_t SlfStrCatFormat(CStr &s, char *pformat, ...);

  // Vergleicht Strings
  // return true, wenn gleich, ansonsten false
  //==========================================
  bool  SlfStrCompare(const CStr &s0, const CStr &s1);
  bool  SlfStrCompare(const CStr &s0, std::size_t pos0, std::size_t len0,
    const CStr &s1, std::size_t pos1, std::size_t len1);
  bool  SlfStrCompare(const char* p0, const char* p1);
  bool  SlfStrCompare(const CStr *ps0, const char* p1);
  bool  SlfStrCompare(const char* p0, std::size_t pos0, std::size_t len0,
    const char* p1, std::size_t pos1, std::size_t len1);

  // vergleicht wieviele Zeichen von vorne  übereinstimmen
  //======================================================
  std::size_t SlfStrCompareCount(const CStr &s0, const CStr &s1);
  std::size_t SlfStrCompareCount(const CStr &s0, std::size_t pos0, std::size_t len0,
    const CStr &s1, std::size_t pos1, std::size_t len1);
  std::size_t SlfStrCompareCount(const char* p0, const char* p1);
  std::size_t SlfStrCompareCount(const char* p0, std::size_t pos0, std::size_t len0,
    const char* p1, std::size_t pos1, std::size_t len1);



  EErrNo SlfStr2Num(const char *t, double *dval, char **tstop = NULL);
  EErrNo SlfStr2Num(const char *t, uint8_t *uival, char **tstop = NULL);
  EErrNo SlfStr2Num(const char *t, sint8_t *ival, char **tstop = NULL);
  EErrNo SlfStr2Num(const char *t, uint16_t *uival, char **tstop = NULL);
  EErrNo SlfStr2Num(const char *t, sint16_t *ival, char **tstop = NULL);
  EErrNo SlfStr2Num(const char *t, std::size_t *uival, char **tstop = NULL);
  EErrNo SlfStr2Num(const char *t, sint32_t *ival, char **tstop = NULL);

  double SlfStrToD(const char *string, char **endPtr);

  okay_t SlfStrChange(CStr &text, char *textsuch, char *textchange);
  okay_t SlfStrChange(CStr &text, char *textsuch, char *textchange, std::size_t pos0, std::size_t len0);

  // Zerlegt name in Pfad(p),body Filename(f),Extension Filename(e)
  //In regel kann stehen:                          
  // "p"    Pfad, "f" wie filename und/oder  "e" wie extension                        
  //
  // z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn       
  // vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt
  //
  // Rückgabewert    == OKAY, wenn oky, in s steht das Ergebnis
  //                 == NOT_OKAY, wenn nicht okay 
  okay_t SlfStrExtractPfe(CStr &s, const char *name, const char *regel);

  CStr SlfStrExtract(const CStr &s, std::size_t i0,std::size_t len);


#if 0

  //char& operator[](std::size_t pos);
  //const char& operator[](std::size_t pos) const;

  CStr operator()(std::size_t pos, std::size_t count) const;
  bool operator<(const CStr& s) const;

  bool operator!() const { return(len == 0); }
  operator char* () const;

  //okay_t insert(char *textins,std::size_t pos0);
  //okay_t insert(char *textins,std::size_t pos0, std::size_t len0);

  // funktioniert nicht mit VisualC 6, SlfStrCpy() benutzen
  //CStr substr(std::size_t pos0, std::size_t len0);





    // friend - Deklarationen
//  friend ostream& operator <<(ostream& os, const CStr& s);
//  friend istream& operator >>(istream& is, CStr& s);

#endif



} //namespace slf
#endif // SLF_STR_H_INCLUDED