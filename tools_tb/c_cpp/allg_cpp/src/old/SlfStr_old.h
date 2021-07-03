//==========================================
//string-Klasse ============================
//==========================================
//Konstruktoren:
//CSlfStr Text1;
//CSlfStr Text2(Text1);
//CSlfStr Text3("abc");
//CSlfStr Text4(10,'a');
//CSlfStr Text5(20);
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
//Text2 = Text1;     // CSlfStr
//
//Anhängen:
//Text1 += " hallo";  // char *
//Text1 += text;      // std::string
//Text1 += Text1;     // CSlfStr
//Text1.append(" hallo");  // char *
//Text1.append(text);      // std::string
//Text1.append(Text1);     // CSlfStr
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
//uint32_t i;
//i=Text1.find('f');          //Suche const char in Text1
//i=Text1.find('f', 10);      //Suche const char ab Position 10 in Text1 
//i=Text1.find("fahren");     //Suche const char* in Text1
//i=Text1.find("fahren",10);  //Suche const char* ab Position 10 in Text1
//i=Text1.find(Text2);        //Suche const CSlfStr &s in Text1
//i=Text1.find(Text2,10);     //Suche const CSlfStr &s ab Position 10 in Text1
//if( i != SlfStr::npos ) ...;   //gefunden, Rückgabeposition immer absolut
//
//i=Text1.find("Suchtext",["vs","rs","vn","rn"],"[","]",["i","a"],[0,1]);
//i=Text1.find(Text2,["vs","rs","vn","rn"],"[","]",["i","a"],[0,1]);
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
//status_t str_elim_a(CSlfStr &text,const char *elim_string=" ");
//status_t str_elim_e(CSlfStr &text,const char *elim_string=" ");
//status_t str_elim_ae(CSlfStr &text,const char *elim_string=" ");
//-----------------------------------------------------------------------------------
//Eliminiert am Anfang bzw. Ende bzw. Anfang und Ende den string elim_string
//In/Out:
//text          Zu bearbeitender Text als string-Klasse
//elim_string   zu eliminierender String default " "
//status        OKAY oder NOT_OKAY aus basic.h
//
//status_t str_elim_a_c(CSlfStr &text,const char *elim_string="\t ");
//status_t str_elim_e_c(CSlfStr &text,const char *elim_string="\t ");
//status_t str_elim_ae_c(CSlfStr &text,const char *elim_string="\t ");
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
//uint32_t SlfStrFind(const char *txt,const char *txtsuch
//                 ,const char *suchregel,const char *txtquot0
//                 ,const char *txtquot1,const char *quotregel
//                 ,char istatus,uint32_t pos0
//                 ,uint32_t len0);
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

#include <string.h>
#include "SlfBasic.h"
#include "SlfNum.h"

#define SLF_STR_NPOS  MAX_UINT32

class CSlfStr 
{
 public:
  uint32_t npos;
  CSlfStr();
  CSlfStr(const char* p);
  CSlfStr(int l, char fillChar);
  CSlfStr(int l);
  virtual ~CSlfStr();

  CSlfStr& operator=(const char* p);
  CSlfStr& operator=(const CSlfStr& s);
  //CSlfStr& operator=(const CSlfStr s);

  //char& operator[](uint32_t pos);
  //const char& operator[](uint32_t pos) const;

  CSlfStr operator()(uint32_t pos,uint32_t count) const;

  CSlfStr& operator+=(const char* p);
  CSlfStr& operator+=(const CSlfStr& s);
  void append(const CSlfStr &s);
  void append(const char* pstr);
  void append(const char u);

  bool operator==(const CSlfStr& s) const; 
  bool operator==(/*const*/char* s) const;
  bool operator==(const char* s) const;
  bool operator!=(const CSlfStr& s) const; 
  bool operator!=(/*const*/char* s) const;

  bool operator<(const CSlfStr& s) const;
  bool  compare(const char* pstr);
  bool  compare(const CSlfStr &s);
  bool  compare(const char* pstr,uint32_t pos1, uint32_t len1);
  bool  compare(const CSlfStr &s,uint32_t pos1, uint32_t len1);

  bool operator!() const { return(len == 0); } 
  operator char* () const;

  uint32_t getLen() const { return len; }
  uint32_t size() const { return len; }
  char * c_str() const { return str; }
  char getChar(uint32_t pos);
  void clear();

  CSlfStr &cut(uint32_t pos0, uint32_t len0);
  CSlfStr &cut(uint32_t pos0);

	status_t elimAnf(const char *elim_string=" ");
	status_t elimEnd(const char *elim_string=" ");
	status_t elimAnfEnd(const char *elim_string=" ");
	status_t elimAnfC(const char *elim_string="\t ");
	status_t elimEndC(const char *elim_string="\t ");
	status_t elimAnfEndC(const char *elim_string="\t ");
    status_t elim(const char *elim_string=" ");

  uint32_t find(const char c);
  uint32_t find(const char c, uint32_t pos);
  
  uint32_t find(const char* pstr);
  uint32_t find(const char* pstr, uint32_t pos0);
  uint32_t find(const char* pstr, uint32_t pos0, uint32_t len0);
  uint32_t find(const CSlfStr &s);
  uint32_t find(const CSlfStr &s, uint32_t pos0);
  uint32_t find(const CSlfStr &s, uint32_t pos0, uint32_t len0);

  // gequotted
  uint32_t find(const char *txtsuch
             ,const char *suchregel
             ,const char *txtquot0
             ,const char *txtquot1
             ,const char *quotregel
             ,char istatus);
  uint32_t find(const CSlfStr &s
             ,const char *suchregel
             ,const char *txtquot0
             ,const char *txtquot1
             ,const char *quotregel
             ,char istatus);

  uint8_t findText(CSlfStr &s
                ,const char *txtquot0
                ,const char *txtquot1
                ,const char *suchregel);


  //uint32_t findQuoted(

  uint32_t findNot(const char c);
  uint32_t findNot(const char c, uint32_t pos);
  uint32_t findNot(const char* pstr);
  uint32_t findNot(const char* pstr, uint32_t pos);
  uint32_t findNot(const CSlfStr &s);
  uint32_t findNot(const CSlfStr &s, uint32_t pos);

  CSlfStr& replace(const CSlfStr &s, uint32_t pos0, uint32_t len0);
  CSlfStr& replace(const char* pstr, uint32_t pos0, uint32_t len0);

  status_t change(char *textsuch,char *textchange);
  status_t insert(char *textins,uint32_t pos0);
  status_t insert(char *textins,uint32_t pos0, uint32_t len0);
  
  // funktioniert nicht mit VisualC 6, SlfStrCpy() benutzen
  //CSlfStr substr(uint32_t pos0, uint32_t len0);
  
  CSlfStr &formatLeft(uint32_t length, char * fil=" ");

  void cat(const char *str);
  void cat(const CSlfStr &s);
  void cat(const char *str,uint32_t pos0);
  void cat(const char *str,uint32_t pos0,uint32_t len0);
  void cat(const CSlfStr &s,uint32_t pos0);
  void cat(const CSlfStr &s,uint32_t pos0,uint32_t len0);

  status_t catFormat(char *pformat,...);



    // friend - Deklarationen
//  friend ostream& operator <<(ostream& os, const CSlfStr& s);
//  friend istream& operator >>(istream& is, CSlfStr& s);
  friend CSlfStr operator+(const CSlfStr& s1, const CSlfStr& s2);

  uint8_t isAlphNum(void);

 private:
  uint32_t    len;
  char*  str;
};

// Vektor-String-Klasse 
class CSlfStrV 
{
 public:
  CSlfStrV();
  CSlfStrV(const char* p);
  CSlfStrV(CSlfStr &s);
  CSlfStrV(CSlfStrV &strv);
  
  ~CSlfStrV();

  uint32_t getNrows() const { return NRow; }

  uint32_t size() const { return NRow; }

  void clear();

  bool compare(CSlfStrV &strv);
  status_t make(uint32_t nrow);
  status_t cutRow(uint32_t irow);

  // return 1 if found or exist
  // return 0 if not found
  uint8_t find(const char* pstr,uint32_t *pirow);
  uint8_t find(const CSlfStr &s,uint32_t *pirow);
  uint8_t exist(const char* pstr);
  uint8_t exist(const CSlfStr &s);
  uint8_t exist_last(const char* pstr);
  uint8_t exist_last(const CSlfStr &s);
  
  void append(const char *str);
  void append(const CSlfStr &s);
  void append(CSlfStrV &sv);
  // Löscht das letzte Element raus
  void delete_last(void);
  void cat(const char *str, uint32_t irow);
  void cat(const CSlfStr &s, uint32_t irow);

  void cpy(const char *str, uint32_t irow);
  void cpy(const CSlfStr &s, uint32_t irow);

  void insert(const char *str, uint32_t irow);
  void insert(const CSlfStr &s, uint32_t irow);

  char *get_str(uint32_t irow);
  CSlfStr *get(uint32_t irow);
  char *get_last_str(void);
  CSlfStr *get_last(void);

  CSlfStrV& operator=(CSlfStrV& sv);
  bool operator==(CSlfStrV& sv) const; 
  bool operator!=(CSlfStrV& sv) const; 

  uint8_t isAnyIdentical(void);
  uint8_t isEmpty(void){return NRow == 0;}

 private:
  uint32_t    NRow;
  CSlfStr   *pStr;
};
// Matrix_t-String-Klasse 
class CSlfStrM 
{
 public:
  CSlfStrM();
  CSlfStrM(const char* p);
  CSlfStrM(CSlfStr &s);
  ~CSlfStrM();

  uint32_t getNrows() const { return NRow; }
  uint32_t getNcols() const { return NCol; }

  uint32_t size() const { return NRow*NCol; }

  void clear();

  status_t make(uint32_t nrow, uint32_t ncol);
  status_t cutRow(uint32_t irow);
  status_t cutCol(uint32_t icol);

  status_t find(const char* pstr,uint32_t *pirow, uint32_t *picol);
  status_t find(const CSlfStr &s,uint32_t *pirow, uint32_t *picol);
  
  void cat(const char *str, uint32_t irow, uint32_t icol);
  void cat(const CSlfStr &s, uint32_t irow, uint32_t icol);

  void cpy(const char *str, uint32_t irow, uint32_t icol);
  void cpy(const CSlfStr &s, uint32_t irow, uint32_t icol);

  char *get_str(uint32_t irow, uint32_t icol);
  CSlfStr *get(uint32_t irow, uint32_t icol);

  void transpone(void);

  CSlfStrM& operator=(CSlfStrM& sm);

 private:
  uint32_t    NRow, NCol;
  uint32_t    Len;
  CSlfStr   *pStr;
};

// String-Funktionen
//==================

uint32_t SlfStrLen (const char *s);
uint32_t SlfStrLen (CSlfStr &s);
char * SlfStrCpy (char * dest, uint32_t len, const char *  src);
char * SlfStrCat (char * dest, uint32_t len, const char * src);
char * SlfStrCpy (char * dest, uint32_t len, const char *  src, uint32_t pos0, uint32_t len0);
char * SlfStrCpy (char * dest, uint32_t len, CSlfStr  &ssrc, uint32_t pos0, uint32_t len0);
char * SlfStrCpy (CSlfStr  &sdest, CSlfStr  &ssrc, uint32_t pos0, uint32_t len0);
char * SlfStrCpy (CSlfStr  &sdest, const char *  src, uint32_t pos0, uint32_t len0);
char * SlfStrFindQuotCpy(CSlfStr  &sdest, CSlfStr  &ssrc,char *txtquot0, char *txtquot1);

status_t SlfStrCatFormat(CSlfStr &s,char *pformat,...);

// Vergleicht Strings
// return true, wenn gleich, ansonsten false
//==========================================
bool  SlfStrCompare(const CSlfStr &s0, const CSlfStr &s1);
bool  SlfStrCompare(const CSlfStr &s0,uint32_t pos0, uint32_t len0,
                    const CSlfStr &s1,uint32_t pos1, uint32_t len1);
bool  SlfStrCompare(const char* p0, const char* p1);
bool  SlfStrCompare(const char* p0,uint32_t pos0, uint32_t len0,
                    const char* p1,uint32_t pos1, uint32_t len1);

// vergleicht wieviele Zeichen von vorne  übereinstimmen
//======================================================
uint32_t SlfStrCompareCount(const CSlfStr &s0, const CSlfStr &s1);
uint32_t SlfStrCompareCount(const CSlfStr &s0,uint32_t pos0, uint32_t len0,
                          const CSlfStr &s1,uint32_t pos1, uint32_t len1);
uint32_t SlfStrCompareCount(const char* p0, const char* p1);
uint32_t SlfStrCompareCount(const char* p0,uint32_t pos0, uint32_t len0,
                          const char* p1,uint32_t pos1, uint32_t len1);


status_t SlfStrElimAnf(CSlfStr &text,const char *elim_string=" ");
status_t SlfStrElimEnd(CSlfStr &text,const char *elim_string=" ");
status_t SlfStrElimAnfEnd(CSlfStr &text,const char *elim_string=" ");
status_t SlfStrElimAnfC(CSlfStr &text,const char *elim_string="\t ");
status_t SlfStrElimEndC(CSlfStr &text,const char *elim_string="\t ");
status_t SlfStrElimAnfEndC(CSlfStr &text,const char *elim_string="\t ");
status_t SlfStrElim(CSlfStr &text,const char *elim_string=" "); // über alles

error_t SlfStr2Num(const char *t, double *dval , char **tstop=NULL);
error_t SlfStr2Num(const char *t, uint8_t *uival , char **tstop=NULL);
error_t SlfStr2Num(const char *t, sint8_t *ival  , char **tstop=NULL);
error_t SlfStr2Num(const char *t, uint16_t *uival, char **tstop=NULL);
error_t SlfStr2Num(const char *t, sint16_t *ival , char **tstop=NULL);
error_t SlfStr2Num(const char *t, uint32_t *uival, char **tstop=NULL);
error_t SlfStr2Num(const char *t, sint32_t *ival , char **tstop=NULL);

double SlfStrToD(const char *string, char **endPtr);

status_t SlfStrChange(CSlfStr &text,char *textsuch,char *textchange);

// einfache Suche
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *regel);
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel);
// Suche mit Anfang bis Ende
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0);
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0);
// Suche mit Anfang und Länge
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0
                 ,uint32_t len0);
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *regel
                 ,uint32_t pos0
                 ,uint32_t len0);
// Suche mit Quot
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus);
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus);
uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0
                 ,uint32_t len0);

uint32_t SlfStrFind(CSlfStr &str
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0);
                 
uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0
                 ,uint32_t len0);

uint32_t SlfStrFind(const char *txt
                 ,const char *txtsuch
                 ,const char *suchregel
                 ,const char *txtquot0
                 ,const char *txtquot1
                 ,const char *quotregel
                 ,char istatus
                 ,uint32_t pos0);
// Sucht Text im Quot nach suchregel
// Rückgabe 0 , wenn nicht gefunden    => kein Text in s zurückgegeben
//          1 , wenn vollständig gefunden  => Text in s zurückgegeben
//          2 , wenn Anfang gefunden aber kein Ende => Text in s zurückgegeben

uint8_t SlfStrFindText(CSlfStr &s
                    ,const char *txt
                    ,const char *txtquot0
                    ,const char *txtquot1
                    ,const char *suchregel);


//String-Vektor-Funktionen =================
//==========================================
// Splittet t mit dem Splitstring tsplit (default Leerzeichen)
// und legt sie in strv ab Rückgabe Anzahl der Splitteile
//
uint32_t SlfStrVSplit( CSlfStrV &strv, const char*t, const char *tsplit=" ");
uint32_t SlfStrVSplit( CSlfStrV &strv, const char*t, const char *tsplit,const char *quot0,const char *quot1);

//Sonstige-Funktionen ======================
//==========================================
char*  SlfCharCpy (char *dest, const char *src, uint32_t len);
char*  SlfCharSet (char *dest, char c, uint32_t len);
bool   SlfCharIsAlphNum(char c);
bool   SlfStrIsAlphNum(char *src);
bool   SlfStrIsAlphNum(CSlfStr &s);

// Zerlegt name in Pfad(p),body Filename(f),Extension Filename(e)
//In regel kann stehen:                          
// "p"    Pfad, "f" wie filename und/oder  "e" wie extension                        
//
// z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn       
// vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt
//
// Rückgabewert    == OKAY, wenn oky, in s steht das Ergebnis
//                 == NOT_OKAY, wenn nicht okay 
status_t SlfStrExtractPfe(CSlfStr &s,const char *name,const char *regel);


    
extern Vector_t *pVecDebugInterim;


#endif