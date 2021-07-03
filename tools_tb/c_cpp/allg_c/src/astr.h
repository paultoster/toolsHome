/* $JustDate::  2.03.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 2.1 12.11.01 TBert astr_tcount_quot,astr_tstrlen_quot,astr_tget_quot eingefhrt */
/* 2.0 13.08.01 TBert Release VehicleModell 2.0           */
/*************************************************************************
* File:             astr_s.h        
*	Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
* Version:          3.0
* Datum:            1.11.00
*************************************************************************
* Kurzbeschreibung: 
*
* Stringbearbeitungsfunktionen:

SINT16_T astr_such(char *txt, char txtsuch, const char *regel)

     Sucht string txtsuch in txt mit den Regeln aus regel

     regel = "vs" sucht      string von txtsuch         vorwaerts   in txt
             "rs" sucht      "                          rueckwaerts "
             "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
             "rn" sucht "                               rueckwaerts "
             
     return(-1)  nicht gefunden ansonsten
     return(i)   Stelle wo gefunden beginnend mit 0

SINT16_T astr_such_quot(char *txt,char *txtsuch,const char *suchregel,
                       char *txtquot0, char *txtquot1,char *quotregel,
                       int istatus);
    Sucht string txtsuch in txt nach der suchregel
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

     return(-1)  nicht gefunden ansonsten
     return(i)   Stelle wo absolut gefunden beginnend mit 0

SINT16_T astr_such_i0i1(char *txt1, char txt2, const char *regel,
                        size_t ianf, size_t iend)

     Sucht string txt2 in txt1 mit den Regeln aus regel, zwischen
     *(txt1+ianf) und *(txt1+iend) und gibt die gefundene Stelle
     absolut zurück

     regel = "vs" sucht      string von txt2            vorwaerts   in txt1
             "rs" sucht      "                          rueckwaerts "
             "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
             "rn" sucht "                               rueckwaerts "
             
     return (-1)  Fehler ansonsten
     return (i)   Stelle wo gefunden beginnend mit 0

SINT16_T astr_nsuch(char *txt1, char txt2, SINT16_T n)

     Sucht txt2 in txt1  n-mal und gibt Stelle aus

size_t astr_count(char *txt1, char *txt2, size_t i0, size_t i1)

     Zählt wie oft string txt2 in txt1 in den Stelle i0 bis i1 liegt
     return(0 - n-mal)

size_t astr_count_zeilen(const char *txt1)

     Zaelt die auszugebenen Zeilenzahl
     
IERR_T astr_cut(char *txt1, size_t i0, size_t i1)

     Schneidet aus txt1 raus, von Stelle i0 bis i1

IERR_T astr_cut_ae(char *txt1,const char *txtcut)

    Schneidet aus txt1 am Anfang und Ende um die Zeichen txtcut
    z.B. txt1 = "   abc def   ", txtcut = " " => txt1="abc def"
    
    return 0   okay

IERR_T astr_cut_e(char *txt1,const char *txtcut)

    Schneidet aus txt1 am Ende um die Zeichen txtcut
    z.B. txt1 = "   abc def   ", txtcut = " " => txt1="   abc def"
    
    return 0   okay

IERR_T astr_cut_a(char *txt1,const char *txtcut)

    Schneidet aus txt1 am Ende um die Zeichen txtcut
    z.B. txt1 = "   abc def   ", txtcut = " " => txt1="abc def   "
    
    return 0   okay

IERR_T astr_cut_comment(char *txt,const char *txtcomment)

    Sucht nach Kommentarzeichen im Text und schneidet ab dem Kommentarzeichen
    den Text raus
    
    return 0, wenn kein Fehler auftritt
    
IERR_T astr_cat(char *txt1, char *txt2,int i0, int i1)

     Haengt an txt1 txt2 von der Stelle i0 bis i1 an

IERR_T astr_cmp(char *txt1, char *txt2,int i0, int i1)

     Vergleicht txt1 (vollstaendig) mit txt2 von der Stelle i0 bis i1
     return(0)  gleich
     return(1)  ungleich
     
IERR_T astr_red(char *txt1, int i0, int i1)

    reduziert txt1 auf die Stelle i0 bis i1 *txt1 = *( txt1+(i0 ... i1) )

    return 0   okay
    return(1)  Fehler

IERR_T astr_insert(char *txt1, char *txt2, int i0, int ninsert)

     Fuegt in txt1 an der Stelle i0 die ersten ninsert Zeichen
     aus txt2 ein.
     (ninsert=strlen(txt2) fueht txt2 vollstaendig ein)

size_t astr_change(char *txt1,char *txtsuch,char *txtchange)

    Sucht in txt1 die Zeichen txtsuch und ersetzt es mit txtchange
    
    return ncount   Anzahl der changes

size_t astr_change_quot(char *txt,char *txtsuch,char *txtchange,
                       char *txtquot0, char *txtquot1,char *regel,
                       UINT8 istatus)
size_t astr_change_quot1(char *txt,char *txtsuch,char *txtchange,
                       char *txtquot0, char *txtquot1,char *regel,
                       UINT8 *istatus)
                       
    Sucht in txt die durch txtquot0 und txtquot1 eingeschlossenen
    String nach txtsuch und ersetzt durch txtchange (wenn txtchange = "\0",
    dann wird kein string ersetzt)
    
    regel = "i"   Es wird der string innerhalb des Quots behandelt
            "a"   Es wird der string ausserhalb des Quots beh.
             
    istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots
              1   "                             innerhalb     " 
    ( bei astr_change_quot1() wird pointer auf istatus übergeben, damit
    der letzte Zustannd nach durchforsten des Strings zurückgegeben wird)
              
    return ncount       Anzahl der Gefundenen Stellen
    
    Beispiel:   txt="  [ab c] [def g]  "; txtsuch=" ";txtchange="\0";
                txtquot0="[";txtquot1="]";regel="i";istatus=0;
                => txt="  [abc] [defg]  "; return 2;
                
                mit regel = "a";
                => txt="[ab c][def g]"; return 5;
    
size_t astr_change_spez1(char *txt,char *txtkenn,char *txtsuch,
                       char *txtchange,char *regel)
                       
    Sucht in txt nach txtkenn und ersetzt txtsuch je nach regel
    vorwaerts/rueckwaerts von txtkenn mit txtchange, solange txtsuch 
    vorhanden. Wenn *txtchange = '\0' ist, wird nichts ersetzt.

    regel = "v"         vorwaerts
            "r"         rueckwaerts
            
    return ncount               Anzahl der Ersetzung
    
    Beispiel::  txt="abc   <m> defg <km>"; txtkenn="<";txtsuch=" ";
                txtchange="\0"; regel="r"; => txt="abc<m> defg<km>;
                return 2;
                
IERR_T astr_copy(char *txt1, const char *txt2, int i0, int i1)

     kopiert Teile von txt2 in txt1  *txt1 = *(txt2+(i0 ... i1))

IERR_T astr_copy_cut(char *txt1, char *txt2, int i0, int i1)

     kopiert Teile von txt2 in txt1  *txt1 = *(txt2+(i0 ... i1))
     dann werden die Teile in Txt2 rausgeschnitten
     
IERR_T astr_copy_quot(char *txt1,const char *txt2,const char *txtquot0,const char *txtquot1);                     

   Sucht in txt2 den ersten durch die Quotzeichen 
   txtquot0 und txtquot1 gequoteten string, der in txt1 kopiert wird. 
   Sonderfall:   txtquot0 == NULL => vom ersten Zeichen an 
                 txtquot1 == NULL => bis zum letzten Zeichen

   return 0     wenn gefunden
          -1;    wenn nicht gefunden   

   Beispiel: txt1="[sef]"; txtkap0="["; txtkap1="]";
             => txt2="sef"; return 0;
             
IERR_T astr_copy_nquot(char *txt1,const char *txt2,
                     const char *txtquot0,const char *txtquot1,int n);

   Sucht in txt2 den n-ten (n=1,2,3,..) durch die Quotzeichen 
   txtquot0 und txtquot1 gequoteten string, der in txt1 kopiert wird. 
   Sonderfall:   txtquot0 == NULL => vom ersten Zeichen an 
                 txtquot1 == NULL => bis zum letzten Zeichen
   
   return 0     wenn gefunden
         -1;    wenn nicht gefunden   

   Beispiel: txt1="[sef] [abcd 123]"; txtkap0="["; txtkap1="]"; n=2;
             => txt2="abcd 123"; return 0;

void astr_upper(char *txt1)

     schreibt alles groá

void astr_lower(char *txt1)

     schreibt alles klein

siz_t astr_tcount(char *txt1,const char *txttrenn);

   Zaehlt wieviele Bloecke in txt1 mit dem Trennzeichen txttrenn
   enthalten sind.

   Beispiel: txt1="abc|de|f"; txttrenn="|"; return(3);
             txt1="abc||f";   txttrenn="|"; return(2);
             txt1="";         txttrenn="|"; return(0);

size_t astr_tcount_quot(char *txt1,const char *txttrenn,
                       char *txtquot0,char *txtquot1);

   Zaehlt wieviele Bloecke in txt1 mit dem Trennzeichen txttrenn
   enthalten sind. 
   Laesst Text innerhalb von txtquot0 und txtquot1 ausser acht

   Beispiel: txt1="abc \" e d f\" f"; txttrenn=" ";txtquot0="\"";txtquot1="\"" -> return(3);
             txt1="abc  f";   txttrenn=" ";txtquot0="\"";txtquot1="\"" -> return(2);
             txt1="";         txttrenn=" ";txtquot0="\"";txtquot1="\"" -> return(0);

size_t astr_tstrlen(char *txt1,const char *txttrenn,size_t istelle)

   Sucht aus txt1 den Block istelle (von null gezaehlt),
   wobei der Text mit txtrenn aufgeteilt sein muss und gibt
   Textlaenge (strlen) zurück.

   Rueckgabewert: Textlaenge des Blocks

   Beispiel: txt1="abc|de|f"; txttrenn="|"; istelle=0 => return(3)
             txt1="abc||f"; txttrenn="|"; istelle=2 => return(0)
             txt1="";         txttrenn="|"; istelle=0 => return(0)

size_t astr_tstrlen_quot(char *txt1,const char *txttrenn,size_t istelle,
                       char *txtquot0,char *txtquot1);

   Sucht aus txt1 den Block istelle (von null gezaehlt),
   wobei der Text mit txtrenn aufgeteilt sein muss und gibt
   Textlaenge (strlen) zurück. 
   Laesst Text innerhalb von txtquot0 und txtquot1 ausser acht


   Rueckgabewert: Textlaenge des Blocks

   Beispiel: txt1="abc \" e d f\" f"; txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=0 -> return(3);
             txt1="abc  f";   txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=2 -> return(0); (=> deswegen besser vorher die Funktion
                                      astr_change_quot verwenden)
             txt1="";         txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=0 -> return(0);


size_t astr_tget(char *txt2,char *txt1,const char *txttrenn,size_t istelle);

   Kopiert aus txt1 den Block istelle (von null gezaehlt) in txt2,
   wobei der Text mit txtrenn aufgeteilt sein muss

   Rueckgabewert: tatsaechliche stelle

   Beispiel: txt1="abc|de|f"; txttrenn="|"; istelle=0 => txt2="abc"; return(0)
             txt1="abc|de|f"; txttrenn="|"; istelle=2 => txt2="f";  return(2)
             txt1="";         txttrenn="|"; istelle=0 => txt2="";   return(0)

size_t astr_tget_quot(char *txt2,char *txt1,const char *txttrenn,size_t istelle,
                       char *txtquot0,char *txtquot1);

   Kopiert aus txt1 den Block istelle (von null gezaehlt) in txt2,
   wobei der Text mit txtrenn aufgeteilt sein muss. 
   Laesst Text innerhalb von txtquot0 und txtquot1 ausser acht


   Rueckgabewert: tatsaechliche Rückgabewert

   Beispiel: txt1="abc \" e d f\" f"; txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=0 -> txt2="abc" return(0);
             txt1="abc  f";   txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=2 -> txt2 = "" return(0); (=> deswegen besser 
                                      vorher die Funktion astr_change_quot verwenden)
             txt1="";         txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=0 -> txt2 ="" return(0);


size_t astr_tget_bse(char *txt2,char *txt1,const char *txttrenn,size_t istelle,
                    UINT8_T bisstringende)
                    
   Wie astr_tget,   
   wenn bisstringende == 1 von istelle bis Ende des stringsübergeben
   
size_t astr_tsuch(char *txt1,char *txt2,const char *txttrenn);

   Sucht die Stelle in txt1, in der txt2 mit Trennzeichen txttrenn
   gefunden wird

   Beispiel: txt1="abc|de|f"; txt2="de"; txttrenn="|"; => return(2);

size_t astr_tsuch_quot(char *txt1,char *txt2,const char *txttrenn,
                       char *txtquot0,char *txtquot1,char *regel);

   Sucht die Stelle in txt1, in der txt2 mit Trennzeichen txttrenn
   gefunden wird. (Der gesamte Texte zwischen den Trennzeichen muss passen.
   Laesst Text innerhalb von txtquot0 und txtquot1 ausser acht


   Beispiel: txt1="abc \"de f\" f"; txt2="de f";txttrenn=" ";txtquot0="\"";txtquot1="\"", => return(2);


size_t astr_tsuch_quot(char *txt1,char *txt2,const char *txttrenn,
                       char *txtquot0,char *txtquot1,char *regel);

   Sucht die Stelle in txt1, in der txt2 mit Trennzeichen txttrenn
   gefunden wird. Dabei lässt es 
   nach der regel innerhalb oder ausserhalb
   des Textes in quots gesetzt ausser acht

   regel = "i" Laesst Text innerhalb von txtquot0 und txtquot1 ausser acht
   regel = "a" Laesst Text ausserhalb von txtquot0 und txtquot1 ausser acht


   Beispiel: txt1="abc|de|f"; txt2="de"; txttrenn="|"; => return(2);


void astr_message_free(void);

  Löscht stringmemory
  
size_t astr_fprintf_text(FILE *handle,char *text,size_t Spaltenbreite,char *wiebuendig)

  Druck an handle mit fprintf den string text in die vorgegebene Spaltenbreite
  linksbuendig mit "l", rechtsbuendig mit "r"
  Der ausgegebene Text wird aus text rausgelöscht und die Restlaenge strlen() 
  mit return zurückgegeben.

void astr_message_init(void);

  Initialisieren eines festen Strings

void astr_message_cat(char *txt);
void astr_message_cat_ival(int ival);
void astr_message_cat_fval(float fval);
void astr_message_cat_dval(double dval);

  Hängt Text oder Wert an den lokalen Message String

void astr_message_cpy(char *txt);
void astr_message_cpy_ival(int ival);
void astr_message_cpy_fval(float fval);
void astr_message_cpy_dval(double dval);

  Kopiert Text oder Wert an den lokalen Message String

void astr_message_ins(char *txt);

  Fügt den Text txt[] am Anfang in den Messagestring ein;

size_t astr_message_count(void);

  Fügt den Text txt[] am Anfang in den Messagestring ein;
char *astr_message(void);
  Gibt pointer vom MessageString aus

int astr_message_cpy(char *txt);

  Kopiert den Text txt[] in einen Messagestring;

int astr_message_cat(char *txt);

  Hängt den Text txt[] an den Messagestring an;


IERR_T astr_extract_pfe(char *pinput,char *poutput, char *pregel);

  Sucht in pinpout Pfad,Filename und Extension nach der vorgegeben
  Regel und schreibt Ergebnis in poutput.

  pregel[] = "pfe"  p => Pfad, f => Filename, e => Extension
  z.B. pinput[] = "./abc.dat", pregel[] = "fe" => poutut[]= "abc.dat"

  return 0, wenn etwas gefunden wurde

int astr_such_pfe(const char *pinput,const char *pregel);

  Sucht in pinpout Pfad,Filename und Extension nach der vorgegeben
  Regel.

  pregel[] = "pfe"  p => Pfad, f => Filename, e => Extension
  z.B. pinput[] = "./abc.dat", pregel[] = "fe" => return 0
  z.B. pinput[] = "abc.dat",   pregel[] = "p" => return -1

  return 0, wenn alles gefunden wurde, ansonsten -1

UNIT8_T astr_exist_extension(char *pinput);
  
  Sucht in pinput, ob ein Filenamenextension (z.B. .dat) vorhanden ist
  Wenn ja dann gibt er 1 (TRUE) zurück ansonsten 0 (FALSE)


SINT16_T astr_such_t(const char *txt,const char *auswahl);
IERR_T astr_extract_t(char *ptrennz,const char *txt,const char *auswahl);

Sucht in txt nach einem Zeichen aus der Auswahl
Extrahiert aus txt das Trennzeichen aus Auswahl in ptrennz.

Rückgabewert   wenn gefunden, Stelle des Zeichens in auswahl
               wenn nicht gefunden, dann -1

        astr_such_t("./abc/","/\\");
                                |
        =>  0-------------------+

        astr_such_t("./abc/",",;");

        =>  -1

IERR_T astr_match(const char *txtsearch,const char *txtlist,const char *txttrenn,
                  char exact_flag,char eindeutig_flag,size_t *imatch);  

  Sucht txtsearch in der Liste txtlist, deren string-Werte durch txttrenn-Zeichen getrennt ist.
  Wenn exact_flag gesetzt, dann muß txtsearch exakt in der Liste zu finden sein. Ansonsten zählen
  die ersten n-Buchstaben.

  Wenn eindeutig_flag gesetzt, dann, darf die Übereinstimmung nur einmal vorkommen.

  imatch enthält die Stelle (gezaehlt von 0) die in der Liste übereinstimmt.

  Rückgabewert == 0  Stelle  in der Liste gefunden
               == -1 Übereinstimmung nicht nicht gefunden 
               == 1  Fehler

  Beispiel:

  txtsearch      = "ma"
  txtlist        = "table,manual"
  txttrenn       = ","
  exact_flag     = 0
  eindeutig_flag = 0

  Rückgabewert   = 0
  imatch         = 1

  txtsearch      = "table"
  txtlist        = "table|manual"
  txttrenn       = "|"
  exact_flag     = 1
  eindeutig_flag = 0

  Rückgabewert   = 0
  imatch         = 0

UINT8_T astr_proof_string(char *ptxt,UINT16_T maxlength);

    Prüft ob string nach maxlength-Zeichen einen Terminator '\0' hat
************************************************************************/
#ifndef astr_s_h_included

#define astr_s_h_included

#include "definer.h"

#ifdef __cplusplus
  extern "C" {
#endif

#include <stdio.h>
#define QUOT '"'
#ifndef BOOL
 #define BOOL int
#endif
/*Prototypendefinition*/
SINT16_T astr_such(const char *txt1, const char *txt2, const char *regel);
SINT16_T astr_such_quot(char *txt,const char *txtsuch,const char *suchregel,
                        const char *txtquot0, const char *txtquot1,const char *quotregel,
                        UINT8_T istatus);
SINT16_T astr_such_i0i1( const char *text1, const char *text2, const char *regel, size_t i0,size_t i1);
SINT16_T astr_nsuch(const char *txt1, const char *txt2, size_t n);

size_t astr_count(const char *txt1, const char *txtsuch, size_t i0, size_t i1);
size_t astr_count_zeilen(const char *txt1);

IERR_T astr_cut(char *txt1, size_t i0, size_t i1);
IERR_T astr_cut_ae(char *txt1,const char *txtcut);
IERR_T astr_cut_a(char *txt1,const char *txtcut);
IERR_T astr_cut_e(char *txt1,const char *txtcut);
IERR_T astr_cut_comment(char *txt,const char *txtcomment);

IERR_T astr_cat(char *txt1, const char *txt2, size_t i0, size_t i1);
IERR_T astr_cmp(const char *txt1, const char *txt2, size_t i0, size_t i1);
IERR_T astr_red(char *txt1, size_t i0, size_t i1);
IERR_T astr_insert(char *txt1,const char *txt2, size_t i0, size_t n);

size_t astr_change(char *txt1,const char *txtsuch,const char *txtchange);
size_t astr_change_quot(char *txt,const char *txtsuch,const char *txtchange,
                        const char *txtquot0, const char *txtquot1,const char *regel,
                        UINT8_T istatus);
size_t astr_change_quot1(char *txt,const char *txtsuch,const char *txtchange,
                         const char *txtquot0,const char *txtquot1,const char *regel,
                         UINT8_T *istatus);
size_t astr_change_spez1(char *txt,const char *txtkenn,const char *txtsuch,
                         const char *txtchange,const char *regel);
IERR_T astr_copy(char *txt1, const char *txt2, size_t i0, size_t i1);
IERR_T astr_copy_cut(char *txt1,char *txt2, size_t i0, size_t i1);
IERR_T astr_copy_nquot(char *txt1,const char *txt2,const char *txtquot0,const char *txtquot1,
                     size_t nqout);
IERR_T astr_copy_quot(char *txt1,const char *txt2,const char *txtquot0,const char *txtquot1);
                    
void astr_upper(char *txt1);
void astr_lower(char *txt1);

size_t astr_tcount(const char *txt1,const char *txttrenn);
size_t astr_tcount_quot(const char *txt1,const char *txttrenn,const char *txtquot0,const char *txtquot1);
size_t astr_tstrlen(const char *txt1,const char *txttrenn,size_t istelle);
size_t astr_tstrlen_quot(const char *txt1,const char *txttrenn,size_t istelle,const char *txtquot0,const char *txtquot1);
size_t astr_tget(char *txtget,const char *txt1,const char *txttrenn,size_t istelle);
size_t astr_tget_quot(char *txtget,const char *txt1,const char *txttrenn,size_t istelle,const char *txtquot0,const char *txtquot1);
size_t astr_tget_bse(char *txtget,char *txt1,const char *txttrenn,size_t istelle,UINT8_T bisstringende);

size_t astr_strlen_zeilen(const char *txt1,size_t istelle);
size_t astr_get_zeilen(char *txt2,const char *txt1,size_t istelle);

size_t astr_tsuch(const char *txt1,const char *txtsearch,const char *txttrenn);

size_t astr_fprintf_text(FILE *handle,char *text,size_t Spaltenbreite,char *wiebuendig);

void astr_message_init(void);
void astr_message_cpy(char *txt);
void astr_message_cpy_ival(int);
void astr_message_cpy_fval(float);
void astr_message_cpy_dval(double);
void astr_message_cat(char *txt);
void astr_message_cat_ival(int);
void astr_message_cat_fval(float);
void astr_message_cat_dval(double);
void astr_message_ins(char *txt);
char *astr_message(void);
void astr_message_free(void);
size_t astr_message_count(void);
size_t astr_message_func(UINT8_T server, char *txt, char **message);


IERR_T astr_such_pfe(const char *pinput,const char *pregel);
IERR_T astr_extract_pfe(char *pinput,char *poutput, char *pregel);
UINT8_T astr_exist_extension(char *pinput);
SINT16_T astr_such_t(const char *txt,const char *auswahl);
IERR_T astr_extract_t(char *ptrennz,const char *txt,const char *auswahl);


IERR_T astr_match(const char *txtsearch,const char *txtlist,const char *txttrenn,
                  char exact_flag,char eindeutig_flag,size_t *imatch);  

UINT8_T astr_proof_string(char *ptxt,UINT16_T maxlength);

#ifdef __cplusplus
  }
#endif

#endif

