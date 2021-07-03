/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 2.1 12.11.01 TBert astrs_tget, astrs_tget_quot astrs_change..., astrs_cut..., astrs_change...eingefhrt */
/* 2.0 13.08.01 TBert Release VehicleModell 2.0           */
/* Aenderungen 
001 07.03.01 TBert      Anlegen einer neuen Struktur
Ver Datum*** Wer        Was
*/
/*************************************************************************
* File:             string_a.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            -
* Version:          1.0
* Datum:            6.03.01
*************************************************************************
* Kurzbeschreibung: 

Struktur:
=========

typedef
struct tag_astrs_s {
    char   *pBuffer;
    size_t memsize;
} astrs_s;

  Es wird mit der Struktur astrs_s gearbeitet. Diese enthält den Pointer
  und die Größe des Speichers. Es wird immer der Pointer der Struktur an die
  Funktionen astrs_...() übergeben. Der Pointer muß natürlich initialisiert werden
  mit Pointer = astrs_new();. Damit wird die Verwaltung vollständig in den Funktionen
  astrs_...() durchgeführt.

typedef
struct tag_astrv_s {
    astrs_s      **pp;
    size_t           nrow;
} astrv_s;

  Es wird mit der Struktur astrv_s gearbeitet. Diese enthält den VektorPointer
  mit astrs_s und die Länge des Vektors nrow. Es wird immer der Pointer der Struktur an die
  Funktionen astrv_...() übergeben. Der Pointer muß natürlich initialisiert werden
  mit Pointer = astrv_new();. Damit wird die Verwaltung vollständig in den Funktionen
  astrv_...() durchgeführt.

typedef
struct tag_astrm_s {
    astrs_s      **pp;
    size_t           nrow;
    size_t           ncol;
} astrm_s;

  Es wird mit der Struktur astrm_s gearbeitet. Diese enthält den MatrixPointer
  mit astrs_s und die Größe der Matrix_t ist nrow*ncol. Es wird immer der Pointer der Struktur an die
  Funktionen astrm_...() übergeben. Der Pointer muß natürlich initialisiert werden
  mit Pointer = astrm_new();. Damit wird die Verwaltung vollständig in den Funktionen
  astrm_...() durchgeführt.


=============================
Stringbearbeitungsfunktionen:
=============================

astrs_s *astrs_new(void);
============================

Anlegen einer neuen Datenstruktur.

Rückgabewert    Pointer auf Struktur
Fehler          Rückgabewert NULL

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return err_code;

            printf("\"%s\"",p->pBuffer);   => ""
            printf("%d",p->memsize);       => 0

int astrs_delete(astrs_s *p);
===============================

Löscht Speicher p->pBuffer und  Struktur *p
       ===================      ===========
astrs_delete() muß benutzt weren, um den Speicher
wieder freizugeben. astrs_delete() kann nur benutzt
nach astrs_new().

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return err_code;

            printf("\"%s\"",p->pBuffer);   => ""
            printf("%d",p->memsize);       => 0

            astrs_delete(p);

int astrs_mem(astrs_s *p,size_t n);
======================================

Legt Characterspeicher in der Struktur p an

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_mem(p,100) != 0 )
                return astrs_err();

int astrs_free(astrs_s *p);
==============================

Gibt Characterspeicher frei, die Struktur bleibt erhalten

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_mem(p,100) != 0 )
                return astrs_err();

            if( astrs_free(p) != 0 )
                return astrs_err();

int astrs_err(void);
===================

Gibt den Wert des Errorcodes zurueck.

int astrs_cpy(astrs_s *p,const char *txtcpy);
================================================
int astrs_scpy(astrs_s *p,const astrs_s *pcpy);
================================================

Kopiert txtcpy vollständig in pBuffer von p (p->pBuffer) und
paßt Memory gegebenenfalls an.

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_cpy(p,"Test") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    => "Test"
            
            ...

int astrs_cpy_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1);                     
int astrs_cpy_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1, size_t i);                     
================================================

   Sucht in txt2 den ersten durch die Quotzeichen oder iten (angefangen bei 0)
   txtquot0 und txtquot1 gequoteten string, der in txt1 kopiert wird. 
   Sonderfall:   txtquot0 == NULL => vom ersten Zeichen an 
                 txtquot1 == NULL => bis zum letzten Zeichen

   return 0     wenn gefunden
          -1;    wenn nicht gefunden   

   Beispiel: txt="[sef]"; txtkap0="["; txtkap1="]";
             => p->pBuffer="sef"; return 0;
             

int astrs_cpyi(astrs_s *p,const char *txtcpy,size_t i0, size_t i1);
======================================================================
int astrs_scpyi( astrs_s *p, const astrs_s *pcpy, size_t i0, size_t i1);
===============================================================================

Kopiert txtcpy von Stelle i0 bis i1 in pBuffer von p (p->pBuffer) und
paßt Memory gegebenenfalls an.

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_cpyi(p,"Test von heute",5,7) != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    => "von"
            
            ...

int astrs_cpy_ival(astrs_s *p, const int i);
int astrs_cpy_lval(astrs_s *p, const signed long int i);
int astrs_cpy_ulval(astrs_s *p, const unsigned long int i);
===============================================

  Kopiert in p->pBuffer den Wert von Integer i als string.

int astrs_cpy_dval(astrs_s *p, const double d);
===============================================

  Kopiert in p->pBuffer den Wert von Double d als string.

int astrs_cat(astrs_s *p,const char *txtcat);
================================================
int astrs_scat(astrs_s *p,const astrs_s *pcat);
================================================

Hängt txtcpy vollständig an pBuffer von p (p->pBuffer) an und
paßt Memory gegebenenfalls an.

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_cpy(p,"Test") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    => "Test"

            if( astrs_cat(p," neu") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    => "Test neu"
            
            ...

int astrs_cati( astrs_s *p, const char *txtcat, size_t i0, size_t i1);
=========================================================================
int astrs_scati( astrs_s *p, const astrs_s *pcat, size_t i0, size_t i1);
===============================================================================

Hängt txtcat von der Stelle i0 bis i1 an pBuffer von p (p->pBuffer) an und
paßt Memory gegebenenfalls an.

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_cati(p,"Test von gestern",0,5) != 0 ){
                printf(string_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    => "Test "

            if( astrs_cati(p,"von heute",0,strlen("von")-1) != 0 ){
                printf(string_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    => "Test von"
            
            ...

int astrs_cat_ival(astrs_s *p, const int i);
int astrs_cat_lval(astrs_s *p, const signed long int i);
int astrs_cat_ulval(astrs_s *p, const unsigned long int i);
===============================================

  Hängt an String von p->pBuffer den Wert von Integer i.

int astrs_cat_dval(astrs_s *p, const double d);
===============================================

  Hängt an String von p->pBuffer den Wert von double d.

int astrs_cat_pfe(astrs_s *p,char *path,char *file, char *ext,char *regel);
==============================================================================

  Setzt aus Pfad,filename und extension die Filebezeichnung zusammen
  und schreib sie in p (p->pBuffer).

  Die Regel besagt: ""  -> Benutze Pfad und Extension, auch wenn in filename
                           Pfad und Extension enthalten.
                    "f" -> force filename; Benutze Pfad und extension aus
                           filename, wenn vorhanden
                    "p" -> force path; Benutze auf jeden Fall Pfadangabe.
                           Extension kann aber auch im filenamen stehen.
                    "e" -> Benutze aus jeden Fall extension, Pfad kann aber
                           auch im filename stehen

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_cat_pfe(p,"./cat","./fre/abc.dat","ini","") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    =>   "./cat/abc.ini"
            
            ...

int astrs_smove(astrs_s *p,astrs_s *pmove);
==================================================

Verschieben des Inhalts einer Struktur

int astrs_insert(astrs_s *p, const char *txtinsert, size_t i0, size_t n);
int astrs_insert_full(astrs_s *p, const char *txtinsert, size_t i0);
============================================================================

Einfügen von String txtins in die Stelle i0 von p->pBuffer n Zeichen (bzw. alle Zeichen von txtinsert)

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)
Beispiel:   astrs_s *p=NULL;

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_cpy(p,"123456") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }
            
            if( astrs_insert(p,"ABCDEF",3,3) != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    =>   "123ABC456"
int astrs_sinsert(astrs_s *p, const astrs_s *pinsert, size_t i0, size_t n);
int astrs_sinsert_full(astrs_s *p, const astrs_s *pinsert, size_t i0);
==================================================================================

Einfügen von String aus pinsert->pBuffer in die Stelle i0 von p->pBuffer (bzw. alle Zeichen von pinsert->pBuffer)
n Zeichen

            
            ...
int astrs_cut(astrs_s *p, size_t i0, size_t i1)
==================================================
     Schneidet aus p->pBuffer raus, von Stelle i0 bis i1

int astrs_cut_ae(char *txt1,const char *txtcut)
==============================================
    Schneidet aus p->pBuffer am Anfang und Ende um die Zeichen txtcut
    z.B. p->pBuffer = "   abc def   ", txtcut = " " => txt1="abc def"
    
    return 0   okay

int astrs_cut_e(astrs_s *p,const char *txtcut)
=================================================
    Schneidet aus p->pBuffer am Ende um die Zeichen txtcut
    z.B. p->pBuffer = "   abc def   ", txtcut = " " => txt1="   abc def"
    
    return 0   okay

int astrs_cut_a(astrs_s *p,const char *txtcut)
=================================================
    Schneidet aus p->pBuffer am Ende um die Zeichen txtcut
    z.B. p->pBuffer = "   abc def   ", txtcut = " " => txt1="abc def   "
    
    return 0   okay

int astrs_cut_comment(astrs_s *p,const char *txtcomment)
===========================================================
    Sucht nach Kommentarzeichen im Text und schneidet ab dem Kommentarzeichen
    den Text raus
    
    return 0, wenn kein Fehler auftritt

int astrs_cut_comment_unquoted(astrs_s *p,const char *txtcomment,const char *txtquot);
===========================================================================================
    Wie oben, es wird aber nur im nicht gequoteten Bereich gesucht
    z.B.
    astrs_cpy(ps,"abc = ""!!!""! Ausrufezeichen");
    astrs_cut_comment_unquoted(ps,"!","""");
    ps-pBuffer => abc "!!!"

char *astrs_string(astrs_s *p);
==================================
    Gibt den pointer auf den String p->pBuffer zurück.
    Wenn Fehler dann return NULL;

size_t astrs_memsize(astrs_s *p);
==================================
    Gibt Länge de allocierten Speichers zurück.

size_t astrs_len(astrs_s *p);
==================================
    Gibt Länge des String p->pBuffer zurück.

int astrs_change(astrs_s *p,char *txtsuch,char *txtchange)
===========================================================
    Sucht in p->pBuffer die Zeichen txtsuch und ersetzt es mit txtchange
    
    return ncount   Anzahl der changes

int astrs_change_quot(astrs_s *p,const char *txtsuch,const char *txtchange,
                       const char *txtquot0, const char *txtquot1,const char *regel,
                       int istatus)
==================================================================
int astrs_change_quot1(astrs_s *p,const char *txtsuch,const char *txtchange,
                       const char *txtquot0, const char *txtquot1,const char *regel,
                       int *istatus)
===================================================================
    Sucht in p-> pBuffer die durch txtquot0 und txtquot1 eingeschlossenen
    String nach txtsuch und ersetzt durch txtchange (wenn txtchange = "\0",
    dann wird txtsuch rausgeschnitten)
    
    regel = "i"   Es wird der string innerhalb des Quots behandelt
            "a"   Es wird der string ausserhalb des Quots beh.
             
    istatus = 0   Das erste Zeichen von txt ist ausserhalb des Quots
              1   "                             innerhalb     " 
    ( bei string_change_quot1() wird pointer auf istatus übergeben, damit
    der letzte Zustannd nach durchforsten des Strings zurückgegeben wird)
              
    return ncount       Anzahl der Gefundenen Stellen
    
    Beispiel:   txt="  [ab c] [def g]  "; txtsuch=" ";txtchange="\0";
                txtquot0="[";txtquot1="]";regel="i";istatus=0;
                => txt="  [abc] [defg]  "; return 2;
                
                mit regel = "a";
                => txt="[ab c][def g]"; return 5;

int astrs_change_ae(astrs_s *p,char *txtsuch,char *txtchange);
int astrs_change_a(astrs_s *p,char *txtsuch,char *txtchange);
int astrs_change_e(astrs_s *p,char *txtsuch,char *txtchange);
===========================================================
    Sucht in p->pBuffer die Zeichen txtsuch und ersetzt es mit txtchange
    am Anfang und Ende (ae), am Anfang (a) oder am Ende (e) des strings p->pBuffer.
    
    return ncount   Anzahl der changes

int astrs_lower(astrs_s *p);
===============================
int astrs_upper(astrs_s *p);
===============================

Schreibt alles aus Buffer groß oder klein

int astrs_extract_t(astrs_s *ptrennz,const char *txt,const char *auswahl);
===============================================================================

Sucht das Trennzeichen aus der Auswahl in txt und schreibt es in ptrennz
Wenn keins gefunden, dann erstes aus auswahl                            

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_extract_t(p,".\\cat\\abc","/\\") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    =>   "\"

  oder 
            if( astrs_extract_t(p,"abc.txt","/\\") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    =>   "/"
            
            ...

int astrs_tget(astrs_s *p,const char *txt1,const char *txttrenn,size_t istelle);
====================================================================================
 
 Sucht in Liste txt1 die Stelle istelle mit txttrenn getrennte String         
 und gibt ihn in *p->pBuffer zurück.  pBuffer wird automatisch verwaltet       

 Rückgabewert    == 0, wenn oky
                 != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

 Beispiel txt1 = "abc,cde,wef"; istelle = 1; txttrenn = ","                   
 
 => p->pBuffer = "cde"; return 0                                             

int astrs_tget_quot(astrs_s *p,char *txt1,const char *txttrenn,int istelle,
                       char *txtquot0,char *txtquot1);
============================================================================
   Kopiert aus txt1 den Block istelle (von null gezaehlt) in *p->pBuffer,
   wobei der Text mit txtrenn aufgeteilt sein muss. .  pBuffer wird 
   automatisch verwaltet. Laesst Text innerhalb von txtquot0 und txtquot1 
   ausser acht.


   Rueckgabewert: tatsaechliche Rückgabewert

   Beispiel: txt1="abc \" e d f\" f"; txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=0 => p->pBuffer ="abc" return(0);
             txt1="abc  f";   txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=2 => p->pBuffer = "" return(0); (=> deswegen besser 
                                      vorher die Funktion string_change_quot verwenden)
             txt1="";         txttrenn=" ";txtquot0="\"";txtquot1="\"",
                                      istelle=0 -> => p->pBuffer ="" return(0);


int astrs_get_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1);
=======================================================================================

  Extrahiert aus txt den in txtquot0 (links) und txtquot1 (rechts) gequotete Text raus

  return 0,  wenn gefunden
         !0, wenn nicht gefunden

  Beispiel 
            astrs_s *ps=astrs_new();
            astrs_get_quot(ps,"[abc]","[","]");
               
            return 0 
            p->pBuffer = "abc"


int astrs_extract_pfe(astrs_s *p,const char *txt,const char *regel) ;
========================================================================

Extrahiert aus txt nach der Regel Pfad, File und Extension und kopiert es in die
 Struktur p (p->pBuffer). In regel kann stehen:                          
 "p"    Pfad, 
 "f" wie filename und 
 "e" wie extension                        
 
 z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn       
 vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt


Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *p=NULL;
            p = astrs_new();
            if(p == NULL) return astrs_err();

            if( astrs_extract_pfe(p,"./cat/abc","p") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    =>   "./cat/"

  oder 
            if( astrs_extract_pfe(p,"./cat/abc.txt","fe") != 0 ){
                printf(strimg_message());
                return astrs_err();
            }

            printf("\"%s\"",p->pBuffer);    =>   "abc.txt"
            
            ...

int astrs_match_spath(astrs_s *ps_new_path,const astrs_s *ps_path0,const astrs_s *ps_path1);
int astrs_match_path(astrs_s *ps_new_path,const char *p_path1,const char *p_path1);
========================================================================

Verbindet path0 mit path1 und kopiert es in new_path

  Rückgabewerte 0, wenn okay

int astrs_ssuch(astrs_s *ps_txt,astrs_s *ps_such,char *regel) ;
int astrs_such(astrs_s *ps_txt,char *p_such,char *regel);
========================================================================

     Sucht string p_such bzw. ps_such in ps_txt mit den Regeln aus regel

     regel = "vs" sucht      string von p_such         vorwaerts   in ps_txt
             "rs" sucht      "                          rueckwaerts "
             "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
             "rn" sucht "                               rueckwaerts "
             
     return(-1)  nicht gefunden ansonsten
     return(i)   Stelle wo gefunden beginnend mit 0

int astrs_such_i0i1(astrs_s *ps_txt, char txt2, const char *regel,
                        size_t ianf, size_t iend)
========================================================================

     Sucht string txt2 in Struktur ps_txt mit den Regeln aus regel, zwischen
     *(txt1+ianf) und *(txt1+iend) und gibt die gefundene Stelle
     absolut zurück

     regel = "vs" sucht      string von txt2            vorwaerts   in txt1
             "rs" sucht      "                          rueckwaerts "
             "vn" sucht wenn string nicht mehr auftritt vorwaerts   "
             "rn" sucht "                               rueckwaerts "
             
     return (-1)  Fehler ansonsten
     return (i)   Stelle wo gefunden beginnend mit 0


int astrs_such_quot(astrs_s *ps_txt,char *txtsuch,const char *suchregel,
                       char *txtquot0, char *txtquot1,char *quotregel,
                       int istatus);
========================================================================
    Sucht string txtsuch in Struktur ps_txt nach der suchregel
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

     z.B. astrs_such_quot(ps_text,"abc","vs","[","]","i",0);

int astr_such_pfe(astrs_s *ps_input,const char *pregel);
========================================================================

    Sucht in ps_inpout Pfad,Filename und Extension nach der vorgegeben
    Regel.

    pregel[] = "pfe"  p => Pfad, f => Filename, e => Extension
    z.B. astrs_cpy(ps_input,"./abc.dat"); pregel[] = "fe" => return 0
    z.B. astrs_cpy(ps_input,"abc.dat");   pregel[] = "p" => return -1

    return 0, wenn alles gefunden wurde, ansonsten -1


====================================================================================
Vektor-Stringbearbeitungsfunktionen:
====================================================================================

astrv_s *astrv_new(void);
============================

Anlegen einer neuen Vektor-Datenstruktur.

Rückgabewert    Pointer auf Struktur astrv_s
Fehler          Rückgabewert NULL

Beispiel:   astrv_s *pv=NULL;

            pv = astrv_new();
            if(pv == NULL) return astrs_err();


int astrv_delete(astrv_s *pv);
===============================

Löscht Vektor *pv und  Struktur pv->pp[i]->pBuffer
astrv_delete() muß benutzt werden, um Speicher wieder 
freizugeben. astrv_delete() kann nur benutzt
nach astrv_new() oder astrv_make().

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrv_s *pv=NULL;

            pv = astrv_new();
            if(pv == NULL) return astrs_err();

            astrv_delete(pv);

int astrv_make(astrv_s *pv,size_t irow);
=======================================================

Legt Characterspeicher in der Struktur pv an

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *pv=NULL;


            if( astrv_make(pv,100) != 0 ) 
                return astrs_err();

  100 Zeilen mit jeweiles String-Strukturen wurden angelegt

int astrv_cpy(astrv_s *pv,size_t irow,char *pstring);
int astrv_scpy(astrv_s *pv,size_t irow,astrs_s *pquelle);
int astrv_mcpy(astrv_s *pv,size_t irow,
               astrv_s *pvquelle,size_t irowquelle);
=================================================================================

  astrv_cpy  kopiert einen string an die Stelle irow von pv 
             (pv->pp[irow]->pBuffer = pstring)
  astrv_scpy kopiert String aus Struktur pquelle in den Vekktor pv an der Stelle irow
             (pm->pp[irow]->pBuffer = pquelle->pBuffer)
  astrv_vcpy kopiert String aus dem Vektor an der Stelle irowquelle
             in den Vektor pv an der Stelle irow
             (pv->pp[irow]->pBuffer = pvquelle->pp[irowquelle]->pBuffer)

int             astrv_insert(astrv_s *pv,char *p_insert,size_t irow);
int             astrv_sinsert(astrv_s *pv,astrs_s *ps_insert,size_t irow);
=================================================================================

  Fügt an der Stelle irow des Vektors den Text p_insert oder Struktur ps_insert ein.
  Der Vektor erhöht sich um eins

int             astrv_ssuch(astrv_s *pv,astrs_s *ps_such,size_t *p_i_found);
int             astrv_such(astrv_s *pv,char *p_such,size_t *p_i_found);
int             astrv_ssuch_irow(astrv_s *pv,astrs_s *ps_such,size_t *p_i_found,size_t irow);
int             astrv_such_irow(astrv_s *pv,char *p_such,size_t *p_i_found,size_t irow);
=======================================================================

  Sucht string p_such oder string-Struktur ps_such in Struktur pv und
  gibt die Position im Vektor in *p_i_found zurück.
  Wenn gefunden gibt die Funktion TRUE(1) zurück, wenn nicht, gibt Funktion FALSE(0) zurück
  Der String muß vollständig in der Vektor-Zelle enthalten sein.
  irow gibt die Position an, ab der im Vektor der String gesucht wird

char *astrv_string(astrv_s *pv, size_t irow);
==================================
    Gibt den pointer auf den String pv->pp[irow]->pBuffer zurück.
    Wenn Fehler dann return NULL;

char *astrv_last_string(astrv_s *pv);
==================================
    Gibt den pointer auf den letzten String pv->pp[pv->nrow]->pBuffer zurück.
    Wenn Fehler dann return NULL;

size_t          astrv_len(astrv_s *pv,size_t irow);
==================================
    Gibt Stringlänge des Elements irow zurück


int astrv_split(astrv_s *pv,char *p_text,char *p_split);
==================================

    Zerlegt p_text in einzelenen Teile in pv mit Trennzeichen p_split
    retunr 0 wenn okay


int astrv_scut(astrv_s *pv,size_t irow0,size_t irow1);
int astrv_scut_first(astrv_s *pv);
int astrv_scut_last(astrv_s *pv);
=======================================================================

  Schneidet Vektor irow0 bis irow1 aus
  bzw. erster Vektorwert (first)
  bzw. letzter Vektorwert (last)
====================================================================================
Matrixx-Stringbearbeitungsfunktionen:
====================================================================================

astrm_s *astrm_new(void);
============================

Anlegen einer neuen Matrix_t-Datenstruktur.

Rückgabewert    Pointer auf Struktur astrm_s
Fehler          Rückgabewert NULL

Beispiel:   astrm_s *pm=NULL;

            pm = astrm_new();
            if(pm == NULL) return err_code;


int astrm_delete(astrm_s *pm);
===============================

Löscht Matrix_t *pm und  Struktur pm->pp[i]->pBuffer
astrm_delete() muß benutzt weren, um den Speicher
wieder freizugeben. astrm_delete() kann nur benutzt
nach astrm_new() oder astrm_make().

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrm_s *pm=NULL;

            pm = astrm_new();
            if(pm == NULL) return err_code;

            astrm_delete(pm);

int astrm_make(astrm_s *pm,size_t row, size_t col);
=======================================================

Legt Characterspeicher in der Struktur p an

Rückgabewert    == 0, wenn oky
                != 0, wenn nicht okay (Message in string_message(),
                      Error siehe Defines)

Beispiel:   astrs_s *pm=NULL;


            if( astrm_make(pm,100,2) != 0 ) 
                return astrs_err();

  100 Zeilen und 2 Spalten mit jeweiles String-Strukturen wurden angelegt

int astrm_cpy(astrm_s *pm,size_t irow, size_t icol,char *pstring);
int astrm_scpy(astrm_s *pm,size_t irow, size_t icol,astrs_s *pquelle);
int astrm_mcpy(astrm_s *pm,size_t irow, size_t icol,
               astrm_s *pmquelle,size_t irowquelle, size_t icolquelle);
=================================================================================

  astrm_cpy  kopiert einen string an die Stelle irow,icol von pm 
             (pm->pp[icol*nrow+irow]->pBuffer = pstring)
  astrm_scpy kopiert String aus Struktur pquelle in die Matrixx pm an der Stelle irow,icol
             (pm->pp[icol*nrow+irow]->pBuffer = pquelle->pBuffer)
  astrm_mcpy kopiert String aus der Matrix_t an der Stelle irowquelle,icolquellee  
             in die Matrixx pm an der Stelle irow,icol
             (pm->pp[icol*nrow+irow]->pBuffer = pmquelle->pp[icolquelle*nrowquelle+irowquelle]->pBuffer)

int astrm_cat(astrm_s *pm,size_t irow, size_t icol,char *pstring);
int astrm_scat(astrm_s *pm,size_t irow, size_t icol,astrs_s *pquelle);
int astrm_mcat(astrm_s *pm,size_t irow, size_t icol,
               astrm_s *pmquelle,size_t irowquelle, size_t icolquelle);
=================================================================================

  astrm_cat  hängt einen string an die Stelle irow,icol von pm an
             (pm->pp[icol*nrow+irow]->pBuffer += pstring)
  astrm_scat hängt String aus Struktur pquelle in die Matrixx pm an der Stelle irow,icol an
             (pm->pp[icol*nrow+irow]->pBuffer += pquelle->pBuffer)
  astrm_mcat hängt String aus der Matrix_t an der Stelle irowquelle,icolquellee  
             in die Matrixx pm an der Stelle irow,icol an
             (pm->pp[icol*nrow+irow]->pBuffer = pmquelle->pp[icolquelle*nrowquelle+irowquelle]->pBuffer)

size_t             astrm_cat_row(astrm_s *pm,size_t icol,char *p_string);
size_t             astrm_scat_row(astrm_s *pm,size_t icol,astrs_s *ps);
size_t          astrm_find_or_cat_row(astrm_s *pm,size_t icol,char *p_name);
=================================================================================

  astrm_cat_row hängt an Matrix_t-Struktur in der Spalte icol den string an

  astrm_scat_row hängt an Matrix_t-Struktur in der Spalte icol den string aus ps an

  astrm_find_or_cat_row sucht oder wenn nicht gefunden hängt string an der stelle icol an

  Gibt irow zurück, sie Stelle an der der String angehängt wurde

char *astrm_string(astrm_s *pm, size_t irow, size_t icol);
==================================
    Gibt den pointer auf den String pv->pp[icol*pv->nrow+irow]->pBuffer zurück.
    Wenn Fehler dann return NULL;

************************************************************************/
/* includes */

#ifndef STRING_A_INCLUDE
#define STRING_A_INCLUDE

#include <stddef.h>
#include "definer.h"

/* Defines */

#define ASTRS_NO_MEMORY                  101
#define ASTRS_FREE_ERR                   102
#define ASTRS_P_NOT_INITIALISED          103
#define ASTRS_WRONG_RULE                 104
#define ASTRS_WRONG_PARAMETER            105
#define ASTRS_NO_CHARACTER_IN_PARAMETER  106
#define ASTRS_ERROR_IN_STRING_S          107
#define ASTRS_WRONG_OFFSET               108
#define ASTRS_QUOT_NOT_FOUND             109
#define ASTRS_TXTSUCH_IN_TXTCHANGE       110 

#define ASTRS_WRONG_OFFSET_WARNING       201
#define ASTRS_WRONG_LENGTH_WARNING       202

#define ASTRV_PV_NOT_INITIALISED        401
#define ASTRV_WRONG_PARAMETER           402

#define ASTRM_PM_NOT_INITIALISED        301
/* Strukturdefinition */
typedef
struct tag_astrs_s {
    char      *pBuffer;
    size_t    memsize;
} astrs_s;
typedef
struct tag_astrv_s {
    astrs_s      **pp;
    size_t           nrow;
} astrv_s;
typedef
struct tag_astrm_s {
    astrs_s      **pp;
    size_t           nrow;
    size_t           ncol;
} astrm_s;

#ifdef  __cplusplus
extern "C" {
#endif

astrs_s     *astrs_new(void);
int             astrs_delete(astrs_s *p);
int             astrs_mem(astrs_s *p,size_t n);
int             astrs_free(astrs_s *p);
int             astrs_err(void);
char*           astrs_err_text(void);

int astrs_cpy(astrs_s *p,const char *txtcpy);
int astrs_cpy_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1);                     
int astrs_cpy_iquot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1, size_t i);                     
int astrs_cpyi( astrs_s *p, const char *txtcpy, size_t i0, size_t i1);
int astrs_scpy(astrs_s *p,const astrs_s *pcpy);
int astrs_scpyi( astrs_s *p, const astrs_s *pcpy, size_t i0, size_t i1);
int astrs_cpy_ival(astrs_s *p, const int i);
int astrs_cpy_lval(astrs_s *p, const signed long int l);
int astrs_cpy_ulval(astrs_s *p, const unsigned long int ul);
int astrs_cpy_dval(astrs_s *p, const double d);

int astrs_cat(astrs_s *p,const char *txtcat);
int astrs_cati( astrs_s *p, const char *txtcat, size_t i0, size_t i1);
int astrs_scat(astrs_s *p,const astrs_s *pcat);
int astrs_scati( astrs_s *p, const astrs_s *pcat, size_t i0, size_t i1);
int astrs_cat_ival(astrs_s *p, const int i);
int astrs_cat_lval(astrs_s *p, const signed long int l);
int astrs_cat_ulval(astrs_s *p, const unsigned long int ul);
int astrs_cat_dval(astrs_s *p, const double d);
int astrs_cat_pfe(astrs_s *p,const char *path,const char *file,const char *ext,const char *regel);

int astrs_smove(astrs_s *p,astrs_s *pmove);

int astrs_insert(astrs_s *p, const char *txtinsert, size_t i0, size_t n);
int astrs_insert_full(astrs_s *p, const char *txtinsert, size_t i0);
int astrs_sinsert(astrs_s *p, const astrs_s *pinsert, size_t i0, size_t n);
int astrs_sinsert_full(astrs_s *p, const astrs_s *pinsert, size_t i0);

int astrs_cut(astrs_s *p, size_t i0, size_t i1);
int astrs_cut_ae(astrs_s *p,const char *txtcut);
int astrs_cut_a(astrs_s *p,const char *txtcut);
int astrs_cut_e(astrs_s *p,const char *txtcut);
int astrs_cut_comment(astrs_s *p,const char *txtcomment);
int astrs_cut_comment_unquoted(astrs_s *p,const char *txtcomment,const char *txtquot);

char *astrs_string(astrs_s *p);
size_t astrs_memsize(astrs_s *p);
size_t astrs_len(const astrs_s *p);

int astrs_change(astrs_s *p,const char *txtsuch,const char *txtchange);
int astrs_change_quot(astrs_s *p,const char *txtsuch,const char *txtchange,
                       const char *txtquot0, const char *txtquot1,const char *regel,
                       int istatus);
int astrs_change_quot1(astrs_s *p,const char *txtsuch,const char *txtchange,
                       const char *txtquot0,const char *txtquot1,const char *regel,
                       int *istatus);
int astrs_change_ae(astrs_s *p,const char *txtsuch,const char *txtchange);
int astrs_change_a(astrs_s *p,const char *txtsuch,const char *txtchange);
int astrs_change_e(astrs_s *p,const char *txtsuch,const char *txtchange);

int astrs_extract_t(astrs_s *ptrennz,const char *txt,const char *auswahl);
int astrs_extract_pfe(astrs_s *p,const char *txt,const char *regel) ;

int astrs_lower(astrs_s *p);
int astrs_upper(astrs_s *p);

int astrs_tget(astrs_s *p,const char *txt1,const char *txttrenn,size_t istelle);
int astrs_tget_quot(astrs_s *p,const char *txt1,const char *txttrenn,size_t istelle,const char *txtquot0,const char *txtquot1);

int astrs_get_quot(astrs_s *p,const char *txt,const char *txtquot0,const char *txtquot1);

int astrs_match_spath(astrs_s *ps_new_path,astrs_s *ps_path0,astrs_s *ps_path1);
int astrs_match_path(astrs_s *ps_new_path,const char *p_path0,const char *p_path1);
int astrs_match_spath_file(astrs_s *ps_path_file,astrs_s *ps_path,astrs_s *ps_file);
int astrs_match_path_file(astrs_s *ps_path_file,const char *p_path,const char *p_file);


int astrs_ssuch(astrs_s *ps_txt,astrs_s *ps_such,char *regel) ;
int astrs_such(astrs_s *ps_txt,char *p_such,char *regel);
int astrs_such_i0i1(astrs_s *ps_txt, const char *text, const char *regel, size_t i0,size_t i1);
int astrs_such_quot(astrs_s *ps_txt,const char *txtsuch,const char *suchregel,
                    const char *txtquot0, const char *txtquot1,const char *quotregel,
                    UINT8_T istatus);
int astrs_such_pfe(astrs_s *ps_input,const char *pregel);

astrm_s         *astrm_new(void);
int             astrm_make(astrm_s *pm,size_t nrow, size_t ncol);
int             astrm_delete(astrm_s *pm);
int             astrm_free(astrm_s *pm);
int             astrm_cpy(astrm_s *pm,size_t irow, size_t icol,char *pstring);
int             astrm_cpyi(astrm_s *pm,size_t irow, size_t icol,char *pstring,size_t i0, size_t i1);
int             astrm_scpy(astrm_s *pm,size_t irow, size_t icol,astrs_s *pquelle);
int             astrm_vcpy(astrm_s *pm,size_t icol,astrv_s *pquelle);
int             astrm_mcpy(astrm_s *pm,size_t irow, size_t icol,
                           astrm_s *pmquelle,size_t irowquelle, size_t icolquelle);

int             astrm_cat(astrm_s *pm,size_t irow, size_t icol,char *pstring);
int             astrm_cati(astrm_s *pm,size_t irow, size_t icol,char *pstring,size_t i0, size_t i1);
int             astrm_scat(astrm_s *pm,size_t irow, size_t icol,astrs_s *pquelle);
size_t          astrm_find_or_cat_row(astrm_s *pm,size_t icol,char *p_name);
size_t          astrm_find_or_scat_row(astrm_s *pm,size_t icol,astrs_s *ps_name);
size_t          astrm_cat_row(astrm_s *pm,size_t icol,char *pstring);
size_t          astrm_scat_row(astrm_s *pm,size_t icol,astrs_s *ps);

int             astrm_cut_irow(astrm_s *pm,size_t irow);
int             astrm_cut_icol(astrm_s *pm,size_t icol);
int             astrm_cut_ae(astrm_s *pm,char *txtcut);
char            *astrm_string(astrm_s *pm,size_t irow, size_t icol);
astrs_s         *astrm_astrs(astrm_s *pm,size_t irow, size_t icol);
size_t          astrm_nrow(astrm_s *pm);
size_t          astrm_ncol(astrm_s *pm);
size_t          astrm_len(astrm_s *pm,size_t irow, size_t icol);

astrv_s         *astrv_new(void);
int             astrv_make(astrv_s *pv,size_t nrow);
int             astrv_free(astrv_s *pv);
int             astrv_delete(astrv_s *pv);
int             astrv_cpy(astrv_s *pv,size_t irow,char *pstring);
int             astrv_cpyi(astrv_s *pv,size_t irow,char *pstring,size_t i0, size_t i1);
int             astrv_scpy(astrv_s *pv,size_t irow, astrs_s *pquelle);
int             astrv_vcpy(astrv_s *pv,size_t irow, astrv_s *pvquelle,size_t irowquelle);
int             astrv_mcpy(astrv_s *pv,size_t irow, astrm_s *pmquelle,size_t irowquelle,size_t icolquelle);

int             astrv_vcat_full(astrv_s *pv, astrv_s *pvquelle);
int             astrv_scat(astrv_s *pv, astrs_s *psquelle);
int             astrv_cat(astrv_s *pv, char *quelle);
int             astrv_cat_str(astrv_s *pv, size_t irow, char *quelle);

int             astrv_scut(astrv_s *pv,size_t irow0,size_t irow1);
int             astrv_scut_first(astrv_s *pv);
int             astrv_scut_last(astrv_s *pv);
int             astrv_scut_such(astrv_s *pv,char *p_such);
int             astrv_insert(astrv_s *pv,char *p_insert,size_t irow);
int             astrv_sinsert(astrv_s *pv,astrs_s *ps_insert,size_t irow);
int             astrv_ssuch(astrv_s *pv,astrs_s *ps_such,size_t *p_i_found);
int             astrv_such(astrv_s *pv,char *p_such,size_t *p_i_found);
int             astrv_ssuch_irow(astrv_s *pv,astrs_s *ps_such,size_t *p_i_found,size_t irow);
int             astrv_such_irow(astrv_s *pv,char *p_such,size_t *p_i_found,size_t irow);
int             astrv_split(astrv_s *pv,char *p_text,char *p_split);
int             astrv_split_quot(astrv_s *pv,char *p_text,char *p_split,char *p_quot0,char *p_quot1);

char            *astrv_string(astrv_s *pv,size_t irow);
char            *astrv_string_last(astrv_s *pv);
char            *astrv_last_string(astrv_s *pv);
astrs_s         *astrv_astrs(astrv_s *pv,size_t irow);
size_t          astrv_nrow(astrv_s *pv);
size_t          astrv_len(astrv_s *pv,size_t irow);


#ifdef  __cplusplus
}
#endif

#endif

/*==========================================================================*/
