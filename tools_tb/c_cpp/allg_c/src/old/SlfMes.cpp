#include "SlfMes.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

CSlfMes SlfErrMes;

// Konstruktor CSlfMes
CSlfMes::CSlfMes() {
    mes_text.clear();
    set_flag = false;
}
bool CSlfMes::exist() {
    return set_flag;
}
void CSlfMes::set(const char *text) {
    if( !set_flag ) {
        mes_text.clear();
        set_flag = true;
    }
    mes_text.append(text);
}
//void CSlfMes::set(const std::string &str_text) {
//  CSlfMes::set(str_text.c_str());
//}
void CSlfMes::set(const CSlfStr &str_text) {
  CSlfMes::set(str_text.c_str());
}
//void CSlfMes::set(const sint64 ival) {
void CSlfMes::set(const sint32 ival) {
  //std::ostringstream temp;
  //temp << ival;
  //CSlfMes::set(temp.str());
  char buffer[100];
  //sprintf_s(buffer,100,"%l",ival);
  sprintf(buffer,"%l",ival);
  CSlfMes::set(buffer);
}
//void CSlfMes::set(const uint64 uival) {
//  std::ostringstream temp;
//  temp << uival;
//  CSlfMes::set(temp.str());
//}
void CSlfMes::setEndl(const char *text) {
  CSlfMes::set(text);
  mes_text.append("\n");
}
//void CSlfMes::set_endl(const std::string &str_text) {
//  CSlfMes::set(str_text.c_str());
//  mes_text.append("\n");
//}
void CSlfMes::setEndl(const CSlfStr &str_text) {
  CSlfMes::set(str_text.c_str());
  mes_text.append("\n");
}
//void CSlfMes::setEndl(const sint64 ival) {
void CSlfMes::setEndl(const sint32 ival) {
  CSlfMes::set(ival);
  mes_text.append("\n");
}
//void CSlfMes::set_endl(const uint64 uival) {
//  CSlfMes::set(uival);
//  mes_text.append("\n");
//}
/*======================================================================================================*/
status_t CSlfMes::setFormat(char *pformat,...) {
    
    CSlfMes::SFormatListe *pformat_liste=0;
    CSlfMes::SFormatListe *p_f_l,*p_f_l1;
    uint16      n_format_liste;
    CSlfStr  text;
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
        
    if( setFormatListe(pformat,&pformat_liste,&n_format_liste) == NOT_OKAY ) {
        printf("Error in <CSlfMes::set_format> aufgetreten\n");
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
                  
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                  //sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
                  sprintf(p,p_f_l->format.c_str(),ivalue);
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

                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),ivalue);
                sprintf(p,p_f_l->format.c_str(),ivalue);
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"e") == 0) 
                     || (strcmp(p_f_l->ptype,"E") == 0) 
                     || (strcmp(p_f_l->ptype,"f") == 0) 
                     || (strcmp(p_f_l->ptype,"g") == 0) 
                     || (strcmp(p_f_l->ptype,"G") == 0) 
                     ) {
                dvalue = va_arg( marker, double );
            
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),dvalue);
                sprintf(p,p_f_l->format.c_str(),dvalue);
                format_text.append(p);
                delete  [] p;
                        
            } else if(  (strcmp(p_f_l->ptype,"n") == 0) 
                ) {
                p_ivalue = va_arg( marker, int* );
            
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_ivalue);
                sprintf(p,p_f_l->format.c_str(),p_ivalue);
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"p") == 0) 
                ) {
                p_void = va_arg( marker, void* );
            
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_void);
                sprintf(p,p_f_l->format.c_str(),p_void);
                format_text.append(p);
                delete  [] p;
                        
            } else if(  (strcmp(p_f_l->ptype,"s") == 0)
                ) {
                p_string = va_arg( marker, char* );
            
                  uint32  ll = MAX((uint32)strlen(p_string),p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),p_string);
                sprintf(p,p_f_l->format.c_str(),p_string);
                format_text.append(p);
                delete  [] p;
                        
            } else if( (strcmp(p_f_l->ptype,"lu") == 0) ) {
            
                luivalue = va_arg( marker, long unsigned int );
            
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),luivalue);
                sprintf(p,p_f_l->format.c_str(),luivalue);
                format_text.append(p);
                delete  [] p;
            
            } else if(  (strcmp(p_f_l->ptype,"ld") == 0) 
                || (strcmp(p_f_l->ptype,"li") == 0) 
                || (strcmp(p_f_l->ptype,"lo") == 0) 
                || (strcmp(p_f_l->ptype,"lx") == 0) 
                || (strcmp(p_f_l->ptype,"lX") == 0) 
                ) {
                livalue = va_arg( marker, long int );
            
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),livalue);
                sprintf(p,p_f_l->format.c_str(),livalue);
                format_text.append(p);
                delete  [] p;
                        
            } else if( (strcmp(p_f_l->ptype,"hu") == 0) ) {
            
                suivalue = va_arg( marker, short unsigned int );

                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),suivalue);
                sprintf(p,p_f_l->format.c_str(),suivalue);
                format_text.append(p);
                delete  [] p;
            
            
            } else if(  (strcmp(p_f_l->ptype,"hd") == 0) 
                || (strcmp(p_f_l->ptype,"hi") == 0) 
                || (strcmp(p_f_l->ptype,"ho") == 0) 
                || (strcmp(p_f_l->ptype,"hx") == 0) 
                || (strcmp(p_f_l->ptype,"hX") == 0) 
                ) {
                sivalue = va_arg( marker, short int );
            
                  int  ll = MAX(SLF_PMES_DEFAULT_LENGTH,p_f_l->width)+1;
                  char *p = new char[ll];
                //sprintf_s(p,ll,p_f_l->format.c_str(),sivalue);
                sprintf(p,p_f_l->format.c_str(),sivalue);
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


    /* erstellten Text in struktur anhängen */
    set(text);

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
status_t CSlfMes::setFormatListe(char *pformat,CSlfMes::SFormatListe **ppformat_liste
                                            ,uint16 *n_format_liste) {
    
    uint16 n;
    uint16 i,itype;
    uint32 i1,i2,ifound;
    SFormatListe *pformat_liste=0;
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
        pformat_liste = new SFormatListe;

        if( *ppformat_liste == 0 )
          *ppformat_liste = pformat_liste;
        else {
          SFormatListe *pP = *ppformat_liste;
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
              
              //pformat_liste->format = format.substr(0,ifound+strlen(format_typ[itype]));
              SlfStrCpy (pformat_liste->format, format, 0, ifound+strlen(format_typ[itype]));
                
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
                pformat_liste->width = (uint16)atol(text.c_str());                
              else
                pformat_liste->width = SLF_PMES_DEFAULT_LENGTH;

              /* format für nächste while loop bereinigen */
              format.cut(0,ifound+strlen(format_typ[itype]));
                
            } else {
                /* Fehler: Format nicht vorhanden */
            }
                        
        }
                
    }
        
    return OKAY;
}

char *CSlfMes::get() {

    if( set_flag ) {
        set_flag = false;
    } else {
        mes_text.clear();
    }
    
    return (char *)mes_text.c_str();
}
void CSlfMes::prompt() {

  if( set_flag ) {
//    std::cout << mes_text << std::endl;
    printf("%s\n",mes_text.c_str());
  }
}
void CSlfMes::clear() {
    if( set_flag ) {
        set_flag = false;
        mes_text.clear();
    }
}
