//
// StringHelp.cpp
//
// Funktionen auf die std::string-Klasse

#include "HlpString.h"
#include "string.h"
# define  HLP_STRING_MAX(x,y)    (((x) > (y)) ? (x) : (y))
# define  HLP_STRING_MIN(x,y)    (((x) < (y)) ? (x) : (y))


namespace Hlp
{
  const std::size_t npos = -1;
  void StrFindQuoted(const char    *txt      // Text der zu durchkämmen
                    ,std::size_t   pos0          // Start, wo gesucht wird
                    ,std::size_t   len0          // Länge, des Stücks zum Suchen
                    ,const char    *txtq0    // Start-Quot
                    ,const char    *txtq1    // End-Quot
                    ,const char    *quotregel// "i": innerhalb quot mit einsen belegen
                                             // "a": außerhalb quot mit -"-
                    ,unsigned char *pmarker      // VEktor mit länge len0
                    ,char          istatus       // 0:startet ausserhalb quot
                                                 // 1:startet innerhalb quot
                    );

  //==========================================================================
  // einfache Suche nach txtsuch
  // regel = "vs", "rs", "vn", "rn"
  std::size_t StringFind(std::string &str, const char *txtsuch, const char *regel)
  {
    return StringFind(str.c_str(),0,str.size(),txtsuch,regel);
  }
  std::size_t StringFind(std::string &str, std::size_t pos0, const char *txtsuch, const char *regel)
  {
    return StringFind(str.c_str(),pos0,str.size()-pos0,txtsuch,regel);
  }
  std::size_t StringFind(std::string &str, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *regel)
  {
    return StringFind(str.c_str(),pos0,len0,txtsuch,regel);
  }
  std::size_t StringFind(const char *txt, const char *txtsuch, const char *regel)
  {
    return StringFind(txt,0,strlen(txt),txtsuch,regel);
  }
  std::size_t StringFind(const char *txt, std::size_t pos0, const char *txtsuch, const char *regel)
  {
    return StringFind(txt,pos0,strlen(txt)-pos0,txtsuch,regel);
  }
  std::size_t StringFind(const char *txt, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *regel)
  {
    std::size_t l2;
    std::size_t i,j,k;
    std::size_t pos1;
  
    if( pos0+len0 > 0 ) pos1 = pos0+len0-1;
    else                pos1 = 0;

    if( strlen(txt)     == 0   ) return npos;
    if( strlen(txtsuch) == 0   ) return npos;
    if( strlen(regel)   == 0   ) return npos;
  
    l2 = strlen(txtsuch);  

    if( pos0 >  pos1           ) return npos;
    if( pos1 >  strlen(txt)-1  ) pos1 = strlen(txt)-1;
    if( l2   >  pos1+1         ) return npos;

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
        return npos;
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
        return npos;
    }
    
    return npos;
  }
  std::size_t StringFindCount(std::string &str,const char *txtsuch)
  {
    return StringFindCount(str.c_str(),0,str.size(),txtsuch);
  }
  std::size_t StringFindCount(const char *txt,const char *txtsuch)
  {
    return StringFindCount(txt,0,strlen(txt),txtsuch);
  }
  std::size_t StringFindCount(std::string &str, std::size_t pos0, const char *txtsuch)
  {
    return StringFindCount(str.c_str(),pos0,str.size()-pos0,txtsuch);
  }
  std::size_t StringFindCount(const char *txt, std::size_t pos0, const char *txtsuch)
  {
    return StringFindCount(txt,pos0,strlen(txt)-pos0,txtsuch);
  }
  std::size_t StringFindCount(std::string &str, std::size_t pos0, std::size_t len0, const char *txtsuch)
  {
    return StringFindCount(str.c_str(),pos0,len0,txtsuch);
  }
  std::size_t StringFindCount(const char *txt, std::size_t pos0, std::size_t len0, const char *txtsuch)
  {
    std::size_t ipos,ntxt = strlen(txt),ncount = 0;
    if( ntxt && len0 ) 
    {
      if( pos0+len0-1 >= ntxt ) 
      {
        if( pos0 >= ntxt )
        {
          return ncount;
        }
        else
        {
          len0 = ntxt-pos0;
        }
      }
      while( len0 && (ipos=StringFind(txt,pos0,len0,txtsuch,"vs")) != Hlp::npos )
      {
        len0 -= (ipos+strlen(txtsuch)-pos0);
        pos0  = ipos+strlen(txtsuch);
        ++ncount;
      }
    }
    return ncount;
  }

  // Sucht Text im Quot nach suchregel
  // Rückgabe 0 , wenn nicht gefunden    => kein Text in s zurückgegeben
  //          1 , wenn vollständig gefunden  => Text in s zurückgegeben
  //          2 , wenn Anfang gefunden aber kein Ende => Text in s zurückgegeben
  unsigned char StrFindQuotText(std::string &s, const char *txt, const char *txtquot0, const char *txtquot1, const char *suchregel) 
  {

      std::size_t i0;
      std::size_t i1;

      if( strlen(txtquot0) == 0 ) 
      {
          i0 = 0;
      }
      else if( (i0=StringFind(txt,txtquot0,suchregel)) == npos )
      {
          return 0;
      }

      i0 = i0+strlen(txtquot0);


      if(  (i1=StringFind(txt,i0,txtquot1,suchregel)) != npos ) {
          
            
          StringCpy(s,txt,i0,i1-i0);

          return 1;

      } else {

          StringCpy(s,txt,i0,strlen(txt)-i0);

          return 2;
            
      }


      return 0;
  }
  std::size_t StringFindQuot(std::string &str, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1,const char *quotregel, char istatus) 
  {
    return StringFindQuot(str.c_str(),0,str.size(),txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
  }
  std::size_t StringFindQuot(const char *txt,const char *txtsuch,const char *suchregel,const char *txtquot0,const char *txtquot1,const char *quotregel,char istatus) 
  {
      return StringFindQuot(txt,0,strlen(txt),txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
  }
  std::size_t StringFindQuot(const char *txt,std::size_t pos0,const char *txtsuch,const char *suchregel,const char *txtquot0,const char *txtquot1,const char *quotregel,char istatus) 
  {  
      return StringFindQuot(txt,pos0,strlen(txt)-pos0,txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
  }
  std::size_t StringFindQuot(std::string &str,std::size_t pos0,std::size_t len0,const char *txtsuch,const char *suchregel,const char *txtquot0,const char *txtquot1,const char *quotregel,char istatus) 
  {
    return StringFindQuot(str.c_str(),pos0,len0,txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
  }
  std::size_t StringFindQuot(std::string &str, std::size_t pos0, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus) 
  {  
      return StringFindQuot(str.c_str(),pos0,str.size()-pos0,txtsuch,suchregel,txtquot0,txtquot1,quotregel,istatus);
  }
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
  std::size_t StringFindQuot(const char *txt, std::size_t pos0, std::size_t len0, const char *txtsuch, const char *suchregel, const char *txtquot0, const char *txtquot1, const char *quotregel, char istatus) 
  {

    std::size_t ind,i,j;
    std::size_t lentxt,lentsuch;
    unsigned char  *ivsuch=NULL;
    char run_flag,found_flag,sflag;
  
    if( strlen(txt)      == 0 ) return npos;
    if( strlen(txtsuch)  == 0 ) return npos;
    if( strlen(txtquot0) == 0 ) return npos;
    if( strlen(txtquot1) == 0 ) return npos;

    lentxt   = HLP_STRING_MIN(len0,strlen(txt+pos0));
    lentsuch = strlen(txtsuch);

    ivsuch = (unsigned char *)malloc(lentxt*sizeof(unsigned char));

    StrFindQuoted(txt,pos0,len0
                 ,txtquot0,txtquot1,quotregel
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
        
    return npos;
  }
  
  void StrFindQuoted(const char    *txt      // Text der zu durchkämmen
                    ,std::size_t   pos0          // Start, wo gesucht wird
                    ,std::size_t   len0          // Länge, des Stücks zum Suchen
                    ,const char    *txtq0    // Start-Quot
                    ,const char    *txtq1    // End-Quot
                    ,const char    *quotregel// "i": innerhalb quot mit einsen belegen
                                             // "a": außerhalb quot mit -"-
                    ,unsigned char *pmarker      // VEktor mit länge len0
                    ,char          istatus       // 0:startet ausserhalb quot
                                                    // 1:startet innerhalb quot
                     ) 
  {

    
    std::size_t ltxt = HLP_STRING_MIN(len0,strlen(txt+pos0));
    std::size_t lq0  = strlen(txtq0);
    std::size_t lq1  = strlen(txtq1);
                     
    std::size_t ind    = 0;
    std::size_t i;
    unsigned char  flag;

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
  // StringGet Functions
  //==============================================================================================================
  std::string StringGetQuotedText(const char *text,const char *txtquot0, const char *txtquot1)
  {
    return StringGetQuotedText(text,0,txtquot0,txtquot1);
  }
  std::string StringGetQuotedText(std::string &str,const char *txtquot0, const char *txtquot1)
  {
    return StringGetQuotedText(str.c_str(),0,txtquot0,txtquot1);
  }
  std::string StringGetQuotedText(const char *text,std::size_t strpos0,const char *txtquot0, const char *txtquot1)
  {
    std::string output;
    std::size_t i0,i1;

    i0 = StringFind(text,strpos0,txtquot0,"vs");
    if( i0 != npos )
    {
      i0 += strlen(txtquot0);
      i1 = StringFind(text,i0,txtquot1,"vs");

      if( i1 != npos ) 
      {
        std::size_t len;
        if( i1 > i0 ) len = i1 - i0;
        else          len = 0;
        output.append(text,i0,len);
      }
    }
    return output;
  }
  std::string StringGetQuotedText(std::string &str,std::size_t strpos0,const char *txtquot0, const char *txtquot1)
  {
    return StringGetQuotedText(str.c_str(),strpos0,txtquot0,txtquot1);
  }
  //CharCpy, CharSet
  //==========================================
  char* CharCpy (char *dest, const char *src, std::size_t len)
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
  char * CharSet (char * dest, char c, std::size_t len)
  {
    unsigned char *ptr = (unsigned char*)dest;
    while (len-- > 0)
      *ptr++ = c;
    return dest;
  }
  //CharIsAlphNum, StringIsAlphNum
  //==========================================
  bool   CharIsAlphNum(char c)
  {
    if( ((c>64) && (c<91)) || ((c>96) && (c<123)) ) return true;
    else                                            return false;
  }
  bool   StringIsAlphNum(const char *src)
  {
    for(std::size_t i=0;i<strlen(src);i++)
    {
      if( !CharIsAlphNum(*(src+i)) )
      {
        return false;
      }
    }
    return true;
  }
  bool   StringIsAlphNum(std::string &s)
  {
    return StringIsAlphNum(s.c_str());
  }

  /*==============================================================================*/
  /* Extrahiert aus name der Regel Pfad, File und Extension und kopiert es in die  */
  /* Stringobjekt s. In regel kann stehen:                               */
  /* "p"    Pfad, "f" wie filename und "e" wie extension                          */
  /* z.B. "pf" bedeutet extrahiere Pfad und Filename ohne extension, wenn         */
  /* vorhanden. Pfad wird mit / oder \ getrennt Extension mit . z.B ./abc/test.txt*/
  /*==============================================================================*/
  bool SlfStrExtractPfe(std::string &s,const char *name,const char *regel) 
  {
    const char *ptxt;
    std::size_t itrennz,iext;
    std::string trennz;   /* Trennzeichen */

    /* Zuallesrt den string in p leeren */
    s = "";

    /* Zuallerst den einfachsten Konstrukt rausfiltern */
    if( StringCompare(name,"") ) 
    {
      return false;
    }

    /* Dann einfache Pfadkonstrukte rausfiltern */
    if( StringCompare(name,".")  ||  StringCompare(name,"..") ) 
    {

      if(  (strlen(regel) == 1)
        && (	(StringFind(regel,"p","vs")!=npos)
        || (StringFind(regel,"P","vs")!=npos) 
        )
        ) 
      {
        s = name;
        return true;
      }
    }

    /* Pfadtrennzeichen extrahieren */
    if( StringFind(name,"\\","vs") != npos )     trennz = "\\";
    else if( StringFind(name,"/","vs") != npos ) trennz = "/";
    else                                         trennz = "\\";

    /* letztes Trennzeichen suchen */
    if( (itrennz=StringFind(name,trennz.c_str(),"rs"))!=npos ) 
    {
      if(  (StringFind(regel,"p","vs")!=npos)
        || (StringFind(regel,"P","vs")!=npos) 
        )
      {

        // anhängen einschliesslich Trennzeichen
        StringCat(s,name,0,itrennz+trennz.size());

      }
      ptxt = &(name[itrennz+1]);
    } 
    else 
    {

      //kein .\ setzen
      //if( (StringFind(regel,"p","vs")!=npos) ||
      //    (StringFind(regel,"P","vs")!=npos) ){

      //	s.cat(".");
      //	s.cat(trennz);
      //}
      ptxt = name;
    }

    /* Jetzt in ptxt extension suchen */
    if( (iext=StringFind(ptxt,".","vs"))!=npos ) 
    {

      if(  (StringFind(regel,"f","vs")!=npos)
        || (StringFind(regel,"F","vs")!=npos) 
        )
      {

        if(  (StringFind(regel,"e","vs")!=npos)
          || (StringFind(regel,"E","vs")!=npos) 
          )
        {

          StringCat(s,ptxt);
        } 
        else if( iext>0 )
        {
          StringCat(s,ptxt,0,iext);
        }
      } 
      else if(  (StringFind(regel,"e","vs")!=npos)
        || (StringFind(regel,"E","vs")!=npos) 
        )
      {

        if( strlen(ptxt) > iext )  StringCat(s,ptxt,iext+1,strlen(ptxt)-iext);
      }
    } 
    else 
    {

      if(  (StringFind(regel,"f","vs")!=npos)
        || (StringFind(regel,"F","vs")!=npos) 
        )
      {

        StringCat(s,ptxt);

      }
    }

    return true;
  }

  // StringElim
  //------------------------------------------------------------------------------
  void  StringElimAnfEnd(std::string &text,std::string &elim)
  {
    StringElimAnf(text,elim.c_str());
    StringElimEnd(text,elim.c_str());
  }
  void  StringElimAnfEnd(std::string &text,const char *pelim)
  {
    StringElimAnf(text,pelim);
    StringElimEnd(text,pelim);
  }
  void  StringElimAnf(std::string &text,std::string &elim)
  {
    StringElimAnf(text,elim.c_str());
  }
  void  StringElimEnd(std::string &text,std::string &elim)
  {
    StringElimEnd(text,elim.c_str());
  }
  void  StringElimEnd(std::string &text,const char *pelim)
  {
    std::size_t i = StringFind(text,pelim,"rn");
    if( text.size() > strlen(pelim) )
    {
      if( i < text.size()-1-strlen(pelim) )
      {
        text.erase(i+1,text.size()-i-1);
      }
    }
  }
  void  StringElimAnf(std::string &text,const char *pelim)
  {
    std::size_t i = StringFind(text,pelim,"vn");
    if( i && (i != Hlp::npos) )
    {
      text.erase(0,i);
    }
  }
  void StringElimAnfEnd(char *ptext,const char *pelim)
  {
    StringElimAnf(ptext,pelim);
    StringElimEnd(ptext,pelim);
  }
  //void  StringElimAnf(std::string &text,std::string &elim)
  //{
  //  std::size_t l0 = elim.size();
  //  bool searchflag = true;
  //  while( searchflag)
  //  {
  //    if( StringCompare(text, 0, l0, elim, 0, l0) ) // Wenn das erste mal nicht gefunden
  //    {                                      // reduzieren
  //      StringReplace(text,"",0, l0);
  //    }
  //    else
  //    {
  //      searchflag = false;
  //    }
  //  }
  //}
  //void  StringElimAnf(std::string &text,const char *pelim)
  //{
  //  std::string elim(pelim);
  //  StringElimAnf(text,elim);
  //}
  void StringElimAnf(char *ptext,const char *pelim)
  {
    std::string text(ptext);
    std::string elim(pelim);
    StringElimAnf(text,elim);
    strcpy_s(ptext,strlen(ptext)+1,text.c_str());
 /*   std::size_t ntext = strlen(ptext);
    std::size_t nelim = strlen(pelim);
    std::size_t i;
    bool flag = true;
    while(flag)
    {
      for(i=0;i<nelim;i++)
      {
        if( i+1 == ntext )
        {
          flag = false;
          break;
        }
        else if( *(ptext+i) != *(pelim+i) )
        {
          flag = false;
          break;
        }
      }
      if( flag )
      {
        for( i=0;i<nelim;i++)
        {
          *(ptext+i) = *(ptext+nelim+i);
        }
        *(ptext+ntext-nelim) = '\0';
        ntext -= nelim;
      }
    }*/
  }
  //void  StringElimEnd(std::string &text,std::string &elim)
  //{
  //  std::size_t l0 = elim.size();

  //  bool searchflag = true;
  //  while( searchflag)
  //  {
  //    if( (text.size() > l0) && StringCompare(text, text.size()-l0, l0, elim, 0, l0) ) // Wenn das erste mal nicht gefunden
  //    {                                                                             // reduzieren
  //      StringReplace(text,"",text.size()-l0, l0);
  //    }
  //    else
  //    {
  //      searchflag = false;
  //    }
  //  }
  //}
  //void  StringElimEnd(std::string &text,const char *pelim)
  //{
  //  std::string elim(pelim);
  //  StringElimEnd(text,elim);
  //}
  void StringElimEnd(char *ptext,const char *pelim)
  {
    std::string text(ptext);
    std::string elim(pelim);
    StringElimEnd(text,elim);
    strcpy_s(ptext,strlen(ptext)+1,text.c_str());
  }
  void StringElimNonAlphaNumAnf(std::string &text)
  {
    std::size_t i,n=0;
    for(i=0;i<text.size();++i)
    {
      if( CharIsAlphNum(text[i]) )
      {
        break;
      }
      else
      { 
        ++n;
      }
    }
    if( n )
    {
      text.erase(0,n);
    }
  }
  void StringElimNonAlphaNumEnd(std::string &text)
  {
    std::size_t i,n=0;

    for(i=text.size();i>0;--i)
    {
      if( CharIsAlphNum(text[i-1]) )
      {
        break;
      }
      else
      { 
        ++n;
      }
    }
    if( n )
    {
      text.erase(text.size()-n,n);
    }
  }
  void   StringElimNonAlphaNumAnfEnd(std::string &text)
  {
    StringElimNonAlphaNumAnf(text);
    StringElimNonAlphaNumEnd(text);
  }
  void StringElimNonAlphaNumAnf(char *ptext)
  {
    std::size_t i,n=0;
    for(i=0;i<strlen(ptext);++i)
    {
      if( CharIsAlphNum(*(ptext+i)) )
      {
        break;
      }
      else
      { 
        ++n;
      }
    }
    if( n )
    {
      for(i=0;i<(strlen(ptext) - n);++i)
      {
        *(ptext+i) = *(ptext+i+n);
      }
      *(ptext+strlen(ptext)-n) = '\0';
    }
  }
  void  StringElimNonAlphaNumEnd(char *ptext)
  {
    std::size_t i,n=0;

    for(i=strlen(ptext);i>0;--i)
    {
      if( CharIsAlphNum(*(ptext+i-1)) )
      {
        break;
      }
      else
      { 
        ++n;
      }
    }
    if( n )
    {
      *(ptext+strlen(ptext)-n) = '\0';
    }

  }
  void StringElimNonAlphaNumAnfEnd(char *ptext)
  {
    StringElimNonAlphaNumAnf(ptext);
    StringElimNonAlphaNumEnd(ptext);
  }

  // StringCpy
  //------------------------------------------------------------------------------
  const char * StringCpy (char * dest, std::size_t len, const char *  src)
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
  const char * StringCpy (char * dest, std::size_t len, const char *  src, std::size_t pos0, std::size_t len0)
  {
    char *d = dest;
    int c;
  

    if( pos0 < strlen(src) )  {

      if( pos0+len0 > strlen(src) ) 
        len0 = strlen(src) - pos0;

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
  const char * StringCpy (char * dest, std::size_t len, std::string  &ssrc, std::size_t pos0, std::size_t len0) 
  {
    return StringCpy(dest,len,ssrc.c_str(),pos0,len0);
  }
  const char * StringCpy (std::string  &sdest, std::string  &ssrc, std::size_t pos0, std::size_t len0) 
  {

    char *p = new char[len0+1];
    StringCpy(p,len0+1,ssrc,pos0,len0);
    sdest = p;
    delete []p;

    return sdest.c_str();
  }
  const char * StringCpy (std::string  &sdest, const char *  src, std::size_t pos0, std::size_t len0) 
  {
    char *p = new char[len0+1];
    StringCpy(p,len0+1,src,pos0,len0);
    sdest = p;
    delete []p;

    return sdest.c_str();
  }

  // StringCat
  //------------------------------------------------------------------------------
  const char * StringCat (char * dest, std::size_t len, const char *  src)
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
    //char *d = dest;
    //int c;
    //while ((c = *src++) != '\0' && len > 0)
    //  {
    //    *d++ = c;
    //    len--;
    //  }
    //*d = '\0';
    //return dest;
  }
  const char * StringCat (char * dest, std::size_t len, const char *  src, std::size_t pos0, std::size_t len0)
  {
    std::string catsrc;

    if( strlen(src) >= pos0 )
    {
      return dest;
    }
    else if( strlen(src) > (pos0 + len0) )
    {
      len0 = strlen(src) - pos0;
    }

    for(std::size_t i=0;i<len0;++i)
    {
      catsrc.push_back(*(src+pos0+i));
    }
    
    return StringCat(dest,len,catsrc.c_str());
  }
  const char * StringCat (char * dest, std::size_t len, std::string  &ssrc, std::size_t pos0, std::size_t len0) 
  {
    return StringCat(dest,len,ssrc.c_str(),pos0,len0);
  }
  const char * StringCat (std::string  &sdest, std::string  &ssrc, std::size_t pos0, std::size_t len0) 
  {

    char *p = new char[len0+1];
    StringCat(p,len0+1,ssrc,pos0,len0);
    sdest = p;
    delete []p;

    return sdest.c_str();
  }
  const char * StringCat (std::string  &sdest, const char *  src, std::size_t pos0, std::size_t len0) 
  {
    char *p = new char[len0+1];
    StringCat(p,len0+1,src,pos0,len0);
    sdest = p;
    delete []p;

    return sdest.c_str();
  }
  const char * StringCat (std::string  &sdest, const char *  src)
  {
    return StringCat(sdest,src,0,strlen(src));
  }
  // StringCompare
  //------------------------------------------------------------------------------
  bool StringCompare(std::string &s0,std::string &s1) 
  {
    return StringCompare(s0.c_str(), 0, s0.size(), s1.c_str(), 0, s1.size());
  }
  bool StringCompare(const char *str0,const char *str1) 
  {
    return StringCompare(str0, 0, strlen(str0), str1, 0, strlen(str1));
  }
  bool StringCompare(const std::string &s0, std::size_t pos0, std::size_t len0,const std::string &s1, std::size_t pos1, std::size_t len1) 
  {
    return StringCompare(s0.c_str(), pos0, len0, s1.c_str(), pos1, len1);
  }
  bool StringCompare(const char *str0, std::size_t pos0, std::size_t len0
                  ,const char *str1, std::size_t pos1, std::size_t len1) 
  {

    int c0, c1;
    std::size_t n=0;
  
    len0 = HLP_STRING_MIN(strlen(str0),len0); 
    len1 = HLP_STRING_MIN(strlen(str1),len1);

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
  // StringInsert
  //------------------------------------------------------------------------------
  void StringInsert(std::string &str,std::size_t posstr0,std::string &sinsert)
  {
    str.insert(posstr0,sinsert);    
  }

  // StringReplace
  //------------------------------------------------------------------------------
  void StringReplace(std::string &str0, std::string &str, std::size_t pos0, std::size_t len0)
  {
    StringReplace(str0, str.c_str(), pos0, len0);
  }
  void StringReplace(std::string &str0, const char *pstr, std::size_t pos0, std::size_t len0) 
  {
    std::size_t  len1 = strlen(pstr);  


    // pos0 geht über die Länge des Strings hinaus
    if( pos0 >  str0.size() ) return;
    if( len0 == 0 )           return;
    if( pos0+len0 > str0.size() ) len0 = str0.size()-pos0;
    if( pos0 )
    {
      std::string str00 = str0.substr(0,pos0);
      std::string str01 = str0.substr(pos0+len0,str0.size());

      str0 = str00 + pstr + str01;
    }
    else
    {
      std::string str01 = str0.substr(pos0+len0,str0.size());

      str0 = pstr + str01;
    }
  }
    // StringChange
  //-------------------------------------------------------------------------------
  void StringChange(std::string &str, std::string &such, std::string &ersetz)
  {
    StringChange(str, such.c_str(), ersetz.c_str() );
  }
  void StringChange(std::string &str, const char *psuch, const char *persetz)
  {
    std::size_t i=0;
    std::size_t npos = -1;
    if(  (strcmp(psuch,persetz) != 0) && (*psuch != '\0') ) 
    {
      std::size_t ns = strlen(psuch);
      std::size_t ne = strlen(persetz);

      while( (i = str.find(psuch,i)) != npos ) 
      {
        str.erase(i,ns);
        str.insert(i,persetz);
        i = i+1-ns+ne;
      }
    }
  }

  // StrinSplit
  // text wird mit einem beliebigen Trenn-string zerlegt in ein Vektor von Strings
  //------------------------------------------------------------------------------
  std::vector<std::string> StringSplit(std::string &text,std::string &delim)
  {
    return StringSplit((char *)text.c_str(),(char *)delim.c_str());
  }
  std::vector<std::string> StringSplit(std::string &text,const char *pdelim)
  {
    return StringSplit((char *)text.c_str(),pdelim);
  }
  std::vector<std::string> StringSplit(const char *ptext,const char *pdelim)
  {
    size_t                   pos = 0;
    std::string              text = ptext;
    std::string              delim = pdelim;
    std::string              token;
    std::vector<std::string> vec;

    while ((pos = text.find(delim)) != std::string::npos) 
    {
      token = text.substr(0, pos);
      vec.push_back(token);
      text.erase(0, pos + delim.length());
    }
    if(text.length() > 0 ) vec.push_back(text);

    return vec;
  }
  // StringConcate
  //------------------------------------------------------------------------------
  std::string StringConcate(std::vector<std::string> &strvec)
  {
    std::string str;
    std::size_t i,n = strvec.size();
    for(i=0;i<n;++i)
    {
      str.append(strvec[i]);
    }
    return str; 
  }
  std::string StringConcate(std::vector<std::string> &strvec,std::string &delim)
  {
    return StringConcate(strvec,(char *)delim.c_str());
  }
  std::string StringConcate(std::vector<std::string> &strvec,const char *pdelim)
  {
    std::string str;
    std::size_t i,nm,n = strvec.size();
    if( n ) nm = n -1;
    else    nm = 0;
    for(i=0;i<strvec.size();++i)
    {
      str.append(strvec[i]);
      if( i != nm ) str.append(pdelim);
    }
    return str; 
  }

  // StringVec
  //------------------------------------------------------------------------------
  bool IsStringVec0InStringVec1(std::vector<std::string> &strvec0, std::vector<std::string> &strvec1, std::size_t pos1)
  {

    if( strvec0.size() == NumOfIdentStringItemsInStrinVec(strvec0,strvec1,pos1) )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
  std::size_t NumOfIdentStringItemsInStrinVec(std::vector<std::string> &strvec0,std::vector<std::string> &strvec1, std::size_t pos1)
  {
    std::size_t n0      = strvec0.size();
    std::size_t n1      = strvec1.size();
    std::size_t nident  = 0;

    // smalest vector
    if( n0 > n1 ) n0 = n1;

    // proof items
    for(std::size_t i=0;i<n0;i++)
    {
      if( i+pos1 >= n1 ) break;
      if( strvec0[i] == strvec1[i+pos1] ) ++nident;
      else                           break;
    }
    return nident;
  }
  // Is string to find in strvec
  bool IsStringInStringVec(std::string &str,std::vector<std::string> &strvec)
  {
    std::size_t n1      = strvec.size();
    // proof items
    for(std::size_t i=0;i<n1;i++)
    {
      if( str == strvec[i] ) return true;
    }
    return false;
  }
  // how many times is string in vector
  std::size_t NumOfStringInStrinVec(std::string &str,std::vector<std::string> &strvec)
  {
    std::size_t n1      = strvec.size();
    std::size_t nident  = 0;
    // proof items
    for(std::size_t i=0;i<n1;i++)
    {
      if( str == strvec[i] ) ++nident;
    }
    return nident;
  }
  // index for icount = 0 ... NumOfStringInStrinVec()
  std::size_t IndexOfStringInStrinVec(std::string &str,std::vector<std::string> &strvec,std::size_t icount)
  {
    std::size_t n1      = strvec.size();
    std::size_t nident  = 0;
    // proof items
    for(std::size_t i=0;i<n1;i++)
    {
      if( str == strvec[i] ) 
      {
        if( nident == icount ) return i;
        ++nident;
      }
    }
    return std::string::npos;
  }
}