#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include "SlfFkt.h"

void SlfFktCalcFreeMemoryPriv(uint32_t i,double *pkb);

//================================================================
// EInheiten konvertieren
//================================================================
struct SSlfUnitConv {
    char * usearch;
    char * ubase;
    double faktor;
    double offset;
};

SSlfUnitConv SlfUnitConvList[] = {
#include "SlfFktUnitTable.h"
};
uint16_t SlfNUnitConvList = sizeof(SlfUnitConvList)/sizeof(SSlfUnitConv);
#if SLF_FKT_USE_STD_STRING == 0
status_t SlfFktUnitConv(const slf::CStr &u_from,const slf::CStr &u_to,
                        double *pfak,double *poff,slf::CStr &ErrText) {

    return SlfFktUnitConv(u_from.c_str(),u_to.c_str(),pfak,poff,ErrText);
}

status_t SlfFktUnitConv(const char *u_from,const char *u_to,
                        double *pfak,double *poffset,slf::CStr &ErrText) {


    slf::CStr UFrom = u_from;
    slf::CStr UTo   = u_to;

    bool from_found = false;
    bool to_found   = false;
    
    uint16_t ifrom,ito;
    
    char *ubase_from;
    char *ubase_to;

    uint16_t i;


    // Bereinigen
    //-----------
    UFrom.elimAnfEnd(" ");
    UFrom.elimAnf("[");
    UFrom.elimEnd("]");
    UFrom.elimAnf("<");
    UFrom.elimEnd(">");
    UFrom.elimAnfEnd(" ");

    UTo.elimAnfEnd(" ");
    UTo.elimAnf("[");
    UTo.elimEnd("]");
    UTo.elimAnf("<");
    UTo.elimEnd(">");
    UTo.elimAnfEnd(" ");

    // Sonderf‰lle
    //============
    if( UFrom == "0" ) { // Nullwert
    
        *pfak    = 0.0;
        *poffset = 0.0;

        return OKAY;
    }
    if( UFrom == UTo ) {

        *pfak    = 1.0;
        *poffset = 0.0;
        return OKAY;
    
    }

    // 1. Einheit (from) suchen
    for(i=0;i<SlfNUnitConvList;i++) {

        if( UFrom == SlfUnitConvList[i].usearch ) {
            from_found = true;
            ubase_from = SlfUnitConvList[i].ubase;
            ifrom      = i;

            break;
        }
    }

    if( !from_found ) {
        if( &ErrText ) {
               ErrText.catFormat("\nError SlfFktUnitConv: Die 1. Einheit (from) [%s] konnte "
                              "in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. "
                              "(2. Einheit (to) [%s])\n"
                              ,UFrom.c_str()
                              ,UTo.c_str()
                             );
        }
        return NOT_OKAY;
    }
    // 2. Einheit (to) suchen
    for(i=0;i<SlfNUnitConvList;i++) {

        if( UTo == SlfUnitConvList[i].usearch ) {
            to_found = true;
            ubase_to = SlfUnitConvList[i].ubase;
            ito    = i;

            break;
        }
    }
    if( !to_found ) {
        if( &ErrText ) {
            ErrText.catFormat("\nError SlfFktUnitConv: Die 2. Einheit (to) [%s] konnte"
                              "in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. "
                              "1. Einheit (from) [%s]\n"
                              ,UTo.c_str()
                              ,UFrom.c_str()
                             );
        }
        return NOT_OKAY;
    }

    // Basis vergleichen
    //------------------
    if( strcmp(ubase_from,ubase_to) != 0 ) {

        if( &ErrText ) {
             ErrText.catFormat("\nError SlfFktUnitConv: Die 1. Einheit [%s] passt "
                               "laut Umrechentabelle <SlfFktUnitTable.h> nicht mit der "
                               "2. Einheit [%s] zusammen !!!\n"
                               ,UFrom.c_str()
                               ,UTo.c_str()
                              );
        }
        return NOT_OKAY;
    }
    // Umrechnung
    // ifrom: unit_base = unit_from * fak_from + off_from;
    // ito:   unit_base = unit_to   * fak_to   + off_to;
    // =>
    // unit_to = unit_from*fak_from/fak_to + (off_from-off_to)/fak_to
    // fak     = fak_from/fak_to
    // off     = (off_from-off_to)/fak_to
    //
    *pfak    = SlfUnitConvList[ifrom].faktor
             / NOT_ZERO(SlfUnitConvList[ito].faktor);

    *poffset = (SlfUnitConvList[ifrom].offset-SlfUnitConvList[ito].offset)
             / NOT_ZERO(SlfUnitConvList[ito].faktor);

    return OKAY;
}
#else
status_t SlfFktUnitConv(const std::string &u_from,const std::string &u_to,
                        double *pfak,double *poff,std::string &ErrText) {

    return SlfFktUnitConv(u_from.c_str(),u_to.c_str(),pfak,poff,ErrText);
}
void SlfFktUnitConvBereinigen(std::string &str)
{
    size_t i0;

    while( (i0=str.find_first_of(" ")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of(" ")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_first_of("[")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of("]")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_first_of("<")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of(">")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_first_of(" ")) == 0 )
    {
      str.erase(i0,1);
    }
    while( (i0=str.find_last_of(" ")) == str.length()-1 )
    {
      str.erase(i0,1);
    }
}
status_t SlfFktUnitConv(const char *u_from,const char *u_to,
                        double *pfak,double *poffset,std::string &ErrText) {


    std::string UFrom = u_from;
    std::string UTo   = u_to;

    bool from_found = false;
    bool to_found   = false;
    
    uint16_t ifrom,ito;
    
    char *ubase_from;
    char *ubase_to;

    uint16_t i;


    // Bereinigen
    //-----------
    SlfFktUnitConvBereinigen(UFrom);
    SlfFktUnitConvBereinigen(UTo);

    // Sonderf‰lle
    //============
    if( UFrom == "0" ) { // Nullwert
    
        *pfak    = 0.0;
        *poffset = 0.0;

        return OKAY;
    }
    if( UFrom == UTo ) {

        *pfak    = 1.0;
        *poffset = 0.0;
        return OKAY;
    
    }

    // 1. Einheit (from) suchen
    for(i=0;i<SlfNUnitConvList;i++) {

        if( UFrom == SlfUnitConvList[i].usearch ) {
            from_found = true;
            ubase_from = SlfUnitConvList[i].ubase;
            ifrom      = i;

            break;
        }
    }

    if( !from_found ) {

        if( &ErrText ) {
#if SLF_FKT_USE_STD_STRING == 0
            ErrText.catFormat("\nError SlfFktUnitConv: Die 1. Einheit (from) [%s] konnte "
                              "in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. "
                              "(2. Einheit (to) [%s])\n"
                              ,UFrom.c_str()
                              ,UTo.c_str()
                              );
#else
      std::stringstream out;
      out << "\nError SlfFktUnitConv: Die 1. Einheit (from) [" << UFrom.c_str() << "] konnte in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. ";
      out << "(2. Einheit (to) [" << UTo.c_str() << "])\n";
      ErrText = out.str();
#endif
        }
        return NOT_OKAY;
    }
    // 2. Einheit (to) suchen
    for(i=0;i<SlfNUnitConvList;i++) {

        if( UTo == SlfUnitConvList[i].usearch ) {
            to_found = true;
            ubase_to = SlfUnitConvList[i].ubase;
            ito    = i;

            break;
        }
    }
    if( !to_found ) {

        if( &ErrText != 0 ) {

#if SLF_FKT_USE_STD_STRING == 0
            ErrText.catFormat("\nError SlfFktUnitConv: Die 2. Einheit (to) [%s] konnte"
                              "in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. "
                              "1. Einheit (from) [%s]\n"
                              ,UTo.c_str()
                              ,UFrom.c_str()
                              );
#else
      std::stringstream out;
      out << "\nError SlfFktUnitConv: Die 2. Einheit (to) [" << UTo.c_str() << "] konnte in der Umrechentabelle <SlfFktUnitTable.h> nicht gefunden werden. ";
      out << "(1. Einheit (to) [" << UFrom.c_str() << "])\n";
      ErrText = out.str();
#endif
        }
        return NOT_OKAY;
    }

    // Basis vergleichen
    //------------------
    if( strcmp(ubase_from,ubase_to) != 0 ) {

        if( &ErrText != 0 ) {
#if SLF_FKT_USE_STD_STRING == 0
            ErrText.catFormat("\nError SlfFktUnitConv: Die 1. Einheit [%s] passt "
                              "laut Umrechentabelle <SlfFktUnitTable.h> nicht mit der "
                              "2. Einheit [%s] zusammen !!!\n"
                              ,UFrom.c_str()
                              ,UTo.c_str()
                              );
#else
          std::stringstream out;
          out << "\nError SlfFktUnitConv: Die 1. Einheit (to) [" << UFrom.c_str() << "] passt laut Umrechentabelle <SlfFktUnitTable.h> nicht mit der ";
          out << "2. Einheit (to) [" << UTo.c_str() << "] zusammen.\n";
          ErrText = out.str();
#endif
        }
        return NOT_OKAY;
    }
    // Umrechnung
    // ifrom: unit_base = unit_from * fak_from + off_from;
    // ito:   unit_base = unit_to   * fak_to   + off_to;
    // =>
    // unit_to = unit_from*fak_from/fak_to + (off_from-off_to)/fak_to
    // fak     = fak_from/fak_to
    // off     = (off_from-off_to)/fak_to
    //
    *pfak    = SlfUnitConvList[ifrom].faktor
             / NOT_ZERO(SlfUnitConvList[ito].faktor);

    *poffset = (SlfUnitConvList[ifrom].offset-SlfUnitConvList[ito].offset)
             / NOT_ZERO(SlfUnitConvList[ito].faktor);

    return OKAY;
}
#endif
//================================================================
// EInheiten in SI-Einheit suchen
//================================================================
#if SLF_FKT_USE_STD_STRING == 0
char *SlfFktUnitSI(const char *u_find,slf::CStr &ErrText) {

    slf::CStr UFind = u_find;
#else
char *SlfFktUnitSI(const char *u_find,std::string &ErrText) {
    std::string UFind = u_find;
#endif

    uint16_t i;


    // Bereinigen
    //-----------
#if SLF_FKT_USE_STD_STRING == 0
    UFind.elimAnfEnd(" ");
    UFind.elimAnf("[");
    UFind.elimEnd("]");
    UFind.elimAnf("<");
    UFind.elimEnd(">");
    UFind.elimAnfEnd(" ");
#else
    SlfFktUnitConvBereinigen(UFind);
#endif
    // Einheit suchen
    for(i=0;i<SlfNUnitConvList;i++) {

        if( UFind == SlfUnitConvList[i].usearch ) {

            return SlfUnitConvList[i].ubase;
        }
    }

    if( &ErrText != 0 )
    {
#if SLF_FKT_USE_STD_STRING == 0
      ErrText.catFormat("Error SlfFktUnitSI: die Einheit <%s> konnte nicht als SI-Einheit in der Tabelle <SlfFktUnitTable.h> gefunden werden!! \n",UFind.c_str());
#else
      std::stringstream out;
      out << "\nError SlfFktUnitSI: Die Einheit [" << UFind.c_str() << "] konnte nicht als SI-Einheit in der Tabelle <SlfFktUnitTable.h> gefunden werden!! \n";
      ErrText = out.str();
#endif
    }
        
    return 0;
}

void SlfFktConvertSingle(void *pval_from, EVarType type_from
                        ,void *pval_to,   EVarType type_to
                        ,double factor,   double offset
                        ) {

    switch(type_to) {

    case DEF_VOID:
        // keinen Wert zuweisen
        break;
    case DEF_CHAR_STRING:

        pval_to = 
                (char *)pval_from;

        break;
    default:
        {
        double yGetD=0.0;

        switch(type_from) {

        case DEF_DOUBLE:
            yGetD = *(double *)pval_from;                        
            break;
        case DEF_FLOAT:                    
            yGetD = (double)*(float *)pval_from;
            break;
        case DEF_SIGNED_LONG:                    
            yGetD = (double)*(signed long *)pval_from;
            break;
        case DEF_UNSIGNED_LONG:                    
            yGetD = (double)*(unsigned long *)pval_from;
            break;
        case DEF_SIGNED_SHORT:
            yGetD = (double)*(signed short *)pval_from;
            break;
        case DEF_UNSIGNED_SHORT:
            yGetD = (double)*(unsigned short *)pval_from;
            break;
        case DEF_SIGNED_CHAR:
            yGetD = (double)*(signed char *)pval_from;
            break;
        case DEF_UNSIGNED_CHAR:
            yGetD = (double)*(unsigned char *)pval_from;
            break;
        }
        switch(type_to) {
        case DEF_DOUBLE:
            *(double *)pval_to 
                = yGetD * factor + offset;
            break;
        case DEF_FLOAT:
            *(float *)pval_to 
                = (float)(yGetD * factor + offset);
            break;
        case DEF_SIGNED_LONG:
            *(signed long *)pval_to 
                = (signed long)(yGetD * factor + offset);
            break;
        case DEF_UNSIGNED_LONG:
            *(unsigned long *)pval_to 
                = (unsigned long)(yGetD * factor + offset);
            break;
        case DEF_SIGNED_SHORT:
            *(signed short *)pval_to 
                = (signed short)(yGetD * factor + offset);
            break;
        case DEF_UNSIGNED_SHORT:
            *(unsigned short *)pval_to 
                = (unsigned short)(yGetD * factor + offset);
            break;
        case DEF_SIGNED_CHAR:
            *(signed char *)pval_to 
                = (signed char)(yGetD * factor + offset);
            break;
        case DEF_UNSIGNED_CHAR:
            *(unsigned char *)pval_to 
                = (unsigned char)(yGetD * factor + offset);
            break;
        }

        break;
        }

    }

}
void SlfFktConvertArray(void *pval_from, EVarType type_from, uint32_t n_from
                        ,void *pval_to,   EVarType type_to, uint32_t n_to
                        ,double factor,   double offset
                        ) {

    uint32_t n = MIN( n_from,n_to);
    uint32_t i;

    switch(type_to) {

    case DEF_VOID:
      // keinen Wert zuweisen
      break;
    case DEF_VEC:
      {
        Vector_t parr_t = *(Vector_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_FLOAT:
          {
            float *parr_f = (float *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        }
      }
      break;
    case DEF_ARR_FLOAT:
      {
        float *parr_t = (float *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_FLOAT:
          {
            float *parr_f = (float *)pval_from;
            float ff = (float)factor;
            float oo = (float)offset;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * ff + oo;
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (float)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;
    case DEF_ARR_DOUBLE:
      {
        double *parr_t = (double *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_FLOAT:
          {
            float *parr_f = (float *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = parr_f[i] * factor + offset;
          }
          break;
        }
      }
      break;
    case DEF_ARR_SIGNED_LONG:
      {
        sint32_t *parr_t = (sint32_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint32_t)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;
    case DEF_ARR_UNSIGNED_LONG:
      {
        uint32_t *parr_t = (uint32_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint32_t)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;
    case DEF_ARR_SIGNED_SHORT:
      {
        sint16_t *parr_t = (sint16_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint16_t)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;
    case DEF_ARR_UNSIGNED_SHORT:
      {
        uint16_t *parr_t = (uint16_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint16_t)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;
    case DEF_ARR_SIGNED_CHAR:
      {
        sint8_t *parr_t = (sint8_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (sint8_t)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;
    case DEF_ARR_UNSIGNED_CHAR:
      {
        uint8_t *parr_t = (uint8_t *)pval_to;
        switch(type_from) {
        case DEF_VEC:
          {
            Vector_t parr_f = *(Vector_t *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_DOUBLE:
          {
            double *parr_f = (double *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_LONG:
          {
            signed long *parr_f = (signed long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_LONG:
          {
            unsigned long *parr_f = (unsigned long *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_SHORT:
          {
            signed short *parr_f = (signed short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_SHORT:
          {
            unsigned short *parr_f = (unsigned short *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_SIGNED_CHAR:
          {
            signed char *parr_f = (signed char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        case DEF_ARR_UNSIGNED_CHAR:
          {
            unsigned char *parr_f = (unsigned char *)pval_from;
            for(i=0;i<n;i++)
              parr_t[i] = (uint8_t)(parr_f[i] * factor + offset);
          }
          break;
        }
      }
      break;

    }

}
void SlfFktConvertSingleToVector(void *pval_from, EVarType type_from
                                ,void *pval_to,   EVarType type_to
                                ,uint32_t ivec
                                ,double factor,   double offset
                                ) {

    switch(type_to) {

    case DEF_VOID:
        // keinen Wert zuweisen
        break;
    default:
        {
        double yGetD=0.0;

        switch(type_from) {

        case DEF_DOUBLE:
            yGetD = *(double *)pval_from;                        
            break;
        case DEF_FLOAT:                    
            yGetD = (double)*(float *)pval_from;
            break;
        case DEF_SIGNED_LONG:                    
            yGetD = (double)*(signed long *)pval_from;
            break;
        case DEF_UNSIGNED_LONG:                    
            yGetD = (double)*(unsigned long *)pval_from;
            break;
        case DEF_SIGNED_SHORT:
            yGetD = (double)*(signed short *)pval_from;
            break;
        case DEF_UNSIGNED_SHORT:
            yGetD = (double)*(unsigned short *)pval_from;
            break;
        case DEF_SIGNED_CHAR:
            yGetD = (double)*(signed char *)pval_from;
            break;
        case DEF_UNSIGNED_CHAR:
            yGetD = (double)*(unsigned char *)pval_from;
            break;
        }
        switch(type_to) {
        case DEF_ARR_DOUBLE:
            *((double *)pval_to+ivec) 
                = yGetD * factor + offset;
            break;
        case DEF_ARR_FLOAT:
            *((float *)pval_to+ivec) 
                = (float)(yGetD * factor + offset);
            break;
        case DEF_ARR_SIGNED_LONG:
            *((signed long *)pval_to+ivec) 
                = (signed long)(yGetD * factor + offset);
            break;
        case DEF_ARR_UNSIGNED_LONG:
            *((unsigned long *)pval_to+ivec)
                = (unsigned long)(yGetD * factor + offset);
            break;
        case DEF_ARR_SIGNED_SHORT:
            *((signed short *)pval_to+ivec) 
                = (signed short)(yGetD * factor + offset);
            break;
        case DEF_ARR_UNSIGNED_SHORT:
            *((unsigned short *)pval_to+ivec) 
                = (unsigned short)(yGetD * factor + offset);
            break;
        case DEF_ARR_SIGNED_CHAR:
            *((signed char *)pval_to+ivec)
                = (signed char)(yGetD * factor + offset);
            break;
        case DEF_ARR_UNSIGNED_CHAR:
            *((unsigned char *)pval_to+ivec) 
                = (unsigned char)(yGetD * factor + offset);
            break;
        }

        break;
        }

    }

}
#if SLF_FKT_USE_STD_STRING == 0
void SlfFktConvertString(void *pval_from, EVarType type_from, uint32_t n_from
                        ,void *pval_to,   EVarType type_to, uint32_t n_to
                        ) {

  switch(type_to) {
  case DEF_STRING:
    {
      slf::CStr *pstr_t = (slf::CStr *)pval_to;
      pstr_t->clear();
      switch(type_from) {
      case DEF_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          pstr_t->cat(pstr_f->c_str());
        }
        break;
      case DEF_STRINGV:
        {
          slf::CStrV *pstrv_f = (slf::CStrV *)pval_from;
          pstr_t->cat(pstrv_f->get_str(0));
        }
        break;
      case DEF_STRINGM:
        {
          slf::CStrM *pstrm_f = (slf::CStrM *)pval_from;
          pstr_t->cat(pstrm_f->get_str(0,0));
        }
        break;
      case DEF_ARR_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          pstr_t->cat(pstr_f[0].c_str());
        }
        break;
      }
    }
  case DEF_STRINGV:
    {
      uint32_t i;
      slf::CStrV *pstrv_t = (slf::CStrV *)pval_to;
      pstrv_t->clear();
      switch(type_from) {
      case DEF_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          pstrv_t->append(pstr_f->c_str());
        }
        break;
      case DEF_STRINGV:
        {
          slf::CStrV *pstrv_f = (slf::CStrV *)pval_from;
          for(i=0;i<pstrv_f->getNrows();i++) 
              pstrv_t->append(pstrv_f->get_str(i));
        }
        break;
      case DEF_STRINGM:
        {
          slf::CStrM *pstrm_f = (slf::CStrM *)pval_from;
          for(i=0;i<pstrm_f->getNrows();i++) 
            pstrv_t->cpy(pstrm_f->get_str(i,0),i);
        }
        break;
      case DEF_ARR_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          for(i=0;i<n_from;i++) 
            pstrv_t->append(pstr_f[i].c_str());
        }
        break;
      }
    }
  case DEF_STRINGM:
    {
      uint32_t i;
      slf::CStrM *pstrm_t = (slf::CStrM *)pval_to;
      pstrm_t->clear();
      switch(type_from) {
      case DEF_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          pstrm_t->cpy(pstr_f->c_str(),0,0);
        }
        break;
      case DEF_STRINGV:
        {
          slf::CStrV *pstrv_f = (slf::CStrV *)pval_from;
          for(i=0;i<pstrv_f->getNrows();i++) 
              pstrm_t->cpy(pstrv_f->get_str(i),i,0);
        }
        break;
      case DEF_STRINGM:
        {
          uint32_t j;
          slf::CStrM *pstrm_f = (slf::CStrM *)pval_from;
          for(i=0;i<pstrm_f->getNrows();i++) 
            for(j=0;j<pstrm_f->getNcols();j++) 
              pstrm_t->cpy(pstrm_f->get_str(i,j),i,j);
        }
        break;
      case DEF_ARR_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          for(i=0;i<n_from;i++) 
            pstrm_t->cpy(pstr_f[i].c_str(),i,0);
        }
        break;
      }
    }
  case DEF_ARR_STRING:
    {
      uint32_t i;
      slf::CStr *pstr_t = (slf::CStr *)pval_to;
      switch(type_from) {
      case DEF_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          pstr_t[0].clear();
          pstr_t[0].append(pstr_f->c_str());
        }
        break;
      case DEF_STRINGV:
        {
          slf::CStrV *pstrv_f = (slf::CStrV *)pval_from;
          uint32_t n = MIN(n_to,pstrv_f->getNrows());
          for(i=0;i<n;i++) {
            pstr_t[i].clear();
            pstr_t[i].append(pstrv_f->get_str(i));
          }
        }
        break;
      case DEF_STRINGM:
        {
          slf::CStrM *pstrm_f = (slf::CStrM *)pval_from;
          for(i=0;i<pstrm_f->getNrows();i++) 
              pstr_t[i].append(pstrm_f->get_str(i,0));
        }
        break;
      case DEF_ARR_STRING:
        {
          slf::CStr *pstr_f = (slf::CStr *)pval_from;
          for(i=0;i<MIN(n_from,n_to);i++) {
            pstr_t[i].clear(); 
            pstr_t[i].append(pstr_f[i].c_str());
          }
        }
        break;
      }
    }
  }
}
#endif
bool SlfFktIsStringToNum(const char *pstring) 
{

    uint8_t found_flag = 0;
    // 
    for(uint32_t i=0;i < strlen(pstring);i++) {

      if( !( ((pstring[i]=='e')&&(i!=0))
           ||((pstring[i]=='E')&&(i!=0))
           ||isdigit(pstring[i])
           || (pstring[i]=='-')
           || (pstring[i]=='+')
           || (pstring[i]=='.')
           )
        )
        return false;
    }

    return true;

}
status_t SlfFktConvertStringToDouble(const char *pstring,double *pval) {

    char *ps = new char[strlen(pstring)+1];

    uint8_t found_flag = 0;
    uint32_t index = 0;

    // Die nichtbrauchbaren Zeichen vornweg wegschneiden
    for(uint32_t i=0;i < strlen(pstring);i++) {

        if(   !found_flag
          &&  (  isdigit(pstring[i])
              || pstring[i]=='-'
              || pstring[i]=='+'
              || pstring[i]=='.'
              ) 
          ) 
          found_flag = 1;

        if( found_flag ) {

            ps[index] = pstring[i];
            index++;
        }
    }
    ps[index] = '\0';

    *pval =  atof(ps);

    delete []ps;

#if 0
    if( sscanf(pstring,"%g",pval) == 0 )
        return NOT_OKAY;
#endif
    return OKAY;

}
#if SLF_FKT_USE_STD_STRING == 0
status_t SlfFktConvertStringToDoubleVec(const char *pstring,double **ppval,uint32_t *pnvec) {

    slf::CStr str = pstring;
    slf::CStr *pstr;
    slf::CStrV vstr;
    uint32_t   i;


    // String bearbeiten
    str.elimAnfEndC();
    str.elimAnf("[");
    str.elimAnf("<");
    str.elimEnd("]");
    str.elimEnd(">");
    str.elimAnfEndC();

    *pnvec = SlfStrVSplit(vstr,str.c_str(),",");

    // *ppval muﬂ null sein
    if( *ppval != 0 ) return NOT_OKAY;
    // Vektor anlegen
    *ppval = new double[*pnvec];

    for(i=0;i<*pnvec;i++) {

        pstr = vstr.get(i);
        pstr->elimAnfEndC();

        if( SlfFktConvertStringToDouble(pstr->c_str(),*ppval+i) != OKAY )
            return NOT_OKAY;
    }
    
    return OKAY;

}
status_t SlfFktConvertStringToDoubleMat(const char *pstring,Matrix_t *pmat) {

    slf::CStr str = pstring;
    slf::CStr *pstr;
    slf::CStrV vstrr;
    slf::CStrV vstrc;
    uint32_t   i,j;
    uint32_t   nr,nc,ncc;
    Matrix_t   mat;

    // mat muﬂ null sein
    if( *pmat != 0 ) return NOT_OKAY;

    // String bearbeiten
    str.elimAnfEndC();
    str.elimAnf("[");
    str.elimAnf("<");
    str.elimEnd("]");
    str.elimEnd(">");
    str.elimAnfEndC();

    nr = SlfStrVSplit(vstrr,str.c_str(),";");
    nc = 0;
    for(i=0;i<nr;i++)
        nc = MAX(nc,SlfStrVSplit(vstrc,vstrr.get_str(i),","));

    *pmat = NewMatrix(nr,nc);
    mat = *pmat;
    for(i=0;i<nr;i++) {
        ncc=SlfStrVSplit(vstrc,vstrr.get_str(i),",");
        for(j=0;j<nc;j++) {

            if( j < ncc ) {

                pstr = vstrc.get(i);
				if( pstr == 0 )
					return NOT_OKAY;
                pstr->elimAnfEndC();

                if( SlfFktConvertStringToDouble(pstr->c_str(),&mat[i][j]) != OKAY )
                    return NOT_OKAY;
                 
            } else {
                mat[i][j] = 0.0;
            }
        }
    }
    
    return OKAY;

}
status_t SlfFktConvertStringToStringVec(const char *pstring,slf::CStrV *pvstr,uint32_t *pnvstr) {

  slf::CStr str = pstring;
  slf::CStrV vstr;
  slf::CStr *pstr;
  uint32_t   i;


  // String bearbeiten
  str.elimAnfEndC();
  str.elimAnf("[");
  str.elimAnf("<");
  str.elimEnd("]");
  str.elimEnd(">");
  str.elimAnfEndC();

  *pnvstr = SlfStrVSplit(vstr,str.c_str(),",");

  pvstr->clear();
  for(i=0;i<*pnvstr;i++) {

      pstr = vstr.get(i);
      pstr->elimAnfEndC();
      pvstr->append(pstr->c_str());

  }

  return OKAY;
}
#endif
#ifdef WIN32
double SlfFktGetMilliSeconds()
{
  double ret_t = 0.0;
  static LONGLONG timefreq_ll = 0; 
  if(QueryPerformanceFrequency((LARGE_INTEGER*)&timefreq_ll))
  {
    LONGLONG timeact_ll = 0;
    QueryPerformanceCounter((LARGE_INTEGER*)&timeact_ll);
    double timeact_d = (double)timeact_ll;
    timeact_d = (timeact_d*1000.0)/timefreq_ll;
    ret_t = timeact_d;
  }
  return(ret_t);
}
double SlfFktGetMilliSeconds(LONGLONG *ptimefreq_ll)
{
  double ret_t = 0.0;
  if(QueryPerformanceFrequency((LARGE_INTEGER*)ptimefreq_ll))
  {
    LONGLONG timeact_ll = 0;
    QueryPerformanceCounter((LARGE_INTEGER*)&timeact_ll);
    double timeact_d = (double)timeact_ll;
    timeact_d = (timeact_d*1000.0)/ *ptimefreq_ll;
    ret_t = timeact_d;
  }
  return(ret_t);
}
#endif
status_t SlfFktCanBufferWrite(double *pdval, double *pbuffer
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset) {

	uint8_t buffer[8];
	memcpy(buffer,pbuffer,MIN(sizeof(double),8));

	if( SlfFktCanBufferWrite(pdval,buffer,8,startbit,bitlength,is_signed,is_intel
		  			        ,faktor,offset) != OKAY )
							return NOT_OKAY;

	memcpy(pbuffer,buffer,MIN(sizeof(double),8));
	return OKAY;

}
status_t SlfFktCanBufferWrite(double *pdval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset) {

	uint8_t startbyte,ibit;
	uint32_t uval;
	sint32_t sval;

	if( is_intel ) {

		// L‰nge pr¸fen
		if( startbit+bitlength > lbuf*8 ) 
			return NOT_OKAY;

		// Erste position suchen
		startbyte = 0;
		while( startbit > 7 ) {
			startbit = startbit - 8;
			startbyte++;
		}

		if( bitlength > 32 )
			return NOT_OKAY;

		// Wert umformen
		if( is_signed ) { 

			sval = (sint32_t)((*pdval - offset)/NOT_ZERO(faktor));
			uval = (uint32_t)sval;
		} else {
			uval = (uint32_t)((*pdval - offset)/NOT_ZERO(faktor));
		}
		
		ibit = 0;
		while(ibit<bitlength) {

			if( uval & 1<<ibit )
				buffer[startbyte] |= 1<<startbit;
			else
				buffer[startbyte] &= ~(1<<startbit);

			ibit++;
			startbit++;
			if( startbit > 7 ) {
				startbit = 0;
				startbyte++;
			}
		}


		return OKAY;

	} else {

		return NOT_OKAY;
	}

	return OKAY;
}
status_t SlfFktCanBufferWrite(sint32_t intval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel) {
	uint8_t  startbyte,ibit;
	uint32_t uval;
	sint32_t sval;

	if( is_intel ) {

		// L‰nge pr¸fen
		if( startbit+bitlength > lbuf*8 ) 
			return NOT_OKAY;

		// Erste position suchen
		startbyte = 0;
		while( startbit > 7 ) {
			startbit = startbit - 8;
			startbyte++;
		}

		if( bitlength > 32 )
			return NOT_OKAY;

		// Wert umformen
		if( is_signed ) { 

			sval = (sint32_t)intval;
			uval = (uint32_t)sval;
		} else {
			uval = (uint32_t)intval;
		}
		
		ibit = 0;
		while(ibit<bitlength) {

			if( uval & 1<<ibit )
				buffer[startbyte] |= 1<<startbit;
			else
				buffer[startbyte] &= ~(1<<startbit);

			ibit++;
			startbit++;
			if( startbit > 7 ) {
				startbit = 0;
				startbyte++;
			}
		}


		return OKAY;

	} else {

		return NOT_OKAY;
	}

	return OKAY;
}
#if 0
status_t SlfFktCanBufferRead(double *pdval, double *pbuffer
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset) {

	uint8_t buffer[8];
	memcpy(buffer,pbuffer,MIN(sizeof(double),8));

	if( SlfFktCanBufferRead(pdval,buffer,8,startbit,bitlength,is_signed,is_intel
		  			        ,faktor,offset) != OKAY )
							return NOT_OKAY;

	return OKAY;

}
#endif
status_t SlfFktCanBufferRead(double *pdval, uint8_t *buffer, uint8_t lbuf
							 , uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel
							 , double faktor, double offset) {


	uint8_t startbyte,endbyte,endbit;
	uint32_t ival=0;

	if( is_intel ) {

		if( startbit+bitlength > lbuf*8 ) 
			return NOT_OKAY;

		startbyte = 0;
		while( startbit > 7 ) {
			startbit = startbit - 8;
			startbyte++;
		}

		if( bitlength > 32 )
			return NOT_OKAY;

		endbit  = startbit + bitlength-1;
		endbyte = startbyte;
		while( endbit > 7 ) {
			endbit = endbit - 8;
			endbyte++;
		}

		memcpy(&ival,&buffer[startbyte],endbyte-startbyte+1);

		ival = ival << (7-endbit);
		ival = ival >> (7-endbit+startbit);

		if( is_signed && bitlength > 1 && ival & 1<<(bitlength-1)) {


				ival = ~(ival & ~(1<<(bitlength-1)));
				ival <<= (32-bitlength+1);
				ival >>= (32-bitlength+1);


				*pdval = (ival+1)*(-1.0) * faktor + offset;

		} else {
		
			*pdval = ival * faktor + offset;
		}

		return OKAY;

	} else {

		return NOT_OKAY;
	}
}
status_t SlfFktCanBufferRead(int *pival, uint8_t *buffer, uint8_t lbuf
							, uint8_t startbit, uint8_t bitlength, uint8_t is_signed, uint8_t is_intel) {


	uint8_t startbyte,endbyte,endbit;
	uint32_t ival=0;

	if( is_intel ) {

		if( startbit+bitlength > lbuf*8 ) 
			return NOT_OKAY;

		startbyte = 0;
		while( startbit > 7 ) {
			startbit = startbit - 8;
			startbyte++;
		}

		if( bitlength > 32 )
			return NOT_OKAY;

		endbit  = startbit + bitlength-1;
		endbyte = startbyte;
		while( endbit > 7 ) {
			endbit = endbit - 8;
			endbyte++;
		}

		memcpy(&ival,&buffer[startbyte],endbyte-startbyte+1);

		ival = ival << (7-endbit);
		ival = ival >> (7-endbit+startbit);

		if( is_signed && bitlength > 1 && ival & 1<<(bitlength-1)) {


				ival = ~(ival & ~(1<<(bitlength-1)));
				ival <<= (32-bitlength+1);
				ival >>= (32-bitlength+1);


				*pival = (ival+1)*(-1);

		} else {
		
			*pival = ival;
		}

		return OKAY;

	} else {

		return NOT_OKAY;
	}
}
int SlfFktCalcNLoop(double Master_Looptime,double Function_Looptime) {
	
	double t = 0.0;
	int nLoop = 0;
	if( Master_Looptime >= Function_Looptime ) {
		while(t+(double)FLT_EPSILON <= Master_Looptime ) {
			nLoop += 1;
			t      += Function_Looptime;
		}
	} else {
		while(t+(double)FLT_EPSILON <= Function_Looptime ) {
			nLoop -= 1;
			t      += Master_Looptime;
		}
	}
  return nLoop;
}

double SlfFktCalcFreeMemory(char *typ) {

    uint32_t i=0;
    double mess = 0.0;

    if( *typ == 'b' || *typ == 'B' )
        i = 1;
    else if( *typ == 'k' || *typ == 'K' )
        i = 1<<10;
    else if( *typ == 'm' || *typ == 'M'  )
        i = 1<<20;
    else if( *typ == 'g' || *typ == 'G' )
        i = 1<<30;

    SlfFktCalcFreeMemoryPriv(i,&mess);
#if 0
    if( *typ == 'b' || *typ == 'B' )
        kbyte *=1024.;

    if( *typ == 'm' || *typ == 'M' || *typ == 'g' || *typ == 'G' )
        kbyte /=1024.;

    if( *typ == 'g' || *typ == 'G' )
        kbyte /=1024.;
#endif
    return mess;
}
void SlfFktCalcFreeMemoryPriv(uint32_t i,double *pmess) {

    uint8_t *p = new uint8_t[i];
    
    if( p ) {
        
        *pmess += 1.;
        SlfFktCalcFreeMemoryPriv(i,pmess);
        delete []p;
    }
#if 0
    else {
        i=i>>1;
        if( i ) SlfFktCalcFreeMemoryPriv(i,pkb);
    }
#endif
}
