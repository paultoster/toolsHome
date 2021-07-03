// SlfParRead.h
// Klasse zum Einlesen der Parameter
// 
// Aus Parameterdatei:
//
// Grupen
// ======
// <Instanz>        ! Kennzeichnung einer Instanz, wenn weggelassen automatisch
//                  ! wird instanz <instanz0> gesetzt
// [group]          ! Gruppenhierachie
// (subgroup)       ! Untergruppe
// ((subsubgroup))  ! Unteruntergruppe
// ((( ... )))      ! usw.
// 
// [groupA] = groupB           ! Kopie der Gruppe A vollständig Gruppe B
//                             ! gleicher Instanz
// [groupA] = Instanz1.groupB  ! vollständige Beschreibung
//
// (sgroup2) = sgroup3                   ! Kopie aus gleicher Übergruppe
// (sgroup2) = groupA.sgroup3            !       aus Gruppe A
// (sgroup2) = Instanz1.groupA.sgroup3   !       aus Instanz 1 und Gruppe A
//
//  Attribute
//
// (groupXY).comment = Kommentar
// (groupXY).copy    = groupA.sgroup3           !       aus Gruppe A
//
//
//
// Variablen
// =========
//  Varname [unit] = Wert                       ! Einzelwert
//  Varname [unit] = [Wert1,Wert2,Wert3,Wert4]  ! Vektor
//  Varname [unit] = [Wert1,Wert2;Wert3,Wert4]  ! Matrix_t ,: trennt Zeile
//                                                       ;: trennt Spalte
//
//  Attribute
//
//  Varname.copy   = groupA.sgroup3.Var1name   ! aus Gruppe A kopieren
//  Varname.copy   = Var1name                    ! aus gleicher Gruppe kopieren
//  Varname.factor = Wert                        ! FAktor
//  Varname.offset = Wert                        ! Offset
//                                               ! = Wert*Faktor + Offset
//  Varname.comment = String                     ! Kommentar
//  Varname.unit    = Einheit                    ! Einheit
//  
//
//  Tabelle
//  =======
//
//  {{Tabelle}}                                  ! 1D-Tabelle
//  {{Tabelle}}.comment = Kommentar
//  XVarname [unit] = [,,,]                      ! X-Vektor Reihenfolge beachten
//  YVarname [unit] = [,,,]                      ! Y-Vektor
//
//  {{{Tabelle}}}                                  ! 2D-Tabelle
//  {{Tabelle}}.comment = Kommentar
//  XVarname [unit] = [,,]                       ! X-Vektor Reihenfolge beachten
//  YVarname [unit] = [,,,]                      ! Y-Vektor
//  ZVarname [unit] = [,,,;,,,;,,,]              ! Z-Matrix_t
//
//  Attribute TAbelle
//
//  {{Tabelle}} = group.Tabelle0              ! Möglichkeiten Tabelle zu kopieren
//  {{Tabelle}}.copy = group.Tabelle0             ! mit Attribute
//  {{Tabelle}}.comment = Kommentar
//
//  Attribute Variable X,Y,Z
//
//  XVarname.copy   = [groupA](sgroup3)Var1name   ! aus Gruppe A kopieren
//  XVarname.copy   = Var1name                    ! aus gleicher Gruppe kopieren
//  XVarname.factor = Wert                        ! FAktor
//  XVarname.offset = Wert                        ! Offset
//                                                ! = Wert*Faktor + Offset
//  XVarname.comment = String                     ! Kommentar
//  XVarname.unit    = Einheit                    ! Einheit
//
//  Spezial Tabelle (time orbit format)
//  ===================================
//  {var1 [m] var2 var3}
//  0 0 10
//  1 2 11
//  2 2 11
//  3 1 12
//  4 0 15
//
//  Entspricht:
//  var1 [m] = 0,1,2,3,4;
//  var2 = 0,2,2,1,0;
//  var3 = 10,11,11,12,15;
//
// Kommentar: Möglichkeiten
// ==========
//
// var.comment = "Das ist ein Kommenatr !!"
// var.comment = "
// Das ist ein Kommenatr !!
// "
//
// Werte: Möglichkeiten
// ======
//
// var = [1,2,3,4;5,6,7,8]
// var = [1,2,3,4;
//        5,6,7,8]
//
// Variablen aus einem File einladen
// Matlab:
// matread(file=v6_dc_4398.mat,variable=omega_Teff)
//
// Include:
// ========
// #include abc.par
// Die Parameterdatei abc.par wird an der Stelle eingefügt
// Das Attribut #include muss am Anfang der Zeile stehen
// Das Attribut #include darf nicht in einem Kommentar stehen
// sonst Fehlinterpretation

#ifndef SLF_PAR_READ
#define SLF_PAR_READ
//
 
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include "SlfStr.h"

#include "SlfParDef.h"
#include "SlfParReadFile.h"
#include "SlfParData.h"


// class definition --------------------------------
class CSlfParRead : public CSlfBasic
{

public:
  CSlfParRead();
  ~CSlfParRead();

  status_t readParFile(char *par_file,CSlfParData *ppar_read_data);
private:

  enum EReadType 
  {
    TYPE_NO,               
    TYPE_INSTANCE,         
    TYPE_GROUP,
    TYPE_VAR,
    TYPE_1DTAB
  };
  struct SSlfParReadCopyStruct 
  {
    slf::CStrV				      strvsrc;     // Herkunft der zu kopierenden Gruppenstruktur
    slf::CStrV			        strvdes;     // Ziel
    enum EReadType        type;        // Type der Kopie
    SSlfParReadCopyStruct *pNext;      // nächster Pointer
  };
  struct SSlfParReadShapeStruct 
  {
    slf::CStrV				       strvsrc;     // Herkunft der zu kopierenden Gruppenstruktur und Variable
    double  			         factor;      // y = y*factor+offset
    double                 offset;      // 
    SSlfParReadShapeStruct *pNext;      // nächster Pointer
  };


  CSlfParReadFile       ParReadFile;
  CSlfParData           *pParReadData;
  SSlfParReadCopyStruct  *ParReadCopyStruct;
  SSlfParReadShapeStruct *ParReadShapeStruct;

  status_t startReadParFile(void);
  status_t readInstance(void);
  status_t readGroup(void);
  status_t readSubGroup(void);
  status_t readSpzTable(void);
  status_t read1DTable(void);
  status_t read2DTable(void);
  status_t readVar(void);
  status_t readVarVal(const char *pstr);
  status_t analyzeVal(const char *pname,slf::CStr &str,slf::CStrM &strM,Matrix_t *pMat,bool *pIsString);

  status_t readAttribute(slf::CStrV &strv,enum CSlfParRead::EReadType type);
  status_t readAttributeKey(slf::CStr &keystring,slf::CStr &valuestring);

  status_t setCopyStruct(slf::CStrV &strvsrc,slf::CStrV &strvdes,enum EReadType type);
  status_t setValShape(slf::CStrV &strvsrc,double val,uint8_t isFactor);

};

#endif