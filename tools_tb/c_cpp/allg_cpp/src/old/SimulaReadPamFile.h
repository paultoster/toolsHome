//
// SimulaReadPamFile.h
//
// Einlesen eines Parameter-Files (extension .pam)
//
// Syntax Parameterdatei:
//
// Grupen
// ======
// [group]          ! Gruppenhierachie   1. Gruppenebene
// (subgroup)       ! Untergruppe        2. Gruppenebene
// ((subsubgroup))  ! Unteruntergruppe   3. Gruppenebene
// ((( ... )))      ! usw.
// 
// [groupA] = groupB           ! Kopie der Gruppe A vollständig Gruppe B
//                             ! gleicher Instanz
// [groupA] = 0:groupB         ! vollständige Beschreibung mit erster Instanz
//
// (sgroup2) = sgroup3                   ! Kopie aus gleicher Übergruppe
// (sgroup2) = groupA.sgroup3            !       aus Gruppe A
// (sgroup2) = 0:groupA.sgroup3          !       aus Instanz 1 und Gruppe A
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
//                                                         ;: trennt Spalte
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
//  |Tabelle|                             ! 1D-Tabelle
//  |Tabelle|.comment = Kommentar
//  x [unit] = [x0,x1,x2,x3]              ! X-Vektor Reihenfolge beachten Name muss x sein
//  y [unit] = [y0,y1,y2,y3]              ! Y-Vektor Name muss y sein
//
//  ||Tabelle||                           ! 2D-Tabelle
//  ||Tabelle||.comment = Kommentar
//  x [unit] = [x0,x1,x2]                 ! X-Vektor Reihenfolge beachten
//  y [unit] = [y0,y1,y2,y3]              ! Y-Vektor
//  z [unit] = [x0y0,x0y1,x0y2,x0y3;
//              x1y0,x1y1,x1y2,x1y3;
//              x2y0,x2y1,x2y2,x2y3]      ! Z-Matrix_t Namme mus y sein
//                     
//
//  Attribute Tabelle
//
//  ||Tabelle|| = group.Tabelle0                 ! Möglichkeiten Tabelle zu kopieren
//  ||Tabelle||.copy = group.Tabelle0            ! mit Attribute
//  ||Tabelle||.comment = Kommentar
//
//  Attribute Variable X,Y,Z
//
//  x.copy   = [groupA](sgroup3)Var1name   ! aus Gruppe A kopieren
//  x.copy   = Var1name                    ! aus gleicher Gruppe kopieren
//  x.factor = Wert                        ! FAktor
//  x.offset = Wert                        ! Offset
//                                                ! = Wert*Faktor + Offset
//  x.comment = String                     ! Kommentar
//  x.unit    = Einheit                    ! Einheit
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
// ========================
//
// ! Das ist ein Kommenatr !!
//  var1 [m] = 0,1,2,3,4; ! Das ist ein Kommentar
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
// #include abc.pam
// Die Parameterdatei abc.pam wird an der Stelle eingefügt
// Das Attribut #include muss am Anfang der Zeile stehen
// Das Attribut #include darf nicht in einem Kommentar stehen
// sonst Fehlinterpretation
// 
// Instanzen
// =========
//
// <Instanz>        ! Kennzeichnung einer Instanz, Instanz ist eine Nummer und sollte bei null beginnen
//                  ! wenn weggelassen automatisch wird instanz <0> gesetzt
//                  ! Beispiel
// <0>              ! erste Instanz
// var1 [m] = 5.0   ! gesamter Datensatz
// <1>              ! zweite Instanz
// var1 [m] = 10.0  ! gesamter oder ausgewählter Datensatz
// <2>              ! dritte Instanz
// var1 [m] = 15.0  ! gesamter oder ausgewählter Datensatz
//
//                  ! andere Möglichkeit für einzelne Variablen, dabei werden alle anderen nicht aufgezählte oder inkrementierte
//                  ! Variablen gleich gelassen
//
// var1 [m] = <5.0,10.0,15.0>
// var1 [m] = <5.0:5.0:15.0>
//
// var2 [m] = <[5.0,20.0],[10.0,30.]>
// var2 [m] = <[5.0,20.0]:[5.0,10.0]:[10.0,30.]>
//
#ifndef SIMULA_READ_PAM_FILE_H
#define SIMULA_READ_PAM_FILE_H

#endif