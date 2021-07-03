/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      05.03.03   TBert aus simSys                                */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/*************************************************************************
* File:             asys.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            simSys.h
*************************************************************************
* Kurzbeschreibung: 
*
* System Funktionen: 
===========================================================================
UINT8_T asys_exist_file(CHAR *pfile);
    
      Sagt ob dieses File existiert (TRUE)

UINT8_T asys_exist_path(CHAR *ppath);

      Sagt ob dieser Path existiert (TRUE)

IERR_T  asys_create_full_path(astrs_s *ps_full_path,CHAR *rel_path);

      Macht aus dem relativen Pfad rel_path, den vollständigen Pfadname und 
      schreibt ihn in ps_full_path Die Funktion schaut, wie der aktuelle Pfad heißt

IERR_T  asys_dir(astrv_s *pv_list,CHAR *path,CHAR *regel);

        Kopiert nach der Regel alle files oder Verzeichnisse in die vektor-Struktur

        regel: "p" Verzeichnis
               "f" Datei
               "pf" beides

        Wenn die Regel groß gecshrieben "P", "F", oder "PF", wird in der Liste
        NAme mit dem path zusammen gesetzt, d.h vollständig mit absoluten Pfadangabe aus *path

IERR_T  asys_dir_full(astrv_s *pv_list,CHAR *path,CHAR *filespec,CHAR *regel);

        Kopiert nach der Regel alle files oder Verzeichnisse  mit der angegebenen Spec
        in die vektor-Struktur

        regel: "p" Verzeichnis          filespec z.B. "*.doc"
               "f" Datei
               "pf" beides

        Wenn die Regel groß gecshrieben "P", "F", oder "PF", wird in der Liste
        NAme mit dem path zusammen gesetzt, d.h vollständig mit absoluten Pfadangabe aus *path

IERR_T  asys_scan_sub_pathes(astrv_s *pv_pathes, CHAR *root_path);

        Scannt alle Unterverzeichnisse vom root_path ab und kopiert sie in die
        Vektor-String_struktur pv_pathes.

IERR_T  asys_get_act_path(astrs_s *ps_ActPath);

        Gibt aktuellen Pfad aus.

IERR_T  asys_create_new_path(CHAR *p_Path,BOOL_T f_new);

        Erstellt neuen Pfad (Verzeichnis) p_Path

        Wenn f_new == TRUE => werden alle Files und Verzeichnisse im angelegen Pfad, wenn vorhanden 
                              gelöscht

                   == FALSE => keine Files und Verzeichnisse werden rausgelöscht

IERR_T  asys_delete_all_in_path(CHAR *p_Path);

        Der Path mit allen Files und Unterverzeichnissen wird gelöscht

  
    simFindFile                 Sucht aus einer Liste von Verzeichnissen und dem Filenamen
                                das existierende File
===========================================================================
************************************************************************/
#ifndef asys_h_included

#define asys_h_included

#include "definer.h"
#include "astrs.h"

#ifdef __cplusplus
  extern "C" {
#endif

BOOL_T  asys_exist_file(CHAR *pfile);
BOOL_T  asys_exist_path(CHAR *ppath);
IERR_T  asys_create_full_path(astrs_s *ps_full_path,CHAR *rel_path);
IERR_T  asys_dir_full(astrv_s *ps_list,CHAR *path,CHAR *regel);
IERR_T  asys_dir(astrv_s *ps_list,CHAR *path,CHAR *filespec,CHAR *regel);
IERR_T  asys_scan_sub_pathes(astrv_s *pv_pathes, CHAR *root_path);
IERR_T  asys_get_act_path(astrs_s *ps_ActPath);
IERR_T  asys_create_new_path(CHAR *p_Path,BOOL_T f_new);
IERR_T  asys_delete_all_in_path(CHAR *p_Path);

#ifdef __cplusplus
  }
#endif

#endif