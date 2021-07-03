/*************************************************************************
* File:             SlfSys.h        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            
*************************************************************************
* Kurzbeschreibung: 
*
* System Funktionen: 
===========================================================================
bool_t  SysExistFile(const char *file);
    
      Sagt ob dieses File existiert (1), ansonsten 0

bool_t  SysExistPath(const char *path);

      Sagt ob dieser Path existiert (1), ansonsten 0

void  SlfSysGetActPath(slf::CStr &ActPath);

        Gibt aktuellen Pfad aus.

old: ---------------------------------------------------------------
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
#ifndef SLF_SYS_H_INCLUDED
#define SLF_SYS_H_INCLUDED

#define SLF_SYS_VISUAL_C

#include "SlfStrV.h"

namespace slf
{

  bool_t  SysExistFile(const char *file);
  bool_t  SysExistPath(const char *path);
  void    SysGetActPath(slf::CStr &ActPath);
  okay_t  SysReadFile(const char *filename, CStrV &v, CStr &errtext);

} //namespace
#endif