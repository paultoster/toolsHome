/* 1.0      05.03.03   TBert aus simSys                                */
/* 2.0      26.01.08   TBert aus ays.c                                */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             SlfSys.cpp        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            asys.c
*************************************************************************
* Kurzbeschreibung: 
*
* memory Funktionen: siehe SlfSys.h
************************************************************************/
/************************************************************************

************************************************************************/
#include <stdlib.h>
#if SLF_SYS_SYSTEM_COMP == SLF_SYS_VISUAL_C
#include <direct.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#else
  #error simFunc: nicht programmiert
#endif

#include "SlfSys.h"
#include "SlfStr.h"

namespace slf
{

  char SysReadFileLine(FILE *fid, CStr &tline);

  //IERR_T Sys_scan_sub_pathes_intern(astrv_s *pv_pathes,CHAR *path);

  /*-------------------------------------------------------------------------*/
  /*- Existiert File                                                        -*/
  /*-------------------------------------------------------------------------*/
  bool_t SysExistFile(const char *p_file) {
#ifdef SLF_SYS_VISUAL_C

    struct _stat buf;

    if (_stat(p_file, &buf) == 0)
      return TRUE;
    else
      return FALSE;

#else
#error Sys (SysExistFile): nicht programmiert
#endif
  }

  /*-------------------------------------------------------------------------*/
  /*- Existiert Path                                                        -*/
  /*-------------------------------------------------------------------------*/
  bool_t SysExistPath(const char *ppath) {
#ifdef SLF_SYS_VISUAL_C

    struct _stat buf;
    slf::CStr text = ppath;

    text.elimAnfEnd(" ");
    SlfStrChange(text, "\\", "/");
    text.elimEnd("/");

    if (_stat(text.c_str(), &buf) == 0)
      return TRUE;
    else
      return FALSE;

#else
#error Sys (SysExistPath): nicht programmiert
#endif
  }
  /*========================================================================*/
  void  SysGetActPath(slf::CStr &ActPath) {
#ifdef SLF_SYS_VISUAL_C

    char actpath[_MAX_PATH];
    int  drive = _getdrive();

    _getdcwd(drive, actpath, _MAX_PATH);

    ActPath = actpath;

    if (ActPath.c_str()[ActPath.getLen() - 1] != '\\')
      ActPath.cat("\\");

#else
#error Sys (SysGetActPath): nicht programmiert
#endif
  }

  okay_t SysReadFile(const char *filename, CStrV &v, CStr &errtext)
  {
    FILE *fid;
    if (!SysExistFile(filename))
    {
      return NOT_OK;
    }
#ifdef SLF_SYS_VISUAL_C
#if _MSC_VER > 1310
    fopen_s(&fid, filename, "r");
#else
    fid = fopen(filename, "r");
#endif
#else
    fid = fopen(filename, "r");
#endif
   
    if (fid == 0) 
    {
      errtext.clear();
      errtext.catFormat("Error in SysReadFile(%s); Could not open file\n", filename);
      return NOT_OK;
    }

    v.clear();
    CStr tline;
    // read line by line
    while (1)
    {
      CStr tline;
      if (SysReadFileLine(fid, tline) == EOF)
      {
        if (tline.size()) v.append(tline);
        break;
      }
      else
      {
        v.append(tline);
      }
    }

    // close
    fclose(fid);

    return OK;
  }
  char SysReadFileLine(FILE *fid, CStr &tline)
  {
    char   cin;

    // Nächste Zeile einlesen
    //=======================
    while (1) 
    {
      cin = fgetc(fid);
      if (cin == EOF)
      {
        //cin = '\0';
        //tline.cat(cin);
        break;
      }
      else if (cin == '\n')
      {
        //cin = '\0';
        //tline.cat(cin);
        break;
      }
      else
      {
        tline.cat(cin);
      }
    }
    return cin;
  }


#if 0
  /*-------------------------------------------------------------------------*/
  /*- Erweitert relativen Path                                              -*/
  /*-------------------------------------------------------------------------*/
  IERR_T  Sys_create_full_path(astrs_s *ps_full_path, CHAR *rel_path) {

    IERR_T ierr = 0;
    astrs_s *ps_rel_path = astrs_new();

    /* Struktur initialisieren */
    if (ps_full_path == NULL)
      ps_full_path = astrs_new();

    /* rel path prüfen */
    if (astr_such(rel_path, ".", "vs") != 0)
      astrs_cpy(ps_rel_path, rel_path);
    else
    {
      astrs_cpy(ps_rel_path, "./");
      astrs_cat(ps_rel_path, rel_path);
    }

    {
#if SYSTEM_COMP == VISUAL_C
      char full_path[_MAX_PATH];
      if (_fullpath(full_path, rel_path, _MAX_PATH) == NULL) {

        astrs_s *ps_err_txt = astrs_new();
        astrs_cpy(ps_err_txt, "relative path:<");
        astrs_cat(ps_err_txt, rel_path);
        astrs_cat(ps_err_txt, "> ");
        astrs_cat(ps_err_txt, FULLPATH_NOT_FOUND_REASON);
        ierr = FULLPATH_NOT_FOUND_ERR;
        amsg_set_status("Sys.c:Sys_create_full_path(1) %i %s %s", FULLPATH_NOT_FOUND_ERR,
          FULLPATH_NOT_FOUND_TEXT, astrs_string(ps_err_txt));
        astrs_delete(ps_err_txt);
        astrs_delete(ps_rel_path);
        return ierr;
      }
      astrs_cpy(ps_full_path, full_path);
#else
#error Sys (Sys_create_full_path): nicht programmiert
#endif
    }

    return ierr;
  }
  /*-------------------------------------------------------------------------*/
  /*- sucht files oder pathes mit dir                                       -*/
  /*-------------------------------------------------------------------------*/
  IERR_T  Sys_dir_full(astrv_s *ps_list, CHAR *path, CHAR *regel) {
    return Sys_dir(ps_list, path, "*.*", regel);
  }
  IERR_T  Sys_dir(astrv_s *ps_list, CHAR *path, CHAR *filespec, CHAR *regel) {

    IERR_T ierr = OK;
    long handle;
    struct _finddata_t fileinfo;
    astrs_s *ps_dir = astrs_new();
    UINT8_T file_flag = FALSE;
    UINT8_T path_flag = FALSE;
    UINT8_T file_full_name_flag = FALSE;
    UINT8_T path_full_name_flag = FALSE;
    UINT16_T irow = 0;

    /* ps_list initialisieren */
    if (ps_list == NULL)
      ps_list = astrv_new();
    else
      astrv_free(ps_list);

    /* regel bestimmen */
    if (astr_such(regel, "f", "vs") >= 0) {
      file_flag = TRUE;
      file_full_name_flag = FALSE;
    }
    if (astr_such(regel, "F", "vs") >= 0) {
      file_flag = TRUE;
      file_full_name_flag = TRUE;
    }
    if (astr_such(regel, "p", "vs") >= 0) {
      path_flag = TRUE;
      path_full_name_flag = FALSE;
    }
    if (astr_such(regel, "P", "vs") >= 0) {
      path_flag = TRUE;
      path_full_name_flag = TRUE;
    }


    /* Pfad und file_spec zusammenführen */
    if ((ierr = astrs_match_path_file(ps_dir, path, filespec)) != OK)
      return ierr;

    /* Files suchen */
    handle = _findfirst(astrs_string(ps_dir), &fileinfo);

    /* wrong filespec */
    if (handle < 0 && errno == EINVAL) {

      astrs_s *ps_err_txt = astrs_new();
      astrs_cpy(ps_err_txt, "filespec :<");
      astrs_scat(ps_err_txt, ps_dir);
      astrs_cat(ps_err_txt, "> ");
      astrs_cat(ps_err_txt, INVALID_FILESPEC_REASON);
      ierr = INVALID_FILESPEC_ERR;
      amsg_set_status("Sys.c:Sys_dir(1) %i %s %s", INVALID_FILESPEC_ERR,
        INVALID_FILESPEC_TEXT, astrs_string(ps_err_txt));
      astrs_delete(ps_err_txt);
      return ierr;
    }

    /* files found */
    if (handle >= 0) {
      do {
        if (file_flag  && fileinfo.attrib != _A_SUBDIR && fileinfo.attrib != _A_SYSTEM) {

          if (file_full_name_flag == TRUE) {
            astrv_cpy(ps_list, irow, path);

            if (path[strlen(path) - 1] != '\\')
              astrv_cat_str(ps_list, irow, "\\");

            astrv_cat_str(ps_list, irow, fileinfo.name);

          }
          else {
            astrv_cpy(ps_list, irow, fileinfo.name);
          }
          irow++;
        }

        if (path_flag && fileinfo.attrib == _A_SUBDIR) {

          if (path_full_name_flag == TRUE) {
            astrv_cpy(ps_list, irow, path);

            if (path[strlen(path) - 1] != '\\')
              astrv_cat_str(ps_list, irow, "\\");

            astrv_cat_str(ps_list, irow, fileinfo.name);
            astrv_cat_str(ps_list, irow, "\\");

          }
          else {
            astrv_cpy(ps_list, irow, fileinfo.name);
            astrv_cat_str(ps_list, irow, "\\");
          }
          irow++;
        }

      } while (_findnext(handle, &fileinfo) == 0);
      _findclose(handle);
    }

    return ierr;
  }


  /*-------------------------------------------------------------------------*/
  /*- Scanned und listed alle subpathes                                     -*/
  /*-------------------------------------------------------------------------*/
  IERR_T Sys_scan_sub_pathes(astrv_s *pv_pathes, CHAR *root_path) {

    IERR_T ierr = 0;
    astrv_s  *psv_liste = NULL;
    size_t i;

    /* Existenz von root_path prüfen */
    if (!Sys_exist_path(root_path)) {
      ierr = PATH_NOT_FOUND_ERR;
      amsg_set_status("Sys.c:Sys_scan_sub_pathes(1) %i %s %s", ierr, PATH_NOT_FOUND_TEXT, root_path);
      return ierr;
    }

    /*Zuerst root_path in pv_pathes ablegen */
    astrv_free(pv_pathes);
    astrv_cpy(pv_pathes, 0, root_path);

    /* Scanned nach allen Unterpfaden */
    psv_liste = astrv_new();
    ierr = Sys_dir(psv_liste, root_path, "*.*", "p");
    if (ierr) {
      amsg_set_status("Sys.c:Sys_scan_sub_pathes %i %s %s", ierr, "Error occured in Sys_dir", "");
      astrv_delete(psv_liste);
      return ierr;
    }

    /* Sucht rekursive in allen Unterpfaden nach weiteren Pfaden */
    for (i = 0; i < astrv_nrow(psv_liste); i++) {

      ierr = Sys_scan_sub_pathes_intern(pv_pathes, astrv_string(psv_liste, i));
      if (ierr) {
        amsg_set_status("Sys.c:Sys_scan_sub_pathes %i %s %s", ierr, "Error occured in Sys_scan_sub_pathes_intern", "");
        astrv_delete(psv_liste);
        return ierr;
      }
    }

    astrv_delete(psv_liste);

    return ierr;

  }
  /*-------------------------------------------------------------------------*/
  /*- Scanned und listed alle subpathes interne Funktion                    -*/
  /*-------------------------------------------------------------------------*/
  IERR_T Sys_scan_sub_pathes_intern(astrv_s *pv_pathes, CHAR *path) {

    astrv_s *psv_liste = NULL;
    size_t i;
    IERR_T ierr = 0;

    /* Existenz von root_path prüfen */
    if (!Sys_exist_path(path)) {
      ierr = PATH_NOT_FOUND_ERR;
      amsg_set_status("Sys.c:Sys_scan_sub_pathes_intern(1) %i %s %s", ierr, PATH_NOT_FOUND_TEXT, path);
      return ierr;
    }

    /*path in pv_pathes ablegen */
    astrv_cat(pv_pathes, path);

    /* Scannen nach allen Unterpfaden */
    psv_liste = astrv_new();
    ierr = Sys_dir(psv_liste, path, "*.*", "p");
    if (ierr) {
      amsg_set_status("Sys.c:Sys_scan_sub_pathes %i %s %s", ierr, "Error occured in Sys_dir", "");
      astrv_delete(psv_liste);
      return ierr;
    }

    for (i = 0; i < astrv_nrow(psv_liste); i++) {

      ierr = Sys_scan_sub_pathes_intern(pv_pathes, astrv_string(psv_liste, i));
      if (ierr) {
        astrv_delete(psv_liste);
        return ierr;
      }
    }
    astrv_delete(psv_liste);
    return ierr;

  }
  /*========================================================================*/
  IERR_T Sys_scan_path(char *pPath, char * trennzeichen, char **ppList, size_t *pList_mem) {

    IERR_T ierr = 0;

    return ierr;

#if 0
#if SYSTEM_OS  ==  WINDOWS

    FILE      *p;
    size_t    List_l = 0, sign, k;
    char      *pList;
    char command[256];

    astr_change(pPath, "/", "\\");
    sprintf(command, "dir /a:d /b %s > %s", pPath, DEF_TEMP_FILE);
    astr_change(pPath, "\\", "/");

    system(command);

    p = fopen(DEF_TEMP_FILE, "r");
    if (p != NULL) {
      List_l = 0;
      while ((sign = fgetc(p)) != EOF) {
        List_l++;
      }
      List_l++;
      if (amem_smem(ppList, pList_mem, List_l) != 0) {
        simMessErr(0.0, "simFuncDirPath", "Not enough Memory", CANNOT_ALLOCATE_MEMORY);
        return CANNOT_ALLOCATE_MEMORY;
      }
      rewind(p);
      pList = *ppList;
      for (k = 0; (sign = fgetc(p)) != EOF; k++) {
        pList[k] = (char)sign;
      }
      pList[List_l - 1] = '\0';

    }
    else {
      simMessErr(0.0, "simFuncDirPath", "Connot open "DEF_TEMP_FILE, UNABLE_TO_OPEN_FILE);
      return UNABLE_TO_OPEN_FILE;
    }
    fclose(p);
    sprintf(command, "del %s", DEF_TEMP_FILE);
    system(command);

    astr_change(pList, "\n", " ");
    astr_change(pList, "  ", " ");
    astr_cut_ae(pList, " ");
    astr_change(pList, " ", trennzeichen);

    return 0;
#else
#error simFuncDirPath: fuer nicht Windows noch nicht programmiert
#endif
#endif
  }
  /*========================================================================*/
  IERR_T  Sys_get_act_path(astrs_s *ps_ActPath) {
    IERR_T prog_state;
    CHAR ActPath[_MAX_PATH];
    int  drive;

    if (ps_ActPath == NULL)
      ps_ActPath = astrs_new();


#if SYSTEM_COMP == VISUAL_C
    drive = _getdrive();
    _getdcwd(drive, ActPath, _MAX_PATH);
#else
#error Sys_get_act_path: nicht programmiert
#endif

    prog_state = astrs_cpy(ps_ActPath, ActPath);

    if (ActPath[strlen(ActPath) - 1] != '\\')
      prog_state += astrs_cat(ps_ActPath, "\\");

    return prog_state;
  }
  /*========================================================================*/
  IERR_T  Sys_create_new_path(CHAR *p_Path, BOOL_T f_new) {

    IERR_T       prog_state = 0;

#if SYSTEM_COMP == VISUAL_C

    _mkdir(p_Path);
#else
#error Sys_create_new_path: nicht programmiert
#endif

    if (f_new == TRUE)
      Sys_delete_all_in_path(p_Path);



    return prog_state;
  }

  /*========================================================================*/
  IERR_T  Sys_delete_all_in_path(CHAR *p_Path) {

    IERR_T       prog_state = 0;
    astrv_s *pv_list = astrv_new();
    size_t i;

    /* Scanned nach Verzeichnissen */
    Sys_dir_full(pv_list, p_Path, "P");

    /* Löscht Verzeichnisse */
    for (i = 0; i < pv_list->nrow; i++) {

      if ((astr_such(astrv_string(pv_list, i), ".\\", "rs") != MAX(0, (int)strlen(astrv_string(pv_list, i)) - 2))
        && (astr_such(astrv_string(pv_list, i), "..\\", "rs") != MAX(0, (int)strlen(astrv_string(pv_list, i)) - 3))
        ) {
        Sys_delete_all_in_path(astrv_string(pv_list, i));

#if SYSTEM_COMP == VISUAL_C
        _rmdir(astrv_string(pv_list, i));
#else
#error Sys_delete_all_in_path: nicht programmiert
#endif
      }
    }

    astrv_free(pv_list);

    /* Scanned nach Dateien */
    Sys_dir_full(pv_list, p_Path, "F");

    /* Löscht Dateien */
    for (i = 0; i < pv_list->nrow; i++)
      remove(astrv_string(pv_list, i));

    astrv_delete(pv_list);

    return prog_state;
  }


#if 0

  /*========================================================================*/
  INT16 simFuncCopyFile(CHAR *pFileName, CHAR *pCopyPath, CHAR *pTargetPath) {

    astr_struct *pastrCopyFileName = astr_new();
    astr_struct *pastrTargetFileName = astr_new();
    INT16 prog_state = OK;

    /* Den vollen NAmen mit Input-Verzeichnis erstellen */
    if (astr_cat_pfe(pastrCopyFileName, pCopyPath, pFileName, "", "pf") != OK) {
      prog_state = (INT16)astr_err();
      simMessErr(0.0, "simCopyFile", astr_message(), prog_state);
      astr_message_init();
      astr_delete(pastrCopyFileName);
      astr_delete(pastrTargetFileName);
      return prog_state;
    }

    if (simExistFile(pastrCopyFileName->pBuffer)) {

      astr_struct *pastrCommand = astr_new();
      /* Zieldateiname mit Output-Verzeichnis erstellen */
      astr_cat_pfe(pastrTargetFileName, pTargetPath, pFileName, "", "pf");

#if SYSTEM_OS  ==  WINDOWS

      /* DOS-Kommando erstellen */
      if (astr_err() == 0)astr_cat(pastrCommand, "copy ");
      if (astr_err() == 0)astr_scat(pastrCommand, pastrCopyFileName);
      if (astr_err() == 0)astr_cat(pastrCommand, " ");
      if (astr_err() == 0)astr_scat(pastrCommand, pastrTargetFileName);
      /* DOS Backslash einführen */
      astr_change(pastrCommand->pBuffer, "/", "\\");
      /* Option anhängen */
      if (astr_err() == 0)astr_cat(pastrCommand, " /Y");
      if (astr_err() != 0) {
        prog_state = (INT16)astr_err();
        simMessErr(0.0, "simCopyFile", astr_message(), prog_state);
        astr_message_init();
        astr_delete(pastrCopyFileName);
        astr_delete(pastrTargetFileName);
        return prog_state;
      }

      /* DOS Kommando ausführen */
      system(pastrCommand->pBuffer);
      astr_delete(pastrCommand);
#else
#error simCopyFile: fuer nicht Windows noch nicht programmiert
#endif
    }
    return prog_state;
  }
#endif

#endif //(#if0)
} // namespace