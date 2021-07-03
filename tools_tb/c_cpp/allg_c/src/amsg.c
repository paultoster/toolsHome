/* $JustDate:: 17.08.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      03.03.03   TBert neu                                       */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             amsg.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            simMess.c
*************************************************************************
* Kurzbeschreibung: 
*
* Ausgabe von info- und error-messages: siehe amsg.h
************************************************************************/
/************************************************************************

************************************************************************/
#include "amsg.h"
#include "astr.h"
#include "astrs.h"
#include "amem.h"

#define AMSG_DEFAULT_FILENAME "logfile.txt"
#define AMSG_DEFAULT_LENGTH 25

/* Strukturdefinition */
typedef
struct tag_amsg_t {
    
    astrs_s    *ps_file;           // file name inclusive path
    BOOL_T      f_write_to_file;    // flag, if to write to file
    BOOL_T      f_write_to_display; // flag, if write display: no ps_msg 
    FILE       *file_handle;       // FILE-Pointer  (has to be initialised with NULL)
    astrs_s    *ps_msg;            // Textpointer to structure
    BOOL_T      f_init_done;        // flag, if init is done
    astrs_s    *ps_ident_name;          // identifikation name;
    BOOL_T     err_flag;            // Fehler flag
} amsg_t;
typedef
struct tag_amsg_format_t {
    
    CHAR         *p_type;
    UINT16_T      width;
    astrs_s *ps_text;
    astrs_s *ps_format;
} amsg_format_t;

static amsg_t s_amsg_internal={NULL,FALSE,TRUE,NULL,NULL,FALSE,NULL,FALSE};

STATUS_T amsg_open_file(void);
STATUS_T amsg_close_file(void);
STATUS_T amsg_set_format_liste(char *p_format,amsg_format_t **p_format_liste,UINT16_T *n_format_liste);

/*======================================================================================================*/
STATUS_T amsg_init(char *p_indent_name,BOOL_T force_init_flag,BOOL_T log_flag, char *file_name,BOOL_T display_flag) {
    
    
    
    if( !s_amsg_internal.f_init_done )
    {
        
        /* festlegen, ob in File geschrieben wird */
        s_amsg_internal.f_write_to_file = log_flag;
        s_amsg_internal.f_write_to_display = display_flag;
        
        
        
        if( s_amsg_internal.f_write_to_file ) {
            
            if( s_amsg_internal.ps_file == NULL )
                s_amsg_internal.ps_file = astrs_new();
            
            if( strlen(file_name) > 0 ) {
                if( astrs_cpy(s_amsg_internal.ps_file,file_name) != 0 ) {
                    printf("Error in <amsg_init> aufgetreten\n");
                    return NOT_OK;
                }
            }
            if( amsg_open_file() != OK ) {
                printf("Error in <amsg_init> aufgetreten\n");
                return NOT_OK;
            }
        }
        
        if( s_amsg_internal.ps_msg != NULL ) {
            
            if( astrs_free(s_amsg_internal.ps_msg) ) {
                printf("Error in <amsg_init> aufgetreten\n");
                return NOT_OK;
            }
            
        } else {
            
            s_amsg_internal.ps_msg=astrs_new();
        }

        if( s_amsg_internal.ps_ident_name == NULL )
            s_amsg_internal.ps_ident_name = astrs_new();

        astrs_cpy(s_amsg_internal.ps_ident_name,p_indent_name);

        
        s_amsg_internal.f_init_done = TRUE;

        // Failure zurücksetzen 
        s_amsg_internal.err_flag = FALSE;

    } else if(  (strcmp(astrs_string(s_amsg_internal.ps_ident_name),p_indent_name) != 0)
             && (force_init_flag == TRUE)
             ) { /* reinitialize if other modul wants to initialize and forces init */

        /*display flag*/
        if( s_amsg_internal.f_write_to_display && !display_flag )
            astrs_free(s_amsg_internal.ps_msg);

        /*log flag */
        s_amsg_internal.f_write_to_display = display_flag;

        /* festlegen, ob in File geschrieben wird */
        s_amsg_internal.f_write_to_file    = log_flag;

        /* wenn kein File geschrieben werden soll und schon besteht => schliessen */
        if( !s_amsg_internal.f_write_to_file && (s_amsg_internal.file_handle != NULL) )
            amsg_close_file();

        if( s_amsg_internal.f_write_to_file ) { /* wenn geschrieben werden soll */

            if( s_amsg_internal.file_handle != NULL ) { /* aber schon ein File geöffnet */

                /* das alte File schliessen und in das neue umkopieren */
                astrs_s *ps_old_file = astrs_new();
                FILE *fid=NULL;
                char cread;

                if( astrs_scpy(ps_old_file,s_amsg_internal.ps_file) != OK ) {
                    amsg_set_status("error with <astrs_cpy> in <amsg_init>");
                    return NOT_OK;
                }

                amsg_close_file();
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                fopen_s(&fid, astrs_string(ps_old_file), "r");
#else
                fid = fopen(astrs_string(ps_old_file), "r");
#endif

               

                if( strlen(file_name) > 0 ) {
                    if( astrs_cpy(s_amsg_internal.ps_file,file_name) != 0 ) {
                        printf("Error in <amsg_init> aufgetreten\n");
                        return NOT_OK;
                    }
                }
                if( amsg_open_file() != OK ) {
                    printf("Error in <amsg_init> aufgetreten\n");
                    return NOT_OK;
                }
                
                if( fid != NULL ) {

                    while( (cread=fgetc(fid)) != EOF )
                        fputc(cread,s_amsg_internal.file_handle);

                    fclose(fid);
                }
                astrs_delete(ps_old_file);

            } else { /* noch kein File geöffnet und jetzt öffnen */
                if( strlen(file_name) > 0 ) {
                    if( astrs_cpy(s_amsg_internal.ps_file,file_name) != 0 ) {
                        printf("Error in <amsg_init> aufgetreten\n");
                        return NOT_OK;
                    }
                }
                if( amsg_open_file() != OK ) {
                    printf("Error in <amsg_init> aufgetreten\n");
                    return NOT_OK;
                }
            }
        }

        if( s_amsg_internal.ps_ident_name == NULL )
            s_amsg_internal.ps_ident_name = astrs_new();

        astrs_cpy(s_amsg_internal.ps_ident_name,p_indent_name);

        s_amsg_internal.f_init_done = TRUE;

        // Failure zurücksetzen 
        s_amsg_internal.err_flag = FALSE;
    }
                    


    return OK;
}
/*======================================================================================================*/
STATUS_T amsg_done(char *p_indent_name) {
    
    if(  s_amsg_internal.f_init_done 
      &&  (  (strcmp(astrs_string(s_amsg_internal.ps_ident_name),p_indent_name) == 0)
          || strlen(p_indent_name) == 0
          )
      ) { /* if init was done and modul with ps_ident_name has done init */

        if( s_amsg_internal.file_handle != NULL ) {
        
            if( amsg_close_file() != OK ) {
                printf("Error in <amsg_done> aufgetreten\n");
                return NOT_OK;
            }
            s_amsg_internal.file_handle = NULL;
        
        }
    
        if( s_amsg_internal.ps_file != NULL ) {
        
            if( astrs_delete(s_amsg_internal.ps_file) ) {
                printf("Error in <amsg_done> aufgetreten\n");
                return NOT_OK;
            }
            s_amsg_internal.ps_file = NULL;
        
        }
    
        if( s_amsg_internal.ps_msg != NULL ) {
        
            if( astrs_delete(s_amsg_internal.ps_msg) ) {
                printf("Error in <amsg_done> aufgetreten\n");
                return NOT_OK;
            }
            s_amsg_internal.ps_msg = NULL;
        }
    
        if( s_amsg_internal.ps_ident_name != NULL ) {
        
            if( astrs_delete(s_amsg_internal.ps_ident_name) ) {
                printf("Error in <amsg_done> aufgetreten\n");
                return NOT_OK;
            }
            s_amsg_internal.ps_ident_name = NULL;
        }

        
        s_amsg_internal.f_init_done = FALSE;

        // Failure zurücksetzen 
        s_amsg_internal.err_flag = FALSE;
    }
    
    return OK;
}
/*======================================================================================================*/
STATUS_T amsg_set_status(char *p_format,...) {
    
    amsg_format_t *p_format_liste=NULL;
    UINT16_T      i,n_format_liste;
    astrs_s  *ps_text = astrs_new();
    astrs_s  *ps_format_text = astrs_new();
    
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
    
    if( s_amsg_internal.f_write_to_display &&  !s_amsg_internal.f_init_done ) {
        if( amsg_init("amsg",FALSE,s_amsg_internal.f_write_to_file,"",s_amsg_internal.f_write_to_display) != OK ) {
            printf("Error in <amsg_set_msg> aufgetreten\n");
            return NOT_OK;
        }
    }

    if( s_amsg_internal.f_write_to_file || s_amsg_internal.f_write_to_display ) {
    
        if( amsg_set_format_liste(p_format,&p_format_liste,&n_format_liste) == NOT_OK ) {
            printf("Error in <amsg_set_msg> aufgetreten\n");
            return NOT_OK;
        }
    
        /* Variable Liste initialisieren */
        va_start(marker,p_format);
    
        for(i=0;i<n_format_liste;i++) {
        
            /* nicht formatierten Text zuerst anhängen */
            if( astrs_scat(ps_text,p_format_liste[i].ps_text) ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                va_end(marker);
                return NOT_OK;
            }
        
        
            /* formatierte Eingabe erzeugen */
            /* altes löschen */
            astrs_cpy(ps_format_text,"\0");
        
            /* formatierte Eingabe übergeben */
            if( p_format_liste[i].p_type != NULL ) {
            
                if( (strcmp(p_format_liste[i].p_type,"c") == 0) ) {
                
                    ivalue = va_arg( marker, int );
                
                    if( ivalue > 31 ) {
                    
                        if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                            printf("Error in <amsg_set_msg> aufgetreten\n");
                            va_end(marker);
                            return NOT_OK;
                        }
                        astrs_cpy(ps_format_text,"\0");
                    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                        sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                        sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                    }
                } else if(  (strcmp(p_format_liste[i].p_type,"d") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"i") == 0)
                    || (strcmp(p_format_liste[i].p_type,"o") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"u") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"x") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"X") == 0) 
                    ) {
                    ivalue = va_arg( marker, int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"e") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"E") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"f") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"g") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"G") == 0) 
                    ) {
                    dvalue = va_arg( marker, double );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"n") == 0) 
                    ) {
                    p_ivalue = va_arg( marker, int* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_ivalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"p") == 0) 
                    ) {
                    p_void = va_arg( marker, void* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_void);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_void);
#endif
                    break;
                
                } else if(  (strcmp(p_format_liste[i].p_type,"s") == 0)
                    ) {
                    p_string = va_arg( marker, char* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_string);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_string);
#endif
                
                } else if( (strcmp(p_format_liste[i].p_type,"lu") == 0) ) {
                
                    luivalue = va_arg( marker, long unsigned int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), luivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), luivalue);
#endif
                } else if(  (strcmp(p_format_liste[i].p_type,"ld") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"li") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lo") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lx") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lX") == 0) 
                    ) {
                    livalue = va_arg( marker, long int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), livalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), livalue);
#endif
                
                } else if( (strcmp(p_format_liste[i].p_type,"hu") == 0) ) {
                
                    suivalue = va_arg( marker, short unsigned int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), suivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), suivalue);
#endif
                } else if(  (strcmp(p_format_liste[i].p_type,"hd") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hi") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"ho") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hx") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hX") == 0) 
                    ) {
                    sivalue = va_arg( marker, short int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), sivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), sivalue);
#endif
                
                }
            }
            /* ps_text anhängen */
            if( astrs_scat(ps_text,ps_format_text) ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                va_end(marker);
                return NOT_OK;
            }
        }
        /* variable Liste beenden */
        va_end(marker);
    
        /* erstellten Text in struktur anhängen */
        if( s_amsg_internal.f_write_to_display && s_amsg_internal.ps_msg != NULL ) {
            if(  astrs_scat(s_amsg_internal.ps_msg,ps_text) != 0  
              || astrs_cat(s_amsg_internal.ps_msg,"\n") != 0 
              ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                return NOT_OK;
            }
        }
        /* erstellten Text in info-File schreiben */
        if( s_amsg_internal.f_write_to_file && s_amsg_internal.file_handle != NULL ) {

            fprintf(s_amsg_internal.file_handle,"%s\n",astrs_string(ps_text));
            fflush(s_amsg_internal.file_handle);
        
        }
        /* Speicher freigeben */
        for(i=0;i<n_format_liste;i++) {
            astrs_delete(p_format_liste[i].ps_text);
            astrs_delete(p_format_liste[i].ps_format);
        }
    
        amem_free_g((void **)&p_format_liste);
    }    
    astrs_delete(ps_format_text);
    astrs_delete(ps_text);
    return OK;
}
/*======================================================================================================*/
STATUS_T amsg_set_error(char *p_format,...) {
    
    amsg_format_t *p_format_liste=NULL;
    UINT16_T      i,n_format_liste;
    astrs_s  *ps_text = astrs_new();
    astrs_s  *ps_format_text = astrs_new();
    
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


    // Fehler setzen
    //==============
    s_amsg_internal.err_flag = TRUE;
    
    if( s_amsg_internal.f_write_to_display &&  !s_amsg_internal.f_init_done ) {
        if( amsg_init("amsg",FALSE,s_amsg_internal.f_write_to_file,"",s_amsg_internal.f_write_to_display) != OK ) {
            printf("Error in <amsg_set_msg> aufgetreten\n");
            return NOT_OK;
        }
    }

    if( s_amsg_internal.f_write_to_file || s_amsg_internal.f_write_to_display ) {
    
        if( amsg_set_format_liste(p_format,&p_format_liste,&n_format_liste) == NOT_OK ) {
            printf("Error in <amsg_set_msg> aufgetreten\n");
            return NOT_OK;
        }
    
        /* Variable Liste initialisieren */
        va_start(marker,p_format);
    
        for(i=0;i<n_format_liste;i++) {
        
            /* nicht formatierten Text zuerst anhängen */
            if( astrs_scat(ps_text,p_format_liste[i].ps_text) ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                va_end(marker);
                return NOT_OK;
            }
        
        
            /* formatierte Eingabe erzeugen */
            /* altes löschen */
            astrs_cpy(ps_format_text,"\0");
        
            /* formatierte Eingabe übergeben */
            if( p_format_liste[i].p_type != NULL ) {
            
                if( (strcmp(p_format_liste[i].p_type,"c") == 0) ) {
                
                    ivalue = va_arg( marker, int );
                
                    if( ivalue > 31 ) {
                    
                        if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                            printf("Error in <amsg_set_msg> aufgetreten\n");
                            va_end(marker);
                            return NOT_OK;
                        }
                        astrs_cpy(ps_format_text,"\0");
                    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                        sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                        sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                    }
                } else if(  (strcmp(p_format_liste[i].p_type,"d") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"i") == 0)
                    || (strcmp(p_format_liste[i].p_type,"o") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"u") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"x") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"X") == 0) 
                    ) {
                    ivalue = va_arg( marker, int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"e") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"E") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"f") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"g") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"G") == 0) 
                    ) {
                    dvalue = va_arg( marker, double );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), dvalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), dvalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"n") == 0) 
                    ) {
                    p_ivalue = va_arg( marker, int* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_ivalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"p") == 0) 
                    ) {
                    p_void = va_arg( marker, void* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_void);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_void);
#endif
                    break;
                
                } else if(  (strcmp(p_format_liste[i].p_type,"s") == 0)
                    ) {
                    p_string = va_arg( marker, char* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_string);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_string);
#endif
                
                } else if( (strcmp(p_format_liste[i].p_type,"lu") == 0) ) {
                
                    luivalue = va_arg( marker, long unsigned int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), luivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), luivalue);
#endif
                } else if(  (strcmp(p_format_liste[i].p_type,"ld") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"li") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lo") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lx") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lX") == 0) 
                    ) {
                    livalue = va_arg( marker, long int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), livalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), livalue);
#endif
                
                } else if( (strcmp(p_format_liste[i].p_type,"hu") == 0) ) {
                
                    suivalue = va_arg( marker, short unsigned int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), suivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), suivalue);
#endif
                } else if(  (strcmp(p_format_liste[i].p_type,"hd") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hi") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"ho") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hx") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hX") == 0) 
                    ) {
                    sivalue = va_arg( marker, short int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), sivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), sivalue);
#endif
                 }
            }
            /* ps_text anhängen */
            if( astrs_scat(ps_text,ps_format_text) ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                va_end(marker);
                return NOT_OK;
            }
        }
        /* variable Liste beenden */
        va_end(marker);
    
        /* erstellten Text in struktur anhängen */
        if( s_amsg_internal.f_write_to_display && s_amsg_internal.ps_msg != NULL ) {
            if(  astrs_scat(s_amsg_internal.ps_msg,ps_text) != 0  
              || astrs_cat(s_amsg_internal.ps_msg,"\n") != 0 
              ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                return NOT_OK;
            }
        }
        /* erstellten Text in info-File schreiben */
        if( s_amsg_internal.f_write_to_file && s_amsg_internal.file_handle != NULL ) {

            fprintf(s_amsg_internal.file_handle,"%s\n",astrs_string(ps_text));
            fflush(s_amsg_internal.file_handle);
        
        }
        /* Speicher freigeben */
        for(i=0;i<n_format_liste;i++) {
            astrs_delete(p_format_liste[i].ps_text);
            astrs_delete(p_format_liste[i].ps_format);
        }
    
        amem_free_g((void **)&p_format_liste);
    }    
    astrs_delete(ps_format_text);
    astrs_delete(ps_text);
    return OK;
}
/*======================================================================================================*/
STATUS_T amsg_set_info(char *p_format,...) {
    
    amsg_format_t *p_format_liste=NULL;
    UINT16_T      i,n_format_liste;
    astrs_s  *ps_text = astrs_new();
    astrs_s  *ps_format_text = astrs_new();
    
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
    
    if( s_amsg_internal.f_write_to_file && !s_amsg_internal.f_init_done ) {
        if( amsg_init("amsg",FALSE,s_amsg_internal.f_write_to_file,"",s_amsg_internal.f_write_to_display) != OK ) {
            printf("Error in <amsg_set_msg> beim Initialisieren aufgetreten\n");
            return NOT_OK;
        }
    }
    
    if( s_amsg_internal.f_write_to_file || s_amsg_internal.f_write_to_display ) {
        if( amsg_set_format_liste(p_format,&p_format_liste,&n_format_liste) == NOT_OK ) {
            printf("Error in <amsg_set_msg> aufgetreten\n");
            return NOT_OK;
        }
    
        /* Variable Liste inizialisieren */
        va_start(marker,p_format);
    
        for(i=0;i<n_format_liste;i++) {
        
            /* nicht formatierten Text zuerst anhängen */
            if( astrs_scat(ps_text,p_format_liste[i].ps_text) ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                va_end(marker);
                return NOT_OK;
            }
        
        
            /* formatierte Eingabe erzeugen */
            /* altes löschen */
            astrs_cpy(ps_format_text,"\0");
        
            /* formatierte Eingabe übergeben */
            if( p_format_liste[i].p_type != NULL ) {
            
                if( (strcmp(p_format_liste[i].p_type,"c") == 0) ) {
                
                    ivalue = va_arg( marker, int );
                
                    if( ivalue > 31 ) {
                    
                        if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                            printf("Error in <amsg_set_msg> aufgetreten\n");
                            va_end(marker);
                            return NOT_OK;
                        }
                        astrs_cpy(ps_format_text,"\0");
                    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                        sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                        sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                    }
                } else if(  (strcmp(p_format_liste[i].p_type,"d") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"i") == 0)
                    || (strcmp(p_format_liste[i].p_type,"o") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"u") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"x") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"X") == 0) 
                    ) {
                    ivalue = va_arg( marker, int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), ivalue);
#endif
                
                } else if(  (strcmp(p_format_liste[i].p_type,"e") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"E") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"f") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"g") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"G") == 0) 
                    ) {
                    dvalue = va_arg( marker, double );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), dvalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), dvalue);
#endif
               
                } else if(  (strcmp(p_format_liste[i].p_type,"n") == 0) 
                    ) {
                    p_ivalue = va_arg( marker, int* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_ivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_ivalue);
#endif
               
                } else if(  (strcmp(p_format_liste[i].p_type,"p") == 0) 
                    ) {
                    p_void = va_arg( marker, void* );
                
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_void);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_void);
#endif
                    break;
                
                } else if(  (strcmp(p_format_liste[i].p_type,"s") == 0)
                    ) {
                    size_t n123=0;
                    p_string = va_arg( marker, char* );
                
                    while( p_string[n123] != 0 )
                        n123++;
                    
                
                    if( astrs_mem(ps_format_text,MAX(strlen(p_string),p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_string);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), p_string);
#endif
                
                } else if( (strcmp(p_format_liste[i].p_type,"lu") == 0) ) {
                
                    luivalue = va_arg( marker, long unsigned int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), luivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), luivalue);
#endif
                } else if(  (strcmp(p_format_liste[i].p_type,"ld") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"li") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lo") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lx") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"lX") == 0) 
                    ) {
                    livalue = va_arg( marker, long int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), livalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), livalue);
#endif
                
                } else if( (strcmp(p_format_liste[i].p_type,"hu") == 0) ) {
                
                    suivalue = va_arg( marker, short unsigned int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten\n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), suivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), suivalue);
#endif
                } else if(  (strcmp(p_format_liste[i].p_type,"hd") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hi") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"ho") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hx") == 0) 
                    || (strcmp(p_format_liste[i].p_type,"hX") == 0) 
                    ) {
                        sivalue = va_arg( marker, short int );
                
                
                    if( astrs_mem(ps_format_text,MAX(AMSG_DEFAULT_LENGTH,p_format_liste[i].width)) != 0 ) {
                        printf("Error in <amsg_set_msg> aufgetreten \n");
                        va_end(marker);
                        return NOT_OK;
                    }
                    astrs_cpy(ps_format_text,"\0");
                
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
                    sprintf_s(astrs_string(ps_format_text), astrs_memsize(ps_format_text), astrs_string(p_format_liste[i].ps_format), sivalue);
#else
                    sprintf(astrs_string(ps_format_text), astrs_string(p_format_liste[i].ps_format), sivalue);
#endif
                
                }
            }
            /* ps_text anhängen */
            if( astrs_scat(ps_text,ps_format_text) ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                va_end(marker);
                return NOT_OK;
            }
        }
        /* variable Liste beenden */
        va_end(marker);
    
#if 0    
        if( s_amsg_internal.ps_msg != NULL ) {
            if( astrs_scat(s_amsg_internal.ps_msg,ps_text) != 0  ) {
                printf("Error in <amsg_set_msg> aufgetreten\n");
                return NOT_OK;
            }
        }
#endif
        /* Info wird in Datei ausgegeben */
        if( s_amsg_internal.file_handle != NULL ) {
            fprintf(s_amsg_internal.file_handle,"%s\n",astrs_string(ps_text));
            fflush(s_amsg_internal.file_handle);
        
        }
    
    
        /* Speicher freigeben */
        for(i=0;i<n_format_liste;i++) {
            astrs_delete(p_format_liste[i].ps_text);
            astrs_delete(p_format_liste[i].ps_format);
        }

        amem_free_g((void **)&p_format_liste);
    }    
    astrs_delete(ps_text);
    astrs_delete(ps_format_text);
    
    return OK;
}
/*======================================================================================================*/
CHAR *amsg_get_msg(void) {

    if( astrs_string(s_amsg_internal.ps_msg) == NULL )
        return "";
    else
        return astrs_string(s_amsg_internal.ps_msg);
}
BOOL amsg_has_msg(void) {

    if( astrs_len(s_amsg_internal.ps_msg) > 0 )
        return TRUE;
    else
        return FALSE;
}
/*======================================================================================================*/
STATUS_T amsg_res_msg(void) {
    
    
    if( s_amsg_internal.ps_msg != NULL ) {
        
        if( astrs_free(s_amsg_internal.ps_msg ) )
            return NOT_OK;
        
    } else {
        
        s_amsg_internal.ps_msg=astrs_new();
    }

    // Fehler
    //=======
    s_amsg_internal.err_flag = FALSE;

    
    return OK;
}
/*======================================================================================================*/
BOOL_T amsg_get_error_status(void) { return s_amsg_internal.err_flag; }
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T amsg_open_file() {
    
    
    
    if( s_amsg_internal.ps_file == NULL ) {
        
        s_amsg_internal.ps_file = astrs_new();
    }
    if( astrs_len(s_amsg_internal.ps_file) == 0 )
    {
        astrs_cpy(s_amsg_internal.ps_file,AMSG_DEFAULT_FILENAME);
    }
    
    if( s_amsg_internal.file_handle != NULL )
        fclose(s_amsg_internal.file_handle);
    
    
#if (SYSTEM_COMP == VISUAL_C) &&  (_MSC_VER > MSC_VER_BIS_VS2005)
    fopen_s(&(s_amsg_internal.file_handle), astrs_string(s_amsg_internal.ps_file), "r");
#else
    s_amsg_internal.file_handle = fopen(astrs_string(s_amsg_internal.ps_file), "r");
#endif
    
    if( s_amsg_internal.file_handle == NULL ) {
        printf("Error in <amsg_open_file> file = %s konnte nicht geöffnet werden\n",astrs_string(s_amsg_internal.ps_file));
        return NOT_OK;
    }
    return OK;
}
/*======================================================================================================*/
STATUS_T amsg_close_file() {
    
    
    if( s_amsg_internal.file_handle != NULL )
        fclose(s_amsg_internal.file_handle);
    
    
    s_amsg_internal.file_handle = NULL;
    
    return OK;
}
/*======================================================================================================*/
STATUS_T amsg_set_format_liste(char *p_format,amsg_format_t **pp_format_liste,UINT16_T *n_format_liste) {
    
    UINT16_T n;
    UINT16_T i,itype,ifound;
    SINT16_T i1,i2;
    amsg_format_t *p_format_liste;
    astrs_s  *ps_format=astrs_new();
    astrs_s  *ps_text=astrs_new();
    char *format_typ[]={"c","d","i","u","o","x","X","f","e","E","g","G","p","n","s",
        "ld","li","lo","lx","lX","lu","hd","hi","ho","hx","hX","hu"};
    
    /* kopiert der format-text in die Struktur */
    astrs_cpy(ps_format,p_format);
    
    *n_format_liste = 0;
    
    while( astrs_len(ps_format) > 0 ) { /* sucht nach Formatzeichen */
        
        /* Anzahl Argumate hochzählen */
        (*n_format_liste)++;
        n = *n_format_liste;
        
        /* Anlegen von n Formatstrukturen */
        amem_mem_g((void **)pp_format_liste,n,sizeof(amsg_format_t));
        p_format_liste = *pp_format_liste;
        
        p_format_liste[n-1].ps_text   = astrs_new();
        p_format_liste[n-1].ps_format = astrs_new();
        
        
        i1 = astr_such(astrs_string(ps_format),"%","vs");
        i2 = astr_such(astrs_string(ps_format),"%%","vs");
        
        if( i1 == i2 ) { /* "%%" gefunden bzw. kein formatzeichen gefunden */
            
            if( i1 == -1 ) { /* keine Formatierung */
                
                astrs_scpy(p_format_liste[n-1].ps_text,ps_format);
                astrs_cpy(p_format_liste[n-1].ps_format,"");
                p_format_liste[n-1].p_type = NULL;
                p_format_liste[n-1].width = 0;
                
                astrs_cut(ps_format,0,MAX(0,(int)astrs_len(ps_format)-1));
                
            } else { /* %% gefunden */
                
                astrs_scpy(p_format_liste[n-1].ps_text,ps_format);
                astrs_cut(p_format_liste[n-1].ps_text,i2+2,MAX(i2+2,(int)astrs_len(p_format_liste[n-1].ps_text)-1));
                
                astrs_cpy(p_format_liste[n-1].ps_format,"");
                p_format_liste[n-1].p_type = NULL;
                p_format_liste[n-1].width = 0;
                
                astrs_cut(ps_format,0,i2+1);
            }
            
        } else { /* Formatstring gefunden */
            
            
            /* Text rauskopieren */
            astrs_scpy(p_format_liste[n-1].ps_text,ps_format);
            astrs_cut(p_format_liste[n-1].ps_text,i1,MAX(i1,(int)astrs_len(p_format_liste[n-1].ps_text)-1));
            if( i1 > 0 )    
                astrs_cut(ps_format,0,i1-1);
            
            ifound = MAX_UINT16;
            for(i=0;i<15;i++) { /* Alle Formattypen durchlaufen */
                
                if(  ((i2=astr_such(astrs_string(ps_format),format_typ[i],"vs")) >= 0) 
                    && (i2 < ifound) /* Den am nächsten liegen Formattyp raussuchen */
                    )
                {
                    ifound = i2;
                    itype  = i;
                }
            }
            
            if( ifound < MAX_UINT16 ) { /* formattyp gefunden */
                
                
                /*  Formatanweisung übergeben */
                astrs_scpy(p_format_liste[n-1].ps_format,ps_format);
                if( ifound < MAX(0,(int)astrs_len(p_format_liste[n-1].ps_format)-1) )
                    astrs_cut(p_format_liste[n-1].ps_format,ifound+1,MAX(ifound+1,(int)astrs_len(p_format_liste[n-1].ps_format)-1));
                
                /* Formattyp */
                p_format_liste[n-1].p_type = format_typ[itype];
                
                /* width */
                astrs_scpy(ps_text,p_format_liste[n-1].ps_format);
                astrs_cut(ps_text,0,0);
                astrs_cut(ps_text,MAX(0,astrs_len(ps_text)-1),MAX(0,astrs_len(ps_text)-1));
                astrs_cut_ae(ps_text," ");
                astrs_cut_a(ps_text,"-");
                astrs_cut_a(ps_text,"+");
                astrs_cut_a(ps_text,"0");
                i1=astr_such(astrs_string(ps_text),".","vs");
                if( i1 > 0 )
                    astrs_cut(ps_text,i1,MAX(i1,(int)astrs_len(ps_text)-1));
                
                if( astrs_len(ps_text) > 0 )
                    p_format_liste[n-1].width = (UINT16_T)atol(astrs_string(ps_text));
                else
                    p_format_liste[n-1].width = 0;
                
                
                /* ps_format für nächste while loop bereinigen */
                astrs_cut(ps_format,0,ifound);
                
                
                
                
                
            } else {
                /* Fehler: Format nicht vorhanden */
            }
            
            
        }
        
        
        
        
        
    }
    
    astrs_delete(ps_format);
    astrs_delete(ps_text);
    
    return OK;
}




