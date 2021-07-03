/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      19.08.04   TBert aus                                       */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             aasap2_read.c        
* Verfasser:        Thomas Berthold (TBert)/3052
* Abteilung:        TZS/Continental TEVES AG & CO. oHG
* Basis:            keine
*************************************************************************
* Kurzbeschreibung: 
*
* Liest asap2-File ein. und gibt Struktur mit eingelesenen Daten zurück
* A
************************************************************************/
/************************************************************************

************************************************************************/

//#include <sys/types.h>
#include <sys/stat.h>

#include "aasap2.h"

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
    #include "amsg.h"
#endif

//#define AASAP2_COPY(txt,lentxt) strncpy(txt,p_token_aasap2_1,MIN(strlen(p_token_aasap2_1),lentxt+1))
#define AASAP2_COPY(txt,lentxt) if(strlen(p_token_1)<lentxt)strncpy(txt,p_token_1,strlen(p_token_1)+1);else{strncpy(txt,p_token_1,lentxt);*(txt+lentxt)='\0';}

/*-------------------------------------------------------------------------*/
/*- read asap-File                                                        -*/
/*-------------------------------------------------------------------------*/
STATUS_T aasap2_c::read(char *p_input_file) {

    struct _stat buf;

    /* existiert das input-File */
    if( _stat( p_input_file, &buf ) != 0 ) {
        
        AASAP2_PRINT("\n\nERROR in <aasap2_c::read>: File %s does not exist",p_input_file);
        return NOT_OK;
    }

    /* File Name kopieren */
	if( file_name )
		delete []file_name;
	file_name = new char[strlen(p_input_file)+1];
	memcpy(file_name,p_input_file,sizeof(char)*(strlen(p_input_file)+1));

    //file_name += p_input_file;
    
    /* File öffnen */
    if( (p_fid = fopen(p_input_file,"r")) == NULL ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read>: File %s could not be opnened",p_input_file);
        return NOT_OK;
    }

    /* Zeilennummer initialisieren */
    fid_izeile = 0;

    /* Projekt einlesen */
    if( read_start() != OK ) {

        fclose(p_fid);
		file_name = 0;
        return NOT_OK;
    }

    fclose(p_fid);
	file_name = 0;
    return OK;
}
/*-------------------------------------------------------------------------*/
/*- return asap-Filename                                                  -*/
/*-------------------------------------------------------------------------*/
const char  *aasap2_c::get_file_name(void) {

    return file_name;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_start(void) {

    /* Suche Projektbeginn */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_read_start>: no more token, no project found");
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
    
        if(  strcmp(p_token_1,"/begin") == 0 
          && strcmp(p_token_2,"PROJECT") == 0
          )  { /* Beginn Projekt */

            read_next_token();


            if( read_project() != OK ) {
    
                AASAP2_PRINT("\nERROR in <aasap2_c::read> with <aasap2_c::read_project>");
                return NOT_OK;
            }

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_project(void ) {

    BOOL_T header_read_flag = FALSE;

    /* Projectdaten einlesen */

    /* Name einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_project>: Projectname not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_project->name,AASAP2_MAX_NAME);

    /* Comment einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c:read_project>: Projectcomment not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_project->comment,AASAP2_MAX_COMMENT);
    
    /* Suche MODULE oder HEADER oder ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_c::read_project>: no more token found in File, no /end PROJECT found");
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
    
        /* begin MODULE */
        if(  strcmp(p_token_1,"/begin") == 0 
          && strcmp(p_token_2,"MODULE") == 0
          )  { 

            aasap2_modul_s *ps_modul=NULL;

            read_next_token();

            /* Modul anlegen */
            ps_modul = new (aasap2_modul_s);


            /* Modul initialisieren */
            ps_modul->ps_characteristic  = NULL;
            ps_modul->n_characteristic   = 0;
            ps_modul->ps_conversion      = 0;
            ps_modul->ps_next            = NULL;

            /* Modul einlesen */
            if( read_modul(ps_modul) != OK ) {
    
                AASAP2_PRINT("\nERROR in <aasap2_c::read_project> with <aasap2_c::read_modul>");
                return NOT_OK;
            }

            if( ps_project->ps_modul != NULL ) 
                ps_modul->ps_next = ps_project->ps_modul;
            
            ps_project->ps_modul = ps_modul;

        /* begin HEADER */    
        } else if(  strcmp(p_token_1,"/begin") == 0 
                 && strcmp(p_token_2,"HEADER") == 0
                 && header_read_flag == FALSE
                 )  { 

            aasap2_projectheader_s *ps_header=NULL;

            read_next_token();
    
            /* Header anlegen */
            ps_header = new (aasap2_projectheader_s);

            ps_project->ps_header = ps_header;

            /* Header einlesen */
            if( read_projectheader(ps_header) != OK ) {
    
                AASAP2_PRINT("\nERROR in <aasap2_c::read_project> with <aasap2_c::read_modul>");
                return NOT_OK;
            }


            header_read_flag = TRUE;

        /* ende project */    
        } else if(  strcmp(p_token_1,"/end") == 0 
                 && strcmp(p_token_2,"PROJECT") == 0
                 )  { 

            read_next_token();

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_projectheader(aasap2_projectheader_s *ps_header ) {

    /* Header Comment einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_projectheader>: Header comment not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_header->comment,AASAP2_MAX_COMMENT);

    /* Suche VERSION, PROJECT_NO und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_c::read_projectheader>: no more token, no /end HEADER found");
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
    
        /* VERSION */
        if(  strcmp(p_token_1,"VERSION") == 0 
          )  { 

            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c::read_projectheader>: no more token, no VERION value found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }

            AASAP2_COPY(ps_header->version,AASAP2_MAX_COMMENT);

        /* PROJECT_NO */
        } else if(  strcmp(p_token_1,"PROJECT_NO") == 0 )  { 

            /* Nächster Token einlesen (p_token_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c::read_projectheader>: no more token, no PROJECT_NO value found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }

            AASAP2_COPY(ps_header->project_no,AASAP2_MAX_COMMENT);


        /* ende header */    
        } else if(  strcmp(p_token_1,"/end") == 0 
                 && strcmp(p_token_2,"HEADER") == 0
                 )  { 

            read_next_token();

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_modul(aasap2_modul_s *ps_modul ) {

    char *key_words[] = {"MOD_PAR","MOD_COMMON","IF_DATA","AXIS_PTS","MEASUREMENT"
                        ,"COMPU_TAB","COMPU_VTAB","FUNCTION","RECORD_LAYOUT"};
    
    UINT8_T n_key_words = sizeof( key_words) / sizeof(char *);
    UINT8_T i;

    /* Moduldaten einlesen */

    /* Name einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_modul>: Modulname not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_modul->name,AASAP2_MAX_NAME);

    /* Comment einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_modul>: Modulcomment not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_modul->comment,AASAP2_MAX_COMMENT);
    
    /* Suche weiteren Blöcken und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_c::read_modul>: no more token, no /end MODULE found");
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
    
        /* Suche alle nicht definierten keywords */
        for(i=0;i<n_key_words;i++) {

            /* begin keyword */
            if(  strcmp(p_token_1,"/begin") == 0 
              && strcmp(p_token_2,key_words[i]) == 0
              )  { 

                read_next_token();


                /* Dummy mit /end einlesen */
                if( read_dummy_end(key_words[i]) != OK ) {
    
                    AASAP2_PRINT("\nERROR in <aasap2_c::read_modul> with <aasap2_c::read_dummy_end>");
                    return NOT_OK;
                }
                break;
            }
        }

        /* begin CHARACTERISTIC */    
        if(  strcmp(p_token_1,"/begin") == 0 
          && strcmp(p_token_2,"CHARACTERISTIC") == 0
          )  { 

            aasap2_characteristic_s *ps_charac=NULL;

            read_next_token();
    
            /* characteristic-struct anlegen */
            ps_charac = new (aasap2_characteristic_s);

            ps_charac->address        = 0L;
            ps_charac->asap1b_address = 0L;
            ps_charac->bit_mask       = 0L;
            ps_charac->lower_limit    = MIN_DOUBLE;
            ps_charac->maxdiff        = 0.;
            ps_charac->type           = AASAP2_DEF_TYPE_NON;
            ps_charac->upper_limit    = MAX_DOUBLE;
            ps_charac->ps_next        = NULL;

            /* CHARACTERISTIC einlesen */
            if( read_characteristic(ps_charac) != OK ) {
    
                AASAP2_PRINT("\nERROR in <aasap2_c::read_modul> with <aasap2_c::read_characteristic>");
                return NOT_OK;
            }

            /* Cahrac an Characliste anhängen */

            if( ps_modul->ps_characteristic != NULL ) 
                ps_charac->ps_next = ps_modul->ps_characteristic;
            
            ps_modul->ps_characteristic = ps_charac;

            ps_modul->n_characteristic++;

#if AASAP2_DEBUG 

                printf("\n%4.0i CHARACTERISTIC %s ",ps_modul->n_characteristic,ps_charac->name);
#endif

        /* begin COMPU_METHOD */    
        } else if(  strcmp(p_token_1,"/begin") == 0 
               && strcmp(p_token_2,"COMPU_METHOD") == 0
               )  { 

            aasap2_conversion_s *ps_conv=NULL;

            read_next_token();
    
            /* conversion-struct anlegen */
            ps_conv = new (aasap2_conversion_s);

            ps_conv->type          = AASAP2_CONV_TYPE_NON;
            ps_conv->formula[0]    = '\0';
            ps_conv->compu_tab[0]  = '\0';
            for(i=0;i<6;i++)
                ps_conv->coeffs[i] = 0;
            ps_conv->ps_next       = NULL;
            
            /* COMPU_METHOD einlesen */
            if( read_conversion(ps_conv) != OK ) {
    
                AASAP2_PRINT("\nERROR in <aasap2_c::read_modul> with <aasap2_c::read_conversion>");
                return NOT_OK;
            }

            /* ps_conv an Cnversionliste anhängen */

            if( ps_modul->ps_conversion != NULL ) 
                ps_conv->ps_next = ps_modul->ps_conversion;
            
            ps_modul->ps_conversion = ps_conv;

        /* ende module */    
        } else if(  strcmp(p_token_1,"/end") == 0 
                 && strcmp(p_token_2,"MODULE") == 0
                 )  { 

            read_next_token();

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_characteristic(aasap2_characteristic_s *ps_charac ) {

    /* keywords with /beegin and /end */
    char *key_words[] = {"FUNCTION_LIST","MAP_LIST","DEPENDENT_CHARACTERISTIC","VIRTUAL_CHARACTERISTIC"
                        ,"ANNOTATION"
                        ,"AXIS_DESCR","ECU_ADDRESS_EXTENSION"};
    
    UINT8_T n_key_words = sizeof( key_words) / sizeof(char *);

#if 0
    /* keywords with 0 parameter */
    char *key_words_0[] = {"READ_ONLY","GUARD_RAILS"};
    
    UINT8_T n_key_words_0 = sizeof( key_words_0) / sizeof(char *);

    /* keywords with 1 parameter */
    char *key_words_1[] = {"DISPLAY_IDENTIFIER","BYTE_ORDER","NUMBER","REF_MEMORY_SEGMENT"
                          ,"COMPARISON_QUANTITY","CALIBRATION_ACCESS","AXIS_DESCR","ECU_ADDRESS_EXTENSION"};
    
    UINT8_T n_key_words_1 = sizeof( key_words_1) / sizeof(char *);

    /* keywords with 2 parameter */
    char *key_words_2[] = {"EXTENDED_LIMITS","MAX_REFRESH"};
    
    UINT8_T n_key_words_2 = sizeof( key_words_2) / sizeof(char *);

    /* keywords with 3 parameter */
    char *key_words_3[] = {"MATRIX_DIM"};
    
    UINT8_T n_key_words_3 = sizeof( key_words_3) / sizeof(char *);
#endif

    UINT8_T i;

    /* Charac daten einlesen */

    /* Name einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: Characteristic name not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->name,AASAP2_MAX_NAME);
    //i = MIN(strlen(p_token_aasap2_1),AASAP2_MAX_NAME+1);
    //strncpy(ps_charac->name,p_token_aasap2_1,i);
    /* Comment einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHACRECTERISTIC comment not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->comment,AASAP2_MAX_COMMENT);
    
    /* Type einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHARACTERISTIC TYPE not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }

    if( strcmp(p_token_1,"VALUE") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_VALUE;
    else if( strcmp(p_token_1,"CURVE") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_CURVE;
    else if( strcmp(p_token_1,"MAP") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_MAP;
    else if( strcmp(p_token_1,"CUBOID") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_CUBOID;
    else if( strcmp(p_token_1,"VAL_BLK") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_VAL_BLK;
    else if( strcmp(p_token_1,"ASCII") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_ASCII;
    else            
            ps_charac->type = AASAP2_DEF_TYPE_NON;

    /* Adresse einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHARACTERISTIC address not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }

    sscanf( p_token_1, "%lx", &ps_charac->address );

    /* Deposit einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c:read_characteristic>: CHACRECTERISTIC deposit not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->deposit,AASAP2_MAX_NAME);

    /* Maxdiff einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHARACTERISTIC maxdiff not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }

    ps_charac->maxdiff = atof(p_token_1);

    /* Conversion einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHACRECTERISTIC conversion not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->conversion,AASAP2_MAX_NAME);

    /* lower_limit einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHARACTERISTIC lower_limit not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }

    ps_charac->lower_limit = atof(p_token_1);

    /* upper_limit einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: CHARACTERISTIC upper_limit not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }

    ps_charac->upper_limit = atof(p_token_1);
    
    /* Suche weiteren Blöcken und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: no more token, no /end CHARACTERISTIC found");
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
    
        /* Suche alle nicht definierten keywords */
        for(i=0;i<n_key_words;i++) {

            /* begin keyword */
            if(  strcmp(p_token_1,"/begin") == 0 
              && strcmp(p_token_2,key_words[i]) == 0
              )  { 

                read_next_token();


                /* Dummy mit /end einlesen */
                if( read_dummy_end(key_words[i]) != OK ) {
    
                    AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic> with <aasap2_c::read_dummy>");
                    return NOT_OK;
                }
                break;
            }
        }

        /* begin IF_DATA */    
        if(  strcmp(p_token_1,"/begin") == 0 
          && strcmp(p_token_2,"IF_DATA") == 0
          )  { 


            read_next_token();
            
            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: no more token, no IF_DATA name found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }
            

            /* ASAP1B_ADDRESS Kennzeichnung prüfen */
            if(  strcmp(p_token_1,"ASAP1B_ADDRESS") == 0 
              && strcmp(p_token_2,"KP_BLOB") == 0
              ) {

                read_next_token();
                
                /* Nächster Token einlesen (p_token_1) */
                if( read_next_token() != OK ) {

                    AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: no more token, no KP_BLOB value found");
                    AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                    return NOT_OK;
                }

                sscanf( p_token_1, "%lx", &ps_charac->asap1b_address );

            } else {

                /* Dummy mit /end einlesen */
                if( read_dummy_end("IF_DATA") != OK ) {
    
                    AASAP2_PRINT("\nERROR in <aasap2_c::read_characeteristic> with <aasap2_c::read_dummy_end>");
                    return NOT_OK;
                }
            }


        /* bit mask */
        } else if ( strcmp(p_token_1,"BIT_MASK") == 0 ) {  

            /* Nächster Token einlesen (p_token_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c::read_characteristic>: no more token, no BIT_MASK value found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }

                
             sscanf( p_token_1, "%lx", &ps_charac->bit_mask );

        /* format */
        } else if ( strcmp(p_token_1,"FORMAT") == 0 ) {  

            /* Nächster Token einlesen (p_token_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c:read_characteristic>: no more token, no FORMAT value found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }

                
            AASAP2_COPY(ps_charac->format,AASAP2_MAX_NAME);

        /* ende module */    
        } else if(  strcmp(p_token_1,"/end") == 0 
                 && strcmp(p_token_2,"CHARACTERISTIC") == 0
                 )  { 

            read_next_token();

            break;
        }
    }


    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_conversion(aasap2_conversion_s *ps_conv ) {


    UINT8_T i;

    /* Conv daten einlesen */

    /* Name einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_conversion>: COMPU_METHOD name not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_conv->name,AASAP2_MAX_NAME);
    //i = MIN(strlen(p_token_aasap2_1),AASAP2_MAX_NAME+1);
    //strncpy(ps_charac->name,p_token_aasap2_1,i);
    /* Comment einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_conversion>: COMPU_METHOD comment not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_conv->comment,AASAP2_MAX_COMMENT);
    
    /* Type einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c::read_conversion>: COMPU_METHOD TYPE not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }

    if( strcmp(p_token_1,"TAB_INTP") == 0 ) 
            ps_conv->type = AASAP2_CONV_TYPE_TAB_INTP;
    else if( strcmp(p_token_1,"TAB_NOINTP") == 0 ) 
            ps_conv->type = AASAP2_CONV_TYPE_TAB_NOINTP;
    else if( strcmp(p_token_1,"TAB_VERB") == 0 ) 
            ps_conv->type = AASAP2_CONV_TYPE_TAB_VERB;
    else if( strcmp(p_token_1,"RAT_FUNC") == 0 ) 
            ps_conv->type = AASAP2_CONV_TYPE_RAT_FUNC;
    else if( strcmp(p_token_1,"FORM") == 0 ) 
            ps_conv->type = AASAP2_CONV_TYPE_FORM;
    else            
            ps_conv->type = AASAP2_CONV_TYPE_NON;


    /* Format einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c:read_characteristic>: COMPU_METHOD format not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_conv->format,AASAP2_MAX_FORMAT);

    /* Einhait einlesen */
    if( read_next_token() != OK ) {

        AASAP2_PRINT("\nERROR in <aasap2_c:read_characteristic>: COMPU_METHOD unit not found");
        AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
        return NOT_OK;
    }
    AASAP2_COPY(ps_conv->unit,AASAP2_MAX_FORMAT);

    
    /* Suche weiteren Blöcken und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_c::read_conversion>: no more token, no /end COMPU_METHOD found");
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
    


        /* FORMULA */
        if ( strcmp(p_token_1,"FORMULA") == 0 ) {  

            /* Nächster Token einlesen (p_token_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c::read_conversion>: no more token, no FORMULA value found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }

                
                AASAP2_COPY(ps_conv->formula,AASAP2_MAX_COMMENT);

        /* COMPU_TAB */
        } else if ( strcmp(p_token_1,"COMPU_TAB") == 0 ) {  

            /* Nächster Token einlesen (p_token_1) */
            if( read_next_token() != OK ) {

                AASAP2_PRINT("\nERROR in <aasap2_c:read_characteristic>: no more token, no COMPU_TAB value found");
                AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                return NOT_OK;
            }

                
            AASAP2_COPY(ps_conv->compu_tab,AASAP2_MAX_NAME);

        /* COEFFS */
        } else if ( strcmp(p_token_1,"COEFFS") == 0 ) { 
            

            for(i=0;i<6;i++) {

                /* Nächster Token einlesen (p_token_1) */
                if( read_next_token() != OK ) {

                    AASAP2_PRINT("\nERROR in <aasap2_c:read_characteristic>: no more token, no COEFFS value found");
                    AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
                    return NOT_OK;
                }

                ps_conv->coeffs[i] = atof(p_token_1);
            }

        /* ende module */    
        } else if(  strcmp(p_token_1,"/end") == 0 
                 && strcmp(p_token_2,"COMPU_METHOD") == 0
                 )  { 

            read_next_token();

            break;
        }
    }


    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_dummy_end(char *p_key_word) {

    /* Suche  ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( read_next_token() != OK ) {

            AASAP2_PRINT("\nERROR in <aasap2_c::read_dummy_end>: no more token, no /end %s found",p_key_word);
            AASAP2_PRINT("\n      File: %s, Line: %i",file_name,fid_izeile);
            return NOT_OK;
        }
        /* ende keyword */    
        if(  strcmp(p_token_1,"/end") == 0 
          && strcmp(p_token_2,p_key_word) == 0
          )  { 

            read_next_token();

            break;
        }
    }
    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_c::read_next_token(void) {

    size_t i;


#if AASAP2_DEBUG

    if( !start_done_flag )
        out_fid = fopen("aasap2.log","w");
#endif


    p_token_1 = token1; 
    p_token_2 = token2; 

    /* Prüfen, ob buffer neu gelesen werden soll */
    while( i_index >= n_index ) { /* buffer neu einlesen */

        int chr1=1,chr2;

        /* index und buffer zurück setzen */
        n_index  = 0;
        i_index  = 0;
        n_zeile  = 0;
        n_buffer = 0;

        /* erstes Zeichen einlesen */
        if( (chr1 = getc(p_fid)) == EOF ) {

            if( !end_done_flag ) {

                strcpy(token1,token2);
                strcpy(token2,"");
                end_done_flag = 1;
#if AASAP2_DEBUG
                fprintf(out_fid,"%3i %s\n",i_index-1,token1);
                fflush(out_fid);
#endif
                return OK;
            } else {
#if AASAP2_DEBUG

        if( out_fid != NULL )
            fclose(out_fid);
#endif

                return NOT_OK;
            }
        }

        while( !(  (read_status == AASAP2_SEARCH_NEXT_TOKEN )  
                && (n_buffer > 65000)
                )
             || ( n_buffer == AASAP2_BUFFER_MAX )
             ) {


#if AASAP2_DEBUG
    if( n_block == 21 ) {
        n_block = n_block;
    }
#endif
            if( chr1 == 0x0A )
                n_zeile++;

            chr2 = getc(p_fid);

            if( chr2 == EOF ) { /* Abbrechen, da Enod of File */

                if(  (read_status == AASAP2_SEARCH_COM_END) ) { /* Es wird kein neuer Token gerade eingelesen */
                        buffer[n_buffer]     = (char)chr1;
                        index_end[n_index++] = n_buffer;

                } else if( read_status == AASAP2_SEARCH_TOKEN_END ) {

                    if(  chr1 == ' ' 
                      || chr1 == 0x09 /* TAB */
                      || chr1 == 0x0D /* CR  */
                      || chr1 == 0x0A /* LF */
                      ) {
                        index_end[n_index++] = n_buffer-1;
                    } else {
                        buffer[n_buffer]     = (char)chr1;
                        index_end[n_index++] = n_buffer;
                    }
                } else { 

                    if( chr1 == '"' ) { /* Qoutende gefunden */

                        index_end[n_index++] = n_buffer-1;
                    } else {

                        buffer[n_buffer]     = (char)chr1;
                        index_end[n_index++] = n_buffer;
                    }                    

                }

                read_status = AASAP2_SEARCH_NEXT_TOKEN;
                break;
            }

            switch( read_status ) {

            case AASAP2_SEARCH_NEXT_TOKEN:


                if( chr1 == '/' && chr2 == '*' ) { /* Kommentar gefunden */

                    read_status = AASAP2_SEARCH_COM_END;

                    chr1 = getc(p_fid);
                    chr2 = getc(p_fid);

                    if( chr1 == '*' && chr2 == '/' ) { /* Kommentarende schon da */
                    
                        read_status = AASAP2_SEARCH_NEXT_TOKEN;

                        chr2 = getc(p_fid);
                        
                    } 

                } else if( chr1 == '"' ) { /* Quottext gefunden */

                    read_status = AASAP2_SEARCH_QUOT_END;

                    if( chr2 == '"' ) { /* Qoutende gefunden, kein Text enthalten */

                        read_status = AASAP2_SEARCH_NEXT_TOKEN;

                        buffer[n_buffer]     = ' ';
                        index_start[n_index] = n_buffer;
                        izeile[n_index]      = n_zeile;
                        index_end[n_index++] = n_buffer++;

                    } else {
                        
                        buffer[n_buffer]     = (char)chr2;
                        index_start[n_index] = n_buffer++;
                        izeile[n_index]      = n_zeile;
                    }
                    chr2 = getc(p_fid);


                } else if(  chr1 != ' ' 
                         && chr1 != 0x09 /* TAB */
                         && chr1 != 0x0D /* CR  */
                         && chr1 != 0x0A /* LF */
                         ) { /* Token gefunden */

                    read_status = AASAP2_SEARCH_TOKEN_END;

                    buffer[n_buffer]     = (char)chr1;
                    index_start[n_index] = n_buffer++;
                    izeile[n_index]      = n_zeile;
                    

                }
                break;

            case AASAP2_SEARCH_COM_END:

                if( chr1 == '*' && chr2 == '/' ) { /* Kommentarende da */

                    read_status = AASAP2_SEARCH_NEXT_TOKEN;

                    chr2 = getc(p_fid);

                }

                break;
            case AASAP2_SEARCH_QUOT_END:
                
                if( chr1 == '"' ) { /* Qoutende gefunden */

                    read_status = AASAP2_SEARCH_NEXT_TOKEN;

                    index_end[n_index++] = n_buffer-1;
                } else {


                    buffer[n_buffer++]     = (char)chr1;
                    

                }


                break;
            case AASAP2_SEARCH_TOKEN_END:
                
                if(  chr1 == ' ' 
                  || chr1 == 0x09 /* TAB */
                  || chr1 == 0x0D /* CR  */
                  || chr1 == 0x0A /* LF */
                  ) { /* Tokenende gefunden */

                    read_status = AASAP2_SEARCH_NEXT_TOKEN;

                    index_end[n_index++] = n_buffer-1;
                } else {

                    buffer[n_buffer++]     = (char)chr1;

                }
            }

            chr1 = chr2;
        } /* end while */

        if( chr2 != EOF ) { /* chr2 zurückgeben, wenn Fileende noch nicht erreicht */

            ungetc(chr2,p_fid);
        }
#if AASAP2_DEBUG

        fprintf(out_fid,"n_block: %3i ================================================\n",n_block++);
#endif
        
    }

    if( !start_done_flag ) {

        
        for(i=0;i<MIN(index_end[i_index]-index_start[i_index]+1,AASAP2_TOKEN_MAX);i++) 
            token2[i] = buffer[index_start[i_index]+i]; 
        token2[i] = '\0';

        act_zeile = izeile[i_index]; 

        i_index++;
        start_done_flag = 1;

    }

    strcpy(token1,token2);
    fid_izeile = act_zeile; 


#if AASAP2_DEBUG
    fprintf(out_fid,"%5i iz: %5i %s  \n",i_index-1,fid_izeile,token1);
    fflush(out_fid);
#endif

#if AASAP2_DEBUG
    if( strcmp("ABSCYCL_FRc",token1) == 0 ) {
        printf("\n\n n_block = %i",n_block);
        printf("\n i_index = %i",i_index);
        printf("\n index_start[i_index] = %i",index_start[i_index]);
        printf("\n index_end[i_index] = %i",index_end[i_index]);
    }
#endif

    for(i=0;i<MIN(index_end[i_index]-index_start[i_index]+1,AASAP2_TOKEN_MAX);i++) 
        token2[i] = buffer[index_start[i_index]+i]; 
    token2[i] = '\0';

    act_zeile = izeile[i_index]; 
    i_index++;



    return OK;
}
