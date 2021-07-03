/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_delete_characteristic(aasap2_characteristic_s *ps_char) {

    aasap2_characteristic_s *ps;

    while( ps_char != NULL ) {

        ps = ps_char->ps_next;

        amem_free_g((void **)&ps_char);

        ps_char = ps;
    }

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_delete_modul(aasap2_modul_s *ps_modul) {

    aasap2_modul_s *ps;

    while( ps_modul != NULL ) {

        if( ps_modul->ps_characteristic != NULL )
            aasap2_delete_characteristic(ps_modul->ps_characteristic);

        ps = ps_modul->ps_next;

        amem_free_g((void **)&ps_modul);

        ps_modul = ps;

    }

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_delete_projectheader(aasap2_projectheader_s *ps_header) {

    amem_free_g((void **)&ps_header);

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_delete_project(aasap2_project_s *ps_project) {


    if( ps_project->ps_modul != NULL ) 
             aasap2_delete_modul(ps_project->ps_modul);

    if( ps_project->ps_header != NULL )
        aasap2_delete_projectheader(ps_project->ps_header);

    amem_free_g((void **)&ps_project);

    return;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
void aasap2_delete_val(aasap2_val_s *ps_val) {

    aasap2_val_s *ps;

    while( ps_val != NULL ) {

        ps = ps_val->ps_next;

        amem_free_g((void **)&ps_val);

        ps_val = ps;
    }
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_next_token(void) {

#define AASAP2_BUFFER_MAX        65535
#define AASAP2_INDEX_MAX         65535
#define AASAP2_TOKEN_MAX         AASAP2_MAX_COMMENT

#define AASAP2_SEARCH_NEXT_TOKEN 0
#define AASAP2_SEARCH_COM_END    1
#define AASAP2_SEARCH_QUOT_END   2
#define AASAP2_SEARCH_TOKEN_END  3



    static char buffer[AASAP2_BUFFER_MAX];
    static size_t n_buffer = 0;

    static size_t index_start[AASAP2_INDEX_MAX];
    static size_t index_end[AASAP2_INDEX_MAX];
    static size_t izeile[AASAP2_INDEX_MAX];
    static size_t n_zeile = 0;
    static size_t n_index = 0;
    static size_t i_index = 0;

    static char token1[AASAP2_TOKEN_MAX+1];
    static char token2[AASAP2_TOKEN_MAX+1];

    static char read_status = AASAP2_SEARCH_NEXT_TOKEN;

    static char start_done_flag = 0;
    static char end_done_flag   = 0;

    static size_t act_zeile;

#if AASAP2_DEBUG

    static FILE *out_fid = NULL;
    static size_t n_block = 0;
#endif

    size_t i;


#if AASAP2_DEBUG

    if( !start_done_flag )
        out_fid = fopen("aasap2.log","w");
#endif


    p_token_aasap2_1 = token1; 
    p_token_aasap2_2 = token2; 

    /* Prüfen, ob buffer neu gelesen werden soll */
    while( i_index >= n_index ) { /* buffer neu einlesen */

        int chr1=1,chr2;

        /* index und buffer zurück setzen */
        n_index  = 0;
        i_index  = 0;
        n_zeile  = 0;
        n_buffer = 0;

        /* erstes Zeichen einlesen */
        if( (chr1 = getc(ps_file_aasap2->p_fid)) == EOF ) {

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


            if( chr1 == 0x0A )
                n_zeile++;

            chr2 = getc(ps_file_aasap2->p_fid);

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

                    chr1 = getc(ps_file_aasap2->p_fid);
                    chr2 = getc(ps_file_aasap2->p_fid);

                    if( chr1 == '*' && chr2 == '/' ) { /* Kommentarende schon da */
                    
                        read_status = AASAP2_SEARCH_NEXT_TOKEN;

                        chr2 = getc(ps_file_aasap2->p_fid);
                        
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
                    chr2 = getc(ps_file_aasap2->p_fid);


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

                    chr2 = getc(ps_file_aasap2->p_fid);

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
#if AASAP2_DEBUG

        fprintf(out_fid,"%3i ================================================\n",n_block++);
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
    ps_file_aasap2->izeile = act_zeile; 


#if AASAP2_DEBUG
    fprintf(out_fid,"%5i iz: %5i %s  \n",i_index-1,ps_file_aasap2->izeile,token1);
    fflush(out_fid);
#endif

    for(i=0;i<MIN(index_end[i_index]-index_start[i_index]+1,AASAP2_TOKEN_MAX);i++) 
        token2[i] = buffer[index_start[i_index]+i]; 
    token2[i] = '\0';

    act_zeile = izeile[i_index]; 
    i_index++;



    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_dummy_end(char *p_key_word) {

    /* Suche  ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
            amsg_set_status("ERROR in <aasap2_read_dummy_end>: no more token, no /end %s found",p_key_word);
            amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
            return NOT_OK;
        }
        /* ende keyword */    
        if(  strcmp(p_token_aasap2_1,"/end") == 0 
          && strcmp(p_token_aasap2_2,p_key_word) == 0
          )  { 

            aasap2_read_next_token();

            break;
        }
    }
    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_characteristic(aasap2_characteristic_s *ps_charac ) {

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
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: Characteristic name not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->name,AASAP2_MAX_NAME);
    //i = MIN(strlen(p_token_aasap2_1),AASAP2_MAX_NAME+1);
    //strncpy(ps_charac->name,p_token_aasap2_1,i);
    /* Comment einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHACRECTERISTIC comment not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->comment,AASAP2_MAX_COMMENT);
    
    /* Type einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHARACTERISTIC TYPE not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }

    if( strcmp(p_token_aasap2_1,"VALUE") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_VALUE;
    else if( strcmp(p_token_aasap2_1,"CURVE") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_CURVE;
    else if( strcmp(p_token_aasap2_1,"MAP") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_MAP;
    else if( strcmp(p_token_aasap2_1,"CUBOID") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_CUBOID;
    else if( strcmp(p_token_aasap2_1,"VAL_BLK") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_VAL_BLK;
    else if( strcmp(p_token_aasap2_1,"ASCII") == 0 ) 
            ps_charac->type = AASAP2_DEF_TYPE_ASCII;
    else            
            ps_charac->type = AASAP2_DEF_TYPE_NON;

    /* Adresse einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHARACTERISTIC address not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }

    sscanf( p_token_aasap2_1, "%lx", &ps_charac->address );

    /* Deposit einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHACRECTERISTIC deposit not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->deposit,AASAP2_MAX_NAME);

    /* Maxdiff einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHARACTERISTIC maxdiff not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }

    ps_charac->maxdiff = atof(p_token_aasap2_1);

    /* Conversion einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHACRECTERISTIC conversion not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_charac->conversion,AASAP2_MAX_NAME);

    /* lower_limit einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHARACTERISTIC lower_limit not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }

    ps_charac->lower_limit = atof(p_token_aasap2_1);

    /* upper_limit einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_characteristic>: CHARACTERISTIC upper_limit not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }

    ps_charac->upper_limit = atof(p_token_aasap2_1);
    
    /* Suche weiteren Blöcken und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
            amsg_set_status("ERROR in <aasap2_read_characteristic>: no more token, no /end CHARACTERISTIC found");
            amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
            return NOT_OK;
        }
    
        /* Suche alle nicht definierten keywords */
        for(i=0;i<n_key_words;i++) {

            /* begin keyword */
            if(  strcmp(p_token_aasap2_1,"/begin") == 0 
              && strcmp(p_token_aasap2_2,key_words[i]) == 0
              )  { 

                aasap2_read_next_token();


                /* Dummy mit /end einlesen */
                if( aasap2_read_dummy_end(key_words[i]) != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                    amsg_set_status("ERROR in <aasap2_read_modul> with <aasap2_read_dummy>");
#endif
                    return NOT_OK;
                }
                break;
            }
        }

        /* begin IF_DATA */    
        if(  strcmp(p_token_aasap2_1,"/begin") == 0 
          && strcmp(p_token_aasap2_2,"IF_DATA") == 0
          )  { 


            aasap2_read_next_token();
            
            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_characteristic>: no more token, no IF_DATA name found");
                amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
                return NOT_OK;
            }
            

            /* ASAP1B_ADDRESS Kennzeichnung prüfen */
            if(  strcmp(p_token_aasap2_1,"ASAP1B_ADDRESS") == 0 
              && strcmp(p_token_aasap2_2,"KP_BLOB") == 0
              ) {

                aasap2_read_next_token();
                
                /* Nächster Token einlesen (p_token_aasap2_1) */
                if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                    amsg_set_status("ERROR in <aasap2_read_characteristic>: no more token, no KP_BLOB value found");
                    amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
                    return NOT_OK;
                }

                sscanf( p_token_aasap2_1, "%lx", &ps_charac->asap1b_address );

            } else {

                /* Dummy mit /end einlesen */
                if( aasap2_read_dummy_end("IF_DATA") != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                    amsg_set_status("ERROR in <aasap2_read_modul> with <aasap2_read_dummy>");
#endif
                    return NOT_OK;
                }
            }


        /* bit mask */
        } else if ( strcmp(p_token_aasap2_1,"BIT_MASK") == 0 ) {  

            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_characteristic>: no more token, no BIT_MASK value found");
                amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
                return NOT_OK;
            }

                
             sscanf( p_token_aasap2_1, "%x", &ps_charac->bit_mask );

        /* format */
        } else if ( strcmp(p_token_aasap2_1,"FORMAT") == 0 ) {  

            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_characteristic>: no more token, no FORMAT value found");
                amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
                return NOT_OK;
            }

                
            AASAP2_COPY(ps_charac->format,AASAP2_MAX_NAME);

        /* ende module */    
        } else if(  strcmp(p_token_aasap2_1,"/end") == 0 
                 && strcmp(p_token_aasap2_2,"CHARACTERISTIC") == 0
                 )  { 

            aasap2_read_next_token();

            break;
        }
    }


    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_modul(aasap2_modul_s *ps_modul ) {

    char *key_words[] = {"MOD_PAR","MOD_COMMON","IF_DATA","AXIS_PTS","MEASUREMENT"
                        ,"COMPU_METHOD","COMPU_TAB","COMPU_VTAB","FUNCTION","RECORD_LAYOUT"};
    
    UINT8_T n_key_words = sizeof( key_words) / sizeof(char *);
    UINT8_T i;

    /* Moduldaten einlesen */

    /* Name einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_project>: Modulname not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_modul->name,AASAP2_MAX_NAME);

    /* Comment einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_project>: Modulcomment not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_modul->comment,AASAP2_MAX_COMMENT);
    
    /* Suche weiteren Blöcken und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
            amsg_set_status("ERROR in <aasap2_read_modul>: no more token, no /end MODULE found");
            amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
            return NOT_OK;
        }
    
        /* Suche alle nicht definierten keywords */
        for(i=0;i<n_key_words;i++) {

            /* begin keyword */
            if(  strcmp(p_token_aasap2_1,"/begin") == 0 
              && strcmp(p_token_aasap2_2,key_words[i]) == 0
              )  { 

                aasap2_read_next_token();


                /* Dummy mit /end einlesen */
                if( aasap2_read_dummy_end(key_words[i]) != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                    amsg_set_status("ERROR in <aasap2_read_modul> with <aasap2_read_dummy_end>");
#endif
                    return NOT_OK;
                }
                break;
            }
        }

        /* begin CHARACTERISTIC */    
        if(  strcmp(p_token_aasap2_1,"/begin") == 0 
          && strcmp(p_token_aasap2_2,"CHARACTERISTIC") == 0
          )  { 

            aasap2_characteristic_s *ps_charac=NULL;

            aasap2_read_next_token();
    
            /* Header anlegen */
            if( amem_mem_g((void **)&ps_charac,1,sizeof(aasap2_characteristic_s)) != 0 ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_modul>: no memory for aasap2_characteristic_s *ps_charac");
#endif
                return NOT_OK;
            }

            ps_charac->address        = 0L;
            ps_charac->asap1b_address = 0L;
            ps_charac->bit_mask       = 0;
            ps_charac->lower_limit    = MIN_DOUBLE;
            ps_charac->maxdiff        = 0.;
            ps_charac->type           = AASAP2_DEF_TYPE_NON;
            ps_charac->upper_limit    = MAX_DOUBLE;
            ps_charac->ps_next        = NULL;

            /* CHARACTERISTIC einlesen */
            if( aasap2_read_characteristic(ps_charac) != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_modul> with <aasap2_read_characteristic>");
#endif
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

        /* ende module */    
        } else if(  strcmp(p_token_aasap2_1,"/end") == 0 
                 && strcmp(p_token_aasap2_2,"MODULE") == 0
                 )  { 

            aasap2_read_next_token();

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_projectheader(aasap2_projectheader_s *ps_header ) {

    /* Header Comment einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_projectheader>: Hedear comment not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_header->comment,AASAP2_MAX_COMMENT);

    /* Suche VERSION, PROJECT_NO und ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
            amsg_set_status("ERROR in <aasap2_read_projectheader>: no more token, no /end HEADER found");
            amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
            return NOT_OK;
        }
    
        /* VERSION */
        if(  strcmp(p_token_aasap2_1,"VERSION") == 0 
          )  { 

            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_projectheader>: no more token, no VERION value found");
                amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
                return NOT_OK;
            }

            AASAP2_COPY(ps_header->version,AASAP2_MAX_COMMENT);

        /* PROJECT_NO */
        } else if(  strcmp(p_token_aasap2_1,"PROJECT_NO") == 0 
          )  { 

            /* Nächster Token einlesen (p_token_aasap2_1) */
            if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_projectheader>: no more token, no PROJECT_NO value found");
                amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
                return NOT_OK;
            }

            AASAP2_COPY(ps_header->project_no,AASAP2_MAX_COMMENT);


        /* ende header */    
        } else if(  strcmp(p_token_aasap2_1,"/end") == 0 
                 && strcmp(p_token_aasap2_2,"HEADER") == 0
                 )  { 

            aasap2_read_next_token();

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_project(aasap2_project_s *ps_project ) {

    BOOL_T header_read_flag = FALSE;

    /* Projectdaten einlesen */

    /* Name einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_project>: Projectname not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_project->name,AASAP2_MAX_NAME);

    /* Comment einlesen */
    if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_read_project>: Projectcomment not found");
        amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
        return NOT_OK;
    }
    AASAP2_COPY(ps_project->comment,AASAP2_MAX_COMMENT);
    
    /* Suche MODULE oder HEADER oder ende */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
            amsg_set_status("ERROR in <aasap2_read_project>: no more token found in File, no /end PROJECT found");
            amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
            return NOT_OK;
        }
    
        /* begin MODULE */
        if(  strcmp(p_token_aasap2_1,"/begin") == 0 
          && strcmp(p_token_aasap2_2,"MODULE") == 0
          )  { 

            aasap2_modul_s *ps_modul=NULL;

            aasap2_read_next_token();

            /* Modul anlegen */
            if( amem_mem_g((void **)&ps_modul,1,sizeof(aasap2_modul_s)) != 0 ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_project>: no memory for aasap2_modul_s *ps_modul");
#endif
                return NOT_OK;
            }

            /* Modul initialisieren */
            ps_modul->ps_characteristic  = NULL;
            ps_modul->n_characteristic   = 0;
            ps_modul->ps_next            = NULL;

            /* Modul einlesen */
            if( aasap2_read_modul(ps_modul) != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_project> with <aasap2_read_modul>");
#endif
                return NOT_OK;
            }

            if( ps_project->ps_modul != NULL ) 
                ps_modul->ps_next = ps_project->ps_modul;
            
            ps_project->ps_modul = ps_modul;

        /* begin HEADER */    
        } else if(  strcmp(p_token_aasap2_1,"/begin") == 0 
                 && strcmp(p_token_aasap2_2,"HEADER") == 0
                 && header_read_flag == FALSE
                 )  { 

            aasap2_projectheader_s *ps_header=NULL;

            aasap2_read_next_token();
    
            /* Header anlegen */
            if( amem_mem_g((void **)&ps_header,1,sizeof(aasap2_projectheader_s)) != 0 ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_project>: no memory for aasap2_header_s *ps_header");
#endif
                return NOT_OK;
            }

            ps_project->ps_header = ps_header;

            /* Header einlesen */
            if( aasap2_read_projectheader(ps_header) != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read_project> with <aasap2_read_modul>");
#endif
                return NOT_OK;
            }


            header_read_flag = TRUE;

        /* ende project */    
        } else if(  strcmp(p_token_aasap2_1,"/end") == 0 
                 && strcmp(p_token_aasap2_2,"PROJECT") == 0
                 )  { 

            aasap2_read_next_token();

            break;
        }
    }

    return OK;
}
/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_read_start(aasap2_s *p) {

    aasap2_project_s    *ps_project = NULL;

    /* Suche Projektbeginn */
    while(1) {
        
        /* Nächster Token einlesen (p_token_aasap2_1) */
        if( aasap2_read_next_token() != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
            amsg_set_status("ERROR in <aasap2_read_start>: no more token, no project found");
            amsg_set_status("      File: %s, Line: %i",astrs_string(ps_file_aasap2->ps_name),ps_file_aasap2->izeile);
#endif
            return NOT_OK;
        }
    
        if(  strcmp(p_token_aasap2_1,"/begin") == 0 
          && strcmp(p_token_aasap2_2,"PROJECT") == 0
          )  { /* Beginn Projekt */

            aasap2_read_next_token();

            /* Projekt-Struktur initialisieren */
            if( amem_mem_g((void **)&ps_project,1,sizeof(aasap2_project_s)) != 0 ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read>: no memory for aasap2_project_s p->ps_project");
#endif
                return NOT_OK;
            }
            ps_project->ps_modul   = NULL;
            ps_project->ps_header  = NULL;

            p->ps_project = ps_project;

            if( aasap2_read_project(ps_project) != OK ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR in <aasap2_read> with <aasap2_read_project>");
#endif
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
STATUS_T aasap2_val_characteristic(aasap2_characteristic_s *ps_charac, aasap2_s *ps) {

    aasap2_val_s *ps_v = NULL;


   /* Value-Struct anlegen */
   if( amem_mem_g((void **)&ps_v,1,sizeof(aasap2_val_s)) != 0 ) {
    
#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
        amsg_set_status("ERROR in <aasap2_val_characteristic>: no memory for aasap2_val_s *ps_v");
#endif
        return NOT_OK;
   }

   /* Werte übergeben */
   ps_v->p_name         = ps_charac->name;
   ps_v->p_comment      = ps_charac->comment;
   ps_v->type           = ps_charac->type;
   ps_v->asap1b_adress  = ps_charac->asap1b_address;
   ps_v->bit_mask       = ps_charac->bit_mask;
   ps_v->p_format       = ps_charac->format;

   
   if( ps->ps_val != NULL ) 
       ps_v->ps_next = ps->ps_val;
   else
       ps_v->ps_next = NULL;
    
   ps->ps_val = ps_v;

   return OK;
}

/*======================================================================================================*/
/*======================================================================================================*/
/*======================================================================================================*/
STATUS_T aasap2_val_modul(aasap2_modul_s *ps_modul,aasap2_s *ps) {

    aasap2_characteristic_s *ps_charac = NULL;

    ps_charac = ps_modul->ps_characteristic;

    while( ps_charac != NULL ) {


        if( aasap2_val_characteristic(ps_charac,ps) != OK ) {

#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB
                amsg_set_status("ERROR with <aasap2_val_characteristic> in <aasap2_val_modul>");
#endif
                return NOT_OK;
        }

        ps_charac = ps_charac->ps_next;
    }
    return OK;
}

