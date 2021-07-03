/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 1.0      19.08.04   TBert aus simSys                                */
/* Version  Datum      Wer   Was                                       */
/* Aenderungen:                                                        */
/************************************************************************
* File:             aasap2.h        
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

------------------------------------------------------------------------
- interne Struktur anlegen 
------------------------------------------------------------------------
aasap2_s *aasap2_new(void);

-------------------------------------------------------------------------
- gesamte Struktur löschen                                                      -
-------------------------------------------------------------------------
void aasap2_delete(aasap_s *p);

-------------------------------------------------------------------------
- datei zum Lesen öffnet                                                -
-------------------------------------------------------------------------
STATUS_T aasap2_read(aasap2_s *p,CHAR *input_file);

  Rückgabewert == 0 , wenn okay
               != 0 , wenn nicht okay

  Fehlertext mit funktion char *amsg_get_msg() holen


-------------------------------------------------------------------------
- gibt externe Struktur zum weiterverarbeiten zurück                                                -
-------------------------------------------------------------------------
asap2_pro_s aasap2_get_list(aasap2_s *p);

  Rückgabewert == 0 , wenn okay
               != 0 , wenn nicht okay

  Fehlertext mit funktion char *amsg_get_msg() holen


************************************************************************/
#ifndef aasap2_h_included

#define aasap2_h_included

//#define STL_USING_STRING
//#include "stl.h"

#include "definer.h"

//***********************************************************************
// Defines
//***********************************************************************

// Ausgabe an Bildschirm bestimmen

#define AASAP2_PRINT_TYPE_TB     0
#define AASAP2_PRINT_TYPE_WAVE   1
#define AASAP2_PRINT_TYPE        AASAP2_PRINT_TYPE_TB

// Debug-Ausgabe in aasap2.log

#define AASAP2_DEBUG                0

// Define for type of characteristic

#define AASAP2_DEF_TYPE_VALUE       0
#define AASAP2_DEF_TYPE_CURVE       1
#define AASAP2_DEF_TYPE_MAP         2
#define AASAP2_DEF_TYPE_CUBOID      3
#define AASAP2_DEF_TYPE_VAL_BLK     4
#define AASAP2_DEF_TYPE_ASCII       5
#define AASAP2_DEF_TYPE_NON         6

// Define for conversion type

#define AASAP2_CONV_TYPE_TAB_INTP   0
#define AASAP2_CONV_TYPE_TAB_NOINTP 1
#define AASAP2_CONV_TYPE_TAB_VERB   2
#define AASAP2_CONV_TYPE_RAT_FUNC   3
#define AASAP2_CONV_TYPE_FORM       4
#define AASAP2_CONV_TYPE_NON        5

// Define for value type

#define AASAP2_DEF_VAL_UBYTE        0
#define AASAP2_DEF_VAL_UWORD        1
#define AASAP2_DEF_VAL_ULONG        2
#define AASAP2_DEF_VAL_SBYTE        3
#define AASAP2_DEF_VAL_SWORD        4
#define AASAP2_DEF_VAL_SLONG        5
#define AASAP2_DEF_VAL_FLOAT32_IEEE 6

// Depositnamen mit den entspr. val-types

#define AASAP2_DEPOSIT_UBYTE        "__UBYTE"
#define AASAP2_DEPOSIT_UWORD        "__UWORD"
#define AASAP2_DEPOSIT_ULONG        "__ULONG"
#define AASAP2_DEPOSIT_SBYTE        "__SBYTE"
#define AASAP2_DEPOSIT_SWORD        "__SWORD"
#define AASAP2_DEPOSIT_SLONG        "__SLONG"
#define AASAP2_DEPOSIT_FLOAT32_IEEE "__FLOAT32_IEEE"

// character-vector-length

#define AASAP2_MAX_NAME            20
#define AASAP2_MAX_COMMENT        120
#define AASAP2_MAX_FORMAT          30

// interne Größen


#define AASAP2_BUFFER_MAX        65535
#define AASAP2_INDEX_MAX         65535
#define AASAP2_TOKEN_MAX         AASAP2_MAX_COMMENT

#define AASAP2_SEARCH_NEXT_TOKEN 0
#define AASAP2_SEARCH_COM_END    1
#define AASAP2_SEARCH_QUOT_END   2
#define AASAP2_SEARCH_TOKEN_END  3


#if AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_TB

    #define AASAP2_PRINT amsg_set_status
#elif AASAP2_PRINT_TYPE == AASAP2_PRINT_TYPE_WAVE

    #define AASAP2_PRINT printf
#else

    #error Wrong AASAP2_PRINT_TYPE
#endif



//***********************************************************************
// struct
//***********************************************************************

// Output-Val-Structure
typedef
struct tag_aasap2_val_s {
    char                    *p_name;
    char                    *p_comment;
    UINT8_T                 char_type;
    UINT8_T                 val_type;
    UINT32_T                asap1b_adress;
    UINT32_T                bit_mask;
    char                    *p_format;
    double                  *p_coeffs;
    char                    *p_unit;
    struct tag_aasap2_val_s *ps_next;
} aasap2_val_s;

// Input-Conversion (COMPU_METHOD) -Structure
typedef
struct tag_aasap2_conversion_s {
    char                    name[AASAP2_MAX_NAME+1];
    char                    comment[AASAP2_MAX_COMMENT+1];
    UINT8_T                 type;
    char                    format[AASAP2_MAX_FORMAT+1];
    char                    unit[AASAP2_MAX_FORMAT+1];
    double                  coeffs[6];
    char                    formula[AASAP2_MAX_COMMENT+1];
    char                    compu_tab[AASAP2_MAX_NAME+1];
    struct tag_aasap2_conversion_s *ps_next;

}aasap2_conversion_s;

// Input-Characteristic -Structure
typedef
struct tag_aasap2_characteristic_s {
    char                    name[AASAP2_MAX_NAME+1];
    char                    comment[AASAP2_MAX_COMMENT+1];
    UINT8_T                 type;
    UINT32_T                address;
    char                    deposit[AASAP2_MAX_NAME+1];
    DOUBLE                  maxdiff;
    char                    conversion[AASAP2_MAX_NAME+1];
    DOUBLE                  lower_limit;
    DOUBLE                  upper_limit;

    UINT32_T                asap1b_address;

    UINT32_T                bit_mask;
    char                    format[AASAP2_MAX_NAME+1];
    struct tag_aasap2_characteristic_s *ps_next;

}aasap2_characteristic_s;

// Input-Projectheader -Structure
typedef
struct tag_aasap2_projectheader_s {
    char                    comment[AASAP2_MAX_COMMENT+1];        // (astrs_s) comment
    char                    version[AASAP2_MAX_COMMENT+1];        // (astrs_s) version
    char                    project_no[AASAP2_MAX_COMMENT+1];     // (astrs_s) ps_project_no
}aasap2_projectheader_s;

// Input-Modul -Structure
typedef
struct tag_aasap2_modul_s {
    char                    name[AASAP2_MAX_NAME+1];
    char                    comment[AASAP2_MAX_COMMENT+1];
    aasap2_characteristic_s *ps_characteristic;
    UINT16_T                n_characteristic;
    aasap2_conversion_s     *ps_conversion;
    struct tag_aasap2_modul_s *ps_next;
}aasap2_modul_s;

// Input-Project -Structure
typedef
struct tag_aasap2_project_s {
    char                    name[AASAP2_MAX_NAME+1];
    char                    comment[AASAP2_MAX_COMMENT+1];
    aasap2_projectheader_s  *ps_header;
    aasap2_modul_s          *ps_modul;
}aasap2_project_s;


typedef
struct tag_aasap2_s {
    aasap2_project_s    *ps_project;   // Projekt asap2
    aasap2_val_s        *ps_val;       // List with all Values

}aasap2_s;


//***********************************************************************
// struct
//***********************************************************************
class aasap2_c {

public:

    /* aasap2.cpp */
    aasap2_c(void);
    ~aasap2_c(void);

    /* aasap2_read.cpp: Read input file and set project structure */
    STATUS_T     read(CHAR *input_file);
    const char         *get_file_name(void);

    /* aasap2_val: set val-structure from project-structure */
    STATUS_T     get_val(aasap2_val_s **pps_val);

protected:

    char *          file_name;

    aasap2_project_s    *ps_project;           // project-structure
    aasap2_val_s        *ps_val;               // Val-structure

    FILE                *p_fid;                // FILE-Structure;
    UINT16_T            fid_izeile;             // Zeilennummer beginnt bei 0

    char                *p_token_1;            // pointer to first token
    char                *p_token_2;            // pointer to second token

    
    char buffer[AASAP2_BUFFER_MAX];
    size_t n_buffer;

    size_t index_start[AASAP2_INDEX_MAX];
    size_t index_end[AASAP2_INDEX_MAX];
    size_t izeile[AASAP2_INDEX_MAX];
    size_t n_zeile;
    size_t n_index;
    size_t i_index;

    char token1[AASAP2_TOKEN_MAX+1];
    char token2[AASAP2_TOKEN_MAX+1];

    char read_status;

    char start_done_flag;
    char end_done_flag;

    size_t act_zeile;

#if AASAP2_DEBUG

    FILE *out_fid;
    size_t n_block;
#endif

    /* aasap2.ccp */
    void delete_val(aasap2_val_s *ps_val);
    void delete_project(aasap2_project_s *ps_project);
    void delete_projectheader(aasap2_projectheader_s *ps_header);
    void delete_modul(aasap2_modul_s *ps_modul);
    void delete_characteristic(aasap2_characteristic_s *ps_char);
    void delete_conversion(aasap2_conversion_s *ps_char);

    /* aasap2_read.ccp */
    STATUS_T read_start(void);
    STATUS_T read_project(void);
    STATUS_T read_projectheader(aasap2_projectheader_s *ps_header );
    STATUS_T read_modul(aasap2_modul_s *ps_modul );
    STATUS_T read_characteristic(aasap2_characteristic_s *ps_charac );
    STATUS_T read_conversion(aasap2_conversion_s *ps_conv );
    STATUS_T read_dummy_end(char *p_key_word);
    STATUS_T read_next_token(void);

    /* aasap2_val.cpp */
    STATUS_T get_val_modul(aasap2_modul_s *ps_modul,aasap2_val_s **pps_val);
    STATUS_T get_val_characteristic(aasap2_characteristic_s *ps_charac,aasap2_conversion_s *ps_conv,
                                    aasap2_val_s **pps_val);
    void     print_characteristic(aasap2_characteristic_s *ps_charac);


};

#endif