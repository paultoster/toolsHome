// SlfParDef.h
// Defs Parameter

#ifndef SLF_PAR_DEF_INCLUDED
#define SLF_PAR_DEF_INCLUDED

#define SLF_PAR_READ_STRING_END_LINE       "\n"
#define SLF_PAR_READ_STRING_SPACE          " "
#define SLF_PAR_READ_STRING_TAB            "\t"
#define SLF_PAR_READ_STRING_START_INSTANCE "<"
#define SLF_PAR_READ_STRING_END_INSTANCE   ">"
#define SLF_PAR_READ_STRING_START_GROUP    "["
#define SLF_PAR_READ_STRING_END_GROUP      "]"
#define SLF_PAR_READ_STRING_START_SUBGROUP "("
#define SLF_PAR_READ_STRING_END_SUBGROUP   ")"
#define SLF_PAR_READ_STRING_COMMENT1       "!"
#define SLF_PAR_READ_STRING_COMMENT2       ";"
#define SLF_PAR_READ_STRING_COMMENT3       "$"
#define SLF_PAR_READ_STRING_ATRIBUTE_DELIM "."
#define SLF_PAR_READ_STRING_EQUAL          "="
#define SLF_PAR_READ_STRING_QUOT           "\""
#define SLF_PAR_READ_STRING_START_UNIT     "["
#define SLF_PAR_READ_STRING_END_UNIT       "]"
#define SLF_PAR_READ_STRING_START_VAL      "["
#define SLF_PAR_READ_STRING_END_VAL        "]"
#define SLF_PAR_READ_STRING_VAL_DELIM_ROW  ";"
#define SLF_PAR_READ_STRING_VAL_DELIM_COL  ","

#define SLF_PAR_READ_STRING_START_TABLE0   "{"
#define SLF_PAR_READ_STRING_START_TABLE1   "{"
#define SLF_PAR_READ_STRING_START_TABLE2   "{"
#define SLF_PAR_READ_STRING_END_TABLE0     "}"
#define SLF_PAR_READ_STRING_END_TABLE1     "}"
#define SLF_PAR_READ_STRING_END_TABLE2     "}"

#define SLF_PAR_READ_END_LINE              '\n'
#define SLF_PAR_READ_SPACE                 ' '
#define SLF_PAR_READ_TAB                   '\t'
#define SLF_PAR_READ_START_INSTANCE        '<'
#define SLF_PAR_READ_END_INSTANCE          '>'
#define SLF_PAR_READ_START_GROUP           '['
#define SLF_PAR_READ_END_GROUP             ']'
#define SLF_PAR_READ_START_SUBGROUP        '('
#define SLF_PAR_READ_END_SUBGROUP          ')'
#define SLF_PAR_READ_COMMENT1              '!'
#define SLF_PAR_READ_COMMENT2              ';'
#define SLF_PAR_READ_COMMENT3              '$'
#define SLF_PAR_READ_ATRIBUTE_DELIM        '.'
#define SLF_PAR_READ_EQUAL                 '='
#define SLF_PAR_READ_QUOT                  '"'
#define SLF_PAR_READ_START_UNIT            '['
#define SLF_PAR_READ_END_UNIT              ']'
#define SLF_PAR_READ_START_VAL             '['
#define SLF_PAR_READ_END_VAL               ']'

#define SLF_PAR_READ_START_TABLE0          '{'
#define SLF_PAR_READ_START_TABLE1          '{'
#define SLF_PAR_READ_START_TABLE2          '{'
#define SLF_PAR_READ_END_TABLE0            '}'
#define SLF_PAR_READ_END_TABLE1            '}'
#define SLF_PAR_READ_END_TABLE2            '}'

#define SLF_PAR_READ_ATRIBUTE_COMMENT      "comment"
#define SLF_PAR_READ_ATRIBUTE_COPY         "copy"
#define SLF_PAR_READ_ATRIBUTE_OFFSET       "offset"
#define SLF_PAR_READ_ATRIBUTE_FACTOR       "factor"
#define SLF_PAR_READ_ATRIBUTE_UNIT         "unit"

enum ESlfParType {
  PAR_VOID,            // nicht belegt
  PAR_D64,             // Pointer eines Einzelwerts type (d64_t),
  PAR_F32,             //                           type (f32_t),
  PAR_SINT64,             // ...
  PAR_UINT64,
  PAR_SINT32,        
  PAR_UINT32,
  PAR_SINT16,
  PAR_UINT16,
  PAR_SINT8,
  PAR_UINT8,
  PAR_CStr,            // Einzelwert String-Klasse slf::CStr
  PAR_PTR_VEC,         // Pointer eines Vectors, der noch nicht definiert ist,
                       // Länge wird von Parametereingabe bestimmt ist
  PAR_PTR_MAT,         // Pointer einer Matrix_t, der noch nicht definiert ist
                       // Länge wird von Parametereingabe bestimmt ist

  PAR_VEC,             // Vektor-Struktur Vector_t Länge ist festgelegt
  PAR_MAT,             // Matrix_t-Struktur Matrix_t Länge ist festgelegt
  PAR_ARR_D64,         // Array mit festgelegter Länge vondouble-Werten, 
  PAR_ARR_F32,         // Länge muß übergerdneter Struct definiert sein
  PAR_ARR_SINT64,      // ...
  PAR_ARR_UINT64,
  PAR_ARR_SINT32,        
  PAR_ARR_UINT32,
  PAR_ARR_SINT16,
  PAR_ARR_UINT16,
  PAR_ARR_SINT8,
  PAR_ARR_UINT8,
  PAR_ARR_CStr,     // Array String-Klasse slf::CStr
  PAR_CHAR_STRING,     // String aus charater mit '\0' beendet z.B. matlab eingabe


  PAR_1D_TAB,          // einfache Tabellen-Struktur SSfl1DTab y = f(x)
  PAR_2D_TAB,          // zweidim. Tabellenstruktur  SSfl2DTab z = f(x,y)
};
extern char *SlfParTypeStr[];

// Trennzeichen für Gruppenhierachie
#define PAR_GROUP_DELIM   "."

#define PAR_INCLUDE       "#include"

// Anzahl Zeich Kommentar pro Zeile
#define N_CHAR_PER_LINE 70


//#ifdef __cplusplus
//// Basismodell 
//class CSlfParBase {
//public:
//
//  inline CSlfParBase() {Status=OKAY;}
//  virtual ~CSlfParBase() { }
//
//  uint32_t getLenErrText(void)                                {return ErrText.getLen();}
//  char * getErrText(void)                                     {return ErrText.c_str();}
//  void   resetErrText(void)                                   {ErrText.clear();}
//  bool     isStatusOkay(void)                                 {if(Status==OKAY)return true;else return false;}
//  bool     isStatusNotOkay(void)                              {if(Status!=OKAY)return true;else return false;}
//  status_t getStatus(void)                                    {return Status;}
//
//  uint32_t getLenLogText(void)                                  {return LogText.getLen();}
//  char * getLogText(void)                                     {return LogText.c_str();}
//  void   resetLogText(void)                                   {LogText.clear();}
//
//
//protected:
//
//  status_t        Status;
//  slf::CStr         ErrText;
//  slf::CStr         LogText;
//};
//#endif

#endif