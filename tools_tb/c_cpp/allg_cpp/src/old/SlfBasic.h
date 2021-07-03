// Grundsatzzuordnungen
//
#ifndef SFL_BASIC_H_INCLUDED
#define SFL_BASIC_H_INCLUDED

#include <stdint.h>
#include <float.h>
#include <math.h>

//globaler DEBUG-Flag
#ifndef SLFBASIC_DEBUG_FLAG
	#define SLFBASIC_DEBUG_FLAG
#endif
// Status-Zuordnung
#ifndef OKAY
	#define OKAY      (0)
#endif
#ifndef OK
	#define OK        (0)
#endif
#ifndef NOT_OKAY
	#define NOT_OKAY  (1)
#endif
#ifndef NOT_OK
	#define NOT_OK    (1)
#endif
#ifndef CANCEL
	#define CANCEL    (2)
#endif
#ifndef TRUE
	#define TRUE 1
	#define FALSE 0
#endif
#ifndef NULL
	#define NULL 0
#endif

#ifndef STATUS_INIT
	#define STATUS_INIT  (0)
#endif
#ifndef STATUS_FIRST
	#define STATUS_FIRST (1)
#endif
#ifndef STATUS_LOOP
	#define STATUS_LOOP  (2)
#endif
#ifndef STATUS_DONE
	#define STATUS_DONE  (3)
#endif

// Error-Zuordnung
//#define NO_ERROR                      (0)
#ifndef NO_ERR
	#define NO_ERR                      (0)
#endif
#ifndef UNKOWN_ERR
	#define UNKOWN_ERR                   (1)
#endif
//#define ERROR                         (1)
#ifndef NOT_FOUND
	#define NOT_FOUND                     (2)
#endif
#ifndef NO_FILE_READ
	#define NO_FILE_READ                  (3)
#endif
#ifndef OPEN_FILE_PROBLEM
	#define OPEN_FILE_PROBLEM             (4)
#endif
#ifndef NO_END_QUOT
	#define NO_END_QUOT                   (5)
#endif
#ifndef NO_PARAMETER_FOUND
	#define NO_PARAMETER_FOUND            (6)
#endif
#ifndef PARAMETER_NOT_FOUND
	#define PARAMETER_NOT_FOUND           (7)
#endif
#ifndef PARAMETER_NAME_NOT_FOUND
	#define PARAMETER_NAME_NOT_FOUND      (8)
#endif
#ifndef NO_VALUE_CONVERTED
	#define NO_VALUE_CONVERTED            (9)
#endif
#ifndef NO_VALUE_FOUND
	#define NO_VALUE_FOUND               (10)
#endif
#ifndef OVER_UNDER_FLOW
	#define OVER_UNDER_FLOW              (11)
#endif
#ifndef INDEX_BRACKET_NOT_OKAY
	#define INDEX_BRACKET_NOT_OKAY       (12)
#endif
#ifndef UNIT_BRACKET_NOT_OKAY
	#define UNIT_BRACKET_NOT_OKAY        (13)
#endif
#ifndef NOT_ENOUGH_MEMORY
	#define NOT_ENOUGH_MEMORY            (14)
#endif
#ifndef NO_CLOSE_BRACKET
	#define NO_CLOSE_BRACKET             (15)
#endif
#ifndef MATRIX_IS_SINGULAR
	#define MATRIX_IS_SINGULAR           (16)
#endif
#ifndef STEPSIZE_TOO_SMALL
	#define STEPSIZE_TOO_SMALL           (17)
#endif
#ifndef NMAX_REACHED
	#define NMAX_REACHED                 (18)
#endif
#ifndef INPUT_NOT_CONSISTENT
	#define INPUT_NOT_CONSISTENT         (19)
#endif
#ifndef STATE_NO_SUCCESS
	#define STATE_NO_SUCCESS             (20)
#endif
#ifndef JACOBI_NO_SUCCESS
	#define JACOBI_NO_SUCCESS            (20)
#endif
#ifndef MASS_NO_SUCCESS
	#define MASS_NO_SUCCESS              (21)
#endif
#ifndef WRONG_POINTER
	#define WRONG_POINTER                (22)
#endif
#ifndef WRONG_TYPE
	#define WRONG_TYPE                   (23)
#endif


// In double-array is for matrix first irow, second icol index = irow + nrow*icol
#ifndef DOUBLE_MATRIX_ROW_BEFORE_COL
	#define DOUBLE_MATRIX_ROW_BEFORE_COL  1
#endif
// In double-array is for matrix first icol, second irow index = irow * ncol+ icol
#ifndef DOUBLE_MATRIX_COL_BEFORE_ROW
	#define DOUBLE_MATRIX_COL_BEFORE_ROW  2
#endif

#ifndef STATE_SEARCH_DATA
	#define STATE_SEARCH_DATA   (0)
#endif
#ifndef STATE_READ_DATA
	#define STATE_READ_DATA     (1)
#endif

#ifndef  byte_t
typedef unsigned char     byte_t;
#endif
#ifndef  bool_t
typedef unsigned char     bool_t;
#endif
#ifndef  status_t
typedef char              status_t;
#endif
#ifndef  error_t
typedef signed short int  error_t;
#endif
#ifndef  IERR_T
#define IERR_T error_t         /*  2 bytes,   signed */
#endif
#ifndef  BOOL_T
#define BOOL_T bool_t            /*  1 bytes, unsigned */
#endif
#ifndef  BYTE_T
#define BYTE_T byte_t            /*  1 bytes, unsigned */
#endif
#ifndef  STATUS_T
#define STATUS_T status_t            /*  1 bytes, unsigned */
#endif

#ifndef d64_t
typedef double              d64_t;
#endif
#ifndef f32_t
typedef float               f32_t;
#endif
#ifndef sint8_t
typedef int8_t         sint8_t ;
#endif
//#ifndef uint8_t
//typedef unsigned char       uint8_t ;
//#endif
#ifndef sint16_t
typedef int16_t               sint16_t ;
#endif
//#ifndef uint16_t
//typedef unsigned short      uint16_t ;
//#endif
#ifndef sint32_t
typedef int32_t               sint32_t ;
#endif
//#ifndef uint32_t
//typedef unsigned long int   uint32_t ;
//#endif
#ifndef sint64_t
typedef int64_t               sint64_t ;
#endif
//#ifndef uint64_t
//typedef unsigned long long  uint64_t ;
//#endif
#ifndef  UINT8_T
#define UINT8_T uint8_t            /*  1 bytes, unsigned */
#endif
#ifndef  SINT8_T
#define SINT8_T sint8_t            /*  1 bytes,   signed */
#endif
#ifndef  UINT16_T
#define UINT16_T uint16_t           /*  2 bytes, unsigned */
#endif
#ifndef  SINT16_T
#define SINT16_T sint16_t           /*  2 bytes,   signed */
#endif
#ifndef  UINT32_T
#define UINT32_T uint32_t          /*  2 bytes, unsigned */
#endif
#ifndef  SINT32_T
#define SINT32_T sint32_t           /*  2 bytes,   signed */
#endif
#ifndef  FLOAT_T
typedef float  FLOAT_T;            /*  4 bytes           */
#endif
#ifndef  DOUBLE_T
typedef double DOUBLE_T;           /*  8 bytes           */
#endif
#ifndef  CHAR_T
typedef char CHAR_T;           /*  8 bytes           */
#endif


#ifndef DEF_UINT8
	#define DEF_UINT8 DEF_UNSIGNED_CHAR
#endif
// Var Types
enum EVarType {
  DEF_VOID,               // nicht belegt
  DEF_DOUBLE,             // Einzelwert double,
  DEF_FLOAT,              // float, ...
  DEF_SIGNED_LONG,
  DEF_UNSIGNED_LONG,
  DEF_SIGNED_SHORT,
  DEF_UNSIGNED_SHORT,
  DEF_SIGNED_CHAR,
  DEF_UNSIGNED_CHAR,
  DEF_STRING,             // Einzelwert String-Klasse slf::CStr
  DEF_CHAR_STRING,        // String aus charater mit '\0' beendet z.B. matlab eingabe

  DEF_ARR_DOUBLE,         // Array mit double-Werten, 
  DEF_ARR_FLOAT,          // Länge muß übergerdneter Struct definiert sein
  DEF_ARR_SIGNED_LONG,
  DEF_ARR_UNSIGNED_LONG,
  DEF_ARR_SIGNED_SHORT,
  DEF_ARR_UNSIGNED_SHORT,
  DEF_ARR_SIGNED_CHAR,
  DEF_ARR_UNSIGNED_CHAR,
  DEF_ARR_STRING,         // Array String-Klasse slf::CStr

  DEF_VEC,             // Vektor-Struktur Vector_t (Numeric.h)
  DEF_MAT,             // Matrix_t-Struktur Matrix_t (Numeric.h)
  DEF_SLFVEC,          // Vektor-Struktur mit SSlfVec (Numeric.h)
  DEF_SLFMAT,          // Matrix_t-Struktur mit SSlfMat (Numeric.h)
  DEF_STRINGV,         // String-Klasse slf::CStrV (Vektor-Klasse)
  DEF_STRINGM,         // String-Matrix_t-Klasse slf::CStrM
  DEF_1D_TAB,          // einfache Tabellen-Struktur SSfl1DTab y = f(x)
  DEF_2D_TAB,          // zweidim. Tabellenstruktur  SSfl2DTab z = f(x,y)

  DEF_DOUBLE_PTR,             // Pointer Einzelwert *double, ...
  DEF_FLOAT_PTR,
  DEF_SIGNED_LONG_PTR,
  DEF_UNSIGNED_LONG_PTR,
  DEF_SIGNED_SHORT_PTR,
  DEF_UNSIGNED_SHORT_PTR,
  DEF_SIGNED_CHAR_PTR,
  DEF_UNSIGNED_CHAR_PTR,
  DEF_STRING_PTR,       // Einzelwert String-Klasse *slf::CStr

  DEF_VEC_PTR,         // Pointer der Vectorstruktur *Vector_t
  DEF_MAT_PTR,         // Pointer der Matrixstruktur *Matrix_t
  DEF_1D_TAB_PTR,      // Pointer der 1d-Tabelle     *CSlfTab1D
};

#define DEF_SINT32 DEF_SIGNED_LONG
#define DEF_UINT32 DEF_UNSIGNED_LONG
#define DEF_SINT16 DEF_SIGNED_SHORT
#define DEF_UINT16 DEF_UNSIGNED_SHORT
#define DEF_SINT8  DEF_SIGNED_CHAR
#define DEF_UINT8  DEF_UNSIGNED_CHAR

#define DEF_SINT32_PTR DEF_SIGNED_LONG_PTR
#define DEF_UINT32_PTR DEF_UNSIGNED_LONG_PTR
#define DEF_SINT16_PTR DEF_SIGNED_SHORT_PTR
#define DEF_UINT16_PTR DEF_UNSIGNED_SHORT_PTR
#define DEF_SINT8_PTR  DEF_SIGNED_CHAR_PTR
#define DEF_UINT8_PTR  DEF_UNSIGNED_CHAR_PTR

// States
# define DEF_DEFI                          8  /* Definition State */
# define DEF_INIT                          1  /* first initialization State */
# define DEF_INIT2                         5  /* second initialization State, for parameter exchange*/      # define DEF_FIRST_START                   6  /* start loop of first run State */
# define DEF_FIRST_RUN                     3  /* loop of first run State */
# define DEF_START                         7  /* Fisrt loop with starting conditions also */
                                              /* last loop of first run State */
# define DEF_LOOP                          0  /* Loop State */
# define DEF_RUN                           0  /* Run State == Loop State */
# define DEF_DONE                          4  /* done State */

# define DEF_RESET                         2


#ifdef SLFBASIC_VAR_TYPE_STR
#ifdef __cplusplus
  extern "C" {
#endif

extern char *VarTypeStr[];

#ifdef __cplusplus
  }
#endif
#endif


// Drive Selector


#ifndef DRIVE_SELECT_P
	#define DRIVE_SELECT_P		1       // parken
#endif
#ifndef DRIVE_SELECT_N
	#define DRIVE_SELECT_N		0       // neutral
#endif
#ifndef DRIVE_SELECT_D
	#define DRIVE_SELECT_D		2       // automatisch fahren
#endif
#ifndef DRIVE_SELECT_R
	#define DRIVE_SELECT_R		3       // rueckwaerts
#endif
#ifndef DRIVE_SELECT_M	
	#define DRIVE_SELECT_M		4       // manual
#endif

#ifndef MAX_SINT8	
	# define    MAX_SINT8             127
#endif
#ifndef MIN_SINT8	
	# define    MIN_SINT8             (-127)
#endif
#ifndef MAX_UINT8	
	# define    MAX_UINT8             255
#endif
#ifndef MIN_UINT8	
	# define    MIN_UINT8             0
#endif
#ifndef MAX_SINT16	
	# define    MAX_SINT16            32767
#endif
#ifndef MIN_SINT16	
	# define    MIN_SINT16            (-32767)
#endif
#ifndef MAX_UINT16	
	# define    MAX_UINT16            (uint16_t)65535L
#endif
#ifndef MIN_UINT16	
	# define    MIN_UINT16            0
#endif
#ifndef MAX_SINT32	
	# define    MAX_SINT32            2147483647L
#endif
#ifndef MIN_SINT32	
	# define    MIN_SINT32            (-2147483647L)
#endif
#ifndef MAX_UINT32	
	# define    MAX_UINT32            4294967295L
#endif
#ifndef MIN_UINT32	
	# define    MIN_UINT32            0L
#endif

#ifndef MAX_INT16	
	# define    MAX_INT16             32767
#endif
#ifndef MIN_INT16	
	# define    MIN_INT16             (-32767)
#endif
#ifndef MAX_UNS16	
	# define    MAX_UNS16             (UNS16)65535L
#endif
#ifndef MIN_UNS16	
	# define    MIN_UNS16             0
#endif
#ifndef DRIVE_SELECT_M	
	# define    MAX_INT32             2147483647L
#endif
#ifndef MIN_INT32	
	# define    MIN_INT32             (-2147483648L)
#endif
#ifndef MAX_UNS32	
	# define    MAX_UNS32             4294967295L
#endif
#ifndef MIN_UNS32	
	# define    MIN_UNS32             0L

#endif
#ifndef EPSILON	
	# define    EPSILON               FLT_EPSILON
#endif
#ifndef DEPSILON	
	# define    DEPSILON              DBL_EPSILON
#endif


//==========================
// simple functions (macros)
//==========================
 //  ABS(x)
 # ifndef  ABS
   # define  ABS(x)      ((x>=0.) ? (x) : (-(x)))
 # endif
 //  IABS(x)
 # ifndef  IABS
   # define  IABS(x)      ((x>=0) ? (x) : (-(x)))
 # endif
// SIGN(x)
 # ifndef  SIGN
  # define  SIGN(x)     ((x>=0.) ? (1.) : (-1.))
 # endif
//  NOT_ZERO(x)
# ifndef  NOT_ZERO
  # define  NOT_ZERO(x)  ((ABS(x)>EPSILON) ? (x) : (EPSILON*SIGN(x)))
# endif
//  MAX(x,y)
# ifndef  MAX
  # define  MAX(x,y)    (((x) > (y)) ? (x) : (y))
# endif
//  MIN(x,y)
# ifndef  MIN
  # define  MIN(x,y)    (((x) < (y)) ? (x) : (y))
# endif

#ifndef NINT
    #define NINT(x)       ((x) <  0  ? (int)(x - 0.5) : (int)(x + 0.5))
#endif

#ifndef TRUNCATE
    #define TRUNCATE(x)   ((x) <  0  ? ceil(x)     : floor(x))
#endif

#ifndef ROUND
    #define ROUND(x)      ((x) <  0  ? ceil(x-0.5) : floor(x+0.5))
#endif

#ifndef ITRUNCATE
    #define ITRUNCATE(x)  ((x) <  0  ? (int)ceil(x)     : (int)floor(x))
#endif

#ifndef IROUND
    #define IROUND(x)     ((x) <  0  ? (int)ceil(x-0.5) : (int)floor(x+0.5))
#endif

#ifndef BOUND
    #define BOUND(x,L,U)  ((x) <  L  ? L : ((x) > U ? U : (x)))
#endif


#ifndef BITAND
    #define BITAND(x,y)   ((x) & (y))
#endif

#ifndef BITOR
    #define BITOR(x,y)    ((x) | (y))
#endif

#ifndef BITNOT
    #define BITNOT(x)     (~x)
#endif

#ifndef BTEST
    #define BTEST(x,y)    (((x) &  (1 << (y))) != 0)
#endif

#ifndef BSET
    #define BSET(x,y)     ((x)  |  (1 << (y)))
#endif

#ifndef BCLEAR
    #define BCLEAR(x,y)   ((x)  & ~(1 << (y)))
#endif

#ifndef BTOGGLE
    #define BTOGGLE(x,y)  ((x)  ^  (1 << (y)))
#endif

#ifndef BLSHIFT
    #define BLSHIFT(x,y)  ((x) << (y))
#endif

#ifndef BRSHIFT
    #define BRSHIFT(x,y)  ((x) >> (y))
#endif

#ifndef ISWAP
    #define ISWAP(a,b)  \
    { RT_INTEGER _temp; \
    _temp = a; a = b; b = _temp; \
    }
#endif

#ifndef RSWAP
    #define RSWAP(a,b)  \
    { RT_FLOAT _temp; \
    _temp = a; a = b; b = _temp; \
    }
#endif

#ifndef MAKE_STRING
    #define MAKE_STRING0( a) #a
    #define MAKE_STRING( a) MAKE_STRING0 (a)
#endif

#ifndef FAC_OMEGA_TO_N
	#define FAC_OMEGA_TO_N    (30./PI)
#endif
#ifndef FAC_N_TO_OMEGA
	#define FAC_N_TO_OMEGA    (PI/30.)
#endif

#ifndef PI
	# define  PI                (4.*PI_4)
#endif
#ifndef PI_2
	# define  PI_2              (2.*PI_4)
#endif
#ifndef PI_4
	# define  PI_4              atan(1.0)
#endif
#ifndef D_PI
	# define  D_PI              (8.*PI_4)
#endif


/* Versionnummer bis Visual C 2005, damit werden diese unsicheren Funktionen abgefangen */
#define MSC_VER_BIS_VS2005  1310




#endif
