// Grundsatzzuordnungen
//
#ifndef SFL_DEF_H_INCLUDED
#define SFL_DEF_H_INCLUDED


#ifndef  SLF_NO_DEFINE_TIMESTAMP_T
typedef long long timestamp_t;
#define SLF_NO_DEFINE_TIMESTAMP_T
#define SLF_TSTAMP_STEP ((double)1.0e-6)
#define SLF_TSTAMP_HALF_STEP ((double)0.5e-6)
#define SLF_TIME_TO_TSTAMP(t) ((timestamp_t)((t)/SLF_TSTAMP_STEP))
#endif

#define SLF_TIMESTAMP_TO_TIME(ts,typ) ((typ)((double)(ts)*SLF_TSTAMP_STEP))

#ifndef  SLF_NO_DEFINE_BYTE_T
typedef unsigned char     byte_t;
#define SLF_NO_DEFINE_BYTE_T
#endif
#ifndef  SLF_NO_DEFINE_BOOL_T
typedef unsigned char     bool_t;
#define SLF_NO_DEFINE_BOOL_T
#endif
#ifndef  SLF_NO_DEFINE_OKAY_T
typedef bool  okay_t;
#define SLF_NO_DEFINE_OKAY_T
#endif

#ifndef SLF_NO_DEFINE_D64_T
typedef double              d64_t;
#define SLF_NO_DEFINE_D64_T
#endif
#ifndef SLF_NO_DEFINE_F32_T
typedef float               f32_t;
#define SLF_NO_DEFINE_D64_T
#endif
#ifndef SLF_NO_DEFINE_SINT8_T
typedef signed char         sint8_t;
#define SLF_NO_DEFINE_SINT8_T
#endif
#ifndef SLF_NO_DEFINE_SINT16_T
typedef short               sint16_t;
#define SLF_NO_DEFINE_SINT16_T
#endif
#ifndef SLF_NO_DEFINE_SINT32_T
typedef int               sint32_t;
#define SLF_NO_DEFINE_SINT32_T
#endif
#ifndef SLF_NO_DEFINE_SINT64_T
typedef long long               sint64_t;
#define SLF_NO_DEFINE_SINT64_T
#endif

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
#define FLOAT_T float            /*  4 bytes           */
#endif
#ifndef  DOUBLE_T
#define DOUBLE_T double;           /*  8 bytes           */
#endif
#ifndef  CHAR_T
#define CHAR_T char;           /*  1 bytes           */
#endif

#ifndef  BOOL_T
#define BOOL_T bool_t            /*  1 bytes, unsigned */
#endif
#ifndef  BYTE_T
#define BYTE_T byte_t            /*  1 bytes, unsigned */
#endif

//globaler DEBUG-Flag
#ifndef SLF_DEBUG_FLAG
	#define SLF_DEBUG_FLAG
#endif
// Status-Zuordnung
#ifndef OKAY
	#define OKAY      true
#endif
#ifndef OK
	#define OK        true
#endif
#ifndef NOT_OKAY
	#define NOT_OKAY  false
#endif
#ifndef NOT_OK
	#define NOT_OK    false
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
#ifndef SLF_EPSILON	
	# define    SLF_EPSILON               FLT_EPSILON
#endif
#ifndef SLF_DEPSILON	
	# define    SLF_DEPSILON              DBL_EPSILON
#endif


//==========================
// simple functions (macros)
//==========================
 //  ABS(x)
 # ifndef  SLF_ABS
   # define  SLF_ABS(x,type)      ((x>=(type)0.) ? (x) : (-(x)))
 # endif
 // SIGN(x)
 # ifndef  SLF_SIGN
  # define  SLF_SIGN(x,type)     ((x>=(type)0.) ? ((type)1.) : ((type)-1.))
 # endif
//  NOT_ZERO(x)
# ifndef  SLF_NOT_ZERO
  # define  SLF_NOT_ZERO(x,type)  ((SLF_ABS(x,type)>(type)SLF_EPSILON) ? (x) : ((type)SLF_EPSILON*SLF_SIGN(x,type)))
# endif
//  MAX(x,y)
# ifndef  SLF_MAX
  # define  SLF_MAX(x,y)    (((x) > (y)) ? (x) : (y))
# endif
//  MIN(x,y)
# ifndef  SLF_MIN
  # define  SLF_MIN(x,y)    (((x) < (y)) ? (x) : (y))
# endif

#ifndef SLF_NINT
    #define SLF_NINT(x)       ((x) <  0  ? (int)(x - 0.5) : (int)(x + 0.5))
#endif

#ifndef SLF_TRUNCATE
    #define SLF_TRUNCATE(x)   ((x) <  0  ? ceil(x)     : floor(x))
#endif

#ifndef SLF_ROUND
    #define SLF_ROUND(x)      ((x) <  0  ? ceil(x-0.5) : floor(x+0.5))
#endif

#ifndef SLF_ITRUNCATE
    #define SLF_ITRUNCATE(x)  ((x) <  0  ? (int)ceil(x)     : (int)floor(x))
#endif

#ifndef SLF_IROUND
    #define SLF_IROUND(x)     ((x) <  0  ? (int)ceil(x-0.5) : (int)floor(x+0.5))
#endif

#ifndef SLF_BOUND
    #define SLF_BOUND(x,L,U)  ((x) <  L  ? L : ((x) > U ? U : (x)))
#endif


#ifndef SLF_BITAND
    #define SLF_BITAND(x,y)   ((x) & (y))
#endif

#ifndef SLF_BITOR
    #define SLF_BITOR(x,y)    ((x) | (y))
#endif

#ifndef SLF_BITNOT
    #define SLF_BITNOT(x)     (~x)
#endif

#ifndef SLF_BTEST
    #define SLF_BTEST(x,y)    (((x) &  (1 << (y))) != 0)
#endif

#ifndef SLF_BSET
    #define SLF_BSET(x,y)     ((x)  |  (1 << (y)))
#endif

#ifndef SLF_BCLEAR
    #define SLF_BCLEAR(x,y)   ((x)  & ~(1 << (y)))
#endif

#ifndef SLF_BTOGGLE
    #define SLF_BTOGGLE(x,y)  ((x)  ^  (1 << (y)))
#endif

#ifndef SLF_BLSHIFT
    #define SLF_BLSHIFT(x,y)  ((x) << (y))
#endif

#ifndef SLF_BRSHIFT
    #define SLF_BRSHIFT(x,y)  ((x) >> (y))
#endif

#ifndef SLF_SWAP
    #define SLF_SWAP(a,b,typ)  \
    { typ _temp; \
    _temp = a; a = b; b = _temp; \
    }
#endif


#ifndef SLF_MAKE_STRING
    #define SLF_MAKE_STRING0( a) #a
    #define SLF_MAKE_STRING( a) SLF_MAKE_STRING0 (a)
#endif

#ifndef SLF_FAC_OMEGA_TO_N
	#define SLF_FAC_OMEGA_TO_N    (30./SLF_PI)
#endif
#ifndef SLF_FAC_N_TO_OMEGA
	#define SLF_FAC_N_TO_OMEGA    (SLF_PI/30.)
#endif

#ifndef SLF_PI
	# define  SLF_PI                (4.*PI_4)
#endif
#ifndef SLF_PI_2
	# define  SLF_PI_2              (2.*PI_4)
#endif
#ifndef SLF_PI_4
	# define  SLF_PI_4              atan(1.0)
#endif
#ifndef SLF_D_PI
	# define  SLF_D_PI              (8.*PI_4)
#endif


/* Versionnummer bis Visual C 2005, damit werden diese unsicheren Funktionen abgefangen */
#define MSC_VER_BIS_VS2005  1310




#endif
