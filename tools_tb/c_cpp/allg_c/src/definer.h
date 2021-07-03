/* $JustDate::  2.03.06  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 2.0 13.08.01 TBert Release VehicleModell 2.0           */
/*  file-header*/
/****************************************************************************/
/*   Copyright (c) 1997   ITT Automotive Europe GmbH    All Rights Reserved */
/*                                                                          */
/*   Department: TER                                    @(#) 27-FEB-1997    */
/*--------------------------------------------------------------------------*/
/*   File: definer.h                                                        */
/*                                                                          */
/*   Author: @(#) U. Judaschke                          Tel.: 3565          */
/*--------------------------------------------------------------------------*/
/*   Description:                                                           */
/*   @(#) global definitions for all programs                               */
/*--------------------------------------------------------------------------*/
/*   changes:                                                               */
/*     0.0 -> 1.0  061295  ju: first implementation                         */
/*     1.0 -> 1.1  240297  ju: new definition of SYSTEM_...                 */
/*     1.0 -> 1.2  001108  TBert: additional defines included               */
/*            1.3  001116  TBert: INIT_RUN,FIRST_RUN,LOOP_RUN,DONE_RUN      */
/*            2.0  030224  TBert: SINTxx and UINTxx introduced              */
/****************************************************************************/
/*  */
#  ifndef _DEFINER_INC
#  define _DEFINER_INC

  //  imported specifications of other modules

    # include "system.h"      /* TARGET SYSTEM specification */

    # if (SYSTEM_OS==WINDOWS) || MATLAB_MEX_FILE
        #ifndef _WINDOWS_
            # include <windows.h>
        #endif
    # endif

    # include <stdint.h>
    # include <stdlib.h>
    # include <stdio.h>
    # include <float.h>
    # include <limits.h>
    # include <math.h>

  //+++++++++++++++++++++++++
  // 1) systep dependent marcos
  //+++++++++++++++++++++++++

    //  defines for HP9000-Unix-Workstation

      # if (SYSTEM_OS==UNIX)
        // memory
        # define  CALLOC(x,y)     calloc(x,y)
        # define  FREE(x)         free(x)
        # define  MAX_VIRTUELL_HEAP_SIZE           0L
        # define  FREE_HEAP
        // clock
        # define  CLOCK       clock
        //  mathlib
        # define  SIN          sin
        # define  COS          cos
        # define  MATADD       simMatAdd
        # define  MATMUL       simMatMul
        # define  VECMUL       simVecMul
        # define  MATSUB       simMatSub
        # define  MATSCALMULT  simMatScalMult
        # define  VECCROSS     simVecCross
        //  file-io
        # define UNGETC      ungetc
        # define FGETC       fgetc
      # endif

      //  defines for PC
      # if (SYSTEM_OS==DOS)||(SYSTEM_OS==WINDOWS)

        //  defines for  all compilers
          // meory
          # define  FREE_HEAP
         // clock
           # define  CLOCK       clock
         //  mathlib
          # define  SIN          sin
          # define  COS          cos
          # define  MATADD       simMatAdd
          # define  MATMUL       simMatMul
          # define  VECMUL       simVecMul
          # define  MATSUB       simMatSub
          # define  MATSCALMULT  simMatScalMult
          # define  VECCROSS     simVecCross
         //  file-io
          # define UNGETC      ungetc
          # define FGETC       fgetc

      //  defines for  Borland compilers
        # if(SYSTEM_COMP==BORLAND)
         // meory
          # define  CALLOC(x,y)     calloc(x,y+100)    // ?????
          # define  FREE(x)         free(x)
          # define  MAX_VIRTUELL_HEAP_SIZE     50L
        # endif

      //  defines for Visual C++ compiler
        # if(SYSTEM_COMP==VISUAL_C)
         // meory
          #if 0
            # define  CALLOC(x,y)     sysCalloc( 1,x,y,0,__FILE__,(unsigned short int)__LINE__)
            # define  DEBCALLOC(x,y,z)sysCalloc( 1,(x),(y),0,"",(unsigned short int)z)
            # define  FREE(x)         sysCalloc(-1,0,0,(x),__FILE__,(unsigned short int)__LINE__)
            # define  MAX_VIRTUELL_HEAP_SIZE     50L
            # undef   FREE_HEAP
            # define  FREE_HEAP       sysCalloc( 0,0,0,0,__FILE__,(unsigned short int)__LINE__)
          #else
            # define  CALLOC(x,y)     calloc(x,y+100)    // ?????
            # define  FREE(x)         free(x)
            # define  MAX_VIRTUELL_HEAP_SIZE     50L
          #endif

        # endif

      //  defines for  WatCom C 10.0
       # if (SYSTEM_COMP==WATCOM)
         //  memory
         # define  CALLOC     calloc
         # define  FREE       free
         # define  MAX_VIRTUELL_HEAP_SIZE      0L
       # endif
      #endif
      

    //+++++++++++++++++++++++++
    // 2) typedef for variavle types
    //+++++++++++++++++++++++++

    //  typedefs for HP9000-Unix-Workstation

    # if (SYSTEM_OS==UNIX)
        typedef unsigned char         BOOL;             /*  boolean           */
        typedef unsigned char         BYTE;             /*  1 byte,  unsigned */
        typedef          char         CHAR;             /*  1 byte,    signed */
        typedef unsigned short int    UNS16;            /*  2 bytes, unsigned */
        typedef          short int    INT16;            /*  2 bytes,   signed */
        typedef unsigned long  int    UNS32;            /*  4 bytes, unsigned */
        typedef          long  int    INT32;            /*  4 bytes,   signed */
        typedef                float  FLOAT;            /*  4 bytes           */
        typedef                double DOUBLE;           /*  8 bytes           */
    # endif

    // typedefs for PC

    # if (SYSTEM_OS==DOS) ||(SYSTEM_OS==WINDOWS)


    #ifndef  sint8_t
        typedef int8_t  sint8_t;
    #endif
#ifndef  sint16_t
        typedef int16_t  sint16_t;
#endif
#ifndef  sint32_t
        typedef int16_t  sint32_t;
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
    #ifndef  UINT8_T
        typedef unsigned char         UINT8_T;            /*  1 bytes, unsigned */
     #endif
     #ifndef  SINT8_T
        typedef signed   char         SINT8_T;            /*  1 bytes,   signed */
     #endif
     #ifndef  UINT16_T
        typedef unsigned short int    UINT16_T;           /*  2 bytes, unsigned */
     #endif
     #ifndef  SINT16_T
        typedef signed   short int    SINT16_T;           /*  2 bytes,   signed */
     #endif
     #ifndef  UINT32_T
        typedef unsigned long  int    UINT32_T;          /*  2 bytes, unsigned */
     #endif
     #ifndef  SINT32_T
        typedef signed   long  int    SINT32_T;           /*  2 bytes,   signed */
     #endif
     #ifndef  FLOAT_T
        typedef                float  FLOAT_T;            /*  4 bytes           */
     #endif
     #ifndef  DOUBLE_T
        typedef                double DOUBLE_T;           /*  8 bytes           */
     #endif

     #ifndef  IERR_T
        typedef signed   short int    IERR_T;         /*  2 bytes,   signed */
     #endif
     #ifndef  BOOL_T
        typedef unsigned char         BOOL_T;            /*  1 bytes, unsigned */
     #endif

     #ifndef  STATUS_T
        typedef unsigned char         STATUS_T;            /*  1 bytes, unsigned */
     #endif

        typedef          char         CHAR;             /*  1 byte,    signed */

      # if (SYSTEM_OS==DOS)
       # ifndef MATLAB_MEX_FILE
        typedef                int    BOOL;             /*  boolean           */
        typedef unsigned char         BYTE;             /*  1 byte,  unsigned */
       # endif
      # endif

        typedef unsigned short int    UNS16;            /*  2 bytes, unsigned */
        typedef          short int    INT16;            /*  2 bytes,   signed */
        typedef unsigned long  int    UNS32;            /*  4 bytes, unsigned */
      # if (SYSTEM_COMP == BORLAND) || (SYSTEM_COMP == WATCOM)
        typedef          long  int    INT32;            /*  4 bytes,   signed */
      # endif
        typedef                float  FLOAT;            /*  4 bytes           */
        typedef                double DOUBLE;           /*  8 bytes           */
    # endif


    //+++++++++++++++++++++++++
    // 3) ANSI or K&R - declarations
    //+++++++++++++++++++++++++

      #if(SYSTEM_OS==UNIX)
          #if defined(__STDC__) || defined(__cplusplus)
           #define _ANSI_
          #else /* not __STDC__ || __cplusplus */
           #ifdef  _ANSI_
             #undef _ANSI_
           #endif
         #endif
      #endif

      #if (SYSTEM_OS==DOS)|| (SYSTEM_OS==WINDOWS)
        #define _ANSI_
      #endif

      #if (SYSTEM_OS==DOS)&&(SYSTEM_PROC==Ix86)&&(SYSTEM_COMP==WATCOMC_10_0)
       # define _ANSI_
      # endif

    //+++++++++++++++++++++++++
    // 4) simple functions
    //+++++++++++++++++++++++++
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
    /*+++++++++++++++++++++++++*/
    /* 5) const */
    /*+++++++++++++++++++++++++*/
      /*------------------------------------------*/
      /* Status-Zuordnung */
      #ifndef NULL
        #define NULL 0
      #endif

      #define CANCEL    (2)
      # define  OK                                  0
      # define  OKAY                                0
      # define  NOT_OK                              1
      # define  NOT_FOUND                           2
      # define  NOT_OKAY                            NOT_OK
      #define STATUS_INIT  (0)
    	#define STATUS_FIRST (1)
    	#define STATUS_LOOP  (2)
    	#define STATUS_DONE  (3)
      # define  TRUE                  1
      # define  FALSE                 0
      # define  YES                   TRUE
      # define  NO                    FALSE
      # define  DO_RESET              TRUE
      # define  NO_RESET              FALSE
      # define  NOT_END_OF_FILE       0
      /*------------------------------------------*/
      # define  PRIVATE               static
      /*------------------------------------------*/
      # define  NO_DYNAMICS           0
      # define  CONTINUOUS            1
      # define  CONTINUOUS_WITH_INT   2
      # define  TIME_DISCRETE         3
      # define  INTEGRATOR            4
      /*------------------------------------------*/
      #ifndef PI
        # define  PI                (4.*PI_4)
      #endif
      # define  PI_2              (2.*PI_4)
      # define  PI_4              atan(1)
      # define  D_PI              (8.*PI_4)
      /*------------------------------------------*/
      # define    EPSILON               FLT_EPSILON
      # define    MAX_DOUBLE            FLT_MAX
      # define    MIN_DOUBLE            -FLT_MAX

      # define    MAX_SINT8             127
      # define    MIN_SINT8             (-127)
      # define    MAX_UINT8             255
      # define    MIN_UINT8             0
      # define    MAX_SINT16            32767
      # define    MIN_SINT16            (-32767)
      # define    MAX_UINT16            (UNS16)65535L
      # define    MIN_UINT16            0
      # define    MAX_SINT32            2147483647L
      # define    MIN_SINT32            (-2147483648L)
      # define    MAX_UINT32            4294967295L
      # define    MIN_UINT32            0L

      # define    MAX_INT16             32767
      # define    MIN_INT16             (-32767)
      # define    MAX_UNS16             (UNS16)65535L
      # define    MIN_UNS16             0
      # define    MAX_INT32             2147483647L
      # define    MIN_INT32             (-2147483648L)
      # define    MAX_UNS32             4294967295L
      # define    MIN_UNS32             0L
      /*-------------------------------------------------------------*/
      #define  IS_SINT8          1          /* variable type         */
      #define  IS_UINT8          2          /* variable type         */
      #define  IS_SINT16         3          /* variable type         */
      #define  IS_UINT16         4          /* variable type         */
      #define  IS_SINT32         5          /* variable type         */
      #define  IS_UINT32         6          /* variable type         */

      #define  IS_INT16          IS_SINT16  /* variable type         */
      #define  IS_UNS16          IS_UINT16  /* variable type         */
      #define  IS_INT32          IS_SINT32  /* variable type         */
      #define  IS_UNS32          IS_UINT32  /* variable type         */

      #define  IS_FLOAT          8          /* variable type         */
      #define  IS_DOUBLE         9          /* variable type         */
      #define  IS_XY_CURVE      10          /* variable type         */
      #define  IS_XYZ_SURFACE   11          /* variable type         */
      #define  IS_BYTE          IS_UINT8    /* variable type         */
      #define  IS_BOOL          IS_UINT8    /* variable type         */
      #define  IS_STRING        14          /* variable type         */
      #define  IS_CASE          15          /* variable type         */
      /*-------------------------------------------------------*/
      #define  IS_CONTINUOUS     1          /* integration type      */
      /*-------------------------------------------------------*/
      /*------------------------------------------*/
      # define  INITAL                          99
      # define  MAX_SIGNS_OF_WORDS              70
      /*------------------------------------------*/
      // interpolation modes                      */
      # define  LINEAR_INTERPOLATION             1
      # define  STEP_INTERPOLATION               2
      /*------------------------------------------*/
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
      /*------------------------------------------*/
      #define  INITIALIZE             DEF_INIT    /* module initialization */
      #define  RESET                  DEF_RESET   /* module reset          */
      #define  CALC_DERIVATES         5           /* calculation of deriv. */
      #define  CALC_OUTPUTS           6           /* calculation of outputs*/
      #define  EXIT                   DEF_DONE    /* module termination    */
      #define  CALC_DERIVATES_SIMPLE  7           /* simpl. cal. of deriv. */
      #define  CALC_OUTPUTS_SIMPLE    8           /* simpl. cal. of outputs*/
      #define  SERVER_START           9           /* server start          */
      /*------------------------------------------------------------*/
      #define SEPERATION_CHARACTER_OF_LIST ","
      /*------------------------------------------------------------*/
      //  error messages
      /*------------------------------------------------*/
      /*  system independent errors     (    0-   255)  */
      /*------------------------------------------------*/

      #define NO_STRING_ERR                         3
      #define NO_STRING_TEXT                        "No valid string"
      #define NO_STRING_REASON                      "Pointer is NULL, not initialised or has no termination"

      #define UNABLE_TO_OPEN_FILE_ERR               4
      #define UNABLE_TO_OPEN_FILE_TEXT              "unable to open file "
      #define UNABLE_TO_OPEN_FILE_REASON            ""

      #define INVALID_FILESPEC_ERR                  5
      #define INVALID_FILESPEC_TEXT                 "Invalid filename specification"
      #define INVALID_FILESPEC_REASON               "invalid characters used (e.g. &$§)"

      #define FILE_NOT_OPENED_ERR                   6
      #define FILE_NOT_OPENED_TEXT                  "No file opened"
      #define FILE_NOT_OPENED_REASON                "No file open routine called"

      #define END_OF_FILE_ERR                       7
      #define END_OF_FILE_TEXT                      "End of File reached"
      #define END_OF_FILE_REASON                    ""

      #define PATH_NOT_FOUND_ERR                    8
      #define PATH_NOT_FOUND_TEXT                   "path not found"
      #define PATH_NOT_FOUND_REASON                 "wrong pathname "

      #define PATH_NOT_CREATED_ERR                  9
      #define PATH_NOT_CREATED_TEXT                 "path not created"
      #define PATH_NOT_CREATED_REASON               "wrong pathname "

      #define FULLPATH_NOT_FOUND_ERR                10
      #define FULLPATH_NOT_FOUND_TEXT               "relative path could not be tranfred to absolute path"
      #define FULLPATH_NOT_FOUND_REASON             "relative path does not exist or is a absolute path"

      #define  CANNOT_ALLOCATE_MEMORY_ERR           11
      #define  CANNOT_ALLOCATE_MEMORY_TEXT          "cannot allocate memory"
      #define  CANNOT_ALLOCATE_MEMORY_REASON        ""

      #define STRUCT_NOT_DEFINED_ERR                12
      #define STRUCT_NOT_DEFINED_TEXT               "structure not defined"
      #define STRUCT_NOT_DEFINED_REASON             "structure not initialised"
      /*------------------------------------------------*/
      /*  application depending errors  (  256-32767)   */
      /*------------------------------------------------*/
      /*                                                */
      /*  vehicle dynamic simulation    (  256-  319)   */
      /*                                                */
      /*  golobal errors                                */
      /*                                                */
      # define  ERR_FILE_NOT_FOUND                256
      # define  TEXT_FILE_NOT_FOUND               "desired file could not be found"

      # define  STRING_NOT_FOUND                  257
      # define  SERVICE_NOT_FOUND                 258
      # define  SWITCH_NOT_FOUND                  259
      # define  CATEGORY_NOT_FOUND                260
      # define  CLASS_NOT_FOUND                   261
      # define  NUMBER_OVERFLOW                   262
      # define  VALUE_OUT_OF_RANGE                263
      # define  INSTANCE_NOT_FOUND                264
      # define  TOO_MANY_LINES                    265
      # define  BASE_CHARACTERISTIC_NOT_FOUND     266
      # define  SIGN_NOT_FOUND                    268
      # define  CLOSE_INITIALIZATION_FILE         270
      # define  OPEN_NEW_INITIALIZATION_FILE      271
      # define  WRONG_NUMBER_FOR_INITIALIZATION   272
      # define  UNKNOWN_INITIALIZATION_PARAMETER  273
      # define  STRING_TOO_SHORT                  274
      # define  CANNOT_OPEN_FILE                  275
      # define  UNABLE_TO_WRITE_DATA              277
      # define  DIA_PROTOCOL_ERROR                278
      # define  KEY_WORD_NOT_FOUND                279
      # define  UNKNOWN_DIA_CHANNEL_FORMAT        280
      # define  UNEXPECTED_END_OF_FILE            281
      # define  CONFIG_FILE_ERROR                 282
      # define  UNKNOWN_CONFIGURATION_INPUT       283
      # define  WHEEL_NUMBER_NOT_FOUND            284
      # define  UNKNOWN_KEYWORD_FOUND             285
      # define  UNKNOWN_INTERPOLATION_MODE        286
      # define  NO_CHANNEL_IN_DATA_MEM            287
      # define  DIA_CHANNEL_NOT_FOUND             288
      # define  UNKNOWN_SYSTEM                    289
      # define  WRONG_DIA_FORMAT                  290
      # define  INCOMPLETE_CHANNEL_HEADER_FOUND   291
      # define  INCOMPLETE_GLOBAL_HEADER_FOUND    292
      # define  UNKNOWN_DIA_HEADER_EXTENSION      293
      # define  SEEK_OVER_END_OF_FILE             294
      # define  LINE_JUST_READ                    295
      # define  END_OF_LINE                       296
      # define  FILE_ALREADY_EXISTS               297
      # define  NO_GLOBAL_INFORMATION_IN_DATA_MEM 298
      # define  UNKNOWN_DATATYPE                  299
      # define  UNKNOWN_DIA_HEADER_FORMAT         300
      # define  FILENAME_TOO_LONG                 301
      # define  POINTER_TO_DATA_IS_NULL           303
      # define  WRONG_TIMESTEP_INITIALIZATION     304
      # define  UNKNOWN_COMMAND					  306
      # define  WRONG_SYNTAX					  307
      # define  UNKNOWN_VARIABLE_TYPE			  308
      # define  UNKNOWN_DIMENSION                 309
      # define  UNKNOWN_UNIT_CONVERSION           310
      # define  UNKNOWN_APPLICATION               311 
      # define  DEFAULT_VALUE_NOT_FOUND           312
      # define  PARAMETER_NOT_FOUND               314
      # define  DEFINITION_NOT_DONE               315
      # define  INITIALISATION_NOT_DONE           316
   


/*--- Wheel Numbering  ---------------------------------------------------*/

/*    ADAMS                                                               */
#define ADA_WH_FL     0     /* front left                                 */
#define ADA_WH_FR     1     /* front right                                */
#define ADA_WH_RL     3     /* rear  left                                 */
#define ADA_WH_RR     2     /* rear  right                                */
#define ADA_WH_OFF    1     /* Offset ADAMS input file to "C" - interface */

/*    AVS                                                                 */
#define AVS_WH_FL     0     /* front left                                 */
#define AVS_WH_FR     1     /* front right                                */
#define AVS_WH_RL     2     /* rear  left                                 */
#define AVS_WH_RR     3     /* rear  right                                *///  # ifdef extern

//  VCS
#define WH_FL     1     // front left
#define WH_FR     2     // front right
#define WH_RL     3     // rear  left
#define WH_RR     4     // rear  right

// axle
#define FRONT_AXLE  0
#define REAR_AXLE   1

#define FRONT_DRIVEN           0
#define REAR_DRIVEN            1

#define FRONT                  0
#define REAR                   1




#define unsigned_char_t   UINT8_T
#define signed_char_t     SINT8_T
#define signed_int32_t    SINT32_T
#define unsigned_int32_t  UINT32_T
#define signed_int16_t    SINT16_T
#define unsigned_int16_t  UINT16_T


/*----------------   Unit definitions --------------------------------------*/
#define UNIT_NO         "[-]"
#define UNIT_TIME       "[s]"
#define UNIT_PHI        "[rad]"
#define UNIT_PHIP       "[rad/s]"
#define UNIT_VEL        "[m/s]"
#define UNIT_ACC        "[m/s/s]"


/*----------------   Comment definitions --------------------------------------*/
#define TXTCOMMENT1   "$"    /* Kommentarzeichen beliebig in der Zeile */
#define TXTCOMMENT2   "!"    /* Komentarzeichen  beliebig in der Zeile */
#define TXTCOMMENT3   "@"    /* Komentarzeichen beliebig in der Zeile */
#define TXTCOMMENT4   "#"    /* Komentarzeichen beliebig in der Zeile */
#define QUOTTEXT1     "\""
#define ZUWEISZEICHEN "="
#define TRENNZEICHEN  ","
#define LEERZEICHEN   " "
#define QUOT0EINHEIT "<"
#define QUOT1EINHEIT ">"
#define QUOT00EINHEIT "["
#define QUOT11EINHEIT "]"
#define TRENNZATTRIBUT "."
#define MATSTARTQUOT  "["
#define MATSTOPQUOT   "]"
#define MATTRENNCOL   ";"
#define MATTRENNROW   ","



// In double-array is for matrix first irow, second icol index = irow + nrow*icol
#ifndef DOUBLE_MATRIX_ROW_BEFORE_COL
#define DOUBLE_MATRIX_ROW_BEFORE_COL  1
#endif
// In double-array is for matrix first icol, second irow index = irow * ncol+ icol
#ifndef DOUBLE_MATRIX_COL_BEFORE_ROW
#define DOUBLE_MATRIX_COL_BEFORE_ROW  2
#endif





//  # undef extern
//  # endif
#  endif /* _DEFINER_INC */


