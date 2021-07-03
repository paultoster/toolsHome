/* $JustDate:: 26.09.05  $, $Revision:: 1 $ $Author:: Tftbe1       $  */
/* 2.0 13.08.01 TBert Release VehicleModell 2.0           */
//  define declarations
//  Changes:
//  081100 TBert: OFFSIMENV_APP defined

// OS - Word Length
# define BIT_16                         1
# define BIT_32                         2
# define BIT_64                         3
// Operating Systems
# define UNIX                          11
# define DOS                           12
# define WINDOWS_31				             13
# define WINDOWS   					           14
// Processors
# define Ix86                          21
# define M68xxx                        22
// Compilers
# define DSP                           23
# define HP_CC                         31
# define HP_cc                         32
# define BORLAND                       33
# define WATCOM                        34
# define VISUAL_C                      35
// Application
# define SYSTEM_APPL_STANDALONE                40
# define SYSTEM_APPL_WAVE                      41
# define SYSTEM_APPL_MATLAB                    42

//  define system define

# define  SYSTEM_OS      WINDOWS                // DOS; WINDOWS; UNIX
# define  SYSTEM_COMP    VISUAL_C               // BORLAND; WATCOM; VISUAL_C; HP_CC; HP_cc
# define  SYSTEM_PROC    Ix86                   // Ix86; M68xxx
# define  SYSTEM         BIT_32                 // BIT_32; BIT_16

#if 0
#if defined(WAVE)
    # define  SYSTEM_APPL    SYSTEM_APPL_WAVE   // SYSTEM_APPL_STANDALONE; SYSTEM_APPL_WAVE; SYSTEM_APPL_SIMULINK
#elif defined(MATLAB)
    # define  SYSTEM_APPL    SYSTEM_APPL_MATLAB   // SYSTEM_APPL_STANDALONE; SYSTEM_APPL_WAVE; SYSTEM_APPL_SIMULINK
#elif defined(MATLAB_MEX_FILE)
    # define  SYSTEM_APPL    SYSTEM_APPL_MATLAB   // SYSTEM_APPL_STANDALONE; SYSTEM_APPL_WAVE; SYSTEM_APPL_SIMULINK
#elif defined(STANDALONE)
    # define  SYSTEM_APPL    SYSTEM_APPL_STANDALONE   // SYSTEM_APPL_STANDALONE; SYSTEM_APPL_WAVE; SYSTEM_APPL_SIMULINK
#else
    # error system.h: Not the right System Application defined
#endif
#endif

#if SYSTEM_COMP == VISUAL_C

  /* Versionnummer bis Visual C 2005, damit werden diese unsicheren Funktionen abgefangen */
  #define MSC_VER_BIS_VS2005  1310

#endif