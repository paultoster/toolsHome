// DsProject.h
// Projektspezifische EInstellungen
// dieses h-File in das Projekt kopieren und anpassen
//
// Defines
//==========
// _DEBUG und NDEBUG wird von Visual C++ gesetzt
//#define DS_PAR_USE_PAR_FILE    1                // Parameterfile im DsParFile-Format (siehe DsParFile.h"
//#define DS_PAR_USE_MATFILELOAD 1                // wenn Parameterfile, können auch Daten aus Matfiles geholt werden
//#define DS_PAR_USE_MEXLOAD     1                // Parameter über mex-Schnittstelle in einer Struktur 
//#define DS_OUT_USE_MEXWRITE    1                // Parameter über mex-Schnittstelle in duh-Struktur zurückgeben


// Debugmode-Einstellung
//----------------------
#if defined _DEBUG
    #if !defined DS_DEBUG
        #define DS_DEBUG
    #endif
    #if defined DS_RELEASE
        #undef DS_RELEASE
    #endif
#endif
#if defined NDEBUG
    #if defined DS_DEBUG
        #undef DS_DEBUG
    #endif
    #if !defined DS_RELEASE
        #define DS_RELEASE
    #endif
#endif
#if !defined NDEBUG && !defined _DEBUG
    #error NDEBUG und _DEBUG sind nicht defniert
#endif

// DS_DEBUG_MODE, bitweise auslesen
// 1: Debug mode init-Ausgabe
// 2: Zustände werden mit ausgegeben
//==================================
#ifdef DS_DEBUG

    #define DS_DEBUG_MODE  1
#else

    #define DS_DEBUG_MODE  0
#endif

  
// Damit können Parameter aus Dateien (bisher matlat)
// mit mit ParFile eingeladen werden DsParFileExternData.cpp
//----------------------------------------------------------
#ifndef DS_PAR_USE_FILELOAD
    #define DS_PAR_USE_FILELOAD         0
#endif
// Damit können Parameter über Matlab weitergereicht werden
// Dazu wird in Matlab mit den m-Funktionen ds_set_par, ds_set_par_single,...
// die Parameter gesetzt werden
//----------------------------------------------------------
#ifndef DS_PAR_USE_MEXLOAD
    #define DS_PAR_USE_MEXLOAD         0
#endif

// Damit kann der Output an matlab weitergereicht werden
// Der Output wird dann in einer duh-Struktur zurückgegeben
//---------------------------------------------------------
#ifndef DS_OUT_USE_MEXWRITE
    #define DS_OUT_USE_MEXWRITE         0
#endif

// neue Berechnung der States in DsMod (bisher noch nicht fertig)
//---------------------------------------------------------------
#ifndef DS_MOD_NEW_STA
    #define DS_MOD_NEW_STA              1
#endif

// Die einzelnen Integratoren können abgeschaltet werden
//------------------------------------------------------
#if DS_MOD_NEW_STA == 1 // nur neue Möglichkeiten 

    #define DS_MOD_INTEGRATION_PIEULER 0
    #define DS_MOD_INTEGRATION_PGEARS  0
    #define DS_MOD_INTEGRATION_IEULER  0
    #define DS_MOD_INTEGRATION_GEARS   0
    #define DS_MOD_INTEGRATION_DOPRI45 0
    #define DS_MOD_INTEGRATION_RADAU   1
    #define DS_MOD_INTEGRATION_LSODA   0

#endif