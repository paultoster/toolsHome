// Schupfberechnung und statische Reifenkraftberechnung
// nach Rill, HSRI, ...
//
#ifndef SLF_TIRE_H_INCLUDED
#define SLF_TIRE_H_INCLUDED

#include "SlfBasic.h"
#include "SlfNum.h"
 
#define SLF_TIRE_INDEX_MAX 4

#define SLF_TIRE_USE_RILL_MODEL 1


#ifdef __cplusplus
  extern "C" {
#endif

//===========================================================
//===========================================================
//===========================================================
// Vertical Tire Calculation
// -------------------------
// Contactpoint, dynamic Radius, contact patch length
//===========================================================
//===========================================================
//===========================================================
typedef error_t (*FcnRoadH)(double *x, double *y, double *z);

//===========================================================
// Vertical Parameterstruktur
//===========================================================
typedef struct SSlfTireVertPar_tag {

    uint8      calc_road;         // 0: flache Ebene mit Höhe h0
                                  // 1: Straßenhöhe berechnen mit z=fRoad(x,y)+h0
                                  //    (Aufruf FcnRoadH(px,py,pz) Rückgabewert: *pz)
    double     h0;                // Grundhöhe
    double     
    FcnRoadH   fcnroadh;          // Funktionspointer auf Roadfunktion


} SSlfTireVertPar;
//===========================================================
// Vertical: Gesamtstruktur
//===========================================================
typedef struct SSlfTireVert_tag {

    SSlfTireVertPar p;
   
} SSlfTireVert;
//===========================================================
// Vertical: Funktionen
//===========================================================
//
// Inizialisierung
//
error_t SlfTireVert_init(SSlfTireVert *ps);           // TireVertstruktur
//
// Berechnung
//
error_t SlfTireVert(SSlfTireVert *ps                  // TireVertstruktur
                   ,Vector       r0M_0                // Radmittelpunkt in 0-Koordinaten
                   ,Vector       eyR_0                // Einheitsvektor Radebene
                   ,Vector       v0M_0                // Geschwindigkeit des Radmittelpunktes

                                                      // Output
                   ,double       dz                   // Einfederung
                   ,double       dzp                  // Einfedergeschwindigkeit
                   );

//===========================================================
//===========================================================
//===========================================================
// Horizontal Tire Calculation
// -------------------------
// Slip, Force and Torque
//===========================================================
//===========================================================
//===========================================================

//===========================================================
// Horizontal Parameterstruktur
//===========================================================
typedef struct SSlfTirePar_tag {

    // Parameter Schlupfberechnung
    //============================
    double vN;              // minimale Geschwindigkeit, zur Verhinderung Singularität

    //Parametre für Rillmodell
    //========================
#if SLF_TIRE_USE_RILL_MODEL != 0

    uint8   calctyp;         // 0: Rill
    uint8   forcetype;       // 0: Reibwerte vorgegeben
                             // 1: Kraftwerte vorgegeben
    Vector *pdfx0_1;         // [-] Anfangssteigung fx/s zu Last P1 und n-verschieden
    Vector *psxmax_1;        // [-] x-Schulpf im Maximum fxmax                          
    Vector *pfxmax_1;        // [-] Maximum fxmax
    Vector *psxslide_1;      // [-] x-Schulpf im Gleiten fxslide                          
    Vector *pfxslide_1;      // [-] Gleiten fxslide
    Vector *pdfy0_1;         // [-] Anfangssteigung fy/s zu Last P1 und n-verschieden
    Vector *psymax_1;        // [-] y-Schulpf im Maximum fymax                          
    Vector *pfymax_1;        // [-] Maximum fymax
    Vector *psyslide_1;      // [-] y-Schulpf im Gleiten fxslide                          
    Vector *pfyslide_1;      // [-] Gleiten fyslide
    Vector *pntol0_1;        // [-] bezogener Nachlauf bei sy = 0
    Vector *psyn0_1;         // [-] Nachlauf wechselt Vorzeichen
    Vector *psyns_1;         // [-] Nachlauf wird ab hier null
    Vector *pP_1;            // [N] Last 1

    Vector *pdfx0_2;         // [-] Anfangssteigung fx/s zu Last P2 und n-verschieden
    Vector *psxmax_2;        // [-] x-Schulpf im Maximum fxmax                          
    Vector *pfxmax_2;        // [-] Maximum fxmax
    Vector *psxslide_2;      // [-] x-Schulpf im Gleiten fxslide                          
    Vector *pfxslide_2;      // [-] Gleiten fxslide
    Vector *pdfy0_2;         // [-] Anfangssteigung fy/s zu Last P2 und n-verschieden
    Vector *psymax_2;        // [-] y-Schulpf im Maximum fymax                          
    Vector *pfymax_2;        // [-] Maximum fymax
    Vector *psyslide_2;      // [-] y-Schulpf im Gleiten fxslide                          
    Vector *pfyslide_2;      // [-] Gleiten fyslide
    Vector *pntol0_2;        // [-] bezogener Nachlauf bei sy = 0
    Vector *psyn0_2;         // [-] Nachlauf wechselt Vorzeichen
    Vector *psyns_2;         // [-] Nachlauf wird ab hier null
    Vector *pP_2;            // [N] Last  2
#endif
} SSlfTirePar;
//===========================================================
// Horizontal: Gesamtstruktur
//===========================================================
typedef struct SSlfTire_tag {


    // Parameter
    SSlfTirePar p;

    //Kraftberechnung 
    uint32 nfrict;         // Anzahl der Reifenkennungen

#if SLF_TIRE_USE_RILL_MODEL != 0

    // Skalierungsfaktoren auf Last P1,P2
    double a_dfx0[SLF_TIRE_INDEX_MAX];          // Anfangssteiung Reibkennlinie
    double b_dfx0[SLF_TIRE_INDEX_MAX];          // Anfangssteiung Reibkennlinie
    double d_sxmax[SLF_TIRE_INDEX_MAX];         // Schlupf bei Max-Reibwert
    double e_sxmax[SLF_TIRE_INDEX_MAX];         // Schlupf bei Max-Reibwert
    double a_fxmax[SLF_TIRE_INDEX_MAX];         // Max-Reibwert
    double b_fxmax[SLF_TIRE_INDEX_MAX];         // Max-Reibwert
    double d_sxslide[SLF_TIRE_INDEX_MAX];       // Schlupf bei Beginn Gleiten
    double e_sxslide[SLF_TIRE_INDEX_MAX];       // Schlupf bei Beginn Gleiten
    double a_fxslide[SLF_TIRE_INDEX_MAX];       // Gleitreibwert
    double b_fxslide[SLF_TIRE_INDEX_MAX];       // Gleitreibwert
                             // Querkraft
    double a_dfy0[SLF_TIRE_INDEX_MAX];          // Anfangssteiung Reibkennlinie
    double b_dfy0[SLF_TIRE_INDEX_MAX];          // Anfangssteiung Reibkennlinie
    double d_symax[SLF_TIRE_INDEX_MAX];         // Schlupf bei Max-Reibwert
    double e_symax[SLF_TIRE_INDEX_MAX];         // Schlupf bei Max-Reibwert
    double a_fymax[SLF_TIRE_INDEX_MAX];         // Max-Reibwert
    double b_fymax[SLF_TIRE_INDEX_MAX];         // Max-Reibwert
    double d_syslide[SLF_TIRE_INDEX_MAX];       // Schlupf bei Beginn Gleiten
    double e_syslide[SLF_TIRE_INDEX_MAX];       // Schlupf bei Beginn Gleiten
    double a_fyslide[SLF_TIRE_INDEX_MAX];       // Gleitreibwert
    double b_fyslide[SLF_TIRE_INDEX_MAX];       // Gleitreibwert

	double d_ntol0[SLF_TIRE_INDEX_MAX];         // Nachlauf
	double e_ntol0[SLF_TIRE_INDEX_MAX];         // Nachlauf
	double d_syn0[SLF_TIRE_INDEX_MAX];          // Schlupf n=0 Nachlauf
	double e_syn0[SLF_TIRE_INDEX_MAX];          // Schlupf n=0 Nachlauf
	double d_syns[SLF_TIRE_INDEX_MAX];          // 2. Schlupf n=0 Nachlauf
	double e_syns[SLF_TIRE_INDEX_MAX];          // 2. Schlupf n=0 Nachlauf

    double dfx0_act;         // [-] aktuelle Anfangssteigung 
    double sxmax_act;        // [-] aktuelle x-Schulpf im Maximum fxmax                          
    double fxmax_act;        // [-] aktuelle Maximum fxmax
    double sxslide_act;      // [-] aktuelle x-Schulpf im Gleiten fxslide                          
    double fxslide_act;      // [-] aktuelle Gleiten fxslide
    double dfy0_act;         // [-] aktuelle Anfangssteigung 
    double symax_act;        // [-] aktuelle y-Schulpf im Maximum fymax                          
    double fymax_act;        // [-] aktuelle Maximum fymax
    double syslide_act;      // [-] aktuelle y-Schulpf im Gleiten fxslide                          
    double fyslide_act;      // [-] aktuelle Gleiten fyslide

    double df0_act;         // [-] aktuelle Anfangssteigung gesamt
    double smax_act;        // [-] aktuelle Schulpf im Maximum fmax                          
    double fmax_act;        // [-] aktuelle Maximum fmax
    double sslide_act;      // [-] aktuelle Schulpf im Gleiten fslide                          
    double fslide_act;      // [-] aktuelle Gleiten fslide

    double ntol0_act;        // [-] relativer Nachlauf bei sy=0
    double syn0_act;        // [-] Schlup, beim dem Nachlauf durch null geht
    double syns_act;        // [-] Schlup, beim dem Nachlauf null bleibt


    // Schlüpfe
    double sxR_norm;         // normalisierter Schlupf Längs
    double syR_norm;         // normalisierter Schlupf quer
    double sR_norm;          // normalisierter Schlupf gesamt
    double sx_dach;          // Normalisierungsschlupf längs
    double sy_dach;          // Normalisierungsschlupf quer
    double cphi;             // Winkelbedingung cos(phi)
    double sphi;             // Winkelbedingung sin(phi)
 
#endif



    double   dsxdvx;
    double   dsxdom;
    double   dsydvx;
    double   dsydvy;
    double   dsydom;

    double   fmaxs_act;
    double   f;                   // aktuelle Kraftschlußausnutzung
    double   dfdsR_norm;          // Änderung nach generalöisertem Schlupf
    double   nMz;                 // aktueller Nachlauf
    double   dFxdsxR;             // aktuelle Ableitung Fx nach sxR
    double   dFxdsyR;             // aktuelle Ableitung Fx nach syR
    double   dFydsxR;             // aktuelle Ableitung Fy nach sxR
    double   dFydsyR;             // aktuelle Ableitung Fy nach syR
    double   dFxdsxH;             // aktuelle Ableitung Fx nach sxR
    double   dFxdsyH;             // aktuelle Ableitung Fx nach syR
    double   dFydsxH;             // aktuelle Ableitung Fy nach sxR
    double   dFydsyH;             // aktuelle Ableitung Fy nach syR
    double   dFxdvx;              // aktuelle Ableitung Fx nach vx
    double   dFxdvy;              // aktuelle Ableitung Fx nach vy
    double   dFxdom;              // aktuelle Ableitung Fx nach om
    double   dFydvx;              // aktuelle Ableitung Fy nach sxR
    double   dFydvy;              // aktuelle Ableitung Fy nach syR
    double   dFydom;              // aktuelle Ableitung Fx nach om
   
} SSlfTire;

//===========================================================
// Horizontal: Funktionen
//===========================================================
//
// initialisiereung des Reifenmodells
//========================================
// Rückgabe 0: okay
//          1: vN < epsilon
//          2: nfrict > SLF_TIRE_INDEX_MAX, d.h zu wenig array angelegt(nicht dynamisch)
//          3: Radlasten P1,P2 sind null
//          4: falscher calctype
//  
error_t SlfTire_init(SSlfTire *ps);           // Tirestruktur
//
// Berechnung
//
error_t SlfTire(SSlfTire *ps                  // Tirestruktur
               ,uint32   index                // Reifenindex, welcher Datensatz
               ,double   *pvx                  // m/s   Längsgeschwindigkeit
               ,double   *pvy                  // m/s   Quergeschwindigkeit
               ,double   *pom                  // rad/s Raddrehwinkelgeschwindigkeit
               ,double   *prdyn                // m     dynamischer Rollradius
               ,double   *pP                   // N     Radlast
               ,double   *pL                   // m     Latschlänge
               ,double   *pfstoch              // -     Gleichverteilte stochastisch
                                               //       Änderung 0.01 ist 1 %
                                              // Output
               ,double   *psxR                 // Längsschlupf Rill-Definition
               ,double   *psyR                 // Querschlupf  Rill-Definition
               ,double   *psxH                 // Längsschlupf HSRI-Definition
               ,double   *psyH                 // Querschlupf  HSRI-Definition
               ,double   *pFx                  // Längskraft
               ,double   *pFy                  // Querkraft
               ,double   *pMz                  // Rückstellmoment
               ,double   *pdFxdsx              // Ableitung Kraft nach Schlupf längs
               ,double   *pdFydsy              // Ableitung Kraft nach Schlupf quer
               );

// Schlupfberechnung Rill
// Berechnung Schlupf
// Input
// ps->sx_dach
// ps->sy_dach
// pvx
// pvy
// prdyn
// pom
//
// Output
// psxR
// psyR
// ps->sxR_norm
// ps->syR_norm
// ps->sR_norm
// ps->dsxdvx
// ps->dsxdom
// ps->dsydvx
// ps->dsydvy
// ps->dsydom
//
void SlfTireSchlupfRill(SSlfTire *ps
                       ,double *pvx
                       ,double *pvy
                       ,double *prdyn
                       ,double *pom
                       ,double *pvom
                       ,double *psxR
                       ,double *psyR);

// Schlupfberechnung HSRI
// Berechnung Schlupf
// Input
// pvx
// pvy
// prdyn
// pom
//
// Output
// psxH
// psyH
// ps->dsxdvx
// ps->dsxdom
// ps->dsydvx
// ps->dsydvy
// ps->dsydom
//
void SlfTireSchlupfHSRI(SSlfTire *ps
                       ,double *pvx
                       ,double *pvy
                       ,double *prdyn
                       ,double *pom
                       ,double *pvom
                       ,double *psxR
                       ,double *psyR);

// Gibt eine Fehlermeldung zurück
char *SlfTireError(error_t ierr);

#ifdef  __cplusplus
}
#endif





#endif