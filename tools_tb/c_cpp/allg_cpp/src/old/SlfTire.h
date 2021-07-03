// Schupfberechnung und statische Reifenkraftberechnung
// nach Rill, HSRI, ...
//
#ifndef SLF_TIRE_H_INCLUDED
#define SLF_TIRE_H_INCLUDED

#include "SlfBasic.h"
#include "SlfNum.h"
 
#define SLF_TIRE_INDEX_MAX 4

#define SLF_TIRE_USE_RILL_MODEL 1
#define SLF_TIRE_USE_HSRI_MODEL 1


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
typedef error_t (*FcnRoadH)(double *px, double *py, double *pz,uint8_t *pindex);

//===========================================================
//===========================================================
//===========================================================
// Tire Calculation
// -------------------------
//===========================================================
//===========================================================
//===========================================================

//===========================================================
// Inputstruktur
//===========================================================
typedef struct SSlfTireInp_tag {

    Vector_t   r0M_0;                // Radmittelpunkt in 0-Koordinaten
    Vector_t   eyR_0;                // Einheitsvektor Radebene
    Matrix_t   A0K;                  // Drehmatrix Radkoordinatensystem in Ursprung
    Vector_t   v0M_0;                // Geschwindigkeit des Radmittelpunktes
    Vector_t   om0RK_0;              // Drehbewegung Radkörper
    double   om;                 // rad/s Raddrehwinkelgeschwindigkeit
    double   xe;                 // Verschiebung in x-Richtung Radebene
    double   ye;                 // Verschiebung in y-Richtung Radebene
    double   Fz;                 // Radaufstandskraft, wenn aus Eingang benutzt, (keine Vertikaldynamik)


} SSlfTireInp;

//===========================================================
// Oututstruktur
//===========================================================
typedef struct SSlfTireOut_tag {
    uint8_t    index;
    Vector_t   r0P_0;                // Radaufstandspunkt in Ursprungskoordinaten
    Vector_t   enS_0;                // Straßennormalenvektor
    Vector_t   exS_0;                // Längsrichtungsvektor Radebene
    Vector_t   eyS_0;                // Querrichtungsvektor Radebene
    Vector_t   ezR_0;                // senkrecht zu Straße x und in Radebene
    double   gam;                // Sturzwinkel zur Straße
    double   RS;                 // eingefederter Weg in Radebene
    double   dz;                 // Einfederweg
    double   dzp;                // Einfedergeschwindigkeit
    double   omB;                // Bohrdrehbewegung
    double   vom;                // Radgeschwindigkeit aus omega in x-Richtung Radebene-Straße
    double   vx;                 // Geschwindigkeit in x-Richtung Radebene-Straße
    double   vy;                 // Geschwindigkeit in y-Richtung Radebene-Straße
    double   RD;                 // dynamischer Rollradius
    double   RB;                 // Bohrradius
    double   L;                  // Latschlänge
    double   sxR;                // Längsschlupf Rill-Definition
    double   syR;                // Querschlupf  Rill-Definition
    double   sxH;                // Längsschlupf HSRI-Definition
    double   syH;                // Querschlupf  HSRI-Definition
    double   sB;                 // Bohrschlupf
    double   dsxdvx;
    double   dsxdom;
    double   dsydvx;
    double   dsydvy;
    double   dsydom;
    double   Fx_s;               // Längskraft statisch
    double   Fx;                 // Längskraft dynamisch
    double   Fy_s;               // Querkraft statisch
    double   Fy;                 // Querkraft dynamisch
    double   Fz;                 // Aufstandskraft
    double   Mz_s;               // Rückstellmoment
    double   Mz;                 // Rückstellmoment
    double   MB;                 // Bohrmoment
    double   dFxdsx;             // Ableitung Kraft nach Schlupf längs
    double   dFydsy;             // Ableitung Kraft nach Schlupf quer
    double   dMzdsy;             // Ableitung Rückstellmoment nach Schlupf quer
    double   dMBdsB;             // Ableitung des Bohrschlupfes

    double   xpe;                // Ableitung d(xe)/dt
    double   ype;                // Ableitung d(ye)/dt
    double   dxpedxe;            // Ableitung d(xpe)/d(xe)
    double   dypedye;            // Ableitung d(xpe)/d(xe)

}SSlfTireOut;
//===========================================================
// Parameterstruktur
//===========================================================
typedef struct SSlfTirePar_tag {

    // Straßenerhebung, Einfederun
    //============================
    uint8_t      calc_Fz;           // 0: wird nicht berechnet, kommt vomm input
                                  // 1: wird berechnet, d.h Vertikaldynamik
    uint8_t      calc_road;         // 0: flache Ebene mit Höhe h0
                                  // 1: Straßenhöhe berechnen mit z=fRoad(x,y)+h0
                                  //    (Aufruf FcnRoadH(px,py,pz) Rückgabewert: *pz)
    FcnRoadH   fcnroadh;          // Funktionspointer auf Roadfunktion
    double     h0;                // Grundhöhe
    double     L0;                // Latschlänge bei Nennlast
    double     B0;                // Reifenbreite
    double     R0;                // unbelasteter Reifendurchmesser
    uint8_t      calc_RD;           // dynamischer Rollradius berechnen, ansonsten Konstante
    double     RD;                // konstanter dynamischer Rollradius
    uint8_t      calc_L;            // Latschlänge berechnen, ansosnten L0
    double     cx;                // Reifensteifigkeit längs
    double     dx;                // Reifendämpfung längs
    double     cy;                // Reifensteifigkeit quer
    double     dy;                // Reifendämpfung    quer
    double     cz;                // Reifensteifigkeit vertikal
    double     czexp;             // Exponent Reifenfeder
    double     dz;                // Reifendämpfung


    // Parameter Schlupfberechnung
    //============================
    double vN;                     // minimale Geschwindigkeit, zur Verhinderung Singularität
    double fstoch;                 // -     Gleichverteilte stochastisch
                                   //       Änderung 0.01 ist 1 %

    uint8_t   calc_tire;             // 0: HSRI, 1: Rill

    //Parametre für Rillmodell
    //========================
#if SLF_TIRE_USE_RILL_MODEL != 0

    uint8_t   force_type;      // 0: Reibwerte vorgegeben
                             // 1: Kraftwerte vorgegeben
    Vector_t *pdfx0_1;         // [-] Anfangssteigung fx/s zu Last P1 und n-verschieden
    Vector_t *psxmax_1;        // [-] x-Schulpf im Maximum fxmax                          
    Vector_t *pfxmax_1;        // [-] Maximum fxmax
    Vector_t *psxslide_1;      // [-] x-Schulpf im Gleiten fxslide                          
    Vector_t *pfxslide_1;      // [-] Gleiten fxslide
    Vector_t *pdfy0_1;         // [-] Anfangssteigung fy/s zu Last P1 und n-verschieden
    Vector_t *psymax_1;        // [-] y-Schulpf im Maximum fymax                          
    Vector_t *pfymax_1;        // [-] Maximum fymax
    Vector_t *psyslide_1;      // [-] y-Schulpf im Gleiten fxslide                          
    Vector_t *pfyslide_1;      // [-] Gleiten fyslide
    Vector_t *pntol0_1;        // [-] bezogener Nachlauf bei sy = 0
    Vector_t *psyn0_1;         // [-] Nachlauf wechselt Vorzeichen
    Vector_t *psyns_1;         // [-] Nachlauf wird ab hier null
    Vector_t *pP_1;            // [N] Last 1

    Vector_t *pdfx0_2;         // [-] Anfangssteigung fx/s zu Last P2 und n-verschieden
    Vector_t *psxmax_2;        // [-] x-Schulpf im Maximum fxmax                          
    Vector_t *pfxmax_2;        // [-] Maximum fxmax
    Vector_t *psxslide_2;      // [-] x-Schulpf im Gleiten fxslide                          
    Vector_t *pfxslide_2;      // [-] Gleiten fxslide
    Vector_t *pdfy0_2;         // [-] Anfangssteigung fy/s zu Last P2 und n-verschieden
    Vector_t *psymax_2;        // [-] y-Schulpf im Maximum fymax                          
    Vector_t *pfymax_2;        // [-] Maximum fymax
    Vector_t *psyslide_2;      // [-] y-Schulpf im Gleiten fxslide                          
    Vector_t *pfyslide_2;      // [-] Gleiten fyslide
    Vector_t *pntol0_2;        // [-] bezogener Nachlauf bei sy = 0
    Vector_t *psyn0_2;         // [-] Nachlauf wechselt Vorzeichen
    Vector_t *psyns_2;         // [-] Nachlauf wird ab hier null
    Vector_t *pP_2;            // [N] Last  2
#endif

#if SLF_TIRE_USE_HSRI_MODEL != 0

    Vector_t *pCs;            // [N]   beginning gradient of long. characteristcs if slip=0"
    Vector_t *pCalpha;        // [N]   beginning gradient of lateral characteristcs if slip=0
    Vector_t *pf0;            // [-]   potential fricional connection if slide_velocity=0
    Vector_t *pk;             // [s/m] fricional connection decrease if forward velocity increase
#endif

} SSlfTirePar;

//===========================================================
// Horizontal: Gesamtstruktur
//===========================================================
typedef struct SSlfTire_tag {


    // Parameter
    SSlfTirePar p;
    SSlfTireInp i;
    SSlfTireOut o;

    // Aufstandspunkt
    double     dx_road;   // Ausdehnung dx um Reifenmitte für Straßenebene
    double     dy_road;   // Ausdehnung dy um Reifenmitte für Straßenebene

    uint8_t      dyn_x;     // dynamic flag x-Richtung
    uint8_t      dyn_y;     // dynamic flag y-Richtung

    //Kraftberechnung 
    uint32_t nfrict;         // Anzahl der Reifenkennungen

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

    double   fmaxs_act;
    double   f;                   // aktuelle Kraftschlußausnutzung
    double   dfdsR_norm;          // Änderung nach generalöisertem Schlupf
    double   nMz;                 // aktueller Nachlauf
    double   dnMzdsy;               // Ableitung Nachlauf
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
// Funktionen
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
error_t SlfTire_init(SSlfTire *ps);
error_t SlfTireRill_init(SSlfTire *ps);
error_t SlfTireHSRI_init(SSlfTire *ps);
error_t SlfTireVert_init(SSlfTire *ps);
//
// Berechnung
//
error_t SlfTire(SSlfTire *ps);                  // Tirestruktur
                
// Berechnung Einfederung, dynamischer Rollradius, Latschlänge Radaufstandskraft
//
error_t SlfTireVert(SSlfTire *ps);                  // Tirestruktur
// Reifenkraftberechnung
//
error_t SlfTireRill(SSlfTire *ps);                  // Tirestruktur
error_t SlfTireHSRI(SSlfTire *ps);                 // Tirestruktur
//
// Dynamik
//
error_t SlfTireDynamic(SSlfTire *ps);                   

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
void SlfTireSchlupfRill(SSlfTire *ps);

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
void SlfTireSchlupfHSRI(SSlfTire *ps);

// Gibt eine Fehlermeldung zurück
char *SlfTireError(error_t ierr);

#ifdef  __cplusplus
}
#endif





#endif