//
// Einfache Euler-Klasse, der den expliziten Euler eines Zustandes rechnet.
// Die set-Funktion errechnet den neuen Wert aus dem Gradienten y(i+1) = y(i) + (tneu-talt)* yd
// Es können mit der get-Funktion zeitlich lineare Zwischenwerte geholt werden
// Es kann auch als eine einfache PT1-Filterfunktion verwendet werden
// Die set-Funktion errechnet den neuen Wert aus y(i+1) = y(i) * lambda + x(i+1) * (1-lambda),
// mit lambda = exp(-(tneu-talt)/time_const)
//
// Beispiel Gardient
//
// CSlfEuler Z;
// double    ystart = 2.,t = 0.0, dt = 0.1,yd;
// double    y1,y2,y3;
// 
// Z.initGrad(t,ystart);             // Initialisieren
// 
// for(i=0;i<100;i++) {
//     t += dt;
//     yd = sin(20*t);
//     y2 = Z.setGrad(t,yd);     // gibt den neuen Wert zurück
//
//     y1 = Z.get(t-dt*0.5);     // wird linear interpoliert
//     y3 = Z.get(t+dt);         // wird linear extrapoliert
// }
//
// Beispiel PT1
//
// CSlfEuler Z;
// double    ystart = 2.,t = 0.0, dt = 0.1,yneu;
// double    y1,y2,y3;
// 
// Z.initPT1(t,ystart,0.2);             // Initialisieren mit T1 = 0.2 sec
// 
// for(i=0;i<100;i++) {
//     t += dt;
//     yneu = 100*sin(0.2*t);
//     yfilt2 = Z.setGrad(t,yneu);     // gibt den neuen Wert zurück
//
//     yfilt1 = Z.get(t-dt*0.5);     // wird linear interpoliert
//     yfilt3 = Z.get(t+dt);         // wird linear extrapoliert
// }
#ifndef SLF_EULER_H_INCLUDED
#define SLF_EULER_H_INCLUDED

#include "SlfBasic.h"

#define SLF_EULER_PT1      1
#define SLF_EULER_GRAD     2

class CSlfEuler {
public:

    CSlfEuler();
    ~CSlfEuler();

    void initGrad(double tstart, double y0);
    void initPT1(double tstart, double y0,double time_const);

    double setGrad(double time_next,double ygrad);
    double setPT1(double time_next,double ynext);

    double get(double time);
    void get(double time,double *yfilt);

private:
    
    uint8_t      Type;
    double       yLast;
    double       tLast;
    double       yNew;
    double       tNew;
    double       TimeConst;
    double       Lambda;
    double       EinsMLambda;
    double       Dt;

};

#endif