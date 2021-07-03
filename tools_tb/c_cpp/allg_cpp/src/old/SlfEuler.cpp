#include <math.h>
#include "SlfEuler.h"

#define SLF_EULER_PT1      1
#define SLF_EULER_GRAD     2


//=================================================
// Konstruktor/Destruktor
//=================================================
CSlfEuler::CSlfEuler(void) {

    Type = SLF_EULER_GRAD;
    yLast = 0.0;
    tLast = 0.0;
    yNew  = 0.0;
    tNew  = 0.0;
    TimeConst = 0.1;
    Dt        = 0.0;

}
CSlfEuler::~CSlfEuler(void) {


}
//=================================================
// init
//=================================================
void CSlfEuler::initGrad(double tstart, double y0) {

    Type  = SLF_EULER_GRAD;
    yLast = y0;
    tLast = tstart;
    yNew  = y0;
    tNew  = tstart;
}
void CSlfEuler::initPT1(double tstart, double y0, double time_const) {

    Type  = SLF_EULER_PT1;
    yLast = y0-EPSILON;
    tLast = tstart;
    yNew  = y0;
    tNew  = tstart;

    TimeConst   = NOT_ZERO(time_const);
    Dt          = EPSILON;
    Lambda      = exp(-Dt/TimeConst);
    EinsMLambda = 1. - Lambda;
}
//=================================================
// set
//=================================================
double CSlfEuler::setGrad(double time_next,double ygrad) {

    double dt = time_next-tNew;
    if( dt > EPSILON ) {

        yLast = yNew;
        yNew  += ygrad * dt;

        tLast = tNew;
        tNew  = time_next;
        Dt    = dt;
    } else {
        
        dt = tLast - time_next;
        if( dt > EPSILON ) {

            yNew  = yLast;
            yLast -= ygrad * dt;
        
            tNew  = tLast;
            tLast = time_next;
            Dt    = dt;

        } else if( dt < -EPSILON ) {

            yNew = yLast - ygrad * dt;
            tNew = time_next;
            Dt   = -dt;
        }
    }
    return yNew;
}
double CSlfEuler::setPT1(double time_next,double ynext) {

    double dt = time_next-tNew;
    
    if( dt > EPSILON ) {

        if( dt != Dt ) {
            Dt          = dt;
            Lambda      = exp(-Dt/TimeConst);
            EinsMLambda = 1. - Lambda;
        }


        yLast = yNew;
        yNew  = yLast * Lambda + ynext * EinsMLambda;

        tLast = tNew;
        tNew  = time_next;
    } else {

        dt = time_next-tLast;
        if( dt > EPSILON ) {

            if( dt != Dt ) {
                Dt          = dt;
                Lambda      = exp(-Dt/TimeConst);
                EinsMLambda = 1. - Lambda;
            }


            yNew  = yLast * Lambda + ynext * EinsMLambda;

            tNew  = time_next;

        } else {

            double y  = yLast;
            yLast -= (yNew-yLast) * (tLast-time_next);
            yNew  = y;
        
            tNew  = tLast;
            tLast = time_next;
            
        }
    }

    return yNew;
}
//=================================================
// get
//=================================================
double CSlfEuler::get(double time) {
    return yLast + (yNew-yLast)/NOT_ZERO(Dt)*(time-tLast);
}
void CSlfEuler::get(double time,double *yfilt) {
    *yfilt = yLast + (yNew-yLast)/NOT_ZERO(Dt)*(time-tLast);
}
