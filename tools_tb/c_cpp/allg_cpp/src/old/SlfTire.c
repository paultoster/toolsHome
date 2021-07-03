#include <math.h>
#include <stdlib.h>
#include "SlfTire.h"
#include "SlfNum.h"

//=================================================
//=================================================
// Initialisieren
//=================================================
//==================================================
error_t SlfTire_init(SSlfTire *ps) { // Tirestruktur

    error_t ierr=0;

    // Reifenbreite
    ps->p.B0 = fabs(ps->p.B0);
    // Nennlatschlänge
    ps->p.L0 = fabs(ps->p.L0);

    // Vertikales Reifenmodell
    if( (ierr=SlfTireVert_init(ps)) )
        return ierr;    

    // Horizontales statisches Reifenmodell
    if(ps->p.calc_tire == 0 ) {
        if( (ierr=SlfTireHSRI_init(ps)) )
            return ierr;
    } else if( ps->p.calc_tire == 1 ) {
        if( (ierr=SlfTireRill_init(ps)) )
            return ierr;
    } else {
        return 4;
    }

    // cx,dx,cy,dy
    //============
    ps->p.cx = fabs(ps->p.cx);
    ps->p.dx = fabs(ps->p.dx);
    ps->p.cy = fabs(ps->p.cy);
    ps->p.dy = fabs(ps->p.dy);

    if( ps->p.dx < EPSILON )
        ps->dyn_x = 0;
    else
        ps->dyn_x = 1;

    if( ps->p.dy < EPSILON )
        ps->dyn_y = 0;
    else
        ps->dyn_y = 1;

    return ierr;
}
//==================================================================================
// Vertikalnmodell
//==================================================================================
error_t SlfTireVert_init(SSlfTire *ps) { // Tirestruktur
    
    if( ps->p.calc_Fz ) {
        // Road-Modul
        if( ps->p.calc_road == 1 ) {
            double x=0,y=0,z;
            uint8_t  ui8;
        
            // kleiner Test
            ps->p.fcnroadh(&x,&y,&z,&ui8);

            // Ausdehnung dx,dy um Reifenmitte für Straßenebene
            ps->dx_road = ps->p.L0/2.;
            ps->dy_road = ps->p.B0/2.;


        } else {
            ps->p.calc_road = 0;
        }
    }

    
    // freier Radius, dynamischer Rollradius
    //======================================
    ps->p.R0 = fabs(ps->p.R0);
    if( ps->p.R0 < EPSILON )
        return 5; 

    // RD Soll konstant gerechnet werden
    if( !ps->p.calc_RD ) {
        ps->p.RD = fabs(ps->p.RD);
        if( ps->p.RD < EPSILON )
            return 6;
    }

    // Aufstandskraft
    //===============
    ps->p.cz    = fabs(ps->p.cz);
    ps->p.czexp = NOT_ZERO(fabs(ps->p.czexp));
    ps->p.dz    = fabs(ps->p.dz);

    return 0;
}
//==================================================================================
// Rillreifenmodell
//==================================================================================
error_t SlfTireRill_init(SSlfTire *ps) { // Tirestruktur

    uint32_t i;
    double dP;
    Vector_t P_1         = *ps->p.pP_1;
    Vector_t dfx0_1      = *ps->p.pdfx0_1;
    Vector_t sxmax_1     = *ps->p.psxmax_1;
    Vector_t fxmax_1     = *ps->p.pfxmax_1;
    Vector_t sxslide_1   = *ps->p.psxslide_1;
    Vector_t fxslide_1   = *ps->p.pfxslide_1;
    Vector_t dfy0_1      = *ps->p.pdfy0_1;
    Vector_t symax_1     = *ps->p.psymax_1;
    Vector_t fymax_1     = *ps->p.pfymax_1;
    Vector_t syslide_1   = *ps->p.psyslide_1;
    Vector_t fyslide_1   = *ps->p.pfyslide_1;
    Vector_t ntol0_1     = *ps->p.pntol0_1;
    Vector_t syn0_1      = *ps->p.psyn0_1;
    Vector_t syns_1      = *ps->p.psyns_1;

    Vector_t P_2         = *ps->p.pP_2;
    Vector_t dfx0_2      = *ps->p.pdfx0_2;
    Vector_t sxmax_2     = *ps->p.psxmax_2;
    Vector_t fxmax_2     = *ps->p.pfxmax_2;
    Vector_t sxslide_2   = *ps->p.psxslide_2;
    Vector_t fxslide_2   = *ps->p.pfxslide_2;
    Vector_t dfy0_2      = *ps->p.pdfy0_2;
    Vector_t symax_2     = *ps->p.psymax_2;
    Vector_t fymax_2     = *ps->p.pfymax_2;
    Vector_t syslide_2   = *ps->p.psyslide_2;
    Vector_t fyslide_2   = *ps->p.pfyslide_2;
    Vector_t ntol0_2     = *ps->p.pntol0_2;
    Vector_t syn0_2      = *ps->p.psyn0_2;
    Vector_t syns_2      = *ps->p.psyns_2;

    // Parameter initialisieren
    //=========================
    ps->p.vN     = fabs(ps->p.vN);
    if( ps->p.vN < EPSILON )
        return 1;

    ps->nfrict = GET_NROWS(*ps->p.pP_1);
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(dfx0_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(sxmax_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fxmax_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(sxslide_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fxslide_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(dfy0_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(symax_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fymax_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(syslide_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fyslide_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(ntol0_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(syn0_1));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(syns_1));

    ps->nfrict = MIN(ps->nfrict, GET_NROWS(P_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(dfx0_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(sxmax_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fxmax_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(sxslide_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fxslide_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(dfy0_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(symax_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fymax_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(syslide_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(fyslide_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(ntol0_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(syn0_2));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(syns_2));


    if( ps->nfrict > SLF_TIRE_INDEX_MAX )
        return 2;

    if( ps->p.force_type ) { // absolute Kraft

        for(i=0;i<ps->nfrict;i++) {

            if( P_1[i] < EPSILON || P_2[i] < EPSILON )
                return 3;


            // gleiche Last
            if( fabs(P_2[i]-P_1[i]) < EPSILON )
                dP = 1000;
            else
                dP = P_2[i]-P_1[i];
        
            ps->a_dfx0[i] = dfx0_2[i] / P_2[i] - dfx0_1[i]/P_1[i]/dP;
            ps->b_dfx0[i] = ( dfx0_1[i] / P_1[i] * P_2[i] 
                            - dfx0_2[i] / P_2[i] * P_1[i]
                            ) / dP;

            ps->d_sxmax[i] = ( sxmax_2[i] - sxmax_1[i] ) / dP ;
            ps->e_sxmax[i] = sxmax_1[i] - P_1[i]*( sxmax_2[i] - sxmax_1[i] ) / dP ;

            ps->a_fxmax[i] = fxmax_1[i]/ P_2[i] - fxmax_1[i]/ P_1[i]/dP;
            ps->b_fxmax[i] = ( fxmax_1[i]/ P_1[i]* P_2[i]
                             - fxmax_2[i]/ P_2[i]* P_1[i]
                             ) / dP;
            ps->d_sxslide[i] = ( sxslide_2[i] - sxslide_1[i] ) / dP ;
            ps->e_sxslide[i] = sxslide_1[i] - P_1[i]*( sxslide_2[i] - sxslide_1[i] ) / dP ;

            ps->a_fxslide[i] = fxslide_1[i]/P_2[i] - fxslide_1[i]/P_1[i]/dP;
            ps->b_fxslide[i] = ( fxslide_1[i]/P_1[i]*P_2[i] - fxslide_2[i]/P_2[i]*P_1[i] ) 
                             / dP;

            ps->a_dfy0[i] = dfy0_2[i]/P_2[i] - dfy0_1[i]/P_1[i]/dP;
            ps->b_dfy0[i] = ( dfy0_1[i]/P_1[i]*P_2[i] - dfy0_2[i]/P_2[i]*P_1[i] ) 
                         / dP;

            ps->d_symax[i] = ( symax_2[i] - symax_1[i] ) / dP ;
            ps->e_symax[i] = symax_1[i] - P_1[i]*( symax_2[i] - symax_1[i] ) / dP ;

            ps->a_fymax[i] = fymax_1[i]/P_2[i] - fymax_1[i]/P_1[i]/dP;
            ps->b_fymax[i] = ( fymax_1[i]/P_1[i]*P_2[i] - fymax_2[i]/P_2[i]*P_1[i] ) 
                          / dP;
            ps->d_syslide[i] = ( syslide_2[i] - syslide_1[i] ) / dP ;
            ps->e_syslide[i] = syslide_1[i] - P_1[i]*( syslide_2[i] - syslide_1[i] ) / dP ;

            ps->a_fyslide[i] = fyslide_1[i]/P_2[i] - fyslide_1[i]/P_1[i]/dP;
            ps->b_fyslide[i] = ( fyslide_1[i]/P_1[i]*P_2[i] - fyslide_2[i]/P_2[i]*P_1[i] ) 
                            / dP;
            ps->d_ntol0[i] = ( ntol0_2[i] - ntol0_1[i] ) / dP ;
            ps->e_ntol0[i] = ntol0_1[i] - P_1[i]*( ntol0_2[i] - ntol0_1[i] ) / dP ;

            ps->d_syn0[i] = ( syn0_2[i] - syn0_1[i] ) / dP ;
            ps->e_syn0[i] = syn0_1[i] - P_1[i]*( syn0_2[i] - syn0_1[i] ) / dP ;
 
            ps->d_syns[i] = ( syns_2[i] - syns_2[i] ) / dP ;
            ps->e_syns[i] = syns_2[i] - P_1[i]*( syns_2[i] - syns_2[i] ) / dP ;
        }

    } else { // Reibwerte
        
        for(i=0;i<ps->nfrict;i++) {

            if( P_1[i] < EPSILON || P_2[i] < EPSILON )
                return 3;


            // gleiche Last
            if( fabs(P_2[i]-P_1[i]) < EPSILON )
                dP = 1000;
            else
                dP = P_2[i]-P_1[i];
        
            ps->a_dfx0[i] = (dfx0_2[i] - dfx0_1[i])/dP;
            ps->b_dfx0[i] = ( dfx0_1[i]*P_2[i] - dfx0_2[i]*P_1[i] ) 
                         / dP;

            ps->d_sxmax[i] = ( sxmax_2[i] - sxmax_1[i] ) / dP ;
            ps->e_sxmax[i] = sxmax_1[i] - P_1[i]*( sxmax_2[i] - sxmax_1[i] ) / dP ;

            ps->a_fxmax[i] = (fxmax_2[i] - fxmax_1[i])/dP;
            ps->b_fxmax[i] = ( fxmax_1[i]*P_2[i] - fxmax_2[i]*P_1[i] ) 
                          / dP;
            ps->d_sxslide[i] = ( sxslide_2[i] - sxslide_1[i] ) / dP ;
            ps->e_sxslide[i] = sxslide_1[i] - P_1[i]*( sxslide_2[i] - sxslide_1[i] ) / dP ;

            ps->a_fxslide[i] = (fxslide_2[i] - fxslide_1[i])/dP;
            ps->b_fxslide[i] = ( fxslide_1[i]*P_2[i] - fxslide_2[i]*P_1[i] ) 
                            / dP;

            ps->a_dfy0[i] = (dfy0_2[i] - dfy0_1[i])/dP;
            ps->b_dfy0[i] = ( dfy0_1[i]*P_2[i] - dfy0_2[i]*P_1[i] ) 
                         / dP;

            ps->d_symax[i] = ( symax_2[i] - symax_1[i] ) / dP ;
            ps->e_symax[i] = symax_1[i] - P_1[i]*( symax_2[i] - symax_1[i] ) / dP ;

            ps->a_fymax[i] = (fymax_2[i] - fymax_1[i])/dP;
            ps->b_fymax[i] = ( fymax_1[i]*P_2[i] - fymax_2[i]*P_1[i] ) 
                          / dP;
            ps->d_syslide[i] = ( syslide_2[i] - syslide_1[i] ) / dP ;
            ps->e_syslide[i] = syslide_1[i] - P_1[i]*( syslide_2[i] - syslide_1[i] ) / dP ;

            ps->a_fyslide[i] = (fyslide_2[i] - fyslide_1[i])/dP;
            ps->b_fyslide[i] = ( fyslide_1[i]*P_2[i] - fyslide_2[i]*P_1[i] ) 
                            / dP;

            ps->d_ntol0[i] = ( ntol0_2[i] - ntol0_1[i] ) / dP ;
            ps->e_ntol0[i] = ntol0_1[i] - P_1[i]*( ntol0_2[i] - ntol0_1[i] ) / dP ;

            ps->d_syn0[i] = ( syn0_2[i] - syn0_1[i] ) / dP ;
            ps->e_syn0[i] = syn0_1[i] - P_1[i]*( syn0_2[i] - syn0_1[i] ) / dP ;
 
            ps->d_syns[i] = ( syns_2[i] - syns_2[i] ) / dP ;
            ps->e_syns[i] = syns_2[i] - P_1[i]*( syns_2[i] - syns_2[i] ) / dP ;
        }
    }

    return 0;
}
//==================================================================================
// HSRIreifenmodell
//==================================================================================
error_t SlfTireHSRI_init(SSlfTire *ps) { // Tirestruktur

    ps->nfrict = GET_NROWS(*ps->p.pCs);
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(*ps->p.pCalpha));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(*ps->p.pf0));
    ps->nfrict = MIN(ps->nfrict, GET_NROWS(*ps->p.pk));

    return 0;
}
//=================================================
//=================================================
// Ausführen
//=================================================
//==================================================
error_t SlfTire(SSlfTire *ps                  // Tirestruktur
                                              // Output
               ) {


    // Berechnung Einfederung, dynamischer Rollradius,
    // Latschlänge Radaufstandskraft
    //================================================
    SlfTireVert(ps);


    // Schlupf bestimmen
    //==================
    ps->o.vom = ps->p.RD * ps->i.om;

    SlfTireSchlupfHSRI(ps);

    if(ps->p.calc_tire == 0 ) {

        // Rillmodel Horizontalkraft
        //==========================
        return SlfTireHSRI(ps);
    } else if(ps->p.calc_tire == 1 ) {

        // Schlupf bestimmen
        //==================
        SlfTireSchlupfRill(ps);

        // Rillmodel Horizontalkraft
        //==========================
        return SlfTireRill(ps);
    }

    SlfTireDynamic(ps);

    return 4;
}
error_t SlfTireVert(SSlfTire *ps                  // Tirestruktur
                   ) {

    double xq1,yq1,zq1;
    double x2,y2,z2,x3,y3,z3;
    double d;

    // Radaufstandspunkt r0P_0
    //========================
    if( ps->p.calc_road && ps->p.calc_Fz ) {


        // Gl. (1.2) rQ1_0, rQ2_0
        //=======================
        xq1 = ps->i.r0M_0[0] + ps->i.A0K[0][0] * ps->dx_road;
        yq1 = ps->i.r0M_0[1] + ps->i.A0K[1][0] * ps->dx_road;

        ps->p.fcnroadh(&xq1,&yq1,&zq1,&ps->o.index);
        zq1 += ps->p.h0;

        x2 = ps->i.r0M_0[0] - ps->i.A0K[0][0] * ps->dx_road;
        y2 = ps->i.r0M_0[1] - ps->i.A0K[1][0] * ps->dx_road;

        ps->p.fcnroadh(&x2,&y2,&z2,&ps->o.index);
        z2 += ps->p.h0;

        // Gl. (1.2) rQ1Q2_0
        //==================
        ps->o.exS_0[0] = x2-xq1;
        ps->o.exS_0[1] = y2-yq1;
        ps->o.exS_0[2] = z2-zq1;

        // Gl. (1.2) rQ3_0, rQ4_0
        //=======================
        x2 = ps->i.r0M_0[0] + ps->i.A0K[0][1] * ps->dy_road;
        y2 = ps->i.r0M_0[1] + ps->i.A0K[1][1] * ps->dy_road;

        ps->p.fcnroadh(&x2,&y2,&z2,&ps->o.index);
        z2 += ps->p.h0;

        x3 = ps->i.r0M_0[0] - ps->i.A0K[0][1] * ps->dy_road;
        y3 = ps->i.r0M_0[1] - ps->i.A0K[1][1] * ps->dy_road;

        ps->p.fcnroadh(&x3,&y3,&z3,&ps->o.index);
        z3 += ps->p.h0;

        // Gl. (1.2) rQ3Q4_0 (nicht eyS_0)
        //================================
        ps->o.eyS_0[0] = x3-x2;
        ps->o.eyS_0[1] = y3-y2;
        ps->o.eyS_0[2] = z3-z2;

        // Gl. (1.1) enS_0 = rQ1Q2_0 x rQ3Q4_0 / ||rQ1Q2_0 x rQ3Q4_0||
        //============================================================
        VectorCrossProduct(ps->o.exS_0, ps->o.eyS_0, ps->o.enS_0);
        UnifyVector(ps->o.enS_0, ps->o.enS_0);

    } else { // Ebene

        // Normalenvektor Ebene
        //=====================

        ps->o.enS_0[0] = ps->o.enS_0[1] = 0.;
        ps->o.enS_0[2] = 1.;

        // Es wird immmer iNdex 0 verwendet
        ps->o.index = 0;

        // Punkt rQ1_0 unter der Radmitte
        //===============================
        xq1 = ps->i.r0M_0[0];
        yq1 = ps->i.r0M_0[1];
        zq1 = ps->p.h0;
    }

    // Gl. (1.4) exS_0 = eyR_0 x enS_0 / ||eyR_0 x enS_0||
    //============================================================
    VectorCrossProduct(ps->i.eyR_0, ps->o.enS_0, ps->o.exS_0);
    UnifyVector(ps->o.exS_0, ps->o.exS_0);

    // Gl. (1.5) eyS_0 = enS_0 x exS_0
    //============================================================
    VectorCrossProduct(ps->o.enS_0, ps->o.exS_0, ps->o.eyS_0);

    // Gl. (1.6) ezR_0 = exS_0 x eyR_0 / ||exS_0 x eyR_0||
    //============================================================
    VectorCrossProduct(ps->o.exS_0, ps->i.eyR_0, ps->o.ezR_0);
    UnifyVector(ps->o.ezR_0, ps->o.ezR_0);

    // Gl. (1.7) gamma = asin(ezR_0'*enS_0)
    //============================================================
    ps->o.gam = asin(InnerProduct(ps->o.ezR_0,ps->o.enS_0));

    // Gl. (2.5) omB = enS_0'*om0RK_0
    //===============================
    ps->o.omB = InnerProduct(ps->o.enS_0,ps->i.om0RK_0);

    // om0RK_0 x ezR_0
    //================
    x2 = ps->i.om0RK_0[1]*ps->o.ezR_0[2]-ps->i.om0RK_0[2]*ps->o.ezR_0[1];
    y2 = ps->i.om0RK_0[2]*ps->o.ezR_0[0]-ps->i.om0RK_0[0]*ps->o.ezR_0[2];
    z2 = ps->i.om0RK_0[0]*ps->o.ezR_0[1]-ps->i.om0RK_0[1]*ps->o.ezR_0[0];
    
    // v0P_0 - dzp*ezR_0 = v0M_0 - RS * (om0RK_0 x ezR_0)
    //===================================================
    x2 = ps->i.v0M_0[0] - ps->o.RS * x2;
    y2 = ps->i.v0M_0[1] - ps->o.RS * y2;
    z2 = ps->i.v0M_0[2] - ps->o.RS * z2;

    // Nur wenn Radaufstandskraft berechnet wird:
    //===========================================
    if( ps->p.calc_Fz ) {

        // Gl. (1.8) RS = enS_0'*(r0M_0-rQ1_0) / enS_0'*ezR_0
        //===================================================
        ps->o.RS = ps->o.enS_0[0]*(ps->i.r0M_0[0]-xq1)
                 + ps->o.enS_0[1]*(ps->i.r0M_0[1]-yq1)
                 + ps->o.enS_0[2]*(ps->i.r0M_0[2]-zq1);

        d         = NOT_ZERO(InnerProduct(ps->o.enS_0,ps->o.ezR_0));
        ps->o.RS /= d;

        // Gl. (2.3) RSS = RS - ezR_0'*(xe*exS_0 + ye*eyS_0)
        //==================================================
        ps->o.RS -= ps->i.xe * InnerProduct(ps->o.ezR_0,ps->o.exS_0)
                  + ps->i.ye * InnerProduct(ps->o.ezR_0,ps->o.eyS_0);

        // Gl. (2.4) dz = R0 - RSS
        //===========================
        ps->o.dz = MAX(0.0,ps->p.R0 - ps->o.RS);


        // Gl. (2.7) del_zp =
        //============================

        // v0Q_0 - dzp*ezR_0 = v0M_0 - RS * (om0RK_0 x ezR_0) + xpe*exS_0 + xe*omB*eyS_0 + ype*eyS_0 - ye*omB*exS_0
        //=============================================================================================
        // d1 = ps->i.xe*ps->o.omB;
        // d2 = ps->i.ye*ps->o.omB;

        // xq1 = ps->i.v0M_0[0]-ps->o.RS*x2+ps->i.xpe*ps->o.exS_0[0]+d1*ps->o.eyS_0[0]+ps->i.ype*ps->o.eyS_0[0]-d2*ps->o.exS_0[0];
        // yq1 = ps->i.v0M_0[1]-ps->o.RS*y2+ps->i.xpe*ps->o.exS_0[1]+d1*ps->o.eyS_0[1]+ps->i.ype*ps->o.eyS_0[1]-d2*ps->o.exS_0[1];
        // zq1 = ps->i.v0M_0[2]-ps->o.RS*z2+ps->i.xpe*ps->o.exS_0[2]+d1*ps->o.eyS_0[2]+ps->i.ype*ps->o.eyS_0[2]-d2*ps->o.exS_0[2];

        // dzp = -enS_0 * ()/(enS_0*ezR_0)
        //================================
        if( ps->o.dz > 0. ) {
            ps->o.dzp  = -ps->o.enS_0[0]*x2-ps->o.enS_0[1]*y2-ps->o.enS_0[2]*z2;
            ps->o.dzp /= d;
        } else {
            ps->o.dzp = 0.;
        }

        // Radausstandskraft
        // Gl. (3.3)
        //==================

        ps->o.Fz  = ps->p.cz * pow(ps->o.dz,ps->p.czexp)
                   + ps->p.dz * ps->o.dzp;

    } else {

        // Radaufstandskraft ist vorgegeben
        //=================================
        ps->o.Fz = MAX(0.,ps->i.Fz);

        ps->o.dz = exp(log(NOT_ZERO(ps->o.Fz/ps->p.cz))/ps->p.czexp);

        ps->o.RS = ps->p.R0 - ps->o.dz;


    }
        
    // dynamischer Rollradius
    //=======================
    if( ps->p.calc_RD )
        // Gl. (3.1)
        ps->o.RD = 0.666 * ps->p.R0 + 0.334 * ps->o.RS;
    else
        ps->o.RD = ps->p.RD;

    // v0P_0 = v0M_0 - RS * (om0RK_0 x ezR_0) + dzp*ezR_0
    //===================================================
    x2 += ps->o.dzp * ps->o.ezR_0[0];
    y2 += ps->o.dzp * ps->o.ezR_0[1];
    z2 += ps->o.dzp * ps->o.ezR_0[2];

    // Horizontalen Geschwindigkeiten
    // Gl. (2.8) vx = exS_0'* v0P_0
    //           vy = eyS_0'* v0P_0
    //=============================
    ps->o.vx = ps->o.exS_0[0]*x2 + ps->o.exS_0[1]*y2 + ps->o.exS_0[2]*z2;
    ps->o.vy = ps->o.eyS_0[0]*x2 + ps->o.eyS_0[1]*y2 + ps->o.eyS_0[2]*z2;
    
    // Latschlänge
    //============
    if( ps->p.calc_L )
        // Gl. 3.2
        ps->o.L = sqrt(4.*ps->p.R0*ps->o.dz);
    else
        ps->o.L  = ps->p.L0;

    return 0;
}
error_t SlfTireRill(SSlfTire *ps                  // Tirestruktur
                   ) {

    double sigma, d;
    double fak,abs_sy;

    // Koeffizienten bestimmen

    ps->o.index = MIN(ps->o.index,SLF_TIRE_INDEX_MAX);
    ps->o.index = MIN(ps->o.index,(uint8_t)ps->nfrict);

    //Radlastabhängige Parameter
    ps->dfx0_act    = ps->a_dfx0[ps->o.index]    * ps->o.Fz + ps->b_dfx0[ps->o.index];
    ps->sxmax_act   = ps->d_sxmax[ps->o.index]   * ps->o.Fz + ps->e_sxmax[ps->o.index];
    ps->fxmax_act   = ps->a_fxmax[ps->o.index]   * ps->o.Fz + ps->b_fxmax[ps->o.index];
    ps->sxslide_act = ps->d_sxslide[ps->o.index] * ps->o.Fz + ps->e_sxslide[ps->o.index];
    ps->fxslide_act = ps->a_fxslide[ps->o.index] * ps->o.Fz + ps->b_fxslide[ps->o.index];

    ps->dfy0_act    = ps->a_dfy0[ps->o.index]    * ps->o.Fz + ps->b_dfy0[ps->o.index];
    ps->symax_act   = ps->d_symax[ps->o.index]   * ps->o.Fz + ps->e_symax[ps->o.index];
    ps->fymax_act   = ps->a_fymax[ps->o.index]   * ps->o.Fz + ps->b_fymax[ps->o.index];
    ps->syslide_act = ps->d_syslide[ps->o.index] * ps->o.Fz + ps->e_syslide[ps->o.index];
    ps->fyslide_act = ps->a_fyslide[ps->o.index] * ps->o.Fz + ps->b_fyslide[ps->o.index];

    ps->ntol0_act   = ps->d_ntol0[ps->o.index]   * ps->o.Fz + ps->e_ntol0[ps->o.index];
    ps->syn0_act    = NOT_ZERO(ps->d_syn0[ps->o.index]    * ps->o.Fz + ps->e_syn0[ps->o.index]);    
    ps->syns_act    = NOT_ZERO(ps->d_syns[ps->o.index]    * ps->o.Fz + ps->e_syns[ps->o.index]);    

    ps->sx_dach     = ps->fxmax_act/ps->dfx0_act;
    ps->sy_dach     = ps->fymax_act/ps->dfy0_act;


    // Schlupfberechnung
    //==================
    SlfTireSchlupfRill(ps);



    // normalisierte Schlupfkurve bestimmen
    ps->df0_act    = SlfNumSqrt2( ps->dfx0_act*ps->sx_dach*ps->cphi
                                , ps->dfy0_act*ps->sy_dach*ps->sphi);
    ps->smax_act   = SlfNumSqrt2( ps->sxmax_act/ps->sx_dach*ps->cphi
                                , ps->symax_act/ps->sy_dach*ps->sphi);
    ps->fmax_act   = SlfNumSqrt2( ps->fxmax_act*ps->cphi
                                , ps->fymax_act*ps->sphi);
    ps->sslide_act = SlfNumSqrt2( ps->sxslide_act/ps->sx_dach*ps->cphi
                                , ps->syslide_act/ps->sy_dach*ps->sphi);
    ps->fslide_act = SlfNumSqrt2( ps->fxslide_act*ps->cphi
                                , ps->fyslide_act*ps->sphi);

	// stochastischer Anteil my-Max
	//=============================
	ps->fmaxs_act  = ((double)rand()/(double)RAND_MAX*2.0-1.0)* ps->p.fstoch + 1.0;
    ps->fmaxs_act *= ps->fmax_act;

    // Gesamtkraft
    //============
    if( ps->sR_norm < ps->smax_act ) {

        sigma = ps->sR_norm / NOT_ZERO(ps->smax_act);
		d     = NOT_ZERO( 1.0+sigma*( sigma
                                    + ps->df0_act*ps->smax_act/NOT_ZERO(ps->fmaxs_act)-2.0
                                    )
                        );

        ps->f = ps->df0_act*ps->smax_act*sigma/d;

        ps->dfdsR_norm = ps->df0_act*ps->smax_act 
                 * ( 1. 
                   - sigma*(2.*sigma+ ps->df0_act*ps->smax_act/NOT_ZERO(ps->fmaxs_act)-2.0)/d
                   ) / d;
        ps->dfdsR_norm /= ps->smax_act;
        
    } else if( ps->sR_norm < ps->sslide_act ) {

        d     = NOT_ZERO(ps->sslide_act-ps->smax_act);
        sigma = (ps->sR_norm-ps->smax_act) / d;

        ps->f = ps->fmaxs_act-(ps->fmaxs_act-ps->fslide_act)*sigma*sigma*(3.-2.0*sigma);

		ps->dfdsR_norm =  (ps->fmaxs_act-ps->fslide_act)*6.0*sigma*(sigma-1.0);
        ps->dfdsR_norm /= d;

    } else {

        ps->f    = ps->fslide_act;
		ps->dfdsR_norm = 0.0; 
    }

	// dynamischer Nachlauf/Rückstellmoment
    //=====================================
	abs_sy = fabs(ps->o.syR);
    d      = SIGN(ps->o.syR);
	if( abs_sy <= ps->syn0_act ) {

        abs_sy /= ps->syn0_act;
        fak = ps->syn0_act/ps->syns_act;
        ps->nMz = (1.-fak) * (1.-abs_sy)
                + fak*(1.-(3.-2.*abs_sy)*abs_sy*abs_sy);
        
        ps->dnMzdsy = ((fak-1)*d+fak*6.*abs_sy*(abs_sy-d))/ps->syn0_act;
        ps->dnMzdsy = (fak-1)+fak*6.*abs_sy*(abs_sy-1);
        ps->dnMzdsy *= d/ps->syn0_act;

	} else if( abs_sy <= ps->syns_act ) {

        ps->nMz = (ps->syn0_act/ps->syns_act-1.)*(abs_sy-ps->syn0_act)/ps->syn0_act;
        fak     = (ps->syns_act-abs_sy)/(ps->syns_act-ps->syn0_act);
        ps->nMz *= fak*fak;
        
        ps->dnMzdsy  = (ps->syn0_act/ps->syns_act-1.)*(2.-3.*fak)*fak*(ps->syns_act-ps->syn0_act)/ps->syn0_act;
        ps->dnMzdsy *= d*(-1.)/(ps->syns_act-ps->syn0_act); 

	} else {

		ps->nMz     = 0.0;
        ps->dnMzdsy = 0.0;
	}

    ps->nMz     *= ps->ntol0_act * ps->o.L;
    ps->dnMzdsy *= ps->ntol0_act * ps->o.L
        ;
    // Kräfte und Momente
    //===================
    ps->o.Fx_s = ps->f   * ps->o.Fz * ps->cphi;
    ps->o.Fy_s = ps->f   * ps->o.Fz * ps->sphi;
	ps->o.Mz_s = ps->nMz * ps->o.Fy_s * (-1.);

    // Bohrmoment
    // Gl. (2.21)
    //===========
    ps->o.MB  = ps->df0_act * ps->o.sB;
    if( ps->o.sB > 0. ) {

        if( ps->o.MB < ps->fslide_act ) {

            ps->o.MB    *= ps->o.Fz * ps->o.RB;
            ps->o.dMBdsB = ps->df0_act * ps->o.Fz * ps->o.RB;
        } else {
            ps->o.MB     = ps->fslide_act*ps->o.Fz * ps->o.RB;
            ps->o.dMBdsB = 0.;
        }
    } else {
        if( ps->o.MB > ps->fslide_act ) {

            ps->o.MB    *= ps->o.Fz * ps->o.RB;
            ps->o.dMBdsB = ps->df0_act * ps->o.Fz * ps->o.RB;
        } else {
            ps->o.MB     = - ps->fslide_act*ps->o.Fz * ps->o.RB;
            ps->o.dMBdsB = 0.;
        }
    }

        ; 
    ps->o.MB *= MAX(ps->df0_act * ps->o.Fz * ps->o.sB
                     ,ps->fslide_act
                     );

    // Ableitungen
    //============
    if( fabs(ps->sR_norm) < EPSILON )
        ps->o.dFxdsx = ps->dfx0_act * ps->o.Fz;
    else {

        ps->o.dFxdsx  = ps->dfdsR_norm * ps->o.sxR/ps->sx_dach/ps->sx_dach/ps->sR_norm;
        ps->o.dFxdsx *= ps->o.Fz * ps->cphi;
        ps->o.dFxdsx += ps->o.Fx_s * ps->syR_norm*ps->syR_norm/ps->sx_dach/ps->sR_norm/ps->sR_norm/ps->sR_norm;
    }

    if( fabs(ps->sR_norm) < EPSILON )
        ps->o.dFydsy = ps->dfy0_act * ps->o.Fz;
    else {
        ps->o.dFydsy  = ps->dfdsR_norm * ps->o.syR/ps->sy_dach/ps->sy_dach/ps->sR_norm;
        ps->o.dFydsy *= ps->o.Fz * ps->sphi;
        ps->o.dFydsy += ps->o.Fy_s * ps->sxR_norm*ps->sxR_norm/ps->sy_dach/ps->sR_norm/ps->sR_norm/ps->sR_norm;
    }

    ps->o.dMzdsy = (-1.)*(ps->dnMzdsy*ps->o.Fy_s+ps->nMz*ps->o.dFydsy);

    return 0;
}
//==============================================================
//==============================================================
// Reifenmodell HSRI
//==============================================================
//==============================================================
error_t SlfTireHSRI(SSlfTire *ps                  // Tirestruktur
                   ) {


	double f1;

	double f2u;
	double f2q;
	double f2;

	double vG;
	double my;
	double sR;

    Vector_t Cs     = *ps->p.pCs;
    Vector_t Calpha = *ps->p.pCalpha;
    Vector_t f0     = *ps->p.pf0;
    Vector_t k      = *ps->p.pk;

    // Koeffizienten bestimmen
    ps->o.index = MIN(ps->o.index,(uint8_t)ps->nfrict);

	// stochastischer Anteil my-Max
	//=============================
	ps->fmaxs_act  = ((double)rand()/(double)RAND_MAX*2.0-1.0)* ps->p.fstoch + 1.0;
    ps->fmaxs_act *= f0[ps->o.index];


    f1 = 1./NOT_ZERO(1.-fabs(ps->o.sxH));
    f2u = Cs[ps->o.index]     * ps->o.sxH;
    f2q = Calpha[ps->o.index] * ps->o.syH;
    f2  = sqrt(f2u*f2u+f2q*f2q);
    vG  = ps->o.vx * sqrt(ps->o.sxH*ps->o.sxH + ps->o.syH*ps->o.syH);
    my  = ps->fmaxs_act*(1.0 - k[ps->o.index] * ps->p.vN);
    sR  = f2/NOT_ZERO(ps->o.Fz*my)*f1;

    // Haften, kein Gleiten
	//=====================
	if( sR <= 0.5 ) {
	

		ps->o.Fx_s = Cs[ps->o.index]     * ps->o.sxH * f1;
		ps->o.Fy_s = Calpha[ps->o.index] * ps->o.syH * f1;

		ps->o.Mz_s    = -0.5 * ps->o.L / 3. * ps->o.Fy_s;
		ps->o.Mz_s   -= ps->o.Fx_s * SIGN(ps->o.vx)
			    * ( 0.5 * ps->o.L * ps->o.syH * 4./3.
				  + ps->o.Fy_s / NOT_ZERO(ps->p.cy)
                  );
		ps->o.Mz_s   *= SIGN(ps->o.vx);

		ps->o.dFxdsx = (fabs(ps->o.sxH) * f1 + 1.) * f1 * Cs[ps->o.index];
		//ps->o.dFxdsy = 0.0;
		//ps->o.dFydsx = SIGN(ps->o.sxH) * ps->o.syH * f1 * f1 * Calpha[ps->o.index];
		ps->o.dFydsy = f1 * Calpha[ps->o.index]; 

	// Haften und Gleiten
    //===================
	} else {

		double sR2 = NOT_ZERO(sR*sR);
		double fsR = (sR-0.25)/sR2;
		double fx  = Cs[ps->o.index]     * ps->o.sxH * f1;
		double fy  = Calpha[ps->o.index] * ps->o.syH * f1;
		double dfxdsu = (fabs(ps->o.sxH) * f1 + 1.) * f1 * Cs[ps->o.index];
		double dfydsu = SIGN(ps->o.sxH) * ps->o.syH * f1 * f1 * Calpha[ps->o.index];
		double dfydsq = f1 * Calpha[ps->o.index];
		double dfsRdsR = (1.-2.*(sR-0.25)/NOT_ZERO(sR))/sR2;
		double dsRdsu  = (Cs[ps->o.index]*Cs[ps->o.index]*ps->o.sxH /f2 + f2*f1*SIGN(ps->o.sxH)) / NOT_ZERO(my*ps->o.Fz) * f1;
		double dsRdsq  = Calpha[ps->o.index]*Calpha[ps->o.index]*ps->o.syH/NOT_ZERO(my*ps->o.Fz*f2) * f1;
		

		ps->o.Fx_s = fx * fsR;
		ps->o.Fy_s = fy * fsR;

		ps->o.Mz_s    = -0.5*ps->o.L * ((12.-1/sR2)/(12-3/sR)-1.) * ps->o.Fy_s;
		ps->o.Mz_s   -= ps->o.Fx_s * SIGN(ps->o.vx)
			          * ( 0.5 * ps->o.L * ps->o.syH * (sR-0.33333333)/NOT_ZERO(sR)/(sR-0.25)
					    + ps->o.Fy_s / ps->p.cy
                        );
		ps->o.Mz_s   *= SIGN(ps->o.vx);

		ps->o.dFxdsx  = dfxdsu * fsR + fx * dfsRdsR * dsRdsu;
		//ps->o.dFxdsy  =                fx * dfsRdsR * dsRdsq;

		//ps->o.dFydsx  = dfydsu * fsR + fy * dfsRdsR * dsRdsu;
		ps->o.dFydsy  = dfydsq * fsR + fy * dfsRdsR * dsRdsq;
			            
			            
	}

    ps->o.dMzdsy = 0.;
    ps->o.MB     = 0.;
    ps->o.dMBdsB = 0.;

    return 0;
}

// Schlupfberechnung Rill
// Berechnung Schlupf
// Input
// sx_dach
// sy_dach
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
void SlfTireSchlupfRill(SSlfTire *ps) {

    double delta = ps->o.vx - ps->o.vom;

    double nenx = NOT_ZERO(fabs(ps->o.vx+fabs(delta)) * ps->sx_dach + ps->p.vN);
    double neny = NOT_ZERO(fabs(ps->o.vx+fabs(delta)) * ps->sy_dach + ps->p.vN);

    // normalisierter Schlupf
    ps->sxR_norm = -delta/nenx;
    ps->syR_norm = - ps->o.vy/neny;

    // absoluter Schlupf
    ps->o.sxR        = ps->sxR_norm * ps->sx_dach;
    ps->o.syR        = ps->syR_norm * ps->sy_dach;

    // normalisierter Gesamtschlupf
    ps->sR_norm  = sqrt(ps->sxR_norm * ps->sxR_norm + ps->syR_norm * ps->syR_norm);

    // Winkelbedingungen
    if( fabs(ps->sR_norm) < EPSILON ) {
            ps->cphi = 0.5*sqrt(2.);
            ps->sphi = ps->cphi;
    } else {
        ps->cphi = ps->sxR_norm/NOT_ZERO(ps->sR_norm);
        ps->sphi = ps->syR_norm/NOT_ZERO(ps->sR_norm);
    }

    // Ableitungen
    // ps->dsxdvx = -1./nenx*ps->sx_dach;
    // ps->dsxdom = ( ps->o.RD
    //              - (ps->o.vom * ps->sx_dach * (ps->o.vom - ps->o.vx)) / (fabs(ps->i.om)*nenx)
    //              ) /nenx * ps->sx_dach;

    // dsx/dvx
    //========
    if( delta >= 0.0 )
        ps->o.dsxdvx = (-ps->o.vx*ps->sx_dach-ps->p.vN) / nenx / nenx;
    else
        ps->o.dsxdvx = -ps->sx_dach/nenx;
    
    // dsx/dom
    //========
    ps->o.dsxdom = ps->o.RD*ps->sx_dach*(ps->o.vx*ps->sx_dach+ps->p.vN / nenx / nenx);

    // dsy/dvy
    //========
    ps->o.dsydvy = -ps->sy_dach/neny;

    // dsy/dvx
    // dsy/dom
    //========
    if( delta >= 0.0 ) {
        ps->o.dsydvx = (2.*ps->sy_dach*ps->sy_dach*ps->o.vy)/neny/neny;
        ps->o.dsydom = ps->o.dsydvx/(-2.)*ps->o.RD;
    } else {
        ps->o.dsydvx = 0.;
        ps->o.dsxdom = (ps->sy_dach*ps->sy_dach*ps->o.vy*ps->o.RD)/neny/neny;;
    }


#if 0

    double nenx = NOT_ZERO(fabs(ps->o.vom) * ps->sx_dach + ps->p.vN);
    double neny = NOT_ZERO(fabs(ps->o.vom) * ps->sy_dach + ps->p.vN);

    // normalisierter Schlupf
    ps->sxR_norm = (ps->o.vom - ps->o.vx)/nenx;
    ps->syR_norm = - ps->o.vy/neny;

    // absoluter Schlupf
    ps->o.sxR        = ps->sxR_norm * ps->sx_dach;
    ps->o.syR        = ps->syR_norm * ps->sy_dach;

    // normalisierter Gesamtschlupf
    ps->sR_norm  = sqrt(ps->sxR_norm * ps->sxR_norm + ps->syR_norm * ps->syR_norm);

    // Winkelbedingungen
    if( fabs(ps->sR_norm) < EPSILON ) {
            ps->cphi = 0.5*sqrt(2.);
            ps->sphi = ps->cphi;
    } else {
        ps->cphi = ps->sxR_norm/NOT_ZERO(ps->sR_norm);
        ps->sphi = ps->syR_norm/NOT_ZERO(ps->sR_norm);
    }

    //Ableitungen
    ps->dsxdvx = -1./nenx*ps->sx_dach;
    ps->dsxdom = ( ps->o.RD
                 - (ps->o.vom * ps->sx_dach * (ps->o.vom - ps->o.vx)) / (fabs(ps->i.om)*nenx)
                 ) /nenx * ps->sx_dach;

    ps->dsydvx = 0.;
    ps->dsydvy = -1./neny*ps->sy_dach;;
    ps->dsydom = (ps->o.vom * ps->sy_dach * ps->o.vy)/(fabs(ps->i.om)*neny*neny)*ps->sy_dach;
#endif
    // Bohrschlupf
    //============
    ps->o.RB = 0.333 * sqrt(ps->p.B0*ps->o.L);
    ps->o.sB = ps->o.RB * ps->o.omB * (1.0) * ps->sx_dach / nenx;
}
// Schlupfberechnung HSRI
// Berechnung Schlupf
// Input
// pvx
// ps->o.pvy
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
void SlfTireSchlupfHSRI(SSlfTire *ps) {

        double d;
		// Längsschlupf sx
		//================
		if( ps->o.vom > ps->o.vx ) {

			//positive Längskraft
			//===================
			if( ps->o.vom >= 0. ) {

				// Antreiben
				//==========

				d           = ps->o.vom+ps->p.vN;
				ps->o.sxH = (ps->o.vom - ps->o.vx)/d;

				ps->o.dsxdom  = 1.0 + (ps->o.vx-ps->o.vom)/d;
				ps->o.dsxdom *= ps->p.RD/d;

				ps->o.dsxdvx  = -1.0/d;

			} else {

				// Bremsen, Rad dreht rückwärts
				//=============================
				d    = ps->o.vx+ps->p.vN;
				ps->o.sxH = (ps->o.vx-ps->o.vom)/d;

				ps->o.dsxdom = ps->p.RD/d*(-1.);

				ps->o.dsxdvx = 1.0-(ps->o.vx-ps->o.vom)/d;
				ps->o.dsxdvx /= d;

			}

		} else {
		
			//negative Längskraft
			//===================

			if( ps->o.vx >= 0. ) {

				// Bremsen
				//==========

					d = ps->o.vx+ps->p.vN;

					ps->o.sxH = (ps->o.vom-ps->o.vx)/d;

					ps->o.dsxdom = ps->p.RD/d;
					
					ps->o.dsxdvx  = -1.0+(ps->o.vx-ps->o.vom)/d;
					ps->o.dsxdvx  /= d;



			} else {

				// Antreibe, Fzg fährt rückwärts
				//===============================
				d        = ps->o.vom+ps->p.vN;
				ps->o.sxH     = (ps->o.vx-ps->o.vom)/d;

				ps->o.dsxdom = -1.0-(ps->o.vx-ps->o.vom)/d;
				ps->o.dsxdom *= ps->p.RD/d;

				ps->o.dsxdvx  = 1.0/d;

			}
		}
		if( ps->o.sxH > 1.0 ) {

			ps->o.sxH = 1.0;

			ps->o.dsxdom = 0.0;
			ps->o.dsxdvx  = 0.0;

		} else if( ps->o.sxH < -1.0 ) {

			ps->o.sxH = -1.0;

			ps->o.dsxdom = 0.0;
			ps->o.dsxdvx  = 0.0;

		}

	    // Querschlupf sq
		//================
		d    = fabs(ps->o.vx)+ps->p.vN;
		ps->o.syH = (-1.)* ps->o.vy/d;

		ps->o.dsydvx  = (-1.)* ps->o.syH/d*SIGN(ps->o.vx);
		ps->o.dsydvy  = -1./d;
}
// Dynamische Berechnung
error_t SlfTireDynamic(SSlfTire *ps){

    double d;

    if( ps->dyn_x ) {

        d = NOT_ZERO(ps->p.dx - ps->o.dFxdsx * ps->o.dsxdvx);

        ps->o.xpe  = (ps->o.Fx_s - ps->p.cx * ps->i.xe)/d;

        ps->o.dxpedxe = -ps->p.cx /d;
        
        ps->o.Fx = ps->p.cx * ps->i.xe + ps->p.dx * ps->o.xpe;
        
    } else {

        ps->o.xpe     = 0.;
        ps->o.dxpedxe = 0.;
        ps->o.Fx      = ps->o.Fx_s;
    }

    if( ps->dyn_y ) {

        d = NOT_ZERO(ps->p.dy - ps->o.dFydsy * ps->o.dsydvy);

        ps->o.ype  = (ps->o.Fy_s - ps->p.cy * ps->i.ye)/d;

        ps->o.dypedye = -ps->p.cy /d;
        
        ps->o.Fy = ps->p.cy * ps->i.ye + ps->p.dy * ps->o.ype;
        
    } else {

        ps->o.ype     = 0.;
        ps->o.dypedye = 0.;
        ps->o.Fy      = ps->o.Fy_s;
    }

    return 0;
}

// Fehlermeldung
//===============
char *SlfTireError(error_t ierr) {

    if( !ierr )
        return "SlfTire: kein Fehler";

    switch(ierr) {
    case 1:
        return "SlfTire: vN is lower Epsilon";
    case 2:
        return "SlfTire: nfrict > SLF_TIRE_INDEX_MAX, too many friction index";
    case 3:
        return "SlfTire: pP1[i] < EPSILON || pP2[i] < EPSILON, Parameter P1 or P2 too small";
    case 4:
        return "SlfTire: ps->p.calc_tire not known";
    case 5:
        return "SlfTire: free radius ps->p.R0 is zero";
    case 6:
        return "SlfTire: dynamic rollradius ps->p.RD is zero";
    default:
        return "SlfTire: unkown error";
    }
}
