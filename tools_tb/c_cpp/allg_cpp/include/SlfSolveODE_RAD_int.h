#ifndef INTEGRATOR_RAD_H_INCLUDED
#define INTEGRATOR_RAD_H_INCLUDED

    /* Common Block Declarations */
    
typedef struct tag_sradau_weight{
        int32_t nn, ns;
        double xsol, hsol, c__[8];
} sradau_weight;

typedef struct tag_sradau_linal{
    int32_t mle, mue, mbjac, mbb, mdiag, mdiff, mbdiag;
} sradau_linal;

typedef struct tag_sradau_coe3{
    double t311, t312, t313, t321, t322, t323, t331, ti311, ti312, ti313, 
        ti321, ti322, ti323, ti331, ti332, ti333;
} sradau_coe3;
typedef struct tag_sradau_coe5{
    double t511, t512, t513, t514, t515, t521, t522, t523, t524, t525, 
        t531, t532, t533, t534, t535, t541, t542, t543, t544, t545, t551, 
        ti511, ti512, ti513, ti514, ti515, ti521, ti522, ti523, ti524, 
        ti525, ti531, ti532, ti533, ti534, ti535, ti541, ti542, ti543, 
        ti544, ti545, ti551, ti552, ti553, ti554, ti555;
} sradau_coe5;
typedef struct tag_sradau_coe7{
    double t711, t712, t713, t714, t715, t716, t717, t721, t722, t723, 
        t724, t725, t726, t727, t731, t732, t733, t734, t735, t736, t737, 
        t741, t742, t743, t744, t745, t746, t747, t751, t752, t753, t754, 
        t755, t756, t757, t761, t762, t763, t764, t765, t766, t767, t771, 
        ti711, ti712, ti713, ti714, ti715, ti716, ti717, ti721, ti722, 
        ti723, ti724, ti725, ti726, ti727, ti731, ti732, ti733, ti734, 
        ti735, ti736, ti737, ti741, ti742, ti743, ti744, ti745, ti746, 
        ti747, ti751, ti752, ti753, ti754, ti755, ti756, ti757, ti761, 
        ti762, ti763, ti764, ti765, ti766, ti767, ti771, ti772, ti773, 
        ti774, ti775, ti776, ti777;
} sradau_coe7;

#endif
