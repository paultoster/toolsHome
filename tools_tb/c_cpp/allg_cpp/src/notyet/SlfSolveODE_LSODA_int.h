#ifndef INTEGRATOR_LSODA_H_INCLUDED
#define INTEGRATOR_LSODA_H_INCLUDED

typedef struct tag_SIntegratorLSODA_1 {
    double rowns[209]
         , ccmax
         , el0
         , h__
         , hmin
         , hmxi
         , hu
         , rc
         , tn
         , uround;
    int32_t init
         , mxstep
         , mxhnil
         , nhnil
         , nslast
         , nyh
         , iowns[6]
         , icf
         , ierpj
         , iersl
         , jcur
         , jstart
         , kflag
         , l
         , lyh
         , lewt
         , lacor
         , lsavf
         , lwm
         , liwm
         , meth
         , miter
         , maxord
         , maxcor
         , msbp
         , mxncf
         , n
         , nq
         , nst
         , nfe
         , nje
         , nqu;
} SIntegratorLSODA_1;

typedef struct tag_SIntegratorLSODA_2 {
    double     conit
             , crate
             , el[13]
             , elco[156]	/* was [13][12] */
             , hold
             , rmax
             , tesco[36]	/* was [3][12] */
             , ccmax
             , el0
             , h__
             , hmin
             , hmxi
             , hu
             , rc
             , tn
             , uround;
    int32_t  iownd[6]
          , ialth
          , ipup
          , lmax
          , meo
          , nqnyh
          , nslp
          , icf
          , ierpj
          , iersl
          , jcur
          , jstart
          , kflag
          , l
          , lyh
          , lewt
          , lacor
          , lsavf
          , lwm
          , liwm
          , meth
          , miter
          , maxord
          , maxcor
          , msbp
          , mxncf
          , n
          , nq
          , nst
          , nfe
          , nje
          , nqu;
} SIntegratorLSODA_2;
typedef struct tag_SIntegratorLSODA_3 {
    double     rowns[209]
             , ccmax
             , el0
             , h__
             , hmin
             , hmxi
             , hu
             , rc
             , tn
             , uround;
    int32_t  iownd[6]
          , iowns[6]
          , icf
          , ierpj
          , iersl
          , jcur
          , jstart
          , kflag
          , l
          , lyh
          , lewt
          , lacor
          , lsavf
          , lwm
          , liwm
          , meth
          , miter
          , maxord
          , maxcor
          , msbp
          , mxncf
          , n
          , nq
          , nst
          , nfe
          , nje
          , nqu;
} SIntegratorLSODA_3;

typedef union tag_UIntegratorLSODA_1{

    SIntegratorLSODA_1 a;
    SIntegratorLSODA_2 b;
    SIntegratorLSODA_3 c;
}UIntegratorLSODA_1;


typedef struct tag_SIntegratorLSODA_a {
    double     tsw
             , rowns2[20]
             , pdnorm;
    int32_t  insufr
          , insufi
          , ixpr
          , iowns2[2]
          , jtyp
          , mused
          , mxordn
          , mxords;
} SIntegratorLSODA_a;
typedef struct tag_SIntegratorLSODA_b {
    double rownd2
         , cm1[12]
         , cm2[5]
         , pdest
         , pdlast
         , ratio
         , pdnorm;
    int32_t iownd2[3]
         , icount
         , irflag
         , jtyp
         , mused
         , mxordn
         , mxords;
} SIntegratorLSODA_b;

typedef union tag_UIntegratorLSODA_a{

    SIntegratorLSODA_a a;
    SIntegratorLSODA_b b;
}UIntegratorLSODA_a;

#endif
