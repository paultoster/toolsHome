#include "DsParData.h"
#include "SlfTab.h"

    
// Konstruktor CDsPar
CDsParData::CDsParData() {

    Status  = OKAY;
    ErrText = "";

    pData = 0;

}
// Destruktor 
CDsParData::~CDsParData() {


    deleteParStruct(pData);






}

// Reset 
void CDsParData::reset() {


    Status = OKAY;
    ErrText = "";
    deleteParStruct(pData);

    pData = 0;

}

//==========================================================================
//==========================================================================
//==========================================================================
//==========================================================================
// set Functions ===========================================================
//==========================================================================
//==========================================================================
//==========================================================================
status_t CDsParData::setSingleData(CSlfStr  *pinstanz     // Instanzname
                                   ,CSlfStrV *pgroup       // Gruppenhierachie
                                   ,CSlfStr  *pvarname     // Variablenname
                                   ,CSlfStr  *punit        // Einheit
                                   ,double   yfactor
                                   ,double   yoffset
                                   ,CSlfStr  *pcomment     // Kommentar
                                   ,double   dval         // Singlewert double
                                   ) {

    return setData(pinstanz
                  ,pgroup
                  ,pvarname
                  ,punit
                  ,pcomment
                  ,DEF_DOUBLE
                  ,1
                  ,1
                  ,1.0    //xfactor hat keine Bedeutung
                  ,0.0    //xoffset hat keine Bedeutung
                  ,yfactor
                  ,yoffset
                  ,1.0    //zfactor hat keine Bedeutung
                  ,0.0    //zoffset hat keine Bedeutung
                  ,(void *)&dval);
}
status_t CDsParData::setStringData(CSlfStr  *pinstanz     // Instanzname
                                  ,CSlfStrV *pgroup       // Gruppenhierachie
                                  ,CSlfStr  *pvarname     // Variablenname
                                  ,CSlfStr  *punit        // Einheit
                                  ,double   yfactor
                                  ,double   yoffset
                                  ,CSlfStr  *pcomment     // Kommentar
                                  ,CSlfStr  *psval         // String
                                   ) {

    return setData(pinstanz
                  ,pgroup
                  ,pvarname
                  ,punit
                  ,pcomment
                  ,DEF_STRING
                  ,1
                  ,1
                  ,1.0    //xfactor hat keine Bedeutung
                  ,0.0    //xoffset hat keine Bedeutung
                  ,yfactor
                  ,yoffset
                  ,1.0    //zfactor hat keine Bedeutung
                  ,0.0    //zoffset hat keine Bedeutung
                  ,(void *)psval);
}
status_t CDsParData::setStringMData(CSlfStr  *pinstanz     // Instanzname
                                  ,CSlfStrV *pgroup       // Gruppenhierachie
                                  ,CSlfStr  *pvarname     // Variablenname
                                  ,CSlfStr  *punit        // Einheit
                                  ,double   yfactor
                                  ,double   yoffset
                                  ,CSlfStr  *pcomment     // Kommentar
                                  ,CSlfStrM *psm          // String Matric
                                   ) {

    return setData(pinstanz
                  ,pgroup
                  ,pvarname
                  ,punit
                  ,pcomment
                  ,DEF_STRINGM
                  ,1
                  ,1
                  ,1.0    //xfactor hat keine Bedeutung
                  ,0.0    //xoffset hat keine Bedeutung
                  ,yfactor
                  ,yoffset
                  ,1.0    //zfactor hat keine Bedeutung
                  ,0.0    //zoffset hat keine Bedeutung
                  ,(void *)psm);
}
status_t CDsParData::setVectorData(CSlfStr  *pinstanz     // Instanzname
                                  ,CSlfStrV *pgroup       // Gruppenhierachie
                                  ,CSlfStr  *pvarname     // Variablenname
                                  ,CSlfStr  *punit        // Einheit
                                  ,double   yfactor
                                  ,double   yoffset
                                  ,CSlfStr  *pcomment     // Kommentar
                                  ,Vector_t   *pvec         // Vector_t double
                                  ,uint32_t   nvec
                                  ) {

    return setData(pinstanz
                  ,pgroup
                  ,pvarname
                  ,punit
                  ,pcomment
                  ,DEF_VEC
                  ,nvec
                  ,1
                  ,1.0    //xfactor hat keine Bedeutung
                  ,0.0    //xoffset hat keine Bedeutung
                  ,yfactor
                  ,yoffset
                  ,1.0    //zfactor hat keine Bedeutung
                  ,0.0    //zoffset hat keine Bedeutung
                  ,(void *)pvec);
}
status_t CDsParData::setMatrixData(CSlfStr  *pinstanz     // Instanzname
                                  ,CSlfStrV *pgroup       // Gruppenhierachie
                                  ,CSlfStr  *pvarname     // Variablenname
                                  ,CSlfStr  *punit        // Einheit
                                  ,double   yfactor
                                  ,double   yoffset
                                  ,CSlfStr  *pcomment     // Kommentar
                                  ,Matrix_t   mat
								  ,uint32_t   nrows
								  ,uint32_t   ncols
                                  ) {

    return setData(pinstanz
                  ,pgroup
                  ,pvarname
                  ,punit
                  ,pcomment
                  ,DEF_MAT
                  ,nrows
                  ,ncols
                  ,1.0    //xfactor hat keine Bedeutung
                  ,0.0    //xoffset hat keine Bedeutung
                  ,yfactor
                  ,yoffset
                  ,1.0    //zfactor hat keine Bedeutung
                  ,0.0    //zoffset hat keine Bedeutung
                  ,(void *)mat);
}
status_t CDsParData::set1DTableData(
                            CSlfStr  *pinstanz     // Instanzname
                           ,CSlfStrV *pgroup       // Gruppenhierachie
                           ,CSlfStr  *ptabname     // Tabellenname
                           ,CSlfStr  *pcomment     // Kommentar
                           ,CSlfStr  *pxname       // Variablenname x
                           ,CSlfStr  *pxunit       // Einheit x
                           ,double   xfactor
                           ,double   xoffset
                           ,CSlfStr  *pxcomment    // Kommentar x
                           ,uint32_t   nxvec
                           ,double   *pxvec        // Vektor x
                           ,CSlfStr  *pyname       // Variablenname x
                           ,CSlfStr  *pyunit       // Einheit x
                           ,double   yfactor
                           ,double   yoffset
                           ,CSlfStr  *pycomment    // Kommentar x
                           ,uint32_t   nyvec
                           ,double   *pyvec        // Vektor x
                           ,uint8_t    lin_flag /*=1*/ // Linear / Step
                           ) {

   CSlfStr  tabname;     // Tabellenname
   CSlfStr  comment;     // Kommentar
   CSlfStr  xname;       // Variablenname x
   CSlfStr  xunit;       // Einheit x
   CSlfStr  xcomment;    // Kommentar x
   CSlfStr  yname;       // Variablenname x
   CSlfStr  yunit;       // Einheit x
   CSlfStr  ycomment;    // Kommentar x

    // Tabelle anlegen
    //================
    if( !ptabname || !pyname || !pxname || !pxvec || !pyvec ) {

        ErrText.cat("CDsParData::set1DTableData: Parameterübergae nicht richtig erstellt\n");
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( ptabname->getLen() == 0 ) {

        ErrText.cat("CDsParData::set1DTableData: Parameterübergae nicht richtig erstellt Tabellenname nicht übergeben\n");
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pxname->getLen() == 0 ) {

        ErrText.catFormat("CDsParData::set1DTableData: Parameterübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für x-Vektor übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pyname->getLen() == 0 ) {

        ErrText.catFormat("CDsParData::set1DTableData: Parameterübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für y-Vektor übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }

    if( ptabname  ) tabname  = ptabname->c_str();
    if( pcomment  ) comment  = pcomment->c_str();
    if( pxname    ) xname    = pxname->c_str();
    if( pxunit    ) xunit    = pxunit->c_str();
    if( pxcomment ) xcomment = pxcomment->c_str();
    if( pyname    ) yname    = pyname->c_str();
    if( pyunit    ) yunit    = pyunit->c_str();
    if( pycomment ) ycomment = pycomment->c_str();

    CSlf1DTab *p1dtab = new CSlf1DTab;

    // Tabelle füllen
    if( p1dtab->set(tabname.c_str()
                   ,comment.c_str()
                   ,xname.c_str()
                   ,xunit.c_str()
                   ,xcomment.c_str()
                   ,pxvec
                   ,nxvec
                   ,yname.c_str()
                   ,yunit.c_str()
                   ,ycomment.c_str()
                   ,pyvec
                   ,nyvec
                   ,lin_flag) != OKAY ) {

                 ErrText.catFormat(
                         "CDsParData::set1DTableData Probleme mit CSlf1DTab mit"
                         " Tabelle: <%s> xvec: <%s> yvec: <%s>\n"
                        ,ptabname->c_str()
                        ,pxname->c_str()
                        ,pyname->c_str());
                 ErrText.cat(p1dtab->getErrText());
                 p1dtab->resetErrText();

                Status = NOT_OKAY;
                return Status;
    }

    Status = setData(pinstanz
                    ,pgroup
                    ,ptabname
                    ,0
                    ,pcomment
                    ,DEF_1D_TAB
                    ,1
                    ,1
                    ,xfactor
                    ,xoffset
                    ,yfactor
                    ,yoffset
                    ,1.0
                    ,0.0
                    ,(void *)p1dtab);
    delete p1dtab;

    return Status;
}
status_t CDsParData::set2DTableData(
                            CSlfStr  *pinstanz     // Instanzname
                           ,CSlfStrV *pgroup       // Gruppenhierachie
                           ,CSlfStr  *ptabname     // Tabellenname
                           ,CSlfStr  *pcomment     // Kommentar
                           ,CSlfStr  *pxname       // Variablenname x
                           ,CSlfStr  *pxunit       // Einheit x
                           ,double   xfactor
                           ,double   xoffset
                           ,CSlfStr  *pxcomment    // Kommentar x
                           ,uint32_t   nxvec
                           ,double   *pxvec        // Vektor x
                           ,CSlfStr  *pyname       // Variablenname y
                           ,CSlfStr  *pyunit       // Einheit y
                           ,double   yfactor
                           ,double   yoffset
                           ,CSlfStr  *pycomment    // Kommentar y
                           ,uint32_t   nyvec
                           ,double   *pyvec        // Vektor y
                           ,CSlfStr  *pzname       // Variablenname z
                           ,CSlfStr  *pzunit       // Einheit z
                           ,double   zfactor
                           ,double   zoffset
                           ,CSlfStr  *pzcomment    // Kommentar z
                           ,Matrix_t   zmat          // Matrix_t z
                           ,uint8_t    lin_flag      // Linear / Step
                           ) {

   CSlfStr  tabname;     // Tabellenname
   CSlfStr  comment;     // Kommentar
   CSlfStr  xname;       // Variablenname x
   CSlfStr  xunit;       // Einheit x
   CSlfStr  xcomment;    // Kommentar x
   CSlfStr  yname;       // Variablenname y
   CSlfStr  yunit;       // Einheit y
   CSlfStr  ycomment;    // Kommentar y
   CSlfStr  zname;       // Variablenname z
   CSlfStr  zunit;       // Einheit z
   CSlfStr  zcomment;    // Kommentar z

    // Tabelle anlegen
    //================
    if( !ptabname || !pzname || !pyname || !pxname || !pxvec || !pyvec || !zmat ) {

        ErrText.cat("CDsParData::set2DTableData: Parameterübergae nicht richtig erstellt\n");
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( ptabname->getLen() == 0 ) {

        ErrText.cat("CDsParData::set2DTableData: Parameterübergae nicht richtig erstellt Tabellenname nicht übergeben\n");
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pxname->getLen() == 0 ) {

        ErrText.catFormat("CDsParData::set2DTableData: Parameterübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für x-Vektor übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pyname->getLen() == 0 ) {

        ErrText.catFormat("CDsParData::set2DTableData: Parameterübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für y-Vektor übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }
    if( pzname->getLen() == 0 ) {

        ErrText.catFormat("CDsParData::set2DTableData: Parameterübergae nicht richtig erstellt. Für Tabelle <%s> keinen Namen für z-Matrix_t übergeben\n"
                         ,ptabname->c_str());
        Status = NOT_OKAY;
        return NOT_OKAY;
    }

    if( ptabname  ) tabname  = ptabname->c_str();
    if( pcomment  ) comment  = pcomment->c_str();
    if( pxname    ) xname    = pxname->c_str();
    if( pxunit    ) xunit    = pxunit->c_str();
    if( pxcomment ) xcomment = pxcomment->c_str();
    if( pyname    ) yname    = pyname->c_str();
    if( pyunit    ) yunit    = pyunit->c_str();
    if( pycomment ) ycomment = pycomment->c_str();
    if( pzname    ) zname    = pzname->c_str();
    if( pzunit    ) zunit    = pzunit->c_str();
    if( pzcomment ) zcomment = pzcomment->c_str();

    CSlf2DTab *p2dtab = new CSlf2DTab;

    // Tabelle füllen
    if( p2dtab->set(tabname.c_str()
                   ,comment.c_str()
                   ,xname.c_str()
                   ,xunit.c_str()
                   ,xcomment.c_str()
                   ,pxvec
                   ,nxvec
                   ,yname.c_str()
                   ,yunit.c_str()
                   ,ycomment.c_str()
                   ,pyvec
                   ,nyvec
                   ,zname.c_str()
                   ,zunit.c_str()
                   ,zcomment.c_str()
                   ,zmat
                   ,lin_flag) != OKAY ) {

                 ErrText.catFormat(
                         "CDsParData::set2DTableData Probleme mit CSlf2DTab mit"
                         " Tabelle: <%s> xvec: <%s> yvec: <%s> zmat:<%s>\n"
                        ,ptabname->c_str()
                        ,pxname->c_str()
                        ,pyname->c_str()
                        ,pzname->c_str());
                 ErrText.cat(p2dtab->getErrText());
                 p2dtab->resetErrText();

                Status = NOT_OKAY;
                return Status;
    }

    Status = setData(pinstanz
                    ,pgroup
                    ,ptabname
                    ,0
                    ,pcomment
                    ,DEF_2D_TAB
                    ,1
                    ,1
                    ,xfactor
                    ,xoffset
                    ,yfactor
                    ,yoffset
                    ,zfactor
                    ,zoffset
                    ,(void *)p2dtab);
    delete p2dtab;

    return Status;
}
status_t CDsParData::setData(CSlfStr     *pi       // Instanzname
                            ,CSlfStrV    *pg       // Gruppenhierachie
                            ,CSlfStr     *pvn      // Variablenname
                            ,CSlfStr     *pu       // Einheit
                            ,CSlfStr     *pcom     // Kommentar
                            ,EVarType    type      // DatenType
                            ,uint32_t    nrows     // Anzahl Reihen
                            ,uint32_t    ncols     // Anzahl Spalten
                            ,double      xfactor
                            ,double      xoffset
                            ,double      yfactor
                            ,double      yoffset
                            ,double      zfactor
                            ,double      zoffset
                            ,void        *pval     // Pointer Wert
                      ) {


    uint32_t      i,j;

    CSlfStr     instanz;
    CSlfStrV    group;
    CSlfStr     varname;
    CSlfStr     unit;
    CSlfStr     comment;

    if( pi   ) instanz = pi->c_str();
    if( pg   ) group   = *pg;
    if( pvn  ) varname = pvn->c_str();
    if( pu   ) unit    = pu->c_str();
    if( pcom ) comment = pcom->c_str();
 

    SDsParDataGroup		*pdgroup;
    SDsParDataVariable  *pdvar;

    // Gültigkeit
    //===========
    if( varname.getLen() == 0 ) {

        ErrText.catFormat("CDsParData::setSingleData: Es wurde kein Variablenname übergeben !!!");
        Status = NOT_OKAY;
        return Status;
    }
        
    //Gruppe finden oder bilden
    //=========================
    pdgroup = findorbuildGroup(instanz,group);

    //Variable finden oder bilden
    //===========================
    pdvar = findorbuildVariable(pdgroup,varname);

	  //alte Zuordnung, wenn besteht lösen
	  deleteVal(pdvar);

    // Varibale setzen
    //================
    pdvar->Comment = comment;
    pdvar->Type    = type;
    pdvar->Unit    = unit;

    switch(type) {

    case DEF_DOUBLE:
        {
        double *pd = (double *)pval;
        double *pv;

        pdvar->NRow    = nrows;
        pdvar->NCol    = ncols;
        pdvar->NVal    = ncols*nrows;

        pdvar->YFactor  = yfactor;
        pdvar->YOffset  = yoffset;

        pv   = new double[pdvar->NVal];
        for(i=0;i<pdvar->NVal;i++)
            pv[i]  = pd[i];

        pdvar->pVal = (void *)pv;
        }
        break;
    case DEF_VEC:
        {
    		Vector_t *pvec = (Vector_t *)pval;
		    Vector_t vecget = *pvec;
        //double *pd = (double *)pval;
        //Vector_t vec;

        pdvar->NRow    = nrows;
        pdvar->NCol    = ncols;
        pdvar->NVal    = ncols*nrows;
        if( pdvar->NVal == 0 )
            pdvar->NRow = 0;

        pdvar->YFactor  = yfactor;
        pdvar->YOffset  = yoffset;

        pdvar->Vec = NewVector(pdvar->NRow); // nicht mit VectorCopy anlegen, da auch echte double übergeben werden könnten
        //pdvar->pVec = &vec;
		    //pVecDebugInterim = pdvar->pVec;
        for(i=0;i<pdvar->NVal;i++)
            pdvar->Vec[i]  = vecget[i];

         }
        break;
    case DEF_MAT:
      {
		    Matrix_t *pmat = (Matrix_t *)pval;
		    Matrix_t matget = *pmat;
        //double *pd = (double *)pval;
        //Vector_t vec;

        pdvar->NRow    = nrows;
        pdvar->NCol    = ncols;
        pdvar->NVal    = ncols*nrows;
        if( pdvar->NVal == 0 )
            pdvar->NRow = 0;

        pdvar->YFactor  = yfactor;
        pdvar->YOffset  = yoffset;

        pdvar->Mat = MatrixCopy(matget);

      }
      break;
    case DEF_STRING:
      {
        CSlfStr  *psval = (CSlfStr  *)pval;

        pdvar->NRow    = 1;
        pdvar->NCol    = 1;
        pdvar->NVal    = 1;

        pdvar->YFactor  = yfactor;
        pdvar->YOffset  = yoffset;

        pdvar->StrMVal.clear();
        
        pdvar->StrMVal.cpy(*psval,0,0);
        
        pdvar->Type    = DEF_STRINGM;
        pdvar->pVal = 0;
      }
      break;
    case DEF_STRINGV:
        {
        CSlfStrV  *psv = (CSlfStrV  *)pval;

        pdvar->NRow    = psv->getNrows();
        pdvar->NCol    = 1;
        pdvar->NVal    = pdvar->NRow * pdvar->NCol;

        pdvar->YFactor  = yfactor;
        pdvar->YOffset  = yoffset;

        for(i=0;i<pdvar->NRow;i++)
            for(j=0;j<pdvar->NCol;j++)
                pdvar->StrMVal.cpy(psv->get_str(i),i,j);

        pdvar->Type    = DEF_STRINGM;
        pdvar->pVal = 0;
        }
        break;
        
    case DEF_STRINGM:
        {
        CSlfStrM  *psm = (CSlfStrM  *)pval;

        pdvar->NRow    = psm->getNrows();
        pdvar->NCol    = psm->getNcols();
        pdvar->NVal    = pdvar->NRow * pdvar->NCol;

        pdvar->YFactor  = yfactor;
        pdvar->YOffset  = yoffset;

        for(i=0;i<pdvar->NRow;i++)
            for(j=0;j<pdvar->NCol;j++)
                pdvar->StrMVal.cpy(psm->get_str(i,j),i,j);

        pdvar->pVal = 0;
        }
        break;
        


    case DEF_1D_TAB:
        {
        CSlf1DTab *p1dtab = (CSlf1DTab  *)pval;

        pdvar->p1DTab = new CSlf1DTab;

        pdvar->p1DTab->set(p1dtab);

        pdvar->NRow    = 1;
        pdvar->NCol    = 1;
        pdvar->NVal    = 1;

        pdvar->XFactor = xfactor;
        pdvar->XOffset = xoffset;

        pdvar->YFactor = yfactor;
        pdvar->YOffset = yoffset;

        }
        break;
    case DEF_2D_TAB:
        {
        CSlf2DTab *p2dtab = (CSlf2DTab  *)pval;

        pdvar->p2DTab = new CSlf2DTab;

        pdvar->p2DTab->set(p2dtab);

        pdvar->NRow    = 1;
        pdvar->NCol    = 1;
        pdvar->NVal    = 1;

        pdvar->XFactor = xfactor;
        pdvar->XOffset = xoffset;

        pdvar->YFactor = yfactor;
        pdvar->YOffset = yoffset;

        pdvar->ZFactor = zfactor;
        pdvar->ZOffset = zoffset;

        }
        break;
    }

    return Status;

}
//==========================================================================
//==========================================================================
//==========================================================================
//==========================================================================
// find Functions ==========================================================
//==========================================================================
//==========================================================================
//==========================================================================
SDsParDataGroup *CDsParData::findGroup(CSlfStr  &instanz     // Instanzname
                                  ,CSlfStrV &vgroup       // Gruppenhierachie
                                  ) {

    uint8_t        find_flag;
    SDsParDataGroup *pdi,*pdg;
    uint32_t       i;
    

    // Instanz suchen
    //===============
    find_flag = 0;
    pdi        = pData;
    while(pdi) {

        if( pdi->Name == instanz ) {

            find_flag = 1;
            break;
        } else {
            pdi = pdi->pNext;
        }
    }

    if( !find_flag ) { // nicht gefunden

        return 0;
    }

    //Gruppen suchen
    //==============
    if( vgroup.getNrows() == 0 ) { // keine Gruppen angegeben

        pdg = pdi;

    } else { // Gruppenhierachie durchgehen

        for(i=0;i<vgroup.getNrows();i++) {

            CSlfStr gname = vgroup.get_str(i);

            pdg        = pdi->pSub;
            find_flag = 0;

            // Gruppe suchen
            //==============
            while(pdg) {

                if( pdg->Name == gname ) {

                    find_flag = 1;
                    break;
                } else {
                    pdg = pdg->pNext;
                }
            }

            if( !find_flag ) { // nicht gefunden

                return 0;
            }

            pdi = pdg;
        }
    }

    return pdg;

}
SDsParDataGroup *CDsParData::findorbuildGroup(CSlfStr  &instanz     // Instanzname
                                           ,CSlfStrV &vgroup       // Gruppenhierachie
                                           ) {

    uint8_t        find_flag;
    SDsParDataGroup *pdi,*pdg;
    uint32_t       i;
    

    // Instanz suchen
    //===============
    find_flag = 0;
    pdi        = pData;
    while(pdi) {

        if( pdi->Name == instanz ) {

            find_flag = 1;
            break;
        } else {
            pdi = pdi->pNext;
        }
    }

    // Instanz anlegen
    //================
    if( !find_flag ) { // neue Instanz anlegen

        pdi = new SDsParDataGroup;

        pdi->Name    = instanz;
        pdi->Comment = "";
        pdi->Hierach = 0;
        pdi->NSub    = 0;
        pdi->NVar    = 0;
        pdi->pSub    = 0;
        pdi->pVar    = 0;

        pdi->pNext   = pData;
        pData        = pdi;
    }

    //Gruppen suchen
    //==============
    if( vgroup.getNrows() == 0 ) { // keine Gruppen angegeben

        pdg = pdi;

    } else { // Gruppenhierachie durchgehen

        for(i=0;i<vgroup.getNrows();i++) {

            CSlfStr gname = vgroup.get_str(i);

            pdg        = pdi->pSub;
            find_flag = 0;

            // Gruppe suchen
            //==============
            while(pdg) {

                if( pdg->Name == gname ) {

                    find_flag = 1;
                    break;
                } else {
                    pdg = pdg->pNext;
                }
            }

            // Gruppe anlegen
            //================
            if( !find_flag ) { // neue Instanz anlegen

                pdg = new SDsParDataGroup;

                pdg->Name    = gname;
                pdg->Comment = "";
                pdg->Hierach = i+1;
                pdg->NSub    = 0;
                pdg->NVar    = 0;
                pdg->pSub    = 0;
                pdg->pVar    = 0;

                pdg->pNext   = pdi->pSub;
                pdi->pSub    = pdg;
            }

            pdi = pdg;
        }
    }

    return pdg;

}
SDsParDataVariable *CDsParData::findVariable(SDsParDataGroup  *pdg  // Pointer auf Gruppe
                                            ,CSlfStr &varname       // Variablenname
                                            ) {


    SDsParDataVariable *pdvar    = pdg->pVar;
    uint8_t              find_flag = 0;


    // Variable suchen
    //================
    while(pdvar) {

        if( pdvar->Name == varname ) {

            find_flag = 1;
            break;
        } else {
            pdvar = pdvar->pNext;
        }
    }

    // Variable nicht gefunden
    //================
    if( !find_flag ) { // neue Instanz anlegen

        return 0;
    }

    return pdvar;
}
SDsParDataVariable *CDsParData::findorbuildVariable(SDsParDataGroup  *pdg  // Pointer auf Gruppe
                                                   ,CSlfStr &varname       // Variablenname
                                                   ) {


    SDsParDataVariable *pdvar     = pdg->pVar;
    uint8_t      find_flag = 0;


    // Variable suchen
    //================
    while(pdvar) {

        if( pdvar->Name == varname ) {

            find_flag = 1;
            break;
        } else {
            pdvar = pdvar->pNext;
        }
    }

    // Variable anlegen
    //================
    if( !find_flag ) { // neue Variable

        pdvar = new SDsParDataVariable;

        pdvar->Name    = varname;
        pdvar->XFactor  = 1.0;
        pdvar->XOffset  = 0.0;
        pdvar->YFactor  = 1.0;
        pdvar->YOffset  = 0.0;
        pdvar->ZFactor  = 1.0;
        pdvar->ZOffset  = 0.0;
        pdvar->ILine   = -1;
        pdvar->NCol    = 0;
        pdvar->NRow    = 0;
        pdvar->NVal    = 0;
        pdvar->Type    = DEF_VOID;
        pdvar->pVal    = 0;
        pdvar->p1DTab  = 0;
        pdvar->p2DTab  = 0;
        //pdvar->pVec    = 0;
		pdvar->Vec     = 0;
		pdvar->Mat     = 0;

        pdvar->pNext   = pdg->pVar;
        pdg->pVar      = pdvar;
    }

    return pdvar;
}
//==========================================================================
//==========================================================================
//==========================================================================
//==========================================================================
// delete Functionq ========================================================
//==========================================================================
//==========================================================================
//==========================================================================
void CDsParData::deleteParStruct(SDsParDataGroup *pdg) {


    SDsParDataGroup *pdg1;
    while( pdg ) {

        if( pdg->pSub ) {
            deleteParStruct(pdg->pSub);
        }

        if( pdg->pVar ) {
            deleteVarStruct(pdg->pVar);
        }
        pdg1 = pdg->pNext;
        delete pdg;
        pdg = pdg1;
    }
}
void CDsParData::deleteVarStruct(SDsParDataVariable *pdvar) {


    SDsParDataVariable *pdvar1;
    while( pdvar ) {

        deleteVal(pdvar);

        pdvar1 = pdvar->pNext;
        delete pdvar;
        pdvar = pdvar1;
    }
}
void CDsParData::deleteVal(SDsParDataVariable *pdvar) {

    

    if( pdvar ) {

        switch(pdvar->Type) {

        case DEF_DOUBLE:
            {
            double *pd = (double *)pdvar->pVal;
            
            delete []pd;

            pdvar->pVal = 0;
            }
            break;
        case DEF_STRINGM:
        case DEF_STRING:
            pdvar->StrMVal.clear();
            break;
        case DEF_VEC:
            {
            //Vector_t *pvec = (Vector_t *);
            //Vector_t vec   = *pvec;
            FreeVector(pdvar->Vec);
            pdvar->Vec = 0;
            }
            break;
        case DEF_MAT:
            {
            //Vector_t *pvec = (Vector_t *);
            //Vector_t vec   = *pvec;
            FreeMatrix(pdvar->Mat);
            pdvar->Mat = 0;
            }
            break;
        case DEF_1D_TAB:
            {
            CSlf1DTab *p1dtab = (CSlf1DTab  *)pdvar->p1DTab;

            delete p1dtab;

            pdvar->p1DTab = 0;
            }
            break;
        case DEF_2D_TAB:
            {
            CSlf2DTab *p2dtab = (CSlf2DTab  *)pdvar->p2DTab;

            delete p2dtab;

            pdvar->p2DTab = 0;
            }
            break;
        }
        pdvar->Type = DEF_VOID;
    }
}
//==========================================================================
//==========================================================================
//==========================================================================
//==========================================================================
// get Functions ===========================================================
//==========================================================================
//==========================================================================

//==========================================================================
// Anzahl der Instanzen (nhierachie = 1, bzw. ihierachie = 0)
//===========================================================
uint32_t CDsParData::getNInstanz(void) {

    uint32_t       n   = 0;
    SDsParDataGroup *pdi = pData;

    while(pdi) {
        n++;
        pdi = pdi->pNext;
    }
    return n;
}
// Name der iten-Instanzen
//========================
char  *CDsParData::getNameInstanz(uint32_t iinst) {

    SDsParDataGroup *pdi = pData;
    uint32_t       i   = 0;
    while(pdi) {
        if( i == iinst )
            return pdi->Name.c_str();
        i++;
        pdi = pdi->pNext;
    }

    return 0;
}
// Besteht die Gruppenhierachie
//=============================
bool   CDsParData::existGroup(CSlfStr &instanz,CSlfStrV &vgroup) {

    if( findGroup(instanz,vgroup) )
        return true;
    else
        return false;
}

// Anzahl der Untergruppen
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
uint32_t CDsParData::getNSubGroup(uint32_t *group_list,uint32_t nh) {

    uint32_t       n;
    SDsParDataGroup *pdg;


    if( (pdg=getGroup(group_list,nh)) == 0 )
        return 0;

    pdg = pdg->pSub;
    n  = 0;
    while(pdg) {
        n++;
        pdg = pdg->pNext;
    }
    return n;

}
// Name der iten Untergruppen
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
char  *CDsParData::getNameGroup(uint32_t *group_list
                                ,uint32_t nh) {
    SDsParDataGroup *pdg;

    if( (pdg=getGroup(group_list,nh)) == 0 )
        return 0;
    return pdg->Name.c_str();
}
// Existiert Kommentar der iten Untergruppen
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
bool   CDsParData::existCommentGroup(uint32_t *group_list,uint32_t nh) {
    
    SDsParDataGroup *pdg;
    if( (pdg=getGroup(group_list,nh)) ) {
        if( pdg->Comment.getLen() > 0 )
            return true;
        else
            return false;
    } else {
        return false;
    }

}
// Geibt Kommentar der iten Untergruppen zurück
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
char  *CDsParData::getCommentGroup(uint32_t *group_list,uint32_t nh) {
    SDsParDataGroup *pdg;

    if( (pdg=getGroup(group_list,nh)) == 0 )
        return 0;
    return pdg->Comment.c_str();
}

// Name der iten Untergruppen
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
char  *CDsParData::getNameSubGroup(uint32_t *group_list
                                   ,uint32_t nh
                                   ,uint32_t ig) {
    uint32_t       i;
    SDsParDataGroup *pdg;

    if( (pdg=getGroup(group_list,nh)) == 0 )
        return 0;
    pdg = pdg->pSub;
    
    i  = 0;
    while(pdg) {
        if( i == ig )
            return pdg->Name.c_str();
        i++;
        pdg = pdg->pNext;
    }
    return 0;

}
// Pointer der iten Untergruppen
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
SDsParDataGroup *CDsParData::getGroup(uint32_t *group_list,uint32_t nh) {

    uint32_t       i;
    SDsParDataGroup *pdg = pData;

    for(i=0;i<group_list[0];i++) {

        pdg = pdg->pNext;
        if( pdg == 0 )
            return 0;
    }
    for(i=1;i<nh;i++) {

            pdg = getSubGroup(pdg,group_list[i]);
            if( pdg == 0 )
                return 0;
    }
   
    return pdg;
}
// Pointer der iten Untergruppen
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// subgroup mit index gesucht
//=======================================================================
SDsParDataGroup *CDsParData::getSubGroup(SDsParDataGroup *pdg,uint32_t index) {

    uint32_t i=0;

    pdg = pdg->pSub;
    while(pdg) {
        if( i == index)
            return pdg;
        i++;
        pdg = pdg->pNext;
    }
    return 0;
}

// Besteht die Gruppenhierachie
//=============================
bool   CDsParData::existVar(CSlfStr &instanz,CSlfStrV &vgroup,CSlfStr &varname) {

    SDsParDataGroup      *pdg;
    if( (pdg=findGroup(instanz,vgroup)) ) {
        if( findVariable(pdg,varname) )
            return true;
        else
            return false;
    } else {
        return false;
    }
    
}
// Variablenstruktur zurückgeben
//=============================
SDsParDataVariable *CDsParData::getVar(CSlfStr &instanz
                                  ,CSlfStrV &vgroup
                                  ,CSlfStr &varname) {

    SDsParDataGroup      *pdg = findGroup(instanz,vgroup);

    if( pdg ) {
        SDsParDataVariable   *pdvar = findVariable(pdg,varname);
        if( pdvar)
            return pdvar;
    }

    return 0;
}
// Anzahl der Variablen dieser Gruppe
//=======================================================================
SDsParDataVariable *CDsParData::getVar(SDsParDataGroup *pdg,uint32_t iv) {

    SDsParDataVariable   *pdvar = pdg->pVar;
    uint32_t       i   = 0;

    if( pdg )
        pdvar = pdg->pVar;
    else
        return 0;

    while(pdvar) {
        if( i == iv )
            return pdvar;
        i++;
        pdvar = pdvar->pNext;
    }
    return 0;
}
// Anzahl der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
//=======================================================================
uint32_t CDsParData::getNVar(uint32_t *group_list
                                 ,uint32_t nh) {

    uint32_t       i;
    SDsParDataGroup *pdg;
    SDsParDataVariable   *pdvar;

    if( (pdg=getGroup(group_list,nh)) == 0 )
        return 0;

    pdvar = pdg->pVar;
    i  = 0;
    while(pdvar) {
        i++;
        pdvar = pdvar->pNext;
    }
    return i;

}
// Name der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
char  *CDsParData::getNameVar(uint32_t *group_list
                                   ,uint32_t nh
                                   ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return pdvar->Name.c_str();
    else
        return 0;
}
// Einheit der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
char  *CDsParData::getUnitVar(uint32_t *group_list
                                   ,uint32_t nh
                                   ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return pdvar->Unit.c_str();
    else
        return 0;
}
// Kommentar der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
char  *CDsParData::getCommentVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return pdvar->Comment.c_str();
    else
        return 0;
}

// Typ der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
EVarType CDsParData::getTypeVar(uint32_t *group_list
                                        ,uint32_t nh
                                        ,uint32_t iv) {
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return pdvar->Type;
    else
        return DEF_VOID;
}

// Spaltenanzahl der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
uint32_t CDsParData::getNColVar(uint32_t *group_list
                             ,uint32_t nh
                             ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return pdvar->NCol;
    else
        return 0;
}

// Reihenanzahl der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
uint32_t CDsParData::getNRowVar(uint32_t *group_list
                             ,uint32_t nh
                             ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return pdvar->NRow;
    else
        return 0;
}

// Reihenanzahl der Variablen dieser Gruppe
// group_list beinhaltet die Hierachie indizes, beginnent bei der Instanz
// iv-te Variable
//=======================================================================
CSlfStr *CDsParData::getVStringVar(uint32_t *group_list
                                  ,uint32_t nh
                                  ,uint32_t iv
                                  ,uint32_t irow
                                  ,uint32_t icol) {
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    
    if( pdvar ) {

        if( pdvar->NRow >0 && pdvar->NCol > 0 ) {
            
            irow = MIN(irow,pdvar->NRow-1);
            icol = MIN(icol,pdvar->NCol-1);

            return pdvar->StrMVal.get(irow,icol);
        } else
            return 0;
    } else
        return 0;
}
double  *CDsParData::getVDoubleVar(uint32_t *group_list
                                        ,uint32_t nh
                                        ,uint32_t iv
                                        ,uint32_t irow
                                        ,uint32_t icol) {
    
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);
    double *pdouble;

    
    if( pdvar ) {

        if( pdvar->NRow >0 && pdvar->NCol > 0 ) {
            
            irow = MIN(irow,pdvar->NRow-1);
            icol = MIN(icol,pdvar->NCol-1);

            pdouble = (double *)pdvar->pVal;

            if( pdouble ) 
                return pdouble+icol*pdvar->NRow+irow;

            else
                return 0;
        } else
            return 0;
    } else
        return 0;
}
uint32_t  *CDsParData::getVUint32Var(uint32_t *group_list
                                        ,uint32_t nh
                                        ,uint32_t iv
                                        ,uint32_t irow
                                        ,uint32_t icol) {
    
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);
    uint32_t           *puint32_t;
    
    if( pdvar ) {

        if( pdvar->NRow >0 && pdvar->NCol > 0 ) {
            
            irow = MIN(irow,pdvar->NRow-1);
            icol = MIN(icol,pdvar->NCol-1);

            puint32_t = (uint32_t *)pdvar->pVal;

            if( puint32_t ) 

                return puint32_t+icol*pdvar->NRow+irow;

            else
                return 0;
        } else
            return 0;
    } else
        return 0;
}
sint32_t  *CDsParData::getVSint32Var(uint32_t *group_list
                                        ,uint32_t nh
                                        ,uint32_t iv
                                        ,uint32_t irow
                                        ,uint32_t icol) {
    
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);
    sint32_t           *psint32;

    
    if( pdvar ) {

        if( pdvar->NRow >0 && pdvar->NCol > 0 ) {
            
            irow = MIN(irow,pdvar->NRow-1);
            icol = MIN(icol,pdvar->NCol-1);

            psint32 = (sint32_t *)pdvar->pVal;

            if( psint32 ) 

                return psint32+icol*pdvar->NRow+irow;

            else
                return 0;
        } else
            return 0;
    } else
        return 0;
}

CSlf1DTab  *CDsParData::getV1DTabVar(uint32_t *group_list
                                    ,uint32_t nh
                                    ,uint32_t iv) {
    
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    
    if( pdvar ) {

        return pdvar->p1DTab; 

    } else
        return 0;
}
CSlf2DTab  *CDsParData::getV2DTabVar(uint32_t *group_list
                                    ,uint32_t nh
                                    ,uint32_t iv) {
    
    SDsParDataGroup     *pdg   = getGroup(group_list,nh);
    SDsParDataVariable  *pdvar = getVar(pdg,iv);

    
    if( pdvar ) {

        return pdvar->p2DTab; 

    } else
        return 0;
}
Vector_t CDsParData::getVVecVar(uint32_t *group_list
                             ,uint32_t nh
                             ,uint32_t iv) {
    
    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    
    if( pdvar ) {

        return pdvar->Vec; 

    } else
        return 0;
}

double *CDsParData::getXFactorVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return &(pdvar->XFactor);
    else
        return 0;
}
double *CDsParData::getXOffsetVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return &(pdvar->XOffset);
    else
        return 0;
}
double *CDsParData::getYFactorVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return &(pdvar->YFactor);
    else
        return 0;
}
double *CDsParData::getYOffsetVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return &(pdvar->YOffset);
    else
        return 0;
}
double *CDsParData::getZFactorVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return &(pdvar->ZFactor);
    else
        return 0;
}
double *CDsParData::getZOffsetVar(uint32_t *group_list
                                      ,uint32_t nh
                                      ,uint32_t iv) {

    SDsParDataGroup      *pdg = getGroup(group_list,nh);
    SDsParDataVariable   *pdvar = getVar(pdg,iv);

    if( pdvar )
        return &(pdvar->ZOffset);
    else
        return 0;
}




