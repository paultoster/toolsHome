#include <stdarg.h>
#include <math.h>

#define SLFBASIC_VAR_TYPE_STR
#include "DsParMex.h"
#include "SlfSys.h"

#define IS_PARAM_DOUBLE(pVal) (mxIsNumeric(pVal) && !mxIsLogical(pVal) &&\
!mxIsEmpty(pVal) && !mxIsSparse(pVal) && !mxIsComplex(pVal) && mxIsDouble(pVal))

    
// Konstruktor CDsPar
CDsParMex::CDsParMex() {


    Status  = OKAY;
    ErrText = "";

    // Schlüsselwörter
    TYPE       = "type";
    INSTANZ    = "instanz";
    GROUP      = "group";
    NAME       = "name";
    VAL        = "val";
    UNIT       = "unit";
    COMMENT    = "comment";
    FACTOR     = "factor";
    OFFSET     = "offset";
    SPLITGROUP = ".";

    XNAME      = "xname";
    XVEC       = "xvec";
    XUNIT      = "xunit";
    XCOMMENT   = "xcomment";
    XFACTOR    = "xfactor";
    XOFFSET    = "xoffset";
    YNAME      = "yname";
    YVEC       = "yvec";
    YUNIT      = "yunit";
    YCOMMENT   = "ycomment";
    YFACTOR    = "yfactor";
    YOFFSET    = "yoffset";
    ZNAME      = "zname";
    ZMAT       = "zmat";
    ZUNIT      = "zunit";
    ZCOMMENT   = "zcomment";
    ZFACTOR    = "zfactor";
    ZOFFSET    = "zoffset";
    LIN        = "lin";

    TYPE_SINGLE[0] = "single";
    TYPE_SINGLE[1] = "scalar";
    TYPE_STRING    = "string";
    TYPE_VEC[0]    = "vec";
    TYPE_VEC[1]    = "vector";
    TYPE_1DTAB[0]  = "1dtable";
    TYPE_1DTAB[1]  = "table1d";
    TYPE_2DTAB[0]  = "2dtable";
    TYPE_2DTAB[1]  = "table2d";



}
// Destruktor 
CDsParMex::~CDsParMex() {



}
void CDsParMex::reset(void) {

    Status = OKAY;
    ErrText = "";

	GroupNameFilterList.clear();
}

status_t CDsParMex::read(const mxArray *par,CDsParData &par_data) {

    CSlfStr   instanz;
    CSlfStrV  vgroup;

    Status = OKAY;

    //Prüfen, ob Instanz über cell-array gemacht wird
    if( mxIsCell(par) ) {

        mwSize total_num_of_cells;
        mwIndex index;
        const mxArray *cell_element_ptr;
  
        total_num_of_cells = mxGetNumberOfElements(par); 

        for (index=0; index<total_num_of_cells; index++)  {

            cell_element_ptr = mxGetCell(par, index);

            //Das Element muß eine Struktur sein
            if( !mxIsStruct(cell_element_ptr) ) {

                ErrText.catFormat("Die cell-Elemente müssen Strukturen sein !!!"); 
                Status = NOT_OKAY;
                return Status;
            }

            // Instanz über index festlegen
            instanz.catFormat("%i",index);
            vgroup.clear();

            //Instanzparameter einlesen
            if( readGroup0(cell_element_ptr,index,par_data,instanz,vgroup) != OKAY )
                return NOT_OKAY;

        }

    } else if( mxIsStruct(par) ) {

        //mwSize num_of_elements;
        mwIndex index=0;
  
        //num_of_elements = mxGetNumberOfElements(par); 

        
        // Instanz über index festlegen
        instanz.catFormat("%i",index);
        vgroup.clear();

        //Instanzparameter einlesen
        if( readGroup0(par,index,par_data,instanz,vgroup) != OKAY )
            return NOT_OKAY;
        
    } else {


        ErrText.catFormat("Der übergebene Parameter muß ein Struktur oder cellarrays mit Strukturen sein !!!"); 
        Status = NOT_OKAY;
        return Status;
    }


    return Status;
}

status_t CDsParMex::readGroup0(const mxArray *par
                              ,mwIndex index
                              ,CDsParData &par_data
                              ,CSlfStr &instanz
                              ,CSlfStrV vgroup) {


    int      nfields = mxGetNumberOfFields(par);
    int      ifield;
    mxArray  *parray;
    const char *fieldname;
	uint16_t   iFilt=0;

    // Schleife über alle Strukturelemente
    //====================================
    for( ifield=0;ifield<nfields;ifield++) {


        parray = mxGetFieldByNumber(par, index, ifield);

        fieldname = mxGetFieldNameByNumber(par,ifield);

		// Weitermachen, wenn keine Filterlsite vorhanden, oder fieldname in
		// der Filterliste steht
		if( GroupNameFilterList.getNrows() == 0 || GroupNameFilterList.exist(fieldname) ) {

			if( isValue(parray) ) { // Prüfen, ob Wert oder neue Gruppe

				CSlfStr name = fieldname;

				if( readValue(parray,par_data,instanz,vgroup,name)  
					!= OKAY ) return NOT_OKAY;

			} else if( mxIsStruct(parray) ) { // eine Gruppe muß Struktur sein

				// Gruppename hinzufügen
				vgroup.append(fieldname);
            
				// weitersuchen
				if( readGroup(parray,index,par_data,instanz,vgroup) != OKAY )
					return NOT_OKAY;

				// Gruppenname wieder rauslöschen
				vgroup.delete_last();

			} else {

				ErrText.catFormat("Die Struktur mit dem Feldnamen <%s> der Parameter ist nicht korrekt\n"
								 ,fieldname);
				Status = NOT_OKAY;
				return NOT_OKAY;
			}
		}
    }

    return Status;
}
status_t CDsParMex::readGroup(const mxArray *par
                              ,mwIndex index
                              ,CDsParData &par_data
                              ,CSlfStr &instanz
                              ,CSlfStrV vgroup) {


    int      nfields = mxGetNumberOfFields(par);
    int      ifield;
    mxArray  *parray;
    const char *fieldname;

    // Schleife über alle Strukturelemente
    //====================================
    for( ifield=0;ifield<nfields;ifield++) {


        parray = mxGetFieldByNumber(par, index, ifield);

        fieldname = mxGetFieldNameByNumber(par,ifield);

        if( isValue(parray) ) { // Prüfen, ob Wert oder neue Gruppe

            CSlfStr name = fieldname;

            if( readValue(parray,par_data,instanz,vgroup,name)  
                != OKAY ) return NOT_OKAY;

        } else if( mxIsStruct(parray) ) { // eine Gruppe muß Struktur sein

            // Gruppename hinzufügen
            vgroup.append(fieldname);
            
            // weitersuchen
            if( readGroup(parray,index,par_data,instanz,vgroup) != OKAY )
                return NOT_OKAY;

			// Gruppenname wieder rauslöschen
			vgroup.delete_last();

        } else {

            ErrText.catFormat("Die Struktur mit dem Feldnamen <%s> der Parameter ist nicht korrekt\n"
                             ,fieldname);
            Status = NOT_OKAY;
            return NOT_OKAY;
        }
    }

    return Status;
}
bool CDsParMex::isValue(const mxArray *par) {

    int      nf = mxGetNumberOfFields(par);
    int      ifield;
    mxArray  *parray;
    const char *fn;
    uint8_t    find_flag = 0;

    // Schleife über alle Strukturelemente
    //====================================
    for( ifield=0;ifield<nf;ifield++) {


        parray = mxGetFieldByNumber(par, 0, ifield);

        fn = mxGetFieldNameByNumber(par,ifield);

        if(  !mxIsStruct(parray)  ){

            if(  (*fn == *VAL) 
              || (SlfStrLen(fn)>1 && *fn == *XVEC && *fn+1 == *XVEC+1)
              ){
                find_flag = 1;
                break;
            }
        }
    }

    if( find_flag )
        return true;

    return false;
}




status_t CDsParMex::readValue(const mxArray *par
                              ,CDsParData &par_data
                              ,CSlfStr &instanz
                              ,CSlfStrV &vgroup
                              ,CSlfStr &name) {


    int      nfields = mxGetNumberOfFields(par);
    int      ifield;
    mxArray  *parray;
    const char *fieldname;
    mxClassID  classid;



    // Variablen für die einzelnen Attribute
    CSlfStr  type;
    CSlfStr  unit;
    CSlfStr  comment;
    CSlfStr  xname, xunit, xcomment;
    CSlfStr  yname, yunit, ycomment;
    CSlfStr  zname, zunit, zcomment;
    double   *pdvalue = 0;
    double   *pdxvalue = 0;
    double   *pdyvalue = 0;
    double   *pdzvalue = 0;
    uint32_t   nrow=0,ncol=0;
    uint32_t   nxrow=0,nxcol=0;
    uint32_t   nyrow=0,nycol=0;
    uint32_t   nzrow=0,nzcol=0;
    uint8_t    lin_flag=1;
    CSlfStr  sval;

    double   xfactor = 1.0;
    double   xoffset = 0.0;

    double   yfactor = 1.0;
    double   yoffset = 0.0;
    
    double   zfactor = 1.0;
    double   zoffset = 0.0;


    // Schleife über alle Strukturelemente
    //====================================
    for( ifield=0;ifield<nfields;ifield++) {


        parray = mxGetFieldByNumber(par, 0, ifield);

        fieldname = mxGetFieldNameByNumber(par,ifield);
        classid = mxGetClassID(parray);


        if( mxIsChar(parray) ) { // String

            // Kopiert den String heraus
            char *p_text = new char[((size_t)mxGetM(parray) * (size_t)mxGetN(parray)+1)];
			if( p_text == NULL ) {
				ErrText.catFormat("malloc-Problem");
				Status = NOT_OKAY;
                return Status;
			}
			mxGetString(parray,p_text,mxGetM(parray)*mxGetN(parray)+1);

            // Feldname, der einen String zugewiesen bekommt
            // identifizieren
            //========================
            if( *fieldname == *TYPE ) {

		        if( SlfStrLen(p_text) == 0 ) {
					ErrText.catFormat("CDsParMex::readValue: Problem Variable <%s>. Type (field <%s>) is empty\n"
								 ,name.c_str()
								 ,fieldname);
					Status = NOT_OKAY;
		            delete []p_text;
					return Status;
				}
                type = p_text;

            } else if( *fieldname == *VAL ) {

                sval = p_text;

            } else if( *fieldname == *COMMENT ) {

                comment = p_text;

            } else if( *fieldname == *UNIT ) {

                unit = p_text;
            
            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *XNAME
                     && *(fieldname+1) == *(XNAME+1)
                     ) {

                xname = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *XUNIT
                     && *(fieldname+1) == *(XUNIT+1)
                     ) {

                xunit = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *XCOMMENT
                     && *(fieldname+1) == *(XCOMMENT+1)
                     ) {

                xcomment = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *YNAME
                     && *(fieldname+1) == *(YNAME+1)
                     ) {

                yname = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *YUNIT
                     && *(fieldname+1) == *(YUNIT+1)
                     ) {

                yunit = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname == *YCOMMENT
                     && *fieldname+1 == *YCOMMENT+1
                     ) {

                ycomment = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *ZNAME
                     && *(fieldname+1) == *(ZNAME+1)
                     ) {

                zname = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *ZUNIT
                     && *(fieldname+1) == *(ZUNIT+1)
                     ) {

                zunit = p_text;

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     == *ZCOMMENT
                     && *(fieldname+1) == *(ZCOMMENT+1)
                     ) {

                zcomment = p_text;
			}
            //String wieder freigeben
            delete []p_text;
            
        
        } else if( mxIsClass(parray,"double") /*IS_PARAM_DOUBLE(parray)*/ ) {


            if( *fieldname == *VAL ) {

                nrow  = (uint32_t)mxGetM(parray);
                ncol  = (uint32_t)mxGetN(parray);

                pdvalue = new double[nrow*ncol];
				if( pdvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdvalue, mxGetPr(parray), sizeof(double)*nrow*ncol);

            } else if(  *fieldname == *FACTOR ) {

                yfactor = *mxGetPr(parray);

            } else if(  *fieldname == *OFFSET ) {

                yoffset = *mxGetPr(parray);

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     ==   *XVEC
                     && *(fieldname+1) ==   *(XVEC+1) ) {

                nxrow  = (uint32_t)mxGetM(parray);
                nxcol  = (uint32_t)mxGetN(parray);

                pdxvalue = new double[nxrow*nxcol];
				if( pdxvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdxvalue, mxGetPr(parray), sizeof(double)*nxrow*nxcol);

            
            } else if(  SlfStrLen(fieldname) > 1
                     && *fieldname          == *XFACTOR
                     && *(fieldname+1)      == *(XFACTOR+1) ) {

                xfactor = *mxGetPr(parray);

            } else if(  SlfStrLen(fieldname) > 1
                     && *fieldname          == *XOFFSET
                     && *(fieldname+1)      == *(XOFFSET+1) ) {

                xoffset = *mxGetPr(parray);

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     ==   *YVEC
                     && *(fieldname+1) ==   *(YVEC+1) ) {

                nyrow  = (uint32_t)mxGetM(parray);
                nycol  = (uint32_t)mxGetN(parray);

                pdyvalue = new double[nyrow*nycol];
				if( pdyvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdyvalue, mxGetPr(parray), sizeof(double)*nyrow*nycol);

            
            } else if(  SlfStrLen(fieldname) > 1
                     && *fieldname          == *YFACTOR
                     && *(fieldname+1)      == *(YFACTOR+1) ) {

                yfactor = *mxGetPr(parray);

            } else if(  SlfStrLen(fieldname) > 1
                     && *fieldname          == *YOFFSET
                     && *(fieldname+1)      == *(YOFFSET+1) ) {

                yoffset = *mxGetPr(parray);

            } else if(  SlfStrLen(fieldname)>1
                     && *fieldname     ==   *ZMAT
                     && *(fieldname+1) ==   *(ZMAT+1) ) {

                nzrow  = (uint32_t)mxGetM(parray);
                nzcol  = (uint32_t)mxGetN(parray);

                pdyvalue = new double[nzrow*nzcol];
				if( pdyvalue == NULL ) {
					ErrText.catFormat("malloc-Problem");
					Status = NOT_OKAY;
                    return Status;
				}
			
                memcpy(pdzvalue, mxGetPr(parray), sizeof(double)*nzrow*nzcol);

            
            } else if(  SlfStrLen(fieldname) > 1
                     && *fieldname          == *ZFACTOR
                     && *(fieldname+1)      == *(ZFACTOR+1) ) {

                zfactor = *mxGetPr(parray);

            } else if(  SlfStrLen(fieldname) > 1
                     && *fieldname          == *ZOFFSET
                     && *(fieldname+1)      == *(ZOFFSET+1) ) {

                zoffset = *mxGetPr(parray);

            } else if( *fieldname == *LIN ) {

                if( fabs(*mxGetPr(parray)) > EPSILON )
                    lin_flag = 1;
                else
                    lin_flag = 0;
			
            }
        } else if(  mxIsLogical(parray)
                 || mxIsNumeric(parray)
                 ) {

            if( *fieldname == *LIN ) {

                if( fabs(mxGetScalar(parray)) > EPSILON )
                    lin_flag = 1;
                else
                    lin_flag = 0;
			
            }

        } else {
            ErrText.catFormat("CDsParMex::readValue: Problem Variable <%s> with field <%s> is not detected\n"
                             ,name.c_str()
                             ,fieldname);
            Status = NOT_OKAY;
            return Status;

        }
    } // ende fields


    // Type bestimmen
    if( type == TYPE_SINGLE[0] || type == TYPE_SINGLE[1] ) {

        if( par_data.setSingleData(&instanz
                                  ,&vgroup
                                  ,&name
                                  ,&unit
                                  ,yfactor
                                  ,yoffset
                                  ,&comment
                                  ,*pdvalue
                                  ) != OKAY ) {

            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }
    } else if( type == TYPE_STRING ) {

        if( par_data.setStringData(&instanz
                                  ,&vgroup
                                  ,&name
                                  ,&unit
                                  ,yfactor
                                  ,yoffset
                                  ,&comment
                                  ,&sval
                                  ) != OKAY ) {

            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }
    } else if( type == TYPE_VEC[0] || type == TYPE_VEC[1] ) {

        if( par_data.setVectorData(&instanz
                                  ,&vgroup
                                  ,&name
                                  ,&unit
                                  ,yfactor
                                  ,yoffset
                                  ,&comment
                                  ,&pdvalue
                                  ,nrow*ncol
                                  ) != OKAY ) {

            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }
    } else if( type == TYPE_1DTAB[0] || type == TYPE_1DTAB[1] ) {

        if( par_data.set1DTableData(&instanz
                                   ,&vgroup
                                   ,&name
                                   ,&comment
                                   ,&xname
                                   ,&xunit
                                   ,xfactor
                                   ,xoffset
                                   ,&xcomment
                                   ,nxrow*nxcol
                                   ,pdxvalue
                                   ,&yname
                                   ,&yunit
                                   ,yfactor
                                   ,yoffset
                                   ,&ycomment
                                   ,nyrow*nycol
                                   ,pdyvalue
                                   ,lin_flag
                                  ) != OKAY ) {

            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }
    } else {

        ErrText.catFormat("Problem CDsParMex::readValue: Der type <%s> der Variable <%s> aus Gruppe <"
                         ,type.c_str()
                         ,name.c_str());
        for(uint16_t k=0;k<vgroup.getNrows();k++)
            ErrText.cat(vgroup.get_str(k));
        ErrText.cat("> konnte nicht identifiziert werden (oder noch nicht programmiert) !!!\n");

        Status = NOT_OKAY;
    }

    if( pdvalue )  delete []pdxvalue;
    if( pdxvalue ) delete []pdxvalue;
    if( pdyvalue ) delete []pdyvalue;
    if( pdzvalue ) delete []pdzvalue;


    return Status;
}
void CDsParMex::addFilterGroup(char *pstring) {


	GroupNameFilterList.append(pstring);

}
//===================================================================================
// Einlesen einer einfacheren Strucktur
// z.B. par.thz2e.pc_dia = 25.56;
//      par.thz2e.sc_dia = 23.81;
// Dazu wird eine Struktur mit {groups,name,unit,typ,xvecname,yvecname,comment} an die
// read-Funktion übergeben, um die fehlenden Werte festzulegen
//====================================================================================
status_t CDsParMex::readSimpleStruct(const mxArray *par,CDsParData &par_data
                                    ,SDsParMexSimpleStruct *pvardes,uint16_t nvardes) {

    CSlfStr   instanz;
    CSlfStrV  vgroup;
    CSlfStr   varname;
    uint16_t    ivardes;
    const mxArray   *par1;
    uint32_t    n;

    Status = OKAY;

    //Prüfen, ob struct
    if( !par || !mxIsStruct(par) ) {
        ErrText.catFormat("Error readSimpleStruct: Der übergebene Parameter muß ein Struktur sein !!!"); 
        Status = NOT_OKAY;
        return Status;
    }
    if( !isSimpleStruct(par) ) {
        ErrText.catFormat("Error readSimpleStruct: Der übergebene Parameter ist keine Simple Struktur !!!"); 
        Status = NOT_OKAY;
        return Status;
    }

    // Liste der beschriebenen Variablen durchgehen
    for(ivardes=0;ivardes<nvardes;ivardes++) {

        n = SlfStrVSplit( vgroup, pvardes[ivardes].varname, SPLITGROUP);
        if( n == 0 ) { // garkein Name entahlten, gehe weiter
            break;
        } else { // varname extrahieren und gruppenhierachie vgroup bilden
            varname = vgroup.get_str(vgroup.getNrows()-1);
            vgroup.delete_last();
        }

        // Gruppenhierchie zerlegen
        if( vgroup.getNrows() == 0 ) // keine Gruppe
            par1 = par;
        else // Gruppe in par suchen
            par1 = findGroupSimpleStruct(vgroup,par);

        if( par1 ) {// Wenn Gruppe gefunden, dann weitersuchen

            //mwSize     num_of_elements= mxGetNumberOfElements(par1);
            mwIndex    index=0;
            int        nfields = mxGetNumberOfFields(par1);
            int        ifield;
            mxArray    *parray;
            const char *fieldname;

            // Instanz über index festlegen
            instanz.clear();
            instanz.catFormat("%i",index);

            // Schleife über alle Strukturelemente
            //====================================
            for( ifield=0;ifield<nfields;ifield++) {

                parray    = mxGetFieldByNumber(par1, index, ifield);
                fieldname = mxGetFieldNameByNumber(par1,ifield);


                if( varname.compare(fieldname)  ) {
                        

                    switch(pvardes[ivardes].type) {

                    case DEF_DOUBLE:
                    case DEF_CHAR_STRING:
                    case DEF_ARR_DOUBLE:
                    case DEF_VEC:

                        // Wert und Struktur übergeben
                        if( readValueSimpleStructDS(parray,par_data,instanz,vgroup
                                                   ,varname,&pvardes[ivardes]) != OKAY )
                            return NOT_OKAY;
                        break;
                    case DEF_1D_TAB:

                        //Es muß eine Zuordnung zu dem X-Vector_t gefunden werden,
                        //wenn nicht, dann ist dieses Feld selbst ein x-Vektor
                        if( SlfStrLen(pvardes[ivardes].xvarname) ) {

                            mxArray    *pxarray;
                            const char *xfieldname;
                            int        ixfield;
                            uint16_t     ixvardes;
                            CSlfStrV   vxgroup;
                            CSlfStr    xvarname;

                            // Struktur auf x-Vektorbeschreibung prüfen
                            // Liste nach x-Vektor suchen
                            for(ixvardes=0;ixvardes<nvardes;ixvardes++) {
                            
                                if( SlfStrCompare(pvardes[ixvardes].varname
                                                 ,pvardes[ivardes].xvarname) )

                                                 break;
                            }
                            // nicht gefunden
                            if( ixvardes == nvardes ) { 

                                ErrText.catFormat("CDsParMex::readSimpleStruct: Parameter <%s> Typ <%s>: could not find x-Vector_t <%s> in internal struct-list\n"
                                                 ,pvardes[ivardes].varname
                                                 ,VarTypeStr[pvardes[ivardes].type]
                                                 ,pvardes[ivardes].xvarname);
                                Status = NOT_OKAY;
                                return Status;
                            }                        
                            // Typ des x-Vektors überprüfen
                            if( pvardes[ixvardes].type != DEF_1D_TAB ) {

                                ErrText.catFormat("CDsParMex::readSimpleStruct: Parameter x-Vector_t <%s> in combination with y-Vector_t <%s> has wrong Typ <%s> must be (DEF_1D_TAB):\n"
                                                 ,pvardes[ixvardes].varname
                                                 ,pvardes[ivardes].varname
                                                 ,VarTypeStr[pvardes[ivardes].type]);
                                Status = NOT_OKAY;
                                return Status;
                            }

                            n = SlfStrVSplit( vxgroup, pvardes[ixvardes].varname, SPLITGROUP);
                            xvarname = vxgroup.get_str(vxgroup.getNrows()-1);
                            vxgroup.delete_last();

                            if( vxgroup != vgroup ) {
                                ErrText.catFormat("CDsParMex::readSimpleStruct: Parameter x-Vector_t <%s> in combination with y-Vector_t <%s> are not from same group:\n"
                                                 ,pvardes[ixvardes].varname
                                                 ,pvardes[ivardes].varname);
                                Status = NOT_OKAY;
                                return Status;
                            }

                            // x-Vektor
                            for( ixfield=0;ixfield<nfields;ixfield++) {

                                pxarray    = mxGetFieldByNumber(par1, 0, ixfield);
                                xfieldname = mxGetFieldNameByNumber(par1,ixfield);

                                if( xvarname.compare(xfieldname)   ) 
                                    break;
                            }

                            // Nicht gefunden
                            if( ixfield == nfields ) {
                                ErrText.catFormat("CDsParMex::readValueSimpleStruct: Parameter <%s> Typ <%s>: could not find x-Vector_t <%s> in input-struct\n"
                                                 ,pvardes[ivardes].varname
                                                 ,VarTypeStr[pvardes[ivardes].type]
                                                 ,pvardes[ivardes].xvarname);
                                Status = NOT_OKAY;
                                return Status;
                            }

                            // Wert und Struktur übergeben
                            if( readValueSimpleStruct1DTable(parray,par_data,instanz,vgroup,varname
                                                       ,&pvardes[ivardes],pxarray,xvarname,&pvardes[ixvardes]) != OKAY )
                                return NOT_OKAY;
                        }
                        
                        break;
                    default:
                        ErrText.catFormat("CDsParMex::readValueSimpleStruct: Typ <%s> of Variable <%s> is not programmed\n"
                                         ,VarTypeStr[pvardes[ivardes].type]
                                         ,fieldname);
                        Status = NOT_OKAY;
                        return Status;

                    }
                    break;
                }                
            }
        }
    }
    return Status;
}
status_t CDsParMex::readValueSimpleStructDS(const mxArray *parray
                                           ,CDsParData &par_data
                                           ,CSlfStr    &instanz
                                           ,CSlfStrV   &vgroup
                                           ,CSlfStr    &varname
                                           ,SDsParMexSimpleStruct *pvardes) {

    // Variablen für die einzelnen Attribute
    CSlfStr  name;
    CSlfStr  type;
    CSlfStr  unit;
    CSlfStr  comment;
    CSlfStr  xname, xunit, xcomment;
    double   *pdvalue = 0;
    char     *p_text  = 0;
    uint32_t   nrow=0,ncol=0;
    CSlfStr  sval;

    double   yfactor = 1.0;
    double   yoffset = 0.0;


    if( pvardes->type == DEF_CHAR_STRING ) { // Es soll ein STring sein

        // Wert prüfen
        if( !mxIsChar(parray) ) {
            ErrText.catFormat("CDsParMex::readValueSimpleStructDS: readed variable from input <%s> from group <%s> is not  a string class(char)\n"
                             ,pvardes->varname
                             ,vgroup.get_str(vgroup.getNrows()-1));
            Status = NOT_OKAY;
            return Status;
        }

        // Kopiert den String heraus
        p_text = new char[((size_t)mxGetM(parray) * (size_t)mxGetN(parray)+1)];
		if( p_text == NULL ) {
			ErrText.catFormat("malloc-Problem");
			Status = NOT_OKAY;
            return Status;
		}
		mxGetString(parray,p_text,mxGetM(parray)*mxGetN(parray)+1);

        name    = varname;
        unit    = pvardes->unit;
        comment = pvardes->comment;
        sval    = p_text;

        if( par_data.setStringData(&instanz
                                  ,&vgroup
                                  ,&name
                                  ,&unit
                                  ,yfactor
                                  ,yoffset
                                  ,&comment
                                  ,&sval
                                  ) != OKAY ) {

            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }

    } else if( pvardes->type == DEF_DOUBLE ) {


        // Wert prüfen
        if( !mxIsNumeric(parray) ) {
            ErrText.catFormat("CDsParMex::readValueSimpleStructDS: readed variable from input <%s> from group <%s> is not  a numeric value \n"
                             ,pvardes->varname
                             ,vgroup.get_str(vgroup.getNrows()-1));
            Status = NOT_OKAY;
            return Status;
        }
        
        nrow  = (uint32_t)mxGetM(parray);
        ncol  = (uint32_t)mxGetN(parray);

        pdvalue = new double[nrow*ncol];
		if( pdvalue == NULL ) {
			ErrText.catFormat("malloc-Problem");
			Status = NOT_OKAY;
            return Status;
		}
	
        memcpy(pdvalue, mxGetPr(parray), sizeof(double)*nrow*ncol);

        name    = varname;
        unit    = pvardes->unit;
        comment = pvardes->comment;

        if( par_data.setSingleData(&instanz
                                  ,&vgroup
                                  ,&name
                                  ,&unit
                                  ,yfactor
                                  ,yoffset
                                  ,&comment
                                  ,*pdvalue
                                  ) != OKAY ) {
            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }

    } else if(  (pvardes->type == DEF_ARR_DOUBLE)
             || (pvardes->type == DEF_VEC)
             ) {


        // Wert prüfen
        if( !mxIsNumeric(parray) ) {
            ErrText.catFormat("CDsParMex::readValueSimpleStructDS: readed variable from input <%s> from group <%s> is not  a numeric value \n"
                             ,pvardes->varname
                             ,vgroup.get_str(vgroup.getNrows()-1));
            Status = NOT_OKAY;
            return Status;
        }
        
        nrow  = (uint32_t)mxGetM(parray);
        ncol  = (uint32_t)mxGetN(parray);

        pdvalue = new double[nrow*ncol];
		
        if( pdvalue == NULL ) {
			    ErrText.catFormat("malloc-Problem");
			    Status = NOT_OKAY;
          return Status;
		    }
	
        memcpy(pdvalue, mxGetPr(parray), sizeof(double)*nrow*ncol);

        name    = varname;
        unit    = pvardes->unit;
        comment = pvardes->comment;

        if( par_data.setVectorData(&instanz
                                  ,&vgroup
                                  ,&name
                                  ,&unit
                                  ,yfactor
                                  ,yoffset
                                  ,&comment
                                  ,&pdvalue
                                  ,nrow*ncol
                                  ) != OKAY ) {
            Status = NOT_OKAY;
            if( par_data.getLenErrText() > 0 ) {

                ErrText.cat(par_data.getErrText());
                par_data.resetErrText();
            }
        }
    }

    if( pdvalue )  delete []pdvalue;
    if( p_text )   delete []p_text;

    return OKAY;
}
status_t CDsParMex::readValueSimpleStruct1DTable(const mxArray *parray
                                                ,CDsParData &par_data
                                                ,CSlfStr    &instanz
                                                ,CSlfStrV   &vgroup
                                                ,CSlfStr    &varname
                                                ,SDsParMexSimpleStruct *pvardes
                                                ,const mxArray *pxarray
                                                ,CSlfStr       &xvarname
                                                ,SDsParMexSimpleStruct *pxvardes) {

    // Variablen für die einzelnen Attribute
    CSlfStr  name,  comment;
    CSlfStr  xname, xunit, xcomment;
    CSlfStr  yname, yunit, ycomment;
    
    double   *pdxvalue = 0;
    double   *pdyvalue = 0;

    uint32_t   nyrow=0,nycol=0;
    uint32_t   nxrow=0,nxcol=0;
    CSlfStr  sval;

    double   yfactor = 1.0;
    double   yoffset = 0.0;
    double   xfactor = 1.0;
    double   xoffset = 0.0;


    // y-Wert prüfen
    if( !mxIsNumeric(parray) ) {
        
        ErrText.catFormat("CDsParMex::readValueSimpleStructDS: readed y-vec-variable from input <%s> from group <%s> is not  a numeric value \n"
                         ,varname.c_str()
                         ,vgroup.get_str(vgroup.getNrows()-1));
        Status = NOT_OKAY;
        return Status;
    }
    
    nyrow  = (uint32_t)mxGetM(parray);
    nycol  = (uint32_t)mxGetN(parray);

    pdyvalue = new double[nyrow*nycol];
	if( pdyvalue == NULL ) {
		ErrText.catFormat("malloc-Problem");
		Status = NOT_OKAY;
        return Status;
	}

    memcpy(pdyvalue, mxGetPr(parray), sizeof(double)*nyrow*nycol);

    // x-Wert prüfen
    if( !mxIsNumeric(pxarray) ) {
        ErrText.catFormat("CDsParMex::readValueSimpleStructDS: readed v-vec-variable from input <%s> from group <%s> is not  a numeric value \n"
                         ,xvarname.c_str()
                         ,vgroup.get_str(vgroup.getNrows()-1));
        Status = NOT_OKAY;
        return Status;
    }
    
    nxrow  = (uint32_t)mxGetM(parray);
    nxcol  = (uint32_t)mxGetN(parray);

    pdxvalue = new double[nxrow*nxcol];
	if( pdxvalue == NULL ) {
		ErrText.catFormat("malloc-Problem");
		Status = NOT_OKAY;
        return Status;
	}

    memcpy(pdxvalue, mxGetPr(pxarray), sizeof(double)*nxrow*nxcol);

    name     = varname;
    comment  = pvardes->comment;

    yname    = varname;
    yunit    = pvardes->unit;
    ycomment = pvardes->comment;

    xname    = xvarname;
    xunit    = pxvardes->unit;
    xcomment = pxvardes->comment;

    if( par_data.set1DTableData(&instanz
                               ,&vgroup
                               ,&name
                               ,&comment
                               ,&xname
                               ,&xunit
                               ,xfactor
                               ,xoffset
                               ,&xcomment
                               ,nxrow*nxcol
                               ,pdxvalue
                               ,&yname
                               ,&yunit
                               ,yfactor
                               ,yoffset
                               ,&ycomment
                               ,nyrow*nycol
                               ,pdyvalue
                               ) != OKAY ) {

        Status = NOT_OKAY;
        if( par_data.getLenErrText() > 0 ) {

            ErrText.cat(par_data.getErrText());
            par_data.resetErrText();
        }
    }


    if( pdxvalue )  delete []pdxvalue;
    if( pdyvalue )  delete []pdyvalue;

    return OKAY;
}
const mxArray *CDsParMex::findGroupSimpleStruct(CSlfStrV   &vgroup          // Stringvekktor mit Gruppennamenhierachie
                                               ,const mxArray *par) {                         // die auszuwertende Struktur von Simulink

    CSlfStr  group;
    uint16_t   i;
    uint8_t    found_flag=0;
    mwIndex  index;
    int      nfields;
    int      ifield;
    const char *fieldname;

    for(i=0;i<vgroup.getNrows();i++) {

        found_flag = 0;

        nfields = mxGetNumberOfFields(par);

        group = vgroup.get_str(i);
        group.elimAnfEndC();

        index=0;

        // Schleife über alle Strukturelemente
        //====================================
        for( ifield=0;ifield<nfields;ifield++) {

            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( group.compare(fieldname) && mxIsStruct(par) ) {

                par        = mxGetFieldByNumber(par, index, ifield);
                found_flag = 1;
                break;
            }
        }
    }    

    if( found_flag )
        return par;
    else
        return 0;
}
uint8_t CDsParMex::isSimpleStruct(const mxArray *par) {                         // die auszuwertende Struktur von Simulink

    CSlfStrV gliste = GroupNameFilterList;
    return isSimpleStructIntern(par,gliste);
}
uint8_t CDsParMex::isSimpleStructIntern(const mxArray *par,CSlfStrV &gliste) {                         // die auszuwertende Struktur von Simulink

    if( !par ) return 0;

    mwSize     num_of_elements= mxGetNumberOfElements(par);
    mwIndex    index;
    
    int        nfields = mxGetNumberOfFields(par);
    int        ifield;
        
    uint8_t flagt=0, flagn=0, flagv=0;
    const char *fieldname;

    for(index=0;index<num_of_elements;index++) {

        flagt = 0;
        flagv = 0;
        // Schleife über alle Strukturelemente
        //====================================
        for( ifield=0;ifield<nfields;ifield++) {

            const mxArray *par1 = mxGetFieldByNumber(par, index, ifield);
            
            fieldname = mxGetFieldNameByNumber(par,ifield);

            if( mxIsStruct(par1) ) { // ist noch eine Untergruppe da

                if(  gliste.isEmpty() // dann sowieso suchen
                  || gliste.exist_last(fieldname) // oder existiert in der Liste
                  ) {

                    gliste.delete_last();

                    if( isSimpleStructIntern(par1,gliste) ) return 1;
                    flagt = 1;
                    flagv = 1;
                }

            } else { // ist eine Variable


                if( SlfStrCompareCount(fieldname,TYPE) == SlfStrLen(fieldname) )
            
                    flagt++;

                else if(  SlfStrCompareCount(fieldname,VAL)  == SlfStrLen(fieldname) 
                       || SlfStrCompareCount(fieldname,XVEC) == SlfStrLen(fieldname)
                       || SlfStrCompareCount(fieldname,YVEC) == SlfStrLen(fieldname)
                       )
            
                    flagv++;
            }
        }

        // Wenn TYPE, NAME und VAL oder XVEC oder YVEC gefunden, ist es keine 
        // einfache Struktur
        if( flagt && flagv )
            return 0;
        else
            return 1;
    }
    return 0;
}



        