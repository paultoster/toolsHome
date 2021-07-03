#include <stdarg.h>

// Stringdefinition von VarTypeStr definieren
#define SLFBASIC_VAR_TYPE_STR
#include "DsPar.h"
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfNum.h"

  
#define DSPAR_CTRENNZEICHENGROUP "."
  
// Konstruktor CDsPar
CDsPar::CDsPar() {

    Status  = OKAY;
    ErrText = "";

    cTrennZeichGroup = DSPAR_CTRENNZEICHENGROUP;

    pLogFile  = 0;             // Logfilehandling
    LogFileSet = 0;            // Sagt ob LogFile geschrieben wird

    // globale Schrittweite, 
    // kann über setExtLoopTime(val); gesetzt werden
    ExtLoopTime  = 0.0;

    pDSParSetVar = 0;
    pDSSDsParSetGroup = 0;
}
// Konstruktor CDsPar
CDsPar::CDsPar(CSlfLogFile *plogfile) {

    Status  = OKAY;
    ErrText = "";

    cTrennZeichGroup = DSPAR_CTRENNZEICHENGROUP;


    if( plogfile == 0 ) { // kein Logfile benutzen

        pLogFile   = 0;
        LogFileSet = 0;

    } else { // Logfile anlegen

        pLogFile   = plogfile;
        LogFileSet = 1;
    }

    // globale Schrittweite public Variable, 
    // kann also auch anderswo initialisiert werden
    ExtLoopTime  = 0.0;

    pDSParSetVar = 0;
    pDSSDsParSetGroup = 0;

}
// Destruktor 
CDsPar::~CDsPar() {

    resetParData();
    setVarDelete();
    setGroupDelete();

}
status_t CDsPar::ini(CSlfLogFile *plogfile/*=0*/) {


    resetErrText();

    Status        = OKAY;

    if( plogfile ) { 

        pLogFile   = plogfile;
        LogFileSet = 1;
    }

    if( pDSParSetVar )
      setVarDelete();

    if( pDSSDsParSetGroup )
      setGroupDelete();

    return OKAY;
}
#if DS_PAR_USE_MEXLOAD != 0
status_t CDsPar::readMex(const mxArray *par)
{
  // Einlesen der Matalb-Parameter
  status_t status = DSParMex.read(par,ParData);

  // Status auswerten
  if( status != OKAY )
  {
    ErrText.catFormat("CDsPar::readMex: %s",DSParMex.getErrText());
  }
  return status;
}
#endif
#if DS_PAR_USE_PAR_FILE != 0
status_t CDsPar::readFile(char *par_file)
{
  // Einlesen der Matalb-Parameter
  status_t status = DSParFile.read(par_file,ParData);

  // Status auswerten
  if( status != OKAY )
  {
    ErrText.catFormat("CDsPar::readFile: %s",DSParFile.getErrText());
  }
  return status;
}
#endif
void CDsPar::resetParData(void) {

    ParData.reset();
    ParDataUsed.reset();

}
status_t CDsPar::done(void) {


    Status = OKAY;
    ErrText = "";
    resetParData();

    return Status;
}
status_t CDsPar::get(uint16_t iinst) {


	Status = getPar(iinst);

    //=======================
    // Ausgabe in das LogFile
    //=======================
#ifdef DS_DEBUG
    writeLogFile("CDsPar::get");
#else
    if( Status != OKAY)
        writeLogFile("CDsPar::get");
#endif

	return Status;
}


//==========================================================================
//==========================================================================
//==========================================================================
//==========================================================================
// get Functions ==========================================================
//==========================================================================
//==========================================================================
//==========================================================================

// get-functions für alle Parameter mit DsParSet() 
// (am besten in Konstruktur aufgerufen) gesetzt
// iinst ist die Instanz beginnend mit 0
//=====================================================================
status_t CDsPar::getPar(uint16_t iinst) {

    SDsParSetVar  *pvar = pDSParSetVar;
    CSlfStr        instanz;
    SDsParDataVariable *pvardata;
    CSlfStr        pstring;
    uint8_t          default_flag;
    uint8_t          found_flag;
    uint8_t          instanz_found_flag = 0;
 
    SDsParSetGroup *pgr = pDSSDsParSetGroup;

    // Istanz suchen
    //==============
    if( ParData.getNInstanz() > iinst ) {

        instanz = ParData.getNameInstanz(iinst);
        instanz_found_flag = 1;

    }

    if( proofSubGroupSwitch(iinst,instanz) != OKAY )
        return NOT_OKAY;


    while( pvar ) {

        // Nur wenn gesetzt, wird ausgewertet
        //===================================
        if( pvar->VarSet ) {


            if(  (  (pvar->Type == DEF_1D_TAB)
                 && SlfStrLen(pvar->XTabDefault)
                 && SlfStrLen(pvar->YTabDefault)
                 )
              ||  
                 (  (pvar->Type == DEF_2D_TAB)
                 && SlfStrLen(pvar->XTabDefault)
                 && SlfStrLen(pvar->YTabDefault)
                 && SlfStrLen(pvar->ZTabDefault)
                 )
              ||
                 SlfStrLen(pvar->Default)
              )
                default_flag = 1;
            else
                default_flag = 0;

        
            if( !default_flag && !instanz_found_flag){

                if( iinst == 0 ) {
                    ErrText.catFormat("Es sind keine Parameter eingelesen !!!\n");
                    ErrText.catFormat("Parameter <%s> [%s] (%s) von Modell <%s> hat kein default-Wert\n"
                                     ,pvar->VarName.c_str()
                                     ,pvar->Unit.c_str()
                                     ,pvar->Comment.c_str()
                                     ,pvar->ModName.c_str()
                                     );
                } else {
                    ErrText.catFormat("Es ist keine Parameterinstanz index=%i vorhanden !!!\n"
                                 ,iinst);
                }
                Status = NOT_OKAY;


                return Status;
            }


            //Gruppe suchen
            if(  ParData.existGroup(instanz,pvar->VGroup) ) {

                found_flag = 1;
            } else {
                found_flag = 0;
              if( !default_flag ) {

    
                ErrText.catFormat("Fuer Modul <%s> Instanz <%s> u. Gruppenhierachie <%s> konnte in den eingelesenen Parametern die Gruppe nicht gefunden werden !!!\n"
                                 ,pvar->ModName.c_str()
                                 ,instanz.c_str()
                                 ,pvar->GroupName.c_str());
                ErrText.catFormat("Parameter <%s> [%s] (%s) von Modell <%s> hat kein default-Wert\n"
                                 ,pvar->VarName.c_str()
                                 ,pvar->Unit.c_str()
                                 ,pvar->Comment.c_str()
                                 ,pvar->ModName.c_str()
                                 );
                Status = NOT_OKAY;

                writeLogFile("CDsPar::get");
                return Status;
              }
            }
            //Variable suchen
            if(  found_flag ) {

                if( ParData.existVar(instanz,pvar->VGroup,pvar->VarName)) {
                    found_flag = 1;
                } else {

                    found_flag = 0;

                    if( !default_flag ) {
        
                        ErrText.catFormat("Der Parameter <%s> für Modul <%s>  aus Instanz <%s> u. Gruppenhierachie <%s> konnte in den eingelesenen Parametern nicht gefunden werden !!!\n"
                                         ,pvar->VarName.c_str()
                                         ,pvar->ModName.c_str()
                                         ,instanz.c_str()
                                         ,pvar->GroupName.c_str());
                        Status = NOT_OKAY;

                        writeLogFile("CDsPar::get");

                        return Status;
                    }
                }
            }


            if( found_flag ) { // Wert in Datenbasis gefunden

                // Pointer der Variable aus Datenbasis holen
                pvardata = ParData.getVar(instanz,pvar->VGroup,pvar->VarName);

                if( getVar(pvar,pvardata,instanz) != OKAY ) {

                    writeLogFile("CDsPar::get");

                    Status = NOT_OKAY;
                    return Status;
                }
            } else { // default muß gesetzt werden

                if( getDefaultVar(pvar) != OKAY ) {

                    writeLogFile("CDsPar::get");

                    Status = NOT_OKAY;
                    return Status;
                }

            }
#ifdef DS_DEBUG
            // schreibt verwendete Werte in Logfle 
			logParDataUsed(pvar,instanz,pvar->VGroup);
#endif
        }
        pvar = pvar->pNext;
    }

    return Status;
}
// 
// Prüft ob ein Switch für eine Untergruppen auswahl vorhanden 
// ist und wird angewendet
//=====================================================================
status_t CDsPar::proofSubGroupSwitch(uint16_t iinst,CSlfStr &instanz) {

    CSlfStrV       vgroup;
    SDsParDataVariable *pvardata;
    CSlfStr        pstring;
    uint8_t          default_flag;
    uint8_t          found_flag;
    uint8_t          instanz_found_flag = 0;
    uint32_t         i,i1;

    SDsParSetVar  *pvar;
    SDsParSetGroup *pgr = pDSSDsParSetGroup;

    // Auswahl von Untergruppen prüfen
    while( pgr ) {

        //Defaultvalue
        if( SlfStrLen(pgr->Default) )
            default_flag = 1;
        else 
            default_flag = 0;

        if( !default_flag && !instanz_found_flag ){

            if( iinst == 0 ) {
                ErrText.catFormat("Es sind keine Parameter eingelesen (keine Instanz) !!!\n");
                ErrText.catFormat("Parameter <%s> aus Modell <%s> hat kein default-Wert\n"
                                 ,pgr->VarName.c_str()
                                 ,pgr->ModName.c_str()
                                 );
            } else {
                ErrText.catFormat("Es ist keine Parameterinstanz index=%i vorhanden !!!\n"
                             ,iinst);
            }
            Status = NOT_OKAY;

            return Status;
        }

        // Gruppenhierachie erstellen
        //===========================
        vgroup.clear();
        SlfStrVSplit(vgroup,pgr->MasterGroup,cTrennZeichGroup);

        //Gruppe suchen
        if(  ParData.existGroup(instanz,vgroup) ) {

            found_flag = 1;
        } else {
            found_flag = 0;
          if( !default_flag ) {

        
            ErrText.catFormat("Fuer Modul <%s> Instanz <%s> u. Gruppenhierachie <%s> konnte in den eingelesenen Parametern die Gruppe nicht gefunden werden !!!\n"
                             ,pgr->ModName.c_str()
                             ,instanz.c_str()
                             ,pgr->MasterGroup.c_str());
            ErrText.catFormat("Parameter <%s> aus Modell <%s> hat kein default-Wert\n"
                             ,pgr->VarName.c_str()
                             ,pgr->ModName.c_str()
                             );
            Status = NOT_OKAY;

            return Status;
          }
        }
        //Variable suchen
        if(  found_flag ) {

            if( ParData.existVar(instanz,vgroup,pgr->VarName)) {
                found_flag = 1;
            } else {

                found_flag = 0;

                if( !default_flag ) {
            
                    ErrText.catFormat("Der Parameter <%s> für Modul <%s>  aus Instanz <%s> u. Gruppenhierachie <%s> konnte in den eingelesenen Parametern nicht gefunden werden !!!\n"
                                     ,pgr->VarName.c_str()
                                     ,pgr->ModName.c_str()
                                     ,instanz.c_str()
                                     ,pgr->MasterGroup.c_str());
                    Status = NOT_OKAY;


                    return Status;
                }
            }
        }


        // Wert zuweisen
        pstring.clear();

        if( found_flag ) { // Wert in Datenbasis gefunden

            // Pointer der Variable aus Datenbasis holen
            pvardata = ParData.getVar(instanz,vgroup,pgr->VarName);

            if( pvardata->Type != DEF_STRING && pvardata->Type != DEF_STRINGM) {
                ErrText.catFormat("Der Parameter <%s> für Modul <%s>  aus Instanz <%s> u. Gruppenhierachie <%s> ist in den eingelesenen Parametern nicht DEF_STRING !!!\n"
                                 ,pgr->VarName.c_str()
                                 ,pgr->ModName.c_str()
                                 ,instanz.c_str()
                                 ,pgr->MasterGroup.c_str());
                Status = NOT_OKAY;
                
                return NOT_OKAY;

            }
            pstring.cat(pvardata->StrMVal.get_str(0,0));

        } else { // default muß gesetzt werden

            pstring = pgr->Default;

 
        }

        // Parameterwert auswerten
        if( pgr->SubGroupList.find(pstring,&i1) ) {

			char *ptest = pgr->SubGroupList.get_str(i1);


            // alle Subgroups ducrhgehen
            for(i=0;i<pgr->SubGroupList.getNrows();i++) {

				ptest = pgr->SubGroupList.get_str(i1);

                if( i != i1 ) { // Die nicht ausgewählten Gruppen reseten

                    vgroup.append(pgr->SubGroupList.get_str(i));

                    // Gruppe suchen
                    pvar = pDSParSetVar;
                    while(pvar) {

                        if( vgroup == pvar->VGroup )
                            pvar->VarSet = 0;

                        pvar = pvar->pNext;
                    }

                    vgroup.delete_last();
                }
            }
        } else {

            ErrText.catFormat("Der Parameter <%s> für Modul <%s>  aus Instanz <%s> u. Gruppenhierachie <%s> hat einen nicht gültigen Wert <%s> !!!\nMögliche Werte sind:\n"
                             ,pgr->VarName.c_str()
                             ,pgr->ModName.c_str()
                             ,instanz.c_str()
                             ,pgr->MasterGroup.c_str()
                             ,pstring.c_str()
                             );
            for(i=0;i<pgr->SubGroupList.getNrows();i++)
                ErrText.catFormat("<%s>\n",pgr->SubGroupList.get_str(i));

            Status = NOT_OKAY;
            
            return NOT_OKAY;
        }


        pgr = pgr->pNext;
    }

    return OKAY;
}
// get-function
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVar(SDsParSetVar *pvar
                       ,SDsParDataVariable *pvardata
                       ,CSlfStr &instanz) {


    // Variable übergeben
    switch(pvar->Type) {

    case DEF_DOUBLE:
    case DEF_FLOAT:
    case DEF_SIGNED_LONG:
    case DEF_UNSIGNED_LONG:
    case DEF_SIGNED_SHORT:
    case DEF_UNSIGNED_SHORT:
    case DEF_SIGNED_CHAR:
    case DEF_UNSIGNED_CHAR:

        if( getVarSingle(pvar,pvardata,instanz) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_STRING:
    case DEF_ARR_STRING:
        if( getVarString(pvar,pvardata,instanz) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;

    case DEF_1D_TAB:
        if( getVar1DTab(pvar,pvardata,instanz) != OKAY ) {
            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_2D_TAB:
        if( getVar2DTab(pvar,pvardata,instanz) != OKAY ) {
            Status = NOT_OKAY;
            return Status;
        }
		    break;
    case DEF_VEC:
    case DEF_VEC_PTR:
        if( getVarVec(pvar,pvardata,instanz) != OKAY ) {
            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_ARR_DOUBLE:
    case DEF_ARR_FLOAT:
    case DEF_ARR_SIGNED_LONG:
    case DEF_ARR_UNSIGNED_LONG:
    case DEF_ARR_SIGNED_SHORT:
    case DEF_ARR_UNSIGNED_SHORT:
    case DEF_ARR_SIGNED_CHAR:
    case DEF_ARR_UNSIGNED_CHAR:

        if( getVarArray(pvar,pvardata,instanz) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;

    }


    return Status;
}
// get-function Single-Wert
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVarSingle(SDsParSetVar *pvar
                             ,SDsParDataVariable *pvardata
                             ,CSlfStr &instanz) {

    double   ufactor,uoffset;
    double   val;
    void     *pv;
    EVarType type;

    // Für Single müssen Daten mindestens 1 lang sein
    //===============================================
    if( pvardata->NRow * pvardata->NCol == 0 ) {

        ErrText.catFormat("Problem mit Variablenlänge aus Datanebestand");
        ErrText.catFormat("Der Parameter <%s> für Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht den passenden Länge zu der angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("NRow =  %i\n"
                         ,pvardata->NRow);
        ErrText.catFormat("NCol =  %i\n"
                         ,pvardata->NCol);
        Status = NOT_OKAY;
        return Status;
    }

    // Wenn String, dann Wert wandeln
    if(  pvardata->Type == DEF_STRING 
      || pvardata->Type == DEF_STRINGM ) {

        char *pstr = pvardata->StrMVal.get_str(0,0);

      if( SlfFktConvertStringToDouble(pstr,&val) != OKAY ) {
        
        ErrText.catFormat("Problem mit Stringwandlung");
        ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> kann nicht in double gewandelt werden !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("Str = %s\n"
                         ,pstr);
        
        Status = NOT_OKAY;
        return Status;

      } else {
          pv   = (void *)&val;
          type = DEF_DOUBLE;

      }
    } else {

        pv   = pvardata->pVal;
        type = pvardata->Type;
    }


    // Type prüfen
    if( proofType(type,pvar->Type) != OKAY ) {

        ErrText.catFormat("Problem mit Typeuebereinstimmung");
        ErrText.catFormat("Der Parameter <%s> für Modell <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht den passenden Typ zu der angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("Type aus gelesenen Daten <%s>\n"
                         ,VarTypeStr[pvardata->Type]);
        ErrText.catFormat("Type angeforderte Variable <%s>\n"
                         ,VarTypeStr[pvar->Type]);
        Status = NOT_OKAY;
        return Status;
    }


    // Einheit prüfen
    //===============
    if( (Status = SlfFktUnitConv(pvardata->Unit.c_str() /*=>*/
                                ,pvar->Unit.c_str()
                                ,&ufactor
                                ,&uoffset
                                ,ErrText
                                ))          != OKAY )  {

        ErrText.catFormat("\nProblem mit Einheiten für Parameterübergabe\n");
        ErrText.catFormat("Der Parameter <%s> für Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht die passende Einheit zu dem angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("Einheit aus gelesenen Daten [%s]\n"
                         ,pvardata->Unit.c_str());
        ErrText.catFormat("Einheit angeforderte Variable [%s]\n"
                         ,pvar->Unit.c_str());
        Status = NOT_OKAY;
        return Status;
    }


    SlfFktConvertSingle(pv,                type
                       ,pvar->pVal,        pvar->Type
                       ,pvardata->YFactor, pvardata->YOffset);

    SlfFktConvertSingle(pvar->pVal,        pvar->Type
                       ,pvar->pVal,        pvar->Type
                       ,ufactor,           uoffset);

    
    return Status;
}
// get-function String-Wert
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVarString(SDsParSetVar *pvar
                             ,SDsParDataVariable *pvardata
                             ,CSlfStr &instanz) {

    // Type prüfen
    if( proofType(pvardata->Type,pvar->Type) != OKAY ) {

        ErrText.catFormat("Problem mit Typeuebereinstimmung");
        ErrText.catFormat("Der Parameter <%s> für Modell <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht den passenden Typ zu der angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("Type aus gelesenen Daten <%s>\n"
                         ,VarTypeStr[pvardata->Type]);
        ErrText.catFormat("Type angeforderte Variable <%s>\n"
                         ,VarTypeStr[pvar->Type]);
        Status = NOT_OKAY;
        return Status;
    }

    if( pvardata->Type == DEF_STRINGM )
    {
      SlfFktConvertString((void *)(&pvardata->StrMVal),    pvardata->Type, pvardata->NVal  
                         ,pvar->pVal,        pvar->Type,     pvardata->NVal);
    }
    else
    {
      SlfFktConvertString(pvardata->pVal,    pvardata->Type, pvardata->NVal  
                         ,pvar->pVal,        pvar->Type,     pvardata->NVal);
    }
    return Status;
}
// get-function 1D-Tabelle-Wert
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVar1DTab(SDsParSetVar *pvar
                            ,SDsParDataVariable *pvardata
                            ,CSlfStr &instanz) {

    CSlf1DTab *p1dtab;

    // Typ überprüfen
    if(  pvardata->Type != DEF_1D_TAB ) {

        ErrText.catFormat("CDsPar::getVar1DTab: Problem Typübereinstimmung\n"
                          "1D-Tabelle <%s> aus Modell <%s> hat den Typ <%s>\n"
                          "Im Parameterdatensatz hat aber den Typ <%s>\n"
                          ,pvar->VarName.c_str()
                          ,pvar->ModName.c_str()
                          ,VarTypeStr[pvar->Type]
                          ,VarTypeStr[pvardata->Type]
                          );
        Status = NOT_OKAY;
        return Status;
    }

    // Pointer übergeben
    //==================
    p1dtab = (CSlf1DTab *)pvar->pVal;

    // Tabelle übergeben
    if( p1dtab->set(pvardata->p1DTab) != OKAY ) {

        ErrText.cat(p1dtab->getErrText());
        p1dtab->resetErrText();
        Status = NOT_OKAY;
        return Status;
    }
    // Order übergeben
    //================
    p1dtab->Order = pvar->TabOrder;

    // Skalierung durch Parameterinput
    //================================
    p1dtab->convXVec(pvardata->XFactor,pvardata->XOffset);
    p1dtab->convYVec(pvardata->YFactor,pvardata->YOffset);

    //Tabelle Einheiten konvertieren
    //==============================
    if( SlfStrLen(pvar->XTabUnit) ) {

        if( p1dtab->convXVec(pvar->XTabUnit) != OKAY ) {
        
            ErrText.catFormat("Problem mit Einheitenkonvertierung");
            ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> !!!\n"
                             ,pvar->VarName.c_str()
                             ,pvar->ModName.c_str()
                             ,instanz.c_str()
                             ,pvar->GroupName.c_str());
            ErrText.cat(p1dtab->getErrText());
            p1dtab->resetErrText();
        
            Status = NOT_OKAY;
            return Status;

        }
    } 
    if( SlfStrLen(pvar->YTabUnit) ) {

        if( p1dtab->convYVec(pvar->YTabUnit) != OKAY ) {
        
            ErrText.catFormat("Problem mit Einheitenkonvertierung");
            ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> !!!\n"
                             ,pvar->VarName.c_str()
                             ,pvar->ModName.c_str()
                             ,instanz.c_str()
                             ,pvar->GroupName.c_str());
            ErrText.cat(p1dtab->getErrText());
            p1dtab->resetErrText();
        
            Status = NOT_OKAY;
            return Status;

        }
    } 

    return Status;
}
// get-function 2D-Tabelle-Wert
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVar2DTab(SDsParSetVar *pvar
                            ,SDsParDataVariable *pvardata
                            ,CSlfStr &instanz) {
    CSlf2DTab *p2dtab;

    // Typ überprüfen
    if(  pvardata->Type != DEF_2D_TAB ) {

        ErrText.catFormat("CDsPar::getVar2DTab: Problem Typübereinstimmung\n"
                          "2D-Tabelle <%s> aus Modell <%s> hat den Typ <%s>\n"
                          "Im Parameterdatensatz hat aber den Typ <%s>\n"
                          ,pvar->VarName.c_str()
                          ,pvar->ModName.c_str()
                          ,VarTypeStr[pvar->Type]
                          ,VarTypeStr[pvardata->Type]
                          );
        Status = NOT_OKAY;
        return Status;
    }

    // Pointer übergeben
    //==================
    p2dtab = (CSlf2DTab *)pvar->pVal;

    // Tabelle übergeben
    if( p2dtab->set(pvardata->p2DTab) != OKAY ) {

        ErrText.cat(p2dtab->getErrText());
        p2dtab->resetErrText();
        Status = NOT_OKAY;
        return Status;
    }
    // Order übergeben
    //================
    p2dtab->Order = pvar->TabOrder;

    // Skalierung durch Parameterinput
    //================================
    p2dtab->convXVec(pvardata->XFactor,pvardata->XOffset);
    p2dtab->convYVec(pvardata->YFactor,pvardata->YOffset);
    p2dtab->convZMat(pvardata->ZFactor,pvardata->ZOffset);

    //Tabelle Einheiten konvertieren
    //==============================
    if( SlfStrLen(pvar->XTabUnit) ) {

        if( p2dtab->convXVec(pvar->XTabUnit) != OKAY ) {
        
            ErrText.catFormat("Problem mit Einheitenkonvertierung");
            ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> !!!\n"
                             ,pvar->VarName.c_str()
                             ,pvar->ModName.c_str()
                             ,instanz.c_str()
                             ,pvar->GroupName.c_str());
            ErrText.cat(p2dtab->getErrText());
            p2dtab->resetErrText();
        
            Status = NOT_OKAY;
            return Status;

        }
    } 
    if( SlfStrLen(pvar->YTabUnit) ) {

        if( p2dtab->convYVec(pvar->YTabUnit) != OKAY ) {
        
            ErrText.catFormat("Problem mit Einheitenkonvertierung");
            ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> !!!\n"
                             ,pvar->VarName.c_str()
                             ,pvar->ModName.c_str()
                             ,instanz.c_str()
                             ,pvar->GroupName.c_str());
            ErrText.cat(p2dtab->getErrText());
            p2dtab->resetErrText();
        
            Status = NOT_OKAY;
            return Status;

        }
    } 
    if( SlfStrLen(pvar->ZTabUnit) ) {

        if( p2dtab->convZMat(pvar->ZTabUnit) != OKAY ) {
        
            ErrText.catFormat("Problem mit Einheitenkonvertierung");
            ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> !!!\n"
                             ,pvar->VarName.c_str()
                             ,pvar->ModName.c_str()
                             ,instanz.c_str()
                             ,pvar->GroupName.c_str());
            ErrText.cat(p2dtab->getErrText());
            p2dtab->resetErrText();
        
            Status = NOT_OKAY;
            return Status;

        }
    } 

    return Status;
}
// get-function Vector_t-Werte
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVarVec(SDsParSetVar *pvar
                          ,SDsParDataVariable *pvardata
                          ,CSlfStr &instanz) {

    double ufactor,uoffset;
    Vector_t *pvec;
    Vector_t vec;
    //Vector_t *pdvec;
    //Vector_t dvec;
    uint32_t irow,nrow;

    // Wenn pointer von Vectorpointer übergebn,
    // wird auch Variable hier kontrolliert
    //=========================================
    if( pvar->Type == DEF_VEC_PTR ) {

        Vector_t **ppvec  = (Vector_t **)pvar->pVal;
        *ppvec         = new Vector_t;

        pvar->VecBuild = 1;      // Variable wurde hier gebildet
        pvec           = *ppvec;
        *pvec          = 0;
        pvar->Type     = DEF_VEC;

    } else { // pointer vom Vector_t wurde übergeben

        pvec = (Vector_t *)pvar->pVal;
    }


    // Pointer prüfen
    //===============
    if( *pvec == 0 ) { // Vector_t noch nicht gesetzt

        vec        = NewVector(pvardata->NRow);
        *pvec      = vec;

        pvar->VecSet = 1; // Vektor wurde hier allociert
    } else {

        vec = *pvec;
        if( GET_NROWS(vec) != pvardata->NRow ) {

            ErrText.catFormat("CDsPar::getVarVec: Problem Vectorlänge\n"
                          "Vector_t <%s> aus Modell <%s> hat die Länge <%i>\n"
                          "Im Parameterdatensatz hat aber die Länge <%i>\n"
                          ,pvar->VarName.c_str()
                          ,pvar->ModName.c_str()
                          ,GET_NROWS(vec)
                          ,pvardata->NRow
                          );
            Status = NOT_OKAY;
            return Status;
        }
    }
    nrow         = pvardata->NRow;

    // Einheitenumrechnung
    // Einheit prüfen
    //===============
    if( (Status = SlfFktUnitConv(pvardata->Unit.c_str() /*=>*/
                                ,pvar->Unit.c_str()
                                ,&ufactor
                                ,&uoffset
                                ,ErrText
                                ))          != OKAY )  {

        ErrText.catFormat("\nProblem mit Einheiten für Parameterübergabe\n");
        ErrText.catFormat("Der Parameter <%s> für Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht die passende Einheit zu dem angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("Einheit aus gelesenen Daten [%s]\n"
                         ,pvardata->Unit.c_str());
        ErrText.catFormat("Einheit angeforderte Variable [%s]\n"
                         ,pvar->Unit.c_str());
        Status = NOT_OKAY;
        return Status;
    }
    
    if( pvardata->Type == DEF_STRINGM ) { // Nrows werden aus Strings ausgelesen

        char   *pstr;
        double val;
        for(irow=0;irow<nrow;irow++) {

            pstr = pvardata->StrMVal.get_str(irow,0);

            if( SlfFktConvertStringToDouble(pstr,&val) != OKAY ) {
        
                ErrText.catFormat("Problem mit Stringwandlung");
                ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> kann nicht in double gewandelt werden !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
                ErrText.catFormat("Str = %s\n"
                         ,pstr);
        
                Status = NOT_OKAY;
                return Status;

            }

            //Skalieren nach Parameterinput
            vec[irow] = val*pvardata->YFactor + pvardata->YOffset;

            //Skalieren nach Einheit
            vec[irow] = vec[irow]*ufactor + uoffset;

        }
    } else if( pvardata->Type == DEF_VEC ) { 

        //pdvec = (Vector_t *)pvardata->pVec;
        //dvec  = *pdvec;

        for(irow=0;irow<nrow;irow++) {

            //Skalieren nach Parameterinput
            vec[irow] = pvardata->Vec[irow]*pvardata->YFactor + pvardata->YOffset;

            //Skalieren nach Einheit
            vec[irow] = vec[irow]*ufactor + uoffset;
        }

    } else {

        ErrText.catFormat("CDsPar::getVarVec: Problem Typübereinstimmung\n"
                          "Vector_t <%s> aus Modell <%s> hat den Typ <%s>\n"
                          "Im Parameterdatensatz hat aber den Typ <%s>\n"
                          ,pvar->VarName.c_str()
                          ,pvar->ModName.c_str()
                          ,VarTypeStr[pvar->Type]
                          ,VarTypeStr[pvardata->Type]
                          );
        Status = NOT_OKAY;
        return Status;
    }


    return Status;
}
// get-function Array-Werte
// Variable aus pvardata auslesen und
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getVarArray(SDsParSetVar *pvar
                             ,SDsParDataVariable *pvardata
                             ,CSlfStr &instanz) {

    double   ufactor,uoffset;
    Vector_t   vec;
    uint32_t   irow,nrow;

    // Für Array müssen Daten mindestens NVal lang sein
    //===============================================
    if( pvardata->NRow <  pvar->NRow ) {

        ErrText.catFormat("Problem mit Variablenlänge aus Datanebestand");
        ErrText.catFormat("Der Parameter <%s> für Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht den passenden Länge (n=%i)zu der angeforderten Parameter (n=%i) !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str()
                         ,pvar->NRow
                         ,pvardata->NRow
                         );
        ErrText.catFormat("NRow =  %i\n"
                         ,pvardata->NRow);
        ErrText.catFormat("NCol =  %i\n"
                         ,pvardata->NCol);
        Status = NOT_OKAY;
        return Status;
    }

    nrow         = pvar->NRow;

    // Einheitenumrechnung
    // Einheit prüfen
    //===============
    if( (Status = SlfFktUnitConv(pvardata->Unit.c_str() /*=>*/
                                ,pvar->Unit.c_str()
                                ,&ufactor
                                ,&uoffset
                                ,ErrText
                                ))          != OKAY )  {

        ErrText.catFormat("\nProblem mit Einheiten für Parameterübergabe\n");
        ErrText.catFormat("Der Parameter <%s> für Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> hat nicht die passende Einheit zu dem angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
        ErrText.catFormat("Einheit aus gelesenen Daten [%s]\n"
                         ,pvardata->Unit.c_str());
        ErrText.catFormat("Einheit angeforderte Variable [%s]\n"
                         ,pvar->Unit.c_str());
        Status = NOT_OKAY;
        return Status;
    }
    
    if( pvardata->Type == DEF_STRINGM ) { // Nrows werden aus Strings ausgelesen

        char   *pstr;
        vec = NewVector(nrow);
        for(irow=0;irow<nrow;irow++) {

            pstr = pvardata->StrMVal.get_str(irow,0);

            if( SlfFktConvertStringToDouble(pstr,&vec[irow]) != OKAY ) {
        
                ErrText.catFormat("Problem mit Stringwandlung");
                ErrText.catFormat("Der Parameter <%s> aus Modul <%s> aus Instanz <%s> u. Gruppenhierachie <%s> kann nicht in double gewandelt werden !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,instanz.c_str()
                         ,pvar->GroupName.c_str());
                ErrText.catFormat("Str = %s\n"
                         ,pstr);
        
                Status = NOT_OKAY;
                return Status;

            }

        }
        SlfFktConvertArray(&vec,              DEF_VEC,    nrow
                          ,pvar->pVal,        pvar->Type, nrow
                          ,pvardata->YFactor, pvardata->YOffset);

        SlfFktConvertArray(pvar->pVal,        pvar->Type, nrow
                           ,pvar->pVal,        pvar->Type, nrow
                           ,ufactor,           uoffset);


    } else if( pvardata->Type == DEF_VEC ) { 

        SlfFktConvertArray(&pvardata->Vec,    pvardata->Type, nrow
                          ,pvar->pVal,        pvar->Type,     nrow
                          ,pvardata->YFactor, pvardata->YOffset);

        SlfFktConvertArray(pvar->pVal,        pvar->Type, nrow
                           ,pvar->pVal,        pvar->Type, nrow
                           ,ufactor,           uoffset);

    } else if(  (pvardata->Type == DEF_ARR_DOUBLE)
             || (pvardata->Type == DEF_ARR_FLOAT)
             || (pvardata->Type == DEF_ARR_SIGNED_LONG)
             || (pvardata->Type == DEF_ARR_UNSIGNED_LONG)
             || (pvardata->Type == DEF_ARR_SIGNED_SHORT)
             || (pvardata->Type == DEF_ARR_UNSIGNED_SHORT)
             || (pvardata->Type == DEF_ARR_SIGNED_CHAR)
             || (pvardata->Type == DEF_ARR_UNSIGNED_CHAR)
             ) { 

        SlfFktConvertArray(pvardata->pVal,    pvardata->Type, nrow
                          ,pvar->pVal,        pvar->Type,     nrow
                          ,pvardata->YFactor, pvardata->YOffset);

        SlfFktConvertArray(pvar->pVal,        pvar->Type, nrow
                           ,pvar->pVal,        pvar->Type, nrow
                           ,ufactor,           uoffset);
    } else {

        ErrText.catFormat("CDsPar::getVarArray: Problem Typübereinstimmung\n"
                          "Vector_t <%s> aus Modell <%s> hat den Typ <%s>\n"
                          "Im Parameterdatensatz hat aber den Typ <%s>\n"
                          ,pvar->VarName.c_str()
                          ,pvar->ModName.c_str()
                          ,VarTypeStr[pvar->Type]
                          ,VarTypeStr[pvardata->Type]
                          );
        Status = NOT_OKAY;
        return Status;
    }

    return Status;
}

// get-function Default-Wert setzen
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getDefaultVar(SDsParSetVar *pvar) {


    // Variable übergeben
    switch(pvar->Type) {

    case DEF_DOUBLE:
    case DEF_FLOAT:
    case DEF_SIGNED_LONG:
    case DEF_UNSIGNED_LONG:
    case DEF_SIGNED_SHORT:
    case DEF_UNSIGNED_SHORT:
    case DEF_SIGNED_CHAR:
    case DEF_UNSIGNED_CHAR:

        if( getDefaultVarSingle(pvar) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_STRING:
    case DEF_ARR_STRING:
        if( getDefaultVarString(pvar) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_1D_TAB:
        if( getDefaultVar1DTab(pvar) != OKAY ) {
            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_2D_TAB:
        if( getDefaultVar2DTab(pvar) != OKAY ) {
            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_VEC_PTR:
    case DEF_VEC:
        if( getDefaultVarVec(pvar) != OKAY ) {
            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_ARR_DOUBLE:
    case DEF_ARR_FLOAT:
    case DEF_ARR_SIGNED_LONG:
    case DEF_ARR_UNSIGNED_LONG:
    case DEF_ARR_SIGNED_SHORT:
    case DEF_ARR_UNSIGNED_SHORT:
    case DEF_ARR_SIGNED_CHAR:
    case DEF_ARR_UNSIGNED_CHAR:

        if( getDefaultVarArray(pvar) != OKAY ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    }


    return Status;
}
// get-function Default-Single-Wert setzen
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getDefaultVarSingle(SDsParSetVar *pvar) {

    double   factor= 1.0;
    double   offset= 0.0;
    double   val;
    void     *pv;
    EVarType type;

    // String wandeln in double
    if( SlfFktConvertStringToDouble(pvar->Default.c_str(),&val) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultwertes (String->double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in double gewandelt werden !!!\n"
                         ,pvar->Default
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    } else {
      pv   = (void *)&val;
      type = DEF_DOUBLE;

    }


    // Type prüfen
    if( proofType(type,pvar->Type) != OKAY ) {

        ErrText.catFormat("Problem mit Typeuebereinstimmung mit dem Defaultwert");
        ErrText.catFormat("Der Parameter <%s> fuer Modell <%s> mit dem in double <%g> gewandelten Defaultwert <%s> hat nicht den passenden Typ zu der angeforderten Parameter !!!\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         ,val
                         ,pvar->Default);
        ErrText.catFormat("geforderter Typ: <%s> !!\n"
                         ,VarTypeStr[pvar->Type]);
        Status = NOT_OKAY;
        return Status;
    }


    SlfFktConvertSingle(pv,         type
                       ,pvar->pVal, pvar->Type
                       ,factor,     offset);

    return Status;
}
// get-function Default-String-Wert setzen
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getDefaultVarString(SDsParSetVar *pvar) {


    if( pvar->Type == DEF_STRING ) {
    
      CSlfStr *ps = (CSlfStr *)pvar->pVal;

      ps->clear();
      ps->cat((char *)pvar->Default);

    } else if( pvar->Type == DEF_ARR_STRING ) {

      uint32_t    n,i;
      CSlfStrV strv;
      CSlfStr *ps = (CSlfStr *)pvar->pVal;
      Status =SlfFktConvertStringToStringVec((char *)pvar->Default,&strv,&n);
      if( Status == OKAY ) {

        for(i=0;i<MIN(n,pvar->NRow);i++)
          ps[i] = strv.get_str(i);

        for(i=MIN(n,pvar->NRow);i<pvar->NRow;i++)
          ps[i] = "";
      }
    }
    return Status;
}
// get-function Default-1D-Tabelle-Wert setzen
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getDefaultVar1DTab(SDsParSetVar *pvar) {

    double   factor= 1.0;
    double   offset= 0.0;
    double   *pxvec = 0;
    double   *pyvec = 0;
    uint32_t   nxvec = 0;
    uint32_t   nyvec = 0;
    CSlf1DTab *p1dtab = (CSlf1DTab *)pvar->pVal;


    // String wandeln in double X-Vektor
    if( SlfFktConvertStringToDoubleVec(pvar->XTabDefault.c_str(),&pxvec,&nxvec) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultvektorwertes (String->*double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in *double gewandelt werden !!!\n"
                         ,pvar->XTabDefault.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }
    // String wandeln in double Y-Vektor
    if( SlfFktConvertStringToDoubleVec(pvar->YTabDefault.c_str(),&pyvec,&nyvec) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultvektorwertes (String->*double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in *double gewandelt werden !!!\n"
                         ,pvar->YTabDefault.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }

    // Tabelle einsetzen
    if( p1dtab->set(pvar->VarName.c_str()
                   ,pvar->Comment.c_str()
                   ,pvar->XTabName.c_str()
                   ,pvar->XTabUnit.c_str()
                   ,pvar->XTabComment.c_str()
                   ,pxvec
                   ,nxvec
                   ,pvar->YTabName.c_str()
                   ,pvar->YTabUnit.c_str()
                   ,pvar->YTabComment.c_str()
                   ,pyvec
                   ,nyvec
                   ,pvar->TabOrder) != OKAY ) {

        ErrText.cat("Problem mit Erstellen der 1D-Tabelle aus Defaultwerten\n");
        ErrText.catFormat("Der Fehler <%s> für Modul <%s> kann nicht in *double gewandelt werden !!!\n"
                         ,p1dtab->getErrText()
                         ,pvar->ModName.c_str()
                         );
        ErrText.cat(p1dtab->getErrText());
        p1dtab->resetErrText();

        
        Status = NOT_OKAY;
        return Status;

    }
    if( pxvec )
        delete []pxvec;
    if( pyvec )
        delete []pyvec;

    return Status;
}
// get-function Default-2D-Tabelle-Wert setzen
// pvar zuordnen instanz nur für Fehlerausgabe
//=====================================================================
status_t CDsPar::getDefaultVar2DTab(SDsParSetVar *pvar) {

    double   factor= 1.0;
    double   offset= 0.0;
    double   *pxvec = 0;
    double   *pyvec = 0;
    Matrix_t   zmat   = 0;
    uint32_t   nxvec = 0;
    uint32_t   nyvec = 0;
    CSlf2DTab *p2dtab = (CSlf2DTab *)pvar->pVal;


    // String wandeln in double X-Vektor
    if( SlfFktConvertStringToDoubleVec(pvar->XTabDefault.c_str(),&pxvec,&nxvec) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultvektorwertes (String->*double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in *double gewandelt werden !!!\n"
                         ,pvar->XTabDefault.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }
    // String wandeln in double Y-Vektor
    if( SlfFktConvertStringToDoubleVec(pvar->YTabDefault.c_str(),&pyvec,&nyvec) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultvektorwertes (String->*double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in *double gewandelt werden !!!\n"
                         ,pvar->YTabDefault.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }
    // String wandeln in Matrix_t Z-Matrix_t
    if( SlfFktConvertStringToDoubleMat(pvar->ZTabDefault.c_str(),&zmat) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultmatrixwertes (String->*double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in *double gewandelt werden !!!\n"
                         ,pvar->ZTabDefault.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }

    // Tabelle einsetzen
    if( p2dtab->set(pvar->VarName.c_str()
                   ,pvar->Comment.c_str()
                   ,pvar->XTabName.c_str()
                   ,pvar->XTabUnit.c_str()
                   ,pvar->XTabComment.c_str()
                   ,pxvec
                   ,nxvec
                   ,pvar->YTabName.c_str()
                   ,pvar->YTabUnit.c_str()
                   ,pvar->YTabComment.c_str()
                   ,pyvec
                   ,nyvec
                   ,pvar->ZTabName.c_str()
                   ,pvar->ZTabUnit.c_str()
                   ,pvar->ZTabComment.c_str()
                   ,zmat
                   ,pvar->TabOrder) != OKAY ) {

        ErrText.catFormat("Problem mit Erstellen der 2D-Tabelle aus Defaultwerten");
        ErrText.cat(p2dtab->getErrText());
        p2dtab->resetErrText();

        
        Status = NOT_OKAY;
        return Status;

    }
    if( pxvec )
        delete []pxvec;
    if( pyvec )
        delete []pyvec;
    if( zmat )
        FreeMatrix(zmat);

    return Status;
}
// get-function Default Vector_t-Werte
//=====================================================================
status_t CDsPar::getDefaultVarVec(SDsParSetVar *pvar) {

    Vector_t *pvec;
	Vector_t vec=0;
    double   *pd = 0;
    uint32_t   nd = 0;

    // String wandeln in double Y-Vektor
    if( SlfFktConvertStringToDoubleVec(pvar->Default.c_str(),&pd,&nd) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultvektorwertes (String->double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in double gewandelt werden !!!\n"
                         ,pvar->Default.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }

    // Wenn pointer von Vectorpointer übergebn,
    // wird auch Variable hier kontrolliert
    //=========================================
    if( pvar->Type == DEF_VEC_PTR ) {

        Vector_t **ppvec  = (Vector_t **)pvar->pVal;
        *ppvec         = new Vector_t;

        pvar->VecBuild = 1;      // Variable wurde hier gebildet
        pvec           = *ppvec;
        *pvec          = 0;
        pvar->Type     = DEF_VEC;

    } else { // pointer vom Vector_t wurde übergeben

        pvec = (Vector_t *)pvar->pVal;
    }


    // Pointer übergeben und prüfen
    //=============================
    if( *pvec == 0 ) { // Vector_t noch nicht gesetzt

        *pvec        = NewVector(nd);
        pvar->VecSet = 1; // Vektor wurde hier allociert
    } else {

        if( GET_NROWS(*pvec) != nd ) {

            ErrText.catFormat("CDsPar::getDefaultVarVec: Problem Vectorlänge\n"
                          "Vector_t <%s> aus Modell <%s> hat die Länge <%i>\n"
                          "Im Parameterdatensatz hat aber die Länge <%i>\n"
                          ,pvar->VarName.c_str()
                          ,pvar->ModName.c_str()
                          ,GET_NROWS(*pvec)
                          ,nd
                          );
            Status = NOT_OKAY;
            return Status;
        }
    }

	  vec = *pvec;

    // Werte zuweisen
    //===============
    for(uint16_t i=0;i<nd;i++)
        vec[i] = pd[i];

    pvar->NRow = nd;
    pvar->NCol = 1;
    pvar->NVal = nd;


    if( pd )
        delete []pd;

    return Status;
}
// get-function Default Array-Werte
//=====================================================================
status_t CDsPar::getDefaultVarArray(SDsParSetVar *pvar) {

    double   *pd = 0;
    uint32_t   nd = 0;

    // String wandeln in double Y-Vektor
    if( SlfFktConvertStringToDoubleVec(pvar->Default.c_str(),&pd,&nd) != OKAY ) {
        
        ErrText.catFormat("Problem mit Wandlung des Defaultvektorwertes (String->double)");
        ErrText.catFormat("Der Defaultwert <%s> für Modul <%s> kann nicht in double gewandelt werden !!!\n"
                         ,pvar->Default.c_str()
                         ,pvar->ModName.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;

    }

    SlfFktConvertArray(pd         ,DEF_ARR_DOUBLE ,nd
                      ,pvar->pVal ,pvar->Type     ,pvar->NRow
                      ,1.0        ,0.0);



    if( pd )
        delete []pd;

    return Status;
}
//========================================================
// Typezuordnung prüfen
//========================================================
status_t CDsPar::proofType(EVarType TypeI
                          ,EVarType TypeO) {


    switch( TypeI ) {
    case DEF_VOID:
        Status = NOT_OKAY;
        return Status;
        break;
    case DEF_DOUBLE:
    case DEF_FLOAT:
    case DEF_SIGNED_LONG:
    case DEF_UNSIGNED_LONG:
    case DEF_SIGNED_SHORT:
    case DEF_UNSIGNED_SHORT:
    case DEF_SIGNED_CHAR:
    case DEF_UNSIGNED_CHAR: 
    
        // Darf alles sein nur klein string und void
        if( TypeO == DEF_VOID || TypeO == DEF_STRING ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_STRING:
    case DEF_STRINGM:
    case DEF_STRINGV:
    case DEF_ARR_STRING:

        // Darf nur string sein
        if(  (TypeO != DEF_STRING)
          && (TypeO != DEF_STRINGM)
          && (TypeO != DEF_STRINGV)
          && (TypeO != DEF_ARR_STRING)
          ) {

            Status = NOT_OKAY;
            return Status;
        }
        break;
    case DEF_VEC:
    case DEF_ARR_DOUBLE:
    case DEF_ARR_FLOAT:
    case DEF_ARR_SIGNED_LONG:
    case DEF_ARR_UNSIGNED_LONG:
    case DEF_ARR_SIGNED_SHORT:
    case DEF_ARR_UNSIGNED_SHORT:
    case DEF_ARR_SIGNED_CHAR:
    case DEF_ARR_UNSIGNED_CHAR:
      if(  (TypeO != DEF_VEC)
        && (TypeO != DEF_ARR_DOUBLE)
        && (TypeO != DEF_ARR_FLOAT)
        && (TypeO != DEF_ARR_SIGNED_LONG)
        && (TypeO != DEF_ARR_UNSIGNED_LONG)
        && (TypeO != DEF_ARR_SIGNED_SHORT)
        && (TypeO != DEF_ARR_UNSIGNED_SHORT)
        && (TypeO != DEF_ARR_SIGNED_CHAR)
        && (TypeO != DEF_ARR_UNSIGNED_CHAR)
        ) {
          Status = NOT_OKAY;
          return Status;
      }
    }
        
    return Status;
}
// Logfile schreiben
//=======================
// Logfile schreiben
//=======================
void CDsPar::writeLogFile(char *name) {

    // Logfile starten
    //================
    if( pLogFile && !pLogFile->isOpen() ) {
      if( pLogFile->open() != OKAY)
		  Status = NOT_OKAY;
	}
    if( pLogFile ) {

        // Fehler aufgetreten
        //===================
        if( Status != OKAY && getLenErrText() > 0 ) {

            pLogFile->writeLine("#",30);
            pLogFile->write(name);
            pLogFile->writeEnd(": Error !!!");
            pLogFile->writeLine("#",30);
            pLogFile->writeEnd(getErrText());
            pLogFile->writeLine("#",30);

            resetErrText();
        }
    }
}

status_t CDsPar::logParDataUsed(SDsParSetVar *pvar,CSlfStr &instanz,CSlfStrV &vgroup) {


    switch(pvar->Type) {

    case DEF_DOUBLE:
        {
        double *pval = (double *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                     ) != OKAY ) {

            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_FLOAT:
        {
        float *pval = (float *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                     ) != OKAY ) {
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_SIGNED_LONG:
        {
        sint32_t *pval = (sint32_t *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                     ) != OKAY ) {
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_UNSIGNED_LONG:
        {
        uint32_t *pval = (uint32_t *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                      ) != OKAY ) {
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_SIGNED_SHORT:
        {
        sint16_t *pval = (sint16_t *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                      ) != OKAY ) {
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_UNSIGNED_SHORT:
        {
        uint16_t *pval = (uint16_t *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                      ) != OKAY ) {
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_SIGNED_CHAR:
        {
        sint8_t *pval = (sint8_t *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                      ) != OKAY ) {
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_UNSIGNED_CHAR:
        {
        uint8_t *pval = (uint8_t *)pvar->pVal;

        if( ParDataUsed.setSingleData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,*pval          // Singlewert
                                     ) != OKAY ){
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_STRING:
        {
        CSlfStr *pstring = (CSlfStr *)pvar->pVal;
        if( ParDataUsed.setStringData(&instanz     // Instanzname
                                     ,&vgroup       // Gruppenhierachie
                                     ,&pvar->VarName     // Variablenname
                                     ,&pvar->Unit        // Einheit
                                     ,1.0            // Faktor
                                     ,0.0            // Offset
                                     ,&pvar->Comment     // Kommentar
                                     ,pstring            // Stringwert
                                     ) != OKAY ){
            Status = NOT_OKAY;
        }
        }
        break;
    case DEF_1D_TAB:

       
        if( ParDataUsed.setData(&instanz
                               ,&vgroup
                               ,&pvar->VarName
                               ,0
                               ,&pvar->Comment
                               ,DEF_1D_TAB
                               ,1
                               ,1
                               ,1.0            // X-Faktor
                               ,0.0            // X-Offset
                               ,1.0            // Y-Faktor
                               ,0.0            // Y-Offset
                               ,1.0            // Z-Faktor
                               ,0.0            // Z-Offset
                               ,pvar->pVal) != OKAY ) {
            Status = NOT_OKAY;
        }
        break;
    case DEF_2D_TAB:

       
        if( ParDataUsed.setData(&instanz
                               ,&vgroup
                               ,&pvar->VarName
                               ,0
                               ,&pvar->Comment
                               ,DEF_2D_TAB
                               ,1
                               ,1
                               ,1.0            // X-Faktor
                               ,0.0            // X-Offset
                               ,1.0            // Y-Faktor
                               ,0.0            // Y-Offset
                               ,1.0            // Z-Faktor
                               ,0.0            // Z-Offset
                               ,pvar->pVal) != OKAY ) {
            Status = NOT_OKAY;
        }
        break;
    case DEF_VEC:

      if( pvar->VecBuild ) {
        Vector_t **ppvec  = (Vector_t **)pvar->pVal;
        if( ParDataUsed.setData(&instanz
                               ,&vgroup
                               ,&pvar->VarName
                               ,&pvar->Unit
                               ,&pvar->Comment
                               ,DEF_VEC
                               ,pvar->NRow
                               ,pvar->NCol
                               ,1.0            // X-Faktor
                               ,0.0            // X-Offset
                               ,1.0            // Y-Faktor
                               ,0.0            // Y-Offset
                               ,1.0            // Z-Faktor
                               ,0.0            // Z-Offset
                               ,(void *)(*ppvec)) != OKAY ) {
            Status = NOT_OKAY;
        }
      } else {
        if( ParDataUsed.setData(&instanz
                               ,&vgroup
                               ,&pvar->VarName
                               ,&pvar->Unit
                               ,&pvar->Comment
                               ,DEF_VEC
                               ,pvar->NRow
                               ,pvar->NCol
                               ,1.0            // X-Faktor
                               ,0.0            // X-Offset
                               ,1.0            // Y-Faktor
                               ,0.0            // Y-Offset
                               ,1.0            // Z-Faktor
                               ,0.0            // Z-Offset
                               ,pvar->pVal) != OKAY ) {
            Status = NOT_OKAY;
        }
      }
        break;
    default:
        ErrText.catFormat("CDsPar::logParDataUsed: Variable %s aus "
                          "Modell <%s> konte nicht geschrieben werden\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         );
         Status = NOT_OKAY;    
        break;
    }


    if( Status != OKAY ) {


        ErrText.catFormat("CDsPar::logParDataUsed: Variable %s aus "
                          "Modell <%s> konnte nicht geschrieben werden\n"
                         ,pvar->VarName.c_str()
                         ,pvar->ModName.c_str()
                         );
        ErrText.catFormat("       Instanz <%s>\n",instanz.c_str());
        for(uint16_t igg=0;igg<vgroup.getNrows();igg++)
            ErrText.catFormat("       Gruppe <%s>\n",vgroup.get_str(igg));

        if( ParDataUsed.getLenErrText() > 0 )
            ErrText.catFormat("\n\n%s\n",ParDataUsed.getErrText());

    }
    return Status;


}

//========================================================
//========================================================
//========================================================
//========================================================
//========================================================
//========================================================
// Parameter abfrage setzen, am besten in Konstruktoraufruf
// damit sichergestellt ist, das alle Parameter gesetzt sind bevor 
// get-Funktion aufgerufen wird
//========================================================
status_t CDsPar::set(char *mod_name,SDsParVar *par,uint16_t npar) {
    
  SDsParSetVar *pNeu;

  uint16_t i;

  for(i=0;i<npar;i++) {

    // Typprüfung
    if( par[i].Type == DEF_VOID ) {

      return OKAY;
    } else if(  (par[i].Type != DEF_DOUBLE)
             && (par[i].Type != DEF_FLOAT)
             && (par[i].Type != DEF_SIGNED_LONG)
             && (par[i].Type != DEF_UNSIGNED_LONG)
             && (par[i].Type != DEF_SIGNED_SHORT)
             && (par[i].Type != DEF_UNSIGNED_SHORT)
             && (par[i].Type != DEF_SIGNED_CHAR)
             && (par[i].Type != DEF_UNSIGNED_CHAR)
             && (par[i].Type != DEF_STRING)
             && (par[i].Type != DEF_VEC)
             && (par[i].Type != DEF_VEC_PTR)             
             ) {

        ErrText.catFormat("CDsPar::set(single values) error: mod_name:<%s>, var_name: <%s>, has wrong type: <%>\n"
                         ,mod_name, par[i].Name, VarTypeStr[par[i].Type]);
        return NOT_OKAY;

    } else {

      pNeu = new SDsParSetVar;

      // Neue Struktur füllen
      //=====================
      pNeu->ModName   = mod_name;
      pNeu->VarName   = par[i].Name;
      pNeu->GroupName = par[i].Group;

      SlfStrVSplit(pNeu->VGroup,pNeu->GroupName,DSPAR_CTRENNZEICHENGROUP);

      pNeu->pVal      = par[i].pVal;
      pNeu->NRow      = 1;
      pNeu->NCol      = 1;
      pNeu->NVal      = 1;
      pNeu->Type      = par[i].Type;
      pNeu->Unit      = par[i].Unit;
      pNeu->Default   = par[i].Default;
      pNeu->Comment   = par[i].Comment;
      pNeu->VecSet    = 0; // initialisieren
      pNeu->VecBuild  = 0; // initialisieren
      pNeu->VarSet    = 1; // per se gesetzt
      pNeu->pNext     = pDSParSetVar;

      pDSParSetVar = pNeu;
    }
  }
    
  return OKAY;
}
status_t CDsPar::set(char *mod_name,SDsParArr *par,uint16_t npar) {
    
  SDsParSetVar *pNeu;

  uint16_t i;

  for(i=0;i<npar;i++) {

    // Typprüfung
    if( par[i].Type == DEF_VOID ) {

      return OKAY;
    } else if(  (par[i].Type != DEF_ARR_DOUBLE)
             && (par[i].Type != DEF_ARR_FLOAT)
             && (par[i].Type != DEF_ARR_SIGNED_LONG)
             && (par[i].Type != DEF_ARR_UNSIGNED_LONG)
             && (par[i].Type != DEF_ARR_SIGNED_SHORT)
             && (par[i].Type != DEF_ARR_UNSIGNED_SHORT)
             && (par[i].Type != DEF_ARR_SIGNED_CHAR)
             && (par[i].Type != DEF_ARR_UNSIGNED_CHAR)
             && (par[i].Type != DEF_ARR_STRING)
             ) {

        ErrText.catFormat("CDsPar::set(array values) error: mod_name:<%s>, var_name: <%s>, has wrong type: <%>\n"
                         ,mod_name, par[i].Name, VarTypeStr[par[i].Type]);
        return NOT_OKAY;

    } else {

      pNeu = new SDsParSetVar;

      // Neue Struktur füllen
      //=====================
      pNeu->ModName   = mod_name;
      pNeu->VarName   = par[i].Name;
      pNeu->GroupName = par[i].Group;

      SlfStrVSplit(pNeu->VGroup,pNeu->GroupName,DSPAR_CTRENNZEICHENGROUP);

      pNeu->pVal      = par[i].pVal;
      pNeu->NRow      = par[i].NRow;
      pNeu->NCol      = 1;
      pNeu->NVal      = pNeu->NRow*pNeu->NCol;
      pNeu->Type      = par[i].Type;
      pNeu->Unit      = par[i].Unit;
      pNeu->Default   = par[i].Default;
      pNeu->Comment   = par[i].Comment;
      pNeu->VecSet    = 0; // initialisieren
      pNeu->VecBuild  = 0; // initialisieren
      pNeu->VarSet    = 1; // per se gesetzt
      pNeu->pNext     = pDSParSetVar;

      pDSParSetVar = pNeu;
    }
  }
    
  return OKAY;
}
status_t CDsPar::set(char *mod_name,SDsParTab *par,uint16_t npar) {
    
  SDsParSetVar *pNeu;

  uint16_t i;

  for(i=0;i<npar;i++) {

    // Typprüfung
    if( par[i].Type == DEF_VOID ) {

      return OKAY;
    } else if(  (par[i].Type != DEF_1D_TAB)
             && (par[i].Type != DEF_2D_TAB)
             ) {

        ErrText.catFormat("CDsPar::set(table values) error: mod_name:<%s>, var_name: <%s>, has wrong type: <%>\n"
                         ,mod_name, par[i].Name, VarTypeStr[par[i].Type]);
        return NOT_OKAY;

    } else {

      pNeu = new SDsParSetVar;

      // Neue Struktur füllen
      //=====================
      pNeu->ModName   = mod_name;
      pNeu->VarName   = par[i].Name;
      pNeu->GroupName = par[i].Group;

      SlfStrVSplit(pNeu->VGroup,pNeu->GroupName,DSPAR_CTRENNZEICHENGROUP);
    
      pNeu->pVal      = par[i].pVal;
      pNeu->NVal      = 1;
      pNeu->Type      = par[i].Type;
      pNeu->Comment   = par[i].Comment;
      pNeu->VecSet    = 0; // initialisieren
      pNeu->VecBuild  = 0; // initialisieren
      pNeu->VarSet    = 1; // per set gesetzt

      switch(pNeu->Type) {

      case DEF_1D_TAB:

          pNeu->TabOrder    = par[i].Order;

          pNeu->XTabName    = par[i].XName;
          pNeu->XTabUnit    = par[i].XUnit;
          pNeu->XTabDefault = par[i].XDefault;
          pNeu->XTabComment = par[i].XComment;

          pNeu->YTabName    = par[i].YName;
          pNeu->YTabUnit    = par[i].YUnit;
          pNeu->YTabDefault = par[i].YDefault;
          pNeu->YTabComment = par[i].YComment;
    
          break;
      case DEF_2D_TAB:

          pNeu->TabOrder    = par[i].Order;

          pNeu->XTabName    = par[i].XName;
          pNeu->XTabUnit    = par[i].XUnit;
          pNeu->XTabDefault = par[i].XDefault;
          pNeu->XTabComment = par[i].XComment;

          pNeu->YTabName    = par[i].YName;
          pNeu->YTabUnit    = par[i].YUnit;
          pNeu->YTabDefault = par[i].YDefault;
          pNeu->YTabComment = par[i].YComment;

          pNeu->ZTabName    = par[i].ZName;
          pNeu->ZTabUnit    = par[i].ZUnit;
          pNeu->ZTabDefault = par[i].ZDefault;
          pNeu->ZTabComment = par[i].ZComment;
    
          break;
      default:
          pNeu->Type      = DEF_VOID;
          break;
      }

      pNeu->pNext     = pDSParSetVar;

      pDSParSetVar = pNeu;
    }
  }
    
  return OKAY;
}
//=========================================
// Alle Default dieses Moduls zurücksetzen
// d.h. keine Deaults zulassen
//=========================================
status_t CDsPar::setNoDefault(char *mod_name) {

    SDsParSetVar *pPar = pDSParSetVar;

    while(pPar) {


        if( pPar->ModName == mod_name ) {

            pPar->Default     = "";
            pPar->XTabDefault = "";
            pPar->YTabDefault = "";
            pPar->ZTabDefault = "";
        }

        pPar = pPar->pNext;
    }

    return OKAY;
}
//=========================================
// Gruppenauswahl
// 
//=========================================
status_t CDsPar::decideGroup(char *mod_name
                         ,char *group
                         ,CSlfStrV &subgrouplist
                         ,char *switchpar
                         ,char *defvalue/*=""*/) {


    SDsParSetGroup *p = new SDsParSetGroup;

    p->ModName      = mod_name;
    p->MasterGroup  = group;
    p->SubGroupList = subgrouplist;
    p->VarName      = switchpar;
    p->pNext        = pDSSDsParSetGroup;
    p->Default      = defvalue;

    pDSSDsParSetGroup = p;

    return OKAY;
}
void CDsPar::setVarDelete(void) {

    SDsParSetVar *pvar;
    
    while( pDSParSetVar ) {

        pvar = pDSParSetVar->pNext;

        if( pDSParSetVar->Type == DEF_VEC )
            deleteVec(pDSParSetVar);


        delete pDSParSetVar;
        pDSParSetVar = pvar;
    }
}
void CDsPar::deleteModul(char *mod_name) {

    SDsParSetVar *p = pDSParSetVar;
    SDsParSetVar *pvar;
    
    while( p ) {

        if( p->ModName == mod_name ) {

            pvar = p->pNext;

            if( pDSParSetVar->Type == DEF_VEC )
                deleteVec(p);

            delete p;

            if( p == pDSParSetVar )
                pDSParSetVar = pvar;

            p = pvar;
        
        } else {
        
            p = p->pNext;
        }
    }
}
void CDsPar::deleteVec(SDsParSetVar *pvar) {

        // Hier geildeter Vektorvariable
        //===============================
        if(  pDSParSetVar->VecBuild ) {

            Vector_t **ppvec = (Vector_t **) pvar->pVal;
            if(  pvar->VecSet 
              && **ppvec != 0 
              )
                FreeVector(**ppvec);
            
            delete *ppvec;

        } else
        // selbsterstellter Vektor löschen
        if(  pDSParSetVar->VecSet 
          && *(Vector_t *)pDSParSetVar->pVal != 0
          ) {
            FreeVector(*(Vector_t *)pDSParSetVar->pVal);
        }
}
void CDsPar::setGroupDelete(void) {

    SDsParSetGroup *p = pDSSDsParSetGroup;
    SDsParSetGroup *pg;
    
    while( p ) {

        pg = p->pNext;

        delete p;

        p = pg;
        
    }
    pDSSDsParSetGroup = p;
}
