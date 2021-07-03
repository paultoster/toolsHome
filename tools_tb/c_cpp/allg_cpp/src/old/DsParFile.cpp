#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#define DS_PAR_FILE_GET_TYPE_NAME_DEFINE
#define SLFBASIC_VAR_TYPE_STR
#include "DsParFile.h"
#include "SlfSys.h"
#include "SlfFkt.h"

#define CIN_LENGTH_JSJS 253

//=================================================================    
//=================================================================    
// Konstruktor CDsParFile
//=================================================================    
//=================================================================    
CDsParFile::CDsParFile(CSlfLogFile *plogfile) {

	
    Status        = OKAY;

    if( plogfile == 0 ) { // kein Logfile benutzen

        pLogFile   = 0;
        LogFileSet = 0;

    } else { // Logfile anlegen

        pLogFile   = plogfile;
        LogFileSet = 1;
    }

	setQuot();

}
CDsParFile::CDsParFile() {

    Status = OKAY;
        
    pLogFile   = 0;
    LogFileSet = 0;

	setQuot();
}
//=================================================================    
//=================================================================    
// Destruktor CDsParFile
//=================================================================    
//=================================================================    
CDsParFile::~CDsParFile() {

    reset();

}
void CDsParFile::setQuot() {

    // Zeichen für die Interpretation aus einer
    // DAtei (tof-angelehnt)
    QuotGrLen  = 0;
    cQuot0Gr[QuotGrLen  ] = "<";
    cQuot1Gr[QuotGrLen++] = ">";
    cQuot0Gr[QuotGrLen  ] = "[";
    cQuot1Gr[QuotGrLen++] = "]";
    cQuot0Gr[QuotGrLen  ] = "(";
    cQuot1Gr[QuotGrLen++] = ")";
    cQuot0Gr[QuotGrLen  ] = "((";
    cQuot1Gr[QuotGrLen++] = "))";
    cQuot0Gr[QuotGrLen  ] = "(((";
    cQuot1Gr[QuotGrLen++] = ")))";
    cQuot0Gr[QuotGrLen  ] = "((((";
    cQuot1Gr[QuotGrLen++] = "))))";
    cQuot0Gr[QuotGrLen  ] = "(((((";
    cQuot1Gr[QuotGrLen++] = ")))))";
    cQuot0Gr[QuotGrLen  ] = "((((((";
    cQuot1Gr[QuotGrLen++] = "((((((";
     
    CommentLen            = 0;
    cComment[CommentLen++] = "!";
    cComment[CommentLen++] = "%";
    cComment[CommentLen++] = "$";

    cQuot0SpezTab          = "{";
    cQuot1SpezTab          = "}";
    cQuot01DTab            = "{{";
    cQuot11DTab            = "}}";
    cQuot02DTab            = "{{{";
    cQuot12DTab            = "}}}";
    
    cZuweis                = "=";
    cQuot0                 = "\"";
    cQuot1                 = "\"";
    cQuota                 = "'";
    cQuotb                 = "'";
    cQuotUnit0             = "[";
    cQuotUnit1             = "]";
    cQuotMat0              = "[";
    cQuotMat1              = "]";
    cTrennZeich            = ",";
    cTrennZeichCol         = ";";
    cTrennZeichAttr        = ".";

    cAttrOffset            = "offset";
    cAttrFactor            = "factor";
    cAttrCopy              = "copy";
    cAttrType              = "type";
    cAttrComment           = "comment";
    cAttrUnit              = "unit";
    cAttrTranspose         = "transpose";

#if DS_PAR_USE_MATFILELOAD == 1
	cAttrMatFileRead       = "matload";
	cQuotFileBracket0      = "(";
	cQuotFileBracket1      = ")";
	cAttrFileName          = "file";
	cAttrFileVarName       = "variable";
#endif

	cAttrInclude           = "#include";

    cValLinear             = "1";
    cValNotLinear          = "0";


    NMas = 0;

    pInstanz = 0;

    setInternGroupListe(0,0);
    pTabAct = 0;
    pVarAct = 0;
    pGroupAct = 0;

#if DS_PAR_USE_MATFILELOAD == 1
    pExternFileList = 0;
#endif
}

status_t CDsParFile::ini(CSlfLogFile *plogfile/*=0*/) {


    resetErrText();

    Status        = OKAY;

    if( plogfile ) { 

        pLogFile   = plogfile;
        LogFileSet = 1;
    }
    return OKAY;
}
//=================================================================    
//=================================================================    
// Reset
//=================================================================    
//=================================================================    
void CDsParFile::reset(void) {

    Status = OKAY;
    ErrText = "";

    deleteInternGroup(pInstanz);
#if DS_PAR_USE_MATFILELOAD == 1
	deleteExternFileList();
#endif

}
//=================================================================    
//=================================================================    
// read: Datei lesen
//=================================================================    
//=================================================================    
status_t CDsParFile::read(char *par_file,CDsParData &par_data) {

    
    CSlfStr         tline;
    SDsParReadLine  rstruct;
    // Parameterfile öffnen
    //=====================
    if( !SlfSysExistFile(par_file) ) 
    {
        Status = NOT_OKAY;
        ErrText.catFormat("Error in CDsParFile::read(); Parameterfile <%s> kann nicht gefunden werden\n"
                         ,par_file);
		    writeLogFile("CDsParFile::read");
        return Status;
    }

	  rstruct.iRF = 0;   // erstes ParameterFile

#if _MSC_VER > 1310
     fopen_s(&(rstruct.ReadFile[rstruct.iRF].Fid),par_file,"r");
#else
    rstruct.ReadFile[rstruct.iRF].Fid = fopen(par_file,"r");
#endif
    if( rstruct.ReadFile[rstruct.iRF].Fid == 0 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("Error in CDsParFile::read(); Parameterfile <%s> kann nicht gelesen werden\n"
                         ,par_file);
		writeLogFile("CDsParFile::read");

        return Status;
    }


    rstruct.ReadFile[rstruct.iRF].Zeile    = 0;
    rstruct.ReadFile[rstruct.iRF].ParFile  = par_file;
	rstruct.ReadFile[rstruct.iRF].ret      = 0;
    rstruct.ReadFlag = 1;
    rstruct.Type     = IS_UNKOWN; 

    while(rstruct.ReadFlag) {

        // Zeile löschen, Structurbereinigen
        resetStruct(tline,&rstruct);
        
        // nächste Zeile einlesen
        //=======================
        if( readLine(tline,&rstruct) == EOF )
            rstruct.ReadFlag = 0;

		    if( Status != OKAY )
			    return Status;

		

	        rstruct.Tline = tline;

         
        // neuen Zustand bestimmen
        //========================
        if( findStruct(tline,&rstruct) != OKAY ) {
			      writeLogFile("CDsParFile::read");
            Status = NOT_OKAY;
            break;
        }

#if DUMP_FILE_INPUT_STRUCT

        if( rstruct.ReadFile[rstruct.iRF].Zeile == 51 )
            rstruct.ReadFile[rstruct.iRF].Zeile = 51;

        dumpStruct(tline,&rstruct);
#endif

        // Werte an Parameterstruktur weitergeben
		// pInstanz aufbauen
        //=======================================
        if( setInternParStruct(&rstruct) != OKAY ) {
			writeLogFile("CDsParFile::read");
            Status = NOT_OKAY;
            break;
        }


    }

    fclose(rstruct.ReadFile[rstruct.iRF].Fid);
#if DUMP_FILE_INPUT_STRUCT
    dumpStruct(tline,0);
#endif

#if DS_PAR_USE_MATFILELOAD == 1
	// extern Filedaten einlesen
	if( readExternPar() != OKAY ) {
		writeLogFile("CDsParFile::read");
		Status = NOT_OKAY;
		return Status;
	}
#endif

    // Parameterstruktur auswerten und an ParData weitergeben
    //=======================================================
	// Bis jetzt noch kein Kopieren möglich
    if( setParData(par_data) != OKAY ) {
		writeLogFile("CDsParFile::read");
        Status = NOT_OKAY;
        return Status;
    }

#ifdef DS_DEBUG
    //=======================
    // Ausgabe in das LogFile
    //=======================
        writeLogFile("CDsParFile::read");
#endif

    return Status;
}
//=================================================================    
//=================================================================    
// readLine nächste Zeile auslesen
//=================================================================    
//=================================================================    
char CDsParFile::readLine(CSlfStr &tline           // Zeilentext
                         ,SDsParReadLine *pstruct     // Hilfsstruct
                         ) {     


    char   cin[CIN_LENGTH_JSJS+2];
    uint32_t i = 0;
	char   isfin = 0;
	uint32_t ipos;


	// Überprüfen, ob aktuelle Datei zuende
	//=====================================
	while( pstruct->ReadFile[pstruct->iRF].ret == EOF ) {

		// Es gibt keine weitere übergordente Datei
		if( pstruct->iRF == 0 ) {

			tline = "";
			return pstruct->ReadFile[pstruct->iRF].ret;

		// es gibtne noch
		} else {

			// Datei schliessen
			fclose(pstruct->ReadFile[pstruct->iRF].Fid);
			pstruct->ReadFile[pstruct->iRF].Fid = 0;
			pstruct->ReadFile[pstruct->iRF].ParFile = "";
			pstruct->ReadFile[pstruct->iRF].ret = 0;
			pstruct->ReadFile[pstruct->iRF].Zeile = 0;

			//übergeordnete Datei
			pstruct->iRF--;
		}
	}

	// Nächste Zeile einlesen
	//=======================
	while(!isfin) {

		i = 0;

		while( 1 ) {

			cin[i]=fgetc(pstruct->ReadFile[pstruct->iRF].Fid);
			if( cin[i] == EOF ) {
				cin[i] = '\0';
				tline.cat(cin);
				pstruct->ReadFile[pstruct->iRF].Zeile += 1;
				pstruct->ReadFile[pstruct->iRF].ret = EOF;
				break;

			} else if( cin[i] == '\n' ) {

				cin[i] = '\0';
				tline.cat(cin);
				pstruct->ReadFile[pstruct->iRF].Zeile += 1;
				break;

			} else if( i == CIN_LENGTH_JSJS ) {

				cin[i+1] = '\0';
				tline.cat(cin);
				i = 0;
			} else if( cin[i] == '\t' ) {

				cin[i] = ' ';
				i++;

			} else {

				i++;

			}
		}

		ipos = tline.findNot(" ");

		// Datei Include suchen
		//=====================
		if( tline.find(cAttrInclude) == ipos && ipos != SLF_STR_NPOS) {

			// noch nicht fertig
			isfin = 0;

			tline.cut(0,ipos+SlfStrLen(cAttrInclude));
			tline.elimAnfEndC();

			// Dateizähler hochzählen
			pstruct->iRF++;

			// Wenn Que am Ende
			if( pstruct->iRF >= DS_PAR_READ_MAX_FILES ) {
				ErrText.catFormat("Das Kommando <%s> in Zeile <%i> aus Datei: <%s> kann nicht erkannt werden !!!\n"
								 ,pstruct->Tline.c_str()
								 ,pstruct->ReadFile[pstruct->iRF].Zeile
								 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
								 );
        
				Status = NOT_OKAY;
				return 0;
			}

			// Neue Datei
			pstruct->ReadFile[pstruct->iRF].ParFile = tline;

			tline.clear();


			// neue Datei prüfen
			//==================
			if( !SlfSysExistFile(pstruct->ReadFile[pstruct->iRF].ParFile.c_str()) ) {
				Status = NOT_OKAY;
				ErrText.catFormat("Error in CDsParFile::readLine(); Die in übergeordenete Parameterdatei <%s> eingebettete (%s) Parameterdatei <%s> kann nicht gefunden werden\n"
								 ,pstruct->ReadFile[pstruct->iRF - 1].ParFile.c_str()
								 ,cAttrInclude
								 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str());
				writeLogFile("CDsParFile::read");
				return 0;
			}

#if _MSC_VER > 1310
     fopen_s(&(pstruct->ReadFile[pstruct->iRF].Fid),pstruct->ReadFile[pstruct->iRF].ParFile.c_str(),"r");
#else
		 pstruct->ReadFile[pstruct->iRF].Fid = fopen(pstruct->ReadFile[pstruct->iRF].ParFile.c_str(),"r");
#endif
			if( pstruct->ReadFile[pstruct->iRF].Fid == 0 ) {
				Status = NOT_OKAY;
				ErrText.catFormat("Error in CDsParFile::read(); Parameterfile <%s> kann nicht gelesen werden\n"
								 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str());
				writeLogFile("CDsParFile::read");

				return 0;
			}

			// iZeile auf null setzen
			pstruct->ReadFile[pstruct->iRF].ret   = 0;
			pstruct->ReadFile[pstruct->iRF].Zeile = 0;


		// EndOfFile überprüfen
		//=====================
		} else if( pstruct->ReadFile[pstruct->iRF].ret == EOF ) {

			// kein weiteres übergeordnetes File
			if( pstruct->iRF == 0 ) {

				return pstruct->ReadFile[pstruct->iRF].ret;

			} else {

				return 0; // Es gibt noch übergeordente Files
			}


		// Leere Zeile
		//============
		} else if( tline.getLen() == 0  ) {

			isfin = 0;

		// Einlesen beenden
		//=================
		} else {

			isfin = 1;
		}

	}
    return pstruct->ReadFile[pstruct->iRF].ret;
}
//=================================================================    
//=================================================================    
// Struktur SDsParReadLine zurücksetzen für nächstes einlesen
//=================================================================    
//=================================================================    
void CDsParFile::resetStruct(CSlfStr &tline,SDsParReadLine *pstruct) {


    tline.clear();
    pstruct->GroupName.clear();
    pstruct->VariableName.clear();
    pstruct->Unit.clear();
    pstruct->Values.clear();
    pstruct->Comment.clear();
    pstruct->Factor.clear();
    pstruct->Offset.clear();
    pstruct->Transpose.clear();
    pstruct->CopyGroup.clear();
    pstruct->CopyVariable.clear();

    pstruct->TableName.clear();
    pstruct->TableOrder.clear();
    pstruct->TableUnit.clear();
    pstruct->TableValue.clear();
	pstruct->FileName.clear();
    pstruct->FileVarName.clear();
	pstruct->FileType = IS_NON_FILE;
}
//#################################################################
//#################################################################
// findStruct: eingelesene Zeile auswerten
//#################################################################
//#################################################################
status_t CDsParFile::findStruct(CSlfStr &tline
                               ,SDsParReadLine *pstruct) {

    CSlfStr   txt1,txt2,txtl,txtu,txta;
    CSlfStr   txt0;

    uint32_t    i0;
    uint8_t     left_flag=0,right_flag=0,unit_flag=0,attr_flag=0;

	// Leerzeichen eliminieren
	//========================
	tline.elimAnfEndC();

	// Kommentar löschen
	//==================
	elimComment(tline);

	if( tline.getLen() == 0 ) {
        pstruct->TypeRead = IS_EMPTY;
        return OKAY;
    }
    tline.elimAnfEndC();

    // Wenn Spezialtabelle, dann solange Werte da sind einlesen:
    if(  pstruct->Type == IS_SPEZTABLE ) {
      
        if( isSpezTableValue(tline) ) {

            findSpezTableValue(tline,pstruct);
            pstruct->TypeRead = IS_SPEZTABLE_VALUE;
            return Status;
        } else {
            pstruct->TypeRead = IS_UNKOWN;
        }
    }



    // Nach Zuweisung suchen
    if( tline.find(cQuota) != SLF_STR_NPOS )
        i0=tline.find(cZuweis,"vs",cQuota,cQuotb,"a",0);
    else
        i0=tline.find(cZuweis,"vs",cQuot0,cQuot1,"a",0);

    if( i0 != SLF_STR_NPOS ) { // Zuweisungszeichen gefunden

        // linker Text
        //============
        txtl = tline;txtl.cut(i0);
        txtl.elimAnfEndC();
        left_flag = 1;

        // rechter Text bleibt in tline
        //=============================
        tline.cut(0,i0+SlfStrLen(cZuweis));
        tline.elimAnfEndC();
        right_flag = 1;
    
    } else {

        // linker Text
        //============
        txtl = tline;
        txtl.elimAnfEndC();
        left_flag = 1;

        tline.clear();
    }

    //linker Seite untersuchen
    //========================
    if( isGroup(txtl) ) {

        pstruct->TypeRead = IS_GROUP;
        findGroup(txtl,pstruct);

    } else if( is1DTable(txtl) ) {

        pstruct->TypeRead = IS_1DTABLE;
        find1DTable(txtl,pstruct);
        
    } else if( is2DTable(txtl) ) {

        pstruct->TypeRead  = IS_2DTABLE;
        find2DTable(txtl,pstruct);

    } else if( isSpezTable(txtl) ) {

        pstruct->TypeRead = IS_SPEZTABLE;
        findSpezTable(txtl,pstruct);

    } else if( isVariable(txtl) ) {
    
        pstruct->TypeRead  = IS_VARIABLE;
        findVariable(txtl,pstruct);
    } else {

        ErrText.catFormat("Das Kommando <%s> in Zeile <%i> aus Datei: <%s> kann nicht erkannt werden !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         );
        
        Status = NOT_OKAY;
        return Status;
    }
    
    // Rest des linken Textes nach Einheit suchen
    //===========================================
    txtl.elimAnfEndC();
    if(  pstruct->TypeRead  == IS_VARIABLE
      && (i0=txtl.find(cQuotUnit0)) == 0 
      ) {
     
        unit_flag = 1;
        txtl.findText(txtu,cQuotUnit0,cQuotUnit1,"vs");
        if( txtu.getLen() == 0 ) {
            ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann die Einheit nicht richtig erkannt werden, da der Einheitenquot <%s> nicht gefunden wurde !!!\n"
                             ,pstruct->Tline.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                             ,cQuotUnit1
                             );
            Status = NOT_OKAY;
            return Status;
        }
        pstruct->Unit = txtu;
	
	} 

    // Rest des linken Textes nach Attributzeichen suchen
    //===================================================
    if( (i0=txtl.find(cTrennZeichAttr)) == 0 ) {

        attr_flag = 1;
        txtl.findText(txta,cTrennZeichAttr,"","vs");

        txta.elimAnfEndC();

        if( pstruct->TypeRead == IS_GROUP ) { 

            if( txta == cAttrCopy ) {

                pstruct->TypeRead = IS_GROUP_COPY;

                // Gruppenhierachie suchen
                getGroupCopy(tline,pstruct);

            } else if( txta == cAttrComment ) {

                pstruct->TypeRead = IS_GROUP_COMMENT;
                getComment(tline,pstruct);

            } else {
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Attribut <%s> nicht für eine Gruppe erkannt werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,txta.c_str()
                                 );
                Status = NOT_OKAY;
                return Status;
            }
        } else if( pstruct->TypeRead == IS_VARIABLE ) {

            if( txta == cAttrCopy ) {

                pstruct->TypeRead = IS_VARIABLE_COPY;

                // Gruppenhierachie suchen
                getGroupCopy( tline,pstruct );
                getVariableCopy( tline,pstruct );

            } else if( txta == cAttrComment ) {

                pstruct->TypeRead = IS_VARIABLE_COMMENT;
                getComment(tline,pstruct);

            } else if( txta == cAttrUnit ) {

                pstruct->TypeRead = IS_VARIABLE_UNIT;

                if( getUnit(tline,pstruct) != OKAY ) {

                    Status = NOT_OKAY;
                    return Status;
                }

            } else if( txta == cAttrFactor ) {

                    pstruct->TypeRead = IS_VARIABLE_FACTOR;

                    pstruct->Factor = tline;

            } else if( txta == cAttrOffset ) {

                    pstruct->TypeRead = IS_VARIABLE_FACTOR;

                    pstruct->Offset = tline;

            } else if( txta == cAttrTranspose ) {

                    pstruct->TypeRead = IS_VARIABLE_TRANSPOSE;

                    pstruct->Transpose = tline;

            } else {
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Attribut <%s> nicht für einer Variable erkannt werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,txta.c_str()
                                 );
                Status = NOT_OKAY;
                return Status;
            }
        } else if( pstruct->TypeRead == IS_1DTABLE ) {

            if( txta == cAttrCopy ) {

                pstruct->TypeRead = IS_1DTABLE_COPY;

                // Gruppenhierachie suchen
                getGroupCopy(tline,pstruct);
                get1DTableCopy(tline,pstruct);
    
            } else if( txta == cAttrComment ) {

                pstruct->TypeRead = IS_1DTABLE_COMMENT;
                getComment(tline,pstruct);

            } else {
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Attribut <%s> nicht für eine 1D-Tablle erkannt werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,txta.c_str()
                                 );
                Status = NOT_OKAY;
                return Status;
            }
        } else if( pstruct->TypeRead == IS_2DTABLE ) {

            if( txta == cAttrCopy ) {

                pstruct->TypeRead = IS_2DTABLE_COPY;

                // Gruppenhierachie suchen
                getGroupCopy(tline,pstruct);
                get1DTableCopy(tline,pstruct);
    
            } else if( txta == cAttrComment ) {

                pstruct->TypeRead = IS_2DTABLE_COMMENT;
                getComment(tline,pstruct);

            } else {
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Attribut <%s> nicht für eine 1D-Tablle erkannt werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,txta.c_str()
                                 );
                Status = NOT_OKAY;
                return Status;
            }
        }

    // Wertezuordnung
    //===============
    } else if( right_flag ) {

		// Hier keine Auswertung nach Klammern für Matrix_t oder Vektor, da kopiert werden kann
		if(  pstruct->TypeRead == IS_GROUP ) {

			pstruct->TypeRead = IS_GROUP_COPY;
			getGroupCopy(tline,pstruct);
			
		} else if(  pstruct->TypeRead == IS_1DTABLE ) {

            pstruct->TypeRead = IS_1DTABLE_COPY;
			getGroupCopy(tline,pstruct);
            get1DTableCopy(tline,pstruct);

		} else if(  pstruct->TypeRead == IS_2DTABLE ) {

            pstruct->TypeRead = IS_2DTABLE_COPY;
			getGroupCopy(tline,pstruct);
            get2DTableCopy(tline,pstruct);
		} else {

			if( getValue(tline,pstruct) != OKAY ) {
				Status = NOT_OKAY;
				return NOT_OKAY;
			}
		}
    }
    return Status;
}
#if DUMP_FILE_INPUT_STRUCT
//#################################################################
//#################################################################
// dumpStruct: eingelesene Struktur zum Überprüfen ausgeben
//#################################################################
//#################################################################
status_t CDsParFile::dumpStruct(CSlfStr &tline
                               ,SDsParReadLine *pstruct) {

    static  FILE  *fid=0;
    uint32_t        irow,icol;

    if( fid == 0 ) {    
        fid = fopen("DumpDsParFile.txt","w");

        if( fid == 0 ) {
            Status = NOT_OKAY;
            ErrText.catFormat("Error in CDsParFile::dumpStruct(); Parameterfile <%s> kann geöffnet werden\n"
                             ,"DumpDsParFile.txt");
            return Status;
        }
        fprintf(fid
               ,"Parameterdatei: %s\n"
               ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str());    

    }

    if( pstruct == 0 && fid != 0 ) {

        fclose(fid);
        fid = 0;
        return Status;
    }

    fprintf(fid
           ,"============================================================\n");

    fprintf(fid
           ,"Zeile: %i\n"
           ,pstruct->ReadFile[pstruct->iRF].Zeile);
    fprintf(fid
           ,"tline: %s\n"
           ,pstruct->Tline.c_str());
    fprintf(fid
           ,"Type: %s(%i)\n"
           ,DsParFileGetTypeName(pstruct->Type),pstruct->Type);
    fprintf(fid
           ,"TypeRead: %s(%i)\n"
           ,DsParFileGetTypeName(pstruct->TypeRead),pstruct->TypeRead);
    fprintf(fid
           ,"TypeNext: %s(%i)\n"
           ,DsParFileGetTypeName(pstruct->TypeNext),pstruct->TypeNext);
    if( pstruct->GroupName.getLen() > 0 )
        fprintf(fid
               ,"Group: %s\n"
               ,pstruct->GroupName.c_str());
    if( pstruct->GroupName.getLen() > 0 )
        fprintf(fid
               ,"Hierach: %i\n"
               ,pstruct->Hierach);

    if( pstruct->Comment.getLen() )
        fprintf(fid
               ,"Comment: %s\n"
               ,pstruct->Comment.c_str());

    for(irow=0;irow<pstruct->CopyGroup.getNrows();irow++)
        fprintf(fid
               ,"CopyGroup(%i): %s\n"
               ,irow,pstruct->CopyGroup.get_str(irow));
    if( pstruct->CopyGroup.getNrows() > 0 )
        fprintf(fid
               ,"CopyHierach: %i\n"
               ,pstruct->CopyHierach);

    if( pstruct->VariableName.getLen() > 0 )
        fprintf(fid
               ,"Variable: %s\n"
               ,pstruct->VariableName.c_str());

    if( pstruct->Unit.getLen() > 0 )
        fprintf(fid
               ,"Unit: %s\n"
               ,pstruct->Unit.c_str());

    if( pstruct->Values.getNcols()*pstruct->Values.getNrows() > 0 )

        for(irow=0;irow<pstruct->Values.getNrows();irow++)
            for(icol=0;icol<pstruct->Values.getNcols();icol++)
                fprintf(fid
                       ,"Value(%i,%i): %s\n"
                       ,irow,icol,pstruct->Values.get_str(irow,icol));


    if( pstruct->CopyVariable.getLen() > 0 )
        fprintf(fid
               ,"CopyVariable: %s\n"
               ,pstruct->CopyVariable.c_str());

    

    if( pstruct->Factor.getLen() > 0 )
        fprintf(fid
               ,"Factor: %s\n"
               ,pstruct->Factor.c_str());
    if( pstruct->Offset.getLen() > 0 )
        fprintf(fid
               ,"Offset: %s\n"
               ,pstruct->Offset.c_str());
    if( pstruct->Transpose.getLen() > 0 )
        fprintf(fid
               ,"Transpose: %s\n"
               ,pstruct->Transpose.c_str());
            
    
    return Status;
}
#endif
//#################################################################
//#################################################################
// elimComment: Kommentar entfernen
//#################################################################
//#################################################################
void CDsParFile::elimComment(CSlfStr &tline) {

    uint32_t index1,index2;

    for(uint8_t i=0;i<CommentLen;i++) {
    
        index1 = tline.find(cComment[i],"vs",cQuot0,cQuot1,"a",0);
        index2 = tline.find(cComment[i],"vs",cQuot0,cQuot1,"a",0);

        if( (index1 != SLF_STR_NPOS) && (index1 == index2) )
            tline.cut(index1);

        tline.elimAnfEndC();
    }
}
//#################################################################
//#################################################################
// isGroup: Prüfen ob, eine Gruppe kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::isGroup(CSlfStr &tline) {

    uint8_t i;
        
    for(i=0;i<QuotGrLen;i++) {

        // Nach Anfangsquotzeichen suchen
        if( tline.find(cQuot0Gr[i]) == 0 
          && isalnum(*(tline.c_str()+SlfStrLen(cQuot0Gr[i])))            
          ) {

            if(tline.find(cQuot1Gr[i]) > SlfStrLen(cQuot0Gr[i]) ) { // mindestens ein Zeichen

                return 1;
            }
        }
    }

    return 0;
}
//#################################################################
//#################################################################
// isGroupAttribut: Prüfen ob, eine Gruppe kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::isGroupAttribut(CSlfStr &tline,SDsParReadLine *pstruct) {

    if( tline.find(cTrennZeichAttr,1) != SLF_STR_NPOS ) {

        CSlfStr txt;
        tline.findText(txt,"",cTrennZeichAttr,"vs");
    
        if( txt == pstruct->GroupName )
            return 1;
    }

    return 0;
}
//#################################################################
//#################################################################
// is1DTable: Prüfen ob, eine 1DTable kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::is1DTable(CSlfStr &tline) {
        
    // Nach Anfangsquotzeichen suchen
    if( tline.find(cQuot01DTab) == 0 && tline.find(cQuot02DTab) != 0) {

        if(tline.find(cQuot11DTab,SlfStrLen(cQuot01DTab)) != SLF_STR_NPOS ) { // mindestens ein Zeichen

            return 1;
        }
    }

    return 0;
}
//#################################################################
//#################################################################
// is2DTable: Prüfen ob, eine 2DTable kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::is2DTable(CSlfStr &tline) {

    // Nach Anfangsquotzeichen suchen
    if( tline.find(cQuot02DTab) == 0 ) {

        if(tline.find(cQuot12DTab,SlfStrLen(cQuot02DTab)) != SLF_STR_NPOS ) { // mindestens ein Zeichen

            return 1;
        }
    }

    return 0;
}
//#################################################################
//#################################################################
// isPezTable: Prüfen ob, eine Pezial-Table kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::isSpezTable(CSlfStr &tline) {

    // Nach Anfangsquotzeichen suchen
    if( tline.find(cQuot0SpezTab) == 0 && tline.find(cQuot01DTab) != 0 && tline.find(cQuot02DTab) != 0) {

        if(tline.find(cQuot12DTab,SlfStrLen(cQuot1SpezTab)) != SLF_STR_NPOS ) { // mindestens ein Zeichen

            return 1;
        }
    }

    return 0;
}
//#################################################################
//#################################################################
// is2DTable: Prüfen ob, ein Wert eine Spezial-Table kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::isSpezTableValue(CSlfStr &tline) {

    uint32_t i0,i1;

    // Nach Zuweisungzeichen suchen, dann kein Wert
    i0=tline.find(cZuweis,"vs",cQuot0,cQuot1,"a",0);
    i1=tline.find(cZuweis,"vs",cQuota,cQuotb,"a",0);

    if(  (i0 != SLF_STR_NPOS && i1 != SLF_STR_NPOS)  // Zuweisungszeichen gefunden
      || isGroup(tline)
      || is1DTable(tline)
      || is2DTable(tline)
      || isSpezTable(tline)
      )
        return 0; // kein spezialwert
    else
        return 1;

}

//#################################################################
//#################################################################
// isVariable: Prüfen ob, Variable kommt
//#################################################################
//#################################################################
uint8_t CDsParFile::isVariable(CSlfStr &tline) {

    // Nach ersten Leerzeichen, Einheit, Zuweisung óder Attributstrennzeichen
    // und erste Zeichen alphanumerisch
    if(  /*(  tline.find(" ",1) != SLF_STR_NPOS
         || tline.find(cQuotUnit0,1) != SLF_STR_NPOS
         || tline.find(cZuweis,1) != SLF_STR_NPOS
         || tline.find(cTrennZeichAttr,1) != SLF_STR_NPOS
         )
      &&*/ isalnum(*tline.c_str())
      ) {

        return 1;
        
    }

    return 0;
}

//#################################################################
//#################################################################
// findGroup: Gruppe linke Seitesuchen
//#################################################################
//#################################################################
status_t CDsParFile::findGroup(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint8_t   ig;
    uint32_t  i1;
    CSlfStr txt;

    //Gruppenhierachie bestimmen
    for(ig=0;ig<QuotGrLen;ig++) {

        // Nach Anfangsquotzeichen suchen, das darauffolgende Zeichen muß
        // alphanumerisch sein
        if(  tline.find(cQuot0Gr[ig]) == 0 
          && isalnum(*(tline.c_str()+SlfStrLen(cQuot0Gr[ig])))            
          ) {

            // Nach Endquotzeichen suchen
            if( (i1=tline.find(cQuot1Gr[ig],SlfStrLen(cQuot0Gr[ig]))) != SLF_STR_NPOS ){

                tline.findText(pstruct->GroupName,cQuot0Gr[ig],cQuot1Gr[ig],"vs");
                tline.cut(0,i1+SlfStrLen(cQuot1Gr[ig]));
                tline.elimAnfEndC();
                pstruct->Hierach =  ig;
                pstruct->GroupName.elimAnfEndC();
                break;
            } else {

                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Gruppenendezeichen <%s> nicht erkannt werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,cQuot1Gr[ig]
                                 );
                Status = NOT_OKAY;
                return Status;
            }
        }

    }

    return Status;
}
//#################################################################
//#################################################################
// findGroupAttribut: Gruppen Attribut
//#################################################################
//#################################################################
status_t CDsParFile::findGroupAttribut(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint32_t   i0;
    CSlfStr txt;

    // Zuweisungszeichen suchen
    if( (i0=tline.find(cZuweis,SlfStrLen(cTrennZeichAttr))) != SLF_STR_NPOS )


        tline.findText(txt,cTrennZeichAttr,cZuweis,"vs");
        txt.elimAnfEndC();

        if( txt == cAttrCopy ) {

            tline.cut(0,i0+SlfStrLen(cZuweis));
            if( getGroupCopy(tline,pstruct) ) {
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann die zu kopierende Gruppen <%s> nicht gefunden werden  nicht erkannt werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,tline.c_str()
                                 );
                Status = NOT_OKAY;
                return Status;
            }
            pstruct->Type = IS_GROUP_COPY;

        } else if( txt == cAttrComment ) {

            tline.cut(0,i0+SlfStrLen(cZuweis));
            if( getComment(tline,pstruct) != OKAY )
                return Status;

            pstruct->Type = IS_GROUP_COMMENT;
        } else {

            ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Attribut <%s> nicht erkannt werden !!!\n"
                             ,pstruct->Tline.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                             ,tline.c_str()
                             );
            Status = NOT_OKAY;
        }

                
    return Status;
}
//#################################################################
//#################################################################
// getGroupCopy: Gruppenzuweisung rechte Seite suchen
//#################################################################
//#################################################################
uint8_t CDsParFile::getGroupCopy(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint8_t   i;
    uint32_t  i1;
    uint8_t   suchflag;

    CSlfStr name;
    tline.elimAnfEndC();

    // Liste löschen
    pstruct->CopyGroup.clear();

    //initialisieren
    suchflag = 0;

    // Nach dem nächsten quot suchen begonnen bei ih
    for(i=0;i<QuotGrLen;i++) {

         // Nach Anfangsquotzeichen suchen
         if(  tline.find(cQuot0Gr[i]) == 0
           && isalnum(*(tline.c_str()+SlfStrLen(cQuot0Gr[i])))             
           && tline.find(cQuot1Gr[i],SlfStrLen(cQuot0Gr[i])) !=  SLF_STR_NPOS 
           ) {

             suchflag = 1;
             break;
         }
    }
    if( !suchflag ) {
        return suchflag;
    }
    while( suchflag ) {

        suchflag = 0;
        // Nach Anfangsquotzeichen suchen
        if(  tline.find(cQuot0Gr[i]) == 0 
          && isalnum(*(tline.c_str()+SlfStrLen(cQuot0Gr[i])))
          ) {

            // Diese Gruppe gilt, Text rausholen
            if( (i1=tline.find(cQuot1Gr[i],SlfStrLen(cQuot0Gr[i]))) != SLF_STR_NPOS 
              ) {
                // Gruppe in Vektor eintragen
                tline.findText(name,cQuot0Gr[i],cQuot1Gr[i],"vs");
                name.elimAnfEndC();
                pstruct->CopyGroup.append(name);


                // Bereitsgefundene Gruppe rausschneiden
                tline.cut(0,i1+SlfStrLen(cQuot1Gr[i]));
                tline.elimAnfC();

                // höchste Gruppe (tiefste Verschachtelung) merken
                pstruct->CopyHierach = i;

                // weitersuchen
                i++;
                suchflag = 1;
            }
        }            
    }

    // Wenn nichts in die Liste geschrieben wurde:
    if( pstruct->CopyGroup.getNrows() == 0 ) {

        return 0;
    }

    return 1;
}
//#################################################################
//#################################################################
// getVariableCopy: Variablenkopierzuweisung rechte Seite suchen
//#################################################################
//#################################################################
uint8_t CDsParFile::getVariableCopy(CSlfStr &tline,SDsParReadLine *pstruct) {

    tline.elimAnfEndC();

    if( tline.getLen() == 0 ) {

		return 0;
    } else if( tline.isAlphNum() ) { //a-z oder A-Z oder 0-9

        pstruct->CopyVariable = tline;
    } else {
		return 0;
    }

    return 1;
}
//#################################################################
//#################################################################
// getComment: Kommentarzuweisung rechte Seite suchen
//#################################################################
//#################################################################
status_t CDsParFile::getComment(CSlfStr &tline,SDsParReadLine *pstruct) {

    tline.elimAnfEndC();
    pstruct->Comment = "";

    // Nach Quots suchen
    if( tline.find(cQuot0) == 0 ) { //Startquot gefunden

        while( 1 ) {
            if( tline.find(cQuot1,SlfStrLen(cQuot0)) != SLF_STR_NPOS ) { // Endquot gefunden
    
                tline.findText(pstruct->Comment,cQuot0,cQuot1,"vs");
                return Status;
        
            // kein Endquotgefunden, Datei zuende
            } else if( pstruct->ReadFlag == 0) {
                
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Endezeichen des Kommentars <%s> nicht gefunden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,cQuot1
                                 );
                Status = NOT_OKAY;
                return Status;

            } else { // Ansonsten weiter das Ende suchen lassen

                CSlfStr txt;

                if( readLine(txt,pstruct) == EOF )
                    pstruct->ReadFlag = 0;

				if( Status != OKAY )
					return Status;

                tline.cat(txt);
                pstruct->Tline.cat(txt);
            }
        }
    } else if( tline.find(cQuota) == 0 ) { //Startquot gefunden

        while( 1 ) {
            if( tline.find(cQuotb,SlfStrLen(cQuota)) != SLF_STR_NPOS ) { // Endquot gefunden
    
                tline.findText(pstruct->Comment,cQuota,cQuotb,"vs");
                return Status;
        
            // kein Endquotgefunden, Datei zuende
            } else if( pstruct->ReadFlag == 0) {
                
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Endezeichen des Kommentars <%s> nicht gefunden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,cQuotb
                                 );
                Status = NOT_OKAY;
                return Status;

            } else { // Ansonsten weiter das Ende suchen lassen

                CSlfStr txt;

                if( readLine(txt,pstruct) == EOF )
                    pstruct->ReadFlag = 0;

				if( Status != OKAY )
					return Status;

                tline.cat(txt);
                pstruct->Tline.cat(txt);
            }
        }
    } else { // Kommentar ohne Quot einlesen

        pstruct->Comment = tline;
    }

    return Status;

}
//#################################################################
//#################################################################
// getUnit: Einheitenzuweisung rechte Seite suchen
//#################################################################
//#################################################################
status_t CDsParFile::getUnit(CSlfStr &tline,SDsParReadLine *pstruct) {

    tline.elimAnfEndC();
    pstruct->Unit = "";
    tline.findText(pstruct->Unit,cQuotUnit0,cQuotUnit1,"vs");

    if( pstruct->Unit.getLen() == 0 ) {

        tline.findText(pstruct->Unit,cQuot0,cQuot1,"vs");

        if( pstruct->Unit.getLen() == 0 ) {

            tline.findText(pstruct->Unit,cQuota,cQuotb,"vs");

            if( pstruct->Unit.getLen() == 0 )
                pstruct->Unit = tline;
        }

    }

    if( pstruct->Unit.getLen() == 0 ) {
        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann die Einheitenzuordnung nicht erkannt werden, da kein Wert gefunden wurde !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         );
        Status = NOT_OKAY;
        return Status;

    }

    return Status;

}
//#################################################################
//#################################################################
// findVariable: Variablenzuordnung
//#################################################################
//#################################################################
status_t CDsParFile::findVariable(CSlfStr &txtl,SDsParReadLine *pstruct) {

    CSlfStr txta;

    uint32_t  i0=txtl.find(cQuotUnit0);
    uint32_t  i1=txtl.find(cTrennZeichAttr);

    uint8_t   flag;

    // Einheit
    if( i0 != SLF_STR_NPOS && i0 < i1 ) {
        
        flag = 0;
        txtl.findText(pstruct->VariableName,"",cQuotUnit0,"vs");
        txtl.cut(0,i0);
        txtl.elimAnfEndC();

    // nach AttributTrennzeichen suchen
    } else if( i1 != SLF_STR_NPOS && i1 < i0 ) {

        flag = 1;
        txtl.findText(pstruct->VariableName,"",cTrennZeichAttr,"vs");
        txtl.cut(0,i1);
        txtl.elimAnfEndC();

    } else { // nur Variable

        flag = 2;
        pstruct->VariableName = txtl;
    }
    pstruct->VariableName.elimAnfEndC();

    if( flag == 0 && pstruct->VariableName.getLen() == 0 ) {

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann die Variable nicht erkannt werden, da vor dem Einheitenquot <%s> keine Variable gefunden wurde !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,cQuotUnit0
                         );
        Status = NOT_OKAY;
        return Status;

    } else if( flag == 1 && pstruct->VariableName.getLen() == 0 ) {

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann die Variable nicht erkannt werden, da vor dem Attributzeichen <%s> keine Variable gefunden wurde !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,cTrennZeichAttr
                         );
        Status = NOT_OKAY;
        return Status;

    } else if( flag == 0 && pstruct->VariableName.getLen() == 0 ) {

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann die Variable nicht erkannt werden, da keine Variable gefunden wurde !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         );
        Status = NOT_OKAY;
        return Status;

    } else if( !pstruct->VariableName.isAlphNum() ) { //a-z oder A-Z oder 0-9

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> ist Variable <%s> nicht alphanumerisch !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,pstruct->VariableName.c_str()
                         );
        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
//#################################################################
//#################################################################
// getValue: Wert/Werte  finden
//#################################################################
//#################################################################
status_t CDsParFile::getValue(CSlfStr &tline,SDsParReadLine *pstruct) {

    CSlfStr txt;
    uint32_t  i0;
    pstruct->Values.clear();

    tline.elimAnfEndC();

#if DS_PAR_USE_MATFILELOAD == 1
	// Matlabfile für den Parameter
	//=============================
	if( tline.find(cAttrMatFileRead) == 0 ) {

		pstruct->FileType = IS_MAT_FILE;

		tline.cut(0,SlfStrLen(cAttrMatFileRead));
		tline.elimAnfEndC();

		//Anfang Parameterliste finden
		if( tline.find(cQuotFileBracket0) != 0 ) {

                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> wird beim Lesen des/der Werts/e <%s> gefunden, es gibt aber keine Klammer <%s> mit der die Parameter übergeben werden!!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,cAttrMatFileRead
                                 ,cQuotFileBracket0
                                 );
                Status = NOT_OKAY;
                return Status;
		}

		//Ende Parameterliste finden
		while(1) {

			// Ende ist da Parameter auslesen
			if( (i0=tline.find(cQuotFileBracket1,SlfStrLen(cQuotFileBracket0))) != SLF_STR_NPOS ) {

				CSlfStrV vstr;
				CSlfStrV vstr1;
    
				tline.findText(txt,cQuotFileBracket0,cQuotFileBracket1,"vs");
				tline.cut(0,i0+SlfStrLen(cQuotFileBracket1));

				txt.elimAnfEndC();

				SlfStrVSplit(vstr,txt.c_str(),cTrennZeich);

				if( vstr.getNrows() == 0 ) {

					ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> wird beim Lesen der matlab-File-PArameter kein Parameter zwischen <%s> u. <%s> gefunden!!!\n"
									 ,pstruct->Tline.c_str()
									 ,pstruct->ReadFile[pstruct->iRF].Zeile
									 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
									 ,cQuotFileBracket0
									 ,cQuotFileBracket1
									 );
					Status = NOT_OKAY;
					return Status;
				}

				for(uint32_t i=0;i<vstr.getNrows();i++) {

					SlfStrVSplit(vstr1,vstr.get_str(i),cZuweis);

					if( vstr1.getNrows() <= 1 ) {

						ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei:<%s> Beim Lesen der matlab-File-PArameter wird der %i-ste Parameter <%s> nicht richtig beschrieben (z.B. file=name )!!!\n"
										 ,pstruct->Tline.c_str()
										 ,pstruct->ReadFile[pstruct->iRF].Zeile
										 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
										 ,i+1
										 ,vstr.get_str(i)
										 );
						Status = NOT_OKAY;
						return Status;
					}
					txt = vstr1.get_str(0);
					txt.elimAnfEndC();

					// Dateiname
					if( txt == cAttrFileName ) {
						

						pstruct->FileName = vstr1.get_str(1);
						pstruct->FileName.elimAnfEndC();
					} else if( txt == cAttrFileVarName ) {

						pstruct->FileVarName = vstr1.get_str(1);
					    pstruct->FileVarName.elimAnfEndC();
					} else {

						ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: Beim Lesen der matlab-File-PArameter wird der %ste Parameter <%s> nicht richtig erkannt (%s,%s)!!!\n"
										 ,pstruct->Tline.c_str()
										 ,pstruct->ReadFile[pstruct->iRF].Zeile
										 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
										 ,i+1
										 ,vstr.get_str(i)
										 ,cAttrFileName
										 ,cAttrFileVarName
										 );
						Status = NOT_OKAY;
						return Status;
					}
				}
				//aus whileschleife springen
				break;
			// nächste Zeile lesen
			} else {
				
				
				txt.clear();
				if( readLine(txt,pstruct) == EOF ) {
				
					ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Endezeichen der Parameterliste <%s> nicht finden !!!\n"
									 ,pstruct->Tline.c_str()
									 ,pstruct->ReadFile[pstruct->iRF].Zeile
									 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
									 ,cQuotMat1
									 );
					Status = NOT_OKAY;
					return Status;
				}

				if( Status != OKAY )
					return Status;

				// Kommentar löschen
				//==================
				elimComment(txt);

				txt.elimAnfEndC();
				tline.cat(txt);
				pstruct->Tline.cat(txt);

				// Ende überprüfen
				for(uint8_t i=0;i<QuotGrLen;i++) {

					if(  tline.find(cQuot0Gr[i],"vs",cQuot0,cQuot1,"a",0) != SLF_STR_NPOS
					  || tline.find(cQuot1Gr[i],"vs",cQuot0,cQuot1,"a",0) != SLF_STR_NPOS
					  ) {

						ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> wird beim Lesen der matlab-File-Parameter ein Gruppenzeichen <%s>/<%s> gefunden, die Liste muß mit <%s> abgeschlossen werden !!!\n"
										 ,pstruct->Tline.c_str()
										 ,pstruct->ReadFile[pstruct->iRF].Zeile
										 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
										 ,cQuot0Gr[i]
										 ,cQuot0Gr[i]
										 ,cQuotFileBracket1
										 );
						Status = NOT_OKAY;
						return Status;
						
					}
				}
			}

		}

	// Wertereihe mit Quotzeichen []
	//==============================
    } else
#endif		
	if( tline.find(cQuotMat0) == 0 ) {

        while( 1 ) {
            //Ende muß da sein
            if( (i0=tline.find(cQuotMat1,SlfStrLen(cQuotMat0))) != SLF_STR_NPOS ) {
            
                tline.findText(txt,cQuotMat0,cQuotMat1,"vs");
                tline.cut(0,i0+SlfStrLen(cQuotMat1));
                return getBlankValue(txt,pstruct);

            //Es darf kein Zweisungszeichn auftreten, dann ist ein Fehler
            } else if(  tline.find(cZuweis,"vs",cQuot0,cQuot1,"a",0) != SLF_STR_NPOS
                     && tline.find(cZuweis,"vs",cQuota,cQuotb,"a",0) != SLF_STR_NPOS ) {
            
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> wird beim Lesen des/der Werts/e ein Zuweisungszeichen <%s> gefunden, die Werte müssen mit <%s> abgeschlossen werden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,cZuweis
                                 ,cQuotMat1
                                 );
                Status = NOT_OKAY;
                return Status;

            } else if( pstruct->ReadFlag == 0) {
                
                ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Endezeichen der Werzuweisung <%s> nicht gefunden !!!\n"
                                 ,pstruct->Tline.c_str()
                                 ,pstruct->ReadFile[pstruct->iRF].Zeile
                                 ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                                 ,cQuotMat1
                                 );
                Status = NOT_OKAY;
                return Status;

            } else { // Ansonsten weiter das Ende suchen lassen

                txt.clear();
                if( readLine(txt,pstruct) == EOF )
                    pstruct->ReadFlag = 0;

				if( Status != OKAY )
					return Status;

				// Kommentar löschen
				//==================
				elimComment(txt);

                txt.elimAnfEndC();
                tline.cat(txt);
                pstruct->Tline.cat(txt);
            }
        }
	//Wertereihe ohne Quotzeichen
	//===========================
    } else {

            return getBlankValue(tline,pstruct);
    }
            
    return Status;
}
//#################################################################
//#################################################################
// getBlankValue: Wert/Werte ohne Klammern  finden
//#################################################################
//#################################################################
status_t CDsParFile::getBlankValue(CSlfStr &tline,SDsParReadLine *pstruct) {

    CSlfStrV vecstr,vecstr1;
    CSlfStr  strv;
    uint32_t   i,j;
    uint32_t   nrows=0,ncols=0;

    tline.elimAnfEndC();

    SlfStrVSplit(vecstr,tline.c_str(),cTrennZeichCol);

    nrows = vecstr.getNrows();
    for(i=0;i<nrows;i++) {

        SlfStrVSplit(vecstr1,vecstr.get_str(i),cTrennZeich);
        ncols = MAX(ncols,vecstr1.getNrows());

        for(j=0;j<vecstr1.getNrows();j++) {

            strv = vecstr1.get_str(j);
            strv.elimAnfEndC();

            pstruct->Values.cpy(strv.c_str(),i,j);
        }
    }
    if( ncols > 1 && nrows == 1 )
        pstruct->Values.transpone();
    
    return Status;
}
//#################################################################
//#################################################################
// find1DTable: 1D-TAbelle auslesen
//#################################################################
//#################################################################
status_t CDsParFile::find1DTable(CSlfStr &txtl,SDsParReadLine *pstruct) {

    uint32_t i1;

    // Nach Endquotzeichen suchen
    if( (i1=txtl.find(cQuot11DTab,SlfStrLen(cQuot01DTab))) != SLF_STR_NPOS ){

        txtl.findText(pstruct->TableName,cQuot01DTab,cQuot11DTab,"vs");
        txtl.cut(0,i1+SlfStrLen(cQuot11DTab));
        txtl.elimAnfEndC();
        pstruct->TableName.elimAnfEndC();
    } else {

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Tabbellenendezeichen <%s> nicht erkannt werden !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,cQuot11DTab
                         );
        Status = NOT_OKAY;
        return Status;
    }


    return Status;
}
//#################################################################
//#################################################################
// get1DTableCopy: Tabellenzuweisung rechte Seite suchen
//#################################################################
//#################################################################
uint8_t CDsParFile::get1DTableCopy(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint32_t   i1;

    CSlfStr name;

    tline.elimAnfEndC();

    // zu kopierende Tabellenname suchen
    //==================================

    // Nach Endquotzeichen suchen
    if( (i1=tline.find(cQuot11DTab,SlfStrLen(cQuot01DTab))) != SLF_STR_NPOS ){

        tline.findText(pstruct->CopyTable,cQuot01DTab,cQuot11DTab,"vs");
        tline.cut(0,i1+SlfStrLen(cQuot11DTab));
        tline.elimAnfEndC();
        pstruct->CopyTable.elimAnfEndC();

        return 1;
    }

    return 0;
}
//#################################################################
//#################################################################
// find2DTable: 2D-TAbelle auslesen
//#################################################################
//#################################################################
status_t CDsParFile::find2DTable(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint32_t i1;

    // Nach Endquotzeichen suchen
    if( (i1=tline.find(cQuot12DTab,SlfStrLen(cQuot02DTab))) != SLF_STR_NPOS ){

        tline.findText(pstruct->TableName,cQuot02DTab,cQuot12DTab,"vs");
        tline.cut(0,i1+SlfStrLen(cQuot12DTab));
        tline.elimAnfEndC();
        pstruct->TableName.elimAnfEndC();
    } else {

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das Tabbellenendezeichen <%s> nicht erkannt werden !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,cQuot12DTab
                         );
        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
//#################################################################
//#################################################################
// get2DTableCopy: Tabellenzuweisung rechte Seite suchen
//#################################################################
//#################################################################
uint8_t CDsParFile::get2DTableCopy(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint32_t   i1;

    CSlfStr name;

    tline.elimAnfEndC();

    // zu kopierende Tabellenname suchen
    //==================================

    // Nach Endquotzeichen suchen
    if( (i1=tline.find(cQuot12DTab,SlfStrLen(cQuot02DTab))) != SLF_STR_NPOS ){

        tline.findText(pstruct->CopyTable,cQuot02DTab,cQuot12DTab,"vs");
        tline.cut(0,i1+SlfStrLen(cQuot12DTab));
        tline.elimAnfEndC();
        pstruct->CopyTable.elimAnfEndC();

        return 1;
    }

    return 0;
}
//#################################################################
//#################################################################
// findSpezTable: Spezial-TAbelle auslesen
//#################################################################
//#################################################################
status_t CDsParFile::findSpezTable(CSlfStr &tline,SDsParReadLine *pstruct) {

    uint32_t i1;

    // Nach Endquotzeichen suchen
    if( (i1=tline.find(cQuot1SpezTab,SlfStrLen(cQuot0SpezTab))) != SLF_STR_NPOS ){

        CSlfStr text;

        tline.findText(text,cQuot0SpezTab,cQuot1SpezTab,"vs");
        tline.cut(0,i1+SlfStrLen(cQuot1SpezTab));
        tline.elimAnfEndC();

        text.elimAnfEndC();
        text.change("\t"," ");
        text.change("  "," ");
        SlfStrVSplit(pstruct->SpezTableVar,text.c_str());


    } else {

        ErrText.catFormat("Im Kommando <%s> in Zeile <%i> aus Datei: <%s> kann das (spezial) Tabbellenendezeichen <%s> nicht erkannt werden !!!\n"
                         ,pstruct->Tline.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,cQuot1SpezTab
                         );
        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
//#################################################################
//#################################################################
// findSpezTableValue: Spezial-TAbelle Werte auslesen
//#################################################################
//#################################################################
status_t CDsParFile::findSpezTableValue(CSlfStr &tline,SDsParReadLine *pstruct) {

    CSlfStrV liste;
    uint32_t   i;

    tline.change("\t"," ");
    tline.change("  "," ");

    if( SlfStrFind(tline,cQuot0,"vs") == SLF_STR_NPOS )    
        SlfStrVSplit(liste,tline.c_str()," ",cQuota,cQuotb);
    else
        SlfStrVSplit(liste,tline.c_str()," ",cQuot0,cQuot1);

    pstruct->Values.clear();

    for(i=0;i<liste.getNrows();i++)
        pstruct->Values.cpy(liste.get_str(i),i,0);


    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setInternParStruct: gefundene Struktur einsortieren
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternParStruct(SDsParReadLine *pstruct) {

    //leere Struktur,weiter nächste Struktur
    if( pstruct->TypeRead == IS_EMPTY )
        return Status;

    // Instanz anlegen,wenn noch nicht angelegt und kein passende Struktur
    // kommt
    if(   pInstanz == 0                     // keine Instanz vorhanden
      &&  (   pstruct->Type != IS_GROUP     // keine Gruppe
          ||  (  pstruct->Type == IS_GROUP  // Gruppe,ab er nicht Instanz
              && pstruct->Hierach != 0
              )
          )
      ) 
        makeInternInstanz("");

    // Bei Tabelle prüfen
    if(  pstruct->Type == IS_1DTABLE 
      && pTabAct->VecSet < 2             // noch nicht fertig
      && (  (pstruct->TypeRead == IS_GROUP)
         || (pstruct->TypeRead == IS_GROUP_COPY)
         || (pstruct->TypeRead == IS_GROUP_COMMENT)
         || (pstruct->TypeRead == IS_2DTABLE)
         || (pstruct->TypeRead == IS_2DTABLE_COMMENT)
         || (pstruct->TypeRead == IS_2DTABLE_COPY)
         || (pstruct->TypeRead == IS_SPEZTABLE)
         )
      ) {
        ErrText.catFormat("CDsParFile::setInternParStruct: In Zeile <%i> Parameterdatei: <%s> ist 1D-Tabelle noch nicht beendet eingelsener Typ: <%s> !!\n"
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,DsParFileGetTypeName(pstruct->TypeRead));

        Status = NOT_OKAY;
        return Status;

    }
    // Prüfen ob die Reihenfolge 2D-Table stimmt
    if(  pstruct->Type == IS_2DTABLE 
      && pTabAct->VecSet < 3             // noch nicht fertig
      && (  (pstruct->TypeRead == IS_GROUP)
         || (pstruct->TypeRead == IS_GROUP_COPY)
         || (pstruct->TypeRead == IS_GROUP_COMMENT)
         || (pstruct->TypeRead == IS_1DTABLE)
         || (pstruct->TypeRead == IS_1DTABLE_COMMENT)
         || (pstruct->TypeRead == IS_1DTABLE_COPY)
         || (pstruct->TypeRead == IS_SPEZTABLE)
         )
      ) {
        ErrText.catFormat("CDsParFile::setInternParStruct: In Zeile <%i> Parameterdatei: <%s> ist 2D-Tabelle noch nicht beendet eingelsener Typ: <%s> !!\n"
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,DsParFileGetTypeName(pstruct->TypeRead));

        Status = NOT_OKAY;
        return Status;

    }

    switch(pstruct->TypeRead) {
    case IS_GROUP:

        if( setInternGroup(pstruct) != OKAY )
            return NOT_OKAY;
        break;
    case IS_GROUP_COPY:

        if( setInternGroupCopy(pstruct) != OKAY)
            return NOT_OKAY;
        break;
    case IS_GROUP_COMMENT:

        if( setInternGroupComment(pstruct) != OKAY)
            return NOT_OKAY;
        break;
    case IS_VARIABLE:

        if(  pstruct->Type == IS_1DTABLE
          && pTabAct->VecSet < 2
          ) {

            if( setIntern1DTable(pstruct) != OKAY)
                return NOT_OKAY;

        } else if(  pstruct->Type == IS_2DTABLE
                 && pTabAct->VecSet < 3
                 ) {

            if( setIntern2DTable(pstruct) != OKAY)
                return NOT_OKAY;
        } else {
            if( setInternVariable(pstruct) != OKAY)
                return NOT_OKAY;
        }
        break;
    case IS_VARIABLE_UNIT:
    case IS_VARIABLE_FACTOR:
    case IS_VARIABLE_OFFSET:
    case IS_VARIABLE_COMMENT:
    case IS_VARIABLE_COPY:
    case IS_VARIABLE_TRANSPOSE:

        if(  pstruct->Type == IS_1DTABLE ) {

			if(  pstruct->TypeRead == IS_VARIABLE_COMMENT
		      && pTabAct->Name == pstruct->VariableName
			  && (  pTabAct->VecSet == 0
			     || (  pTabAct->VecSet == 1
				    && pTabAct->XVec.Name != pstruct->VariableName
					)
				 || (  pTabAct->VecSet == 2
				    && pTabAct->XVec.Name != pstruct->VariableName
				    && pTabAct->YVec.Name != pstruct->VariableName
					)
				 )
			  ) {

				pstruct->TypeRead  = IS_1DTABLE_COMMENT;
				pstruct->TableName = pstruct->VariableName;
                if( setIntern1DTable(pstruct) != OKAY)
                    return NOT_OKAY;

			} else if(  pstruct->TypeRead == IS_VARIABLE_COPY
		             && pTabAct->Name == pstruct->VariableName
			         && (  pTabAct->VecSet == 0
			            || (  pTabAct->VecSet == 1
				           && pTabAct->XVec.Name != pstruct->VariableName
					       )
				        || (  pTabAct->VecSet == 2
				           && pTabAct->XVec.Name != pstruct->VariableName
				           && pTabAct->YVec.Name != pstruct->VariableName
					       )
				        )
			         ) {

				pstruct->TypeRead = IS_1DTABLE_COPY;
				pstruct->TableName = pstruct->VariableName;
                if( setIntern1DTable(pstruct) != OKAY)
                    return NOT_OKAY;

            } else if(  pTabAct->VecSet < 2 
                     || pTabAct->XVec.Name == pstruct->VariableName
                     || pTabAct->YVec.Name == pstruct->VariableName
                     ) {

                if( setIntern1DTable(pstruct) != OKAY)
                    return NOT_OKAY;
			} else {

				if( setInternVariable(pstruct) != OKAY)
					return NOT_OKAY;
			}
        } else if(  pstruct->Type == IS_2DTABLE ) { 

            if(  pstruct->TypeRead == IS_VARIABLE_COMMENT
		      && pTabAct->Name == pstruct->VariableName
			  && (  pTabAct->VecSet == 0
			     || (  pTabAct->VecSet == 1
				    && pTabAct->XVec.Name != pstruct->VariableName
					)
				 || (  pTabAct->VecSet == 2
				    && pTabAct->XVec.Name != pstruct->VariableName
				    && pTabAct->YVec.Name != pstruct->VariableName
					)
				 || (  pTabAct->VecSet == 3
				    && pTabAct->XVec.Name != pstruct->VariableName
				    && pTabAct->YVec.Name != pstruct->VariableName
				    && pTabAct->ZMat.Name != pstruct->VariableName
					)
				 )
			  ) {

				pstruct->TypeRead = IS_2DTABLE_COMMENT;
				pstruct->TableName = pstruct->VariableName;
                if( setIntern2DTable(pstruct) != OKAY)
                    return NOT_OKAY;

			} else if(  pstruct->TypeRead == IS_VARIABLE_COPY
		             && pTabAct->Name == pstruct->VariableName
			         && (  pTabAct->VecSet == 0
			            || (  pTabAct->VecSet == 1
				           && pTabAct->XVec.Name != pstruct->VariableName
					       )
				        || (  pTabAct->VecSet == 2
				           && pTabAct->XVec.Name != pstruct->VariableName
				           && pTabAct->YVec.Name != pstruct->VariableName
					       )
  					    || (  pTabAct->VecSet == 3
						   && pTabAct->XVec.Name != pstruct->VariableName
						   && pTabAct->YVec.Name != pstruct->VariableName
						   && pTabAct->ZMat.Name != pstruct->VariableName
						   )
				        )
			         ) {

				pstruct->TypeRead = IS_2DTABLE_COPY;
				pstruct->TableName = pstruct->VariableName;
                if( setIntern2DTable(pstruct) != OKAY)
                    return NOT_OKAY;

            } else if(  pTabAct->VecSet < 3 
                     || pTabAct->XVec.Name == pstruct->VariableName
                     || pTabAct->YVec.Name == pstruct->VariableName
                     || pTabAct->ZMat.Name == pstruct->VariableName
                     ) {

				if( isIntern2DTable(pstruct) != OKAY)
					return NOT_OKAY;
			} else {

				if( setInternVariable(pstruct) != OKAY)
					return NOT_OKAY;
			}
        } else {

            if( setInternVariable(pstruct) != OKAY)
                return NOT_OKAY;
        }
        break;
    case IS_1DTABLE:
    case IS_1DTABLE_COPY:
    case IS_1DTABLE_COMMENT:

        if( setIntern1DTable(pstruct) != OKAY)
            return NOT_OKAY;

        break;
    case IS_2DTABLE:
    case IS_2DTABLE_COPY:
    case IS_2DTABLE_COMMENT:

        if( setIntern2DTable(pstruct) != OKAY)
            return NOT_OKAY;

        break;
    case IS_SPEZTABLE:
    case IS_SPEZTABLE_VALUE:

        if( setInternSpezTable(pstruct) != OKAY)
            return NOT_OKAY;

    default:
        ErrText.catFormat("CDsParFile::setInternParStruct: Programmierfehler der Typ: <%s> ist nicht bekannt !!\n"
                         ,DsParFileGetTypeName(pstruct->Type));

        Status = NOT_OKAY;
        return Status;
    }
    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// makeInternInstanz: neue Instanz anlegen,ohne Namen,dann Nummer
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void CDsParFile::makeInternInstanz(char *name) {

    uint32_t counter=0;
    SDsParFileGroup *pi = pInstanz;

    while( pi ) {

        counter++;
        pi = pi->pNext;

    }

    pi = new SDsParFileGroup;

    if( SlfStrLen(name) == 0 )
        pi->Name.catFormat("%d",counter);
    else
        pi->Name = name;

    pi->Comment = "";
    pi->Hierach = 0;
    pi->NSub    = 0;
    pi->NVar    = 0;
    pi->NTab    = 0;
    pi->pSub    = 0;
    pi->pVar    = 0;
    pi->pTab    = 0;
    pi->CopyFlag= 0;

    pi->pNext   = pInstanz;
    pInstanz    = pi;

    setInternGroupListe(0,pi);
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternGroupListe: pListe von istart an resetten und setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void CDsParFile::setInternGroupListe(uint8_t istart,SDsParFileGroup *pi) {

    for(uint8_t i=istart;i<QuotGrLen;i++)
        pListe[i]=0;

    pListe[istart] = pi;
    iHierAct       = istart;
    pGroupAct      = pi;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternGroup: Gruppe setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternGroup(SDsParReadLine *pstruct) {

    SDsParFileGroup *pg;
    uint8_t           found_flag;

    // Prüfen, ob Hierachie stimmt
    if( pstruct->Hierach > (iHierAct+1) ) {
        ErrText.catFormat("CDsParFile::setInternGroup: In Datei <%s> in Zeile <%i> hat die Hierchie <%i,name:%s> keine übergeordnete Hierachie!!\n"
                         ,pstruct->ReadFile[pstruct->iRF].ParFile.c_str()
                         ,pstruct->ReadFile[pstruct->iRF].Zeile
                         ,pstruct->Hierach
                         ,pstruct->GroupName.c_str());

        Status = NOT_OKAY;
        return Status;
    }

    //neue Instanz
    if( pstruct->Hierach == 0 ){

        makeInternInstanz(pstruct->GroupName.c_str());
    } else {

        pg = pListe[pstruct->Hierach-1]->pSub;

        found_flag = 0;
        while(pg) {

            if( pg->Name == pstruct->GroupName ) {

                found_flag = 1;
                setInternGroupListe(pstruct->Hierach,pg);
                break;
            }
            pg = pg->pNext;
        }
        if( !found_flag ) {

            pg = new SDsParFileGroup;

            pg->Name = pstruct->GroupName;
            pg->Comment = "";
            pg->Hierach = pstruct->Hierach;
            pg->NSub    = 0;
            pg->NVar    = 0;
            pg->NTab    = 0;
            pg->pSub    = 0;
            pg->pVar    = 0;
            pg->pTab    = 0;
            pg->CopyFlag= 0;

            pg->pNext   = pListe[pstruct->Hierach-1]->pSub;
            pListe[pstruct->Hierach-1]->pSub    = pg;

            pListe[pstruct->Hierach-1]->NSub    += 1;

            setInternGroupListe(pstruct->Hierach,pg);

        }

    }

    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternGroupCopy: GruppeKopie setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternGroupCopy(SDsParReadLine *pstruct) {

    SDsParFileGroup *pg;
    uint32_t ngc;
    uint32_t i;

    if( setInternGroup(pstruct) != OKAY )
        return NOT_OKAY;

    //Länge des Kopiergruppenhierachievektors
    ngc = pstruct->CopyGroup.getNrows();

    pg = pListe[iHierAct];

    pg->CopyGroup.clear();

    // nichtvollstädige CopyGroup-Liste mit aktueller Liste auffüllen
    for(i=0;i<pstruct->CopyHierach - (ngc-1);i++)
        pg->CopyGroup.append(pListe[i]->Name);

    for(i=0;i<ngc;i++)
        pg->CopyGroup.append(pstruct->CopyGroup.get_str(i));

    pg->CopyFlag = 1;

    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternGroupComment: GruppeKommentar setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternGroupComment(SDsParReadLine *pstruct) {

    SDsParFileGroup *pg;

    if( setInternGroup(pstruct) != OKAY )
        return NOT_OKAY;

    pg = pListe[iHierAct];

    pg->Comment = pstruct->Comment;

    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternVariable: Variable mit allem drumund drann setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternVariable(SDsParReadLine *pstruct) {

    uint32_t i,j;
    //Struktur suchen oder setzen
    if( pstruct->Type   == IS_1DTABLE ) {

        if(pTabAct->VecSet == 0) {

            pVarAct         = &pTabAct->XVec;
            pVarAct->Name   = pstruct->VariableName;
            pTabAct->VecSet = 1;

        } else if(  pTabAct->VecSet == 1
                 && pTabAct->XVec.Name == pstruct->VariableName
                 ) {

            pVarAct         = &pTabAct->XVec;
        
        } else if(  pTabAct->VecSet == 1 
                 ) {

            pVarAct         = &pTabAct->YVec;
            pVarAct->Name   = pstruct->VariableName;
            pTabAct->VecSet = 2;

        } else if(  pTabAct->VecSet == 2
                 && pTabAct->XVec.Name == pstruct->VariableName
                 ) {
        
            pVarAct         = &pTabAct->XVec;

        } else if(  pTabAct->VecSet == 2
                 && pTabAct->YVec.Name == pstruct->VariableName
                 ) {
            
            pVarAct         = &pTabAct->YVec;
        
        } else {
            pstruct->Type = IS_VARIABLE;
            if( setInternVariableStruct(pstruct) != OKAY )
                return NOT_OKAY;
        }
    } else if( pstruct->Type   == IS_2DTABLE ) {

        if(pTabAct->VecSet == 0) {

            pVarAct         = &pTabAct->XVec;
            pVarAct->Name   = pstruct->VariableName;
            pTabAct->VecSet = 1;

        } else if(  pTabAct->VecSet == 1
                 && pTabAct->XVec.Name == pstruct->VariableName
                 ) {

            pVarAct         = &pTabAct->XVec;
        
        } else if(  pTabAct->VecSet == 1 
                 ) {

            pVarAct         = &pTabAct->YVec;
            pVarAct->Name   = pstruct->VariableName;
            pTabAct->VecSet = 2;

        } else if(  pTabAct->VecSet == 2
                 && pTabAct->XVec.Name == pstruct->VariableName
                 ) {
        
            pVarAct         = &pTabAct->XVec;

        } else if(  pTabAct->VecSet == 2
                 && pTabAct->YVec.Name == pstruct->VariableName
                 ) {
            
            pVarAct         = &pTabAct->YVec;
        } else if(  pTabAct->VecSet == 2 
                 ) {

            pVarAct         = &pTabAct->ZMat;
            pVarAct->Name   = pstruct->VariableName;
            pTabAct->VecSet = 3;

        } else if(  pTabAct->VecSet == 3
                 && pTabAct->XVec.Name == pstruct->VariableName
                 ) {
        
            pVarAct         = &pTabAct->XVec;

        } else if(  pTabAct->VecSet == 3
                 && pTabAct->YVec.Name == pstruct->VariableName
                 ) {
            
            pVarAct         = &pTabAct->YVec;
        
        } else if(  pTabAct->VecSet == 3
                 && pTabAct->ZMat.Name == pstruct->VariableName
                 ) {
            
            pVarAct         = &pTabAct->ZMat;
        
        } else {
            pstruct->Type = IS_VARIABLE;
            if( setInternVariableStruct(pstruct) != OKAY )
                return NOT_OKAY;
        }
    } else {
        
        pstruct->Type = IS_VARIABLE;
        if( setInternVariableStruct(pstruct) != OKAY )
        return NOT_OKAY;
    }


    switch( pstruct->TypeRead ) {
    case IS_VARIABLE:

		// Matlabfile festlegen
		if( pstruct->FileType == IS_MAT_FILE ) {

			if( pstruct->FileName.getLen() == 0 ) {

				ErrText.catFormat("CDsParFile::setInternVariable: Zu Variable <%s> in Zeile <%i> kann kein Dateiname für matread gefunden werden\n"
								 ,pstruct->VariableName.c_str()
								 ,pstruct->ReadFile[pstruct->iRF].Zeile
								 );
				Status = NOT_OKAY;
				return Status;
			}
			pVarAct->FileName = pstruct->FileName;
			
			if( pstruct->FileVarName.getLen() > 0 )

				pVarAct->FileVarName = pstruct->FileVarName;
			else
				pVarAct->FileVarName = pVarAct->Name;

			// falls andere Typen schon gelesen, dann zurücksetzen
			if( pVarAct->Type == DEF_STRINGM ) {
				pVarAct->Type     = DEF_VOID;
				pVarAct->StrMVal.clear();
			}

			// jetzt Filetype
			pVarAct->FileType = IS_MAT_FILE;
			
		} else {

			pVarAct->NRow = pstruct->Values.getNrows();
			pVarAct->NCol = pstruct->Values.getNcols();
			pVarAct->NVal = pVarAct->NRow*pVarAct->NCol;

			// Wert setzen
			pVarAct->StrMVal.clear();
			for(i=0;i<pstruct->Values.getNrows();i++)
				for(j=0;j<pstruct->Values.getNcols();j++)
					pVarAct->StrMVal.cpy(pstruct->Values.get_str(i,j),i,j);

			// falls andere Typen bestehen, wieder löschen
		    if( pVarAct->FileType != IS_NON_FILE )
				pVarAct->FileType = IS_NON_FILE;

			//Typ
			pVarAct->Type = DEF_STRINGM;


		}
        //Unit
        if( pstruct->Unit.getLen() > 0 ) {
            pVarAct->Unit = pstruct->Unit;
        }
        break;


    case IS_VARIABLE_UNIT:

        //Unit
        if( pstruct->Unit.getLen() > 0 ) {
            pVarAct->Unit = pstruct->Unit;
        }
        break;
    case IS_VARIABLE_FACTOR:

        //Factor
        if( SlfStr2Num(pstruct->Factor.c_str(),&pVarAct->Factor) != NO_ERR ) {

            ErrText.catFormat("CDsParFile::setInternVariableFactor: Zu Variable <%s> in Zeile <%i> konnte der Faktor <%s> nicht von string in double gewandelt werden\n"
                             ,pVarAct->Name.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->Factor.c_str()
                             );
            Status = NOT_OKAY;
            return Status;
        }
        pVarAct->FSet = 1;
        break;
    case IS_VARIABLE_OFFSET:

        if( SlfStr2Num(pstruct->Offset.c_str(),&pVarAct->Offset) != NO_ERR ) {

            ErrText.catFormat("CDsParFile::setInternVariableOffset: Zu Variable <%s> in Zeile <%i> konnte der Offset <%s> nicht von string in double gewandelt werden\n"
                             ,pVarAct->Name.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->Offset.c_str()
                             );
            Status = NOT_OKAY;
            return Status;


        }
        pVarAct->OSet = 1;
        break;
    case IS_VARIABLE_TRANSPOSE:

        if( SlfStr2Num(pstruct->Transpose.c_str(),&pVarAct->Transpose) != NO_ERR ) {

            ErrText.catFormat("CDsParFile::setInternVariableTranspose: Zu Variable <%s> in Zeile <%i> konnte Transpose <%s> nicht von string in int gewandelt werden\n"
                             ,pVarAct->Name.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->Transpose.c_str()
                             );
            Status = NOT_OKAY;
            return Status;


        }
        pVarAct->TSet = 1;
        break;
    case IS_VARIABLE_COMMENT:

        //Comment
        pVarAct->Comment = pstruct->Comment;
        break;
    case IS_VARIABLE_COPY:

        {
        SDsParFileGroup *pg;
        uint32_t ngc;
        uint32_t i;


        //CopyVariable
        pVarAct->CopyVariable = pstruct->CopyVariable;

        //CopyGroup
        //Länge des Kopiergruppenhierachievektors
        ngc = pstruct->CopyGroup.getNrows();

        pg = pListe[iHierAct];

        pVarAct->CopyGroup.clear();

        if( ngc == 0 ) { //keine Gruppe angegeben
            for(i=0;i<iHierAct+1;i++)
                pVarAct->CopyGroup.append(pListe[i]->Name);
        } else {
            // nichtvollstädige CopyGroup-Liste mit aktueller Liste auffüllen
            for(i=0;i<pstruct->CopyHierach - (ngc-1);i++)
                pVarAct->CopyGroup.append(pListe[i]->Name);

            for(i=0;i<ngc;i++)
                pVarAct->CopyGroup.append(pstruct->CopyGroup.get_str(i));
        }
        //CopyFlag
        pVarAct->CopyFlag     = 1;
        }
        break;
    }
    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternVariableStruct: Variable setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternVariableStruct(SDsParReadLine *pstruct) {

    SDsParFileGroup    *pg = pListe[iHierAct];
    SDsParFileVariable *pv = pg->pVar;

    uint8_t              found_flag;

    //nach Variablennamen suchen oder anlegen
    found_flag = 0;
    while(pv) {

        if( pv->Name == pstruct->VariableName ) {

            found_flag = 1;
            pVarAct    = pv;
            break;
        }
        pv = pv->pNext;
    }
    if( !found_flag ) {

        pv = new SDsParFileVariable;

        pv->Name      = pstruct->VariableName;
        pv->Factor    = 1.0;
        pv->FSet      = 0;
        pv->Offset    = 0.0;
        pv->OSet      = 0;
        pv->Transpose = 0;
        pv->TSet      = 0;
        pv->ILine     = -1;
        pv->NCol      = 0;
        pv->NRow      = 0;
        pv->NVal      = 0;
        pv->Type      = DEF_VOID;
		pv->Vec       = 0;
		pv->Mat       = 0;

        pv->CopyFlag= 0;

		pv->FileType  = IS_NON_FILE;

        pv->pNext   = pg->pVar;
        pg->pVar    = pv;

        pg->NVar    += 1;

        pVarAct     = pv;
    }

    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setIntern1DTable: 1D-Tabelle setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setIntern1DTable(SDsParReadLine *pstruct) {


    switch(pstruct->TypeRead) {

    case IS_1DTABLE:
    case IS_1DTABLE_COPY:
    case IS_1DTABLE_COMMENT:

        // Wenn bereits Tabelle erstellt, muß geprüft werden,
        // ob es die richtige ist
        if(  pstruct->Type == IS_1DTABLE
          && pTabAct->VecSet < 2
          && pstruct->TableName != pTabAct->Name
          ) {

            ErrText.catFormat("CDsParFile::setIntern1DTable: Die Tabelle <%s> ist nicht vollständig beschrieben, wenn in Zeile <%i> eine neue Tabelle <%s> erstellt werden soll\n"
                             ,pTabAct->Name.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->TableName.c_str()
                             );
            Status = NOT_OKAY;
            return Status;
        } 
#if 0
        if( !pTabAct )
            pstruct->Type = IS_1DTABLE;

        else if( pstruct->TableName != pTabAct->Name )
            pstruct->Type = IS_1DTABLE;
#endif   
       
        //Struktur suchen oder setzen
        if( setInternTableStruct(pstruct) != OKAY )
            return NOT_OKAY;

        if( pTabAct->VecSet < 2 )
            pstruct->Type = IS_1DTABLE;

        
        pTabAct->Type = DEF_1D_TAB;
        if( pstruct->TypeRead == IS_1DTABLE_COPY ) {

            SDsParFileGroup *pg;
            uint32_t ngc;
            uint32_t i;


            //CopyTable
            pTabAct->CopyTable = pstruct->CopyTable;

            //CopyGroup
            //Länge des Kopiergruppenhierachievektors
            ngc = pstruct->CopyGroup.getNrows();

            pg = pListe[iHierAct];

            pTabAct->CopyGroup.clear();

            if( ngc == 0 ) { //keine Gruppe angegeben
                for(i=0;i<iHierAct+1;i++)
                    pTabAct->CopyGroup.append(pListe[i]->Name);
            } else {
                // nichtvollstädige CopyGroup-Liste mit aktueller Liste auffüllen
                for(i=0;i<pstruct->CopyHierach - (ngc-1);i++)
                    pTabAct->CopyGroup.append(pListe[i]->Name);

                for(i=0;i<ngc;i++)
                    pTabAct->CopyGroup.append(pstruct->CopyGroup.get_str(i));
            }
            //CopyFlag
            pTabAct->CopyFlag     = 1;
 
        } else if( pstruct->TypeRead == IS_1DTABLE_COMMENT ) {

            
            //Comment
            pTabAct->Comment = pstruct->Comment;
            
        }
        break;
    case IS_VARIABLE:
    case IS_VARIABLE_UNIT:
    case IS_VARIABLE_FACTOR:
    case IS_VARIABLE_OFFSET:
    case IS_VARIABLE_COMMENT:
    case IS_VARIABLE_COPY:
    case IS_VARIABLE_TRANSPOSE:

        // Variable setzen
        if( setInternVariable(pstruct) != OKAY ) {
            return NOT_OKAY;
        }
        break;

    }
    return OKAY;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setIntern2DTable: 2D-Tabelle setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setIntern2DTable(SDsParReadLine *pstruct) {

    switch(pstruct->TypeRead) {

    case IS_2DTABLE:
    case IS_2DTABLE_COPY:
    case IS_2DTABLE_COMMENT:

        // Wenn bereits Tabelle erstellt, muß geprüft werden,
        // ob es die richtige ist
        if(  pstruct->Type == IS_2DTABLE
          && pTabAct->VecSet < 3
          && pstruct->TableName != pTabAct->Name
          ) {

            ErrText.catFormat("CDsParFile::setIntern2DTable: Die Tabelle <%s> ist nicht vollständig beschrieben, wenn in Zeile <%i> eine neue Tabelle <%s> erstellt werden soll\n"
                             ,pTabAct->Name.c_str()
                             ,pstruct->ReadFile[pstruct->iRF].Zeile
                             ,pstruct->TableName.c_str()
                             );
            Status = NOT_OKAY;
            return Status;
        } 
#if 0
        if( !pTabAct )
            pstruct->Type = IS_2DTABLE;

        else if( pstruct->TableName != pTabAct->Name )
            pstruct->Type = IS_2DTABLE;
        
#endif  
        //Struktur suchen oder setzen
        if( setInternTableStruct(pstruct) != OKAY )
            return NOT_OKAY;

        if( pTabAct->VecSet < 3 )
            pstruct->Type = IS_1DTABLE;
        
        pTabAct->Type = DEF_2D_TAB;
        if( pstruct->TypeRead == IS_2DTABLE_COPY ) {

            SDsParFileGroup *pg;
            uint32_t ngc;
            uint32_t i;


            //CopyVariable
            pTabAct->CopyTable = pstruct->CopyTable;

            //CopyGroup
            //Länge des Kopiergruppenhierachievektors
            ngc = pstruct->CopyGroup.getNrows();

            pg = pListe[iHierAct];

            pTabAct->CopyGroup.clear();

            if( ngc == 0 ) { //keine Gruppe angegeben
                for(i=0;i<iHierAct+1;i++)
                    pTabAct->CopyGroup.append(pListe[i]->Name);
            } else {
                // nichtvollstädige CopyGroup-Liste mit aktueller Liste auffüllen
                for(i=0;i<pstruct->CopyHierach - (ngc-1);i++)
                    pTabAct->CopyGroup.append(pListe[i]->Name);

                for(i=0;i<ngc;i++)
                    pTabAct->CopyGroup.append(pstruct->CopyGroup.get_str(i));
            }
            //CopyFlag
            pTabAct->CopyFlag     = 1;
 
        } else if( pstruct->TypeRead == IS_2DTABLE_COMMENT ) {

            
            //Comment
            pTabAct->Comment = pstruct->Comment;
            
        }
        break;
    case IS_VARIABLE:
    case IS_VARIABLE_UNIT:
    case IS_VARIABLE_FACTOR:
    case IS_VARIABLE_OFFSET:
    case IS_VARIABLE_COMMENT:
    case IS_VARIABLE_COPY:
    case IS_VARIABLE_TRANSPOSE:

        // Variable setzen
        if( setInternVariable(pstruct) != OKAY ) {
            return NOT_OKAY;
        }
        break;

    }
    return OKAY;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternSpezTable: Spez-Tabelle setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternSpezTable(SDsParReadLine *pstruct) {

    uint32_t i;

    switch(pstruct->TypeRead) {

    case IS_SPEZTABLE:

        pstruct->Type = IS_SPEZTABLE;

        if( pstruct->SpezTableVar.getNrows() > DS_PAR_FILE_MAX_VAR_ACT_LIST ) {

            ErrText.catFormat("CDsParFile::setInternSpezTable: Vectorlength of pVarActList[%li] to small !!!\n"
							 ,DS_PAR_FILE_MAX_VAR_ACT_LIST);
        
			Status = NOT_OKAY;
			return Status;

        }
        // Aus der Tabelle werden n Variablen gesetzt
        for(i=0;i<pstruct->SpezTableVar.getNrows();i++) {

            pstruct->VariableName = pstruct->SpezTableVar.get_str(i);

            //Struktur suchen oder setzen
            if( setInternVariableStruct(pstruct) != OKAY )
                return NOT_OKAY;
            
            pVarActList[i] = pVarAct;
        }
 
        break;
    case IS_SPEZTABLE_VALUE:

        // Aus der Tabelle werden n Variablen gesetzt
        for(i=0;i<pstruct->SpezTableVar.getNrows();i++) {

            pVarAct = pVarActList[i];

            if( i < pstruct->Values.getNrows() ) {

                pVarAct->NRow += 1;
                pVarAct->NCol = 1;
			    pVarAct->NVal = pVarAct->NRow*pVarAct->NCol;

                pVarAct->StrMVal.cpy(pstruct->Values.get_str(i,0),i,0);

                pVarAct->FileType = IS_NON_FILE;


			    //Typ
			    pVarAct->Type = DEF_STRINGM;

            }
        }

        break;

    }
    return OKAY;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//isIntern1DTable: Prüfen ob Variable zu 1D-Tabelle gehört
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
uint8_t CDsParFile::isIntern1DTable(SDsParReadLine *pstruct) {

    if( pTabAct->XVec.Name == pstruct->VariableName )
        return 1;
    else if( pTabAct->YVec.Name == pstruct->VariableName )
        return 1;
    return 0;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//isIntern2DTable: Prüfen ob Variable zu 2D-Tabelle gehört
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
uint8_t CDsParFile::isIntern2DTable(SDsParReadLine *pstruct) {

    if( pTabAct->XVec.Name == pstruct->VariableName )
        return 1;
    else if( pTabAct->YVec.Name == pstruct->VariableName )
        return 1;
    else if( pTabAct->ZMat.Name == pstruct->VariableName )
        return 1;
    return 0;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//setInternTableStruct: Variable setzen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
status_t CDsParFile::setInternTableStruct(SDsParReadLine *pstruct) {

    SDsParFileGroup    *pg = pListe[iHierAct];
    SDsParFileTable    *pt = pg->pTab;

    uint8_t              found_flag;

    //nach Variablennamen suchen oder anlegen
    found_flag = 0;
    while(pt) {

        if( pt->Name == pstruct->TableName ) {

            found_flag = 1;
            pTabAct    = pt;
            break;
        }
        pt = pt->pNext;
    }
    if( !found_flag ) {

        pt = new SDsParFileTable;

        pt->Name    = pstruct->TableName;
        pt->ILine   = -1;
        pt->Type    = DEF_VOID;
        pt->CopyFlag= 0;
        pt->Order    = 1; // default
        pt->OrderSet = 0;
        
        pt->VecSet   = 0;
        pt->XVec.CopyFlag = 0;
        pt->XVec.Factor   = 1.0;
        pt->XVec.FSet     = 0;
        pt->XVec.Offset   = 0.0;
        pt->XVec.OSet     = 0;
        pt->XVec.Transpose= 0;
        pt->XVec.TSet     = 0;
        pt->XVec.ILine    = -1;
        pt->XVec.NCol     = 0;
        pt->XVec.NRow     = 0;
        pt->XVec.NVal     = 0;
		pt->XVec.Vec      = 0;
		pt->XVec.Mat      = 0;
        pt->XVec.Type     = DEF_VOID;
		pt->XVec.FileType = IS_NON_FILE;

        pt->YVec.CopyFlag = 0;
        pt->YVec.Factor   = 1.0;
        pt->YVec.FSet     = 0;
        pt->YVec.Offset   = 0.0;
        pt->YVec.OSet     = 0;
        pt->YVec.Transpose= 0;
        pt->YVec.TSet     = 0;
        pt->YVec.ILine    = -1;
        pt->YVec.NCol     = 0;
        pt->YVec.NRow     = 0;
        pt->YVec.NVal     = 0;
		pt->YVec.Vec      = 0;
		pt->YVec.Mat      = 0;
        pt->YVec.Type     = DEF_VOID;
		pt->YVec.FileType = IS_NON_FILE;
        
        pt->ZMat.CopyFlag = 0;
        pt->ZMat.Factor   = 1.0;
        pt->ZMat.FSet     = 0;
        pt->ZMat.Offset   = 0.0;
        pt->ZMat.OSet     = 0;
        pt->ZMat.Transpose= 0;
        pt->ZMat.TSet     = 0;
        pt->ZMat.ILine    = -1;
        pt->ZMat.NCol     = 0;
        pt->ZMat.NRow     = 0;
        pt->ZMat.NVal     = 0;
		pt->ZMat.Vec      = 0;
		pt->ZMat.Mat      = 0;
        pt->ZMat.Type     = DEF_VOID;
		pt->ZMat.FileType = IS_NON_FILE;

        pt->pNext   = pg->pTab;
        pg->pTab    = pt;

        pg->NTab    += 1;

        pTabAct     = pt;
    }

    return Status;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//findInternGroup: Gruppenhierachie in interner Liste finden
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SDsParFileGroup *CDsParFile::findInternGroup(CSlfStrV &groupV) {

    uint32_t ng = groupV.getNrows();
    uint32_t ih = 0;

    SDsParFileGroup *pg = pInstanz;

    for(ih=0;ih<ng;ih++) {

        while(pg) {

            if( pg->Name == groupV.get_str(ih) ) {

                if( ih == ng-1 ) // gefunden
                    return pg;
                else             // nächste Untergruppe
                    pg = pg->pSub;
                break;
            }
            pg = pg->pNext;

        }
        if( !pg )
            return 0;
    }
    return 0;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//findInternVariable: Variable in interner Liste von pg finden
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SDsParFileVariable *CDsParFile::findInternVariable(SDsParFileGroup *pg
                                               ,CSlfStr &varname) {

    SDsParFileVariable *pv = pg->pVar;

    while(pv) {

        if( pv->Name == varname )
            return pv;

        pv = pv->pNext;
    }
    return 0;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//findInternTable: Tabelle in interner Liste von pg finden
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SDsParFileTable *CDsParFile::findInternTable(SDsParFileGroup *pg
                                               ,CSlfStr &tabname) {

    SDsParFileTable *pt = pg->pTab;

    while(pt) {

        if( pt->Name == tabname )
            return pt;

        pt = pt->pNext;
    }
    return 0;
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//deleteInternGroup: gesamte Liste löschen
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void CDsParFile::deleteInternGroup(SDsParFileGroup *pSub) {

    SDsParFileGroup *pg;

    while(pSub) {

        pg = pSub->pNext;

        if( pSub->NSub )
            deleteInternGroup(pSub->pSub);

        if( pSub->NVar )
            deleteInternVariable(pSub->pVar);

        if( pSub->NTab )
            deleteInternTable(pSub->pTab);

        delete pSub;

        pSub = pg;
    }
}
void CDsParFile::deleteInternVariable(SDsParFileVariable *pVar) {

    SDsParFileVariable *pv;

    while(pVar) {

        pv = pVar->pNext;

        delete pVar;

        pVar = pv;
    }
}
void CDsParFile::deleteInternTable(SDsParFileTable *pTab) {

    SDsParFileTable *pt;

    while(pTab) {

        pt = pTab->pNext;

        delete pTab;

        pTab = pt;
    }
}
#if DS_PAR_USE_MATFILELOAD == 1
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// readExternPar: wertet interne Struktur nach externen Daten aus und 
// liest externe Daten ein
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
status_t CDsParFile::readExternPar(void) {

    SDsParFileGroup   *pi = pInstanz;
	


	if( findandsetExternFileList(pi) != OKAY ) {

		Status = NOT_OKAY;
		return Status;
	}
	if( readExternFileList() != OKAY ) {

		Status = NOT_OKAY;
		return Status;
	}

	return Status;
}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// readExternParGroupList: wertet interne Struktur nach externen Daten aus und 
// liest externe Daten ein
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
status_t CDsParFile::findandsetExternFileList(SDsParFileGroup *pg) {

	SDsParFileVariable   *pv;
	SDsParFileTable      *pt;
	SDsParFileGroup      *psg;

    while(pg) {

        //Variablen durchsuchen
        //=====================
		pv = pg->pVar;
		while( pv ) {


			if( pv->FileType != IS_NON_FILE ) {

				if( setExternFileList(pv) != OKAY ) {
					Status = NOT_OKAY;
					return NOT_OKAY;
				}
			}
			pv = pv->pNext;
		}
        //Tabellen durchsuchen
        //=====================
		pt = pg->pTab;
		while( pt ) {

			if( pt->XVec.FileType != IS_NON_FILE ) {

				if( setExternFileList(&pt->XVec) != OKAY ) {
					Status = NOT_OKAY;
					return NOT_OKAY;
				}
			}
			if( pt->YVec.FileType != IS_NON_FILE ) {

				if( setExternFileList(&pt->YVec) != OKAY ) {
					Status = NOT_OKAY;
					return NOT_OKAY;
				}
			}
			if( pt->ZMat.FileType != IS_NON_FILE ) {

				if( setExternFileList(&pt->ZMat) != OKAY ) {
					Status = NOT_OKAY;
					return NOT_OKAY;
				}
			}
			pt = pt->pNext;
		}
        //Untergruppen durchsuchen
        //===================
		psg = pg->pSub;
		while(psg) {

			if( findandsetExternFileList(psg) != OKAY ) {

				Status = NOT_OKAY;
				return NOT_OKAY;
			}
			psg = psg->pNext;
		}

        //nächste Gruppe
        //===============
        pg = pg->pNext;
    }
	return Status;
}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// setExternFileList: wertet interne Struktur nach externen Daten aus und 
// liest externe Daten ein
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
status_t CDsParFile::setExternFileList(SDsParFileVariable *pv) {

	SDsExternFileList *pf=pExternFileList;
	SDsExternFileVarList *pev;
	uint8_t             flag = 0;

	// File suchen
	while(pf) {

		if( pf->FileName == pv->FileName && pf->FileType == pv->FileType) {
			flag = 1;
			break;
		}
	}
	if( !flag ) {

		SDsExternFileList *pn = new SDsExternFileList;

		pn->FileName = pv->FileName;
		pn->FileType = pv->FileType;
		pn->NVar     = 0;
		pn->pVar     = 0;

		pn->pNext    = pExternFileList;
		pExternFileList = pn;
		pf              = pn;
	}

	flag = 0;
	// Variable suche
	pev = pf->pVar;
	while(pev) {

		if( pev->Name == pv->FileVarName && pev->pVar == pv) {

			flag = 1;
			break;
		}
		pev = pev->pNext;
	}
	if( !flag ) {

		SDsExternFileVarList *pnew = new SDsExternFileVarList;

		pnew->Name  = pv->FileVarName;
		pnew->pVar  = pv;
		pnew->isRead= 0;
		pnew->pNext = pf->pVar;
		pf->pVar    = pnew;
		pev         = pnew;
	}


	return Status;
}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// deleteExternFileList: löschen
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
void CDsParFile::deleteExternFileList() {

	SDsExternFileList    *pf;
	SDsExternFileVarList *pfv;

	while(pExternFileList) {

		while(pExternFileList->pVar) {
		
			pfv = pExternFileList->pVar->pNext;

			delete pExternFileList->pVar;

			pExternFileList->pVar = pfv;
		}

		pf = pExternFileList->pNext;

		delete pExternFileList;

		pExternFileList = pf;
	}

}
#endif
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParData: wertet interne Struktur aus und übergibt an ParData
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParData(CDsParData &par_data) {

    SDsParFileGroup *pi = pInstanz;

    CSlfStr         instanz;
    CSlfStrV        groupV;


    pi = pInstanz;
    while(pi) {

        //Gruppenhierachieliste
        groupV.clear();
        instanz = pi->Name;

#if 0
        //Gruppe kopieren
        //===============
        if(pi->CopyFlag) {
            if( setParDataCopyGroup(pi,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }
#endif
        //Variablen setzen
        //================
        if( pi->NVar > 0 ) {
            if( setParDataVariable(pi->pVar,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }

        //Tabelle setzen
        //================
        if( pi->NTab > 0 ) {
            if( setParDataTable(pi->pTab,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }

        //Untergruppen setzen
        //===================
        if( pi->NSub > 0 ) {
            if( setParDataSubGroup(pi->pSub,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }


        //nächste Instanz
        //===============
        pi = pi->pNext;
    }
        

    return OKAY;
}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataSubGroup: Auswerten der nächsten Subgroup
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataCopyGroup(SDsParFileGroup *pg
                                        ,CSlfStr         &instanz
                                        ,CSlfStrV        &groupV
                                        ,CDsParData      &par_data) {

    SDsParFileGroup    *pgc;

    // Suche die zu kopierende Gruppe
    //===============================
    if( (pgc = findInternGroup(pg->CopyGroup)) == 0 ) { // nicht gefunden

        CSlfStr grouph;
        CSlfStr grouphcopy;

        grouph.cat(instanz);
        makeGroupName(groupV,1,grouph);
        makeGroupName(pg->CopyGroup,0,grouphcopy);

        ErrText.catFormat("CDsParFile::setParDataCopyGroup error: Die Gruppenhierachie <%s> \n"
                          "  soll von Gruppenhierachie <%s> kopiert werden.\n"
                          " Die zu kopierende Gruppe kann nicht gefunden werden!!!\n"
                         ,grouph.c_str()
                         ,grouphcopy.c_str()
                         );
        Status = NOT_OKAY;
        return Status;
    }

    // Die Gruppe kopieren
    //====================
    if( setParDataSubGroup(pgc,instanz,groupV,par_data) != OKAY ) {
        
        Status = NOT_OKAY;
        return Status;
    }

    return OKAY;
}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataSubGroup: Auswerten der nächsten Subgroup
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataSubGroup(SDsParFileGroup *pg
                                       ,CSlfStr         &instanz
                                       ,CSlfStrV        groupV
                                       ,CDsParData      &par_data) {

    while(pg) {


        //Gruppenhierachieliste
        groupV.append(pg->Name);
#if 0
        //Gruppe kopieren
        //===============
        if(pg->CopyFlag && copy_flag) {
            if( setParDataCopyGroup(pg,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }
#endif
        //Variablen setzen
        //================
        if( pg->NVar > 0 ) {
            if( setParDataVariable(pg->pVar,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }

        //Tabelle setzen
        //================
        if( pg->NTab > 0 ) {
            if( setParDataTable(pg->pTab,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }

        //Untergruppen setzen
        //===================
        if( pg->NSub > 0 ) {
            if( setParDataSubGroup(pg->pSub,instanz,groupV,par_data) != OKAY ) {
                
                Status = NOT_OKAY;
                return Status;
            }
        }

        //nächste Gruppe
        //===============
        pg = pg->pNext;

        //Gruppenhierachieliste
        groupV.delete_last();

    }

    return Status;
}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataVariable: übergibt Variable an Datenstruktur
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataVariable(SDsParFileVariable *pv
                                       ,CSlfStr &instanz
                                       ,CSlfStrV &groupV
                                       ,CDsParData &par_data) {


    while(pv) {

#if 0
        // VAriable kopieren
        //==================
        if(pv->CopyFlag) {

            // Kopie anlegen, damit Werte ergänzt werden können
        	SDsParFileVariable *pvmod = new SDsParFileVariable;

            if( setParDataCopyVariable(pv,pvmod,instanz,groupV) != OKAY ) {
                Status = NOT_OKAY;
                delete pvmod;
                return NOT_OKAY;
            }
            // modifizierte Variable setzen
            //=============================
            if( setParDataSetValue(pv->Name,pvmod,instanz,groupV,par_data) != OKAY ) {
                Status = NOT_OKAY;
                delete pvmod;
                return Status;
            }
            delete pvmod;

        } else {
#endif
		if( !pv->CopyFlag) {

            // Variable setzen
            //================
            if( setParDataSetValue(pv->Name,pv,instanz,groupV,par_data) != OKAY ) {
                Status = NOT_OKAY;
                return Status;
            }

        }

        // nächste Variable
        pv = pv->pNext;

    }
    return OKAY;

}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataCopyVariable: kopiert interne Variable einer anderen internen
//                         Struktur an die Datenstruktur
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataCopyVariable(SDsParFileVariable *pv
                                           ,SDsParFileVariable *pvmod
                                           ,CSlfStr &instanz
                                           ,CSlfStrV &groupV) {

    SDsParFileGroup    *pg;
    SDsParFileVariable *pvc;

    // Suche die zu kopierende Gruppe
    //===============================
    if( (pg = findInternGroup(pv->CopyGroup)) == 0 ) { // nicht gefunden

        CSlfStr grouph;
        CSlfStr grouphcopy;

        grouph.cat(instanz);
        makeGroupName(groupV,1,grouph);
        makeGroupName(pv->CopyGroup,0,grouphcopy);

        ErrText.catFormat("CDsParFile::setParDataCopyVariable error: Die Variable <%s> aus Gruppenhierachie <%s> \n"
                          "  soll von Variable <%s> Gruppenhierachie <%s> kopiert werden.\n"
                          " Die zu kopierende Gruppe kann nicht gefunden werden!!!\n"
                         ,pv->Name.c_str()
                         ,grouph.c_str()
                         ,pv->CopyVariable.c_str()
                         ,grouphcopy.c_str()
                         );
        Status = NOT_OKAY;
        return Status;
    }

    // Suche die zu kopierende Variable
    //=================================
    if( (pvc = findInternVariable(pg,pv->CopyVariable)) == 0 ) { // nicht gefunden

        CSlfStr grouph;
        CSlfStr grouphcopy;

        grouph.cat(instanz);
        makeGroupName(groupV,1,grouph);
        makeGroupName(pv->CopyGroup,0,grouphcopy);

        ErrText.catFormat("CDsParFile::setParDataCopyVariable error: Die Variable <%s> aus Gruppenhierachie <%s> \n"
                          "  soll von Variable <%s> Gruppenhierachie <%s> kopiert werden.\n"
                          " Die zu kopierende Variable kann in der Gruppe nicht gefunden werden!!!\n"
                         ,pv->Name.c_str()
                         ,grouph.c_str()
                         ,pv->CopyVariable.c_str()
                         ,grouphcopy.c_str()
                         );
        Status = NOT_OKAY;
        return Status;
    }

    //Kopie muß Werte enthalten
    //=========================
    if( pvc->Type == DEF_VOID ) {

        CSlfStr grouph;
        CSlfStr grouphcopy;

        grouph.cat(instanz);
        makeGroupName(groupV,1,grouph);
        makeGroupName(pv->CopyGroup,0,grouphcopy);

        ErrText.catFormat("CDsParFile::setParDataCopyVariable error: Die Variable <%s> aus Gruppenhierachie <%s> \n"
                          "  soll von Variable <%s> Gruppenhierachie <%s> kopiert werden.\n"
                          "  Die zu kopierende Variable besitzt keinen Wert!!!\n"
                         ,pv->Name.c_str()
                         ,grouph.c_str()
                         ,pv->CopyVariable.c_str()
                         ,grouphcopy.c_str()
                         );

        if( pvc->CopyFlag )
            ErrText.catFormat("  Die zu kopierende Variable wird auch kopiert (bislang nicht möglich) !!!\n");

        Status = NOT_OKAY;
        return Status;
    }


	 //memcpy(pvmod, pvc, sizeof(SDsParFileVariable));
	 pvmod->Name = pv->Name;

     pvmod->Comment   = pvc->Comment;
     pvmod->Factor    = pvc->Factor;
     pvmod->FSet      = pvc->FSet;
     pvmod->Offset    = pvc->Offset;
     pvmod->OSet      = pvc->OSet;
     pvmod->Transpose = pvc->Transpose;
     pvmod->TSet      = pvc->TSet;
     pvmod->ILine     = pvc->ILine;
     pvmod->NCol      = pvc->NCol;
     pvmod->NRow      = pvc->NRow;
     pvmod->NVal      = pvc->NVal;
     pvmod->StrMVal   = pvc->StrMVal;
     pvmod->Type      = pvc->Type;
     pvmod->Unit      = pvc->Unit;


	// Prüfen, ob Werte in der eigentlichen Variablen gesetzt
	if( pv->Comment.getLen() > 0 )
		pvmod->Comment = pv->Comment;

	if( pv->Unit.getLen() > 0 )
		pvmod->Unit = pv->Unit;

    if( pv->FSet ) {
		pvmod->Factor = pv->Factor;
        pvmod->FSet   = pv->FSet;
    }
    if( pv->OSet ) {
		pvmod->Offset = pv->Offset;
        pvmod->OSet   = pv->OSet;
    }
    if( pv->TSet ) {
		pvmod->Transpose = pv->Transpose;
        pvmod->TSet      = pv->TSet;
    }

	if(  pv->StrMVal.getNcols() > 0 
      && pv->StrMVal.getNrows() > 0 
      && pv->Type == DEF_STRINGM
      ) {

		pvmod->StrMVal = pv->StrMVal;
		pvmod->NCol    = pv->StrMVal.getNcols();
		pvmod->NRow    = pv->StrMVal.getNrows();
		pvmod->NVal    = pvmod->NCol * pvmod->NRow;
        pvmod->Type    = pv->Type;
	}

    return OKAY;

}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataSetValue: Werte werden an Datenstruktur übergeben
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataSetValue(CSlfStr &varname
                                       ,SDsParFileVariable *pv
                                       ,CSlfStr &instanz
                                       ,CSlfStrV &groupV
                                       ,CDsParData &par_data) {

    switch(pv->Type) {

    case DEF_VOID:

        // keine Aktion, sollte durch eine Kopie erstellt worden sein
        break;
    case DEF_STRINGM: // String-Matrix_t

        // Transponieren:
        if(  (  pv->Transpose 
             && pv->StrMVal.getNcols() > 1 
             && pv->StrMVal.getNrows() > 1 
             ) // echteMatrix
          || (  pv->StrMVal.getNcols() >  1 
             && pv->StrMVal.getNrows() == 1 
             ) // Zeilenvektor wird gedreht
          )
            pv->StrMVal.transpone();


        if( par_data.setStringMData(&instanz
                                   ,&groupV
                                   ,&varname
                                   ,&pv->Unit
                                   ,pv->Factor
                                   ,pv->Offset
                                   ,&pv->Comment
                                   ,&pv->StrMVal) != OKAY ) {

            ErrText.catFormat("CDsParFile::setParDataVariable error: Problem beim Setzen der eingelesenen Variablen <%s>!!!\n%s\n"
                             ,pv->Name.c_str()
                             ,par_data.getErrText()
                             );
            Status = NOT_OKAY;
        }
        break;

    case DEF_VEC: // double-Vector_t

        if( par_data.setVectorData(&instanz
                                  ,&groupV
                                  ,&varname
                                  ,&pv->Unit
                                  ,pv->Factor
                                  ,pv->Offset
                                  ,&pv->Comment
                                  ,&pv->Vec
								  ,GET_NROWS(pv->Vec)
								  ) != OKAY ) {

            ErrText.catFormat("CDsParFile::setParDataVariable error: Problem beim Setzen der eingelesenen Variablen <%s>!!!\n%s\n"
                             ,pv->Name.c_str()
                             ,par_data.getErrText()
                             );
            Status = NOT_OKAY;
        }
        break;
    case DEF_MAT: // double-Vector_t

        if( par_data.setMatrixData(&instanz
                                  ,&groupV
                                  ,&varname
                                  ,&pv->Unit
                                  ,pv->Factor
                                  ,pv->Offset
                                  ,&pv->Comment
                                  ,pv->Mat
								  ,GET_NROWS(pv->Mat)
								  ,GET_NCOLS(pv->Mat)
								  ) != OKAY ) {

            ErrText.catFormat("CDsParFile::setParDataVariable error: Problem beim Setzen der eingelesenen Variablen <%s>!!!\n%s\n"
                             ,pv->Name.c_str()
                             ,par_data.getErrText()
                             );
            Status = NOT_OKAY;
        }
        break;
    default:
        ErrText.catFormat("CDsParFile::setParDataSetValue error: Problem beim Setzen der eingelesenen Variablen <%s>!!!\n"
                          "Type <%s> ist nicht programmiert.\n"
                         ,pv->Name.c_str()
                         ,VarTypeStr[pv->Type]
                         );
        Status = NOT_OKAY;
    }

    return OKAY;
}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataTable: übergibt Tabelle an Datenstruktur
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataTable(SDsParFileTable *pt
                                       ,CSlfStr &instanz
                                       ,CSlfStrV &groupV
                                       ,CDsParData &par_data) {


    while(pt) {

        // 1DTable
        if( pt->Type == DEF_1D_TAB 
          ||pt->Type == DEF_2D_TAB
          ) {
#if 0
            // Tabelle kopieren
            //==================
            if(pt->CopyFlag) {

                // Kopie anlegen, damit Werte ergänzt werden können
            	SDsParFileTable *ptmod = new SDsParFileTable;

                if( setParDataCopyTable(pt,ptmod,instanz,groupV) != OKAY ) {
                    Status = NOT_OKAY;
                    delete ptmod;
                    return NOT_OKAY;
                }
                // mod. Tabelle setzen
                //====================
                if( setParDataSetTable(pt->Name,ptmod,instanz,groupV,par_data) != OKAY ) {
                    Status = NOT_OKAY;
                    delete ptmod;
                    return Status;
                }
                delete ptmod;
            } else {
#endif
            if( !pt->CopyFlag ) {

                // Tabelle setzen
                //================
                if( setParDataSetTable(pt->Name,pt,instanz,groupV,par_data) != OKAY ) {
                    Status = NOT_OKAY;
                    return Status;
                }
            }
        }

        // nächste Tabelle
        pt = pt->pNext;

    }
    return OKAY;

}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataCopyTable: kopiert interne Tabelle einer anderen internen
//                         Struktur an die Datenstruktur
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataCopyTable(SDsParFileTable *pt
                                        ,SDsParFileTable *ptmod
                                        ,CSlfStr &instanz
                                        ,CSlfStrV &groupV
                                        ) {

    SDsParFileGroup    *pg;
    SDsParFileTable    *ptcopy;

    // Suche die zu kopierende Gruppe
    //===============================
    if( (pg = findInternGroup(pt->CopyGroup)) == 0 ) { // nicht gefunden

        CSlfStr grouph;
        CSlfStr grouphcopy;

        grouph.cat(instanz);
        makeGroupName(groupV,1,grouph);
        makeGroupName(pt->CopyGroup,0,grouphcopy);

        ErrText.catFormat("CDsParFile::setParDataCopyTable error: Die Tabelle <%s> aus Gruppenhierachie <%s> \n"
                          "  soll von Tabelle <%s> Gruppenhierachie <%s> kopiert werden.\n"
                          " Die zu kopierende Gruppe kann nicht gefunden werden!!!\n"
                         ,pt->Name.c_str()
                         ,grouph.c_str()
                         ,pt->CopyTable.c_str()
                         ,grouphcopy.c_str()
                         );
        Status = NOT_OKAY;
        return Status;
    }

    // Suche die zu kopierende Tabelle
    //=================================
    if( (ptcopy = findInternTable(pg,pt->CopyTable)) == 0 ) { // nicht gefunden

        CSlfStr grouph;
        CSlfStr grouphcopy;

        grouph.cat(instanz);
        makeGroupName(groupV,1,grouph);
        makeGroupName(pt->CopyGroup,0,grouphcopy);

        ErrText.catFormat("CDsParFile::setParDataCopyTable error: Die Tabelle <%s> aus Gruppenhierachie <%s> \n"
                          "  soll von Tabelle <%s> Gruppenhierachie <%s> kopiert werden.\n"
                          " Die zu kopierende Tabelle kann in der Gruppe nicht gefunden werden!!!\n"
                         ,pt->Name.c_str()
                         ,grouph.c_str()
                         ,pt->CopyTable.c_str()
                         ,grouphcopy.c_str()
                         );
        Status = NOT_OKAY;
        return Status;
    }

    // Kopie
    ptmod->Name  = pt->Name;
    ptmod->ILine = pt->ILine;
    ptmod->Type  = pt->Type;

    if( pt->Comment.getLen() > 0 )
        ptmod->Comment = pt->Comment;
    else
        ptmod->Comment = ptcopy->Comment;
    
    // Variablen
    setParDataCopyTableVec(&pt->XVec,&ptmod->XVec,&ptcopy->XVec);
    setParDataCopyTableVec(&pt->YVec,&ptmod->YVec,&ptcopy->YVec);
    if(  pt->Type == IS_2DTABLE )  
        setParDataCopyTableVec(&pt->ZMat,&ptmod->ZMat,&ptcopy->ZMat);

    return OKAY;

}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataCopyTableVec: kopiert Vektor unter bestimmten Bedingungen
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataCopyTableVec(SDsParFileVariable *pv
                                           ,SDsParFileVariable *pvmod
                                           ,SDsParFileVariable *pvc
                                           ) {
    if( pv->Name.getLen() > 0 ) {
        pvmod->Name         = pv->Name;
    } else {
        pvmod->Name         = pvc->Name;
    }
    if( pv->Comment.getLen() > 0 ) {
        pvmod->Comment         = pv->Comment;
    } else {
        pvmod->Comment         = pvc->Comment;
    }
    if( pv->Unit.getLen() > 0) {
        pvmod->Unit         = pv->Unit;
    } else {
        pvmod->Unit         = pvc->Unit;
    }
    if( pv->FSet ) {
        pvmod->Factor       = pv->Factor;
        pvmod->FSet         = pv->FSet;
    } else {
        pvmod->Factor       = pvc->Factor;
        pvmod->FSet         = pvc->FSet;
    }
    if( pv->OSet ) {
        pvmod->Offset       = pv->Offset;
        pvmod->OSet         = pv->OSet;
    } else {
        pvmod->Offset       = pvc->Offset;
        pvmod->OSet         = pvc->OSet;
    }
    if( pv->TSet ) {
        pvmod->Transpose    = pv->Transpose;
        pvmod->TSet         = pv->TSet;
    } else {
        pvmod->Offset       = pvc->Offset;
        pvmod->OSet         = pvc->OSet;
    }
    if( pv->NVal > 0 ) {
        pvmod->Type         = pv->Type;

        pvmod->NCol         = pv->NCol;
        pvmod->NRow         = pv->NRow;
        pvmod->NVal         = pv->NVal;
        pvmod->StrMVal      = pv->StrMVal;
        pvmod->CopyFlag     = 0;

        pvmod->ILine        = pv->ILine;
        pvmod->ParamFile    = pv->ParamFile;
    } else {
        pvmod->Type         = pvc->Type;

        pvmod->NCol         = pvc->NCol;
        pvmod->NRow         = pvc->NRow;
        pvmod->NVal         = pvc->NVal;
        pvmod->StrMVal      = pvc->StrMVal;
        pvmod->CopyFlag     = pvc->CopyFlag;
        pvmod->CopyGroup    = pvc->CopyGroup;
        pvmod->CopyVariable = pvc->CopyVariable;

        pvmod->ILine        = pvc->ILine;
        pvmod->ParamFile    = pvc->ParamFile;
    }

    return Status;
}
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
// setParDataSetTable: Tabelle an Datenstruktur übergeben
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
//[][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
status_t CDsParFile::setParDataSetTable(CSlfStr &varname
                                       ,SDsParFileTable *pt
                                       ,CSlfStr &instanz
                                       ,CSlfStrV &groupV
                                       ,CDsParData &par_data) {

    SDsParFileVariable *pvxmod=0;
    SDsParFileVariable *pvymod=0;
    SDsParFileVariable *pmzmod=0;
    
    SDsParFileVariable *pvx;
    SDsParFileVariable *pvy;
    SDsParFileVariable *pmz;

    Vector_t xvec=0;
    Vector_t yvec=0;
    Matrix_t zmat=0;
    uint32_t i;
    uint32_t j;

    switch(pt->Type) {

    case DEF_VOID:

        // keine Aktion
        break;
    case DEF_1D_TAB: // 1D-TABLE
	case DEF_2D_TAB:

        // X-Vektor bilden
        //=======================

        // Wenn zu kopieren ist
        if( pt->XVec.CopyFlag ) {

            // Kopie anlegen, damit Werte ergänzt werden können
        	pvxmod = new SDsParFileVariable;

            if( setParDataCopyVariable(&pt->XVec,pvxmod,instanz,groupV) != OKAY ) {
                Status = NOT_OKAY;
                delete pvxmod;
                return NOT_OKAY;
            }
            pvx = pvxmod;
        } else {
            pvx = &pt->XVec;
        }

		if( pvx->Type == DEF_STRINGM ) {

			//den längeren Vektor  benutzen
			if( pvx->StrMVal.getNcols() > pvx->StrMVal.getNrows() )
				pvx->StrMVal.transpone();

			xvec = NewVector(pvx->StrMVal.getNrows());

			for(i=0;i<pvx->StrMVal.getNrows();i++) {


				if( SlfStr2Num(pvx->StrMVal.get_str(i,0),&xvec[i]) != NO_ERR ) {

					CSlfStr texta;
					makeGroupName(groupV,1,texta);                
					ErrText.catFormat("CDsParFile::setParDataSetTable: Aus Instanz <%s> Gruppe <%s> Tabelle <%s> xvec <%s> konnte der Wert <%s> Index <%i> nicht von string in double gewandelt werden\n"
									 ,instanz.c_str()
									 ,texta.c_str()
									 ,varname.c_str()
									 ,pvx->Name.c_str()
									 ,pvx->StrMVal.get_str(i,0)
									 ,i
									 );
					Status = NOT_OKAY;
					if( pvxmod )
						delete pvxmod;
					if(xvec)
						FreeVector(xvec);
					return Status;
				}
			}

		} else if( pvx->Type == DEF_VEC ) {

			xvec = VectorCopy(pvx->Vec);

		} else if( pvx->Type == DEF_MAT ) {

			//den längeren Vektor  benutzen
			if( GET_NROWS(pvx->Mat) >= GET_NCOLS(pvx->Mat) ) {

				xvec = NewVector(GET_NROWS(pvx->Mat));
				for(i=0;i<GET_NROWS(pvx->Mat);i++) 
					xvec[i]  = pvx->Mat[i][0];
			} else {
				xvec = NewVector(GET_NCOLS(pvx->Mat));
				for(i=0;i<GET_NCOLS(pvx->Mat);i++) 
					xvec[i]  = pvx->Mat[0][i];
			}
		} else {

			CSlfStr texta;
			makeGroupName(groupV,1,texta);                
			ErrText.catFormat("CDsParFile::setParDataSetTable: Aus Instanz <%s> Gruppe <%s> Tabelle <%s> xvec <%s> konnte der Typ <%s> nicht verarbeitet werden (Möglicherweise nicht prograammiert)\n"
							 ,instanz.c_str()
							 ,texta.c_str()
							 ,varname.c_str()
							 ,pvx->Name.c_str()
							 ,VarTypeStr[pvx->Type]
							 );
			Status = NOT_OKAY;
			if( pvxmod )
				delete pvxmod;
			if(xvec)
				FreeVector(xvec);
			return Status;
		}

        // Y-Vektor bilden
        //=====================

        // Wenn zu kopieren ist
        if( pt->YVec.CopyFlag ) {

            // Kopie anlegen, damit Werte ergänzt werden können
        	pvymod = new SDsParFileVariable;

            if( setParDataCopyVariable(&pt->YVec,pvymod,instanz,groupV) != OKAY ) {
                Status = NOT_OKAY;
                if( pvxmod )
                    delete pvxmod;
                delete pvymod;
                if(xvec)
                    FreeVector(xvec);
                if(yvec)
                    FreeVector(yvec);
                return NOT_OKAY;
            }
            pvy = pvymod;
        } else {
            pvy = &pt->YVec;
        }


		if( pvy->Type == DEF_STRINGM ) {

			//den längeren Vektor  benutzen
			if( pvy->StrMVal.getNcols() > pvy->StrMVal.getNrows() )
				pvy->StrMVal.transpone();

			yvec = NewVector(pvy->StrMVal.getNrows());

			for(i=0;i<pvy->StrMVal.getNrows();i++) {


				if( SlfStr2Num(pvy->StrMVal.get_str(i,0),&yvec[i]) != NO_ERR ) {

					CSlfStr texta;
					makeGroupName(groupV,1,texta);                
					ErrText.catFormat("CDsParFile::setParDataSetTable: Aus Instanz <%s> Gruppe <%s> Tabelle <%s> yvec <%s> konnte der Wert <%s> Index <%i> nicht von string in double gewandelt werden\n"
									 ,instanz.c_str()
									 ,texta.c_str()
									 ,varname.c_str()
									 ,pvy->Name.c_str()
									 ,pvy->StrMVal.get_str(i,0)
									 ,i
									 );
					Status = NOT_OKAY;
					if( pvxmod )
						delete pvxmod;
					if( pvymod )
						delete pvymod;
					if(xvec)
						FreeVector(xvec);
					if(yvec)
						FreeVector(yvec);
					return Status;
				}
			}

		} else if( pvy->Type == DEF_VEC ) {

			yvec = VectorCopy(pvy->Vec);

		} else if( pvy->Type == DEF_MAT ) {

			//den längeren Vektor  benutzen
			if( GET_NROWS(pvy->Mat) >= GET_NCOLS(pvy->Mat) ) {

				yvec = NewVector(GET_NROWS(pvy->Mat));
				for(i=0;i<GET_NROWS(pvy->Mat);i++) 
					yvec[i]  = pvy->Mat[i][0];
			} else {
				yvec = NewVector(GET_NCOLS(pvy->Mat));
				for(i=0;i<GET_NCOLS(pvy->Mat);i++) 
					yvec[i]  = pvy->Mat[0][i];
			}
		} else {

			CSlfStr texta;
			makeGroupName(groupV,1,texta);                
			ErrText.catFormat("CDsParFile::setParDataSetTable: Aus Instanz <%s> Gruppe <%s> Tabelle <%s> yvec <%s> konnte der Typ <%s> nicht verarbeitet werden (Möglicherweise nicht prograammiert)\n"
							 ,instanz.c_str()
							 ,texta.c_str()
							 ,varname.c_str()
							 ,pvy->Name.c_str()
							 ,VarTypeStr[pvy->Type]
							 );
			Status = NOT_OKAY;
			if( pvxmod )
				delete pvxmod;
			if( pvymod )
				delete pvymod;
			if(xvec)
				FreeVector(xvec);
			if(yvec)
				FreeVector(yvec);
			return Status;
		}


		// 1d-Tabelle bilden
		//==================
		if( pt->Type == DEF_1D_TAB ) {

			// alle Daten an par_data in die Tabelle übergeben
			//================================================
			if( par_data.set1DTableData(&instanz
									   ,&groupV
									   ,&varname
									   ,&pt->Comment
									   ,&pvx->Name
									   ,&pvx->Unit
									   ,pvx->Factor
									   ,pvx->Offset
									   ,&pvx->Comment
									   ,GET_NROWS(xvec)
									   ,xvec
									   ,&pvy->Name
									   ,&pvy->Unit
									   ,pvy->Factor
									   ,pvy->Offset
									   ,&pvy->Comment
									   ,GET_NROWS(yvec)
									   ,yvec
									   ,pt->Order
									   ) != OKAY ) {

				ErrText.catFormat("CDsParFile::setParDataSetTable error: Problem beim Setzen der eingelesenen Tabelle <%s>!!!\n%s\n"
								 ,pt->Name.c_str()
								 ,par_data.getErrText()
								 );
				Status = NOT_OKAY;
			}

		// 2d-Tabelle bilden
		//==================
		} else {

			// Matrix_t-Z-Matrix_t bilden
			//=====================

			// Wenn zu kopieren ist
			if( pt->ZMat.CopyFlag ) {

				// Kopie anlegen, damit Werte ergänzt werden können
        		pmzmod = new SDsParFileVariable;

				if( setParDataCopyVariable(&pt->ZMat,pmzmod,instanz,groupV) != OKAY ) {
					Status = NOT_OKAY;
					if( pvxmod )
						delete pvxmod;
					if( pvymod )
						delete pvymod;
					delete pmzmod;
					return NOT_OKAY;
				}
				pmz = pmzmod;
			} else {
				pmz = &pt->ZMat;
			}

			if( pmz->Type == DEF_STRINGM ) {

				zmat = NewMatrix(pmz->StrMVal.getNrows(),pmz->StrMVal.getNcols());

				for(i=0;i<pmz->StrMVal.getNrows();i++) {
					for(j=0;j<pmz->StrMVal.getNcols();j++) {


						if( SlfStr2Num(pmz->StrMVal.get_str(i,j),&zmat[i][j]) != NO_ERR ) {

							CSlfStr texta;
							makeGroupName(groupV,1,texta);                
							ErrText.catFormat("CDsParFile::setParDataSetTable: Aus Instanz <%s> Gruppe <%s> Tabelle <%s>, zmat <%s> konnte der Wert <%s> Index <%i,%i> nicht von string in double gewandelt werden\n"
											 ,instanz.c_str()
											 ,texta.c_str()
											 ,varname.c_str()
											 ,pmz->Name
											 ,pmz->StrMVal.get_str(i,j)
											 ,i,j
											 );
							if( pvxmod )
								delete pvxmod;
							if( pvymod )
								delete pvymod;
							if( pmzmod )
								delete pmzmod;
							if(xvec)
								FreeVector(xvec);
							if(yvec)
								FreeVector(yvec);
							if( zmat )
								FreeMatrix(zmat);
							Status = NOT_OKAY;
							return Status;
						}
					}
				}
			} else if( pmz->Type == DEF_MAT ) {

				zmat = MatrixCopy(pmz->Mat);

			} else {

				CSlfStr texta;
				makeGroupName(groupV,1,texta);                
				ErrText.catFormat("CDsParFile::setParDataSetTable: Aus Instanz <%s> Gruppe <%s> Tabelle <%s> zmat <%s> konnte der Typ <%s> nicht verarbeitet werden (Möglicherweise nicht prograammiert)\n"
								 ,instanz.c_str()
								 ,texta.c_str()
								 ,varname.c_str()
								 ,pmz->Name.c_str()
								 ,VarTypeStr[pmz->Type]
								 );
				Status = NOT_OKAY;
				if( pvxmod )
					delete pvxmod;
				if( pvymod )
					delete pvymod;
				if(xvec)
					FreeVector(xvec);
				if(yvec)
					FreeVector(yvec);
				if(zmat)
					FreeMatrix(zmat);
				return Status;
			}

			// Transponieren, wenn angegeben:
			if( pmz->Transpose ) 
				TransponeMatrix(zmat,zmat);

			if( par_data.set2DTableData(&instanz
									   ,&groupV
									   ,&varname
									   ,&pt->Comment
									   ,&pvx->Name
									   ,&pvx->Unit
									   ,pvx->Factor
									   ,pvx->Offset
									   ,&pvx->Comment
									   ,GET_NROWS(xvec)
									   ,xvec
									   ,&pvy->Name
									   ,&pvy->Unit
									   ,pvy->Factor
									   ,pvy->Offset
									   ,&pvy->Comment
									   ,GET_NROWS(yvec)
									   ,yvec
									   ,&pmz->Name
									   ,&pmz->Unit
									   ,pmz->Factor
									   ,pmz->Offset
									   ,&pmz->Comment
									   ,zmat
									   ,pt->Order
									   ) != OKAY ) {

				ErrText.catFormat("CDsParFile::setParDataSetTable error: Problem beim Setzen der eingelesenen Tabelle <%s>!!!\n%s\n"
								 ,pt->Name.c_str()
								 ,par_data.getErrText()
								 );
				Status = NOT_OKAY;
			}
		}
        if( pvxmod )
            delete pvxmod;
        if( pvymod )
            delete pvymod;
        if( pmzmod )
            delete pmzmod;
        if( xvec )
            FreeVector(xvec);
        if( yvec )
            FreeVector(yvec);
        if( zmat )
            FreeMatrix(zmat);

        break;
    default:
        ErrText.catFormat("CDsParFile::setParDataSetTable error: Problem beim Setzen der eingelesenen Tabelle <%s>!!!\n"
                          "Type <%s> ist nicht programmiert.\n"
                         ,pt->Name.c_str()
                         ,VarTypeStr[pt->Type]
                         );
        Status = NOT_OKAY;
    }

    return OKAY;
}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// write: schreibe Datensatz in Logfile
//-----------------------------------------------------------------
//-----------------------------------------------------------------
status_t CDsParFile::write(CSlfLogFile *plogfile,CDsParData &ParData) {

    FILE   *fid;
    uint32_t ninst,iinst;
    uint32_t ihier;

    uint32_t g_list[20];


    // Logfile starten, wenn nicht geöffnet
    //=====================================
    if( pLogFile && !pLogFile->isOpen() ) {
      if( pLogFile->open() != OKAY)
		  Status = NOT_OKAY;
	}
    if( Status != OKAY ) {

        ErrText.catFormat("CDsParFile::write: Logfile <%s> konnte nicht göffnet werden\n"
                         ,plogfile->getLogFileName()
                         );
        writeLogFile("CDsParFile::write1");

        return Status;
    }


    fid = plogfile->getFid();

    ninst = ParData.getNInstanz();
    for(iinst=0;iinst<ninst;iinst++) {

        ihier = 0;
        g_list[ihier] = iinst;

        //Instanz schreiben
        if( SlfStrLen(ParData.getNameInstanz(iinst)) > 0 )
            fprintf(fid
                   ,"\n%s%s%s\n"
                   ,cQuot0Gr[ihier]
                   ,ParData.getNameInstanz(iinst)
                   ,cQuot1Gr[ihier]);

        writeVar(fid,ParData,g_list,ihier);
        writeGroup(fid,ParData,g_list,ihier);
    }    
#ifdef DS_DEBUG
    writeLogFile("CDsParFile::write1");
#else
    if( Status != OKAY)
		writeLogFile("CDsParFile::write1");
#endif
    return Status;
}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// write: schreibe Datensatz in beliebige Datei
//-----------------------------------------------------------------
//-----------------------------------------------------------------
status_t CDsParFile::write(char *par_file,CDsParData &ParData) {

    FILE   *fid;
    uint32_t ninst,iinst;
    uint32_t ihier;

    uint32_t g_list[20];

#if _MSC_VER > 1310
     fopen_s(&fid,par_file,"w");
#else
    fid = fopen(par_file,"w");
#endif
    if( fid == 0 ) {
        Status = NOT_OKAY;
        ErrText.catFormat("Error in CDsParFile::write(); Parameterfile <%s> kann nicht geöffnet werden\n"
                         ,par_file);
        writeLogFile("CDsParFile::write2");
        return Status;
    }

    ninst = ParData.getNInstanz();
    for(iinst=0;iinst<ninst;iinst++) {

        ihier = 0;
        g_list[ihier] = iinst;

        //Instanz schreiben
        fprintf(fid
               ,"\n%s%s%s\n"
               ,cQuot0Gr[ihier]
               ,ParData.getNameInstanz(iinst)
               ,cQuot1Gr[ihier]);

        writeVar(fid,ParData,g_list,ihier);
        writeGroup(fid,ParData,g_list,ihier);
    }    
    fclose(fid);

#ifdef DS_DEBUG
    writeLogFile("CDsParFile::write1");
#else
    if( Status != OKAY)
		writeLogFile("CDsParFile::write1");
#endif

    return Status;
}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// writeGroup: schreibe Gruppe
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeGroup(FILE *fid
                            ,CDsParData &ParData
                            ,uint32_t *g_list
                            ,uint32_t ihier) {

        uint32_t ngr,igr;

        ngr =ParData.getNSubGroup(g_list,ihier+1);
        ihier++;

        for(igr=0;igr<ngr;igr++) {

            g_list[ihier] = igr;

            //Gruppe schreiben
            fprintf(fid
                   ,"\n%s%s%s\n"
                   ,cQuot0Gr[ihier]
                   ,ParData.getNameGroup(g_list,ihier+1)
                   ,cQuot1Gr[ihier]);

            if( ParData.existCommentGroup(g_list,ihier+1) ) {

                fprintf(fid
                       ,"%s%s%s %s %s\n"
                       ,ParData.getNameGroup(g_list,ihier+1)
                       ,cTrennZeichAttr
                       ,cAttrComment
                       ,cZuweis
                       ,ParData.getCommentGroup(g_list,ihier+1)
                       );
            }
            fprintf(fid,"\n");

            writeVar(fid,ParData,g_list,ihier);
            writeGroup(fid,ParData,g_list,ihier);
        }

}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// writeVar: schreibe Variable
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeVar(FILE *fid
                          ,CDsParData &ParData
                          ,uint32_t *g_list
                          ,uint32_t ihier) {

    uint32_t nvar = ParData.getNVar(g_list,ihier+1);
    uint32_t ivar;
    EVarType type;

    for(ivar=0;ivar<nvar;ivar++) {

		char *name = ParData.getNameVar(g_list,ihier+1,ivar);

        type = ParData.getTypeVar(g_list,ihier+1,ivar);

        switch(type) {

        case DEF_DOUBLE:
        case DEF_SIGNED_CHAR:
        case DEF_SIGNED_SHORT:
        case DEF_SIGNED_LONG:
        case DEF_UNSIGNED_CHAR:
        case DEF_UNSIGNED_SHORT:
        case DEF_UNSIGNED_LONG:

            writeVal(fid,ParData,g_list,ihier,ivar,type);
            break;
        case DEF_STRING:
        case DEF_STRINGM:
            
            writeString(fid,ParData,g_list,ihier,ivar,type);
            break;
        case DEF_VEC:
        case DEF_1D_TAB:
        case DEF_2D_TAB:
            
            writeTab(fid,ParData,g_list,ihier,ivar,type);
            break;
        }
    }
}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// write: schreibe Wert
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeVal(FILE *fid
                         ,CDsParData &ParData
                         ,uint32_t *g_list
                         ,uint32_t ihier
                         ,uint32_t ivar
                         ,EVarType type) {

    char *unit;
    char *comment;
    double *pfactor,*poffset;
    uint32_t nrow,ncol;



    nrow = ParData.getNRowVar(g_list,ihier+1,ivar);
    ncol = ParData.getNColVar(g_list,ihier+1,ivar);

    unit = ParData.getUnitVar(g_list,ihier+1,ivar);

    fprintf(fid,"%s",ParData.getNameVar(g_list,ihier+1,ivar));

    if( SlfStrLen(unit) > 0 ) {

        fprintf(fid
               ," %s%s%s"
               ,cQuotUnit0
               ,unit
               ,cQuotUnit1);
    }

    switch(type) {

    case DEF_DOUBLE:

        if( nrow*ncol == 1 ) {

            double *pval = ParData.getVDoubleVar(g_list,ihier+1,ivar,0,0);

            //Einzelvariable schreiben
            if( pval )
                fprintf(fid
                       ," %s %g\n"
                       ,cZuweis
                       ,*pval);
        }
        break;
    case DEF_SIGNED_CHAR:
    case DEF_SIGNED_SHORT:

        if( nrow*ncol == 1 ) {

            sint32_t *pval = ParData.getVSint32Var(g_list,ihier+1,ivar,0,0);

            //Einzelvariable schreiben
            if( pval )
                fprintf(fid
                       ," %s %i\n"
                       ,cZuweis
                       ,*pval);
        }
        break;
    case DEF_SIGNED_LONG:

        if( nrow*ncol == 1 ) {

            sint32_t *pval = ParData.getVSint32Var(g_list,ihier+1,ivar,0,0);

            //Einzelvariable schreiben
            if( pval )
                fprintf(fid
                       ," %s %li\n"
                       ,cZuweis
                       ,*pval);
        }
        break;
    case DEF_UNSIGNED_CHAR:
    case DEF_UNSIGNED_SHORT:

        if( nrow*ncol == 1 ) {

            uint32_t *pval = ParData.getVUint32Var(g_list,ihier+1,ivar,0,0);

            //Einzelvariable schreiben
            if( pval )
                fprintf(fid
                       ," %s %u\n"
                       ,cZuweis
                       ,*pval);
        }
        break;
    case DEF_UNSIGNED_LONG:

        if( nrow*ncol == 1 ) {

            uint32_t *pval = ParData.getVUint32Var(g_list,ihier+1,ivar,0,0);

            //Einzelvariable schreiben
            if( pval )
                fprintf(fid
                       ," %s %lu\n"
                       ,cZuweis
                       ,*pval);
        }
        break;
    }

    pfactor = ParData.getYFactorVar(g_list,ihier+1,ivar);
    if( pfactor )
        if( fabs(*pfactor-1.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,ParData.getNameVar(g_list,ihier+1,ivar)
                   ,cTrennZeichAttr
                   ,cAttrFactor
                   ,cZuweis
                   ,*pfactor);

    poffset = ParData.getYOffsetVar(g_list,ihier+1,ivar);
    if( poffset )
        if( fabs(*poffset-0.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,ParData.getNameVar(g_list,ihier+1,ivar)
                   ,cTrennZeichAttr
                   ,cAttrOffset
                   ,cZuweis
                   ,*poffset);


    comment = ParData.getCommentVar(g_list,ihier+1,ivar);
    if( SlfStrLen(comment) > 0 ) 
        fprintf(fid
               ,"%s%s%s %s %s\n"
               ,ParData.getNameVar(g_list,ihier+1,ivar)
               ,cTrennZeichAttr
               ,cAttrComment
               ,cZuweis
               ,comment);

    fprintf(fid,"\n");
                
}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// write: schreibe Wert
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeString(FILE *fid
                            ,CDsParData &ParData
                            ,uint32_t *g_list
                            ,uint32_t ihier
                            ,uint32_t ivar
                            ,EVarType type) {

    char *name;
    char *unit;
    char *comment;
    double *pfactor,*poffset;
    uint32_t nrow,ncol;
    uint32_t nfree = 0;



    nrow = ParData.getNRowVar(g_list,ihier+1,ivar);
    ncol = ParData.getNColVar(g_list,ihier+1,ivar);

    name = ParData.getNameVar(g_list,ihier+1,ivar);
    unit = ParData.getUnitVar(g_list,ihier+1,ivar);

    fprintf(fid,"%s",name);
    nfree += SlfStrLen(name);

    if( SlfStrLen(unit) > 0 ) {

        fprintf(fid
               ," %s%s%s"
               ,cQuotUnit0
               ,unit
               ,cQuotUnit1);
        nfree += 1+SlfStrLen(cQuotUnit0)+SlfStrLen(unit)+SlfStrLen(cQuotUnit1);
    }

    switch(type) {

    case DEF_STRING:
    case DEF_STRINGM:

        if( nrow*ncol == 1 ) {

            CSlfStr *pval = ParData.getVStringVar(g_list,ihier+1,ivar,0,0);
            //Einzelvariable schreiben
            if( pval )

                fprintf(fid
                       ," %s %s\n"
                       ,cZuweis
                       ,pval->c_str());
        }
        else
        {
        CSlfStr *pstr; 
        uint32_t nval   = nrow*ncol;
        uint32_t nzeil  = (uint32_t)(ceil((double)nval/(double)DS_PAR_FILE_NVAL_PER_LINE));
        uint32_t izeil  = 0;
        uint32_t iczeil = 0;
        uint32_t icount = 0;
        uint32_t irow,icol;

        fprintf(fid," %s ",cZuweis);

        nfree += 1+SlfStrLen(cZuweis)+1;

        while( izeil < nzeil ) {

            for(irow=0;irow<nrow;irow++) {
                for(icol=0;icol<ncol;icol++) {

                    if( icount == 0 )
                        fprintf(fid,"%s ",cQuotMat0);
                    else if( icount < nval ) {

                        if( icol == 0 )
                            fprintf(fid,"%s ",cTrennZeichCol);
                        else
                            fprintf(fid,"%s ",cTrennZeich);
                    }

                    pstr = ParData.getVStringVar(g_list,ihier+1,ivar,irow,icol);
                    fprintf(fid,"%s",pstr->c_str());
 
                    if( ++icount == nval )
                        break;

                    if( ++iczeil == DS_PAR_FILE_NVAL_PER_LINE ) {
                        fprintf(fid,"\n");
                        for(uint32_t ifree=0;ifree<nfree;ifree++)
                            fprintf(fid," ");
                        iczeil = 0;
                        ++izeil;
                    }
                }
            }
            if( icount == nval )
                break;
        }
        fprintf(fid,"%s\n",cQuotMat1);

        }
        break;
    }
    

    pfactor = ParData.getYFactorVar(g_list,ihier+1,ivar);
    if( pfactor )
        if( fabs(*pfactor-1.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,ParData.getNameVar(g_list,ihier+1,ivar)
                   ,cTrennZeichAttr
                   ,cAttrFactor
                   ,cZuweis
                   ,*pfactor);

    poffset = ParData.getYOffsetVar(g_list,ihier+1,ivar);
    if( poffset )
        if( fabs(*poffset-0.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,ParData.getNameVar(g_list,ihier+1,ivar)
                   ,cTrennZeichAttr
                   ,cAttrOffset
                   ,cZuweis
                   ,*poffset);

    comment = ParData.getCommentVar(g_list,ihier+1,ivar);
    if( SlfStrLen(comment) > 0 ) 
        fprintf(fid
               ,"%s%s%s %s %s\n"
               ,ParData.getNameVar(g_list,ihier+1,ivar)
               ,cTrennZeichAttr
               ,cAttrComment
               ,cZuweis
               ,comment);

    fprintf(fid,"\n");
                
}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// writeTab: schreibe Tabelle und Vektoren
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeTab(FILE *fid
                         ,CDsParData &ParData
                         ,uint32_t *g_list
                         ,uint32_t ihier
                         ,uint32_t ivar
                         ,EVarType type) {

    double *pxfactor;
    double *pxoffset;
    double *pyfactor;
    double *pyoffset;
    double *pzfactor;
    double *pzoffset;

	char   *nametest    = ParData.getNameVar(g_list,ihier+1,ivar);

    switch(type) {

    case DEF_VEC:
        {
        Vector_t vec   = ParData.getVVecVar(g_list,ihier+1,ivar);

        if( vec ) {
            char   *name    = ParData.getNameVar(g_list,ihier+1,ivar);
            char   *unit    = ParData.getUnitVar(g_list,ihier+1,ivar);
            char   *comment = ParData.getCommentVar(g_list,ihier+1,ivar);;
            uint32_t nrow     = ParData.getNRowVar(g_list,ihier+1,ivar);
            uint32_t ncol     = ParData.getNColVar(g_list,ihier+1,ivar);
            pyfactor = ParData.getYFactorVar(g_list,ihier+1,ivar);
            pyoffset = ParData.getYOffsetVar(g_list,ihier+1,ivar);
        
            writeVec(fid
                    ,name
                    ,unit
                    ,vec
                    ,nrow*ncol
                    ,comment
                    ,pyfactor
                    ,pyoffset);
        
        
        }
        }
        break;
    case DEF_1D_TAB:
        {
        CSlf1DTab  *p1dtab = ParData.getV1DTabVar(g_list,ihier+1,ivar);
        pxfactor = ParData.getXFactorVar(g_list,ihier+1,ivar);
        pxoffset = ParData.getXOffsetVar(g_list,ihier+1,ivar);
        pyfactor = ParData.getYFactorVar(g_list,ihier+1,ivar);
        pyoffset = ParData.getYOffsetVar(g_list,ihier+1,ivar);


        if( p1dtab ) {

            fprintf(fid
                   ,"%s%s%s    %s 1dtable 1.Var: X-Vektor 2.Var: Y-Vektor\n"
                   ,cQuot01DTab
                   ,p1dtab->Name.c_str()
                   ,cQuot11DTab
                   ,cComment[0]
                   );

            if( p1dtab->Comment.getLen() > 0 )
                fprintf(fid
                       ,"%s%s%s %s %s\n"
                       ,p1dtab->Name.c_str()
                       ,cTrennZeichAttr
                       ,cAttrComment
                       ,cZuweis
                       ,p1dtab->Comment.c_str());


            fprintf(fid,"\n");

            writeVec(fid
                    ,p1dtab->XName.c_str()
                    ,p1dtab->XUnit.c_str()
                    ,p1dtab->pXVec
                    ,p1dtab->NRows
                    ,p1dtab->XComment.c_str()
                    ,pxfactor
                    ,pxoffset);

            writeVec(fid
                    ,p1dtab->YName.c_str()
                    ,p1dtab->YUnit.c_str()
                    ,p1dtab->pYVec
                    ,p1dtab->NRows
                    ,p1dtab->YComment.c_str()
                    ,pyfactor
                    ,pyoffset);


        }
        }
        break;
    case DEF_2D_TAB:
        {
        CSlf2DTab  *p2dtab = ParData.getV2DTabVar(g_list,ihier+1,ivar);
        pxfactor = ParData.getXFactorVar(g_list,ihier+1,ivar);
        pxoffset = ParData.getXOffsetVar(g_list,ihier+1,ivar);
        pyfactor = ParData.getYFactorVar(g_list,ihier+1,ivar);
        pyoffset = ParData.getYOffsetVar(g_list,ihier+1,ivar);
        pzfactor = ParData.getZFactorVar(g_list,ihier+1,ivar);
        pzoffset = ParData.getZOffsetVar(g_list,ihier+1,ivar);


        if( p2dtab ) {

            fprintf(fid
                   ,"%s%s%s    %s 2dtable 1.Var: X-Vektor 2.Var: Y-Vektor 3.Var:ZMat(ny,nx)\n"
                   ,cQuot02DTab
                   ,p2dtab->Name.c_str()
                   ,cQuot12DTab
                   ,cComment[0]
                   );

            if( p2dtab->Comment.getLen() > 0 )
                fprintf(fid
                       ,"%s%s%s %s %s\n"
                       ,p2dtab->Name.c_str()
                       ,cTrennZeichAttr
                       ,cAttrComment
                       ,cZuweis
                       ,p2dtab->Comment.c_str());


            fprintf(fid,"\n");

            writeVec(fid
                    ,p2dtab->XName.c_str()
                    ,p2dtab->XUnit.c_str()
                    ,p2dtab->pXVec
                    ,p2dtab->XNRows
                    ,p2dtab->XComment.c_str()
                    ,pxfactor
                    ,pxoffset);

            writeVec(fid
                    ,p2dtab->YName.c_str()
                    ,p2dtab->YUnit.c_str()
                    ,p2dtab->pYVec
                    ,p2dtab->YNRows
                    ,p2dtab->YComment.c_str()
                    ,pyfactor
                    ,pyoffset);

            writeMat(fid
                    ,p2dtab->ZName.c_str()
                    ,p2dtab->ZUnit.c_str()
                    ,p2dtab->ZMat
                    ,p2dtab->ZComment.c_str()
                    ,pzfactor
                    ,pzoffset);

        }
        }
        break;
    }

}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// writeVec: schreibe Vektoren
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeVec(FILE *fid
                         ,char *name
                         ,char *unit
                         ,double *pvec
                         ,uint32_t nrows
                         ,char *comment
                         ,double *pfactor
                         ,double *poffset) {

    uint32_t nfree = 0;

    fprintf(fid,"%s",name);

    nfree += SlfStrLen(name);

    if( SlfStrLen(unit) > 0 ) {

        fprintf(fid
               ," %s%s%s"
               ,cQuotUnit0
               ,unit
               ,cQuotUnit1);

        nfree += 1+SlfStrLen(cQuotUnit0)+SlfStrLen(unit)+SlfStrLen(cQuotUnit1);
    }

    fprintf(fid," %s ",cZuweis);

    nfree += 1+SlfStrLen(cZuweis)+1;

    if( nrows == 1 ) {

        fprintf(fid," %g\n",pvec[0]);

    } else {


        uint32_t nzeil  = (uint32_t)(ceil((double)nrows/(double)DS_PAR_FILE_NVAL_PER_LINE));
        uint32_t izeil  = 0;
        uint32_t iczeil = 0;
        uint32_t icount = 0;

        while( izeil < nzeil ) {

            if( icount == 0 )
                fprintf(fid,"%s ",cQuotMat0);
            else if( icount < nrows )
                fprintf(fid,"%s ",cTrennZeich);

            fprintf(fid,"%g",pvec[icount]);
 
            if( ++icount == nrows )
                break;

            if( ++iczeil == DS_PAR_FILE_NVAL_PER_LINE ) {
                fprintf(fid,"\n");
                for(uint32_t ifree=0;ifree<nfree;ifree++)
                    fprintf(fid," ");
                iczeil = 0;
                ++izeil;
            }
        }
        fprintf(fid,"%s\n",cQuotMat1);
    }

    if( comment && SlfStrLen(comment) > 0 )
        fprintf(fid
               ,"%s%s%s %s %s\n"
               ,name
               ,cTrennZeichAttr
               ,cAttrComment
               ,cZuweis
               ,comment);

    if( pfactor )
        if( fabs(*pfactor-1.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,name
                   ,cTrennZeichAttr
                   ,cAttrFactor
                   ,cZuweis
                   ,*pfactor);

    if( poffset )
        if( fabs(*poffset-0.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,name
                   ,cTrennZeichAttr
                   ,cAttrOffset
                   ,cZuweis
                   ,*poffset);
    fprintf(fid,"\n");

}
//-----------------------------------------------------------------
//-----------------------------------------------------------------
// writeMAt: schreibe Matrix_t
//-----------------------------------------------------------------
//-----------------------------------------------------------------
void CDsParFile::writeMat(FILE *fid
                         ,char *name
                         ,char *unit
                         ,Matrix_t mat
                         ,char *comment
                         ,double *pfactor
                         ,double *poffset) {

    uint32_t nfree = 0;

    uint32_t nrows = GET_NROWS(mat);
    uint32_t ncols = GET_NCOLS(mat);

    fprintf(fid,"%s",name);

    nfree += SlfStrLen(name);

    if( SlfStrLen(unit) > 0 ) {

        fprintf(fid
               ," %s%s%s"
               ,cQuotUnit0
               ,unit
               ,cQuotUnit1);

        nfree += 1+SlfStrLen(cQuotUnit0)+SlfStrLen(unit)+SlfStrLen(cQuotUnit1);
    }

    fprintf(fid," %s ",cZuweis);

    nfree += 1+SlfStrLen(cZuweis)+1;

    if( nrows == 1 ) {

        fprintf(fid," %g\n",mat[0][0]);

    } else {


        uint32_t nzeil  = (uint32_t)(ceil((double)nrows*(double)ncols/(double)DS_PAR_FILE_NVAL_PER_LINE));
        uint32_t izeil  = 0;
        uint32_t iczeil = 0;
        uint32_t i,j;

        for(i=0;i<nrows;i++) {
            for(j=0;j<ncols;j++) {

                while( izeil < nzeil ) {

                    if( i == 0 && j ==0 )
                        fprintf(fid,"%s ",cQuotMat0);
                    else if( j == ncols-1 && i < nrows-1 )
                        fprintf(fid,"%s ",cTrennZeichCol);
                    else if( j < ncols-1 && i < nrows-1  )
                        fprintf(fid,"%s ",cTrennZeich);

                    fprintf(fid,"%g",mat[i][j]);
 

                    if( ++iczeil == DS_PAR_FILE_NVAL_PER_LINE ) {
                        fprintf(fid,"\n");
                        for(uint32_t ifree=0;ifree<nfree;ifree++)
                            fprintf(fid," ");
                        iczeil = 0;
                        ++izeil;
                    }
                }
            }
        }
        fprintf(fid,"%s\n",cQuotMat1);
    }

    if( comment && SlfStrLen(comment) > 0 )
        fprintf(fid
               ,"%s%s%s %s %s\n"
               ,name
               ,cTrennZeichAttr
               ,cAttrComment
               ,cZuweis
               ,comment);

    if( pfactor )
        if( fabs(*pfactor-1.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,name
                   ,cTrennZeichAttr
                   ,cAttrFactor
                   ,cZuweis
                   ,*pfactor);

    if( poffset )
        if( fabs(*poffset-0.0) > EPSILON )
            fprintf(fid
                   ,"%s%s%s %s %g\n"
                   ,name
                   ,cTrennZeichAttr
                   ,cAttrOffset
                   ,cZuweis
                   ,*poffset);
    fprintf(fid,"\n");

}
void CDsParFile::makeGroupName(CSlfStrV &groupV,uint32_t ioff,CSlfStr &grouph) {


    uint32_t n = groupV.getNrows();
    uint32_t i;

    for(i=0;i<n&&i<QuotGrLen;i++) {

        grouph.cat(cQuot0Gr[i+ioff]);
        grouph.cat(groupV.get_str(i));
        grouph.cat(cQuot1Gr[i+ioff]);
    }
}

// Logfile schreiben
//=======================
void CDsParFile::writeLogFile(char *name) {

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