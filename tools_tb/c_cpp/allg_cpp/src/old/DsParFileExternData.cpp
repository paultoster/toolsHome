#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include "DsParFile.h"
#include "SlfSys.h"
#include "SlfFkt.h"

#if DS_PAR_USE_MATFILELOAD == 1

// Matlab
//#include "mat.h"
//#include "matrix.h"
#include "SlfMat.h"

//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// readExternFileList: liest externe Daten ein
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
status_t CDsParFile::readExternFileList(void) {

	SDsExternFileList *pf = pExternFileList;

	while(pf) {

		if( pf->FileType == IS_MAT_FILE )
			return readExternMatFile(pf);

		pf = pf->pNext;
	}

	return Status;
}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// readExternMatFile: liest externe Matlab-Datei ein
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
status_t CDsParFile::readExternMatFile(SDsExternFileList *pf) {

	SDsExternFileVarList *pfv=0;

	sMATFile* pMatFile = 0;
    int ndir = 0;
	char** ppMatDir = 0;	
	char*  strVar;
	
	smxArray* pDirArray = 0;
	smxArray* pChanArray = NULL;
	
	//Datei pr¸fen
	if( !SlfSysExistFile(pf->FileName.c_str()) ) {
        Status = NOT_OKAY;
        ErrText.catFormat("Error in CDsParFile::readExternMatFile(); externe Matlab-Datei <%s> kann nicht gefunden werden\n"
                         ,pf->FileName.c_str());
        return Status;
    }

	pMatFile = smatOpen(pf->FileName.c_str(), "r");
  
	if(!pMatFile) {
		Status = NOT_OKAY;
		ErrText.catFormat("Error in CDsParFile::readExternMatFile(); externe Matlab-Datei <%s> kann nicht geˆffnet werden\n"
						 ,pf->FileName.c_str());
		return Status;
	}

	/* Get directory of mat-file */
	ppMatDir = smatGetDir(pMatFile, &ndir);

	if(!ppMatDir) {
		Status = NOT_OKAY;
		ErrText.catFormat("Error in CDsParFile::readExternMatFile(); externe Matlab-Datei <%s> enth‰lt keine Daten\n"
						 ,pf->FileName.c_str());
		return Status;
	}


	// alle Dirs durchprobieren
	for(int idir=0;idir<ndir;idir++) {

		strVar = ppMatDir[idir];
		pDirArray = smatGetVariable(pMatFile, ppMatDir[idir]);
    
		if(smxIsStruct(pDirArray)) {
	    
			int        num_of_elements;
			int        index;
			int        nfields;
			int        ifield;
			const char *fieldname;
			smxArray  *pDsParArray;


			num_of_elements = smxGetNumberOfElements(pDirArray); 
			nfields = smxGetNumberOfFields(pDirArray);

			for (index=0; index<num_of_elements; index++)  {

				// Schleife ¸ber alle Strukturelemente
				//====================================
				for( ifield=0;ifield<nfields;ifield++) {


					pDsParArray = smxGetFieldByNumber(pDirArray, index, ifield);

					fieldname   = smxGetFieldNameByNumber(pDirArray,ifield);

					pfv = pf->pVar;
					while( pfv ) {

	                    // Pr¸fen, ob Wert double ist und der Name ¸bereinstimmt
						if(  smxIsDouble(pDsParArray) 
						  && SlfStrCompare(fieldname,pfv->Name.c_str())
						  && !pfv->isRead ) {

							if( readExternMatVar(pfv,pf,pDsParArray) != OKAY ) {
								Status = NOT_OKAY;
								return Status;
							}
						}
						pfv = pfv->pNext;
					}
				}

			}

		} else if(smxIsDouble(pDirArray)) {

			smxArray  *pDsParArray = pDirArray;
			//matGetVariableInfo(pMatFile, strVar);
			pfv = pf->pVar;
			while( pfv ) {

	            // Pr¸fen, ob Wert double ist und der Name ¸bereinstimmt
				if(  SlfStrCompare(strVar,pfv->Name.c_str())
				  && !pfv->isRead ) {

					if( readExternMatVar(pfv,pf,pDsParArray) != OKAY ) {
						Status = NOT_OKAY;
						return Status;
					}
				}
				pfv = pfv->pNext;
			}
		}

	}
	
	/* Dir freigeben */
	if(ppMatDir) {
		smxFree(ppMatDir);
		ppMatDir = 0;
	}

	/* Matlabdatei schlieﬂen */
	if(pMatFile) 
		smatClose(pMatFile);
	pMatFile = 0;
	
	return Status;
}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
// readExternMatVar: liest Variable aus externe Matlab-Datei ein
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
status_t CDsParFile::readExternMatVar(SDsExternFileVarList *pfv,SDsExternFileList *pf,smxArray *pDsParArray) {

	uint32_t  nrow  = (uint32_t)smxGetM(pDsParArray);
    uint32_t  ncol  = (uint32_t)smxGetN(pDsParArray);
	uint16_t  irow,icol;
	double  *pdvalue;
	CSlfStr text;

	if( nrow == 0 || ncol == 0 ) {
		Status = NOT_OKAY;
		ErrText.catFormat("Error in CDsParFile::readExternMatVar(); Variable <%s> in externer Matlab-Datei <%s> enth‰lt keine Daten (nrow=%i,ncol=%i)\n"
			             ,pfv->Name.c_str()
						 ,pf->FileName.c_str()
						 ,nrow,ncol);
		return Status;
	}


	 
	pfv->pVar->NRow = nrow;
	pfv->pVar->NCol = ncol;
	pfv->pVar->NVal = nrow*ncol;

	if( pfv->pVar->NVal == 1 ) {

		pdvalue = smxGetPr(pDsParArray);
				
		//Typ
		pfv->pVar->Type = DEF_STRINGM;

		// Wert setzen
		pfv->pVar->StrMVal.clear();
		for(irow=0;irow<nrow;irow++) {
			for(icol=0;icol<ncol;icol++) {
				text.clear();
				text.catFormat("%g",pdvalue[nrow*icol+irow]);
				pfv->pVar->StrMVal.cpy(text.c_str(),irow,icol);
			}
		}


		// als gelesen kennzeichnen
		pfv->isRead = 1;

	} else if( pfv->pVar->NCol == 1 ) { // Zeilenvektor

		if( pfv->pVar->Vec )
			FreeVector(pfv->pVar->Vec);

		pfv->pVar->Vec = VectorCopyD(smxGetPr(pDsParArray),pfv->pVar->NRow);

		//Typ
		pfv->pVar->Type = DEF_VEC;

		// als gelesen kennzeichnen
		pfv->isRead = 1;

	} else if( pfv->pVar->NRow == 1 ) { // Spaltenvektor

		if( pfv->pVar->Vec )
			FreeVector(pfv->pVar->Vec);

		pfv->pVar->Vec = VectorCopyD(smxGetPr(pDsParArray),pfv->pVar->NCol);

		//Typ
		pfv->pVar->Type = DEF_VEC;

		// als gelesen kennzeichnen
		pfv->isRead = 1;

	} else { // Matrix_t

		if( pfv->pVar->Mat )
			FreeMatrix(pfv->pVar->Mat);

		pfv->pVar->Mat = MatrixCopyD(smxGetPr(pDsParArray),pfv->pVar->NRow,pfv->pVar->NCol,DOUBLE_MATRIX_ROW_BEFORE_COL);

		//Typ
		pfv->pVar->Type = DEF_MAT;

		// als gelesen kennzeichnen
		pfv->isRead = 1;

	}		
	return OKAY;
}
#endif