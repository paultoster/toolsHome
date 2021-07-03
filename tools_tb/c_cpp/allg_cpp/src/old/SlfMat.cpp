// SlfMat.cpp
//===========
//
// nachgeildete Matlabfunktionen
//
#include "SlfMat.h"
#include "zlib.h"
#ifdef STDC
#  include <stdio.h>
#  include <stdlib.h>
#  include <string.h>
#endif

SlfMATFile * SlfMatNew();
SlfMATFile * SlfMatDelete(SlfMATFile *pmatf);
void         SlfMatDeletePData(SlfMATData *pdata);
void         SlfMatDecideFileType(SlfMATFile *pmatf);
void         SlfMatWriteFileType(SlfMATFile *pmatf);
SlfMATFile * SlfMatReadMatV5(SlfMATFile *pmatf);
SlfMATFile * SlfMatReadMatV5NextDataElement(SlfMATFile *pmatf);
SlfMATFile * SlfMatReadMatV5CompressedData(SlfMATFile *pmatf);
SlfMATFile * SlfMatReadBinaryDataCompressed(SlfMATFile *pmatf);
SlfMATFile * SlfMatReadMatV5Matrix(SlfMATFile *pmatf,SlfMATData  *pdata,unsigned char **ppbuffer,unsigned int *pbytecount);
char      SlfMatReadMatV5FRead(void *pval,size_t lsize,size_t length,SlfMATFile *pmatf);
unsigned char * SlfMatSetBuffer(unsigned char *pbuffer,unsigned int *pbufferlength, unsigned int newlength);
unsigned char SlfMatGetInfo(SlfMATFile *pmatf, unsigned char **ppbuffer
			           	      ,unsigned int *pbytecount,SlfMATData  *pdata);
char SlfMatGetDouble(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata);
char SlfMatGetChar(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata);
char SlfMatGetCell(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata);
char SlfMatGetStruct(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata);
unsigned short	SlfMatGetSubelement(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount);
int SlfMatPutDouble(SlfMATFile *pmatf, const char *name, const SlfMATData *pm);
int SlfMatWriteBinaryDataCompressed(unsigned char *pun,unsigned int nun,unsigned char *pcom,unsigned int *ncom);
#if SLF_MAT_BUFFER_OUT == 1
void BufferOut(unsigned char *buffer,unsigned long blength,char *filename){

	unsigned long i,j;

	FILE *fid=fopen(filename,"w");
	fprintf(fid,"l:%u\n",blength);

	j = 0;
	for(i=0;i<blength;i++) {
		fprintf(fid,"0x%2x ",buffer[i]);
		//fprintf(fid,"%2i ",buffer[i]);
		j++;
		if(j==8) {
			fprintf(fid,"\n");
			j=0;
		}
	}
	fclose(fid);
}
#endif
char SlfMatErrText[SLF_MAT_ERR_TEXT_SIZE];

#if _MSC_VER > MSC_VER_BIS_VS2005
  #define CHECK_ERR(err, msg) { \
      if (err != Z_OK) { \
          sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE, "SlfMatReadBinaryDataCompressed: %s error: %d\n", msg, err); \
          return 0; \
      } \
}
#else
  #define CHECK_ERR(err, msg) { \
      if (err != Z_OK) { \
          sprintf(SlfMatErrText, "SlfMatReadBinaryDataCompressed: %s error: %d\n", msg, err); \
          return 0; \
      } \
}
#endif

#define MATVERROR		0
#define MATVUNKNOWN     1
#define MATV5           2
#define MATV4           3


//===============================================================================
//===============================================================================
// Schnittstellenfunktionen
//===============================================================================
//===============================================================================
// SlfMatOpen, Einlesen einer Matlabdatei
//=======================================
SlfMATFile * SlfMatOpen(const char *filename, const char * mode) {

  SlfMatErrText[0] = '\0';

	if( !(*mode == 'r' || *mode == 'w') )
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatOpen: Unbekannter mode \"%s\" \n",mode);
#else
		sprintf(SlfMatErrText,"SlfMatOpen: Unbekannter mode \"%s\" \n",mode);
#endif
		return 0;
	}


  /* Struktur anlegen */
  /*------------------*/
	SlfMATFile *pmatf = SlfMatNew(); 

	if( !pmatf )
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatOpen: Struktur SlfMatFile konnte nicht angelegt werden\n");
#else
		sprintf(SlfMatErrText,"SlfMatOpen: Struktur SlfMatFile konnte nicht angelegt werden\n");
#endif
    return 0;
	}

  if( *mode == 'r' )
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    errno_t err = fopen_s(&pmatf->fid,filename,"rb");
	  if( err ) 
    {
		  sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatOpen (read): Datei <%f> kann nicht geöffnet werden\n",filename);
		  return SlfMatDelete(pmatf);
	  }
#else
	  pmatf->fid = fopen(filename,"rb");
	  if( !pmatf->fid ) 
    {
		  sprintf(SlfMatErrText,"SlfMatOpen (read): Datei <%f> kann nicht geöffnet werden\n",filename);
		  return SlfMatDelete(pmatf);
	  }
#endif

	  SlfMatDecideFileType(pmatf);

	  switch(pmatf->filetype) {

	  case MATV5:

		  if( !SlfMatReadMatV5(pmatf) )
			  return SlfMatDelete(pmatf);


		  break;
	  case MATVUNKNOWN:
#if _MSC_VER > MSC_VER_BIS_VS2005
		  sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatOpen: unkown Matlab-Format\n");
#else
		  sprintf(SlfMatErrText,"SlfMatOpen: unkown Matlab-Format\n");
#endif
		  return SlfMatDelete(pmatf);
		  break;
	  default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		  sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatOpen: Error Matlab-Format\n");
#else
		  sprintf(SlfMatErrText,"SlfMatOpen: Error Matlab-Format\n");
#endif
      return SlfMatDelete(pmatf);
		  break;
	  }


	  //Name des Datenelements in einer List für matGetDir() Funktion aufführen
	  //=======================================================================
	  if( pmatf->nelements ) {

		  int i;
		  SlfMATData *pdata = pmatf->pdata;
		  pmatf->elementnamelist    = new char*[pmatf->nelements];
		  pmatf->elementpointerlist = new SlfMATData*[pmatf->nelements];

		  for(i=0;i<pmatf->nelements;i++) {

			  if( pdata && pdata->name) {

				  pmatf->elementnamelist[i] = new char[strlen(pdata->name)+1];
#if _MSC_VER > MSC_VER_BIS_VS2005
				  strcpy_s(pmatf->elementnamelist[i],strlen(pdata->name)+1,pdata->name);
#else
				  strcpy(pmatf->elementnamelist[i],pdata->name);
#endif
				  pmatf->elementpointerlist[i] = pdata;
  				
				  pdata = pdata->pnext;
			  }
		  }
	  }
  }
  else /* write */
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
    errno_t err = fopen_s(&pmatf->fid,filename,"wb");
	  if( err ) 
    {
		  sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatOpen (write): Datei <%f> kann nicht geöffnet werden\n",filename);
		  return SlfMatDelete(pmatf);
	  }
#else
	  pmatf->fid = fopen(filename,"wb");
	  if( !pmatf->fid ) 
    {
		  sprintf(SlfMatErrText,"SlfMatOpen (write): Datei <%f> kann nicht geöffnet werden\n",filename);
		  return SlfMatDelete(pmatf);
	  }
#endif

    /* Write Header */
    /*--------------*/
	  SlfMatWriteFileType(pmatf);

  }

	return pmatf;
}
// SlfMatClose, Schliessen einer Matlabdatei und Löschen der Daten
//================================================================
void      SlfMatClose(SlfMATFile *pmatf) {

	if( pmatf )
		SlfMatDelete(pmatf);

}
// SlfMatGetDir, Übergibt eine Liste mit den Datenelementen
//================================================================
char       **SlfMatGetDir(SlfMATFile *pmatf, int *pndir) {

	if( pmatf ) {
	
		*pndir = pmatf->nelements;
		return pmatf->elementnamelist;
	}
	return 0;
}
// SlfMatGetVariable, Übergibt Datenstruktur
//================================================================
SlfMATData * SlfMatGetVariable(SlfMATFile *pmatf, const char *varname) {

	if( pmatf ) {
	
		SlfMATData *pdata = pmatf->pdata;

		while(pdata) {

			if( strcmp(pdata->name,varname) == 0)
				return pdata;

			pdata = pdata->pnext;
		}
	}
	return 0;
}
// SlfMxGetM, Anzahl Zeilen (1. Index)
//================================================================
int SlfMxGetM(SlfMATData * pelement) {

	return pelement->nrows;
}
// SlfMxGetN, anzahl Spalten (2. Index)
//================================================================
int SlfMxGetN(SlfMATData * pelement) {

	return pelement->ncols;
}
// SlfMxGetPr, double Pointer
//================================================================
double *SlfMxGetPr(SlfMATData * pelement) {

	return pelement->pdr;
}
// SlfMxGetName, string
//================================================================
char *SlfMxGetName(SlfMATData * pelement)
{
  return pelement->name;
}

int SlfmxGetString(SlfMATData * pelement, char *buf, int buflen) {

	int i = 0;
	int ret  = 0;
	
	if( buflen = 0 )
		return 1;

	if( pelement->mxClass == mxCHAR_CLASS ) {

		buflen--; // '\0' Abschluß
		if( buflen > (pelement->nrows*pelement->ncols) )
			buflen = pelement->nrows*pelement->ncols;
		else
			ret = 1;

		for(i=0;i<buflen;i++) 

			buf[i] = pelement->pchar[i];

	} else {
		ret = 1;
	}
	buf[i] = '\0';
	return ret;
}
// SlfMxGetNumberOfElements, Anzahl Element
//================================================================
int SlfMxGetNumberOfElements(SlfMATData * pelement) {
	
	return pelement->ncols*pelement->nrows;
}
// SlfMxGetNumberOfFields, Anzahl Feldelemente
//================================================================
int SlfMxGetNumberOfFields(SlfMATData * pelement) {

	return pelement->nfields;
}
// SlfMxGetFieldByNumber, Pointer der Datenstruktur mit index
// und ifield-te strukturelemet
//================================================================
SlfMATData *  SlfMxGetFieldByNumber(SlfMATData * pelement,int index,int ifield) {

	int ni,nf;
	SlfMATData *pdata;

	if( !pelement )
		return 0;
	index++;
	if( index > pelement->nrows*pelement->ncols )
		return 0;
	ifield++;
	if( ifield > pelement->nfields )
		return 0;

	pdata = pelement->psub;

	for(ni=pelement->nrows*pelement->ncols;ni>0;ni--) {

		for(nf=pelement->nfields;nf>0;nf--) {

			if( (ni == index) && (nf == ifield) )
				return pdata;

			pdata = pdata->pnext;
		}
	}


	return 0;
}
// SlfMxGetFieldNameByNumber, Name des ifield-ten StrukturElements
//================================================================
const char *SlfMxGetFieldNameByNumber(SlfMATData * pelement,int ifield) {

	if( pelement && ifield < pelement->nfields ) {

		return pelement->fieldnames[ifield];
	}
	return 0;
}
// SlfMxIsStruct, Prüft, ob Struktur
//================================================================
unsigned char SlfMxIsStruct(SlfMATData * pelement) {

	return  pelement->mxClass == mxSTRUCT_CLASS;
}
// SlfMxIsDouble, prüft ob double
//================================================================
unsigned char SlfMxIsDouble(SlfMATData * pelement) {

	return  pelement->mxClass == mxDOUBLE_CLASS;
}
// SlfMxIsChar, prüft ob char
//================================================================
unsigned char SlfMxIsChar(SlfMATData * pelement) {

	return  pelement->mxClass == mxCHAR_CLASS;
}
// SlfMxGetClassID, gibt class-id zurück
//================================================================
SlfMxClassID  SlfMxGetClassID(SlfMATData * pelement) {

	return pelement->mxClass;
}

// SlfMxFree, dummy-Fkt
//================================================================
void         SlfMxFree(char **ppMatDir) {}

//===============================================================================
//===============================================================================
// interne Funktionen
//===============================================================================
//===============================================================================
//======================================
// neue Struktur anlegen
//======================================
SlfMATFile * SlfMatNew() {

	SlfMATFile *pmatf = new SlfMATFile;

	if( !pmatf )
		return pmatf;

	pmatf->fid       = 0;
	pmatf->bytecount = 0;
	pmatf->swap      = 0;
	pmatf->filetype  = MATVUNKNOWN;
	pmatf->bufferlength = 0;
	pmatf->pbuffer      = 0;
	pmatf->pdata        = 0;
	pmatf->MatrixHasValues = 0;

	pmatf->ps8          = 0;
	pmatf->pu8          = 0;
	pmatf->ps16         = 0;
	pmatf->pu16         = 0;
	pmatf->ps32         = 0;
	pmatf->pu32         = 0;
	pmatf->pd           = 0;
	pmatf->ls8          = 0;
	pmatf->lu8          = 0;
	pmatf->ls16         = 0;
	pmatf->lu16         = 0;
	pmatf->ls32         = 0;
	pmatf->lu32         = 0;
	pmatf->ld           = 0;

	pmatf->nelements    = 0;
	pmatf->elementnamelist  = 0;
	pmatf->elementpointerlist  = 0;

	return pmatf;
}
//======================================
// Struktur löschen und File schliessen
//======================================
SlfMATFile * SlfMatDelete(SlfMATFile *pmatf) {
	
	int i;

	if( pmatf->fid ) {
		fclose(pmatf->fid);
	}

	if( pmatf->bufferlength )
		delete []pmatf->pbuffer;

	if(pmatf->ls8)
		delete []pmatf->ps8;
	if(pmatf->lu8)
		delete []pmatf->pu8;
	if(pmatf->ls16)
		delete []pmatf->ps16;
	if(pmatf->lu16)
		delete []pmatf->pu16;
	if(pmatf->ls32)
		delete []pmatf->ps32;
	if(pmatf->lu32)
		delete []pmatf->pu32;
	if(pmatf->ld)
		delete []pmatf->pd;


	SlfMatDeletePData(pmatf->pdata);

	if( pmatf->elementnamelist ) {
		for(i=0;i<pmatf->nelements;i++) {

			if( pmatf->elementnamelist[i] )
				delete []pmatf->elementnamelist[i];
		}
	}
	if( pmatf->elementnamelist )
		delete []pmatf->elementnamelist;
	if( pmatf->elementpointerlist )
		delete []pmatf->elementpointerlist;

	delete pmatf;

	return 0;
}
//======================================
// Daten-Struktur
//======================================
void SlfMatDeletePData(SlfMATData *pdata) {

	int i;

	if(pdata) {

		if( pdata->name )
			delete []pdata->name;

		for(i=0;i<pdata->nfields;i++) {

			if( pdata->fieldnames[i] )
				delete []pdata->fieldnames[i];
		}
		if( pdata->fieldnames )
			delete []pdata->fieldnames;

		if( pdata->pdi )
			delete []pdata->pdi;
		if( pdata->pdr )
			delete []pdata->pdr;

		if( pdata->pchar )
			delete []pdata->pchar;


		SlfMatDeletePData(pdata->psub);
		SlfMatDeletePData(pdata->pnext);

		delete pdata;

	}
}
//======================================
// Buffer der Struktur anlegen
//======================================
unsigned char *SlfMatSetBuffer(unsigned char *pbuffer,unsigned int *pbufferlength, unsigned int newlength) {

	if( !*pbufferlength && newlength) {
		
		pbuffer      = new unsigned char[newlength];
		if( !pbuffer ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatSetBuffer: Error no buffer allacation length: %i",newlength);
#else
			sprintf(SlfMatErrText,"SlfMatSetBuffer: Error no buffer allacation length: %i",newlength);
#endif
      return 0;
		}
		*pbufferlength = newlength;

	} else if( *pbufferlength && !newlength ) {

		delete []pbuffer;
		pbuffer      = 0;
		*pbufferlength = 0;

	} else if( *pbufferlength < newlength ) {

		unsigned char *pnewbuffer = new unsigned char[newlength];
		if( !pnewbuffer ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatSetBuffer: Error no buffer allacation length: %i",newlength);
#else
			sprintf(SlfMatErrText,"SlfMatSetBuffer: Error no buffer allacation length: %i",newlength);
#endif
      return 0;
		}
		memcpy(pnewbuffer,pbuffer,*pbufferlength);

		delete []pbuffer;
		pbuffer      = pnewbuffer;
		*pbufferlength = newlength;

	}
	return pbuffer;
}
//==========================================
// Entscheidet, welche Type von Matlab-File
//==========================================
void SlfMatDecideFileType(SlfMATFile *pmatf) {

	//Matlab Version 5 hat ersten 4 bytes mit acsii belegt
	//und in byte 125,126  befindet sich version 0x0100, wenn endian indicator = 0
	// bzw. version 0x0001 wenn endian indicator = 1 (sagt actave???)
	//============================================================================
	if( !(pmatf->pbuffer=SlfMatSetBuffer(pmatf->pbuffer,&pmatf->bufferlength,128)) ) {
		pmatf->filetype = MATVERROR;
		return;
	}

	// Header mit Text und binaer
	fread(pmatf->pbuffer,sizeof(unsigned char),128,pmatf->fid);


	if(  pmatf->pbuffer[0] > 31
	  && pmatf->pbuffer[0] < 127
	  && pmatf->pbuffer[1] > 31
	  && pmatf->pbuffer[1] < 127
	  && pmatf->pbuffer[2] > 31
	  && pmatf->pbuffer[2] < 127
	  && pmatf->pbuffer[3] > 31
	  && pmatf->pbuffer[3] < 127
	  && (  pmatf->pbuffer[127] == 'M' //endian indicator little endian
	     && pmatf->pbuffer[126] == 'I'
	     && pmatf->pbuffer[125] == 0x01
	     && pmatf->pbuffer[124] == 0x00
		 )
		 ||
		 (  pmatf->pbuffer[127] == 'I'    //endian indicator big endian
	     && pmatf->pbuffer[126] == 'M'
	     && pmatf->pbuffer[125] == 0x00
	     && pmatf->pbuffer[124] == 0x01
		 )
      ) {
	  
		pmatf->filetype = MATV5;
		if( pmatf->pbuffer[127] == 'I'    //endian indicator big endian
	     && pmatf->pbuffer[126] == 'M' )
			
			pmatf->swap     = 1;
		else
			pmatf->swap     = 0;

	} else {

		pmatf->filetype = MATVUNKNOWN;
	}

}
//==========================================
// Schriebt Type/Header von Matlab-File
//==========================================
void SlfMatWriteFileType(SlfMATFile *pmatf) {
 
  unsigned int i;
  char HeaderText[] = "MATLAB 5.0 MAT-file, Platform: PCWIN, Created on: Fri Feb 17 16:55:07 2012";

	//Matlab Version 5 hat ersten 4 bytes mit acsii belegt
	//und in byte 125,126  befindet sich version 0x0100, wenn endian indicator = 0
	// bzw. version 0x0001 wenn endian indicator = 1 (sagt actave???)
	//============================================================================
	if( !(pmatf->pbuffer=SlfMatSetBuffer(pmatf->pbuffer,&pmatf->bufferlength,128)) ) {
		pmatf->filetype = MATVERROR;
		return;
	}

  /* Leerzeichen vorbelegen */
  for(i=0;i<128;i++)
  {
    pmatf->pbuffer[i] = ' ';
  }
  /* HeaderText reinkopieren */
  memcpy(pmatf->pbuffer,HeaderText,sizeof(char)*strlen(HeaderText));

  // Version
  pmatf->pbuffer[124] = 0x00;
  pmatf->pbuffer[125] = 0x01;

  //endian indicator little endian
  pmatf->pbuffer[126] = 'I';
  pmatf->pbuffer[127] = 'M';

  // Schreiben
	fwrite(pmatf->pbuffer,sizeof(unsigned char),128,pmatf->fid);

  // Kennszeichnen
  pmatf->filetype = MATV5;
  pmatf->swap     = 0;

}
//==========================================
// Matlab bbinär-Format 
//==========================================
SlfMATFile * SlfMatReadMatV5(SlfMATFile *pmatf) {

	size_t nn;
	unsigned char bb;
	if( feof(pmatf->fid) ) // Ende erreicht
		return pmatf;

	nn=fread(&bb,1,1,pmatf->fid);
	if( nn == 0 )
		return pmatf;
	else
		fseek(pmatf->fid,-1,SEEK_CUR);

	if( !SlfMatReadMatV5NextDataElement(pmatf) )
		return 0;


	// Type erkennen
	if( pmatf->type == miMATRIX ) {

		unsigned char *pbuffer  = pmatf->pbuffer;
		unsigned int  bytecount = pmatf->bytecount;
		SlfMATData       *pdata;
		pdata  = new SlfMATData;

		if( !pdata ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatReadMatV5: data allaocation error");
#else
			sprintf(SlfMatErrText,"SlfMatReadMatV5: data allaocation error");
#endif
      return 0;
		}

		pdata->pnext = pmatf->pdata;
		pmatf->pdata = pdata;

		pdata->name       = 0;
		pdata->ncols      = 0;
		pdata->nrows      = 0;
		pdata->nfields    = 0;
		pdata->pdi        = 0;
		pdata->pdr        = 0;
		pdata->psub       = 0;
		pdata->psub       = 0;
		pdata->fieldnames = 0;
		pdata->pchar      = 0;

#if SLF_MAT_BUFFER_OUT == 1
		// Überprüfung des Buffers
		BufferOut(pmatf->pbuffer,pmatf->bytecount,"buffer.out");
#endif
		if( !SlfMatReadMatV5Matrix(pmatf,pdata,&pbuffer,&bytecount) )
			return 0;
	} else {

#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatReadMatV5: Datentyp %i nicht programmiert",pmatf->bytecount);
#else
		sprintf(SlfMatErrText,"SlfMatReadMatV5: Datentyp %i nicht programmiert",pmatf->bytecount);
#endif
    return 0;

	}

	pmatf->nelements++;

	
	return SlfMatReadMatV5(pmatf);
}
SlfMATFile *SlfMatReadMatV5NextDataElement(SlfMATFile *pmatf) {

	
	unsigned short type;
	unsigned short upper;
	unsigned int   temp;
	unsigned int   i;
	// den nächsten Tag 8 byte auslesen
	//=================================
	// ersten 2 Bytes type
	if( !SlfMatReadMatV5FRead(&type,sizeof(unsigned short),1,pmatf) )
		return 0;

	// zweiten 2 Bytes upper
	if( !SlfMatReadMatV5FRead(&upper,sizeof(unsigned short),1,pmatf) )
		return 0;

		
	// restlichen 4 Bytes
	if( !SlfMatReadMatV5FRead(&temp,sizeof(unsigned int),1,pmatf) )
		return 0;

	if (upper) { // small data format
		
		pmatf->bytecount = upper;
		memcpy(pmatf->pbuffer,&temp,upper);

	} else { // normal data format
		
		// zweiten 4 bytes sind die Länge
		pmatf->bytecount = temp;

		if( !(pmatf->pbuffer=SlfMatSetBuffer(pmatf->pbuffer,&pmatf->bufferlength,pmatf->bytecount)) )
			return 0;

		if( !SlfMatReadMatV5FRead(pmatf->pbuffer,sizeof(unsigned char),pmatf->bytecount,pmatf) )
			return 0;
	}

	// komprimierte Daten werden ausgepackt:
	if( type == miCOMPRESSED ) {

		if( !SlfMatReadMatV5CompressedData(pmatf) )
			return 0;

		// Es müssen mindestens 8 byte vorhanden sein
		if( pmatf->bytecount < 8 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatReadMatV5: read buffer length: %i too small",pmatf->bytecount);
#else
			sprintf(SlfMatErrText,"SlfMatReadMatV5: read buffer length: %i too small",pmatf->bytecount);
#endif
      return 0;
		}
		// Tag aus den dekomriemierten Daten auslesen
		//BufferOut(pmatf->pbuffer,pmatf->bytecount,"prot1.dat");
		upper = 0;
		upper = (pmatf->pbuffer[3]<<8)&0xff00;
		upper |= pmatf->pbuffer[2];
		type  = 0;
		type  = (pmatf->pbuffer[1]<<8)&0xff00;
		type  |= pmatf->pbuffer[0];

		if (upper) { // small data format
			
			for(i=0;i<pmatf->bytecount-4;i++)
				pmatf->pbuffer[i] = pmatf->pbuffer[i+4];			
			pmatf->bytecount = upper;

		} else { // normal data format

			memcpy(&temp,&pmatf->pbuffer[4],sizeof(unsigned int));
			for(i=0;i<pmatf->bytecount-8;i++)
				pmatf->pbuffer[i] = pmatf->pbuffer[i+8];			
			pmatf->bytecount = temp;
		}
	}
	pmatf->type = type;
	return pmatf;
}
SlfMATFile * SlfMatReadMatV5CompressedData(SlfMATFile *pmatf) {

    int err;

	// buffer für unkomprimiert
	unsigned int  luncomp  = pmatf->bytecount*30;
	unsigned char *puncomp = 0;

    z_stream d_stream; /* decompression stream */

	unsigned char flag_run = 1;

	while(flag_run) {

		flag_run = 0;

		if( !puncomp )
			puncomp = new unsigned char[luncomp];

		d_stream.zalloc = (alloc_func)0;
		d_stream.zfree = (free_func)0;
		d_stream.opaque = (voidpf)0;

		d_stream.next_in  = pmatf->pbuffer;
		d_stream.avail_in = 0;
		d_stream.next_out = puncomp;


		err = inflateInit(&d_stream);
		CHECK_ERR(err, "inflateInit");

		while (d_stream.total_in < pmatf->bytecount) {

			// Ausgabe vergößern
			if( d_stream.total_out >= luncomp ) {

				flag_run = 1;

				//unsigned char *puncompnew = new unsigned char[luncomp*2];
				//memcpy(puncompnew,puncomp,luncomp);

				delete []puncomp;

				//puncomp = puncompnew;
				puncomp = 0;
				luncomp *= 2;

				break;

			}

			d_stream.avail_in = d_stream.avail_out = 1; /* force small buffers */
			err = inflate(&d_stream, Z_NO_FLUSH);
			if (err == Z_STREAM_END) break;
			if( err != Z_OK )
				CHECK_ERR(err, "inflate");
		}

		err = inflateEnd(&d_stream);
		CHECK_ERR(err, "inflateEnd");
	}
	if( !(pmatf->pbuffer=SlfMatSetBuffer(pmatf->pbuffer,&pmatf->bufferlength,d_stream.total_out)) )
		return 0;

	memcpy(pmatf->pbuffer,puncomp,d_stream.total_out);

	pmatf->bytecount = d_stream.total_out;

	delete []puncomp;

	return pmatf;

}
SlfMATFile * SlfMatReadMatV5Matrix(SlfMATFile *pmatf,SlfMATData  *pdata,unsigned char **ppbuffer,unsigned int *pbytecount) {

	// 1. Subelement Classdefinition
	//==============================
	if( !(SlfMatGetInfo(pmatf,ppbuffer,pbytecount,pdata)) )
		return 0;

	switch(pdata->mxClass) {

	case mxDOUBLE_CLASS:


		if( !SlfMatGetDouble(pmatf,ppbuffer,pbytecount,pdata) )
		     return 0;

		break;
	case mxCHAR_CLASS:


		if( !SlfMatGetChar(pmatf,ppbuffer,pbytecount,pdata) )
		     return 0;

		break;
	case mxCELL_CLASS:


		if( !SlfMatGetCell(pmatf,ppbuffer,pbytecount,pdata) )
		     return 0;

		break;
	case mxSTRUCT_CLASS:


		if( !SlfMatGetStruct(pmatf,ppbuffer,pbytecount,pdata) )
		     return 0;

		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatReadMatV5Matrix: data class unknown mxClass=%i",pdata->mxClass);
#else
		sprintf(SlfMatErrText,"SlfMatReadMatV5Matrix: data class unknown mxClass=%i",pdata->mxClass);
#endif
    return 0;
	}

	return pmatf;
}
char SlfMatReadMatV5FRead(void *pval,size_t lsize,size_t length,SlfMATFile *pmatf) {

	size_t num = fread(pval,lsize,length,pmatf->fid);

	if( num != length ) {
		
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatReadMatV5FRead: Es konnten nicht alle bytes eingelsen werden");
#else
		sprintf(SlfMatErrText,"SlfMatReadMatV5FRead: Es konnten nicht alle bytes eingelsen werden");
#endif
    return 0;
	}
	if( pmatf->swap ) {

		unsigned char *pchar = (unsigned char *)pval;
		unsigned char dum;
        size_t i    = 0;
		
		while(i+1<num) {
			dum = pchar[i];
			pchar[i]   = pchar[i+1];
			pchar[i+1] = dum;
			i          += 2;
		}
	}
	return 1;
}
unsigned char SlfMatGetInfo(SlfMATFile *pmatf, unsigned char **ppbuffer
		           	       ,unsigned int *pbytecount,SlfMATData  *pdata) {

	unsigned short	type = 0;
	unsigned short	upper = 0;
	unsigned int    j;

	//Array flags
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN) 
		return 0;

	switch(type) {
	case miUINT32:

		if( pmatf->lu32 < 2 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetInfo: lu32=%i < 2 dimension arry",pmatf->lu32);
#else
			sprintf(SlfMatErrText,"SlfMatGetInfo: lu32=%i < 2 dimension arry",pmatf->lu32);
#endif
      return 0;
		}
		pdata->mxClass = (SlfMxClassID)(pmatf->pu32[0]&0xff);
		pdata->logical = (pmatf->pu32[0]&0x0200)>>9;
		pdata->global  = (pmatf->pu32[0]&0x0400)>>10;
		pdata->complex = (mxComplexity)((pmatf->pu32[0]&0x0800)>>11);
		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetInfo: type=%i for array flags is not mxUINT32",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetInfo: type=%i for array flags is not mxUINT32",type);
#endif
    return 0;
	}

	//Dimensionarray
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
		return 0;

	switch(type) {
	case miINT32:

		if( pmatf->ls32 < 2 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetInfo: ls32=%i < 2 dimension arry",pmatf->ls32);
#else
			sprintf(SlfMatErrText,"SlfMatGetInfo: ls32=%i < 2 dimension arry",pmatf->ls32);
#endif
      return 0;
		}
		pdata->nrows = pmatf->ps32[0];
		pdata->ncols = pmatf->ps32[1];
		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
    sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetInfo: type=%i for dimension is not mxINT32",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetInfo: type=%i for dimension is not mxINT32",type);
#endif
    return 0;
	}

	
	//Arrayname
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
		return 0;

	switch(type) {
	case miINT8:

		pdata->name = new char[pmatf->ls8+1];
	
		for(j=0;j<pmatf->ls8;j++)
			pdata->name[j] = pmatf->ps8[j];
				
		pdata->name[j] = '\0';
		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
 		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetInfo: type=%i for array name is not mxINT8",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetInfo: type=%i for array name is not mxINT8",type);
#endif
    return 0;
	}

	return 1;
}
char SlfMatGetDouble(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata) {


	unsigned short type;
	unsigned int   i;	

	//Realpart
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
		return 0;

	switch(type) {
	case miUINT8:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->lu8 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint8-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->lu8);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint8-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->lu8);
#endif
      return 0;
		}
		if( pdata->pdr )
			delete []pdata->pdr;

		pdata->pdr = new double[pmatf->lu8];
		for(i=0;i<pmatf->lu8;i++) 
			pdata->pdr[i]=pmatf->pu8[i];
		break;
	case miINT8:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls8 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int8-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls8);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int8-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls8);
#endif
      return 0;
		}
		if( pdata->pdr )
			delete []pdata->pdr;

		pdata->pdr = new double[pmatf->ls8];
		for(i=0;i<pmatf->ls8;i++) 
			pdata->pdr[i]=pmatf->ps8[i];
		break;
	case miUINT16:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->lu16 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint16-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->lu16);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint16-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->lu16);
#endif
      return 0;
		}
		if( pdata->pdr )
			delete []pdata->pdr;

		pdata->pdr = new double[pmatf->lu16];
		for(i=0;i<pmatf->lu16;i++) 
			pdata->pdr[i]=pmatf->pu16[i];
		break;
	case miINT16:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls16 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int16-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls16);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int16-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls16);
#endif
        return 0;
		}
		if( pdata->pdr )
			delete []pdata->pdr;

		pdata->pdr = new double[pmatf->ls16];
		for(i=0;i<pmatf->ls16;i++) 
			pdata->pdr[i]=pmatf->ps16[i];
		break;
	case miUINT32:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->lu32 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint32-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->lu32);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint32-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->lu32);
#endif
      return 0;
		}
		if( pdata->pdr )
			delete []pdata->pdr;

		pdata->pdr = new double[pmatf->lu32];
		for(i=0;i<pmatf->lu32;i++) 
			pdata->pdr[i]=pmatf->pu32[i];
		break;
	case miINT32:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls32 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int32-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls32);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int32-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls32);
#endif
      return 0;
		}
		if( pdata->pdr )
			delete []pdata->pdr;

		pdata->pdr = new double[pmatf->ls32];
		for(i=0;i<pmatf->ls32;i++) 
			pdata->pdr[i]=pmatf->ps32[i];
		break;
	case miDOUBLE:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ld ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich double-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ld);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich double-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ld);
#endif
      return 0;
		}
		pdata->pdr=pmatf->pd;
		pmatf->ld = 0;
		pmatf->pd = 0;
		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: type=%i for real part array is not mxDOUBLE",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetDouble: type=%i for real part array is not mxDOUBLE",type);
#endif
    return 0;
	}

	//Imagpart
	if( pdata->complex ) {
		if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
			return 0;

		switch(type) {
		case miUINT8:

			if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->lu8 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint8-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->lu8);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint8-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->lu8);
#endif
        return 0;
			}
			if( pdata->pdi )
				delete []pdata->pdi;

			pdata->pdi = new double[pmatf->lu8];
			for(i=0;i<pmatf->lu8;i++) 
				pdata->pdi[i]=pmatf->pu8[i];
			break;
		case miINT8:

			if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls8-1 ) { //angehängte null
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int8-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ls8-1);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int8-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ls8-1);
#endif
        return 0;
			}
			if( pdata->pdi )
				delete []pdata->pdi;

			pdata->pdi = new double[pmatf->ls8-1];
			for(i=0;i<pmatf->ls8-1;i++) 
				pdata->pdi[i]=pmatf->ps8[i];
			break;
		case miUINT16:

			if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->lu16 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint16-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->lu16);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint16-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->lu16);
#endif
        return 0;
			}
			if( pdata->pdi )
				delete []pdata->pdi;

			pdata->pdi = new double[pmatf->lu16];
			for(i=0;i<pmatf->lu16;i++) 
				pdata->pdi[i]=pmatf->pu16[i];
			break;
		case miINT16:

			if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls16 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int16-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ls16);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int16-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ls16);
#endif
        return 0;
			}
			if( pdata->pdi )
				delete []pdata->pdi;

			pdata->pdi = new double[pmatf->ls16];
			for(i=0;i<pmatf->ls16;i++) 
				pdata->pdi[i]=pmatf->ps16[i];
			break;
		case miUINT32:

			if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->lu32 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint32-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->lu32);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich uint32-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->lu32);
#endif
        return 0;
			}
			if( pdata->pdi )
				delete []pdata->pdi;

			pdata->pdi = new double[pmatf->lu32];
			for(i=0;i<pmatf->lu32;i++) 
				pdata->pdi[i]=pmatf->pu32[i];
			break;
		case miINT32:

			if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls32 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int32-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ls32);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int32-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ls32);
#endif
        return 0;
			}
			if( pdata->pdi )
				delete []pdata->pdi;

			pdata->pdi = new double[pmatf->ls32];
			for(i=0;i<pmatf->ls32;i++) 
				pdata->pdi[i]=pmatf->ps32[i];
			break;
		case miDOUBLE:

			if( (unsigned int)pdata->nrows*pdata->ncols != pmatf->ld ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich double-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ld);
#else
				sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich double-Länge=%i"
					   ,pdata->nrows,pdata->ncols,pmatf->ld);
#endif
          return 0;
			}
			pdata->pdi=pmatf->pd;
			pmatf->ld = 0;
			pmatf->pd = 0;
			break;
		default:
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: type=%i for imag part array is not mxDOUBLE",type);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: type=%i for imag part array is not mxDOUBLE",type);
#endif
      return 0;
		}
	}
	return 1;
}
char SlfMatGetChar(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata) {


	unsigned short type;
	unsigned int   i;	

	//char
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
		return 0;

	switch(type) {
	case miUTF8:
	case miINT8:

		if( (unsigned int)pdata->nrows*(unsigned int)pdata->ncols != pmatf->ls8 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int8-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls8);
#else
			sprintf(SlfMatErrText,"SlfMatGetDouble: dimension nrows=%i x ncols=%i ungleich int8-Länge=%i"
				   ,pdata->nrows,pdata->ncols,pmatf->ls8);
#endif
      return 0;
		}

		if( pdata->pchar )
			delete []pdata->pchar;
		

		pdata->pchar = new char[pmatf->ls8];
		for(i=0;i<pmatf->ls8;i++) {

			pdata->pchar[i] = pmatf->ps8[i];

		}

		// kein Abschluß, wird erst bei übergabe gesetzt

		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetDouble: type=%i for real part array is not mxDOUBLE",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetDouble: type=%i for real part array is not mxDOUBLE",type);
#endif
    return 0;
	}

	return 1;
}
char SlfMatGetCell(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata) {


	unsigned short type;
	SlfMATData *psub;

	for(signed int i=0;i<pdata->nrows*pdata->ncols;i++) {

		//Cellelement
		if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
			return 0;

		switch(type) {
		case miMATRIX:

			psub=new SlfMATData;
			if( !psub ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetCell: data allacation error");
#else
				sprintf(SlfMatErrText,"SlfMatGetCell: data allacation error");
#endif
        return 0;
			}

			psub->pnext = pdata->psub;
			pdata->psub = psub;

			psub->name   = 0;
			psub->ncols  = 0;
			psub->nfields= 0;
			psub->nrows  = 0;
			psub->pdi    = 0;
			psub->pdr    = 0;
			psub->psub   = 0;
			psub->psub = 0;
			pdata->fieldnames = 0;
			pdata->pchar      = 0;

			if( !SlfMatReadMatV5Matrix(pmatf,psub,ppbuffer,pbytecount) )
				return 0;

			break;
		default:
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetCell: type=%i for cell array usually miMATRIX(%i)",type,miMATRIX);
#else
			sprintf(SlfMatErrText,"SlfMatGetCell: type=%i for cell array usually miMATRIX(%i)",type,miMATRIX);
#endif
      return 0;
		}
	}
	return 1;
}
char SlfMatGetStruct(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount,SlfMATData *pdata) {


	unsigned short type;
	SlfMATData *psub;
	signed int flength;
	char **ppnames=0;
	signed int i,j,icount;

	// fieldnames rauslesen
	//=====================
	// Länge der NAmen
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
		return 0;
	switch(type) {
	case miINT32:

		flength = pmatf->ps32[0];
		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetStruct: type=%i for flength is not mxINT32",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetStruct: type=%i for flength is not mxINT32",type);
#endif
    return 0;
	}


	// Namenauslesen
	if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
		return 0;
	switch(type) {
	case miINT8:

		pdata->nfields = pmatf->ls8/flength;
		pdata->fieldnames = new char*[pdata->nfields];
		icount = 0;
		for(i=0;i<pdata->nfields;i++) {

			pdata->fieldnames[i] = new char[flength+1];

			for(j=0;j<flength;j++)
				pdata->fieldnames[i][j] = pmatf->ps8[icount++];
				
			pdata->fieldnames[i][j] = '\0';
		}
		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetStruct: type=%i for fieldnames is not mxINT8",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetStruct: type=%i for fieldnames is not mxINT8",type);
#endif
    return 0;
	}

	for(i=0;i<pdata->nrows*pdata->ncols;i++) {

		for(j=0;j<pdata->nfields;j++) {
		
			//
			if( (type=SlfMatGetSubelement(pmatf,ppbuffer,pbytecount)) == miUNKNOWN)
				return 0;

			switch(type) {
			case miMATRIX:

				psub=new SlfMATData;
				if( !psub ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
					sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetCell: data allacation error");
#else
					sprintf(SlfMatErrText,"SlfMatGetCell: data allacation error");
#endif
          return 0;
				}

				psub->pnext = pdata->psub;
				pdata->psub = psub;

				psub->name   = 0;
				psub->ncols  = 0;
				psub->nrows  = 0;
				psub->nfields= 0;
				psub->pdi    = 0;
				psub->pdr    = 0;
				psub->psub   = 0;
				psub->fieldnames = 0;
				psub->pchar      = 0;

				if( pmatf->MatrixHasValues ) {
					if( !SlfMatReadMatV5Matrix(pmatf,psub,ppbuffer,pbytecount) )
						return 0;
				}
				break;
			default:
#if _MSC_VER > MSC_VER_BIS_VS2005
				sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetStruct: type=%i for struct array usually miMATRIX(%i)",type,miMATRIX);
#else
				sprintf(SlfMatErrText,"SlfMatGetStruct: type=%i for struct array usually miMATRIX(%i)",type,miMATRIX);
#endif
        return 0;
			}
		}
	}
	return 1;
}
unsigned short	SlfMatGetSubelement(SlfMATFile *pmatf,unsigned char **ppbuffer,unsigned int *pbytecount) {

	unsigned short	type = 0;
	unsigned short	upper = 0;
	unsigned char   *pbuf;
	unsigned int	bcount;
	unsigned int    typelength;
	unsigned char   *pbuffer = *ppbuffer;

	upper = (pbuffer[3]<<8) | pbuffer[2];
	type  = (pbuffer[1]<<8) | pbuffer[0];

	if (upper) { // small data format
	
		bcount = upper;
		if( *pbytecount < 8 ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetSubelement: buffer length for small data element too small bytecount=%i",*pbytecount);
#else
			sprintf(SlfMatErrText,"SlfMatGetSubelement: buffer length for small data element too small bytecount=%i",*pbytecount);
#endif
      return miUNKNOWN;
		}
		pbuf = &pbuffer[4];

	} else { // normal data format

		memcpy(&bcount,&pbuffer[4],sizeof(unsigned int));
		if( *pbytecount < (8+bcount) ) {
#if _MSC_VER > MSC_VER_BIS_VS2005
			sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetClass: buffer length data element too small bytecount=%i",*pbytecount);
#else
			sprintf(SlfMatErrText,"SlfMatGetClass: buffer length data element too small bytecount=%i",*pbytecount);
#endif
      return miUNKNOWN;
		}
		pbuf = &pbuffer[8];
	}


	switch(type) {

	case miINT8:
	case miUTF8:

		if( pmatf->ls8 )
			delete []pmatf->ps8;
		pmatf->ps8 = new signed char[bcount];
		pmatf->ls8 = bcount;
		memcpy(pmatf->ps8,pbuf,bcount);

		break;
	case miUINT8:

		if( pmatf->lu8 )
			delete []pmatf->pu8;
		pmatf->pu8 = new unsigned char[bcount];
		pmatf->lu8 = bcount;
		memcpy(pmatf->pu8,pbuf,bcount);

		break;
	case miINT16:

		typelength = bcount/sizeof(signed short);
		if( pmatf->ls16 )
			delete []pmatf->ps16;
		pmatf->ps16 = new signed short[typelength];
		pmatf->ls16 = typelength;
		memcpy(pmatf->ps16,pbuf,bcount);
		break;
	case miUINT16:

		typelength = bcount/sizeof(unsigned short);
		if( pmatf->lu16 )
			delete []pmatf->pu16;
		pmatf->pu16 = new unsigned short[typelength];
		pmatf->lu16 = typelength;
		memcpy(pmatf->pu16,pbuf,bcount);
		break;
	case miINT32:

		typelength = bcount/sizeof(signed int);
		if( pmatf->ls32 )
			delete []pmatf->ps32;
		pmatf->ps32 = new signed int[typelength];
		pmatf->ls32 = typelength;
		memcpy(pmatf->ps32,pbuf,bcount);

		break;
	case miUINT32:

		typelength = bcount/sizeof(unsigned int);
		if( pmatf->lu32 )
			delete []pmatf->pu32;
		pmatf->pu32 = new unsigned int[typelength];
		pmatf->lu32 = typelength;
		memcpy(pmatf->pu32,pbuf,bcount);

		break;
	case miDOUBLE:

		typelength = bcount/sizeof(double);
		if( pmatf->ld )
			delete []pmatf->pd;
		pmatf->pd = new double[typelength];
		pmatf->ld = typelength;
		memcpy(pmatf->pd,pbuf,bcount);
		break;

	case miMATRIX:

		if( bcount > 0 )
			pmatf->MatrixHasValues = 1;
		else
			pmatf->MatrixHasValues = 0;

		break;
	default:
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatGetSubelement: Type=%i kann nicht umgesetzt werden",type);
#else
		sprintf(SlfMatErrText,"SlfMatGetSubelement: Type=%i kann nicht umgesetzt werden",type);
#endif
    return miUNKNOWN;
	}
	if (upper) { // small data format

		*pbytecount -= 8;
		*ppbuffer = &pbuffer[8];
	} else {

		if( (bcount/8)*8 < bcount )
			bcount = (bcount/8+1)*8;

		if( type == miMATRIX ) {// nur Kopf weggeschitten
		*	pbytecount -= (8);
			*ppbuffer = &pbuffer[8];
		} else {
			*pbytecount -= (8+bcount);
			*ppbuffer = &pbuffer[8+bcount];
		}
	}

	return type;
}

SlfMATData *  SlfMxCreateDoubleMatrix(int m, int n,mxComplexity ComplexFlag)
{
		SlfMATData       *pdata;
		pdata  = new SlfMATData;

    if( !pdata ) 
    {
#if _MSC_VER > MSC_VER_BIS_VS2005
  		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMxCreateDoubleMatrix: Cannot create SlfMATData-structure");
#else
	  	sprintf(SlfMatErrText,"SlfMxCreateDoubleMatrix: Cannot create SlfMATData-structure");
#endif

      return pdata;
    }
    pdata->complex    = ComplexFlag;
    pdata->logical    = 0;
    pdata->global     = 1;
 		pdata->name       = 0;
		pdata->psub       = 0;
		pdata->psub       = 0;
		pdata->nfields    = 0;
		pdata->fieldnames = 0;
		pdata->pchar      = 0;

    pdata->mxClass    = mxDOUBLE_CLASS;
		pdata->nrows      = m;
		pdata->ncols      = n;
    pdata->pdr        = new double[n*m];
    if( !pdata->pdr )
    {
#if _MSC_VER > MSC_VER_BIS_VS2005
  		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMxCreateDoubleMatrix: mem alloc Problem real part nrows(m)=%i,ncols(n)=%i\n",m,n);
#else
  		sprintf(SlfMatErrText,"SlfMxCreateDoubleMatrix: mem alloc Problem real part nrows(m)=%i,ncols(n)=%i\n",m,n);
#endif
      delete pdata;
      pdata = 0;
      return pdata;
    }
    if( pdata->complex == mxCOMPLEX )
    {
		  pdata->pdi        = new double[n*m];
      if( !pdata->pdi )
      {
#if _MSC_VER > MSC_VER_BIS_VS2005
  		  sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMxCreateDoubleMatrix: mem alloc Problem real part nrows(m)=%i,ncols(n)=%i\n",m,n);
#else
  		  sprintf(SlfMatErrText,"SlfMxCreateDoubleMatrix: mem alloc Problem real part nrows(m)=%i,ncols(n)=%i\n",m,n);
#endif
      }
        delete []pdata->pdr;
        delete pdata;
        pdata = 0;
        return pdata;
    }
    else
    {
      pdata->pdi        = 0;
    }

    return pdata;
}
void SlfMxDestroyArray(SlfMATData *pd)
{
  if(pd->pdr)
  {
    delete []pd->pdr;
    pd->pdr = 0;
  }
  if(pd->pdi)
  {
    delete []pd->pdi;
    pd->pdi = 0;
  }
  if(pd->pchar)
  {  
    delete []pd->pchar;
    pd->pchar = 0;
  }
  if( pd->name )
  {
    delete []pd->name;
    pd->name = 0;
  }
  if( pd->nfields > 0 )
  {
    for(int i=0;i<pd->nfields;i++)
    {
      delete []pd->fieldnames[i];
    }
    delete []pd->fieldnames;
  }
  pd->complex = mxREAL;
  pd->global  = 0;
  pd->logical = 0;
  pd->mxClass = mxUNKNOWN_CLASS;
  pd->ncols   = 0;
  pd->nrows   = 0;
  pd->psub    = 0;
}
int SlfMatPutVariable(SlfMATFile *pmatf, const char *name, const SlfMATData *pm)
{
  if( pm->mxClass == mxDOUBLE_CLASS )
  {
    return SlfMatPutDouble(pmatf,name,pm);
  }
  else
  {
#if _MSC_VER > MSC_VER_BIS_VS2005
		sprintf_s(SlfMatErrText,SLF_MAT_ERR_TEXT_SIZE,"SlfMatPutVariable: mxClass=%i kann nicht umgesetzt werden",pm->mxClass);
#else
		sprintf(SlfMatErrText,"SlfMatPutVariable: mxClass=%i kann nicht umgesetzt werden",pm->mxClass);
#endif
    return 1;
  }

  return 0;
}
int SlfMatPutDouble(SlfMATFile *pmatf, const char *name, const SlfMATData *pm)
{
  size_t        n              = strlen(name);
  unsigned int  ndata          = pm->nrows*pm->ncols;
  unsigned int  number_of_byte = 0;
  unsigned long word;
  unsigned long offset;
  unsigned long length;

  // Array Flags
  number_of_byte += 16;  // 8 Header + 8 Data
  // Dimension Array
  number_of_byte += 16;  // 8 Header + 8 Data
  // NAme
  if( n < 5 ) //small format
  {
    number_of_byte += 8; // Header+Data
  }
  else 
  {
    number_of_byte += 8;       //Header
    number_of_byte += (n/8)*8; // Data
    if( n != ((n/8)*8) ) number_of_byte += 8;
  }

  // real Data 
  number_of_byte += 8;       //Header
  number_of_byte += ndata*8; // Data

  // imag. Data
  if( pm->complex == mxCOMPLEX )
  {
    number_of_byte += 8;       //Header
    number_of_byte += ndata*8; // Data
  }
  pmatf->bytecount = number_of_byte + 8;  // add Header Tag
  // bytes allocieren
	if( !(pmatf->pbuffer=SlfMatSetBuffer(pmatf->pbuffer,&pmatf->bufferlength,pmatf->bytecount)) )
    return 2;
  //=========================================
  // Fill byte-structure
  //--------------------
  // Header Tag DataType
  word   = miMATRIX;
  offset = 0;
  length = sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // Header Tag bytes
  word   =  number_of_byte;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);

  // ArrayFlags DataType
  word   =  miUINT32;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // ArrayFlags byte
  word   =  8;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // ArrayFlags data first 4 bytes
  word   =  mxDOUBLE_CLASS;

  if( pm->logical )               word |= 0x0200;
  if( pm->global )                word |= 0x0400;
  if( pm->complex == mxCOMPLEX )  word |= 0x0800;
  
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // ArrayFlags data second 4 bytes
  word   =  0;  // undefined
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);

  // Dimensions Array DataType
  word   =  miINT32;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // Dimensions Array byte
  word   =  8;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // Dimensions Array data first 4 bytes
  word   =  pm->nrows;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // Dimensions Array data second 4 bytes
  word   =  pm->ncols;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);

  // Name
  if( n < 5 ) //small format
  {
    // Name DataType+strlen
    word   =  miINT8;
    word   |= ((unsigned char)n)<<16;
    offset += length;
    length =  sizeof(word);
    memcpy(pmatf->pbuffer+offset,&word,length);

    // Name Daten
    offset += length;
    length =  4;
    memcpy(pmatf->pbuffer+offset,name,n);
  }
  else 
  {
    // Name DataType
    word   =  miINT8;
    offset += length;
    length =  sizeof(word);
    memcpy(pmatf->pbuffer+offset,&word,length);
    // Name byte
    word   =  n;
    offset += length;
    length =  sizeof(word);
    memcpy(pmatf->pbuffer+offset,&word,length);
    // Name Daten
    offset += length;
    length =  (n/8)*8;
    if( n != ((n/8)*8) ) length += 8;
    memcpy(pmatf->pbuffer+offset,name,n);
  }

  // Real Data
  // DataType
  word   =  miDOUBLE;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  //  byte
  word   =  ndata*8;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // data
  for(unsigned int i=0;i<ndata;i++)
  {
    offset += length;
    length = sizeof(double);
    memcpy(pmatf->pbuffer+offset,&(pm->pdr[i]),length);
  }
  // imag. Data
  if( pm->complex == mxCOMPLEX )
  {
    // DataType
    word   =  miDOUBLE;
    offset += length;
    length =  sizeof(word);
    memcpy(pmatf->pbuffer+offset,&word,length);
    //  byte
    word   =  ndata*8;
    offset += length;
    length =  sizeof(word);
    memcpy(pmatf->pbuffer+offset,&word,length);
    // data
    for(unsigned int i=0;i<ndata;i++)
    {
      offset += length;
      length = sizeof(double);
      memcpy(pmatf->pbuffer+offset,&(pm->pdi[i]),length);
    }
  }
#if SLF_MAT_USE_COMPRESSION != 0

  unsigned char *pcompressed = new unsigned char[pmatf->bytecount];
  unsigned int  ncompressed  = 0;
  if( (n=SlfMatWriteBinaryDataCompressed(pmatf->pbuffer,pmatf->bytecount,pcompressed,&ncompressed)) )
    return n;

  // Store compressed DAta
  //======================
  //=========================================
  // Fill byte-structure
  //--------------------
  // Header Tag DataType
  word   = miCOMPRESSED;
  offset = 0;
  length = sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // Header Tag bytes
  word   =  ncompressed;
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,&word,length);
  // data
  offset += length;
  length =  sizeof(word);
  memcpy(pmatf->pbuffer+offset,pcompressed,ncompressed);

  delete []pcompressed;

  // Daten schreiben
  fwrite(pmatf->pbuffer,sizeof(unsigned char),ncompressed+2*sizeof(word),pmatf->fid);

#else
  // Daten schreiben
  fwrite(pmatf->pbuffer,sizeof(unsigned char),pmatf->bytecount,pmatf->fid);
#endif

  return 0;
}
int SlfMatWriteBinaryDataCompressed(unsigned char *pun,unsigned int nun,unsigned char *pcom,unsigned int *pncom)
{
  z_stream strm;
  int ret;

  /* allocate deflate state */
  strm.zalloc = Z_NULL;
  strm.zfree  = Z_NULL;
  strm.opaque = Z_NULL;
  ret = deflateInit(&strm, Z_DEFAULT_COMPRESSION);
  if (ret != Z_OK)
      return ret;

  strm.avail_in = nun;
  strm.next_in  = pun;

  /* run deflate() on input until output buffer not full, finish
     compression if all of source has been read in */
  do 
  {
    strm.avail_out = nun;
    strm.next_out  = pcom;
    ret = deflate(&strm, Z_FINISH);    /* no bad return value */
    if(ret == Z_STREAM_ERROR)
      return ret;  /* state not clobbered */
    *pncom = nun - strm.avail_out;
  } while (strm.avail_out == 0);


  /* clean up and return */
  (void)deflateEnd(&strm);
  return Z_OK;
}
