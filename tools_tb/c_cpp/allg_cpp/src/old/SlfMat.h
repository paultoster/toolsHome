// SlfMat.h
//=========
//
// nachgeildete Matlabfunktionen
//
#ifndef SLF_MAT_H_INCLUDED
#define SLF_MAT_H_INCLUDED

#define SLF_MAT_BUFFER_OUT 0      // Schreibt für debug-Zwecke den Buffer raus in eine Datei buffer.out
		              					      // sollte 0 sein
#define SLF_MAT_USE_COMPRESSION 1 // Use compression for output

#include <stdio.h>
#include "SlfBasic.h"

#define smatOpen        SlfMatOpen
#define smatClose       SlfMatClose
#define	smatGetDir      SlfMatGetDir
#define smatGetVariable SlfMatGetVariable
#define sMATFile        SlfMATFile
#define smxArray        SlfMATData
#define smxClassID      SlfMxClassID
#define smwSize         int

#define smxGetM                 SlfMxGetM
#define smxGetN                 SlfMxGetN
#define smxGetPr                SlfMxGetPr
#define smxGetNumberOfElements  SlfMxGetNumberOfElements
#define smxGetNumberOfFields    SlfMxGetNumberOfFields
#define smxGetFieldByNumber     SlfMxGetFieldByNumber
#define smxGetFieldNameByNumber SlfMxGetFieldNameByNumber
#define smxFree                 SlfMxFree
#define smxGetClassID           SlfMxGetClassID
#define smxIsStruct             SlfMxIsStruct
#define smxIsDouble             SlfMxIsDouble
#define smxIsChar               SlfMxIsChar


// MAT-File Data Types
#define miUNKNOWN       0
#define miINT8			1
#define miUINT8			2
#define miINT16			3
#define miUINT16		4
#define miINT32			5
#define miUINT32		6
#define miSINGLE		7
#define miDOUBLE		9
#define miINT64			12
#define miUINT64		13
#define miMATRIX		14
#define miCOMPRESSED	15
#define miUTF8			16
#define miUTF16			17
#define miUTF32			18

#if 0
#define mxCELL_CLASS    1
#define mxSTRUCT_CLASS  2
#define mxOBJECT_CLASS  3
#define mxCHAR_CLASS    4
#define mxSPARSE_CLASS  5
#define mxDOUBLE_CLASS  6
#define mxSINGLE_CLASS  7
#define mxINT8_CLASS    8
#define mxUINT8_CLASS   9
#define mxINT16_CLASS   10
#define mxUINT16_CLASS  11
#define mxINT32_CLASS   12
#define mxUINT32_CLASS  13
#endif
typedef enum {
	mxUNKNOWN_CLASS = 0,
	mxCELL_CLASS,
	mxSTRUCT_CLASS,
	mxLOGICAL_CLASS,
	mxCHAR_CLASS,
	mxVOID_CLASS,
	mxDOUBLE_CLASS,
	mxSINGLE_CLASS,
	mxINT8_CLASS,
	mxUINT8_CLASS,
	mxINT16_CLASS,
	mxUINT16_CLASS,
	mxINT32_CLASS,
	mxUINT32_CLASS,
	mxINT64_CLASS,
	mxUINT64_CLASS,
	mxFUNCTION_CLASS,
  mxOPAQUE_CLASS,
	mxOBJECT_CLASS
} SlfMxClassID;

typedef enum mxComplexity {mxREAL=0, mxCOMPLEX};

typedef
struct tag_SlfMATData {

	SlfMxClassID mxClass;
	unsigned char     logical;
	unsigned char     global;
	enum mxComplexity complex;

	int          nrows;
	int          ncols;
	int          nfields;

	char        **fieldnames;         // Feldnamen, wenn struct

	char         *name;               // Name, wenn array

	double       *pdr;                // Realpart double
	double       *pdi;                // Imagpart double

	char         *pchar;             // char-element mit länge nrows*ncols ohne '\0' (strings)


	tag_SlfMATData  *psub;               // Bei cells,structs Unterelemente

	tag_SlfMATData *pnext;
} SlfMATData;

typedef
struct tag_SlfMATFile {
	FILE			*fid;
	unsigned char   filetype;
	char            swap;				// byteswap-indicator
	unsigned int	bytecount;			// aktuelle byte-Länge
	unsigned char   *pbuffer;
	unsigned int    bufferlength;
	unsigned short  type;              // type des aktuellen Datenelememts z.B. miMATRIX
	SlfMATData      *pdata;
	char            MatrixHasValues;

	signed char     *ps8;
	unsigned int    ls8;
	unsigned char   *pu8;
	unsigned int    lu8;
	signed short    *ps16;
	unsigned int    ls16;
	unsigned short  *pu16;
	unsigned int    lu16;
	signed int      *ps32;
	unsigned int    ls32;
	unsigned int    *pu32;
	unsigned int    lu32;
	double          *pd;
	unsigned int    ld;

	int             nelements;
	char          **elementnamelist; 
	SlfMATData    **elementpointerlist; 

} SlfMATFile;



SlfMATFile * SlfMatOpen(const char *filename, const char * mode);
void         SlfMatClose(SlfMATFile *pmatf);
char       **SlfMatGetDir(SlfMATFile *pmatf, int *ndir);
SlfMATData * SlfMatGetVariable(SlfMATFile *pmatf, const char *varname);

void         SlfMxFree(char **ppMatDir);

int           SlfMxGetM(SlfMATData * pelement);
int           SlfMxGetN(SlfMATData * pelement);
double     *  SlfMxGetPr(SlfMATData * pelement);
char       *  SlfMxGetName(SlfMATData * pelement);
int           SlfmxGetString(SlfMATData * pelement, char *buf, int buflen);

int			      SlfMxGetNumberOfElements(SlfMATData * pelement); 
int           SlfMxGetNumberOfFields(SlfMATData * pelement);
SlfMATData *  SlfMxGetFieldByNumber(SlfMATData * pelement,int index,int ifield);
const char *  SlfMxGetFieldNameByNumber(SlfMATData * pelement,int ifield);

unsigned char SlfMxIsStruct(SlfMATData * pelement);
unsigned char SlfMxIsDouble(SlfMATData * pelement);
unsigned char SlfMxIsChar(SlfMATData * pelement);
SlfMxClassID  SlfMxGetClassID(SlfMATData * pelement);

SlfMATData *  SlfMxCreateDoubleMatrix(int m, int n,mxComplexity ComplexFlag);
void SlfMxDestroyArray(SlfMATData *pd);
int SlfMatPutVariable(SlfMATFile *pmatf, const char *name, const SlfMATData *pm);

#define SLF_MAT_ERR_TEXT_SIZE 255
extern char SlfMatErrText[SLF_MAT_ERR_TEXT_SIZE];

#endif
