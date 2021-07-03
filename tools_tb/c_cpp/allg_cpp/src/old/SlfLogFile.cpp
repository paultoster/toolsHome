#include "SlfLogFile.h"
#include "SlfSys.h"
    
CSlfLogFile::CSlfLogFile() {

    FileName         = "Logfile.txt";
    FlagFileOpen     = false;
    FlagValueWritten = false;
    Fid              = 0;

    Status  = OKAY;

}
CSlfLogFile::CSlfLogFile(const char *file_name) {

    FileName         = file_name;
    FlagFileOpen     = false;
    FlagValueWritten = false;
    Fid              = 0;

    Status = OKAY;
}
    
CSlfLogFile::~CSlfLogFile() {

    if( Fid != NULL )
        close();


}

status_t CSlfLogFile::open() {
    return open(FileName);
}
status_t CSlfLogFile::open(slf::CStr &file_name) {
    return CSlfLogFile::open(file_name.c_str());
}
status_t CSlfLogFile::open(const char *filename) {

    if( !FlagFileOpen ) {

		  slf::CStr path="";

      if( SlfStrCompare(filename,FileName.c_str()) ) {

#if _MSC_VER > MSC_VER_BIS_VS2005
        if( fopen_s(&Fid,filename,"a") != 0 )
        {
          Fid = 0;
        }
#else
			  Fid = fopen(filename,"a");
#endif
		  } else {
		
#if _MSC_VER > MSC_VER_BIS_VS2005
        if( fopen_s(&Fid,filename,"w") != 0 )
        {
          Fid = 0;
        }
#else
			  Fid = fopen(filename,"w");
#endif
        FileName = filename;
		  }

		SlfStrExtractPfe(path,FileName.c_str(),"p");

		if( SlfStrLen(path) > 0 ) {

			FullFileName = FileName;
		    SlfStrExtractPfe(FileName,FullFileName.c_str(),"fe");
		} else {
		
			SlfSysGetActPath(FullFileName);
			FullFileName.append(FileName);
		}

        if( Fid == NULL )
            Status           = NOT_OKAY;
        else {
            Status           = OKAY;
            FlagFileOpen     = true;
            FlagValueWritten = false;
        }
    } else
        Status = NOT_OKAY;

    return Status;
}
status_t CSlfLogFile::opennew() {
    return opennew(FileName);
}
status_t CSlfLogFile::opennew(slf::CStr &file_name) {
    return opennew(file_name.c_str());
}
status_t CSlfLogFile::opennew(const char *filename) {

    if( !FlagFileOpen ) {

		  slf::CStr path="";

#if _MSC_VER > MSC_VER_BIS_VS2005
      if( fopen_s(&Fid,filename,"w") != 0 )
      {
        Fid = 0;
      }
#else
		  Fid = fopen(filename,"w");
#endif
      FileName.clear();
      FileName.cat(filename);

		SlfStrExtractPfe(path,FileName.c_str(),"p");

		if( SlfStrLen(path) > 0 ) {

			FullFileName = FileName;
		    SlfStrExtractPfe(FileName,FullFileName.c_str(),"fe");
		} else {
		
			SlfSysGetActPath(FullFileName);
			FullFileName.append(FileName);
		}

        if( Fid == NULL )
            Status           = NOT_OKAY;
        else {
            Status           = OKAY;
            FlagFileOpen     = true;
            FlagValueWritten = false;
        }
    } else
        Status = NOT_OKAY;

    return Status;
}

status_t CSlfLogFile::writeEnd(slf::CStr &text) {
    return writeEnd(text.c_str());
}
status_t CSlfLogFile::writeEnd(const char *text) {

    if( Fid == NULL ) {
        if( open() != OKAY)
            return Status;
    }
    if( fprintf(Fid,"%s\n",text) < 0 ) {
        Status = NOT_OKAY;
    } else {
    
        if( fflush(Fid) != 0 )
            Status = NOT_OKAY;
        else
            FlagValueWritten = true;
    }

    return Status;
}

status_t CSlfLogFile::write(slf::CStr &text) {
    return write(text.c_str());
}
status_t CSlfLogFile::write(const char *text) {

    if( Fid == NULL ) {
        if( open() != OKAY)
            return Status;
    }
    if( fprintf(Fid,"%s",text) < 0 ) {
        Status = NOT_OKAY;
    } else {
    
        if( fflush(Fid) != 0 )
            Status = NOT_OKAY;
        else
            FlagValueWritten = true;

    }
    return Status;
}
status_t CSlfLogFile::writeLine(const char *text,uint16_t l) {

    if( Fid == NULL ) {
        if( open() != OKAY)
            return Status;
    }
    for(uint16_t i=0;i<l;i++) {
        if( fprintf(Fid,"%s",text) < 0 ) {
            Status = NOT_OKAY;
            return Status;
        } 
    }
    if( fprintf(Fid,"\n") < 0 ) {
        Status = NOT_OKAY;
        return Status;
    } 

    if( fflush(Fid) != 0 )
        Status = NOT_OKAY;
    else
        FlagValueWritten = true;
    
    return Status;
}
status_t CSlfLogFile::close() {

    if( Fid != NULL ) {
        fclose(Fid);
        Fid = 0;
        FlagValueWritten = false;
        FlagFileOpen     = false;
        Status = OKAY;
    } else {
        Status = NOT_OKAY;
    }
    return Status;
}
status_t CSlfLogFile::closeFinal() {

    close();
    FileName.clear();
    return Status;
}
