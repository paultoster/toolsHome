#include "SlfParWrite.h"
#include <sstream>


//=================================================================    
//=================================================================    
// Konstruktor
//=================================================================    
//=================================================================    
CSlfParWrite::CSlfParWrite() 
{
    Status   = OKAY;   
}
//=================================================================    
//=================================================================    
// Destruktor 
//=================================================================    
//=================================================================    
CSlfParWrite::~CSlfParWrite()
{
}
//=================================================================    
//=================================================================    
// Write Parameterfile
//=================================================================    
//=================================================================    
status_t CSlfParWrite::writeParFile(char *par_file,CSlfParData *pdata)
{
  pData = pdata;

  // Parameterdatei öffnen
  //----------------------------------
  fs.open(par_file);
  if( !fs.good() )
  {
    Status = NOT_OKAY;
    ErrText.catFormat("writeParFile error: Datei <%s> konnte zum Schreiben nicht geöffnet werden"
                     ,par_file);
    fs.close();
    return Status;
  }

  // data drucken  
  uint16_t i,n = pData->getNInstance();
  slf::CStrV strv;
  for(i=0;i<n;i++)
  {
    strv.append(pData->getInstName(i));
    if( i != 0 ) fs << std::endl;
    Status = printGroup(strv);
    strv.clear();
  }

  // Parameterdatei schliessen
  //---------------------------
  fs.close();

  return Status;
}
//=================================================================    
//=================================================================    
// Gibt eine Gruppe aus
//=================================================================    
//=================================================================    
status_t CSlfParWrite::printGroup(slf::CStrV &strv)
{
  sint16_t ii;
  sint16_t ihir = (sint16_t)strv.getNrows()-2;
  slf::CStr  comment;

  // Gruppenname schreiben
  //----------------------
  if( ihir < 0 )
  {
    fs << SLF_PAR_READ_STRING_START_INSTANCE;
    fs << strv.get_last_str() << SLF_PAR_READ_STRING_END_INSTANCE << std::endl;
  }
  else if( ihir == 0 )
  {
    fs << std::endl;
    fs << SLF_PAR_READ_STRING_START_GROUP;
    fs << strv.get_last_str() << SLF_PAR_READ_STRING_END_GROUP << std::endl;
  }
  else
  {
    fs << std::endl;
    for(ii=0;ii<ihir;ii++)
    {
      fs << SLF_PAR_READ_STRING_START_SUBGROUP;
    }
    fs << strv.get_last_str();
    for(ii=0;ii<ihir;ii++)
    {
      fs << SLF_PAR_READ_STRING_END_SUBGROUP;
    }
    fs << std::endl;
  }
  
  // Kommentar schreiben
  //--------------------
  comment = pData->getGroupComment(strv);

  if( SlfStrLen(comment) > 0 )
  {
    slf::CStrV v;
    uint32_t count=0,i,n = SlfStrVSplit( v, comment.c_str(), " ");
    for(i=0;i<n;i++)
    {
      if( count > N_CHAR_PER_LINE )
      {
        fs << std::endl;
        count = 0;
      }
      if( count == 0 )
      {
        fs << SLF_PAR_READ_COMMENT1;
        count += SlfStrLen(SLF_PAR_READ_STRING_COMMENT1);
      }
      fs << " " << v.get_str(i);
      count += SlfStrLen(v.get_str(i));
    }
    fs << std::endl;
  }

  // Variablen
  uint16_t iv,nv = pData->getNVar(strv);
  for(iv=0;iv<nv;iv++)
  {
    strv.append(pData->getVarName(strv,iv));
    if( iv == 0 ) fs << std::endl;
    Status = printVar(strv);
    strv.delete_last();
  }
  // Tabellen

  // Nächste Gruppenebene
  uint16_t ig,ng = pData->getNSubGroup(strv);
  for(ig=0;ig<ng;ig++)
  {
    strv.append(pData->getSubGroupName(strv,ig));
    Status = printGroup(strv);
    strv.delete_last();
  }
  return Status;
}
//=================================================================    
//=================================================================    
// Gibt eine Variable aus
//=================================================================    
//=================================================================    
status_t CSlfParWrite::printVar(slf::CStrV &strv)
{
  sint32_t icol,irow,nrow,ncol;
  slf::CStr  comment;
  slf::CStrM strm;
  Matrix_t mat;
  uint32_t count=0;
  uint32_t count0=0;

  // Variablenname schreiben
  //------------------------
  fs << strv.get_last_str() << SLF_PAR_READ_STRING_SPACE;

  count0 += SlfStrLen(strv.get_last_str());
  count0 += SlfStrLen(SLF_PAR_READ_STRING_SPACE);

  // Einheit schreiben
  //------------------
  if( SlfStrLen(pData->getVarUnit(strv)) > 0 )
  {
    fs << SLF_PAR_READ_STRING_START_UNIT << pData->getVarUnit(strv) << SLF_PAR_READ_STRING_END_UNIT << SLF_PAR_READ_STRING_SPACE;
    
    count0 += SlfStrLen(SLF_PAR_READ_STRING_START_UNIT);
    count0 += SlfStrLen(pData->getVarUnit(strv));
    count0 += SlfStrLen(SLF_PAR_READ_STRING_END_UNIT);
    count0 += SlfStrLen(SLF_PAR_READ_STRING_SPACE);
  }

  // IstGleichZeichen
  //-----------------
  fs << SLF_PAR_READ_STRING_EQUAL << SLF_PAR_READ_STRING_SPACE;
  count0 += SlfStrLen(SLF_PAR_READ_STRING_EQUAL);
  count0 += SlfStrLen(SLF_PAR_READ_STRING_SPACE);

  // Wert bestimmen
  if( pData->getVarIsString(strv) )
  {
    pData->getVarStringM(strv,strm);

    nrow = strm.getNrows();
    ncol = strm.getNcols();
    if( nrow*ncol == 1 ) // Einzelner wert
    {
      fs << SLF_PAR_READ_STRING_QUOT << strm.get_str(0,0) << SLF_PAR_READ_STRING_QUOT;
    }
    else if( nrow*ncol > 1 ) // mehrere Werte
    {
      count = count0;
      fs << SLF_PAR_READ_STRING_START_VAL;
      count += SlfStrLen(SLF_PAR_READ_STRING_START_VAL);

      for(irow=0;irow<nrow;irow++)
      {
        for(icol=0;icol<ncol;icol++)
        {
          fs << SLF_PAR_READ_STRING_QUOT << strm.get_str(irow,icol) << SLF_PAR_READ_STRING_QUOT;
          count += SlfStrLen(SLF_PAR_READ_STRING_QUOT);
          count += SlfStrLen(strm.get_str(irow,icol));
          count += SlfStrLen(SLF_PAR_READ_STRING_QUOT);

          if( icol < ncol-1 )
          {
            fs << SLF_PAR_READ_STRING_VAL_DELIM_COL;
            count += SlfStrLen(SLF_PAR_READ_STRING_VAL_DELIM_COL);
          }
          if( count > N_CHAR_PER_LINE )
          {
            fs << std::endl;
            count = 0;
            while( count < count0 )
            {
              ++count;
              fs << SLF_PAR_READ_STRING_SPACE;
            }
          }
        }
        if( irow < nrow-1 )
        {
          fs << SLF_PAR_READ_STRING_VAL_DELIM_ROW;
          count += SlfStrLen(SLF_PAR_READ_STRING_VAL_DELIM_ROW);
        }
      }
      fs << SLF_PAR_READ_STRING_END_VAL; 
      count += SlfStrLen(SLF_PAR_READ_STRING_END_VAL);
    }
  }
  else
  {
    pData->getVarMat(strv,mat);

    if( mat )
    {
      nrow = GET_NROWS(mat);
      ncol = GET_NCOLS(mat);
      if( nrow*ncol == 1 ) // Einzelner wert
      {
        fs << mat[0][0];
      }
      else if( nrow*ncol > 1 ) // mehrere Werte
      {
        
        count = count0;
        fs << SLF_PAR_READ_STRING_START_VAL;
        count += SlfStrLen(SLF_PAR_READ_STRING_START_VAL);

        for(irow=0;irow<nrow;irow++)
        {
          for(icol=0;icol<ncol;icol++)
          {
            std::stringstream ss;
            ss << mat[irow][icol];
            count += SlfStrLen(ss.str().c_str());

            fs << ss.str().c_str();
            if( icol < ncol-1 )
            {
              fs << SLF_PAR_READ_STRING_VAL_DELIM_COL;
              count += SlfStrLen(SLF_PAR_READ_STRING_VAL_DELIM_ROW);
            }
            if( count > N_CHAR_PER_LINE )
            {
              fs << std::endl;
              count = 0;
              while( count < count0 )
              {
                ++count;
                fs << SLF_PAR_READ_STRING_SPACE;
              }
            }
          }
          if( irow < nrow-1 )
          {
            fs << SLF_PAR_READ_STRING_VAL_DELIM_ROW;
            count += SlfStrLen(SLF_PAR_READ_STRING_VAL_DELIM_ROW);
          }
        }
        fs << SLF_PAR_READ_STRING_END_VAL; 
        count += SlfStrLen(SLF_PAR_READ_STRING_END_VAL);
      }
    }
  }
  // Kommentar schreiben
  //--------------------
  comment = pData->getVarComment(strv);

  if( SlfStrLen(comment) > 0 )
  {
    slf::CStrV v;
    uint32_t count=0,i,n = SlfStrVSplit( v, comment.c_str(), " ");
    fs << std::endl;
    for(i=0;i<n;i++)
    {
      if( count > N_CHAR_PER_LINE )
      {
        fs << std::endl;
        count = 0;
      }
      if( count == 0 )
      {
        fs << SLF_PAR_READ_COMMENT1;
        count += SlfStrLen(SLF_PAR_READ_STRING_COMMENT1);
      }
      fs << " " << v.get_str(i);
      count += SlfStrLen(v.get_str(i));
    }
  }

  fs << std::endl << std::endl;
  return Status;
}