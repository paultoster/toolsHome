#include "SlfSys.h"
#include "SlfParReadFile.h"


//=================================================================    
//=================================================================    
// Konstruktor
//=================================================================    
//=================================================================    
CSlfParReadFile::CSlfParReadFile() {

    Status     = OKAY;
    pF         = 0;
    nF         = 0;
    ReadStatus = PAR_READ_FILE_OKAY;

}
//=================================================================    
//=================================================================    
// Destruktor 
//=================================================================    
//=================================================================    
CSlfParReadFile::~CSlfParReadFile() {

  close();
}
//=================================================================    
//=================================================================    
// Read Parameterfile
//=================================================================    
//=================================================================    
status_t CSlfParReadFile::open(char *par_file)
{
    // Parameterfile prüfen
    //---------------------
    if( !SlfSysExistFile(par_file) ) {
      Status     = NOT_OKAY;
      ReadStatus = PAR_READ_FILE_ERR;
      ErrText.catFormat("Error in CSlfParReadFile::open(\"%s\"); Parameterfile kann nicht gefunden werden\n"
                         ,par_file);
      return Status;
    }
    if( nF  > 0 )
    {
      Status     = NOT_OKAY;
      ReadStatus = PAR_READ_FILE_ERR;
      ErrText.catFormat("Error in CSlfParReadFile::open(\"%s\"); Das Paramterfile <%s> ist bereitsgeöffnet, sollte erst geschlossen werden\n"
                         ,par_file,pF->ParFile.c_str());
      return Status;
    }
    // neue Struktur anlegen
    //----------------------
    pF = new SParReadFile;
    pF->pNext   = 0;
    pF->iZeile  = 0;
    pF->ParFile = par_file;
    nF = 1;

    // Parameterdatei öffnen
    //----------------------
    pF->fs.open(par_file);

    if( pF->fs.good() != true )
    {
      Status     = NOT_OKAY;
      ReadStatus = PAR_READ_FILE_ERR;
      ErrText.catFormat("Error in CSlfParReadFile::open(\"%s\"); Parameterfile kann nicht geöffnet werden\n"
                         ,par_file);
      return Status;
    }

    return Status;
    
}
//=================================================================    
//=================================================================    
// Close
//=================================================================    
//=================================================================    
status_t CSlfParReadFile::close(void)
{
  while( pF )
  {
    SParReadFile *p = pF->pNext;
    if( pF->fs.is_open() )
    {
      pF->fs.close();
    }
    delete pF;
    pF = p;
  }
  nF = 0;
  return Status;
}
status_t CSlfParReadFile::throwaway(uint16_t n)
{
  char c=0;
  uint16_t i;


  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::peekChar(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return Status;
  }
  if( buffer.size() < n )
  {
    ReadStatus = read_in_buffer(n); 
    if( ReadStatus == PAR_READ_FILE_ERR )
    {
      Status = NOT_OKAY;
      return Status;
    }
  }
  else
  {
    ReadStatus = PAR_READ_FILE_OKAY;
  }
  for(i=0;i<n;i++)
  {
    buffer.pop_front();
  }

  return Status;
}
status_t CSlfParReadFile::throwaway_until_eol(void)
{
  uint16_t n = search_until('\n');
  return throwaway(n);
}
//uint16_t  CSlfParReadFile::peek(char *pval, uint16_t n=1)
//{
//  uint16_t i,ncount=0;
//
//  if( nF  == 0 )
//  {
//    Status = NOT_OKAY;
//    ReadStatus = PAR_READ_FILE_ERR;
//    ErrText.catFormat("Error in CSlfParReadFile::get(); Paramterfile muss mit open geöffnet werden\n"
//                     );
//    return ncount;
//  }
//  if( buffer.size() < n )
//  {
//    ReadStatus = read_in_buffer(n); 
//    if( ReadStatus == PAR_READ_FILE_ERR )
//    {
//      Status = NOT_OKAY;
//    }
//  }
//  else
//  {
//    ReadStatus = PAR_READ_FILE_OKAY;
//  }
//  for(i=0;i<n;i++)
//  {
//    if( (uint16_t)(buffer.size()) >= i+1 )
//    {     
//      pval[i] = buffer[i];
//      ++ncount;
//    }
//  }
//  pval[ncount] = '\0';
//
//  return ncount;
//}
char CSlfParReadFile::peekChar(uint16_t ipos/*=0*/)
{
  char c=0;
  uint16_t n = ipos+1;


  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::peekChar(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return c;
  }
  if( buffer.size() < n )
  {
    ReadStatus = read_in_buffer(n); 
    if( ReadStatus == PAR_READ_FILE_ERR )
    {
      Status = NOT_OKAY;
      return 0;
    }
  }
  else
  {
    ReadStatus = PAR_READ_FILE_OKAY;
  }

  if( buffer.size() > ipos  )
  {
    c = buffer.at(ipos);
  }
  else
  {
    c = 0;
  }

  return c;
}
char CSlfParReadFile::getChar(void)
{
  char c=0;

  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::getChar(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return c;
  }
  if( buffer.size() < 1 )
  {
    ReadStatus = read_in_buffer(1); 
    if( ReadStatus == PAR_READ_FILE_ERR )
    {
      Status = NOT_OKAY;
    }
  }
  else
  {
    ReadStatus = PAR_READ_FILE_OKAY;
  }
  if( buffer.size() > 0 )
  {
    c = buffer.front();
    buffer.pop_front();
  }

  return c;
}
uint16_t CSlfParReadFile::get(char *pval, uint16_t n/*=1*/)
{
  uint16_t i,ncount=0;

  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::get(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return ncount;
  }
  if( buffer.size() < n )
  {
    ReadStatus = read_in_buffer(n); 
    if( ReadStatus == PAR_READ_FILE_ERR )
    {
      Status = NOT_OKAY;
    }
  }
  else
  {
    ReadStatus = PAR_READ_FILE_OKAY;
  }
  for(i=0;i<n;i++)
  {
    if( buffer.size() > 0 )
    {     
      pval[i] = buffer.front();
      buffer.pop_front();
      ++ncount;
    }
  }
  pval[ncount] = '\0';

  return ncount;
}
uint16_t CSlfParReadFile::get(slf::CStr &string, uint16_t n/*=1*/)
{
  uint16_t i,ncount=0;

  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::get(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return ncount;
  }
  if( buffer.size() < n )
  {
    ReadStatus = read_in_buffer(n); 
    if( ReadStatus == PAR_READ_FILE_ERR )
    {
      Status = NOT_OKAY;
    }
  }
  else
  {
    ReadStatus = PAR_READ_FILE_OKAY;
  }
  for(i=0;i<n;i++)
  {
    if( buffer.size() > 0 )
    {     
      string.push_back(buffer.front());
      buffer.pop_front();
      ++ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::get_until(slf::CStr &string, char c,bool *pflag)
{
  char cc[2];
  cc[0] = c;
  cc[1] = '\0';
  return get_until(string,cc,pflag);
}
uint16_t CSlfParReadFile::get_until(slf::CStr &string, char *psuch,bool *pflag)
{
  uint16_t ncount=0;
  char     c,c1;
  uint32_t i,ncheck=0,n=SlfStrLen(psuch);

  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::get(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return ncount;
  }
  
  *pflag = false;
  while( (c=getChar()) )
  {
    if( c == psuch[0] )
    {
      *pflag = true;
      for(i=1;i<n;i++)
      {
        c1 = peekChar((uint16_t)i-1);
        if( c1 != psuch[i] )
        {
          *pflag = false;
          break;
        }
      }
    }
    if( !(*pflag) )
    {
      string.push_back(c);
      ++ncount;
    }
    else
    {
      putChar(c);
      return ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::get_until_eol(slf::CStr &string, char c,bool *pflag)
{
  char cc[2];
  cc[0] = c;
  cc[1] = '\0';
  return get_until_eol(string,cc,pflag);
}
uint16_t CSlfParReadFile::get_until_eol(slf::CStr &string, char *psuch,bool *pflag)
{
  uint16_t ncount=0;
  char     c,c1;
  uint32_t i,ncheck=0,n=SlfStrLen(psuch);

  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::get(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return ncount;
  }
  
  *pflag=false;
  while( (c=getChar()) )
  {
    if( c == '\n' )
    {
      putChar(c);
      return ncount;
    }
    if( c == psuch[0] )
    {
      *pflag = true;
      for(i=1;i<n;i++)
      {
        c1 = peekChar((uint16_t)i-1);
        if( c1 != psuch[i] )
        {
          *pflag = false;
          break;
        }
      }
    }
    if( !(*pflag) )
    {
      string.push_back(c);
      ++ncount;
    }
    else
    {
      putChar(c);
      return ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::get_until_eol(slf::CStr &string, char **ppsuch,uint16_t nsuch,bool *pflag)
{
  uint16_t ncount=0,isuch;
  char     c;
  char     *csuch;
  uint32_t i,n;
  uint8_t  flag_eol=0;

  string.clear();
  if( nF  == 0 )
  {
    Status     = NOT_OKAY;
    ReadStatus = PAR_READ_FILE_ERR;
    ErrText.catFormat("Error in CSlfParReadFile::get(); Paramterfile muss mit open geöffnet werden\n"
                     );
    return ncount;
  }
  
  for(isuch=0;isuch<nsuch;++isuch)
  {
    csuch = ppsuch[isuch];
    if( SlfStrFind(csuch,"\n","vs") != SLF_STR_NPOS )
    {
      flag_eol = 1;
    }
  }
  *pflag=false;
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' && !flag_eol )
    {
      get(string, ncount);
      return ncount;
    }
    for(isuch=0;isuch<nsuch;++isuch)
    {
      *pflag = true;
      csuch = ppsuch[isuch];
      n=SlfStrLen(csuch);
      for(i=0;i<n;i++)
      {
        c = peekChar(ncount+(uint16_t)i);
        if( c != csuch[i] )
        {
          *pflag = false;
          break;
        }
      }
      if( *pflag )
      {
        get(string, ncount);
        return ncount;
      }
    }
    ++ncount;
  }
  get(string, ncount);
  return ncount;
}
status_t CSlfParReadFile::putChar(char c)
{
  buffer.push_front(c);
  return Status;
}
status_t CSlfParReadFile::put(char *pval)
{
  uint32_t n = SlfStrLen(pval);
  uint32_t i;

  while( n > 0 )
  {
    i = n-1;
    buffer.push_front(pval[i]);
    --n;
  }
  pval[0] = '\0';
  return Status;
}
status_t CSlfParReadFile::put(slf::CStr &string)
{
  uint32_t n = SlfStrLen(string);
  uint32_t i;

  while( n > 0 )
  {
    i = n-1;
    buffer.push_front(string[i]);
    --n;
  }
  string.clear();
  return Status;
}
uint16_t CSlfParReadFile::search_until(char csuch)
{
  char     c;
  uint16_t ncount=0;
  bool     flag=false;

  while( (c=peekChar(ncount)) )
  {
    flag = false;
    if( c == csuch )
    {
      flag = true;
    }
    if( flag )
    {
      return ncount;
    }
    else
    {
      ++ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_eol(char csuch, bool *pflag)
{
  char     c;
  uint16_t ncount=0;
  bool     flag=false;

  *pflag = true;
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' )
    {
      *pflag = false;
      return ncount;
    }
    flag = false;
    if( c == csuch )
    {
      flag = true;
    }
    if( flag )
    {
      return ncount;
    }
    else
    {
      ++ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until(char *pcsuch)
{
  char     c;
  uint16_t ncount=0;
  uint32_t i;
  bool     flag=false;

  while( (c=peekChar(ncount)) )
  {
    flag  = true;
    for(i=0;i<SlfStrLen(pcsuch);i++)
    {
      c=peekChar(ncount+(uint16_t)i);
      if( c != pcsuch[i] )
      {
        flag = false;
        break;
      }
    }
    if( flag )
    {
      return ncount;
    }
    ++ncount;
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_eol(char *pcsuch,bool *pflag)
{
  char     c;
  uint16_t ncount=0;
  uint32_t i;
  bool     flag=false;

  *pflag = true;
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' )
    {
      *pflag = false;
      return ncount;
    }
    flag  = true;
    for(i=0;i<SlfStrLen(pcsuch);i++)
    {
      c=peekChar(ncount+(uint16_t)i);
      if( c != pcsuch[i] )
      {
        flag = false;
        break;
      }
    }
    if( flag )
    {
      return ncount;
    }
    ++ncount;
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until(char **ppcsuch,uint16_t nsuch)
{
  char     c;
  char     *csuch;
  uint16_t ncount=0,isuch;
  uint32_t i;
  bool     flag=false;

  while( (c=peekChar(ncount)) )
  {
    for(isuch=0;isuch<nsuch;isuch++)
    {
      csuch = ppcsuch[isuch];
      flag  = true;
      for(i=0;i<SlfStrLen(csuch);i++)
      {
        c=peekChar(ncount+(uint16_t)i);
        if( c != csuch[i] )
        {
          flag = false;
          break;
        }
      }
      if( flag )
      {
        return ncount;
      }
    }
    ++ncount;
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_eol(char **ppcsuch,uint16_t nsuch,bool *pflag)
{
  char     c;
  char     *csuch;
  uint16_t ncount=0,isuch;
  uint32_t i;
  bool     flag=false;
  
  *pflag = true;   
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' )
    {
      *pflag = false;
      return ncount;
    }
    for(isuch=0;isuch<nsuch;isuch++)
    {
      csuch = ppcsuch[isuch];
      flag  = true;
      for(i=0;i<SlfStrLen(csuch);i++)
      {
        c=peekChar(ncount+(uint16_t)i);
        if( c != csuch[i] )
        {
          flag = false;
          break;
        }
      }
      if( flag )
      {
        return ncount;
      }
    }
    ++ncount;
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_not(char csuch)
{
  char     c;
  uint16_t ncount=0;
  bool     flag=false;

  while( (c=peekChar(ncount)) )
  {
    flag = true;
    if( c == csuch )
    {
      flag = false;
      break;
    }
    if( flag )
    {
      return ncount;
    }
    else
    {
      ++ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_not_eol(char csuch,bool *pflag)
{
  char     c;
  uint16_t ncount=0;
  bool     flag=false;

  *pflag = true;
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' )
    {
      *pflag = false;
      return ncount;
    }
    flag = true;
    if( c == csuch )
    {
      flag = false;
      break;
    }
    if( flag )
    {
      return ncount;
    }
    else
    {
      ++ncount;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_not(char *pcsuch)
{
  char     c;
  uint16_t ncount=0;
  uint32_t i;
  uint16_t nflag = 0;
  bool     flag;
  while( (c=peekChar(ncount)) )
  {
    flag = true;
    for(i=0;i<SlfStrLen(pcsuch);i++)
    {
      c=peekChar(ncount+(uint16_t)i);
      if( c != pcsuch[i] )
      {
        flag = false;
        break;
      }
    }
    if( flag ) // also gleich
    {
      nflag += 1;
    }
    if( nflag == 0 )
    {
      return ncount;
    }
    else
    {
      ++ncount;
      nflag = 0;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_not_eol(char *pcsuch,bool *pflag)
{
  char     c;
  uint16_t ncount=0;
  uint32_t i;
  uint16_t nflag = 0;
  bool     flag;
  *pflag = true;
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' )
    {
      *pflag = false;
      return ncount;
    }
    flag = true;
    for(i=0;i<SlfStrLen(pcsuch);i++)
    {
      c=peekChar(ncount+(uint16_t)i);
      if( c != pcsuch[i] )
      {
        flag = false;
        break;
      }
    }
    if( flag ) // also gleich
    {
      nflag += 1;
    }
    if( nflag == 0 )
    {
      return ncount;
    }
    else
    {
      ++ncount;
      nflag = 0;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_not(char **ppcsuch,uint16_t nsuch)
{
  char     c;
  char     *csuch;
  uint16_t ncount=0,isuch;
  uint32_t i;
  uint16_t nflag = 0;
  bool     flag;
  while( (c=peekChar(ncount)) )
  {
    for(isuch=0;isuch<nsuch;isuch++)
    {
      csuch = ppcsuch[isuch];
      flag = true;
      for(i=0;i<SlfStrLen(csuch);i++)
      {
        c=peekChar(ncount+(uint16_t)i);
        if( c != csuch[i] )
        {
          flag = false;
          break;
        }
      }
      if( flag ) // also gleich
      {
        nflag += 1;
      }
    }
    if( nflag == 0 )
    {
      return ncount;
    }
    else
    {
      ++ncount;
      nflag = 0;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_until_not_eol(char **ppcsuch,uint16_t nsuch,bool *pflag)
{
  char     c;
  char     *csuch;
  uint16_t ncount=0,isuch;
  uint32_t i;
  uint16_t nflag = 0;
  bool     flag;

  *pflag = true;
  while( (c=peekChar(ncount)) )
  {
    if( c == '\n' )
    {
      *pflag = false;
      return ncount;
    }
    for(isuch=0;isuch<nsuch;isuch++)
    {
      csuch = ppcsuch[isuch];
      flag = true;
      for(i=0;i<SlfStrLen(csuch);i++)
      {
        c=peekChar(ncount+(uint16_t)i);
        if( c != csuch[i] )
        {
          flag = false;
          break;
        }
      }
      if( flag ) // also gleich
      {
        nflag += 1;
      }
    }
    if( nflag == 0 )
    {
      return ncount;
    }
    else
    {
      ++ncount;
      nflag = 0;
    }
  }
  return ncount;
}
uint16_t CSlfParReadFile::search_throw_until(char **ppisuch,uint16_t n)
{
  uint16_t nn = search_until(ppisuch,n);
  throwaway(nn);
  return nn;
}
uint16_t CSlfParReadFile::search_throw_until(char *pisuch)
{
  uint16_t nn = search_until(pisuch);
  throwaway(nn);
  return nn;
}
uint16_t CSlfParReadFile::search_throw_until(char csuch)
{
  uint16_t nn = search_until(csuch);
  throwaway(nn);
  return nn;
}
uint16_t CSlfParReadFile::search_throw_until_not(char **ppisuch,uint16_t n)
{
  uint16_t nn = search_until_not(ppisuch,n);
  throwaway(nn);
  return nn;
}
uint16_t CSlfParReadFile::search_throw_until_not(char *pisuch)
{
  uint16_t nn = search_until_not(pisuch);
  throwaway(nn);
  return nn;
}
uint16_t CSlfParReadFile::search_throw_until_not(char csuch)
{
  uint16_t nn = search_until_not(csuch);
  throwaway(nn);
  return nn;
}
CSlfParReadFile::EParReadFile CSlfParReadFile::length_until(char *psuch,uint16_t *plength)
{
  uint16_t n = (uint16_t)SlfStrLen(psuch);
  uint16_t j,i = 0;
  bool flag;

  while(1)
  {
    if( ((uint16_t)buffer.size() < i+n) && (ReadStatus == PAR_READ_FILE_OKAY) )
    {
      ReadStatus = read_in_buffer(MAX(i+n-(uint16_t)buffer.size(),100));

      if( ReadStatus == PAR_READ_FILE_ERR )
      {
        return ReadStatus;
      }
    }

    flag = true;
    for(j=0;j<n;j++)
    {
      if( (uint16_t)(buffer.size()) < i+1 )
      {
        flag = false;
        ReadStatus = PAR_READ_FILE_END;
        return PAR_READ_FILE_END;
      }
      else
      {
        if( psuch[j] != buffer[i+j] )
        {
          flag = false;
          break;
        }
      }
    }

    if( flag )
    {
      *plength = i;
      break;
    }
    else
    {
      i += 1;
    }
  }
  return ReadStatus;
}
const char * CSlfParReadFile::getFileName(void)
{
  if( pF && pF->ParFile.c_str() )
  {
    return pF->ParFile.c_str();
  }
  else
  {
    return "";
  }
}
void CSlfParReadFile::getZeilenInfo(slf::CStr &src)
{
  SParReadFile *p = pF;
  src.cat("\n");
  while( p )
  {
    src.catFormat("Zeile: <%i>, File: <%s>\n",p->iZeile,p->ParFile.c_str());
    p = p->pNext;
  }
}
//=====================================================================================
//=====================================================================================
//=====================================================================================
//=====================================================================================
CSlfParReadFile::EParReadFile CSlfParReadFile::read_in_buffer(uint16_t nmin)
{
  EParReadFile sw;
  slf::CStr line;
  uint8_t flag = 1;
  uint16_t i;

  while(flag)
  {
    // Read next line
    //---------------
    sw = read_next_line(line);
    for(i=0;i<SlfStrLen(line);i++)
    {
      buffer.push_back(line[i]);
    }
    //if( sw == PAR_READ_FILE_OKAY )
    //{
    //  buffer.push_back('\n');
    //}
    line.clear();
    if( sw == PAR_READ_FILE_ERR )
    {
      set_err_mess();
      Status = NOT_OKAY;
      return PAR_READ_FILE_ERR;
    }
    else if( sw == PAR_READ_FILE_END )
    {
      return PAR_READ_FILE_END;
    }
    else if( buffer.size() >= nmin )
    {
      flag = 0;
    }
  }
  return PAR_READ_FILE_OKAY;
}
CSlfParReadFile::EParReadFile CSlfParReadFile::read_next_line(slf::CStr &line)
{
  int     c;
  bool    includeflag;
  while(1)
  {
    // Jedes Zeichen bis Ende oder Zeilenumbruch einlesen
    c=pF->fs.get();

    if( pF->fs.good() )
    {
       if( (char)c == '\t' )
       {
         c = ' ';
       }
       line.push_back((char)c);

       // zeilenumbruch
       if( c == '\n' )
       {
         pF->iZeile += 1;

         if( check_for_include(line,includeflag) != OKAY )
         {
           Status = NOT_OKAY;
           return PAR_READ_FILE_ERR;
         }
         // Wenn include war, dann nächste Zeile neu einlesen
         //
         if( includeflag )
         {
           line.clear();
         }
         else
         {
           return PAR_READ_FILE_OKAY;
         }
       }
    }
    else if( pF->fs.eof() )
    {
      // Es steht in dem letzten Stückchen was drin
      if( line.findNot(' ') != SLF_STR_NPOS )
      {
        return PAR_READ_FILE_OKAY;
      }
      // Wenn nicht ein eingebettete Datei, dann Ende
      else if( !check_end_of_include() )
      {
        return PAR_READ_FILE_END;
      }
    }
    else
    {
      Status = NOT_OKAY;
      return PAR_READ_FILE_ERR;
    }
  }
}      
void CSlfParReadFile::set_err_mess(void)
{
  SParReadFile *p = pF;
  ErrText.catFormat("Error in CSlfParReadFile; Fehler beim Einlesen in:\n");
  while( p )
  {
    ErrText.catFormat("Parameterfile <%s> Zeile <%i>\n",p->ParFile.c_str(),p->iZeile+1);
    p = p->pNext;
  }
}  
status_t CSlfParReadFile::check_for_include(slf::CStr &line,bool &includeflag)
{
  uint32_t i = line.find(PAR_INCLUDE);
  slf::CStr file_name;

  includeflag = false;

  // Includekennung muss in der ersten Zeile sein
  //---------------------------------------------
  if( i == 0 )
  {
    line.cut(0,SlfStrLen(PAR_INCLUDE));
    if( SlfStrFindQuotCpy(file_name, line,"\"","\"") == NULL )
    {
      file_name = line;
    }
    file_name.elimAnfC("\t ");
    file_name.elimEndC("\n\t ");

    // Parameterfile prüfen
    //---------------------
    if( !SlfSysExistFile(file_name.c_str()) ) {
      Status = NOT_OKAY;
      ErrText.catFormat("Error in CSlfParReadFile::open(\"%s\"); Parameterfile kann nicht gefunden werden\n"
                         ,line.c_str());
      return Status;
    }

    open_inlude(file_name.c_str());
    
    includeflag = true;
  }
  return Status;
}
status_t CSlfParReadFile::open_inlude(const char *file_name)
{
  SParReadFile *p = new SParReadFile;
  p->pNext  = pF;
  p->iZeile = 0;
  p->ParFile= file_name;
  pF = p;
  nF += 1;

  // Parameterdatei öffnen
  //----------------------
  pF->fs.open(file_name);

  if( pF->fs.good() != true )
  {
    Status = NOT_OKAY;
    ErrText.catFormat("Error in CSlfParReadFile::open(\"%s\"); Parameterfile kann nicht geöffnet werden\n"
                       ,file_name);
    return Status;
  }

  return Status;
}
bool CSlfParReadFile::check_end_of_include(void)
{
  // Prüfen, ob eingebettete Parameterdatei
  // geöffnet ist
  //---------------------------------------
  if( nF > 1 )
  {
    SParReadFile *p = pF->pNext;

    // schliessen
    pF->fs.close();

    // Löschen
    delete pF;

    pF = p;
    --nF;

    return true;
  }
  return false;
}