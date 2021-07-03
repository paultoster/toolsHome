#include <stdarg.h>
#include <math.h>
#include <ctype.h>
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfParReg.h"


//=================================================================    
//=================================================================    
// Konstruktor CDsParFile
//=================================================================    
//=================================================================    
CSlfParReg::CSlfParReg() {

    Status = OKAY;

}
//=================================================================    
//=================================================================    
// Destruktor CDsParFile
//=================================================================    
//=================================================================    
CSlfParReg::~CSlfParReg() {

  SSlfParRegVar *prv;
  // verkettete Liste mit registrierten Parameter löschen

  while( pRegVar )
  {
    prv = pRegVar->pNext;
    delete pRegVar;
    pRegVar = prv;
  }

}
status_t CSlfParReg::reg(char *varname,char *groupname,char *unit,enum ESlfParType Type
                        ,void *pval,char *Default,char *Comment)
{

  status_t status=OKAY;
  error_t  err;

  // neues Element bilden
  //=====================
  SSlfParRegVar *prv = new SSlfParRegVar;
  
  // Testen, ob pval ein richtiger pointer ist und
  // der richtige Typ verwendet wird (hier single-Werte)
  //====================================================
  err = CSlfParReg::regCheckType(Type,pval);
  if( err == NO_ERR )
  {
    prv->ErrFlag = 0;
  }
  else if( err == WRONG_POINTER )
  {
      prv->ErrFlag = 1;
      status       = NOT_OKAY;
      ErrText.catFormat("CSlfParReg::reg: Der Parameter <%s> mit der Gruppenhierachie <%s> übergibt "
                        " einen ungültigen Pointer für den Typ <%s> !!!"
                       ,varname,groupname,SlfParTypeStr[Type]);
  }
  else if( err == WRONG_TYPE )
  {
      prv->ErrFlag = 1;
      status       = NOT_OKAY;
      ErrText.catFormat("CSlfParReg::reg: Der Parameter <%s> mit der Gruppenhierachie <%s> übergibt "
                        " einen ungültigen Type für diese Funktion <%s> !!!"
                       ,varname,groupname,SlfParTypeStr[Type]);
  }
  //====================================================

  // Liste vervollständigen
  //=======================
  // Parametername
  prv->VarName = varname;
  // Gruppenhierachie über PAR_GROUP_DELIM (".") getrennt
  SlfStrVSplit( prv->GroupHierachie, groupname, PAR_GROUP_DELIM);
  // Einheit
  prv->Unit = unit;
  // Default
  prv->Default = Default;
  // Comment
  prv->Comment = Comment;

  // Flags
  if( (Type == PAR_VEC) || (Type == PAR_MAT) ) prv->HasLength = 1;
  else                                         prv->HasLength = 0;
  
  

  // Verketete Liste anlegen
  //========================
  if( !pRegVar )
  {
    pRegVar        = prv;
    pRegVar->pNext = 0;
  }
  else
  {
    prv->pNext     = pRegVar;
    pRegVar        = prv;
  }

  return status;
}
status_t CSlfParReg::reg(char *varname,char *groupname,char *unit,enum ESlfParType Type
                        ,void *pval,uint32_t nval,char *Default,char *Comment)
{

  status_t status=OKAY;
  error_t  err;

  // neues Element bilden
  //=====================
  SSlfParRegVar *prv = new SSlfParRegVar;
  
  // Testen, ob pval ein richtiger pointer ist und
  // der richtige Typ verwendet wird (hier single-Werte)
  //====================================================
  err = CSlfParReg::regCheckTypeWithLength(Type,pval,nval);
  if( err == NO_ERR )
  {
    prv->ErrFlag = 0;
  }
  else if( err == WRONG_POINTER )
  {
      prv->ErrFlag = 1;
      status       = NOT_OKAY;
      ErrText.catFormat("CSlfParReg::reg: Der Parameter <%s> mit der Gruppenhierachie <%s> übergibt "
                        " einen ungültigen Pointer für den Typ <%s> !!!"
                       ,varname,groupname,SlfParTypeStr[Type]);
  }
  else if( err == WRONG_TYPE )
  {
      prv->ErrFlag = 1;
      status       = NOT_OKAY;
      ErrText.catFormat("CSlfParReg::reg: Der Parameter <%s> mit der Gruppenhierachie <%s> übergibt "
                        " einen ungültigen Type für diese Funktion <%s> !!!"
                       ,varname,groupname,SlfParTypeStr[Type]);
  }
  //====================================================

  // Liste vervollständigen
  //=======================
  // Parametername
  prv->VarName = varname;
  // Gruppenhierachie über PAR_GROUP_DELIM (".") getrennt
  SlfStrVSplit( prv->GroupHierachie, groupname, PAR_GROUP_DELIM);
  // Einheit
  prv->Unit = unit;
  // Default
  prv->Default = Default;
  // Comment
  prv->Comment = Comment;
  // Flags
  prv->HasLength = 1;  // Ist vom Typ ARR oder CHAR_STRING mit fest vorgegebener Länge NArray

  // Verketete Liste anlegen
  //========================
  if( !pRegVar )
  {
    pRegVar        = prv;
    pRegVar->pNext = 0;
  }
  else
  {
    prv->pNext     = pRegVar;
    pRegVar        = prv;
  }

  return status;
}
/*=====================================================================*/
/*=====================================================================*/
/*=====================================================================*/
/*=====================================================================*/
uint8_t CSlfParReg::regCheckType(enum ESlfParType type,void *pval)
{
  if( type == PAR_D64 )
  {
    try
    {
      d64_t *pd = (d64_t*)pval;
      *pd = 0.0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_F32 )
  {
    try
    {
      f32_t *pf = (f32_t*)pval;
      *pf = 0.0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_SINT64 )
  {
    try
    {
      sint64_t *pl = (sint64_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_UINT64 )
  {
    try
    {
      uint64_t *pl = (uint64_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_SINT32 )
  {
    try
    {
      sint32_t *pl = (sint32_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_UINT32 )
  {
    try
    {
      uint32_t *pl = (uint32_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_SINT16 )
  {
    try
    {
      sint16_t *pl = (sint16_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_UINT16 )
  {
    try
    {
      uint16_t *pl = (uint16_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_SINT8 )
  {
    try
    {
      sint8_t *pl = (sint8_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_UINT8 )
  {
    try
    {
      uint8_t *pl = (uint8_t*)pval;
      *pl = 0;
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_slf::CStr )
  {
    try
    {
      slf::CStr *pcstr = (slf::CStr*)pval;
      pcstr->clear();
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_VEC )
  {
    try
    {
      Vector_t v = (Vector_t)pval;
      uarray n = GET_NROWS(v);
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_MAT )
  {
    try
    {
      Matrix_t m = (Matrix_t)pval;
      uarray n = GET_NROWS(m);
      n        = GET_NCOLS(m);
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_PTR_VEC )
  {
    try
    {
      Vector_t *pv = (Vector_t*)pval;
      *pv = NewVector(2);
      FreeVector(*pv);
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_PTR_MAT )
  {
    try
    {
      Matrix_t *pm = (Matrix_t*)pval;
      *pm = NewMatrix(2,2);
      FreeMatrix(*pm);
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else
  {
    return WRONG_TYPE;
  }
    return NO_ERR;
}
/*=====================================================================*/
/*=====================================================================*/
/*=====================================================================*/
/*=====================================================================*/
uint8_t CSlfParReg::regCheckTypeWithLength(enum ESlfParType type,void *pval,uint32_t nval)
{
  uint32_t i;
  if( type == PAR_ARR_D64 )
  {
    try
    {
      d64_t *d = (d64_t*)pval;
      for(i=0;i<nval;i++)
      {
       d[i] = 0.0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_F32 )
  {
    try
    {
      f32_t *f = (f32_t*)pval;
      for(i=0;i<nval;i++)
      {
       f[i] = 0.0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_SINT64 )
  {
    try
    {
      sint64_t *l = (sint64_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_UINT64 )
  {
    try
    {
      uint64_t *l = (uint64_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_SINT32 )
  {
    try
    {
      sint32_t *l = (sint32_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }

    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_UINT32 )
  {
    try
    {
      uint32_t *l = (uint32_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_SINT16 )
  {
    try
    {
      sint16_t *l = (sint16_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_UINT16 )
  {
    try
    {
      uint16_t *l = (uint16_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }

    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_SINT8 )
  {
    try
    {
      sint8_t *l = (sint8_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_UINT8 )
  {
    try
    {
      uint8_t *l = (uint8_t*)pval;
      for(i=0;i<nval;i++)
      {
       l[i] = 0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_ARR_slf::CStr )
  {
    try
    {
      slf::CStr *cstr = (slf::CStr*)pval;
      for(i=0;i<nval;i++)
      {
        cstr[i].clear();
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else if( type == PAR_CHAR_STRING )
  {
    try
    {
      char *str = (char*)pval;
      for(i=0;i<nval;i++)
      {
        str[i]=0;
      }
    }
    catch(...) 
    {
      return WRONG_POINTER;
    }
  }
  else
  {
    return WRONG_TYPE;
  }
    return NO_ERR;
}



