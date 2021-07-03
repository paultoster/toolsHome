//
// SimulaParam.h
//
// Parameter zur Simulation Simula einlesen und weiterverarbeiten

#ifndef SIMULADEF_H
#define SIMULADEF_H

#include <string>
#include <vector>
#include <float.h>


namespace Simula
{
  typedef std::size_t               s_t;
  typedef std::vector<unsigned int> vec_uint_t;
  typedef std::vector<int>          vec_int_t;
  typedef std::vector<double>       vec_double_t;
  typedef std::vector<std::string>  vec_string_t;

  enum status_type
  {   STATUS_NOT_OKAY = 0
  ,   STATUS_OKAY
  };
  enum var_type
  {  SIMULA_VARTYPE_NON = 0                      // Non Variable type
  ,  SIMULA_VARTYPE_SINGLE_INT                   // Integer Variable type
  ,  SIMULA_VARTYPE_SINGLE_DOUBLE                // double Variable type
  ,  SIMULA_VARTYPE_SINGLE_STRING                // string Variable type
  ,  SIMULA_VARTYPE_VECTOR_INT                   // Integer Vector type
  ,  SIMULA_VARTYPE_VECTOR_DOUBLE                // Double Vector type
  ,  SIMULA_VARTYPE_VECTOR_STRING                // String Vector type
  };

  extern const char *LOGFILENAME;                // Logfilename
  
  extern const char *DELIM_HIERACHY;             // Character to delimitate GroupHierachy in a charcater-string
  extern const char *EQUALSIGN;
  extern const char *UNITSIGN0;
  extern const char *UNITSIGN1;
  extern const char *ENTITYSIGN0;
  extern const char *ENTITYSIGN1;
  extern const char *ENTITYDELIM;
  extern const char *STRINGQUOT;
  extern const char *GROUPSIGN0[];
  extern const char *GROUPSIGN1[];
  extern const char NGROUPSIGN;
  extern const char *COMMENTSIGN;
  extern const char *COMMENTLINE;
  extern const char *SPACETOSEPERAT;
  extern const char *SPACE;
  extern const char *TABULATOR;
  extern const char *ENDOFLINESIGN;
  extern const char *EMPTYLINESIGN;
  extern const char NENDOFLINESIGN;
  extern const char *INCLUDESIGN;
  
  extern const std::size_t npos;                 // npos for indicating not found with std::size_t


}

#ifndef SIMULA_EPSILON	
	# define    SIMULA_EPSILON               FLT_EPSILON
#endif
#ifndef SIMULA_DEPSILON	
	# define    SIMULA_DEPSILON              DBL_EPSILON
#endif

//  SIMULA_ABS(x)
# ifndef  SIMULA_ABS
  # define  SIMULA_ABS(x)      ((x>=0.) ? (x) : (-(x)))
# endif
//  SIMULA_IABS(x)
# ifndef  SIMULA_IABS
  # define  SIMULA_IABS(x)      ((x>=0) ? (x) : (-(x)))
# endif
// SIMULA_SIGN(x)
 # ifndef  SIMULA_SIGN
  # define  SIMULA_SIGN(x)     ((x>=0.) ? (1.) : (-1.))
 # endif
//  SIMULA_NOT_ZERO(x)
# ifndef  SIMULA_NOT_ZERO
  # define  SIMULA_NOT_ZERO(x)  ((SIMULA_ABS(x)>SIMULA_EPSILON) ? (x) : (SIMULA_EPSILON*SIMULA_SIGN(x)))
# endif
//  SIMULA_MAX(x,y)
# ifndef  SIMULA_MAX
  # define  SIMULA_MAX(x,y)    (((x) > (y)) ? (x) : (y))
# endif
//  SIMULA_MIN(x,y)
# ifndef  SIMULA_MIN
  # define  SIMULA_MIN(x,y)    (((x) < (y)) ? (x) : (y))
# endif

# ifndef  SIMULA_MINMAXLIMIT
  # define  SIMULA_MINMAXLIMIT(val,vmin,vmax)    (val > vmax ? vmax: (val < vmin ? vmin :val))
# endif

#ifndef SIMULA_BOUND
    #define SIMULA_BOUND(x,L,U)  ((x) <  L  ? L : ((x) > U ? U : (x)))
#endif


#endif