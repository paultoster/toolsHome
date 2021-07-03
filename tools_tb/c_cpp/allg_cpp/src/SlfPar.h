#ifndef SLF_PAR_H_INCLUDED
#define SLF_PAR_H_INCLUDED
 
#include "SlfBase.h"
#include "SlfParVar.h"
#include "SlfParVarVal.h"
#include "SlfMessage.h"
#include "SlfStrV.h"
#include "SlfStrM.h"
#include "SlfStr.h"
#include "SlfLog.h"
#include "SlfParDef.h"
#include <vector>
#include <sstream>

//========================================================================================================
// 
// Standard Parameter description
// ==============================
// group definiton
// ---------------
// name1{ ... }                          definition of a group name1
// name1{ name2{ } }                     defenition of a subgroup name2
// name1{ name2{ var1 = value;} }        definition of variable var1 in subgroup name2 of group name 1
//
// alternative description:              
// name1.name2.var1=value;
////
// varaiable definition
// --------------------
// var1=10.2;                                  single value double w/o unit
// var1[m/s]=10.2;                             single value double w unit m/s
// var1[m/s]=[10.2,10.5,10.7,11];              vector value double w unit m/s
// var1[m/s]=                                  same
// 10.2,
// 10.5,
// 10.7,
// 11;
// var1[m/s]=[[10.2,10.5,10.7],[9.1,9.2,9.3]]; matrix value double w unit m/s and 2 rows and 3 columns
// var1[m/s]=[10.2,10.5,10.7;9.1,9.2,9.3];     same
//
// <var1, var2, var3>[m,m/s,m/s/s]=
//  1   , 10   , 100,
//  2   ,  8   , 101,
//  3   ,  4   , 110,
//  4   ,  2   , 130;
//                                             table w 3 vectors  same as (var1[m>=[1,2,3,4];var2<m/s>=[10,8,4,2];var3<m/s/s>=[100,101,110,130];)
//
// var2 = "text"                               text variable text always in quotes ""
// var2 = ["text1","text2"]                    text vector
//
// Comment
// ------
// !
//
// Simple Parameter description
// ==============================
//
// group definiton
// ---------------
// [groupname]                                                // written in the beginning of a line
//
// varaiable definition
// --------------------
//
// variablename = value                                       // written in one line, value can be:
//                                                            // double, int32_t, bool and std::string
// Comment
// ------
// ;
//
// Functions:
// ----------
// 
//                                                            // Parameterdescription in Standard as described
// okay_t readFileInNewParSet(const char *filename);          // read a new File and set a new Parameterset
// okay_t readStrInNewParSet(const CStr &text);               // read CStr varbiable and set a new Parameterset
// okay_t readStrVecInNewParSet(const CStrV &textvec);        // read CStrV varbiable and set a new Parameterset
// okay_t readFileInParSet( const char *filename              // read a File and set Parameterset with index i_par_set
//                        , const std::size_t i_par_set);
// const CParVarValCollect &getLastVarValCollect(void);                    // get last collected parameter set
// const CParVarValCollect &getVarValCollect(const std::size_t i_par_set); // get collected parameter set by index
// 
// okay_t readSimpleParFileInNewParSet(const char *filename);    // read a new File and set a new Parameterset
//                                                            // Parameterdescription in Simple Way(ini-File)
// 
//========================================================================================================


namespace slf
{
  //struct SParSearch
  //{
  //  CStrV             inputvec;
  //  CParVarValCollect &parmeterset;
  //  CStrV             groupsvec;
  //  CStr              name;
  //  std::size_t  iactparamvec;
  //  std::size_t  iactparamstr;
  //  std::size_t  istartparamvec;
  //  std::size_t  istartparamstr;
  //  std::size_t  iendparamvec;
  //  std::size_t  iendparamstr;

  //  SParSearch(const CStrV &pv, CParVarValCollect &ps) :parmeterset(ps), groupsvec(), iactparamvec(0), iactparamstr(0)
  //  {
  //    inputvec = pv;
  //  }

  //};
  struct SValueStrings
  {
    std::vector<CStr> groupvec;
    CStr  valname;
    CStr  unit;
    CStr  value;
    std::size_t ilinevalue;
  };
  struct STableValueStrings
  {
    std::vector<CStr> groupvec;
    CStr  valnames;
    CStr  units;
    CStr  values;
    std::size_t ilinevalue;
  };
  struct SParSearch
  {
    CParVarValCollect &parmeterset;     // parameter set 
    CStrV       textvec;     // text parts from parameter input replaced by #00001, ...
    CStr        inputstr;    // inputstring from input
    CVectorU    i0linevec;  // vector with position in string for each starting line from input
    CVectorU    llinevec;   // vector with length in string for each line from input w/o comment and substituted text (#00001, #00002, ... )
    CVectorU    i0textvec;   // vector with position in string for each substituted textvec-item (#00001, #00002, ... ) in inpustring
    
    std::vector<SValueStrings> valueStringVec;  // Vector with all information for value in strings
    std::vector<STableValueStrings> tableValueStringVec;  // Vector with all information for table values in strings

    SParSearch(CParVarValCollect &ps) :parmeterset(ps){}
  };



  // Parameter-Variablen-Strukturr
  class CPar
  {
  protected:
    CMessage                       mMessage;
    CLog                           *pLog;
    std::vector<CParVarValCollect> mInputParVec;   // sets of input parameter vectors means parameter wih are read by any input (file,...)
    std::size_t                    mNInputParVec;
    std::size_t                    mIInputParVec;
  public:
    const CMessage &Message;
    CPar(const char *name, CLog &log)
           : mMessage(name)
           , pLog(&log)
           , mInputParVec()
           , mNInputParVec(0)
           , mIInputParVec(0)
           , Message(mMessage)
           {  };

    // read a new File and set a new Parameterset
    okay_t readFileInNewParSet(const char *filename);
    
    // read CStr varbiable and set a new Parameterset
    okay_t readStrInNewParSet(const CStr &text);

    // read CStrV varbiable and set a new Parameterset
    okay_t readStrVecInNewParSet(const CStrV &textvec);

    // read a File and set Parameterset with index i_par_set
    okay_t readFileInParSet(const char *filename, const std::size_t i_par_set);

    const CParVarValCollect &getLastVarValCollect(void)
    {
      std::size_t index = mInputParVec.size();
      if (index)--index;
      return mInputParVec[index];
    }

    const CParVarValCollect &getVarValCollect(const std::size_t i_par_set) 
    {
      if (i_par_set < mInputParVec.size())
      {
        return mInputParVec[i_par_set];
      }
      else
      {
        return getLastVarValCollect();
      }
    }

    // Parameterdescription in Simple Way(ini-File)

    // read a new File and set a new Parameterset
    okay_t readSimpleParFileInNewParSet(const char *filename);    
                                                                
  protected:

    // SlfPar.cpp
    //================================================================
    // create new Data-Set
    void createNewParSet(void);
    // delete last set
    void deleteLastParSet(void);

    // SlfParRead.cpp
    //================================================================

    // read file and put each line into inputvec
    okay_t readFile(const char *filename, CStrV &inputvec, CStr &texterr);

    // read Parameters from inputvec
    //okay_t readParameters(CStrV &inputvec, CStr &texterr);
    // read same but with string
    okay_t readParameters(CStrV &inputvec, CStr &texterr);

    // read Parameters from inputvec
    // as Simple description
    okay_t readSimpleParameters(CStrV &inputvec, CStr &texterr);

    //-------------------------------------------------------------------------------------------
    // parses string-vector inputvec
    // inputvec    string-vector with all parameter input
    // group_innfo group-infos and depth group mentioned here
    // textvec     contains all substituded text pattern from inputvec
    // texterr     contains error log
    //okay_t readGroup(SParSearch &group_info, CStrV &textvec, CStr &texterr);

    //---------------------------------------------------------------------------------------------
    // search for nested {} reads group
    //okay_t readNestedGroup(SParSearch &group_info, CStrV &textvec, CStr &texterr);
    // read variable
    //okay_t readVariable(SParSearch &group_info, CStrV &textvec, CStr &texterr);
    // read and return unit from variable
    //CStr readGetVariableUnit(const CStrV &inputvec, const std::size_t &ivec0, const std::size_t &istr0, const std::size_t &ivec1, const std::size_t &istr1);
    // set variable Values
    //okay_t setVariabeValues(CParVarValCollect &parmeterset, CStr &varname, CStrV &groupsvec, CStr &varunit, CStrM &vavalues, bool flag_is_text, CStr &texterr);

    //bool readCheckWordIfNestedGroup(SParSearch &group_info);
    //bool readCheckWordIfVariable(SParSearch &group_info);



  };

  // write Parameterdefinition into a string vector (each Parameter one line)
  void WriteParameterStructure(const CParVarDefCollect &pardef, CStrV &strvec);
  // find pardef in pvvc and bset value from pvvc (read Parameters) intp pardef
  void SetParameterFromVarValCollect(const CParVarValCollect &pvvc, CParVarDef &pardef);
  // inspect defaultvalue to set this value to pardef
  okay_t SetParameterFromDefault(const CStr &defaultvalue, CParVarDef &pardef, CStr &texterr);

  // prepare by take out comment signed with SLFPAR_COMMMENT and collect and replace text ("text1", "text2", "text3" ... with #00000,#00001,#00002, ...)
  okay_t PrepareStructure(const char *commentsign,CStrV &inputvec, CStrV &textvec, CStr &texterr);

  // make string from inputvec and put start position of each line in lineposvec
  // inputvec = {"0123456","78901234","","567"}
  // => inputstr = "012345678901234567"
  // => lineposvec = {0,7,15,15}
  void MakeStringFromInputVec(CStrV &inputvec, CStr &inputstr, CVectorU &i0linevec, CVectorU &llinevec, CVectorU &i0textvec);


  // read all values from varialbel-input into varvalues
  // inputvalvec               Input-String-Vector of variable Zuweisungs part it contains only parts of the value. if input is over more then one line, evrey line is a item in vector
  //                           e.g. {"s [m] = [10.","20.","30.]" -> Vector or {"a = 101"} -> single value or {"tt = ["abc","def","ghi"] -> text vector
  // ivecoffset                is index offset if inputvalvec is picked from entire parameter set described by String Vector (if not set to 0)
  // istroffset                is string count offset if inputval vec is picked from entire parameter set (if not set to 0)
  // textvec                   string vector with all texts from PrepareStructure() wich is substiduted in inputvector
  // varvalues                 output: a string matrix (CStrM) is filled with all values as text (flag_is_text==true) or numeric values (flag_is_text==false) 
  // flag_is_text              output: if variable is text or numeric
  // texterr                   output: text with err message if not_okay
  //
  okay_t ReadVariableValues(CStrV &inputvalvec, std::size_t ivecoffset, std::size_t istroffset, CStrV &textvec, CStrM &varvalues, bool &flag_is_text, CStr &texterr);

  // set variable values from varvalues to parameterset
  void ConvertVariableValues(CStrM &varvalues, CMatrixD &mat);
}


#endif // SLF_PAR_H_INCLUDED