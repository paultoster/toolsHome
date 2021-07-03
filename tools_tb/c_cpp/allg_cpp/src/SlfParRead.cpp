
#include "SlfPar.h"
#include "SlfSys.h"
#include "SlfFkt.h"
#include "SlfStrRegEx.h"


namespace slf
{
  std::size_t FindLineInInputString(SParSearch &input, std::size_t index);
  // check for brackets, change from SLFPAR_VAL_END to SLFPAR_VEC_ROW_DELIM in Vector brackets
  // get nested indices of group
  okay_t CheckInputString(SParSearch &input, CStr &texterr);
  // check for brackets
  okay_t CheckInputStringBrackets(SParSearch &input, char *tb0, char *tb1, CStr &texterr, std::vector<std::size_t> *pi0vec=NULL, std::vector<std::size_t> *pi1vec = NULL);
  // find all input values restructure them to a list
  okay_t SortValuesInInputString(SParSearch &input,std::size_t &i0,std::size_t &l0, std::vector<CStr>  &groupvec, CStr &texterr);
  // find end of nested struct and give back length inside, start in nested struct
  std::size_t FindLengthOfNested(CStr &inputstr, std::size_t istr, char *tb0, char *tb1);
  // set a new struct item for a value
  void GetValueAssign(SParSearch &input, std::vector<CStr>  &groups, CStr &valname, CStr &unit, std::size_t istr, std::size_t lstr);
  // Get Unit if available before equal sign set istr and lstr accordingly
  okay_t GetUnitBeforeEqualSign(SParSearch &input, std::size_t &istr, std::size_t &lstr, CStr &unit, CStr &texterr);
  // set a new struct item for a table values
  void GetTableAssign(SParSearch &input, std::vector<CStr>  &groups, CStr &tablevalname, CStr &tableunits, std::size_t istr, std::size_t lstr);
  // structure from valstr all values 
  okay_t StructVariableValuesFromString(CStr &valstr, std::size_t iline, CStrV textvec, CStrM &valstrmat, bool &istext, CStr &texterr);
  // Suche alle Variablen
  okay_t VariabeValuesFindWithRegEx(CStr &inputval, CStrV &valvec, CStr &texterr);

  okay_t  VariabeValuesCheckValueVector(std::size_t iline, CStrV &valstr, CStr &texterr);

  okay_t VariabeValuesSetValueMAtrix(std::size_t iline, CStrV &valvec, CStrV &textvec, CStrM &varvalues, bool &flag_is_text, CStr &texterr);

  okay_t SetVariabeValues(CParVarValCollect &parmeterset, CStr &varname, std::vector<CStr> &groupsvec, CStr &varunit, CStrM &varvalues, bool flag_is_text, CStr &texterr);
  okay_t SetTableVariabeValues(CParVarValCollect &parmeterset, CStr &varnames, std::vector<CStr> &groupsvec, CStr &varunits, CStrM &varvalues, bool flag_is_text, CStr &texterr);

  CStrM  GetTableVariables(const CStrM &vartabvalues, std::size_t i, std::size_t n);

  bool ValueIsNumber(const slf::CStr &item);
  bool ValueIsNumber(const slf::CStr *pitem);
  bool ValueIsNumberOrText(const slf::CStr &item);
  bool ValueIsText(const slf::CStr &item);
  bool ValueIsText(const slf::CStr *pitem);
  
  okay_t FindValuesParamtersSimple(CStrV &inputvec, SParSearch &input, CStr &texterr);

  //-------------------------
  // read parameter from file
  //-------------------------
  okay_t CPar::readFileInNewParSet(const char *filename)
  {
    CStrV inputvec;
    CStr  texterr,tt;

    if (readFile(filename, inputvec, texterr) != OKAY)
    {
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    // read Parameter from inputvec
    if (readParameters(inputvec, tt) != OKAY)
    {
      deleteLastParSet();
      texterr.catFormat("Error in CPar::readStrVecInNewParSet(); %s\n", tt.c_str());
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    if (pLog) pLog->writeMessage(mMessage);
    return mMessage.IsOkay();
  }
  //------------------------------------------------
  // read parameter CStr 
  // read CStr varbiable and set a new Parameterset
  //------------------------------------------------
  okay_t CPar::readStrInNewParSet(const CStr &text)
  {
    CStrV inputvec;
    CStr  texterr, tt;

    slf::SlfStrVSplit(inputvec, text, "\n");
    // read Parameter from inputvec
    if (readParameters(inputvec, tt) != OKAY)
    {
      deleteLastParSet();
      texterr.catFormat("Error in CPar::readStrVecInNewParSet(); %s\n", tt.c_str());
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    if (pLog) pLog->writeMessage(mMessage);
    return mMessage.IsOkay();
  }
  //------------------------------------------------
  // read parameter CStrV text vector
  // read CStrV varbiable and set a new Parameterset
  //------------------------------------------------
  okay_t CPar::readStrVecInNewParSet(const CStrV &textvec)
  {
    CStr  texterr, tt;
    // create new structure
    createNewParSet();

    CStrV textinternvec(textvec);

    // read Parameter from inputvec
    if (readParameters(textinternvec, tt) != OKAY)
    {
      deleteLastParSet();
      texterr.catFormat("Error in CPar::readStrVecInNewParSet(); %s\n", tt.c_str());
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
         
      return mMessage.IsOkay();
    }

    if (pLog) pLog->writeMessage(mMessage);
    
    return mMessage.IsOkay();

  }

  // Parameterdescription in Simple Way(ini-File)
  // read a new File and set a new Parameterset
  okay_t CPar::readSimpleParFileInNewParSet(const char *filename)
  {
    CStrV inputvec;
    CStr  texterr, tt;

    if (readFile(filename, inputvec, texterr) != OKAY)
    {
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    // read Parameter from inputvec
    if (readSimpleParameters(inputvec, tt) != OKAY)
    {
      deleteLastParSet();
      texterr.catFormat("Error in CPar::readStrVecInNewParSet(); %s\n", tt.c_str());
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    if (pLog) pLog->writeMessage(mMessage);
    return mMessage.IsOkay();
  }

  okay_t CPar::readFileInParSet(const char *filename, const std::size_t i_par_set)
  {
    CStr  texterr, tt;
    CStrV inputvec;

    if (readFile(filename, inputvec, tt) != OKAY)
    {
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    if (i_par_set < mNInputParVec) // exists
    {
      mIInputParVec = i_par_set;
    }
    else if (i_par_set == mNInputParVec) // build new parset
    {
      createNewParSet();
    }
    else // error
    {
      mMessage.SetErr(PAR_PARSET, "wrong i_par_set demand (i_par_set > n_par_set)");
      if (pLog) pLog->writeMessage(mMessage);
    }

    // read Parameter from inputvec
    if (readParameters(inputvec, tt) != OKAY)
    {
      deleteLastParSet();
      texterr.catFormat("Error in CPar::readFileAndParameters(%s); %s\n", filename, tt.c_str());
      mMessage.SetErr(PAR_FILEREAD, texterr.c_str());
      if (pLog) pLog->writeMessage(mMessage);
      return mMessage.IsOkay();
    }

    // write to log
    if (pLog) pLog->writeMessage(mMessage);
    mMessage.Clear();

    return mMessage.IsOkay();
  }
  okay_t CPar::readFile(const char *filename, CStrV &inputvec, CStr &texterr)
  {
    // check Parameterfile 
    if (SysExistFile(filename))
    {
      if (SysReadFile(filename, inputvec, texterr) != OKAY)
      {
        return NOT_OK;
      }
    }
    else
    {
      texterr.catFormat("Error in CPar::readFileAndParameters(%s); Could not find file\n", filename);
      return NOT_OKAY;
    }
    return OKAY;
  }
  // read parameter with string
  // inputvec      
  okay_t CPar::readParameters(CStrV &inputvec, CStr &texterr)
  {
    // build a new parameterset
    mInputParVec.resize(mInputParVec.size() + 1);

    SParSearch input(mInputParVec.back());
    
    // check if empty
    if (inputvec.isEmpty())
    {
      texterr.cat("Input is empty");
      return NOT_OK;
    }

    // replace all text "text0", "text1", "text2" ... with #00000,#00001,#00002, ...
    // textvec[i] => "texti" => #i (5 stellen)
    // and cutoff comment sign with SLFPAR_COMMMENT
    if (PrepareStructure(SLFPAR_COMMMENT, inputvec, input.textvec, texterr) != OKAY)
    {
      return NOT_OK;
    }

    // make string from inputvec and put start position of each line in lineposvec
    // inputvec = {"a=10;","b=","0.3;","txt=#00001;"}
    // => inputstr = "a=10;b=0.3;txt=#00001;"  
    // => i0linevec = {0,5,7,11}
    // => llinevec = {4,2,4,11}
    // => i0textvec = {15}
    MakeStringFromInputVec(inputvec, input.inputstr, input.i0linevec, input.llinevec, input.i0textvec);

    // no more needed
    inputvec.clear();

    // check for brackets, change from SLFPAR_VAL_END to SLFPAR_VEC_ROW_DELIM in Vector brackets
    // get nested indices of group
    if (CheckInputString(input, texterr) != OKAY)
    {
      return NOT_OK;
    }

    // find all input values restructure them to a list valueStringVec and tableValueStringVec
    std::vector<CStr> groupvec;
    std::size_t i0 = 0, l0 = input.inputstr.size();
    while (true)
    {
      if (SortValuesInInputString(input, i0, l0, groupvec, texterr) != OKAY)
      {
        return NOT_OK;
      }
      if (l0 == 0) break;
    }

    // structure values from valueStringVec
    for (std::size_t i = 0; i < input.valueStringVec.size(); ++i)
    {
      CStrM valstrmat;
      bool istext;
      if (StructVariableValuesFromString(input.valueStringVec[i].value,input.valueStringVec[i].ilinevalue
                                        ,input.textvec, valstrmat, istext,texterr) != OKAY)
      {
        return NOT_OK;
      }
      // set variable to parameterset
      if (SetVariabeValues(input.parmeterset, input.valueStringVec[i].valname, input.valueStringVec[i].groupvec
                          , input.valueStringVec[i].unit, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OKAY;
      }


    }

    // structure values from valueStringVec
    for (std::size_t i = 0; i < input.tableValueStringVec.size(); ++i)
    {
      CStrM valstrmat;
      bool istext;
      if (StructVariableValuesFromString(input.tableValueStringVec[i].values, input.tableValueStringVec[i].ilinevalue
        , input.textvec, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OK;
      }

      // set variable to parameterset
      if (SetTableVariabeValues(input.parmeterset, input.valueStringVec[i].valname, input.valueStringVec[i].groupvec
        , input.valueStringVec[i].unit, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OKAY;
      }


    }

    return OKAY;
  }
  
  // read parameter with string
  // inputvec in Simple description      
  okay_t CPar::readSimpleParameters(CStrV &inputvec, CStr &texterr)
  {
    // build a new parameterset
    mInputParVec.resize(mInputParVec.size() + 1);

    SParSearch input(mInputParVec.back());
    // check if empty
    if (inputvec.isEmpty())
    {
      texterr.cat("Input is empty");
      return NOT_OK;
    }

    // replace all text "text0", "text1", "text2" ... with #00000,#00001,#00002, ...
    // textvec[i] => "texti" => #i (5 stellen)
    // and cutoff comment sign with SLFPAR_COMMMENT
    if (PrepareStructure(SLFPAR_COMMMENT_SIMPLE,inputvec, input.textvec, texterr) != OKAY)
    {
      return NOT_OK;
    }

    // go through each line and detect group and variable
    if( FindValuesParamtersSimple(inputvec, input, texterr) != OKAY)
    {
      return NOT_OK;
    }

    // structure values from valueStringVec
    for (std::size_t i = 0; i < input.valueStringVec.size(); ++i)
    {
      CStrM valstrmat;
      bool istext;
      if (StructVariableValuesFromString(input.valueStringVec[i].value, input.valueStringVec[i].ilinevalue
        , input.textvec, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OK;
      }
      // set variable to parameterset
      if (SetVariabeValues(input.parmeterset, input.valueStringVec[i].valname, input.valueStringVec[i].groupvec
        , input.valueStringVec[i].unit, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OKAY;
      }


    }

    // structure values from valueStringVec
    for (std::size_t i = 0; i < input.tableValueStringVec.size(); ++i)
    {
      CStrM valstrmat;
      bool istext;
      if (StructVariableValuesFromString(input.tableValueStringVec[i].values, input.tableValueStringVec[i].ilinevalue
        , input.textvec, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OK;
      }

      // set variable to parameterset
      if (SetTableVariabeValues(input.parmeterset, input.valueStringVec[i].valname, input.valueStringVec[i].groupvec
        , input.valueStringVec[i].unit, valstrmat, istext, texterr) != OKAY)
      {
        return NOT_OKAY;
      }


    }

    return OKAY;
  }



  //okay_t CPar::readParameters(CStrV &inputvec, CStr &texterr)
  //{
  //  std::size_t irow = 0;
  //  std::size_t istr = 0;

  //  // build a new parameterset
  //  mInputParVec.resize(mInputParVec.size() + 1);

  //  SParSearch  group_info(inputvec,mInputParVec.back());
  //  CStrV       textvec;

  //  if (inputvec.isEmpty())
  //  {
  //    texterr.cat("Input is empty");
  //    return NOT_OK;
  //  }
 
  //  // replace all text "text1", "text2", "text3" ... with #00000,#00001,#00002, ...
  //  // and cutoff comment sign with SLFPAR_COMMMENT
  //  if (PrepareStructure(inputvec, textvec, texterr) != OKAY)
  //  {
  //    return NOT_OK;
  //  }

  //  // parse structure for data
  //  if (readGroup(group_info, textvec, texterr) != OKAY)
  //  {
  //    return NOT_OK;
  //  }

  //  return OKAY;
  //}


  //-------------------------------------------------------------------------------------------
  // parses string-vector inputvec
  // inputvec    string-vector with all parameter input
  // group_innfo group-infos and depth group mentioned here
  // textvec     contains all substituded text pattern from inputvec
  // texterr     contains error log
  ////okay_t CPar::readGroup(SParSearch &group_info, CStrV &textvec, CStr &texterr)
  ////{
  ////  CStr word;
  ////  std::size_t itype;
  ////  while (!group_info.inputvec.isoverend(group_info.iactparamvec, group_info.iactparamstr))
  ////  {
  ////    // search next item
  ////    if (group_info.inputvec.getNextWord(group_info.iactparamvec, group_info.iactparamstr, word, &itype, SLF_SYM_LIST_ALL) == OKAY)
  ////    {

  ////      if (itype == std::string::npos) // text gefunden
  ////      {
  ////        if (readCheckWordIfNestedGroup(group_info))
  ////        {
  ////          // add groupname
  ////          group_info.groupsvec.append(word);
  ////          // read nested group
  ////          if (readNestedGroup(group_info, textvec, texterr) != OKAY)
  ////          {
  ////            return NOT_OKAY;
  ////          }
  ////          group_info.groupsvec.delete_last();
  ////        }
  ////        else if (readCheckWordIfVariable(group_info))
  ////        {
  ////          group_info.name = word;
  ////          if (readVariable(group_info, textvec, texterr) != OKAY)
  ////          {
  ////            return NOT_OKAY;
  ////          }

  ////        }
  ////      }
  ////    } //
  ////    else
  ////    {
  ////      break;
  ////    }
  ////  } // end while
  ////  
  ////  return OKAY;
  ////}
  //okay_t CPar::readNestedGroup(SParSearch &group_info, CStrV &textvec, CStr &texterr)
  //{
  //  CStr word;

  //  // build nested group
  //  CStrV parametervec;
  //  SParSearch group_info_nested(parametervec, group_info.parmeterset);

  //  // copy groups
  //  group_info_nested.groupsvec = group_info.groupsvec;

  //  // copy nested text into nested group
  //  group_info.inputvec.cpy_to(group_info_nested.inputvec, group_info.istartparamvec, group_info.istartparamstr, group_info.iendparamvec, group_info.iendparamstr);

  //  // read nested group
  //  if (readGroup(group_info_nested, textvec, texterr) != OKAY)
  //  {
  //    return NOT_OK;
  //  }

  //  // count one position up to go on in search
  //  //++group_info.iactparamstr;

  //  // return
  //  return OKAY;
  //}
  // read variable
//  okay_t CPar::readVariable(SParSearch &group_info, CStrV &textvec, CStr &texterr)
//  {
//    CStr word;
//    std::size_t itype;
//    std::size_t ivec = group_info.istartparamvec;
//    std::size_t istr = group_info.istartparamstr;
//    std::size_t iivec = group_info.istartparamvec;
//    std::size_t iistr = group_info.istartparamstr;
//
//    CStrV groupsvec = group_info.groupsvec;
//    std::size_t igroupsadd = 0;
//    CStr  varname;
//    CStr  varunit;
//    CStrM varvalues;
//    bool  flag_is_text;
//
//    // check if subgroups with "." is set
//search_for_group:
//    if (group_info.inputvec.getNextWord(ivec, istr, word, &itype, SLFPAR_GROUP_DELIM) == OKAY)
//    {
//      if (itype == 0) // SLFPAR_GROUP_DELIM
//      {
//        // add name to group
//        groupsvec.append(group_info.name);
//        ++igroupsadd;
//        iivec = ivec;
//        iistr = istr;
//
//
//        // find next name
//        if (group_info.inputvec.getNextWord(ivec, istr, word, &itype, SLF_SYM_LIST_ALL) == OKAY)
//        {
//          if (itype == std::string::npos) // text gefunden
//          {
//            group_info.name = word;
//            goto search_for_group;
//          }
//          else
//          {
//            texterr.catFormat("during reading grooups with delimiter = <%s> in line <%i> at postion <%i> no varaiblename found", SLFPAR_GROUP_DELIM, iivec, iistr);
//            return NOT_OKAY;
//          }
//        }
//      }      
//    } // end subgroup
//
//    varname = group_info.name;
//
//    // search for equal sign
//    ivec = iivec;
//    istr = iistr;
//    if (group_info.inputvec.findFrom(SLFPAR_EQUAL_SIGN, ivec, istr))
//    {
//      // search for unit 
//      varunit = readGetVariableUnit(group_info.inputvec, iivec, iistr, ivec, istr);
//    }
//
//    group_info.istartparamvec = ivec;
//    group_info.istartparamstr = istr+strlen(SLFPAR_EQUAL_SIGN);
//
//
//    // get in string vector value part of variable
//    CStrV inputvec;
//    group_info.inputvec.cpy_to(inputvec, group_info.istartparamvec, group_info.istartparamstr, group_info.iendparamvec, group_info.iendparamstr);
//
//    if (ReadVariableValues(inputvec, group_info.istartparamvec, group_info.istartparamstr, textvec, varvalues, flag_is_text, texterr) != OKAY)
//    {
//      return NOT_OKAY;
//    }
//
//    // set variable to parameterset
//    if (setVariabeValues(group_info.parmeterset, varname, groupsvec,varunit, varvalues,flag_is_text, texterr) != OKAY)
//    {
//      return NOT_OKAY;
//    }
//
//    while (igroupsadd)
//    {
//      group_info.groupsvec.delete_last();
//      --igroupsadd;
//    }
//
//    return OKAY;
//  }
  //okay_t CPar::setVariabeValues(CParVarValCollect &parmeterset, CStr &varname, CStrV &groupsvec, CStr &varunit, CStrM &varvalues, bool flag_is_text, CStr &texterr)
  //{
  //  double val = 10.;
  //  CStr group = SlfStrVReSplit(groupsvec, ".");


  //  if (flag_is_text)
  //  {
  //    if ((varvalues.getNrows() == 1) && (varvalues.getNcols() == 1)) // single value
  //    {
  //      CStr val = varvalues.getval(0, 0);
  //      parmeterset.SetParVal(val, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //    else if ((varvalues.getNrows() > 1) && (varvalues.getNcols() == 1)) //  vector
  //    {
  //      CStrV vec = varvalues.getCol(0);
  //      parmeterset.SetParVal(vec, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //    else if ((varvalues.getNrows() == 1) && (varvalues.getNcols() > 1)) //  vector
  //    {
  //      CStrV vec = varvalues.getRow(0);
  //      parmeterset.SetParVal(vec, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //    else
  //    {
  //      parmeterset.SetParVal(varvalues, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //  }
  //  else // numeric value
  //  {
  //    CMatrixD mat;

  //    ConvertVariableValues(varvalues, mat);

  //    if ((mat.GetNrow() == 1) && (mat.GetNcol() == 1)) // single value
  //    {
  //      parmeterset.SetParVal(mat[0][0], group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //    else if ((mat.GetNrow() > 1) && (mat.GetNcol() == 1)) //  vector
  //    {
  //      
  //      CVectorD dvec(mat.GetNrow());
  //      for (std::size_t i = 0; i < dvec.size(); ++i)
  //      {
  //        dvec[i] = mat[i][0];
  //      }
  //      parmeterset.SetParVal(dvec, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //    else if ((mat.GetNrow() == 1) && (mat.GetNcol() > 1)) //  vector
  //    {
  //      CVectorD dvec(mat.GetNcol());
  //      for (std::size_t i = 0; i < dvec.size(); ++i)
  //      {
  //        dvec[i] = mat[0][i];
  //      }
  //      parmeterset.SetParVal(dvec, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //    else
  //    {
  //      parmeterset.SetParVal(mat, group.c_str(), varname.c_str(), varunit.c_str());
  //    }
  //  }

  //  return OKAY;
  //}
  //bool CPar::readCheckWordIfNestedGroup(SParSearch &group_info)
  //{
  //  CStr word;
  //  std::size_t itype;
  //  std::size_t ivec  = group_info.iactparamvec;
  //  std::size_t istr  = group_info.iactparamstr;

  //  // find nested start
  //  if (group_info.inputvec.getNextWord(ivec, istr, word, &itype, SLF_SYM_LIST_ALL))
  //  {
  //    if (itype == SLFPAR_LIST_ALL_GROUP_START_INDEX)
  //    {
  //      group_info.istartparamvec = ivec;
  //      group_info.istartparamstr = istr;
  //      if (group_info.istartparamstr >= group_info.inputvec[group_info.istartparamvec].size())
  //      {
  //        group_info.istartparamvec += 1;
  //        group_info.istartparamstr = 0;
  //      }

  //      // find nested end
  //      if (group_info.inputvec.findEndingSymbol(ivec, istr, SLFPAR_GROUP_END[0], SLFPAR_GROUP_START[0]))
  //      {
  //        if (istr)
  //        {
  //          group_info.iendparamvec = ivec;
  //          group_info.iendparamstr = istr - 1;
  //        }
  //        else
  //        {
  //          group_info.iendparamvec = ivec;
  //          if (group_info.iendparamvec) --group_info.iendparamvec;
  //          group_info.iendparamstr = group_info.inputvec.lengthOf(group_info.iendparamvec);
  //          if (group_info.iendparamstr) --group_info.iendparamstr;
  //        }
  //        group_info.iactparamvec = ivec;
  //        group_info.iactparamstr = istr + strlen(SLFPAR_GROUP_END);

  //        return true;
  //      }
  //    }
  //  }
  //  return false;
  //}
  //bool CPar::readCheckWordIfVariable(SParSearch &group_info)
  //{
  //  CStr word;
  //  std::size_t itype;
  //  std::size_t ivec0 = group_info.iactparamvec;
  //  std::size_t istr0 = group_info.iactparamstr;
  //  std::size_t ivec1 = group_info.iactparamvec;
  //  std::size_t istr1 = group_info.iactparamstr;

  //  // search first equal sign
  //  if (group_info.inputvec.findFrom(SLFPAR_EQUAL_SIGN, ivec1, istr1))
  //  {
  //    // search second end variable sign
  //    if (group_info.inputvec.findFrom(SLFPAR_VAL_END, ivec1, istr1))
  //    {
  //      // check if no wrong sign is inbetween
  //      CStrV strvec;
  //      strvec.append(SLFPAR_GROUP_START); strvec.append(SLFPAR_GROUP_END);
  //      strvec.append(SLFPAR_TABLE_START); strvec.append(SLFPAR_TABLE_END);       
  //      if (group_info.inputvec.findFromTo(strvec, itype,ivec0, istr0, ivec1, istr1))
  //      {
  //        return false;
  //      }

  //      group_info.istartparamvec = ivec0;
  //      group_info.istartparamstr = istr0;

  //      if (istr1)
  //      {
  //        group_info.iendparamvec = ivec1;
  //        group_info.iendparamstr = istr1 - 1;
  //      }
  //      else
  //      {
  //        if(ivec1) group_info.iendparamvec = ivec1-1;
  //        group_info.iendparamstr = group_info.inputvec[ivec1-1].size() - 1;
  //      }
  //      group_info.iactparamvec = ivec1;
  //      group_info.iactparamstr = istr1+strlen(SLFPAR_VAL_END);

  //      return true;
  //    }
  //  }
  //  return false;
  //}
  // read and return unit from variable
  //CStr CPar::readGetVariableUnit(const CStrV &inputvec,const std::size_t &ivec0, const std::size_t &istr0,const std::size_t &ivec1,const std::size_t &istr1)
  //{
  //  CStr unit;

  //  CStr tt = inputvec.make_str(inputvec, ivec0, istr0, ivec1, istr1);

  //  tt.findText(unit, SLFPAR_UNIT_START, SLFPAR_UNIT_END, "vs");

  //  return unit;
  //}

  //===========================================================================================================================
  //===========================================================================================================================

  //-----------------------------------------------------------------------------------------
  // prepare by take out comment signed with SLFPAR_COMMMENT and 
  // collect and replace text ("text1", "text2", "text3" ... with #00000,#00001,#00002, ...)
  //------------------------------------------------------------------------------------------
  okay_t PrepareStructure(const char *commentsign, CStrV &inputvec, CStrV &textvec, CStr &texterr)
  {
    CStrV searchvec;
    CStr  s;
    std::size_t id, irow = 0, istr = 0;
    searchvec.append(commentsign);
    searchvec.append(SLFPAR_QUOT_TEXT);

    while (inputvec.findFrom(searchvec, id, irow, istr))
    {
      if (id == 0) //SLFPAR_COMMMENT => cut off rest off line
      {
        inputvec.cut_in_row(irow, istr, std::string::npos);
        // Insert single space so that comment line is also counted
        if (inputvec[irow].size() == 0)
        {
          inputvec.insert_in_row(" ", irow, istr, 1);
        }
        
        ++irow;
        istr = 0;
      }
      else //SLFPAR_QUOT_TEXT => cut off text and reset with a #index
      {
        std::size_t iii = irow;           // store irow
        std::size_t istrstart = istr;    // store start of cutting out inclusive \"
        istr += strlen(SLFPAR_QUOT_TEXT); // add length of qout to start searching end of text
                                          // search end of text
        if (inputvec.findFrom(SLFPAR_QUOT_TEXT, irow, istr))
        {
          // Text getht über mehere Zeilen
          if (irow != iii)
          {
            texterr.catFormat("Problem in Line %i: Quotes for Text (%sText%s) not correct set ", iii, SLFPAR_QUOT_TEXT, SLFPAR_QUOT_TEXT);
            return NOT_OK;

          }

          // length of text
          iii = istr - istrstart + strlen(SLFPAR_QUOT_TEXT);

          // get text and cut out in row
          s = inputvec.get_and_cut_in_row(irow, istrstart, iii);
          // cut out quotes
          SlfStrElimAnfEnd(s, SLFPAR_QUOT_TEXT);
          // add to textvec
          textvec.append(s);

          // replace in row with #index
          s.clear();
          s.catFormat("%s%-5.5i", SLFPAR_TEXT_SUB, textvec.size() - 1);
          inputvec.insert_in_row(s, irow, istrstart, 6);

          // continue search in same row and behind text replacement
          istr = istrstart + 6;

        }
        else
        {
          texterr.catFormat("Problem in Line %i: Quotes for Text (%sText%s) not correct set ", iii, SLFPAR_QUOT_TEXT, SLFPAR_QUOT_TEXT);
          return NOT_OK;
        }
      }
    }

    return OKAY;
  }
  // make string from inputvec and put start position of each line in lineposvec
  // inputvec = {"0123456","78901234","","567"}
  // => inputstr = "012345678901234567"
  // => lineposvec = {0,7,15}
  // inputvec = {"a=10;","b=","0.3;"}
  // => inputstr = "a=10;b=0.3;"
  // => lineposvec = {0,5,7}
  void MakeStringFromInputVec(CStrV &inputvec, CStr &inputstr, CVectorU &i0linevec, CVectorU &llinevec, CVectorU &i0textvec)
  {
    std::size_t i,n;
    inputstr.clear();
    i0linevec.clear();
    llinevec.clear();
    i0textvec.clear();

    inputstr.append(inputvec[0]);
    i0linevec.push_back(0);
    llinevec.push_back(inputvec[0].size());
    n = inputvec[0].size();
    for (i = 1; i < inputvec.size(); ++i)
    {
      inputstr.append(inputvec[i]);
      i0linevec.push_back(i0linevec.back()+n);
      llinevec.push_back(inputvec[i].size());
      n = inputvec[i].size();
    }

    i = 0;
    while (true)
    {
      i = inputstr.findText(SLFPAR_TEXT_SUB, i);

      if (i == SLF_STR_NPOS)
      {
        break;
      }
      else
      {
        i0textvec.push_back(i);
        i += strlen(SLFPAR_TEXT_SUB);
      }
    }
  }
  // check for brackets
  // input      structure with string inputstr as Text for Parameterinput and
  //            vector i0linevec with start position of each line
  // tb0        c-stringcharacter set for opening (brackets)
  // tb1        c-string character set for closing (brackets)
  // texterr    string to fill incase of error
  // pi0vec     poniter to vector of start position of each opening tb0 (if wanted)
  // pi1vec     pointer to vector of end position of each closing tb1 (if wanted)
  okay_t CheckInputStringBrackets(SParSearch &input, char *tb0, char *tb1, CStr &texterr, std::vector<std::size_t> *pi0vec/* = NULL*/, std::vector<std::size_t> *pi1vec/* = NULL*/)
  {
    std::size_t index = 0,icount = 0;
    index = input.inputstr.findText(tb0, 0);
    std::vector<std::size_t> indexvec;
    std::size_t n = 0;

    if (pi0vec != NULL) pi0vec->clear();
    if (pi1vec != NULL) pi1vec->clear();


    if (index < SLF_STR_NPOS)
    {
      ++icount;
      if (pi0vec != NULL) pi0vec->push_back(index);
      if (pi1vec != NULL) 
      {
        pi1vec->push_back(index);
        indexvec.push_back(n);
        ++n;
      }
      index += strlen(tb0);
    }
    else // no tb0 found
    {
      index = input.inputstr.findText(tb1, 0);
      if (index < SLF_STR_NPOS) // but tb1 found
      {
        std::size_t iline = FindLineInInputString(input, index);
        texterr.catFormat("Problem in Line %i: closing bracket %s but no opening bracket %s found, not correct set ", iline, tb1,tb0);
        return NOT_OK;
      }

      return OKAY;
    }
    while (true)
    {
      std::size_t i0 = input.inputstr.findText(tb0, index);
      std::size_t i1 = input.inputstr.findText(tb1, index);

      if (i0 < i1) // tb0 opening bracket firt found
      {
        ++icount;
        
        if (pi0vec != NULL) pi0vec->push_back(i0);
        if (pi1vec != NULL)
        {
          pi1vec->push_back(i0);
          indexvec.push_back(n);
          ++n;
        }
        index = i0 + strlen(tb0);
      }
      else if (i1 < i0)
      {
        if (icount == 0)
        {
          std::size_t iline = FindLineInInputString(input, i1);
          texterr.catFormat("Problem in Line %i: Closing Bracket %s has no opening brakcet %s, not correct set ", iline, tb1,tb0);
          return NOT_OK;
        }
        --icount;
        
        if (pi1vec != NULL)
        {
          std::size_t i = indexvec.back();
          indexvec.pop_back();
          pi1vec->at(i) = i1;
        }
        index = i1 + strlen(tb1);
      }
      else // nothing found end
      {
        break;
      }
    }

    if (icount > 0)
    {
      std::size_t iline = FindLineInInputString(input, index);
      texterr.catFormat("Problem in Line %i: %i times no closing bracket %s has found", iline,icount, tb1);
      return NOT_OK;
    }

    return OKAY;
  }
  // check for brackets, change from SLFPAR_VAL_END to SLFPAR_VEC_ROW_DELIM in Vector brackets
  // get nested indices of group
  okay_t CheckInputString(SParSearch &input, CStr &texterr)
  {
    // check for SLFPAR_GROUP_START, SLFPAR_GROUP_END and safe start and end position
    // and safe in input.i0nestgroupvec + input.lnestgroupvec
    std::vector<std::size_t> i0vec, i1vec;
    if (CheckInputStringBrackets(input, SLFPAR_GROUP_START, SLFPAR_GROUP_END, texterr) != OKAY)
    {
      return NOT_OK;
    }
    // check for SLFPAR_VEC_START, SLFPAR_VEC_END and change after inside brackets
    // SLFPAR_VAL_END (as row delimiter matlab) to SLFPAR_VEC_ROW_DELIM
    if (CheckInputStringBrackets(input, SLFPAR_VEC_START, SLFPAR_VEC_END, texterr, &i0vec, &i1vec) != OKAY)
    {
      return NOT_OK;
    }
    for (std::size_t i = 0; i < i0vec.size(); ++i)
    {
      input.inputstr.change(SLFPAR_VAL_END, SLFPAR_VEC_ROW_DELIM
        , i0vec[i] + strlen(SLFPAR_VEC_START)
        , i1vec[i] - i0vec[i] - strlen(SLFPAR_VEC_START));
    }
    // check for SLFPAR_UNIT_START, SLFPAR_UNIT_END 
    if (CheckInputStringBrackets(input, SLFPAR_UNIT_START, SLFPAR_UNIT_END, texterr) != OKAY)
    {
      return NOT_OK;
    }
    // check for SLFPAR_TABLE_START, SLFPAR_TABLE_END 
    if (CheckInputStringBrackets(input, SLFPAR_TABLE_START, SLFPAR_TABLE_END, texterr) != OKAY)
    {
      return NOT_OK;
    }
    // check for SLFPAR_EQUAL_SIGN, SLFPAR_VAL_END 
    if (CheckInputStringBrackets(input, SLFPAR_EQUAL_SIGN, SLFPAR_VAL_END, texterr) != OKAY)
    {
      return NOT_OK;
    }

    return OKAY;
  }
  // find all input values restructure them to a list
  okay_t SortValuesInInputString(SParSearch &input,std::size_t &i0,std::size_t &l0, std::vector<CStr>  &groupvec, CStr &texterr)
  {
    std::size_t istr=i0,lstr=l0,istrnext,lstrnext,itype,itypenext;
    CStr word,wordnext;
    bool flag,flagnext;

    while (true)
    {
      // search next word istr is position after next word
      flag = input.inputstr.getNextWord(istr, word, &itype, SLF_SYM_LIST_ALL);
      // check if outside
      if (flag && (istr >= (i0 + l0))) flag = false;
      // new length
      lstr = l0 + i0 - istr;

      if (flag)
      {
        // search for overnext word
        istrnext = istr;
        flagnext = input.inputstr.getNextWord(istrnext, wordnext, &itypenext, SLF_SYM_LIST_ALL);
        // check if overnext isoutside
        if (flagnext && (istrnext >= (i0 + l0))) flagnext = false;
        // new length
        lstrnext = l0 + i0 - istrnext;

        //===========================================================================
        if (itype == SLF_STR_NPOS) // text found
        {
          //-------------------------------------------------------------------------
          if (flagnext && (itypenext == SLFPAR_LIST_ALL_GROUP_START_INDEX)) // nested group
          {
            // add groupname
            groupvec.push_back(word);

            // determine length of nested group
            lstrnext = FindLengthOfNested(input.inputstr, istrnext, SLFPAR_GROUP_START, SLFPAR_GROUP_END);

            if ((istrnext + lstrnext) > (i0 + l0)) // not found in range
            {
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: group is wrongly nested starting bracket (%s), closing bracket (%s) set wrongly,after (%s)", iline, SLFPAR_GROUP_START, SLFPAR_GROUP_END,word.c_str());
              return NOT_OK;
            }
            // build startpoint after nested
            istr = istrnext + lstrnext + strlen(SLFPAR_GROUP_END);
            // read nested
            while (true)
            {
              if (SortValuesInInputString(input, istrnext, lstrnext, groupvec, texterr) != OKAY)
              {
                return NOT_OK;
              }
              if (lstrnext == 0) break;
            }
            
            groupvec.pop_back();

            // adapt i0,l0,lstr
            l0 = i0 + l0 - istr;
            i0 = istr;
            istr = i0;
            lstr = l0;

          }
          //-------------------------------------------------------------------------
          else if (flagnext && (itypenext == SLFPAR_LIST_ALL_GROUP_DELIM_INDEX)) // struced group
          {
            // add groupname
            groupvec.push_back(word);
            // read nested
            if (SortValuesInInputString(input, istrnext, lstrnext, groupvec, texterr) != OKAY)
            {
              return NOT_OK;
            }
            groupvec.pop_back();
            // build startpoint after nested
            i0 = istrnext;
            l0 = lstrnext;
            //istr = istrnext;
            //lstr = l0;
            return OKAY;
          }
          //-------------------------------------------------------------------------
          else if (flagnext && (itypenext == SLFPAR_LIST_ALL_EQUAL_SIGN_INDEX)) // value by equal sign, no unit
          {
            // find variable assign end
            std::size_t i1 = input.inputstr.findText(SLFPAR_VAL_END, istrnext, lstrnext);
            if (i1 == SLF_STR_NPOS)
            {
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: according to equal sign no end sign (%s) found,after (%s)", iline, SLFPAR_VAL_END,word.c_str());
              return NOT_OK;
            }
            
            istrnext += strlen(SLFPAR_EQUAL_SIGN);
            lstrnext = i1 - istrnext;
            CStr unit = "";
            GetValueAssign(input, groupvec, word, unit, istrnext, lstrnext);

            // build startpoint after nested
            l0 = i0 + l0 - i1 - strlen(SLFPAR_VAL_END);
            i0 = i1 + strlen(SLFPAR_VAL_END);
            //istr = i0;
            //lstr = l0;
            return OKAY;

          }
          //-------------------------------------------------------------------------
          else if (flagnext && (itypenext == SLFPAR_LIST_ALL_UNIT_START_INDEX)) // value by unit sign start
          {
            std::size_t i1, l1;
            // determine length of nested unit
            i1 = istrnext;
            l1 = FindLengthOfNested(input.inputstr, i1, SLFPAR_UNIT_START, SLFPAR_UNIT_END);
            if ((i1 + l1) > (i0 + l0)) // not found in range
            {
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: unit is wrongly nested starting bracket (%s), closing bracket (%s) set wrongly,after (%s)", iline, SLFPAR_UNIT_START, SLFPAR_UNIT_END,word.c_str());
              return NOT_OK;
            }
            // fill unit
            CStr unit= input.inputstr.substr(i1, l1);

            istrnext = i1 + l1 + strlen(SLFPAR_UNIT_END);
            lstrnext = l0 + i0 - istrnext;

            // find variable assign
            i1 = input.inputstr.findText(SLFPAR_EQUAL_SIGN, istrnext, lstrnext);
            if (i1 == SLF_STR_NPOS)
            {
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: according to unit found no equal sign (%s) found,after (%s)", iline, SLFPAR_EQUAL_SIGN,word.c_str());
              return NOT_OK;
            }

            istrnext = i1 + strlen(SLFPAR_EQUAL_SIGN);
            lstrnext = l0 + i0 - istrnext;

            // find variable assign end
            i1 = input.inputstr.findText(SLFPAR_VAL_END, istrnext, lstrnext);
            if (i1 == SLF_STR_NPOS)
            {
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: according to equal sign no end sign (%s) found,after (%s)", iline, SLFPAR_VAL_END,word.c_str());
              return NOT_OK;
            }

            lstrnext = i1 - istrnext;
            // Value Assignment
            GetValueAssign(input, groupvec, word, unit, istrnext, lstrnext);

            // build startpoint after nested
            l0 = i0 + l0 - i1 - strlen(SLFPAR_VAL_END);
            i0 = i1 + strlen(SLFPAR_VAL_END);
            //istr = i0;
            //lstr = l0;
            return OKAY;

          }
          else
          {
            if (!flagnext)
            {
              std::size_t iline = FindLineInInputString(input, istr);
              texterr.catFormat("Problem from Line %i: no parameter structure found after name: (%s)", iline, word.c_str());
              return NOT_OK;
            }
            else if (itypenext == SLF_STR_NPOS)
            {
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: new name (%s) found after name: (%s) no istructing between", iline, wordnext.c_str(),word.c_str());
              return NOT_OK;
            }
            else
            {
              CStr t;
              if      (itypenext == SLFPAR_LIST_ALL_GROUP_START_INDEX) t = SLFPAR_GROUP_START;
              else if (itypenext == SLFPAR_LIST_ALL_GROUP_END_INDEX) t = SLFPAR_GROUP_END;
              else if (itypenext == SLFPAR_LIST_ALL_GROUP_DELIM_INDEX) t = SLFPAR_GROUP_DELIM;
              else if (itypenext == SLFPAR_LIST_ALL_UNIT_START_INDEX) t = SLFPAR_UNIT_START;
              else if (itypenext == SLFPAR_LIST_ALL_UNIT_END_INDEX) t = SLFPAR_UNIT_END;
              else if (itypenext == SLFPAR_LIST_ALL_TABLE_START_INDEX) t = SLFPAR_TABLE_START;
              else if (itypenext == SLFPAR_LIST_ALL_TABLE_END_INDEX) t = SLFPAR_TABLE_END;
              else if (itypenext == SLFPAR_LIST_ALL_EQUAL_SIGN_INDEX) t = SLFPAR_EQUAL_SIGN;
              else if (itypenext == SLFPAR_LIST_ALL_VEC_START_INDEX) t = SLFPAR_VEC_START;
              else if (itypenext == SLFPAR_LIST_ALL_VEC_END_INDEX) t = SLFPAR_VEC_END;
              else if (itypenext == SLFPAR_LIST_ALL_VEC_ROW_DELIM_INDEX) t = SLFPAR_VEC_ROW_DELIM;
              else if (itypenext == SLFPAR_LIST_ALL_VEC_COL_DELIM_INDEX) t = SLFPAR_VEC_COL_DELIM;
              else if (itypenext == SLFPAR_LIST_ALL_VAL_END_INDEX) t = SLFPAR_VAL_END;
              else                                                 t = SLFPAR_TEXT_SUB;
              std::size_t iline = FindLineInInputString(input, istrnext);
              texterr.catFormat("Problem from Line %i: Sign (%s) found after name: (%s) wrongly placed", iline, t.c_str(), word.c_str());
              return NOT_OK;


            }
          }

        }
        //===========================================================================
        else if(itype == SLFPAR_LIST_ALL_GROUP_START_INDEX) // nested group w/o name found
        {
          // determine length of nested group
          istrnext = istr;
          lstrnext = FindLengthOfNested(input.inputstr, istrnext, SLFPAR_GROUP_START, SLFPAR_GROUP_END);
          if ((istrnext + lstrnext) > (i0 + l0)) // not found in range
          {
            std::size_t iline = FindLineInInputString(input, istrnext);
            texterr.catFormat("Problem from Line %i: group is wrongly nested starting bracket (%s), closing bracket (%s) set wrongly", iline, SLFPAR_GROUP_START, SLFPAR_GROUP_END);
            return NOT_OK;
          }
          // build startpoint after nested
          istr = istrnext + lstrnext + strlen(SLFPAR_GROUP_END);
          // read nested
          while (true)
          {
            if (SortValuesInInputString(input, istrnext, lstrnext, groupvec, texterr) != OKAY)
            {
              return NOT_OK;
            }
            if (lstrnext == 0) break;
          }

          // adapt i0,l0,lstr
          l0 = i0 + l0 - istrnext;
          i0 = istrnext;
          istr = istrnext;
          lstr = l0;
        }
        //===========================================================================
        else if (itype == SLFPAR_LIST_ALL_GROUP_DELIM_INDEX) // structered group w/o name found
        {
          // determine length of nested group
          istrnext = istr;
          // determine length of nested group
          lstrnext = i0 + l0 - istrnext;
          // read nested
          if (SortValuesInInputString(input, istrnext, lstrnext, groupvec, texterr) != OKAY)
          {
            return NOT_OK;
          }
          // build startpoint after nested
          i0 = istrnext;
          l0 = lstrnext;
          return OK;
          //istr = istrnext;
          //lstr = l0;
        }
        //===========================================================================
        else if (itype == SLFPAR_LIST_ALL_TABLE_START_INDEX) // table found
        {
          // determine length of nested table
          istrnext = istr;
          lstrnext = FindLengthOfNested(input.inputstr, istrnext, SLFPAR_TABLE_START, SLFPAR_TABLE_END);
          if ((istrnext + lstrnext) > (i0 + l0)) // not found in range
          {
            std::size_t iline = FindLineInInputString(input, istrnext);
            texterr.catFormat("Problem from Line %i: table is wrongly nested starting bracket (%s), closing bracket (%s) set wrongly", iline, SLFPAR_TABLE_START, SLFPAR_TABLE_END);
            return NOT_OK;
          }
          CStr tablevalnames = input.inputstr.substr(istrnext, lstrnext);

          // build startpoint after nested
          istrnext = istrnext + lstrnext + strlen(SLFPAR_TABLE_END);
          lstrnext = i0 + l0 - istrnext;

          CStr unitnames;
          if (GetUnitBeforeEqualSign(input, istrnext, lstrnext, unitnames, texterr) != OKAY)
          {
            return NOT_OK;
          }

          // find variable assign
          std::size_t i1 = input.inputstr.findText(SLFPAR_EQUAL_SIGN, istrnext, lstrnext);
          if (i1 == SLF_STR_NPOS)
          {
            std::size_t iline = FindLineInInputString(input, istrnext);
            texterr.catFormat("Problem from Line %i: according to unit found no equal sign (%s) found", iline, SLFPAR_EQUAL_SIGN);
            return NOT_OK;
          }

          istrnext = i1 + strlen(SLFPAR_EQUAL_SIGN);
          lstrnext = l0 + i0 - istrnext;

          // find variable assign end
          i1 = input.inputstr.findText(SLFPAR_VAL_END, istrnext, lstrnext);
          if (i1 == SLF_STR_NPOS)
          {
            std::size_t iline = FindLineInInputString(input, istrnext);
            texterr.catFormat("Problem from Line %i: according to equal sign no end sign (%s) found", iline, SLFPAR_VAL_END);
            return NOT_OK;
          }

          lstrnext = i1  - istrnext;
          // Value Assignment
          GetTableAssign(input, groupvec, tablevalnames, unitnames, istrnext, lstrnext);

          // build startpoint after nested
          l0 = i0 + l0 - i1 - strlen(SLFPAR_VAL_END);
          i0 = i1 + strlen(SLFPAR_VAL_END);
          //istr = i0;
          //lstr = l0;
          return OKAY;
        }
        else
        {
          CStr t;
          if (itypenext == SLFPAR_LIST_ALL_GROUP_START_INDEX) t = SLFPAR_GROUP_START;
          else if (itypenext == SLFPAR_LIST_ALL_GROUP_END_INDEX) t = SLFPAR_GROUP_END;
          else if (itypenext == SLFPAR_LIST_ALL_GROUP_DELIM_INDEX) t = SLFPAR_GROUP_DELIM;
          else if (itypenext == SLFPAR_LIST_ALL_UNIT_START_INDEX) t = SLFPAR_UNIT_START;
          else if (itypenext == SLFPAR_LIST_ALL_UNIT_END_INDEX) t = SLFPAR_UNIT_END;
          else if (itypenext == SLFPAR_LIST_ALL_TABLE_START_INDEX) t = SLFPAR_TABLE_START;
          else if (itypenext == SLFPAR_LIST_ALL_TABLE_END_INDEX) t = SLFPAR_TABLE_END;
          else if (itypenext == SLFPAR_LIST_ALL_EQUAL_SIGN_INDEX) t = SLFPAR_EQUAL_SIGN;
          else if (itypenext == SLFPAR_LIST_ALL_VEC_START_INDEX) t = SLFPAR_VEC_START;
          else if (itypenext == SLFPAR_LIST_ALL_VEC_END_INDEX) t = SLFPAR_VEC_END;
          else if (itypenext == SLFPAR_LIST_ALL_VEC_ROW_DELIM_INDEX) t = SLFPAR_VEC_ROW_DELIM;
          else if (itypenext == SLFPAR_LIST_ALL_VEC_COL_DELIM_INDEX) t = SLFPAR_VEC_COL_DELIM;
          else if (itypenext == SLFPAR_LIST_ALL_VAL_END_INDEX) t = SLFPAR_VAL_END;
          else                                                 t = SLFPAR_TEXT_SUB;
          std::size_t iline = FindLineInInputString(input, istr);
          texterr.catFormat("Problem from Line %i: Sign or word (%s)  (%s) wrongly placed", iline, t.c_str(), word.c_str());
          return NOT_OK;

        }
      }
      else
      {
        break;
      }
    }

    i0 = i0 + l0;
    l0 = 0;
    return OKAY;
  }
  // find end of nested struct and give back length inside, start in nested struct
  std::size_t FindLengthOfNested(CStr &inputstr, std::size_t istr, char *tb0, char *tb1)
  {
    std::size_t index = istr;
    std::size_t icount = 1; // is inside nested struct
    while (true)
    {
      std::size_t i0 = inputstr.findText(tb0, index);
      std::size_t i1 = inputstr.findText(tb1, index);

      if (i0 < i1) // tb0 opening bracket firt found
      {
        ++icount;
        index = i0 + strlen(tb0);
      }
      else if (i1 < i0)
      {
        --icount;
        if (icount == 0) // end found
        {
          return (i1 - istr);
        }
        index = i1 + strlen(tb1);
      }
      else // nothing found end
      {
        break;
      }
    }
    return (inputstr.size() - 1 - istr);
  }
  // set a new struct item for a value
  void GetValueAssign(SParSearch &input, std::vector<CStr>  &groupvec, CStr &valname, CStr &unit, std::size_t istr, std::size_t lstr)
  {
    SValueStrings valstr;

    valstr.groupvec = groupvec;
    valstr.valname = valname;
    valstr.unit = unit;
    valstr.value = input.inputstr.substr(istr, lstr);
    valstr.ilinevalue = FindLineInInputString(input, istr);


    input.valueStringVec.push_back(valstr);
  }
  // Get Unit if available before equal sign set istr and lstr accordingly
  okay_t GetUnitBeforeEqualSign(SParSearch &input, std::size_t &istr, std::size_t &lstr,CStr &unit,CStr &texterr)
  {
    std::size_t l0,i0 = input.inputstr.findText(SLFPAR_UNIT_START, istr);
    std::size_t i1 = input.inputstr.findText(SLFPAR_EQUAL_SIGN, istr);

    if (i0 < i1) // unit found
    {
      i0 += strlen(SLFPAR_UNIT_START);
      l0 = FindLengthOfNested(input.inputstr, i0, SLFPAR_UNIT_START, SLFPAR_UNIT_END);
      if ((i0 + l0) > (istr + lstr)) // not found in range
      {
        std::size_t iline = FindLineInInputString(input, i0);
        texterr.catFormat("Problem from Line %i: unit is wrongly nested starting bracket (%s), closing bracket (%s) set wrongly", iline, SLFPAR_TABLE_START, SLFPAR_TABLE_END);
        return NOT_OK;
      }
      unit = input.inputstr.substr(i0, l0);
      i0 = i0 + l0 + strlen(SLFPAR_UNIT_END);
      lstr =  lstr + istr - i0;
      istr = i0;
      return OKAY;
    }
    else if (i1 < i0)
    {
      unit = "";
      return OKAY;
    }
    else // i0 = i1 =npos
    {
      std::size_t iline = FindLineInInputString(input, istr);
      texterr.catFormat("Problem from Line %i: no unit (%s) or equal sign (%s) found", iline, SLFPAR_UNIT_START, SLFPAR_EQUAL_SIGN);
      return NOT_OK;
    }
  }
  void GetTableAssign(SParSearch &input, std::vector<CStr>  &groupvec, CStr &tablevalname, CStr &tableunits, std::size_t istr, std::size_t lstr)
  {
    STableValueStrings tablevalstr;

    tablevalstr.groupvec = groupvec;
    tablevalstr.valnames = tablevalname;
    tablevalstr.units = tableunits;
    tablevalstr.values = input.inputstr.substr(istr, lstr);
    tablevalstr.ilinevalue = FindLineInInputString(input, istr);

    input.tableValueStringVec.push_back(tablevalstr);

  }

  std::size_t GetPostionFromText(const CStr &text)
  {
    double val;
    SlfFktConvertStringToDouble(text.c_str(), &val);
    return std::size_t(val + 0.5);
  }
  okay_t StructVariableValuesFromString(CStr &valstr, std::size_t iline, CStrV textvec, CStrM &valstrmat, bool &istext, CStr &texterr)
  {
    CStrV valVec;
    // Find pieces of value with regex
    if (VariabeValuesFindWithRegEx(valstr, valVec, texterr) != OKAY)
    {
      return NOT_OKAY;
    }

    // Check vektor pieces of value
    if (VariabeValuesCheckValueVector(iline, valVec, texterr) != OKAY)
    {
      return NOT_OKAY;
    }

    // Sort values into matrix, reset textsign with stored text and set flag
    if (VariabeValuesSetValueMAtrix(iline, valVec, textvec, valstrmat, istext, texterr) != OKAY)
    { 
      return NOT_OKAY;
    }

    return OKAY;
  }
  okay_t  VariabeValuesCheckValueVector(std::size_t iline, CStrV &valvec, CStr &texterr)
  {
    std::size_t icount = 0, level_brackets = 0;
    CStr        lastitem, nextitem;

    while (icount < valvec.size())
    {
      if (icount + 1 < valvec.size())
      {
        nextitem = valvec[icount + 1];
      }
      else
      {
        nextitem = "";
      }

      //--------------------------------------------------------------------------------------
      if (icount == 0) // first line
      {
        if (valvec[icount].compareText(SLFPAR_VEC_START))
        {
          if (level_brackets == 0) // ground level 0
          {
            valvec.cutRow(icount); // take out bracket
            level_brackets = 1;    // remember now in level 1
                                   // no count up
          }
          else if (level_brackets == 1) // level 1
          {
            valvec.cutRow(icount); // take out bracket
            level_brackets = 2;    // remember now in level 1
                                   // no count up
          }
          else                         // level > 2 illigal
          {
            texterr.catFormat("Error try to read values beginning from iline:%i: found mor then 2 brackets <%s>"
              , iline, SLFPAR_VEC_START);
            return NOT_OKAY;
          }
        }
        else if (valvec[icount].compareText(SLFPAR_VEC_ROW_DELIM)
          || valvec[icount].compareText(SLFPAR_VEC_COL_DELIM)
          || valvec[icount].compareText(SLFPAR_VEC_END)) // kein ,;]
        {
          valvec.cutRow(icount);     // take out ;,
        }
        else
        {
          ++icount;   // next value
        }
      }// icount = 0
      else // icount > 0 
      {
        if (ValueIsNumberOrText(lastitem)) // Vorgänger war Wert
        {
          if (valvec[icount].compareText(SLFPAR_VEC_END)) // SChliessen der Klammer
          {
            if (level_brackets == 0) //kein level
            {
              texterr.catFormat("Error try to read values start from iline:%i : found bracket close <%s> but no bracket open <%s>found at position <%i>"
                , iline, SLFPAR_VEC_END, SLFPAR_VEC_START, icount);
              return NOT_OKAY;
            }
            else if (nextitem.isEmpty()) // Ende erreicht
            {
              valvec.cutRow(icount);
              --level_brackets;
            }
            else
            {
              if (nextitem.compareText(SLFPAR_VEC_ROW_DELIM) || nextitem.compareText(SLFPAR_VEC_COL_DELIM))
              {
                valvec.cutRow(icount);
                valvec.set(SLFPAR_VEC_ROW_DELIM, icount);
                --level_brackets;
                ++icount;
              }
              else
              {
                valvec.set(SLFPAR_VEC_ROW_DELIM,icount);
                --level_brackets;
                ++icount;
              }
            }
          }
          else if (ValueIsNumberOrText(valvec[icount])) // zwei mal geht nicht
          {
            texterr.catFormat("Error try to read values start from iline:%i: found two or more values at position <%i>"
              , iline, icount);
            return NOT_OKAY;
          }
          else if (valvec[icount].compareText(SLFPAR_VEC_START))  // neuer Vector
          {
            texterr.catFormat("Error try to read values start from iline:%i: found bracket open <%s> after value <%s> at position <%i>"
              , iline, SLFPAR_VEC_START, lastitem.c_str(), icount);
            return NOT_OKAY;
          }
          else // must be delimiter
          {
            ++icount;
          }
        } // Vorgänger war Wert
        else  // Vorgänger war delimiter
        {
          if (valvec[icount].compareText(SLFPAR_VEC_START)) // Öffnen der Klammer
          {
            if (level_brackets == 0) // ground level 0
            {
              texterr.catFormat("Error try to read values start from iline:%i: found a first level bracket after a <%s> inside vector"
                , iline, SLFPAR_VEC_START);
              return NOT_OKAY;

            }
            else if (level_brackets == 1) // level 1
            {
              valvec.cutRow(icount); // take out bracket
              level_brackets = 2;    // remember now in level 1
                                     // no count up
            }
            else                         // level > 2 illigal
            {
              texterr.catFormat("Error try to read values start from iline:%i: found mor then 2 brackets <%s>"
                , iline, SLFPAR_VEC_START);
              return NOT_OKAY;
            }
          }
          else if (valvec[icount].compareText(SLFPAR_VEC_ROW_DELIM) || valvec[icount].compareText(SLFPAR_VEC_COL_DELIM)) // zweiter delimiter
          {
            valvec.cutRow(icount); // take out bracket
                                   // no count up
          }
          else
          {
            ++icount;
          }
        } // Vorgänger war delimiter
      } // icount > 0

      if (icount) lastitem = valvec[icount - 1]; // store last item

    } // end while

    if (valvec.size() == 0)
    {
      texterr.catFormat("No Value found start from iline:%i"
        , iline);
      return NOT_OKAY;
    }

    return OKAY;
  }
  okay_t VariabeValuesSetValueMAtrix(std::size_t iline, CStrV &valvec, CStrV &textvec, CStrM &varvalues, bool &flag_is_text, CStr &texterr)
  {
    std::size_t irow = 0;
    std::size_t icol = 0;
    std::size_t icount = 0;

    if (ValueIsNumber(valvec[0]))
    {
      flag_is_text = false;
    }
    else
    {
      flag_is_text = true;
    }

    while (icount < valvec.size())
    {
      if (ValueIsNumber(valvec[icount])) // is number
      {
        if (flag_is_text)
        {
          texterr.catFormat("Mixt values Text and numbers found start at iline:%i"
            , iline);
          return NOT_OKAY;
        }

        // einsortieren
        varvalues.cpy(valvec[icount], irow, icol);

      } // isnumber
      else  // is text
      {
        if (!flag_is_text)
        {
          texterr.catFormat("Mixt values Text and numbers found start at iline:%i"
            , iline);
          return NOT_OKAY;
        }

        // einsortieren
        std::size_t itext = GetPostionFromText(valvec[icount]);
        varvalues.cpy(textvec[itext], irow, icol);

      }

      // next in valvec must be delimiter
      if (++icount < valvec.size())
      {
        if (valvec[icount].compareText(SLFPAR_VEC_ROW_DELIM))
        {
          ++irow;
          icol = 0;
        }
        else
        {
          ++icol;
        }
      }
      // next value repeat while loop
      ++icount;
    } // while

      // transpose if vector and nrows == 1
    if (varvalues.getNrows() == 1)
    {
      varvalues.transpone();
    }
    return OKAY;
  }

  void GetRegExString(slf::CStrV &strv)
  {
    strv.append(SLFPAR_VEC_START);
    strv.append(SLFPAR_VEC_END);
    strv.append(SLFPAR_VEC_ROW_DELIM);
    strv.append(SLFPAR_VEC_COL_DELIM);
    strv.append("number");
    slf::CStr str;
    str.catFormat("\\%s\\d{5}", SLFPAR_TEXT_SUB);
    strv.append(str);
  }
  bool ValueIsNumber(const slf::CStr &item)
  {
    char c = item.getChar(0);
    if (!SlfStrCompare(item, SLFPAR_VEC_START)
      && !SlfStrCompare(item, SLFPAR_VEC_END)
      && !SlfStrCompare(item, SLFPAR_VEC_ROW_DELIM)
      && !SlfStrCompare(item, SLFPAR_VEC_COL_DELIM)
      && (c != SLFPAR_CHAR_TEXT_SUB)
      )
    {
      return true;
    }

    return false;
  }
  bool ValueIsNumber(const slf::CStr *pitem)
  {
    return ValueIsNumber(*pitem);
  }
  bool ValueIsText(const slf::CStr &item)
  {
    char c = item.getChar(0);
    if (c == SLFPAR_CHAR_TEXT_SUB)
    {
      return true;
    }

    return false;
  }
  bool ValueIsText(const slf::CStr *pitem)
  {
    return ValueIsText(*pitem);
  }
  bool ValueIsNumberOrText(const slf::CStr &item)
  {
    if (!SlfStrCompare(item, SLFPAR_VEC_START)
      && !SlfStrCompare(item, SLFPAR_VEC_END)
      && !SlfStrCompare(item, SLFPAR_VEC_ROW_DELIM)
      && !SlfStrCompare(item, SLFPAR_VEC_COL_DELIM)
      )
    {
      return true;
    }

    return false;
  }
  okay_t VariabeValuesFindWithRegEx(CStrV &inputvalvec, CStrV &valvec, CStr &texterr)
  {
    slf::CStrV strv;

    // build regex string
    GetRegExString(strv);
    CStr rxstr = slf::SlfBuildRegExString(strv);


    // read each line with regex into string-vector
    for (std::size_t i = 0; i < inputvalvec.size(); ++i)
    {
      CStrV linevalvec = SlfMatchWithRegExString(inputvalvec[i], rxstr);

      // look for ending to add
      if (linevalvec.size() && (i + 1 != inputvalvec.size())) // etwas ausgelesen und not last line
      {
        CStr * pstr = linevalvec.get_last();
        if (SlfStrCompare(pstr, SLFPAR_VEC_END)) // wenn vector ende, dann füge semikolon hinzu
        {
          linevalvec.append(SLFPAR_VEC_ROW_DELIM);

        }
        else if (ValueIsText(pstr)) // wenn text, dann füge komma hinzu
        {
          linevalvec.append(SLFPAR_VEC_COL_DELIM);

        }
        else if (ValueIsNumber(pstr)) // wenn number, dann füge komma hinzu
        {
          linevalvec.append(SLFPAR_VEC_COL_DELIM);
        }
      }

      // Hänge an vaVec an
      valvec.append(linevalvec);
    }

    return OKAY;
  }
  okay_t VariabeValuesFindWithRegEx(CStr &inputval, CStrV &valvec, CStr &texterr)
  {
    slf::CStrV strv;

    // build regex string
    GetRegExString(strv);
    CStr rxstr = slf::SlfBuildRegExString(strv);


    // read each line with regex into string-vector
    valvec = SlfMatchWithRegExString(inputval, rxstr);

    return OKAY;
  }
  okay_t SetVariabeValues(CParVarValCollect &parmeterset, CStr &varname, std::vector<CStr> &groupsvec, CStr &varunit, CStrM &varvalues, bool flag_is_text, CStr &texterr)
  {
    double val = 10.;
    CStr group = SlfStrVReSplit(groupsvec, ".");


    if (flag_is_text)
    {
      if ((varvalues.getNrows() == 1) && (varvalues.getNcols() == 1)) // single value
      {
        CStr val = varvalues.getval(0, 0);
        parmeterset.SetParVal(val, group.c_str(), varname.c_str(), varunit.c_str());
      }
      else if ((varvalues.getNrows() > 1) && (varvalues.getNcols() == 1)) //  vector
      {
        CStrV vec = varvalues.getCol(0);
        parmeterset.SetParVal(vec, group.c_str(), varname.c_str(), varunit.c_str());
      }
      else if ((varvalues.getNrows() == 1) && (varvalues.getNcols() > 1)) //  vector
      {
        CStrV vec = varvalues.getRow(0);
        parmeterset.SetParVal(vec, group.c_str(), varname.c_str(), varunit.c_str());
      }
      else
      {
        parmeterset.SetParVal(varvalues, group.c_str(), varname.c_str(), varunit.c_str());
      }
    }
    else // numeric value
    {
      CMatrixD mat;

      ConvertVariableValues(varvalues, mat);

      if ((mat.GetNrow() == 1) && (mat.GetNcol() == 1)) // single value
      {
        parmeterset.SetParVal(mat[0][0], group.c_str(), varname.c_str(), varunit.c_str());
      }
      else if ((mat.GetNrow() > 1) && (mat.GetNcol() == 1)) //  vector
      {

        CVectorD dvec(mat.GetNrow());
        for (std::size_t i = 0; i < dvec.size(); ++i)
        {
          dvec[i] = mat[i][0];
        }
        parmeterset.SetParVal(dvec, group.c_str(), varname.c_str(), varunit.c_str());
      }
      else if ((mat.GetNrow() == 1) && (mat.GetNcol() > 1)) //  vector
      {
        CVectorD dvec(mat.GetNcol());
        for (std::size_t i = 0; i < dvec.size(); ++i)
        {
          dvec[i] = mat[0][i];
        }
        parmeterset.SetParVal(dvec, group.c_str(), varname.c_str(), varunit.c_str());
      }
      else
      {
        parmeterset.SetParVal(mat, group.c_str(), varname.c_str(), varunit.c_str());
      }
    }

    return OKAY;
  }
  okay_t SetTableVariabeValues(CParVarValCollect &parmeterset, CStr &varnames, std::vector<CStr> &groupsvec, CStr &varunits, CStrM &vartabvalues, bool flag_is_text, CStr &texterr)
  {
    double val = 10.;
    CStr group = SlfStrVReSplit(groupsvec, ".");
    CStrV varnamevec,varunitvec;
    std::size_t nv = SlfStrVSplit(varnamevec, varnames, SLFPAR_TABLE_DELIM);
    std::size_t nu = SlfStrVSplit(varunitvec, varunits, SLFPAR_TABLE_DELIM);


    for (std::size_t i = 0; i < SLF_MIN(nu, nv); ++i)
    {
      CStr varname = varnamevec[i];
      CStr varunit = varunitvec[i];

      CStrM valstrmat = GetTableVariables(vartabvalues, i, SLF_MIN(nu, nv));

      if (SetVariabeValues( parmeterset, varname, groupsvec
                          , varunit, valstrmat, flag_is_text, texterr) != OKAY)
      {
        return NOT_OK;
      }
        
    }

    return OKAY;
  }
  // get tabel variables from table poition itab of a table with ntab positions
  //
  //<abc, def>[m, m] = 
  //  1, 2,
  //  3, 5,
  //  4, 6,
  //  7, 8,
  //  10, 34;
  //
  // ntab = 2  and itab = 0 abc [m] = 1,3,4,7,10;
  //                    = 1 def [m] = 2,5,6,8,34;
  //
  CStrM  GetTableVariables(const CStrM &vartabvalues, std::size_t itab, std::size_t ntab)
  {
    CStrV vec = vartabvalues.buildVector(true);
    CStrM mat;
    if (itab >= ntab)
    {
      itab = ntab;
      if (itab)--itab;
    }

    for (std::size_t i = 0; i < vec.getNrows(); ++i)
    {
      if (i == itab)
      {
        mat.setValue(i, 1, vec[i]);
        itab += ntab;
      }
    }
    return mat;
  }
  okay_t  VariabeValuesCheckValueVector(std::size_t ivecoffset, std::size_t istroffset, CStrV &valvec, CStr &texterr)
  {
    std::size_t icount = 0, level_brackets = 0;
    CStr        lastitem, nextitem;

    while (icount < valvec.size())
    {
      if (icount + 1 < valvec.size())
      {
        nextitem = valvec[icount + 1];
      }
      else
      {
        nextitem = "";
      }

      //--------------------------------------------------------------------------------------
      if (icount == 0) // first line
      {
        if (valvec[icount].compareText(SLFPAR_VEC_START))
        {
          if (level_brackets == 0) // ground level 0
          {
            valvec.cutRow(icount); // take out bracket
            level_brackets = 1;    // remember now in level 1
                                   // no count up
          }
          else if (level_brackets == 1) // level 1
          {
            valvec.cutRow(icount); // take out bracket
            level_brackets = 2;    // remember now in level 1
                                   // no count up
          }
          else                         // level > 2 illigal
          {
            texterr.catFormat("Error try to read values beginning from iline:%i istr:%i: found mor then 2 brackets <%s>"
              , ivecoffset, istroffset, SLFPAR_VEC_START);
            return NOT_OKAY;
          }
        }
        else if (valvec[icount].compareText(SLFPAR_VEC_ROW_DELIM)
          || valvec[icount].compareText(SLFPAR_VEC_COL_DELIM)
          || valvec[icount].compareText(SLFPAR_VEC_END)) // kein ,;]
        {
          valvec.cutRow(icount);     // take out ;,
        }
        else
        {
          ++icount;   // next value
        }
      }// icount = 0
      else // icount > 0 
      {
        if (ValueIsNumberOrText(lastitem)) // Vorgänger war Wert
        {
          if (valvec[icount].compareText(SLFPAR_VEC_END)) // SChliessen der Klammer
          {
            if (level_brackets == 0) //kein level
            {
              texterr.catFormat("Error try to read values start from iline:%i istr:%i : found bracket close <%s> but no bracket open <%s>found at position <%i>"
                , ivecoffset, istroffset, SLFPAR_VEC_END, SLFPAR_VEC_START, icount);
              return NOT_OKAY;
            }
            else if (nextitem.isEmpty()) // Ende erreicht
            {
              valvec.cutRow(icount);
              --level_brackets;
            }
            else
            {
              if (nextitem.compareText(SLFPAR_VEC_ROW_DELIM) || nextitem.compareText(SLFPAR_VEC_COL_DELIM))
              {
                valvec.cutRow(icount);
                valvec.set(SLFPAR_VEC_ROW_DELIM,icount);
                --level_brackets;
                ++icount;
              }
              else
              {
                valvec.set(SLFPAR_VEC_ROW_DELIM,icount);
                --level_brackets;
                ++icount;
              }
            }
          }
          else if (ValueIsNumberOrText(valvec[icount])) // zwei mal geht nicht
          {
            texterr.catFormat("Error try to read values start from iline:%i istr:%i: found two or more values at position <%i>"
              , ivecoffset, istroffset, icount);
            return NOT_OKAY;
          }
          else if (valvec[icount].compareText(SLFPAR_VEC_START))  // neuer Vector
          {
            texterr.catFormat("Error try to read values start from iline:%i istr:%i: found bracket open <%s> after value <%s> at position <%i>"
              , ivecoffset, istroffset, SLFPAR_VEC_START, lastitem.c_str(), icount);
            return NOT_OKAY;
          }
          else // must be delimiter
          {
            ++icount;
          }
        } // Vorgänger war Wert
        else  // Vorgänger war delimiter
        {
          if (valvec[icount].compareText(SLFPAR_VEC_START)) // Öffnen der Klammer
          {
            if (level_brackets == 0) // ground level 0
            {
              texterr.catFormat("Error try to read values start from iline:%i istr:%i: found a first level bracket after a <%s> inside vector"
                , ivecoffset, istroffset, SLFPAR_VEC_START);
              return NOT_OKAY;

            }
            else if (level_brackets == 1) // level 1
            {
              valvec.cutRow(icount); // take out bracket
              level_brackets = 2;    // remember now in level 1
                                     // no count up
            }
            else                         // level > 2 illigal
            {
              texterr.catFormat("Error try to read values start from iline:%i istr:%i: found mor then 2 brackets <%s>"
                , ivecoffset, istroffset, SLFPAR_VEC_START);
              return NOT_OKAY;
            }
          }
          else if (valvec[icount].compareText(SLFPAR_VEC_ROW_DELIM) || valvec[icount].compareText(SLFPAR_VEC_COL_DELIM)) // zweiter delimiter
          {
            valvec.cutRow(icount); // take out bracket
                                   // no count up
          }
          else
          {
            ++icount;
          }
        } // Vorgänger war delimiter
      } // icount > 0

      if (icount) lastitem = valvec[icount - 1]; // store last item

    } // end while

    if (valvec.size() == 0)
    {
      texterr.catFormat("No Value found start from iline:%i istr:%i"
        , ivecoffset, istroffset);
      return NOT_OKAY;
    }

    return OKAY;
  }
  okay_t VariabeValuesSetValueMAtrix(std::size_t ivecoffset, std::size_t istroffset, CStrV &valvec, CStrV &textvec, CStrM &varvalues, bool &flag_is_text, CStr &texterr)
  {
    std::size_t irow = 0;
    std::size_t icol = 0;
    std::size_t icount = 0;

    if (ValueIsNumber(valvec[0]))
    {
      flag_is_text = false;
    }
    else
    {
      flag_is_text = true;
    }

    while (icount < valvec.size())
    {
      if (ValueIsNumber(valvec[icount])) // is number
      {
        if (flag_is_text)
        {
          texterr.catFormat("Mixt values Text and numbers found start at iline:%i istr:%i"
            ,ivecoffset, istroffset);
          return NOT_OKAY;
        }

        // einsortieren
        varvalues.cpy(valvec[icount], irow, icol);

      } // isnumber
      else  // is text
      {
        if (!flag_is_text)
        {
          texterr.catFormat("Mixt values Text and numbers found start at iline:%i istr:%i"
            , ivecoffset, istroffset);
          return NOT_OKAY;
        }

        // einsortieren
        std::size_t itext = GetPostionFromText(valvec[icount]);
        varvalues.cpy(textvec[itext], irow, icol);

      }

      // next in valvec must be delimiter
      if (++icount < valvec.size())
      {
        if (valvec[icount].compareText(SLFPAR_VEC_ROW_DELIM))
        {
          ++irow;
          icol = 0;
        }
        else
        {
          ++icol;
        }
      }
      // next value repeat while loop
      ++icount;
    } // while

      // transpose if vector and nrows == 1
    if (varvalues.getNrows() == 1)
    {
      varvalues.transpone();
    }
    return OKAY;
  }

  okay_t ReadVariableValues(CStrV &inputvalvec, std::size_t ivecoffset, std::size_t istroffset, CStrV &textvec, CStrM &varvalues, bool &flag_is_text, CStr &texterr)
  {
    CStrV valVec;
    // Find pieces of value with regex
    if (VariabeValuesFindWithRegEx(inputvalvec, valVec, texterr) != OKAY)
    {
      return NOT_OKAY;
    }

    // Check vektor pieces of value
    if (VariabeValuesCheckValueVector(ivecoffset, istroffset, valVec, texterr) != OKAY)
    {
      return NOT_OKAY;
    }

    // Sort values into matrix, reset textsign with stored text and set flag
    if (VariabeValuesSetValueMAtrix(ivecoffset, istroffset, valVec, textvec, varvalues, flag_is_text, texterr) != OKAY)
    {
      return NOT_OKAY;
    }

    return OKAY;
  }

  void ConvertVariableValues(CStrM &varvalues, CMatrixD &mat)
  {
    mat.SetNrow(varvalues.getNrows());
    mat.SetNcol(varvalues.getNcols());
    
    for (std::size_t i = 0; i < mat.GetNrow(); ++i)
    {
      for (std::size_t j = 0; j < mat.GetNcol(); ++j)
      {
        SlfFktConvertStringToDouble(varvalues.getval(i, j), mat[i][j]);
      }
    }
    
  }

  std::size_t FindLineInInputString(SParSearch &input, std::size_t istr)
  {
    std::size_t i;

    for (i = input.i0linevec.size(); i > 0; --i)
    {
      if (istr > input.i0linevec[i - 1])
      {
        return i-1;
      }
    }
    return SLF_STR_NPOS;
  }

  // read variable from Simple Struct
  okay_t FindValuesParamtersSimple(CStrV &inputvec, SParSearch &input, CStr &texterr)
  {
    CStrV searchvec;
    CStr  s;
    CStr  group = "";
    std::size_t id, irow = 0, istr = 0;
    searchvec.append(SLFPAR_GROUP_START_SIMPLE);
    searchvec.append(SLFPAR_EQUAL_SIGN_SIMPLE);

    while (inputvec.findFrom(searchvec, id, irow, istr))
    {
      if (id == 0) //SLFPAR_GROUP_START_SIMPLE => read group
      {
        s = inputvec[irow];
        std::size_t istr0 = istr + 1,istr1;
        istr1 = s.findText(SLFPAR_GROUP_END_SIMPLE, istr0);

        if (istr1 == SLF_STR_NPOS)
        {
          texterr.catFormat("In Parameterfile in Line <%i> no closing bracket <%s> found", irow + 1, SLFPAR_GROUP_END_SIMPLE);
          return NOT_OKAY;
        }
        if (istr1) --istr1;
        if (istr1 >= istr0)
        {
          group = s.getString(istr0, istr1 - istr0 + 1);
        }
      }
      else //SLFPAR_EQUAL_SIGN_SIMPLE variable
      {
        s = inputvec[irow];
        CStr valname;
        if (istr > 1)
        {
          valname = s.getString(0, (istr-1) + 1);
          valname.elimAnfEndC();
        }
        CStr val = s.getString(istr + strlen(SLFPAR_EQUAL_SIGN_SIMPLE), s.size());
        val.elimAnfEndC();

        SValueStrings valstr;

        valstr.groupvec.resize(1);
        valstr.groupvec[0] = group;
        valstr.valname = valname;
        valstr.unit = "";
        valstr.value = val;
        valstr.ilinevalue = 0;


        input.valueStringVec.push_back(valstr);

      }

      ++irow;
      istr = 0;
    }
    return OKAY;
  }
} // namespace slf