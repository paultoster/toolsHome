#ifndef SLF_PARDEF_H_INCLUDED
#define SLF_PARDEF_H_INCLUDED
 
//========================================================================================================
//
// Parameter description
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
// 10.2
// 10.5
// 10.7
// 11;
// var1[m/s]=[[10.2,10.5,10.7],[9.1,9.2,9.3]]; matrix value double w unit m/s and 2 rows and 3 columns
// var1[m/s]=[10.2,10.5,10.7;9.1,9.2,9.3];     same
//
// <var1, var2, var3>[m,m/s,m/s/s]=
//  1   , 10   , 100
//  2   ,  8   , 101
//  3   ,  4   , 110
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
// Functions:
// ----------
// 
//========================================================================================================
#define SLFPAR_COMMMENT     "!"
#define SLFPAR_QUOT_TEXT    "\""


#define SLFPAR_GROUP_START  "{"
#define SLFPAR_GROUP_END    "}"
#define SLFPAR_GROUP_DELIM  "."
#define SLFPAR_UNIT_START   "["
#define SLFPAR_UNIT_END     "]"
#define SLFPAR_TABLE_START  "<"
#define SLFPAR_TABLE_END    ">"
#define SLFPAR_EQUAL_SIGN   "="
#define SLFPAR_VEC_START    "["
#define SLFPAR_VEC_END      "]"
#define SLFPAR_VEC_ROW_DELIM "|"
#define SLFPAR_VEC_COL_DELIM ","
#define SLFPAR_VAL_END      ";"
#define SLFPAR_TEXT_SUB     "#"


#define SLF_SYM_LIST_ALL    SLFPAR_GROUP_START \
                            SLFPAR_GROUP_END \
                            SLFPAR_GROUP_DELIM \
                            SLFPAR_UNIT_START \
                            SLFPAR_UNIT_END \
                            SLFPAR_TABLE_START \
                            SLFPAR_TABLE_END \
                            SLFPAR_EQUAL_SIGN \
                            SLFPAR_VEC_START \
                            SLFPAR_VEC_END \
                            SLFPAR_VEC_ROW_DELIM \
                            SLFPAR_VEC_COL_DELIM \
                            SLFPAR_VAL_END \
                            SLFPAR_TEXT_SUB

#define SLFPAR_LIST_ALL_GROUP_START_INDEX 0
#define SLFPAR_LIST_ALL_GROUP_END_INDEX   1
#define SLFPAR_LIST_ALL_GROUP_DELIM_INDEX 2
#define SLFPAR_LIST_ALL_UNIT_START_INDEX  3
#define SLFPAR_LIST_ALL_UNIT_END_INDEX    4
#define SLFPAR_LIST_ALL_TABLE_START_INDEX 5
#define SLFPAR_LIST_ALL_TABLE_END_INDEX   6
#define SLFPAR_LIST_ALL_EQUAL_SIGN_INDEX  7
#define SLFPAR_LIST_ALL_VEC_START_INDEX   8
#define SLFPAR_LIST_ALL_VEC_END_INDEX           9
#define SLFPAR_LIST_ALL_VEC_ROW_DELIM_INDEX    10
#define SLFPAR_LIST_ALL_VEC_COL_DELIM_INDEX    11
#define SLFPAR_LIST_ALL_VAL_END_INDEX          12
#define SLFPAR_LIST_ALL_TEXT_SUB_INDEX         13
#define SLFPAR_LIST_ALL_LENGTH                 14


#define SLFPAR_TABLE_DELIM  ","

#define SLFPAR_CHAR_TEXT_SUB '#'

#define SLFPAR_COMMMENT_SIMPLE     ";"
#define SLFPAR_GROUP_START_SIMPLE  "["
#define SLFPAR_GROUP_END_SIMPLE    "]"
#define SLFPAR_EQUAL_SIGN_SIMPLE   "="

#endif // SLF_PARDEF_H_INCLUDED