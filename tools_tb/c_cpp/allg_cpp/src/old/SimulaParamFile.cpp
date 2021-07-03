//
// SimulaParamFile.cpp
//
// Functions to read Parameterfile as described
// 
//
// group-definition
// ================
// [group]          ! highest group-level (level 0)
// (subgroup)       ! first   subgroup    (level 1)
// ((subsubgroup))  ! second  subgroup    (level 2)
// ((( ... )))      ! etc
// 
// copy
// -----------------
// [groupA].copy = [groupB]         ! copy from group A into grou B
//                                  ! always same level
//
// (sgroup2).copy = (sgroup3)                   ! copy of same subgroup level
// (sgroup2).copy = [groupA](sgroup3)           !
//
//
// variable-definition
// ===================
//  varname [unit] = value                          ! Single-value
//  varname [unit] = [value1,value2,value3,value4]  ! Vector-values
//  varname [unit] = [value1,value2;value3,value4]  ! matrix-values ,: trennt Zeile
//                                                       ;: trennt Spalte
//
//  Attribute
//
//  Varname.copy   = [groupA](sgroup3)Var1name ! copy of a variable
//  Varname.copy   = Var1name                  ! copy from same /group/subgroup
//  Varname.factor = value                     ! factor
//  Varname.offset = value                     ! offset
//  Varname.unit   = unit                     ! unit
//
//  table
//  =========================
//
//  /tablename\                              ! 1D-table
//  xtable [unit] = [,,,]                    ! x-Vector variable-name must be xtable
//  ytable [unit] = [,,,]                    ! y-Vector variable-name must be ytable  ytable = f(xtable)
//
//  //tablename\\                            ! 2D-table
//  xtable [unit] = [,,]                     ! x-Vector variable-name must be xtable len(xtable) = n
//  ytable [unit] = [,,,]                    ! y-Vector variable-name must be ytable len(ytable) = m
//  ztable [unit] = [,,,;,,,;,,,,]           ! z-Matrix variable-name must be ytable length must be [nxm] 
//
//  attributs table
//  ---------------
//  /table1\.copy = [group]/table0\               ! copy table
//
//  attributs variable must be written direct unde definition to recognize correctly
//  /table1\
//  xtable.copy   = [groupA](sgroup3)Var1name   ! copy
//  xtable.copy   = Var1name                    !
//  xtable.copy   = [groupA](sgroup3)/table0\
//  xtable.factor = value                       ! factor
//  xtable.offset = value                       ! offset
//                                              !  = value*factor + offset
//  xtable.unit    = unit                       ! unit
//
//  special vectorlist (time orbit format)
//  ===================================
//  {var1 [m] var2 var3}
//  0 0 10
//  1 2 11
//  2 2 11
//  3 1 12
//  4 0 15
//
//  is equal to:
//  var1 [m] = 0,1,2,3,4;
//  var2 = 0,2,2,1,0;
//  var3 = 10,11,11,12,15;
//
// comment
// ==========
//
// start with
// !
//
// values: 
// ======
//
// var = [1,2,3,4;5,6,7,8]
// var = [1,2,3,4;
//        5,6,7,8]
//
// variable from matlabfile
// ========================
//
// #matread filename.mat variablename2_in_file variablename2_in_file
// for example:   
//                    #matread v6_dc_4398.mat omega_Teff
//                    Vel        = omega_Teff
//                    Vel.unit   = m/s
//                    Vel.factor = 0.27778
// include:
// ========
// #include abc.par
// introduce paramter-file at this point
// 
// instances:
// ===========
// -could be mentioned with <0>,<1>,<2>
// for example 3 different vaules for vel0, vel1 = const:
//                   <0>
//                   [ABC]
//                   vel0 [km/h] = 10
//                   vel1 [km/h] = 100
//                   <1>
//                   [ABC]
//                   vel0 [km/h] = 20
//                   <3>
//                   [ABC]
//                   vel0 [km/h] = 30
//
// -or directly in variable by counting
//                   
//                   [ABC]
//                   vel0 [km/h] = <10,20,30>
//                   vel1 [km/h] = 100
// -or directly in variable by iterating
//                   
//                   [ABC]
//                   vel0 [km/h] = <10:10:30>
//                   vel1 [km/h] = 100
//

#include <fstream>      // std::ofstream

#include "Simula.h"
#include "SimulaParam.h"
#include "SimulaHelp.h"
#include "HlpString.h"


//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
// Parameter Klasse File Funtionen Read
//====================================================================================
//====================================================================================
//====================================================================================
//====================================================================================
namespace Simula
{

  // read  functions
  // read parameter file
  // - read all parameters in string-buffer, resolve all includes in parameterfile
  //================================================================================================================================
  //================================================================================================================================
  status_type CPar::readParFile(char *pfullfilename)
  {
    std::string buffer;

    // read Parfile in string buffer and eliminate includes
    //-----------------------------------------------------
    if( readRarFileStringBuffer(pfullfilename,buffer) != STATUS_OKAY )
    {
      return STATUS_NOT_OKAY;
    }

    // Interpretate buffer, build group hierachie with variables
    //---------------------------------------------------------
    ParInterpr.interprStringBuffer(buffer);

    return STATUS_OKAY;
  }
  //====================================================================================
  // call from
  // CPar::readParFile()
  // private: fill buffer eliminate includes
  //====================================================================================
  status_type CPar::readRarFileStringBuffer(char *pfullfilename,std::string &buffer)
  {
    std::string fullfilename = pfullfilename;
    s_t         ibuffer=0;
    bool flag = true;
    while( flag )
    {
      // read param-file and fill buffer at position ibuffer
      if( readParFileIntoString(fullfilename,buffer,ibuffer) != STATUS_OKAY )
      {
        return STATUS_NOT_OKAY;
      }
      // search for #include to fill in from this parameterfile
      ibuffer = Hlp::StringFind(buffer,INCLUDESIGN,"vs");
      if( ibuffer == Hlp::npos )
      {
        // no includes found
        flag = false;
      }
      else  // found #include and extract filename
      {
        // take string between include and end of line 
        std::string name = Hlp::StringGetQuotedText(buffer,ibuffer,INCLUDESIGN,ENDOFLINESIGN);
        // take away all non alphanumerical characters front and back
        Hlp::StringElimNonAlphaNumAnfEnd(name);

        // if no filename found
        if( name.size() == 0 )
        {
          Status = STATUS_NOT_OKAY;
          std::size_t zeile = Hlp::StringFindCount(buffer,0,ibuffer+1,ENDOFLINESIGN);
          std::stringstream t;
          t << "\nError readParFile: In Input-File: <" << fullfilename << "> " << INCLUDESIGN << "was found in line " << zeile <<  ", but no filename !!!";
          ErrText.append(t.str().c_str());
          Log.setLogPrintToScreen(true);
          Log.writeEndL(ErrText);
          return Status;
        }
        
        fullfilename = name;

        // eliminate line with include
        std::size_t  i1  = Hlp::StringFind(buffer,ibuffer,ENDOFLINESIGN,"vs");
        if( i1 != Hlp::npos )
        {
          std::size_t len = i1+strlen(ENDOFLINESIGN) - ibuffer;
          buffer.erase(ibuffer,len);
        }
        
        // fill in next while-loop with include-data
      }
    }
    
    // elimiate empty-lines
    Hlp::StringChange(buffer,EMPTYLINESIGN,ENDOFLINESIGN);
  }
  //====================================================================================
  // call from
  // readRarFileStringBuffe()
  // read file into stringbuffer
  // eliminate comment
  //====================================================================================
  status_type CPar::readParFileIntoString(std::string &fullfilename,std::string &buffer,std::size_t ibuffer)
  {
    // proof File
    //-----------
    if( !Hlp::SysExistFile(fullfilename) )
    {
      Status = STATUS_NOT_OKAY;
      std::string t  = "\nError readParFileIntoString: Input-File: ";
      t             += fullfilename;
      t             += "could not be found !!!" ;
      ErrText.append(t);
      Log.setLogPrintToScreen(true);
      Log.writeEndL(ErrText);
      return Status;
    }
    
    std::string line;
    std::ifstream ifs (fullfilename);

    // Proof if open
    //--------------
    if (!ifs.is_open() )
    {
      Status = STATUS_NOT_OKAY;
      std::string t  = "\nError readParFileIntoString: Input-File: ";
      t             += fullfilename;
      t             += "could not be opened !!!" ;
      ErrText.append(t);
      Log.setLogPrintToScreen(true);
      Log.writeEndL(ErrText);
      return Status;
    }
    else
    {
      std::string t  = "Log readParFileIntoString: open  Input-Par-File: ";
      t             += fullfilename;
      Log.writeEndL(t);
    }

    // read line by line
    while(!ifs.eof())
    {
      std::getline(ifs, line);

      // Comment
      // search comment sign in string outside of quted string and erase till end of line
      std::size_t icom = Hlp::StringFindQuot(line, COMMENTSIGN, "vs", STRINGQUOT, STRINGQUOT, "a", 0);

      if( icom != Hlp::npos )
      {
        line.erase(icom,line.size()-icom);
      }

      // reduce blanks at front and end
      Hlp::StringChange(line,TABULATOR,SPACE);
      Hlp::StringElimAnfEnd(line,SPACE);

      // if line > 0 add endofline sign and insert to buffer
      //if( line.size() )
      //{

        line += ENDOFLINESIGN;
        buffer.insert(ibuffer,line);
        ibuffer += line.size();
      //}
    }

    // close File
    ifs.close();
    std::string t  = "Log readParFileIntoString: close Input-Par-File: ";
    t             += fullfilename;
    Log.writeEndL(t);

    // Ende
    return Status;
  }
}