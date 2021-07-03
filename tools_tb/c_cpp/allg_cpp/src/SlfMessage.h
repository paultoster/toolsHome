/******************************************************************************
 * @file  SlfMessage.h
 *
 * @author  Thomas Berthold 
 * @date    10.1.2019
 *
 * @brief   Defenition for Message
 *
******************************************************************************/

#ifndef SLF_MESSAGE_H_INCLUDED
#define SLF_MESSAGE_H_INCLUDED

#include <list>
#include <vector>
#include "SlfStr.h"
#include <stdint.h>
#include "SlfBase.h"
//---------------------------------------------------------------------------------------------------------------------------
// makro
//---------------------------------------------------------------------------------------------------------------------------


namespace slf
{
  //===========================================================================================================================
  // enums
  //===========================================================================================================================


  //===========================================================================================================================
  // class
  //===========================================================================================================================
  //---------------------------------------------------------------------------------------------------------------------------
  // message class
  // CMessage   mMessage("modul1");
  //
  // in modul
  // clear error and log:              mMessage.Clear();
  // clear Error:                      mMessage.ClearErr();
  // clear log:                        mMessage.ClearLog();
  // set   error:                      mMessage.SetErr(UNKOWN_ERR,"comment");
  // set  unregistered  error:         mMessage.SetUnRegErr(errnum,"comment"); uint16_t errnum
  // set   log:                        mMessage.SetLog("comment");
  //
  //
  // e.g. build const reference in each class with reference to private definition
  //
  // const CMessage &Message; 
  //
  // hand over errvec to collected message with information of new errors
  //
  // CMessage   mess("collect")
  //
  // collect all new messages a) error: collect new errors, set old errors newFlag = false and returns erased errors
  //                          b) log:   old logs erased, new logs added
  //
  // mess.Collect(Message,oldErr);   // set Message and give back oldErr, err which are erased 
  // mess.Collect(Message);          // set Message
  //
  // bool IsError(void)              // true if error is set
  // bool IsOkay(void)               // true if no error is set
    //---------------------------------------------------------------------------------------------------------------------------
  class CMessage
  {
  public:
    struct SErrStruct
    {
      EErrNo             errNo;
      uint16_t           errUnReg; // not registered error number
      std::size_t        id;
      std::string        errText;
      bool               newFlag;
      SErrStruct() :errNo(NO_ERR), id(0), errText(),newFlag(false) {};
    };
    struct SLogStruct
    {
      std::size_t        id;
      std::string        logText;
      SLogStruct() : logText() {};
    };
    const std::vector<SErrStruct> &ErrVec;
    const std::vector<SLogStruct> &LogVec;
  private:
    std::vector<SErrStruct> errVec;
    std::vector<SLogStruct> logVec;
    std::string             modulName;
    std::size_t             modulId;
  public:
    CMessage(const char *name) :errVec(),ErrVec(errVec),logVec(),LogVec(logVec), modulName(name){ modulId = CalcId(); };

    //-------------------------------------------------------------------------------------
    // Clear(), ClearErr(), ClearLog()
    // clear errList and logList 
    //-------------------------------------------------------------------------------------
    void Clear(void)
    {
      errVec.clear();
      logVec.clear();
    }
    void ClearErr(void)
    {
      errVec.clear();
    }
    void ClearLog(void)
    {
      logVec.clear();
    }
    //-------------------------------------------------------------------------------------
    // SetErr(UNKNOWN_ERR,"comment"), SetLog("comment")
    // set Error set LogComment
    //-------------------------------------------------------------------------------------
    void SetErr(std::vector<SErrStruct> &errInVec)
    {
      for (std::size_t i = 0; i < errInVec.size(); ++i)
      {
        SetErr(errInVec[i].errNo, errInVec[i].errText.c_str());
      }
    }
    void SetErr(EErrNo num, const char *text)
    {
      bool flag = true;
      for (std::size_t index = 0; index < errVec.size(); ++index)
      {
        if (num == errVec[index].errNo)
        {
          flag = false;
          break;
        }
      }
      if (flag)
      {
        SErrStruct err;
        err.errNo = num;
        err.errUnReg = 0;
        err.id = modulId;
        err.errText = modulName;
        err.errText.push_back(':');
        err.errText.append(text);
        err.newFlag = true;

        errVec.push_back(err);
      }
    }
    void SetUnRegErr(uint16_t urerr, const char *text)
    {
      bool flag = true;
      for (std::size_t index = 0; index < errVec.size(); ++index)
      {
        if( (UNREG_ERROR == errVec[index].errNo) && (errVec[index].errUnReg == urerr) )
        {
          flag = false;
          break;
        }
      }
      if (flag)
      {
        SErrStruct err;
        err.errNo = UNREG_ERROR;
        err.errUnReg = urerr;
        err.id = modulId;
        err.errText = modulName;
        err.errText.push_back(':');
        err.errText.append(text);
        err.newFlag = true;

        errVec.push_back(err);
      }
    }
    void SetLog(CStr &text)
    {
      SLogStruct log;
      log.id = modulId;
      log.logText.append(text);

      logVec.push_back(log);
    }
    void SetLog(const char *text)
    {
      SLogStruct log;
      log.id = modulId;
      log.logText.append(text);

      logVec.push_back(log);
    }
    void SetLogPlus(const char *text)
    {
      SLogStruct log;
      log.id = modulId;
      log.logText = modulName;
      log.logText.push_back(':');
      log.logText.append(text);

      logVec.push_back(log);
    }
    //----------------------------------------------------------------------------------------------
    // collect all new messages a) error: collect new errors from messagelist to internal, set old errors newFlag = false and returns erased errors
    //                          b) log:   old logs erased, new logs added
    //
    // std::vector<SErrStruct>  oldErrList
    // mess.CollectNew(Message,oldErrList);
    //----------------------------------------------------------------------------------------------
    void  Collect(const CMessage &messagelist, std::vector<SErrStruct> &erroldlist)
    {
      // find old no more existing errors, 
      // collect them in erroldlist and erase them
      erroldlist.clear();
      std::size_t id = messagelist.GetId();
      std::vector<std::size_t> index_vec = GetErrIndicesWithId(id);

      for (std::size_t index = 0; index < index_vec.size(); ++index)
      {
        if (!messagelist.IsErrInList(errVec[index]))
        {
          erroldlist.push_back(errVec[index]);
          EraseErrorStruct(index);
        }
      }
      // add new errors and sign old errors
      for (std::size_t index = 0; index < messagelist.ErrVec.size(); ++index)
      {
        if (IsErrInList(messagelist.ErrVec[index])) // old error
        {
          errVec[index].newFlag = false;
        }
        else // new error
        {
          AddErrorStruct(messagelist.ErrVec[index]);
        }
      }

      // erase old Logs with id
      EraseLogs(id);
      // add new logs
      for (std::size_t index = 0; index < messagelist.LogVec.size(); ++index)
      {
        AddLog(messagelist.LogVec[index]);
      }
    }
    void  Collect(const CMessage *pmessagelist)
    {
      // find old no more existing errors, 
      // collect them in erroldlist and erase them
      std::size_t id = pmessagelist->GetId();
      std::vector<std::size_t> index_vec = GetErrIndicesWithId(id);

      for (std::size_t index = 0; index < index_vec.size(); ++index)
      {
        if (!pmessagelist->IsErrInList(errVec[index]))
        {
          EraseErrorStruct(index);
        }
      }
      // add new errors and sign old errors
      for (std::size_t index = 0; index < pmessagelist->ErrVec.size(); ++index)
      {
        if (IsErrInList(pmessagelist->ErrVec[index])) // old error
        {
          errVec[index].newFlag = false;
        }
        else // new error
        {
          AddErrorStruct(pmessagelist->ErrVec[index]);
        }
      }

      // erase old Logs with id
      EraseLogs(id);
      // add new logs
      for (std::size_t index = 0; index < pmessagelist->LogVec.size(); ++index)
      {
        AddLog(pmessagelist->LogVec[index]);
      }
    }
    void  Collect(const CMessage &messagelist)
    {
      Collect(&messagelist);
    }
    //std::vector<SErrStruct> SetErrorIfNew(const CMessage &errlist)
    //{
    //  std::vector<SErrStruct> newerrs;
    //  for (std::size_t index = 0; index < errlist.Size(); ++index)
    //  {
    //    SErrStruct err;
    //    errlist.GetErrorStruct(index, err);

    //    if (SetErrorStructIfNew(err))
    //    {
    //      newerrs.push_back(err);
    //    }
    //  }
    //  return newerrs;
    //}
    //bool SetErrorStructIfNew(SErrStruct &err)
    //{
    //  bool flag = true;
    //  for (std::size_t index = 0; index < errVec.size(); ++index)
    //  {
    //    if (err.id == errVec[index].id)
    //    {
    //      flag = false;
    //      break;
    //    }
    //  }
    //  if (flag)
    //  {
    //    errVec.push_back(err);
    //  }
    //  return flag;
    //}
    //bool GetErrorStruct(std::size_t index, SErrStruct &err) const
    //{
    //  if (index < errVec.size())
    //  {
    //    err = errVec[index];
    //    return true;
    //  }
    //  else
    //  {
    //    return false;
    //  }
    //}
    const char *GetLastErrMess(void) const
    {
      if (errVec.size())
      {
        return errVec.back().errText.c_str();
      }
      else
      {
        return "";
      }
    }
    std::size_t GetId(void) const
    {
      return modulId;
    }
    //std::size_t Size(void) const
    //{
    //  return errVec.size();
    //}
    bool IsError(void) const
    {
      return (errVec.size() != 0);
    }
    bool IsOkay(void) const
    {
      return (errVec.size() == 0);
    }
    //bool IsErrorStructSet(SErrStruct &err)
    //{
    //  for (std::size_t index = 0; index < errVec.size(); ++index)
    //  {
    //    if (err.id == errVec[index].id)
    //    {
    //      return true;
    //    }
    //  }
    //  return false;
    //}
    bool IsErrInList(const SErrStruct &err) const 
    {
      for (std::size_t index = 0; index < errVec.size(); ++index)
      {
        if (errVec[index].errNo == err.errNo)
        {
          return true;
        }
      }
      return false;
    }
  private:
    std::size_t CalcId(void)
    {
      std::size_t id = 0;
      for (std::size_t i = 0; i < modulName.size(); ++i) id += (modulName[i] % 10)*(i + 1);
      return id;
    }
    std::vector<std::size_t> GetErrIndicesWithId(std::size_t id)
    {
      std::vector<std::size_t> index_vec;
      for (std::size_t index = 0; index < errVec.size(); ++index)
      {
        if (errVec[index].id == id)
        {
          index_vec.push_back(index);
        }
      }
      return index_vec;
    }
    std::vector<std::size_t> GetLogIndicesWithId(std::size_t id)
    {
      std::vector<std::size_t> index_vec;
      for (std::size_t index = 0; index < errVec.size(); ++index)
      {
        if (logVec[index].id == id)
        {
          index_vec.push_back(index);
        }
      }
      return index_vec;
    }
    void AddErrorStruct(const SErrStruct &err)
    {
      if (errVec.size() < 100)
      {
        errVec.push_back(err);
        errVec.back().newFlag = true;
      }
    }
    void EraseErrorStruct(std::size_t index)
    {
      if (index < errVec.size())
      {
        for (std::size_t i = index + 1; i < errVec.size(); ++i)
        {
          errVec[i - 1] = errVec[i];
        }
        errVec.resize(errVec.size() - 1);
      }
    }
    void EraseLogs(const std::size_t id)
    {
      for (std::size_t index = 0; index < logVec.size(); ++index)
      {
        if (logVec[index].id == id)
        {
          for (std::size_t i = index + 1; i < logVec.size(); ++i)
          {
            logVec[i - 1] = logVec[i];
          }
          logVec.resize(logVec.size() - 1);
        }

      }
    }
    void AddLog(const SLogStruct &log)
    {
      if (logVec.size() < 100)
      {
        logVec.push_back(log);
      }

    }
  }; // CMessage

} // namespace slf
#endif //SLF_MESSAGE_H_INCLUDED
