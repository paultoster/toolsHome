/******************************************************************************
 * @file  ReadCanAscii.cpp
 *
 * @author  Thomas Berthold
 * @date    22/2/2014
 *
 * @brief read a Can-Ascii file
 *
 * @subversion_tags (not part of doxygen)
 *   $LastChangedBy: berthold $
 *   $LastChangedRevision: 38987 $
 *   $LastChangedDate: 2014-02-06 15:49:32 +0100 (Do, 06 Feb 2014) $
 *   $URL: http://frd2ahjg/svn/tze/Departments/EnvironmentPerception/Components/ArbiDev2PathTask/src/Application/ArbiDev2PathMain.cpp $
******************************************************************************/

#include "ReadCanAscii.h"
#include "strhelp.h"

#define READ_CAN_ASCII_MOTOROLA            0
#define READ_CAN_ASCII_INTEL               1

bool GetMotorolaValue(unsigned char* RMsgBuf, int BitStart, int BitLen, int Sign, double* pValue);
bool GetIntelValue(unsigned char* RMsgBuf, int BitStart, int BitLen, int Sign, double* pValue);

bool ReadDbcFile(const char* dbc_filename, const char* sigformat, CStringListT      *pSigNameList, CDbcBufListT      *pDbcBufList)
{
  bool ret_state = true;

  // open the dbc file
  FILE* dbc_fp = NULL;
  if(strlen(dbc_filename) == 0) return(false);
  dbc_fp = fopen(dbc_filename, "rt");
  if(!dbc_fp) return(false);

  char line[512];
  unsigned long  msg_id   = 0;
  unsigned long  msg_dlc  = 0;
  std::string    last_sg = "";
  std::string    last_id = "";
  std::string    last_bo = "";
  bool flag = false;


  while(fgets(line, sizeof(line), dbc_fp))
  {
    StrVecT arg_vec;
    if(IdentLineStr(line, &arg_vec) <= 0) continue;

    // new frame ?
    if(arg_vec[0] == "BO_")
    {
      msg_id   = atoi(arg_vec[1].c_str());
      msg_dlc  = atoi(arg_vec[3].c_str());
      last_id  = arg_vec[1];
      last_bo  = arg_vec[2];
      if(last_bo.length() > 1)
      {
        last_bo  = arg_vec[2].substr(0,arg_vec[2].length()-1);
      }

      if(last_bo == "VECTOR__INDEPENDENT_SIG_MSG")
      {
        msg_id   = 0;
        msg_dlc  = 0;
        last_id  = "";
        last_bo  = "";
      }
      //else
      //{
      //  Msg2ID.AddMember(last_bo, msg_id);
      //}
    }

    if(msg_id)
    {
      bool mplex = false;
      if(arg_vec[0] == "SG_")
      {
        // name
        last_sg = arg_vec[1];
        std::string sig_name = sigformat;
        StrReplace(&sig_name, "%BO", (char*)last_bo.c_str());
        StrReplace(&sig_name, "%ID", (char*)last_id.c_str());
        StrReplace(&sig_name, "%SG", (char*)last_sg.c_str());

        if( pSigNameList->size() == 0 )
        {
          flag = true;
        }
        else if( (arg_vec.size() >= 3) && (arg_vec[2] == "M") ) // Multiplexersignal?
        {
          mplex = true;
        }
        else
        {
          CStringListT::iterator iter;
          flag = false;
          for ( iter = pSigNameList->begin(); iter != pSigNameList->end(); ++iter)
          {
            if( strcmp((*iter).c_str(),sig_name.c_str()) == 0 )
            {
              flag = true;
              break;
            }
          }
        }
        if( flag )
        {
          if( (strcmp(sig_name.c_str(),"VPU0_ErrMultiplexor") == 0)  )
          {
            flag = true;
          }
          // Multiplexsignal
          int multiplexindex = -1;
          if( (arg_vec.size() >= 3) && (arg_vec[2][0] == 'm') )
          {
            sscanf(arg_vec[2].c_str(), "m%d", &multiplexindex);
          }


          // props
          std::string sig_line = line;
          int pos = sig_line.find(":");
          if(pos <= 0) continue;
          sig_line = sig_line.substr(pos+1);
          StrVecT sig_vec;
          int prop_num = IdentLineStr((char*)sig_line.c_str(), &sig_vec);
          if(prop_num < 2) continue;
          std::string bit_str   = sig_vec[0];
          std::string scale_str = sig_vec[1];
          std::string unit_str  = sig_vec[3];

          // sign ?
          pos = bit_str.find("-");
          bool sign = pos > 0;

          // bitstart, bitlen, format ?
          int bitstart   = 0;
          int bitlen   = 0;
          int format = 0;
          sscanf(bit_str.c_str(), "%d|%d@%d", &bitstart, &bitlen, &format);

          // scale / offset ?
          double scale  = 1.0;
          double offset = 0.0;
          sscanf(scale_str.c_str(), "(%lf,%lf)", &scale, &offset);

          // add variable
          if(bitstart > 63) bitstart = 63;
          if(bitlen > 63)   bitlen   = 63;

          SDbcBuf dbcBuf;

          dbcBuf.MesRead        = false;
          dbcBuf.IsMultiplexer  = mplex;
          dbcBuf.MultiplexIndex = multiplexindex;
          dbcBuf.Name     = sig_name;
          dbcBuf.Unit     = unit_str;
          dbcBuf.id       = (size_t)msg_id;
          dbcBuf.BitStart = (unsigned char)bitstart;
          dbcBuf.BitLen   = (unsigned char)bitlen;
          dbcBuf.Sign     = sign;
          dbcBuf.Format   = (unsigned char)format;
          dbcBuf.Scale    = scale;
          dbcBuf.Offset   = offset;

          pDbcBufList->push_back(dbcBuf);
        }
      }
    }
  }

  fclose(dbc_fp);
  dbc_fp = NULL;

  return(ret_state);
}
double ReadDatFile(std::string asc_file_name,CVecIdCh *pVecIdCh,CRecBufListT *pRecBufList)
{
  const char* file_buf = NULL;
  FILE* fp = fopen(asc_file_name.c_str(), "r");
  if(!fp) return(0.0);

  /* Read dat-file */
  long line_no = 0;
  unsigned char chan;
  unsigned char receive;
  size_t id;
  double time;
  double t_start  = -1.0;
  double t_end    =  0.0;
  bool   base_hex = false;
  char    line_buf[1024];
  StrVecT LineArray;
  std::string val_str="";
  char flag = 0;
	size_t nident = pVecIdCh->size();
	struct SIdCh idCh;

  while(fgets(line_buf, sizeof(line_buf), fp))
  {
    if(GetLineTokens(line_buf, &LineArray))
    {
      size_t count = LineArray.size();
      if(count < 6)
      {
        if(count > 1)
        {
          if(LineArray[0] == "base")
          {
            if(LineArray[1] == "hex")
            {
              base_hex = true;
            }
          }
        }
        if(count > 3)
        {
          if(LineArray[2] == "timestamps")
          {
            if(LineArray[3] == "absolute")
            {
              // todo
            }
          }
        }
      }
      if(count > 6)
      {
        if(  ((LineArray[3] == "Rx") || (LineArray[3] == "Tx"))
          && ((LineArray[4] == "d")  || (LineArray[4] == "D"))
          )
        {
          // time
          std::string time_str = LineArray[0];
          time = atof(time_str.c_str());
          if(t_start < 0.0) t_start = time;

          // channel
          chan = atoi(LineArray[1].c_str());

          // receive
          if( LineArray[3] == "Rx" ) receive = 1;
          else                       receive = 0;

          /* Channelauswahl */
          //if( (channel == 0) ||  (chan == channel) )
          //{
          std::string id_str = LineArray[2];
          if(base_hex) id_str = "0x" + id_str;
          sscanf(id_str.c_str(), "%i", &id);


          flag = 0;
          for(size_t i=0;i< nident;i++)
          {
						idCh = (*pVecIdCh)[i];
            if( (idCh.id == id) && (idCh.channel == chan) )
            {
              flag = 1;
              break;
            }
          }

          /* Identauswahl */
          if( flag || (nident == 0) )
          {
            SRecBuf recBuf;
            recBuf.time = 0.0;
            recBuf.id   = 0;
            recBuf.len  = 0;
            memset(recBuf.data, 0, sizeof(recBuf.data));
            // time
            recBuf.time = time;
            if(recBuf.time >= t_start)
            {
              //recBuf.time -= t_start;
              t_end = recBuf.time;
            }

            // channel
            recBuf.channel = chan;

            // id number ?
            recBuf.id = id;

            recBuf.receive = receive;

            // len
            std::string len_str = LineArray[5];
            sscanf(len_str.c_str(), "%i", &recBuf.len);

            // data
            for(size_t i = 6; (i < count) && (i < 14); i++)
            {
              if(base_hex) val_str = "0x" + LineArray[i];
              else         val_str = LineArray[i];
              try {
                sscanf(val_str.c_str(), "%i", &recBuf.data[i-6]);
              } catch(...) {
                recBuf.data[i-6] = 0;
              }
              //sscanf(val_str.c_str(), "%i", &recBuf.data[i-6]);
            }

            if((recBuf.time >= 0.0) && (recBuf.len > 0))
            {
              pRecBufList->push_back(recBuf);
            }
          }
          //}
        }
      }
    }
  }
  fclose(fp);

  if(t_end < 0.0) t_end = 0.0;
  return(t_end);
}
double ReadDatFile(std::string asc_file_name,unsigned char channel,size_t *pident,size_t nident,CRecBufListT *pRecBufList)
{
  const char* file_buf = NULL;
  FILE* fp = fopen(asc_file_name.c_str(), "r");
  if(!fp) return(0.0);

  /* Read dat-file */
  long line_no = 0;
  unsigned char chan;
  unsigned char receive;
  size_t id;
  double time;
  double t_start  = -1.0;
  double t_end    =  0.0;
  bool   base_hex = false;
  char    line_buf[1024];
  StrVecT LineArray;
  std::string val_str="";
  char flag = 0;
  bool read_all_ident = false;

  if( (pident == NULL) || (nident == 0) )  read_all_ident = true;

  while(fgets(line_buf, sizeof(line_buf), fp))
  {
    if(GetLineTokens(line_buf, &LineArray))
    {
      size_t count = LineArray.size();
      if(count < 6)
      {
        if(count > 1)
        {
          if(LineArray[0] == "base")
          {
            if(LineArray[1] == "hex")
            {
              base_hex = true;
            }
          }
        }
        if(count > 3)
        {
          if(LineArray[2] == "timestamps")
          {
            if(LineArray[3] == "absolute")
            {
              // todo
            }
          }
        }
      }
      if(count > 6)
      {
        if(  ((LineArray[3] == "Rx") || (LineArray[3] == "Tx"))
          && ((LineArray[4] == "d")  || (LineArray[4] == "D"))
          )
        {
          // time
          std::string time_str = LineArray[0];
          time = atof(time_str.c_str());
          if(t_start < 0.0) t_start = time;

          // channel
          chan = atoi(LineArray[1].c_str());

          // receive
          if( LineArray[3] == "Rx" ) receive = 1;
          else                       receive = 0;

          /* Channelauswahl */
          if( (channel == 0) ||  (chan == channel) )
          {
            std::string id_str = LineArray[2];
            if(base_hex) id_str = "0x" + id_str;
            sscanf(id_str.c_str(), "%i", &id);


            if( read_all_ident )
            {
              flag = 1;
            }
            else
            {
              flag = 0;
              for(size_t i=0;i< nident;i++)
              {
                if( pident[i] == id )
                {
                  flag = 1;
                  break;
                }
              }
            }
            /* Identauswahl */
            if( flag  )
            {
              SRecBuf recBuf;
              recBuf.time = 0.0;
              recBuf.id   = 0;
              recBuf.len  = 0;
              memset(recBuf.data, 0, sizeof(recBuf.data));
              // time
              recBuf.time = time;
              if(recBuf.time >= t_start)
              {
                //recBuf.time -= t_start;
                t_end = recBuf.time;
              }

              // channel
              recBuf.channel = chan;

              // id number ?
              recBuf.id = id;

              recBuf.receive = receive;

              // len
              std::string len_str = LineArray[5];
              sscanf(len_str.c_str(), "%i", &recBuf.len);

              // data
              for(size_t i = 6; (i < count) && (i < 14); i++)
              {
                if(base_hex) val_str = "0x" + LineArray[i];
                else         val_str = LineArray[i];
                try {
                  sscanf(val_str.c_str(), "%i", &recBuf.data[i-6]);
                } catch(...) {
                  recBuf.data[i-6] = 0;
                }
                //sscanf(val_str.c_str(), "%i", &recBuf.data[i-6]);
              }

              if((recBuf.time >= 0.0) && (recBuf.len > 0))
              {
                pRecBufList->push_back(recBuf);
              }
            }
          }
        }
      }
    }
  }
  fclose(fp);

  if(t_end < 0.0) t_end = 0.0;
  return(t_end);
}
bool SetDbcValues(CRecBufListT *pRecBufList,CDbcBufListT      *pDbcBufList)
{
  bool value_found = false;
  CDbcBufListT::iterator iterD;
  CRecBufListT::iterator iterR;
  double                 value;
  bool                   flag;

  for ( iterR = pRecBufList->begin(); iterR != pRecBufList->end(); ++iterR)
  {
    for ( iterD = pDbcBufList->begin(); iterD != pDbcBufList->end(); ++iterD)
    {
      /* Identifier vergleichen */
      if( (*iterR).id == (*iterD).id )
      {
        if( !((*iterD).IsMultiplexer) && ((*iterD).MultiplexIndex < 0) ) //normales Signal
        {
          // decode the buffer
          switch((*iterD).Format)
          {
          case READ_CAN_ASCII_INTEL:
            flag = GetIntelValue((*iterR).data, (*iterD).BitStart, (*iterD).BitLen, (*iterD).Sign, &value);
            break;
          case READ_CAN_ASCII_MOTOROLA:
            flag = GetMotorolaValue((*iterR).data, (*iterD).BitStart, (*iterD).BitLen, (*iterD).Sign, &value);
            break;
          }

          // scale & offset
          if( flag )
          {
            value_found = true;
            value       = value * (*iterD).Scale + (*iterD).Offset;

            (*iterD).TimeList.push_back((*iterR).time);
            (*iterD).VecList.push_back(value);
            (*iterD).MesRead = true;
          }
        }
        else if( ((*iterD).MultiplexIndex >= 0) )   // Multiplexsignal
        {
          // Multiplexer suchen
          CDbcBufListT::iterator iterDD;
          for ( iterDD = pDbcBufList->begin(); iterDD != pDbcBufList->end(); ++iterDD)
          {
            if( ((*iterD).id == (*iterDD).id) && (*iterDD).IsMultiplexer ) // Multiplexersignal suchen
            {
              // decode the buffer
              switch((*iterD).Format)
              {
              case READ_CAN_ASCII_INTEL:
                flag = GetIntelValue((*iterR).data, (*iterDD).BitStart, (*iterDD).BitLen, (*iterDD).Sign, &value);
                break;
              case READ_CAN_ASCII_MOTOROLA:
                flag = GetMotorolaValue((*iterR).data, (*iterDD).BitStart, (*iterDD).BitLen, (*iterDD).Sign, &value);
                break;
              }
              if( flag )
              {
                int index = (int)(value+0.5);
                if( (*iterD).MultiplexIndex == index )
                {
                  // decode the buffer
                  switch((*iterD).Format)
                  {
                  case READ_CAN_ASCII_INTEL:
                    flag = GetIntelValue((*iterR).data, (*iterD).BitStart, (*iterD).BitLen, (*iterD).Sign, &value);
                    break;
                  case READ_CAN_ASCII_MOTOROLA:
                    flag = GetMotorolaValue((*iterR).data, (*iterD).BitStart, (*iterD).BitLen, (*iterD).Sign, &value);
                    break;
                  }

                  // scale & offset
                  if( flag )
                  {
                    value_found = true;
                    value       = value * (*iterD).Scale + (*iterD).Offset;

                    (*iterD).TimeList.push_back((*iterR).time);
                    (*iterD).VecList.push_back(value);
                    (*iterD).MesRead = true;
                  }
                }
              }
              break;
            } // if( ((*iterD).id == (*iterDD).id) && (*iterDD).IsMultiplexer ) // Multiplexersignal suchen
          } // for ( iterDD = pDbcBufList->begin(); iterDD != pDbcBufList->end(); ++iterDD)
        } // Ende Multiplexer

      } // if( (*iterR).id == (*iterD).id )
    } // for ( iterD = pDbcBufList->begin(); iterD != pDbcBufList->end(); ++iterD)
  } // for ( iterR = pRecBufList->begin(); iterR != pRecBufList->end(); ++iterR)
  return(value_found);
}
bool GetMotorolaValue(unsigned char* RMsgBuf, int BitStart, int BitLen, int Sign, double* pValue)
{
  if(!pValue) return(false);

  // swap bytes
  char TmpBuf[8];
  for(int i = 0; i < 8; i++)
  {
    TmpBuf[i] = RMsgBuf[7-i];
  }

  // make a 64 bit value
  _int64 RMsgInt = 0;
  memcpy(&RMsgInt, TmpBuf, 8);

  // new startbit
  int bitstart = ((63-BitStart)/8)*8+BitStart%8-BitLen+1;

  // move it left
  RMsgInt = RMsgInt << (64-(bitstart+BitLen));

  // move to the right end and fill with sign bits
  RMsgInt = RMsgInt >> (64-BitLen);

  // check sign
  bool sign = false;
  if(Sign) sign    = (RMsgInt & 0x8000000000000000) != 0;
  else     RMsgInt = RMsgInt & ~(0xFFFFFFFFFFFFFFFF << BitLen);

  // set value
  if(sign)
  {
    *pValue = (double)((signed long)RMsgInt);
  }
  else
  {
    *pValue = (double)((unsigned long)RMsgInt);
  }

  return(true);
}

/////////////////////////////////////////////////////////////////////////////
// GetIntelValue
bool GetIntelValue(unsigned char* RMsgBuf, int BitStart, int BitLen, int Sign, double* pValue)
{
  if(!pValue) return(false);

  // make a 64 bit value
  _int64 RMsgInt = 0;
  memcpy(&RMsgInt, RMsgBuf, 8);

  // move it left
  RMsgInt = RMsgInt << (64-(BitStart+BitLen));

  // move to the right end and fill with sign bits
  RMsgInt = RMsgInt >> (64-BitLen);

  // check sign
  bool sign = false;
  if(Sign) sign    = (RMsgInt & 0x8000000000000000) != 0;
  else     RMsgInt = RMsgInt & ~(0xFFFFFFFFFFFFFFFF << BitLen);

  // set value
  if(sign)
  {
    *pValue = (double)((signed long)RMsgInt);
  }
  else
  {
    *pValue = (double)((unsigned long)RMsgInt);
  }

  return(true);
}
