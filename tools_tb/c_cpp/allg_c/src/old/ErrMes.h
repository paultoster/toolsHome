#ifndef ERR_MES_H_INCLUDED
#define ERR_MES_H_INCLUDED

#include "SlfMes.h"

// eigenständige
// Fehlermessage
//===============
#define SLF_ERR_SET        ErrorMessage.set
#define SLF_ERR_SET_ENDL   ErrorMessage.setEndl
#define SLF_ERR_SET_FORMAT ErrorMessage.setFormat
#define SLF_ERR_IS_SET     ErrorMessage.exist()
#define SLF_ERR_EXIST      ErrorMessage.exist()
#define SLF_ERR_GET        ErrorMessage.get()

extern CSlfMes ErrrorMessage;

#endif