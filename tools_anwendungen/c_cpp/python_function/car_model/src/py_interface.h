#ifndef PY_INTERFACE_H_FILE
#define PY_INTERFACE_H_FILE

#ifndef PY_SSIZE_T_CLEAN
 #define PY_SSIZE_T_CLEAN
#endif

#ifdef _DEBUG
#define PY_INTERFACE_H_FILE_DEBUG_HOLD
#undef _DEBUG
#endif

#include "Python.h"

#ifdef PY_INTERFACE_H_FILE_DEBUG_HOLD
#define  _DEBUG
#endif

PyObject *set_dict_value(PyObject *dict, const char *keyname, bool val);
PyObject *set_dict_value(PyObject *dict, const char *keyname, long val);
PyObject *set_dict_value(PyObject *dict, const char *keyname, double val);
PyObject *set_dict_value(PyObject *dict, const char *keyname, const char * text);

#endif