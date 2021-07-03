#include "py_interface.h"

//====================================================================
// set dictionary for bool
//
//====================================================================
PyObject *set_dict_value(PyObject *dict, const char *keyname, bool val)
{
  PyObject *key = NULL;
  PyObject *value = NULL;

  key = PyUnicode_FromString(keyname);
  if (!key)
  {
    Py_XDECREF(key);
    return NULL;
  }
  value = PyBool_FromLong(val);
  if (!value)
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }
  if (-1 == PyDict_SetItem(dict, key, value))
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }

  Py_XDECREF(key);
  Py_XDECREF(value);
  return dict;
}
//====================================================================
// set dictionary for long
//
//====================================================================
PyObject *set_dict_value(PyObject *dict, const char *keyname, long val)
{
  PyObject *key = NULL;
  PyObject *value = NULL;

  key = PyUnicode_FromString(keyname);
  if (!key)
  {
    Py_XDECREF(key);
    return NULL;
  }
  value = PyLong_FromLong(val);
  if (!value)
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }
  if (-1 == PyDict_SetItem(dict, key, value))
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }

  Py_XDECREF(key);
  Py_XDECREF(value);
  return dict;
}
//====================================================================
// set dictionary for double
//
//====================================================================
PyObject *set_dict_value(PyObject *dict, const char *keyname, double val)
{
  PyObject *key = NULL;
  PyObject *value = NULL;

  key = PyUnicode_FromString(keyname);
  if (!key)
  {
    Py_XDECREF(key);
    return NULL;
  }
  value = PyFloat_FromDouble(val);
  if (!value)
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }
  if (-1 == PyDict_SetItem(dict, key, value))
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }

  Py_XDECREF(key);
  Py_XDECREF(value);
  return dict;
}
//====================================================================
// set dictionary for string
//
//====================================================================
PyObject *set_dict_value(PyObject *dict, const char *keyname, const char *val)
{
  PyObject *key = NULL;
  PyObject *value = NULL;

  key = PyUnicode_FromString(keyname);
  if (!key)
  {
    Py_XDECREF(key);
    return NULL;
  }
  value = PyUnicode_FromString(val);
  if (!value)
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }
  if (-1 == PyDict_SetItem(dict, key, value))
  {
    Py_XDECREF(key);
    Py_XDECREF(value);
    return NULL;
  }

  Py_XDECREF(key);
  Py_XDECREF(value);
  return dict;
}

