
#include "SlfDef.h"
#include "run_mod.h"
#include "py_interface_definition.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string>

CRunMod RunMod;

// init function to generate dictionary
//=====================================================================
//=====================================================================
PyObject *py_def_output_dict(void)
{
  PyObject *dict = NULL;

  dict = PyDict_New();
  if (!dict) {
    goto error;
  }

  if (!set_dict_value(dict, NAME_OKAY_FLAG, true)) goto error;
  if (!set_dict_value(dict, NAME_ERR_TXT, "")) goto error;

  return dict;

error:
  Py_XDECREF(dict);
  return NULL;
}


//===============================================================================
// static PyObject *py_def_init()
//
// init function first method
//===============================================================================
static PyObject *py_def_init(PyObject *self, PyObject *args, PyObject *keywds)
{
  okay_t flag = true;

  // keyword list interface to python
  static char *kwlist[] = { "param_string", NULL };
  
  PyObject *pyDictOut;
  PyObject* pyObj;
  Py_ssize_t len;

  // values for keywords
  char   *param_string = NULL;

  // initiate dict for return
  pyDictOut = py_def_output_dict();
  
  // read interface by keywords
  if (PyArg_ParseTupleAndKeywords(args, keywds, "O&", kwlist  
     , PyUnicode_FSConverter, &pyObj) )
  {
    // build parameter string
    PyBytes_AsStringAndSize(pyObj, &param_string, &len);
    
    // failure if no parameter is set
    if (param_string == NULL)
    {
      if (!set_dict_value(pyDictOut, NAME_OKAY_FLAG, false))
      {
        return NULL;
      }
      if (!set_dict_value(pyDictOut, NAME_ERR_TXT, "no parameter string set"))
      {
        return NULL;
      }
    }
    else
    // Parameter parameter-string instead of file
    {
      if (RunMod.Init(param_string) != OKAY)
      {
        if (!set_dict_value(pyDictOut, NAME_OKAY_FLAG, false))
        {
          return NULL;
        }
      }
      if (!set_dict_value(pyDictOut, NAME_ERR_TXT, RunMod.GetLogText()))
      {
        return NULL;
      }
      RunMod.resetLogText();
    }
  }

  //assert(!PyErr_Occurred());
  if ( true /*!carModelCtrl.Init(dt_sim)*/)
  {
    if (!set_dict_value(pyDictOut, NAME_ERR_TXT, param_string))
    {
      return NULL;
    }
    if (!set_dict_value(pyDictOut, NAME_OKAY_FLAG, false))
    {
      return NULL;
    }
  }
  Py_DECREF(pyObj);
  return pyDictOut;
}
//===============================================================================
// py_def_calc()
//
// second method defined
//===============================================================================
static PyObject *py_def_calc(PyObject *self, PyObject *args, PyObject *keywds)
{
  static char *kwlist[] = { "param_string", NULL };

  PyObject *pyDictOut;
  PyObject* pyObj;
  Py_ssize_t len;

  // values for keywords
  char   *param_string = NULL;

  // initiate dict for return
  pyDictOut = py_def_output_dict();

  // read interface by keywords
  if (PyArg_ParseTupleAndKeywords(args, keywds, "O&", kwlist
    , PyUnicode_FSConverter, &pyObj))
  {
    PyBytes_AsStringAndSize(pyObj, &param_string, &len);

    if (!set_dict_value(pyDictOut, NAME_ERR_TXT, param_string))
    {
      return NULL;
    }
    if (!set_dict_value(pyDictOut, NAME_OKAY_FLAG, false))
    {
      return NULL;
    }
  }
  Py_DECREF(pyObj);
  return pyDictOut;
}

//===============================================================================
// PyMethods_definition()
//
// eingebundene Funtionen
//===============================================================================
PyMethodDef PyMethods_definition[] =
{
  { "init", (PyCFunction)(void(*)(void))py_def_init, METH_VARARGS | METH_KEYWORDS,"make init handover parameter dt and a string with parameters" },
  { "calc", (PyCFunction)(void(*)(void))py_def_calc, METH_VARARGS,"make a caclulation" },
//  { "loop", (PyCFunction)py_def_loop, METH_VARARGS,"ma  ke loop calculation with dt" },
  { NULL, NULL, 0, NULL } /* Sentinel */
};
//===============================================================================
// PyModule_definition()
//
// interface Modul Definition
//===============================================================================
static struct PyModuleDef PyModule_definition = 
{
  PyModuleDef_HEAD_INIT,
  "tire_model_static",
  "calculation of a tire forces in a static way",
  -1,
  PyMethods_definition
}; 

/* Add a dict of {str : int, ...}.
* Returns 0 on success, 1 on failure.
*/

//int add_dict_to_module(PyObject *module) 
//{
//  int ret = 0;
//  PyObject *pMap = NULL;
//  pMap = PyDict_New();
//
//  if (!pMap) {
//    goto except;
//  }
//
//  /* Load map. */
//  if (PyDict_SetItemString(pMap,"logtext", Py_BuildValue("s",""))) {
//    goto except;
//  }
//  if (PyDict_SetItemString(pMap, "okayflag", PyLong_FromLong(1))) {
//    goto except;
//  }
//  /* Add map to module. */
//  if (PyModule_AddObject(module, "out", pMap)) 
//  {
//    goto except;
//  }
//  ret = 0;
//  goto finally;
//except:
//  Py_XDECREF(pMap);
//  ret = 1;
//finally:
//  return ret;
//}

//===============================================================================
// Modul Init()
//
// Init of interface Modul 
//===============================================================================
PyMODINIT_FUNC
PyInit_tire_model_static_calc(void)
{
  PyObject *m = NULL;
 
  m = PyModule_Create(&PyModule_definition);
  
  if (m == NULL) 
  {
    goto except;
  }

//  /* Adding module globals */
//#define NAME_ERG "abc" 
//  if (PyModule_AddObject(m, NAME_ERG, Py_BuildValue("f", 0.0))) 
//  {
//    goto except;
//  }
//
//  /* add a dict "out" */
//  if (add_dict_to_module(m)) 
//  {
//    goto except;
//  }

  goto finally;
except:
  Py_XDECREF(m);
  m = NULL;
finally:
  return m;
}

