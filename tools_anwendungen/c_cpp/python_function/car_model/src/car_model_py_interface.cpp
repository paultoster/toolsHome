

#include "car_model_handling.h"
#include "car_model_py_interface.h"
#include "py_interface.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string>

const SDefCarModelInterface defstruct;   // Definition of const values

CCarModelHandling carModelCtrl;          // Definition of carmodel control

PyObject *car_model_init_dict(void);     // init function to genarate dictionary



//===============================================================================
// static PyObject *car_model_init()
//
// init function
//===============================================================================
static PyObject *car_model_init(PyObject *self, PyObject *args, PyObject *keywds)
{
  okay_t flag = true;

  // keyword list interface to python
  static char *kwlist[] = { defstruct.TXT_DT_SIM,"file_name", "param_string", NULL };

  // values for keywords
  double dt_sim        = defstruct.DEFAULT_DT_SIM;
  char   *file_name    = NULL;
  char   *param_string = NULL;

  // initiate dict for return
  PyObject *dict = car_model_init_dict();
  
  // read interface by keywords
  if (PyArg_ParseTupleAndKeywords(args, keywds, "|dss", kwlist
     , &dt_sim, file_name, param_string))
  {
    // Parameter file name for reading parameter 
    if (file_name != NULL)
    {
      if (!carModelCtrl.SetParamByFileName(file_name))
      {
        if (!set_dict_value(dict, defstruct.TXT_ERR_TXT, carModelCtrl.message.GetLastErrMess()))
        {
          return NULL;
        }
        if (!set_dict_value(dict, defstruct.TXT_OKAY_FLAG, false))
        {
          return NULL;
        }
        return dict;
      }
    }

    // Parameter parameter-string instead of file
    if (param_string != NULL)
    {
      if (!carModelCtrl.SetParamByString(param_string))
      {
        if (!set_dict_value(dict, defstruct.TXT_ERR_TXT, carModelCtrl.message.GetLastErrMess()))
        {
          return NULL;
        }
        if (!set_dict_value(dict, defstruct.TXT_OKAY_FLAG, false))
        {
          return NULL;
        }
        return dict;
      }
    }
    /* Parameter dt */
    carModelCtrl.SetParDtSim(dt_sim);
    set_dict_value(dict, defstruct.TXT_DT_SIM, carModelCtrl.GetParDtSim());
  }

  //assert(!PyErr_Occurred());
  if (!carModelCtrl.Init(dt_sim))
  {
    if (!set_dict_value(dict, defstruct.TXT_ERR_TXT, carModelCtrl.message.GetLastErrMess()))
    {
      return NULL;
    }
    if (!set_dict_value(dict, defstruct.TXT_OKAY_FLAG, false))
    {
      return NULL;
    }
    return dict;
  }
  return dict;
}
#if 0
static PyObject *
car_model_first(PyObject *pMod, PyObject *args)
{
  double y0;

  unsigned char okay = 1;

  if (!PyArg_ParseTuple(args, "d", &y0))
  {
    goto except;
  }

  okay = carModel.First(y0);

  // set output
  if (PyModule_AddObject(pMod, NAME_ERG, Py_BuildValue("f", carModel.output.y)))
  {
    goto except;
  }

  assert(!PyErr_Occurred());
  goto finally;
except:
  okay = 0;
finally:
//Py_DECREF(pItem);
  
return PyLong_FromLong(okay);
}
static PyObject *
car_model_loop(PyObject *pMod, PyObject *args)
{
  double x;

  unsigned char okay = 1;

  if (!PyArg_ParseTuple(args, "d", &x))
  {
    goto except;
  }

  okay = carModel.Loop(x);

  // set output
  if (PyModule_AddObject(pMod, NAME_ERG, Py_BuildValue("f", carModel.output.y)))
  {
    goto except;
  }
  assert(!PyErr_Occurred());
  goto finally;
except:
  okay = 0;
  finally:
  return PyLong_FromLong(okay);
}
#endif
PyMethodDef PyMethods_car_model[] =
{
  { "init", (PyCFunction)(void(*)(void))car_model_init, METH_VARARGS | METH_KEYWORDS,"make init handover parameter dt and parameter description or filename" },
//  { "first", (PyCFunction)car_model_first, METH_VARARGS,"make first calculation at t=0" },
//  { "loop", (PyCFunction)car_model_loop, METH_VARARGS,"make loop calculation with dt" },
  { NULL, NULL, 0, NULL } /* Sentinel */
};

static struct PyModuleDef PyModule_car_model = 
{
  PyModuleDef_HEAD_INIT,
  "car_model_py",
  "calculation of a car model",
  -1,
  PyMethods_car_model
}; 

/* Add a dict of {str : int, ...}.
* Returns 0 on success, 1 on failure.
*/

int add_parameter_map_to_module(PyObject *module) 
{
  int ret = 0;
  PyObject *pMap = NULL;
  pMap = PyDict_New();

  if (!pMap) {
    goto except;
  }

  /* Load map. */
  if (PyDict_SetItemString(pMap, "a", PyLong_FromLong(66))) {
    goto except;
  }
  if (PyDict_SetItemString(pMap, "b", PyLong_FromLong(123))) {
    goto except;
  }
  /* Add map to module. */
#define NAME_MAP "map"
  if (PyModule_AddObject(module, NAME_MAP, pMap)) {
    goto except;
  }
  ret = 0;
  goto finally;
except:
  Py_XDECREF(pMap);
  ret = 1;
  finally:
  return ret;
}

PyMODINIT_FUNC
PyInit_car_model_py(void)
{
  PyObject *m = NULL;
 
  m = PyModule_Create(&PyModule_car_model);
  
  if (m == NULL) 
  {
    goto except;
  }

  /* Adding module globals */
#define NAME_ERG "abc" 
  if (PyModule_AddObject(m, NAME_ERG, Py_BuildValue("f", 0.0))) 
  {
    goto except;
  }

  /* An invented convenience function for this dict. */
  if (add_parameter_map_to_module(m)) 
  {
    goto except;
  }

  goto finally;
except:
  Py_XDECREF(m);
  m = NULL;
finally:
  return m;
}

//=====================================================================
//=====================================================================
PyObject *car_model_init_dict(void)
{
  PyObject *dict = NULL;

  dict = PyDict_New();
  if (!dict) {
    goto error;
  }

  if (!set_dict_value(dict, defstruct.TXT_OKAY_FLAG, true)) goto error;
  if (!set_dict_value(dict, defstruct.TXT_ERR_TXT, "")) goto error;

  return dict;

error:
  Py_XDECREF(dict);
  return NULL;
}
