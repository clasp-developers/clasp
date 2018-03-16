/*
    File: structureObject.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister
 
CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.
 
See directory 'clasp/licenses' for full details.
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
//#define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/numbers.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/array.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/sysprop.h>
#include <clasp/core/instance.h>
#include <clasp/core/primitives.h>
#include <clasp/core/wrappers.h>

namespace core {

#define USE_INSTANCES_FOR_STRUCTURES

CL_LAMBDA(type &rest slot-values);
CL_DECLARE();
CL_DOCSTRING("makeStructure");
CL_DEFUN T_sp core__make_structure(T_sp type, List_sp slot_values) {
  if (type.nilp()) {
    SIMPLE_ERROR(BF("You cannot makeStructure of type nil"));
  }
#ifdef CLOS
  if (Class_sp ctype = type.asOrNull<Class_O>()) {
    ASSERTF(!type.nilp(), BF("Tried to make-structure with type = nil"));
    //	printf("%s:%d  core__make_structure of %s  slot_values: %s\n",
    //	       __FILE__, __LINE__, _rep_(type).c_str(), _rep_(slot_values).c_str());
    Instance_sp so = core__allocate_new_instance(ctype, cl__length(slot_values));
    int idx = 0;
    for (auto slot : slot_values) {
      so->instanceSet(idx, oCar(slot));
      ++idx;
    }
    return so;
  }
#endif // CLOS
  SIMPLE_ERROR(BF("You are trying to make a structure of type %s before CLOS is available - this will not work") % _rep_(type));
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("copyStructure");
CL_DEFUN T_sp cl__copy_structure(T_sp arg) {
  if (arg.nilp()) {
    SIMPLE_ERROR(BF("You cannot copyStructure nil"));
  }
#ifdef CLOS
  if (Instance_sp iarg = arg.asOrNull<Instance_O>()) {
    return iarg->copyInstance();
  }
#endif
  SIMPLE_ERROR(BF("You cannot copy-structure a %s") % _rep_(arg));
};

SYMBOL_EXPORT_SC_(CorePkg, makeStructure);
SYMBOL_EXPORT_SC_(ClPkg, copyStructure);
SYMBOL_EXPORT_SC_(CorePkg, structurep);
SYMBOL_EXPORT_SC_(ClPkg,structure_object);
};
