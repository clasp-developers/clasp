/*
    File: creator.cc
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
#include <clasp/core/array.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/package.h>
//#include "debugger.h"
#include <clasp/core/iterator.h>
#include <clasp/core/designators.h>
#include <clasp/core/primitives.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/activationFrame.h>
#include <clasp/clbind/class_rep.h>
#include <clasp/core/lambdaListHandler.h>
//#i n c l u d e "environmentDependent.h"
#include <clasp/core/environment.h>
#include <clasp/core/evaluator.h>
// to avoid Generic to_object include headers here
#include <clasp/core/wrappers.h>



namespace core {
T_sp InstanceCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<Instance_O>();
#ifdef METER_ALLOCATIONS
  if (this->_class) {
    this->_class->_allocation_counter += 1;
    this->_class->_allocation_total_size += size;
  }
#endif
  Instance_sp instance = gctools::GC<Instance_O>::allocate_instance(gctools::Header_s::Value::make_instance(), size);
  return instance;
    };
};

namespace core {
T_sp FuncallableInstanceCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<FuncallableInstance_O>();
#ifdef METER_ALLOCATIONS
  if (this->_class) {
    this->_class->_allocation_counter += 1;
    this->_class->_allocation_total_size += size;
  }
#endif
  FuncallableInstance_sp instance = gctools::GC<FuncallableInstance_O>::allocate_instance(gctools::Header_s::Value::make_funcallable_instance(), size);
  return instance;
    };
};


namespace core {
T_sp ClassCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<Class_O>();
#ifdef METER_ALLOCATIONS
  if (this->_class) {
    this->_class->_allocation_counter += 1;
    this->_class->_allocation_total_size += size;
  }
#endif
  Class_sp class_ = gctools::GC<Class_O>::allocate_class(gctools::Header_s::Value::make_instance(), size);
  return class_;
};
};

namespace core {
T_sp StandardClassCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<Class_O>();
#ifdef METER_ALLOCATIONS
  Class_sp c = lisp_standard_class();
  c->_allocation_counter += 1;
  c->_allocation_total_size += size;
#endif
  GC_ALLOCATE_VARIADIC(Class_O,class_,lisp_standard_class()  /*,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS*/);
//  printf("%s:%d:%s   created class\n", __FILE__, __LINE__, __FUNCTION__ );
  return class_;
};
};

namespace core {
T_sp StructureClassCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<Class_O>();
#ifdef METER_ALLOCATIONS
  Class_sp c = lisp_standard_class();
  c->_allocation_counter += 1;
  c->_allocation_total_size += size;
#endif
  GC_ALLOCATE_VARIADIC(Class_O,class_,lisp_standard_class()/*,REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS*/);
  return class_;
};

};




namespace core {
T_sp DerivableCxxClassCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<Class_O>();
#ifdef METER_ALLOCATIONS
  Class_sp c = lisp_standard_class();
  c->_allocation_counter += 1;
  c->_allocation_total_size += size;
#endif
  GC_ALLOCATE_VARIADIC(Class_O,class_,lisp_standard_class()/*,REF_CLASS_NUMBER_OF_SLOTS_IN_STRUCTURE_CLASS*/);
  return class_;
};

};

namespace core {
T_sp ClassRepCreator_O::creator_allocate() {
  size_t size = gctools::sizeof_with_header<Class_O>();
#ifdef METER_ALLOCATIONS
  Class_sp c = lisp_standard_class();
  c->_allocation_counter += 1;
  c->_allocation_total_size += size;
#endif
  GC_ALLOCATE_VARIADIC(clbind::ClassRep_O,class_,lisp_class_rep_class());
  return class_;
};

};

namespace core {

LCC_RETURN Creator_O::LISP_CALLING_CONVENTION() {
  LCC_RETURN v;
  return v;
}

CL_DEFUN T_sp core__run_creator(Creator_sp c)
{
  return c->creator_allocate();
}
};
