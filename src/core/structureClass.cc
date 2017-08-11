/*
    File: structureClass.cc
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
#include <clasp/core/evaluator.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/standardObject.h>
#include <clasp/core/package.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/wrappers.h>

namespace core {
StructureClass_sp StructureClass_O::createUncollectable(gctools::Stamp is) {
  GC_ALLOCATE_UNCOLLECTABLE(StructureClass_O, bic, is);
  return bic;
}


void StructureClass_O::initialize() {
  DEPRECATED();
  this->Base::initialize();
  this->initializeSlots(REF_NUMBER_OF_SLOTS_IN_CLASSES);
  this->initializeClassSlots();
  
//  this->_InstanceCoreClass = _Nil<Class_O>();
}

void StructureClass_O::reinitialize_class() {
  this->_instance_stamp = gctools::NextStamp();
//  printf("%s:%d StructureClass_O::reinitialize_class setting new stamp -> %" PRu "\n", __FILE__, __LINE__, this->_instance_stamp );
}


CL_LISPIFY_NAME("core:ensure-structure-class");
CL_DEFUN StructureClass_sp StructureClass_O::ensure_structure_class(Symbol_sp name, T_sp included_class, List_sp mixins)
{
  GC_ALLOCATE(StructureClass_O, sc);
  sc->setName(name);
  List_sp direct_superclasses = _Nil<T_O>();
  if ( included_class.notnilp() ) {
    direct_superclasses = Cons_O::create(included_class,_Nil<T_O>());
  }
  for ( auto cur : mixins ) {
    T_sp mix = oCar(cur);
    direct_superclasses = Cons_O::create(mix,direct_superclasses);
  }
  sc->setInstanceBaseClasses(direct_superclasses);
  eval::funcall(core::_sym_setf_findClass,sc,name);
  return sc;
}
};
