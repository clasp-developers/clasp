/*
    File: metaClass.cc
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

//#i n c l u d e <boost/graph/properties.hpp>
#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunneeded-internal-declaration"
//#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#include <boost/graph/vector_as_graph.hpp>
#include <boost/graph/topological_sort.hpp>
#pragma clang diagnostic pop
#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/metaClass.h>
#include <clasp/core/package.h>
#include <clasp/core/pointer.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/cons.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/lispList.h>
#include <clasp/core/array.h>
#include <clasp/core/instance.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/wrappers.h>

#define NAMESPACE_gctools_mem
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_gctools_mem

SYMBOL_EXPORT_SC_(ClosPkg,forward_referenced_class);
SYMBOL_EXPORT_SC_(ClPkg,built_in_class);
SYMBOL_EXPORT_SC_(ClPkg,standard_class);
SYMBOL_EXPORT_SC_(CorePkg,std_class);
SYMBOL_EXPORT_SC_(ClPkg,structure_class);
SYMBOL_EXPORT_SC_(CorePkg,cxx_class);
      

namespace core {

gc::Nilable<Instance_sp> identifyCxxDerivableAncestorClass(Instance_sp aClass) {
  if (aClass->cxxClassP()) {
    if (aClass->cxxDerivableClassP()) {
      return aClass;
    }
  }
  for (auto supers : aClass->directSuperclasses()) {
    Instance_sp aSuperClass = gc::As<Instance_sp>(oCar(supers));
    gc::Nilable<Instance_sp> taPossibleCxxDerivableAncestorClass = identifyCxxDerivableAncestorClass(aSuperClass);
    if (taPossibleCxxDerivableAncestorClass.notnilp())
      return taPossibleCxxDerivableAncestorClass;
  }
  return _Nil<Instance_O>();
}

SYMBOL_EXPORT_SC_(KeywordPkg,creator);

CL_DEFUN T_sp core__compute_instance_creator(T_sp tinstance, T_sp tmetaclass, List_sp superclasses)
{
  // If there is no metaclass - then use _TheStandardClass
  if (tmetaclass.nilp()) {
    tmetaclass = _lisp->_Roots._TheStandardClass;
  }
  Instance_sp instance = gc::As<Instance_sp>(tinstance);
  Instance_sp metaclass = gc::As<Instance_sp>(tmetaclass);
  // If instance class already has an allocator then leave it alone
  if (instance->CLASS_has_creator()) return instance->CLASS_get_creator();
  if (metaclass->_className() == clos::_sym_funcallable_standard_class) {
    FunctionDescription* fdesc = makeFunctionDescription(kw::_sym_creator);
    Creator_sp funcallableInstanceCreator = gc::GC<FuncallableInstanceCreator_O>::allocate(fdesc,instance);
    return funcallableInstanceCreator;
  };
  Instance_sp aCxxDerivableAncestorClass_unsafe; // Danger!  Unitialized!
#ifdef DEBUG_CLASS_INSTANCE
  printf("%s:%d:%s   for class -> %s   superclasses -> %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(instance->name()).c_str(), _rep_(superclasses).c_str());
#endif
  for (auto cur : superclasses) {
    T_sp tsuper = oCar(cur);
    if (Instance_sp aSuperClass = tsuper.asOrNull<Instance_O>() ) {
      if (aSuperClass->cxxClassP() && !aSuperClass->cxxDerivableClassP()) {
        SIMPLE_ERROR(BF("You cannot derive from the non-derivable C++ class %s\n"
                        "any C++ class you want to derive from must inherit from the clbind derivable class") %
                     _rep_(aSuperClass->_className()));
      }
      gc::Nilable<Instance_sp> aPossibleCxxDerivableAncestorClass = identifyCxxDerivableAncestorClass(aSuperClass);
      if (aPossibleCxxDerivableAncestorClass.notnilp()) {
        if (!aCxxDerivableAncestorClass_unsafe) {
          aCxxDerivableAncestorClass_unsafe = aPossibleCxxDerivableAncestorClass;
        } else {
          SIMPLE_ERROR(BF("Only one derivable C++ class is allowed to be"
                          " derived from at a time instead we have two %s and %s ") %
                       _rep_(aCxxDerivableAncestorClass_unsafe->_className()) % _rep_(aPossibleCxxDerivableAncestorClass->_className()));
        }
      }
    } else if ( Instance_sp iSuperClass = tsuper.asOrNull<Instance_O>() ) {
      SIMPLE_ERROR(BF("In Clasp, Instances are never Classes - so instance error should never occur.  If it does - figure out why tsuper is an Instance"));
      // I don't think I do anything here
      // If aCxxDerivableAncestorClass_unsafe is left unchanged then
      // an InstanceCreator_O will be created for this class.
    }
  }
  if (aCxxDerivableAncestorClass_unsafe) {
    // Here aCxxDerivableAncestorClass_unsafe has a value - so it's ok to dereference it
    Creator_sp aCxxAllocator(gctools::As<Creator_sp>(aCxxDerivableAncestorClass_unsafe->CLASS_get_creator()));
#ifdef DEBUG_CLASS_INSTANCE
    printf("%s:%d   duplicating aCxxDerivableAncestorClass_unsafe %s creator\n", __FILE__, __LINE__, _rep_(aCxxDerivableAncestorClass_unsafe).c_str());
#endif
    Creator_sp dup = aCxxAllocator->duplicateForClassName(instance->_className());
    return dup;
  } else {
 // I think this is the most common outcome -
#ifdef DEBUG_CLASS_INSTANCE
    printf("%s:%d   Creating an InstanceCreator_O for the class: %s\n", __FILE__, __LINE__, _rep_(instance->name()).c_str());
#endif
    FunctionDescription* fdesc = makeFunctionDescription(kw::_sym_creator);
    InstanceCreator_sp instanceAllocator = gc::GC<InstanceCreator_O>::allocate(fdesc,instance);
    return instanceAllocator;
  }
}

/*! Return true if every member of subset is in superset */
bool subsetp(List_sp subset, List_sp superset) {
  ASSERT(subset);
  ASSERT(superset);
  if (subset.nilp() && superset.nilp())
    return true;
  if (superset.nilp())
    return false;
  if (subset.nilp())
    return true;
  for (; subset.notnilp(); subset = oCdr(subset)) {
    T_sp o = oCar(subset);
    if (!superset.asCons()->memberEq(o))
      return false;
  }
  return true;
}
  
CL_LAMBDA(low high);
CL_DECLARE();
CL_DOCSTRING("subclassp");
CL_DEFUN bool core__subclassp(T_sp low, T_sp high) {
  ASSERT(clos__classp(low));
  ASSERT(clos__classp(high));
  if (low == high)
    return true;
  if (Instance_sp lowmc = low.asOrNull<Instance_O>()) {
    List_sp lowClassPrecedenceList = lowmc->instanceRef(Instance_O::REF_CLASS_CLASS_PRECEDENCE_LIST); // classPrecedenceList();
    return lowClassPrecedenceList.asCons()->memberEq(high).notnilp();
  }
  SIMPLE_ERROR(BF("Illegal argument for subclassp: %s") % _rep_(low));
};

SYMBOL_SC_(CorePkg, subclassp);
SYMBOL_SC_(CorePkg, allocateRawClass);
SYMBOL_EXPORT_SC_(CorePkg, inheritDefaultAllocator);




};
