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
#include <clasp/core/builtInClass.h>
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

CL_LAMBDA(class theDirectSuperclasses);
CL_DECLARE();
CL_DOCSTRING("inheritDefaultAllocator - make this a regular function so that there are no dispatching problems at boot time");
CL_DEFUN void core__inherit_default_allocator(Class_sp cl, T_sp directSuperclasses) {
  //        printf("%s:%d In core__inherit_default_allocator for class: %s direct-superclasses: %s\n",__FILE__,__LINE__, _rep_(cl).c_str(), _rep_(directSuperclasses).c_str());
  cl->inheritDefaultAllocator(directSuperclasses);
};


#if 0
void Class_O::initializeSlots(Fixnum stamp, size_t slots) {
  if (slots==0) {
    printf("%s:%d initializeSlots slots = 0\n", __FILE__, __LINE__ );
  }
//  if ( _lisp->_PackagesInitialized ) printf("%s:%d Changing the #slots to %lu\n", __FILE__, __LINE__, slots );
  this->_Rack = SimpleVector_O::make(slots+1, _Unbound<T_O>(),true);
  this->stamp_set(stamp);
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, _Nil<T_O>());
  this->instanceSet(REF_CLASS_DIRECT_DEFAULT_INITARGS, _Nil<T_O>());
  this->instanceSet(REF_CLASS_FINALIZED, _Nil<T_O>());
}
#endif

#if 0
CL_LISPIFY_NAME("core:nameOfClass");
CL_DEFMETHOD Symbol_sp Class_O::className() const {
  return this->name();
  //    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
}

string Class_O::classNameAsString() const {
  return _rep_(this->name());
  //    SIMPLE_ERROR(BF("You should use instanceClassName rather than className for classes"));
}
#endif


#if 0
string Class_O::__repr__() const {
  if (this == _lisp->_true().get()) {
    return "#<built-in-class t>";
  }
  stringstream ss;
  ss << "#<" << _rep_(this->_MetaClass->name()) << " " << this->instanceClassName() << ">";

  return ss.str();
}
#endif


#if 0
string Class_O::getPackageName() const {
  return gc::As<Package_sp>(this->name()->getPackage())->getName();
}
#endif


};

namespace core {

#if 0
void Class_O::appendDirectSuperclassAndResetClassPrecedenceList(Class_sp superClass) {
  List_sp directSuperclasses = this->directSuperclasses();
  directSuperclasses = Cons_O::create(superClass, directSuperclasses);
  this->instanceSet(REF_CLASS_DIRECT_SUPERCLASSES, directSuperclasses);
  this->instanceSet(REF_CLASS_CLASS_PRECEDENCE_LIST, _Nil<T_O>());
}
#endif



/*
  __BEGIN_DOC(classes.classMethods.describe,describe)
  \scriptCmd{describe}{classObject}

  Dumps a description of the class to stdout.
  __END_DOC
*/
#if 0
void Class_O::describe(T_sp stream) {
  stringstream ss;
  ss << (BF("Class instanceClassName %s\n") % this->instanceClassName().c_str()).str();
  ss << (BF("FullName %s\n") % this->name()->fullName().c_str()).str();
  if (this->directSuperclasses().nilp()) {
    ss << (BF("There are no super-classes!!!!!!\n")).str();
  } else {
    for (Cons_sp cc : this->directSuperclasses()) {
      ss << (BF("directSuperclasses: %s\n") % gc::As<Class_sp>(oCar(cc))->instanceClassName().c_str()).str();
    }
  }
  ss << (BF(" this.instanceCreator* = %p\n") % (void *)(this->class_creator().raw_())).str();
  ss << (BF("cxxClassP[%d]  cxxDerivableClassP[%d]   primaryCxxDerivableClassP[%d]\n") % this->cxxClassP() % this->cxxDerivableClassP() % this->primaryCxxDerivableClassP()).str();
  clasp_write_string(ss.str(), stream);
}
#endif


#if 0
T_sp Class_O::instanceRef(size_t idx) const {
  if ((idx+1)>= this->_Rack->length()) {
    SIMPLE_ERROR(BF("Class slot %d is out of bounds only %d slots available with metaclass %s") % (idx+1) % this->_Rack->length() % this->_MetaClass->_classNameAsString().c_str());
  }
  ASSERT((*this->_Rack)[idx+1]);
  T_sp val = (*this->_Rack)[idx+1];
  return val;
}
#endif


#if 0
T_sp Class_O::instanceSet(size_t idx, T_sp val) {
  if ((idx+1)>= this->_Rack->length()) {
    SIMPLE_ERROR(BF("Class slot %d is out of bounds only %d slots available with metaclass %s") % idx % this->_Rack->length()  % this->_MetaClass->_classNameAsString().c_str());
  }
  (*this->_Rack)[idx+1] = val;
  return val;
}
#endif


#if 0
Fixnum Class_O::stamp() const { return (*this->_Rack)[0].unsafe_fixnum();};
void Class_O::stamp_set(Fixnum s) { (*this->_Rack)[0] = clasp_make_fixnum(s); };
#endif

#if 0
T_sp Class_O::copyInstance() const {
  GC_COPY(Class_O,c,*this);
  return c;
};
#endif

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

#if 0
T_sp Class_O::instanceSigSet() {
  // Do nothing
  Class_sp mc = gc::As<Class_sp>(this->_instanceClass());
  ASSERTNOTNULL(mc);
  T_sp sig = mc->slots();
  ASSERTNOTNULL(sig);
  this->_Signature_ClassSlots = sig;
#if DEBUG_CLOS >= 2
  printf("\nMLOG instance_set_sig object %p\n", (void *)(this));
#endif
  return sig;
}
#endif


#if 0
T_sp Class_O::instanceSig() const {
  ASSERTNOTNULL(this->_Signature_ClassSlots);
#if DEBUG_CLOS >= 2
  printf("\nMLOG instance_sig object %p\n", (void *)(this));
#endif
  return this->_Signature_ClassSlots;
}
#endif

  
CL_LAMBDA(low high);
CL_DECLARE();
CL_DOCSTRING("subclassp");
CL_DEFUN bool core__subclassp(T_sp low, T_sp high) {
  if (low == high)
    return true;
  if (Class_sp lowmc = low.asOrNull<Class_O>()) {
    List_sp lowClassPrecedenceList = lowmc->instanceRef(Class_O::REF_CLASS_CLASS_PRECEDENCE_LIST); // classPrecedenceList();
    return lowClassPrecedenceList.asCons()->memberEq(high).notnilp();
  } else if (Instance_sp inst = low.asOrNull<Instance_O>()) {
    (void)inst;
    IMPLEMENT_MEF(BF("Run some other tests to make sure that instance is a Class: %s") % _rep_(low));
  }
  SIMPLE_ERROR(BF("Illegal argument for subclassp: %s") % _rep_(low));
};

SYMBOL_SC_(CorePkg, subclassp);
SYMBOL_SC_(CorePkg, allocateRawClass);
SYMBOL_EXPORT_SC_(CorePkg, inheritDefaultAllocator);




};
