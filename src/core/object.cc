/*
    File: object.cc
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
#define DEBUG_LEVEL_FULL

#include <clasp/core/useBoostPython.h>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/conditions.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/symbolTable.h>
#if defined(XML_ARCHIVE)
#include <xmlSaveAorchive.h>
#include <xmlLoadArchive.h>
#endif // defined(XML_ARCHIVE)
//#i n c l u d e "hierarchy.h"
//#i n c l u d e "render.h"

#include <clasp/core/externalObject.h>
#include <clasp/core/null.h>
#include <clasp/core/environment.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/wrappers.h>

/*
__BEGIN_DOC( classes, Cando Object Classes)

This chapter describes the classes and methods available within Cando-Script.
__END_DOC
*/

using namespace core;

uint __nextGlobalClassSymbol = 1;

void set_nextGlobalClassSymbol(uint z) {
  __nextGlobalClassSymbol = z;
}

uint get_nextGlobalClassSymbol() {
  return __nextGlobalClassSymbol;
}

uint get_nextGlobalClassSymbolAndAdvance() {
  uint n = __nextGlobalClassSymbol;
  __nextGlobalClassSymbol++;
  return n;
}

std::ostream &operator<<(std::ostream &out, T_sp obj) {
  out << _rep_(obj);
  return out;
}

_RootDummyClass::_RootDummyClass() : GCObject(){};

namespace core {

#define ARGS_af_lowLevelDescribe "(arg)"
#define DECL_af_lowLevelDescribe ""
#define DOCS_af_lowLevelDescribe "lowLevelDescribe"
void af_lowLevelDescribe(T_sp obj) {
  _G();
  if (obj.nilp()) {
    printf("NIL\n");
    return;
  }
  obj->describe();
};

#define ARGS_cl_eq "(x y)"
#define DECL_cl_eq ""
#define DOCS_cl_eq "eq"
bool cl_eq(T_sp x, T_sp y) {
  _G();
  return (x == y);
};

#define ARGS_cl_eql "(x y)"
#define DECL_cl_eql ""
#define DOCS_cl_eql "eql"
bool cl_eql(T_sp x, T_sp y) {
  _G();
  if (x.nilp()) {
    return y.nilp();
  }
  return x->eql(y);
};

#define ARGS_cl_equal "(x y)"
#define DECL_cl_equal ""
#define DOCS_cl_equal "equal"
bool cl_equal(T_sp x, T_sp y) {
  _G();
  if (x.nilp()) {
    return y.nilp();
  }
  return x->equal(y);
};

#define ARGS_cl_equalp "(x y)"
#define DECL_cl_equalp ""
#define DOCS_cl_equalp "equalp"
bool cl_equalp(T_sp x, T_sp y) {
  _G();
  if (x.nilp()) {
    return y.nilp();
  }
  return x->equalp(y);
};

#define ARGS_af_copyTree "(arg)"
#define DECL_af_copyTree ""
#define DOCS_af_copyTree "copyTree"
T_sp af_copyTree(T_sp arg) {
  _G();
  if (arg.nilp())
    return _Nil<T_O>();
  if (Cons_sp c = arg.asOrNull<Cons_O>()) {
    return c->copyTree();
  }
  return arg;
};

#define ARGS_af_implementationClass "(arg)"
#define DECL_af_implementationClass ""
#define DOCS_af_implementationClass "implementationClass"
T_sp af_implementationClass(T_sp arg) {
  _G();
  return lisp_static_class(arg);
};

#define ARGS_af_instanceClass "(arg)"
#define DECL_af_instanceClass ""
#define DOCS_af_instanceClass "instanceClass"
Class_sp af_instanceClass(T_sp arg) {
  _G();
  return lisp_instance_class(arg);
};

#define ARGS_af_classNameAsString "(arg)"
#define DECL_af_classNameAsString ""
#define DOCS_af_classNameAsString "classNameAsString"
string af_classNameAsString(T_sp arg) {
  _G();
  Class_sp c = af_instanceClass(arg);
  return c->name()->fullName();
};

#define ARGS_af_instanceSig "(arg)"
#define DECL_af_instanceSig ""
#define DOCS_af_instanceSig "instanceSig"
T_sp af_instanceSig(T_sp obj) {
  _G();
  return obj->instanceSig();
};

#define ARGS_af_instanceSigSet "(arg)"
#define DECL_af_instanceSigSet ""
#define DOCS_af_instanceSigSet "instanceSigSet"
T_sp af_instanceSigSet(T_sp arg) {
  _G();
  return arg->instanceSigSet();
};

#define ARGS_af_instanceSet "(obj idx val)"
#define DECL_af_instanceSet ""
#define DOCS_af_instanceSet "instanceSet - set the (idx) slot of (obj) to (val)"
T_sp af_instanceSet(T_sp obj, int idx, T_sp val) {
  _G();
  return obj->instanceSet(idx, val);
};

#define ARGS_af_instanceRef "(obj idx)"
#define DECL_af_instanceRef ""
#define DOCS_af_instanceRef "instanceRef - return the (idx) slot value of (obj)"
T_sp af_instanceRef(T_sp obj, int idx) {
  _G();
  return obj->instanceRef(idx);
};

#define ARGS_af_instancep "(obj)"
#define DECL_af_instancep ""
#define DOCS_af_instancep "instancep"
T_sp af_instancep(T_sp obj) {
  _G();
  return obj->oinstancep();
};

#define ARGS_af_isNil "(arg)"
#define DECL_af_isNil ""
#define DOCS_af_isNil "isNil"
bool af_isNil(T_sp arg) {
  _G();
  return arg.nilp();
};

void T_O::initialize() {
  // do nothing
}

string T_O::className() const {
  // TODO: refactor this as ->__class()->classNameAsString
  // everywhere where we have obj->className()
  return this->__class()->classNameAsString();
}

void T_O::sxhash(HashGenerator &hg) const {
  _G();
  int res = (int)((reinterpret_cast<unsigned long long int>(GC_BASE_ADDRESS_FROM_PTR(this)) >> 4) & INT_MAX);
  hg.addPart(res);
}

/*! Return new Object but keep same contents */
T_sp T_O::shallowCopy() const {
  _G();
  SUBCLASS_MUST_IMPLEMENT();
}

T_sp T_O::deepCopy() const {
  _G();
  SUBCLASS_MUST_IMPLEMENT();
}

bool T_O::eql(T_sp obj) const {
  return this->eq(obj);
}

bool T_O::equal(T_sp obj) const {
  _G();
  bool b = this->eq(obj);
  LOG(BF("Checking to see if this(%s) is equal to other(%s) result = %d") % this->__repr__() % _rep_(obj) % b);
  return b;
}

bool T_O::equalp(T_sp obj) const {
  if (obj.nilp())
    return false;
  return this->equal(obj);
}

bool T_O::isAInstanceOf(Class_sp mc) {
  if (this->eq(mc))
    return true;
  Symbol_sp classSymbol = mc->className();
  if (this->isAssignableToByClassSymbol(classSymbol))
    return true;
  return false;
}
};

void HashGenerator::hashObject(T_sp obj) {
  if (obj.nilp()) {
    this->addPart(0);
    return;
  }
  obj->sxhash(*this);
}

static BignumExportBuffer static_HashGenerator_addPart_buffer;

bool HashGenerator::addPart(const mpz_class &bignum) {
  unsigned int *buffer = static_HashGenerator_addPart_buffer.getOrAllocate(bignum, 0);
  size_t count(0);
  buffer = (unsigned int *)::mpz_export(buffer, &count,
                                        _lisp->integer_ordering()._mpz_import_word_order,
                                        _lisp->integer_ordering()._mpz_import_size,
                                        _lisp->integer_ordering()._mpz_import_endian,
                                        0,
                                        bignum.get_mpz_t());
  if (buffer != NULL) {
    for (int i = 0; i < (int)count; i++) {
      this->addPart(buffer[i]);
      if (this->isFull())
        return false;
    }
  } else {
    this->addPart(0);
  }
  return this->isFilling();
}

string T_O::asXmlString() const {
  return "T_O::asXmlString() ADD SUPPORT TO DUMP OBJECTS AS XML STRINGS";
}

core::Lisp_sp T_O::lisp() {
  return _lisp;
}

core::Lisp_sp T_O::lisp() const {
  return _lisp;
}

#if 0
void	T_O::setOwner(T_sp obj)
{
    IMPLEMENT_MEF(BF("Handle loss of _InitializationOwner"));
//    this->_InitializationOwner = obj;
}


void	T_O::initialize_setOwner(T_sp obj)
{
    this->_InitializationOwner = obj;
}
#endif

#define ARGS_af_slBoundp "(arg)"
#define DECL_af_slBoundp ""
#define DOCS_af_slBoundp "Return t if obj is equal to T_O::_class->unboundValue()"
bool af_slBoundp(T_sp obj) {
  _G();
  //    bool boundp = (obj.get() != T_O::___staticClass->unboundValue().get());
  bool boundp = !obj.unboundp();
#if DEBUG_CLOS >= 2
  printf("\nMLOG sl-boundp = %d address: %p\n", boundp, obj.get());
#endif
  return boundp;
};

void T_O::describe() {
  string s = this->__str__();
  _lisp->print(BF("%s") % s.c_str());
  //    _lisp->print(BF(""));
  //    dumpSourceInfo(this->sharedThis<T_O>());
}

void T_O::__write__(T_sp strm) const {
  clasp_write_string(this->__repr__(), strm);
}

bool T_O::eq(T_sp obj) const {
  if (!obj.pointerp())
    return false;
  return this == obj.get();
}

void T_O::setTrackName(const string &msg) {
  _G();
  //#ifdef	DEBUG_OBJECT_ON
  //    this->_TrackWhenDestructed = true;
  //    this->_TrackId = msg;
  //#endif
}

#if defined(OLD_SERIALIZE)
void T_O::serialize(serialize::SNode node) {
  _OF();
  SIMPLE_ERROR(BF("T_O::serialize was invoked for an object of class[%s]\n"
                  "- you should implement serialize  for this class so that it doesn't fall through to here") %
               this->_instanceClass()->classNameAsString());
}
#endif

void T_O::archiveBase(core::ArchiveP node) {
  // Nothing to do here
  SUBIMP();
}

bool T_O::loadFinalize(core::ArchiveP node) {
  _G();
  return true;
}

string T_O::descriptionOfContents() const {
  return "";
};

string T_O::description() const {
  _OF();
  stringstream ss;
  if (this == _lisp->_true().get()) {
    ss << "t";
  } else {
    T_O *me_gc_safe = const_cast<T_O *>(this);
    ss << "#<" << me_gc_safe->_instanceClass()->classNameAsString() << " ";

    ss << "@" << std::hex << this << std::dec;
    ss << ")";
    ss << this->descriptionOfContents() << " > ";
  }
  return ss.str();
};

string T_O::descriptionNonConst() {
  return this->description();
}

bool T_O::isAssignableToByClassSymbol(Symbol_sp ancestorClassSymbol) const {
  T_sp ancestorClass = eval::funcall(cl::_sym_findClass, ancestorClassSymbol, _lisp->_true());
  Class_sp myClass = this->__class();
  bool b = af_subclassp(myClass, ancestorClass);
  return b;
}

bool T_O::isAssignableToClass(core::Class_sp mc) const {
  return this->isAssignableToByClassSymbol(mc->className());
}

#if 0
bool T_O::isOfClassByClassSymbol(Symbol_sp classSymbol)
{_G();
    Class_sp mc = _lisp->classFromClassSymbol(classSymbol);
    Class_sp myClass = this->__class();
    bool sameClass = (myClass.get() == mc.get() );
    LOG(BF("Checking if this->_class(%s) == other class(%s) --> %d") % myClass->instanceClassName() % mc->instanceClassName() % sameClass );
    return sameClass;
}
#endif

void T_O::initializeSlots(int slots) {
  SIMPLE_ERROR(BF("T_O::initializeSlots invoked - subclass must implement"));
};

T_sp T_O::instanceRef(int idx) const {
  _G();
  SIMPLE_ERROR(BF("T_O::instanceRef(%d) invoked on object class[%s] val-->%s") % idx % this->_instanceClass()->classNameAsString() % this->__repr__());
}

T_sp T_O::instanceClassSet(Class_sp val) {
  _G();
  SIMPLE_ERROR(BF("T_O::instanceClassSet to class %s invoked on object class[%s] val-->%s - subclass must implement") % _rep_(val) % this->_instanceClass()->classNameAsString() % _rep_(this));
}

T_sp T_O::instanceSet(int idx, T_sp val) {
  _G();
  SIMPLE_ERROR(BF("T_O::instanceSet(%d,%s) invoked on object class[%s] val-->%s") % idx % _rep_(val) % this->_instanceClass()->classNameAsString() % _rep_(this));
}

T_sp T_O::instanceSig() const {
  _G();
  SIMPLE_ERROR(BF("T_O::instanceSig() invoked on object class[%s] val-->%s") % this->_instanceClass()->classNameAsString() % this->__repr__());
}

T_sp T_O::instanceSigSet() {
  _G();
  SIMPLE_ERROR(BF("T_O::instanceSigSet() invoked on object class[%s] val-->%s") % this->_instanceClass()->classNameAsString() % _rep_(this));
}

void T_O::exposeCando(core::Lisp_sp lisp) {
  class_<T_O> ot;
  Defun(lowLevelDescribe);
  SYMBOL_SC_(CorePkg, slBoundp);
  Defun(slBoundp);
  SYMBOL_SC_(CorePkg, isNil);
  Defun(isNil);
  SYMBOL_SC_(CorePkg, instanceRef);
  Defun(instanceRef);
  SYMBOL_SC_(CorePkg, instanceSet);
  Defun(instanceSet);
  SYMBOL_SC_(CorePkg, instancep);
  Defun(instancep);
  SYMBOL_SC_(CorePkg, instanceSigSet);
  Defun(instanceSigSet);
  SYMBOL_SC_(CorePkg, instanceSig);
  Defun(instanceSig);
  SYMBOL_EXPORT_SC_(ClPkg, eq);
  ClDefun(eq);
  SYMBOL_EXPORT_SC_(ClPkg, eql);
  ClDefun(eql);
  SYMBOL_EXPORT_SC_(ClPkg, equal);
  ClDefun(equal);
  SYMBOL_EXPORT_SC_(ClPkg, equalp);
  ClDefun(equalp);
  SYMBOL_EXPORT_SC_(CorePkg, instanceClass);
  Defun(instanceClass);
  SYMBOL_EXPORT_SC_(CorePkg, implementationClass);
  Defun(implementationClass);
  SYMBOL_EXPORT_SC_(CorePkg, classNameAsString);
  Defun(classNameAsString);
  SYMBOL_EXPORT_SC_(ClPkg, copyTree);
  Defun(copyTree);
};

void T_O::exposePython(Lisp_sp lisp) { // lisp will be undefined - don't use it
#ifdef USEBOOSTPYTHON
  boost::python::class_<T_O, T_sp,
                        boost::noncopyable>("Object", boost::python::no_init)
      //    	.def("class",&T_O::_class)
      //	.def("instanceBaseClass",&T_O::instanceBaseClass)
      .def("classSymbol", &T_O::classSymbol)
      .def("describe", &T_O::describe)
      .def("description", &T_O::descriptionNonConst)
      .def("core:className", &T_O::className_notConst)
      .def("classNameSymbol", &T_O::classNameSymbol)
      //	.def("baseClassSymbol",&T_O::baseClassSymbol)
      .def("sameAs", &T_O::sameAs)
      //	.def("atom", &T_O::atomp)
      .def("consp", &T_O::consP)
      .def("sourceCodeConsP", &T_O::sourceCodeConsP)
      .def("vectorp", &T_O::vectorP)
      .def("symbolp", &T_O::symbolP)
      .def("numberp", &T_O::numberP)
      .def("stringp", &T_O::stringP)
      .def("booleanp", &T_O::booleanP)
      .def("specialFormp", &T_O::specialFormP)
      .def("isNil", &T_O::isNil)
      //	.def("notNil",&T_O::notNil)
      .def("eq", &T_O::eq)
      //	.def("eql", &T_O::eql)
      .def("eqn", &T_O::eqn)
      //	.def("equal", &T_O::equal)
      .def("equalp", &T_O::equalp)
      .def("neq", &T_O::neql)
      .def("lt", &T_O::operator<)
      .def("shallowCopy", &T_O::shallowCopy)
      .def("deepCopy", &T_O::deepCopy)

      .def("car", &T_O::ocar)
      .def("cdr", &T_O::ocdr)
      .def("caar", &T_O::ocaar)
      .def("cadr", &T_O::ocadr)
      .def("cdar", &T_O::ocdar)
      .def("cddr", &T_O::ocddr)
      .def("caaar", &T_O::ocaaar)
      .def("caadr", &T_O::ocaadr)
      .def("cadar", &T_O::ocadar)
      .def("caddr", &T_O::ocaddr)
      .def("cdaar", &T_O::ocdaar)
      .def("cdadr", &T_O::ocdadr)
      .def("cddar", &T_O::ocddar)
      .def("cdddr", &T_O::ocdddr)
      .def("caaaar", &T_O::ocaaaar)
      .def("caadar", &T_O::ocaadar)
      .def("cadaar", &T_O::ocadaar)
      .def("caddar", &T_O::ocaddar)
      .def("cdaaar", &T_O::ocdaaar)
      .def("cdadar", &T_O::ocdadar)
      .def("cddaar", &T_O::ocddaar)
      .def("cdddar", &T_O::ocdddar)
      .def("caaadr", &T_O::ocaaadr)
      .def("caaddr", &T_O::ocaaddr)
      .def("cadadr", &T_O::ocadadr)
      .def("cadddr", &T_O::ocadddr)
      .def("cdaadr", &T_O::ocdaadr)
      .def("cdaddr", &T_O::ocdaddr)
      .def("cddadr", &T_O::ocddadr)
      .def("cddddr", &T_O::ocddddr)

      .def("first", &T_O::ocar)
      .def("rest", &T_O::ocdr)

      .def("first", &T_O::ofirst)
      .def("second", &T_O::osecond)
      .def("third", &T_O::othird)
      .def("fourth", &T_O::ofourth)
      .def("fifth", &T_O::ofifth)
      .def("sixth", &T_O::osixth)
      .def("seventh", &T_O::oseventh)
      .def("eighth", &T_O::oeighth)
      .def("ninth", &T_O::oninth)
      .def("tenth", &T_O::otenth)

      ;
#endif
}

namespace core {

EXPOSE_CLASS(core, T_O);

#include <clasp/core/multipleValues.h>
};
