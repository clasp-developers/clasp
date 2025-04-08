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
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/symbolTable.h>

#include <clasp/core/externalObject.h>
#include <clasp/core/null.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/lispDefinitions.h>
#include <clasp/core/symbol.h>
#include <clasp/core/instance.h>
#include <clasp/core/creator.h>
#include <clasp/core/record.h>
#include <clasp/core/print.h>
#include <clasp/core/wrappers.h>

/*
__BEGIN_DOC( classes, Cando Object Classes)

This chapter describes the classes and methods available within Cando-Script.
__END_DOC
*/

extern "C" {
bool low_level_equal(core::T_O* a, core::T_O* b) {
  core::T_sp ta((gctools::Tagged)a);
  core::T_sp tb((gctools::Tagged)b);
  return cl__equal(ta, tb);
};
};

extern "C" {
__attribute__((noinline)) void drag_delay(){};
};

namespace core {

uint __nextGlobalClassSymbol = 1;

void set_nextGlobalClassSymbol(uint z) { __nextGlobalClassSymbol = z; }

uint get_nextGlobalClassSymbol() { return __nextGlobalClassSymbol; }

uint get_nextGlobalClassSymbolAndAdvance() {
  uint n = __nextGlobalClassSymbol;
  __nextGlobalClassSymbol++;
  return n;
}

std::ostream& operator<<(std::ostream& out, T_sp obj) {
  out << _rep_(obj);
  return out;
}

T_sp core_initialize(T_sp obj, core::List_sp arg);

T_sp alist_from_plist(List_sp plist) {
  T_sp alist(nil<T_O>());
  while (plist.notnilp()) {
    T_sp key = oCar(plist);
    plist = oCdr(plist);
    T_sp val = oCar(plist);
    plist = oCdr(plist);
    alist = Cons_O::create(Cons_O::create(key, val), alist);
  }
  return alist; // should I reverse this?
}

CL_LAMBDA(class-name &rest args);
CL_DECLARE();
CL_DOCSTRING(R"dx(make-cxx-object makes a C++ object using the encode/decode/fields functions)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__make_cxx_object(T_sp class_or_name, T_sp args) {
  Instance_sp theClass;
  if (Instance_sp argClass = class_or_name.asOrNull<Instance_O>()) {
    theClass = argClass;
  } else if (class_or_name.nilp()) {
    goto BAD_ARG0;
  } else if (Symbol_sp name = class_or_name.asOrNull<Symbol_O>()) {
    theClass = gc::As_unsafe<Instance_sp>(cl__find_class(name, true, nil<T_O>()));
  } else {
    goto BAD_ARG0;
  }
  {
    T_sp instance = theClass->make_instance();
    if (args.notnilp()) {
      args = alist_from_plist(args);
      //      printf("%s:%d initializer alist = %s\n", __FILE__, __LINE__, _rep_(args).c_str());
      if (instance.generalp()) {
        General_sp ginstance(instance.unsafe_general());
        ginstance->initialize(args);
      } else {
        SIMPLE_ERROR("Add support to decode object of class: {}", _rep_(cl__class_of(instance)));
      }
    }
    return instance;
  }
BAD_ARG0:
  TYPE_ERROR(class_or_name, Cons_O::createList(cl::_sym_or, cl::_sym_class, cl::_sym_Symbol_O));
  UNREACHABLE();
}

CL_LAMBDA(class-name &rest args);
CL_DECLARE();
CL_DOCSTRING(
    R"dx(load-cxx-object makes a C++ object using the encode/decode/fields functions using decoder/loader(s) - they support patching of objects)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__load_cxx_object(T_sp class_or_name, T_sp args) {
  Instance_sp theClass;
  if (Instance_sp argClass = class_or_name.asOrNull<Instance_O>()) {
    theClass = argClass;
  } else if (class_or_name.nilp()) {
    goto BAD_ARG0;
  } else if (Symbol_sp name = class_or_name.asOrNull<Symbol_O>()) {
    theClass = gc::As_unsafe<Instance_sp>(cl__find_class(name, true, nil<T_O>()));
  } else {
    goto BAD_ARG0;
  }
  {
    T_sp instance = theClass->make_instance();
    if (args.notnilp()) {
      //      args = alist_from_plist(args);
      //      printf("%s:%d initializer alist = %s\n", __FILE__, __LINE__, _rep_(args).c_str());
      if (instance.generalp()) {
        General_sp ginstance(instance.unsafe_general());
        ginstance->decode(args);
      } else {
        SIMPLE_ERROR("Add support to decode object of class: {}", _rep_(cl__class_of(instance)));
      }
    }
    return instance;
  }
BAD_ARG0:
  TYPE_ERROR(class_or_name, Cons_O::createList(cl::_sym_class, cl::_sym_Symbol_O));
  UNREACHABLE();
}

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING(R"dx(fieldsp returns true if obj has a fields function)dx");
DOCGROUP(clasp);
CL_DEFUN bool core__fieldsp(T_sp obj) {
  if (obj.generalp()) {
    return obj.unsafe_general()->fieldsp();
  }
  SIMPLE_ERROR("Add support for fieldsp for {}", _rep_(cl__class_of(obj)));
}
CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(encode object as an a-list)dx");
DOCGROUP(clasp);
CL_DEFUN core::List_sp core__encode(T_sp arg) {
  if (arg.generalp())
    return arg.unsafe_general()->encode();
  IMPLEMENT_MEF("Implement for non-general objects");
};

CL_LAMBDA(obj arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(decode object from a-list)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__decode(T_sp obj, core::List_sp arg) {
  if (obj.generalp())
    obj.unsafe_general()->decode(arg);
  return obj;
  IMPLEMENT_MEF("Implement for non-general objects");
};

CL_LAMBDA(obj stream);
CL_DECLARE();
CL_DOCSTRING(R"dx(printCxxObject)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__print_cxx_object(T_sp obj, T_sp stream) {
  if (core__fieldsp(obj)) {
    stream_write_char(stream, '#');
    stream_write_char(stream, 'I');
    stream_write_char(stream, '(');
    Instance_sp myclass = lisp_instance_class(obj);
    ASSERT(myclass);
    Symbol_sp className = myclass->_className();
    cl__prin1(className, stream);
    core::List_sp alist = core__encode(obj);
    for (auto cur : alist) {
      Cons_sp entry = gc::As<Cons_sp>(oCar(cur));
      Symbol_sp key = gc::As<Symbol_sp>(oCar(entry));
      T_sp val = oCdr(entry);
      stream_write_char(stream, ' ');
      cl__prin1(key, stream);
      stream_finish_output(stream);
      stream_write_char(stream, ' ');
      cl__prin1(val, stream);
    }
    stream_write_char(stream, ' ');
    stream_write_char(stream, ')');
    stream_finish_output(stream);
  } else {
    SIMPLE_ERROR("Object does not provide fields");
  }
  return obj;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(lowLevelDescribe)dx");
DOCGROUP(clasp);
CL_DEFUN void core__low_level_describe(T_sp obj) {
  if (obj.generalp()) {
    General_sp gobj(obj.unsafe_general());
    if (gobj.nilp()) {
      printf("NIL\n");
      return;
    }
    gobj->describe(_lisp->_true());
  } else if (obj.consp()) {
    obj.unsafe_cons()->describe(_lisp->_true());
  } else {
    SIMPLE_ERROR("Add support to low-level-describe for object");
  }
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(copyTree)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp cl__copy_tree(T_sp arg) {
  if (arg.nilp())
    return nil<T_O>();
  if (Cons_sp c = arg.asOrNull<Cons_O>()) {
    return c->copyTree();
  }
  return arg;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(implementationClass)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__implementation_class(T_sp arg) { return lisp_static_class(arg); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(instanceClass)dx");
DOCGROUP(clasp);
CL_DEFUN Instance_sp core__instance_class(T_sp arg) { return lisp_instance_class(arg); };

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(classNameAsString)dx");
DOCGROUP(clasp);
CL_DEFUN string core__class_name_as_string(T_sp arg) {
  Instance_sp c = core__instance_class(arg);
  return c->_className()->fullName();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(instanceSig)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__instance_sig(T_sp obj) {
  if (obj.generalp()) {
    return obj.unsafe_general()->instanceSig();
  }
  IMPLEMENT_MEF("Implement for non-general objects");
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(instanceSigSet)dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__instance_sig_set(T_sp arg) {
  if (arg.generalp()) {
    return arg.unsafe_general()->instanceSigSet();
  }
  IMPLEMENT_MEF("Implement for non-general objects");
};

CL_NAME("INSTANCE-REF");
CL_LAMBDA(val obj idx);
CL_DECLARE();
CL_DOCSTRING(R"dx(set the (idx) slot of (obj) to (val))dx");
DOCGROUP(clasp);
CL_DEFUN_SETF T_sp core__instance_set(T_sp val, T_sp obj, int idx) {
  if (obj.generalp()) {
    return obj.unsafe_general()->instanceSet(idx, val);
  }
  IMPLEMENT_MEF("Implement for non-general objects");
};

CL_LAMBDA(obj idx);
CL_DECLARE();
CL_DOCSTRING(R"dx(instanceRef - return the (idx) slot value of (obj))dx");
DOCGROUP(clasp);
CL_DEFUN T_sp core__instance_ref(T_sp obj, int idx) {
  if (obj.generalp())
    return obj.unsafe_general()->instanceRef(idx);
  IMPLEMENT_MEF("Implement for non-general objects");
};

void General_O::initialize() {
  // do nothing
}

void General_O::initialize(core::List_sp alist) {
  Record_sp record = Record_O::create_initializer(alist);
  if (this->fieldsp())
    this->fields(record);
  record->errorIfInvalidArguments();
}

void General_O::fields(Record_sp record) {
  // Do nothing here
  // Any subclass that has slots that need to be (de)serialized must, MUST, MUST!
  // implement fieldsp() and fields(Record_sp node)
  // subclasses must also invoke their Base::fields(node) method so that base classes can
  // (de)serialize their fields.
  // Direct base classes of General_O or CxxObject_O don't need to call Base::fields(Record_sp node)
  //  because the base methods don't do anything.
  // But it's convenient to always call the this->Base::fields(node) in case the class hierarchy changes
  //  you don't want to be caught not calling a this->Base::fields(node) because the system will silently
  //  fail to (de)serialize any base class fields.
  // If subclasses don't implement these methods - then serialization will fail SILENTLY!!!!!
}

List_sp General_O::encode() {
  if (this->fieldsp()) {
    Record_sp record = Record_O::create_encoder();
    this->fields(record);
    return record->data();
  }
  return nil<T_O>();
}

void General_O::decode(core::List_sp alist) {
  if (this->fieldsp()) {
    Record_sp record = Record_O::create_decoder(alist);
    this->fields(record);
  }
}

string General_O::className() const {
  // TODO: refactor this as ->__class()->_classNameAsString
  // everywhere where we have obj->className()
  return this->__class()->_classNameAsString();
}

void General_O::sxhash_equal(HashGenerator& hg) const {
  if (hg.isFilling())
    hg.addGeneralAddress(this->asSmartPtr());
}

bool General_O::eql_(T_sp obj) const { return this->eq(obj); }

bool General_O::equal(T_sp obj) const { return this->eq(obj); }

bool General_O::equalp(T_sp obj) const { return this->equal(obj); }

T_sp HashGenerator::asList() const {
  ql::list l;
  for (int ii = 0; ii < this->_NextPartIndex; ii++) {
    l << make_fixnum(this->_Parts[ii]);
  }
  return l.cons();
}

#if 0
bool HashGenerator::addGeneralAddress(General_sp part) {
  ASSERT(part.generalp());
  if (this->isFull())
    return false;
  this->_Parts[this->_NextAddressIndex] = lisp_general_badge(part);
  this->_NextAddressIndex--;
  return true;
}

bool HashGenerator::addConsAddress(Cons_sp part) {
  ASSERT(part.consp());
  if (this->isFull()) {
    return false;
  }
  this->_Parts[this->_NextAddressIndex] = lisp_badge(part);
  this->_NextAddressIndex--;
  return true;
}
#endif
void Hash1Generator::hashObject(T_sp obj) { clasp_sxhash(obj, *this); }

#if 0
// Add an address - this may need to work with location dependency
bool Hash1Generator::addGeneralAddress(General_sp part) {
  ASSERT(part.generalp());
  this->_Part = lisp_general_badge(part);
  this->_PartIsPointer = true;
#ifdef DEBUG_HASH_GENERATOR
  if (this->_debug) {
    lisp_write(fmt::format("{}:{}:{} Added part --> {}\n", __FILE__, __LINE__, __FUNCTION__, _rep_(part)));
  }
#endif
  return true;
}

bool Hash1Generator::addConsAddress(Cons_sp part) {
  ASSERT(part.consp());
  this->_Part = lisp_badge(part);
  this->_PartIsPointer = true;
#ifdef DEBUG_HASH_GENERATOR
  if (this->_debug) {
    lisp_write(fmt::format("{}:{}:{} Added part --> {}\n", __FILE__, __LINE__, __FUNCTION__, _rep_(part)));
  }
#endif
  return true;
}
#endif

/*! Recursive hashing of objects need to be prevented from
    infinite recursion.   So we keep track of the depth of
    recursion.  clasp_sxhash will modify this->_Depth and
    when we return we have to restore _Depth to its
    value when it entered here. */
void HashGenerator::hashObject(T_sp obj) {
  int depth = this->_Depth;
  ++this->_Depth;
  LIKELY_if(this->_Depth < MaxDepth) clasp_sxhash(obj, *this);
  this->_Depth = depth;
}
void HashGenerator::hashObjectEqualp(T_sp obj) {
  int depth = this->_Depth;
  ++this->_Depth;
  LIKELY_if(this->_Depth < MaxDepth) clasp_sxhash_equalp(obj, *this);
  this->_Depth = depth;
}

Fixnum bignum_hash(const mpz_class& bignum) {
  auto bn = bignum.get_mpz_t();
  unsigned int size = bn->_mp_size;
  if (size < 0)
    size = -size;
  gc::Fixnum hash(5381);
  for (int i = 0; i < (int)size; i++) {
    hash = (gc::Fixnum)hash_word(hash, (uintptr_t)bn->_mp_d[i]);
  }
  return hash;
}

bool Hash1Generator::addValue(const mpz_class& bignum) {
  gc::Fixnum hash = bignum_hash(bignum);
  return this->addValue(hash);
}

bool HashGenerator::addValue(const mpz_class& bignum) {
  gc::Fixnum hash = bignum_hash(bignum);
  return this->addValue(hash);
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING(R"dx(Return t if obj is equal to T_O::_class->unboundValue())dx");
DOCGROUP(clasp);
CL_DEFUN bool core__sl_boundp(T_sp obj) {
  //    bool boundp = (obj.get() != T_O::___staticClass->unboundValue().get());
  bool boundp = !obj.unboundp();
#if DEBUG_CLOS >= 2
  printf("\nMLOG sl-boundp = %d address: %p\n", boundp, obj.get());
#endif
  return boundp;
};

void General_O::describe(T_sp stream) { clasp_write_string(this->__repr__(), stream); }

void General_O::__write__(T_sp strm) const {
  if (clasp_print_readably() && this->fieldsp()) {
    core__print_cxx_object(this->asSmartPtr(), strm);
  } else {
    clasp_write_string(this->__repr__(), strm);
  }
}

string General_O::descriptionOfContents() const { return ""; };

string General_O::description() const {
  stringstream ss;
  if (this == &*(_lisp->_true())) {
    ss << "t";
  } else {
    General_O* me_gc_safe = const_cast<General_O*>(this);
    ss << "#<" << me_gc_safe->_instanceClass()->_classNameAsString() << " ";
    ss << this->descriptionOfContents() << ">";
  }
  return ss.str();
};

T_sp General_O::instanceRef(size_t idx) const {
  SIMPLE_ERROR("T_O::instanceRef({}) invoked on object class[{}] val-->{}", idx, this->_instanceClass()->_classNameAsString(),
               this->__repr__());
}

T_sp General_O::instanceClassSet(Instance_sp val) {
  SIMPLE_ERROR("T_O::instanceClassSet to class {} invoked on object class[{}] val-->{} - subclass must implement", _rep_(val),
               this->_instanceClass()->_classNameAsString(), _rep_(this->asSmartPtr()));
}

T_sp General_O::instanceSet(size_t idx, T_sp val) {
  SIMPLE_ERROR("T_O::instanceSet({},{}) invoked on object class[{}] val-->{}", idx, _rep_(val),
               this->_instanceClass()->_classNameAsString(), _rep_(this->asSmartPtr()));
}

T_sp General_O::instanceSig() const {
  SIMPLE_ERROR("T_O::instanceSig() invoked on object class[{}] val-->{}", this->_instanceClass()->_classNameAsString(),
               this->__repr__());
}

T_sp General_O::instanceSigSet() {
  SIMPLE_ERROR("T_O::instanceSigSet() invoked on object class[{}] val-->{}", this->_instanceClass()->_classNameAsString(),
               _rep_(this->asSmartPtr()));
}

Instance_sp instance_class(T_sp obj) { return cl__class_of(obj); }

SYMBOL_SC_(CorePkg, slBoundp);
SYMBOL_SC_(CorePkg, instanceRef);
SYMBOL_SC_(CorePkg, instanceSet);
SYMBOL_EXPORT_SC_(CorePkg, instancep); // move to predicates.cc?
SYMBOL_SC_(CorePkg, instanceSigSet);
SYMBOL_SC_(CorePkg, instanceSig);
SYMBOL_EXPORT_SC_(CorePkg, instanceClass);
SYMBOL_EXPORT_SC_(CorePkg, implementationClass);
SYMBOL_EXPORT_SC_(CorePkg, classNameAsString);
SYMBOL_EXPORT_SC_(ClPkg, copyTree);

;

#include <clasp/core/multipleValues.h>

SYMBOL_EXPORT_SC_(ClPkg, eq);
SYMBOL_EXPORT_SC_(ClPkg, eql);
SYMBOL_EXPORT_SC_(ClPkg, equal);
SYMBOL_EXPORT_SC_(ClPkg, equalp);
}; // namespace core

namespace core {

void lisp_setStaticClass(gctools::Header_s::StampWtagMtag header, Instance_sp value) {
  if (_lisp->_Roots.staticClassesUnshiftedNowhere.size() == 0) {
    ASSERT(gctools::Header_s::StampWtagMtag::is_unshifted_stamp(gctools::STAMPWTAG_max));
    size_t stamp = gctools::Header_s::StampWtagMtag::make_nowhere_stamp(gctools::STAMPWTAG_max);
    //    printf("%s:%d:%s allocating %lu entries for stamp classes STAMWTAG_max %lu\n", __FILE__, __LINE__, __FUNCTION__, stamp,
    //    (size_t)gctools::STAMPWTAG_max );
    _lisp->_Roots.staticClassesUnshiftedNowhere.resize(stamp + 1);
  }
  size_t stamp = gctools::Header_s::Stamp(header);
  //  printf("%s:%d:%s header: %u  stamp: %lu  value: %s\n", __FILE__, __LINE__, __FUNCTION__, header, stamp, _rep_(value).c_str());
  _lisp->_Roots.staticClassesUnshiftedNowhere[stamp] = value;
}

void lisp_setStaticClassSymbol(gctools::Header_s::StampWtagMtag header, Symbol_sp value) {
  //  printf("%s:%d:%s  gctools::STAMP_max -> %u\n", __FILE__, __LINE__, __FUNCTION__, gctools::STAMP_max);
  if (_lisp->_Roots.staticClassSymbolsUnshiftedNowhere.size() == 0) {
    ASSERT(gctools::Header_s::StampWtagMtag::is_unshifted_stamp(gctools::STAMPWTAG_max));
    size_t stamp = gctools::Header_s::StampWtagMtag::make_nowhere_stamp(gctools::STAMPWTAG_max);
    //    printf("%s:%d:%s allocating %lu entries for stamp classes STAMWTAG_max %lu\n", __FILE__, __LINE__, __FUNCTION__, stamp,
    //    (size_t)gctools::STAMPWTAG_max );
    _lisp->_Roots.staticClassSymbolsUnshiftedNowhere.resize(stamp + 1);
  }
  size_t stamp = gctools::Header_s::Stamp(header);
  _lisp->_Roots.staticClassSymbolsUnshiftedNowhere[stamp] = value;
  //  printf("%s:%d:%s stamp: %lu  value: %s@%p\n", __FILE__, __LINE__, __FUNCTION__, stamp, _rep_(value).c_str(),
  //  &_lisp->_Roots.staticClassSymbolsUnshiftedNowhere[stamp]);
}
Symbol_sp lisp_getStaticClassSymbol(gctools::Header_s::StampWtagMtag header) {
  size_t stamp = gctools::Header_s::Stamp(header);
  T_sp value = _lisp->_Roots.staticClassSymbolsUnshiftedNowhere[stamp];
  //  printf("%s:%d:%s stamp: %lu  value: %s@%p\n", __FILE__, __LINE__, __FUNCTION__, stamp, _rep_(value).c_str(),
  //  &_lisp->_Roots.staticClassSymbolsUnshiftedNowhere[stamp]);
  Symbol_sp svalue = gc::As<Symbol_sp>(value);
  return svalue;
}

void lisp_setStaticInstanceCreator(gctools::Header_s::StampWtagMtag header, Creator_sp value) {
  if (_lisp->_Roots.staticInstanceCreatorsUnshiftedNowhere.size() == 0) {
    size_t stamp = gctools::Header_s::StampWtagMtag::make_nowhere_stamp(gctools::STAMPWTAG_max);
    _lisp->_Roots.staticInstanceCreatorsUnshiftedNowhere.resize(stamp + 1);
  }
  size_t stamp = gctools::Header_s::Stamp(header);
  _lisp->_Roots.staticInstanceCreatorsUnshiftedNowhere[stamp] = value;
}

Instance_sp lisp_getStaticClass(gctools::Header_s::StampWtagMtag header) {
  size_t stamp = gctools::Header_s::Stamp(header);
  Instance_sp class_ = _lisp->_Roots.staticClassesUnshiftedNowhere[stamp];
  // printf("%s:%d:%s stamp = %lu   class_ = %s\n", __FILE__, __LINE__, __FUNCTION__, stamp, _rep_(class_).c_str());
  return class_;
}
Creator_sp lisp_getStaticInstanceCreator(gctools::Header_s::StampWtagMtag header) {
  size_t stamp = gctools::Header_s::Stamp(header);
  return _lisp->_Roots.staticInstanceCreatorsUnshiftedNowhere[stamp];
}

}; // namespace core

namespace core {

CL_LAMBDA(x y);
CL_DECLARE();
CL_DOCSTRING(R"dx(equalp)dx");
DOCGROUP(clasp);
CL_DEFUN bool cl__equalp(T_sp x, T_sp y) {
  if (x.fixnump()) {
    if (y.fixnump()) {
      return x.raw_() == y.raw_();
    } else if (y.single_floatp()) {
      return (x.unsafe_fixnum() == y.unsafe_single_float());
    } else if (gc::IsA<Number_sp>(y)) {
      return basic_equalp(gc::As_unsafe<Fixnum_sp>(x), gc::As_unsafe<Number_sp>(y));
    } else
      return false;
  } else if (x.single_floatp()) {
    if (y.single_floatp()) {
      return x.unsafe_single_float() == y.unsafe_single_float();
    } else if (y.fixnump()) {
      return x.unsafe_single_float() == y.unsafe_fixnum();
    } else if (gc::IsA<Number_sp>(y)) {
      return basic_equalp(gc::As_unsafe<SingleFloat_sp>(x), gc::As_unsafe<Number_sp>(y));
    } else
      return false;
  } else if (x.characterp()) {
    return clasp_charEqual2(x, y);
  } else if (x.consp()) {
    Cons_O* cons = x.unsafe_cons();
    return cons->equalp(y);
  } else if (x.generalp()) {
    General_O* genx = x.unsafe_general();
    return genx->equalp(y);
  }
  SIMPLE_ERROR("Bad equalp comparison");
};
}; // namespace core
