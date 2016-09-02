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
#include <clasp/core/record.h>
#include <clasp/core/print.h>
#include <clasp/core/wrappers.h>

/*
__BEGIN_DOC( classes, Cando Object Classes)

This chapter describes the classes and methods available within Cando-Script.
__END_DOC
*/


namespace core {

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

T_sp core_initialize(T_sp obj, core::List_sp arg);

T_sp alist_from_plist(List_sp plist) {
  T_sp alist(_Nil<T_O>());
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
CL_DOCSTRING("make-cxx-object makes a C++ object using the encode/decode/fields functions");
CL_DEFUN T_sp core__make_cxx_object(T_sp class_or_name, T_sp args) {
  Class_sp theClass;
  if (Class_sp argClass = class_or_name.asOrNull<Class_O>()) {
    theClass = argClass;
  } else if (class_or_name.nilp()) {
    goto BAD_ARG0;
  } else if (Symbol_sp name = class_or_name.asOrNull<Symbol_O>()) {
    theClass = cl__find_class(name, true, _Nil<T_O>());
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
        SIMPLE_ERROR(BF("Add support to decode object of class: %s") % _rep_(cl__class_of(instance)));
      }
    }
    return instance;
  }
BAD_ARG0:
  TYPE_ERROR(class_or_name, Cons_O::createList(cl::_sym_Class_O, cl::_sym_Symbol_O));
  UNREACHABLE();
}


CL_LAMBDA(class-name &rest args);
CL_DECLARE();
CL_DOCSTRING("load-cxx-object makes a C++ object using the encode/decode/fields functions using decoder/loader(s) - they support patching of objects");
CL_DEFUN T_sp core__load_cxx_object(T_sp class_or_name, T_sp args) {
  Class_sp theClass;
  if (Class_sp argClass = class_or_name.asOrNull<Class_O>()) {
    theClass = argClass;
  } else if (class_or_name.nilp()) {
    goto BAD_ARG0;
  } else if (Symbol_sp name = class_or_name.asOrNull<Symbol_O>()) {
    theClass = cl__find_class(name, true, _Nil<T_O>());
  } else {
    goto BAD_ARG0;
  }
  {
    T_sp instance = theClass->make_instance();
    if (args.notnilp()) {
      args = alist_from_plist(args);
      //      printf("%s:%d initializer alist = %s\n", __FILE__, __LINE__, _rep_(args).c_str());
      if ( instance.generalp() ) {
        General_sp ginstance(instance.unsafe_general());
        ginstance->decode(args);
      } else {
        SIMPLE_ERROR(BF("Add support to decode object of class: %s") % _rep_(cl__class_of(instance)));
      }
    }
    return instance;
  }
BAD_ARG0:
  TYPE_ERROR(class_or_name, Cons_O::createList(cl::_sym_Class_O, cl::_sym_Symbol_O));
  UNREACHABLE();
}

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING("fieldsp returns true if obj has a fields function");
CL_DEFUN bool core__fieldsp(T_sp obj) {
  if ( obj.generalp() ) {
    return obj.unsafe_general()->fieldsp();
  }
  SIMPLE_ERROR(BF("Add support for fieldsp for %s") % _rep_(cl__class_of(obj)));
}
CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("encode object as an a-list");
CL_DEFUN core::List_sp core__encode(T_sp arg) {
  if (arg.generalp()) return arg.unsafe_general()->encode();
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(obj arg);
CL_DECLARE();
CL_DOCSTRING("decode object from a-list");
CL_DEFUN T_sp core__decode(T_sp obj, core::List_sp arg) {
  if (obj.generalp()) obj.unsafe_general()->decode(arg);
  return obj;
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(obj stream);
CL_DECLARE();
CL_DOCSTRING("printCxxObject");
CL_DEFUN T_sp core__print_cxx_object(T_sp obj, T_sp stream) {
  if (core__fieldsp(obj)) {
    clasp_write_char('#', stream);
    clasp_write_char('I', stream);
    clasp_write_char('(', stream);
    Class_sp myclass = lisp_instance_class(obj);
    ASSERT(myclass);
    Symbol_sp className = myclass->name();
    cl__prin1(className, stream);
    core::List_sp alist = core__encode(obj);
    for (auto cur : alist) {
      Cons_sp entry = gc::As<Cons_sp>(oCar(cur));
      Symbol_sp key = gc::As<Symbol_sp>(oCar(entry));
      T_sp val = oCdr(entry);
      clasp_write_char(' ', stream);
      cl__prin1(key, stream);
      clasp_finish_output(stream);
      clasp_write_char(' ', stream);
      cl__prin1(val, stream);
    }
    clasp_write_char(' ', stream);
    clasp_write_char(')', stream);
    clasp_finish_output(stream);
  } else {
    SIMPLE_ERROR(BF("Object does not provide fields"));
  }
  return obj;
}

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("lowLevelDescribe");
CL_DEFUN void core__low_level_describe(T_sp obj) {
  if ( obj.generalp() ) {
    General_sp gobj(obj.unsafe_general());
    if (gobj.nilp()) {
      printf("NIL\n");
      return;
    }
    gobj->describe(_lisp->_true());
  } else if (obj.consp()) {
    obj.unsafe_cons()->describe(_lisp->_true());
  } else {
    SIMPLE_ERROR(BF("Add support to low-level-describe for object"));
  }
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("copyTree");
CL_DEFUN T_sp cl__copy_tree(T_sp arg) {
  if (arg.nilp())
    return _Nil<T_O>();
  if (Cons_sp c = arg.asOrNull<Cons_O>()) {
    return c->copyTree();
  }
  return arg;
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("implementationClass");
CL_DEFUN T_sp core__implementation_class(T_sp arg) {
  return lisp_static_class(arg);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("instanceClass");
CL_DEFUN Class_sp core__instance_class(T_sp arg) {
  return lisp_instance_class(arg);
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("classNameAsString");
CL_DEFUN string core__class_name_as_string(T_sp arg) {
  Class_sp c = core__instance_class(arg);
  return c->name()->fullName();
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("instanceSig");
CL_DEFUN T_sp core__instance_sig(T_sp obj) {
  if ( obj.generalp() ) {
    return obj.unsafe_general()->instanceSig();
  }
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("instanceSigSet");
CL_DEFUN T_sp core__instance_sig_set(T_sp arg) {
  if ( arg.generalp() ) {
    return arg.unsafe_general()->instanceSigSet();
  }
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(obj idx val);
CL_DECLARE();
CL_DOCSTRING("instanceSet - set the (idx) slot of (obj) to (val)");
CL_DEFUN T_sp core__instance_set(T_sp obj, int idx, T_sp val) {
  if ( obj.generalp() ) {
    return obj.unsafe_general()->instanceSet(idx, val);
  }
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(obj idx);
CL_DECLARE();
CL_DOCSTRING("instanceRef - return the (idx) slot value of (obj)");
CL_DEFUN T_sp core__instance_ref(T_sp obj, int idx) {
  if (obj.generalp())return obj.unsafe_general()->instanceRef(idx);
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING("instancep");
CL_DEFUN T_sp core__instancep(T_sp obj) {
  if (obj.generalp()) return obj.unsafe_general()->oinstancep();
  IMPLEMENT_MEF(BF("Implement for non-general objects"));
};

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("isNil");
CL_DEFUN bool core__is_nil(T_sp arg) {
  return arg.nilp();
};


void General_O::initialize() {
  // do nothing
}

void General_O::initialize(core::List_sp alist) {
  Record_sp record = Record_O::create_initializer(alist);
  this->fields(record);
  record->errorIfInvalidArguments();
}

List_sp General_O::encode() {
  Record_sp record = Record_O::create_encoder();
  this->fields(record);
  return record->data();
}

void General_O::decode(core::List_sp alist) {
  Record_sp record = Record_O::create_decoder(alist);
  this->fields(record);
}

string General_O::className() const {
  // TODO: refactor this as ->__class()->classNameAsString
  // everywhere where we have obj->className()
  return this->__class()->classNameAsString();
}

void General_O::sxhash_(HashGenerator &hg) const {
  Fixnum res = (Fixnum)((((uintptr_t)this) >> gctools::tag_shift));
  hg.addPart(res);
}

T_sp General_O::deepCopy() const {
  SUBCLASS_MUST_IMPLEMENT();
}

bool General_O::eql_(T_sp obj) const {
  return this->eq(obj);
}

bool General_O::equal(T_sp obj) const {
  return this->eq(obj);
}

bool General_O::equalp(T_sp obj) const {
  return this->equal(obj);
}
#if 0
bool General_O::isAInstanceOf(Class_sp mc) {
  if (this->eq(mc))
    return true;
  Symbol_sp classSymbol = mc->className();
  if (this->isAssignableToByClassSymbol(classSymbol))
    return true;
  return false;
}
#endif

void HashGenerator::hashObject(T_sp obj) {
  clasp_sxhash(obj, *this);
}

static BignumExportBuffer static_HashGenerator_addPart_buffer;

bool HashGenerator::addPart(const mpz_class &bignum) {
  unsigned int *buffer = static_HashGenerator_addPart_buffer.getOrAllocate(bignum, 0);
  size_t count(0);
#ifdef DEBUG_HASH_GENERATOR
  if (this->_debug) {
    printf("%s:%d Adding hash bignum\n", __FILE__, __LINE__);
  }
#endif
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

CL_LAMBDA(arg);
CL_DECLARE();
CL_DOCSTRING("Return t if obj is equal to T_O::_class->unboundValue()");
CL_DEFUN bool core__sl_boundp(T_sp obj) {
  //    bool boundp = (obj.get() != T_O::___staticClass->unboundValue().get());
  bool boundp = !obj.unboundp();
#if DEBUG_CLOS >= 2
  printf("\nMLOG sl-boundp = %d address: %p\n", boundp, obj.get());
#endif
  return boundp;
};

void General_O::describe(T_sp stream) {
  clasp_write_string(this->__str__(), stream);
}

void General_O::__write__(T_sp strm) const {
  if (clasp_print_readably() && this->fieldsp()) {
    core__print_cxx_object(this->asSmartPtr(), strm);
  } else {
    clasp_write_string(this->__repr__(), strm);
  }
}



string General_O::descriptionOfContents() const {
  return "";
};

string General_O::description() const {
  _OF();
  stringstream ss;
  if (this == _lisp->_true().get()) {
    ss << "t";
  } else {
    General_O *me_gc_safe = const_cast<General_O *>(this);
    ss << "#<" << me_gc_safe->_instanceClass()->classNameAsString() << " ";

//    ss << "@" << std::hex << this << std::dec;
    ss << ")";
    ss << this->descriptionOfContents() << " > ";
  }
  return ss.str();
};

void General_O::initializeSlots(int slots) {
  SIMPLE_ERROR(BF("T_O::initializeSlots invoked - subclass must implement"));
};

T_sp General_O::instanceRef(int idx) const {
  SIMPLE_ERROR(BF("T_O::instanceRef(%d) invoked on object class[%s] val-->%s") % idx % this->_instanceClass()->classNameAsString() % this->__repr__());
}

T_sp General_O::instanceClassSet(Class_sp val) {
  SIMPLE_ERROR(BF("T_O::instanceClassSet to class %s invoked on object class[%s] val-->%s - subclass must implement") % _rep_(val) % this->_instanceClass()->classNameAsString() % _rep_(this->asSmartPtr()));
}

T_sp General_O::instanceSet(int idx, T_sp val) {
  SIMPLE_ERROR(BF("T_O::instanceSet(%d,%s) invoked on object class[%s] val-->%s") % idx % _rep_(val) % this->_instanceClass()->classNameAsString() % _rep_(this->asSmartPtr()));
}

T_sp General_O::instanceSig() const {
  SIMPLE_ERROR(BF("T_O::instanceSig() invoked on object class[%s] val-->%s") % this->_instanceClass()->classNameAsString() % this->__repr__());
}

T_sp General_O::instanceSigSet() {
  SIMPLE_ERROR(BF("T_O::instanceSigSet() invoked on object class[%s] val-->%s") % this->_instanceClass()->classNameAsString() % _rep_(this->asSmartPtr()));
}

CL_LAMBDA(obj);
CL_DECLARE();
CL_DOCSTRING("deepCopy");
CL_DEFUN T_sp core__deep_copy(T_sp obj) {
  if ( obj.generalp() ) return obj.unsafe_general()->deepCopy();
  SIMPLE_ERROR(BF("Cannot do deep copy of obj"));
}

Class_sp instance_class(T_sp obj)
{
  return cl__class_of(obj);
}

SYMBOL_SC_(CorePkg, slBoundp);
SYMBOL_SC_(CorePkg, isNil);
SYMBOL_SC_(CorePkg, instanceRef);
SYMBOL_SC_(CorePkg, instanceSet);
SYMBOL_SC_(CorePkg, instancep);
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
};
