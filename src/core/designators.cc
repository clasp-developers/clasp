/*
    File: designators.cc
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
#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/ql.h>
#include <clasp/core/designators.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/character.h>
#include <clasp/core/array.h>
#include <clasp/core/wrappers.h>
namespace core {

namespace coerce {

Function_sp functionDesignator(T_sp obj) {
  if (Function_sp fnobj = obj.asOrNull<Function_O>()) {
    return fnobj;
  } else if (obj.nilp()) {
    ERROR_UNDEFINED_FUNCTION(obj);
  } else if (obj.unboundp()) {
    ERROR_UNDEFINED_FUNCTION(obj);
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    if (!sym->fboundp())
      SIMPLE_ERROR(BF("Function value for %s is unbound") % _rep_(sym));
    return sym->symbolFunction();
  }
  SIMPLE_ERROR(BF("Illegal function designator %s") % _rep_(obj));
}

Closure_sp closureDesignator(T_sp obj) {
  if (Closure_sp fnobj = obj.asOrNull<Closure_O>()) {
    return fnobj;
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    if (!sym->fboundp())
      SIMPLE_ERROR(BF("Function value for %s is unbound") % _rep_(sym));
    Closure_sp closure = sym->symbolFunction().asOrNull<Closure_O>();
    ASSERT(closure);
    return closure;
  }
  SIMPLE_ERROR(BF("Illegal function designator %s") % _rep_(obj));
}

#if 0
core::Path_sp pathDesignator(core::T_sp obj) {
  if (cl__string_p(obj)) {
    return Path_O::create(gc::As<String_sp>(obj)->get_std_string());
  } else if ( Pathname_sp pn = obj.asOrNull<Pathname_O>() ) {
    String_sp spn = cl__namestring(pn);
    return Path_O::create(spn->get());
  }else if (gc::IsA<Path_sp>(obj)) {
    return gc::As<Path_sp>(obj);
  }
  SIMPLE_ERROR(BF("Illegal path designator[%s]") % _rep_(obj));
}
#endif
};
};

#if 0
namespace core {
CL_PKG_NAME(CorePkg,path-designator);
CL_DEFUN core::Path_sp core__path_designator(core::T_sp obj) {
  return coerce::pathDesignator(obj);
}
};
#endif


namespace core {
namespace coerce {
core::Package_sp packageDesignator(core::T_sp obj) {
  // TODO: Add support for Unicode package names
  String_sp packageName;
  if (Package_sp apkg = obj.asOrNull<Package_O>()) {
    return apkg;
  } else if (cl__stringp(obj)) {
    packageName = gc::As_unsafe<String_sp>(obj);
    goto PACKAGE_NAME;
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    packageName = cl__symbol_name(sym);
    goto PACKAGE_NAME;
  } else if (Character_sp chr = obj.asOrNull<Character_O>()) {
    if (clasp_base_char_p(chr))
      packageName = SimpleBaseString_O::make(1,chr.unsafe_character());
    else
      packageName = SimpleCharacterString_O::make(1,chr.unsafe_character());
    goto PACKAGE_NAME;
  }
  TYPE_ERROR(obj, Cons_O::createList(cl::_sym_or, cl::_sym_string, cl::_sym_Symbol_O, cl::_sym_character));
 PACKAGE_NAME:
  T_sp tpkg = _lisp->findPackage(packageName->get(),false);
  if (tpkg.notnilp()) {
    Package_sp pkg = gc::As<Package_sp>(tpkg);
    return pkg;
  }
  PACKAGE_ERROR(packageName);
}
};
};

namespace core {
CL_PKG_NAME(CorePkg,coerce-to-package);
CL_DEFUN core::Package_sp coerce_to_package(core::T_sp obj) {
  return coerce::packageDesignator(obj);
}
};

namespace core {
namespace coerce {
string packageNameDesignator(T_sp obj) {
  if (cl__packagep(obj)) {
    return gc::As<Package_sp>(obj)->getName();
  }
  String_sp packageName = stringDesignator(obj);
  return packageName->get_std_string();
}

List_sp listOfPackageDesignators(T_sp obj) {
  if (obj.nilp())
    return _Nil<T_O>();
  if (obj.consp()) {
    ql::list res;
    List_sp lobj = obj;
    for (auto cur : lobj) {
      Package_sp pkg = packageDesignator(oCar(cur));
      res << pkg;
    }
    return res.cons();
  }
  Package_sp onePackage = packageDesignator(obj);
  return Cons_O::create(onePackage);
}

List_sp listOfSymbols(T_sp syms) {
  if (syms.nilp())
    return _Nil<List_V>();
  List_sp symbols;
  if (cl__symbolp(syms)) {
    symbols = Cons_O::create(syms);
  } else {
    symbols = syms;
  }
  return symbols;
}

SimpleString_sp simple_string(T_sp obj) {
  if (cl__stringp(obj)) {
    if (cl__simple_string_p(obj)) {
      return gc::As_unsafe<SimpleString_sp>(obj);
    } else if (gc::IsA<Str8Ns_sp>(obj)) {
      Str8Ns_sp s8 = gc::As_unsafe<Str8Ns_sp>(obj);
      AbstractSimpleVector_sp base;
      size_t start, end;
      s8->asAbstractSimpleVectorRange(base,start,end);
      return gc::As_unsafe<SimpleString_sp>(base->unsafe_subseq(start,end));
    }
    StrWNs_sp sw = gc::As_unsafe<StrWNs_sp>(obj);
    AbstractSimpleVector_sp base;
    size_t start, end;
    sw->asAbstractSimpleVectorRange(base,start,end);
    return gc::As_unsafe<SimpleString_sp>(base->unsafe_subseq(start,end));
    SIMPLE_ERROR(BF("This should never happen - the string %s was not recognized as a concrete string type") % _rep_(obj));
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    return cl__symbol_name(sym);
  } else if (Character_sp chr = obj.asOrNull<Character_O>()) {
    if (clasp_base_char_p(chr)) {
      return SimpleBaseString_O::make(1,chr.unsafe_character());
    }
    return SimpleCharacterString_O::make(1,chr.unsafe_character());
  }
  TYPE_ERROR(obj, Cons_O::createList(cl::_sym_or, cl::_sym_string, cl::_sym_Symbol_O, cl::_sym_character));
}

String_sp stringDesignator(T_sp obj) {
  if (cl__stringp(obj)) {
    return gc::As_unsafe<String_sp>(obj);
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    return cl__symbol_name(sym);
  } else if (Character_sp chr = obj.asOrNull<Character_O>()) {
    if (clasp_base_char_p(chr)) {
      return SimpleBaseString_O::make(1,chr.unsafe_character());
    }
    return SimpleCharacterString_O::make(1,chr.unsafe_character());
  }
  TYPE_ERROR(obj, Cons_O::createList(cl::_sym_or, cl::_sym_string, cl::_sym_Symbol_O, cl::_sym_character));
}

List_sp listOfStringDesignators(T_sp obj) {
  if (obj.consp()) {
    List_sp lobj = gc::As_unsafe<Cons_sp>(obj);
    Cons_sp first = Cons_O::create(_Nil<T_O>());
    Cons_sp cur = first;
    for (auto ic : lobj) {
      Cons_sp one = Cons_O::create(stringDesignator(oCar(ic)));
      cur->setCdr(one);
      cur = one;
    }
    return oCdr(first);
  } else if (obj.nilp()) {
    return _Nil<List_V>();
  } else {
    return Cons_O::create(stringDesignator(obj));
  }
  SIMPLE_ERROR(BF("Illegal list of string designators[%s]") % _rep_(obj));
}

T_sp inputStreamDesignator(T_sp obj) {
  if (obj.nilp()) {
    return cl::_sym_STARstandard_inputSTAR->symbolValue();
  } else if (obj == _lisp->_true()) {
    return cl::_sym_STARterminal_ioSTAR->symbolValue();
  } else if (cl__streamp(obj)) {
    return obj;
  }
  // kpoeck
  // SIMPLE_ERROR(BF("Cannot convert object[%s] into a Stream") % _rep_(obj));
  TYPE_ERROR(obj, cl::_sym_streamError);
}

T_sp outputStreamDesignator(T_sp obj) {
  if (obj.nilp()) {
    return cl::_sym_STARstandard_outputSTAR->symbolValue();
  } else if (obj == _lisp->_true()) {
    return cl::_sym_STARterminal_ioSTAR->symbolValue();
  } else if (cl__streamp(obj)) {
    return obj;
  }
  // kpoeck
  // SIMPLE_ERROR(BF("Cannot convert object[%s] into a Stream") % _rep_(obj));
  TYPE_ERROR(obj, cl::_sym_streamError);
}


T_sp coerce_to_base_string(T_sp str) {
  if (gc::IsA<SimpleBaseString_sp>(str)) {
    return str;
  } else if (gc::IsA<Str8Ns_sp>(str)) {
    return str;
  } else if (gc::IsA<SimpleCharacterString_sp>(str)) {
    return core__copy_to_simple_base_string(str);
  } else if (gc::IsA<StrWNs_sp>(str)) {
    return core__copy_to_simple_base_string(str);
  }
  SIMPLE_ERROR(BF("Cannot coerce %s to base-string") % _rep_(str));
}




}; /* coerce */






}; /* core */
