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
#define DEBUG_LEVEL_FULL

#include <clasp/core/common.h>
#include <clasp/core/environment.h>
#include <clasp/core/ql.h>
#include <clasp/core/designators.h>
#include <clasp/core/package.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/character.h>
#include <clasp/core/str.h>
#include <clasp/core/wrappers.h>
namespace core {

namespace coerce {

Function_sp functionDesignator(T_sp obj) {
  if (Function_sp fnobj = obj.asOrNull<Function_O>()) {
    return fnobj;
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    if (!sym->fboundp())
      SIMPLE_ERROR(BF("Function value for %s is unbound") % _rep_(sym));
    return sym->symbolFunction();
  }
  SIMPLE_ERROR(BF("Illegal function designator %s") % _rep_(obj));
}

Path_sp pathDesignator(T_sp obj) {
  _G();
  if (af_strP(obj)) {
    return Path_O::create(gc::As<Str_sp>(obj)->get());
  } else if (gc::IsA<Path_sp>(obj)) {
    return gc::As<Path_sp>(obj);
  }
  SIMPLE_ERROR(BF("Illegal path designator[%s]") % _rep_(obj));
}

Package_sp packageDesignator(T_sp obj) {
  // TODO: Add support for Unicode package names
  Str_sp packageName;
  if (Package_sp apkg = obj.asOrNull<Package_O>()) {
    return apkg;
  } else if (Str_sp str = obj.asOrNull<Str_O>()) {
    packageName = str;
    goto PACKAGE_NAME;
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    packageName = af_symbolName(sym);
    goto PACKAGE_NAME;
  } else if (Character_sp chr = obj.asOrNull<Character_O>()) {
    stringstream ss;
    ss << clasp_as_char(chr);
    packageName = Str_O::create(ss.str());
    goto PACKAGE_NAME;
  }
  TYPE_ERROR(obj, Cons_O::createList(cl::_sym_or, cl::_sym_String_O, cl::_sym_Symbol_O, cl::_sym_character));
PACKAGE_NAME:
  Package_sp pkg = gc::As<Package_sp>(_lisp->findPackage(packageName->get(), true));
  return pkg;
}

string packageNameDesignator(T_sp obj) {
  _G();
  if (cl_packagep(obj)) {
    return gc::As<Package_sp>(obj)->getName();
  }
  Str_sp packageName = stringDesignator(obj);
  return packageName->get();
}

List_sp listOfPackageDesignators(T_sp obj) {
  _G();
  if (obj.nilp())
    return _Nil<T_O>();
  if (obj.consp()) {
    ql::list res(_lisp);
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
  _G();
  if (syms.nilp())
    return _Nil<List_V>();
  List_sp symbols;
  if (cl_symbolp(syms)) {
    symbols = Cons_O::create(syms);
  } else {
    symbols = syms;
  }
  return symbols;
}

Str_sp stringDesignator(T_sp obj) {
  if (Str_sp str = obj.asOrNull<Str_O>()) {
    return str;
  } else if (Symbol_sp sym = obj.asOrNull<Symbol_O>()) {
    return af_symbolName(sym);
  } else if (Character_sp chr = obj.asOrNull<Character_O>()) {
    stringstream ss;
    ss << clasp_as_char(chr);
    return Str_O::create(ss.str());
  }
  TYPE_ERROR(obj, Cons_O::createList(cl::_sym_or, cl::_sym_String_O, cl::_sym_Symbol_O, cl::_sym_character));
}

List_sp listOfStringDesignators(T_sp obj) {
  _G();
  if (obj.nilp())
    return _Nil<List_V>();
  if (Cons_sp cobj = obj.asOrNull<Cons_O>()) {
    List_sp lobj = cobj;
    Cons_sp first = Cons_O::create(_Nil<T_O>());
    Cons_sp cur = first;
    for (auto ic : lobj) {
      Cons_sp one = Cons_O::create(stringDesignator(oCar(ic)));
      cur->setCdr(one);
      cur = one;
    }
    return oCdr(first);
  } else {
    return Cons_O::create(stringDesignator(obj));
  }
  SIMPLE_ERROR(BF("Illegal list of string designators[%s]") % _rep_(obj));
}

T_sp inputStreamDesignator(T_sp obj) {
  _G();
  if (obj.nilp()) {
    return cl::_sym_STARstandard_inputSTAR->symbolValue();
  } else if (obj == _lisp->_true()) {
    return cl::_sym_STARterminal_ioSTAR->symbolValue();
  } else if (cl_streamp(obj)) {
    return obj;
  }
  SIMPLE_ERROR(BF("Cannot convert object[%s] into a Stream") % _rep_(obj));
}

T_sp outputStreamDesignator(T_sp obj) {
  _G();
  if (obj.nilp()) {
    return cl::_sym_STARstandard_outputSTAR->symbolValue();
  } else if (obj == _lisp->_true()) {
    return cl::_sym_STARterminal_ioSTAR->symbolValue();
  } else if (cl_streamp(obj)) {
    return obj;
  }
  SIMPLE_ERROR(BF("Cannot convert object[%s] into a Stream") % _rep_(obj));
}

}; /* desig */

void initialize_designators() {
  _G();
  af_def(CorePkg, "pathDesignator", &coerce::pathDesignator);
  af_def(CorePkg, "coerce-to-package", &coerce::packageDesignator);
}
}; /* core */
