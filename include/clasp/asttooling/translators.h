/*
    File: translators.h
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
#ifndef asttooling_translators_H
#define asttooling_translators_H

#include <clasp/core/foundation.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/vectorObjects.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/clbind/clbind.h>

#include <clang/Tooling/JSONCompilationDatabase.h>
#include <clang/AST/Stmt.h>
#include <clang/Frontend/ASTUnit.h>
#include <clang/AST/DeclBase.h>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclCXX.h>

namespace translate {

template <>
struct from_object<int &, std::true_type> {
  typedef int DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(0) {
    _G();
    if (core::Fixnum_sp fn = o.asOrNull<core::Fixnum_O>()) {
      this->_v = (int)(fn->get());
      return;
    }
    SIMPLE_ERROR(BF("Add support to convert other types to int"));
  }
};

template <>
struct to_object<int &> {
  static core::T_sp convert(const int &val) {
    return (core::Integer_O::create(val));
  }
};

#if 0
    template<>
    struct to_object<clang::CXXRecordDecl*>
    {
	static core::T_sp convert(clang::CXXRecordDecl* ptr)
	{
            IMPLEMENT_MEF(BF("Handle more complex wrappers"));
//	    return (clbind::Wrapper<clang::CXXRecordDecl>::create(ptr));
	}
    };

#endif

template <>
struct to_object<std::vector<std::string>, translate::adopt_pointer> {
  static core::T_sp convert(std::vector<std::string> strings) {
    core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(), strings.size(), cl::_sym_T_O);
    int i(0);
    for (auto ai = strings.begin(); ai != strings.end(); ai++) {
      vo->setf_elt(i++, core::lisp_createStr(*ai));
    }
    return vo;
  }
};

template <>
struct to_object<std::vector<std::string>, translate::dont_adopt_pointer> {
  static core::T_sp convert(std::vector<std::string> strings) {
    core::VectorObjects_sp vo = core::VectorObjects_O::create(_Nil<core::T_O>(), strings.size(), cl::_sym_T_O);
    int i(0);
    for (auto ai = strings.begin(); ai != strings.end(); ai++) {
      vo->setf_elt(i++, core::lisp_createStr(*ai));
    }
    return vo;
  }
};

template <>
struct from_object<const vector<string> &> {
  typedef vector<string> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    _G();
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::VectorObjects_sp vo = o.asOrNull<core::VectorObjects_O>()) {
      _v.resize(vo->length());
      for (int i(0), iEnd(vo->length()); i < iEnd; ++i) {
        _v[i] = vo->elt(i).as<core::Str_O>()->get();
      }
      return;
    } else if (core::Cons_sp lo = o.asOrNull<core::Cons_O>()) {
      _v.resize(lo->length());
      int i = 0;
      for (core::Cons_sp cur = lo; cur.notnilp(); cur = cCdr(cur)) {
        _v[i] = oCar(cur).as<core::Str_O>()->get();
        ++i;
      }
      return;
    }
    SIMPLE_ERROR(BF("Add support to convert other types to vector<string>"));
  }
};

// You will need the following from_object and to_object to wrap ClangTool::buildASTs
// You will also need to make clbind::Wrappers do the right thing with std::unique_ptrs
//
template <>
struct from_object<std::vector<std::unique_ptr<clang::ASTUnit>> &, std::false_type> {
  typedef std::vector<std::unique_ptr<clang::ASTUnit>> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    // Do nothing
  }
};

template <>
struct to_object<std::vector<std::unique_ptr<clang::ASTUnit>> &> {
  typedef std::vector<std::unique_ptr<clang::ASTUnit>> GivenType;
  static core::T_sp convert(std::vector<std::unique_ptr<clang::ASTUnit>> &vals) {
    core::VectorObjectsWithFillPtr_sp vo = core::VectorObjectsWithFillPtr_O::make(_Nil<core::T_O>(), _Nil<core::Cons_O>(), vals.size(), 0, true);
    for (int i(0), iEnd(vals.size()); i < iEnd; ++i) {
      vo->vectorPushExtend(clbind::Wrapper<clang::ASTUnit, std::unique_ptr<clang::ASTUnit>>::create(std::move(vals[i]), reg::registered_class<clang::ASTUnit>::id));
    }
    return vo;
  }
};

template <>
struct to_object<std::vector<clang::tooling::CompileCommand>> {
  typedef std::vector<clang::tooling::CompileCommand> GivenType;
  static core::T_sp convert(GivenType vals) {
    core::VectorObjectsWithFillPtr_sp vo = core::VectorObjectsWithFillPtr_O::make(_Nil<core::T_O>(), _Nil<core::Cons_O>(), vals.size(), 0, true);
    for (int i(0), iEnd(vals.size()); i < iEnd; ++i) {
      vo->vectorPushExtend(clbind::Wrapper<clang::tooling::CompileCommand, std::unique_ptr<clang::tooling::CompileCommand>>::create(vals[i], reg::registered_class<clang::tooling::CompileCommand>::id));
    }
    return vo;
  }
};
};

#endif
