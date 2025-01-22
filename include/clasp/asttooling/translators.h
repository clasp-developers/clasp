#pragma once

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

#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/clbind/clbind.h>
#include <clang/Tooling/JSONCompilationDatabase.h>
#include <clang/AST/Stmt.h>
#include <clang/Frontend/ASTUnit.h>
#include <clang/AST/DeclBase.h>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclCXX.h>

#include <clang/Tooling/Refactoring.h>

namespace translate {

template <> struct to_object<std::vector<std::string>, translate::adopt_pointer> {
  static core::T_sp convert(std::vector<std::string> strings) {
    core::ComplexVector_T_sp vo = core::ComplexVector_T_O::make(strings.size(), nil<core::T_O>());
    int i(0);
    for (auto ai = strings.begin(); ai != strings.end(); ai++) {
      vo->rowMajorAset(i++, core::lisp_createStr(*ai));
    }
    return vo;
  }
};

template <> struct to_object<std::vector<std::string>, translate::dont_adopt_pointer> {
  static core::T_sp convert(std::vector<std::string> strings) {
    core::ComplexVector_T_sp vo = core::ComplexVector_T_O::make(strings.size(), nil<core::T_O>());
    int i(0);
    for (auto ai = strings.begin(); ai != strings.end(); ai++) {
      vo->rowMajorAset(i++, core::lisp_createStr(*ai));
    }
    return vo;
  }
};

template <> struct from_object<const vector<string>&> {
  typedef vector<string> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::cl__vectorp(o)) {
      core::Vector_sp vo = gc::As_unsafe<core::Vector_sp>(o);
      _v.resize(vo->length());
      for (int i(0), iEnd(vo->length()); i < iEnd; ++i) {
        _v[i] = gc::As<core::String_sp>(vo->rowMajorAref(i))->get_std_string();
      }
      return;
    } else if (o.consp()) {
      core::Cons_sp co = gc::As_unsafe<core::Cons_sp>(o);
      _v.resize(co->length());
      int i = 0;
      for (auto cur : (core::List_sp)co) {
        _v[i] = gc::As<core::String_sp>(oCar(cur))->get_std_string();
        ++i;
      }
      return;
    }
    SIMPLE_ERROR("Add support to convert {} to vector<string>", _rep_(o));
  }
};

template <> struct to_object<std::vector<std::unique_ptr<clang::ASTUnit>>&> {
  typedef std::vector<std::unique_ptr<clang::ASTUnit>> GivenType;
  static core::T_sp convert(std::vector<std::unique_ptr<clang::ASTUnit>>& vals) {
    core::ComplexVector_T_sp vo = core::ComplexVector_T_O::make(vals.size(), nil<core::T_O>(), core::clasp_make_fixnum(0));
    for (int i(0), iEnd(vals.size()); i < iEnd; ++i) {
      vo->vectorPushExtend(clbind::Wrapper<clang::ASTUnit, std::unique_ptr<clang::ASTUnit>>::make_wrapper(
          std::move(vals[i]), reg::registered_class<clang::ASTUnit>::id));
    }
    return vo;
  }
};

template <> struct to_object<std::vector<clang::tooling::CompileCommand>> {
  typedef std::vector<clang::tooling::CompileCommand> GivenType;
  static core::T_sp convert(GivenType vals) {
    core::ComplexVector_T_sp vo = core::ComplexVector_T_O::make(vals.size(), nil<core::T_O>(), core::clasp_make_fixnum(0));
    for (int i(0), iEnd(vals.size()); i < iEnd; ++i) {
      vo->vectorPushExtend(
          clbind::Wrapper<clang::tooling::CompileCommand, std::unique_ptr<clang::tooling::CompileCommand>>::make_wrapper(
              vals[i], reg::registered_class<clang::tooling::CompileCommand>::id));
    }
    return vo;
  }
};

template <> struct to_object<std::map<std::string, clang::tooling::Replacements>&, translate::dont_adopt_pointer> {
  typedef std::map<std::string, clang::tooling::Replacements> GivenType;
  static core::T_sp convert(GivenType vals) {
    core::HashTable_sp result = core::HashTable_O::createEqual();
    for (auto pair : vals) {
      core::SimpleBaseString_sp str = core::SimpleBaseString_O::make(pair.first);
      core::T_sp obj = to_object<const clang::tooling::Replacements&>::convert(pair.second);
      result->setf_gethash(str, obj);
    }
    return result;
  }
};

template <> struct from_object<clang::QualType> {
  typedef clang::QualType DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(gc::As<asttooling::QualType_sp>(o)->_Value._value) {
    //     printf("%s:%d:%s value -> %p\n", __FILE__, __LINE__, __FUNCTION__, this->_v.getAsOpaquePtr());
  }
};

template <> struct to_object<clang::QualType, translate::dont_adopt_pointer> {
  typedef clang::QualType HolderType;
  typedef clbind::Wrapper<clang::QualType, HolderType> WrapperType;
  static core::T_sp convert(clang::QualType val) {
    auto qtval = gctools::GC<asttooling::QualType_O>::allocate(val);
    //    printf("%s:%d:%s clang::QualType size -> %lu  value -> %p\n", __FILE__, __LINE__, __FUNCTION__, sizeof(val),
    //    val.getAsOpaquePtr() );
    return qtval;
  }
};

template <> struct to_object<clang::QualType, translate::adopt_pointer> {
  typedef clang::QualType HolderType;
  typedef clbind::Wrapper<clang::QualType, HolderType> WrapperType;
  static core::T_sp convert(clang::QualType val) {
    auto qtval = gctools::GC<asttooling::QualType_O>::allocate(val);
    printf("%s:%d:%s clang::QualType size -> %lu  value -> %p\n", __FILE__, __LINE__, __FUNCTION__, sizeof(val),
           val.getAsOpaquePtr());
    return qtval;
  }
};

template <> struct from_object<clang::PresumedLoc> {
  typedef clang::PresumedLoc DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(gc::As<asttooling::PresumedLoc_sp>(o)->_Value._value) {
    //     printf("%s:%d:%s value -> %p\n", __FILE__, __LINE__, __FUNCTION__, this->_v.getAsOpaquePtr());
  }
};

template <> struct from_object<const clang::PresumedLoc&> {
  typedef clang::PresumedLoc DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(gc::As<asttooling::PresumedLoc_sp>(o)->_Value._value) {
    //     printf("%s:%d:%s value -> %p\n", __FILE__, __LINE__, __FUNCTION__, this->_v.getAsOpaquePtr());
  }
};

template <> struct to_object<clang::PresumedLoc, translate::dont_adopt_pointer> {
  typedef clang::PresumedLoc HolderType;
  typedef clbind::Wrapper<clang::PresumedLoc, HolderType> WrapperType;
  static core::T_sp convert(clang::PresumedLoc val) {
    auto ploc = gctools::GC<asttooling::PresumedLoc_O>::allocate(val);
    //    printf("%s:%d:%s clang::PresumedLoc size -> %lu  value -> %p\n", __FILE__, __LINE__, __FUNCTION__, sizeof(val),
    //    val.getAsOpaquePtr() );
    return ploc;
  }
};

template <> struct to_object<clang::PresumedLoc, translate::adopt_pointer> {
  typedef clang::PresumedLoc HolderType;
  typedef clbind::Wrapper<clang::PresumedLoc, HolderType> WrapperType;
  static core::T_sp convert(clang::PresumedLoc val) {
    auto ploc = gctools::GC<asttooling::PresumedLoc_O>::allocate(val);
    //    printf("%s:%d:%s clang::PresumedLoc size -> %lu  value -> %p\n", __FILE__, __LINE__, __FUNCTION__, sizeof(val),
    //    val.getAsOpaquePtr() );
    return ploc;
  }
};

template <> struct from_object<clang::SourceLocation> {
  typedef clang::SourceLocation DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(gc::As<asttooling::SourceLocation_sp>(o)->_Value._value) {
    //     printf("%s:%d:%s value -> %p\n", __FILE__, __LINE__, __FUNCTION__, this->_v.getAsOpaquePtr());
  }
};

template <> struct from_object<const clang::SourceLocation&> {
  typedef clang::SourceLocation DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(gc::As<asttooling::SourceLocation_sp>(o)->_Value._value) {
    //     printf("%s:%d:%s value -> %p\n", __FILE__, __LINE__, __FUNCTION__, this->_v.getAsOpaquePtr());
  }
};

template <> struct to_object<clang::SourceLocation, translate::dont_adopt_pointer> {
  typedef clang::SourceLocation HolderType;
  typedef clbind::Wrapper<clang::SourceLocation, HolderType> WrapperType;
  static core::T_sp convert(clang::SourceLocation val) {
    auto ploc = gctools::GC<asttooling::SourceLocation_O>::allocate(val);
    //    printf("%s:%d:%s clang::SourceLocation size -> %lu  value -> %p\n", __FILE__, __LINE__, __FUNCTION__, sizeof(val),
    //    val.getAsOpaquePtr() );
    return ploc;
  }
};

template <> struct to_object<clang::SourceLocation, translate::adopt_pointer> {
  typedef clang::SourceLocation HolderType;
  typedef clbind::Wrapper<clang::SourceLocation, HolderType> WrapperType;
  static core::T_sp convert(clang::SourceLocation val) {
    auto ploc = gctools::GC<asttooling::SourceLocation_O>::allocate(val);
    //    printf("%s:%d:%s clang::SourceLocation size -> %lu  value -> %p\n", __FILE__, __LINE__, __FUNCTION__, sizeof(val),
    //    val.getAsOpaquePtr() );
    return ploc;
  }
};

}; // namespace translate
