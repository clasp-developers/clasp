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
#ifndef _llvmo_translators_H
#define _llvmo_translators_H

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/array.h>
#include <clasp/llvmo/llvmoExpose.fwd.h>

namespace translate {

template <>
struct from_object<llvm::StringRef, std::true_type> {
  typedef llvm::StringRef DeclareType;
  string _Storage; // Store the string here so it won't get wiped out before its used by the callee
  DeclareType _v;
  from_object(T_P o) : _Storage(gc::As<core::String_sp>(o)->get_std_string()),
    _v(llvm::StringRef(this->_Storage)) {};
};

template <>
struct from_object<const llvm::Twine &> {
  typedef llvm::Twine DeclareType;
  string _Storage; // Store the string here so it won't get wiped out before its used by the callee
  DeclareType _v;
  from_object(T_P o) : _Storage(gc::As<core::String_sp>(o)->get_std_string()),
    _v(llvm::Twine(this->_Storage)) {};
};

template <>
struct to_object<const llvm::StringRef &> {
  static core::T_sp convert(llvm::StringRef const &sr) {
    _G();
    return (Values(core::SimpleBaseString_O::make(sr.data())));
  }
};

template <>
struct to_object<llvm::StringRef &> {
  static core::T_sp convert(llvm::StringRef &sr) { return core::SimpleBaseString_O::make(sr.data()); }
};

template <>
struct to_object<llvm::StringRef> {
  static core::T_sp convert(const llvm::StringRef &sr) { return core::SimpleBaseString_O::make(sr.data()); }
};

 template <>
   struct from_object<llvm::DINode::DIFlags> {
   typedef llvm::DINode::DIFlags DeclareType;
   DeclareType _v;
   from_object(core::T_sp o) {
     if (o.fixnump()) {
       llvm::DINode::DIFlags f = static_cast<llvm::DINode::DIFlags>(o.unsafe_fixnum());
       this->_v = f;
       return;
     }
     SIMPLE_ERROR_SPRINTF("Only fixnums can be converted to llvm::DINode::DIFlags");
   }
 };

 template <>
 struct from_object<llvm::DISubprogram::DISPFlags> {
   typedef llvm::DISubprogram::DISPFlags DeclareType;
   DeclareType _v;
   from_object(core::T_sp o) {
     if (o.fixnump()) {
       llvm::DISubprogram::DISPFlags f = static_cast<llvm::DISubprogram::DISPFlags>(o.unsafe_fixnum());
       this->_v = f;
       return;
     }
     SIMPLE_ERROR_SPRINTF("Only fixnums can be converted to llvm::DISubprogram::DISPFlags");
   }
 };

  template <>
   struct from_object<llvm::DITemplateParameterArray> {
   typedef llvm::DITemplateParameterArray DeclareType;
   DeclareType _v;
   from_object(core::T_sp o) {
     if (o.nilp()) {
       this->_v = nullptr;
       return;
     }
     SIMPLE_ERROR_SPRINTF("Only NIL is supported for DITemplateParameterArray at this point");
   }
 };

    template <>
   struct from_object<llvm::DITypeArray> {
   typedef llvm::DITypeArray DeclareType;
   DeclareType _v;
   from_object(core::T_sp o) {
     if (o.nilp()) {
       this->_v = nullptr;
       return;
     }
     SIMPLE_ERROR_SPRINTF("Only NIL is supported for DITypeArray at this point");
   }
 };

template <>
struct from_object<llvm::DICompileUnit::DebugEmissionKind> {
  typedef llvm::DICompileUnit::DebugEmissionKind DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (core::Symbol_sp sym = o.asOrNull<core::Symbol_O>()) {
      if (sym == kw::_sym_FullDebug) {
        this->_v = llvm::DICompileUnit::FullDebug;
        return;
      } else if (sym == kw::_sym_LineTablesOnly) {
        this->_v = llvm::DICompileUnit::LineTablesOnly;
        return;
      }
    }
    SIMPLE_ERROR_SPRINTF("You must pass :full-debug or :line-tables-only, only those are valid DebugEmissionKind");
  }
};
 
template <>
struct from_object<llvm::ArrayRef<std::string>> {
  typedef std::vector<std::string> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcstrs = o.asOrNull<core::Cons_O>()) {
      for (auto cstrs : lcstrs) {
        core::String_sp s = gc::As<core::String_sp>(core::oCar(cstrs));
        _v.push_back(s->get_std_string());
      }
      return;
    } else if (core::Vector_sp vstrs = o.asOrNull<core::Vector_O>()) {
      _v.resize(vstrs->length());
      for (int i(0), iEnd(vstrs->length()); i < iEnd; ++i) {
        _v[i] = gc::As<core::String_sp>(vstrs->rowMajorAref(i))->get_std_string();
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to llvm::ArrayRef<std::string>", core::_rep_(o).c_str());
  }
};

 
template <>
struct from_object<llvm::ArrayRef<int>> {
  typedef std::vector<int> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcstrs = o.asOrNull<core::Cons_O>()) {
      for (auto cstrs : lcstrs) {
        core::Fixnum_sp s = gc::As<core::Fixnum_sp>(core::oCar(cstrs));
        _v.push_back(s.unsafe_fixnum());
      }
      return;
    } else if (core::Vector_sp vstrs = o.asOrNull<core::Vector_O>()) {
      _v.resize(vstrs->length());
      for (int i(0), iEnd(vstrs->length()); i < iEnd; ++i) {
        _v[i] = gc::As<core::Fixnum_sp>(vstrs->rowMajorAref(i)).unsafe_fixnum();
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to llvm::ArrayRef<int>", core::_rep_(o).c_str());
  }
};


 template <>
   struct from_object<std::function<bool (const llvm::Function& )> > {
   // createCFGSimplificationPass takes a function object like this as an argument
   // as of llvm3.7 - for now wrap a lambda
  typedef std::function<bool (const llvm::Function& )> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if ( core::Function_sp func = gc::As<core::Function_sp>(o) ) {
        printf("%s:%d  translate::from_object<std::function<bool (const llvm::Function&)> passing dummy for now \n", __FILE__, __LINE__ );
        _v = std::function<bool (const llvm::Function&)> ([&func] (const llvm::Function& f)->bool {
          return true;
      }
        );
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to std::function<bool (const llvm::Function&)", core::_rep_(o).c_str());
  }
};
};

#endif
