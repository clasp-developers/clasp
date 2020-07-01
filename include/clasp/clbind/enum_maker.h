/*
    File: enum_maker.h
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
// Copyright (c) 2003 Daniel Wallin and Arvid Norberg

// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
// ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef CLBIND_ENUM_MAKER_HPP_INCLUDED
#define CLBIND_ENUM_MAKER_HPP_INCLUDED

#include <vector>
#include <string>

#include <clasp/clbind/config.h>
#include <clasp/clbind/class_rep.h>

#include <clasp/core/symbolToEnumConverter.h>

namespace clbind {
struct enum_base {
  explicit enum_base(scope_& from, core::Symbol_sp converterSym) : _from(&from) {
    core::SymbolToEnumConverter_sp converter = core::SymbolToEnumConverter_O::create(converterSym->symbolNameAsString());
    converterSym->defparameter(converter);
    this->m_converterSymbolName = converterSym->symbolNameAsString();
    this->m_converterPackageName = core::lisp_packageName(converterSym->getPackage());
    LOG_SCOPE(("%s:%d enum_base @%p  scope %p  symbol: %s  name: %s  package: %s\n", __FILE__, __LINE__, this, &from, _rep_(converterSym).c_str(), this->m_converterSymbolName.c_str(), this->m_converterPackageName.c_str() ));
  }
  scope_ *_from;
  std::string m_converterSymbolName;
  std::string m_converterPackageName;
};

namespace detail {
template <class ValueType>
struct enum_value_registration : registration {
  enum_value_registration(const std::string& converterSymbolName, const std::string& converterPackageName, const std::string &name, ValueType val ) 
    : m_converterSymbolName(converterSymbolName), m_converterPackageName(converterPackageName), m_name(name), m_value(val) {};

  void register_() const {
    LOG_SCOPE(("%s:%d register_ enum_value_registration converter: %s::%s %s/%s\n", __FILE__, __LINE__,
               this->m_converterSymbolName.c_str(),
               this->m_converterPackageName.c_str(),
               this->kind().c_str(), this->name().c_str()));
    LOG_SCOPE(("    %s:%d  this@%p this->m_converterSymbolName: %s   this->m_converterPackageName: %s\n", __FILE__, __LINE__,
               this, this->m_converterSymbolName.c_str(), this->m_converterPackageName.c_str() ));
    core::Package_sp pkg = gc::As<core::Package_sp>(_lisp->findPackage(this->m_converterPackageName));
    core::Symbol_sp sym = _lisp->intern(this->m_converterSymbolName,pkg);
    core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(sym->symbolValue());
    core::Symbol_sp nameSym = core::lispify_intern(this->m_name, core::lisp_currentPackageName(), true);
    core::lisp_extendSymbolToEnumConverter(converter, nameSym, nameSym, this->m_value);
  }

  virtual std::string name() const { return this->m_name; };
  virtual std::string kind() const { return "enum_value_registration"; };

  std::string m_converterSymbolName;
  std::string m_converterPackageName;
  std::string m_name;
  ValueType m_value;
};
}

template <typename EnumType>
struct enum_ : public enum_base {
  explicit enum_(scope_& from, core::Symbol_sp converterSym) : enum_base(from,converterSym) {};

  template <class ValueType>
  enum_& value(const char* name, ValueType val) {
    std::unique_ptr<detail::registration> ptr(new detail::enum_value_registration<ValueType>(this->m_converterSymbolName,this->m_converterPackageName,name,val));
    this->_from->operator,(scope_(std::move(ptr)));
    return *this;
  }

private:
  //            void operator=(enum_ const&); // C4512, assignment operator could not be generated
  template <class T>
  void operator, (T const &) const;
};
}

#define CLBIND_TRANSLATE_SYMBOL_TO_ENUM(_ENUM_TYPE_, _SYM_)                                                      \
  namespace translate {                                                                                          \
  template <> struct from_object<_ENUM_TYPE_> {                                                                  \
    typedef _ENUM_TYPE_ DeclareType;                                                                             \
    DeclareType _v;                                                                                              \
    from_object(T_P object) {                                                                                    \
      _G();                                                                                                      \
      if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {                                             \
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(_SYM_->symbolValue()); \
        this->_v = converter->enumForSymbol<_ENUM_TYPE_>(sym);                                                   \
        return;                                                                                                  \
      }                                                                                                          \
      SIMPLE_ERROR_SPRINTF("Cannot convert object %s to " #_ENUM_TYPE_, _rep_(object).c_str()); \
    }                                                                                                            \
  };                                                                                                             \
  template <> struct to_object<_ENUM_TYPE_> {                                                                    \
    static core::T_sp convert(_ENUM_TYPE_ val) {                                                                 \
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(_SYM_->symbolValue());   \
      return converter->symbolForEnum<_ENUM_TYPE_>(val);                                                         \
    };                                                                                                           \
  };                                                                                                             \
  };

#endif // CLBIND_ENUM_MAKER_HPP_INCLUDED
