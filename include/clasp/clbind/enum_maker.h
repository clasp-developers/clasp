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
struct value;

struct value_vector : public std::vector<value> {
  // a bug in intel's compiler forces us to declare these constructors explicitly.
  value_vector();
  virtual ~value_vector();
  value_vector(const value_vector &v);
  value_vector &operator, (const value &rhs);
};

struct value {
  friend class std::vector<value>;
  template <class T>
  value(const char *name, T v)
      : name_(name), val_(v) {}

  const char *name_;
  int val_;

  value_vector operator, (const value &rhs) const {
    value_vector v;

    v.push_back(*this);
    v.push_back(rhs);

    return v;
  }

private:
  value() {}
};

inline value_vector::value_vector()
    : std::vector<value>() {
}

inline value_vector::~value_vector() {}

inline value_vector::value_vector(const value_vector &rhs)
    : std::vector<value>(rhs) {
}

inline value_vector &value_vector::operator, (const value &rhs) {
  push_back(rhs);
  return *this;
}

namespace detail {
template <class From, class EnumType>
struct enum_maker {
  explicit enum_maker(From &from, core::Symbol_sp converterSym) : _from(from), _converterSym(converterSym) {}
  From &operator[](const value &val) {
    core::SymbolToEnumConverter_sp converter = core::SymbolToEnumConverter_O::create(this->_converterSym->symbolName());
    this->_converterSym->defparameter(converter);
    core::Symbol_sp nameSym = core::lispify_intern(val.name_, core::lisp_currentPackageName(), true);
    core::lisp_extendSymbolToEnumConverter(converter, nameSym, nameSym, val.val_);
    return _from;
  }

  From &operator[](const value_vector &values) {
    core::SymbolToEnumConverter_sp converter = core::SymbolToEnumConverter_O::create(this->_converterSym->symbolName()->get());
    this->_converterSym->defparameter(converter);
    for (value_vector::const_iterator i = values.begin(); i != values.end(); ++i) {
      core::Symbol_sp nameSym = core::lispify_intern(i->name_, core::lisp_currentPackageName(), true);
      core::lisp_extendSymbolToEnumConverter(converter, nameSym, nameSym, i->val_);
    }
    return _from;
  }

  From &_from;
  core::Symbol_sp _converterSym;

private:
  //            void operator=(enum_maker const&); // C4512, assignment operator could not be generated
  template <class T>
  void operator, (T const &) const;
};
}
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
