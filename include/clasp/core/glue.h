#pragma once
/*
    File: glue.h
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

#define T_P const core::T_sp&

namespace translate {
template <class oClass> struct from_object;

// Shortcut operator when we just want a new object and don't care about
// references etc.
template <typename T> T make_from_object(core::T_sp o) {
  return from_object<T>(o)._v;
}

struct dont_adopt_pointer {};
struct adopt_pointer {};
/*! to_object takes a class to convert to an T_sp type and a template parameter
      that specifies if the pointer should be adopted or not adopted */
template <class oClass, class AdoptPolicy = dont_adopt_pointer> struct to_object {};

template <typename T> struct from_object<gctools::smart_ptr<T>> {
  typedef gctools::smart_ptr<T> DeclareType;
  DeclareType _v;
  from_object(const core::T_sp& o) : _v(gc::As<gctools::smart_ptr<T>>(o)) {};
};

template <typename T> struct from_object<gctools::smart_ptr<T>&> {
  typedef gctools::smart_ptr<T> DeclareType;
  DeclareType _v;
  from_object(const core::T_sp& o) : _v(gc::As<gctools::smart_ptr<T>>(o)) {};
};

template <> struct from_object<gctools::smart_ptr<core::Character_I>&> {
  typedef gctools::smart_ptr<core::Character_I> DeclareType;
  DeclareType _v;
  from_object(const core::T_sp& o) : _v(gc::As<gctools::smart_ptr<core::Character_I>>(o)) {};
};

template <> struct from_object<core::T_sp> {
  typedef core::T_sp DeclareType;
  DeclareType _v;
  from_object(const core::T_sp& o) : _v(o){};
};

template <> struct from_object<gctools::smart_ptr<core::List_V>&> {
  typedef core::List_sp DeclareType;
  DeclareType _v;
  from_object(const core::T_sp& o) : _v(gc::As<core::List_sp>(o)) {};
};

template <typename T> struct from_object<gc::Nilable<gc::smart_ptr<T>>> {
  typedef gctools::Nilable<gc::smart_ptr<T>> DeclareType;
  DeclareType _v;
  from_object(const core::T_sp& o) : _v(o){};
};

template <class T> struct to_object<gctools::smart_ptr<T>> {
  static core::T_sp convert(const gctools::smart_ptr<T>& o) { return o; }
};

template <class T> struct to_object<gc::Nilable<gctools::smart_ptr<T>>> {
  static core::T_sp convert(const gc::Nilable<gc::smart_ptr<T>>& o) { return static_cast<core::T_sp>(o); }
};
}; // namespace translate

#define DECLARE_ENUM_SYMBOL_TRANSLATOR(enumType, psid)                                                                             \
  namespace translate {                                                                                                            \
  template <> struct from_object<enumType> {                                                                       \
    typedef enumType DeclareType;                                                                                                  \
    DeclareType _v;                                                                                                                \
    from_object(T_P o) : _v(static_cast<DeclareType>(core::lisp_lookupEnumForSymbol(psid, o.as<core::Symbol_O>()))){};             \
  };                                                                                                                               \
  template <> struct to_object<enumType> {                                                                                         \
    typedef enumType GivenType;                                                                                                    \
    static core::T_sp convert(enumType e) {                                                                                        \
      _G();                                                                                                                        \
      return core::lisp_lookupSymbolForEnum(psid, (int)(e));                                                                       \
    }                                                                                                                              \
  };                                                                                                                               \
  };
