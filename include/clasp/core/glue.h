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
#ifndef glue_H
#define glue_H

template <class to_class, class from_class>
inline gctools::smart_ptr<to_class> safe_downcast(const gctools::smart_ptr<from_class> &c) {
  if (c.nilp())
    return _Nil<to_class>();
  gctools::smart_ptr<to_class> dc = gctools::dynamic_pointer_cast<to_class>(c);
  if (dc.objectp())
    return dc;
  lisp_throwUnexpectedType(c, to_class::static_classSymbol());
  return _Nil<to_class>();
}

template <>
inline gctools::smart_ptr<core::T_O> safe_downcast(const gctools::smart_ptr<core::T_O> &c) {
  return c;
}

template <>
inline gctools::smart_ptr<core::Cons_O> safe_downcast(const gctools::smart_ptr<core::Cons_O> &c) {
  return c;
}

template <>
inline gctools::smart_ptr<core::Symbol_O> safe_downcast(const gctools::smart_ptr<core::Symbol_O> &c) {
  return c;
}

/*! Downcast multiple_values<
 */

template <class to_class, class from_class>
inline gctools::multiple_values<to_class> safe_downcast(const gctools::multiple_values<from_class> &c) {
  if (c.nilp())
    return gctools::multiple_values<to_class>(_Nil<to_class>(), c.number_of_values());
  gctools::multiple_values<to_class> dc = gctools::dynamic_pointer_cast<to_class>(c);
  if (dc.pointerp())
    return dc;
  lisp_throwUnexpectedType(c, to_class::static_classSymbol());
  // dummy return */
  return gctools::multiple_values<to_class>(_Nil<to_class>(), 1);
}

template <>
inline gctools::multiple_values<core::T_O> safe_downcast(const gctools::multiple_values<core::T_O> &c) {
  return c;
}

template <>
inline gctools::multiple_values<core::Cons_O> safe_downcast(const gctools::multiple_values<core::Cons_O> &c) {
  return c;
}

template <>
inline gctools::multiple_values<core::Symbol_O> safe_downcast(const gctools::multiple_values<core::Symbol_O> &c) {
  return c;
}

#define T_P const core::T_sp &

namespace translate {
/*! The second template argument can be std::true_type if the value passed to the from_object ctor should be used
      to construct the value or std::false_type if a default initialization value should be used */
template <class oClass, typename Doit = std::true_type>
struct from_object {
#if 0
	    typedef	gctools::smart_ptr<oClass>	DeclareType;	
	    DeclareType _v;
	    from_object(T_P o) : _v(o.as<oClass>()) {};
#endif
};

#if 0
    template <>
    struct	from_object<bool*,std::false_type>
    {
	typedef	bool*	DeclareType;
        bool            _val;
	DeclareType     _v;
        from_object(T_P o) : _val(false), _v(&_val) {};
    };
    template <>
    struct	from_object<string&,std::false_type>
    {
	typedef	string		DeclareType;
	DeclareType _v;
        from_object(T_P o) : _v("") {};
    };

#endif

struct dont_adopt_pointer {};
struct adopt_pointer {};
/*! to_object takes a class to convert to an T_sp type and a template parameter
      that specifies if the pointer should be adopted or not adopted */
template <class oClass, class AdoptPolicy = dont_adopt_pointer>
struct to_object {
};

template <typename T>
struct from_object<gctools::smart_ptr<T>, std::true_type> {
  typedef gctools::smart_ptr<T> ExpectedType;
  typedef gctools::smart_ptr<T> DeclareType;
  DeclareType _v;
  from_object(const core::T_sp &o) : _v(gc::As<gctools::smart_ptr<T>>(o)){};
};

template <>
struct from_object<core::T_sp, std::true_type> {
  typedef core::T_sp ExpectedType;
  typedef core::T_sp DeclareType;
  DeclareType _v;
  from_object(const core::T_sp &o) : _v(o){};
};

template <typename T>
struct from_object<gc::Nilable<gc::smart_ptr<T>>, std::true_type> {
  typedef gctools::Nilable<gc::smart_ptr<T>> ExpectedType;
  typedef ExpectedType DeclareType;
  DeclareType _v;
  from_object(const core::T_sp &o) : _v(o){};
};

template <class T>
struct to_object<gctools::smart_ptr<T>> {
  static core::T_sp convert(const gctools::smart_ptr<T> &o) {
    return o;
  }
};

template <class T>
struct to_object<gc::Nilable<gctools::smart_ptr<T>>> {
  static core::T_sp convert(const gc::Nilable<gc::smart_ptr<T>> &o) {
    return static_cast<core::T_sp>(o);
  }
};
};

#define __FROM_OBJECT_CONVERTER(oClass)                                        \
  namespace translate {                                                        \
  template <> struct from_object<gctools::smart_ptr<oClass>, std::true_type> { \
    typedef gctools::smart_ptr<oClass> ExpectedType;                           \
    typedef gctools::smart_ptr<oClass> DeclareType;                            \
    DeclareType _v;                                                            \
    from_object(T_P o) : _v(o.as<oClass>()) {}                                 \
  };                                                                           \
  };

#define __TO_OBJECT_CONVERTER(oClass)                        \
  namespace translate {                                      \
  template <> struct to_object<gctools::smart_ptr<oClass>> { \
    typedef gctools::smart_ptr<oClass> GivenType;            \
    static core::T_sp convert(GivenType o) {                 \
      _G();                                                  \
      return o;                                              \
    }                                                        \
  };                                                         \
  };

#define DECLARE_ENUM_SYMBOL_TRANSLATOR(enumType, psid)                                                                 \
  namespace translate {                                                                                                \
  template <> struct from_object<enumType, std::true_type> {                                                           \
    typedef enumType ExpectedType;                                                                                     \
    typedef enumType DeclareType;                                                                                      \
    DeclareType _v;                                                                                                    \
    from_object(T_P o) : _v(static_cast<DeclareType>(core::lisp_lookupEnumForSymbol(psid, o.as<core::Symbol_O>()))){}; \
  };                                                                                                                   \
  template <> struct to_object<enumType> {                                                                             \
    typedef enumType GivenType;                                                                                        \
    static core::T_sp convert(enumType e) {                                                                            \
      _G();                                                                                                            \
      return core::lisp_lookupSymbolForEnum(psid, (int)(e));                                                           \
    }                                                                                                                  \
  };                                                                                                                   \
  };

#define STREAMIO(classo)                                                     \
  std::ostream &operator<<(std::ostream &os, gctools::smart_ptr<classo> p) { \
    THROW_HARD_ERROR(boost::format("Illegal operator<<"));                   \
    return os;                                                               \
  }

#define TRANSLATE(classo)

//    STREAMIO(classo);

#endif
