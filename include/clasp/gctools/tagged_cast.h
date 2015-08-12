namespace gctools {
template <typename TOPTR, typename FROMPTR>
struct TaggedCast {
  typedef TOPTR ToType;
  typedef FROMPTR FromType;
  static bool isA(FromType client) {
    if (tagged_generalp(client)) {
      return (dynamic_cast<ToType>(untag_general(client)) != NULL);
    } else if (tagged_consp(client)) {
      return (dynamic_cast<ToType>(untag_cons(client)) != NULL);
    }
    return false; //THROW_HARD_ERROR(BF("An immediate should never be isA tested by this function - it should have a specialized version")); // Must be specialized
  }
  static ToType castOrNULL(FromType client) {
    if (tagged_generalp(client)) {
      ToType ptr = dynamic_cast<ToType>(untag_general(client));
      if (ptr) return reinterpret_cast<ToType>(client);
      return NULL;
    } else if (tagged_consp(client)) {
      ToType ptr = dynamic_cast<ToType>(untag_cons(client));
      if (ptr) return reinterpret_cast<ToType>(client);
      return NULL;
    }
    return NULL; // handle with specializations
  }
};
};

namespace core {
  class Fixnum_I {};
  class SingleFloat_I {};
  class Character_I {};
  class Integer_O;
  class Environment_O;
  class Rational_O;
  class Real_O;
  class Number_O;
  class T_O;
  class Float_O;
  typedef Fixnum_I Fixnum_O;
  typedef SingleFloat_I SingleFloat_O;
  typedef Character_I Character_O;
};

////////////////////////////////////////////////////////////////////////
//
// Downcast from a supertype to a subtype
//
// Every possible downcast involving immediate types needs an TaggedCast template function
namespace gctools {
template <>
struct TaggedCast<core::Fixnum_I *, core::Fixnum_I *> {
  typedef core::Fixnum_I *ToType;
  typedef core::Fixnum_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Fixnum_I *, FROM> {
  typedef core::Fixnum_I *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_fixnump(ptr);
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Integer_O *, core::Integer_O *> {
  typedef core::Integer_O *ToType;
  typedef core::Integer_O *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Integer_O *, core::Fixnum_I *> {
  typedef core::Integer_O *ToType;
  typedef core::Fixnum_I *FromType;
  static bool isA(FromType ptr) { return true; };
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Integer_O *, FROM> {
  typedef core::Integer_O *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || (tagged_generalp(ptr) && (dynamic_cast<ToType>(untag_general(ptr)) != NULL));
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Rational_O *, core::Rational_O *> {
  typedef core::Rational_O *ToType;
  typedef core::Rational_O *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) { return client; }
};
template <typename FROM>
struct TaggedCast<core::Rational_O *, FROM> {
  typedef core::Rational_O *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || (tagged_generalp(ptr) && (dynamic_cast<ToType>(untag_general(ptr)) != NULL));
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Real_O *, core::Real_O *> {
  typedef core::Real_O *ToType;
  typedef core::Real_O *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Real_O *, core::Fixnum_I *> {
  typedef core::Real_O *ToType;
  typedef core::Fixnum_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Real_O *, core::SingleFloat_I *> {
  typedef core::Real_O *ToType;
  typedef core::SingleFloat_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Real_O *, FROM> {
  typedef core::Real_O *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || tagged_single_floatp(ptr) || (tagged_generalp(ptr) && (dynamic_cast<ToType>(untag_general(ptr)) != NULL));
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Number_O *, core::Number_O *> {
  typedef core::Number_O *ToType;
  typedef core::Number_O *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Number_O *, core::Fixnum_I *> {
  typedef core::Number_O *ToType;
  typedef core::Fixnum_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Number_O *, core::SingleFloat_I *> {
  typedef core::Number_O *ToType;
  typedef core::SingleFloat_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Number_O *, FROM> {
  typedef core::Number_O *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || tagged_single_floatp(ptr) || (tagged_generalp(ptr) && (dynamic_cast<ToType>(untag_general(ptr)) != NULL));
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::T_O *, core::T_O *> {
  typedef core::T_O *ToType;
  typedef core::T_O *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) { return client; }
};
template <>
struct TaggedCast<core::T_O *, core::Fixnum_I *> {
  typedef core::T_O *ToType;
  typedef core::Fixnum_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::T_O *, core::SingleFloat_I *> {
  typedef core::T_O *ToType;
  typedef core::SingleFloat_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::T_O *, core::Character_I *> {
  typedef core::T_O *ToType;
  typedef core::Character_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::T_O *, FROM> {
  typedef core::T_O *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) { return true; };
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

// Trivial cast from SingleFloat_I* to SingleFloat_I*
template <>
struct TaggedCast<core::SingleFloat_I *, core::SingleFloat_I *> {
  typedef core::SingleFloat_I *ToType;
  typedef core::SingleFloat_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) { return client; }
};
// Cast from anything to SingleFloat_I*
template <typename FROM>
struct TaggedCast<core::SingleFloat_I *, FROM> {
  typedef core::SingleFloat_I *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_single_floatp(ptr);
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<core::SingleFloat_I *, FromType>::isA(client)) {
      return reinterpret_cast<ToType>(client);
    }
    return NULL;
  }
};

template <>
struct TaggedCast<core::Float_O *, core::Float_O *> {
  typedef core::Float_O *ToType;
  typedef core::Float_O *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) { return client; }
};
template <>
struct TaggedCast<core::Float_O *, core::SingleFloat_I *> {
  typedef core::Float_O *ToType;
  typedef core::SingleFloat_I *FromType;
  static bool isA(FromType ptr) { return true; };
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Float_O *, FROM> {
  typedef core::Float_O *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_single_floatp(ptr) || (tagged_generalp(ptr) && (dynamic_cast<ToType>(untag_general(ptr)) != NULL));
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<core::Float_O *, FromType>::isA(client)) {
      return reinterpret_cast<ToType>(client);
    }
    return NULL;
  }
};

// Trivial cast from Character_I* to Character_I*
template <>
struct TaggedCast<core::Character_I *, core::Character_I *> {
  typedef core::Character_I *ToType;
  typedef core::Character_I *FromType;
  static bool isA(FromType ptr) { return true; }
  static ToType castOrNULL(FromType client) { return client; }
};
// Cast from anything to Character_I*
template <typename FROM>
struct TaggedCast<core::Character_I *, FROM> {
  typedef core::Character_I *ToType;
  typedef FROM FromType;
  static bool isA(FromType ptr) {
    return tagged_characterp(ptr);
  }
  static ToType castOrNULL(FromType client) {
    if (TaggedCast<core::Character_I *, FromType>::isA(client)) {
      return reinterpret_cast<ToType>(client);
    }
    return NULL;
  }
};
};


// more specializations in clasp/include/clasp/core/tagged_cast_specializations.h

