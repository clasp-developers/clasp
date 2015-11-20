namespace gctools {

// Cast assumes that the client pointer is untagged already
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
template <typename TOPTR, typename FROMPTR>
struct Cast {
  typedef TOPTR ToType;
  typedef FROMPTR FromType;
  inline static bool isA(FromType client) {
    return (dynamic_cast<ToType>(client) != NULL);
  }
};
#else
template <typename TOPTR, typename FROMPTR>
struct Cast {
  typedef TOPTR ToType;
  typedef FROMPTR FromType;
  // Very few Cast's should default back to this one.
  // Maybe keep a count of how often it gets called?
  inline static bool isA(FromType client) {
    return (dynamic_cast<ToType>(client) != NULL);
  }
};
#endif
#endif
#ifdef USE_MPS
template <typename TOPTR, typename FROMPTR>
struct Cast {
  typedef TOPTR ToType;
  typedef FROMPTR FromType;
  // No default methods for isA or castOrNULL are provided for MPS - they must all be provided by clasp_gc.cc
  inline static bool isA(FromType client) {
    printf("%s:%d Add support for Cast::isA for this type\n", __FILE__, __LINE__);
    return false;
  }
};
#endif // USE_MPS

template <typename TOPTR, typename FROMPTR>
struct TaggedCast {
  typedef TOPTR ToType;
  typedef FROMPTR FromType;
  inline static bool isA(FromType client) {
    if (tagged_generalp(client)) {
      return Cast<ToType, FromType>::isA(untag_general<FromType>(client));
    } else if (tagged_consp(client)) {
      return Cast<ToType, FromType>::isA(untag_cons(client));
    }
    return false; //THROW_HARD_ERROR(BF("An immediate should never be isA tested by this function - it should have a specialized version")); // Must be specialized
  }
  inline static ToType castOrNULL(FromType client) {
    if (tagged_generalp(client)) {
      if (Cast<ToType, FromType>::isA(untag_general(client)))
        return reinterpret_cast<ToType>(client);
      return NULL;
    } else if (tagged_consp(client)) {
      if (Cast<ToType, FromType>::isA(untag_cons(client)))
        return reinterpret_cast<ToType>(client);
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
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Fixnum_I *, FROM> {
  typedef core::Fixnum_I *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_fixnump(ptr);
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Integer_O *, core::Integer_O *> {
  typedef core::Integer_O *ToType;
  typedef core::Integer_O *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Integer_O *, core::Fixnum_I *> {
  typedef core::Integer_O *ToType;
  typedef core::Fixnum_I *FromType;
  inline static bool isA(FromType ptr) { return true; };
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <typename FROM>
struct TaggedCast<core::Integer_O *, FROM> {
  typedef core::Integer_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || (tagged_generalp(ptr) && (Cast<ToType, FromType>::isA(untag_general(ptr))));
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Rational_O *, core::Rational_O *> {
  typedef core::Rational_O *ToType;
  typedef core::Rational_O *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) { return client; }
};

template <typename FROM>
struct TaggedCast<core::Rational_O *, FROM> {
  typedef core::Rational_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || (tagged_generalp(ptr) && (Cast<ToType, FromType>::isA(untag_general(ptr))));
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Real_O *, core::Real_O *> {
  typedef core::Real_O *ToType;
  typedef core::Real_O *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Real_O *, core::Fixnum_I *> {
  typedef core::Real_O *ToType;
  typedef core::Fixnum_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Real_O *, core::SingleFloat_I *> {
  typedef core::Real_O *ToType;
  typedef core::SingleFloat_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <typename FROM>
struct TaggedCast<core::Real_O *, FROM> {
  typedef core::Real_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || tagged_single_floatp(ptr) || (tagged_generalp(ptr) && (Cast<ToType, FromType>::isA(untag_general(ptr))));
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::Number_O *, core::Number_O *> {
  typedef core::Number_O *ToType;
  typedef core::Number_O *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Number_O *, core::Fixnum_I *> {
  typedef core::Number_O *ToType;
  typedef core::Fixnum_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::Number_O *, core::SingleFloat_I *> {
  typedef core::Number_O *ToType;
  typedef core::SingleFloat_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <typename FROM>
struct TaggedCast<core::Number_O *, FROM> {
  typedef core::Number_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_fixnump(ptr) || tagged_single_floatp(ptr) || (tagged_generalp(ptr) && (Cast<ToType, FromType>::isA(untag_general(ptr))));
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <>
struct TaggedCast<core::T_O *, core::T_O *> {
  typedef core::T_O *ToType;
  typedef core::T_O *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) { return client; }
};
template <>
struct TaggedCast<core::T_O *, core::Fixnum_I *> {
  typedef core::T_O *ToType;
  typedef core::Fixnum_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::T_O *, core::SingleFloat_I *> {
  typedef core::T_O *ToType;
  typedef core::SingleFloat_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
template <>
struct TaggedCast<core::T_O *, core::Character_I *> {
  typedef core::T_O *ToType;
  typedef core::Character_I *FromType;
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <typename FROM>
struct TaggedCast<core::T_O *, FROM> {
  typedef core::T_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) { return true; };
  inline static ToType castOrNULL(FromType client) {
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
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) { return client; }
};
// Cast from anything to SingleFloat_I*
template <typename FROM>
struct TaggedCast<core::SingleFloat_I *, FROM> {
  typedef core::SingleFloat_I *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_single_floatp(ptr);
  }
  inline static ToType castOrNULL(FromType client) {
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
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) { return client; }
};
template <>
struct TaggedCast<core::Float_O *, core::SingleFloat_I *> {
  typedef core::Float_O *ToType;
  typedef core::SingleFloat_I *FromType;
  inline static bool isA(FromType ptr) { return true; };
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <typename FROM>
struct TaggedCast<core::Float_O *, FROM> {
  typedef core::Float_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_single_floatp(ptr) ||
           (tagged_generalp(ptr) && (Cast<ToType, FromType>::isA(untag_general(ptr))));
  }
  inline static ToType castOrNULL(FromType client) {
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
  inline static bool isA(FromType ptr) { return true; }
  inline static ToType castOrNULL(FromType client) { return client; }
};
// Cast from anything to Character_I*
template <typename FROM>
struct TaggedCast<core::Character_I *, FROM> {
  typedef core::Character_I *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_characterp(ptr);
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<core::Character_I *, FromType>::isA(client)) {
      return reinterpret_cast<ToType>(client);
    }
    return NULL;
  }
};

template <typename FP>
struct TaggedCast<core::Cons_O *, FP> {
  typedef core::Cons_O *ToType;
  typedef FP FromType;
  inline static bool isA(FromType ptr) { return gctools::tagged_consp<FP>(ptr); }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

template <typename FROM>
struct TaggedCast<core::VaList_S *, FROM> {
  typedef core::VaList_S *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType ptr) {
    return tagged_valistp(ptr);
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

}; // namespace gctools

// more specializations in clasp/include/clasp/core/tagged_cast_specializations.h
