#ifndef tagged_cast_specializations_H
#define tagged_cast_specializations_H

#ifdef USE_BOEHM
namespace gctools {
#if 1
template <>
struct TaggedCast<core::Environment_O *, core::T_O *> {
  typedef core::Environment_O *ToType;
  typedef core::T_O *FromType;
  inline static bool isA(FromType ptr) {
    return untag_object<core::T_O *>(ptr)->environmentp();
  }
  inline static ToType castOrNULL(FromType client) {
    if (isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
#endif
};

#endif

#endif
