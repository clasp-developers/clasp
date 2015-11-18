#ifndef clbind_taggedCast_H
#define clbind_taggedCast_H

namespace gctools {

template <typename Pols, typename Pointer, typename T, typename Sig>
struct TaggedCast<core::BuiltinClosure *, clbind::VariadicConstructorFunctoid<Pols, Pointer, T, Sig> *> {
  typedef core::BuiltinClosure *ToType;
  typedef clbind::VariadicConstructorFunctoid<Pols, Pointer, T, Sig> *FromType;
  inline static bool isA(FromType tagged_client) {
    return true;
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
};
#endif
