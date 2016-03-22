#ifndef other_tagged_casts_H
#define other_tagged_casts_H

namespace core {
  class Closure_O;
  class MacroClosure_O;
  class InstanceClosure_O;
  class SingleDispatchGenericFunctionClosure_O;
  class CompiledClosure_O;
};

namespace gctools {

  #if 0
template <typename FROM>
struct TaggedCast<core::BuiltinClosure_O *, FROM> {
  typedef core::BuiltinClosure_O *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType tagged_client) {
    if (tagged_generalp(tagged_client)) {
      void *ptr = untag_general(tagged_client);
      gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(ptr));
      int kindVal = header->kind();
      return (kindVal == gctools::GCKind<core::BuiltinClosure_O>::Kind || kindVal == gctools::GCKind<core::MacroClosure>::Kind);
    }
    return false;
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
#endif

#if 0
template <typename FROM>
struct TaggedCast<core::Closure *, FROM> {
  typedef core::Closure *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType tagged_client) {
    if (tagged_generalp(tagged_client)) {
      void *ptr = untag_general(tagged_client);
      gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(ptr));
      int kindVal = header->kind();
      return (kindVal == gctools::GCKind<core::BuiltinClosure>::Kind || kindVal == gctools::GCKind<core::InstanceClosure>::Kind || kindVal == gctools::GCKind<llvmo::CompiledClosure>::Kind || kindVal == gctools::GCKind<core::MacroClosure>::Kind || kindVal == gctools::GCKind<core::SingleDispatchGenericFunctionClosure>::Kind || kindVal == gctools::GCKind<core::InterpretedClosure>::Kind
                                                                                                                                                                                                                                                                                                                                           //               || kindVal == gctools::GCKind<core::SingleDispatchGenericFunctionClosure::Kind
              );
    }
    return false;
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
#endif

 #if 0
template <typename FROM>
struct TaggedCast<core::SequenceStepper_O *, FROM> {
  typedef core::SequenceStepper *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType tagged_client) {
    if (tagged_generalp(tagged_client)) {
      // Should I have more here?
      return dynamic_cast<ToType>(untag_general(tagged_client)) != NULL;
    }
    return false;
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};
#endif



};

#endif
