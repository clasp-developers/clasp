#ifndef other_tagged_casts_H
#define other_tagged_casts_H

namespace core {
class Closure;
class MacroClosure;
class InstanceClosure;
class SingleDispatchGenericFunctionClosure;
};
namespace llvmo {
class CompiledClosure;
};

namespace gctools {

template <typename FROM>
struct TaggedCast<core::BuiltinClosure *, FROM> {
  typedef core::BuiltinClosure *ToType;
  typedef FROM FromType;
  inline static bool isA(FromType tagged_client) {
    if (tagged_generalp(tagged_client)) {
      void *ptr = untag_general(tagged_client);
      gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(ptr));
      int kindVal = header->kind();
      return (kindVal == gctools::GCKind<core::BuiltinClosure>::Kind || kindVal == gctools::GCKind<core::MacroClosure>::Kind);
    }
    return false;
  }
  inline static ToType castOrNULL(FromType client) {
    if (TaggedCast<ToType, FromType>::isA(client))
      return reinterpret_cast<ToType>(client);
    return NULL;
  }
};

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

template <typename FROM>
struct TaggedCast<gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O>> *, FROM> {
  typedef gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O>> *ToType;
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

template <typename FROM>
struct TaggedCast<core::SequenceStepper *, FROM> {
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
};

#endif
