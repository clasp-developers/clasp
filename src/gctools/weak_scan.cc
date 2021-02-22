



/*
 * Object scanner - include this and modify the following macros 
 *  to customize this code for different purposes.
 *
 *
#define SCAN_STRUCT_T int          // Type of scanning struct
#define ADDR_T mps_addr_t          // Type of addresses
#define OBJECT_SCAN fixup_objects  // Name of function
#define SCAN_BEGIN(x)              // Macro for starting scanning block
#define SCAN_END(x)                // Macro for end of scanning block
#define POINTER_FIX(field)         // Macro to fix pointer at field
#define RESULT_TYPE                // Result type 
#define RESULT_OK                  // value to return on OK - MPS_RES_OK
#define WEAK_SCAN             // Macro to turn on #ifdef inclusion of code
#define WEAK_SKIP             // Macro to turn on #ifdef inclusion of code
#define WEAK_FWD              // Macro to turn on #ifdef inclusion of code
 */

// !!!!! DEBUG_OBJECT_SCAN can only be on when DEBUG_GUARD_VALIDATE is on!!!!!!
//#define DEBUG_OBJECT_SCAN 1
//#define DEBUG_POINTER_BITMAPS 1

//#define DEBUG_CONTAINER_SCAN 1
//#define DEBUG_CONTAINER_POINTER_BITMAPS 1

#ifdef WEAK_SCAN
RESULT_TYPE WEAK_SCAN(SCAN_STRUCT_T ss, ADDR_T client, ADDR_T limit) {
  SCAN_BEGIN(ss) {
    while (client < limit) {
      const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(gctools::ClientPtrToBasePtr(client));
      WeakObject *weakObj = reinterpret_cast<WeakObject *>(client);
      switch (header._stamp_wtag_mtag._value) {
      case WeakBucketKind: {
        WeakBucketsObjectType *obj = reinterpret_cast<WeakBucketsObjectType *>(weakObj);
        // Fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          POINTER_FIX((core::T_O**)&obj->bucket[i].rawRef_());
        }
        client = (ADDR_T)((char*)client + sizeof(WeakBucketsObjectType) + sizeof(typename WeakBucketsObjectType::value_type) * obj->length() + sizeof(gctools::Header_s));
      } break;
      case StrongBucketKind: {
        StrongBucketsObjectType *obj = reinterpret_cast<StrongBucketsObjectType *>(client);
        // Fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          core::T_O** objptr = (core::T_O**)&obj->bucket[i].rawRef_();
          printf("%s:%d [%d of %d] objptr@ %p -> %p\n", __FILE__, __LINE__, i, obj->length(), objptr, (void*)(*objptr) );
          POINTER_FIX(objptr);
        }
        client = (ADDR_T)((char *)client + sizeof(StrongBucketsObjectType) + sizeof(typename StrongBucketsObjectType::value_type) * obj->length() + sizeof(gctools::Header_s));
      } break;
      case WeakMappingKind: {
        WeakMappingObjectType *obj = reinterpret_cast<WeakMappingObjectType *>(weakObj);
        // Fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        POINTER_FIX((core::T_O**)&obj->bucket.rawRef_());
        client = (ADDR_T)((char *)client + sizeof(WeakMappingObjectType) + sizeof(gctools::Header_s));
      } break;
      case StrongMappingKind: {
        StrongMappingObjectType *obj = reinterpret_cast<StrongMappingObjectType *>(client);
        // Fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        POINTER_FIX((core::T_O**)&obj->bucket.rawRef_());
        client = (ADDR_T)((char *)client + sizeof(StrongMappingObjectType) + sizeof(gctools::Header_s));
      } break;
      case WeakPointerKind: {
        WeakPointer *obj = reinterpret_cast<WeakPointer *>(client);
        POINTER_FIX((core::T_O**)&obj->value.rawRef_());
        client = (ADDR_T)((char *)client + sizeof(WeakPointer) + sizeof(gctools::Header_s));
      } break;
      default:
        THROW_HARD_ERROR(BF("Handle other weak kind %d") % weakObj->kind());
      }
    };
  }
  SCAN_END(ss);
  return RESULT_OK;
}
#endif

#ifdef WEAK_SKIP
ADDR_T WEAK_SKIP(ADDR_T client, bool dbg) {
  GCWEAK_LOG(BF("weak_obj_skip client=%p") % ((void *)client));
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(gctools::ClientPtrToBasePtr(client));
  switch (header._stamp_wtag_mtag._value) {
  case WeakBucketKind: {
    WeakBucketsObjectType *obj = reinterpret_cast<WeakBucketsObjectType *>(client);
    GCWEAK_LOG(BF("WeakBucketKind sizeof(WeakBucketsObjectType)=%d + sizeof(typename WeakBucketsObjectType::value_type)=%d * obj->length()=%d") % sizeof(WeakBucketsObjectType) % sizeof(typename WeakBucketsObjectType::value_type) % obj->length());
    client = (ADDR_T)((char *)client + sizeof(WeakBucketsObjectType) + sizeof(typename WeakBucketsObjectType::value_type) * obj->length() + sizeof(gctools::Header_s));
  } break;
  case StrongBucketKind: {
    StrongBucketsObjectType *obj = reinterpret_cast<StrongBucketsObjectType *>(client);
    GCWEAK_LOG(BF("StrongBucketKind sizeof(StrongBucketsObjectType)=%d + sizeof(typename StrongBucketsObjectType::value_type)=%d * obj->length()=%d") % sizeof(StrongBucketsObjectType) % sizeof(typename StrongBucketsObjectType::value_type) % obj->length());
    client = (ADDR_T)((char *)client + sizeof(StrongBucketsObjectType) + sizeof(typename StrongBucketsObjectType::value_type) * obj->length() + sizeof(gctools::Header_s));
  } break;
  case WeakMappingKind: {
    GCWEAK_LOG(BF("WeakMappingKind"));
    client = (ADDR_T)((char *)client + sizeof(WeakMappingObjectType) + sizeof(gctools::Header_s));
  } break;
  case StrongMappingKind: {
    GCWEAK_LOG(BF("StrongMappingKind"));
    client = (ADDR_T)((char *)client + sizeof(StrongMappingObjectType) + sizeof(gctools::Header_s));
  } break;
  case WeakPointerKind: {
    GCWEAK_LOG(BF("WeakPointerKind"));
    client = (ADDR_T)((char *)client + sizeof(WeakPointer) + sizeof(gctools::Header_s));
  } break;
  case WeakFwdKind: {
    GCWEAK_LOG(BF("WeakFwdKind"));
    weak_fwd_s *obj = reinterpret_cast<weak_fwd_s *>(client);
    client = (ADDR_T)((char *)client + Align(obj->size.unsafe_fixnum()));
  } break;
  case WeakFwd2Kind: {
    GCWEAK_LOG(BF("WeakFwd2Kind"));
    client = (ADDR_T)((char *)client + Align(sizeof(weak_fwd2_s)));
  } break;
  case WeakPadKind: {
    GCWEAK_LOG(BF("WeakPadKind"));
    weak_pad_s *obj = reinterpret_cast<weak_pad_s *>(client);
    client = (ADDR_T)((char *)client + Align(obj->size.unsafe_fixnum()));
  } break;
  case WeakPad1Kind: {
    GCWEAK_LOG(BF("WeakPad1Kind"));
    client = (ADDR_T)((char *)client + Align(sizeof(weak_pad1_s)));
  }
  default:
      THROW_HARD_ERROR(BF("Handle weak_obj_skip other weak kind %d") % header._stamp_wtag_mtag._value);
  }
  GCWEAK_LOG(BF("weak_obj_skip returning client=%p") % ((void *)client));
  return client;
};
#endif

#ifdef WEAK_FWD
void WEAK_FWD(ADDR_T old_client, ADDR_T new_client) {
  gctools::Header_s& header = *reinterpret_cast<gctools::Header_s*>(gctools::ClientPtrToBasePtr(old_client));
  ADDR_T limit = WEAK_SKIP_IN_WEAK_FWD(old_client,false);
  size_t size = (char *)limit - (char *)old_client;
  assert(size >= Align(sizeof(weak_fwd2_s)));
  if (size == Align(sizeof(weak_fwd2_s))) {
    header._stamp_wtag_mtag.setFwdPointer(new_client);
  } else {
    header._stamp_wtag_mtag.setFwdPointer(new_client);
    header._stamp_wtag_mtag.setFwdSize(size);
  }
}
#endif

