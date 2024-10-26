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
// #define DEBUG_OBJECT_SCAN 1
// #define DEBUG_POINTER_BITMAPS 1

// #define DEBUG_CONTAINER_SCAN 1
// #define DEBUG_CONTAINER_POINTER_BITMAPS 1

#ifdef WEAK_SCAN
RESULT_TYPE WEAK_SCAN(SCAN_STRUCT_T ss, ADDR_T client, ADDR_T limit EXTRA_ARGUMENTS) {
  SCAN_BEGIN(ss) {
    while (client < limit) {
      const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(WEAK_PTR_TO_HEADER_PTR(client));
      gctools::WeakObject* weakObj = reinterpret_cast<gctools::WeakObject*>(client);
      switch (header._badge_stamp_wtag_mtag._value) {
      case gctools::Header_s::WeakBucketKind: {
        gctools::WeakBucketsObjectType* obj = reinterpret_cast<gctools::WeakBucketsObjectType*>(weakObj);
        // Fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          POINTER_FIX((core::T_O**)&obj->bucket[i].rawRef_());
        }
        client = (ADDR_T)((char*)client + sizeof(gctools::WeakBucketsObjectType) +
                          sizeof(typename gctools::WeakBucketsObjectType::value_type) * obj->length() + sizeof(gctools::Header_s));
      } break;
      case gctools::Header_s::StrongBucketKind: {
        gctools::StrongBucketsObjectType* obj = reinterpret_cast<gctools::StrongBucketsObjectType*>(client);
        // fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          core::T_O** objptr = (core::T_O**)&obj->bucket[i].rawRef_();
          //          printf("%s:%d [%d of %d] objptr@ %p -> %p\n", __FILE__, __LINE__, i, obj->length(), objptr, (void*)(*objptr)
          //          );
          POINTER_FIX(objptr);
        }
        client =
            (ADDR_T)((char*)client + sizeof(gctools::StrongBucketsObjectType) +
                     sizeof(typename gctools::StrongBucketsObjectType::value_type) * obj->length() + sizeof(gctools::Header_s));
      } break;
      case gctools::Header_s::WeakMappingKind: {
        gctools::WeakMappingObjectType* obj = reinterpret_cast<gctools::WeakMappingObjectType*>(weakObj);
        // fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        POINTER_FIX((core::T_O**)&obj->bucket.rawRef_());
        client = (ADDR_T)((char*)client + sizeof(gctools::WeakMappingObjectType) + sizeof(gctools::Header_s));
      } break;
      case gctools::Header_s::StrongMappingKind: {
        gctools::StrongMappingObjectType* obj = reinterpret_cast<gctools::StrongMappingObjectType*>(client);
        // fix the obj->dependent pointer
        POINTER_FIX((core::T_O**)&obj->dependent.rawRef_());
        POINTER_FIX((core::T_O**)&obj->bucket.rawRef_());
        client = (ADDR_T)((char*)client + sizeof(gctools::StrongMappingObjectType) + sizeof(gctools::Header_s));
      } break;
      default:
        THROW_HARD_ERROR("handle other weak kind {}", header._badge_stamp_wtag_mtag._value);
      }
    };
  }
  SCAN_END(ss);
  return RESULT_OK;
}
#endif

#ifdef WEAK_SKIP
ADDR_T WEAK_SKIP(ADDR_T client, bool dbg, size_t& objectSize) {
  GCWEAK_LOG(fmt::format("weak_obj_skip client={}", ((void*)client)));
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(WEAK_PTR_TO_HEADER_PTR(client));
  if (header._badge_stamp_wtag_mtag.weakObjectP()) {
    switch (header._badge_stamp_wtag_mtag._value) {
    case gctools::Header_s::WeakBucketKind: {
      gctools::WeakBucketsObjectType* obj = reinterpret_cast<gctools::WeakBucketsObjectType*>(client);
      GCWEAK_LOG(fmt::format("WeakBucketKind sizeof(WeakBucketsObjectType)={} + sizeof(typename "
                             "WeakBucketsObjectType::value_type)={} * obj->length()={}",
                             sizeof(WeakBucketsObjectType), sizeof(typename WeakBucketsObjectType::value_type), obj->length()));
      objectSize = gctools::AlignUp(sizeof(gctools::WeakBucketsObjectType) +
                                    sizeof(typename gctools::WeakBucketsObjectType::value_type) * obj->length());
    } break;
    case gctools::Header_s::StrongBucketKind: {
      gctools::StrongBucketsObjectType* obj = reinterpret_cast<gctools::StrongBucketsObjectType*>(client);
      GCWEAK_LOG(fmt::format("StrongBucketKind sizeof(StrongBucketsObjectType)={} + sizeof(typename "
                             "StrongBucketsObjectType::value_type)={} * obj->length()={}",
                             sizeof(StrongBucketsObjectType), sizeof(typename StrongBucketsObjectType::value_type), obj->length()));
      objectSize = gctools::AlignUp(sizeof(gctools::StrongBucketsObjectType) +
                                    sizeof(typename gctools::StrongBucketsObjectType::value_type) * obj->length());
    } break;
    case gctools::Header_s::WeakMappingKind: {
      GCWEAK_LOG(fmt::format("WeakMappingKind"));
      objectSize = gctools::AlignUp(sizeof(gctools::WeakMappingObjectType));
    } break;
    case gctools::Header_s::StrongMappingKind: {
      GCWEAK_LOG(fmt::format("StrongMappingKind"));
      objectSize = gctools::AlignUp(sizeof(gctools::StrongMappingObjectType));
    } break;
    }
  } else {
    THROW_HARD_ERROR("Handle weak_obj_skip other weak kind {}", header._badge_stamp_wtag_mtag._value);
  }
  client = (ADDR_T)((char*)client + objectSize + sizeof(gctools::Header_s));
  GCWEAK_LOG(fmt::format("weak_obj_skip returning client={}", ((void*)client)));
  return client;
};
#endif

#ifdef WEAK_FWD
void WEAK_FWD(ADDR_T old_client, ADDR_T new_client) {
  gctools::Header_s& header = *reinterpret_cast<gctools::Header_s*>(WEAK_PTR_TO_HEADER_PTR(old_client));
  size_t objectSize;
  ADDR_T limit = WEAK_SKIP_IN_WEAK_FWD(old_client, false, objectSize);
  size_t size = (char*)limit - (char*)old_client;
  assert(size >= Align(sizeof(weak_fwd2_s)));
  if (size == gctools::Align(sizeof(gctools::weak_fwd2_s))) {
    header._badge_stamp_wtag_mtag.setFwdPointer((void*)new_client);
  } else {
    header._badge_stamp_wtag_mtag.setFwdPointer((void*)new_client);
    header._badge_stamp_wtag_mtag.setFwdSize(size);
  }
}
#endif
