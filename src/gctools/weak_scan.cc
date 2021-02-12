



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

#ifdef GC_WEAK_SCAN
RESULT_TYPE    WEAK_SCAN(SCAN_STRUCT_T ss, ADDR_T base, ADDR_T limit) {
  MPS_SCAN_BEGIN(ss) {
    while (base < limit) {
      WeakObject *weakObj = reinterpret_cast<WeakObject *>(base);
      switch (weakObj->kind()) {
      case WeakBucketKind: {
        WeakBucketsObjectType *obj = reinterpret_cast<WeakBucketsObjectType *>(weakObj);
        // Fix the obj->dependent pointer
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->dependent.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::ptag<core::T_O *>(p);
          RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
          if (res != RESULT_OK)
            return res;
          p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
          obj->dependent.rawRef_() = reinterpret_cast<gctools::tagged_pointer<gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O> > >::Type *>(p); //reinterpret_cast<gctools::Header_s*>(p);
        }
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket[i].raw_());
          if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
            core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
            core::T_O *tag = gctools::ptag<core::T_O *>(p);
            RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
            if (res != RESULT_OK)
              return res;
            if (pobj == NULL && obj->dependent) {
              obj->dependent->bucket[i] = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
              obj->bucket[i] = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
            } else {
              p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
              obj->bucket[i].setRaw_(reinterpret_cast<gc::Tagged>(p)); //reinterpret_cast<gctools::Header_s*>(p);
            }
          }
        }
        base = (char *)base + sizeof(WeakBucketsObjectType) + sizeof(typename WeakBucketsObjectType::value_type) * obj->length();
      } break;
      case StrongBucketKind: {
        StrongBucketsObjectType *obj = reinterpret_cast<StrongBucketsObjectType *>(base);
        // Fix the obj->dependent pointer
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->dependent.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::ptag<core::T_O *>(p);
          RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
          if (res != RESULT_OK)
            return res;
          p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
          obj->dependent.rawRef_() = reinterpret_cast<gctools::tagged_pointer<gctools::BucketsBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O> > >::Type *>(p); //reinterpret_cast<gctools::Header_s*>(p);
        }
        for (int i(0), iEnd(obj->length()); i < iEnd; ++i) {
          // MPS_FIX12(ss,reinterpret_cast<ADDR_T*>(&(obj->bucket[i].raw_())));
          core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket[i].raw_());
          if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
            core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
            core::T_O *tag = gctools::ptag<core::T_O *>(p);
            RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
            if (res != RESULT_OK)
              return res;
            p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
            obj->bucket[i].setRaw_((gc::Tagged)(p)); //reinterpret_cast<gctools::Header_s*>(p);
          }
        }
        base = (char *)base + sizeof(StrongBucketsObjectType) + sizeof(typename StrongBucketsObjectType::value_type) * obj->length();
      } break;
      case WeakMappingKind: {
        WeakMappingObjectType *obj = reinterpret_cast<WeakMappingObjectType *>(weakObj);
        // Fix the obj->dependent pointer
        {
          core::T_O *p = reinterpret_cast<core::T_O *>(obj->dependent.raw_());
          if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
            core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
            core::T_O *tag = gctools::ptag<core::T_O *>(p);
            RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
            if (res != RESULT_OK)
              return res;
            p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
            obj->dependent.rawRef_() = reinterpret_cast<gctools::tagged_pointer<gctools::MappingBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O> > >::Type *>(p); //reinterpret_cast<gctools::Header_s*>(p);
          }
        }
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::ptag<core::T_O *>(p);
          RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
          if (res != RESULT_OK)
            return res;
          if (p == NULL && obj->dependent) {
            obj->dependent->bucket = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
            obj->bucket = WeakBucketsObjectType::value_type(gctools::make_tagged_deleted<core::T_O *>());
          } else {
            p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
            obj->bucket.setRaw_((gc::Tagged)(p)); // raw_() = reinterpret_cast<core::T_O*>(p);
          }
        }
        base = (char *)base + sizeof(WeakMappingObjectType);
      } break;
      case StrongMappingKind: {
        StrongMappingObjectType *obj = reinterpret_cast<StrongMappingObjectType *>(base);
        // Fix the obj->dependent pointer
        {
          core::T_O *p = reinterpret_cast<core::T_O *>(obj->dependent.raw_());
          if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
            core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
            core::T_O *tag = gctools::ptag<core::T_O *>(p);
            RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
            if (res != RESULT_OK)
              return res;
            p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
            obj->dependent.rawRef_() = reinterpret_cast<gctools::tagged_pointer<gctools::MappingBase<gctools::smart_ptr<core::T_O>, gctools::smart_ptr<core::T_O> > >::Type *>(p); //reinterpret_cast<gctools::Header_s*>(p);
          }
        }
        //                    MPS_FIX12(ss,reinterpret_cast<ADDR_T*>(&(obj->bucket.raw_())));
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->bucket.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::ptag<core::T_O *>(p);
          RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
          if (res != RESULT_OK)
            return res;
          p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
          obj->bucket.setRaw_((gc::Tagged)(p)); //reinterpret_cast<gctools::Header_s*>(p);
        }
        base = (char *)base + sizeof(StrongMappingObjectType);
      } break;
      case WeakPointerKind: {
        WeakPointer *obj = reinterpret_cast<WeakPointer *>(base);
        // MPS_FIX12(ss,reinterpret_cast<ADDR_T*>(&(obj->value.raw_())));
        core::T_O *p = reinterpret_cast<core::T_O *>(obj->value.raw_());
        if (gctools::tagged_objectp(p) && MPS_FIX1(ss, p)) {
          core::T_O *pobj = gctools::untag_object<core::T_O *>(p);
          core::T_O *tag = gctools::ptag<core::T_O *>(p);
          RESULT_TYPE    res = MPS_FIX2(ss, reinterpret_cast<ADDR_T *>(&pobj));
          if (res != RESULT_OK)
            return res;
          p = reinterpret_cast<core::T_O *>(reinterpret_cast<uintptr_t>(pobj) | reinterpret_cast<uintptr_t>(tag));
          obj->value.setRaw_((gc::Tagged)(p)); //reinterpret_cast<gctools::Header_s*>(p);
        }
        base = (char *)base + sizeof(WeakPointer);
      } break;
      default:
        THROW_HARD_ERROR(BF("Handle other weak kind %d") % weakObj->kind());
      }
    };
  }
  MPS_SCAN_END(ss);
  return RESULT_OK;
}
#endif

#ifdef GC_WEAK_SKIP
ADDR_T WEAK_SKIP_DEBUG(ADDR_T base, bool dbg) {
  GCWEAK_LOG(BF("weak_obj_skip base=%p") % ((void *)base));
  WeakObject *weakObj = reinterpret_cast<WeakObject *>(base);
  switch (weakObj->kind()) {
  case WeakBucketKind: {
    WeakBucketsObjectType *obj = reinterpret_cast<WeakBucketsObjectType *>(weakObj);
    GCWEAK_LOG(BF("WeakBucketKind sizeof(WeakBucketsObjectType)=%d + sizeof(typename WeakBucketsObjectType::value_type)=%d * obj->length()=%d") % sizeof(WeakBucketsObjectType) % sizeof(typename WeakBucketsObjectType::value_type) % obj->length());
    base = (char *)base + sizeof(WeakBucketsObjectType) + sizeof(typename WeakBucketsObjectType::value_type) * obj->length();
  } break;
  case StrongBucketKind: {
    StrongBucketsObjectType *obj = reinterpret_cast<StrongBucketsObjectType *>(base);
    GCWEAK_LOG(BF("StrongBucketKind sizeof(StrongBucketsObjectType)=%d + sizeof(typename StrongBucketsObjectType::value_type)=%d * obj->length()=%d") % sizeof(StrongBucketsObjectType) % sizeof(typename StrongBucketsObjectType::value_type) % obj->length());
    base = (char *)base + sizeof(StrongBucketsObjectType) + sizeof(typename StrongBucketsObjectType::value_type) * obj->length();
  } break;
  case WeakMappingKind: {
    GCWEAK_LOG(BF("WeakMappingKind"));
    base = (char *)base + sizeof(WeakMappingObjectType);
  } break;
  case StrongMappingKind: {
    GCWEAK_LOG(BF("StrongMappingKind"));
    base = (char *)base + sizeof(StrongMappingObjectType);
  } break;
  case WeakPointerKind: {
    GCWEAK_LOG(BF("WeakPointerKind"));
    base = (char *)base + sizeof(WeakPointer);
  } break;
  case WeakFwdKind: {
    GCWEAK_LOG(BF("WeakFwdKind"));
    weak_fwd_s *obj = reinterpret_cast<weak_fwd_s *>(base);
    base = (char *)base + Align(obj->size.unsafe_fixnum());
  } break;
  case WeakFwd2Kind: {
    GCWEAK_LOG(BF("WeakFwd2Kind"));
    base = (char *)base + Align(sizeof(weak_fwd2_s));
  } break;
  case WeakPadKind: {
    GCWEAK_LOG(BF("WeakPadKind"));
    weak_pad_s *obj = reinterpret_cast<weak_pad_s *>(base);
    base = (char *)base + Align(obj->size.unsafe_fixnum());
  } break;
  case WeakPad1Kind: {
    GCWEAK_LOG(BF("WeakPad1Kind"));
    base = (char *)base + Align(sizeof(weak_pad1_s));
  }
  default:
    THROW_HARD_ERROR(BF("Handle weak_obj_skip other weak kind %d") % weakObj->kind());
  }
  GCWEAK_LOG(BF("weak_obj_skip returning base=%p") % ((void *)base));
  return base;
};
#endif

#ifdef GC_WEAK_FWD
void WEAK_FWD(ADDR_T old, ADDR_T newv) {
  WeakObject *weakObj = reinterpret_cast<WeakObject *>(old);
  ADDR_T limit = weak_obj_skip(old);
  size_t size = (char *)limit - (char *)old;
  assert(size >= Align(sizeof(weak_fwd2_s)));
  if (size == Align(sizeof(weak_fwd2_s))) {
    weak_fwd2_s *weak_fwd2_obj = reinterpret_cast<weak_fwd2_s *>(weakObj);
    weak_fwd2_obj->setKind(WeakFwd2Kind);
    weak_fwd2_obj->fwd = reinterpret_cast<WeakObject *>(newv);
  } else {
    weak_fwd_s *weak_fwd_obj = reinterpret_cast<weak_fwd_s *>(weakObj);
    weak_fwd_obj->setKind(WeakFwdKind);
    weak_fwd_obj->fwd = reinterpret_cast<WeakObject *>(newv);
    weak_fwd_obj->size = gc::make_tagged_fixnum<core::Fixnum_I *>(size);
  }
}
#endif

