//#include <clasp/gctools/gcweak.h>

/*
 * Object scanner - include this and modify the following macros
 *  to customize this code for different purposes.
 *
 *
#define ADDR_T mps_addr_t          // Type of addresses
#define OBJECT_SCAN fixup_objects  // Name of function
#define POINTER_FIX(field)         // Macro to fix pointer at field
#define OBJECT_SCAN             // Macro to turn on #ifdef inclusion of code
#define CLIENT_PTR_TO_HEADER_PTR(client) // Replace with function to get base pointer from client
 * Macros for weak pointers and ephemerons.
 * If left undefined, weak pointers are ignored and ephemerons have their
 * values scanned. HOWEVER this is done using temporary, fake "field"
 * pointers as the weak ptrs/ephemerons may not actually contain a T_O**.
 * SO make sure you don't try to store those pointers for later!
#define WEAK_POINTER_FIX(field)
#define EPHEMERON_FIX(key, value)
 */

// !!!!! DEBUG_OBJECT_SCAN can only be on when DEBUG_GUARD_VALIDATE is on!!!!!!
// #define DEBUG_OBJECT_SCAN 1
// #define DEBUG_POINTER_BITMAPS 1

// #define DEBUG_CONTAINER_SCAN 1
// #define DEBUG_CONTAINER_POINTER_BITMAPS 1

#if (defined(DEBUG_OBJECT_SCAN) || defined(DEBUG_CONTAINER_SCAN)) && !defined(DEBUG_GUARD_VALIDATE)
#error "DEBUG_OBJECT_SCAN || DEBUG_CONTAINER_SCAN needs DEBUG_GUARD_VALIDATE to be turned on"
#endif

#ifdef OBJECT_SCAN

#ifndef EPHEMERON_FIX
#define EPHEMERON_FIX(key, value) POINTER_FIX(value)
#endif

ADDR_T OBJECT_SCAN(ADDR_T client EXTRA_ARGUMENTS) {

  size_t stamp_index;
  size_t size;
  // The client must have a valid header
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(GENERAL_PTR_TO_HEADER_PTR(client));
  const gctools::Header_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;
  stamp_index = header._badge_stamp_wtag_mtag.stamp_();
  LOG("obj_scan client={} stamp={}\n", (void*)client, stamp_index);
  switch (header_value.mtag()) {
    [[likely]] // dunno why this is indenting stupidly
  case gctools::Header_s::general_mtag: {
#ifdef DEBUG_VALIDATE_GUARD
    header->validate();
#endif
    gctools::GCStampEnum stamp_wtag = header._badge_stamp_wtag_mtag.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    if (stamp_index == STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_core__DerivableCxxObject_O)) { // wasMTAG
      // If this is true then I think we need to call virtual functions on the client
      // to determine the Instance_O offset and the total size of the object.
      printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__);
    }
    if (stamp_layout.layout_op == gctools::templated_op) {
      size = ((core::General_O*)client)->templatedSizeof();
    } else {
      size = stamp_layout.size;
    }
    if (stamp_layout.field_layout_start) {
#ifdef USE_PRECISE_GC
      if (stamp_layout.flags & gctools::COMPLEX_SCAN) {
        // This object is too big for the bitmap, or has something weird in it
        // like weak references. Scan by iterating over the fields.
        int num_fields = stamp_layout.number_of_fields;
        const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
        for (int i = 0; i < num_fields; ++i) {
          if (field_layout_cur->type == gctools::WEAK_PTR_OFFSET) [[unlikely]] {
#ifdef WEAK_POINTER_FIX
            gctools::WeakPointer* weak = (gctools::WeakPointer*)((const char*)client + field_layout_cur->offset);
            std::optional<core::T_sp> v = weak->value_no_lock();
            if (v) {
              core::T_O* raw = v->raw_();
              WEAK_POINTER_FIX(&raw);
              // Store it back in the weak pointer - this is needed for when the
              // object scanner is used in image save/load as it needs to
              // alter pointers.
              // Do not change the pointer outside of image save/load.
              weak->store_no_lock(core::T_sp((gctools::Tagged)raw));
            }
#endif
          } else if (field_layout_cur->type == gctools::EPHEMERON_OFFSET) [[unlikely]] {
            gctools::Ephemeron* eph = (gctools::Ephemeron*)((const char*)client + field_layout_cur->offset);
            auto kv = eph->get_no_lock();
            if (!kv.key.deletedp()) {
              core::T_O* rkey = kv.key.raw_();
              core::T_O* rval = kv.value.raw_();
              EPHEMERON_FIX(&rkey, &rval);
              // See comment on weak pointers above.
              eph->reinit_no_lock(core::T_sp((gctools::Tagged)rkey),
                                  core::T_sp((gctools::Tagged)rval));
            }
          } else [[likely]] {
            core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->offset);
            POINTER_FIX(field);
          }
          ++field_layout_cur;
        }
      } else {
        // Use pointer bitmaps
        uintptr_t pointer_bitmap = stamp_layout.class_field_pointer_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
        const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
        for (uintptr_t* addr = (uintptr_t*)client; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
          if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS

            core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->offset);
            if (addr != (uintptr_t*)field) {
              printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! offset=%lu\n", __FILE__,
                     __LINE__, stamp_index, client, addr, field, field_layout_cur->offset);
            }
            ++field_layout_cur;
#endif
            POINTER_FIX((core::T_O**)addr);
          }
        }
      }
#endif
    }
    if (stamp_layout.container_layout) {
      const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
      size_t capacity = std::abs(*(int64_t*)((const char*)client + container_layout.capacity_offset));
      size = container_layout.element_size * capacity + container_layout.data_offset;
      if (stamp_wtag == gctools::STAMPWTAG_core__SimpleBaseString_O)
        size = size + 1; // Add \0 for SimpleBaseString
      size_t end = *(size_t*)((const char*)client + container_layout.end_offset);
      // Use new way with pointer bitmaps
      uintptr_t start_pointer_bitmap = container_layout.container_field_pointer_bitmap;
      if (header._badge_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O)) {
        llvmo::ObjectFile_O* code = (llvmo::ObjectFile_O*)client;
        core::T_O** addr = (core::T_O**)code->literalsStart();
        core::T_O** addrEnd = addr + (code->literalsSize() / sizeof(core::T_O*));
        for (; addr < addrEnd; addr++) {
          POINTER_FIX(addr);
        }
      } else if (stamp_layout.flags & gctools::COMPLEX_SCAN) {
        const char* element = ((const char*)client + container_layout.data_offset);
        for (int i = 0; i < end; ++i, element += container_layout.element_size) {
          size_t nfields = container_layout.number_of_fields;
          const gctools::Field_layout* field_layout = container_layout.field_layout_start;
          for (size_t j = 0; j < nfields; ++j, ++field_layout) {
            const char* field = element + field_layout->offset;
            if (field_layout->type == gctools::WEAK_PTR_OFFSET) [[unlikely]] {
#ifdef WEAK_POINTER_FIX
              gctools::WeakPointer* weak = (gctools::WeakPointer*)field;
              std::optional<core::T_sp> v = weak->value_no_lock();
              if (v) {
                core::T_O* raw = v->raw_();
                WEAK_POINTER_FIX(&raw);
                weak->store_no_lock(core::T_sp((gctools::Tagged)raw));
              }
#endif
            } else if (field_layout->type == gctools::EPHEMERON_OFFSET) [[unlikely]] {
              gctools::Ephemeron* eph = (gctools::Ephemeron*)field;
              auto kv = eph->get_no_lock();
              if (!kv.key.deletedp()) {
                core::T_O* rkey = kv.key.raw_();
                core::T_O* rval = kv.value.raw_();
                EPHEMERON_FIX(&rkey, &rval);
                // See comment on weak pointers above.
                eph->reinit_no_lock(core::T_sp((gctools::Tagged)rkey),
                                    core::T_sp((gctools::Tagged)rval));
              }
            } else [[likely]] {
              core::T_O** tfield = (core::T_O**)field;
              POINTER_FIX(tfield);
            }
          }
        }
      } else if (!start_pointer_bitmap) {
        // nothing to scan
      } else if (!(start_pointer_bitmap << 1)) {
        // Trivial case - there is a single pointer to fix in every element and its the only element
        const char* element = ((const char*)client + container_layout.data_offset);
        for (int i = 0; i < end; ++i, element += container_layout.element_size) {
          uintptr_t* addr = (uintptr_t*)element;
#ifdef DEBUG_POINTER_BITMAPS
          gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
          const char* element = ((const char*)client + container_layout.data_offset + container_layout.element_size * i);
          core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->offset);
          if (addr != (uintptr_t*)field) {
            printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address "
                     "mismatch!!!! field_layout_cur->offset=%lu\n",
                   __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field,
                   field_layout_cur->offset);
          }
          ++field_layout_cur;
#endif
          POINTER_FIX((core::T_O**)addr);
        }
      } else {
        // Multiple fields we can scan with a bitmap
        const char* element = ((const char*)client + container_layout.data_offset);
        for (int i = 0; i < end; ++i, element += container_layout.element_size) {
#ifdef DEBUG_POINTER_BITMAPS
          gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
#endif
          uintptr_t pointer_bitmap = start_pointer_bitmap;
          for (uintptr_t* addr = (uintptr_t*)element; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
            if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
              const char* element = ((const char*)client + container_layout.data_offset + stamp_layout.element_size * i);
              core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->offset);
              if (addr != (uintptr_t*)field) {
                printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address "
                       "mismatch!!!! field_layout_cur->offset=%lu\n",
                       __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field,
                       field_layout_cur->offset);
              }
              ++field_layout_cur;
#endif
              POINTER_FIX((core::T_O**)addr);
            }
          }
        }
      }
    }
    client = (ADDR_T)((char*)client + gctools::AlignUp(size + sizeof(gctools::Header_s)) + header.tail_size());
  } break;
#ifdef USE_MPS
  case gctools::Header_s::pad_mtag: {
    if (header_value.pad1P()) {
      client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.pad1Size());
    } else if (header._badge_stamp_wtag_mtag.padP()) {
      client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.padSize());
    }
  } break;
#endif // USE_MPS
  default: {
    throw_hard_error_bad_client((void*)client);
  }
  }
  LOG("obj_scan ENDING client={}\n", (void*)client);
  return client;
}
#endif // OBJECT_SCAN

#ifdef OBJECT_SKIP
ADDR_T OBJECT_SKIP(ADDR_T client, size_t& obj_size) {
  const gctools::Header_s* header_ptr = reinterpret_cast<const gctools::Header_s*>(GENERAL_PTR_TO_HEADER_PTR(client));
  const gctools::Header_s& header = *header_ptr;
  const gctools::Header_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;
  switch (header_value.mtag()) {
    [[likely]]
  case gctools::Header_s::general_mtag: {
#ifdef DEBUG_VALIDATE_GUARD
    header->validate();
#endif
    gctools::GCStampEnum stamp_wtag = header._badge_stamp_wtag_mtag.stamp_wtag();
    size_t stamp_index = header._badge_stamp_wtag_mtag.stamp_();
#ifdef DEBUG_ON
    LOG("stamp_wtag = {} stamp_index={}\n", (size_t)stamp_wtag, stamp_index);
#endif
    if (stamp_wtag == gctools::STAMPWTAG_core__DerivableCxxObject_O) {
#ifdef DEBUG_ON
      LOG("DerivableCxxObject\n");
#endif
      // If this is true then I think we need to call virtual functions on the client
      // to determine the Instance_O offset and the total size of the object.
      printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__);
    }
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    const gctools::Container_layout* container_layout = stamp_layout.container_layout;
    if (container_layout) {
      // abs is there because for bignums, we use a negative length to
      // indicate that the bignum is negative.
      size_t capacity = std::abs(*(int64_t*)((const char*)client + container_layout->capacity_offset));
      if (container_layout->bits_per_bitunit) { // sub-byte array
        obj_size =
          gctools::AlignUp(gctools::bitunit_sizeof(container_layout->bits_per_bitunit, capacity) + container_layout->data_offset);
      } else if (stamp_wtag == gctools::STAMPWTAG_core__SimpleBaseString_O) [[unlikely]] { // Account for the SimpleBaseString additional byte for \0
        obj_size = gctools::AlignUp(container_layout->element_size * (capacity + 1) + container_layout->data_offset);
      } else if (stamp_wtag == gctools::STAMPWTAG_llvmo__ObjectFile_O) [[unlikely]] {
        llvmo::ObjectFile_O* code = (llvmo::ObjectFile_O*)client;
        obj_size = gctools::AlignUp(llvmo::ObjectFile_O::sizeofInState(code, code->_State));
      } else {
        obj_size = gctools::AlignUp(container_layout->element_size * capacity + container_layout->data_offset);
      }
    } else if (stamp_layout.layout_op == gctools::templated_op) {
      obj_size = gctools::AlignUp(((core::General_O*)client)->templatedSizeof());
    } else { // normal object, not a container or template or anything
      obj_size = gctools::AlignUp(stamp_layout.size);
    }
    size_t align_up_size = obj_size + sizeof(gctools::Header_s);

    client = (ADDR_T)((char*)client + align_up_size + header.tail_size());
  } break; // stampP is false, so this is something weird like a forwarding pointer
  case gctools::Header_s::pad1_mtag: {
    client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.pad1Size());
  } break;
  case gctools::Header_s::pad_mtag: {
    client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.padSize());
  } break;
  default: {
    throw_hard_error_bad_client((void*)client);
  } break;
  }
  return client;
}
#endif // OBJECT_SKIP

#ifdef GC_LISP_OBJECT_MARK

union word_ptr_ao_u {
  GC_word w;
};

typedef struct stolen_GC_ms_entry {
  char* mse_start; /* First word of object, word aligned.  */
  union word_ptr_ao_u mse_descr;
  /* Descriptor; low order two bits are tags,     */
  /* as described in gc_mark.h.                   */
} mse;
extern "C" {
GC_ms_entry* GC_signal_mark_stack_overflow(GC_ms_entry* msp);
};

#define MAYBE_MARK(taggedP)                                                                                                        \
  {                                                                                                                                \
    gctools::Tagged tagged_obj = (gctools::Tagged)(core::T_O*)*taggedP;                                                            \
    if (gctools::tagged_objectp(tagged_obj)) {                                                                                     \
      gctools::Tagged obj = gctools::untag_object<gctools::Tagged>(tagged_obj);                                                    \
      msp = GC_MARK_AND_PUSH((void*)obj, msp, msl, (void**)taggedP);                                                               \
    }                                                                                                                              \
  }

extern "C" {
int global_scan_stamp = -1;
NEVER_OPTIMIZE
struct GC_ms_entry* Lisp_object_mark(GC_word addr, struct GC_ms_entry* msp, struct GC_ms_entry* msl, GC_word env) {
  // The client must have a valid header
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(addr);
  if (header._badge_stamp_wtag_mtag._header_badge == 0)
    return msp; // If addr points to unused object residing on a free list then second word is zero
  (void)ENSURE_VALID_HEADER((void*)addr);
  void* client = (char*)addr + sizeof(gctools::Header_s);
  size_t stamp_index = header._badge_stamp_wtag_mtag.stamp_();
  const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
#ifdef DEBUG_OBJECT_SCAN
  if (global_scan_stamp == -1 || global_scan_stamp == stamp_index) {
    printf("%s:%d:%s  addr = %p client = %p stamp = %lu %s\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr, client, stamp_index,
           stamp_layout.name);
  }
#endif
  int num_fields = stamp_layout.number_of_fields;
  const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
  if (field_layout_cur) {
    for (int i = 0; i < num_fields; ++i) {
      gctools::Tagged* taggedP = (gctools::Tagged*)((const char*)client + field_layout_cur->offset);
#ifdef DEBUG_OBJECT_SCAN
      if (global_scan_stamp == -1 || global_scan_stamp == stamp_index) {
        printf("%s:%d [%d]   offset %zu %s  taggedP -> %p\n", __FILE__, __LINE__, i, field_layout_cur->offset,
               field_layout_cur->name, *(void**)taggedP);
      }
#endif
#ifdef DEBUG_GUARD_VALIDATE
      if (field_layout_cur->type == gctools::SMART_PTR_OFFSET) {
        (void)ENSURE_VALID_OBJECT((core::T_O*)taggedP);
      }
#endif
      MAYBE_MARK(taggedP);
      ++field_layout_cur;
    }
  }
  return msp;
}

NEVER_OPTIMIZE
struct GC_ms_entry* class_mark(GC_word addr, struct GC_ms_entry* msp, struct GC_ms_entry* msl, GC_word env) {
#ifdef DEBUG_OBJECT_SCAN
  printf("%s:%d:%s addr = 0x%lX\n", __FILE__, __LINE__, __FUNCTION__, addr);
#endif
  // The client must have a valid header
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>((void*)addr);
  if (header._badge_stamp_wtag_mtag._value == 0)
    return msp;
  (void)ENSURE_VALID_HEADER((void*)addr);
  void* client = (char*)addr + sizeof(gctools::Header_s);
  size_t stamp_index = header._badge_stamp_wtag_mtag.stamp_();
  const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
#ifdef DEBUG_OBJECT_SCAN
  if (global_scan_stamp == -1 || global_scan_stamp == stamp_index) {
    printf("%s:%d:%s  addr = %p client = %p stamp = %lu %s\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr, client, stamp_index,
           stamp_layout.name);
  }
#endif
  uintptr_t pointer_bitmap = stamp_layout.class_field_pointer_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
  const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
  for (uintptr_t* field_addr = (uintptr_t*)client; pointer_bitmap; field_addr++, pointer_bitmap <<= 1) {
    if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
      core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->offset);
      printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] offset=%lu %s\n", __FILE__, __LINE__, stamp_index, client,
             field_addr, field, field_layout_cur->offset, field_layout_cur->name);
      if (field_addr != (uintptr_t*)field) {
        printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! offset=%lu\n", __FILE__, __LINE__,
               stamp_index, client, field_addr, field, field_layout_cur->offset);
        abort();
      }
      ++field_layout_cur;
#endif
      MAYBE_MARK(field_addr);
    }
  }
  return msp;
}

#if 0
struct GC_ms_entry* dumb_class_container_mark(GC_word addr,
                                              struct GC_ms_entry* msp,
                                              struct GC_ms_entry* msl,
                                              GC_word env)
{
#define MARK_WORK 8
    // The client must have a valid header
  gctools::Header_s& header = *reinterpret_cast<gctools::Header_s *>((void*)addr);
  env = header._boehm_mark_work;
  if (header._badge_stamp_wtag_mtag._value == 0 ) return msp;
  (void)ENSURE_VALID_HEADER((void*)addr);
  void* client = (char*)addr + sizeof(gctools::Header_s);
  void* next_client = obj_skip(client);
  void* next_base = gctools::GeneralPtrToHeaderPtr(next_client);
  printf("%s:%d addr = %p next_base = %p stamp: %s  num = %lu env = %lu\n", __FILE__, __LINE__, (void*)addr, next_base, obj_name(header.stamp_()), ((GC_word*)next_base-(GC_word*)addr), env);
  GC_word* start = (GC_word*)client + env;
  GC_word* end = (GC_word*)start + MARK_WORK;
  if (end < (GC_word*)next_base) {
    header._boehm_mark_work = env + MARK_WORK;
  } else {
    end = (GC_word*)next_base;
    header._boehm_mark_work = 0;
  }
  for ( GC_word* ptr = start; ptr < end; ptr++ ) {
    gctools::Tagged tagged_obj = (gctools::Tagged)*ptr; 
    if (gctools::tagged_objectp(tagged_obj)) {                            
      gctools::Tagged obj = gctools::untag_object<gctools::Tagged>(tagged_obj); 
      msp = ((GC_word)(obj) >= (GC_word)GC_least_plausible_heap_addr && 
             (GC_word)(obj) <= (GC_word)GC_greatest_plausible_heap_addr ? 
             GC_mark_and_push((void*)obj, msp, msl, (void**)ptr) : (msp));
    } 
  }
  return msp;
}
#endif

#if 0
/* class_container_mark
 * Marks pointers within container objects that contain pointers to mark!
 * This function shouldn't be invoked UNLESS there are pointers within the container part of the 
 * object that need to be marked.
 * In boehm - we can only do a certain amount of marking work per call of this function.
 * So we need to carefully keep track of how much work is done
*/
  struct GC_ms_entry* class_container_mark(GC_word addr,
                                     struct GC_ms_entry* msp,
                                     struct GC_ms_entry* msl,
                                     GC_word env)
  {
    // The client must have a valid header
    gctools::Header_s& header = *reinterpret_cast<gctools::Header_s *>((void*)addr);
    if (header._badge_stamp_wtag_mtag._value == 0 ) return msp;
    (void)ENSURE_VALID_HEADER((void*)addr);
    void* client = (char*)addr + sizeof(gctools::Header_s);
    const gctools::Header_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;
    size_t stamp_index = header.stamp_();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    env = header._boehm_mark_work;
#ifdef DEBUG_CONTAINER_SCAN
    if (global_scan_stamp==-1 || global_scan_stamp == stamp_index) {
      printf("%s:%d:%s  addr = %p env = %lu client = %p stamp = %lu %s\n", __FILE__, __LINE__,__FUNCTION__, (void*)addr, env, client, stamp_index, stamp_layout.name );
    }
#endif
    gctools::tagged_stamp_t mtag = header_value.mtag();
    gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
    int idx = 0;
    uintptr_t pointer_bitmap = stamp_layout.boehm._class_bitmap;
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
    const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
    // Only mark the class fields if env == 0 
    if (!env) {
      for (uintptr_t* field_bitmap_addr = (uintptr_t*)client; pointer_bitmap; field_bitmap_addr++, pointer_bitmap<<=1) {
        if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
          core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->offset);
          printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] offset=%lu %s\n", __FILE__, __LINE__, stamp_index, client, field_bitmap_addr, field, field_layout_cur->offset, field_layut_cur->name);
          if (field_bitmap_addr != (uintptr_t*)field) {
            printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] field_bitmap_addr mismatch!!!! offset=%lu\n", __FILE__, __LINE__, stamp_index, client, field_bitmap_addr, field, field_layout_cur->offset);
            abort();
          }
          ++field_layout_cur;
#endif
          MAYBE_MARK(field_bitmap_addr);
        }
      }
    }
    // Now mark the container pointers
    const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
    size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
    size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
#ifdef DEBUG_CONTAINER_SCAN
    printf("%s:%d Container size = %lu\n", __FILE__, __LINE__, end );
#endif
    // Use new way with pointer bitmaps
    uintptr_t start_pointer_bitmap = container_layout.container_field_pointer_bitmap;
    if (start_pointer_bitmap) {
#if 0 // OPTIMIZATION - TURN OFF FOR NOW
      if (!(start_pointer_bitmap<<1)) {
        // Trivial case - there is a single pointer to fix in every element and its the only element
        const char* element = ((const char*)client + stamp_layout.data_offset);
        for ( size_t i=0; i<end; ++i, element += (stamp_layout.element_size)) {
          uintptr_t* field_only_addr = (uintptr_t*)element;
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
          printf("%s:%d container marking [%lu<%lu] %p\n", __FILE__, __LINE__, i, end, (void*)field_only_addr);
#endif
          MAYBE_MARK(field_only_addr);
        }
      } else
#endif
      {
        // The contents of the container are more complicated - so use the bitmap to scan pointers within them
        const char* element_start = ((const char*)client + stamp_layout.data_offset + (stamp_layout.element_size * env));
        int work_to_do = stamp_layout.boehm._container_element_work;
        for ( size_t i=env; i<end; ++i, element_start += (stamp_layout.element_size)) {
          // THIS IS WHERE WE BREAK THE WORK INTO CHUNKS AND UPDATE ENV
          if (work_to_do) {
            uintptr_t pointer_bitmap = start_pointer_bitmap;
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
            printf("%s:%d:%s Marking element[%lu/%lu] addr=%p :  chunks work_to_do= %d env = %lu\n", __FILE__, __LINE__, __FUNCTION__, i, end, (void*)addr, work_to_do, env );
            gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
#endif
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
            printf("%s:%d container pointer bitmap: 0x%lx marking from %p\n", __FILE__, __LINE__, pointer_bitmap, (void*)element_start);
#endif
            for (uintptr_t* element_field_bitmap_addr = (uintptr_t*)element_start; pointer_bitmap; element_field_bitmap_addr++, pointer_bitmap<<=1) {
              if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
                printf("%s:%d container marking %p\n", __FILE__, __LINE__, (void*)element_field_bitmap_addr);
#endif
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
                const char* elementdbg = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
                core::T_O** field = (core::T_O**)((const char*)elementdbg + field_layout_cur->offset);
                if (element_field_bitmap_addr != (uintptr_t*)field) {
                  printf("%s:%d stamp: %lu elementdbg@%p i = %lu start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address mismatch!!!! field_layout_cur->offset=%lu %s\n",
                         __FILE__, __LINE__, stamp_index, elementdbg, i, start_pointer_bitmap, element_field_bitmap_addr, field, field_layout_cur->offset, field_layout_cur->name);
                }
                ++field_layout_cur;
#endif
                MAYBE_MARK(element_field_bitmap_addr);
              }
            }
          } else {
            // Update work_done and return msp
#if 1
            // simple mark and push and store work done in header
            size_t new_env = i;
            header._boehm_mark_work = i;
            msp = GC_mark_and_push((void*)client, msp, msl, (void**)addr);
#else
            if (i<end) {
              msp++;
              if ((GC_word)msp >= (GC_word)msl) {
                msp = GC_signal_mark_stack_overflow(msp);
              }
              stolen_GC_ms_entry* stolen_msp = (stolen_GC_ms_entry*)msp;
              stolen_msp->mse_start = (char*)addr;
              size_t new_env = i; // env+stamp_layout.boehm._container_element_work;
              stolen_msp->mse_descr.w = GC_MAKE_PROC(gctools::global_container_proc_index,new_env);
              printf("%s:%d Hacking the GC_ms_entry stack - pushing container back on stack with global_container_proc_index: %d  env %lu\nj", __FILE__, __LINE__, gctools::global_container_proc_index, new_env );
            // msp = GC_mark_and_push((void*)client,msp,msl,(void**)NULL);
            // printf("%s:%d You need to update the amount of work you have done in env = %lu\n", __FILE__, __LINE__, env );
            }
#endif
            goto DONE;
          }
          --work_to_do;
        }
      }
    }
    // Zero out work_done
    header._boehm_mark_work = 0;
  DONE:
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
    printf("%s:%d:%s Leaving addr = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr);
#endif    
    return msp;
  }

#endif
}
#endif // #ifdef GC_LISP_OBJECT_MARK
