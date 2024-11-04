

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
#define OBJECT_SCAN             // Macro to turn on #ifdef inclusion of code
#define RESULT_TYPE                // Result type
#define RESULT_OK                  // value to return on OK - MPS_RES_OK
#define CLIENT_PTR_TO_HEADER_PTR(client) // Replace with function to get base pointer from client
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
#ifdef OBJECT_SKIP_IN_OBJECT_SCAN
ADDR_T OBJECT_SKIP_IN_OBJECT_SCAN(ADDR_T client, bool dbg, size_t& objectSize);
#endif // OBJECT_SKIP_IN_OBJECT_SCAN

RESULT_TYPE OBJECT_SCAN(SCAN_STRUCT_T ss, ADDR_T client, ADDR_T limit EXTRA_ARGUMENTS) {
#ifdef DEBUG_OBJECT_SCAN
//  printf("%s:%d obj_scan client = %p  limit = %p\n", __FILE__, __LINE__, client, limit );
#endif

  ADDR_T oldClient;
  size_t stamp_index;
  size_t size;
  SCAN_BEGIN(ss) {
    while (client < limit) {
      oldClient = (ADDR_T)client;
      // The client must have a valid header
      const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(GENERAL_PTR_TO_HEADER_PTR(client));
      const gctools::Header_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;
      stamp_index = header._badge_stamp_wtag_mtag.stamp_();
      LOG("obj_scan client={} stamp={}\n", (void*)client, stamp_index);
      if (header._badge_stamp_wtag_mtag.stampP()) {
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
          // Handle Lisp object specially because it's bitmask will be too large
#ifdef USE_PRECISE_GC
          if (stamp_index == STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_core__Lisp)) { // wasMTAG
            int num_fields = stamp_layout.number_of_fields;
            const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
            core::T_O** prevField = NULL;
            for (int i = 0; i < num_fields; ++i) {
              core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
              if (field == prevField) {
                printf("%s:%d:%s ---- scanning object %p stamp %lu field %p is about to be POINTER_FIXed for a second time\n",
                       __FILE__, __LINE__, __FUNCTION__, (void*)client, stamp_index, field);
                printf("%s:%d:%s field_layout_cur = %p  field_layout_cur->field_offset = %p field_layout_cur->field_offset = %lu\n",
                       __FILE__, __LINE__, __FUNCTION__, field_layout_cur, &field_layout_cur->field_offset,
                       field_layout_cur->field_offset);
                printf("%s:%d:%s prev field_layout_cur = %p  prev &field_layout_cur->field_offset = %p prev "
                       "field_layout_cur->field_offset = %lu\n",
                       __FILE__, __LINE__, __FUNCTION__, (field_layout_cur - 1), &(field_layout_cur - 1)->field_offset,
                       (field_layout_cur - 1)->field_offset);
                abort();
              }
              POINTER_FIX(field);
              prevField = field;
              ++field_layout_cur;
            }
          } else {
#if 1
            // Use pointer bitmaps
            uintptr_t pointer_bitmap = stamp_layout.class_field_pointer_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
            const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
            for (uintptr_t* addr = (uintptr_t*)client; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
              if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS

                core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
                if (addr != (uintptr_t*)field) {
                  printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! field_offset=%lu\n", __FILE__,
                         __LINE__, stamp_index, client, addr, field, field_layout_cur->field_offset);
                }
                ++field_layout_cur;
#endif
                POINTER_FIX((core::T_O**)addr);
              }
            }
#else
            int num_fields = stamp_layout.number_of_fields;
            const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
            for (int i = 0; i < num_fields; ++i) {
              core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
              POINTER_FIX(field);
              ++field_layout_cur;
            }
#endif
          }
#endif
        }
        if (stamp_layout.container_layout) {
#ifdef DEBUG_POINTER_BITMAPS
          const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
#endif
          size_t capacity = std::abs(*(int64_t*)((const char*)client + stamp_layout.capacity_offset));
          size = stamp_layout.element_size * capacity + stamp_layout.data_offset;
          if (stamp_wtag == gctools::STAMPWTAG_core__SimpleBaseString_O)
            size = size + 1; // Add \0 for SimpleBaseString
          size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
          // Use new way with pointer bitmaps
          uintptr_t start_pointer_bitmap = stamp_layout.container_layout->container_field_pointer_bitmap;
          if (header._badge_stamp_wtag_mtag._value == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O)) {
            //            printf("%s:%d:%s obj_scan for Code_o object with header %p\n", __FILE__, __LINE__, __FUNCTION__, &header
            //            );
            llvmo::ObjectFile_O* code = (llvmo::ObjectFile_O*)client;
            core::T_O** addr = (core::T_O**)code->literalsStart();
            core::T_O** addrEnd = addr + (code->literalsSize() / sizeof(core::T_O*));
            for (; addr < addrEnd; addr++) {
              POINTER_FIX(addr);
            }
          } else {
            if (start_pointer_bitmap) {
              if (!(start_pointer_bitmap << 1)) {
                // Trivial case - there is a single pointer to fix in every element and its the only element
                const char* element = ((const char*)client + stamp_layout.data_offset);
                for (int i = 0; i < end; ++i, element += (stamp_layout.element_size)) {
                  uintptr_t* addr = (uintptr_t*)element;
#ifdef DEBUG_POINTER_BITMAPS
                  gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
                  const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size * i);
                  core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
                  if (addr != (uintptr_t*)field) {
                    printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address "
                           "mismatch!!!! field_layout_cur->field_offset=%lu\n",
                           __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field,
                           field_layout_cur->field_offset);
                  }
                  ++field_layout_cur;
#endif
                  POINTER_FIX((core::T_O**)addr);
                }
              } else {
                // The contents of the container are more complicated - so use the bitmap to scan pointers within them
                const char* element = ((const char*)client + stamp_layout.data_offset);
                for (int i = 0; i < end; ++i, element += (stamp_layout.element_size)) {
#ifdef DEBUG_POINTER_BITMAPS
                  gctools::Field_layout* field_layout_cur = container_layout.field_layout_start;
#endif
                  uintptr_t pointer_bitmap = start_pointer_bitmap;
                  for (uintptr_t* addr = (uintptr_t*)element; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
                    if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
                      const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size * i);
                      core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
                      if (addr != (uintptr_t*)field) {
                        printf("%s:%d stamp: %lu element@%p i = %d start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address "
                               "mismatch!!!! field_layout_cur->field_offset=%lu\n",
                               __FILE__, __LINE__, stamp_index, element, i, start_pointer_bitmap, addr, field,
                               field_layout_cur->field_offset);
                      }
                      ++field_layout_cur;
#endif
                      POINTER_FIX((core::T_O**)addr);
                    }
                  }
                }
              }
            }
          }
        }
        client = (ADDR_T)((char*)client + gctools::AlignUp(size + sizeof(gctools::Header_s)) + header.tail_size());
      } else {
        gctools::tagged_stamp_t mtag = header_value.mtag();
        switch (mtag) {
#ifdef USE_MPS
        case gctools::Header_s::fwd_mtag: {
          client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.fwdSize());
          break;
        }
        case gctools::Header_s::pad_mtag: {
          if (header_value.pad1P()) {
            client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.pad1Size());
          } else if (header._badge_stamp_wtag_mtag.padP()) {
            client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.padSize());
          }
          break;
        }
#endif // USE_MPS
        case gctools::Header_s::invalid0_mtag:
        case gctools::Header_s::invalid1_mtag: {
          throw_hard_error_bad_client((void*)client);
        }
        }
      }
    }
  }
  SCAN_END(ss);
  LOG("obj_scan ENDING client={}\n", (void*)client);
  return RESULT_OK;
}
#endif // OBJECT_SCAN

#ifdef OBJECT_SKIP
ADDR_T OBJECT_SKIP(ADDR_T client, bool dbg, size_t& obj_size) {
  const gctools::Header_s* header_ptr = reinterpret_cast<const gctools::Header_s*>(GENERAL_PTR_TO_HEADER_PTR(client));
  const gctools::Header_s& header = *header_ptr;
  const gctools::Header_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;
  if (header._badge_stamp_wtag_mtag.stampP()) {
#ifdef DEBUG_VALIDATE_GUARD
    header->validate();
#endif
    gctools::GCStampEnum stamp_wtag = header._badge_stamp_wtag_mtag.stamp_wtag();
    size_t stamp_index = header._badge_stamp_wtag_mtag.stamp_();
#ifdef DEBUG_ON
    if (dbg) {
      LOG("stamp_wtag = {} stamp_index={}\n", (size_t)stamp_wtag, stamp_index);
    }
#endif
    if (stamp_wtag == gctools::STAMPWTAG_core__DerivableCxxObject_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("DerivableCxxObject\n");
      }
#endif
      // If this is true then I think we need to call virtual functions on the client
      // to determine the Instance_O offset and the total size of the object.
      printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__);
    }
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    unlikely_if(stamp_wtag == gctools::STAMPWTAG_core__SimpleBitVector_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("SimpleBitVector\n");
      }
#endif
      size_t capacity = std::abs(*(int64_t*)((const char*)client + stamp_layout.capacity_offset));
      obj_size =
          gctools::AlignUp(core::SimpleBitVector_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset);
      goto STAMP_CONTINUE;
      // Do other bitunit vectors here
    }
    unlikely_if(stamp_wtag == gctools::STAMPWTAG_core__SimpleVector_byte2_t_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("STAMP_core__SimpleVector_byte2_t_O");
      }
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      obj_size = gctools::AlignUp(core::SimpleVector_byte2_t_O::bitunit_array_type::sizeof_for_length(capacity) +
                                  stamp_layout.data_offset);
      goto STAMP_CONTINUE;
    }
    unlikely_if(stamp_wtag == gctools::STAMPWTAG_core__SimpleVector_int2_t_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("STAMPWTAG_core__SimpleVector_int2_t_O");
      }
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      obj_size =
          gctools::AlignUp(core::SimpleVector_int2_t_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset);
      goto STAMP_CONTINUE;
    }
    unlikely_if(stamp_wtag == gctools::STAMPWTAG_core__SimpleVector_byte4_t_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("STAMP_core__SimpleVector_byte4_t_O");
      }
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      obj_size = gctools::AlignUp(core::SimpleVector_byte4_t_O::bitunit_array_type::sizeof_for_length(capacity) +
                                  stamp_layout.data_offset);
      goto STAMP_CONTINUE;
    }
    unlikely_if(stamp_wtag == gctools::STAMPWTAG_core__SimpleVector_int4_t_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("STAMP_core__SimpleVector_int4_t_O");
      }
#endif
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      obj_size =
          gctools::AlignUp(core::SimpleVector_int4_t_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset);
      goto STAMP_CONTINUE;
    }
    unlikely_if(stamp_wtag == gctools::STAMPWTAG_core__SimpleBaseString_O) {
#ifdef DEBUG_ON
      if (dbg) {
        LOG("SimpleBaseString\n");
      }
#endif
      // Account for the SimpleBaseString additional byte for \0
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset) + 1;
      obj_size = gctools::AlignUp(stamp_layout.element_size * capacity + stamp_layout.data_offset);
      goto STAMP_CONTINUE;
    }
    {
      gctools::Container_layout* container_layoutP = stamp_layout.container_layout;
      if (container_layoutP) {
#ifdef DEBUG_ON
        if (dbg) {
          LOG("container_layout\n");
        }
#endif
        // special cases
        if (stamp_wtag == gctools::STAMPWTAG_llvmo__ObjectFile_O) {
          llvmo::ObjectFile_O* code = (llvmo::ObjectFile_O*)client;
          obj_size = gctools::AlignUp(llvmo::ObjectFile_O::sizeofInState(code, code->_State));
        } else {
          // For bignums we allow the _MaybeSignedLength(capacity) to be a negative value to represent negative bignums
          // because GMP only stores positive bignums.  So the value at stamp_layout.capacity_offset is a signed int64_t
          // Because of this we need to take the absolute value to get the number of entries.
          size_t capacity = (size_t)std::llabs(*(int64_t*)((const char*)client + stamp_layout.capacity_offset));
          obj_size = gctools::AlignUp(stamp_layout.element_size * capacity + stamp_layout.data_offset);
        }
      } else {
        if (stamp_layout.layout_op == gctools::templated_op) {
#ifdef DEBUG_ON
          if (dbg) {
            LOG("templatedSizeof\n");
          }
#endif
          obj_size = gctools::AlignUp(((core::General_O*)client)->templatedSizeof());
        } else {
#ifdef DEBUG_ON
          if (dbg) {
            LOG("stamp_layout.size = %lu\n", stamp_layout.size);
          }
#endif
          obj_size = gctools::AlignUp(stamp_layout.size);
        }
      }
    }
  STAMP_CONTINUE:
    size_t align_up_size = obj_size + sizeof(gctools::Header_s);

    client = (ADDR_T)((char*)client + align_up_size + header.tail_size());
  } else {
    gctools::tagged_stamp_t mtag = header_value.mtag();
    switch (mtag) {
    case gctools::Header_s::fwd_mtag: {
      client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.fwdSize());
      break;
    }
    case gctools::Header_s::pad1_mtag: {
      client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.pad1Size());
      break;
    }
    case gctools::Header_s::pad_mtag: {
      client = (ADDR_T)((char*)(client) + header._badge_stamp_wtag_mtag.padSize());
      break;
    }
    case gctools::Header_s::invalid0_mtag:
    case gctools::Header_s::invalid1_mtag: {
      throw_hard_error_bad_client((void*)client);
      break;
    }
    }
  }
  return client;
}
#endif // OBJECT_SKIP

#ifdef OBJECT_FWD
static void OBJECT_FWD(ADDR_T old_client, ADDR_T new_client) {
  // I'm assuming both old and new client pointers have valid headers at this point
  DEBUG_THROW_IF_INVALID_CLIENT(old_client);
  DEBUG_THROW_IF_INVALID_CLIENT(new_client);
  size_t obj_size;
  ADDR_T limit = OBJECT_SKIP_IN_OBJECT_FWD(old_client, false, obj_size);
  size_t size = (char*)limit - (char*)old_client;
  if (size < gctools::global_sizeof_fwd) {
    THROW_HARD_ERROR("obj_fwd needs size >= {}", gctools::global_sizeof_fwd);
  }
  gctools::Header_s* header = (gctools::Header_s*)(GENERAL_PTR_TO_HEADER_PTR(old_client));
  header->_badge_stamp_wtag_mtag.setFwdSize(size);
  header->_badge_stamp_wtag_mtag.setFwdPointer((void*)new_client);
}

#endif // OBJECT_FWD

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
#ifdef DEBUG_GUARD_VALIDATE
  const gctools::Stamp_info& stamp_info = gctools::global_stamp_info[stamp_index];
#endif
#ifdef DEBUG_OBJECT_SCAN
  if (global_scan_stamp == -1 || global_scan_stamp == stamp_index) {
    printf("%s:%d:%s  addr = %p client = %p stamp = %lu %s\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr, client, stamp_index,
           stamp_info.name);
  }
#endif
#ifdef DEBUG_GUARD_VALIDATE
  const gctools::Field_info* field_info_cur = stamp_info.field_info_ptr;
#endif
  const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
  int num_fields = stamp_layout.number_of_fields;
  const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
  if (field_layout_cur) {
    for (int i = 0; i < num_fields; ++i) {
      gctools::Tagged* taggedP = (gctools::Tagged*)((const char*)client + field_layout_cur->field_offset);
#ifdef DEBUG_OBJECT_SCAN
      if (global_scan_stamp == -1 || global_scan_stamp == stamp_index) {
        printf("%s:%d [%d]   offset %zu %s  taggedP -> %p\n", __FILE__, __LINE__, i, field_layout_cur->field_offset,
               field_info_cur->field_name, *(void**)taggedP);
      }
#endif
#ifdef DEBUG_GUARD_VALIDATE
      if (field_info_cur->data_type == gctools::SMART_PTR_OFFSET) {
        (void)ENSURE_VALID_OBJECT((core::T_O*)taggedP);
      }
      ++field_info_cur;
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
#ifdef DEBUG_OBJECT_SCAN
  const gctools::Stamp_info& stamp_info = gctools::global_stamp_info[stamp_index];
  if (global_scan_stamp == -1 || global_scan_stamp == stamp_index) {
    printf("%s:%d:%s  addr = %p client = %p stamp = %lu %s\n", __FILE__, __LINE__, __FUNCTION__, (void*)addr, client, stamp_index,
           stamp_info.name);
  }
  const gctools::Field_info* field_info_cur = stamp_info.field_info_ptr;
#endif
  const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
  uintptr_t pointer_bitmap = stamp_layout.boehm._class_bitmap;
#ifdef DEBUG_POINTER_BITMAPS
  const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
#endif
  for (uintptr_t* field_addr = (uintptr_t*)client; pointer_bitmap; field_addr++, pointer_bitmap <<= 1) {
    if ((intptr_t)pointer_bitmap < 0) {
#ifdef DEBUG_POINTER_BITMAPS
      core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
      printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] field_offset=%lu %s\n", __FILE__, __LINE__, stamp_index, client,
             field_addr, field, field_layout_cur->field_offset, field_info_cur->name);
      ++field_info;
      if (field_addr != (uintptr_t*)field) {
        printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] address mismatch!!!! field_offset=%lu\n", __FILE__, __LINE__,
               stamp_index, client, field_addr, field, field_layout_cur->field_offset);
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
    env = header._boehm_mark_work;
#ifdef DEBUG_CONTAINER_SCAN
    const gctools::Stamp_info& stamp_info = gctools::global_stamp_info[stamp_index];
    if (global_scan_stamp==-1 || global_scan_stamp == stamp_index) {
      printf("%s:%d:%s  addr = %p env = %lu client = %p stamp = %lu %s\n", __FILE__, __LINE__,__FUNCTION__, (void*)addr, env, client, stamp_index, stamp_info.name );
    }
    const gctools::Field_info* field_info_cur = stamp_info.field_info_ptr;
#endif
    gctools::tagged_stamp_t mtag = header_value.mtag();
    gctools::GCStampEnum stamp_wtag = header.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
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
          core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
          printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] field_offset=%lu %s\n", __FILE__, __LINE__, stamp_index, client, field_bitmap_addr, field, field_layout_cur->field_offset, field_info_cur->field_name);
          ++field_info_cur;
          if (field_bitmap_addr != (uintptr_t*)field) {
            printf("%s:%d stamp: %lu client@%p bitmap[%p]/field[%p] field_bitmap_addr mismatch!!!! field_offset=%lu\n", __FILE__, __LINE__, stamp_index, client, field_bitmap_addr, field, field_layout_cur->field_offset);
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
#ifdef DEBUG_CONTAINER_POINTER_BITMAPS
    gctools::Container_info& container_info = *stamp_info.container_info_ptr;
#endif
    size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
    size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
#ifdef DEBUG_CONTAINER_SCAN
    printf("%s:%d Container size = %lu\n", __FILE__, __LINE__, end );
#endif
    // Use new way with pointer bitmaps
    uintptr_t start_pointer_bitmap = stamp_layout.boehm._container_bitmap;
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
            gctools::Container_info* container_info_cur = stamp_info.container_info_ptr;
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
                core::T_O** field = (core::T_O**)((const char*)elementdbg + field_layout_cur->field_offset);
                if (element_field_bitmap_addr != (uintptr_t*)field) {
                  printf("%s:%d stamp: %lu elementdbg@%p i = %lu start_pointer_bitmap=0x%lX bitmap[%p]/field[%p] address mismatch!!!! field_layout_cur->field_offset=%lu %s\n",
                         __FILE__, __LINE__, stamp_index, elementdbg, i, start_pointer_bitmap, element_field_bitmap_addr, field, field_layout_cur->field_offset, container_info_cur->field_name);
                }
                ++field_layout_cur;
                ++container_info_cur;
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
