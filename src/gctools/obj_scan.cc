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
