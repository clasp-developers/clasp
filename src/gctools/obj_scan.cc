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
 * Macros for weak pointers and ephemerons.
 * If left undefined, weak pointers are ignored and ephemerons have their
 * values scanned. HOWEVER this is done using temporary, fake "field"
 * pointers as the weak ptrs/ephemerons may not actually contain a T_O**.
 * SO make sure you don't try to store those pointers for later!
#define WEAK_POINTER_FIX(field)
#define EPHEMERON_FIX(key, value)
 */

#ifdef OBJECT_SCAN

#ifndef EPHEMERON_FIX
#define EPHEMERON_FIX(key, value) POINTER_FIX(value)
#endif

void OBJECT_SCAN(ADDR_T client EXTRA_ARGUMENTS) {

  size_t stamp_index;
  // The client must have a valid header
  core::General_O* general = reinterpret_cast<core::General_O*>(client);
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s*>(gctools::GeneralPtrToHeaderPtr(general));
  const gctools::Header_s::BadgeStampWtagMtag& header_value = header._badge_stamp_wtag_mtag;
  stamp_index = header._badge_stamp_wtag_mtag.stamp_();
  switch (header_value.mtag()) {
    [[likely]] // dunno why this is indenting stupidly
  case gctools::Header_s::general_mtag: {
    gctools::GCStampEnum stamp_wtag = header._badge_stamp_wtag_mtag.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
    if (stamp_index == STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_core__DerivableCxxObject_O)) { // wasMTAG
      // If this is true then I think we need to call virtual functions on the client
      // to determine the Instance_O offset and the total size of the object.
      printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__);
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
        for (uintptr_t* addr = (uintptr_t*)client; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
          if ((intptr_t)pointer_bitmap < 0) {
            POINTER_FIX((core::T_O**)addr);
          }
        }
      }
#endif
    }
    if (stamp_layout.container_layout) {
      const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
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
          POINTER_FIX((core::T_O**)addr);
        }
      } else {
        // Multiple fields we can scan with a bitmap
        const char* element = ((const char*)client + container_layout.data_offset);
        for (int i = 0; i < end; ++i, element += container_layout.element_size) {
          uintptr_t pointer_bitmap = start_pointer_bitmap;
          for (uintptr_t* addr = (uintptr_t*)element; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
            if ((intptr_t)pointer_bitmap < 0) {
              POINTER_FIX((core::T_O**)addr);
            }
          }
        }
      }
    }
  } break;
  default: {
    throw_hard_error_bad_client((void*)client);
  }
  }
}
#endif // OBJECT_SCAN
