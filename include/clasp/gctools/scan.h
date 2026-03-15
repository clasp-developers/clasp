#pragma once

/* This file contains the scanning interface. This lets you map over the fields
 * of any Lisp object, fields being pointers to other Lisp objects.
 * This is useful for ROOM, snapshots, and precise garbage collection (though is
 * not used for this in Boehm).
 * scan::cons and scan::general are the main interfaces. They accept an object
 * and a "fixer" callback. This fixer is called on each field (i.e. a T_O**) and
 * its return value is ignored. scan::general also accepts callbacks to call on
 * WeakPointer* and Ephemeron* fields.
 * scan::general_pointers is an additional interface that scans a general object
 * but only accepts the fixer callback and not the others. The fixer callback is
 * used synthetically on weak pointers and ephemeron key and values, i.e. a
 * temporary T_O* is constructed, the fixer is called, and then the value is
 * stored back into the weak pointer of ephemeron, to emulate any modifications.
 * Since the T_O* is temporary, the fixer must not store the T_O**, as it will
 * soon be invalid.
 */

#include <concepts>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/memoryManagement.h>

namespace gctools {

class scan {
public:
  template <std::invocable<core::T_O**> Fixer>
  static void cons(core::Cons_O* client, Fixer&& fix) {
    gctools::Header_s& header = *(gctools::Header_s*)gctools::ConsPtrToHeaderPtr(client);
    if (header._badge_stamp_wtag_mtag.consObjectP()) {
      fix((core::T_O**)&client->_Car);
      fix((core::T_O**)&client->_Cdr);
    } else if (!header._badge_stamp_wtag_mtag.fwdP()) {
      printf("%s:%d CONS in cons_scan client=%p\n(it's not a CONS or any of fwd car=%p "
           "cdr=%p\n",
             __FILE__, __LINE__, (void*)client, client->car().raw_(), client->cdr().raw_());
      abort();
    }
  }
  
  template <std::invocable<core::T_O**> Fixer,
            std::invocable<WeakPointer*> WeakFixer,
            std::invocable<Ephemeron*> EphFixer>
  static void general(core::General_O* client, Fixer&& fix,
                      WeakFixer&& weakfix, EphFixer&& ephfix) {
    const gctools::Header_s& header = *(const gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(client);
    size_t stamp_index = header._badge_stamp_wtag_mtag.stamp_();
    switch (header._badge_stamp_wtag_mtag.mtag()) {
      [[likely]] // dunno why this is indenting stupidly
    case gctools::Header_s::general_mtag: {
      gctools::GCStampEnum stamp_wtag = header._badge_stamp_wtag_mtag.stamp_wtag();
      const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];
      if (stamp_index == STAMP_UNSHIFT_WTAG(gctools::STAMPWTAG_core__DerivableCxxObject_O)) { // wasMTAG
        // If this is true then I think we need to call virtual functions on the client
        // to determine the Instance_O offset and the total size of the object.
        printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__);
      }
      // Basic fields
      if (stamp_layout.field_layout_start) {
#ifdef USE_PRECISE_GC
        if (stamp_layout.flags & gctools::COMPLEX_SCAN) {
          // This object is too big for the bitmap, or has something weird in it
          // like weak references. Scan by iterating over the fields.
          int num_fields = stamp_layout.number_of_fields;
          const gctools::Field_layout* field_layout_cur = stamp_layout.field_layout_start;
          for (int i = 0; i < num_fields; ++i, ++field_layout_cur) {
            void* field = (void*)((const char*)client + field_layout_cur->offset);
            switch (field_layout_cur->type) {
              [[unlikely]]
            case WEAK_PTR_OFFSET: weakfix((WeakPointer*)field); break;
                [[unlikely]]
            case EPHEMERON_OFFSET: ephfix((Ephemeron*)field); break;
                [[likely]] // normal field
            default: fix((core::T_O**)field); break;
            }
          }
        } else {
          // Use pointer bitmaps
          uintptr_t pointer_bitmap = stamp_layout.class_field_pointer_bitmap;
          for (core::T_O** addr = (core::T_O**)client; pointer_bitmap;
               addr++, pointer_bitmap <<= 1) {
            if ((intptr_t)pointer_bitmap < 0) { // checking high bit
              fix(addr);
            }
          }
        }
#endif // USE_PRECISE_GC
      }
      // Container fields
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
            fix(addr);
          }
        } else if (stamp_layout.flags & gctools::COMPLEX_SCAN) {
          const char* element = ((const char*)client + container_layout.data_offset);
          for (int i = 0; i < end; ++i, element += container_layout.element_size) {
            size_t nfields = container_layout.number_of_fields;
            const gctools::Field_layout* field_layout = container_layout.field_layout_start;
            for (size_t j = 0; j < nfields; ++j, ++field_layout) {
              void* field = (void*)(element + field_layout->offset);
              switch (field_layout->type) {
                [[unlikely]]
              case WEAK_PTR_OFFSET: weakfix((WeakPointer*)field); break;
                  [[unlikely]]
              case EPHEMERON_OFFSET: ephfix((Ephemeron*)field); break;
                  [[likely]] // normal field
              default: fix((core::T_O**)field); break;
              }
            }
          }
        } else if (!start_pointer_bitmap) {
          // nothing to scan
        } else if (!(start_pointer_bitmap << 1)) {
          // Trivial case - there is a single pointer to fix in every element and its the only element
          const char* element = ((const char*)client + container_layout.data_offset);
          for (int i = 0; i < end; ++i, element += container_layout.element_size) {
            fix((core::T_O**)element);
          }
        } else {
          // Multiple fields we can scan with a bitmap
          const char* element = ((const char*)client + container_layout.data_offset);
          for (int i = 0; i < end; ++i, element += container_layout.element_size) {
            uintptr_t pointer_bitmap = start_pointer_bitmap;
            for (core::T_O** addr = (core::T_O**)element; pointer_bitmap; addr++, pointer_bitmap <<= 1) {
              if ((intptr_t)pointer_bitmap < 0) {
                fix((core::T_O**)addr);
              }
            }
          }
        }
      }
    } break;
    default: throw_hard_error_bad_client((void*)client);
    }
  }

private:
  template <std::invocable<core::T_O**> Fixer>
  static void weak_shim(gctools::WeakPointer* weak, Fixer&& fix) {
    std::optional<core::T_sp> v = weak->value_no_lock();
    if (v) {
      core::T_O* raw = v->raw_();
      fix(&raw);
      // Store it back in the weak pointer - this is needed for when the
      // object scanner is used in image save/load as it needs to
      // alter pointers.
      // Do not change the pointer outside of image save/load.
      weak->store_no_lock(core::T_sp((gctools::Tagged)raw));
    }
  }

  template <std::invocable<core::T_O**> Fixer>
  static void eph_shim(gctools::Ephemeron* eph, Fixer&& fix) {
    auto kv = eph->get_no_lock();
    if (!kv.key.deletedp()) {
      core::T_O* rkey = kv.key.raw_();
      core::T_O* rval = kv.value.raw_();
      fix(&rkey); fix(&rval);
      // See comment on weak pointers above.
      eph->reinit_no_lock(core::T_sp((gctools::Tagged)rkey),
                          core::T_sp((gctools::Tagged)rval));
    }
  }

public:
  template <std::invocable<core::T_O**> Fixer>
  static void general_pointers(core::General_O* client, Fixer&& fix) {
    general(client, fix,
            [&](WeakPointer* weak) { weak_shim(weak, fix); },
            [&](Ephemeron* eph) { eph_shim(eph, fix); });
  }
};

}; // namespace gctools
