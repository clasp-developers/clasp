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

#include <bit>
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
    gctools::ConsHeader_s& header = *(gctools::ConsHeader_s*)gctools::ConsPtrToHeaderPtr(client);
    fix((core::T_O**)&client->_Car);
    fix((core::T_O**)&client->_Cdr);
  }

private:
  template <std::invocable<core::T_O**> Fixer>
  static void fix_bitmap(core::T_O** base, uintptr_t bitmap, Fixer&& fix) {
    while (bitmap) {
      int pos = std::countl_zero(bitmap); // find first 1 bit (field)
      // optimization note: based on the disassembly, it seems clang is smart enough
      // to see that bitmap != 0 and so it can use bsr (on x86) no problem.
      fix(base + pos); // scan it
      // Then clear that bit so we can get the next field or exit.
      bitmap ^= (uintptr_t)1 << ((sizeof(uintptr_t)*CHAR_BIT) - 1 - pos);
    }
  }

  template <std::invocable<core::T_O**> Fixer,
            std::invocable<WeakPointer*> WeakFixer,
            std::invocable<QueueableWeakReference*> QueueableFixer,
            std::invocable<Ephemeron*> EphFixer>
  static void fix_complex(core::T_O** base, int num_fields,
                          const gctools::Field_layout* field_layout,
                          Fixer&& fix, WeakFixer&& weakfix,
                          QueueableFixer&& queueablefix, EphFixer&& ephfix) {
    for (int i = 0; i < num_fields; ++i, ++field_layout) {
      void* field = (void*)((const char*)base + field_layout->offset);
      switch (field_layout->type) {
      case WEAK_PTR_OFFSET: weakfix((WeakPointer*)field); break;
      case QWEAK_OFFSET: queueablefix((QueueableWeakReference*)field); break;
      case EPHEMERON_OFFSET: ephfix((Ephemeron*)field); break;
      default: fix((core::T_O**)field); break;
      }
    }
  }

public:
  template <std::invocable<core::T_O**> Fixer,
            std::invocable<WeakPointer*> WeakFixer,
            std::invocable<QueueableWeakReference*> QueueableFixer,
            std::invocable<Ephemeron*> EphFixer>
  static void general(core::General_O* client, Fixer&& fix,
                      WeakFixer&& weakfix, QueueableFixer queueablefix,
                      EphFixer&& ephfix) {
    const gctools::Header_s& header = *(const gctools::Header_s*)gctools::GeneralPtrToHeaderPtr(client);
    size_t stamp_index = header._badge_stamp_wtag_mtag.stamp_();

    // Try the fastest possible thing first - get the simple bitmap, and if it
    // hasn't been set to ~0 it's valid (or is coincidentally ~0, but whatever).
    {
      uintptr_t fast_bitmap = gctools::global_stamp_bitmaps[stamp_index];
      if (fast_bitmap != ~(uintptr_t)0) [[likely]] {
        fix_bitmap((core::T_O**)client, fast_bitmap, fix);
        return; // done!
      }
    }

    // Slightly slower path using the fuller stamp layouts.
    // Ideally we can still use a bitmap, but there might be a container
    // and/or weird fields.
    gctools::GCStampEnum stamp_wtag = header._badge_stamp_wtag_mtag.stamp_wtag();
    const gctools::Stamp_layout& stamp_layout = gctools::global_stamp_layout[stamp_index];

    // First we check for complex scan. This is unusual so we don't care so much
    // about its performance, we just want it out of the way so that the faster
    // paths can be good.
    // We only use complex scan when there are unusual fields (e.g. weak pointers)
    // or if the object has too many fixed fields (> 63) to fit in a bitmap.
    if (stamp_layout.flags & gctools::COMPLEX_SCAN) [[unlikely]] {
      fix_complex((core::T_O**)client, stamp_layout.number_of_fields,
                  stamp_layout.field_layout_start,
                  fix, weakfix, queueablefix, ephfix);
      // now container fields.
      if (stamp_layout.container_layout) {
        const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
        size_t end = *(size_t*)((const char*)client + container_layout.end_offset);
        const char* element = ((const char*)client + container_layout.data_offset);
        for (int i = 0; i < end; ++i, element += container_layout.element_size)
          fix_complex((core::T_O**)element, container_layout.number_of_fields,
                      container_layout.field_layout_start,
                      fix, weakfix, queueablefix, ephfix);
      }
    } else {
      // Use pointer bitmaps.
      fix_bitmap((core::T_O**)client,
                 stamp_layout.class_field_pointer_bitmap,
                 fix);
      if (stamp_layout.container_layout) {
        // evil special case. FIXME
        if (header._badge_stamp_wtag_mtag._value
            == DO_SHIFT_STAMP(gctools::STAMPWTAG_llvmo__ObjectFile_O))
          [[unlikely]] {
          llvmo::ObjectFile_O* code = (llvmo::ObjectFile_O*)client;
          core::T_O** addr = (core::T_O**)code->literalsStart();
          core::T_O** addrEnd = addr + (code->literalsSize() / sizeof(core::T_O*));
          for (; addr < addrEnd; addr++) fix(addr);
          return;
        }
        // back to normal case
        const gctools::Container_layout& container_layout = *stamp_layout.container_layout;
        size_t end = *(size_t*)((const char*)client + container_layout.end_offset);
        const char* element = ((const char*)client + container_layout.data_offset);
        uintptr_t bitmap = container_layout.container_field_pointer_bitmap;
        if (!bitmap) [[unlikely]] { // no fields, do nothing.
          // note that this should be covered by fast bitmaps (above)
          // and is only included here for correctness.
        } else if (!(bitmap << 1)) [[likely]] {
          // only one field, which is common enough (e.g. simple vector) that
          // we try to handle it directly.
          for (int i = 0; i < end; ++i, element += container_layout.element_size)
            fix((core::T_O**)element);
        } else {
          for (int i = 0; i < end; ++i, element += container_layout.element_size)
            fix_bitmap((core::T_O**)element, bitmap, fix);
        }
      }
    }
  }

private:
  template <std::invocable<core::T_O**> Fixer>
  static void weak_shim(gctools::WeakPointer* weak, Fixer&& fix) {
    std::optional<core::T_sp> v = weak->value_no_lock();
    if (v) {
#ifdef USE_BOEHM
      core::T_O* raw = v->raw_();
      fix(&raw);
      // Store it back in the weak pointer - this is needed for when the
      // object scanner is used in image save/load as it needs to
      // alter pointers.
      // Do not change the pointer outside of image save/load.
      weak->store_no_lock(core::T_sp((gctools::Tagged)raw));
#else
      fix((core::T_O**)&weak->_value);
#endif
    }
  }

  template <std::invocable<core::T_O**> Fixer>
  static void qweak_shim(gctools::QueueableWeakReference* qweak, Fixer&& fix) {
    std::optional<core::T_sp> v = qweak->value_no_lock();
    if (v) {
#ifdef USE_BOEHM
      core::T_O* raw = v->raw_();
      fix(&raw);
      qweak->store_no_lock(core::T_sp((gctools::Tagged)raw));
#else
      fix((core::T_O**)&qweak->_referent);
#endif
    }
  }

  template <std::invocable<core::T_O**> Fixer>
  static void eph_shim(gctools::Ephemeron* eph, Fixer&& fix) {
    auto kv = eph->get_no_lock();
    if (!kv.key.deletedp()) {
#ifdef USE_BOEHM
      core::T_O* rkey = kv.key.raw_();
      core::T_O* rval = kv.value.raw_();
      fix(&rkey); fix(&rval);
      // See comment on weak pointers above.
      eph->reinit_no_lock(core::T_sp((gctools::Tagged)rkey),
                          core::T_sp((gctools::Tagged)rval));
#else
      // FIXME: do we need the deletedp check before this? I don't think so?
      // Resolve once things are stable with MMTk
      fix((core::T_O**)&eph->_key);
      fix((core::T_O**)&eph->_value);
#endif
    }
  }

public:
  template <std::invocable<core::T_O**> Fixer>
  static void general_pointers(core::General_O* client, Fixer&& fix) {
    general(client, fix,
            [&](WeakPointer* weak) { weak_shim(weak, fix); },
            [&](QueueableWeakReference* q) { qweak_shim(q, fix); },
            [&](Ephemeron* eph) { eph_shim(eph, fix); });
  }
};

}; // namespace gctools
