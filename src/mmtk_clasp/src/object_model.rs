use std::ffi::c_void;

use crate::ClaspVM;
use mmtk::util::alloc::fill_alignment_gap;
use mmtk::util::copy::{CopySemantics, GCWorkerCopyContext};
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::*;

extern "C" {
    fn clasp_object_size(client: *mut c_void) -> usize;
}

/// The Clasp Header_s is 8 bytes and is placed immediately before the client
/// pointer.  MMTk ObjectReference == client pointer == alloc_start + 8.
pub const OBJECT_REF_OFFSET: usize = 8;

/// Object alignment in bytes.  TAG_BITS=3 → CLASP_ALIGNMENT=8.
pub const CLASP_ALIGNMENT: usize = 8;

pub struct VMObjectModel;

impl ObjectModel<ClaspVM> for VMObjectModel {
    const GLOBAL_LOG_BIT_SPEC: VMGlobalLogBitSpec = VMGlobalLogBitSpec::side_first();

    // Forwarding pointer and forwarding bits both live in the first header word
    // (Header_s._value). Clasp's general_mtag is zero, and objects are 8-byte
    // aligned, so it's okay for MMTk to use the low bits as a forwarding tag.
    // Note that we don't control what values MMTk puts in these bits, so they
    // may NOT match Clasp's fwd_mtag (indeed they won't, since fwd_mtag is
    // actually three bits).
    const LOCAL_FORWARDING_POINTER_SPEC: VMLocalForwardingPointerSpec =
        VMLocalForwardingPointerSpec::in_header(0);
    const LOCAL_FORWARDING_BITS_SPEC: VMLocalForwardingBitsSpec =
        VMLocalForwardingBitsSpec::side_first();
        //VMLocalForwardingBitsSpec::in_header(0);
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec =
        VMLocalMarkBitSpec::side_after(Self::LOCAL_FORWARDING_BITS_SPEC.as_spec());
//        VMLocalMarkBitSpec::side_first();
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec =
        VMLocalLOSMarkNurserySpec::side_after(Self::LOCAL_MARK_BIT_SPEC.as_spec());

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = OBJECT_REF_OFFSET as isize;

    fn copy(
        from: ObjectReference,
        semantics: CopySemantics,
        copy_context: &mut GCWorkerCopyContext<ClaspVM>,
    ) -> ObjectReference {
        let bytes = Self::get_current_size(from);
        let dst = copy_context.alloc_copy(from, bytes, CLASP_ALIGNMENT, 0, semantics);
        unsafe {
            std::ptr::copy_nonoverlapping(
                Self::ref_to_object_start(from).to_ptr::<u8>(),
                dst.to_mut_ptr::<u8>(),
                bytes,
            );
        }
        let to = unsafe { ObjectReference::from_raw_address_unchecked(dst + OBJECT_REF_OFFSET) };
        copy_context.post_copy(to, bytes, semantics);
        to
    }

    fn copy_to(from: ObjectReference, to: ObjectReference, region: Address) -> Address {
        let bytes = Self::get_current_size(from);
        // MMTk's mark-compact space can apparently copy objects to themselves
        // as it sets forwarding pointers for all objects.
        // In this case we obviously don't need to do actual copying.
        if from != to {
            let src = Self::ref_to_object_start(from);
            let dst = Self::ref_to_object_start(to);
            unsafe {
                std::ptr::copy_nonoverlapping(src.to_ptr::<u8>(), dst.to_mut_ptr::<u8>(), bytes);
            }
        }
        let start = Self::ref_to_object_start(to);
        if !region.is_zero() {
            fill_alignment_gap::<ClaspVM>(region, start);
        }
        start + bytes
    }

    fn get_current_size(object: ObjectReference) -> usize {
        unsafe { clasp_object_size(object.to_raw_address().to_mut_ptr::<c_void>()) }
    }

    fn get_size_when_copied(object: ObjectReference) -> usize {
        Self::get_current_size(object)
    }

    fn get_align_when_copied(_object: ObjectReference) -> usize {
        CLASP_ALIGNMENT
    }

    fn get_align_offset_when_copied(_object: ObjectReference) -> usize {
        0
    }

    fn get_reference_when_copied_to(_from: ObjectReference, to: Address) -> ObjectReference {
        unsafe {
            ObjectReference::from_raw_address_unchecked(to + OBJECT_REF_OFFSET)
        }
    }

    fn get_type_descriptor(_reference: ObjectReference) -> &'static [i8] {
        unimplemented!()
    }

    fn ref_to_object_start(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_REF_OFFSET)
    }

    fn ref_to_header(object: ObjectReference) -> Address {
        object.to_raw_address().sub(OBJECT_REF_OFFSET)
    }

    fn dump_object(_object: ObjectReference) {
        unimplemented!()
    }
}
