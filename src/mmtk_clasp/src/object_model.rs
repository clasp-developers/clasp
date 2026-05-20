use std::ffi::c_void;

use crate::ClaspVM;
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
        VMLocalForwardingBitsSpec::in_header(0);
    const LOCAL_MARK_BIT_SPEC: VMLocalMarkBitSpec =
        VMLocalMarkBitSpec::side_first();
    const LOCAL_LOS_MARK_NURSERY_SPEC: VMLocalLOSMarkNurserySpec =
        VMLocalLOSMarkNurserySpec::side_after(Self::LOCAL_MARK_BIT_SPEC.as_spec());

    const OBJECT_REF_OFFSET_LOWER_BOUND: isize = OBJECT_REF_OFFSET as isize;

    fn copy(
        _from: ObjectReference,
        _semantics: CopySemantics,
        _copy_context: &mut GCWorkerCopyContext<ClaspVM>,
    ) -> ObjectReference {
        unimplemented!()
    }

    fn copy_to(_from: ObjectReference, _to: ObjectReference, _region: Address) -> Address {
        unimplemented!()
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
