use std::sync::atomic::{AtomicUsize, Ordering};

use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::Slot;

// Tag constants from include/clasp/gctools/configure_memory.h (TAG_BITS = 3):
//   GENERAL_TAG = 0b001 = 1
//   CONS_TAG    = 0b011 = 3
//   UNBOUND_TAG = 0b111 = 7
//   ZERO_TAG_MASK = 0x07
//
// A slot holds a GC-managed heap pointer iff (value & TAG_MASK) == GENERAL_TAG or CONS_TAG.
// All other tags (fixnums, characters, single-floats, vaslist, unbound, ...) return None.
const TAG_MASK: usize = 0b111;
const PTR_MASK: usize = !TAG_MASK;
const GENERAL_TAG: usize = 0b001;
const CONS_TAG: usize = 0b011;
const UNBOUND_TAG: usize = 0b111;

// Also from configure-memory.h:
//   DELETED_UNBOUND_BYTE = 0x18 | UNBOUND_TAG
const TAGGED_DELETED: usize = 0x1f;

/// An MMTk slot holding a Clasp tagged pointer.
///
/// `load` strips the tag to yield an ObjectReference; `store` preserves the original
/// tag when writing a (possibly relocated) ObjectReference back.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ClaspVMSlot {
    slot: *mut AtomicUsize,
}

unsafe impl Send for ClaspVMSlot {}

// Sometimes we need to know more than whether a slot contains a heap object
// or not, as for resolving ephemerons.
// So here's a basic approximation of what can be in a Clasp slot.
pub(crate) enum ClaspValue {
    Immediate,
    Unbound,
    Object(ObjectReference)
}

impl ClaspVMSlot {
    pub fn from_address(address: Address) -> Self {
        ClaspVMSlot { slot: address.to_mut_ptr::<AtomicUsize>() }
    }
    
    // Replace the value with the deleted marker (tag_deleted()).
    // Used for weak references and ephemerons.
    pub(crate) fn delete(&self) {
        unsafe { (*self.slot).store(TAGGED_DELETED, Ordering::Relaxed) };
    }

    pub(crate) fn load_value(&self) -> ClaspValue {
        let tagged = unsafe { (*self.slot).load(Ordering::Relaxed) };
        let tag = tagged & TAG_MASK;
        if tag == GENERAL_TAG || tag == CONS_TAG {
            let addr = unsafe { Address::from_usize(tagged & PTR_MASK) };
            ClaspValue::Object(ObjectReference::from_raw_address(addr).expect("null pointer should not have tag"))
        } else if tag == UNBOUND_TAG {
            ClaspValue::Unbound
        } else {
            // Just assume immediate, but maybe we should be more careful? FIXME
            ClaspValue::Immediate
        }
    }
}

impl Slot for ClaspVMSlot {
    fn load(&self) -> Option<ObjectReference> {
        let tagged = unsafe { (*self.slot).load(Ordering::Relaxed) };
        let tag = tagged & TAG_MASK;
        if tag == GENERAL_TAG || tag == CONS_TAG {
            let addr = unsafe { Address::from_usize(tagged & PTR_MASK) };
            // from_raw_address returns None for null pointers, but we should
            // crash early if we somehow have a tagged null pointer.
            Some(ObjectReference::from_raw_address(addr).expect("null pointer should not have tag"))
        } else {
            None
        }
    }

    fn store(&self, object: ObjectReference) {
        let tagged = unsafe { (*self.slot).load(Ordering::Relaxed) };
        let tag = tagged & TAG_MASK;
        let new_tagged = object.to_raw_address().as_usize() | tag;
        unsafe { (*self.slot).store(new_tagged, Ordering::Relaxed) };
    }
}
