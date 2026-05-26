use std::sync::atomic::{AtomicUsize, Ordering};

use mmtk::util::{Address, ObjectReference};
use mmtk::vm::slot::Slot;

// Tag constants from include/clasp/gctools/configure_memory.h (TAG_BITS = 3):
//   GENERAL_TAG = 0b001 = 1
//   CONS_TAG    = 0b011 = 3
//   ZERO_TAG_MASK = 0x07
//
// A slot holds a GC-managed heap pointer iff (value & TAG_MASK) == GENERAL_TAG or CONS_TAG.
// All other tags (fixnums, characters, single-floats, vaslist, unbound, ...) return None.
const TAG_MASK: usize = 0b111;
const PTR_MASK: usize = !TAG_MASK;
const GENERAL_TAG: usize = 0b001;
const CONS_TAG: usize = 0b011;

/// An MMTk slot holding a Clasp tagged pointer.
///
/// `load` strips the tag to yield an ObjectReference; `store` preserves the original
/// tag when writing a (possibly relocated) ObjectReference back.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ClaspVMSlot {
    slot: *mut AtomicUsize,
}

unsafe impl Send for ClaspVMSlot {}

impl ClaspVMSlot {
    pub fn from_address(address: Address) -> Self {
        ClaspVMSlot { slot: address.to_mut_ptr::<AtomicUsize>() }
    }
}

impl Slot for ClaspVMSlot {
    fn load(&self) -> Option<ObjectReference> {
        let tagged = unsafe { (*self.slot).load(Ordering::Relaxed) };
        let tag = tagged & TAG_MASK;
        if tag == GENERAL_TAG || tag == CONS_TAG {
            ObjectReference::from_raw_address(unsafe { Address::from_usize(tagged & PTR_MASK) })
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
