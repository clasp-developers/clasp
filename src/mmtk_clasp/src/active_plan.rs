use std::ffi::c_void;

use crate::ClaspVM;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::ActivePlan;
use mmtk::Mutator;

extern "C" {
    fn clasp_get_mutator(tls: *mut c_void) -> *mut c_void;
}

use std::collections::HashSet;
use std::sync::{LazyLock, RwLock};

// Raw mutator pointer wrapped in a newtype so we can store it in a global.
// Safety invariant: each pointer is valid as long as its mutator thread is
// alive; we only dereference during stop-the-world when all mutator threads
// are suspended.
pub(crate) struct MutatorPtr(*mut Mutator<ClaspVM>);
unsafe impl Send for MutatorPtr {}
unsafe impl Sync for MutatorPtr {}
impl std::hash::Hash for MutatorPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { (self.0 as usize).hash(state); }
}
impl PartialEq for MutatorPtr {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}
impl Eq for MutatorPtr {}

pub(crate) static MUTATORS: LazyLock<RwLock<HashSet<MutatorPtr>>> =
    LazyLock::new(|| RwLock::new(HashSet::new()));

pub(crate) fn register_mutator(mutator: *mut Mutator<ClaspVM>) {
    MUTATORS.write().unwrap().insert(MutatorPtr(mutator));
}

pub(crate) fn unregister_mutator(mutator: *mut Mutator<ClaspVM>) {
    MUTATORS.write().unwrap().remove(&MutatorPtr(mutator));
}

pub struct MutatorIterator<'a> {
    _marker: std::marker::PhantomData<&'a mut Mutator<ClaspVM>>,
    keys: Vec<usize>,
    cursor: usize,
}

impl<'a> MutatorIterator<'a> {
    fn new() -> Self {
        let keys = MUTATORS.read().unwrap().iter().map(|p| p.0 as usize).collect();
        Self {
            _marker: std::marker::PhantomData,
            keys,
            cursor: 0,
        }
    }
}

impl<'a> Iterator for MutatorIterator<'a> {
    type Item = &'a mut Mutator<ClaspVM>;

    fn next(&mut self) -> Option<Self::Item> {
        let ptr = *self.keys.get(self.cursor)?;
        self.cursor += 1;
        // Safety: mutators are valid as long as their threads are alive.
        // Iteration only occurs during stop-the-world, when no mutator thread
        // is modifying its own Mutator.
        Some(unsafe { &mut *(ptr as *mut Mutator<ClaspVM>) })
    }
}

pub struct VMActivePlan;

impl ActivePlan<ClaspVM> for VMActivePlan {
    fn number_of_mutators() -> usize {
        MUTATORS.read().unwrap().len()
    }

    fn is_mutator(_tls: VMThread) -> bool {
        true
    }

    fn mutator(tls: VMMutatorThread) -> &'static mut Mutator<ClaspVM> {
        let thread_state = tls.0.0.to_address().to_mut_ptr::<c_void>();
        unsafe { &mut *(clasp_get_mutator(thread_state) as *mut Mutator<ClaspVM>) }
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut Mutator<ClaspVM>> + 'a> {
        Box::new(MutatorIterator::new())
    }
}
