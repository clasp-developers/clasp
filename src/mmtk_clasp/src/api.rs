#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::active_plan::{register_mutator, unregister_mutator};
use crate::mmtk;
use crate::ClaspVM;
use crate::SINGLETON;
use crate::scanning::WeakPointer;
use crate::scanning::WEAK_POINTERS;
use crate::scanning::Ephemeron;
use crate::scanning::EPHEMERONS;
use crate::scanning::QueueableWeakReference;
use crate::scanning::QWEAKS;
use crate::scanning::QWEAKS_RESURRECT;
use libc::c_char;
use mmtk::memory_manager;
use mmtk::plan::Mutator;
use mmtk::scheduler::GCWorker;
use mmtk::util::alloc::Allocator;
use mmtk::util::alloc::ImmixAllocator;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::AllocationSemantics;
use mmtk::MMTKBuilder;
use std::ffi::CStr;

#[no_mangle]
pub extern "C" fn mmtk_clasp_create_builder() -> *mut MMTKBuilder {
    Box::into_raw(Box::new(MMTKBuilder::new()))
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_set_option(
    builder: *mut MMTKBuilder,
    name: *const c_char,
    value: *const c_char,
) -> bool {
    let builder = unsafe { &mut *builder };
    let name_str = unsafe { CStr::from_ptr(name) }.to_str().unwrap();
    let value_str = unsafe { CStr::from_ptr(value) }.to_str().unwrap();
    builder.set_option(name_str, value_str)
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_set_fixed_heap_size(
    builder: *mut MMTKBuilder,
    heap_size: usize,
) -> bool {
    let builder = unsafe { &mut *builder };
    builder
        .options
        .gc_trigger
        .set(mmtk::util::options::GCTriggerSelector::FixedHeapSize(
            heap_size,
        ))
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_set_dynamic_heap_size(
    builder: *mut MMTKBuilder,
    min_heap: usize,
    max_heap: usize,
) -> bool {
    let builder = unsafe { &mut *builder };
    builder
        .options
        .gc_trigger
        .set(mmtk::util::options::GCTriggerSelector::DynamicHeapSize(
            min_heap, max_heap,
        ))
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_init(builder: *mut MMTKBuilder) {
    let builder = unsafe { Box::from_raw(builder) };
    let mmtk_instance = memory_manager::mmtk_init::<ClaspVM>(&builder);
    SINGLETON
        .set(mmtk_instance)
        .unwrap_or_else(|_| panic!("MMTk already initialized"));
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_max_default_alloc_bytes(
) -> usize {
    mmtk().get_plan().constraints().max_non_los_default_alloc_bytes
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_initialize_collection(tls: VMThread) {
    memory_manager::initialize_collection(mmtk(), tls);
}

/// Prepare for a `fork()`. Stops the MMTk GC worker threads and blocks until
/// their underlying native threads have exited, so the process is safe to fork.
/// After `fork()` returns, `mmtk_clasp_after_fork` must be called in BOTH the
/// parent and the child. The VM must not allocate on the MMTk heap between this
/// call and `mmtk_clasp_after_fork`.
#[no_mangle]
pub extern "C" fn mmtk_clasp_prepare_to_fork() {
    // Asynchronously request the workers to save context and exit...
    mmtk().prepare_to_fork();
    // ...then wait for their native threads to actually terminate.
    crate::collection::join_all_gc_worker_threads();
}

/// Re-spawn the MMTk GC worker threads after a `fork()`. Must be called in both
/// the parent and the child once `fork()` returns (paired with a preceding
/// `mmtk_clasp_prepare_to_fork`).
#[no_mangle]
pub extern "C" fn mmtk_clasp_after_fork(tls: VMThread) {
    mmtk().after_fork(tls);
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_bind_mutator(tls: VMMutatorThread) -> *mut Mutator<ClaspVM> {
    let mutator = Box::into_raw(memory_manager::bind_mutator(mmtk(), tls));
    register_mutator(mutator);
    mutator
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_destroy_mutator(mutator: *mut Mutator<ClaspVM>) {
    unregister_mutator(mutator);
    memory_manager::destroy_mutator(unsafe { &mut *mutator });
    // Create a Box from the raw pointer which immediately goes
    // out of scope. Box going out of scope frees the pointed object.
    let _ = unsafe { Box::from_raw(mutator) };
}

/// Returns the byte offset from a Mutator pointer to its default (Immix) allocator.
/// This is constant for a given plan and VM binding — compute it once at mutator
/// bind time and reuse it for every allocation to skip semantic dispatch.
#[no_mangle]
pub extern "C" fn mmtk_clasp_get_default_allocator_offset() -> usize {
    let selector = memory_manager::get_allocator_mapping(mmtk(), AllocationSemantics::Default);
    Mutator::<ClaspVM>::get_allocator_base_offset(selector)
}

/// Fast-path alloc for Default semantics: bypasses AllocationSemantics dispatch by
/// accessing the ImmixAllocator directly via the pre-computed offset.
#[no_mangle]
pub extern "C" fn mmtk_clasp_alloc_immix(
    mutator: *mut Mutator<ClaspVM>,
    immix_offset: usize,
    size: usize,
    align: usize,
) -> Address {
    debug_assert!(size < mmtk().get_plan().constraints().max_non_los_default_alloc_bytes);
    let allocator = unsafe {
        (Address::from_ptr(mutator) + immix_offset).as_mut_ref::<ImmixAllocator<ClaspVM>>()
    };
    allocator.alloc(size, align, 0)
}

/// Fast-path post_alloc for Default semantics: skips the LOS redirect check in
/// mmtk_clasp_post_alloc and calls directly with Default, relying on the caller
/// having already verified the object is below the LOS threshold.
#[no_mangle]
pub extern "C" fn mmtk_clasp_post_alloc_immix(
    mutator: *mut Mutator<ClaspVM>,
    object: ObjectReference,
    bytes: usize,
) {
    debug_assert!(bytes < mmtk().get_plan().constraints().max_non_los_default_alloc_bytes);
    memory_manager::post_alloc::<ClaspVM>(
        unsafe { &mut *mutator },
        object,
        bytes,
        AllocationSemantics::Default,
    );
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_alloc(
    mutator: *mut Mutator<ClaspVM>,
    size: usize,
    align: usize,
    mut semantics: AllocationSemantics,
) -> Address {
    if size >= mmtk().get_plan().constraints().max_non_los_default_alloc_bytes {
        semantics = AllocationSemantics::Los;
    }
    memory_manager::alloc::<ClaspVM>(unsafe { &mut *mutator }, size, align, 0, semantics)
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_post_alloc(
    mutator: *mut Mutator<ClaspVM>,
    object: ObjectReference,
    bytes: usize,
    mut semantics: AllocationSemantics,
) {
    if bytes >= mmtk().get_plan().constraints().max_non_los_default_alloc_bytes {
        semantics = AllocationSemantics::Los;
    }
    memory_manager::post_alloc::<ClaspVM>(unsafe { &mut *mutator }, object, bytes, semantics)
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_scan_weak(
    weak: ObjectReference,
    field_offset: usize,
) {
    let ptr = WeakPointer { object: weak, offset: field_offset };
    WEAK_POINTERS.lock().unwrap().push(ptr);
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_scan_ephemeron(
    eph: ObjectReference,
    key_offset: usize,
    value_offset: usize,
) {
    let eph = Ephemeron { object: eph, key: key_offset, value: value_offset };
    EPHEMERONS.lock().unwrap().push(eph);
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_scan_qweak(
    weak: ObjectReference,
    offset: usize,
    resurrectp: bool,
) {
    let weak = QueueableWeakReference { object: weak, offset: offset };
    if resurrectp {
        QWEAKS_RESURRECT.lock().unwrap().push(weak);
    } else {
        QWEAKS.lock().unwrap().push(weak);
    }
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_start_worker(
    tls: VMWorkerThread,
    worker: *mut GCWorker<ClaspVM>,
) {
    let worker = unsafe { Box::from_raw(worker) };
    memory_manager::start_worker::<ClaspVM>(mmtk(), tls, worker)
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_used_bytes() -> usize {
    memory_manager::used_bytes(mmtk())
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_free_bytes() -> usize {
    memory_manager::free_bytes(mmtk())
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_total_bytes() -> usize {
    memory_manager::total_bytes(mmtk())
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_is_live_object(object: ObjectReference) -> bool {
    memory_manager::is_live_object(object)
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_will_never_move(object: ObjectReference) -> bool {
    !object.is_movable()
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_is_in_mmtk_spaces(object: ObjectReference) -> bool {
    memory_manager::is_in_mmtk_spaces(object)
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_is_mapped_address(address: Address) -> bool {
    memory_manager::is_mapped_address(address)
}

#[cfg(feature = "vo_bit")]
#[no_mangle]
pub extern "C" fn mmtk_clasp_is_mmtk_object(address: Address) -> bool {
    memory_manager::is_mmtk_object(address).is_some()
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_handle_user_collection_request(tls: VMMutatorThread) {
    memory_manager::handle_user_collection_request::<ClaspVM>(mmtk(), tls);
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_starting_heap_address() -> Address {
    memory_manager::starting_heap_address()
}

#[no_mangle]
pub extern "C" fn mmtk_clasp_last_heap_address() -> Address {
    memory_manager::last_heap_address()
}
