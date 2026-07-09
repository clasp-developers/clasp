#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::active_plan::{register_mutator, unregister_mutator};
use crate::mmtk;
use crate::ClaspVM;
use crate::SINGLETON;
use libc::c_char;
use mmtk::memory_manager;
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::AllocationSemantics;
use mmtk::MMTKBuilder;
use mmtk::Mutator;
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
pub extern "C" fn mmtk_clasp_initialize_collection(tls: VMThread) {
    memory_manager::initialize_collection(mmtk(), tls);
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
