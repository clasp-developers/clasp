use std::ffi::c_void;

use crate::ClaspVM;
use crate::ClaspVMSlot;
use mmtk::scheduler::EDGES_WORK_BUFFER_SIZE;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::RootsWorkFactory;
use mmtk::vm::Scanning;
use mmtk::vm::SlotVisitor;
use mmtk::Mutator;
use mmtk::MutatorContext;

extern "C" {
    fn clasp_walk_global_roots(
        callback: unsafe extern "C" fn(*mut c_void, *mut c_void),
        data: *mut c_void,
    );
    fn clasp_walk_thread_precise_roots(
        tls: *mut c_void,
        callback: unsafe extern "C" fn(*mut c_void, *mut c_void),
        data: *mut c_void,
    );
    fn clasp_walk_thread_conservative_roots(
        tls: *mut c_void,
        callback: unsafe extern "C" fn(*mut c_void, *mut c_void),
        data: *mut c_void,
    );
}

unsafe extern "C" fn precise_root_cb(slot: *mut c_void, data: *mut c_void) {
    let slots = &mut *(data as *mut Vec<ClaspVMSlot>);
    slots.push(ClaspVMSlot::from_address(Address::from_usize(slot as usize)));
}

unsafe extern "C" fn conservative_root_cb(client_ptr: *mut c_void, data: *mut c_void) {
    let roots = &mut *(data as *mut Vec<ObjectReference>);
    if let Some(obj) =
        ObjectReference::from_raw_address(Address::from_usize(client_ptr as usize))
    {
        roots.push(obj);
    }
}

fn report_precise_roots(slots: Vec<ClaspVMSlot>, factory: &mut impl RootsWorkFactory<ClaspVMSlot>) {
    for chunk in slots.chunks(EDGES_WORK_BUFFER_SIZE) {
        factory.create_process_roots_work(chunk.to_vec());
    }
}

fn report_pinning_roots(roots: Vec<ObjectReference>, factory: &mut impl RootsWorkFactory<ClaspVMSlot>) {
    for chunk in roots.chunks(EDGES_WORK_BUFFER_SIZE) {
        factory.create_process_pinning_roots_work(chunk.to_vec());
    }
}

pub struct VMScanning;

impl Scanning<ClaspVM> for VMScanning {
    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        mutator: &'static mut Mutator<ClaspVM>,
        mut factory: impl RootsWorkFactory<ClaspVMSlot>,
    ) {
        // Get the ThreadLocalState* as a void* from the mutator object.
        // This is apparently the correct way to do it, ugly as it is.
        let thread_state = mutator.get_tls().0.0.to_address().to_mut_ptr::<c_void>();

        let mut precise_slots: Vec<ClaspVMSlot> = Vec::new();
        unsafe {
            clasp_walk_thread_precise_roots(
                thread_state,
                precise_root_cb,
                &mut precise_slots as *mut _ as *mut c_void,
            );
        }
        report_precise_roots(precise_slots, &mut factory);

        let mut pinning_roots: Vec<ObjectReference> = Vec::new();
        unsafe {
            clasp_walk_thread_conservative_roots(
                thread_state,
                conservative_root_cb,
                &mut pinning_roots as *mut _ as *mut c_void,
            );
        }
        report_pinning_roots(pinning_roots, &mut factory);
    }

    fn scan_vm_specific_roots(
        _tls: VMWorkerThread,
        mut factory: impl RootsWorkFactory<ClaspVMSlot>,
    ) {
        let mut precise_slots: Vec<ClaspVMSlot> = Vec::new();
        unsafe {
            clasp_walk_global_roots(
                precise_root_cb,
                &mut precise_slots as *mut _ as *mut c_void,
            );
        }
        report_precise_roots(precise_slots, &mut factory);
    }

    fn scan_object<SV: SlotVisitor<ClaspVMSlot>>(
        _tls: VMWorkerThread,
        _object: ObjectReference,
        _slot_visitor: &mut SV,
    ) {
        unimplemented!()
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {}

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}
}
