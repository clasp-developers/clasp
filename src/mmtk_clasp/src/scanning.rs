use std::ffi::c_void;

use crate::ClaspVM;
use crate::ClaspVMSlot;
use crate::slot::ClaspValue::*;
use mmtk::scheduler::EDGES_WORK_BUFFER_SIZE;
use mmtk::scheduler::GCWorker;
use mmtk::util::opaque_pointer::*;
use mmtk::util::{Address, ObjectReference};
use mmtk::vm::RootsWorkFactory;
use mmtk::vm::Scanning;
use mmtk::vm::slot::Slot;
use mmtk::vm::SlotVisitor;
use mmtk::vm::ObjectTracer;
use mmtk::vm::ObjectTracerContext;
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
    fn clasp_scan_object(
        client: *mut c_void,
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

// Structure for gctools::Ephemeron.
// TODO: ideally we would just use the actual ephemeron, but then, ideally we
// would be scanning in Rust instead of using a callback.
pub(crate) struct Ephemeron {
    pub(crate) key: ClaspVMSlot,
    pub(crate) value: ClaspVMSlot
}

// Vec of Ephemerons that need processing.
// This is added to during scanning and partially emptied by process_weak_refs.
// The mutex is because multiple scan workers may need to add to it.
// TODO?: Would be less contentious if each GC worker could have its own Vec.
pub(crate) static EPHEMERONS: std::sync::Mutex<Vec<Ephemeron>>
    = std::sync::Mutex::new(Vec::new());

// Vec of WeakPointer (the C++ class) that need processing.
// This is added to during scanning and emptied by process_weak_refs.
// The mutex is because multiple scan workers may need to add to it.
pub(crate) static WEAK_POINTERS: std::sync::Mutex<Vec<ClaspVMSlot>>
    = std::sync::Mutex::new(Vec::new());

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
        object: ObjectReference,
        slot_visitor: &mut SV,
    ) {
        // Monomorphised trampoline: each instantiation of scan_object gets its
        // own field_cb that calls visit_slot on the concrete SV type directly.
        unsafe extern "C" fn field_cb<SV: SlotVisitor<ClaspVMSlot>>(
            slot_addr: *mut c_void,
            data: *mut c_void,
        ) {
            let visitor = &mut *(data as *mut SV);
            visitor.visit_slot(ClaspVMSlot::from_address(Address::from_usize(
                slot_addr as usize,
            )));
        }
        unsafe {
            clasp_scan_object(
                object.to_raw_address().to_mut_ptr::<c_void>(),
                field_cb::<SV>,
                slot_visitor as *mut SV as *mut c_void,
            );
        }
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {}

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {}

    fn process_weak_refs(worker: &mut GCWorker<ClaspVM>,
                         tracer_context: impl ObjectTracerContext<ClaspVM>,
    ) -> bool {
        let mut trace_again = false;
        // Process ephemerons
        let mut ephs = EPHEMERONS.lock().unwrap();
        tracer_context.with_tracer(worker, |tracer| {
            let mut resolve = |eph: &mut Ephemeron| {
                // This ephemeron has a live key, so forward the value
                // and note that there may be new live objects to trace.
                // If load() returns None it's immediate or something
                // so we don't need to do anything.
                if let Some(val) = eph.value.load() {
                    tracer.trace_object(val);
                    trace_again = true;
                    eph.value.store(val.get_forwarded_object().unwrap_or(val));
                }
            };
            // Iterate over EPHEMERONS, retaining only those whose keys are not
            // known to be alive. Any ephemerons with live keys add to a new trace
            // and forward objects before they are removed from EPHEMERONS.
            ephs.retain_mut(|eph| {
                match eph.key.load_value() {
                    // deleted - make sure the value is deleted as well.
                    Unbound => {
                        eph.value.delete();
                        false
                    }
                    // immediate - alive forever, therefore value is too
                    Immediate => {
                        resolve(eph);
                        false
                    }
                    Object(key) => {
                        if key.is_reachable() {
                            // forward key if we need to
                            eph.key.store(key.get_forwarded_object().unwrap_or(key));
                            // set up to trace value
                            resolve(eph);
                            false
                        } else {
                            // key does not seem to be reachable, but tracing
                            // another ephemeron's value may make it reachable,
                            // so delay.
                            true
                        }
                    }
                }
            });
        });
        // We've now run through all the ephemerons. If any of them added new
        // values to trace, any seemingly dead ephemeron keys may now be
        // found to be alive,
        // so leave the ephemerons in EPHEMERONS and run another trace.
        if trace_again == true { return true; }
        // Otherwise, there is no longer any way for ephemeron keys to turn up
        // alive, so we're done with ephemerons. Their keys are all dead
        // so delete their keys and values, and clear EPHEMERONS.
        for dead in ephs.drain(..) {
            dead.key.delete();
            dead.value.delete();
        }

        // Process weak pointers
        let mut weaks = WEAK_POINTERS.lock().unwrap();
        for weak_slot in weaks.drain(..) {
            // If the referent is deleted or an immediate we don't care about it.
            // during scanning we shouldn't even collect them, but just in case.
            if let Some(obj) = weak_slot.load() {
                if obj.is_reachable() {
                    // object is reachable, so forward the ref if need be
                    weak_slot.store(obj.get_forwarded_object().unwrap_or(obj));
                } else {
                    // object is dead so splat the pointer
                    weak_slot.delete();
                }
            }
        }
        false
    }
}
