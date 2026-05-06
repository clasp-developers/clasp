use crate::ClaspVM;
use crate::ClaspVMSlot;
use mmtk::util::opaque_pointer::*;
use mmtk::util::ObjectReference;
use mmtk::vm::RootsWorkFactory;
use mmtk::vm::Scanning;
use mmtk::vm::SlotVisitor;
use mmtk::Mutator;

pub struct VMScanning;

impl Scanning<ClaspVM> for VMScanning {
    fn scan_roots_in_mutator_thread(
        _tls: VMWorkerThread,
        _mutator: &'static mut Mutator<ClaspVM>,
        _factory: impl RootsWorkFactory<ClaspVMSlot>,
    ) {
        unimplemented!()
    }

    fn scan_vm_specific_roots(
        _tls: VMWorkerThread,
        _factory: impl RootsWorkFactory<ClaspVMSlot>,
    ) {
        unimplemented!()
    }

    fn scan_object<SV: SlotVisitor<ClaspVMSlot>>(
        _tls: VMWorkerThread,
        _object: ObjectReference,
        _slot_visitor: &mut SV,
    ) {
        unimplemented!()
    }

    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
        unimplemented!()
    }

    fn supports_return_barrier() -> bool {
        false
    }

    fn prepare_for_roots_re_scanning() {
        unimplemented!()
    }
}
