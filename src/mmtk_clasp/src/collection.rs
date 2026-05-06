use crate::mmtk;
use crate::ClaspVM;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::Collection;
use mmtk::vm::GCThreadContext;
use mmtk::Mutator;

pub struct VMCollection;

impl Collection<ClaspVM> for VMCollection {
    fn stop_all_mutators<F>(_tls: VMWorkerThread, _mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<ClaspVM>),
    {
        unimplemented!()
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unimplemented!()
    }

    fn block_for_gc(_tls: VMMutatorThread) {
        unimplemented!()
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<ClaspVM>) {
        // We drop the thread JoinHandle as we expect the worker thread
        // to run until the process quits.
        let _ = std::thread::Builder::new()
            .name("MMTk GC Worker".to_string())
            .spawn(move || {
                let worker_tls =
                    VMWorkerThread(VMThread(OpaquePointer::UNINITIALIZED));
                match ctx {
                    GCThreadContext::Worker(w) => {
                        mmtk::memory_manager::start_worker(mmtk(), worker_tls, w);
                    }
                }
            })
            .expect("Failed to spawn GC worker thread");
    }
}
