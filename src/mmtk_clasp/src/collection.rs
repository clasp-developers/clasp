use crate::active_plan::VMActivePlan;
use crate::mmtk;
use crate::ClaspVM;
use mmtk::util::alloc::AllocationError;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::ActivePlan;
use mmtk::vm::Collection;
use mmtk::vm::GCThreadContext;
use mmtk::Mutator;

extern "C" {
    fn clasp_stop_the_world();
    fn clasp_resume_the_world();
    fn clasp_pause_thread_for_gc();
    fn clasp_mask_signals_for_alien();
}

pub struct VMCollection;

impl Collection<ClaspVM> for VMCollection {
    fn out_of_memory(_tls: VMThread, err_kind: AllocationError) {
        eprintln!(
            "clasp-mmtk: out of memory ({:?}) — no GC plan collected anything. \
             Increase heap size or switch to a collecting GC plan.",
            err_kind
        );
        std::process::abort();
    }

    fn stop_all_mutators<F>(_tls: VMWorkerThread, mut mutator_visitor: F)
    where
        F: FnMut(&'static mut Mutator<ClaspVM>),
    {
        // GC worker threads are not registered as Clasp mutators, so calling
        // clasp_stop_the_world() here stops all Clasp mutator threads without
        // including this thread in the wait.
        unsafe { clasp_stop_the_world() };
        for mutator in VMActivePlan::mutators() {
            mutator_visitor(mutator);
        }
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unsafe { clasp_resume_the_world() };
    }

    fn block_for_gc(_tls: VMMutatorThread) {
        // Park this mutator and wait until the GC has finished and
        // resume_mutators has been called.
        unsafe { clasp_pause_thread_for_gc() };
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<ClaspVM>) {
        // We drop the thread JoinHandle as we expect the worker thread
        // to run until the process quits.
        let _ = std::thread::Builder::new()
            .name("MMTk GC Worker".to_string())
            .spawn(move || {
                // mask most signals for this thread so Lisp threads can deal
                // with them. We do keep hardware signals like SEGV to make bugs
                // more immediate; the process-wide signal handlers will
                // recognize that we are not a Lisp thread and SIG_DFL.
                unsafe { clasp_mask_signals_for_alien() };
                // do whatever mmtk wants us doing
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
