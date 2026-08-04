use crate::active_plan::VMActivePlan;
use crate::mmtk;
use crate::ClaspVM;
use mmtk::util::alloc::AllocationError;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::ActivePlan;
use mmtk::vm::Collection;
use mmtk::vm::GCThreadContext;
use mmtk::Mutator;
use std::sync::Mutex;
use std::thread::JoinHandle;

extern "C" {
    fn clasp_stop_the_world();
    fn clasp_resume_the_world();
    fn clasp_pause_thread_for_gc();
    fn clasp_mask_signals_for_alien();
}

/// Join handles for the GC worker threads spawned by `spawn_gc_thread`.
///
/// `MMTK::prepare_to_fork()` only *asks* the workers to save their context and
/// return from their entry points; it is asynchronous. Before the VM may call
/// `fork()` it must wait for the underlying native threads to actually exit.
/// We keep their join handles here so `join_all_gc_worker_threads` can provide
/// that synchronization (see `mmtk_clasp_prepare_to_fork`).
static GC_WORKER_JOIN_HANDLES: Mutex<Vec<JoinHandle<()>>> = Mutex::new(Vec::new());

/// Wait for every GC worker thread previously spawned by `spawn_gc_thread` to
/// exit. Call this only after `MMTK::prepare_to_fork()` has requested them to
/// stop, otherwise it will block forever.
pub fn join_all_gc_worker_threads() {
    // Take the handles out under the lock, then join without holding it so a
    // worker that is still shutting down can't deadlock against us.
    let handles = std::mem::take(&mut *GC_WORKER_JOIN_HANDLES.lock().unwrap());
    for handle in handles {
        let _ = handle.join();
    }
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
        // Keep the JoinHandle so that fork support can wait for this worker to
        // exit after `prepare_to_fork` (see GC_WORKER_JOIN_HANDLES). Outside of
        // forking, workers run until the process quits and the handle is simply
        // never joined.
        let handle = std::thread::Builder::new()
            .name("MMTk GC Worker".to_string())
            .spawn(move || {
                use mmtk::util::opaque_pointer::*;
                use mmtk::util::Address;
                // mask most signals for this thread so Lisp threads can deal
                // with them. We do keep hardware signals like SEGV to make bugs
                // more immediate; the process-wide signal handlers will
                // recognize that we are not a Lisp thread and SIG_DFL.
                unsafe { clasp_mask_signals_for_alien() };
                // do whatever mmtk wants us doing
                // This seems like an insane way to get a VMWorkerThread,
                // but the docs aren't helpful. This is what Julia does.
                let worker_tls =
                    VMWorkerThread(VMThread(OpaquePointer::from_address(unsafe {
                        Address::from_usize(thread_id::get())
                    })));
                match ctx {
                    GCThreadContext::Worker(w) => {
                        mmtk::memory_manager::start_worker(mmtk(), worker_tls, w);
                    }
                }
            })
            .expect("Failed to spawn GC worker thread");
        GC_WORKER_JOIN_HANDLES.lock().unwrap().push(handle);
    }
}
