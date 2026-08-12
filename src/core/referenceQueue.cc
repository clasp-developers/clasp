#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/referenceQueue.h>

#ifdef USE_BOEHM
// see gctools/gcweak.h
namespace gctools {
const size_t QueueableWeakReference::OFFSET = offsetof(core::QueueableWeakReference_O, _link);
}
#endif

namespace core {

// making queueable weak references from lisp is handled in weakPointer.cc
QueueableWeakReference_sp QueueableWeakReference_O::make(T_sp referent,
                                                         ReferenceQueue_sp queue,
                                                         bool resurrectp = false) {
  return gctools::GC<QueueableWeakReference_O>::allocate(referent, queue,
                                                         resurrectp);
}

CL_LISPIFY_NAME("weak-reference-enqueue");
CL_DEFMETHOD bool QueueableWeakReference_O::enqueue() {
  // Check if the reference is cleared already, and if it's not, do so.
  // This is atomic so races with GC/other threads are ok.
  if (_link.clear()) {
    // We won the race so we're good to enqueue.
    _queue->enqueue(this->asSmartPtr());
    return true;
  } else return false;
}

CL_LISPIFY_NAME("make-reference-queue");
CL_DEFUN ReferenceQueue_sp ReferenceQueue_O::make() {
  mp::Mutex_sp mut = mp::Mutex_O::make_mutex(nil<T_O>()); // dumb anonymous mutex
  return gctools::GC<ReferenceQueue_O>::allocate(mut);
}

CL_LISPIFY_NAME("reference-queue-poll");
CL_DEFMETHOD QueueableWeakReference_sp ReferenceQueue_O::poll() {
  while (true) {
    mutex->lock(true);
    // wait for the queue to have something in it
    // we do this while holding the lock, but that's ok since anyone else trying
    // to grab the lock is either polling (in which case they get in line) or
    // removing (in which case the queue is empty and they can return immediately).
    BEGIN_PARK {
      head.wait(nil<T_O>(), std::memory_order_relaxed);
    } END_PARK;
    // We have the lock and there's something in the queue.
    T_sp r = dequeue();
    if (r.isA<QueueableWeakReference_O>()) {
      // should always be true but i'm feeling paranoid
      QueueableWeakReference_sp q = r.as_unsafe<QueueableWeakReference_O>();
      mutex->unlock();
      return q;
    }
    mutex->unlock();
  }
}

CL_LISPIFY_NAME("reference-queue-remove");
CL_DEFMETHOD T_sp ReferenceQueue_O::remove() {
  if (mutex->lock(false)) { // try lock
    // Lock is held, so we can grab the tail.
    T_sp h = dequeue();
    mutex->unlock();
    return h;
  } else { // another thread has the lock and we're not waiting for it
    return nil<T_O>();
  }
}

T_sp ReferenceQueue_O::dequeue()  {
  // First, if the queue head is the tail (the queue only has one element)
  // see if we can dequeue before a GC worker adds anything else.
  T_sp th = head.load(std::memory_order_relaxed);
  if (th.isA<QueueableWeakReference_O>()) {
    if (th.as_unsafe<QueueableWeakReference_O>()->next.nilp()) {
      if (head.compare_exchange_strong(th, nil<T_O>(), std::memory_order_relaxed))
        // done!
        return th;
      // We hold the lock, so we're the only ones dequeuing.
      // Thus, the CAS failing must mean someone queued something.
      // That new head is now in th, and we know that that new th has a non-nil
      // next, since it was something added and not removed.
    }
  } else {
    // th is not a weak pointer, so it must be nil (the queue is empty)
    return th;
  }
  // At this point we know that the queue has more than one element,
  // and that th (the head we grabbed) has a non-nil next.
  QueueableWeakReference_sp h = th.as_assert<QueueableWeakReference_O>();
  while (true) {
    QueueableWeakReference_sp n = h->next.as_assert<QueueableWeakReference_O>();
    if (n->next.nilp()) { // we hit the end!
      h->next = nil<T_O>();
      return n;
    } else h = n; // keep trawling
  }
}

}; // namespace core
