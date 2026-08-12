#pragma once

#include <atomic>
#include <clasp/core/object.h>
#include <clasp/core/corePackage.fwd.h>
#include <clasp/core/mpPackage.h>
#include <clasp/gctools/gcweak.h>

namespace core {

FORWARD(ReferenceQueue);
FORWARD(QueueableWeakReference);

/* Like WeakPointer (weakPointer.h) with a few differences:
 ** I changed it to "reference" to be more so-fist-icated.
 ** It can be used in a ReferenceQueue (below) which can in turn be used to build
    finalizers and other weirder things. This entails some space overhead.
 ** It can be resurrected by the GC. This turns it into a strong reference, and
    that change is permanent. This is used for C++ destructors, and so far nothing
    else, since object resurrection is to be avoided.
 * See gcweak.h for more details.
 */
class QueueableWeakReference_O : public General_O {
  LISP_CLASS(core, CorePkg, QueueableWeakReference_O, "QueueableWeakReference", General_O);
  QueueableWeakReference_O(T_sp referent, ReferenceQueue_sp queue,
                           bool resurrectp = false)
    : _link(referent, resurrectp), _queue(queue) {}

public:
  gctools::QueueableWeakReference _link;
  ReferenceQueue_sp _queue;
  T_sp next = nil<T_O>();

public:
  static QueueableWeakReference_sp make(T_sp, ReferenceQueue_sp, bool);
public:
  public:
  bool valid() const { return _link.valid(); }
  T_sp value() const { return _link.value().value_or(nil<T_O>()); }

  // Invalidate the reference and put it on its queue, or don't if that's
  // already been done. Returns whether it was done.
  // Called by the GC and can also be called by the user.
  bool enqueue();
};

// Like Java's ReferenceQueue.
// The GC adds to the head of the queue atomically by swapping out the head.
// The mutator pulls from the tail of the queue while holding a lock.
class ReferenceQueue_O : public General_O {
  LISP_CLASS(core, CorePkg, ReferenceQueue_O, "ReferenceQueue", General_O);

  ReferenceQueue_O(mp::Mutex_sp m) : mutex(m) {}

  static ReferenceQueue_sp make();

public:
  QueueableWeakReference_sp poll();
  // TODO: timeout version
  T_sp remove();

private:
  friend class QueueableWeakReference_O;
  // Add a reference to the queue.
  // Should only be called by QueueableWeakReference::enqueue.
  // Preconditions:
  // * you just cleared the underlying reference successfully
  // * the reference is not already in the queue (implied by the above)
  // * This is the queue the reference is registered with.
  void enqueue(QueueableWeakReference_sp ref) noexcept {
    T_sp mhead = head.load(std::memory_order_relaxed);
    do {
      ref->next = mhead;
    } while (!head.compare_exchange_weak(mhead, ref, std::memory_order_relaxed));
    head.notify_one();
  }

private:
  // call with lock held
  T_sp dequeue();

public:
  // The newest element of the queue (i.e. last to remove), or nil if there
  // is nothing in the queue. We try to keep the code in the GC that has to plop
  // stuff here simple, so polling is more involved.
  std::atomic<T_sp> head = nil<T_O>();
  mp::Mutex_sp mutex;
};
  
}; // namespace core
