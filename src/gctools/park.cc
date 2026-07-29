#include <clasp/core/foundation.h>
#include <clasp/gctools/memoryManagement.h> // my_thread
#include <clasp/gctools/interrupt.h>        // handle_all_queued_interrupts
#include <clasp/gctools/stw.h>

namespace gctools {

// Behavior depends on the existing park state.
// If it's Running (0) park.
// If it's GCSafe (1), set the thread state and handle interrupts,
// but don't block for GC as we'll already do that at the end of the GCSafe block.
// This is also important for when a mutator does call_with_stopped_world.
// If we're already Parked (2), do nothing. This shouldn't happen though.
int begin_park(void* sp) {
  if (my_thread->blockingp()) [[unlikely]] return 2;
  else if (my_thread->gcsafep()) [[unlikely]] {
    handle_all_queued_interrupts();
    my_thread->block();
    return 1;
  }
  else {
    handle_all_queued_interrupts();
    my_thread->block();
    begin_gcsafe_shared(my_thread, sp);
    return 0;
  }
}

void end_park(int oldstate) {
  switch (oldstate) {
  case 2: break; // was parked: do nothing
  case 1: my_thread->gcsafe(); break; // was gcsafe
  default: // was running
      // Blocks here if the world is currently stopped.
      end_gcsafe_shared(my_thread);
      my_thread->unblock();
      break;
  }
}

// End a park but return the saved stack pointer for re-parking.
// This is important so that we don't accidentally expand the region of stack
// that we make available to the GC.
void* end_park_temporarily() {
  void* sp = end_gcsafe_shared(my_thread);
  my_thread->unblock();
  return sp;
}

}; // namespace gctools
