#include <clasp/core/foundation.h>
#include <clasp/gctools/memoryManagement.h> // my_thread
#include <clasp/gctools/interrupt.h>        // handle_all_queued_interrupts
#include <clasp/gctools/stw.h>

namespace gctools {

void begin_park(void* sp) {
  handle_all_queued_interrupts();
  my_thread->block();
  begin_gcsafe_shared(my_thread, sp);
}

void end_park() {
  // Blocks here if the world is currently stopped.
  end_gcsafe_shared(my_thread);
  my_thread->unblock();
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
