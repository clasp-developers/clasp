#include <clasp/core/foundation.h>
#include <clasp/gctools/memoryManagement.h> // my_thread
#include <clasp/gctools/interrupt.h>        // handle_all_queued_interrupts
#include <clasp/gctools/stw.h>

namespace gctools {

void begin_park() {
  handle_all_queued_interrupts();
  my_thread->block();
  begin_gcless_shared();
}

void end_park() {
  // Blocks here if the world is currently stopped.
  end_gcless_shared();
  my_thread->unblock();
}

}; // namespace gctools
