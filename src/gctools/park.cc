#include <clasp/core/foundation.h> // bunch of transitive includes FIXME
#include <clasp/gctools/memoryManagement.h> // my_thread
#include <clasp/gctools/interrupt.h> // handle_all_queued_interrupts

namespace gctools {

void begin_park() {
  handle_all_queued_interrupts();
  my_thread->set_blockingp(true);
}

void end_park() {
  my_thread->set_blockingp(false);
}

}; // namespace gctools
