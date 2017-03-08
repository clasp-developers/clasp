#ifndef gctools_interrupt_H
#define gctools_interrupt_H

namespace gctools {

  void handle_or_queue(core::ThreadLocalState* thread, core::T_sp signal_code /*, int code */);
  void initialize_signals();
  
};

#endif
