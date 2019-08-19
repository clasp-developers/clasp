#ifndef gctools_interrupt_H
#define gctools_interrupt_H

namespace gctools {

  void clasp_interrupt_process(mp::Process_sp process, core::T_sp function);

  void handle_signal_now(int signo);
  void handle_all_queued_interrupts();
  
  void initialize_signals(int clasp_signal);
  void initialize_unix_signal_handlers();
  
};

#endif
