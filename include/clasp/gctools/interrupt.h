#ifndef gctools_interrupt_H
#define gctools_interrupt_H

namespace gctools {

  void clasp_interrupt_process(mp::Process_sp process, core::T_sp function);

  void handle_signal_now(int signo);
  void handle_all_queued_interrupts();
  
  void initialize_signals(int clasp_signal);
  void initialize_unix_signal_handlers();

  // Use the following mechanism to pause clasp 
#define USE_SIGUSR1
  extern bool global_SIGUSR1;
  void wait_for_SIGUSR1(const char* message);
};

#endif
