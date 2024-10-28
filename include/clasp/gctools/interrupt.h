#pragma once

namespace gctools {

void clasp_interrupt_process(mp::Process_sp process, core::Function_sp function);

void handle_signal_now(int signo);
template <typename Stage = RuntimeStage> void handle_all_queued_interrupts();

template <>
inline void handle_all_queued_interrupts<SnapshotLoadStage>(){
    // Do nothing
};

void initialize_signals(int clasp_signal);
void initialize_unix_signal_handlers();

// Use the following mechanism to pause clasp
#define USE_USER_SIGNAL 1
extern bool global_user_signal;
void wait_for_user_signal(const char* message);
void setup_user_signal();
}; // namespace gctools
