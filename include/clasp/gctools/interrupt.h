#pragma once

namespace gctools {

void clasp_interrupt_process(mp::Process_sp process, core::T_sp function);

template <typename Stage = RuntimeStage> void handle_all_queued_interrupts();

template <>
inline void handle_all_queued_interrupts<SnapshotLoadStage>(){
    // Do nothing
};

void initialize_signals();

// Use the following mechanism to pause clasp
#define USE_USER_SIGNAL 1
extern bool global_user_signal;
void wait_for_user_signal(const char* message);
void setup_user_signal();
}; // namespace gctools
