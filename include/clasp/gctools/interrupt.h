#pragma once

#include <csetjmp>

namespace gctools {

template <typename Stage = RuntimeStage> void handle_all_queued_interrupts();

template <>
inline void handle_all_queued_interrupts<SnapshotLoadStage>(){
    // Do nothing
};

void initialize_signals();

struct address_range {
  uintptr_t low, high;
};

constexpr size_t max_fault_detection_ranges = 4;

void begin_fault_detection(size_t nranges, const address_range* ranges,
                           sigjmp_buf* ret);
void end_fault_detection();

// Call the thunk. If a memory fault in the given ranges occurs during the call,
// return true. Otherwise return false. This is used to test object validity in
// isValidGeneralObject and isValidConsObject.
template <typename Thunk>
bool detect_fault(size_t nranges, const address_range* ranges, Thunk&& thunk) {
  sigjmp_buf buf;
  // we don't need the sigmask since we have SA_DEFER and in general don't
  // really mess with sigmasks.
  if (sigsetjmp(buf, 0)) {
    end_fault_detection();
    return true;
  }
  begin_fault_detection(nranges, ranges, &buf);
  thunk();
  end_fault_detection();
  return false;
}

// Use the following mechanism to pause clasp
#define USE_USER_SIGNAL 1
extern bool global_user_signal;
void wait_for_user_signal(const char* message);
void setup_user_signal();
[[noreturn]] void truly_abort();

}; // namespace gctools
