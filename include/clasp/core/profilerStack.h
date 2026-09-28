#ifndef CLASP_CORE_PROFILER_STACK_H
#define CLASP_CORE_PROFILER_STACK_H

// Native stack primitives shared by the CPU/allocation profilers and their
// standalone tests. Keep these independent of Lisp and signal-unsafe services.
#include <cstdint>
#include <ucontext.h>
#include <clasp/core/profilerWalkStop.h>

namespace core { namespace profiler_detail {

inline uintptr_t context_pc(void* context) {
  auto* uc = static_cast<ucontext_t*>(context);
#if defined(__linux__) && defined(__aarch64__)
  return uc->uc_mcontext.pc;
#elif defined(__linux__) && defined(__x86_64__)
  return uc->uc_mcontext.gregs[REG_RIP];
#elif defined(__APPLE__) && defined(__aarch64__)
  return __darwin_arm_thread_state64_get_pc(uc->uc_mcontext->__ss);
#elif defined(__APPLE__) && defined(__x86_64__)
  return uc->uc_mcontext->__ss.__rip;
#else
  return 0;
#endif
}

inline uintptr_t context_fp(void* context) {
  auto* uc = static_cast<ucontext_t*>(context);
#if defined(__linux__) && defined(__aarch64__)
  return uc->uc_mcontext.regs[29];
#elif defined(__linux__) && defined(__x86_64__)
  return uc->uc_mcontext.gregs[REG_RBP];
#elif defined(__APPLE__) && defined(__aarch64__)
  return __darwin_arm_thread_state64_get_fp(uc->uc_mcontext->__ss);
#elif defined(__APPLE__) && defined(__x86_64__)
  return uc->uc_mcontext->__ss.__rbp;
#else
  return 0;
#endif
}

inline uintptr_t context_sp(void* context) {
  auto* uc = static_cast<ucontext_t*>(context);
#if defined(__linux__) && defined(__aarch64__)
  return uc->uc_mcontext.sp;
#elif defined(__linux__) && defined(__x86_64__)
  return uc->uc_mcontext.gregs[REG_RSP];
#elif defined(__APPLE__) && defined(__aarch64__)
  return __darwin_arm_thread_state64_get_sp(uc->uc_mcontext->__ss);
#elif defined(__APPLE__) && defined(__x86_64__)
  return uc->uc_mcontext->__ss.__rsp;
#else
  return 0;
#endif
}

inline uintptr_t strip_return_address(uintptr_t address) {
#if defined(__aarch64__)
  // XPACLRI removes instruction-pointer authentication bits. Its HINT
  // encoding is a no-op on CPUs without PAC, so this also handles signed
  // frames from libraries when Clasp itself was built without PAC.
  uintptr_t result;
  __asm__ volatile("mov x30, %1\n\thint #7\n\tmov %0, x30"
                   : "=r"(result) : "r"(address) : "x30");
  return result;
#else
  return address;
#endif
}

inline bool valid_frame_pointer(uintptr_t fp, uintptr_t lo, uintptr_t hi) {
  // Both ABIs use a two-word record: previous FP, then saved return address.
  // Check the complete record before either load; do not overflow at hi.
  return fp != 0 && (fp & 7) == 0 && hi >= lo && hi - lo >= 16 &&
      fp >= lo && fp <= hi - 16;
}

// Call only after valid_frame_pointer has rejected fp. Prefer the most
// specific directly observable reason over the more general bounds failure.
inline WalkStopReason invalid_frame_pointer_reason(uintptr_t fp) {
  if (fp == 0) return WalkStopReason::null_frame_pointer;
  if ((fp & 7) != 0) return WalkStopReason::unaligned_frame_pointer;
  return WalkStopReason::frame_outside_stack;
}

template <typename IsExecutable>
inline uint32_t walk_frame_chain(uintptr_t pc, uintptr_t fp,
                                uint64_t* out, uint32_t capacity,
                                uintptr_t lo, uintptr_t hi,
                                IsExecutable is_executable,
                                WalkStop* stop = nullptr) {
  if (stop) *stop = {};
  auto finish = [stop](uint32_t depth, WalkStopReason reason,
                       uintptr_t frame_pointer, uintptr_t previous = 0,
                       uintptr_t return_address = 0, uint32_t flags = 0) {
    if (stop) {
      *stop = {reason, flags, frame_pointer, previous, return_address};
    }
    return depth;
  };
  if (capacity == 0) return finish(0, WalkStopReason::depth_limit, fp);
  uint32_t depth = 0;
  out[depth++] = pc;
  while (depth < capacity) {
    if (!valid_frame_pointer(fp, lo, hi)) {
      return finish(depth, invalid_frame_pointer_reason(fp), fp);
    }
    auto* frame = reinterpret_cast<const uintptr_t*>(fp);
    uintptr_t previous = frame[0];
    uintptr_t return_address = strip_return_address(frame[1]);
    if (return_address == 0) {
      return finish(depth, WalkStopReason::null_return_address, fp,
                    previous, return_address, HaveFrameRecord);
    }
    if (!is_executable(return_address)) {
      uint32_t flags = HaveFrameRecord;
      if (stop && is_executable(return_address - 1)) {
        flags |= PrecedingByteExecutable;
      }
      return finish(depth, WalkStopReason::unrecognized_return_address, fp,
                    previous, return_address, flags);
    }
    out[depth++] = return_address;
    // Stop at the outermost record, a cycle, or a backwards link.
    if (previous == 0) {
      return finish(depth, WalkStopReason::null_frame_pointer, fp,
                    previous, return_address, HaveFrameRecord);
    }
    if (previous <= fp) {
      return finish(depth, WalkStopReason::nonadvancing_frame, fp,
                    previous, return_address, HaveFrameRecord);
    }
    fp = previous;
  }
  // The next frame has not been inspected. Do not read it just to diagnose a
  // depth cap, or attach the preceding frame's record to this frame pointer.
  return finish(depth, WalkStopReason::depth_limit, fp);
}

}} // namespace core::profiler_detail
#endif
