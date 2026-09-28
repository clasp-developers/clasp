#ifndef CLASP_CORE_PROFILER_WALK_STOP_H
#define CLASP_CORE_PROFILER_WALK_STOP_H

#include <cstdint>

namespace core { namespace profiler_detail {

enum class WalkStopReason : uint32_t {
  null_frame_pointer,
  null_return_address,
  unaligned_frame_pointer,
  frame_outside_stack,
  unrecognized_return_address,
  nonadvancing_frame,
  depth_limit,
  no_stack_bounds
};

enum WalkStopFlags : uint32_t {
  // previous_frame_pointer and return_pc came from a validated frame record.
  HaveFrameRecord = 1u << 0,
  // A rejected, nonzero return_pc had an executable preceding byte at capture.
  PrecedingByteExecutable = 1u << 1,
  // The recorder's own frame pointer failed validation before caller seeding.
  InitialFrame = 1u << 2
};

// Capture-time evidence only: a terminating frame is not proof that a trace
// contains every logical caller. Without HaveFrameRecord, frame_pointer
// identifies the frame that was not read and the other address fields are zero.
struct WalkStop {
  WalkStopReason reason;
  uint32_t flags;
  uint64_t frame_pointer;
  uint64_t previous_frame_pointer;
  uint64_t return_pc;
};

static_assert(sizeof(WalkStop) == 32, "WalkStop must occupy 32 bytes");

}} // namespace core::profiler_detail
#endif
