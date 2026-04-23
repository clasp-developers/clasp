/*
 * sampling_profiler.h — CPU-time sampling profiler.
 *
 * At rate `N` Hz, an ITIMER_PROF timer delivers SIGPROF to an arbitrary
 * running thread. The handler walks the frame-pointer chain via the
 * ucontext registers and appends a sample (timestamp, thread id, depth,
 * optional bytecode-VM pc, variable-length PC array) to a per-process
 * bump-allocated ring.
 *
 * Separate from src/core/profiler.cc's RangePush/RangePop instrumentation.
 * That profiler measures user-annotated regions; this one periodically
 * snapshots whatever code is running.
 *
 * See Phase 4 / Phase 5 for post-mortem symbolication and flame-graph
 * output — this header covers the recording side only.
 */
#pragma once

#include <cstdint>
#include <cstddef>

namespace core {

// Per-sample header (variable-length record). A SampleHeader is followed
// immediately in the ring buffer by `depth` × uint64_t native PCs.
struct SampleHeader {
  uint64_t timestamp_ns;   // CLOCK_MONOTONIC at signal delivery
  uint64_t vm_pc;          // bytecode VM's _pc at sample time, or 0
  uint32_t thread_id;      // Linux tid / macOS port id (truncated)
  uint32_t depth;          // number of trailing PCs (0 if walk failed)
};

// Start the profiler.
//   rate_hz          : sampling rate in Hz (e.g. 97). Clamped to [1, 10000].
//   max_depth        : per-sample stack-depth cap. Clamped to [1, 8192].
//   buffer_bytes     : ring buffer size (0 = default 256 MiB).
// Returns true on success. Fails if the profiler is already running or the
// OS timer/signal setup fails.
bool sampling_profiler_start(unsigned rate_hz,
                             unsigned max_depth,
                             size_t buffer_bytes);

// Stop sampling. The buffer is preserved; call
// sampling_profiler_save / sampling_profiler_reset to drain / clear.
void sampling_profiler_stop();

// True while a profile session is active.
bool sampling_profiler_running();

// Discard all captured samples and reset the bump pointer.
void sampling_profiler_reset();

// Drop the ring buffer contents to `path` as collapsed-stacks format
// (one stack per line, semicolon-separated, trailing ' <count>'), ready
// to feed Brendan Gregg's flamegraph.pl. Symbolicates on the fly using
// the arena side table, ObjectFile lookup, bytecode-module scan, and
// dladdr. Returns true on success, false on I/O error.
bool sampling_profiler_save(const char* path);

// Diagnostics.
size_t sampling_profiler_samples_recorded();
size_t sampling_profiler_samples_dropped();
size_t sampling_profiler_bytes_used();

} // namespace core
