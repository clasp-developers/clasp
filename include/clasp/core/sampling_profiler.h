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
 * Phase 4 is Symbolication
 * Phase 5 is collapsed-stacks aggregation - see sampling_profiler.cc
 */
#pragma once

#include <cstdint>
#include <cstddef>
#include <string>
#include <vector>
#include <clasp/core/profilerWalkStop.h>

namespace core {

// Per-sample header (variable-length record). A SampleHeader is followed
// immediately in the ring buffer by `depth` × uint64_t native PCs.
struct SampleHeader {
  uint64_t timestamp_ns;   // CLOCK_MONOTONIC at signal delivery
  uint64_t vm_pc;          // bytecode VM's _pc at sample time, or 0
  uint32_t thread_id;      // Linux tid / macOS port id (truncated)
  uint32_t depth;          // number of trailing PCs (0 if walk failed)
  profiler_detail::WalkStop walk_stop;
};

static_assert(sizeof(SampleHeader) == 56, "SampleHeader layout changed");

// Per-allocation sample header. An AllocationSampleHeader is followed
// immediately by `depth` native PCs.
//
// This record represents one or more byte-sampling boundaries crossed by
// the allocation. No pointers into the GC heap are retained.
struct AllocationSampleHeader {
  uint64_t timestamp_ns;
  uint64_t vm_pc;             // bytecode VM pc, or 0 when unavailable
  uint64_t sampled_bytes;     // bytes represented by this sample
  uint64_t allocation_size;   // actual size of the triggering allocation
  uint32_t thread_id;
  uint32_t depth;
  uint32_t stamp_wtag;        // preserve the allocator's raw unshifted value
  uint32_t flags;             // allocation-policy flags; initially zero
  profiler_detail::WalkStop walk_stop;
};

static_assert(sizeof(AllocationSampleHeader) == 80,
              "AllocationSampleHeader layout changed");

// Aggregated symbolicated sample: one entry per unique (thread_id, frames)
// group. `frames` is outermost-first (index 0 is the root, last is the
// leaf). For CPU profiles, `sample_count` is the number of raw samples.
// For allocation profiles, it is the number of attributed bytes.
struct SymbolicatedSample {
  uint32_t thread_id;
  size_t   sample_count;
  std::vector<std::string> frames;
  core::T_sp encode();
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

// Return one entry per recorded sample. Each inner vector holds the
// symbolicated frame names for that sample, outermost-first (index 0
// is the root, last index is the leaf). Prints a warning and returns
// an empty vector if the profiler is still running.
std::vector<SymbolicatedSample> sampling_profiler_symbolicated_samples();

// Populate the calling thread's stack bounds for later frame-walking.
// Must be called from a non-signal context.
void sampling_profiler_register_current_thread();

// Register an executable memory range with the profiler's return-address
// validator. Call this when new executable pages are allocated (JIT, arena)
// so the frame-pointer walker recognizes return addresses in them. Calls
// from multiple JIT threads are serialized; signal-handler reads remain
// lock-free. This function must not itself be called from a signal handler.
void sampling_profiler_add_executable_range(uintptr_t lo, uintptr_t hi);

// Diagnostics.
size_t sampling_profiler_samples_recorded();
size_t sampling_profiler_samples_dropped();
size_t sampling_profiler_bytes_used();

// Allocation profiler -------------------------------------------------------
//
// This profiles managed allocation traffic. It does not measure retained
// objects, collector fragmentation, native malloc, or resident memory.
// A zero bytes-per-sample selects 1 MiB; smaller values are clamped to
// 1 MiB. max-depth is clamped to [1, 4096]. A zero buffer size selects
// a 64 MiB allocation-sample ring.
bool allocation_profiler_start(size_t bytes_per_sample,
                               unsigned max_depth,
                               size_t buffer_bytes);
void allocation_profiler_stop();
bool allocation_profiler_running();
void allocation_profiler_reset();

// Return a coherent snapshot of the active allocation-profile session.
// Used only by the sparse allocation slow path.
bool allocation_profiler_session(uint64_t& session_epoch,
                                 size_t& bytes_per_sample);

// Called only when the allocation fast path crosses a sampling boundary.
// It must remain allocation-free, lock-free, and safe against reentry.
void allocation_profiler_record(uint32_t stamp_wtag,
                                size_t allocation_size,
                                size_t sampled_bytes,
                                uint32_t flags,
                                uint64_t session_epoch);

std::vector<SymbolicatedSample>
allocation_profiler_symbolicated_samples();
bool allocation_profiler_save(const char* path);

size_t allocation_profiler_samples_recorded();
size_t allocation_profiler_samples_dropped();
size_t allocation_profiler_bytes_attributed();
size_t allocation_profiler_bytes_dropped();
size_t allocation_profiler_bytes_used();
size_t allocation_profiler_bytes_available();

} // namespace core
