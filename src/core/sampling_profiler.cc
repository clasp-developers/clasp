/*
 * sampling_profiler.cc — Phase 1: timer + SIGPROF + bump-buffer plumbing.
 *
 * Phase 1 scope:
 *   - Allocate a single process-wide ring buffer (bump pointer, drop-on-full).
 *   - Install an async-signal-safe SIGPROF handler that records a sample
 *     header plus only the interrupted RIP. Frame-pointer walking is a
 *     Phase 2 addition.
 *   - Drive ITIMER_PROF at the requested rate. Portable between Linux and
 *     macOS — both support setitimer(ITIMER_PROF).
 *   - Expose start / stop / reset / save / diagnostics to Lisp via CL_DEFUN.
 *
 * The existing clasp signal infrastructure (src/gctools/interrupt.cc)
 * installs its own SIGPROF handler. We save it at profile-start and
 * restore it at profile-stop; when not profiling clasp's original
 * dispatch is untouched.
 *
 * Save format in Phase 1 is deliberately minimal — hex-only stack traces,
 * no symbolication. Symbolication lands in Phase 4.
 */

#include <atomic>
#include <cerrno>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <limits>
#include <mutex>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <clasp/core/common.h>
#ifdef _TARGET_OS_DARWIN
#define _XOPEN_SOURCE 600
#include <ucontext.h>
#else
#include <ucontext.h>
#endif
#if defined(__linux__)
#  include <sys/syscall.h>
#endif
#if defined(__APPLE__)
#  include <mach-o/dyld.h>
#  include <mach-o/loader.h>
#  include <mach/vm_prot.h>
#endif

#include <cxxabi.h>
#include <dlfcn.h>
#include <unordered_map>
#include <string>
#include <thread>
#include <vector>
#include <algorithm>

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/numbers.h>
#include <clasp/core/cons.h>
#include <clasp/core/sampling_profiler.h>
#include <clasp/core/profilerStack.h>
#include <clasp/core/profilerExecutableRanges.h>
#include <clasp/gctools/gc_boot.h>
#include <clasp/gctools/threadlocal.fwd.h>
#include <clasp/llvmo/trampoline_arena.h>   // arena_lookup_by_pc

namespace core {

namespace {

// ---------------------------------------------------------------------------
// State (file-static). The profiler is process-wide, so a single set of
// globals is correct. Load/store discipline must be async-signal-safe for
// anything the SIGPROF handler touches.
// ---------------------------------------------------------------------------

std::atomic<bool>     g_running{false};     // handler fast-exits if false
uint8_t*              g_buffer = nullptr;   // bump region (mmap'd)
size_t                g_buffer_bytes = 0;
std::atomic<size_t>   g_write_offset{0};    // next free byte in g_buffer
unsigned              g_max_depth = 8192;
std::atomic<uint64_t> g_samples_recorded{0};
std::atomic<uint64_t> g_samples_dropped{0};
std::atomic<uint32_t> g_active_sample_writers{0};
std::atomic<uint64_t> g_sampling_session_epoch{0};

static constexpr size_t kAllocationPollBytes =
  1024ull * 1024ull;
static constexpr size_t kDefaultAllocationBufferBytes =
  64ull * 1024ull * 1024ull;
static constexpr unsigned kMaxAllocationDepth = 4096;

std::atomic<bool>     g_allocation_running{false};
std::atomic<uint64_t> g_allocation_session_epoch{0};
std::atomic<size_t>   g_allocation_bytes_per_sample{
  kAllocationPollBytes
};
uint8_t*              g_allocation_buffer = nullptr;
size_t                g_allocation_buffer_bytes = 0;
std::atomic<size_t>   g_allocation_write_offset{0};
unsigned              g_allocation_max_depth = kMaxAllocationDepth;
std::atomic<uint64_t> g_allocation_samples_recorded{0};
std::atomic<uint64_t> g_allocation_samples_dropped{0};
std::atomic<uint64_t> g_allocation_bytes_attributed{0};
std::atomic<uint64_t> g_allocation_bytes_dropped{0};
std::atomic<uint32_t> g_active_allocation_writers{0};

struct sigaction      g_prev_sigaction;     // clasp's original SIGPROF handler
bool                  g_prev_sigaction_saved = false;

std::mutex            g_lifecycle_lock;     // serializes both profiler lifecycles

// ---------------------------------------------------------------------------
// Executable-range cache for return-address validation.
//
// Built at profile-start from /proc/self/maps (Linux) or dyld image list
// (macOS), sorted by start address. The SIGPROF handler binary-searches
// each saved_rip against this cache: if the address isn't in any executable
// mapping, the frame-pointer chain is broken (the "saved rip" is garbage
// from a frame compiled without -fno-omit-frame-pointer) and the walk stops.
//
// The cache is immutable once published. New JIT pages allocated during
// profiling are registered via sampling_profiler_add_executable_range(),
// which extends or appends to a separate table with lock-free readers.
// ---------------------------------------------------------------------------

struct ExecRange {
  uintptr_t lo;
  uintptr_t hi;  // exclusive
};

// Snapshot from /proc/self/maps at profile-start. Sorted by lo, searched
// with binary search. Not modified after construction.
static ExecRange*  g_exec_ranges = nullptr;
static size_t      g_exec_range_count = 0;

// Dynamic additions during profiling (JIT, arena pages). Writers serialize
// registration; readers never lock. The last entry can grow atomically when
// the new interval overlaps or touches it, without consuming another slot.
static constexpr size_t MAX_DYNAMIC_EXEC_RANGES = 16 * 1024;
static profiler_detail::DynamicExecutableRanges<MAX_DYNAMIC_EXEC_RANGES>
  g_dynamic_exec_ranges;
static std::mutex  g_dynamic_exec_range_writer_lock;
// Protected by the writer mutex; signal handlers never read this counter.
static size_t g_dynamic_exec_range_rejections = 0;

static void build_exec_range_cache() {
  // JIT threads may register executable ranges concurrently with setup.
  // The signal handler never takes this lock; the dynamic table publishes
  // new slots and endpoint extensions atomically.
  std::lock_guard<std::mutex> writer_guard(
    g_dynamic_exec_range_writer_lock);

  // Free previous cache if any.
  if (g_exec_ranges) { free(g_exec_ranges); g_exec_ranges = nullptr; }
  g_exec_range_count = 0;
  g_dynamic_exec_ranges.reset();
  g_dynamic_exec_range_rejections = 0;

  std::vector<ExecRange> ranges;

#if defined(__linux__)
  FILE* fp = fopen("/proc/self/maps", "r");
  if (!fp) return;
  char line[512];
  while (fgets(line, sizeof(line), fp)) {
    uintptr_t lo = 0, hi = 0;
    char perms[8] = {};
    if (sscanf(line, "%lx-%lx %4s", &lo, &hi, perms) >= 3) {
      if (perms[2] == 'x')
        ranges.push_back({lo, hi});
    }
  }
  fclose(fp);
#elif defined(__APPLE__)
  // dyld provides the loaded image list. Each Mach-O segment with VM_PROT_EXECUTE
  // is an executable range.
  uint32_t count = _dyld_image_count();
  for (uint32_t i = 0; i < count; ++i) {
    const struct mach_header* hdr = _dyld_get_image_header(i);
    if (!hdr) continue;
    intptr_t slide = _dyld_get_image_vmaddr_slide(i);
    if (hdr->magic == MH_MAGIC_64) {
      const struct mach_header_64* h64 = (const struct mach_header_64*)hdr;
      const uint8_t* p = (const uint8_t*)(h64 + 1);
      for (uint32_t j = 0; j < h64->ncmds; ++j) {
        const struct load_command* lc = (const struct load_command*)p;
        if (lc->cmd == LC_SEGMENT_64) {
          const struct segment_command_64* seg = (const struct segment_command_64*)p;
          if (seg->initprot & VM_PROT_EXECUTE) {
            uintptr_t lo = (uintptr_t)(seg->vmaddr + slide);
            ranges.push_back({lo, lo + seg->vmsize});
          }
        }
        p += lc->cmdsize;
      }
    }
  }
#endif

  // Sort and coalesce adjacent/overlapping ranges.
  std::sort(ranges.begin(), ranges.end(),
            [](const ExecRange& a, const ExecRange& b) { return a.lo < b.lo; });
  std::vector<ExecRange> merged;
  for (auto& r : ranges) {
    if (!merged.empty() && r.lo <= merged.back().hi)
      merged.back().hi = std::max(merged.back().hi, r.hi);
    else
      merged.push_back(r);
  }

  g_exec_range_count = merged.size();
  g_exec_ranges = (ExecRange*)malloc(g_exec_range_count * sizeof(ExecRange));
  if (g_exec_ranges)
    std::memcpy(g_exec_ranges, merged.data(), g_exec_range_count * sizeof(ExecRange));
  else
    g_exec_range_count = 0;
}

// Signal-safe binary search of the static cache.
static inline bool exec_cache_contains(uintptr_t pc) {
  // Check static cache (sorted, binary search).
  size_t lo = 0, hi = g_exec_range_count;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    if (pc < g_exec_ranges[mid].lo)
      hi = mid;
    else if (pc >= g_exec_ranges[mid].hi)
      lo = mid + 1;
    else
      return true;
  }
  // Check dynamic ranges (linear scan with atomic endpoint reads).
  return g_dynamic_exec_ranges.contains(pc);
}

static inline bool plausible_rip(uintptr_t rip) {
  return rip != 0 && exec_cache_contains(rip);
}

// Read CLOCK_MONOTONIC in nanoseconds. clock_gettime is async-signal-safe
// on Linux and macOS for CLOCK_MONOTONIC.
static inline uint64_t now_ns_signal_safe() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}

// Read the interrupted instruction pointer out of the context structure.
// Linux AArch64 stores the interrupted PC and frame pointer in pc and x29.
static inline uintptr_t ucontext_rip(void* ucptr) {
  return profiler_detail::context_pc(ucptr);
}

// Read the frame-pointer register (rbp on x86_64, x29 on AArch64).
static inline uintptr_t ucontext_rbp(void* ucptr) {
  return profiler_detail::context_fp(ucptr);
}

// ---------------------------------------------------------------------------
// Per-thread stack-bounds cache. Used by the walker to bound frame-pointer
// chasing: any frame outside [stack_lo, stack_hi) ends the walk. Populate
// only during thread registration, outside the signal handler: pthread
// stack queries are not async-signal-safe. Unregistered threads get a
// sample of the interrupted PC only.
// ---------------------------------------------------------------------------

struct ThreadStackBounds {
  uintptr_t lo;
  uintptr_t hi;
  bool populated;
};

thread_local ThreadStackBounds t_stack_bounds{0, 0, false};

static void populate_stack_bounds_for_this_thread() {
  if (t_stack_bounds.populated) return;
#if defined(__linux__)
  pthread_attr_t attr;
  if (pthread_getattr_np(pthread_self(), &attr) != 0) return;
  void* addr = nullptr;
  size_t size = 0;
  int status = pthread_attr_getstack(&attr, &addr, &size);
  pthread_attr_destroy(&attr);
  if (status != 0) return;
  t_stack_bounds.lo = (uintptr_t)addr;
  t_stack_bounds.hi = (uintptr_t)addr + size;
#elif defined(__APPLE__)
  void* top = pthread_get_stackaddr_np(pthread_self()); // top (high addr)
  size_t size = pthread_get_stacksize_np(pthread_self());
  t_stack_bounds.hi = (uintptr_t)top;
  t_stack_bounds.lo = (uintptr_t)top - size;
#else
  return;
#endif
  t_stack_bounds.populated = true;
}

// Validate an rbp candidate: word-aligned and inside the current thread's
// stack range. The walker terminates as soon as this check fails.
static inline bool plausible_rbp(uintptr_t rbp,
                                 uintptr_t stack_lo,
                                 uintptr_t stack_hi) {
  return profiler_detail::valid_frame_pointer(rbp, stack_lo, stack_hi);
}

// Walk the frame-pointer chain starting at (rip, rbp) and fill `out` with
// up to `max_depth` native PCs. Returns the number of frames recorded.
// Terminates on: out-of-stack-range rbp, null saved rbp, non-executable
// saved rip, non-advancing rbp, or max_depth.
//
// The plausible_rip check catches frames compiled without frame pointers:
// if a function uses rbp as a general register, the "saved rip" read from
// [rbp+8] will typically not point into executable memory, stopping the
// walk before it follows garbage.
//
// Safety: uses only register-read + bounded pointer walk + plausibility
// checks + writes to the caller's buffer. No libc calls, allocation, or locks.
static uint32_t walk_fp(uintptr_t rip_top, uintptr_t rbp_top,
                        uint64_t* out, uint32_t max_depth,
                        uintptr_t stack_lo, uintptr_t stack_hi,
                        profiler_detail::WalkStop& stop) {
  return profiler_detail::walk_frame_chain(rip_top, rbp_top, out, max_depth,
                                          stack_lo, stack_hi, plausible_rip, &stop);
}

// Reserve `bytes` from the bump buffer. Returns nullptr when the buffer is
// full — the caller increments the drop counter. Async-signal-safe: single
// CAS loop on a plain atomic counter, no allocation, no libc.
static inline uint8_t* ring_reserve(
    uint8_t* buffer,
    size_t buffer_bytes,
    std::atomic<size_t>& write_offset,
    size_t bytes) {
  if (!buffer || bytes > buffer_bytes) return nullptr;
  size_t cur = write_offset.load(std::memory_order_relaxed);
  for (;;) {
    if (cur > buffer_bytes - bytes) return nullptr;
    size_t next = cur + bytes;
    if (write_offset.compare_exchange_weak(cur, next,
                                           std::memory_order_acq_rel,
                                           std::memory_order_relaxed)) {
      return buffer + cur;
    }
    // cur was updated by CAS failure; retry.
  }
}

// SIGPROF handler. Runs on an arbitrary Lisp thread at signal-delivery time.
// Must be async-signal-safe end-to-end.
//
// Strategy: walk the frame-pointer chain into a small on-stack buffer with
// plausibility checks, then reserve exactly the right number of bytes in
// the ring and copy the result in. This avoids over-reserving or needing
// a two-step reserve/commit protocol.
static void sigprof_handler(int /*sig*/, siginfo_t* /*info*/, void* ucptr) {
  uint64_t session_epoch = g_sampling_session_epoch.load();

  // Increment before testing g_running. Together with the sequentially
  // consistent stop-side store/load, this closes the race in which stop
  // could otherwise miss a handler that had begun but not yet published
  // its record.
  g_active_sample_writers.fetch_add(1);
  if (!g_running.load() ||
      session_epoch != g_sampling_session_epoch.load()) {
    g_active_sample_writers.fetch_sub(1);
    return;
  }

  // NEVER call pthread_getattr_np (or anything that can malloc) from a
  // signal handler. On glibc pthread_getattr_np calls malloc, and if the
  // interrupted thread was already inside malloc holding the glibc arena
  // lock, the handler's malloc call deadlocks waiting for the same lock.
  // Observed concretely under SLIME+compile: __cxa_allocate_exception
  // from sjlj_unwind holds the arena lock when SIGPROF fires.
  //
  // Stack bounds must be populated before the thread ever receives a
  // sample: in sampling_profiler_start for the calling thread, and via
  // ext:profile-register-thread for any other Lisp thread. If bounds
  // are not populated we fall back to leaf-only sampling, which is a
  // usable data point and never deadlocks.

  uintptr_t rip = ucontext_rip(ucptr);
  uintptr_t rbp = ucontext_rbp(ucptr);

  // Walk into a stack-local buffer. 8K worst case = 64 KiB, on a typical
  // 8 MiB stack that's fine; samples with shorter stacks don't waste the
  // ring buffer because we use the actual depth when reserving.
  uint64_t pcs[8192];
  uint32_t cap = g_max_depth;
  if (cap > 8192) cap = 8192;
  uint32_t depth;
  profiler_detail::WalkStop stop{};
  if (t_stack_bounds.populated) {
    depth = walk_fp(rip, rbp, pcs, cap,
                    std::max(t_stack_bounds.lo, profiler_detail::context_sp(ucptr)),
                    t_stack_bounds.hi, stop);
  } else {
    pcs[0] = (uint64_t)rip;
    depth = 1;
    stop.reason = profiler_detail::WalkStopReason::no_stack_bounds;
    stop.frame_pointer = rbp;
  }

  const size_t record_bytes = sizeof(SampleHeader) + depth * sizeof(uint64_t);
  uint8_t* slot = ring_reserve(g_buffer, g_buffer_bytes,
                               g_write_offset, record_bytes);
  if (!slot) {
    g_samples_dropped.fetch_add(1, std::memory_order_relaxed);
    g_active_sample_writers.fetch_sub(1);
    return;
  }

  SampleHeader* h = (SampleHeader*)slot;
  h->timestamp_ns = now_ns_signal_safe();
  h->vm_pc = 0;  // TODO: Phase 2b — capture my_thread->_VM._pc if in bytecode_vm
#if defined(__linux__)
  h->thread_id = (uint32_t)syscall(SYS_gettid);
#else
  h->thread_id = 0;  // TODO: pthread_mach_thread_np on macOS
#endif
  h->depth = depth;
  h->walk_stop = stop;
  std::memcpy(slot + sizeof(SampleHeader), pcs, depth * sizeof(uint64_t));

  g_samples_recorded.fetch_add(1, std::memory_order_relaxed);
  g_active_sample_writers.fetch_sub(1);
}

// ---------------------------------------------------------------------------
// Timer control.
// ---------------------------------------------------------------------------

static bool install_sigaction() {
  struct sigaction sa;
  std::memset(&sa, 0, sizeof(sa));
  sa.sa_sigaction = &sigprof_handler;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | SA_RESTART;
  if (sigaction(SIGPROF, &sa, &g_prev_sigaction) != 0) {
    fprintf(stderr, "[sampling-profiler] sigaction failed: %s\n", strerror(errno));
    return false;
  }
  g_prev_sigaction_saved = true;
  return true;
}

static void restore_sigaction() {
  if (!g_prev_sigaction_saved) return;
  sigaction(SIGPROF, &g_prev_sigaction, nullptr);
  g_prev_sigaction_saved = false;
}

static bool arm_timer(unsigned rate_hz) {
  struct itimerval it;
  // Interval chosen so the first tick arrives promptly rather than after
  // a full period (value = it_interval = 1/rate).
  long usec = 1000000L / (long)rate_hz;
  if (usec < 1) usec = 1;
  it.it_interval.tv_sec = 0;
  it.it_interval.tv_usec = usec;
  it.it_value = it.it_interval;
  if (setitimer(ITIMER_PROF, &it, nullptr) != 0) {
    fprintf(stderr, "[sampling-profiler] setitimer failed: %s\n", strerror(errno));
    return false;
  }
  return true;
}

static void disarm_timer() {
  struct itimerval it;
  std::memset(&it, 0, sizeof(it));
  setitimer(ITIMER_PROF, &it, nullptr);
}

} // anonymous namespace

// ---------------------------------------------------------------------------
// Public API.
// ---------------------------------------------------------------------------

bool allocation_profiler_running() {
  return g_allocation_running.load(std::memory_order_acquire);
}

bool allocation_profiler_session(uint64_t& session_epoch,
                                 size_t& bytes_per_sample) {
  if (!g_allocation_running.load(std::memory_order_acquire))
    return false;

  uint64_t epoch =
    g_allocation_session_epoch.load(std::memory_order_acquire);
  size_t interval =
    g_allocation_bytes_per_sample.load(std::memory_order_relaxed);

  // Reject a snapshot that straddled stop/restart.
  if (!g_allocation_running.load(std::memory_order_acquire) ||
      epoch !=
        g_allocation_session_epoch.load(std::memory_order_acquire))
    return false;

  session_epoch = epoch;
  bytes_per_sample = interval;
  return interval != 0;
}

__attribute__((noinline))
void allocation_profiler_record(uint32_t stamp_wtag,
                                size_t allocation_size,
                                size_t sampled_bytes,
                                uint32_t flags,
                                uint64_t session_epoch) {
  // Increment before checking session state. This prevents stop from
  // overlooking an active writer, while the epoch rejects delayed calls
  // belonging to an older session.
  g_active_allocation_writers.fetch_add(
    1, std::memory_order_acq_rel);
  if (!g_allocation_running.load(std::memory_order_acquire) ||
      session_epoch !=
        g_allocation_session_epoch.load(std::memory_order_acquire)) {
    g_active_allocation_writers.fetch_sub(
      1, std::memory_order_acq_rel);
    return;
  }

  uint64_t pcs[kMaxAllocationDepth];
  uint32_t cap = g_allocation_max_depth;
  uintptr_t rip = reinterpret_cast<uintptr_t>(
    __builtin_extract_return_addr(__builtin_return_address(0)));
  uint32_t depth = 1;
  pcs[0] = static_cast<uint64_t>(rip);
  profiler_detail::WalkStop stop{};
  stop.reason = profiler_detail::WalkStopReason::no_stack_bounds;
  uintptr_t current_fp = reinterpret_cast<uintptr_t>(__builtin_frame_address(0));
  stop.frame_pointer = current_fp;

  if (my_thread_low_level) {
    uintptr_t stack_lo = reinterpret_cast<uintptr_t>(
      my_thread_low_level->_ControlStackTop);
    uintptr_t stack_hi = reinterpret_cast<uintptr_t>(
      my_thread_low_level->_ControlStackBottom);
    // Starting with our caller's frame avoids duplicating rip as the
    // first frame read from our own frame record.
    if (plausible_rbp(current_fp, stack_lo, stack_hi)) {
      uintptr_t caller_fp = *reinterpret_cast<uintptr_t*>(current_fp);
      depth = walk_fp(rip, caller_fp, pcs, cap, stack_lo, stack_hi, stop);
    } else {
      // Do not turn failure to validate our own frame into a fictitious
      // null caller link. No memory at current_fp has been read here.
      stop.reason = profiler_detail::invalid_frame_pointer_reason(current_fp);
      stop.flags = profiler_detail::InitialFrame;
    }
  }

  size_t record_bytes =
    sizeof(AllocationSampleHeader) + depth * sizeof(uint64_t);
  uint8_t* slot =
    ring_reserve(g_allocation_buffer,
                 g_allocation_buffer_bytes,
                 g_allocation_write_offset,
                 record_bytes);
  if (!slot) {
    g_allocation_samples_dropped.fetch_add(
      1, std::memory_order_relaxed);
    g_allocation_bytes_dropped.fetch_add(
      sampled_bytes, std::memory_order_relaxed);
    g_active_allocation_writers.fetch_sub(
      1, std::memory_order_acq_rel);
    return;
  }

  AllocationSampleHeader* header =
    reinterpret_cast<AllocationSampleHeader*>(slot);
  header->timestamp_ns = now_ns_signal_safe();
  header->vm_pc = 0;
  header->sampled_bytes = static_cast<uint64_t>(sampled_bytes);
  header->allocation_size = static_cast<uint64_t>(allocation_size);
#if defined(__linux__)
  header->thread_id =
    static_cast<uint32_t>(syscall(SYS_gettid));
#else
  header->thread_id = 0;
#endif
  header->depth = depth;
  header->stamp_wtag = stamp_wtag;
  header->flags = flags;
  header->walk_stop = stop;
  std::memcpy(slot + sizeof(AllocationSampleHeader),
              pcs, depth * sizeof(uint64_t));

  g_allocation_samples_recorded.fetch_add(
    1, std::memory_order_relaxed);
  g_allocation_bytes_attributed.fetch_add(
    sampled_bytes, std::memory_order_relaxed);
  g_active_allocation_writers.fetch_sub(
    1, std::memory_order_acq_rel);
}

bool allocation_profiler_start(size_t bytes_per_sample,
                               unsigned max_depth,
                               size_t buffer_bytes) {
  std::lock_guard<std::mutex> guard(g_lifecycle_lock);
  if (g_allocation_running.load(std::memory_order_acquire)) {
    fprintf(stderr,
            "[allocation-profiler] start: already running\n");
    return false;
  }

  if (bytes_per_sample < kAllocationPollBytes)
    bytes_per_sample = kAllocationPollBytes;
  if (max_depth < 1)
    max_depth = 1;
  if (max_depth > kMaxAllocationDepth)
    max_depth = kMaxAllocationDepth;
  if (buffer_bytes == 0)
    buffer_bytes = kDefaultAllocationBufferBytes;

  if (!g_allocation_buffer ||
      g_allocation_buffer_bytes != buffer_bytes) {
    if (g_allocation_buffer) {
      munmap(g_allocation_buffer,
             g_allocation_buffer_bytes);
      g_allocation_buffer = nullptr;
      g_allocation_buffer_bytes = 0;
    }

    void* mapped =
      mmap(nullptr, buffer_bytes, PROT_READ | PROT_WRITE,
           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mapped == MAP_FAILED) {
      fprintf(stderr,
              "[allocation-profiler] mmap(%zu) failed: %s\n",
              buffer_bytes, strerror(errno));
      return false;
    }
    g_allocation_buffer = static_cast<uint8_t*>(mapped);
    g_allocation_buffer_bytes = buffer_bytes;
  }

  g_allocation_write_offset.store(0, std::memory_order_release);
  g_allocation_samples_recorded.store(0,
                                      std::memory_order_release);
  g_allocation_samples_dropped.store(0,
                                     std::memory_order_release);
  g_allocation_bytes_attributed.store(0,
                                      std::memory_order_release);
  g_allocation_bytes_dropped.store(0,
                                  std::memory_order_release);
  g_allocation_bytes_per_sample.store(
    bytes_per_sample, std::memory_order_relaxed);
  g_allocation_max_depth = max_depth;

  // The first active profiler builds the immutable executable-range
  // cache. A second profiler started concurrently reuses it.
  if (!g_running.load(std::memory_order_acquire))
    build_exec_range_cache();

  uint64_t session_epoch =
    g_allocation_session_epoch.fetch_add(
      1, std::memory_order_acq_rel) + 1;

  // Avoid mixing the initiating thread's pre-start remainder into this
  // session. Other existing threads adopt the epoch lazily.
  if (my_thread_low_level) {
    gctools::AllocationProfiler& allocations =
      my_thread_low_level->_Allocations;
    allocations._AllocationSizeCounter = 0;
    allocations._AllocationProfileEpoch = session_epoch;
    allocations._AllocationProfileBytesPending = 0;
  }

  g_allocation_running.store(true, std::memory_order_release);
  return true;
}

void allocation_profiler_stop() {
  std::lock_guard<std::mutex> guard(g_lifecycle_lock);
  if (!g_allocation_running.load(std::memory_order_acquire))
    return;

  g_allocation_running.store(false, std::memory_order_release);
  while (g_active_allocation_writers.load(
           std::memory_order_acquire) != 0)
    std::this_thread::yield();
}

void allocation_profiler_reset() {
  std::lock_guard<std::mutex> guard(g_lifecycle_lock);
  if (g_allocation_running.load(std::memory_order_acquire)) {
    fprintf(stderr,
            "[allocation-profiler] reset: stop the profiler first\n");
    return;
  }

  g_allocation_write_offset.store(0, std::memory_order_release);
  g_allocation_samples_recorded.store(0,
                                      std::memory_order_release);
  g_allocation_samples_dropped.store(0,
                                     std::memory_order_release);
  g_allocation_bytes_attributed.store(0,
                                      std::memory_order_release);
  g_allocation_bytes_dropped.store(0,
                                  std::memory_order_release);
}

size_t allocation_profiler_samples_recorded() {
  return g_allocation_samples_recorded.load();
}

size_t allocation_profiler_samples_dropped() {
  return g_allocation_samples_dropped.load();
}

size_t allocation_profiler_bytes_attributed() {
  return g_allocation_bytes_attributed.load();
}

size_t allocation_profiler_bytes_dropped() {
  return g_allocation_bytes_dropped.load();
}

size_t allocation_profiler_bytes_used() {
  return g_allocation_write_offset.load();
}

size_t allocation_profiler_bytes_available() {
  std::lock_guard<std::mutex> guard(g_lifecycle_lock);
  return g_allocation_buffer_bytes;
}

bool sampling_profiler_running() {
  return g_running.load(std::memory_order_acquire);
}

bool sampling_profiler_start(unsigned rate_hz, unsigned max_depth, size_t buffer_bytes) {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  if (g_running.load(std::memory_order_acquire)) {
    fprintf(stderr, "[sampling-profiler] start: already running\n");
    return false;
  }

  if (rate_hz < 1)     rate_hz = 1;
  if (rate_hz > 10000) rate_hz = 10000;
  if (max_depth < 1)   max_depth = 1;
  if (max_depth > 8192) max_depth = 8192;
  if (buffer_bytes == 0) buffer_bytes = 256ull * 1024ull * 1024ull;  // 256 MiB

  // (Re-)allocate the ring buffer if size changed or first use.
  if (!g_buffer || g_buffer_bytes != buffer_bytes) {
    if (g_buffer) {
      munmap(g_buffer, g_buffer_bytes);
      g_buffer = nullptr;
      g_buffer_bytes = 0;
    }
    void* p = mmap(nullptr, buffer_bytes, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) {
      fprintf(stderr, "[sampling-profiler] mmap(%zu) failed: %s\n",
              buffer_bytes, strerror(errno));
      return false;
    }
    g_buffer = (uint8_t*)p;
    g_buffer_bytes = buffer_bytes;
  }
  g_write_offset.store(0, std::memory_order_release);
  g_samples_recorded.store(0, std::memory_order_release);
  g_samples_dropped.store(0, std::memory_order_release);
  g_max_depth = max_depth;

  // The first active profiler builds the immutable executable-range
  // cache. A second profiler started concurrently reuses it.
  if (!g_allocation_running.load(std::memory_order_acquire))
    build_exec_range_cache();

  if (!install_sigaction()) return false;
  // Populate this thread's stack bounds now, from a safe context, before
  // any sample can fire. pthread_getattr_np is not async-signal-safe.
  sampling_profiler_register_current_thread();
  // Invalidate any delayed handler belonging to an earlier session before
  // publishing the new session as running.
  g_sampling_session_epoch.fetch_add(1);
  // Publish running=true BEFORE arming the timer so the first tick sees
  // it. Release ordering pairs with the handler's acquire load.
  g_running.store(true, std::memory_order_release);
  if (!arm_timer(rate_hz)) {
    g_running.store(false, std::memory_order_release);
    restore_sigaction();
    return false;
  }

#if 0
  fprintf(stderr,
          "[sampling-profiler] started: rate=%u Hz  max_depth=%u  buffer=%zu MiB\n",
          rate_hz, max_depth, buffer_bytes / (1024 * 1024));
  fflush(stderr);
#endif
  return true;
}

void sampling_profiler_stop() {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  if (!g_running.load(std::memory_order_acquire)) return;
  disarm_timer();
  // Prevent new writers, then wait until every writer that observed the
  // active session has completed its record.
  g_running.store(false);
  while (g_active_sample_writers.load() != 0)
    std::this_thread::yield();
  restore_sigaction();
#if 0
  fprintf(stderr,
          "[sampling-profiler] stopped: %lu samples recorded, %lu dropped, %zu/%zu bytes used\n",
          (unsigned long)g_samples_recorded.load(),
          (unsigned long)g_samples_dropped.load(),
          g_write_offset.load(), g_buffer_bytes);
  fflush(stderr);
#endif
}

void sampling_profiler_reset() {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  if (g_running.load(std::memory_order_acquire)) {
    fprintf(stderr,
            "[sampling-profiler] reset: stop the profiler first\n");
    return;
  }
  g_write_offset.store(0, std::memory_order_release);
  g_samples_recorded.store(0, std::memory_order_release);
  g_samples_dropped.store(0, std::memory_order_release);
}

// ---------------------------------------------------------------------------
// Symbolication (Phase 4) + collapsed-stacks aggregation (Phase 5).
// ---------------------------------------------------------------------------

namespace {

// Sanitize a symbol name for collapsed-stacks output. flamegraph.pl uses
// ';' as the frame separator and treats the trailing token as a numeric
// count — so any ';' or whitespace in a Lisp symbol name (e.g.
// "FOO; BAR" or "(SETF X)") would corrupt the line. Replace them with '_'.
static std::string sanitize_frame(const std::string& s) {
  std::string out; out.reserve(s.size());
  for (char c : s) {
    if (c == ';' || c == ' ' || c == '\t' || c == '\n' || c == '\r')
      out += '_';
    else
      out += c;
  }
  return out;
}

// Per-process JIT-symbol index built from /tmp/perf-<pid>.map. Clasp's
// trampoline arena and the LLVM-ORC post-link callback both write
// <addr_hex> <size_hex> <name> lines to this file as code is generated;
// we read the current state at symbolication time. Sorted by start
// address so lookup is O(log N).
struct PerfMapEntry {
  uint64_t start;
  uint64_t size;   // 0 bumped to 1 so single-byte stubs cover their own PC
  std::string name;
};

static std::vector<PerfMapEntry> load_perf_map() {
  std::vector<PerfMapEntry> out;
  char path[64];
  snprintf(path, sizeof path, "/tmp/perf-%d.map", getpid());
  FILE* fp = fopen(path, "r");
  if (!fp) return out;
  char line[2048];
  while (fgets(line, sizeof line, fp)) {
    uint64_t addr = 0, size = 0;
    char name[1024] = {0};
    if (sscanf(line, "%" SCNx64 " %" SCNx64 " %1023[^\n]", &addr, &size, name) >= 3) {
      out.push_back({addr, size ? size : 1, std::string(name)});
    }
  }
  fclose(fp);
  std::sort(out.begin(), out.end(),
            [](const PerfMapEntry& a, const PerfMapEntry& b) {
              return a.start < b.start;
            });
  return out;
}

static const PerfMapEntry*
perf_map_lookup(const std::vector<PerfMapEntry>& idx, uint64_t pc) {
  auto it = std::upper_bound(idx.begin(), idx.end(), pc,
                             [](uint64_t p, const PerfMapEntry& e) {
                               return p < e.start;
                             });
  if (it == idx.begin()) return nullptr;
  --it;
  if (pc >= it->start && pc < it->start + it->size) return &*it;
  return nullptr;
}

// Resolve a PC to a human-readable frame name. Lookup order:
//   1. Trampoline arenas (bytecode + GF) — O(log N) or O(N) side-table scan.
//   2. perf-map — Clasp-JIT'd native code (both bytecode trampolines and
//      ORC-JIT-linked ObjectFile symbols end up here).
//   3. dladdr — covers libclasp, libc, libLLVM, other shared objects.
//   4. Hex fallback.
//
// Cache results: the same PC reappears in many samples (especially the
// bytecode VM's inner-loop return address) and dladdr isn't free.
static std::string symbolicate_one(uint64_t pc,
                                   std::unordered_map<uint64_t, std::string>& cache,
                                   const std::vector<PerfMapEntry>& perf_map) {
  auto it = cache.find(pc);
  if (it != cache.end()) return it->second;

  std::string name;
  if (const llvmo::TrampolineEntry* e = llvmo::arena_lookup_by_pc((uintptr_t)pc)) {
    name = e->name;
  } else if (const PerfMapEntry* p = perf_map_lookup(perf_map, pc)) {
    name = p->name;
  } else {
    Dl_info info;
    if (dladdr((void*)(uintptr_t)pc, &info) && info.dli_sname && info.dli_sname[0]) {
      int status = -1;
      char* demangled = abi::__cxa_demangle(info.dli_sname, nullptr, nullptr, &status);
      name = (status == 0 && demangled) ? demangled : info.dli_sname;
      free(demangled);
    } else {
      char buf[32];
      snprintf(buf, sizeof buf, "0x%lx", (unsigned long)pc);
      name = buf;
    }
  }
  name = sanitize_frame(name);
  cache.emplace(pc, name);
  return name;
}

static std::string allocation_type_frame(
    uint32_t stamp_wtag,
    uint32_t flags) {
  size_t stamp_index =
    gctools::Header_s::StampWtagMtag::make_nowhere_stamp(
      static_cast<gctools::UnshiftedStamp>(stamp_wtag));

  std::string name;
  if (gctools::global_stamp_layout &&
      stamp_index < gctools::global_stamp_max) {
    const gctools::Stamp_layout& layout =
      gctools::global_stamp_layout[stamp_index];
    if (layout.layout_op != gctools::undefined_op &&
        layout.name && layout.name[0])
      name = layout.name;
  }

  if (name.empty()) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer),
             "stamp-%zu/raw-0x%08x",
             stamp_index, stamp_wtag);
    name = buffer;
  }

  if (flags != 0) {
    char buffer[32];
    snprintf(buffer, sizeof(buffer),
             ":flags-0x%08x", flags);
    name += buffer;
  }

  return sanitize_frame("allocation:" + name);
}

}  // anonymous namespace

std::vector<SymbolicatedSample> sampling_profiler_symbolicated_samples() {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  std::vector<SymbolicatedSample> out;
  if (g_running.load(std::memory_order_acquire)) {
    fprintf(stderr, "[sampling-profiler] symbolicated-samples: stop the profiler first\n");
    return out;
  }
  if (!g_buffer || g_write_offset.load() == 0) return out;

  std::unordered_map<uint64_t, std::string> sym_cache;
  std::vector<PerfMapEntry> perf_map = load_perf_map();
  // Dedup by (thread_id, joined frames). Value is an index into `out`.
  std::unordered_map<std::string, size_t> group_index;

  size_t end = g_write_offset.load();
  size_t off = 0;
  while (off + sizeof(SampleHeader) <= end) {
    SampleHeader* h = (SampleHeader*)(g_buffer + off);
    size_t record_bytes = sizeof(SampleHeader) + h->depth * sizeof(uint64_t);
    if (off + record_bytes > end) break;
    uint64_t* pcs = (uint64_t*)(g_buffer + off + sizeof(SampleHeader));

    std::vector<std::string> frames;
    frames.reserve(h->depth);
    for (uint32_t i = h->depth; i-- > 0; ) {
      frames.push_back(symbolicate_one(pcs[i], sym_cache, perf_map));
    }
    // Build dedup key: "tid|f0;f1;...;fN".
    std::string key;
    {
      char tidbuf[16];
      int n = snprintf(tidbuf, sizeof tidbuf, "%u|", (unsigned)h->thread_id);
      key.append(tidbuf, n);
    }
    for (const auto& f : frames) { key += ';'; key += f; }

    auto it = group_index.find(key);
    if (it == group_index.end()) {
      SymbolicatedSample s;
      s.thread_id = h->thread_id;
      s.sample_count = 1;
      s.frames = std::move(frames);
      group_index.emplace(std::move(key), out.size());
      out.push_back(std::move(s));
    } else {
      out[it->second].sample_count++;
    }
    off += record_bytes;
  }
  return out;
}

std::vector<SymbolicatedSample>
allocation_profiler_symbolicated_samples() {
  std::lock_guard<std::mutex> guard(g_lifecycle_lock);
  std::vector<SymbolicatedSample> out;
  if (g_allocation_running.load(std::memory_order_acquire)) {
    fprintf(stderr,
            "[allocation-profiler] symbolicated-samples: "
            "stop the profiler first\n");
    return out;
  }
  if (!g_allocation_buffer ||
      g_allocation_write_offset.load() == 0)
    return out;

  std::unordered_map<uint64_t, std::string> sym_cache;
  std::vector<PerfMapEntry> perf_map = load_perf_map();
  std::unordered_map<std::string, size_t> group_index;

  size_t end = g_allocation_write_offset.load();
  size_t off = 0;
  while (off < end) {
    size_t remaining = end - off;
    if (remaining < sizeof(AllocationSampleHeader))
      break;

    AllocationSampleHeader* header =
      reinterpret_cast<AllocationSampleHeader*>(
        g_allocation_buffer + off);

    if (header->depth == 0 ||
        header->depth > kMaxAllocationDepth ||
        header->depth >
          (remaining - sizeof(AllocationSampleHeader)) /
            sizeof(uint64_t) ||
        header->sampled_bytes == 0) {
      fprintf(stderr,
              "[allocation-profiler] malformed record at offset %zu\n",
              off);
      break;
    }

    size_t record_bytes =
      sizeof(AllocationSampleHeader) +
      header->depth * sizeof(uint64_t);
    uint64_t* pcs = reinterpret_cast<uint64_t*>(
      g_allocation_buffer + off + sizeof(AllocationSampleHeader));

    std::vector<std::string> frames;
    frames.reserve(header->depth + 1);
    for (uint32_t index = header->depth; index-- > 0;)
      frames.push_back(
        symbolicate_one(pcs[index], sym_cache, perf_map));
    frames.push_back(
      allocation_type_frame(header->stamp_wtag,
                            header->flags));

    std::string key;
    {
      char thread_buffer[16];
      int length =
        snprintf(thread_buffer, sizeof(thread_buffer), "%u|",
                 static_cast<unsigned>(header->thread_id));
      key.append(thread_buffer, length);
    }
    for (const auto& frame : frames) {
      key += ';';
      key += frame;
    }

    size_t weight =
      static_cast<size_t>(header->sampled_bytes);
    auto found = group_index.find(key);
    if (found == group_index.end()) {
      SymbolicatedSample sample;
      sample.thread_id = header->thread_id;
      sample.sample_count = weight;
      sample.frames = std::move(frames);
      group_index.emplace(std::move(key), out.size());
      out.push_back(std::move(sample));
    } else {
      out[found->second].sample_count += weight;
    }

    off += record_bytes;
  }

  return out;
}

bool sampling_profiler_save(const char* path) {
  auto groups = sampling_profiler_symbolicated_samples();
  if (groups.empty()) {
    fprintf(stderr, "[sampling-profiler] save: no samples available\n");
    return false;
  }

  FILE* fp = fopen(path, "w");
  if (!fp) {
    fprintf(stderr, "[sampling-profiler] save: fopen(%s) failed: %s\n", path, strerror(errno));
    return false;
  }

  // Collapsed-stacks output for flamegraph.pl:
  //   frame_root;frame_mid;...;frame_leaf <count>\n
  // The flamegraph format has no thread dimension, so we collapse
  // same-frames groups across threads by summing sample_count.
  std::unordered_map<std::string, size_t> counts;
  size_t total_samples = 0;
  for (const auto& g : groups) {
    std::string key;
    size_t est = 0;
    for (const auto& f : g.frames) est += f.size() + 1;
    key.reserve(est);
    for (const auto& f : g.frames) {
      if (!key.empty()) key += ';';
      key += f;
    }
    counts[key] += g.sample_count;
    total_samples += g.sample_count;
  }

  for (const auto& kv : counts) {
    fprintf(fp, "%s %zu\n", kv.first.c_str(), kv.second);
  }
  fclose(fp);
  fprintf(stderr,
          "[sampling-profiler] wrote %zu samples (%zu unique stacks) to %s\n",
          total_samples, counts.size(), path);
  return true;
}

bool allocation_profiler_save(const char* path) {
  auto groups = allocation_profiler_symbolicated_samples();
  if (groups.empty()) {
    fprintf(stderr,
            "[allocation-profiler] save: no samples available\n");
    return false;
  }

  FILE* file = fopen(path, "w");
  if (!file) {
    fprintf(stderr,
            "[allocation-profiler] save: fopen(%s) failed: %s\n",
            path, strerror(errno));
    return false;
  }

  std::unordered_map<std::string, size_t> counts;
  size_t total_bytes = 0;
  for (const auto& group : groups) {
    std::string key;
    for (const auto& frame : group.frames) {
      if (!key.empty())
        key += ';';
      key += frame;
    }
    counts[key] += group.sample_count;
    total_bytes += group.sample_count;
  }

  for (const auto& entry : counts)
    fprintf(file, "%s %zu\n",
            entry.first.c_str(), entry.second);

  fclose(file);
  fprintf(stderr,
          "[allocation-profiler] wrote %zu attributed bytes "
          "(%zu unique stacks) to %s\n",
          total_bytes, counts.size(), path);
  return true;
}

size_t sampling_profiler_samples_recorded() { return g_samples_recorded.load(); }
size_t sampling_profiler_samples_dropped() { return g_samples_dropped.load(); }
size_t sampling_profiler_bytes_used() { return g_write_offset.load(); }

void sampling_profiler_register_current_thread() {
  populate_stack_bounds_for_this_thread();
}

void sampling_profiler_add_executable_range(uintptr_t lo, uintptr_t hi) {
  std::lock_guard<std::mutex> writer_guard(
    g_dynamic_exec_range_writer_lock);
  if (g_dynamic_exec_ranges.add(lo, hi) ==
      profiler_detail::ExecutableRangeRegistration::full) {
    ++g_dynamic_exec_range_rejections;
  }
}

core::T_sp SymbolicatedSample::encode() {
  core::SimpleVector_sp sample = core::SimpleVector_O::make(3);
  (*sample)[0] = clasp_make_fixnum(this->thread_id);
  (*sample)[1] = clasp_make_fixnum(this->sample_count);
  core::SimpleVector_sp frames = core::SimpleVector_O::make(this->frames.size());
  size_t idx = 0;
  for ( auto& fr : this->frames ) {
    (*frames)[idx++] = core::SimpleBaseString_O::make(fr);
  }
  (*sample)[2] = frames;
  return sample;
}


// ---------------------------------------------------------------------------
// Lisp bindings.
// ---------------------------------------------------------------------------

namespace {

// A stopped-buffer snapshot contains only integers, not references to stack
// memory. Release the lifecycle mutex before allocating any Lisp objects.
struct CapturedWalkStop {
  profiler_detail::WalkStop stop;
  uint64_t timestamp_ns;
  uint64_t last_pc;
  uint64_t attributed_bytes;
  uint32_t thread_id;
  uint32_t depth;
};

template <typename Header, typename Visitor>
static const char* visit_walk_stops(const uint8_t* buffer, size_t end,
                                   size_t capacity, uint32_t max_depth,
                                   Visitor visit) {
  if (end > capacity || (end != 0 && !buffer))
    return "Invalid profiling buffer extent";
  for (size_t off = 0; off < end;) {
    size_t remaining = end - off;
    if (remaining < sizeof(Header)) return "Incomplete profiling sample header";
    const auto* header = reinterpret_cast<const Header*>(buffer + off);
    if (header->depth == 0 || header->depth > max_depth ||
        header->depth > (remaining - sizeof(Header)) / sizeof(uint64_t))
      return "Invalid profiling sample depth";
    const auto* pcs = reinterpret_cast<const uint64_t*>(buffer + off + sizeof(Header));
    visit(*header, pcs);
    off += sizeof(Header) + header->depth * sizeof(uint64_t);
  }
  return nullptr;
}

template <typename Header, typename Weight>
static const char* copy_walk_stops(const uint8_t* buffer, size_t end,
                                  size_t capacity, uint32_t max_depth,
                                  Weight weight,
                                  std::vector<CapturedWalkStop>& records) {
  return visit_walk_stops<Header>(buffer, end, capacity, max_depth,
    [&](const Header& header, const uint64_t* pcs) {
      records.push_back({header.walk_stop, header.timestamp_ns,
                        pcs[header.depth - 1], weight(header),
                        header.thread_id, header.depth});
    });
}

struct WalkStopSummary {
  // The final bucket preserves the raw API's :UNKNOWN fallback.
  static constexpr size_t known_reasons =
    static_cast<size_t>(profiler_detail::WalkStopReason::no_stack_bounds) + 1;
  struct Counts {
    uint64_t samples = 0;
    uint64_t attributed_bytes = 0;
  } counts[known_reasons + 1]{};

  void add(profiler_detail::WalkStopReason reason, uint64_t weight) {
    size_t index = static_cast<size_t>(reason);
    auto& bucket = counts[index < known_reasons ? index : known_reasons];
    ++bucket.samples;
    bucket.attributed_bytes += weight;
  }
};

template <typename Header, typename Weight>
static const char* summarize_walk_stops(const uint8_t* buffer, size_t end,
                                       size_t capacity, uint32_t max_depth,
                                       Weight weight, WalkStopSummary& summary) {
  return visit_walk_stops<Header>(buffer, end, capacity, max_depth,
    [&](const Header& header, const uint64_t*) {
      summary.add(header.walk_stop.reason, weight(header));
    });
}

static const char* walk_stop_reason_name(profiler_detail::WalkStopReason reason) {
  using Reason = profiler_detail::WalkStopReason;
  switch (reason) {
  case Reason::null_frame_pointer: return "NULL-FRAME-POINTER";
  case Reason::null_return_address: return "NULL-RETURN-ADDRESS";
  case Reason::unaligned_frame_pointer: return "UNALIGNED-FRAME-POINTER";
  case Reason::frame_outside_stack: return "FRAME-OUTSIDE-STACK";
  case Reason::unrecognized_return_address: return "UNRECOGNIZED-RETURN-ADDRESS";
  case Reason::nonadvancing_frame: return "NONADVANCING-FRAME";
  case Reason::depth_limit: return "DEPTH-LIMIT";
  case Reason::no_stack_bounds: return "NO-STACK-BOUNDS";
  }
  return "UNKNOWN";
}

} // anonymous namespace

CL_DOCSTRING(R"dx(Return a vector of per-sample stack-walk stop diagnostic plists.
With :ALLOCATION T, read allocation samples; otherwise read CPU samples.
The selected profiler must be stopped. This does not reset its buffer.
With :SUMMARY-ONLY T, return one plist per observed reason containing only
:REASON, :SAMPLES and :ATTRIBUTED-BYTES (NIL for CPU samples). This scans the
buffer using fixed-size counters, without copying samples or symbol lookup.
By default, return the full per-sample diagnostics described below.
Each plist preserves :REASON, :FLAGS, :FRAME-POINTER, :PREVIOUS-FRAME-POINTER,
:RETURN-PC, :LAST-PC, :LAST-FUNCTION, :RETURN-FUNCTION, :THREAD-ID, :DEPTH,
:TIMESTAMP-NS, :SAMPLES and :ATTRIBUTED-BYTES (NIL for CPU samples).
Unreadable saved frame fields are NIL, distinct from a captured zero value.
:RETURN-PC has architecture-specific authentication bits stripped. It is
rejected only for an unrecognized/null-return-address stop, not every reason.
:PRECEDING-BYTE-EXECUTABLE records a capture-time range-boundary candidate;
it is not proof that the range ended there. :FLAGS bits are 1=frame record
read, 2=preceding byte recognized, 4=allocation recorder's initial FP failed.
Null termination and depth-limit stops do not certify a complete stack.
All symbol lookup and Lisp allocation happen here, not during capture.)dx");
CL_LAMBDA(&key allocation summary-only);
DOCGROUP(clasp);
CL_DEFUN core::T_sp ext__profile_walk_stops(bool allocation, bool summary_only) {
  std::vector<CapturedWalkStop> records;
  WalkStopSummary summary;
  const char* failure = nullptr;
  {
    std::lock_guard<std::mutex> guard(g_lifecycle_lock);
    if (allocation) {
      if (g_allocation_running.load(std::memory_order_acquire))
        failure = "Stop allocation profiling before reading walk diagnostics";
      else
        failure = summary_only
          ? summarize_walk_stops<AllocationSampleHeader>(
              g_allocation_buffer, g_allocation_write_offset.load(), g_allocation_buffer_bytes,
              kMaxAllocationDepth,
              [](const AllocationSampleHeader& h) { return h.sampled_bytes; }, summary)
          : copy_walk_stops<AllocationSampleHeader>(
              g_allocation_buffer, g_allocation_write_offset.load(), g_allocation_buffer_bytes,
              kMaxAllocationDepth,
              [](const AllocationSampleHeader& h) { return h.sampled_bytes; }, records);
    } else {
      if (g_running.load(std::memory_order_acquire))
        failure = "Stop CPU profiling before reading walk diagnostics";
      else
        failure = summary_only
          ? summarize_walk_stops<SampleHeader>(
              g_buffer, g_write_offset.load(), g_buffer_bytes, 8192,
              [](const SampleHeader&) { return uint64_t{0}; }, summary)
          : copy_walk_stops<SampleHeader>(
              g_buffer, g_write_offset.load(), g_buffer_bytes, 8192,
              [](const SampleHeader&) { return uint64_t{0}; }, records);
    }
  }
  if (failure) SIMPLE_ERROR("{}", failure);

  if (summary_only) {
    size_t count = 0;
    for (const auto& bucket : summary.counts)
      if (bucket.samples != 0) ++count;
    auto result = core::SimpleVector_O::make(count);
    size_t index = 0;
    for (size_t reason = 0; reason <= WalkStopSummary::known_reasons; ++reason) {
      const auto& bucket = summary.counts[reason];
      if (bucket.samples == 0) continue;
      core::List_sp plist = nil<core::T_O>();
      auto field = [&](const char* key, core::T_sp value) {
        plist = core::Cons_O::create(_lisp->internKeyword(key),
                                    core::Cons_O::create(value, plist));
      };
      field("ATTRIBUTED-BYTES", allocation
            ? core::T_sp(Integer_O::create(bucket.attributed_bytes)) : nil<core::T_O>());
      field("SAMPLES", Integer_O::create(bucket.samples));
      field("REASON", _lisp->internKeyword(walk_stop_reason_name(
        static_cast<profiler_detail::WalkStopReason>(reason))));
      (*result)[index++] = plist;
    }
    return result;
  }

  auto result = core::SimpleVector_O::make(records.size());
  std::unordered_map<uint64_t, std::string> symbols;
  auto perf_map = load_perf_map();
  for (size_t index = 0; index < records.size(); ++index) {
    const auto& record = records[index];
    const auto& stop = record.stop;
    bool have_frame = (stop.flags & profiler_detail::HaveFrameRecord) != 0;
    core::List_sp plist = nil<core::T_O>();
    auto field = [&](const char* key, core::T_sp value) {
      plist = core::Cons_O::create(_lisp->internKeyword(key),
                                  core::Cons_O::create(value, plist));
    };
    field("ATTRIBUTED-BYTES", allocation
          ? core::T_sp(Integer_O::create(record.attributed_bytes)) : nil<core::T_O>());
    field("SAMPLES", clasp_make_fixnum(1));
    field("TIMESTAMP-NS", Integer_O::create(record.timestamp_ns));
    field("DEPTH", Integer_O::create(record.depth));
    field("THREAD-ID", Integer_O::create(record.thread_id));
    field("RETURN-FUNCTION", have_frame && stop.return_pc != 0
          ? core::T_sp(SimpleBaseString_O::make(symbolicate_one(stop.return_pc, symbols, perf_map)))
          : nil<core::T_O>());
    field("LAST-FUNCTION", SimpleBaseString_O::make(symbolicate_one(record.last_pc, symbols, perf_map)));
    field("LAST-PC", Integer_O::create(record.last_pc));
    field("PRECEDING-BYTE-EXECUTABLE",
          (stop.flags & profiler_detail::PrecedingByteExecutable) ? _lisp->_true() : nil<core::T_O>());
    field("RETURN-PC", have_frame ? core::T_sp(Integer_O::create(stop.return_pc)) : nil<core::T_O>());
    field("PREVIOUS-FRAME-POINTER", have_frame
          ? core::T_sp(Integer_O::create(stop.previous_frame_pointer)) : nil<core::T_O>());
    field("FRAME-POINTER", Integer_O::create(stop.frame_pointer));
    field("FLAGS", Integer_O::create(stop.flags));
    field("REASON", _lisp->internKeyword(walk_stop_reason_name(stop.reason)));
    (*result)[index] = plist;
  }
  return result;
}

CL_DOCSTRING(R"dx(Test frame-walk stop reasons and stopped-buffer decoding on isolated
fixtures. Returns T on success or signals an error naming the failed check.
Does not start profiling or change profiler buffers or executable ranges.)dx");
CL_LAMBDA();
DOCGROUP(clasp);
CL_DEFUN bool ext__test_profile_walk_stops() {
  using namespace profiler_detail;
  using Reason = WalkStopReason;
  auto check = [](bool passed, const char* description) {
    if (!passed) SIMPLE_ERROR("Walk-stop regression failed: {}", description);
  };
  auto same_stop = [](const WalkStop& a, const WalkStop& b) {
    return a.reason == b.reason && a.flags == b.flags &&
      a.frame_pointer == b.frame_pointer &&
      a.previous_frame_pointer == b.previous_frame_pointer &&
      a.return_pc == b.return_pc;
  };
  alignas(16) uintptr_t frames[6] = {};
  const uintptr_t lo = reinterpret_cast<uintptr_t>(frames);
  const uintptr_t hi = lo + sizeof frames;
  frames[0] = lo + 16; frames[1] = 0x1000;
  frames[2] = lo + 32; frames[3] = 0x2000;
  frames[4] = 0;       frames[5] = 0x3000;
  uintptr_t stack_lo = lo, stack_hi = hi;
  const uint64_t expected_pcs[] = {0x4000, 0x1000, 0x2000, 0x3000};
  constexpr uint64_t sentinel = 0xdeadbeef;
  WalkStop stop{Reason::no_stack_bounds, ~uint32_t{0}, 1, 2, 3};
  auto executable = [](uintptr_t pc) { return pc >= 0x1000 && pc < 0x4000; };
  auto run = [&](const char* label, uintptr_t fp, uint32_t capacity,
                 uint32_t expected_depth, const WalkStop& expected) {
    uint64_t pcs[5], legacy[5];
    std::fill_n(pcs, 5, sentinel);
    std::fill_n(legacy, 5, sentinel);
    uint32_t depth = walk_frame_chain(0x4000, fp, pcs, capacity,
                                      stack_lo, stack_hi, executable, &stop);
    check(depth == expected_depth && same_stop(stop, expected), label);
    check(walk_frame_chain(0x4000, fp, legacy, capacity,
                           stack_lo, stack_hi, executable) == depth, label);
    for (uint32_t i = 0; i < 5; ++i) {
      check(pcs[i] == legacy[i], label);
      check(pcs[i] == (i < depth ? expected_pcs[i] : sentinel), label);
    }
  };
  run("null outer link", lo, 5, 4,
      {Reason::null_frame_pointer, HaveFrameRecord, lo + 32, 0, 0x3000});
  run("null initial FP clears reused evidence", 0, 5, 1,
      {Reason::null_frame_pointer, 0, 0, 0, 0});
  run("unaligned FP", lo + 1, 5, 1,
      {Reason::unaligned_frame_pointer, 0, lo + 1, 0, 0});
  run("FP below stack", lo - 8, 5, 1,
      {Reason::frame_outside_stack, 0, lo - 8, 0, 0});
  run("incomplete frame at upper bound", hi - 8, 5, 1,
      {Reason::frame_outside_stack, 0, hi - 8, 0, 0});
  run("unreadable out-of-stack FP", 8, 5, 1,
      {Reason::frame_outside_stack, 0, 8, 0, 0});
  frames[1] = 0;
  run("null saved PC", lo, 5, 1,
      {Reason::null_return_address, HaveFrameRecord, lo, lo + 16, 0});
  frames[1] = 0x5000;
  run("unrecognized saved PC", lo, 5, 1,
      {Reason::unrecognized_return_address, HaveFrameRecord, lo, lo + 16, 0x5000});
  frames[1] = 0x4000;
  run("exclusive executable endpoint remains rejected", lo, 5, 1,
      {Reason::unrecognized_return_address, HaveFrameRecord | PrecedingByteExecutable,
       lo, lo + 16, 0x4000});
  frames[1] = 0x1000;
  frames[2] = lo;
  run("backward link", lo, 5, 3,
      {Reason::nonadvancing_frame, HaveFrameRecord, lo + 16, lo, 0x2000});
  frames[0] = lo;
  run("self link", lo, 5, 2,
      {Reason::nonadvancing_frame, HaveFrameRecord, lo, lo, 0x1000});
  run("observed self link takes priority at cap", lo, 2, 2,
      {Reason::nonadvancing_frame, HaveFrameRecord, lo, lo, 0x1000});
  frames[0] = lo + 16; frames[2] = lo + 32;
  run("depth cap clears reused frame evidence", lo, 2, 2,
      {Reason::depth_limit, 0, lo + 16, 0, 0});
  run("observed null link takes priority at cap", lo, 4, 4,
      {Reason::null_frame_pointer, HaveFrameRecord, lo + 32, 0, 0x3000});
  stack_lo = 8; stack_hi = 24;
  run("capacity zero does not read or write", 8, 0, 0,
      {Reason::depth_limit, 0, 8, 0, 0});
  run("leaf-only capacity does not read nominally valid FP", 8, 1, 1,
      {Reason::depth_limit, 0, 8, 0, 0});
  stack_lo = hi; stack_hi = lo;
  run("reversed stack bounds", lo, 5, 1,
      {Reason::frame_outside_stack, 0, lo, 0, 0});
  stack_lo = lo; stack_hi = lo + 8;
  run("stack smaller than a frame record", lo, 5, 1,
      {Reason::frame_outside_stack, 0, lo, 0, 0});

  struct { SampleHeader header; uint64_t pcs[2]; } cpu{};
  cpu.header.depth = 2; cpu.header.thread_id = 7; cpu.header.timestamp_ns = 11;
  cpu.header.walk_stop = {Reason::unrecognized_return_address,
    HaveFrameRecord | PrecedingByteExecutable, lo + 16, lo + 32, 0x4000};
  cpu.pcs[0] = 0x4000; cpu.pcs[1] = 0x1000;
  struct { AllocationSampleHeader header; uint64_t pcs[1]; } allocation{};
  allocation.header.depth = 1; allocation.header.thread_id = 13;
  allocation.header.timestamp_ns = 17; allocation.header.sampled_bytes = 12345;
  allocation.header.walk_stop = {Reason::no_stack_bounds, 0, 8, 0, 0};
  allocation.pcs[0] = 0x2000;
  std::vector<CapturedWalkStop> records;
  auto cpu_weight = [](const SampleHeader&) { return uint64_t{0}; };
  check(!copy_walk_stops<SampleHeader>(reinterpret_cast<const uint8_t*>(&cpu),
      sizeof(cpu), sizeof(cpu), 5, cpu_weight, records), "CPU header decoding");
  check(!copy_walk_stops<AllocationSampleHeader>(
      reinterpret_cast<const uint8_t*>(&allocation), sizeof(allocation),
      sizeof(allocation), 5,
      [](const AllocationSampleHeader& h) { return h.sampled_bytes; }, records),
      "allocation header decoding");
  check(records.size() == 2 && same_stop(records[0].stop, cpu.header.walk_stop) &&
      records[0].depth == 2 && records[0].last_pc == 0x1000 &&
      records[0].thread_id == 7 && records[0].timestamp_ns == 11 &&
      records[0].attributed_bytes == 0 &&
      same_stop(records[1].stop, allocation.header.walk_stop) &&
      records[1].depth == 1 && records[1].last_pc == 0x2000 &&
      records[1].thread_id == 13 && records[1].timestamp_ns == 17 &&
      records[1].attributed_bytes == 12345, "snapshot metadata and weights");
  check(copy_walk_stops<SampleHeader>(reinterpret_cast<const uint8_t*>(&cpu),
      sizeof(cpu) - 1, sizeof(cpu), 5, cpu_weight, records) != nullptr,
      "truncated saved-PC array rejected");

  WalkStopSummary cpu_summary;
  decltype(cpu) cpu_samples[] = {cpu, cpu};
  check(!summarize_walk_stops<SampleHeader>(
      reinterpret_cast<const uint8_t*>(cpu_samples), sizeof(cpu_samples),
      sizeof(cpu_samples), 5, cpu_weight, cpu_summary), "CPU summary decoding");
  const auto& cpu_counts = cpu_summary.counts[
    static_cast<size_t>(Reason::unrecognized_return_address)];
  check(cpu_counts.samples == 2 && cpu_counts.attributed_bytes == 0,
        "CPU summary counts repeated reasons");
  check(summarize_walk_stops<SampleHeader>(reinterpret_cast<const uint8_t*>(&cpu),
      sizeof(cpu) - 1, sizeof(cpu), 5, cpu_weight, cpu_summary) != nullptr &&
      cpu_counts.samples == 2, "summary rejects truncated saved-PC array");

  WalkStopSummary allocation_summary;
  decltype(allocation) allocation_samples[] = {allocation, allocation, allocation};
  allocation_samples[1].header.sampled_bytes = 6789;
  allocation_samples[2].header.sampled_bytes = 5;
  allocation_samples[2].header.walk_stop.reason = Reason::depth_limit;
  check(!summarize_walk_stops<AllocationSampleHeader>(
      reinterpret_cast<const uint8_t*>(allocation_samples), sizeof(allocation_samples),
      sizeof(allocation_samples), 5,
      [](const AllocationSampleHeader& h) { return h.sampled_bytes; }, allocation_summary),
      "allocation summary decoding");
  const auto& missing_bounds = allocation_summary.counts[
    static_cast<size_t>(Reason::no_stack_bounds)];
  const auto& depth_limit = allocation_summary.counts[
    static_cast<size_t>(Reason::depth_limit)];
  check(missing_bounds.samples == 2 && missing_bounds.attributed_bytes == 19134 &&
      depth_limit.samples == 1 && depth_limit.attributed_bytes == 5,
      "allocation summary keeps sample counts and weighted bytes separate");
  cpu.header.walk_stop.reason = static_cast<Reason>(~uint32_t{0});
  check(!summarize_walk_stops<SampleHeader>(reinterpret_cast<const uint8_t*>(&cpu),
      sizeof(cpu), sizeof(cpu), 5, cpu_weight, cpu_summary) &&
      cpu_summary.counts[WalkStopSummary::known_reasons].samples == 1,
      "summary preserves unknown-reason fallback");
  return true;
}

CL_DOCSTRING(R"dx(Test executable-range merging on isolated tables using the
same implementation as the CPU and allocation profilers. Returns T on success;
signals an error naming the failed check otherwise. Tests adjacency in both
directions, overlaps, duplicates, gaps, capacity, reset, and address limits.
Does not start profiling or modify the live executable-range registry.)dx");
CL_LAMBDA();
DOCGROUP(clasp);
CL_DEFUN bool ext__test_profile_executable_ranges() {
  using profiler_detail::DynamicExecutableRanges;
  using Result = profiler_detail::ExecutableRangeRegistration;
  auto check = [](bool passed, const char* description) {
    if (!passed)
      SIMPLE_ERROR("Executable-range regression failed: {}", description);
  };

  DynamicExecutableRanges<1> single;
  check(single.add(100, 200) == Result::added, "initial registration");
  check(!single.contains(99) && single.contains(100) &&
        single.contains(199) && !single.contains(200), "half-open bounds");
  check(single.add(200, 300) == Result::merged, "upward adjacency at capacity");
  check(single.add(50, 100) == Result::merged, "downward adjacency at capacity");
  check(single.add(250, 400) == Result::merged, "upper overlap");
  check(single.add(25, 75) == Result::merged, "lower overlap");
  check(single.add(100, 200) == Result::merged, "contained range");
  check(single.add(25, 400) == Result::merged, "exact duplicate");
  check(single.add(10, 500) == Result::merged, "extension at both ends");
  check(single.size() == 1, "merges must not consume slots");
  for (uintptr_t pc = 10; pc < 500; ++pc)
    check(single.contains(pc), "merged interval coverage");
  check(!single.contains(9) && !single.contains(500), "merged interval bounds");

  DynamicExecutableRanges<2> gaps;
  check(gaps.add(10, 20) == Result::added, "first gap fixture");
  check(gaps.add(21, 30) == Result::added, "one-byte gap must not merge");
  check(gaps.size() == 2 && !gaps.contains(20), "gap remains unregistered");
  check(gaps.add(100, 110) == Result::full && !gaps.contains(100),
        "full table rejects disjoint range");
  check(gaps.add(30, 40) == Result::merged, "full table accepts upward merge");
  check(gaps.add(20, 21) == Result::merged, "full table accepts downward merge");
  check(gaps.size() == 2 && gaps.contains(20) && gaps.contains(39) &&
        !gaps.contains(40), "full-table merge preserves coverage and bounds");
  check(gaps.add(0, 10) == Result::full && !gaps.contains(0),
        "only the most recent entry is considered for merging");
  gaps.reset();
  check(gaps.size() == 0 && !gaps.contains(10) && !gaps.contains(39),
        "reset hides previously published slots");
  check(gaps.add(500, 600) == Result::added && gaps.size() == 1 &&
        gaps.contains(500) && !gaps.contains(10), "reuse after reset");

  DynamicExecutableRanges<3> interleaved;
  check(interleaved.add(100, 200) == Result::added &&
        interleaved.add(400, 500) == Result::added &&
        interleaved.add(200, 300) == Result::added,
        "interleaved registrations are not assumed address-ordered");
  check(interleaved.size() == 3 && !interleaved.contains(300),
        "interleaved gap remains unregistered");
  check(interleaved.add(300, 400) == Result::merged &&
        interleaved.size() == 3 && interleaved.contains(300),
        "merge last entry without moving earlier published entries");

  const uintptr_t limit = std::numeric_limits<uintptr_t>::max();
  DynamicExecutableRanges<1> edge;
  check(edge.add(0, 0) == Result::invalid &&
        edge.add(limit, limit) == Result::invalid &&
        edge.add(limit, 0) == Result::invalid && edge.size() == 0,
        "empty and reversed ranges do not consume slots");
  check(edge.add(limit - 20, limit - 10) == Result::added &&
        edge.add(limit - 10, limit) == Result::merged &&
        edge.add(limit - 30, limit - 15) == Result::merged,
        "merges near the address limit");
  check(edge.contains(limit - 30) && edge.contains(limit - 1) &&
        !edge.contains(limit - 31) && !edge.contains(limit),
        "exclusive address-limit bounds");
  check(edge.add(0, 1) == Result::full && !edge.contains(0),
        "no wraparound adjacency");
  check(edge.add(40, 30) == Result::invalid && edge.size() == 1,
        "invalid ranges are checked before capacity");
  edge.reset();
  check(!edge.contains(limit - 1) &&
        edge.add(0, 1) == Result::added && edge.add(1, 2) == Result::merged &&
        edge.contains(0) && edge.contains(1) && !edge.contains(2),
        "reset and adjacency at address zero");
  return true;
}

CL_DOCSTRING(R"dx(Return three values: filled dynamic executable ranges, capacity,
and registrations rejected because that capacity was exhausted.
These describe the shared CPU/allocation profiler executable-range cache since
its most recent rebuild, normally the start of the first active profiler.
They are not dropped-sample or truncated-stack counts. Reading the counters
does not reset them. This diagnostic takes a mutex: do not call from a signal
handler or while other threads are stopped.)dx");
CL_LAMBDA();
DOCGROUP(clasp);
CL_DEFUN core::T_mv ext__profile_executable_range_stats() {
  size_t filled, rejected;
  {
    std::lock_guard<std::mutex> guard(g_dynamic_exec_range_writer_lock);
    filled = g_dynamic_exec_ranges.size();
    rejected = g_dynamic_exec_range_rejections;
  }
  // Construct Lisp values only after releasing the mutex: an allocation can
  // itself be profiled, or cause a GC that stops another registering thread.
  return Values(Integer_O::create(filled),
                Integer_O::create(MAX_DYNAMIC_EXEC_RANGES),
                Integer_O::create(rejected));
}

CL_DOCSTRING(R"dx(Start the sampling profiler.
Args:
  rate          : sampling rate in Hz (default 97). Clamped to [1, 10000].
  max-depth     : per-sample stack cap (default 8192). Clamped to [1, 8192].
  buffer-bytes  : ring size in bytes, 0 = 256 MiB default.
Returns T on success, NIL if already running or setup failed.)dx");
CL_LAMBDA(&key (rate 97) (max-depth 8192) (buffer-bytes 0));
DOCGROUP(clasp);
CL_DEFUN bool ext__profile_start(uint rate, uint max_depth, size_t buffer_bytes) {
  return sampling_profiler_start(rate, max_depth, buffer_bytes);
}

CL_DOCSTRING(R"dx(Stop the sampling profiler. Samples remain in the buffer
until profile-save or profile-reset is called.)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__profile_stop() { sampling_profiler_stop(); }

CL_DOCSTRING(R"dx(True while the sampling profiler is running.)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__profile_running_p() { return sampling_profiler_running(); }

CL_DOCSTRING(R"dx(Discard all recorded samples and reset counters.)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__profile_reset() { sampling_profiler_reset(); }

CL_DOCSTRING(R"dx(Return the symbolicated samples as a vector of symbolicated-sample instances)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp ext__profile_symbolicated_samples() {
  std::vector<SymbolicatedSample> res = sampling_profiler_symbolicated_samples();
  core::ComplexVector_T_sp vec = core::ComplexVector_T_O::make(16384,nil<core::T_O>(),clasp_make_fixnum(0));
  for ( auto& one : res ) {
    core::T_sp obj = one.encode();
    vec->vectorPushExtend(obj);
  }
  return vec;
}

CL_DOCSTRING(R"dx(Write the captured samples to PATH.
Phase 1: one raw record per line — timestamp, tid, depth, hex PCs.
Later phases will emit symbolicated collapsed-stacks / speedscope JSON.)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__profile_save(core::String_sp path) {
  return sampling_profiler_save(path->get_std_string().c_str());
}

CL_DOCSTRING(R"dx(Return the number of samples recorded so far.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__profile_samples_recorded() {
  return sampling_profiler_samples_recorded();
}

CL_DOCSTRING(R"dx(Return the number of samples dropped because the buffer was full.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__profile_samples_dropped() {
  return sampling_profiler_samples_dropped();
}

CL_DOCSTRING(R"dx(Return the bytes used in the ring buffer so far.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__profile_bytes_used() {
  return sampling_profiler_bytes_used();
}
CL_DOCSTRING(R"dx(Return the bytes available in the ring buffer.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__profile_bytes_available() {
  return g_buffer_bytes;
}

CL_DOCSTRING(R"dx(Start managed-allocation profiling.
BYTES-PER-SAMPLE is the sampling interval; zero selects 1 MiB and smaller
values are clamped to 1 MiB. MAX-DEPTH is clamped to [1,4096].
BUFFER-BYTES zero selects a 64 MiB ring. Returns T on success.)dx");
CL_LAMBDA(&key (bytes-per-sample 0) (max-depth 4096) (buffer-bytes 0));
DOCGROUP(clasp);
CL_DEFUN bool ext__allocation_profile_start(
    size_t bytes_per_sample,
    uint max_depth,
    size_t buffer_bytes) {
  return allocation_profiler_start(
    bytes_per_sample, max_depth, buffer_bytes);
}

CL_DOCSTRING(R"dx(Stop allocation profiling, preserving recorded samples.)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__allocation_profile_stop() {
  allocation_profiler_stop();
}

CL_DOCSTRING(R"dx(Return true while allocation profiling is active.)dx");
DOCGROUP(clasp);
CL_DEFUN bool ext__allocation_profile_running_p() {
  return allocation_profiler_running();
}

CL_DOCSTRING(R"dx(Discard recorded allocation samples and counters.
The profiler must first be stopped.)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__allocation_profile_reset() {
  allocation_profiler_reset();
}

CL_DOCSTRING(R"dx(Return allocation stacks aggregated by attributed bytes.
Each stack ends in an allocation:type frame.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp
ext__allocation_profile_symbolicated_samples() {
  std::vector<SymbolicatedSample> result =
    allocation_profiler_symbolicated_samples();
  core::ComplexVector_T_sp vector =
    core::ComplexVector_T_O::make(
      result.size(), nil<core::T_O>(), clasp_make_fixnum(0));
  for (auto& sample : result)
    vector->vectorPushExtend(sample.encode());
  return vector;
}

CL_DOCSTRING(R"dx(Write allocation stacks and attributed-byte counts to PATH
in collapsed-stacks format.)dx");
DOCGROUP(clasp);
CL_DEFUN bool
ext__allocation_profile_save(core::String_sp path) {
  return allocation_profiler_save(
    path->get_std_string().c_str());
}

CL_DOCSTRING(R"dx(Return the number of allocation records captured.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__allocation_profile_samples_recorded() {
  return allocation_profiler_samples_recorded();
}

CL_DOCSTRING(R"dx(Return allocation records dropped because the ring was full.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__allocation_profile_samples_dropped() {
  return allocation_profiler_samples_dropped();
}

CL_DOCSTRING(R"dx(Return bytes represented by captured allocation records.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__allocation_profile_bytes_attributed() {
  return allocation_profiler_bytes_attributed();
}

CL_DOCSTRING(R"dx(Return bytes represented by allocation records dropped
because the ring was full.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__allocation_profile_bytes_dropped() {
  return allocation_profiler_bytes_dropped();
}

CL_DOCSTRING(R"dx(Return bytes currently used in the allocation sample ring.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__allocation_profile_bytes_used() {
  return allocation_profiler_bytes_used();
}

CL_DOCSTRING(R"dx(Return the allocation sample ring capacity in bytes.)dx");
DOCGROUP(clasp);
CL_DEFUN size_t ext__allocation_profile_bytes_available() {
  return allocation_profiler_bytes_available();
}

CL_DOCSTRING(R"dx(Populate the current thread's stack bounds so that samples
taken on this thread include full frame-pointer-walked stacks rather than
leaf-only PCs. Call once per Lisp thread that should be fully profiled,
from a safe context (not a signal handler).)dx");
DOCGROUP(clasp);
CL_DEFUN void ext__profile_register_thread() {
  sampling_profiler_register_current_thread();
}

} // namespace core
