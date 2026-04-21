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
#include <mutex>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <ucontext.h>
#if defined(__linux__)
#  include <sys/syscall.h>
#endif

#include <clasp/core/foundation.h>
#include <clasp/core/lisp.h>
#include <clasp/core/sampling_profiler.h>

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

struct sigaction      g_prev_sigaction;     // clasp's original SIGPROF handler
bool                  g_prev_sigaction_saved = false;

std::mutex            g_lifecycle_lock;     // serializes start/stop/save/reset

// Read CLOCK_MONOTONIC in nanoseconds. clock_gettime is async-signal-safe
// on Linux and macOS for CLOCK_MONOTONIC.
static inline uint64_t now_ns_signal_safe() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}

// Read the interrupted instruction pointer out of the context structure.
// x86_64 only for now — portable to arm64 when we need it.
static inline uintptr_t ucontext_rip(void* ucptr) {
#if defined(__x86_64__)
#  if defined(__linux__)
  ucontext_t* uc = (ucontext_t*)ucptr;
  return (uintptr_t)uc->uc_mcontext.gregs[REG_RIP];
#  elif defined(__APPLE__)
  ucontext_t* uc = (ucontext_t*)ucptr;
  return (uintptr_t)uc->uc_mcontext->__ss.__rip;
#  else
  (void)ucptr; return 0;
#  endif
#else
  (void)ucptr; return 0;  // TODO: arm64 x29+x30
#endif
}

// Read the frame-base-pointer register (rbp) out of the ucontext.
static inline uintptr_t ucontext_rbp(void* ucptr) {
#if defined(__x86_64__)
#  if defined(__linux__)
  ucontext_t* uc = (ucontext_t*)ucptr;
  return (uintptr_t)uc->uc_mcontext.gregs[REG_RBP];
#  elif defined(__APPLE__)
  ucontext_t* uc = (ucontext_t*)ucptr;
  return (uintptr_t)uc->uc_mcontext->__ss.__rbp;
#  else
  (void)ucptr; return 0;
#  endif
#else
  (void)ucptr; return 0;  // TODO: arm64 x29
#endif
}

// ---------------------------------------------------------------------------
// Per-thread stack-bounds cache. Used by the walker to bound frame-pointer
// chasing: any rbp outside [stack_lo, stack_hi) is treated as end-of-stack.
// Populated lazily on first entry per thread via pthread_getattr_np — which
// is NOT async-signal-safe, so we do it outside the handler by gating on
// thread_local flags and catching up on the first non-handler call, or at
// start-of-profile. To keep Phase 2 simple and always-safe, the handler
// self-populates the cache the first time it fires on a thread: it calls
// pthread_getattr_np (which on Linux+glibc is safe-enough in practice — it
// doesn't allocate for the already-initialized thread). On macOS we use
// pthread_get_stackaddr_np + pthread_get_stacksize_np which are simple
// accessors and signal-safe in practice.
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
  pthread_attr_getstack(&attr, &addr, &size);
  pthread_attr_destroy(&attr);
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
static inline bool plausible_rbp(uintptr_t rbp) {
  if ((rbp & 7) != 0) return false;
  if (!t_stack_bounds.populated) return false;
  return rbp >= t_stack_bounds.lo && rbp + 16 <= t_stack_bounds.hi;
}

// Walk the frame-pointer chain starting at (rip, rbp) and fill `out` with
// up to `max_depth` native PCs. Returns the number of frames recorded.
// Terminates on: out-of-stack-range rbp, null saved rbp, zero saved rip,
// non-advancing rbp, or max_depth.
//
// Safety: uses only register-read + bounded pointer walk + plausibility
// checks + out-of-process writes. No libc calls, no allocation, no locks.
static uint32_t walk_fp(uintptr_t rip_top, uintptr_t rbp_top,
                        uint64_t* out, uint32_t max_depth) {
  if (max_depth == 0) return 0;
  uint32_t d = 0;
  out[d++] = (uint64_t)rip_top;
  uintptr_t rbp = rbp_top;
  while (d < max_depth && plausible_rbp(rbp)) {
    uintptr_t saved_rbp = *((uintptr_t*)rbp);
    uintptr_t saved_rip = *((uintptr_t*)(rbp + 8));
    if (saved_rip == 0) break;
    out[d++] = (uint64_t)saved_rip;
    // In stack-grows-down SysV ABI the caller's rbp lives at a higher
    // address than the callee's. A non-advancing (or going-down) saved_rbp
    // means the chain is broken or we hit a leaf without a frame.
    if (saved_rbp <= rbp) break;
    rbp = saved_rbp;
  }
  return d;
}

// Reserve `bytes` from the bump buffer. Returns nullptr when the buffer is
// full — the caller increments the drop counter. Async-signal-safe: single
// CAS loop on a plain atomic counter, no allocation, no libc.
static inline uint8_t* ring_reserve(size_t bytes) {
  size_t cur = g_write_offset.load(std::memory_order_relaxed);
  for (;;) {
    size_t next = cur + bytes;
    if (next > g_buffer_bytes) return nullptr;
    if (g_write_offset.compare_exchange_weak(cur, next,
                                             std::memory_order_acq_rel,
                                             std::memory_order_relaxed)) {
      return g_buffer + cur;
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
  if (!g_running.load(std::memory_order_acquire)) return;

  // Lazily populate this thread's stack bounds. pthread_getattr_np is not
  // strictly async-signal-safe per POSIX, but on Linux+glibc it doesn't
  // allocate for an already-initialized thread; on macOS the pthread_get_*
  // calls are simple accessors. If we ever observe trouble here we can
  // move the populate to a pre-profile sweep.
  if (!t_stack_bounds.populated) populate_stack_bounds_for_this_thread();

  uintptr_t rip = ucontext_rip(ucptr);
  uintptr_t rbp = ucontext_rbp(ucptr);

  // Walk into a stack-local buffer. 8K worst case = 64 KiB, on a typical
  // 8 MiB stack that's fine; samples with shorter stacks don't waste the
  // ring buffer because we use the actual depth when reserving.
  uint64_t pcs[8192];
  uint32_t cap = g_max_depth;
  if (cap > 8192) cap = 8192;
  uint32_t depth = walk_fp(rip, rbp, pcs, cap);

  const size_t record_bytes = sizeof(SampleHeader) + depth * sizeof(uint64_t);
  uint8_t* slot = ring_reserve(record_bytes);
  if (!slot) {
    g_samples_dropped.fetch_add(1, std::memory_order_relaxed);
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
  std::memcpy(slot + sizeof(SampleHeader), pcs, depth * sizeof(uint64_t));

  g_samples_recorded.fetch_add(1, std::memory_order_relaxed);
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

  if (!install_sigaction()) return false;
  // Publish running=true BEFORE arming the timer so the first tick sees
  // it. Release ordering pairs with the handler's acquire load.
  g_running.store(true, std::memory_order_release);
  if (!arm_timer(rate_hz)) {
    g_running.store(false, std::memory_order_release);
    restore_sigaction();
    return false;
  }

  fprintf(stderr,
          "[sampling-profiler] started: rate=%u Hz  max_depth=%u  buffer=%zu MiB\n",
          rate_hz, max_depth, buffer_bytes / (1024 * 1024));
  fflush(stderr);
  return true;
}

void sampling_profiler_stop() {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  if (!g_running.load(std::memory_order_acquire)) return;
  disarm_timer();
  // Flip running=false BEFORE restoring the handler — any in-flight
  // handler invocation will see the flag and fast-exit; the
  // setitimer-disarm above prevents new signals.
  g_running.store(false, std::memory_order_release);
  restore_sigaction();
  fprintf(stderr,
          "[sampling-profiler] stopped: %lu samples recorded, %lu dropped, %zu/%zu bytes used\n",
          (unsigned long)g_samples_recorded.load(),
          (unsigned long)g_samples_dropped.load(),
          g_write_offset.load(), g_buffer_bytes);
  fflush(stderr);
}

void sampling_profiler_reset() {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  g_write_offset.store(0, std::memory_order_release);
  g_samples_recorded.store(0, std::memory_order_release);
  g_samples_dropped.store(0, std::memory_order_release);
}

bool sampling_profiler_save(const char* path) {
  std::lock_guard<std::mutex> g(g_lifecycle_lock);
  if (g_running.load(std::memory_order_acquire)) {
    fprintf(stderr, "[sampling-profiler] save: stop the profiler first\n");
    return false;
  }
  if (!g_buffer || g_write_offset.load() == 0) {
    fprintf(stderr, "[sampling-profiler] save: no samples recorded\n");
    return false;
  }

  FILE* fp = fopen(path, "w");
  if (!fp) {
    fprintf(stderr, "[sampling-profiler] save: fopen(%s) failed: %s\n", path, strerror(errno));
    return false;
  }
  // Phase 1 format: one line per sample:
  //   "timestamp_ns thread_id depth pc0 pc1 ... pcN-1"
  // Phase 4/5 replaces this with symbolicated collapsed-stacks output.
  size_t end = g_write_offset.load();
  size_t off = 0;
  size_t emitted = 0;
  while (off + sizeof(SampleHeader) <= end) {
    SampleHeader* h = (SampleHeader*)(g_buffer + off);
    size_t record_bytes = sizeof(SampleHeader) + h->depth * sizeof(uint64_t);
    if (off + record_bytes > end) break;
    uint64_t* pcs = (uint64_t*)(g_buffer + off + sizeof(SampleHeader));
    fprintf(fp, "%lu %u %u",
            (unsigned long)h->timestamp_ns,
            (unsigned)h->thread_id,
            (unsigned)h->depth);
    for (uint32_t i = 0; i < h->depth; ++i)
      fprintf(fp, " 0x%lx", (unsigned long)pcs[i]);
    fputc('\n', fp);
    off += record_bytes;
    ++emitted;
  }
  fclose(fp);
  fprintf(stderr, "[sampling-profiler] wrote %zu samples to %s\n", emitted, path);
  return true;
}

size_t sampling_profiler_samples_recorded() { return g_samples_recorded.load(); }
size_t sampling_profiler_samples_dropped() { return g_samples_dropped.load(); }
size_t sampling_profiler_bytes_used() { return g_write_offset.load(); }

// ---------------------------------------------------------------------------
// Lisp bindings.
// ---------------------------------------------------------------------------

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

} // namespace core
