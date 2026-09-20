// Standalone Linux regression test; no built Clasp image is required.
// From the repository root:
// c++ -std=c++17 -O2 -g -fno-omit-frame-pointer -fno-optimize-sibling-calls \
//   -rdynamic -Iinclude src/tests/cxx/profilerStack.cc -pthread -ldl -o /tmp/profiler-stack-test
// timeout 10 /tmp/profiler-stack-test
// On AArch64 also run with -mno-omit-leaf-frame-pointer and
// -mbranch-protection=pac-ret to exercise signed return addresses.
#include <clasp/core/profilerStack.h>
#include <clasp/llvmo/trampoline_aarch64.h>
#include <algorithm>
#include <cassert>
#include <csignal>
#include <cstdio>
#include <cstring>
#include <dlfcn.h>
#include <fstream>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>
#include <vector>

using namespace core::profiler_detail;

static void test_frame_records() {
  alignas(16) uintptr_t frames[6] = {};
  uintptr_t lo = reinterpret_cast<uintptr_t>(frames);
  uintptr_t hi = lo + sizeof frames;
  frames[0] = lo + 16; frames[1] = 0x1000;
  frames[2] = lo + 32; frames[3] = 0x2000;
  frames[4] = 0; frames[5] = 0x3000;
  uint64_t out[5] = {};
  auto executable = [](uintptr_t pc) { return pc >= 0x1000 && pc <= 0x3000; };
  auto walk = [&](uintptr_t fp, unsigned cap) {
    return walk_frame_chain(0x4000, fp, out, cap, lo, hi, executable);
  };
  assert(walk(lo, 5) == 4);
  assert(out[0] == 0x4000 && out[1] == 0x1000 && out[3] == 0x3000);
  out[2] = 0xdead;
  assert(walk(lo, 2) == 2 && out[2] == 0xdead);
  assert(walk(lo, 0) == 0);
  assert(walk(0, 5) == 1);
  assert(walk(lo + 1, 5) == 1);
  assert(walk(lo - 8, 5) == 1);
  assert(walk(hi - 8, 5) == 1);
  assert(!valid_frame_pointer(lo, hi, lo));
  assert(!valid_frame_pointer(lo, lo, lo + 8));
  frames[2] = lo; // Cycle: must stop rather than revisit earlier frames.
  assert(walk(lo, 5) == 3);
  frames[0] = lo; // Self-link.
  assert(walk(lo, 5) == 2);
  frames[1] = 0x5000; // Non-executable saved address.
  assert(walk(lo, 5) == 1);

  // An incomplete record at a guard page must never read the next word.
  size_t page = sysconf(_SC_PAGESIZE);
  auto* memory = static_cast<char*>(mmap(nullptr, 2 * page, PROT_READ | PROT_WRITE,
                                       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
  assert(memory != MAP_FAILED);
  assert(mprotect(memory + page, page, PROT_NONE) == 0);
  uintptr_t end = reinterpret_cast<uintptr_t>(memory + page);
  assert(walk_frame_chain(0x4000, end - 8, out, 5, end - page, end, executable) == 1);
  assert(munmap(memory, 2 * page) == 0);
}

struct Range { uintptr_t lo, hi; };
static std::vector<Range> executable_ranges;
static uintptr_t stack_lo, stack_hi;
static uint64_t sampled_pcs[64];
static uint32_t sampled_depth;
static volatile sig_atomic_t sampled = 0;

static void on_sigprof(int, siginfo_t*, void* context) {
  if (sampled) return;
  auto executable = [](uintptr_t pc) {
    for (const auto& range : executable_ranges)
      if (pc >= range.lo && pc < range.hi) return true;
    return false;
  };
  sampled_depth = walk_frame_chain(context_pc(context), context_fp(context),
                                  sampled_pcs, 64,
                                  std::max(stack_lo, context_sp(context)), stack_hi,
                                  executable);
  sampled = 1;
}

extern "C" __attribute__((noinline)) void profiler_test_leaf() {
  while (!sampled) __asm__ volatile("" ::: "memory");
}
static void (*leaf_entry)() = profiler_test_leaf;
extern "C" __attribute__((noinline)) void profiler_test_middle() {
  leaf_entry();
  __asm__ volatile("" ::: "memory");
}
extern "C" __attribute__((noinline)) void profiler_test_outer() {
  profiler_test_middle();
  __asm__ volatile("" ::: "memory");
}

int main() {
  test_frame_records();
  pthread_attr_t attr;
  assert(pthread_getattr_np(pthread_self(), &attr) == 0);
  void* base;
  size_t size;
  assert(pthread_attr_getstack(&attr, &base, &size) == 0);
  pthread_attr_destroy(&attr);
  stack_lo = reinterpret_cast<uintptr_t>(base);
  stack_hi = stack_lo + size;
  std::ifstream maps("/proc/self/maps");
  std::string line;
  while (std::getline(maps, line)) {
    unsigned long lo, hi;
    char permissions[5];
    if (sscanf(line.c_str(), "%lx-%lx %4s", &lo, &hi, permissions) == 3 &&
        permissions[2] == 'x')
      executable_ranges.push_back({lo, hi});
  }
  assert(!executable_ranges.empty());
#if defined(__aarch64__)
  // Exercise Clasp's actual generated-code frame layout as well as C++ frames.
  size_t page = sysconf(_SC_PAGESIZE);
  void* trampoline = mmap(nullptr, page, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(trampoline != MAP_FAILED);
  using namespace llvmo::trampoline_aarch64;
  memcpy(trampoline, gf_code, gf_code_size);
  uintptr_t target = reinterpret_cast<uintptr_t>(&profiler_test_leaf);
  memcpy(static_cast<char*>(trampoline) + gf_target_offset, &target, sizeof target);
  __builtin___clear_cache(static_cast<char*>(trampoline), static_cast<char*>(trampoline) + gf_code_size);
  assert(mprotect(trampoline, page, PROT_READ | PROT_EXEC) == 0);
  uintptr_t trampoline_pc = reinterpret_cast<uintptr_t>(trampoline);
  executable_ranges.push_back({trampoline_pc, trampoline_pc + gf_code_size});
  constexpr unsigned runs = 2;
#else
  constexpr unsigned runs = 1;
#endif
  for (unsigned run = 0; run < runs; ++run) {
#if defined(__aarch64__)
  if (run == 1) leaf_entry = reinterpret_cast<void (*)()>(trampoline);
#endif
  sampled = 0;
  struct sigaction action{};
  action.sa_sigaction = on_sigprof;
  action.sa_flags = SA_SIGINFO;
  sigemptyset(&action.sa_mask);
  assert(sigaction(SIGPROF, &action, nullptr) == 0);
  itimerval timer{};
  timer.it_value.tv_usec = 1000;
  timer.it_interval.tv_usec = 1000;
  assert(setitimer(ITIMER_PROF, &timer, nullptr) == 0);
  profiler_test_outer();
  timer = {};
  assert(setitimer(ITIMER_PROF, &timer, nullptr) == 0);

  // Symbolication happens only after sampling, never in the signal handler.
  const char* expected[] = {"profiler_test_leaf", "profiler_test_middle", "profiler_test_outer"};
  unsigned matched = 0;
  bool saw_trampoline = false;
  for (uint32_t i = 0; i < sampled_depth; ++i) {
#if defined(__aarch64__)
    if (sampled_pcs[i] >= trampoline_pc && sampled_pcs[i] < trampoline_pc + gf_code_size)
      saw_trampoline = true;
#endif
    Dl_info info{};
    if (dladdr(reinterpret_cast<void*>(sampled_pcs[i]), &info) && info.dli_sname) {
      printf("  %s\n", info.dli_sname);
      if (matched < 3 && strcmp(info.dli_sname, expected[matched]) == 0) ++matched;
    }
  }
  assert(matched == 3);
  if (run == 1) {
    assert(saw_trampoline);
    puts("PASS: real SIGPROF call chain through Clasp's ARM64 GF trampoline");
  }
  }
#if defined(__aarch64__)
  assert(munmap(trampoline, page) == 0);
#endif
  puts("PASS: frame bounds, depth limits, malformed chains, guard page, and real SIGPROF call chain");
}
