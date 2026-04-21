/*
 * trampoline_arena.cc — interleaved-slot arena.
 *
 * Each slot is a byte-identical blob of [code | CIE | FDE | 4B terminator],
 * padded to a 16-byte stride. The caller supplies pre-composed tramp/CIE/FDE
 * byte arrays, already patched so that:
 *   - FDE's PC begin uses pcrel|sdata4 encoding whose value equals the
 *     compile-time-constant distance back to the code (= -(code_size +
 *     cie_size + 8)).
 *   - FDE's CIE pointer equals the compile-time-constant distance back to
 *     the preceding CIE (= cie_size + 4).
 *   - FDE's PC range = code_size.
 * Because every slot has the same layout, the bytes are identical across
 * slots and a single memcpy is all that's required.
 *
 * For unwinding, each slot registers its own CIE+FDE with libgcc via
 * __register_frame(slot + code_size). The trailing 4-byte zero terminator
 * in the slot stops libgcc's walk after that one FDE.
 *
 * Memory: each page is mmap'd PROT_READ|PROT_WRITE|PROT_EXEC and never
 * reprotected. Works on Linux (default policy allows RWX). On macOS arm64
 * (W^X enforced) this needs MAP_JIT + per-thread pthread_jit_write_protect_np
 * toggling, OR a dual-mapped scheme via shm_open.
 *
 * Concurrency:
 *   - allocate(): mutex-locked. Page growth happens under the same mutex.
 *   - owns() reads the page list using an atomic-published size; lock-free.
 *   - The side table is append-only with atomic-published size; lookups are
 *     lock-free and signal-safe.
 *   - Slot bytes are memcpy'd and FDE registered before side-table publish,
 *     so a concurrent reader either sees no entry yet, or sees one whose
 *     memory is fully populated and executable.
 */

#include <sys/mman.h>
#include <unistd.h>
#include <cstring>
#include <cstdio>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/pointer.h>
#include <clasp/llvmo/trampoline_arena.h>

namespace llvmo {

// ============================================================================
// TrampolineSideTable
// ============================================================================

TrampolineSideTable::TrampolineSideTable() {
  // Reserve a generous max capacity up front so push_back never reallocates,
  // keeping pointers to existing entries stable for lock-free readers.
  _entries.reserve(1u << 20);
}

void TrampolineSideTable::append(TrampolineEntry e) {
  std::lock_guard<std::mutex> g(_write_lock);
  _entries.push_back(std::move(e));
  _published.store(_entries.size(), std::memory_order_release);
}

const TrampolineEntry* TrampolineSideTable::find(uintptr_t pc) const {
  // Linear scan. mmap places arena pages at non-monotonic addresses, so a
  // binary search on a flat vector won't work. With ~100K entries this scan
  // is a few hundred microseconds — fine for backtrace frequency.
  size_t n = _published.load(std::memory_order_acquire);
  for (size_t i = 0; i < n; ++i) {
    const TrampolineEntry& e = _entries[i];
    uintptr_t s = (uintptr_t)e.code_start;
    if (s <= pc && pc < s + e.code_size) return &e;
  }
  return nullptr;
}

// ============================================================================
// ExecutableArena
// ============================================================================

// libgcc unwind-info registration. The pointer passed to __register_frame
// must remain valid for the registration's lifetime — each arena slot holds
// its own CIE+FDE inline, so that's automatic (slots are never freed).
extern "C" void __register_frame(const void* begin);

ExecutableArena::ExecutableArena(const uint8_t* tramp_bytes, size_t tramp_size,
                                 const uint8_t* cie_bytes,   size_t cie_len,
                                 const uint8_t* fde_bytes,   size_t fde_len)
    : _tramp_size(tramp_size),
      _cie_size(cie_len),
      _fde_size(fde_len) {
  // Slot stride: code + CIE + FDE + 4B terminator, padded to 16-byte
  // alignment so each new slot's code starts on a boundary the CPU likes.
  size_t payload = tramp_size + cie_len + fde_len + 4;
  _slot_stride = (payload + 15u) & ~size_t(15);
  _slot_template.assign(_slot_stride, 0);
  std::memcpy(_slot_template.data(), tramp_bytes, tramp_size);
  std::memcpy(_slot_template.data() + tramp_size, cie_bytes, cie_len);
  std::memcpy(_slot_template.data() + tramp_size + cie_len, fde_bytes, fde_len);
  // Last 4 bytes of the payload are the zero terminator — already zero from
  // assign(_slot_stride, 0). Remaining padding bytes are also zero, harmless.

  // 64 KB chunks. Bigger than the OS page (4 KB on Linux, 16 KB on macOS arm64)
  // so each mmap holds many slots.
  size_t os_page = (size_t)sysconf(_SC_PAGESIZE);
  size_t desired = 64u * 1024u;
  _page_size = ((desired + os_page - 1) / os_page) * os_page;
  _pages.reserve(1u << 14);
}

uint8_t* ExecutableArena::allocate() {
  std::lock_guard<std::mutex> g(_lock);

  if (!_current_page || _current_offset + _slot_stride > _page_size) {
    void* p = mmap(nullptr, _page_size,
                   PROT_READ | PROT_WRITE | PROT_EXEC,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) {
      perror("[trampoline-arena] mmap");
      abort();
    }
    _current_page = (uint8_t*)p;
    _current_offset = 0;
    _pages.push_back({(uintptr_t)p, (uintptr_t)p + _page_size});
    _pages_published.store(_pages.size(), std::memory_order_release);
  }

  uint8_t* slot = _current_page + _current_offset;
  std::memcpy(slot, _slot_template.data(), _slot_stride);
  // Register this slot's CIE+FDE with libgcc. The CIE starts at
  // slot + _tramp_size; the FDE follows; the 4B zero terminator after the
  // FDE stops libgcc's classify walk.
  __register_frame(slot + _tramp_size);
  _current_offset += _slot_stride;
  return slot;
}

bool ExecutableArena::owns(uintptr_t pc) const {
  size_t n = _pages_published.load(std::memory_order_acquire);
  for (size_t i = 0; i < n; ++i) {
    const PageRange& r = _pages[i];
    if (r.start <= pc && pc < r.end) return true;
  }
  return false;
}

// ============================================================================
// Top-level arena state and APIs
// ============================================================================

namespace {

class TrampolineArenaInstance {
public:
  TrampolineArenaInstance(const char* label) : _label(label) {}

  bool install_template(const uint8_t* tramp_bytes, size_t tramp_size,
                        const uint8_t* cie_bytes,   size_t cie_len,
                        const uint8_t* fde_bytes,   size_t fde_len) {
    std::lock_guard<std::mutex> g(_init_lock);
    if (_initialized.load(std::memory_order_relaxed)) return true;
    if (!tramp_bytes || tramp_size == 0) {
      fprintf(stderr, "[trampoline-arena] %s install_template: invalid tramp args\n", _label);
      return false;
    }
    if (!cie_bytes || cie_len == 0 || !fde_bytes || fde_len == 0) {
      fprintf(stderr, "[trampoline-arena] %s install_template: invalid CIE/FDE args\n", _label);
      return false;
    }
    _tramp_size = tramp_size;
    _arena = new ExecutableArena(tramp_bytes, tramp_size,
                                 cie_bytes,   cie_len,
                                 fde_bytes,   fde_len);
    _side_table = new TrampolineSideTable();
    fprintf(stderr,
            "[trampoline-arena] installed %s template: code=%zu CIE=%zu FDE=%zu\n",
            _label, tramp_size, cie_len, fde_len);
    fprintf(stderr, "[trampoline-arena] %s code:", _label);
    for (size_t i = 0; i < tramp_size; ++i) fprintf(stderr, " %02x", tramp_bytes[i]);
    fprintf(stderr, "\n[trampoline-arena] %s cie:", _label);
    for (size_t i = 0; i < cie_len; ++i) fprintf(stderr, " %02x", cie_bytes[i]);
    fprintf(stderr, "\n[trampoline-arena] %s fde:", _label);
    for (size_t i = 0; i < fde_len; ++i) fprintf(stderr, " %02x", fde_bytes[i]);
    fprintf(stderr, "\n");
    fflush(stderr);
    _initialized.store(true, std::memory_order_release);
    return true;
  }

  bool is_initialized() const {
    return _initialized.load(std::memory_order_acquire);
  }

  core::Pointer_sp compile(const std::string& name) {
    if (!is_initialized()) {
      fprintf(stderr, "[trampoline-arena] %s compile before init\n", _label);
      abort();
    }
    uint8_t* slot = _arena->allocate();
    // No per-slot patching: the slot template is byte-identical across
    // all slots (the call target is embedded as an absolute immediate in
    // the template by LLVM at capture time; FDE fields use pcrel-sdata4
    // distances that are constant within the slot layout).
    _side_table->append(TrampolineEntry{slot, (uint32_t)_tramp_size, name});
    perf_map_append(slot, _tramp_size, name);
    int n = _debug_count.fetch_add(1);
    if (n < 3) {
      fprintf(stderr, "[trampoline-arena] %s compile #%d '%s' -> %p\n",
              _label, n, name.c_str(), slot);
      fflush(stderr);
    }
    return core::Pointer_O::create((void*)slot);
  }

  const TrampolineEntry* lookup_if_owned(uintptr_t pc) const {
    if (!is_initialized()) return nullptr;
    if (!_arena->owns(pc)) return nullptr;
    return _side_table->find(pc);
  }

  bool owns(uintptr_t pc) const {
    return is_initialized() && _arena->owns(pc);
  }

private:
  const char*           _label;
  std::atomic<bool>     _initialized{false};
  std::mutex            _init_lock;
  size_t                _tramp_size = 0;
  ExecutableArena*      _arena = nullptr;
  TrampolineSideTable*  _side_table = nullptr;
  std::atomic<int>      _debug_count{0};
};

TrampolineArenaInstance g_bytecode("bytecode trampoline");
TrampolineArenaInstance g_gf("GF trampoline");

FILE*                 g_perf_map = nullptr;
std::mutex            g_perf_map_lock;
}  // anonymous namespace

// perf-PID.map writer — opens lazily, one append per registered entry.
// Shared across callers (both arena instances and the LLVM-ORC link-plugin
// per-symbol callback); the first caller truncates any stale file, subsequent
// callers append.
void perf_map_append(uint8_t* addr, size_t size, const std::string& name) {
  std::lock_guard<std::mutex> g(g_perf_map_lock);
  if (!g_perf_map) {
    char path[64];
    snprintf(path, sizeof path, "/tmp/perf-%d.map", getpid());
    g_perf_map = fopen(path, "w");
    if (!g_perf_map) return;
  }
  fprintf(g_perf_map, "%lx %zx %s\n", (uintptr_t)addr, size, name.c_str());
  fflush(g_perf_map);
}

// -------------------------------------------------------------------------
// Public C-level wrappers — delegate to the appropriate arena instance.
// -------------------------------------------------------------------------

bool arena_install_trampoline_template(const uint8_t* tramp_bytes, size_t tramp_size,
                                        const uint8_t* cie_bytes,   size_t cie_len,
                                        const uint8_t* fde_bytes,   size_t fde_len) {
  return g_bytecode.install_template(tramp_bytes, tramp_size,
                                     cie_bytes,   cie_len,
                                     fde_bytes,   fde_len);
}
bool arena_is_initialized() { return g_bytecode.is_initialized(); }
core::Pointer_sp arena_compile_trampoline(const std::string& name) {
  return g_bytecode.compile(name);
}

bool gf_arena_install_trampoline_template(const uint8_t* tramp_bytes, size_t tramp_size,
                                           const uint8_t* cie_bytes,   size_t cie_len,
                                           const uint8_t* fde_bytes,   size_t fde_len) {
  return g_gf.install_template(tramp_bytes, tramp_size,
                               cie_bytes,   cie_len,
                               fde_bytes,   fde_len);
}
bool gf_arena_is_initialized() { return g_gf.is_initialized(); }
core::Pointer_sp gf_arena_compile_trampoline(const std::string& name) {
  return g_gf.compile(name);
}

// Unified lookup — checks both arenas so backtrace / debugger code can
// resolve a PC without caring which kind it is.
const TrampolineEntry* arena_lookup_by_pc(uintptr_t pc) {
  if (const TrampolineEntry* e = g_bytecode.lookup_if_owned(pc)) return e;
  return g_gf.lookup_if_owned(pc);
}

bool arena_owns_pc(uintptr_t pc) {
  return g_bytecode.owns(pc) || g_gf.owns(pc);
}

}; // namespace llvmo
