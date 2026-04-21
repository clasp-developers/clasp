/*
 * trampoline_arena.cc — implementation of the stub-design arena.
 *
 * Memory: each page is mmap'd PROT_READ|PROT_WRITE|PROT_EXEC and never
 * reprotected. Works on Linux (default policy allows RWX). On macOS arm64
 * (W^X enforced) this needs MAP_JIT + per-thread pthread_jit_write_protect_np
 * toggling, OR a dual-mapped scheme via shm_open. Both can be added behind
 * the ExecutableArena::allocate() API without touching callers.
 *
 * Concurrency:
 *   - allocate(): mutex-locked. Page growth happens under the same mutex.
 *   - owns() reads the page list using an atomic-published size; lock-free.
 *   - The side table is append-only with atomic-published size; lookups are
 *     lock-free and signal-safe.
 *   - Stub bytes are written before the side table publish, so a concurrent
 *     reader either sees no entry yet, or sees one whose memory is fully
 *     populated and executable.
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
  // 1M * ~64B = ~64 MB virtual; real RSS scales with actual entries.
  _entries.reserve(1u << 20);
}

void TrampolineSideTable::append(TrampolineEntry e) {
  std::lock_guard<std::mutex> g(_write_lock);
  _entries.push_back(std::move(e));
  _published.store(_entries.size(), std::memory_order_release);
}

const TrampolineEntry* TrampolineSideTable::find(uintptr_t pc) const {
  // Linear scan. We can't binary-search because entries are appended in
  // compile order, but mmap may place arena pages at non-monotonic addresses
  // (each new page often comes back at a *lower* address than the previous
  // one). With ~100K entries this scan is a few hundred microseconds — fine
  // for backtrace frequency. If hot, add a per-page lookup index later.
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

// libgcc unwind-info registration. Tells the C++ unwinder that the given
// eh_frame data describes function unwind rules; subsequent throws whose
// frame walk hits PCs covered by the registered FDEs will use these rules.
extern "C" void __register_frame(const void* begin);

ExecutableArena::ExecutableArena(size_t slot_size, size_t slot_alignment, size_t slot_code_size)
    : _slot_size(slot_size),
      _slot_alignment(slot_alignment),
      _slot_code_size(slot_code_size) {
  // 64 KB chunks. Bigger than the OS page (4 KB on Linux, 16 KB on macOS arm64)
  // so each mmap holds many slots and we register far fewer eh_frame objects
  // with libgcc — the unwinder walks the registered-objects list, so fewer is
  // faster. Rounded up to an OS-page multiple as a defensive measure.
  size_t os_page = (size_t)sysconf(_SC_PAGESIZE);
  size_t desired = 64u * 1024u;
  _page_size = ((desired + os_page - 1) / os_page) * os_page;
  _pages.reserve(1u << 14);
}

// x86_64 eh_frame templates. The CIE and FDE CFI bytes are fixed for our
// stub layout — every slot in every arena page has the same prologue/body/
// epilogue, so the same CFI rules apply uniformly. The PC begin field of
// each FDE is the only thing that varies per-slot (it points at the slot's
// absolute address).
//
// PC encoding is DW_EH_PE_absptr (8-byte absolute) rather than the more
// common pcrel|sdata4. The eh_frame buffer is heap-allocated (std::vector
// inside a PageRange) while the arena slots are mmap'd; on 64-bit Linux the
// two are routinely > 2 GB apart, which overflows sdata4 and leaves libgcc
// computing a garbage PC begin. absptr sidesteps the problem entirely.
//
// Layout per FDE (40 bytes total):
//   +0   length     = 0x24  (36 bytes follow)
//   +4   CIE ptr    = offset back to the per-page CIE (filled in at build)
//   +8   PC begin   = 8-byte absolute address of the slot
//   +16  PC range   = 8-byte slot_code_size
//   +24  aug length = 0 (1 byte) + 15 bytes CFI

namespace {
// CIE: 24 bytes including 4-byte length header.
const uint8_t kCieTemplate[24] = {
  0x14, 0x00, 0x00, 0x00,       // length = 20
  0x00, 0x00, 0x00, 0x00,       // CIE ID = 0
  0x01,                         // version
  0x7a, 0x52, 0x00,             // augmentation "zR\0"
  0x01,                         // code align factor (ULEB128)
  0x78,                         // data align factor (SLEB128 = -8)
  0x10,                         // return register (16 = RIP)
  0x01,                         // augmentation length
  0x00,                         // FDE encoding: DW_EH_PE_absptr (8-byte abs)
  0x0c, 0x07, 0x08,             // DW_CFA_def_cfa rsp 8 (initial CFA = rsp+8)
  0x90, 0x01,                   // DW_CFA_offset RIP 1 (RIP at CFA-8)
  0x00, 0x00,                   // padding nops
};
// CFI for one FDE — describes the prologue/epilogue of our trampoline.
// Expected layout (~42 bytes of code, padded by LLVM):
//   0:  endbr64                (4)
//   4:  push rbp               (1)   -> offset 5
//   5:  mov rbp, rsp           (3)   -> offset 8
//   8:  sub rsp, 0x20          (4)   -> offset 12
//   12: mov [rbp-0x20], rsi    (4)   -> offset 16  (closure)
//   16: mov [rbp-0x18], rdx    (4)   -> offset 20  (nargs)
//   20: mov [rbp-0x10], rcx    (4)   -> offset 24  (args)
//   24: movabs rax, imm64      (10)  -> offset 34
//   34: call rax               (2)   -> offset 36
//   36: add rsp, 0x20          (4)   -> offset 40
//   40: pop rbp                (1)   -> offset 41
//   41: ret                    (1)   -> offset 42
// CFA rules:
//   [0, 5):    initial — CFA = rsp+8 (from CIE)
//   [5, 8):    after push rbp   — CFA = rsp+16, rbp saved at CFA-16
//   [8, 41):   after mov rbp,rsp — CFA = rbp+16 (unchanged by sub rsp / restores)
//   [41, ...): after pop rbp    — CFA = rsp+8 again
const uint8_t kFdeCfiBytes[16] = {
  0x00,                         // aug length = 0 (followed by 15 bytes of CFI)
  0x45,                         // DW_CFA_advance_loc 5 (after endbr64+push rbp)
  0x0e, 0x10,                   // DW_CFA_def_cfa_offset 16
  0x86, 0x02,                   // DW_CFA_offset rbp 2 (rbp at CFA-16)
  0x43,                         // DW_CFA_advance_loc 3 (after mov rbp, rsp)
  0x0d, 0x06,                   // DW_CFA_def_cfa_register rbp
  0x61,                         // DW_CFA_advance_loc 33 (after pop rbp, at offset 41)
  0x0c, 0x07, 0x08,             // DW_CFA_def_cfa rsp 8 (post-epilogue)
  0x00, 0x00, 0x00,             // nop padding to 16 bytes
};
constexpr size_t kCieSize = sizeof(kCieTemplate);
constexpr size_t kFdeSize = 40;
}  // anonymous namespace

void ExecutableArena::registerEhFrameForPage(uint8_t* page) {
  // The page is just-mmap'd. Build per-page eh_frame data covering all the
  // slots in it. Slots in the page are at byte offsets 0, slot_size,
  // 2*slot_size, ... up to (slots_per_page - 1) * slot_size.
  size_t slots_per_page = _page_size / _slot_size;
  size_t total_size = kCieSize + slots_per_page * kFdeSize + 4;  // +4 zero terminator

  // Initialize the storage in the just-pushed PageRange's eh_frame member.
  PageRange& pr = _pages.back();
  pr.eh_frame.assign(total_size, 0);
  uint8_t* eh = pr.eh_frame.data();

  // Copy the CIE.
  std::memcpy(eh, kCieTemplate, kCieSize);

  // Build each FDE. With DW_EH_PE_absptr in the CIE, PC begin and PC range
  // are both 8-byte absolute values.
  for (size_t i = 0; i < slots_per_page; ++i) {
    uint8_t* fde = eh + kCieSize + i * kFdeSize;
    // length = 36 (0x24): bytes following the length field.
    fde[0] = 0x24; fde[1] = 0x00; fde[2] = 0x00; fde[3] = 0x00;
    // CIE pointer = offset from this field (fde+4) back to CIE start (eh).
    uint32_t cie_ptr = (uint32_t)((fde + 4) - eh);
    std::memcpy(fde + 4, &cie_ptr, 4);
    // PC begin: 8-byte absolute address of this slot.
    uint64_t pc_begin = (uint64_t)(uintptr_t)(page + i * _slot_size);
    std::memcpy(fde + 8, &pc_begin, 8);
    // PC range: 8-byte slot code length.
    uint64_t pc_range = (uint64_t)_slot_code_size;
    std::memcpy(fde + 16, &pc_range, 8);
    // CFI bytes (1-byte aug length = 0, then 15 bytes of CFI).
    std::memcpy(fde + 24, kFdeCfiBytes, 16);
  }
  // The last 4 bytes are already zero — that's the terminator (length=0).

  // Register with libgcc. The pointer must remain valid for the life of
  // the registration; pr.eh_frame keeps it alive (we never free pages).
  fprintf(stderr,
          "[trampoline-arena] registerEhFrameForPage: page=%p, eh_frame=%p, "
          "fdes=%zu, total_size=%zu, first_slot=%p, last_slot=%p\n",
          page, eh, slots_per_page, total_size,
          page, page + (slots_per_page - 1) * _slot_size);
  fflush(stderr);
  __register_frame(eh);
}

uint8_t* ExecutableArena::allocate() {
  std::lock_guard<std::mutex> g(_lock);

  size_t aligned = (_current_offset + _slot_alignment - 1) & ~(_slot_alignment - 1);
  if (!_current_page || aligned + _slot_size > _page_size) {
    void* p = mmap(nullptr, _page_size,
                   PROT_READ | PROT_WRITE | PROT_EXEC,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) {
      perror("[trampoline-arena] mmap");
      abort();
    }
    _current_page = (uint8_t*)p;
    _current_offset = 0;
    aligned = 0;
    _pages.push_back({(uintptr_t)p, (uintptr_t)p + _page_size, {}});
    registerEhFrameForPage(_current_page);
    _pages_published.store(_pages.size(), std::memory_order_release);
  }

  uint8_t* slot = _current_page + aligned;
  _current_offset = aligned + _slot_size;
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

// One arena instance (template bytes + ExecutableArena + side table) plus
// its initialization state. Two file-static instances exist below: one for
// bytecode trampolines and one for generic-function dispatch trampolines.
// Both kinds use identical machinery; only the captured template bytes
// differ.
class TrampolineArenaInstance {
public:
  TrampolineArenaInstance(const char* label) : _label(label) {}

  bool install_template(const uint8_t* tramp_bytes, size_t tramp_size) {
    std::lock_guard<std::mutex> g(_init_lock);
    if (_initialized.load(std::memory_order_relaxed)) return true;
    if (!tramp_bytes || tramp_size == 0) {
      fprintf(stderr, "[trampoline-arena] %s install_template: invalid args\n", _label);
      return false;
    }
    _tramp_bytes.assign(tramp_bytes, tramp_bytes + tramp_size);
    _tramp_size = tramp_size;
    // Slot stride padded up to 16-byte alignment. The actual code is
    // _tramp_size bytes; the FDE PC range uses _tramp_size, but slots are
    // spaced by slot_stride so subsequent slots align cleanly.
    size_t slot_stride = (_tramp_size + 15u) & ~size_t(15);
    _arena = new ExecutableArena(slot_stride, 16, _tramp_size);
    _side_table = new TrampolineSideTable();
    fprintf(stderr, "[trampoline-arena] installed %s template: %zu bytes (slot stride %zu)\n",
            _label, _tramp_size, slot_stride);
    fprintf(stderr, "[trampoline-arena] %s bytes:", _label);
    for (size_t i = 0; i < _tramp_size; ++i) fprintf(stderr, " %02x", _tramp_bytes[i]);
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
    std::memcpy(slot, _tramp_bytes.data(), _tramp_size);
    // No patching: trampoline bytes are byte-identical across all slots
    // (the call target is encoded as an absolute 64-bit immediate inside
    // the template by LLVM at capture time).
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

  // Returns the entry for `pc` if owned, else nullptr.
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
  std::vector<uint8_t>  _tramp_bytes;
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
// Format: <hex addr>  <hex size>  <symbol name>
// Shared across callers (both arena instances and the LLVM-ORC link-plugin
// per-symbol callback in runtimeJit.cc / compiler.cc). The first caller
// truncates any stale file contents; subsequent callers append.
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

bool arena_install_trampoline_template(const uint8_t* bytes, size_t size) {
  return g_bytecode.install_template(bytes, size);
}
bool arena_is_initialized() { return g_bytecode.is_initialized(); }
core::Pointer_sp arena_compile_trampoline(const std::string& name) {
  return g_bytecode.compile(name);
}

bool gf_arena_install_trampoline_template(const uint8_t* bytes, size_t size) {
  return g_gf.install_template(bytes, size);
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
