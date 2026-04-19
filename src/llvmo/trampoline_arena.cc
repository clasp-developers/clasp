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
  size_t n = _published.load(std::memory_order_acquire);
  size_t lo = 0, hi = n;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    const TrampolineEntry& e = _entries[mid];
    uintptr_t s = (uintptr_t)e.code_start;
    if (s + e.code_size <= pc)      lo = mid + 1;
    else if (s > pc)                hi = mid;
    else                            return &e;
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
// CFI for one FDE — describes the prologue/epilogue of our 22-byte stub.
const uint8_t kFdeCfiBytes[16] = {
  0x00,                         // nop (pad to advance from 0 to 5 minus 5 = 0)
  0x45,                         // DW_CFA_advance_loc 5 (after endbr64+push rbp)
  0x0e, 0x10,                   // DW_CFA_def_cfa_offset 16
  0x86, 0x02,                   // DW_CFA_offset rbp 2 (rbp at CFA-16)
  0x43,                         // DW_CFA_advance_loc 3 (after mov rbp, rsp)
  0x0d, 0x06,                   // DW_CFA_def_cfa_register rbp
  0x4d,                         // DW_CFA_advance_loc 13 (after movabs+call+pop rbp)
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
std::atomic<bool>     g_initialized{false};
std::mutex            g_init_lock;
std::vector<uint8_t>  g_stub_bytes;
size_t                g_stub_size = 0;
ExecutableArena*      g_arena = nullptr;
TrampolineSideTable*  g_side_table = nullptr;

FILE*                 g_perf_map = nullptr;
std::mutex            g_perf_map_lock;
}  // anonymous namespace

bool arena_install_stub_template(const uint8_t* stub_bytes, size_t stub_size) {
  std::lock_guard<std::mutex> g(g_init_lock);
  if (g_initialized.load(std::memory_order_relaxed)) return true;
  if (!stub_bytes || stub_size == 0) {
    fprintf(stderr, "[trampoline-arena] install_stub_template: invalid args\n");
    return false;
  }
  g_stub_bytes.assign(stub_bytes, stub_bytes + stub_size);
  g_stub_size = stub_size;
  // Slot stride padded up to 16-byte alignment. The actual code is
  // g_stub_size bytes; the FDE PC range uses g_stub_size, but slots are
  // spaced by slot_stride so subsequent slots align cleanly.
  size_t slot_stride = (g_stub_size + 15u) & ~size_t(15);
  g_arena = new ExecutableArena(slot_stride, 16, g_stub_size);
  g_side_table = new TrampolineSideTable();
  fprintf(stderr, "[trampoline-arena] installed stub template: %zu bytes (slot stride %zu)\n",
          g_stub_size, slot_stride);
  fprintf(stderr, "[trampoline-arena] stub bytes:");
  for (size_t i = 0; i < g_stub_size; ++i) fprintf(stderr, " %02x", g_stub_bytes[i]);
  fprintf(stderr, "\n");
  fflush(stderr);
  g_initialized.store(true, std::memory_order_release);
  return true;
}

bool arena_is_initialized() {
  return g_initialized.load(std::memory_order_acquire);
}

// perf-PID.map writer — opens lazily, one append per registered trampoline.
// Format: <hex addr>  <hex size>  <symbol name>
static void write_perf_map_entry(uint8_t* addr, size_t size, const std::string& name) {
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

core::Pointer_sp arena_compile_trampoline(const std::string& name) {
  if (!g_initialized.load(std::memory_order_acquire)) {
    fprintf(stderr, "[trampoline-arena] arena_compile_trampoline before init\n");
    abort();
  }
  uint8_t* slot = g_arena->allocate();
  std::memcpy(slot, g_stub_bytes.data(), g_stub_size);
  // No patching: stub is byte-identical across all slots (the indirect
  // call target is encoded as an absolute address in the stub's data area).

  TrampolineEntry e{slot, (uint32_t)g_stub_size, name};
  g_side_table->append(std::move(e));
  write_perf_map_entry(slot, g_stub_size, name);

  // Diagnostic for the first few arena allocations.
  static std::atomic<int> debug_count{0};
  int n = debug_count.fetch_add(1);
  if (n < 3) {
    fprintf(stderr, "[trampoline-arena] arena_compile_trampoline #%d '%s' -> %p\n",
            n, name.c_str(), slot);
    fflush(stderr);
  }
  return core::Pointer_O::create((void*)slot);
}

const TrampolineEntry* arena_lookup_by_pc(uintptr_t pc) {
  if (!g_initialized.load(std::memory_order_acquire)) return nullptr;
  if (!g_arena->owns(pc)) return nullptr;
  return g_side_table->find(pc);
}

bool arena_owns_pc(uintptr_t pc) {
  if (!g_initialized.load(std::memory_order_acquire)) return false;
  return g_arena->owns(pc);
}

}; // namespace llvmo
