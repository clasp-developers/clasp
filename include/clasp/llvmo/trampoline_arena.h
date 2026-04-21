/*
 * trampoline_arena.h — bytecode trampoline arena.
 *
 * Each "trampoline" is a small function that calls bytecode_call via an
 * embedded absolute address. Each slot is a memcpy of a template the JIT
 * compiles once at init; the template embeds bytecode_call's address as a
 * 64-bit literal (movabs on x86_64), so all slots are byte-identical and
 * need no per-slot patching.
 *
 *   caller -> trampoline (unique address per fn, shared bytes)
 *                 `-> bytecode_call
 *
 * Backtrace identifies which Lisp function a frame represents by looking
 * up the trampoline's address (which is the return-address from
 * bytecode_call's frame) in the side table.
 *
 * Public API:
 *   - arena_install_trampoline_template(bytes, size): provided by the
 *     wire-up code in llvmoPackage.cc once it has compiled the template
 *     and captured its bytes. Idempotent.
 *   - arena_compile_trampoline(name): allocates a slot, memcpys the
 *     template, registers in the side table + perf-PID.map, returns the
 *     slot's address.
 *   - arena_lookup_by_pc(pc), arena_owns_pc(pc): for backtrace.
 */
#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <atomic>
#include <vector>
#include <mutex>
#include <clasp/core/object.h>
#include <clasp/core/pointer.h>

namespace llvmo {

struct TrampolineEntry {
  uint8_t*    code_start;
  uint32_t    code_size;
  std::string name;
};

class TrampolineSideTable {
public:
  TrampolineSideTable();
  // Append a new entry. Mutex-locked. Entries are appended in arena
  // allocation order (which is also code-address-ascending, since the
  // arena bump allocator only goes forward).
  void append(TrampolineEntry e);
  // Lock-free reader. Binary searches the published portion of the table
  // for the entry whose [code_start, code_start+code_size) contains pc.
  // Safe to call concurrently with new appends, including from signal
  // handlers (no lock, no allocation).
  const TrampolineEntry* find(uintptr_t pc) const;
  size_t published() const { return _published.load(std::memory_order_acquire); }

private:
  std::mutex _write_lock;
  std::vector<TrampolineEntry> _entries;
  std::atomic<size_t> _published{0};
};

class ExecutableArena {
public:
  // Interleaved slot layout: [code | CIE | FDE | 4B terminator], padded to
  // 16-byte stride. Every slot is a byte-identical memcpy of a single
  // composed template. This works because the FDE uses pcrel|sdata4 for its
  // PC begin field (offset from the field's own address to the function
  // start), and within a slot both the field position and the code position
  // are at compile-time-constant offsets from the slot base — so the PC
  // begin value is a constant across every slot. Likewise the FDE's CIE
  // pointer (distance back to the preceding CIE) is a constant.
  //
  // Each slot registers its own CIE+FDE with libgcc via
  // __register_frame(slot + code_size) — the trailing 4B zero terminator
  // stops libgcc's walk after the single FDE.
  //
  // Caller provides tramp/CIE/FDE byte blobs that are pre-patched for this
  // layout (CIE pointer = cie_size + 4; PC begin = -(code_size + cie_size + 8);
  // PC range = code_size).
  ExecutableArena(const uint8_t* tramp_bytes, size_t tramp_size,
                  const uint8_t* cie_bytes,   size_t cie_len,
                  const uint8_t* fde_bytes,   size_t fde_len);
  // Allocate a fresh slot, memcpy the composed template into it, and
  // register its FDE with libgcc. Single-mapped: returned address is both
  // writeable and executable.
  uint8_t* allocate();
  // True if pc lies in any committed page. Lock-free.
  bool owns(uintptr_t pc) const;
  // Code length within each slot — used by the side table for [start, start+size).
  size_t slot_code_size() const { return _tramp_size; }

private:
  std::mutex _lock;
  size_t _tramp_size = 0;          // code length, for FDE PC range and side-table size
  size_t _cie_size = 0;
  size_t _fde_size = 0;
  size_t _slot_stride = 0;         // 16-aligned: code + CIE + FDE + 4B term + padding
  size_t _page_size = 0;
  // Pre-composed slot template [code | CIE | FDE | terminator | pad]. Memcpy'd
  // verbatim into every allocated slot; no per-slot patching.
  std::vector<uint8_t> _slot_template;
  uint8_t* _current_page = nullptr;
  size_t _current_offset = 0;
  struct PageRange {
    uintptr_t start;
    uintptr_t end;
  };
  std::vector<PageRange> _pages;
  std::atomic<size_t> _pages_published{0};
};

// Install the captured trampoline template. Returns true on success;
// subsequent calls are idempotent no-ops.
//
// tramp_bytes   : exact machine bytes memcpy'd into each arena slot.
// tramp_size    : length of tramp_bytes.
// cie_bytes     : DWARF eh_frame CIE bytes (length prefix + CIE body). Same
//                 bytes are memcpy'd into every slot immediately after the
//                 code. Must specify DW_EH_PE_pcrel|DW_EH_PE_sdata4 for the
//                 FDE encoding so the FDE's PC begin is a distance (constant
//                 in this layout) rather than an absolute address.
// cie_len       : length of cie_bytes.
// fde_bytes     : DWARF eh_frame FDE bytes (length prefix + FDE body),
//                 pre-patched for the slot layout: CIE pointer field set to
//                 (cie_len + 4), PC begin (sdata4) set to
//                 -(tramp_size + cie_len + 8), PC range set to tramp_size.
// fde_len       : length of fde_bytes.
bool arena_install_trampoline_template(const uint8_t* tramp_bytes, size_t tramp_size,
                                        const uint8_t* cie_bytes,   size_t cie_len,
                                        const uint8_t* fde_bytes,   size_t fde_len);

// True once arena_install_trampoline_template has been called successfully.
bool arena_is_initialized();

// Allocate and register a new trampoline. Pre: arena is initialized.
core::Pointer_sp arena_compile_trampoline(const std::string& name);

// Generic-function dispatch arena. A second arena with its own template —
// the GF template is a 3-arg trampoline (closure, nargs, args) that tail-calls
// GFBytecodeEntryPoint::entry_point_n at an embedded absolute address, so
// every generic function gets a unique PC and its name shows up in backtraces
// and the perf-PID.map. Separate template because the signature differs from
// the bytecode trampoline; shares the lookup/owns API below.
bool gf_arena_install_trampoline_template(const uint8_t* tramp_bytes, size_t tramp_size,
                                           const uint8_t* cie_bytes,   size_t cie_len,
                                           const uint8_t* fde_bytes,   size_t fde_len);
bool gf_arena_is_initialized();
core::Pointer_sp gf_arena_compile_trampoline(const std::string& name);

// Backtrace lookup. Lock-free, signal-handler safe. Checks both arenas
// (bytecode first, then GF) so the caller doesn't care which kind a frame
// belongs to.
const TrampolineEntry* arena_lookup_by_pc(uintptr_t pc);
bool arena_owns_pc(uintptr_t pc);

// Post-snapshot-load pass: walk every BytecodeSimpleFun reachable from
// _AllBytecodeModules and install a fresh arena trampoline. Required because
// the trampoline pointer baked into a snapshot points at an mmap'd page that
// no longer exists after restart; the save side substitutes bytecode_call,
// and this restores wrapped trampolines so backtrace / perf-map work as
// before. No-op when CLASP_TRAMPOLINE_BACKEND != "arena".
void arena_post_load_regenerate_trampolines();

// Append one entry to /tmp/perf-PID.map. Lazy-opens the file once per
// process (truncating any stale contents from a previous-PID-with-the-same-id
// rerun) and appends thereafter. Thread-safe. Used by arena trampoline
// registration and by the LLVM-ORC link plugin's per-symbol callback so the
// two never race over fopen("w") and clobber each other's data.
void perf_map_append(uint8_t* addr, size_t size, const std::string& name);

}; // namespace llvmo
