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
  // slot_code_size: the actual code length within each slot (used for FDE
  // PC range — must be the function's true size, NOT the slot stride).
  ExecutableArena(size_t slot_size, size_t slot_alignment, size_t slot_code_size);
  // Allocate a fresh slot. Single-mapped: returned address is both
  // writeable and executable.
  uint8_t* allocate();
  // True if pc lies in any committed page. Lock-free.
  bool owns(uintptr_t pc) const;

private:
  // Build eh_frame data covering one page's slots and register it with
  // libgcc via __register_frame. Called when a new page is mmap'd.
  void registerEhFrameForPage(uint8_t* page);

  std::mutex _lock;
  size_t _slot_size;        // stride per slot (alignment-padded)
  size_t _slot_alignment;
  size_t _slot_code_size;   // actual code length, for FDE PC range
  size_t _page_size;
  uint8_t* _current_page = nullptr;
  size_t _current_offset = 0;
  struct PageRange {
    uintptr_t start;
    uintptr_t end;
    // Owned eh_frame data, registered via __register_frame for this page.
    // Kept alive (never freed) for the lifetime of the registration.
    std::vector<uint8_t> eh_frame;
  };
  std::vector<PageRange> _pages;
  std::atomic<size_t> _pages_published{0};
};

// Install the captured trampoline template. Returns true on success;
// subsequent calls are idempotent no-ops.
bool arena_install_trampoline_template(const uint8_t* tramp_bytes, size_t tramp_size);

// True once arena_install_stub_template has been called successfully.
bool arena_is_initialized();

// Allocate and register a new trampoline. Pre: arena is initialized.
core::Pointer_sp arena_compile_trampoline(const std::string& name);

// Backtrace lookup. Lock-free, signal-handler safe.
const TrampolineEntry* arena_lookup_by_pc(uintptr_t pc);
bool arena_owns_pc(uintptr_t pc);

// Post-snapshot-load pass: walk every BytecodeSimpleFun reachable from
// _AllBytecodeModules and install a fresh arena trampoline. Required because
// the trampoline pointer baked into a snapshot points at an mmap'd page that
// no longer exists after restart; the save side substitutes bytecode_call,
// and this restores wrapped trampolines so backtrace / perf-map work as
// before. No-op when CLASP_TRAMPOLINE_BACKEND != "arena".
void arena_post_load_regenerate_trampolines();

}; // namespace llvmo
