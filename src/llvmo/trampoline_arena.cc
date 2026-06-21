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
 * slots and a single memcpy is all that's required.  Each copy does NOT
 * need to be patched with a different address because we use an 8-byte absolute
 * address to bytecode_vm.  The trampolines are fixed once at startup and then
 * copied without further changes.  All each trampoline needs is to exist in
 * a unique memory space so the return address from bytecode_vm on the stack
 * can be associated with a function name in /tmp/perf-<pid>.map
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
#include <clasp/core/sampling_profiler.h>
#include <clasp/llvmo/trampoline_arena.h>
#include <elf.h>
#endif
#if defined(CLASP_APPLE_SILICON)
#include <pthread.h>                  // pthread_jit_write_protect_np
#include <libkern/OSCacheControl.h>   // sys_icache_invalidate
#endif

// macOS arm64 enforces W^X: MAP_ANONYMOUS pages cannot be PROT_EXEC and writable
// at once unless mapped with MAP_JIT, after which writability is toggled per
// thread via pthread_jit_write_protect_np. Linux/x86-64 has no such flag.
#if !defined(MAP_JIT)
#define MAP_JIT 0
#endif

// ============================================================================
// GDB JIT interface — allows GDB to resolve trampoline frames with symbol
// names and unwind info. We declare these weak so that LLVM's stronger
// definitions (if linked) take precedence.
// ============================================================================

extern "C" {

typedef enum { JIT_NOACTION = 0, JIT_REGISTER_FN, JIT_UNREGISTER_FN } jit_actions_t;

struct jit_code_entry {
    jit_code_entry* next_entry;
    jit_code_entry* prev_entry;
    const char* symfile_addr;
    uint64_t symfile_size;
};

struct jit_descriptor {
    uint32_t version;
    uint32_t action_flag;
    jit_code_entry* relevant_entry;
    jit_code_entry* first_entry;
};

__attribute__((weak))
jit_descriptor __jit_debug_descriptor = { 1, 0, nullptr, nullptr };

__attribute__((weak, noinline))
void __jit_debug_register_code() { asm volatile("" ::: "memory"); }

} // extern "C"

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
//
// ELF/libgcc only. macOS ships libunwind, whose __register_frame takes a single
// FDE and walks differently; our pcrel|sdata4 CIE/FDE blobs are composed for the
// libgcc sequence-walk-until-terminator contract, so we skip registration there.
#if defined(_TARGET_OS_LINUX)
extern "C" void __register_frame(const void* begin);
#endif

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
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT, -1, 0);
    if (p == MAP_FAILED) {
      perror("[trampoline-arena] mmap");
      abort();
    }
    _current_page = (uint8_t*)p;
    _current_offset = 0;
    _pages.push_back({(uintptr_t)p, (uintptr_t)p + _page_size});
    _pages_published.store(_pages.size(), std::memory_order_release);
    core::sampling_profiler_add_executable_range((uintptr_t)p, (uintptr_t)p + _page_size);
  }

  uint8_t* slot = _current_page + _current_offset;
#if defined(CLASP_APPLE_SILICON)
  // Drop the per-thread W^X write-protect so this MAP_JIT page is writable,
  // copy the template, re-enable execute protection, then flush the I-cache so
  // the CPU sees the freshly written instructions.
  pthread_jit_write_protect_np(false);
  std::memcpy(slot, _slot_template.data(), _slot_stride);
  pthread_jit_write_protect_np(true);
  sys_icache_invalidate(slot, _slot_stride);
#else
  std::memcpy(slot, _slot_template.data(), _slot_stride);
#endif
#if defined(_TARGET_OS_LINUX)
  // Register this slot's CIE+FDE with libgcc. The CIE starts at
  // slot + _tramp_size; the FDE follows; the 4B zero terminator after the
  // FDE stops libgcc's classify walk.
  __register_frame(slot + _tramp_size);
#endif
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

// Forward declarations for GDB JIT ELF builder (defined below, after the
// arena instances, so that they sit next to the perf_map code they parallel).
// The GDB JIT ELF image is ELF-only; on non-Linux targets it's omitted.
#if defined(_TARGET_OS_LINUX)
static std::pair<char*, size_t>
build_gdb_jit_elf(uintptr_t code_addr, size_t code_size,
                  uintptr_t eh_frame_addr,
                  const uint8_t* eh_frame_bytes, size_t eh_frame_size,
                  const std::string& name);
static void gdb_jit_register(const char* elf_buf, size_t elf_size);
#endif

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
#if defined(_TARGET_OS_LINUX)
    // Build minimal ELF and register with GDB JIT interface (ELF/libgcc only).
    size_t ef_size = _arena->eh_frame_size();
    auto [elf_buf, elf_sz] = build_gdb_jit_elf(
        (uintptr_t)slot, _tramp_size,
        (uintptr_t)(slot + _tramp_size), slot + _tramp_size, ef_size,
        name);
    gdb_jit_register(elf_buf, elf_sz);
#endif
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

// ============================================================================
// Minimal ELF builder for GDB JIT interface
// ============================================================================
//
// Build a tiny ELF64 ET_EXEC image that tells GDB the symbol name and
// .eh_frame unwind info for a single trampoline slot. GDB reads the
// registered ELF via the GDB JIT interface (__jit_debug_descriptor /
// __jit_debug_register_code). The returned buffer is malloc'd and
// intentionally leaked (slots are never freed).
//
// Layout:
//   [0]:                Elf64_Ehdr           (64 bytes)
//   [64]:               Elf64_Shdr[6]        (6 * 64 = 384 bytes)
//   [448]:              shstrtab content     (~43 bytes)
//   [after shstrtab]:   Elf64_Sym[2]         (2 * 24 = 48 bytes)
//   [after symtab]:     strtab content       (1 + name.size() + 1)
//   [after strtab]:     eh_frame content     (CIE + FDE + terminator)

#if defined(_TARGET_OS_LINUX)
static std::pair<char*, size_t>
build_gdb_jit_elf(uintptr_t code_addr, size_t code_size,
                  uintptr_t eh_frame_addr,
                  const uint8_t* eh_frame_bytes, size_t eh_frame_size,
                  const std::string& name) {
  // Section-name string table (indices hardcoded below).
  //  0: ""  1: ".text"  7: ".eh_frame"  17: ".symtab"  25: ".strtab"  33: ".shstrtab"
  static const char shstrtab[] =
      "\0.text\0.eh_frame\0.symtab\0.strtab\0.shstrtab";
  static const size_t shstrtab_size = sizeof(shstrtab); // includes trailing NUL

  const size_t ehdr_size     = sizeof(Elf64_Ehdr);                // 64
  const size_t shdrs_size    = 6 * sizeof(Elf64_Shdr);            // 384
  const size_t shstrtab_off  = ehdr_size + shdrs_size;            // 448
  const size_t symtab_off    = shstrtab_off + shstrtab_size;
  const size_t symtab_size   = 2 * sizeof(Elf64_Sym);             // 48
  const size_t strtab_off    = symtab_off + symtab_size;
  const size_t strtab_size   = 1 + name.size() + 1;               // NUL + name + NUL
  const size_t ehframe_off   = strtab_off + strtab_size;
  const size_t total_size    = ehframe_off + eh_frame_size;

  char* buf = (char*)malloc(total_size);
  memset(buf, 0, total_size);

  // ---- ELF header ----
  Elf64_Ehdr* ehdr = (Elf64_Ehdr*)buf;
  memcpy(ehdr->e_ident, ELFMAG, SELFMAG);
  ehdr->e_ident[EI_CLASS]   = ELFCLASS64;
  ehdr->e_ident[EI_DATA]    = ELFDATA2LSB;
  ehdr->e_ident[EI_VERSION] = EV_CURRENT;
  ehdr->e_ident[EI_OSABI]   = ELFOSABI_NONE;
  ehdr->e_type      = ET_EXEC;
  ehdr->e_machine   = EM_X86_64;
  ehdr->e_version   = EV_CURRENT;
  ehdr->e_entry     = code_addr;
  ehdr->e_phoff     = 0;
  ehdr->e_shoff     = ehdr_size;    // section headers immediately after ELF header
  ehdr->e_ehsize    = ehdr_size;
  ehdr->e_phentsize = 0;
  ehdr->e_phnum     = 0;
  ehdr->e_shentsize = sizeof(Elf64_Shdr);
  ehdr->e_shnum     = 6;
  ehdr->e_shstrndx  = 5;           // .shstrtab is section 5

  // ---- Section headers ----
  Elf64_Shdr* shdr = (Elf64_Shdr*)(buf + ehdr_size);

  // [0] SHN_UNDEF — null section (already zeroed)

  // [1] .text — code lives in the inferior's memory, GDB reads it from there
  shdr[1].sh_name      = 1;        // offset in shstrtab
  shdr[1].sh_type      = SHT_PROGBITS;
  shdr[1].sh_flags     = SHF_ALLOC | SHF_EXECINSTR;
  shdr[1].sh_addr      = code_addr;
  shdr[1].sh_offset    = 0;        // GDB reads SHF_ALLOC from inferior memory
  shdr[1].sh_size      = code_size;
  shdr[1].sh_addralign = 16;

  // [2] .eh_frame — unwind info, content embedded in this ELF buffer
  shdr[2].sh_name      = 7;
  shdr[2].sh_type      = SHT_PROGBITS;
  shdr[2].sh_flags     = SHF_ALLOC;
  shdr[2].sh_addr      = eh_frame_addr;
  shdr[2].sh_offset    = ehframe_off;
  shdr[2].sh_size      = eh_frame_size;
  shdr[2].sh_addralign = 8;

  // [3] .symtab
  shdr[3].sh_name      = 17;
  shdr[3].sh_type      = SHT_SYMTAB;
  shdr[3].sh_offset    = symtab_off;
  shdr[3].sh_size      = symtab_size;
  shdr[3].sh_link      = 4;        // associated .strtab section
  shdr[3].sh_info      = 1;        // index of first non-local symbol
  shdr[3].sh_entsize   = sizeof(Elf64_Sym);
  shdr[3].sh_addralign = 8;

  // [4] .strtab
  shdr[4].sh_name      = 25;
  shdr[4].sh_type      = SHT_STRTAB;
  shdr[4].sh_offset    = strtab_off;
  shdr[4].sh_size      = strtab_size;
  shdr[4].sh_addralign = 1;

  // [5] .shstrtab
  shdr[5].sh_name      = 33;
  shdr[5].sh_type      = SHT_STRTAB;
  shdr[5].sh_offset    = shstrtab_off;
  shdr[5].sh_size      = shstrtab_size;
  shdr[5].sh_addralign = 1;

  // ---- Section contents ----

  // .shstrtab
  memcpy(buf + shstrtab_off, shstrtab, shstrtab_size);

  // .symtab — sym[0] is the null symbol (already zeroed)
  Elf64_Sym* sym = (Elf64_Sym*)(buf + symtab_off);
  sym[1].st_name  = 1;             // offset in strtab (after leading NUL)
  sym[1].st_info  = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
  sym[1].st_other = STV_DEFAULT;
  sym[1].st_shndx = 1;             // .text section
  sym[1].st_value = code_addr;
  sym[1].st_size  = code_size;

  // .strtab — "\0<name>\0"
  buf[strtab_off] = '\0';
  memcpy(buf + strtab_off + 1, name.data(), name.size());
  buf[strtab_off + 1 + name.size()] = '\0';

  // .eh_frame — copy the CIE + FDE + terminator bytes
  memcpy(buf + ehframe_off, eh_frame_bytes, eh_frame_size);

  return {buf, total_size};
}

static void gdb_jit_register(const char* elf_buf, size_t elf_size) {
  auto* entry = new jit_code_entry();
  entry->symfile_addr = elf_buf;
  entry->symfile_size = elf_size;
  entry->prev_entry   = nullptr;
  entry->next_entry   = __jit_debug_descriptor.first_entry;
  if (__jit_debug_descriptor.first_entry)
    __jit_debug_descriptor.first_entry->prev_entry = entry;
  __jit_debug_descriptor.first_entry   = entry;
  __jit_debug_descriptor.relevant_entry = entry;
  __jit_debug_descriptor.action_flag    = JIT_REGISTER_FN;
  __jit_debug_register_code();
}

#endif

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
