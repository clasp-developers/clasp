/*
 * trampolineWork.cc — trampoline code
 *
 */

#ifdef __APPLE__
#pragma message("trampolineWork.cc: trampoline arena is not ported to macOS — " \
                "ensure_trampoline_arena_initialized will fail at runtime and " \
                "callers will fall back to default_bytecode_trampoline. See the " \
                "port-plan comment above ensure_trampoline_arena_initialized.")
#endif

#include <sys/mman.h>
#include <unistd.h>
#include <cstring>
#include <cstdio>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/pointer.h>
#include <clasp/core/bytecode.h>             // bytecode_call (call target of every trampoline)
#include <clasp/core/wrappers.h>             // Values() multi-value ctor
#include <clasp/core/lisp.h>                 // _lisp root, used by the post-load regen pass
#include <clasp/core/compiler.h>             // comp::_sym_STARthread_safe_contextSTAR
#include <clasp/core/funcallableInstance.h>
#include "llvm/IR/DebugInfo.h"  // StripDebugInfo (trampoline shrink)
#include "llvm/Object/ObjectFile.h"  // for CFI extraction
#include "llvm/Object/SymbolSize.h"  // computeSymbolSizes (trampoline sizing)
#include <clasp/llvmo/trampoline_arena.h>   // arena-mode trampolines
#include <clasp/llvmo/jit.h>                 // ClaspJIT_O full def (member access on ClaspJIT_sp)
#include <clasp/llvmo/llvmoExpose.h>         // Module_sp, LLVMContext_sp
#include <clasp/llvmo/code.h>                // ObjectFile_O, lookupObjectFileFromEntryPoint
#include "clasp/llvmo/trampolineWork.h"

// Bring core:: (Pointer_O, SimpleBaseString_O, T_sp, etc.) into scope for
// unqualified use inside `namespace llvmo`. The pre-refactor llvmoPackage.cc
// did the same — most of the trampoline code was written against core:: names
// without qualification.
using namespace core;



extern "C" {
NEVER_OPTIMIZE
gctools::return_type default_bytecode_trampoline(unsigned char* pc, core::T_O* closure, uint64_t nargs, core::T_O** args) {
  return bytecode_call(pc, closure, nargs, args);
}

NEVER_OPTIMIZE
gctools::return_type unknown_bytecode_trampoline(unsigned char* pc, core::T_O* closure, uint64_t nargs, core::T_O** args) {
  return bytecode_call(pc, closure, nargs, args);
}

NEVER_OPTIMIZE
gctools::return_type lambda_nil(unsigned char* pc, core::T_O* closure, uint64_t nargs, core::T_O** args) {
  return bytecode_call(pc, closure, nargs, args);
}
};


namespace llvmo {
#include <trampoline.h>

std::atomic<size_t> global_trampoline_counter;



JITDylib_sp loadModule(llvmo::Module_sp module, size_t startupID, const std::string& libname) {
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  JITDylib_sp jitDylib = jit->createAndRegisterJITDylib(libname);
  //  printf("%s:%d:%s jit = %p  jitDylib = %p\n", __FILE__, __LINE__, __FUNCTION__, jit.raw_(), jitDylib.raw_() );
  ThreadSafeContext_sp tsc = gc::As<ThreadSafeContext_sp>(comp::_sym_STARthread_safe_contextSTAR->symbolValue());
  jit->addIRModule(jitDylib, module, tsc, startupID);
  return jitDylib;
}



// Shared JITDylib that owns the arena-init trampoline template module.
// Created lazily on the first trampoline compile. This dylib is not
// registered with _lisp->_Roots._JITDylibs (it holds only the template,
// whose ObjectFile is marked _TransientSkipSnapshot and isn't serialized).
JITDylib_sp global_trampoline_dylib;

// Add an IR module to an already-existing JITDylib. Mirrors loadModule()
// but skips the per-call createAndRegisterJITDylib (which would push a
// new entry onto _Roots._JITDylibs every time).
static void addModuleToDylib(JITDylib_sp jitDylib, llvmo::Module_sp module, size_t startupID) {
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  ThreadSafeContext_sp tsc = gc::As<ThreadSafeContext_sp>(comp::_sym_STARthread_safe_contextSTAR->symbolValue());
  jit->addIRModule(jitDylib, module, tsc, startupID);
}

string escapeNameForLlvm(const string& inp) {
  stringstream sout;
  stringstream sin(inp);
  char c;
  while (1) {
    sin.get(c);
    if (!sin.good())
      break;
    switch (c) {
    case '"':
      sout << "_";
      break;
    default:
      sout << c;
    }
  };
  return sout.str();
}


// Trampoline arena initialization.
//
// At first use we build a small IR module for the trampoline template: a
// function that calls a pointer-typed value created from an inttoptr of
// bytecode_call's address (so the address is encoded as an absolute 64-bit
// literal, not a symbol relocation). Compile via the JIT, look up the
// resulting bytes, capture them. Hand them to arena_install_trampoline_template().
//
// From then on, each cmp__compile_trampoline call routed to the arena
// backend memcpys the template bytes into a fresh slot — no patching,
// no per-arch code, no per-slot indirection.
enum class TrampolineKind;
static bool ensure_trampoline_arena_initialized(ClaspJIT_sp jit, TrampolineKind kind);

namespace {

// Extract the leading "target datalayout = ..." and "target triple = ..."
// lines from the existing trampoline IR template, so the stub IR uses the
// same architecture-correct strings without us having to know them at
// compile time.
static std::string extract_target_lines(const std::string& source) {
  std::string out;
  std::string::size_type pos = 0;
  for (int line_count = 0; line_count < 50 && pos < source.size(); ++line_count) {
    auto eol = source.find('\n', pos);
    if (eol == std::string::npos) break;
    std::string line = source.substr(pos, eol - pos);
    pos = eol + 1;
    if (line.find("target datalayout") != std::string::npos
        || line.find("target triple") != std::string::npos) {
      out += line;
      out += '\n';
    }
  }
  return out;
}

}  // anonymous namespace

// Declared in runtimeJit.cc. When true, prepareObjectFileForMaterialization
// marks the created ObjectFile_O's _TransientSkipSnapshot flag. We set it
// while compiling the trampoline template so the snapshot save walker
// excludes that scaffolding ObjectFile. It is still registered in
// _AllObjectFiles (LLVM's link layer needs that), but is filtered out at
// snapshot save time.
extern thread_local bool t_mark_transient_snapshot;

// Defined in src/core/funcallableInstance.cc — pointer to the C++ static
// inline GFBytecodeEntryPoint::entry_point_n (which we can't take the
// address of from this TU since the struct is local to that file).
extern "C" gctools::return_type (*g_gf_dispatch_entry_point_n)(core::T_O*, size_t, core::T_O**);

// ---------------------------------------------------------------------------
// Unified trampoline-template machinery. The bytecode and GF trampolines have
// the same structure (alloca + volatile arg-save + movabs/call + epilogue)
// and only differ in (a) signature arity (4 args vs 3) and (b) target
// function address (bytecode_call vs entry_point_n). One IR builder + one
// capture + one ensure-initialized parameterized by TrampolineKind.
// ---------------------------------------------------------------------------

enum class TrampolineKind { Bytecode, GF };

namespace {

static const char* kind_label(TrampolineKind k) {
  return k == TrampolineKind::Bytecode ? "bytecode" : "GF";
}

static std::string build_trampoline_ir(uint64_t target_addr,
                                       const std::string& tramp_name,
                                       TrampolineKind kind) {
  std::string targets = extract_target_lines(global_trampoline);
  char addr_buf[32];
  snprintf(addr_buf, sizeof addr_buf, "%lu", (unsigned long)target_addr);
  // Per-kind IR fragments. Bytecode: (i64 pc, ptr closure, i64 nargs, ptr args)
  // — closure at %1, nargs at %2, args at %3. GF: (ptr closure, i64 nargs,
  // ptr args) — closure at %0, nargs at %1, args at %2.
  const bool gf = (kind == TrampolineKind::GF);
  const char* sig_params  = gf ? "ptr %0, i64 %1, ptr %2"
                               : "i64 %0, ptr %1, i64 %2, ptr %3";
  const char* sig_fn_type = gf ? "(ptr, i64, ptr)"
                               : "(i64, ptr, i64, ptr)";
  const char* call_args   = gf ? "ptr %0, i64 %1, ptr %2"
                               : "i64 %0, ptr %1, i64 %2, ptr %3";
  const char* closure_param = gf ? "%0" : "%1";
  const char* nargs_param   = gf ? "%1" : "%2";
  const char* args_param    = gf ? "%2" : "%3";

  std::string ir;
  ir.reserve(2048);
  ir += targets;
  ir += "\n";
  // Mirror the legacy trampoline IR's literals stub. clasp's
  // ObjectFile-on-load machinery expects every JIT'd module to define it.
  ir += "@__clasp_literals_trampoline_stub = internal local_unnamed_addr global [0 x ptr] zeroinitializer, align 8\n\n";
  // No personality. The trampoline is a pure passthrough with no landing
  // pads; the C++ unwinder only needs CFI to step through this frame, not
  // a personality callback. Keeping personality off means LLVM emits a
  // plain "zR" CIE whose only aug-data byte is the FDE encoding, with no
  // pcrel-encoded personality pointer — so the CIE bytes are fully
  // position-independent and can be memcpy'd verbatim into every arena
  // slot.
  //
  // Direct inttoptr-in-call: forces LLVM to materialize the address as a
  // 64-bit immediate (movabs on x86_64; MOVZ+MOVK*3 on arm64). The bytes
  // generated are byte-identical for every compile (the immediate is the
  // same value across all slots), so memcpy alone is enough — no per-slot
  // patching, no RIP-relative offsets to fix up.
  ir += "define { ptr, i64 } @\""; ir += tramp_name; ir += "\"(";
  ir += sig_params;
  ir += ") #0 {\n";
  // Save (closure, nargs, args) to a stack-allocated 3-slot array so a
  // debugger / backtrace can recover them from this frame. volatile prevents
  // LLVM from optimizing the stores away. Identical for both kinds — only
  // the source registers differ.
  ir += "  %saved = alloca [3 x i64], align 8\n";
  ir += "  %p0 = getelementptr inbounds [3 x i64], ptr %saved, i64 0, i64 0\n";
  ir += "  %p1 = getelementptr inbounds [3 x i64], ptr %saved, i64 0, i64 1\n";
  ir += "  %p2 = getelementptr inbounds [3 x i64], ptr %saved, i64 0, i64 2\n";
  ir += "  %closure_int = ptrtoint ptr "; ir += closure_param; ir += " to i64\n";
  ir += "  %args_int = ptrtoint ptr ";    ir += args_param;    ir += " to i64\n";
  ir += "  store volatile i64 %closure_int, ptr %p0, align 8\n";
  ir += "  store volatile i64 ";          ir += nargs_param;   ir += ", ptr %p1, align 8\n";
  ir += "  store volatile i64 %args_int, ptr %p2, align 8\n";
  ir += "  %fp = inttoptr i64 ";          ir += addr_buf;      ir += " to ptr\n";
  ir += "  %r = call { ptr, i64 } ";      ir += sig_fn_type;
  ir += " %fp(";                          ir += call_args;     ir += ")\n";
  ir += "  ret { ptr, i64 } %r\n";
  ir += "}\n";
  // No _end marker: on x86_64 Linux LLVM's target defaults force unwind
  // tables on ALL functions regardless of attribute, so any second
  // function in this module would produce an extra .eh_frame FDE and
  // break the "exactly one FDE" slot-layout invariant. Instead the
  // caller queries the trampoline symbol's size from the linked
  // ObjectFile (computeSymbolSizes), which also avoids the fragile
  // `end - start` address arithmetic.
  //
  // uwtable: emit unwind tables so Lisp Unwind exceptions can propagate.
  // frame-pointer=all: keep the frame pointer chain intact for backtrace.
  ir += "attributes #0 = { uwtable \"frame-pointer\"=\"all\" }\n";
  return ir;
}

// Walk an x86_64-ELF .eh_frame section and extract the single CIE and the
// single FDE as verbatim byte blobs. Relies on the trampoline IR having
// exactly one uwtable function (the template), with the _end marker
// attributed as no-uwtable — so LLVM emits one CIE + one FDE. Any other
// count (zero, or two+ of either) is a sign that something upstream
// changed and silent copy-of-wrong-bytes is about to happen: we fail loud.
//
// Structural bytes (length, CIE pointer, aug data, PC range, CFI opcodes)
// need no relocations, so what's in the unlinked ObjectFile is already
// correct. The only field that requires a relocation is the FDE's PC
// begin; its pre-relocation value is typically zero, and the caller
// patches it for the slot layout.
static bool extract_cie_fde_from_object_file(llvm::object::ObjectFile& obj,
                                              std::vector<uint8_t>& out_cie,
                                              std::vector<uint8_t>& out_fde) {
  for (const llvm::object::SectionRef& sect : obj.sections()) {
    auto nameOrErr = sect.getName();
    if (!nameOrErr) { llvm::consumeError(nameOrErr.takeError()); continue; }
    if (*nameOrErr != ".eh_frame") continue;
    auto contentsOrErr = sect.getContents();
    if (!contentsOrErr) { llvm::consumeError(contentsOrErr.takeError()); continue; }
    llvm::StringRef contents = *contentsOrErr;
    const uint8_t* p = (const uint8_t*)contents.data();
    const uint8_t* end = p + contents.size();
    const uint8_t* cie_entry = nullptr; size_t cie_entry_size = 0;
    const uint8_t* fde_entry = nullptr; size_t fde_entry_size = 0;
    size_t cie_count = 0, fde_count = 0;
    while (p + 4 <= end) {
      uint32_t length;
      std::memcpy(&length, p, 4);
      if (length == 0) break;  // CIE/FDE terminator
      if (length == 0xffffffffu) {
        fprintf(stderr, "[trampoline-arena] .eh_frame: unexpected 64-bit extended length\n");
        return false;
      }
      const uint8_t* entry = p;
      size_t entry_size = 4 + length;
      if (entry + entry_size > end) {
        fprintf(stderr, "[trampoline-arena] .eh_frame: entry runs past section end\n");
        return false;
      }
      uint32_t id;
      std::memcpy(&id, p + 4, 4);
      if (id == 0) {
        ++cie_count;
        cie_entry = entry; cie_entry_size = entry_size;
      } else {
        ++fde_count;
        fde_entry = entry; fde_entry_size = entry_size;
      }
      p += entry_size;
    }
    if (cie_count != 1 || fde_count != 1) {
      fprintf(stderr,
              "[trampoline-arena] .eh_frame: expected exactly 1 CIE + 1 FDE, "
              "got %zu CIE(s) + %zu FDE(s). Trampoline IR may have grown "
              "extra uwtable functions — re-check build_trampoline_ir.\n",
              cie_count, fde_count);
      return false;
    }
    out_cie.assign(cie_entry, cie_entry + cie_entry_size);
    out_fde.assign(fde_entry, fde_entry + fde_entry_size);
    return true;
  }
  fprintf(stderr, "[trampoline-arena] .eh_frame section not found in ObjectFile\n");
  return false;
}

// Verify that the CIE we extracted matches our slot-layout assumptions:
//   - version 1
//   - augmentation string exactly "zR" (no personality / no LSDA — either
//     would add pcrel-encoded pointers that break the byte-identical slot
//     copy)
//   - FDE encoding byte == DW_EH_PE_pcrel | DW_EH_PE_sdata4 (0x1B), so
//     the FDE's PC begin field is a 4-byte signed distance (constant in
//     this layout)
// Returns true if all invariants hold; prints a diagnostic and returns
// false otherwise.
static bool validate_trampoline_cie(const uint8_t* cie, size_t n) {
  // Minimum for "zR" CIE: length(4)+id(4)+ver(1)+"zR\0"(3)+ca(1)+da(1)
  // +rr(1)+aug_len(1)+fde_enc(1) = 17 bytes. Anything shorter is broken.
  if (n < 17) {
    fprintf(stderr, "[trampoline-arena] CIE too small: %zu bytes\n", n);
    return false;
  }
  if (cie[8] != 1) {
    fprintf(stderr, "[trampoline-arena] CIE version = %u (expected 1)\n", cie[8]);
    return false;
  }
  // Aug string is null-terminated starting at offset 9.
  size_t pos = 9;
  size_t aug_start = pos;
  while (pos < n && cie[pos] != 0) ++pos;
  if (pos >= n) {
    fprintf(stderr, "[trampoline-arena] CIE: aug string not null-terminated\n");
    return false;
  }
  std::string aug((const char*)cie + aug_start, pos - aug_start);
  ++pos;  // skip null terminator
  if (aug != "zR") {
    fprintf(stderr,
            "[trampoline-arena] CIE aug string = \"%s\" (expected \"zR\"). "
            "Trampoline IR likely added personality/LSDA — byte-identical "
            "slot copy would embed pcrel-encoded pointers pointing to the "
            "wrong address. Fix: keep build_trampoline_ir free of "
            "personality attributes.\n", aug.c_str());
    return false;
  }
  // Skip code_align (ULEB128), data_align (SLEB128), return register (1B).
  // ULEB128/SLEB128 continuation bit = high bit set.
  auto skip_leb = [&]() -> bool {
    while (pos < n && (cie[pos] & 0x80)) ++pos;
    if (pos >= n) return false;
    ++pos;  // final byte
    return true;
  };
  if (!skip_leb() || !skip_leb()) {
    fprintf(stderr, "[trampoline-arena] CIE: malformed code/data align\n");
    return false;
  }
  if (pos >= n) return false;
  ++pos;  // return register (1 byte in eh_frame)
  // Aug length (ULEB128). For "zR", value must be 1 (one FDE-encoding byte).
  if (pos >= n || (cie[pos] & 0x80)) {
    fprintf(stderr, "[trampoline-arena] CIE: unexpected multi-byte aug_length\n");
    return false;
  }
  uint8_t aug_len = cie[pos++];
  if (aug_len != 1) {
    fprintf(stderr,
            "[trampoline-arena] CIE aug_length = %u (expected 1 for \"zR\")\n",
            aug_len);
    return false;
  }
  if (pos >= n) return false;
  uint8_t fde_enc = cie[pos];
  // DW_EH_PE_pcrel = 0x10, DW_EH_PE_sdata4 = 0x0B. Combined = 0x1B.
  constexpr uint8_t kExpectedFdeEncoding = 0x1B;
  if (fde_enc != kExpectedFdeEncoding) {
    fprintf(stderr,
            "[trampoline-arena] CIE FDE encoding = 0x%02x (expected 0x%02x = "
            "pcrel|sdata4). LLVM may have switched .eh_frame defaults — the "
            "PC-begin patch in ensure_trampoline_arena_initialized assumes "
            "sdata4 and would corrupt slot unwind info. Audit LLVM's "
            "TargetLoweringObjectFile settings for this triple.\n",
            fde_enc, kExpectedFdeEncoding);
    return false;
  }
  return true;
}

static bool capture_trampoline_template(ClaspJIT_sp jit,
                                        uint64_t target_addr,
                                        TrampolineKind kind,
                                        std::vector<uint8_t>& out_bytes,
                                        std::vector<uint8_t>& out_cie,
                                        std::vector<uint8_t>& out_fde) {
  std::string tramp_name = (kind == TrampolineKind::GF)
                              ? "__gf_trampoline_template"
                              : "__bytecode_trampoline_template";
  std::string ir = build_trampoline_ir(target_addr, tramp_name, kind);
  fprintf(stderr, "[trampoline-arena] %s trampoline IR (%zu bytes):\n%s\n",
          kind_label(kind), ir.size(), ir.c_str());
  fflush(stderr);

  Module_sp module;
#if LLVM_VERSION_MAJOR < 21
  {
    LLVMContext_sp context = llvm_sys__thread_local_llvm_context();
    module = llvm_sys__parseIRString(ir, context, tramp_name.c_str());
  }
#else
  llvm::orc::ThreadSafeContext* tsc =
      ((llvm::orc::ThreadSafeContext*)gc::As<ThreadSafeContext_sp>(
          comp::_sym_STARthread_safe_contextSTAR->symbolValue())->externalObject());
  tsc->withContextDo([&](llvm::LLVMContext* lc) {
    auto context = gctools::GC<LLVMContext_O>::allocate();
    context->_ptr = lc;
    module = llvm_sys__parseIRString(ir, context, tramp_name.c_str());
  });
#endif
  if (module.nilp() || !module->wrappedPtr()) {
    fprintf(stderr, "[trampoline-arena] failed to parse %s trampoline IR\n", kind_label(kind));
    return false;
  }
  llvm::StripDebugInfo(*module->wrappedPtr());

  if (!global_trampoline_dylib) {
    global_trampoline_dylib = jit->createAndRegisterJITDylib("trampoline");
  }
  // Use a large startupID so the ObjectFile codeName won't collide with
  // user-compiled trampolines (whose IDs come from global_trampoline_counter
  // starting near zero). Bytecode template uses 1M+, GF template uses 2M+.
  static std::atomic<size_t> bytecode_id_counter{1000000};
  static std::atomic<size_t> gf_id_counter{2000000};
  size_t tramp_id = (kind == TrampolineKind::GF
                       ? gf_id_counter
                       : bytecode_id_counter).fetch_add(1);
  addModuleToDylib(global_trampoline_dylib, module, tramp_id);

  void* start = nullptr;
  if (!jit->do_lookup(global_trampoline_dylib, tramp_name, start)) {
    fprintf(stderr, "[trampoline-arena] %s template lookup '%s' failed\n",
            kind_label(kind), tramp_name.c_str());
    return false;
  }

  // Find the ObjectFile that owns this trampoline's code, and extract the
  // CIE and FDE bytes from its .eh_frame section. The arena slot's
  // [code | CIE | FDE | terminator] layout copies these bytes verbatim.
  ObjectFile_sp of;
  if (!lookupObjectFileFromEntryPoint((uintptr_t)start, of)) {
    fprintf(stderr, "[trampoline-arena] %s: ObjectFile lookup for %p failed\n",
            kind_label(kind), start);
    return false;
  }
  auto expected = of->getObjectFile();
  if (!expected) {
    llvm::consumeError(expected.takeError());
    fprintf(stderr, "[trampoline-arena] %s: getObjectFile() failed\n", kind_label(kind));
    return false;
  }
  std::unique_ptr<llvm::object::ObjectFile> obj = std::move(*expected);

  // Resolve the trampoline's size from the ELF symbol table instead of
  // using an `_end` marker. LLVM's x86_64-Linux target forces unwind
  // tables on every function regardless of attribute, so a second
  // function would inevitably produce a second FDE and break the 1-FDE
  // invariant. The module has exactly one defined function (the
  // trampoline) plus a few data-type symbols (the literals-stub global),
  // so picking the unique ST_Function with nonzero size is unambiguous
  // and robust to name mangling or linker-private prefixes.
  size_t sz = 0;
  auto sizes = llvm::object::computeSymbolSizes(*obj);
  for (auto& p : sizes) {
    auto typeOrErr = p.first.getType();
    if (!typeOrErr) { llvm::consumeError(typeOrErr.takeError()); continue; }
    if (*typeOrErr != llvm::object::SymbolRef::ST_Function) continue;
    if (p.second == 0) continue;
    if (sz != 0) {
      fprintf(stderr,
              "[trampoline-arena] %s: more than one defined function in "
              "module — symbol-size disambiguation is unsafe\n",
              kind_label(kind));
      return false;
    }
    sz = (size_t)p.second;
  }
  if (sz == 0) {
    fprintf(stderr,
            "[trampoline-arena] %s: no function symbol with nonzero size in module. "
            "Symbols visible to computeSymbolSizes:\n", kind_label(kind));
    for (auto& p : sizes) {
      auto nameOrErr = p.first.getName();
      std::string n = "<err>";
      if (nameOrErr) n = nameOrErr->str();
      else llvm::consumeError(nameOrErr.takeError());
      auto typeOrErr = p.first.getType();
      int t = -1;
      if (typeOrErr) t = (int)*typeOrErr;
      else llvm::consumeError(typeOrErr.takeError());
      fprintf(stderr, "    name=%s type=%d size=%llu\n",
              n.c_str(), t, (unsigned long long)p.second);
    }
    return false;
  }
  out_bytes.assign((uint8_t*)start, (uint8_t*)start + sz);

  if (!extract_cie_fde_from_object_file(*obj, out_cie, out_fde)) {
    fprintf(stderr, "[trampoline-arena] %s: .eh_frame CIE/FDE extraction failed\n",
            kind_label(kind));
    return false;
  }
  return true;
}

}  // anonymous namespace

// ===========================================================================
// macOS port plan
// ===========================================================================
//
// The interleaved-slot trampoline arena is currently x86_64-Linux-ELF only.
// On macOS, `ensure_trampoline_arena_initialized` short-circuits to the
// "init failed" path below; callers fall back to default_bytecode_trampoline
// (and g_gf_dispatch_entry_point_n for GF dispatch). Flame charts and
// perf-PID.map on macOS therefore show generic entry-point names instead of
// per-Lisp-function names until this port is completed.
//
// What needs to change, in increasing order of work:
//
// (1) SECTION NAME — extract_cie_fde_from_object_file searches for
//     ".eh_frame". MachO stores the same data in segment __TEXT section
//     __eh_frame. Try both names (".eh_frame" on ELF, "__eh_frame" on MachO).
//     ~10 LoC.
//
// (2) libgcc → libunwind — Darwin uses LLVM's libunwind, whose
//     __register_frame takes a *single FDE* pointer, not an eh_frame range.
//     Register `slot + tramp_size + cie_size` (FDE start) instead of
//     `slot + tramp_size` (CIE start); the 4-byte zero terminator at the
//     end of each slot becomes unused padding but is otherwise harmless.
//     ~20 LoC, gated on __APPLE__.
//
// (3) FDE ENCODING — validate_trampoline_cie asserts pcrel|sdata4 (0x1B).
//     LLVM's Darwin x86_64 default is still 0x1B; verify on actual builds.
//     If it's different, validate fires with an actionable diagnostic.
//
// (4) W^X ENFORCEMENT — macOS arm64 forbids PROT_READ|WRITE|EXEC pages.
//     Two implementation options:
//       (a) MAP_JIT + pthread_jit_write_protect_np(false) before the slot
//           memcpy, (true) after. Requires the
//           com.apple.security.cs.allow-jit entitlement on the binary.
//           Every thread that calls arena_compile_trampoline must be a
//           JIT-writer thread at startup. Unmarked threads SIGBUS silently.
//           ~30 LoC + entitlement plumbing.
//       (b) shm_open dual mapping: one RW view, one RX view of the same
//           physical memory. ~80 LoC, doubles VA usage, no thread protocol.
//
// (5) compact_unwind (__unwind_info) — macOS arm64 libunwind consults the
//     compact-unwind lookup table first and falls back to eh_frame only
//     if nothing matches. Our JIT slots aren't in any compact table, so
//     libunwind's fallback path handles them. That path is less
//     battle-tested than the compact lookup; budget debugging time.
//     Plan-B if the fallback is unreliable: emit a 32-bit compact_unwind
//     encoding per slot, maintain a PC-range lookup we register with
//     libunwind (no clean API — probably ~200 LoC of private helper).
//
// (6) arm64 IR LOWERING — the `inttoptr i64 ADDR to ptr` + indirect-call
//     pattern becomes `movz + movk*3 + blr` (≈20 bytes) on arm64. Slot
//     stride grows; otherwise unchanged. arm64 prologue CFI (stp x29,x30
//     + mov x29,sp) is extracted from LLVM automatically — the byte-
//     identical memcpy still works because pcrel-sdata4 distances remain
//     slot-layout constants.
//
// (7) SIGCHECK — add an install-time assert that the compiled ObjectFile's
//     __compact_unwind section is either absent or empty for the template.
//     Catches LLVM emitting a compact entry that libunwind might prefer
//     over our eh_frame FDE, turning subtle unwind failures into loud
//     init-time errors.
//
// Total new code is modest (100–300 LoC). The real cost is verification:
// Lisp Unwind exceptions must propagate through an arena frame without
// std::terminate; Instruments and perf must resolve arena PCs to names;
// both arches need end-to-end testing. Budget 1 day for x86_64 macOS,
// 5–10 days for arm64 macOS dominated by libunwind debugging.
// ===========================================================================

// One-time arena initialization for either kind. Each kind has independent
// state; calling for one kind doesn't initialize the other.
static bool ensure_trampoline_arena_initialized(ClaspJIT_sp jit, TrampolineKind kind) {
  static std::atomic<int> bytecode_state{0};   // 0=uninit, 1=ready, 2=failed
  static std::atomic<int> gf_state{0};
  std::atomic<int>& state = (kind == TrampolineKind::GF) ? gf_state : bytecode_state;
  int s = state.load(std::memory_order_acquire);
  if (s == 1) return true;
  if (s == 2) return false;

#ifdef __APPLE__
  // Trampoline arena is not yet ported to macOS — see the long comment just
  // above for the port plan. Fail init so callers fall back to
  // default_bytecode_trampoline / g_gf_dispatch_entry_point_n. Log once per
  // kind at first touch so the fallback is visible.
  static std::atomic<bool> warned_bytecode{false};
  static std::atomic<bool> warned_gf{false};
  std::atomic<bool>& warned = (kind == TrampolineKind::GF) ? warned_gf : warned_bytecode;
  bool expected = false;
  if (warned.compare_exchange_strong(expected, true)) {
    fprintf(stderr,
            "[trampoline-arena] DISABLED on macOS (%s) — falling back to "
            "%s. See trampolineWork.cc above ensure_trampoline_arena_initialized "
            "for the port plan. Consequence: flame charts and perf-PID.map "
            "show generic entry-point names instead of per-function names.\n",
            kind_label(kind),
            kind == TrampolineKind::GF
                ? "g_gf_dispatch_entry_point_n"
                : "default_bytecode_trampoline");
    fflush(stderr);
  }
  state.store(2, std::memory_order_release);
  return false;
#endif

  static std::mutex bytecode_init_lock;
  static std::mutex gf_init_lock;
  std::mutex& init_lock = (kind == TrampolineKind::GF) ? gf_init_lock : bytecode_init_lock;
  std::lock_guard<std::mutex> g(init_lock);
  s = state.load(std::memory_order_acquire);
  if (s == 1) return true;
  if (s == 2) return false;

  // The trampoline template ObjectFile is scaffolding — mark it so the
  // snapshot walker skips it.
  struct MarkTransientGuard {
    MarkTransientGuard()  { t_mark_transient_snapshot = true;  }
    ~MarkTransientGuard() { t_mark_transient_snapshot = false; }
  } mark_transient_guard;

  uint64_t target_addr = (kind == TrampolineKind::GF)
                            ? (uint64_t)g_gf_dispatch_entry_point_n
                            : (uint64_t)&bytecode_call;
  std::vector<uint8_t> tramp_bytes, cie_bytes, fde_bytes;
  if (!capture_trampoline_template(jit, target_addr, kind,
                                   tramp_bytes, cie_bytes, fde_bytes)) {
    state.store(2, std::memory_order_release);
    return false;
  }

  // Loud-failure invariant checks. The slot-layout design relies on the
  // CIE being position-independent ("zR" augmentation, no personality /
  // LSDA) and the FDE PC encoding being pcrel|sdata4. If either drifts
  // out from under us, the byte-identical memcpy silently corrupts unwind
  // info. Fail at install time instead.
  if (!validate_trampoline_cie(cie_bytes.data(), cie_bytes.size())) {
    state.store(2, std::memory_order_release);
    return false;
  }

  // Patch the FDE's PC begin field for the slot layout
  // [code | CIE | FDE | terminator]. The CIE uses DW_EH_PE_pcrel|sdata4
  // for FDE PC encoding (LLVM's default on x86_64 ELF), so PC begin is
  // a signed 4-byte offset from the field's own address to the function
  // start. In our layout the PC begin field is at
  //   slot + tramp_size + cie_size + 8
  // and the code is at slot + 0, so the offset is
  //   -(tramp_size + cie_size + 8).
  // Both quantities are slot-layout constants, so every slot's bytes match.
  //
  // Other FDE fields do NOT need patching:
  //  - Length: unchanged.
  //  - CIE pointer (offset 4): self-relative distance from the field to
  //    the preceding CIE. In LLVM's .eh_frame the CIE is at section offset
  //    0 and the FDE starts at cie_size, so the stored value is
  //    cie_size + 4 — which is the SAME value we'd need for our slot
  //    layout (CIE immediately precedes FDE).
  //  - PC range (offset 12): the function length, already correct.
  //  - Aug data + CFI: no relocations.
  if (fde_bytes.size() < 16) {
    fprintf(stderr, "[trampoline-arena] %s: FDE too small (%zu bytes)\n",
            kind_label(kind), fde_bytes.size());
    state.store(2, std::memory_order_release);
    return false;
  }
  int32_t pc_begin = -(int32_t)(tramp_bytes.size() + cie_bytes.size() + 8);
  std::memcpy(fde_bytes.data() + 8, &pc_begin, 4);

  bool installed = (kind == TrampolineKind::GF)
                      ? gf_arena_install_trampoline_template(
                            tramp_bytes.data(), tramp_bytes.size(),
                            cie_bytes.data(),   cie_bytes.size(),
                            fde_bytes.data(),   fde_bytes.size())
                      : arena_install_trampoline_template(
                            tramp_bytes.data(), tramp_bytes.size(),
                            cie_bytes.data(),   cie_bytes.size(),
                            fde_bytes.data(),   fde_bytes.size());
  if (!installed) {
    fprintf(stderr, "[trampoline-arena] %s install failed\n", kind_label(kind));
    state.store(2, std::memory_order_release);
    return false;
  }
  fprintf(stderr, "[trampoline-arena] %s template %zu bytes, target %p (CIE %zu, FDE %zu)\n",
          kind_label(kind), tramp_bytes.size(), (void*)target_addr,
          cie_bytes.size(), fde_bytes.size());
  state.store(1, std::memory_order_release);
  return true;
}

// Compile a GF trampoline (per-generic-function), called from
// GFBytecodeSimpleFun_O::make and from the post-snapshot-load regen pass.
// Returns the address of the per-GF arena slot, or the static
// entry_point_n forwarder pointer if the JIT/arena isn't ready.
core::Pointer_sp cmp__compile_gf_trampoline(core::T_sp tname) {
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  if (jit.nilp() || !ensure_trampoline_arena_initialized(jit, TrampolineKind::GF)) {
    return Pointer_O::create((void*)g_gf_dispatch_entry_point_n);
  }
  std::string aname;
  if (gc::IsA<core::Symbol_sp>(tname)) {
    aname = gc::As_unsafe<core::Symbol_sp>(tname)->fullName();
  } else if (tname.notnilp()) {
    aname = _rep_(tname);
    if (aname.size() >= 2 && aname[0] == '"' && aname.back() == '"')
      aname = aname.substr(1, aname.size() - 2);
  } else {
    aname = "anonymous_gf";
  }
  static std::atomic<size_t> gf_counter{0};
  size_t id = gf_counter++;
  std::string mangled = escapeNameForLlvm(aname) + "_gft" + std::to_string(id);
  return gf_arena_compile_trampoline(mangled);
}

CL_DEFUN core::Pointer_mv cmp__compile_trampoline(core::T_sp tname) {
  // Special cases: functions that share a single libclasp fallback symbol.
  // These aren't worth giving individual arena slots.
  //   - (lambda nil ...) — anonymous lambdas with no arglist that Lisp code
  //     distinguishes by source-file position rather than name.
  //   - "" — empty-string names (makeBytecodeSimpleFun before set_trampoline
  //     fires with the real name).
  if (tname.consp() && CONS_CAR(tname) == ::cl::_sym_lambda
      && CONS_CDR(tname).consp() && CONS_CAR(CONS_CDR(tname)).nilp()) {
    return Values(Pointer_O::create((void*)lambda_nil),
                  SimpleBaseString_O::make("lambda_nil"));
  }

  // If the JIT isn't up yet (very early startup), return the default
  // trampoline pointer. It's a plain passthrough to bytecode_call and gets
  // replaced later when the function is named (loadltv calls set_trampoline).
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  if (jit.nilp()) {
    return Values(Pointer_O::create((void*)default_bytecode_trampoline),
                  SimpleBaseString_O::make("default_bytecode_trampoline"));
  }

  // Derive a printable name.
  std::string aname;
  if (gc::IsA<core::Symbol_sp>(tname)) {
    aname = gc::As_unsafe<core::Symbol_sp>(tname)->fullName();
  } else {
    aname = _rep_(tname);
    if (aname.size() < 3 && aname.size() >= 2 && aname[0] == '"' && aname.back() == '"') {
      return Values(Pointer_O::create((void*)unknown_bytecode_trampoline),
                    SimpleBaseString_O::make("unknown_bytecode_trampoline"));
    }
    if (aname.size() >= 2 && aname[0] == '"' && aname.back() == '"')
      aname = aname.substr(1, aname.size() - 2);
  }

  // Initialize the arena on first call. Init fails only if the JIT can't
  // compile the template; fall back to bytecode_call in that case so the
  // system degrades gracefully.
  if (!ensure_trampoline_arena_initialized(jit, TrampolineKind::Bytecode)) {
    return Values(Pointer_O::create((void*)default_bytecode_trampoline),
                  SimpleBaseString_O::make("default_bytecode_trampoline"));
  }

  size_t arenaId = global_trampoline_counter++;
  std::string mangled = escapeNameForLlvm(aname) + "_bct" + std::to_string(arenaId);
  core::Pointer_sp p = arena_compile_trampoline(mangled);
  return Values(p, SimpleBaseString_O::make(mangled));
}

// Compile callbacks for FFI.
CL_DEFUN JITDylib_sp jit_module_to_dylib(Module_sp module, const std::string& libname) { return loadModule(module, 0, libname); }

CL_DEFUN core::Pointer_sp jit_lookup(JITDylib_sp dylib, const std::string& name) {
  return llvm_sys__clasp_jit()->lookup(dylib, name);
}

// Access a T_sp in a variable.
CL_DEFUN core::T_sp jit_lookup_t(JITDylib_sp dylib, const std::string& name) {
  void* ptr;
  bool found = llvm_sys__clasp_jit()->do_lookup(dylib, name, ptr);
  if (!found)
    SIMPLE_ERROR("Could not find pointer for name |{}|", name);
  core::T_O** tptr = (core::T_O**)ptr;
  T_sp ret((gctools::Tagged)(*tptr));
  return ret;
}

CL_LISPIFY_NAME("llvmo:jit-lookup-t");
CL_DEFUN_SETF core::T_sp setf_jit_lookup_t(core::T_sp value, JITDylib_sp dylib, const std::string& name) {
  void* ptr;
  bool found = llvm_sys__clasp_jit()->do_lookup(dylib, name, ptr);
  if (!found)
    SIMPLE_ERROR("Could not find pointer for name |{}|", name);
  core::T_O** tptr = (core::T_O**)ptr;
  *tptr = value.raw_();
  return value;
}

// Re-attach an arena trampoline to every BytecodeSimpleFun reachable from the
// snapshot. Called after snapshot_load completes its fixup pass. The save side
// substituted bytecode_call for any arena-owned _Trampoline (since the slot
// address is not stable across a restart); this restores wrapped trampolines
// so backtraces and the perf-PID.map continue to identify Lisp frames.
//
// No-op when the env var requesting the arena backend is unset — in that case
// the saved bytecode_call value is the right runtime trampoline anyway.
void arena_post_load_regenerate_trampolines() {
  size_t n_regen = 0;
  core::List_sp modules = _lisp->_Roots._AllBytecodeModules.load(std::memory_order_relaxed);
  for (auto mods : modules) {
    core::BytecodeModule_sp module = gc::As_assert<core::BytecodeModule_sp>(oCar(mods));
    core::T_sp debuginfo = module->debugInfo();
    if (debuginfo.nilp()) continue;
    for (auto const& info : debuginfo.as_assert<core::SimpleVector_O>()) {
      if (gc::IsA<core::BytecodeSimpleFun_sp>(info)) {
        core::BytecodeSimpleFun_sp fun = gc::As_unsafe<core::BytecodeSimpleFun_sp>(info);
        core::Pointer_sp tramp = cmp__compile_trampoline(fun->functionName());
        fun->set_trampoline(tramp);
        ++n_regen;
      }
    }
  }
  // Generic-function dispatch trampolines (GFBytecodeSimpleFun). Install a
  // fresh per-GF trampoline as _EntryPoints[0] so flame charts and backtrace
  // see per-GF names instead of one shared entry_point_n.
  size_t n_gf = 0;
  core::List_sp gfs = _lisp->_Roots._AllGFBytecodeFuns.load(std::memory_order_relaxed);
  for (auto gf_cons : gfs) {
    core::GFBytecodeSimpleFun_sp gf = gc::As_assert<core::GFBytecodeSimpleFun_sp>(oCar(gf_cons));
    core::Pointer_sp tramp = cmp__compile_gf_trampoline(gf->functionName());
    gf->_EntryPoints._EntryPoints[0] = (core::ClaspXepAnonymousFunction)tramp->ptr();
    ++n_gf;
  }
  fprintf(stderr, "[trampoline-arena] post-load regenerated %zu bytecode + %zu gf trampolines\n",
          n_regen, n_gf);
  fflush(stderr);
}

}; // namespace llvmo
