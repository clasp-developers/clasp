/*
 * trampolineWork.cc — trampoline code
 *
 */

#ifdef __APPLE__
#pragma message("trampolineWork.cc: trampoline arena is not ported to macOS — " \
                "ensure_trampoline_arena_initialized will fail at runtime and " \
                "callers will fall back to default_bytecode_trampoline.")
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
#include <clasp/llvmo/trampoline_arena.h>    // arena-mode trampolines
#include <clasp/llvmo/trampoline_x86_64.h>   // hardcoded x86_64 trampoline templates
#include <clasp/llvmo/trampoline_aarch64.h>  // hardcoded AArch64 trampoline templates
#include <clasp/llvmo/jit.h>                 // ClaspJIT_O full def (member access on ClaspJIT_sp)
#include <clasp/llvmo/llvmoExpose.h>         // Module_sp, LLVMContext_sp
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

std::atomic<size_t> global_trampoline_counter;



JITDylib_sp loadModule(llvmo::Module_sp module, size_t startupID, const std::string& libname) {
  ClaspJIT_sp jit = llvm_sys__clasp_jit();
  JITDylib_sp jitDylib = jit->createAndRegisterJITDylib(libname);
  ThreadSafeContext_sp tsc = gc::As<ThreadSafeContext_sp>(comp::_sym_STARthread_safe_contextSTAR->symbolValue());
  jit->addIRModule(jitDylib, module, tsc, startupID);
  return jitDylib;
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


// Defined in src/core/funcallableInstance.cc — pointer to the C++ static
// inline GFBytecodeEntryPoint::entry_point_n.
extern "C" gctools::return_type (*g_gf_dispatch_entry_point_n)(core::T_O*, size_t, core::T_O**);

enum class TrampolineKind { Bytecode, GF };

static const char* kind_label(TrampolineKind k) {
  return k == TrampolineKind::Bytecode ? "bytecode" : "GF";
}

// One-time arena initialization for either kind. Copies the hardcoded
// machine-code template, patches the call-target address, and installs
// the code + CIE + FDE into the arena. Each kind has independent state;
// calling for one kind doesn't initialize the other.
static bool ensure_trampoline_arena_initialized(TrampolineKind kind) {
  static std::atomic<int> bytecode_state{0};   // 0=uninit, 1=ready, 2=failed
  static std::atomic<int> gf_state{0};
  std::atomic<int>& state = (kind == TrampolineKind::GF) ? gf_state : bytecode_state;
  int s = state.load(std::memory_order_acquire);
  if (s == 1) return true;
  if (s == 2) return false;

#ifdef __APPLE__
  static std::atomic<bool> warned_bytecode{false};
  static std::atomic<bool> warned_gf{false};
  std::atomic<bool>& warned = (kind == TrampolineKind::GF) ? warned_gf : warned_bytecode;
  bool expected = false;
  if (warned.compare_exchange_strong(expected, true)) {
    fprintf(stderr,
            "[trampoline-arena] DISABLED on macOS (%s) — falling back to %s.\n",
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

#if defined(__x86_64__)
  using namespace trampoline_x86_64;
  const uint8_t* code_template;
  size_t code_sz;
  size_t patch_offset;
  const uint8_t* fde_data;
  size_t fde_sz;
  uint64_t target_addr;

  if (kind == TrampolineKind::GF) {
    code_template = gf_code;
    code_sz       = gf_code_size;
    patch_offset  = gf_target_offset;
    fde_data      = gf_fde;
    fde_sz        = gf_fde_size;
    target_addr   = (uint64_t)g_gf_dispatch_entry_point_n;
  } else {
    code_template = bytecode_code;
    code_sz       = bytecode_code_size;
    patch_offset  = bytecode_target_offset;
    fde_data      = bytecode_fde;
    fde_sz        = bytecode_fde_size;
    target_addr   = (uint64_t)&bytecode_call;
  }

  // Copy code template and patch the target address into the movabs immediate
  std::vector<uint8_t> code(code_template, code_template + code_sz);
  std::memcpy(code.data() + patch_offset, &target_addr, 8);

  bool installed = (kind == TrampolineKind::GF)
      ? gf_arena_install_trampoline_template(code.data(), code_sz,
                                              cie, cie_size,
                                              fde_data, fde_sz)
      : arena_install_trampoline_template(code.data(), code_sz,
                                           cie, cie_size,
                                           fde_data, fde_sz);
  if (!installed) {
    fprintf(stderr, "[trampoline-arena] %s install failed\n", kind_label(kind));
    state.store(2, std::memory_order_release);
    return false;
  }
  fprintf(stderr, "[trampoline-arena] %s template installed, target %p\n",
          kind_label(kind), (void*)target_addr);
  state.store(1, std::memory_order_release);
  return true;
#elif defined(__aarch64__)
  using namespace trampoline_aarch64;
  const uint8_t* code_template;
  size_t code_sz;
  size_t patch_offset;
  const uint8_t* fde_data;
  size_t fde_sz;
  uint64_t target_addr;

  if (kind == TrampolineKind::GF) {
    code_template = gf_code;
    code_sz       = gf_code_size;
    patch_offset  = gf_target_offset;
    fde_data      = gf_fde;
    fde_sz        = gf_fde_size;
    target_addr   = (uint64_t)g_gf_dispatch_entry_point_n;
  } else {
    code_template = bytecode_code;
    code_sz       = bytecode_code_size;
    patch_offset  = bytecode_target_offset;
    fde_data      = bytecode_fde;
    fde_sz        = bytecode_fde_size;
    target_addr   = (uint64_t)&bytecode_call;
  }

  std::vector<uint8_t> code(code_template, code_template + code_sz);
  std::memcpy(code.data() + patch_offset, &target_addr, 8);

  bool installed = (kind == TrampolineKind::GF)
      ? gf_arena_install_trampoline_template(code.data(), code_sz,
                                              cie, cie_size,
                                              fde_data, fde_sz)
      : arena_install_trampoline_template(code.data(), code_sz,
                                           cie, cie_size,
                                           fde_data, fde_sz);
  if (!installed) {
    fprintf(stderr, "[trampoline-arena] %s install failed\n", kind_label(kind));
    state.store(2, std::memory_order_release);
    return false;
  }
  fprintf(stderr, "[trampoline-arena] %s template installed, target %p\n",
          kind_label(kind), (void*)target_addr);
  state.store(1, std::memory_order_release);
  return true;
#else
  fprintf(stderr, "[trampoline-arena] %s: no hardcoded template for this architecture\n",
          kind_label(kind));
  state.store(2, std::memory_order_release);
  return false;
#endif
}

bool enable_trampolines() {
  return getenv("CLASP_ENABLE_TRAMPOLINES");
}

// Compile a GF trampoline (per-generic-function), called from
// GFBytecodeSimpleFun_O::make and from the post-snapshot-load regen pass.
// Returns the address of the per-GF arena slot, or the static
// entry_point_n forwarder pointer if the arena isn't ready.
core::Pointer_sp cmp__compile_gf_trampoline(core::T_sp tname) {
  if (!enable_trampolines() || !ensure_trampoline_arena_initialized(TrampolineKind::GF)) {
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
  if (tname.consp() && CONS_CAR(tname) == ::cl::_sym_lambda
      && CONS_CDR(tname).consp() && CONS_CAR(CONS_CDR(tname)).nilp()) {
    return Values(Pointer_O::create((void*)lambda_nil),
                  SimpleBaseString_O::make("lambda_nil"));
  }

  if (!enable_trampolines()) {
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

  if (!ensure_trampoline_arena_initialized(TrampolineKind::Bytecode)) {
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
// snapshot. Called after snapshot_load completes its fixup pass.
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
