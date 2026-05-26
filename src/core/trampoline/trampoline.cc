#include <cstdint>
#include <config.h>
#include <clasp/core/configure_clasp.h>

#define MAGIC 3735879680

struct return_type {
  void *_ptr;
  uint64_t _nvals;
  return_type(void *ptr, uint64_t nvals) : _ptr(ptr), _nvals(nvals){};
};

typedef uint64_t *save_args;

typedef return_type(bytecode_trampoline_type)(uint64_t pc, void *closure, uint64_t nargs, void **args);

extern "C" {

void *CLASP_LITERALS(trampoline)[0];

// Use asm and nodebug to add declaration for the LLVM intrinsic.
__attribute__((nodebug)) void LLVM_EXPERIMENTAL_STACKMAP(uint64_t type, uint32_t dummy, ...) asm("llvm.experimental.stackmap");

// Indirect through a global function pointer rather than calling bytecode_call
// directly. Each compiled trampoline's call topology is now identical
// (load-from-global + indirect call), with the per-instance difference
// confined to a single RIP-relative offset field in the load instruction.
// The global is defined in bytecode.cc and initialized to &bytecode_call
// at static-initialization time.
typedef return_type (*bytecode_call_fn_t)(uint64_t pc, void *closure, uint64_t nargs, void **args);
extern bytecode_call_fn_t g_bytecode_call_ptr;

// The wrapper name may have a scope resolution operator which would require quotes so we use asm with colon to ensure the name is
// quoted in the bitcode.
return_type WRAPPER_NAME(uint64_t pc, void *closure, uint64_t nargs, void **args) asm("wrapper:name");

return_type WRAPPER_NAME(uint64_t pc, void *closure, uint64_t nargs, void **args) {
  uint64_t trampoline_save_args[3];
  LLVM_EXPERIMENTAL_STACKMAP((uint64_t)MAGIC, 0, &trampoline_save_args);
  trampoline_save_args[0] = (uintptr_t)closure;
  trampoline_save_args[1] = (uintptr_t)nargs;
  trampoline_save_args[2] = (uintptr_t)args;
  return g_bytecode_call_ptr(pc, closure, nargs, args);
}

// End marker placed directly after the trampoline body so we can compute
// the trampoline's compiled size at runtime: size = &end_marker - &wrapper.
// __attribute__((used)) prevents the optimizer from dropping it; the asm
// name participates in the same "wrapper:name" -> unique-name substitution
// (the suffix "_end" survives unchanged), so each trampoline gets its own
// matching end marker symbol.
__attribute__((used, noinline)) void WRAPPER_END_MARKER() asm("wrapper:name_end");
__attribute__((used, noinline)) void WRAPPER_END_MARKER() {}
};
