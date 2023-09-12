#include <cstdint>
#include <config.h>
#include <clasp/core/configure_clasp.h>

#define MAGIC 3735879680

struct Gcroots {
  uint64_t val1;
  void *val2;
  uint64_t val3;
  uint64_t val4;
  void **val5;
  uint64_t val6;
};

struct return_type {
  void *_ptr;
  uint64_t _nvals;
  return_type(void *ptr, uint64_t nvals) : _ptr(ptr), _nvals(nvals){};
};

typedef uint64_t *save_args;

typedef return_type(bytecode_trampoline_type)(uint64_t pc, void *closure, uint64_t nargs, void **args);

extern "C" {

Gcroots CLASP_GCROOTS_IN_MODULE(trampoline)[0];
void *CLASP_LITERALS(trampoline)[0];

// Use asm and nodebug to add declaration for the LLVM intrinsic.
__attribute__((nodebug)) void LLVM_EXPERIMENTAL_STACKMAP(uint64_t type, uint32_t dummy, ...) asm("llvm.experimental.stackmap");

return_type bytecode_call(uint64_t pc, void *closure, uint64_t nargs, void **args);

// The wrapper name may have a scope resolution operator which would require quotes so we use asm with colon to ensure the name is
// quoted in the bitcode.
return_type WRAPPER_NAME(uint64_t pc, void *closure, uint64_t nargs, void **args) asm("wrapper:name");

return_type WRAPPER_NAME(uint64_t pc, void *closure, uint64_t nargs, void **args) {
  uint64_t trampoline_save_args[3];
  LLVM_EXPERIMENTAL_STACKMAP((uint64_t)MAGIC, 0, &trampoline_save_args);
  trampoline_save_args[0] = (uintptr_t)closure;
  trampoline_save_args[1] = (uintptr_t)nargs;
  trampoline_save_args[2] = (uintptr_t)args;
  return bytecode_call(pc, closure, nargs, args);
}
};
