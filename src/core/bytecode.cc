
#define DEBUG_LEVEL_FULL

// #include <llvm/Support/system_error.h>
#include <unistd.h>
#include <dlfcn.h>
#include <iomanip>
#include <cstdint>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/array.h>
#include <clasp/core/unwind.h>
#include <clasp/core/ql.h>
#include <clasp/core/designators.h> // calledFunctionDesignator
#include <clasp/core/evaluator.h>   // eval::funcall

#define VM_CODES
#include <virtualMachine.h>
#undef VM_CODES

extern "C" {
bool global_debug_vm = false;
}

#ifdef DEBUG_VIRTUAL_MACHINE
#define DBG_printf(...)                                                                                                            \
  {                                                                                                                                \
    if (global_debug_vm) {                                                                                                         \
      printf(__VA_ARGS__);                                                                                                         \
    }                                                                                                                              \
  }
#define DBG_VM(...)                                                                                                                \
  {                                                                                                                                \
    if (global_debug_vm) {                                                                                                         \
      printf("%s:%6d pc: %p sp: %p fp: %p | ", __FILE__, __LINE__, (void*)vm._pc, sp, fp);                                         \
      printf(__VA_ARGS__);                                                                                                         \
    }                                                                                                                              \
  }
#if 1
#define DBG_VM1(...) DBG_VM(__VA_ARGS__)
#else
#define DBG_VM1(...)
#endif
#else
#define DBG_printf(...)
#define DBG_VM(...)
#define DBG_VM1(...)
#endif

namespace core {

void BytecodeModule_O::register_for_debug() {
  // An atomic push, as the variable is shared.
  T_sp old = _lisp->_Roots._AllBytecodeModules.load(std::memory_order_relaxed);
  Cons_sp newc = Cons_O::create(this->asSmartPtr(), old);
  while (!_lisp->_Roots._AllBytecodeModules.compare_exchange_weak(old, newc, std::memory_order_relaxed))
    newc->setCdr(old);
}

static inline int16_t read_s16(unsigned char* pc) {
  uint8_t byte0 = *pc;
  uint8_t byte1 = *(pc + 1);
  uint16_t nibble = byte0 | (byte1 << 8);
  // Not sure how standard-conformant this is, but it seems to work.
  union {
    uint16_t u;
    int16_t i;
  } convert;
  convert.u = nibble;
  return convert.i;
}

static inline int32_t read_label(unsigned char* pc, size_t nbytes) {
  // Labels are stored little-endian.
  uint32_t result = 0;
  for (size_t i = 0; i < nbytes - 1; ++i)
    result |= *(++pc) << i * 8;
  uint8_t msb = *(++pc);
  result |= (msb & 0x7f) << ((nbytes - 1) * 8);
  // Signed conversion.
  union {
    uint32_t u;
    int32_t i;
  } convert;
  convert.u = result;
  return convert.i;
}

#define DEBUG_VM_RECORD_PLAYBACK 0

struct VM_error {};

#if 0

static FILE* global_bytecode_messages = NULL;

void bytecode_message(const std::string& msg) {
  if (!global_bytecode_messages) {
    global_bytecode_messages = fopen("/tmp/bytecode.log","w");
  }
  fprintf(global_bytecode_messages,"%s", msg.c_str());
  fflush(global_bytecode_messages);
}

#define VM_WRITE(...)                                                                                                              \
  { bytecode_message(fmt::format(__VA_ARGS__)); }
#else
#define VM_WRITE(...)
#endif

#if DEBUG_VM_RECORD_PLAYBACK == 1
static size_t global_counter = 0;
static size_t global_counterStep = 1024;
static size_t global_stackTrigger = 16384;

enum RecordingEnum { idle, playback, record };
static FILE* global_recordingFile = NULL;
static RecordingEnum global_recordingState = idle;

void open_telemetry_file(const std::string& filename, RecordingEnum state) {
  if (state == record) {
    global_recordingFile = fopen(filename.c_str(), "wb");
    global_recordingState = record;
  } else if (state == playback) {
    global_recordingFile = fopen(filename.c_str(), "rb");
    global_recordingState = playback;
  }
}

CL_DEFUN void core__start_recording_vm(const std::string& filename) {
  global_recordingFile = fopen(filename.c_str(), "wb");
  global_recordingState = record;
}

CL_DEFUN void core__stop_recording_vm() {
  if (global_recordingFile) {
    fclose(global_recordingFile);
    global_recordingFile = NULL;
  }
  global_recordingState = idle;
}

CL_DEFUN void core__start_playback_vm(const std::string& filename) {
  global_recordingFile = fopen(filename.c_str(), "rb");
  global_recordingState = playback;
}

CL_DEFUN void core__stop_playback_vm() {
  if (global_recordingFile) {
    fclose(global_recordingFile);
    global_recordingFile = NULL;
  }
  global_recordingState = idle;
}

void vm_error() { printf("Error in vm\n"); }

void vm_record_playback(void* value, const char* name) {
  if (global_recordingFile) {
    if (global_recordingState == record) {
      fwrite((void*)&(value), sizeof(value), 1, global_recordingFile);
    }
    if (global_recordingState == playback) {
      void* read_value;
      fread((void*)&read_value, sizeof(read_value), 1, global_recordingFile);
      if (read_value != (void*)value) {
        printf("%s:%d:%s Mismatch between recorded %s %p and current %p\n", __FILE__, __LINE__, __FUNCTION__, name, read_value,
               (void*)value);
        vm_error();
        throw VM_error();
      }
    }
  }
}
#endif

#if DEBUG_VM_RECORD_PLAYBACK == 1
#define VM_RECORD_PLAYBACK(value, name) vm_record_playback(value, name)
#else
#define VM_RECORD_PLAYBACK(value, name)
#endif

static unsigned char* long_dispatch(VirtualMachine&, unsigned char*, MultipleValues& multipleValues, T_O**, T_O**, Closure_O*,
                                    core::T_O**, core::T_O**, size_t, core::T_O**, uint8_t);

SYMBOL_EXPORT_SC_(KeywordPkg, name);
#ifdef DEBUG_VIRTUAL_MACHINE
__attribute__((optnone))
#endif
gctools::return_type
bytecode_vm(VirtualMachine& vm, T_O** literals, T_O** closed, Closure_O* closure,
            core::T_O** fp, // frame pointer
            core::T_O** sp, // stack pointer
            size_t lcc_nargs, core::T_O** lcc_args) {
  ASSERT(literals == NULL || (uintptr_t)literals > 65536);
  ASSERT((((uintptr_t)literals) & 0x7) == 0); // must be aligned
  ASSERT((((uintptr_t)closure) & 0x7) == 0);  // must be aligned
  ASSERT((((uintptr_t)lcc_args) & 0x7) == 0); // must be aligned
  VM_WRITE("{}\n", (uintptr_t)vm._stackPointer - (uintptr_t)vm._stackBottom);
#ifdef DEBUG_VIRTUAL_MACHINE
  if (lcc_nargs > 65536) {
    printf("%s:%d:%s A very large number of arguments %lu are being passed - check if there is a problem\n", __FILE__, __LINE__,
           __FUNCTION__, lcc_nargs);
  }
  BytecodeSimpleFun_sp ep = gc::As<BytecodeSimpleFun_sp>(closure->entryPoint());
  BytecodeModule_sp bm = gc::As<BytecodeModule_sp>(ep->_Code);
  Vector_sp bc = bm->bytecode();
  uintptr_t bytecode_start = (uintptr_t)(bc->rowMajorAddressOfElement_(0));
  uintptr_t bytecode_end = (uintptr_t)(bc->rowMajorAddressOfElement_(bc->length()));
#endif
  MultipleValues& multipleValues = core::lisp_multipleValues();
  unsigned char* pc = vm._pc;
  while (1) {
    VM_PC_CHECK(vm, pc, bytecode_start, bytecode_end);
#if DEBUG_VM_RECORD_PLAYBACK == 1
    global_counter++;
    size_t stackHeight = (uintptr_t)(vm)._stackPointer - (uintptr_t)(vm)._stackBottom;
    if (stackHeight > global_stackTrigger) {
      printf("%s:%d:%s Exceeded stackTrigger %lu stackHeight = %lu returning\n", __FILE__, __LINE__, __FUNCTION__,
             global_stackTrigger, stackHeight);
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    // printf("%c", (unsigned char)(*vm._pc)+32);
    if (global_recordingFile) {
      VM_RECORD_PLAYBACK(vm._pc, "pc");
      VM_RECORD_PLAYBACK(vm._stackPointer, "stackPointer");
    }
#endif
    switch (*pc) {
    case vm_ref: {
      uint8_t n = *(++pc);
      DBG_VM1("ref %" PRIu8 "\n", n);
      vm.push(sp, *(vm.reg(fp, n)));
      pc++;
      break;
    }
    case vm_const: {
      uint8_t n = *(++pc);
      DBG_VM1("const %" PRIu8 "\n", n);
      T_O* value = literals[n];
      vm.push(sp, value);
      VM_RECORD_PLAYBACK(value, "const");
      pc++;
      break;
    }
    case vm_closure: {
      uint8_t n = *(++pc);
      DBG_VM("closure %" PRIu8 "\n", n);
      vm.push(sp, closed[n]);
      pc++;
      break;
    }
    case vm_call: {
      uint8_t nargs = *(++pc);
      DBG_VM1("call %" PRIu8 "\n", nargs);
      T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
      Function_sp func = gc::As_assert<Function_sp>(tfunc);
      T_O** args = vm.stackref(sp, nargs - 1);
      vm.push(sp, (T_O*)pc);
      vm._pc = pc;
      vm._stackPointer = sp;
      T_mv res = func->apply_raw(nargs, args);
      multipleValues.setN(res.raw_(), res.number_of_values());
      vm.drop(sp, nargs + 2);
      pc++;
      break;
    }
    case vm_call_receive_one: {
      uint8_t nargs = *(++pc);
      DBG_VM1("call-receive-one %" PRIu8 "\n", nargs);
      T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
      Function_sp func = gc::As_assert<Function_sp>(tfunc);
      VM_RECORD_PLAYBACK(func, "vm_call_receive_one_func");
      VM_RECORD_PLAYBACK((void*)(uintptr_t)nargs, "vm_call_receive_one_nargs");
      T_O** args = vm.stackref(sp, nargs - 1);
#if DEBUG_VM_RECORD_PLAYBACK == 1
      for (size_t ii = 0; ii < nargs; ii++) {
        stringstream name_args;
        name_args << "vm_call_receive_one_arg" << ii;
        VM_RECORD_PLAYBACK(args[ii], name_args.str().c_str());
      }
#endif
      vm.push(sp, (T_O*)pc);
      vm._pc = pc;
      vm._stackPointer = sp;
      T_sp res = func->apply_raw(nargs, args);
      vm.drop(sp, nargs + 2);
      vm.push(sp, res.raw_());
      VM_RECORD_PLAYBACK(res.raw_(), "vm_call_receive_one");
      pc++;
      break;
    }
    case vm_call_receive_fixed: {
      uint8_t nargs = *(++pc);
      uint8_t nvals = *(++pc);
      DBG_VM("call-receive-fixed %" PRIu8 " %" PRIu8 "\n", nargs, nvals);
      T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
      Function_sp func = gc::As_assert<Function_sp>(tfunc);
      T_O** args = vm.stackref(sp, nargs - 1);
      vm.push(sp, (T_O*)pc);
      vm._pc = pc;
      vm._stackPointer = sp;
      T_mv res = func->apply_raw(nargs, args);
      vm.drop(sp, nargs + 2);
      if (nvals != 0) {
        vm.push(sp, res.raw_()); // primary
        size_t svalues = multipleValues.getSize();
        for (size_t i = 1; i < nvals; ++i)
          vm.push(sp, multipleValues.valueGet(i, svalues).raw_());
      }
      pc++;
      break;
    }
    case vm_bind: {
      uint8_t nelems = *(++pc);
      uint8_t base = *(++pc);
      DBG_VM1("bind %" PRIu8 " %" PRIu8 "\n", nelems, base);
      vm.copytoreg(fp, vm.stackref(sp, nelems - 1), nelems, base);
      vm.drop(sp, nelems);
      pc++;
      break;
    }
    case vm_set: {
      uint8_t n = *(++pc);
      DBG_VM("set %" PRIu8 "\n", n);
      vm.setreg(fp, n, vm.pop(sp));
      pc++;
      break;
    }
    case vm_make_cell: {
      DBG_VM1("make-cell\n");
      T_sp car((gctools::Tagged)(vm.pop(sp)));
      T_sp cdr((gctools::Tagged)nil<T_O>().raw_());
      vm.push(sp, Cons_O::create(car, cdr).raw_());
      pc++;
      break;
    }
    case vm_cell_ref: {
      DBG_VM1("cell-ref\n");
      T_sp cons((gctools::Tagged)vm.pop(sp));
      vm.push(sp, cons.unsafe_cons()->car().raw_());
      pc++;
      break;
    }
    case vm_cell_set: {
      DBG_VM("cell-set\n");
      T_sp cons((gctools::Tagged)vm.pop(sp));
      Cons_sp ccons = gc::As_assert<Cons_sp>(cons);
      T_O* val = vm.pop(sp);
      T_sp tval((gctools::Tagged)val);
      ccons->rplaca(tval);
      pc++;
      break;
    }
    case vm_make_closure: {
      uint8_t c = *(++pc);
      DBG_VM("make-closure %" PRIu8 "\n", c);
      T_sp fn_sp((gctools::Tagged)literals[c]);
      BytecodeSimpleFun_sp fn = gc::As_assert<BytecodeSimpleFun_sp>(fn_sp);
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      Closure_sp closure = Closure_O::make_bytecode_closure(fn, nclosed);
      // FIXME: Can we use some more abstracted access?
      vm.copyto(sp, nclosed, (T_O**)(closure->_Slots.data()));
      vm.drop(sp, nclosed);
      vm.push(sp, closure.raw_());
      pc++;
      break;
    }
    case vm_make_uninitialized_closure: {
      uint8_t c = *(++pc);
      DBG_VM("make-uninitialized-closure %" PRIu8 "\n", c);
      T_sp fn_sp((gctools::Tagged)literals[c]);
      BytecodeSimpleFun_sp fn = gc::As_assert<BytecodeSimpleFun_sp>(fn_sp);
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      Closure_sp closure = Closure_O::make_bytecode_closure(fn, nclosed);
      vm.push(sp, closure.raw_());
      pc++;
      break;
    }
    case vm_initialize_closure: {
      uint8_t c = *(++pc);
      DBG_VM("initialize-closure %" PRIu8 "\n", c);
      T_sp tclosure((gctools::Tagged)(*(vm.reg(fp, c))));
      Closure_sp closure = gc::As_assert<Closure_sp>(tclosure);
      // FIXME: We ought to be able to get the closure size directly
      // from the closure through some nice method.
      BytecodeSimpleFun_sp fn = gc::As_assert<BytecodeSimpleFun_sp>(closure->entryPoint());
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      vm.copyto(sp, nclosed, (T_O**)(closure->_Slots.data()));
      vm.drop(sp, nclosed);
      pc++;
      break;
    }
    case vm_return: {
      DBG_VM1("return\n");
      // since the stack pointer is a local variable we don't need to
      // adjust it.
      size_t nvalues = multipleValues.getSize();
      return gctools::return_type(multipleValues.valueGet(0, nvalues).raw_(), nvalues);
    }
    case vm_bind_required_args: {
      uint8_t nargs = *(++pc);
      DBG_VM("bind-required-args %" PRIu8 "\n", nargs);
      vm.copytoreg(fp, lcc_args, nargs, 0);
      pc++;
      break;
    }
    case vm_bind_optional_args: {
      uint8_t nreq = *(++pc);
      uint8_t nopt = *(++pc);
      DBG_VM("bind-optional-args %" PRIu8 " %" PRIu8 "\n", nreq, nopt);
      if (lcc_nargs >= nreq + nopt) { // enough args- easy mode
        DBG_VM("  enough args\n");
        vm.copytoreg(fp, lcc_args + nreq, nopt, nreq);
      } else { // put in some unbounds
        DBG_VM("  not enough args\n");
        vm.copytoreg(fp, lcc_args + nreq, lcc_nargs - nreq, nreq);
        vm.fillreg(fp, unbound<T_O>().raw_(), nreq + nopt - lcc_nargs, lcc_nargs);
      }
      pc++;
      break;
    }
    case vm_listify_rest_args: {
      uint8_t start = *(++pc);
      DBG_VM("listify-rest-args %" PRIu8 "\n", start);
      ql::list rest;
      for (size_t i = start; i < lcc_nargs; ++i) {
        T_sp tobj((gctools::Tagged)lcc_args[i]);
        rest << tobj;
      }
      vm.push(sp, rest.cons().raw_());
      pc++;
      break;
    }
    case vm_vaslistify_rest_args: {
      //
      // This pushes two vaslist structures (each two words that look like fixnums)
      // onto the stack.  the theVaslist_backup is used by vaslist_rewind
      //
      uint8_t start = *(++pc);
      DBG_VM("vaslistify-rest-args %" PRIu8 "\n", start);
      auto theVaslist = vm.alloca_vaslist2(sp, lcc_args + start, lcc_nargs - start);
      vm.push(sp, theVaslist);
      pc++;
      break;
    }
    case vm_parse_key_args: {
      uint8_t more_start = *(++pc);
      uint8_t key_count_info = *(++pc);
      uint8_t key_literal_start = *(++pc);
      uint8_t key_frame_start = *(++pc);
      DBG_VM("parse-key-args %" PRIu8 " %" PRIu8 " %" PRIu8 " %" PRIu8 "\n", more_start, key_count_info, key_literal_start,
             key_frame_start);
      uint8_t key_count = key_count_info & 0x7f;
      bool ll_aokp = key_count_info & 0x80;
      bool aokp = false;
      T_sp unknown_keys = nil<T_O>();
      // Set keyword arguments to unbound.
      vm.fillreg(fp, unbound<T_O>().raw_(), key_count, key_frame_start);
      if (lcc_nargs > more_start) {
        if (((lcc_nargs - more_start) % 2) != 0) {
          T_sp tclosure((gctools::Tagged)gctools::tag_general(closure));
          throwOddKeywordsError(tclosure);
        }
        // We grab keyword arguments from the end to the beginning.
        // This means that earlier arguments are put in their variables
        // last, matching the CL semantics.
        // KLUDGE: We use a signed type so that if more_start is zero we don't
        // wrap arg_index around. There's probably a cleverer solution.
        ptrdiff_t arg_index;
        for (arg_index = lcc_nargs - 1; arg_index >= more_start; arg_index -= 2) {
          bool valid_key_p = false;
          T_O* key = lcc_args[arg_index - 1];
          if (key == kw::_sym_allow_other_keys.raw_()) {
            valid_key_p = true; // aok is always valid.
            T_sp value((gctools::Tagged)(lcc_args[arg_index]));
            aokp = value.notnilp();
          }
          for (size_t key_id = 0; key_id < key_count; ++key_id) {
            T_O* ckey = literals[key_id + key_literal_start];
            if (key == ckey) {
              valid_key_p = true;
              *vm.reg(fp, key_frame_start + key_id) = lcc_args[arg_index];
              break;
            }
          }
          if (!valid_key_p & !ll_aokp) {
            T_sp tunknown((gctools::Tagged)(lcc_args[arg_index - 1]));
            unknown_keys = Cons_O::create(tunknown, unknown_keys);
          }
        }
      }
      if (unknown_keys.notnilp() && !aokp) {
        T_sp tclosure((gctools::Tagged)gctools::tag_general(closure));
        throwUnrecognizedKeywordArgumentError(tclosure, unknown_keys);
      }
      pc++;
      break;
    }
    case vm_jump_8: {
      int8_t rel = *(pc + 1);
      DBG_VM1("jump %" PRId8 "\n", rel);
      pc += rel;
      break;
    }
    case vm_jump_16: {
      int16_t rel = read_s16(pc + 1);
      DBG_VM("jump %" PRId16 "\n", rel);
      pc += rel;
      break;
    }
    case vm_jump_24: {
      int32_t rel = read_label(pc, 3);
      DBG_VM("jump %" PRId32 "\n", rel);
      pc += rel;
      break;
    }
    case vm_jump_if_8: {
      int8_t rel = *(pc + 1);
      DBG_VM1("jump-if %" PRId8 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop(sp));
      VM_RECORD_PLAYBACK(tval.raw_(), "vm_jump_if_8");
      if (tval.notnilp())
        pc += rel;
      else
        pc += 2;
      break;
    }
    case vm_jump_if_16: {
      int16_t rel = read_s16(pc + 1);
      DBG_VM("jump-if %" PRId16 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop(sp));
      if (tval.notnilp())
        pc += rel;
      else
        pc += 3;
      break;
    }
    case vm_jump_if_24: {
      int32_t rel = read_label(pc, 3);
      DBG_VM("jump-if %" PRId32 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop(sp));
      if (tval.notnilp())
        pc += rel;
      else
        pc += 4;
      break;
    }
    case vm_jump_if_supplied_8: {
      uint8_t slot = *(pc + 1);
      int32_t rel = *(pc + 2);
      DBG_VM("jump-if-supplied %" PRIu8 " %" PRId8 "\n", slot, rel);
      T_sp tval((gctools::Tagged)(*(vm.reg(fp, slot))));
      if (tval.unboundp())
        pc += 3;
      else
        pc += rel;
      break;
    }
    case vm_jump_if_supplied_16: {
      uint8_t slot = *(pc + 1);
      int16_t rel = read_s16(pc + 2);
      DBG_VM("jump-if-supplied %" PRIu8 " %" PRId16 "\n", slot, rel);
      T_sp tval((gctools::Tagged)(*(vm.reg(fp, slot))));
      if (tval.unboundp())
        pc += 4;
      else
        pc += rel;
      break;
    }
    case vm_check_arg_count_LE: {
      uint8_t max_nargs = *(++pc);
      DBG_VM("check-arg-count<= %" PRIu8 "\n", max_nargs);
      if (lcc_nargs > max_nargs) {
        T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
        throwTooManyArgumentsError(tclosure, lcc_nargs, max_nargs);
      }
      pc++;
      break;
    }
    case vm_check_arg_count_GE: {
      uint8_t min_nargs = *(++pc);
      DBG_VM("check-arg-count>= %" PRIu8 "\n", min_nargs);
      if (lcc_nargs < min_nargs) {
        T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
        throwTooFewArgumentsError(tclosure, lcc_nargs, min_nargs);
      }
      pc++;
      break;
    }
    case vm_check_arg_count_EQ: {
      uint8_t req_nargs = *(++pc);
      DBG_VM1("check-arg-count= %" PRIu8 "\n", req_nargs);
      if (lcc_nargs != req_nargs) {
        T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
        wrongNumberOfArguments(tclosure, lcc_nargs, req_nargs);
      }
      pc++;
      break;
    }
    case vm_push_values: {
      // TODO: Direct copy?
      DBG_VM("push-values\n");
      size_t nvalues = multipleValues.getSize();
      DBG_VM("  nvalues = %zu\n", nvalues);
      for (size_t i = 0; i < nvalues; ++i)
        vm.push(sp, multipleValues.valueGet(i, nvalues).raw_());
      // We could skip tagging this, but that's error-prone.
      vm.push(sp, make_fixnum(nvalues).raw_());
      pc++;
      break;
    }
    case vm_append_values: {
      DBG_VM("append-values\n");
      T_sp texisting_values((gctools::Tagged)vm.pop(sp));
      size_t existing_values = texisting_values.unsafe_fixnum();
      DBG_VM("  existing-values = %zu\n", existing_values);
      size_t nvalues = multipleValues.getSize();
      DBG_VM("  nvalues = %zu\n", nvalues);
      for (size_t i = 0; i < nvalues; ++i)
        vm.push(sp, multipleValues.valueGet(i, nvalues).raw_());
      vm.push(sp, make_fixnum(nvalues + existing_values).raw_());
      pc++;
      break;
    }
    case vm_pop_values: {
      DBG_VM("pop-values\n");
      T_sp texisting_values((gctools::Tagged)vm.pop(sp));
      size_t existing_values = texisting_values.unsafe_fixnum();
      DBG_VM("  existing-values = %zu\n", existing_values);
      vm.copyto(sp, existing_values, &my_thread->_MultipleValues._Values[0]);
      multipleValues.setSize(existing_values);
      vm.drop(sp, existing_values);
      pc++;
      break;
    }
    case vm_mv_call: {
      DBG_VM("mv-call\n");
      T_sp tnargs((gctools::Tagged)vm.pop(sp));
      size_t nargs = tnargs.unsafe_fixnum();
      DBG_VM("  nargs = %zu\n", nargs);
      T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
      Function_sp func = gc::As_assert<Function_sp>(tfunc);
      T_O** args = vm.stackref(sp, nargs - 1);
      vm.push(sp, (T_O*)pc);
      vm._pc = pc;
      vm._stackPointer = sp;
      T_mv res = func->apply_raw(nargs, args);
      vm.drop(sp, nargs + 1 + 1); // 1 each for func, pc
      multipleValues.setN(res.raw_(), res.number_of_values());
      pc++;
      break;
    }
    case vm_mv_call_receive_one: {
      DBG_VM("mv-call-receive-one\n");
      T_sp tnargs((gctools::Tagged)vm.pop(sp));
      size_t nargs = tnargs.unsafe_fixnum();
      DBG_VM("  nargs = %zu\n", nargs);
      T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
      Function_sp func = gc::As_assert<Function_sp>(tfunc);
      T_O** args = vm.stackref(sp, nargs - 1);
      vm.push(sp, (T_O*)pc);
      vm._pc = pc;
      vm._stackPointer = sp;
      T_sp res = func->apply_raw(nargs, args);
      vm.drop(sp, nargs + 2);
      multipleValues.set1(res);
      vm.push(sp, res.raw_());
      pc++;
      break;
    }
    case vm_mv_call_receive_fixed: {
      uint8_t nvals = *(++pc);
      DBG_VM("mv-call-receive-fixed %" PRIu8 "\n", nvals);
      T_sp tnargs((gctools::Tagged)vm.pop(sp));
      size_t nargs = tnargs.unsafe_fixnum();
      T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
      Function_sp func = gc::As_assert<Function_sp>(tfunc);
      T_O** args = vm.stackref(sp, nargs - 1);
      vm.push(sp, (T_O*)pc);
      vm._pc = pc;
      vm._stackPointer = sp;
      T_mv res = func->apply_raw(nargs, args);
      vm.drop(sp, nargs + 2);
      if (nvals != 0) {
        vm.push(sp, res.raw_()); // primary
        size_t svalues = multipleValues.getSize();
        for (size_t i = 1; i < nvals; ++i)
          vm.push(sp, multipleValues.valueGet(i, svalues).raw_());
      }
      pc++;
      break;
    }
    case vm_save_sp: {
      uint8_t n = *(++pc);
      DBG_VM("save sp %" PRIu8 "\n", n);
      vm.savesp(fp, sp, n);
      pc++;
      break;
    }
    case vm_restore_sp: {
      uint8_t n = *(++pc);
      DBG_VM("restore sp %" PRIu8 "\n", n);
      vm.restoresp(fp, sp, n);
      pc++;
      break;
    }
    case vm_entry: {
      uint8_t n = *(++pc);
      DBG_VM("entry %" PRIu8 "\n", n);
      pc++;
      jmp_buf target;
      void* frame = __builtin_frame_address(0);
      vm._pc = pc;
      TagbodyDynEnv_sp env = TagbodyDynEnv_O::create(frame, &target);
      vm.setreg(fp, n, env.raw_());
      gctools::StackAllocate<Cons_O> sa_ec(env, my_thread->dynEnvStackGet());
      DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
      _setjmp(target);
    again:
      try {
        bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
        sp = vm._stackPointer;
        pc = vm._pc;
      } catch (Unwind& uw) {
        if (uw.getFrame() == frame) {
          my_thread->dynEnvStackGet() = sa_ec.asSmartPtr();
          goto again;
        } else
          throw;
      }
      break;
    }
    case vm_exit_8: {
      int8_t rel = *(pc + 1);
      DBG_VM("exit %" PRId8 "\n", rel);
      vm._pc = pc + rel;
      T_sp ttde((gctools::Tagged)(vm.pop(sp)));
      TagbodyDynEnv_sp tde = gc::As_assert<TagbodyDynEnv_sp>(ttde);
      sjlj_unwind(tde, 1);
    }
    case vm_exit_16: {
      int16_t rel = read_s16(pc + 1);
      DBG_VM("exit %" PRId16 "\n", rel);
      vm._pc = pc + rel;
      T_sp ttde((gctools::Tagged)(vm.pop(sp)));
      TagbodyDynEnv_sp tde = gc::As_assert<TagbodyDynEnv_sp>(ttde);
      sjlj_unwind(tde, 1);
    }
    case vm_exit_24: {
      int32_t rel = read_label(pc, 3);
      DBG_VM("exit %" PRId32 "\n", rel);
      vm._pc = pc + rel;
      T_sp ttde((gctools::Tagged)(vm.pop(sp)));
      TagbodyDynEnv_sp tde = gc::As_assert<TagbodyDynEnv_sp>(ttde);
      sjlj_unwind(tde, 1);
    }
    case vm_entry_close: {
      DBG_VM("entry-close\n");
      // This sham return value just gets us out of the bytecode_vm call in
      // vm_entry, above.
      vm._pc = pc + 1;
      vm._stackPointer = sp;
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    case vm_catch_8: {
      int8_t rel = *(pc + 1);
      DBG_VM("catch-8 %" PRId8 "\n", rel);
      unsigned char* target = pc + rel;
      bool thrown = true;
      pc += 2;
      T_sp tag((gctools::Tagged)(vm.pop(sp)));
      vm._pc = pc;
      call_with_catch(tag, [&]() {
        T_mv result = bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
        thrown = false;
        return result;
      });
      if (thrown) pc = target;
      else pc = vm._pc;
      break;
    }
    case vm_catch_16: {
      int16_t rel = read_s16(pc + 1);
      DBG_VM("catch-8 %" PRId16 "\n", rel);
      unsigned char* target = pc + rel;
      bool thrown = true;
      pc += 3;
      T_sp tag((gctools::Tagged)(vm.pop(sp)));
      vm._pc = pc;
      call_with_catch(tag, [&]() {
        T_mv result = bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
        thrown = false;
        return result;
      });
      if (thrown) pc = target;
      else pc = vm._pc;
      break;
    }
    case vm_throw: {
      DBG_VM("throw\n");
      T_sp tag((gctools::Tagged)(vm.pop(sp)));
      sjlj_throw(tag);
    }
    case vm_catch_close: {
      DBG_VM("entry-close\n");
      vm._pc = pc + 1;
      vm._stackPointer = sp;
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    case vm_special_bind: {
      uint8_t c = *(++pc);
      DBG_VM("special-bind %" PRIu8 "\n", c);
      T_sp value((gctools::Tagged)(vm.pop(sp)));
      pc++;
      T_sp cell((gctools::Tagged)literals[c]);
      vm._pc = pc;
      call_with_cell_bound(gc::As_assert<VariableCell_sp>(cell), value,
                           [&]() { return bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args); });
      sp = vm._stackPointer;
      pc = vm._pc;
      break;
    }
    case vm_symbol_value: {
      uint8_t c = *(++pc);
      DBG_VM("symbol-value %" PRIu8 "\n", c);
      T_sp cell_sp((gctools::Tagged)literals[c]);
      VariableCell_sp cell = gc::As_assert<VariableCell_sp>(cell_sp);
      vm.push(sp, cell->value().raw_());
      pc++;
      break;
    }
    case vm_symbol_value_set: {
      uint8_t c = *(++pc);
      DBG_VM("symbol-value-set %" PRIu8 "\n", c);
      T_sp cell_sp((gctools::Tagged)literals[c]);
      VariableCell_sp cell = gc::As_assert<VariableCell_sp>(cell_sp);
      T_sp value((gctools::Tagged)(vm.pop(sp)));
      cell->set_value(value);
      pc++;
      break;
    }
    case vm_unbind: {
      DBG_VM("unbind\n");
      vm._pc = pc + 1;
      vm._stackPointer = sp;
      // This return value is not actually used - we're just returning from
      // a bytecode_vm recursively invoked by vm_special_bind above.
      // (or vm_progv)
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    case vm_progv: {
      uint8_t c = *(++pc); // environment
      DBG_VM1("progv %" PRIu8 "\n", c);
      T_sp vals((gctools::Tagged)(vm.pop(sp)));
      T_sp vars((gctools::Tagged)(vm.pop(sp)));
      vm._pc = ++pc;
      fprogv(vars, vals, [&]() { return bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args); });
      sp = vm._stackPointer;
      pc = vm._pc;
      break;
    }
    case vm_fdefinition: {
      // We have function cells in the literals vector. While these are
      // themselves callable, we have to resolve the cell because we
      // use vm_fdefinition for lookup of #'foo.
      uint8_t c = *(++pc);
      DBG_VM1("fdefinition %" PRIu8 "\n", c);
      T_sp cell((gctools::Tagged)literals[c]);
      Function_sp fun = gc::As_assert<FunctionCell_sp>(cell)->fdefinition();
      vm.push(sp, fun.raw_());
      VM_RECORD_PLAYBACK(fun.raw_(), "fdefinition");
      pc++;
      break;
    }
    case vm_nil:
      DBG_VM("nil\n");
      vm.push(sp, nil<T_O>().raw_());
      pc++;
      break;
    case vm_push: {
      DBG_VM1("push\n");
      vm.push(sp, multipleValues.valueGet(0, multipleValues.getSize()).raw_());
      pc++;
      break;
    }
    case vm_pop: {
      DBG_VM1("pop\n");
      T_sp obj((gctools::Tagged)vm.pop(sp));
      multipleValues.set1(obj);
      pc++;
      break;
    }
    case vm_dup: {
      DBG_VM1("dup\n");
      T_O* obj = vm.pop(sp);
      vm.push(sp, obj);
      vm.push(sp, obj);
      pc++;
      break;
    }
    case vm_fdesignator: {
      uint8_t c = *(++pc); // ignored environment parameter
      DBG_VM1("fdesignator %" PRIu8 "\n", c);
      T_sp desig((gctools::Tagged)vm.pop(sp));
      Function_sp fun = coerce::calledFunctionDesignator(desig);
      vm.push(sp, fun.raw_());
      VM_RECORD_PLAYBACK(run.raw_(), "fdesignator");
      pc++;
      break;
    }
    case vm_called_fdefinition: {
      // This is like FDEFINITION except that we know the result will
      // just be called. So, we can just use the cell directly
      // without checking fboundedness, and this is just like const.
      // (const would be different on an implementation that doesn't
      //  have funcallable function cells.)
      uint8_t c = *(++pc);
      DBG_VM1("called-fdefinition %" PRIu8 "\n", c);
      T_O* fun = literals[c];
      vm.push(sp, fun);
      VM_RECORD_PLAYBACK(fun, "called-fdefinition");
      pc++;
      break;
    }
    case vm_protect: {
      uint8_t c = *(++pc);
      DBG_VM("protect %" PRIu8 "\n", c);
      // Build a closure - this works mostly like make_closure.
      T_sp fn_sp((gctools::Tagged)literals[c]);
      BytecodeSimpleFun_sp fn = fn_sp.as_assert<BytecodeSimpleFun_O>();
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      // Technically we could avoid consing a closure when nclosed = 0
      // but I don't know that it's worth the trouble.
      Closure_sp cleanup = Closure_O::make_bytecode_closure(fn, nclosed);
      vm.copyto(sp, nclosed, (T_O**)(cleanup->_Slots.data()));
      vm.drop(sp, nclosed);
      // Now stick it onto the dynamic environment.
      vm._pc = ++pc;
      T_mv result = funwind_protect([&]() {
        return bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
      },
        [&]() { eval::funcall(cleanup); });
      // copied from vm_call - required to avoid the cleanup's values
      // for... some reason. I'm not totally sure.
      multipleValues.setN(result.raw_(), result.number_of_values());
      sp = vm._stackPointer;
      pc = vm._pc;
      break;
    }
    case vm_cleanup: {
      DBG_VM("cleanup\n");
      vm._pc = pc + 1;
      vm._stackPointer = sp;
      // We need to return the actual current values, or at least
      // their correct count, so that funwind_protect can save them.
      size_t nvalues = multipleValues.getSize();
      return gctools::return_type(multipleValues.valueGet(0, nvalues).raw_(), nvalues);
    }
    case vm_encell: {
      // abbreviation for ref N; make-cell; set N
      uint8_t n = *(++pc);
      DBG_VM1("encell %" PRIu8 "\n", n);
      T_sp val((gctools::Tagged)(*(vm.reg(fp, n))));
      vm.setreg(fp, n, Cons_O::create(val, nil<T_O>()).raw_());
      pc++;
      break;
    }
    case vm_long: {
      // In a separate function to facilitate better icache utilization
      // by bytecode_vm (hopefully)
      pc++;
      // FIXME: This is a stupid way of returning two values.
      pc = long_dispatch(vm, pc, multipleValues, literals, closed, closure, fp, sp, lcc_nargs, lcc_args, *pc);
      sp = vm._stackPointer;
      break;
    }
    default:
      SimpleFun_sp ep = closure->entryPoint();
      BytecodeModule_sp bcm = gc::As<BytecodeSimpleFun_sp>(ep)->code();
      unsigned char* codeStart = (unsigned char*)bcm->bytecode()->rowMajorAddressOfElement_(0);
      unsigned char* codeEnd = codeStart + bcm->bytecode()->arrayTotalSize();
      SIMPLE_ERROR("Unknown opcode {} pc: {}  module: {} - {}", *pc, (void*)pc, (void*)codeStart, (void*)codeEnd);
    };
  }
}

static unsigned char* long_dispatch(VirtualMachine& vm, unsigned char* pc, MultipleValues& multipleValues, T_O** literals,
                                    T_O** closed, Closure_O* closure, core::T_O** fp, core::T_O** sp, size_t lcc_nargs,
                                    core::T_O** lcc_args, uint8_t sub_opcode) {
  switch (sub_opcode) {
  case vm_ref: {
    uint8_t low = *(pc + 1);
    uint16_t n = low + (*(pc + 2) << 8);
    DBG_VM1("long ref %" PRIu16 "\n", n);
    vm.push(sp, *(vm.reg(fp, n)));
    pc += 3;
    break;
  }
  case vm_const: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("long const %" PRIu16 "\n", n);
    T_O* value = literals[n];
    vm.push(sp, value);
    VM_RECORD_PLAYBACK(value, "long const");
    pc++;
    break;
  }
  case vm_closure: {
    uint8_t low = *(pc + 1);
    uint16_t n = low + (*(pc + 2) << 8);
    DBG_VM1("long closure %" PRIu16 "\n", n);
    vm.push(sp, closed[n]);
    pc += 3;
    break;
  }
  case vm_call: {
    uint8_t low = *(pc + 1);
    uint16_t nargs = low + (*(pc + 2) << 8);
    DBG_VM1("long call %" PRIu16 "\n", nargs);
    T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
    Function_sp func = gc::As_assert<Function_sp>(tfunc);
    T_O** args = vm.stackref(sp, nargs - 1);
    vm.push(sp, (T_O*)pc);
    vm._pc = pc;
    vm._stackPointer = sp;
    T_mv res = func->apply_raw(nargs, args);
    multipleValues.setN(res.raw_(), res.number_of_values());
    vm.drop(sp, nargs + 2);
    pc += 3;
    break;
  }
  case vm_call_receive_one: {
    uint8_t low = *(pc + 1);
    uint16_t nargs = low + (*(pc + 2) << 8);
    DBG_VM1("long call-receive-one %" PRIu16 "\n", nargs);
    T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
    Function_sp func = gc::As_assert<Function_sp>(tfunc);
    VM_RECORD_PLAYBACK(func, "vm_call_receive_one_func");
    VM_RECORD_PLAYBACK((void*)(uintptr_t)nargs, "vm_call_receive_one_nargs");
    T_O** args = vm.stackref(sp, nargs - 1);
#if DEBUG_VM_RECORD_PLAYBACK == 1
    for (size_t ii = 0; ii < nargs; ii++) {
      stringstream name_args;
      name_args << "vm_call_receive_one_arg" << ii;
      VM_RECORD_PLAYBACK(args[ii], name_args.str().c_str());
    }
#endif
    vm.push(sp, (T_O*)pc);
    vm._pc = pc;
    vm._stackPointer = sp;
    T_sp res = func->apply_raw(nargs, args);
    vm.drop(sp, nargs + 2);
    vm.push(sp, res.raw_());
    VM_RECORD_PLAYBACK(res.raw_(), "vm_call_receive_one");
    pc += 3;
    break;
  }
  case vm_call_receive_fixed: {
    uint8_t low_nargs = *(pc + 1);
    uint16_t nargs = low_nargs + (*(pc + 2) << 8);
    uint8_t low_nvals = *(pc + 3);
    uint16_t nvals = low_nvals + (*(pc + 4) << 8);
    DBG_VM("long call-receive-fixed %" PRIu16 " %" PRIu16 "\n", nargs, nvals);
    T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
    Function_sp func = gc::As_assert<Function_sp>(tfunc);
    T_O** args = vm.stackref(sp, nargs - 1);
    vm.push(sp, (T_O*)pc);
    vm._pc = pc;
    vm._stackPointer = sp;
    T_mv res = func->apply_raw(nargs, args);
    vm.drop(sp, nargs + 2);
    if (nvals != 0) {
      vm.push(sp, res.raw_()); // primary
      size_t svalues = multipleValues.getSize();
      for (size_t i = 1; i < nvals; ++i)
        vm.push(sp, multipleValues.valueGet(i, svalues).raw_());
    }
    pc += 5;
    break;
  }
  case vm_bind: {
    uint8_t low_count = *(pc + 1);
    uint16_t count = low_count + (*(pc + 2) << 8);
    uint8_t low_offset = *(pc + 3);
    uint16_t offset = low_offset + (*(pc + 4) << 8);
    DBG_VM1("long bind %" PRIu16 " %" PRIu16 "\n", count, offset);
    vm.copytoreg(fp, vm.stackref(sp, count - 1), count, offset);
    vm.drop(sp, count);
    pc += 5;
    break;
  }
  case vm_set: {
    uint8_t low = *(pc + 1);
    uint16_t n = low + (*(pc + 2) << 8);
    DBG_VM("long set %" PRIu16 "\n", n);
    vm.setreg(fp, n, vm.pop(sp));
    pc += 3;
    break;
  }
  case vm_fdefinition: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("long fdefinition %" PRIu16 "\n", n);
    T_sp cell((gctools::Tagged)literals[n]);
    Function_sp fun = gc::As_assert<FunctionCell_sp>(cell)->fdefinition();
    vm.push(sp, fun.raw_());
    VM_RECORD_PLAYBACK(fun.raw_(), "long fdefinition");
    pc++;
    break;
  }
  case vm_make_closure: {
    uint8_t low = *(pc + 1);
    uint16_t c = low + (*(pc + 2) << 8);
    DBG_VM("long make-closure %" PRIu16 "\n", c);
    T_sp fn_sp((gctools::Tagged)literals[c]);
    BytecodeSimpleFun_sp fn = gc::As_assert<BytecodeSimpleFun_sp>(fn_sp);
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    Closure_sp closure = Closure_O::make_bytecode_closure(fn, nclosed);
    // FIXME: Can we use some more abstracted access?
    vm.copyto(sp, nclosed, (T_O**)(closure->_Slots.data()));
    vm.drop(sp, nclosed);
    vm.push(sp, closure.raw_());
    pc += 3;
    break;
  }
  case vm_make_uninitialized_closure: {
    uint8_t low = *(pc + 1);
    uint16_t c = low + (*(pc + 2) << 8);
    DBG_VM("long make-uninitialized-closure %" PRIu16 "\n", c);
    T_sp fn_sp((gctools::Tagged)literals[c]);
    BytecodeSimpleFun_sp fn = gc::As_assert<BytecodeSimpleFun_sp>(fn_sp);
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    Closure_sp closure = Closure_O::make_bytecode_closure(fn, nclosed);
    vm.push(sp, closure.raw_());
    pc += 3;
    break;
  }
  case vm_initialize_closure: {
    uint8_t low = *(pc + 1);
    uint16_t c = low + (*(pc + 2) << 8);
    DBG_VM("long initialize-closure %" PRIu16 "\n", c);
    T_sp tclosure((gctools::Tagged)(*(vm.reg(fp, c))));
    Closure_sp closure = gc::As_assert<Closure_sp>(tclosure);
    // FIXME: We ought to be able to get the closure size directly
    // from the closure through some nice method.
    BytecodeSimpleFun_sp fn = gc::As_assert<BytecodeSimpleFun_sp>(closure->entryPoint());
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    vm.copyto(sp, nclosed, (T_O**)(closure->_Slots.data()));
    vm.drop(sp, nclosed);
    pc += 3;
    break;
  }
  case vm_bind_required_args: {
    uint8_t low = *(pc + 1);
    uint16_t nargs = low + (*(pc + 2) << 8);
    DBG_VM("long bind-required-args %" PRIu16 "\n", nargs);
    vm.copytoreg(fp, lcc_args, nargs, 0);
    pc += 3;
    break;
  }
  case vm_bind_optional_args: {
    uint8_t nreq_low = *(pc + 1);
    uint16_t nreq = nreq_low + (*(pc + 2) << 8);
    uint8_t nopt_low = *(pc + 3);
    uint16_t nopt = nopt_low + (*(pc + 4) << 8);
    DBG_VM("long bind-optional-args %" PRIu16 " %" PRIu16 "\n", nreq, nopt);
    if (lcc_nargs >= nreq + nopt) {
      DBG_VM("  enough args\n");
      vm.copytoreg(fp, lcc_args + nreq, nopt, nreq);
    } else {
      DBG_VM("  not enough args\n");
      vm.copytoreg(fp, lcc_args + nreq, lcc_nargs - nreq, nreq);
      vm.fillreg(fp, unbound<T_O>().raw_(), nreq + nopt - lcc_nargs, lcc_nargs);
    }
    pc += 5;
    break;
  }
  case vm_listify_rest_args: {
    uint8_t low = *(pc + 1);
    uint16_t start = low + (*(pc + 2) << 8);
    DBG_VM("long listify-rest-args %" PRIu16 "\n", start);
    ql::list rest;
    for (size_t i = start; i < lcc_nargs; ++i) {
      T_sp tobj((gctools::Tagged)lcc_args[i]);
      rest << tobj;
    }
    vm.push(sp, rest.cons().raw_());
    pc += 3;
    break;
  }
  case vm_parse_key_args: {
    uint8_t more_start_low = *(pc + 1);
    uint16_t more_start = more_start_low + (*(pc + 2) << 8);
    uint8_t key_count_info_low = *(pc + 3);
    uint16_t key_count_info = key_count_info_low + (*(pc + 4) << 8);
    uint8_t key_literal_start_low = *(pc + 5);
    uint16_t key_literal_start = key_literal_start_low + (*(pc + 6) << 8);
    uint8_t key_frame_start_low = *(pc + 7);
    uint16_t key_frame_start = key_frame_start_low + (*(pc + 8) << 8);
    DBG_VM("long parse-key-args %" PRIu16 " %" PRIu16 " %" PRIu16 " %" PRIu16 "\n", more_start, key_count_info, key_literal_start,
           key_frame_start);
    uint16_t key_count = key_count_info & 0x7fff;
    bool ll_aokp = key_count_info & 0x8000;
    bool aokp = false;
    T_sp unknown_keys = nil<T_O>();
    // Set keyword arguments to unbound.
    vm.fillreg(fp, unbound<T_O>().raw_(), key_count, key_frame_start);
    if (lcc_nargs > more_start) {
      if (((lcc_nargs - more_start) % 2) != 0) {
        T_sp tclosure((gctools::Tagged)gctools::tag_general(closure));
        throwOddKeywordsError(tclosure);
      }
      // KLUDGE: We use a signed type so that if more_start is zero we don't
      // wrap arg_index around. There's probably a cleverer solution.
      ptrdiff_t arg_index;
      for (arg_index = lcc_nargs - 1; arg_index >= more_start; arg_index -= 2) {
        bool valid_key_p = false;
        T_O* key = lcc_args[arg_index - 1];
        if (key == kw::_sym_allow_other_keys.raw_()) {
          valid_key_p = true; // aok is always valid.
          T_sp value((gctools::Tagged)(lcc_args[arg_index]));
          aokp = value.notnilp();
        }
        for (size_t key_id = 0; key_id < key_count; ++key_id) {
          T_O* ckey = literals[key_id + key_literal_start];
          if (key == ckey) {
            valid_key_p = true;
            *vm.reg(fp, key_frame_start + key_id) = lcc_args[arg_index];
            break;
          }
        }
        if (!valid_key_p && !ll_aokp) {
          T_sp tunknown((gctools::Tagged)key);
          unknown_keys = Cons_O::create(tunknown, unknown_keys);
        }
      }
    }
    if (unknown_keys.notnilp() && !aokp) {
      T_sp tclosure((gctools::Tagged)gctools::tag_general(closure));
      throwUnrecognizedKeywordArgumentError(tclosure, unknown_keys);
    }
    pc += 9;
    break;
  }
  case vm_jump_if_supplied_8: {
    uint8_t low = *(pc + 1);
    uint16_t slot = low + (*(pc + 2) << 8);
    int32_t rel = *(pc + 3);
    DBG_VM("long jump-if-supplied %" PRIu16 " %" PRId8 "\n", slot, rel);
    T_sp tval((gctools::Tagged)(*(vm.reg(fp, slot))));
    if (tval.unboundp())
      pc += 4;
    else
      pc += rel - 1; // -1 for the long opcode at pc-1
    break;
  }
  case vm_jump_if_supplied_16: {
    uint8_t low = *(pc + 1);
    uint16_t slot = low + (*(pc + 2) << 8);
    int32_t rel = read_s16(pc + 3);
    DBG_VM("long jump-if-supplied %" PRIu16 " %" PRId8 "\n", slot, rel);
    T_sp tval((gctools::Tagged)(*(vm.reg(fp, slot))));
    if (tval.unboundp())
      pc += 5;
    else
      pc += rel - 1; // -1 for the long opcode at pc-1
    break;
  }
  case vm_check_arg_count_LE: {
    uint8_t low = *(pc + 1);
    uint16_t max_nargs = low + (*(pc + 2) << 8);
    DBG_VM("long check-arg-count<= %" PRIu16 "\n", max_nargs);
    if (lcc_nargs > max_nargs) {
      T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
      throwTooManyArgumentsError(tclosure, lcc_nargs, max_nargs);
    }
    pc += 3;
    break;
  }
  case vm_check_arg_count_GE: {
    uint8_t low = *(pc + 1);
    uint16_t min_nargs = low + (*(pc + 2) << 8);
    DBG_VM("long check-arg-count>= %" PRIu16 "\n", min_nargs);
    if (lcc_nargs < min_nargs) {
      T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
      throwTooFewArgumentsError(tclosure, lcc_nargs, min_nargs);
    }
    pc += 3;
    break;
  }
  case vm_check_arg_count_EQ: {
    uint8_t low = *(pc + 1);
    uint16_t req_nargs = low + (*(pc + 2) << 8);
    DBG_VM1("long check-arg-count= %" PRIu16 "\n", req_nargs);
    if (lcc_nargs != req_nargs) {
      T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
      wrongNumberOfArguments(tclosure, lcc_nargs, req_nargs);
    }
    pc += 3;
    break;
  }
  case vm_mv_call_receive_fixed: {
    uint8_t low = *(pc + 1);
    uint16_t nvals = low + (*(pc + 2) << 8);
    DBG_VM("long mv-call-receive-fixed %" PRIu16 "\n", nvals);
    T_sp tnargs((gctools::Tagged)vm.pop(sp));
    size_t nargs = tnargs.unsafe_fixnum();
    T_sp tfunc((gctools::Tagged)(*(vm.stackref(sp, nargs))));
    Function_sp func = gc::As_assert<Function_sp>(tfunc);
    T_O** args = vm.stackref(sp, nargs - 1);
    vm.push(sp, (T_O*)pc);
    vm._pc = pc;
    vm._stackPointer = sp;
    T_mv res = func->apply_raw(nargs, args);
    vm.drop(sp, nargs + 2); // 2 = func + nargs
    if (nvals != 0) {
      vm.push(sp, res.raw_()); // primary
      size_t svalues = multipleValues.getSize();
      for (size_t i = 1; i < nvals; ++i)
        vm.push(sp, multipleValues.valueGet(i, svalues).raw_());
    }
    pc += 3;
    break;
  }
  case vm_save_sp: {
    uint8_t low = *(pc + 1);
    uint16_t n = low + (*(pc + 2) << 8);
    DBG_VM("long save sp %" PRIu16 "\n", n);
    vm.savesp(fp, sp, n);
    pc += 3;
    break;
  }
  case vm_restore_sp: {
    uint8_t low = *(pc + 1);
    uint16_t n = low + (*(pc + 2) << 8);
    DBG_VM("long restore sp %" PRIu16 "\n", n);
    vm.restoresp(fp, sp, n);
    pc += 3;
    break;
  }
  case vm_entry: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM("long entry %" PRIu16 "\n", n);
    pc++;
    jmp_buf target;
    void* frame = __builtin_frame_address(0);
    vm._pc = pc;
    TagbodyDynEnv_sp env = TagbodyDynEnv_O::create(frame, &target);
    vm.setreg(fp, n, env.raw_());
    gctools::StackAllocate<Cons_O> sa_ec(env, my_thread->dynEnvStackGet());
    DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
    _setjmp(target);
  again:
    try {
      bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
      sp = vm._stackPointer;
      pc = vm._pc;
    } catch (Unwind& uw) {
      if (uw.getFrame() == frame) {
        my_thread->dynEnvStackGet() = sa_ec.asSmartPtr();
        goto again;
      } else
        throw;
    }
    break;
  }
  case vm_special_bind: {
    uint8_t low = *(pc + 1);
    uint16_t c = low + (*(pc + 2) << 8);
    DBG_VM("long special-bind %" PRIu16 "\n", c);
    T_sp value((gctools::Tagged)(vm.pop(sp)));
    pc += 3;
    T_sp cell((gctools::Tagged)literals[c]);
    vm._pc = pc;
    call_with_cell_bound(gc::As_assert<VariableCell_sp>(cell), value,
                         [&]() { return bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args); });
    pc = vm._pc;
    sp = vm._stackPointer;
    break;
  }
  case vm_symbol_value: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("long symbol-value %" PRIu16 "\n", n);
    T_sp cell_sp((gctools::Tagged)literals[n]);
    VariableCell_sp cell = gc::As_assert<VariableCell_sp>(cell_sp);
    vm.push(sp, cell->value().raw_());
    pc++;
    break;
  }
  case vm_symbol_value_set: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("long symbol-value %" PRIu16 "\n", n);
    T_sp cell_sp((gctools::Tagged)literals[n]);
    VariableCell_sp cell = gc::As_assert<VariableCell_sp>(cell_sp);
    T_sp value((gctools::Tagged)(vm.pop(sp)));
    cell->set_value(value);
    pc++;
    break;
  }
  case vm_progv: {
    uint8_t low = *(++pc);
    uint16_t c = low + (*(++pc) << 8);
    DBG_VM1("long progv %" PRIu16 "\n", c);
    T_sp vals((gctools::Tagged)(vm.pop(sp)));
    T_sp vars((gctools::Tagged)(vm.pop(sp)));
    vm._pc = ++pc;
    fprogv(vars, vals, [&]() { return bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args); });
    sp = vm._stackPointer;
    pc = vm._pc;
    break;
  }    
  case vm_fdesignator: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("long fdesignator %" PRIu16 "\n", n);
    T_sp desig((gctools::Tagged)vm.pop(sp));
    Function_sp fun = coerce::calledFunctionDesignator(desig);
    vm.push(sp, fun.raw_());
    pc++;
    break;
  }
  case vm_called_fdefinition: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("long called-fdefinition %" PRIu16 "\n", n);
    T_O* fun = literals[n];
    vm.push(sp, fun);
    VM_RECORD_PLAYBACK(fun, "long called-fdefinition");
    pc++;
    break;
  }
  case vm_protect: {
    uint8_t low = *(++pc);
    uint16_t c = low + (*(++pc) << 8);
    DBG_VM1("long protect %" PRIu16 "\n", c);
    T_sp fn_sp((gctools::Tagged)literals[c]);
    BytecodeSimpleFun_sp fn = fn_sp.as_assert<BytecodeSimpleFun_O>();
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    Closure_sp cleanup = Closure_O::make_bytecode_closure(fn, nclosed);
    vm.copyto(sp, nclosed, (T_O**)(cleanup->_Slots.data()));
    vm.drop(sp, nclosed);
    vm._pc = ++pc;
    T_mv result = funwind_protect([&]() {
      return bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
    },
      [&]() { eval::funcall(cleanup); });
    multipleValues.setN(result.raw_(), result.number_of_values());
    sp = vm._stackPointer;
    pc = vm._pc;
    break;
  }
  case vm_encell: {
    uint8_t low = *(++pc);
    uint16_t n = low + (*(++pc) << 8);
    DBG_VM1("encell %" PRIu16 "\n", n);
    T_sp val((gctools::Tagged)(*(vm.reg(fp, n))));
    vm.setreg(fp, n, Cons_O::create(val, nil<T_O>()).raw_());
    pc++;
    break;
  }
  default:
    SIMPLE_ERROR("Unknown LONG sub_opcode {}", sub_opcode);
  }
  vm._stackPointer = sp;
  return pc;
}

void VMFrameDynEnv_O::proceed() {
  VirtualMachine& vm = my_thread->_VM;
  vm._stackPointer = this->old_sp;
  vm._framePointer = this->old_fp;
}

}; // namespace core

extern "C" {

#define BYTECODE_COMPILE_THRESHOLD 65535

gctools::return_type bytecode_call(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args) {
  core::Closure_O* closure = gctools::untag_general<core::Closure_O*>((core::Closure_O*)lcc_closure);
  ASSERT(gc::IsA<core::BytecodeSimpleFun_sp>(closure->entryPoint()));
  auto entry = closure->entryPoint();
  core::BytecodeSimpleFun_sp entryPoint = gctools::As_assert<core::BytecodeSimpleFun_sp>(entry);
  // Maybe compile this function, if it's been called a lot.
  entryPoint->countCall();
  if (entryPoint->callCount() >= BYTECODE_COMPILE_THRESHOLD && comp::_sym_STARautocompile_hookSTAR->boundP() &&
      comp::_sym_STARautocompile_hookSTAR->symbolValue().notnilp()) {
    core::T_sp nat = core::eval::funcall(comp::_sym_STARautocompile_hookSTAR->symbolValue(), entry, nil<core::T_O>());
    entryPoint->setSimpleFun(gc::As_assert<core::SimpleFun_sp>(nat));
    // Since in practice the hook doesn't replace the simple fun
    // immediately, we just continue as we were.
  }
  // Proceed with the bytecode call.
  DBG_printf("%s:%d:%s This is where we evaluate bytecode functions pc: %p\n", __FILE__, __LINE__, __FUNCTION__, pc);
  size_t nlocals = entryPoint->_LocalsFrameSize;
  core::BytecodeModule_sp module = gc::As_assert<core::BytecodeModule_sp>(entryPoint->_Code);
  core::T_O** literals = (core::T_O**)&gc::As_assert<core::SimpleVector_sp>(module->_Literals)->_Data[0];
  core::T_O** closed = (core::T_O**)(closure->_Slots.data());
  core::VirtualMachine& vm = my_thread->_VM;
  VM_CURRENT_DATA(vm, (lcc_nargs >= 2) ? lcc_args[1] : NULL);
  VM_CURRENT_DATA1(vm, (lcc_nargs >= 3) ? lcc_args[2] : NULL);
  VM_INC_COUNTER0(vm);
  // We save the old PC for returns. We do _not_ do this for nonlocal exits,
  // since in that case the NLXing VM invocation sets the PC before escaping.
  unsigned char* old_pc = vm._pc;
  vm._pc = pc;
  // This is the only place we read the vm._stackPointer. Everywhere else we
  // use a local variable. This means we start new frames appropriately when
  // we're called from wherever, and also that we use the correct sp when
  // being unwound to.
  core::T_O** old_fp = vm._framePointer;
  core::T_O** old_sp = vm._stackPointer;
  vm.push(vm._stackPointer, (core::T_O*)old_fp);
  core::T_O** fp = vm._framePointer = vm._stackPointer;
  core::T_O** sp = vm.push_frame(fp, nlocals);
  try {
    gctools::StackAllocate<core::VMFrameDynEnv_O> frame(old_sp, old_fp);
    gctools::StackAllocate<core::Cons_O> sa_ec(frame.asSmartPtr(), my_thread->dynEnvStackGet());
    core::DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
    gctools::return_type res = bytecode_vm(vm, literals, closed, closure, fp, sp, lcc_nargs, lcc_args);
    vm._pc = old_pc;
    return res;
  } catch (core::VM_error& err) {
    printf("%s:%d:%s Recovering from VM_error\n", __FILE__, __LINE__, __FUNCTION__);
    return gctools::return_type(nil<core::T_O>().raw_(), 0);
  }
}

}; // extern C

namespace core {

CL_DEFUN Integer_sp core__side_stack_pointer() { return Integer_O::create((uint64_t)my_thread->_VM._stackPointer); }

#if DEBUG_VM_RECORD_PLAYBACK == 1
CL_DEFUN void core__vm_counter_step(size_t counterStep) { global_counterStep = counterStep; }

CL_DEFUN void core__vm_stack_trigger(size_t trigger) { global_stackTrigger = trigger; }
#endif

bool bytecode_module_contains_address_p(BytecodeModule_sp module, void* pc) {
  // FIXME: Not sure if this is the best way to go about it.
  Array_sp bytecode = module->bytecode();
  void* start = bytecode->rowMajorAddressOfElement_(0);
  void* end = (byte8_t*)start + bytecode->length() * sizeof(byte8_t);
  return (start <= pc) && (pc <= end);
}

bool bytecode_function_contains_address_p(BytecodeSimpleFun_sp fun, void* pc) {
  BytecodeModule_sp module = fun->code();
  Array_sp bytecode = module->bytecode();
  void* start = bytecode->rowMajorAddressOfElement_(fun->entryPcN());
  void* end = (byte8_t*)bytecode->rowMajorAddressOfElement_(fun->entryPcN() + fun->bytecodeSize()) + sizeof(byte8_t);
  return (start <= pc) && (pc <= end);
}

T_sp bytecode_function_for_pc(BytecodeModule_sp module, void* pc) {
  T_sp debuginfo = module->debugInfo();
  if (debuginfo.notnilp()) {
    for (auto info : *(gc::As_assert<SimpleVector_sp>(module->debugInfo()))) {
      if (gc::IsA<BytecodeSimpleFun_sp>(info) &&
          bytecode_function_contains_address_p(gc::As_unsafe<BytecodeSimpleFun_sp>(info), pc))
        return info;
    }
    // Should be impossible, but we don't want to err while a backtrace
    // is getting put together. TODO: Issue warning?
  }
  return nil<T_O>();
}

T_sp bytecode_spi_for_pc(BytecodeModule_sp module, void* pc) {
  Array_sp bytecode = module->bytecode();
  void* start = bytecode->rowMajorAddressOfElement_(0);
  ptrdiff_t bpc = (byte8_t*)pc - (byte8_t*)start;
  // Find the location info with the tightest enclosing bounds.
  size_t best_start = 0;
  size_t best_end = SIZE_MAX;
  T_sp best_spi = nil<T_O>();
  for (T_sp info : *(gc::As_assert<SimpleVector_sp>(module->debugInfo()))) {
    if (gc::IsA<BytecodeDebugLocation_sp>(info)) {
      BytecodeDebugLocation_sp entry = gc::As_unsafe<BytecodeDebugLocation_sp>(info);
      size_t start = entry->start().unsafe_fixnum();
      size_t end = entry->end().unsafe_fixnum();
      if ((start <= bpc) && (bpc < end) && (start >= best_start) && (end <= best_end)) {
        best_start = start;
        best_end = end;
        best_spi = entry->location();
      }
    }
  }
  return best_spi;
}

List_sp bytecode_bindings_for_pc(BytecodeModule_sp module, void* pc, T_O** fp) {
  Array_sp bytecode = module->bytecode();
  void* start = bytecode->rowMajorAddressOfElement_(0);
  ptrdiff_t bpc = (byte8_t*)pc - (byte8_t*)start;
  ql::list bindings;
  for (T_sp info : *(gc::As_assert<SimpleVector_sp>(module->debugInfo()))) {
    if (gc::IsA<BytecodeDebugVars_sp>(info)) {
      BytecodeDebugVars_sp entry = gc::As_unsafe<BytecodeDebugVars_sp>(info);
      size_t start = entry->start().unsafe_fixnum();
      size_t end = entry->end().unsafe_fixnum();
      if ((start <= bpc) && (bpc < end)) {
        for (Cons_sp cur : entry->bindings()) {
          auto bdv = gc::As_assert<BytecodeDebugVar_sp>(cur->car());
          T_sp name = bdv->name();
          // We add one here because *fp is the previous fp and so the variables
          // actually start at *(fp+1).
          T_O* tvalue = *(fp + bdv->frameIndex() + 1);
          T_sp value((gctools::Tagged)tvalue);
          if (bdv->cellp())
            bindings << Cons_O::create(name, gc::As_assert<Cons_sp>(value)->car());
          else
            bindings << Cons_O::create(name, value);
        }
      }
    }
  }
  return bindings.cons();
}

void* bytecode_pc() { return my_thread->_VM._pc; }

CL_DOCSTRING(
    R"(Call a function on each registered bytecode module. Order is undefined. New modules created during the mapping process may be skipped; this function does not synchronize. Return value undefined.)")
DOCGROUP(clasp);
CL_DEFUN void core__map_bytecode_modules(Function_sp f) {
  List_sp modules = _lisp->_Roots._AllBytecodeModules.load(std::memory_order_relaxed);
  for (auto mods : modules) {
    BytecodeModule_sp mod = gc::As_assert<BytecodeModule_sp>(oCar(mods));
    eval::funcall(f, mod);
  }
}

}; // namespace core
