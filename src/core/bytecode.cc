
#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <unistd.h>
#include <dlfcn.h>
#include <iomanip>
#include <cstdint>
#include <clasp/core/foundation.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/array.h>
#include <clasp/core/virtualMachine.h>
#include <clasp/core/primitives.h> // cl__fdefinition
#include <clasp/core/evaluator.h> // af_interpreter_lookup_macro
#include <clasp/core/sysprop.h> // core__get_sysprop
#include <clasp/core/unwind.h>
#include <clasp/core/ql.h>

#if 0
# define DBG_printf printf
# define DBG_VM(...) {printf("%s:%d:%s pc %p : stack %lu\n", __FILE__, __LINE__, __FUNCTION__, (void*)vm._pc, (uintptr_t)(vm)._stackPointer-(uintptr_t)(vm)._stackBottom ); printf(__VA_ARGS__); }
# if 0
#  define DBG_VM1(...) DBG_VM(__VA_ARGS__)
# else
#  define DBG_VM1(...)
# endif
#else
# define DBG_printf(...)
# define DBG_VM(...)
# define DBG_VM1(...)
#endif

namespace core {


void BytecodeModule_O::initialize() {
  this->_Literals = ComplexVector_T_O::make(16);
  this->_Bytecode = ComplexVector_byte8_t_O::make(1024,0,true,core::make_fixnum(0),nil<core::T_O>(),false, core::make_fixnum(0));
}

CL_DEFMETHOD
BytecodeModule_O::Literals_sp_Type BytecodeModule_O::literals() const {
  return this->_Literals;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_literals(BytecodeModule_O::Literals_sp_Type o) {
  this->_Literals = o;
}

CL_DEFMETHOD
BytecodeModule_O::Bytecode_sp_Type BytecodeModule_O::bytecode() const {
  return this->_Bytecode;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_bytecode(BytecodeModule_O::Bytecode_sp_Type o) {
  this->_Bytecode = o;
}

CL_DEFMETHOD
T_sp BytecodeModule_O::compileInfo() const {
  return this->_CompileInfo;
}

CL_DEFMETHOD
void BytecodeModule_O::setf_compileInfo(T_sp o) {
  this->_CompileInfo = o;
}

T_sp BytecodeCmpEnv_O::variableInfo(T_sp varname) {
  T_sp vars = this->vars();
  if (vars.nilp()) return vars;
  else {
    T_sp pair = gc::As<Cons_sp>(vars)->assoc(varname, nil<T_O>(),
                                             cl::_sym_eq, nil<T_O>());
    if (pair.nilp()) return pair;
    else return oCdr(pair);
  }
}

T_sp BytecodeCmpEnv_O::lookupSymbolMacro(T_sp sname) {
  T_sp info = this->variableInfo(sname);
  if (gc::IsA<BytecodeCmpSymbolMacroVarInfo_sp>(info))
    return gc::As_unsafe<BytecodeCmpSymbolMacroVarInfo_sp>(info)->expander();
  else if (info.notnilp()) { // global?
    T_mv result = core__get_sysprop(sname, ext::_sym_symbolMacro);
    if (gc::As<T_sp>(result.valueGet_(1)).notnilp()) {
      return result;
    } else return nil<T_O>();
  } else return nil<T_O>();
}

T_sp BytecodeCmpEnv_O::functionInfo(T_sp funname) {
  T_sp funs = this->funs();
  if (funs.nilp()) return funs;
  else {
    T_sp pair = gc::As<Cons_sp>(funs)->assoc(funname, nil<T_O>(),
                                             cl::_sym_equal, nil<T_O>());
    if (pair.nilp()) return pair;
    else return oCdr(pair);
  }
}

T_sp BytecodeCmpEnv_O::lookupMacro(T_sp macroname) {
  T_sp info = this->functionInfo(macroname);
  if (gc::IsA<BytecodeCmpGlobalMacroInfo_sp>(info))
    return gc::As_unsafe<BytecodeCmpGlobalMacroInfo_sp>(info)->expander();
  else if (gc::IsA<BytecodeCmpLocalMacroInfo_sp>(info))
    return gc::As_unsafe<BytecodeCmpLocalMacroInfo_sp>(info)->expander();
  else if (info.nilp()) // could be global
    return af_interpreter_lookup_macro(macroname, nil<T_O>());
  else return nil<T_O>();
}

static inline uint8_t read_uint8( unsigned char*& pc ) {
  return *(++pc);
}

static inline int32_t read_label(unsigned char* pc, size_t nbytes) {
  // Labels are stored little-endian.
  uint32_t result = 0;
  for (size_t i = 0; i < nbytes - 1; ++i) result |= *(++pc) << i * 8;
  uint8_t msb = *(++pc);
  result |= (msb & 0x7f) << ((nbytes - 1) * 8);
  // Signed conversion. TODO: Get something that optimizes well.
  int32_t returnResult;
  if (msb & 0x80)
    returnResult = static_cast<int32_t>(result) - (1 << (8 * nbytes - 1));
  else
    returnResult = static_cast<int32_t>(result);
  return returnResult;
}

#define DEBUG_VM_RECORD_PLAYBACK 0

struct VM_error {
};


#if DEBUG_VM_RECORD_PLAYBACK==1
static size_t global_counter = 0;
static size_t global_counterStep = 1024;
static size_t global_stackTrigger = 16384;

enum RecordingEnum { idle, playback, record };
static FILE*  global_recordingFile = NULL;
static RecordingEnum global_recordingState = idle;


void open_telemetry_file(const std::string& filename, RecordingEnum state) {
  if (state == record ) {
    global_recordingFile = fopen(filename.c_str(),"wb");
    global_recordingState = record;
  } else if (state == playback) {
    global_recordingFile = fopen(filename.c_str(),"rb");
    global_recordingState = playback;
  }
}

CL_DEFUN void core__start_recording_vm(const std::string& filename) {
  global_recordingFile = fopen(filename.c_str(),"wb");
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
  global_recordingFile = fopen(filename.c_str(),"rb");
  global_recordingState = playback;
}

CL_DEFUN void core__stop_playback_vm() {
  if (global_recordingFile) {
    fclose(global_recordingFile);
    global_recordingFile = NULL;
  }
  global_recordingState = idle;
}


void vm_error() {
  printf("Error in vm\n");
}

void vm_record_playback(void* value, const char* name) {
  if (global_recordingFile) {
    if (global_recordingState == record) {
      fwrite( (void*)&(value), sizeof(value), 1, global_recordingFile );
    }
    if (global_recordingState == playback) {
      void* read_value;
      fread( (void*)&read_value, sizeof(read_value), 1, global_recordingFile );
      if (read_value != (void*)value ) {
        printf("%s:%d:%s Mismatch between recorded %s %p and current %p\n", __FILE__, __LINE__, __FUNCTION__, name, read_value, (void*)value );
        vm_error();
        throw VM_error();
      }
    }
  }
}
#endif


#if DEBUG_VM_RECORD_PLAYBACK==1
# define VM_RECORD_PLAYBACK(value,name) vm_record_playback(value,name)
#else
# define VM_RECORD_PLAYBACK(value,name)
#endif


static void bytecode_vm_long(VirtualMachine& vm, T_O** literals, size_t nlocals,
                             Closure_O* closure,
                             uint8_t sub_opcode) {
  switch (sub_opcode) {
  case vm_ref: {
    uint8_t low = *(vm._pc + 1);
    uint16_t n = low + (*(vm._pc + 2) << 8);
    DBG_VM1("long ref %" PRIu16 "\n", n);
    vm.push(*(vm.reg(n)));
    vm._pc += 3;
    break;
  }
  case vm_const: {
    uint8_t low = *(++vm._pc);
    uint16_t n = low + (*(++vm._pc) << 8);
    DBG_VM1("long const %" PRIu16 "\n", n);
    T_O* value = literals[n];
    vm.push(value);
    VM_RECORD_PLAYBACK(value,"long const");
    vm._pc++;
    break;
  }
  case vm_closure: {
    uint8_t low = *(vm._pc + 1);
    uint16_t n = low + (*(vm._pc + 2) << 8);
    DBG_VM1("long closure %" PRIu16 "\n", n);
    vm.push((*closure)[n].raw_());
    vm._pc += 3;
    break;
  }
  case vm_call: {
    uint8_t low = *(vm._pc + 1);
    uint16_t nargs = low + (*(vm._pc + 2) << 8);
    DBG_VM1("long call %" PRIu16 "\n", nargs);
#ifdef DBG_VM1
    if (nargs + 1 > vm.npushed(nlocals))
      SIMPLE_ERROR("Help");
#endif
    T_O* func = *(vm.stackref(nargs));
    T_O** args = vm.stackref(nargs-1);
    T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
    res.saveToMultipleValue0();
    vm.drop(nargs+1);
    vm._pc += 3;
    break;
  }
  case vm_call_receive_one: {
    uint8_t low = *(vm._pc + 1);
    uint16_t nargs = low + (*(vm._pc + 2) << 8);
    DBG_VM1("long call-receive-one %" PRIu16 "\n", nargs);
    T_O* func = *(vm.stackref(nargs));
    VM_RECORD_PLAYBACK(func,"vm_call_receive_one_func");
    VM_RECORD_PLAYBACK((void*)(uintptr_t)nargs,"vm_call_receive_one_nargs");
    T_O** args = vm.stackref(nargs-1);
#if DEBUG_VM_RECORD_PLAYBACK==1
    for ( size_t ii=0; ii<nargs; ii++ ) {
      stringstream name_args;
      name_args << "vm_call_receive_one_arg" << ii;
      VM_RECORD_PLAYBACK(args[ii],name_args.str().c_str() );
    }
#endif
    T_sp res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
    vm.drop(nargs+1);
    vm.push(res.raw_());
    VM_RECORD_PLAYBACK(res.raw_(),"vm_call_receive_one");
    vm._pc += 3;
    break;
  }
  case vm_call_receive_fixed: {
    uint8_t low_nargs = *(vm._pc + 1);
    uint16_t nargs = low_nargs + (*(vm._pc + 2) << 8);
    uint8_t low_nvals = *(vm._pc + 3);
    uint16_t nvals = low_nvals + (*(vm._pc + 4) << 8);
    DBG_VM("long call-receive-fixed %" PRIu16 " %" PRIu16 "\n", nargs, nvals);
    T_O* func = *(vm.stackref(nargs));
    T_O** args = vm.stackref(nargs-1);
    T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
    vm.drop(nargs+1);
    if (nvals != 0) {
      MultipleValues &mv = lisp_multipleValues();
      vm.push(res.raw_()); // primary
      size_t svalues = mv.getSize();
      for (size_t i = 1; i < svalues; ++i)
        vm.push(mv.valueGet(i, svalues).raw_());
    }
    vm._pc += 5;
    break;
  }
  case vm_bind: {
    uint8_t low_count = *(vm._pc + 1);
    uint16_t count = low_count + (*(vm._pc + 2) << 8);
    uint8_t low_offset = *(vm._pc + 3);
    uint16_t offset = low_offset + (*(vm._pc + 4) << 8);
    DBG_VM1("long bind %" PRIu16 " %" PRIu16 "\n", count, offset);
    vm.copytoreg(vm.stackref(count-1), count, offset);
    vm.drop(count);
    vm._pc += 5;
    break;
  }
  case vm_set: {
    uint8_t low = *(vm._pc + 1);
    uint16_t n = low + (*(vm._pc + 2) << 8);
    DBG_VM("long set %" PRIu16 "\n", n);
    vm.setreg(n, vm.pop());
    vm._pc += 3;
    break;
  }
  case vm_fdefinition: {
    uint8_t low = *(++vm._pc);
    uint16_t n = low + (*(++vm._pc) << 8);
    DBG_VM1("long fdefinition %" PRIu16 "\n", n);
    T_sp sym((gctools::Tagged)literals[n]);
    vm.push(cl__fdefinition(sym).raw_());
    vm._pc++;
    break;
  }
  case vm_make_closure: {
    uint8_t low = *(vm._pc + 1);
    uint16_t c = low + (*(vm._pc + 2) << 8);
    DBG_VM("long make-closure %" PRIu16 "\n", c);
    T_sp fn_sp((gctools::Tagged)literals[c]);
    GlobalBytecodeEntryPoint_sp fn
      = gc::As<GlobalBytecodeEntryPoint_sp>(fn_sp);
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    Closure_sp closure
      = Closure_O::make_bytecode_closure(fn, nclosed);
      // FIXME: Can we use some more abstracted access?
    vm.copyto(nclosed, (T_O**)(closure->_Slots.data()));
    vm.drop(nclosed);
    vm.push(closure.raw_());
    vm._pc += 3;
    break;
  }
  case vm_make_uninitialized_closure: {
    uint8_t low = *(vm._pc + 1);
    uint16_t c = low + (*(vm._pc + 2) << 8);
    DBG_VM("long make-uninitialized-closure %" PRIu16 "\n", c);
    T_sp fn_sp((gctools::Tagged)literals[c]);
    GlobalBytecodeEntryPoint_sp fn
      = gc::As<GlobalBytecodeEntryPoint_sp>(fn_sp);
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    Closure_sp closure
      = Closure_O::make_bytecode_closure(fn, nclosed);
    vm.push(closure.raw_());
    vm._pc += 3;
    break;
  }
  case vm_initialize_closure: {
    uint8_t low = *(vm._pc + 1);
    uint16_t c = low + (*(vm._pc + 2) << 8);
    DBG_VM("long initialize-closure %" PRIu16 "\n", c);
    T_sp tclosure((gctools::Tagged)(*(vm.reg(c))));
    Closure_sp closure = gc::As<Closure_sp>(tclosure);
      // FIXME: We ought to be able to get the closure size directly
      // from the closure through some nice method.
    GlobalBytecodeEntryPoint_sp fn
      = gc::As<GlobalBytecodeEntryPoint_sp>(closure->entryPoint());
    size_t nclosed = fn->environmentSize();
    DBG_VM("  nclosed = %zu\n", nclosed);
    vm.copyto(nclosed, (T_O**)(closure->_Slots.data()));
    vm.drop(nclosed);
    vm._pc += 3;
    break;
  }
  case vm_mv_call_receive_fixed: {
    uint8_t low = *(vm._pc + 1);
    uint16_t nvals = low + (*(vm._pc + 2) << 8);
    DBG_VM("long mv-call-receive-fixed %" PRIu16 "\n", nvals);
    T_O* func = vm.pop();
    MultipleValues& mv = lisp_multipleValues();
    size_t nargs = mv.getSize();
    T_O* args[nargs];
    multipleValuesSaveToTemp(nargs, args);
    T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
    if (nvals != 0) {
      vm.push(res.raw_()); // primary
      size_t svalues = mv.getSize();
      for (size_t i = 1; i < svalues; ++i)
        vm.push(mv.valueGet(i, svalues).raw_());
    }
    vm._pc += 3;
    break;
  }
  case vm_symbol_value: {
    uint8_t low = *(++vm._pc);
    uint16_t n = low + (*(++vm._pc) << 8);
    DBG_VM1("long symbol-value %" PRIu16 "\n", n);
    T_sp sym_sp((gctools::Tagged)literals[n]);
    ASSERT(gc::IsA<Symbol_sp>(sym_sp));
    Symbol_sp sym = gc::As_unsafe<Symbol_sp>(sym_sp);
    vm.push(sym->symbolValue().raw_());
    vm._pc++;
    break;
  }
  case vm_symbol_value_set: {
    uint8_t low = *(++vm._pc);
    uint16_t n = low + (*(++vm._pc) << 8);
    DBG_VM1("long symbol-value %" PRIu16 "\n", n);
    T_sp sym_sp((gctools::Tagged)literals[n]);
    ASSERT(gc::IsA<Symbol_sp>(sym_sp));
    Symbol_sp sym = gc::As_unsafe<Symbol_sp>(sym_sp);
    T_sp value((gctools::Tagged)(vm.pop()));
    sym->setf_symbolValue(value);
    vm._pc++;
    break;
  }
  default:
      SIMPLE_ERROR("Unknown LONG sub_opcode %hu", sub_opcode);
  }
}

static gctools::return_type bytecode_vm(VirtualMachine& vm,
                                        T_O** literals,
                                        size_t nlocals, Closure_O* closure,
                                        size_t lcc_nargs,
                                        core::T_O** lcc_args) {
  while (1) {
#if DEBUG_VM_RECORD_PLAYBACK==1
    global_counter++;
    size_t stackHeight = (uintptr_t)(vm)._stackPointer-(uintptr_t)(vm)._stackBottom;
#if 0
    if (global_counter%global_counterStep==0) {
      printf("%lu %lu\n", global_counter, stackHeight);
    }
#endif
    if (stackHeight>global_stackTrigger) {
      printf("%s:%d:%s Exceeded stackTrigger %lu stackHeight = %lu returning\n", __FILE__, __LINE__, __FUNCTION__, global_stackTrigger, stackHeight );
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    //printf("%c", (unsigned char)(*vm._pc)+32);
    if (global_recordingFile) {
      VM_RECORD_PLAYBACK(vm._pc,"pc");
      VM_RECORD_PLAYBACK(vm._stackPointer,"stackPointer");
    }
#endif
    switch (*vm._pc) {
    case vm_ref: {
      uint8_t n = read_uint8(vm._pc);
      DBG_VM1("ref %" PRIu8 "\n", n);
      vm.push(*(vm.reg(n)));
      vm._pc++;
      break;
    }
    case vm_const: {
      uint8_t n = read_uint8(vm._pc);
      DBG_VM1("const %" PRIu8 "\n", n);
      T_O* value = literals[n];
      vm.push(value);
      VM_RECORD_PLAYBACK(value,"const");
      vm._pc++;
      break;
    }
    case vm_closure: {
      uint8_t n = read_uint8(vm._pc);
      DBG_VM("closure %" PRIu8 "\n", n);
      vm.push((*closure)[n].raw_());
      vm._pc++;
      break;
    }
    case vm_call: {
      uint8_t nargs = read_uint8(vm._pc);
      DBG_VM1("call %" PRIu8 "\n", nargs);
#ifdef DBG_VM1
      if (nargs + 1 > vm.npushed(nlocals))
        SIMPLE_ERROR("Help");
#endif
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      res.saveToMultipleValue0();
      vm.drop(nargs+1);
      vm._pc++;
      break;
    }
    case vm_call_receive_one: {
      uint8_t nargs = read_uint8(vm._pc);
      DBG_VM1("call-receive-one %" PRIu8 "\n", nargs);
      T_O* func = *(vm.stackref(nargs));
      VM_RECORD_PLAYBACK(func,"vm_call_receive_one_func");
      VM_RECORD_PLAYBACK((void*)(uintptr_t)nargs,"vm_call_receive_one_nargs");
      T_O** args = vm.stackref(nargs-1);
#if DEBUG_VM_RECORD_PLAYBACK==1
      for ( size_t ii=0; ii<nargs; ii++ ) {
        stringstream name_args;
        name_args << "vm_call_receive_one_arg" << ii;
        VM_RECORD_PLAYBACK(args[ii],name_args.str().c_str() );
      }
#endif
      T_sp res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.drop(nargs+1);
      vm.push(res.raw_());
      VM_RECORD_PLAYBACK(res.raw_(),"vm_call_receive_one");
      vm._pc++;
      break;
    }
    case vm_call_receive_fixed: {
      uint8_t nargs = read_uint8(vm._pc);
      uint8_t nvals = read_uint8(vm._pc);
      DBG_VM("call-receive-fixed %" PRIu8 " %" PRIu8 "\n", nargs, nvals);
      T_O* func = *(vm.stackref(nargs));
      T_O** args = vm.stackref(nargs-1);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.drop(nargs+1);
      if (nvals != 0) {
        MultipleValues &mv = lisp_multipleValues();
        vm.push(res.raw_()); // primary
        size_t svalues = mv.getSize();
        for (size_t i = 1; i < svalues; ++i)
          vm.push(mv.valueGet(i, svalues).raw_());
      }
      vm._pc++;
      break;
    }
    case vm_bind: {
      uint8_t nelems = read_uint8(vm._pc);
      uint8_t base = read_uint8(vm._pc);
      DBG_VM1("bind %" PRIu8 " %" PRIu8 "\n", nelems, base);
      vm.copytoreg(vm.stackref(nelems-1), nelems, base);
      vm.drop(nelems);
      vm._pc++;
      break;
    }
    case vm_set: {
      uint8_t n = read_uint8(vm._pc);
      DBG_VM("set %" PRIu8 "\n", n);
      vm.setreg(n, vm.pop());
      vm._pc++;
      break;
    }
    case vm_make_cell: {
      DBG_VM1("make-cell\n");
      T_sp car((gctools::Tagged)(vm.pop()));
      T_sp cdr((gctools::Tagged)nil<T_O>().raw_());
      vm.push(Cons_O::create(car, cdr).raw_());
      vm._pc++;
      break;
    }
    case vm_cell_ref: {
      DBG_VM1("cell-ref\n");
      T_sp cons((gctools::Tagged)vm.pop());
      vm.push(oCar(cons).raw_());
      vm._pc++;
      break;
    }
    case vm_cell_set: {
      DBG_VM("cell-set\n");
      T_sp cons((gctools::Tagged)vm.pop());
      Cons_sp ccons = gc::As<Cons_sp>(cons);
      T_O* val = vm.pop();
      T_sp tval((gctools::Tagged)val);
      ccons->rplaca(tval);
      vm._pc++;
      break;
    }
    case vm_make_closure: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM("make-closure %" PRIu8 "\n", c);
      T_sp fn_sp((gctools::Tagged)literals[c]);
      GlobalBytecodeEntryPoint_sp fn
        = gc::As<GlobalBytecodeEntryPoint_sp>(fn_sp);
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      Closure_sp closure
        = Closure_O::make_bytecode_closure(fn, nclosed);
      // FIXME: Can we use some more abstracted access?
      vm.copyto(nclosed, (T_O**)(closure->_Slots.data()));
      vm.drop(nclosed);
      vm.push(closure.raw_());
      vm._pc++;
      break;
    }
    case vm_make_uninitialized_closure: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM("make-uninitialized-closure %" PRIu8 "\n", c);
      T_sp fn_sp((gctools::Tagged)literals[c]);
      GlobalBytecodeEntryPoint_sp fn
        = gc::As<GlobalBytecodeEntryPoint_sp>(fn_sp);
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      Closure_sp closure
        = Closure_O::make_bytecode_closure(fn, nclosed);
      vm.push(closure.raw_());
      vm._pc++;
      break;
    }
    case vm_initialize_closure: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM("initialize-closure %" PRIu8 "\n", c);
      T_sp tclosure((gctools::Tagged)(*(vm.reg(c))));
      Closure_sp closure = gc::As<Closure_sp>(tclosure);
      // FIXME: We ought to be able to get the closure size directly
      // from the closure through some nice method.
      GlobalBytecodeEntryPoint_sp fn
        = gc::As<GlobalBytecodeEntryPoint_sp>(closure->entryPoint());
      size_t nclosed = fn->environmentSize();
      DBG_VM("  nclosed = %zu\n", nclosed);
      vm.copyto(nclosed, (T_O**)(closure->_Slots.data()));
      vm.drop(nclosed);
      vm._pc++;
      break;
    }
    case vm_return: {
      DBG_VM1("return\n");
#ifdef DBG_VM1
      if (vm.npushed(nlocals) != 0) {
        gctools::wait_for_user_signal(fmt::sprintf("vm_return - vm.npushed(nlocals) = %lu   nlocals = %lu", vm.npushed(nlocals), nlocals).c_str());
        SIMPLE_ERROR("In vm_return - vm.npushed(nlocals) = %lu   nlocals = %lu", vm.npushed(nlocals), nlocals);
      }
#endif
      core::MultipleValues &mv = core::lisp_multipleValues();
      size_t nvalues = mv.getSize();
      return gctools::return_type(mv.valueGet(0, nvalues).raw_(), nvalues);
    }
    case vm_bind_required_args: {
      uint8_t nargs = read_uint8(vm._pc);
      DBG_VM("bind-required-args %" PRIu8 "\n", nargs);
      vm.copytoreg(lcc_args, nargs, 0);
      vm._pc++;
      break;
    }
    case vm_bind_optional_args: {
      uint8_t nreq = read_uint8(vm._pc);
      uint8_t nopt = read_uint8(vm._pc);
      DBG_VM("bind-optional-args %" PRIu8 " %" PRIu8 "\n", nreq, nopt);
      if (lcc_nargs >= nreq + nopt) { // enough args- easy mode
        DBG_VM("  enough args\n");
        vm.copytoreg(lcc_args + nreq, nopt, nreq);
      } else { // put in some unbounds
        DBG_VM("  not enough args\n");
        vm.copytoreg(lcc_args + nreq, lcc_nargs - nreq, nreq);
        vm.fillreg(unbound<T_O>().raw_(), nreq + nopt - lcc_nargs, lcc_nargs);
      }
      vm._pc++;
      break;
    }
    case vm_listify_rest_args: {
      uint8_t start = read_uint8(vm._pc);
      DBG_VM("listify-rest-args %" PRIu8 "\n", start);
      ql::list rest;
      for (size_t i = start; i < lcc_nargs; ++i) {
        T_sp tobj((gctools::Tagged)lcc_args[i]);
        rest << tobj;
      }
      vm.push(rest.cons().raw_());
      vm._pc++;
      break;
    }
    case vm_parse_key_args: {
      uint8_t more_start = read_uint8(vm._pc);
      uint8_t key_count_info = read_uint8(vm._pc);
      uint8_t key_literal_start = read_uint8(vm._pc);
      uint8_t key_frame_start = read_uint8(vm._pc);
      DBG_VM("parse-key-args %" PRIu8 " %" PRIu8 " %" PRIu8 " %" PRIu8 "\n",
             more_start, key_count_info, key_literal_start, key_frame_start);
      uint8_t key_count = key_count_info & 0x7f;
      bool ll_aokp = key_count_info & 0x80;
      bool seen_aokp = false;
      bool aokp = false;
      bool unknown_key_p = false;
      T_O* unknown_key = unbound<T_O>().raw_();
      // Set keyword arguments to unbound.
      vm.fillreg(unbound<T_O>().raw_(), key_count, key_frame_start);
      if (lcc_nargs > more_start) {
        // FIXME: Check for odd keyword portion
        // KLUDGE: We use a signed type so that if more_start is zero we don't
        // wrap arg_index around. There's probably a cleverer solution.
        ptrdiff_t arg_index;
        for (arg_index = lcc_nargs - 1; arg_index >= more_start;
             arg_index -= 2) {
          bool valid_key_p = false;
          T_O* key = lcc_args[arg_index - 1];
          if (key == kw::_sym_allow_other_keys.raw_()) {
            valid_key_p = true; // aok is always valid.
            T_sp value((gctools::Tagged)(lcc_args[arg_index]));
            if (!seen_aokp) aokp = value.notnilp();
          }
          for (size_t key_id = 0; key_id < key_count; ++key_id) {
            T_O* ckey = literals[key_id + key_literal_start];
            if (key == ckey) {
              valid_key_p = true;
              *vm.reg(key_frame_start + key_id) = lcc_args[arg_index];
              break;
            }
          }
          if (!valid_key_p) {
            if (!unknown_key_p) unknown_key = key;
            unknown_key_p = true;
          }
        }
      }
      if (unknown_key_p && !aokp && !ll_aokp) {
        T_sp tclosure((gctools::Tagged)gctools::tag_general(closure));
        T_sp tunknown((gctools::Tagged)unknown_key);
        throwUnrecognizedKeywordArgumentError(tclosure, tunknown);
      }
      vm._pc++;
      break;
    }
    case vm_jump_8: {
      int8_t rel = *(vm._pc + 1);
      DBG_VM1("jump %" PRId8 "\n", rel);
      vm._pc += rel;
      break;
    }
    case vm_jump_16: {
      int32_t rel = read_label(vm._pc, 2);
      DBG_VM("jump %" PRId32 "\n", rel);
      vm._pc += rel;
      break;
    }
    case vm_jump_24: {
      int32_t rel = read_label(vm._pc, 3);
      DBG_VM("jump %" PRId32 "\n", rel);
      vm._pc += rel;
      break;
    }
    case vm_jump_if_8: {
      int8_t rel = *(vm._pc + 1);
      DBG_VM1("jump-if %" PRId8 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop());
      VM_RECORD_PLAYBACK(tval.raw_(),"vm_jump_if_8");
      if (tval.notnilp()) vm._pc += rel;
      else vm._pc += 2;
      break;
    }
    case vm_jump_if_16: {
      int32_t rel = read_label(vm._pc, 2);
      DBG_VM("jump-if %" PRId32 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop());
      if (tval.notnilp()) vm._pc += rel;
      else vm._pc += 3;
      break;
    }
    case vm_jump_if_24: {
      int32_t rel = read_label(vm._pc, 3);
      DBG_VM("jump-if %" PRId32 "\n", rel);
      T_sp tval((gctools::Tagged)vm.pop());
      if (tval.notnilp()) vm._pc += rel;
      else vm._pc += 4;
      break;
    }
    case vm_jump_if_supplied_8: {
      uint8_t slot = *(vm._pc + 1);
      int32_t rel = *(vm._pc + 2);
      DBG_VM("jump-if-supplied %" PRIu8 " %" PRId8 "\n", slot, rel);
      T_sp tval((gctools::Tagged)(*(vm.reg(slot))));
      if (tval.unboundp()) vm._pc += 3;
      else vm._pc += rel;
      break;
    }
    case vm_jump_if_supplied_16: {
      uint8_t slot = read_uint8(vm._pc);
      int32_t rel = read_label(vm._pc, 2);
      DBG_VM("jump-if-supplied %" PRIu8 " %" PRId32 "\n", slot, rel);
      T_sp tval((gctools::Tagged)(*(vm.reg(slot))));
      if (tval.unboundp()) vm._pc += 4;
      else vm._pc += rel - 1;
      break;
    }
    case vm_check_arg_count_LE_: {
      uint8_t max_nargs = read_uint8(vm._pc);
      DBG_VM("check-arg-count<= %" PRIu8 "\n", max_nargs);
      if (lcc_nargs > max_nargs) {
        T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
        throwTooManyArgumentsError(tclosure, lcc_nargs, max_nargs);
      }
      vm._pc++;
      break;
    }
    case vm_check_arg_count_GE_: {
      uint8_t min_nargs = read_uint8(vm._pc);
      DBG_VM("check-arg-count>= %" PRIu8 "\n", min_nargs);
      if (lcc_nargs < min_nargs) {
        T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
        throwTooFewArgumentsError(tclosure, lcc_nargs, min_nargs);
      }
      vm._pc++;
      break;
    }
    case vm_check_arg_count_EQ_: {
      uint8_t req_nargs = read_uint8(vm._pc);
      DBG_VM1("check-arg-count= %" PRIu8 "\n", req_nargs);
      if (lcc_nargs != req_nargs) {
        T_sp tclosure((gctools::Tagged)(gctools::tag_general(closure)));
        wrongNumberOfArguments(tclosure, lcc_nargs, req_nargs);
      }
      vm._pc++;
      break;
    }
    case vm_push_values: {
      // TODO: Direct copy?
      DBG_VM("push-values\n");
      MultipleValues &mv = lisp_multipleValues();
      size_t nvalues = mv.getSize();
      DBG_VM("  nvalues = %zu\n", nvalues);
      for (size_t i = 0; i < nvalues; ++i) vm.push(mv.valueGet(i, nvalues).raw_());
      // We could skip tagging this, but that's error-prone.
      vm.push(make_fixnum(nvalues).raw_());
      vm._pc++;
      break;
    }
    case vm_append_values: {
      DBG_VM("append-values\n");
      T_sp texisting_values((gctools::Tagged)vm.pop());
      size_t existing_values = texisting_values.unsafe_fixnum();
      DBG_VM("  existing-values = %zu\n", existing_values);
      MultipleValues &mv = lisp_multipleValues();
      size_t nvalues = mv.getSize();
      DBG_VM("  nvalues = %zu\n", nvalues);
      for (size_t i = 0; i < nvalues; ++i) vm.push(mv.valueGet(i, nvalues).raw_());
      vm.push(make_fixnum(nvalues + existing_values).raw_());
      vm._pc++;
      break;
    }
    case vm_pop_values: {
      DBG_VM("pop-values\n");
      MultipleValues &mv = lisp_multipleValues();
      T_sp texisting_values((gctools::Tagged)vm.pop());
      size_t existing_values = texisting_values.unsafe_fixnum();
      DBG_VM("  existing-values = %zu\n", existing_values);
      vm.copyto(existing_values, &my_thread->_MultipleValues._Values[0]);
      mv.setSize(existing_values);
      vm.drop(existing_values);
      vm._pc++;
      break;
    }
    case vm_mv_call: {
      DBG_VM("mv-call\n");
      T_O* func = vm.pop();
      size_t nargs = lisp_multipleValues().getSize();
      T_O* args[nargs];
      multipleValuesSaveToTemp(nargs, args);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      res.saveToMultipleValue0();
      vm._pc++;
      break;
    }
    case vm_mv_call_receive_one: {
      DBG_VM("mv-call-receive-one\n");
      T_O* func = vm.pop();
      size_t nargs = lisp_multipleValues().getSize();
      T_O* args[nargs];
      multipleValuesSaveToTemp(nargs, args);
      T_sp res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      vm.push(res.raw_());
      vm._pc++;
      break;
    }
    case vm_mv_call_receive_fixed: {
      uint8_t nvals = read_uint8(vm._pc);
      DBG_VM("mv-call-receive-fixed %" PRIu8 "\n", nvals);
      T_O* func = vm.pop();
      MultipleValues& mv = lisp_multipleValues();
      size_t nargs = mv.getSize();
      T_O* args[nargs];
      multipleValuesSaveToTemp(nargs, args);
      T_mv res = funcall_general<core::Function_O>((gc::Tagged)func, nargs, args);
      if (nvals != 0) {
        vm.push(res.raw_()); // primary
        size_t svalues = mv.getSize();
        for (size_t i = 1; i < svalues; ++i)
          vm.push(mv.valueGet(i, svalues).raw_());
      }
      vm._pc++;
      break;
    }
    case vm_entry: {
      uint8_t n = read_uint8(vm._pc);
      DBG_VM("entry %" PRIu8 "\n", n);
      VirtualMachineStackState vmss = vm.save();
      vm._pc++;
      jmp_buf target;
      void* frame = __builtin_frame_address(0);
      TagbodyDynEnv_sp env = TagbodyDynEnv_O::create(frame, &target);
      vm.setreg(n, env.raw_());
      gctools::StackAllocate<Cons_O> sa_ec(env, my_thread->dynEnvStackGet());
      DynEnvPusher dep(my_thread, sa_ec.asSmartPtr());
      if (setjmp(target)) vm.load(vmss);
      again:
      try { bytecode_vm(vm, literals, nlocals, closure, lcc_nargs, lcc_args); }
      catch (Unwind &uw) {
        if (uw.getFrame() == frame) {
          my_thread->dynEnvStackGet() = sa_ec.asSmartPtr();
          goto again;
        }
        else throw;
      }
      break;
    }
    case vm_exit_8: {
      int8_t rel = *(vm._pc + 1);
      DBG_VM("exit %" PRId8 "\n", rel);
      vm._pc += rel;
      T_sp ttde((gctools::Tagged)(vm.pop()));
      TagbodyDynEnv_sp tde = gc::As<TagbodyDynEnv_sp>(ttde);
      sjlj_unwind(tde, 1);
    }
    case vm_exit_16: {
      int32_t rel = read_label(vm._pc, 2);
      DBG_VM("exit %" PRId32 "\n", rel);
      vm._pc += rel;
      T_sp ttde((gctools::Tagged)(vm.pop()));
      TagbodyDynEnv_sp tde = gc::As<TagbodyDynEnv_sp>(ttde);
      sjlj_unwind(tde, 1);
    }
    case vm_exit_24: {
      int32_t rel = read_label(vm._pc, 3);
      DBG_VM("exit %" PRId32 "\n", rel);
      vm._pc += rel;
      T_sp ttde((gctools::Tagged)(vm.pop()));
      TagbodyDynEnv_sp tde = gc::As<TagbodyDynEnv_sp>(ttde);
      sjlj_unwind(tde, 1);
    }
    case vm_entry_close: {
      DBG_VM("entry-close\n");
      // This sham return value just gets us out of the bytecode_vm call in
      // vm_entry, above.
      vm._pc++;
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    case vm_special_bind: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM("special-bind %" PRIu8 "\n", c);
      T_sp value((gctools::Tagged)(vm.pop()));
      vm._pc++;
      T_sp symbol((gctools::Tagged)literals[c]);
      call_with_variable_bound(symbol, value,
                               [&]() { return bytecode_vm(vm, literals,
                                                          nlocals, closure,
                                                          lcc_nargs, lcc_args);
                               });
      break;
    }
    case vm_symbol_value: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM("symbol-value %" PRIu8 "\n", c);
      T_sp sym_sp((gctools::Tagged)literals[c]);
      Symbol_sp sym = gc::As<Symbol_sp>(sym_sp);
      vm.push(sym->symbolValue().raw_());
      vm._pc++;
      break;
    }
    case vm_symbol_value_set: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM("symbol-value-set %" PRIu8 "\n", c);
      T_sp sym_sp((gctools::Tagged)literals[c]);
      Symbol_sp sym = gc::As<Symbol_sp>(sym_sp);
      T_sp value((gctools::Tagged)(vm.pop()));
      sym->setf_symbolValue(value);
      vm._pc++;
      break;
    }
    case vm_unbind: {
      DBG_VM("unbind\n");
      vm._pc++;
      // This return value is not actually used - we're just returning from
      // a bytecode_vm recursively invoked by vm_special_bind above.
      return gctools::return_type(nil<T_O>().raw_(), 0);
    }
    case vm_fdefinition: {
      uint8_t c = read_uint8(vm._pc);
      DBG_VM1("fdefinition %" PRIu8 "\n", c);
      T_sp sym((gctools::Tagged)literals[c]);
      vm.push(cl__fdefinition(sym).raw_());
      vm._pc++;
      break;
    }
    case vm_nil:
        DBG_VM("nil\n");
        vm.push(nil<T_O>().raw_());
        vm._pc++;
        break;
    case vm_push: {
      DBG_VM1("push\n");
      MultipleValues &mv = lisp_multipleValues();
      vm.push(mv.valueGet(0, mv.getSize()).raw_());
      vm._pc++;
      break;
    }
    case vm_pop:{
      DBG_VM1("pop\n");
      T_sp obj((gctools::Tagged)vm.pop());
      core::MultipleValues &mv = core::lisp_multipleValues();
      mv.setSize(1);
      mv.valueSet(0, obj);
      vm._pc++;
      break;
    }
    case vm_long: {
      // In a separate function to facilitate better icache utilization
      // by bytecode_vm (hopefully)
      bytecode_vm_long(vm, literals, nlocals, closure, *++vm._pc);
      break;
    }
    default:
        EntryPoint_sp ep = closure->entryPoint();
        BytecodeModule_sp bcm = gc::As<GlobalBytecodeEntryPoint_sp>(ep)->code();
        unsigned char* codeStart = (unsigned char*)gc::As<Array_sp>(bcm->_Bytecode)->rowMajorAddressOfElement_(0);
        unsigned char* codeEnd = codeStart + gc::As<Array_sp>(bcm->_Bytecode)->arrayTotalSize();
        SIMPLE_ERROR("Unknown opcode %hu pc: %p  module: %p - %p", *vm._pc, vm._pc, codeStart, codeEnd );
    };
  }
}

gctools::return_type bytecode_call(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args)
{
  Closure_O* closure = gctools::untag_general<Closure_O*>((Closure_O*)lcc_closure);
  core::GlobalBytecodeEntryPoint_sp entryPoint = gctools::As_unsafe<core::GlobalBytecodeEntryPoint_sp>(closure->_EntryPoint.load());
  //DBG_printf("%s:%d:%s This is where we evaluate bytecode functions pc: %p\n", __FILE__, __LINE__, __FUNCTION__, pc );
  size_t nlocals = entryPoint->localsFrameSize();
  BytecodeModule_sp module = entryPoint->code();
  T_O** literals = ((T_O**)gc::As<SimpleVector_sp>(module->literals())->
                    rowMajorAddressOfElement_(0));

  VirtualMachine& vm = my_thread->_VM;
  // We save the old PC for returns. We do _not_ do this for nonlocal exits,
  // since in that case the NLXing VM invocation sets the PC before escaping.
  unsigned char* old_pc = vm._pc;
  vm._pc = pc;
  // The frame itself we do even if we do exit.
  T_O** old_fp = vm.push_frame(nlocals);
  try {
    T_mv res = funwind_protect([&] {
      gctools::return_type res = bytecode_vm(vm, literals, nlocals, closure, lcc_nargs, lcc_args);
      vm._pc = old_pc;
      return T_mv(res);
    },
      [&] { vm.pop_frame(old_fp); });
    return res.as_return_type();
  } catch (VM_error& err) {
    printf("%s:%d:%s Recovering from VM_error\n", __FILE__, __LINE__, __FUNCTION__ );
    return gctools::return_type(nil<T_O>().raw_(), 0 );
  }
}


CL_DEFUN Integer_sp core__side_stack_pointer() {
  return Integer_O::create((uint64_t) my_thread->_VM._stackPointer);
}

#if DEBUG_VM_RECORD_PLAYBACK==1
CL_DEFUN void core__vm_counter_step(size_t counterStep) {
  global_counterStep = counterStep;
}

CL_DEFUN void core__vm_stack_trigger(size_t trigger) {
  global_stackTrigger = trigger;
}
#endif

}; // namespace core
