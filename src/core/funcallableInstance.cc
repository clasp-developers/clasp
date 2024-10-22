/*
    File: instance.cc
*/

/*
Copyright (c) 2014, Christian E. Schafmeister

CLASP is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

See directory 'clasp/licenses' for full details.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/* -^- */
// #define DEBUG_LEVEL_FULL

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/symbolTable.h>
#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/lispList.h>
#include <clasp/core/debugger.h>
#include <clasp/core/wrappedPointer.h>
#include <clasp/core/derivableCxxObject.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/primitives.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/wrappers.h>
#include <tuple>

namespace core {
#ifdef DEBUG_DTREE_INTERPRETER // for debugging
#define DTILOG(...)                                                                                                                \
  {                                                                                                                                \
    fprintf(DTILOG_fout, "%5lu: %s", my_thread->_DtreeInterpreterCallCount, (fmt::format(__VA_ARGS__).c_str()));                   \
    fflush(DTILOG_fout);                                                                                                           \
  }
#define DTIDO(x)                                                                                                                   \
  do {                                                                                                                             \
    x;                                                                                                                             \
  } while (0)
#define DTIDO_ALWAYS(x) x
#else
#define DTILOG(...)
#define DTIDO(x)                                                                                                                   \
  do {                                                                                                                             \
  } while (0)
#define DTIDO_ALWAYS(x)
#endif
}; // namespace core

namespace core {

void FuncallableInstance_O::initializeSlots(gctools::BaseHeader_s::StampWtagMtag stamp, T_sp sig, size_t numberOfSlots) {
  //  ASSERT(gctools::BaseHeader_s::StampWtagMtag::is_rack_shifted_stamp((uint64_t)stamp));
  this->_Rack = Rack_O::make(numberOfSlots, sig, unbound<T_O>());
  this->stamp_set(stamp.as_fixnum());
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
}

#if 0
void FuncallableInstance_O::initializeClassSlots(Creator_sp creator, gctools::BaseHeader_s::StampWtagMtag stamp) {
  ASSERT(gctools::Header_s::BaseHeader_s::StampWtagMtag::is_rack_shifted_stamp((uint64_t)stamp));
  DEPRECATED();
}
#endif

// FIXME: Exists solely for cases where the list of slotds is hard to get.
CL_LAMBDA(class slot-count);
DOCGROUP(clasp);
CL_DEFUN T_sp core__allocate_funcallable_standard_instance(Instance_sp cl, size_t slot_count) {
  SimpleFun_sp entryPoint =
      makeSimpleFunAndFunctionDescription<FuncallableInstance_O>(cl::_sym_lambda);
  auto obj = gctools::GC<FuncallableInstance_O>::allocate(entryPoint);
  obj->_Class = cl;
  obj->initializeSlots(cl->CLASS_stamp_for_instances(), cl->slots(), slot_count);
  return obj;
}

DOCGROUP(clasp);
CL_DEFUN FuncallableInstance_sp core__allocate_raw_funcallable_instance(Instance_sp cl, Rack_sp rack) {
  SimpleFun_sp entryPoint =
      makeSimpleFunAndFunctionDescription<FuncallableInstance_O>(cl::_sym_lambda);
  auto obj = gctools::GC<FuncallableInstance_O>::allocate(entryPoint, cl, rack);
  return obj;
}

size_t FuncallableInstance_O::rack_stamp_offset() {
  SimpleVector_O dummy_rack(0, nil<T_O>(), false);
  return (char*)&(dummy_rack.operator[](0)) - (char*)&dummy_rack;
}

Fixnum FuncallableInstance_O::stamp() const { return rack()->stamp_get(); };

void FuncallableInstance_O::stamp_set(Fixnum s) { rack()->stamp_set(s); };

size_t FuncallableInstance_O::numberOfSlots() const { return rack()->length(); };

T_sp FuncallableInstance_O::instanceSig() const {
#if DEBUG_CLOS >= 2
  stringstream ssig;
  if (rack()->_Sig) {
    ssig << rack()->_Sig->__repr__();
  } else {
    ssig << "UNDEFINED ";
  }
  printf("\nMLOG INSTANCE-SIG of Instance %p \n", (void*)(this));
#endif
  return ((rack()->_Sig));
}

SYMBOL_EXPORT_SC_(ClosPkg, setFuncallableInstanceFunction);
SYMBOL_EXPORT_SC_(CorePkg, instanceClassSet);

T_sp FuncallableInstance_O::instanceClassSet(Instance_sp mc) {
  this->_Class = mc;
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
  return (this->sharedThis<FuncallableInstance_O>());
}

T_sp FuncallableInstance_O::instanceRef(size_t idx) const {
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
#if DEBUG_CLOS >= 2
  printf("\nMLOG INSTANCE-REF[%d] of Instance %p --->%s\n", idx, (void*)(this),
         low_level_instanceRef(rack(), idx)->__repr__().c_str());
#endif
  return low_level_instanceRef(rack(), idx);
}
T_sp FuncallableInstance_O::instanceSet(size_t idx, T_sp val) {
#if DEBUG_CLOS >= 2
  printf("\nMLOG SI-INSTANCE-SET[%d] of Instance %p to val: %s\n", idx, (void*)(this), val->__repr__().c_str());
#endif
  low_level_instanceSet(rack(), idx, val);
#ifdef DEBUG_GUARD_VALIDATE
  client_validate(rack());
#endif
  return val;
}

// Get the name of a generic function without calling any generic functions
// (e.g., generic-function-name). Nice for debugging CLOS.
DOCGROUP(clasp);
CL_DEFUN T_sp core__low_level_standard_generic_function_name(FuncallableInstance_sp gfun) { return gfun->functionName(); }

string FuncallableInstance_O::__repr__() const {
  stringstream ss;
  ss << "#<";
  if (Instance_sp mc = this->_Class.asOrNull<Instance_O>()) {
    ss << mc->_classNameAsString();
  } else {
    ss << "<ADD SUPPORT FOR INSTANCE _CLASS=" << _rep_(this->_Class);
  }
  ss << " " << _rep_(this->functionName()) << ">";
  return ss.str();
}

SYMBOL_EXPORT_SC_(ClPkg, standardGenericFunction);
T_sp FuncallableInstance_O::setFuncallableInstanceFunction(T_sp function) {
  this->REAL_FUNCTION_set(gc::As<Function_sp>(function));
  return ((this->sharedThis<FuncallableInstance_O>()));
}

void FuncallableInstance_O::describe(T_sp stream) {
  stringstream ss;
  ss << (fmt::format("FuncallableInstance\n"));
  ss << (fmt::format("_Class: {}\n", _rep_(this->_Class)));
  for (int i(1); i < rack()->length(); ++i) {
    ss << fmt::format("_Rack[{}]: {}\n", i, _rep_(low_level_instanceRef(rack(), i)));
  }
  clasp_write_string(ss.str(), stream);
}

// FIXME: Don't export this.
DOCGROUP(clasp);
CL_DEFUN Function_sp clos__getFuncallableInstanceFunction(FuncallableInstance_sp obj) { return obj->REAL_FUNCTION(); };

DOCGROUP(clasp);
CL_DEFUN T_sp clos__setFuncallableInstanceFunction(T_sp obj, T_sp func) {
  if (FuncallableInstance_sp iobj = obj.asOrNull<FuncallableInstance_O>()) {
    return iobj->setFuncallableInstanceFunction(func);
  }
  SIMPLE_ERROR("You can only setFuncallableInstanceFunction on funcallable instances - you tried to set it on a: {}", _rep_(obj));
};

}; // namespace core

namespace core {

DOCGROUP(clasp);
CL_DEFUN size_t clos__generic_function_interpreted_calls(FuncallableInstance_sp gf) { return gf->interpreted_calls(); }

DOCGROUP(clasp);
CL_DEFUN T_sp clos__generic_function_compiled_dispatch_function(T_sp obj) {
  return gc::As<FuncallableInstance_sp>(obj)->REAL_FUNCTION();
}
DOCGROUP(clasp);
CL_DEFUN void clos__set_generic_function_compiled_dispatch_function(T_sp obj, T_sp val) {
  gc::As<FuncallableInstance_sp>(obj)->REAL_FUNCTION_set(gc::As<Function_sp>(val));
}
} // namespace core

#define READ_RACK_STAMP
#define READ_WRAPPED_STAMP
#define READ_RACK_STAMP
#define READ_DERIVED_STAMP
#define READ_GENERAL_STAMP
#include <clasp/llvmo/read-stamp.cc>
#undef READ_WRAPPED_STAMP
#undef READ_RACK_STAMP
#undef READ_DERIVED_STAMP
#undef READ_GENERAL_STAMP
#undef READ_RACK_STAMP

namespace core {

#define GF_BYTECODE_VM
#include <virtualMachine.h>
#undef GF_BYTECODE_VM

SYMBOL_EXPORT_SC_(CorePkg, STARdtreeSymbolsSTAR);

void registerOneDtreeInfo(const std::string& name, int val) {
  printf("%s:%d:%s  name: %s   val: %d\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), val);
  if (_sym_STARdtreeSymbolsSTAR->symbolValue().nilp()) {
    _sym_STARdtreeSymbolsSTAR->defparameter(HashTableEq_O::create_default());
  }
  HashTableEq_sp ht = gc::As<HashTableEq_sp>(_sym_STARdtreeSymbolsSTAR->symbolValue());
  Symbol_sp key = lisp_upcase_intern(name, "KEYWORD");
  ht->setf_gethash(key, make_fixnum(val));
}

std::string dtree_op_name(int dtree_op) {
  switch (dtree_op) {
#define GF_BYTECODE_VM_NAMES
#include <virtualMachine.h>
#undef GF_BYTECODE_VM_NAMES
  default:
    return "UNKNOWN_OP";
  };
};

SYMBOL_EXPORT_SC_(ClosPkg, interp_wrong_nargs);
SYMBOL_EXPORT_SC_(ClosPkg, compile_discriminating_function);

#define COMPILE_TRIGGER ((size_t)1024)
size_t global_compile_discriminating_function_trigger = COMPILE_TRIGGER;

//

template <int Size> struct ReadArg;

template <> struct ReadArg<1> {
  inline static size_t read(unsigned char* addr, uintptr_t offset) {
    unsigned char val = *(addr + offset);
    return val;
  }
  inline static T_sp read_literal(unsigned char* addr, uintptr_t offset, T_sp* literals) {
    size_t index = read(addr, offset);
    return literals[index];
  }
  inline static uintptr_t read_literal_tagged(unsigned char* addr, uintptr_t offset, T_sp* literals) {
    size_t index = read(addr, offset);
    return literals[index].tagged_();
  }
  inline static uintptr_t offset(uintptr_t offset) { return 1 + (offset - 1); }
};

template <> struct ReadArg<2> {
  inline static size_t read(unsigned char* addr, uintptr_t offset) {
    unsigned char low = *(addr + 1 + 2 * (offset - 1));
    unsigned char high = *(addr + 1 + 2 * (offset - 1) + 1);
#if 0
    size_t val = (high << 8) + low;
    printf("%s:%d:%s read low %u  high %u  val = %lu\n", __FILE__, __LINE__, __FUNCTION__, low, high, val );
#endif
    return (high << 8) + low;
  }
  inline static T_sp read_literal(unsigned char* addr, uintptr_t offset, T_sp* literals) {
    size_t index = read(addr, offset);
    return literals[index];
  }
  inline static uintptr_t read_literal_tagged(unsigned char* addr, uintptr_t offset, T_sp* literals) {
    size_t index = read(addr, offset);
    return literals[index].tagged_();
  }
  inline static uintptr_t offset(uintptr_t offset) { return 1 + (offset - 1) * 2; }
};

CL_DEFUN void clos__validate_dtree_bytecode_vm(size_t opcount) {
  if (opcount != DTREE_OP_COUNT) {
    SIMPLE_ERROR("opcount %zu does not match DTREE_OP_COUNT %zu", opcount, DTREE_OP_COUNT);
  }
}

}; // namespace core

namespace core {

void prepare_vm(core::T_O* lcc_closure, GFBytecodeSimpleFun_sp& gfep, SimpleVector_byte8_t_sp& program,
                SimpleVector_sp& literal_vec, T_sp*& literals, unsigned char*& ip0) {
  gfep = GFBytecodeSimpleFun_sp((gctools::Tagged)lcc_closure);
  program = gc::As_assert<SimpleVector_byte8_t_sp>(gfep->_Bytecode);
  literal_vec = gfep->_Literals;
  literals = (T_sp*)&literal_vec->_Data[0];
  ip0 = (unsigned char*)&program->_Data[0];
}

#define DTI_DUMP_PROGRAM()                                                                                                         \
  DO_DRAG_INTERPRET_DTREE();                                                                                                       \
  DTILOG("---- program length: %d\n", program->length());                                                                          \
  DTIDO(                                                                                                                           \
      for (size_t i = 0; i < program->length(); ++i) {                                                                             \
        DTILOG("[%lu @%p] : %5u \n", i, (void*)(i + ip), (*program)[i]);                                                           \
      } for (size_t i = 0; i < literal_vec->length(); ++i) { DTILOG("literal[%lu] : %s \n", i, _safe_rep_((*literal_vec)[i])); });

[[noreturn]] void wrongNumberOfArgumentsForGenericFunction(T_O* lcc_closure, size_t nargs) {
  Function_sp closure((gctools::Tagged)lcc_closure);
  GFBytecodeSimpleFun_sp gfbsf = gc::As_assert<GFBytecodeSimpleFun_sp>(closure->entryPoint());
  int required_args = gfbsf->specializedLength();
  wrongNumberOfArguments(closure, nargs, required_args);
  UNREACHABLE();
}

struct GFBytecodeEntryPoint {
  static inline LCC_RETURN entry_point_n(T_O* lcc_closure, size_t lcc_nargs, T_O** lcc_args) {
    // this define controls dtree-interpreter.cc.
#define GENERAL_ENTRY
    T_sp gfunction((gctools::Tagged)lcc_closure);
    DTIDO_ALWAYS(FILE* DTILOG_fout = monitor_file("dtree-interp"); my_thread->_DtreeInterpreterCallCount++;);
    DTILOG("=============================== Entered clos__interpret_dtree_program\n");
    DTILOG("---- generic function: %s\n", _safe_rep_(gfunction));
    DTILOG("About to dump incoming arguments\n");
    DTIDO(dump_lcc_args(monitor_file("dtree-interp"), lcc_nargs, lcc_args));
    GFBytecodeSimpleFun_sp gfep;
    SimpleVector_byte8_t_sp program;
    SimpleVector_sp literal_vec;
    T_sp* literals;
    unsigned char* ip0;
    prepare_vm(lcc_closure, gfep, program, literal_vec, literals, ip0);
    // Check argcount, so that we don't try to read invalid arguments.
    if (lcc_nargs < gfep->specializedLength())
      wrongNumberOfArgumentsForGenericFunction(lcc_closure, lcc_nargs);
    unsigned char* ip = ip0;
    DTI_DUMP_PROGRAM();
    T_sp arg;
    uintptr_t stamp;
    while (1) {
      unsigned char op = *ip;
      DTILOG("ip[%lu @%p]: %u/%s\n", (uintptr_t)(ip - ip0), (void*)ip, op, dtree_op_name(op));
      switch (op) {
#define MAYBE_LONG_MUL 1
#define MAYBE_LONG_ADD 0
#include "src/core/dtree-interpreter.cc"
#undef MAYBE_LONG_MUL
#define MAYBE_LONG_MUL 2
#define MAYBE_LONG_ADD DTREE_OP_COUNT
#include "src/core/dtree-interpreter.cc"
#undef MAYBE_LONG_MUL
      default:
        printf("%s:%d:%s Invalid dtree opcode ip0: %p  ip: %p  opcode %u %s\n", __FILE__, __LINE__, __FUNCTION__, ip0, ip, op,
               dtree_op_name(op).c_str());
        SIMPLE_ERROR("%zu/{} is not a valid dtree opcode", op, dtree_op_name(op).c_str());
        break;
      } /*switch*/
    }   /* while(1) */
  DISPATCH_MISS : {
    DTILOG("dispatch miss. arg %lu stamp %lu\n", arg, stamp);
    Vaslist vaslist(lcc_nargs, lcc_args);
    Vaslist_sp error_args(&vaslist);
    Function_sp generic_function = gfep->_GenericFunction;
    return core::eval::funcall(clos::_sym_dispatch_miss_va, generic_function, error_args);
  }
  SINGLE_DISPATCH_MISS : {
    DTILOG("dispatch miss. arg %lu stamp %lu\n", arg, stamp);
    Vaslist vaslist(lcc_nargs, lcc_args);
    Vaslist_sp error_args(&vaslist);
    Function_sp generic_function = gfep->_GenericFunction;
    return core::eval::funcall(clos::_sym_single_dispatch_miss_va, generic_function, error_args);
  }
#undef GENERAL_ENTRY
  }

private:
  template <size_t M, typename... Ts>
  static inline T_O* fargn_impl(T_O* lcc_closure, size_t n,
                                const std::tuple<Ts...>& args) {
    if constexpr(M < sizeof...(Ts)) {
      if (n == M)
        return std::get<M>(args);
      else
        return fargn_impl<M + 1>(lcc_closure, n, args);
    } else {
      wrongNumberOfArgumentsForGenericFunction(lcc_closure, sizeof...(Ts));
    }
  }
  // Get the nth argument, for runtime n.
  // Used in DTREE_OP_ARGN for fixed arity functions.
  // After inlining, this should look like
  // if (n == 0) return std::get<0>(args); else if (n == 1) ...
  // else wrongNumberOfArgumentsForGenericFunction(...) hopefully.
  // inline spec is probably pointless but makes me feel better.
  template <typename... Ts>
  static inline T_O* fargn(T_O* lcc_closure, size_t n,
                           const std::tuple<Ts...>& args) {
    return fargn_impl<0>(lcc_closure, n, args);
  }

public:
  template <typename... Ts>
  [[noreturn]] static inline LCC_RETURN error_entry_point_fixed(core::T_O* lcc_closure, Ts... args) {
    wrongNumberOfArgumentsForGenericFunction(lcc_closure, sizeof...(Ts));
  }
  template <typename... Ts>
  static inline LCC_RETURN entry_point_fixed(core::T_O* lcc_closure, Ts... lcc_args) {
    constexpr size_t fixed_nargs = sizeof...(Ts);
    const std::tuple<Ts...> args = std::make_tuple(lcc_args...);
    T_sp gfunction((gctools::Tagged)lcc_closure);
    DTIDO_ALWAYS(FILE* DTILOG_fout = monitor_file("dtree-interp"); my_thread->_DtreeInterpreterCallCount++;);
    DTILOG("===== %s ========================== Entered clos__interpret_dtree_program\n", __FUNCTION__);
    DTILOG("---- generic function: %s\n", _safe_rep_(gfunction));
    GFBytecodeSimpleFun_sp gfep;
    SimpleVector_byte8_t_sp program;
    SimpleVector_sp literal_vec;
    T_sp* literals;
    unsigned char* ip0;
    prepare_vm(lcc_closure, gfep, program, literal_vec, literals, ip0);
    unsigned char* ip = ip0;
    DTI_DUMP_PROGRAM();
    T_sp arg;
    uintptr_t stamp;
    while (1) {
      unsigned char op = *ip;
      DTILOG("ip[%lu @%p]: %u/%s\n", (uintptr_t)(ip - ip0), (void*)ip, op, dtree_op_name(op));
      switch (op) {
#define MAYBE_LONG_MUL 1
#define MAYBE_LONG_ADD 0
#include "src/core/dtree-interpreter.cc"
#undef MAYBE_LONG_MUL
#define MAYBE_LONG_MUL 2
#define MAYBE_LONG_ADD DTREE_OP_COUNT
#include "src/core/dtree-interpreter.cc"
#undef MAYBE_LONG_MUL
      default:
        printf("%s:%d:%s Invalid dtree opcode ip0: %p  ip: %p  opcode %u %s\n", __FILE__, __LINE__, __FUNCTION__, ip0, ip, op,
               dtree_op_name(op).c_str());
        SIMPLE_ERROR("%zu/{} is not a valid dtree opcode", op, dtree_op_name(op).c_str());
        break;
      } /*switch*/
    }   /* while(1) */
  DISPATCH_MISS : {
      return clos::_sym_dispatch_miss->symbolFunction()->funcall_raw(gfep->_GenericFunction.raw_(), lcc_args...);
  }
  SINGLE_DISPATCH_MISS : {
      return clos::_sym_single_dispatch_miss->symbolFunction()->funcall_raw(gfep->_GenericFunction.raw_(), lcc_args...);
  }
  }
};

GFBytecodeSimpleFun_O::GFBytecodeSimpleFun_O(FunctionDescription_sp fdesc, unsigned int entryPcN, SimpleVector_byte8_t_sp bytecode,
                                             SimpleVector_sp literals, Function_sp generic_function, size_t specialized_length)
  : SimpleFun_O(fdesc, nil<T_O>(), XepStereotype<GFBytecodeEntryPoint>(specialized_length)),
      _EntryPcN(entryPcN), _Bytecode(bytecode), _Literals(literals), _GenericFunction(generic_function),
      _SpecializedLength(specialized_length){};

SYMBOL_EXPORT_SC_(ClosPkg, bytecode_dtree_compile);
CL_LISPIFY_NAME(GFBytecodeSimpleFun/make);
CL_DEF_CLASS_METHOD
GFBytecodeSimpleFun_sp GFBytecodeSimpleFun_O::make(Function_sp generic_function) {
  T_sp name = generic_function->functionName();
  FunctionDescription_sp fdesc = makeFunctionDescription(name);
  T_mv compiled = eval::funcall(clos::_sym_bytecode_dtree_compile, generic_function);
  SimpleVector_byte8_t_sp bytecode = gc::As<SimpleVector_byte8_t_sp>(compiled);
  MultipleValues& mv = my_thread->_MultipleValues;
  // SimpleVector_sp entryPoints = mv.second(compiled.number_of_values());
  SimpleVector_sp literals = gc::As<SimpleVector_sp>(mv.third(compiled.number_of_values()));
  size_t specialized_length = mv.fourth(compiled.number_of_values()).unsafe_fixnum();
  auto obj = gctools::GC<GFBytecodeSimpleFun_O>::allocate(fdesc, 0, bytecode, literals, generic_function, specialized_length);
  return obj;
}

Pointer_sp GFBytecodeSimpleFun_O::defaultEntryAddress() const { return Pointer_O::create((void*)this->_EntryPoints[0]); };

std::string GFBytecodeSimpleFun_O::__repr__() const {
  stringstream ss;
  ss << "#<GF-BYTECODE-ENTRY-POINT ";
  for (size_t ii = 0; ii < NUMBER_OF_ENTRY_POINTS; ii++) {
    if (ii == 0) {
      ss << "xep@";
    } else {
      ss << "xep" << (ii - 1) << "@";
    }
    ss << (void*)this->_EntryPoints[ii] << " ";
  }
  ss << " @" << (void*)this << ">";
  return ss.str();
}

void GFBytecodeSimpleFun_O::fixupInternalsForSnapshotSaveLoad(snapshotSaveLoad::Fixup* fixup) {
  this->fixupOneCodePointer(fixup, (void**)&this->_Trampoline);
  this->Base::fixupInternalsForSnapshotSaveLoad(fixup);
}

CL_DEFMETHOD size_t GFBytecodeSimpleFun_O::entryPcN() const { return this->_EntryPcN; }

}; // namespace core

namespace core {

SYMBOL_EXPORT_SC_(KeywordPkg, force_compile);
SYMBOL_EXPORT_SC_(KeywordPkg, generic_function_name);

DOCGROUP(clasp);
CL_DEFUN void core__verify_funcallable_instance_layout(size_t funcallableInstance_size, size_t funcallableInstance_rack_offset) {
  if (funcallableInstance_size != sizeof(FuncallableInstance_O))
    SIMPLE_ERROR("The cmpintrinsics.lisp funcallableInstance_size {} does not match sizeof(FuncallableInstance_O) {}",
                 funcallableInstance_size, sizeof(FuncallableInstance_O));
  if (funcallableInstance_rack_offset != offsetof(FuncallableInstance_O, _Rack))
    SIMPLE_ERROR("funcallableInstance_rack_offset {} does not match offsetof(_Rack,FuncallableInstance_O) {}",
                 funcallableInstance_rack_offset, offsetof(FuncallableInstance_O, _Rack));
}

CL_DEFUN T_mv core__build_single_dispatch_bytecode(List_sp call_history, size_t dispatch_argument_index) {
  /* get reg|read stamp|miss +  4 bytes per call history entry */
  size_t nentries = cl__length(call_history);
  size_t bytecode_length = 3 + nentries * 5;
  SimpleVector_byte8_t_sp bytecode = SimpleVector_byte8_t_O::make(bytecode_length);
  // stamp + effective method function
  SimpleVector_sp literals = SimpleVector_O::make(nentries * 2);
  if (dispatch_argument_index >= 5)
    SIMPLE_ERROR("Not supported dispatching on args after DTREE_OP_FARG4");
  ASSERT(DTREE_OP_FARG0 == (DTREE_OP_FARG1 - 1) == (DTREE_OP_FARG2 - 2) == (DTREE_OP_FARG3 - 3) == (DTREE_OP_FARG4 - 4));
  (*bytecode)[0] = DTREE_OP_FARG0 + dispatch_argument_index;
  (*bytecode)[1] = DTREE_OP_STAMP_READ;
  size_t ip = 2;
  std::vector<size_t> fixups;
  for (size_t ii = 0; ii < cl__length(call_history); ++ii) {
    List_sp specializers = oCar(cl__elt(call_history, ii));
    Instance_sp specializer_class = gc::As_assert<Instance_sp>(cl__elt(specializers, dispatch_argument_index));
    T_sp stamp_for_instances = specializer_class->instanceRef(Instance_O::REF_CLASS_STAMP_FOR_INSTANCES_);
    T_sp efm = oCdr(cl__elt(call_history, ii));
    (*bytecode)[ip + 0] = DTREE_OP_SD_EQ_BRANCH;
    (*bytecode)[ip + DTREE_SD_STAMP_OFFSET] = ii; // stamp index in literals
    (*bytecode)[ip + DTREE_SD_FAIL_OFFSET] = 5;   // skip EFM
    (*bytecode)[ip] = DTREE_OP_EFFECTIVE_METHOD;
    (*bytecode)[ip + DTREE_EFFECTIVE_METHOD_OFFSET] = nentries + ii; // index into literals for EFM
    (*literals)[ii] = stamp_for_instances;
    (*literals)[ii + nentries] = efm;
    ip += 5;
  }
  (*bytecode)[ip++] = DTREE_OP_SINGLE_DISPATCH_MISS;
  ASSERT(bytecode_length);
  return Values(bytecode, literals);
}
}; // namespace core
