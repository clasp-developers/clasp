#pragma once
/*
    File: byteCode.h
*/

#include <unistd.h>
#include <clasp/core/common.h>
#include <clasp/core/unwind.h>

namespace core {
class Bytecode_O;
};

template <> struct gctools::GCInfo<core::Bytecode_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};

namespace core {

FORWARD(BytecodeModule);
class BytecodeModule_O : public core::CxxObject_O {
  LISP_CLASS(core, CorePkg, BytecodeModule_O, "BytecodeModule", core::CxxObject_O);

public:
  // The actual bytecode.
  SimpleVector_byte8_t_sp _Bytecode;
  // A simple vector of literal objects referred to by the bytecode.
  // During loadltv, this will be default-initialized for a bit, hence
  // why it's not a SimpleVector_sp.
  T_sp _Literals = nil<T_O>();
  // A list of literal indices.
  // This indicates the constant at that index is a mutable
  // load-time-value (i.e. from (load-time-value foo [nil]))
  // which is important to know for BTB compilation.
  // Some kind of bit vector could be used instead, but it's
  // expected that these load time values will be rare.
  T_sp _MutableLiterals = nil<T_O>();
  // A simple-vector of BytecodeDebugInfo_sp (below)
  // ordered by start index
  T_sp _DebugInfo = nil<T_O>();
  // If this bytecode has been native-compiled, this can be
  // the corresponding JITDylib_sp.
  T_sp _NativeModule = nil<T_O>();

public:
  BytecodeModule_O(SimpleVector_byte8_t_sp bytecode)
    : _Bytecode(bytecode) {};
  CL_LISPIFY_NAME(BytecodeModule/make)
  CL_DEF_CLASS_METHOD
  static BytecodeModule_sp make(SimpleVector_byte8_t_sp bytecode) {
    // When we can gc code allocate entire block in GC memory
    BytecodeModule_sp codeblock = gctools::GC<BytecodeModule_O>::allocate<gctools::RuntimeStage>(bytecode);
    // Register the new module for the debugger
    codeblock->register_for_debug();
    return codeblock;
  };

  CL_DEFMETHOD T_sp literals() const { return this->_Literals; }
  CL_DEFMETHOD void setf_literals(SimpleVector_sp obj) { this->_Literals = obj; }
  CL_DEFMETHOD SimpleVector_byte8_t_sp bytecode() const { return this->_Bytecode; }
  CL_LISPIFY_NAME(BytecodeModule/debugInfo)
  CL_DEFMETHOD T_sp debugInfo() const { return this->_DebugInfo; }
  CL_LISPIFY_NAME(BytecodeModule/setfDebugInfo)
  CL_DEFMETHOD void setf_debugInfo(T_sp info) { this->_DebugInfo = info; }
  CL_LISPIFY_NAME(BytecodeModule/mutableLiterals)
  CL_DEFMETHOD T_sp mutableLiterals() const { return this->_MutableLiterals; }
  void setf_mutableLiterals(T_sp indices) { this->_MutableLiterals = indices; }
  CL_LISPIFY_NAME(BytecodeModule/nativeModule)
  CL_DEFMETHOD T_sp nativeModule() const { return this->_NativeModule; }
  void setf_nativeModule(T_sp mod) { this->_NativeModule = mod; }

  // Add the module to *all-bytecode-modules* for the debugger.
  void register_for_debug();
};

// Abstract debug information structure.
// START and END are labels during compilation, and resolved to
// fixnums (instruction pointers) during linking.
FORWARD(BytecodeDebugInfo);
class BytecodeDebugInfo_O : public General_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugInfo_O, "BytecodeDebugInfo", core::General_O);

public:
  BytecodeDebugInfo_O(T_sp start, T_sp end) : _start(start), _end(end) {}

public:
  T_sp _start;
  T_sp _end;

public:
  CL_LISPIFY_NAME(BytecodeDebugInfo/start)
  CL_DEFMETHOD T_sp start() const { return this->_start; }
  void setStart(T_sp start) { this->_start = start; }
  CL_LISPIFY_NAME(BytecodeDebugInfo/end)
  CL_DEFMETHOD T_sp end() const { return this->_end; }
  void setEnd(T_sp end) { this->_end = end; }
};

// Debug information about one lexical variable.
// Stored in a BytecodeDebugVars, below.
FORWARD(BytecodeDebugVar);
class BytecodeDebugVar_O : public General_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugVar_O, "BytecodeDebugVar", core::General_O);
public:
  BytecodeDebugVar_O(T_sp name, uint16_t index, bool cellp, List_sp decls)
    : _name(name), _index(index), _cellp(cellp), _decls(decls) {}
  CL_LISPIFY_NAME(BytecodeDebugVar/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugVar_sp make(T_sp name, uint16_t index, bool cellp, List_sp decls) {
    return gctools::GC<BytecodeDebugVar_O>::allocate<gctools::RuntimeStage>(name, index, cellp, decls);
  }
public:
  T_sp _name;
  uint16_t _index; // index in the bytecode stack frame.
  bool _cellp;
  List_sp _decls; // other declarations (e.g. type, user-defined)
public:
  CL_LISPIFY_NAME(BytecodeDebugVar/name)
  CL_DEFMETHOD T_sp name() const { return this->_name; }
  CL_LISPIFY_NAME(BytecodeDebugVar/frameIndex)
  CL_DEFMETHOD uint16_t frameIndex() const { return this->_index; }
  CL_LISPIFY_NAME(BytecodeDebugVar/cellp)
  CL_DEFMETHOD bool cellp() const { return this->_cellp; }
  CL_LISPIFY_NAME(BytecodeDebugVar/decls)
  CL_DEFMETHOD List_sp decls() const { return this->_decls; }
};

// Debug information structure for bindings.
FORWARD(BytecodeDebugVars);
class BytecodeDebugVars_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugVars_O, "BytecodeDebugVars", BytecodeDebugInfo_O);

public:
  BytecodeDebugVars_O(T_sp start, T_sp end, List_sp bindings) : BytecodeDebugInfo_O(start, end), _bindings(bindings) {}
  CL_LISPIFY_NAME(BytecodeDebugVars/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugVars_sp make(T_sp start, T_sp end, List_sp bindings) {
    return gctools::GC<BytecodeDebugVars_O>::allocate<gctools::RuntimeStage>(start, end, bindings);
  }

public:
  List_sp _bindings;

public:
  CL_LISPIFY_NAME(BytecodeDebugVars/bindings)
  CL_DEFMETHOD List_sp bindings() const { return this->_bindings; }
};

// Debug information for source form locations.
FORWARD(BytecodeDebugLocation);
class BytecodeDebugLocation_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugLocation_O, "BytecodeDebugLocation", BytecodeDebugInfo_O);

public:
  BytecodeDebugLocation_O(T_sp start, T_sp end, T_sp location) : BytecodeDebugInfo_O(start, end), _location(location) {}
  CL_LISPIFY_NAME(BytecodeDebugLocation/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugLocation_sp make(T_sp start, T_sp end, T_sp location) {
    return gctools::GC<BytecodeDebugLocation_O>::allocate<gctools::RuntimeStage>(start, end, location);
  }

public:
  T_sp _location;

public:
  CL_LISPIFY_NAME(BytecodeDebugLocation/location)
  CL_DEFMETHOD T_sp location() const { return this->_location; }
};

// Information about declarations.
// This records declarations from the source code.
// Some declarations are ignored by the simplistic bytecode compiler,
// but may be relevant for further compilation.
// (The debugger doesn't really need them.)
// START and END delineate the declarations' scope.
FORWARD(BytecodeAstDecls);
class BytecodeAstDecls_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeAstDecls_O, "BytecodeAstDecls", BytecodeDebugInfo_O);

public:
  BytecodeAstDecls_O(T_sp start, T_sp end, List_sp decls) : BytecodeDebugInfo_O(start, end), _decls(decls) {}
  CL_LISPIFY_NAME(BytecodeAstDecls/make)
  CL_DEF_CLASS_METHOD
  static BytecodeAstDecls_sp make(T_sp start, T_sp end, List_sp decls) {
    return gctools::GC<BytecodeAstDecls_O>::allocate<gctools::RuntimeStage>(start, end, decls);
  }

public:
  List_sp _decls;

public:
  CL_LISPIFY_NAME(BytecodeAstDecls/decls)
  CL_DEFMETHOD List_sp decls() const { return this->_decls; }
};

// Information about a THE form, also for the compiler.
FORWARD(BytecodeAstThe);
class BytecodeAstThe_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeAstThe_O, "BytecodeAstThe", BytecodeDebugInfo_O);

public:
  BytecodeAstThe_O(T_sp start, T_sp end, T_sp type, int receiving)
      : BytecodeDebugInfo_O(start, end), _type(type), _receiving(receiving) {}
  CL_LISPIFY_NAME(BytecodeAstThe/make)
  CL_DEF_CLASS_METHOD
  static BytecodeAstThe_sp make(T_sp start, T_sp end, T_sp type, int receiving) {
    return gctools::GC<BytecodeAstThe_O>::allocate<gctools::RuntimeStage>(start, end, type, receiving);
  }

public:
  T_sp _type;
  int _receiving; // as in compiler contexts - which values this decl applies to
public:
  CL_LISPIFY_NAME(BytecodeAstThe/receiving)
  CL_DEFMETHOD Fixnum receiving() const { return this->_receiving; }
  CL_LISPIFY_NAME(BytecodeAstThe/type)
  CL_DEFMETHOD T_sp type() const { return this->_type; }
};

// Information about an IF form.
// the START is just after the jump-if, and the END the start
// of the merge block. the THEN block starts at the destination
// of the jump-if.
// RECEIVING indicates how many values the merge block receives.
FORWARD(BytecodeAstIf);
class BytecodeAstIf_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeAstIf_O, "BytecodeAstIf", BytecodeDebugInfo_O);
  
public:
  BytecodeAstIf_O(T_sp start, T_sp end, int receiving)
      : BytecodeDebugInfo_O(start, end), _receiving(receiving) {}
  CL_LISPIFY_NAME(BytecodeAstIf/make)
  CL_DEF_CLASS_METHOD
  static BytecodeAstIf_sp make(T_sp start, T_sp end, int receiving) {
    return gctools::GC<BytecodeAstIf_O>::allocate<gctools::RuntimeStage>(start, end, receiving);
  }

public:
  int _receiving; // as in compiler contexts
public:
  CL_LISPIFY_NAME(BytecodeAstIf/receiving)
  CL_DEFMETHOD Fixnum receiving() const { return this->_receiving; }
};

// Information about a TAGBODY form.
// START and END delineate the extent of the tagbody, so START
// is immediately after the entry or save-sp.
// TAGS is an alist (go-tag . label), where label is an actual
// label during compilation, but resolved to an instruction
// index at the end of compilation.
FORWARD(BytecodeAstTagbody);
class BytecodeAstTagbody_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeAstTagbody_O, "BytecodeAstTagbody", BytecodeDebugInfo_O);
  
public:
  BytecodeAstTagbody_O(T_sp start, T_sp end, List_sp tags) : BytecodeDebugInfo_O(start, end), _tags(tags) {}
  CL_LISPIFY_NAME(BytecodeAstTagbody/make)
  CL_DEF_CLASS_METHOD
  static BytecodeAstTagbody_sp make(T_sp start, T_sp end, List_sp tags) {
    return gctools::GC<BytecodeAstTagbody_O>::allocate<gctools::RuntimeStage>(start, end, tags);
  }

public:
  List_sp _tags;

public:
  CL_LISPIFY_NAME(BytecodeAstTagbody/tags)
  CL_DEFMETHOD List_sp tags() const { return this->_tags; }

};

// Information about a BLOCK (or, despite the name, CATCH) form.
// START and END delineate the block's extent.
// RECEIVING indicates what values the block returns.
FORWARD(BytecodeAstBlock);
class BytecodeAstBlock_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeAstBlock_O, "BytecodeAstBlock", BytecodeDebugInfo_O);

public:
  BytecodeAstBlock_O(T_sp start, T_sp end, T_sp name, int receiving)
      : BytecodeDebugInfo_O(start, end), _name(name), _receiving(receiving) {}
  CL_LISPIFY_NAME(BytecodeAstBlock/make)
  CL_DEF_CLASS_METHOD
  static BytecodeAstBlock_sp make(T_sp start, T_sp end, T_sp name, int receiving) {
    return gctools::GC<BytecodeAstBlock_O>::allocate<gctools::RuntimeStage>(start, end, name, receiving);
  }

public:
  T_sp _name;
  int _receiving; // meaning is as for compiler contexts
public:
  CL_LISPIFY_NAME(BytecodeAstBlock/receiving)
  CL_DEFMETHOD Fixnum receiving() const { return this->_receiving; }
  CL_LISPIFY_NAME(BytecodeAstBlock/name)
  CL_DEFMETHOD T_sp name() const { return this->_name; }
};

// Indicates that a macroexpansion occurred. This is used to
// implement xref; see ext:who-macroexpands.
// Only stored for global macros. Not stored for compiler macros.
FORWARD(BytecodeDebugMacroexpansion);
class BytecodeDebugMacroexpansion_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugMacroexpansion_O, "BytecodeDebugMacroexpansion", BytecodeDebugInfo_O);

public:
  BytecodeDebugMacroexpansion_O(T_sp start, T_sp end, T_sp macro_name) : BytecodeDebugInfo_O(start, end), _macro_name(macro_name) {}
  CL_LISPIFY_NAME(BytecodeDebugMacroexpansion/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugMacroexpansion_sp make(T_sp start, T_sp end, T_sp macro_name) {
    return gctools::GC<BytecodeDebugMacroexpansion_O>::allocate<gctools::RuntimeStage>(start, end, macro_name);
  }

public:
  T_sp _macro_name;

public:
  CL_LISPIFY_NAME(BytecodeDebugMacroexpansion/macro-name)
  CL_DEFMETHOD T_sp macro_name() const { return this->_macro_name; }
};

// Dynenv used for VM call frames to ensure the unwinder properly
// cleans up stack frames.
class VMFrameDynEnv_O : public DynEnv_O {
  LISP_CLASS(core, CorePkg, VMFrameDynEnv_O, "VMFrameDynEnv", DynEnv_O);

public:
  VMFrameDynEnv_O(T_O** a_old_sp, T_O** a_old_fp) : old_sp(a_old_sp), old_fp(a_old_fp) {}
  // Slightly sketchy: We use the destructor to reset the stack pointer,
  // so that C++ unwinds are also affected by this dynenv.
  // This means VMFrames must be stack allocated.
  ~VMFrameDynEnv_O() {
    VirtualMachine& vm = my_thread->_VM;
    vm._stackPointer = this->old_sp;
    vm._framePointer = this->old_fp;
  }

public:
  T_O** old_sp;
  T_O** old_fp;

public:
  virtual SearchStatus search() const { return Continue; }
  virtual void proceed();
};
}; // namespace core

namespace core {

// Offsets on the VM stack. These are pushed throughout bytecode.cc,
// e.g. the PC is pushed by call instructions, and the FP by bytecode_call.
const size_t BYTECODE_FRAME_PC_OFFSET = 1;
const size_t BYTECODE_FRAME_FP_OFFSET = 0;

bool bytecode_module_contains_address_p(BytecodeModule_sp, void*);
bool bytecode_function_contains_address_p(BytecodeSimpleFun_sp, void*);
T_sp bytecode_function_for_pc(BytecodeModule_sp, void*);
T_sp bytecode_spi_for_pc(BytecodeModule_sp, void*);
List_sp bytecode_bindings_for_pc(BytecodeModule_sp, void*, T_O**);
void* bytecode_pc();

}; // namespace core

extern "C" {
gctools::return_type bytecode_call(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args);

gctools::return_type gfbytecode_call(unsigned char* pc, core::T_O* lcc_closure, size_t lcc_nargs, core::T_O** lcc_args);
};
