/*
    File: byteCode.h
*/


#ifndef bytecode_H //[
#define bytecode_H
#include <unistd.h>
#include <clasp/core/common.h>
#include <clasp/core/unwind.h>

namespace core {
  class Bytecode_O;
};

template <>
struct gctools::GCInfo<core::Bytecode_O> {
  static bool constexpr NeedsInitialization = true;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = normal;
};


namespace core {

FORWARD(BytecodeModule);
class BytecodeModule_O : public core::CxxObject_O {
  LISP_CLASS(core, CorePkg, BytecodeModule_O, "BytecodeModule", core::CxxObject_O);
 public:
  void initialize();
public:
  typedef T_O                        Literals_O_Type;
  typedef T_O                        Bytecode_O_Type;
  typedef T_sp                       Literals_sp_Type;
  typedef T_sp                       Bytecode_sp_Type;
public:
  Literals_sp_Type                   _Literals;
  Bytecode_sp_Type                   _Bytecode;
  T_sp                               _CompileInfo;
  T_sp                               _DebugInfo = nil<T_O>();

public:
  BytecodeModule_O() {};
  CL_LISPIFY_NAME(BytecodeModule/make)
  CL_DEF_CLASS_METHOD
  static BytecodeModule_sp make() {
    // When we can gc code allocate entire block in GC memory
    BytecodeModule_sp codeblock = gctools::GC<BytecodeModule_O>::allocate<gctools::RuntimeStage>();
    // Register the new module for the debugger
    codeblock->register_for_debug();
    return codeblock;
  };

  Literals_sp_Type literals() const;
  void setf_literals(T_sp obj);
  Bytecode_sp_Type bytecode() const;
  void setf_bytecode(T_sp obj);
  Bytecode_sp_Type compileInfo() const;
  void setf_compileInfo(T_sp obj);
  CL_LISPIFY_NAME(BytecodeModule/debugInfo)
  CL_DEFMETHOD T_sp debugInfo() const { return this->_DebugInfo; }
  CL_LISPIFY_NAME(BytecodeModule/setfDebugInfo)
  CL_DEFMETHOD void setf_debugInfo(T_sp info) { this->_DebugInfo = info; }

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
  BytecodeDebugInfo_O() {} // dummy required due to being inherited?
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

// Debug information structure for bindings.
FORWARD(BytecodeDebugVars);
class BytecodeDebugVars_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugVars_O, "BytecodeDebugVars", BytecodeDebugInfo_O);
public:
  BytecodeDebugVars_O(T_sp start, T_sp end, List_sp bindings)
    : BytecodeDebugInfo_O(start, end), _bindings(bindings) {}
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
  BytecodeDebugLocation_O(T_sp start, T_sp end, T_sp location)
    : BytecodeDebugInfo_O(start, end), _location(location) {}
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

// Debug information for declarations.
// This records declarations from the source code.
// Some declarations are ignored by the simplistic bytecode compiler,
// but may be relevant for further compilation.
// (The debugger doesn't really need them.)
FORWARD(BytecodeDebugDecls);
class BytecodeDebugDecls_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugDecls_O, "BytecodeDebugDecls", BytecodeDebugInfo_O);
public:
  BytecodeDebugDecls_O(T_sp start, T_sp end, List_sp decls)
    : BytecodeDebugInfo_O(start, end), _decls(decls) {}
  CL_LISPIFY_NAME(BytecodeDebugDecls/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugDecls_sp make(T_sp start, T_sp end, List_sp decls) {
    return gctools::GC<BytecodeDebugDecls_O>::allocate<gctools::RuntimeStage>(start, end, decls);
  }
public:
  List_sp _decls;
public:
  CL_LISPIFY_NAME(BytecodeDebugDecls/decls)
  CL_DEFMETHOD List_sp decls() const { return this->_decls; }
};

// Information about a THE form, also for the compiler.
FORWARD(BytecodeDebugThe);
class BytecodeDebugThe_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugThe_O, "BytecodeDebugThe", BytecodeDebugInfo_O);
public:
  BytecodeDebugThe_O(T_sp start, T_sp end, T_sp type, int receiving)
    : BytecodeDebugInfo_O(start, end), _type(type), _receiving(receiving) {}
  CL_LISPIFY_NAME(BytecodeDebugThe/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugThe_sp make(T_sp start, T_sp end, T_sp type, int receiving) {
    return gctools::GC<BytecodeDebugThe_O>::allocate<gctools::RuntimeStage>(start, end, type, receiving);
  }
public:
  T_sp _type;
  int _receiving; // as in compiler contexts - which values this decl applies to
public:
  CL_LISPIFY_NAME(BytecodeDebugThe/receiving)
  CL_DEFMETHOD Fixnum receiving() const { return this->_receiving; }
  CL_LISPIFY_NAME(BytecodeDebugThe/type)
  CL_DEFMETHOD T_sp type() const { return this->_type; }
};

// Information about the beginning of a semi-basic block.
// RECEIVING indicates what values the block will pull from the stack or
// MV register; this information can be used by the compiler to determine
// how many PHI nodes are needed.
FORWARD(BytecodeDebugBlock);
class BytecodeDebugBlock_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugBlock_O, "BytecodeDebugBlock", BytecodeDebugInfo_O);
public:
  BytecodeDebugBlock_O(T_sp start, T_sp end, T_sp name, int receiving)
    : BytecodeDebugInfo_O(start, end), _name(name), _receiving(receiving) {}
  CL_LISPIFY_NAME(BytecodeDebugBlock/make)
    CL_DEF_CLASS_METHOD
    static BytecodeDebugBlock_sp make(T_sp start, T_sp end, T_sp name, int receiving) {
    return gctools::GC<BytecodeDebugBlock_O>::allocate<gctools::RuntimeStage>(start, end, name, receiving);
  }
public:
  T_sp _name;
  int _receiving; // meaning is as for compiler contexts
public:
  CL_LISPIFY_NAME(BytecodeDebugBlock/receiving)
    CL_DEFMETHOD Fixnum receiving() const { return this->_receiving; }
  CL_LISPIFY_NAME(BytecodeDebugBlock/name)
    CL_DEFMETHOD T_sp name() const { return this->_name; }
};

// Indicates a nonlocal exit. This is used by the compiler to ensure consistency
// when compiling the unreachable code following an exit.
// The idea is that this unreachable code can be compiled as if the exit only
// acted as a nonexiting form.
// For example take (foo (return (bar)) ...); computation of the remaining
// arguments and the call to FOO are unreachable, but if the argument was just
// (bar), this unreachable code would just need to have one value added to the
// stack and to be in whatever dynamic environment is in place before.
FORWARD(BytecodeDebugExit);
class BytecodeDebugExit_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugExit_O, "BytecodeDebugExit", BytecodeDebugInfo_O);
public:
  BytecodeDebugExit_O(T_sp start, T_sp end, int receiving)
    : BytecodeDebugInfo_O(start, end), _receiving(receiving) {}
  CL_LISPIFY_NAME(BytecodeDebugExit/make)
    CL_DEF_CLASS_METHOD
    static BytecodeDebugExit_sp make(T_sp start, T_sp end, int receiving) {
    return gctools::GC<BytecodeDebugExit_O>::allocate<gctools::RuntimeStage>(start, end, receiving);
  }
public:
  int _receiving; // meaning is as for compiler contexts
public:
  CL_LISPIFY_NAME(BytecodeDebugExit/receiving)
    CL_DEFMETHOD Fixnum receiving() const { return this->_receiving; }
};

// Indicates that a macroexpansion occurred. This is used to
// implement xref; see ext:who-macroexpands.
// Only stored for global macros. Not stored for compiler macros.
FORWARD(BytecodeDebugMacroexpansion);
class BytecodeDebugMacroexpansion_O : public BytecodeDebugInfo_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugMacroexpansion_O, "BytecodeDebugMacroexpansion", BytecodeDebugInfo_O);
public:
  BytecodeDebugMacroexpansion_O(T_sp start, T_sp end, T_sp macro_name)
    : BytecodeDebugInfo_O(start, end), _macro_name(macro_name) {}
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
  VMFrameDynEnv_O(T_O** a_old_sp, T_O** a_old_fp)
    : old_sp(a_old_sp), old_fp(a_old_fp) {}
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

bool bytecode_module_contains_address_p(BytecodeModule_sp, void*);
bool bytecode_function_contains_address_p(GlobalBytecodeSimpleFun_sp, void*);
T_sp bytecode_function_for_pc(BytecodeModule_sp, void*);
T_sp bytecode_spi_for_pc(BytecodeModule_sp, void*);
List_sp bytecode_bindings_for_pc(BytecodeModule_sp, void*, T_O**);
void* bytecode_pc();

}; // namespace core

extern "C" {
gctools::return_type bytecode_call(unsigned char* pc, core::T_O* lcc_closure,
                                   size_t lcc_nargs, core::T_O** lcc_args);

gctools::return_type gfbytecode_call(unsigned char* pc, core::T_O* lcc_closure,
                                     size_t lcc_nargs, core::T_O** lcc_args);

};

#endif
