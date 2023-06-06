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

// Debug information structure for bindings.
FORWARD(BytecodeDebugVars);
class BytecodeDebugVars_O : public General_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugVars_O, "BytecodeDebugVars", core::General_O);
public:
  BytecodeDebugVars_O(T_sp start, T_sp end, List_sp bindings)
    : _start(start), _end(end), _bindings(bindings) {}
  CL_LISPIFY_NAME(BytecodeDebugVars/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugVars_sp make(T_sp start, T_sp end, List_sp bindings) {
    return gctools::GC<BytecodeDebugVars_O>::allocate<gctools::RuntimeStage>(start, end, bindings);
  }
public:
  T_sp _start;
  T_sp _end;
  List_sp _bindings;
public:
  CL_LISPIFY_NAME(BytecodeDebugVars/start)
  CL_DEFMETHOD T_sp start() const { return this->_start; }
  void setStart(T_sp start) { this->_start = start; }
  CL_LISPIFY_NAME(BytecodeDebugVars/end)
  CL_DEFMETHOD T_sp end() const { return this->_end; }
  void setEnd(T_sp end) { this->_end = end; }
  CL_LISPIFY_NAME(BytecodeDebugVars/bindings)
  CL_DEFMETHOD List_sp bindings() const { return this->_bindings; }
};

// Debug information for source form locations.
FORWARD(BytecodeDebugLocation);
class BytecodeDebugLocation_O : public General_O {
  LISP_CLASS(core, CorePkg, BytecodeDebugLocation_O, "BytecodeDebugLocation", core::General_O);
public:
  BytecodeDebugLocation_O(T_sp start, T_sp end, T_sp location)
    : _start(start), _end(end), _location(location) {}
  CL_LISPIFY_NAME(BytecodeDebugLocation/make)
  CL_DEF_CLASS_METHOD
  static BytecodeDebugLocation_sp make(T_sp start, T_sp end, T_sp location) {
    return gctools::GC<BytecodeDebugLocation_O>::allocate<gctools::RuntimeStage>(start, end, location);
  }
public:
  T_sp _start;
  T_sp _end;
  T_sp _location;
  CL_LISPIFY_NAME(BytecodeDebugLocation/start)
  CL_DEFMETHOD T_sp start() const { return this->_start; }
  void setStart(T_sp start) { this->_start = start; }
  CL_LISPIFY_NAME(BytecodeDebugLocation/end)
  CL_DEFMETHOD T_sp end() const { return this->_end; }
  void setEnd(T_sp end) { this->_end = end; }
  CL_LISPIFY_NAME(BytecodeDebugLocation/location)
  CL_DEFMETHOD T_sp location() const { return this->_location; }
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
