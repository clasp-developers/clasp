/*
    File: byteCode.h
*/


#ifndef bytecode_H //[
#define bytecode_H
#include <unistd.h>
#include <clasp/core/common.h>

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
  Literals_sp_Type                   _Literals;
  Bytecode_sp_Type                   _Bytecode;

public:
  CL_LISPIFY_NAME(BytecodeModule/make)
  CL_DEF_CLASS_METHOD
  static BytecodeModule_sp make() {
    // When we can gc code allocate entire block in GC memory
    BytecodeModule_sp codeblock = gctools::GC<BytecodeModule_O>::allocate<gctools::RuntimeStage>();
    return codeblock;
  };

  Literals_sp_Type literals() const;
  void setf_literals(T_sp obj);
  Bytecode_sp_Type bytecode() const;
  void setf_bytecode(T_sp obj);
  BytecodeModule_O() {};
};
};

#endif
