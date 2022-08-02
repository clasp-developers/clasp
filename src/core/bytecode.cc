
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

};

