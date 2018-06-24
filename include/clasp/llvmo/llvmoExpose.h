/*
    File: llvmoExpose.h
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
#ifndef llvmoExpose_H //[
#define llvmoExpose_H

#include <clasp/core/common.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/array.h>
#include <clasp/core/ql.h>
#include <llvm/Object/SymbolSize.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Linker/Linker.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
//#include "llvm/ExecutionEngine/JIT.h"
#include <llvm/ExecutionEngine/MCJIT.h>
//#include "llvm/ExecutionEngine/JITMemoryManager.h"
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/LambdaResolver.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
//#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
//#include "llvm/Support/IRBuilder.h"

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/metaClass.fwd.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/array.h>
#include <clasp/llvmo/llvmoExpose.fwd.h>
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/debugInfoExpose.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/llvmo/translators.h>
#include <clasp/llvmo/insertPoint.fwd.h>
#include <clasp/llvmo/debugLoc.fwd.h>
#include <clasp/llvmo/llvmoPackage.h>



namespace llvmo {
  FORWARD(CallInst);
  FORWARD(InvokeInst);
FORWARD(LLVMContext);
class LLVMContext_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LLVMContext, LLVMContext_O, "llvm-context", core::ExternalObject_O);
  typedef llvm::LLVMContext ExternalType;
  typedef llvm::LLVMContext *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  string __repr__() const;
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  bool equal(core::T_sp obj) const;
  static LLVMContext_sp create_llvm_context();
  ;
  LLVMContext_O() : Base(), _ptr(NULL){};
  ~LLVMContext_O() {
    if (_ptr != NULL) {
      delete _ptr;
      _ptr = NULL;
    };
  }

}; // LLVMContext_O
}; // llvmo

namespace translate {
  template <>
    struct from_object<llvm::LLVMContext &, std::true_type> {
    typedef llvm::LLVMContext &DeclareType;
    DeclareType _v;
  from_object(T_P object) : _v(*(gc::As<llvmo::LLVMContext_sp>(object)->wrappedPtr())){};
  };

  template <>
    struct to_object<llvm::LLVMContext &> {
    static core::T_sp convert(llvm::LLVMContext& lc) {
      return ((core::RP_Create_wrapped<llvmo::LLVMContext_O,llvm::LLVMContext*>(&lc)));
    };
  };
};
    ;
/* to_object translators */

namespace llvmo {
FORWARD(Linker);
class Linker_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Linker, Linker_O, "Linker", core::ExternalObject_O);
  typedef llvm::Linker ExternalType;
  typedef llvm::Linker *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  static Linker_sp make(Module_sp module);

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  Linker_O() : Base(), _ptr(NULL){};
  ~Linker_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

}; // Linker_O
}; // llvmo

namespace translate {
template <>
struct from_object<llvm::Linker &, std::true_type> {
  typedef llvm::Linker &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(gc::As<llvmo::Linker_sp>(object)->wrappedPtr())){};
};
};
    ;
/* to_object translators */

namespace llvmo {
FORWARD(Pass);
class Pass_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Pass, Pass_O, "Pass", core::ExternalObject_O);
  typedef llvm::Pass ExternalType;
  typedef llvm::Pass *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  Pass_O() : Base(), _ptr(NULL){};
  ~Pass_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

}; // Pass_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Pass *, std::true_type> {
  typedef llvm::Pass *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Pass_sp>(object)->wrappedPtr()){};
};
};

/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Pass *> {
  static core::T_sp convert(llvm::Pass *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Pass_O, llvm::Pass *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(AttributeSet);
class AttributeSet_O : public core::General_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AttributeSet, AttributeSet_O, "AttributeSet", core::General_O);
 public:
  typedef llvm::AttributeSet ExternalType;
protected:
  llvm::AttributeSet val;

public:
  llvm::AttributeSet getAttributeSet() { return this->val; };
  AttributeSet_O(){};
  AttributeSet_O(llvm::AttributeSet v) : val(v){};
}; // AttributeSet_O
}; // llvmo
/* from_object translators */

/* to_object translators */

namespace translate {
template <>
struct from_object<llvm::AttributeSet> {
  typedef llvm::AttributeSet DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) : _v(gc::As<llvmo::AttributeSet_sp>(object)->getAttributeSet()){};
};
template <>
struct to_object<llvm::AttributeSet> {
  static core::T_sp convert(llvm::AttributeSet val) {
    _G();
    GC_ALLOCATE_VARIADIC(llvmo::AttributeSet_O, obj, val);
    return obj;
  };
};
};

namespace translate {
template <>
struct from_object<llvm::ArrayRef<llvm::Attribute::AttrKind>> {
  typedef std::vector<llvm::Attribute::AttrKind> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_AttributeEnum->symbolValue());
      for (auto cvals : lcvals) {
        llvm::Attribute::AttrKind ak = converter->enumForSymbol<llvm::Attribute::AttrKind>(gc::As<core::Symbol_sp>(core::oCar(cvals)));
        _v.push_back(ak);
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to llvm::ArrayRef<llvm::Attribute::AttrKind>",  core::_rep_(o).c_str());
  }
};
};

namespace llvmo {
FORWARD(Triple);
class Triple_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Triple, Triple_O, "Triple", core::ExternalObject_O);
  typedef llvm::Triple ExternalType;
  typedef llvm::Triple *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  static Triple_sp make(const string &triple);

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL)
      delete this->_ptr;
    this->_ptr = ptr;
  }
  Triple_O() : Base(), _ptr(NULL){};
  ~Triple_O() {
    if (_ptr != NULL) {
      delete _ptr;
      _ptr = NULL;
    };
  }

}; // Triple_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Triple *, std::true_type> {
  typedef llvm::Triple *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::Triple_sp>(object)->wrappedPtr()){};
};

template <>
struct from_object<llvm::Triple &, std::true_type> {
  typedef llvm::Triple &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::Triple_sp>(object)->wrappedPtr()){};
};
};

/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Triple *> {
  static core::T_sp convert(llvm::Triple *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Triple_O, llvm::Triple *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(TargetOptions);
class TargetOptions_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetOptions, TargetOptions_O, "TargetOptions", core::ExternalObject_O);
  typedef llvm::TargetOptions ExternalType;
  typedef llvm::TargetOptions *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  static TargetOptions_sp make();

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL)
      delete this->_ptr;
    this->_ptr = ptr;
  }

public:
  bool NoFramePointerElim();
  void setfNoFramePointerElim(bool val);
  bool JITEmitDebugInfo();
  void setfJITEmitDebugInfo(bool val);
  bool JITEmitDebugInfoToDisk();
  void setfJITEmitDebugInfoToDisk(bool val);

public:
  TargetOptions_O() : Base(), _ptr(NULL){};
  ~TargetOptions_O() {
    if (_ptr != NULL) {
      delete _ptr;
      _ptr = NULL;
    };
  }

}; // TargetOptions_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::TargetOptions *, std::true_type> {
  typedef llvm::TargetOptions *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::TargetOptions_sp>(object)->wrappedPtr()){};
};
template <>
struct from_object<const llvm::TargetOptions &, std::true_type> {
  typedef const llvm::TargetOptions &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::TargetOptions_sp>(object)->wrappedPtr()){};
};
};

/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::TargetOptions *> {
  static core::T_sp convert(llvm::TargetOptions *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::TargetOptions_O, llvm::TargetOptions *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(Target);
class Target_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Target, Target_O, "Target", core::ExternalObject_O);
  typedef llvm::Target ExternalType;
  typedef llvm::Target *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  Target_O() : Base(), _ptr(NULL){};
  ~Target_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

}; // Target_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Target *, std::true_type> {
  typedef llvm::Target *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::Target_sp>(object)->wrappedPtr()){};
};
};
/* to_object translators */
namespace translate {
template <>
struct to_object<llvm::Target *, translate::dont_adopt_pointer> {
  static core::T_sp convert(llvm::Target *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Target_O, llvm::Target *>(ptr)));
  }
};

template <>
struct to_object<const llvm::Target *> {
  static core::T_sp convert(const llvm::Target *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Target_O, llvm::Target *>(const_cast<llvm::Target *>(ptr))));
  }
};
};
    ;

namespace llvmo {
FORWARD(MCSubtargetInfo);
class MCSubtargetInfo_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MCSubtargetInfo, MCSubtargetInfo_O, "MCSubtargetInfo", core::ExternalObject_O);
  typedef llvm::MCSubtargetInfo ExternalType;
  typedef llvm::MCSubtargetInfo *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  MCSubtargetInfo_O() : Base(), _ptr(NULL){};
  ~MCSubtargetInfo_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

}; // MCSubtargetInfo_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::MCSubtargetInfo *, std::true_type> {
  typedef llvm::MCSubtargetInfo *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::MCSubtargetInfo_sp>(object)->wrappedPtr()){};
};
};
/* to_object translators */
namespace translate {
template <>
struct to_object<llvm::MCSubtargetInfo *, translate::dont_adopt_pointer> {
  static core::T_sp convert(llvm::MCSubtargetInfo *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::MCSubtargetInfo_O, llvm::MCSubtargetInfo *>(ptr)));
  }
};

template <>
struct to_object<const llvm::MCSubtargetInfo *> {
  static core::T_sp convert(const llvm::MCSubtargetInfo *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::MCSubtargetInfo_O, llvm::MCSubtargetInfo *>(const_cast<llvm::MCSubtargetInfo *>(ptr))));
  }
};
};
    ;

namespace llvmo {
class TargetSubtargetInfo_O : public MCSubtargetInfo_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetSubtargetInfo, TargetSubtargetInfo_O, "TargetSubtargetInfo", MCSubtargetInfo_O);
  typedef llvm::TargetSubtargetInfo ExternalType;
  typedef llvm::TargetSubtargetInfo *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }

  TargetSubtargetInfo_O() : Base(){};
  ~TargetSubtargetInfo_O() {}

}; // TargetSubtargetInfo_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::TargetSubtargetInfo *, std::true_type> {
  typedef llvm::TargetSubtargetInfo *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<gc::smart_ptr<llvmo::TargetSubtargetInfo_O>>(object)->wrappedPtr()){};
};
template <>
struct to_object<const llvm::TargetSubtargetInfo *> {
  static core::T_sp convert(const llvm::TargetSubtargetInfo *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::TargetSubtargetInfo_O, llvm::TargetSubtargetInfo *>(const_cast<llvm::TargetSubtargetInfo *>(ptr))));
  }
};
};



namespace translate {
template <>
struct from_object<llvm::CodeGenOpt::Level, std::true_type> {
  typedef llvm::CodeGenOpt::Level DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CodeGenOpt::Default) {
    if (object.nilp()) {
      SIMPLE_ERROR_SPRINTF("You must pass a valid CodeGenOpt");
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeGenOpt->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CodeGenOpt::Level>(so);
    } else {
      SIMPLE_ERROR_SPRINTF("You must pass a valid CodeGenOpt");
    }
  }
};

template <>
  struct from_object<llvm::Optional<llvm::Reloc::Model>, std::true_type> {
  typedef llvm::Optional<llvm::Reloc::Model> DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.nilp()) {
//      SIMPLE_ERROR(BF("You must pass a valid RelocModel"));
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      if ( so == llvmo::_sym_RelocModel_undefined ) {
        //printf("%s:%d Leaving llvm::Reloc::Model Undefined\n", __FILE__, __LINE__ );
      } else {
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_RelocModel->symbolValue());
        this->_v = converter->enumForSymbol<llvm::Reloc::Model>(so);
      }
    } else {
      SIMPLE_ERROR_SPRINTF("You must pass a valid RelocModel or %s", _rep_(llvmo::_sym_RelocModel_undefined).c_str());
    }
  }
};

template <>
struct from_object<llvm::CodeModel::Model, std::true_type> {
  typedef llvm::CodeModel::Model DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CodeModel::Small) {
    if (object.nilp()) {
      SIMPLE_ERROR_SPRINTF("You must pass a valid CodeModel");
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeModel->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CodeModel::Model>(so);
    } else {
      SIMPLE_ERROR_SPRINTF("You must pass a valid CodeModel");
    }
  }
};
template <>
struct from_object<llvm::TargetMachine::CodeGenFileType, std::true_type> {
  typedef llvm::TargetMachine::CodeGenFileType DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::TargetMachine::CGFT_ObjectFile) {
    if (object.notnilp()) {
      if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeGenFileType->symbolValue());
        this->_v = converter->enumForSymbol<llvm::TargetMachine::CodeGenFileType>(so);
        return;
      }
    }
    SIMPLE_ERROR_SPRINTF("You must pass a valid ");
  }
};
};






namespace llvmo {
FORWARD(TargetMachine);
FORWARD(PassManager);
class TargetMachine_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetMachine, TargetMachine_O, "TargetMachine", core::ExternalObject_O);
  typedef llvm::TargetMachine ExternalType;
  typedef llvm::TargetMachine *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  /*! Return (values CodeGenFileType-symbol) */
  void addPassesToEmitFileAndRunPassManager(PassManager_sp passManager,
                                            core::T_sp stream,
                                            llvm::TargetMachine::CodeGenFileType,
                                            Module_sp module);

  TargetMachine_O() : Base(), _ptr(NULL){};
  ~TargetMachine_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }
}; // TargetMachine_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::TargetMachine *, std::true_type> {
  typedef llvm::TargetMachine *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::TargetMachine_sp>(object)->wrappedPtr()){};
};
};

/* to_object translators */

namespace translate {
  template <>
    struct to_object<llvm::TargetMachine *> {
    static core::T_sp convert(llvm::TargetMachine *ptr) {
      _G();
      return ((core::RP_Create_wrapped<llvmo::TargetMachine_O, llvm::TargetMachine *>(ptr)));
    }
  };
};

extern llvm::Value* llvm_cast_error_ptr;
template <typename T, typename U>
  T* llvm_cast(U* p) {
  if (!llvm::isa<T>(p)) {
    // save the pointer in a global so we can take a look at it
    llvm_cast_error_ptr = reinterpret_cast<llvm::Value*>(p);
    SIMPLE_ERROR_SPRINTF("llvm_cast<T> argument of incompatible type - bad pointer stored in (void*)llvm_cast_error_ptr!");
  }
  return reinterpret_cast<T*>(p);
}

namespace llvmo {
FORWARD(PassManagerBase);
FORWARD(LLVMTargetMachine);
class LLVMTargetMachine_O : public TargetMachine_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LLVMTargetMachine, LLVMTargetMachine_O, "LLVMTargetMachine", TargetMachine_O);
  typedef llvm::LLVMTargetMachine ExternalType;
  typedef llvm::LLVMTargetMachine *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return /*dynamic_*/reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }

  //        static LLVMTargetMachine_sp makeLLVMTargetMachine(
  bool LLVMTargetMachine_addPassesToEmitFile(PassManagerBase_sp pm,
                                             core::T_sp stream,
                                             core::Symbol_sp fileType);
  LLVMTargetMachine_O() : Base(){};
  ~LLVMTargetMachine_O() {}

}; // LLVMTargetMachine_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::LLVMTargetMachine *, std::true_type> {
  typedef llvm::LLVMTargetMachine *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::LLVMTargetMachine_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::LLVMTargetMachine *> {
  static core::T_sp convert(llvm::LLVMTargetMachine *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::LLVMTargetMachine_O, llvm::LLVMTargetMachine *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(FunctionPass);
class FunctionPass_O : public Pass_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::FunctionPass, FunctionPass_O, "FunctionPass", Pass_O);
  typedef llvm::FunctionPass ExternalType;
  typedef llvm::FunctionPass *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return /*dynamic_*/reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  FunctionPass_O() : Base(){};
  ~FunctionPass_O() {}

}; // FunctionPass_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::FunctionPass *, std::true_type> {
  typedef llvm::FunctionPass *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::FunctionPass_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::FunctionPass *> {
  static core::T_sp convert(llvm::FunctionPass *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::FunctionPass_O, llvm::FunctionPass *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ModulePass);
class ModulePass_O : public Pass_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ModulePass, ModulePass_O, "ModulePass", Pass_O);
  typedef llvm::ModulePass ExternalType;
  typedef llvm::ModulePass *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return /*dynamic_*/reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  ModulePass_O() : Base(){};
  ~ModulePass_O() {}

}; // ModulePass_O
}; // llvmo

namespace translate {
template <>
struct from_object<llvm::ModulePass *, std::true_type> {
  typedef llvm::ModulePass *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ModulePass_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ModulePass *> {
  static core::T_sp convert(llvm::ModulePass *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ModulePass_O, llvm::ModulePass *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ImmutablePass);
/*! ImmutablePass_O doesn't own its pointer because ownership is given to the PassManager */
class ImmutablePass_O : public ModulePass_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ImmutablePass, ImmutablePass_O, "ImmutablePass", ModulePass_O);
  typedef llvm::ImmutablePass ExternalType;
  typedef llvm::ImmutablePass *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return /*dynamic_*/reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  ImmutablePass_O() : Base(){};
  ~ImmutablePass_O() {}

}; // ImmutablePass_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::ImmutablePass *, std::true_type> {
  typedef llvm::ImmutablePass *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ImmutablePass_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ImmutablePass *> {
  static core::T_sp convert(llvm::ImmutablePass *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ImmutablePass_O, llvm::ImmutablePass *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(PassManagerBase);
class PassManagerBase_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::legacy::PassManagerBase, PassManagerBase_O, "PassManagerBase", core::ExternalObject_O);
  typedef llvm::legacy::PassManagerBase ExternalType;
  typedef llvm::legacy::PassManagerBase *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL)
      delete this->_ptr;
    this->_ptr = ptr;
  }
  PassManagerBase_O() : Base(), _ptr(NULL){};
  ~PassManagerBase_O() {
    if (_ptr != NULL) {
      delete _ptr;
      _ptr = NULL;
    };
  }

}; // PassManagerBase_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
  struct from_object<llvm::legacy::PassManagerBase *, std::true_type> {
  typedef llvm::legacy::PassManagerBase *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::PassManagerBase_sp>(object)->wrappedPtr()){};
};
template <>
  struct from_object<llvm::legacy::PassManagerBase &, std::true_type> {
  typedef llvm::legacy::PassManagerBase &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::PassManagerBase_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
  struct to_object<llvm::legacy::PassManagerBase *> {
  static core::T_sp convert(llvm::legacy::PassManagerBase *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::PassManagerBase_O, llvm::legacy::PassManagerBase *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(Value);
class Value_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Value, Value_O, "value", core::ExternalObject_O);
  typedef llvm::Value ExternalType;
  typedef llvm::Value *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Value_sp create(llvm::Value *ptr);
  ;
  Value_O() : Base(), _ptr(NULL){};
  ~Value_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }
  bool llvm_sys_value_p() const { return true; };
  string __repr__() const;
  bool valid() const;
}; // Value_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Value *, std::true_type> {
  typedef llvm::Value *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Value_sp>(object)->wrappedPtr()){};
};
template <>
struct from_object<llvm::ArrayRef<llvm::Value *>> {
  typedef std::vector<llvm::Value *> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      for (auto cvals : lcvals) {
        llvm::Value *vP = gc::As<llvmo::Value_sp>(core::oCar(cvals))->wrappedPtr();
        _v.push_back(vP);
      }
      return;
    } else if (core::Vector_sp vvals = o.asOrNull<core::Vector_O>()) {
      _v.resize(vvals->length());
      for (int i(0), iEnd(vvals->length()); i < iEnd; ++i) {
        _v[i] = gc::As<llvmo::Value_sp>(vvals->rowMajorAref(i))->wrappedPtr();
        printf("%s:%d   Entry[%d] <-- %s\n", __FILE__, __LINE__, i, _rep_(vvals->rowMajorAref(i)).c_str());
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to llvm::ArrayRef<llvm::Value*>", core::_rep_(o).c_str());
  }
};

};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Value *> {
  static core::T_sp convert(llvm::Value *ptr) {
    _G();
    return ((llvmo::Value_O::create(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(Metadata);
class Metadata_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Metadata, Metadata_O, "metadata", core::ExternalObject_O);
  typedef llvm::Metadata ExternalType;
  typedef llvm::Metadata *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Metadata_sp create(llvm::Metadata *ptr);
  ;
  Metadata_O() : Base(), _ptr(NULL){};
  ~Metadata_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }
  //	string __repr__() const;
  //	bool valid() const;
}; // Metadata_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Metadata *, std::true_type> {
  typedef llvm::Metadata *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Metadata_sp>(object)->wrappedPtr()){};
};
template <>
struct from_object<llvm::ArrayRef<llvm::Metadata *>> {
  typedef std::vector<llvm::Metadata *> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      for (auto cvals : lcvals) {
        llvm::Metadata *vP = gc::As<llvmo::Metadata_sp>(core::oCar(cvals))->wrappedPtr();
        _v.push_back(vP);
      }
      return;
    } else if (core::Vector_sp vvals = o.asOrNull<core::Vector_O>()) {
      _v.resize(vvals->length());
      for (int i(0), iEnd(vvals->length()); i < iEnd; ++i) {
        _v[i] = gc::As<llvmo::Metadata_sp>(vvals->rowMajorAref(i))->wrappedPtr();
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to llvm::ArrayRef<llvm::Metadata*>", core::_rep_(o).c_str());
  }
};

};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Metadata *> {
  static core::T_sp convert(llvm::Metadata *ptr) {
    _G();
    return ((llvmo::Metadata_O::create(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(User);
class User_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::User, User_O, "user", Value_O);
  typedef llvm::User ExternalType;
  typedef llvm::User *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  User_O() : Base(){};
  ~User_O() {}

}; // User_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
class Attribute_O : public core::General_O {
  LISP_CLASS(llvmo, LlvmoPkg, Attribute_O, "Attribute",core::General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(Attribute_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit Attribute_O() : T_O(), T(mc) {};
        //    virtual ~Attribute_O() {};
public:
private: // instance variables here
  llvm::Attribute _Attribute;

public: // Functions here
  static Attribute_sp get(LLVMContext_sp context, core::List_sp attribute_symbols);

  llvm::Attribute attributes() { return this->_Attribute; };
  void setAttribute(llvm::Attribute attr) { this->_Attribute = attr; };
}; // Attribute class

}; // llvmo namespace
namespace translate {
template <>
struct from_object<llvm::Attribute::AttrKind, std::true_type> {
  typedef llvm::Attribute::AttrKind DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) {
    _G();
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_AttributeEnum->symbolValue());
      this->_v = converter->enumForSymbol<llvm::Attribute::AttrKind>(sym);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::Attribute::AttrKind", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::Attribute, std::true_type> {
  typedef llvm::Attribute DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Attribute_sp>(object)->attributes()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Attribute> {
  static core::T_sp convert(llvm::Attribute attr) {
    _G();
    GC_ALLOCATE(llvmo::Attribute_O, oattr);
    oattr->setAttribute(attr);
    return ((oattr));
  }
};

};
    ;

namespace llvmo {
FORWARD(DataLayout);
 /*! DataLayout_O
As of llvm3.7 the llvm::DataLayout seems to be passed around as a simple object
and pointers to it are no longer required by functions or returned by functions.
So I'm changing DataLayout_O so that it wraps a complete llvm::DataLayout object
*/
class DataLayout_O : public core::General_O {
  LISP_CLASS(llvmo, LlvmoPkg, DataLayout_O, "DataLayout", core::General_O);
 protected:
  llvm::DataLayout* _DataLayout;
public:
  CL_LISPIFY_NAME("getStringRepresentation");
  CL_DEFMETHOD std::string getStringRepresentation() const { return this->_DataLayout->getStringRepresentation(); };
  size_t getTypeAllocSize(llvm::Type* ty);
  const llvm::DataLayout& dataLayout() { return *(this->_DataLayout); };
 DataLayout_O(const llvm::DataLayout& orig)  {
   this->_DataLayout = new llvm::DataLayout(orig);
  };
  /*! Delete the default constructor because llvm::DataLayout doesn't have one */
  DataLayout_O() = delete;
  ~DataLayout_O() {delete this->_DataLayout;}
  DataLayout_sp copy() const;

}; // DataLayout_O
}; // llvmo
/* from_object translators */

namespace translate {
  // Since llvm3.8 there don't appear to be functions that
  // take or return llvm::DataLayout* pointers.  So I am commenting out
  // their converters and I changed the DataLayout_O class to store a llvm::DataLayout
  template <>
    struct from_object<llvm::DataLayout const &, std::true_type> {
    typedef llvm::DataLayout const &DeclareType;
    DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DataLayout_sp>(object)->dataLayout()) {};
  };

  // ----------   to_object converters
  template <>
    struct to_object<const llvm::DataLayout &> {
    static core::T_sp convert(const llvm::DataLayout & ref) {
      // Use the copy constructor to create a DataLayout_O
      GC_ALLOCATE_VARIADIC(llvmo::DataLayout_O,val,ref);
      return val;
    }
  };

  /*! This copies the DataLayout so it doesn't deal with pointers at all */
  template <>
    struct to_object<llvm::DataLayout const, translate::dont_adopt_pointer> {
    static core::T_sp convert(llvm::DataLayout orig) {
      // Use the copy constructor to create a DataLayout_O
      GC_ALLOCATE_VARIADIC(llvmo::DataLayout_O,val,orig);
      return val;
    }
  };
};


namespace llvmo {
FORWARD(Constant);
class Constant_O : public User_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Constant, Constant_O, "constant", User_O);
  typedef llvm::Constant ExternalType;
  typedef llvm::Constant *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Constant_sp create(llvm::Constant *ptr);
  ;
  Constant_O() : Base(){};
  ~Constant_O() {}

}; // Constant_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Constant *> {
  static core::T_sp convert(llvm::Constant *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Constant_O, llvm::Constant *>(ptr)));
  };
};
template <>
struct from_object<llvm::Constant *, std::true_type> {
  typedef llvm::Constant *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Constant_sp>(object)->wrappedPtr()){};
};

template <>
struct from_object<llvm::ArrayRef<llvm::Constant *>> {
  typedef std::vector<llvm::Constant *> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      for (auto cvals : lcvals) {
        llvm::Constant *vP = gc::As<llvmo::Constant_sp>(core::oCar(cvals))->wrappedPtr();
        _v.push_back(vP);
      }
      return;
    } else if (core::Vector_sp vvals = o.asOrNull<core::Vector_O>()) {
      _v.resize(vvals->length());
      for (int i(0), iEnd(vvals->length()); i < iEnd; ++i) {
        _v[i] = gc::As<llvmo::Constant_sp>(vvals->rowMajorAref(i))->wrappedPtr();
      }
      return;
    }
    SIMPLE_ERROR_SPRINTF("Could not convert %s to llvm::ArrayRef<llvm::Constant*>", core::_rep_(o).c_str());
  }
};

};
    ;

namespace llvmo {
FORWARD(ConstantArray);
class ConstantArray_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantArray, ConstantArray_O, "constant-array", Constant_O);
  typedef llvm::ConstantArray ExternalType;
  typedef llvm::ConstantArray *PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    this->_ptr = ptr;
  }
  ConstantArray_O() : Base(){};
  virtual ~ConstantArray_O(){};

public:
  static Constant_sp get(ArrayType_sp type, core::List_sp values);
}; // ConstantArray_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(BlockAddress);
class BlockAddress_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::BlockAddress, BlockAddress_O, "BlockAddress", Constant_O);
  typedef llvm::BlockAddress ExternalType;
  typedef llvm::BlockAddress *PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    this->_ptr = ptr;
  }
  BlockAddress_O() : Base(){};
  virtual ~BlockAddress_O(){};

public:
  static BlockAddress_sp get(Function_sp func, BasicBlock_sp bb);
}; // BlockAddress_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(ConstantDataSequential);
class ConstantDataSequential_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantDataSequential, ConstantDataSequential_O, "ConstantDataSequential", Constant_O);
  typedef llvm::ConstantDataSequential ExternalType;
  typedef llvm::ConstantDataSequential *PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    this->_ptr = ptr;
  }
  ConstantDataSequential_O() : Base(){};
  virtual ~ConstantDataSequential_O(){};

public:
}; // ConstantDataSequential_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(ConstantDataArray);
class ConstantDataArray_O : public ConstantDataSequential_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantDataArray, ConstantDataArray_O, "constant-data-array", ConstantDataSequential_O);
  typedef llvm::ConstantDataArray ExternalType;
  typedef llvm::ConstantDataArray *PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    this->_ptr = ptr;
  }
  ConstantDataArray_O() : Base(){};
  virtual ~ConstantDataArray_O(){};

public:
  static Constant_sp getUInt32(LLVMContext_sp context, core::T_sp values);
}; // ConstantDataArray_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(ConstantExpr);
class ConstantExpr_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantExpr, ConstantExpr_O, "ConstantExpr", Constant_O);
  typedef llvm::ConstantExpr ExternalType;
  typedef llvm::ConstantExpr *PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    this->_ptr = ptr;
  }
  ConstantExpr_O() : Base(){};
  virtual ~ConstantExpr_O(){};

public:
  static Constant_sp getInBoundsGetElementPtr(llvm::Type* element_type, Constant_sp constant, core::List_sp idxList);

}; // ConstantExpr_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(GlobalValue);
class GlobalValue_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::GlobalValue, GlobalValue_O, "global-value", Constant_O);
  typedef llvm::GlobalValue ExternalType;
  typedef llvm::GlobalValue *PointerToExternalType;

private:
  bool _PtrIsOwned;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL && this->_PtrIsOwned)
      this->_ptr->deleteValue();
    this->_ptr = ptr;
  }
  void set_ptrIsOwned(bool b) { this->_PtrIsOwned = b; };
  GlobalValue_O() : Base(), _PtrIsOwned(false){};
  virtual ~GlobalValue_O() {
    if (this->_ptr != NULL && this->_PtrIsOwned)
      this->_ptr->deleteValue();
  }

}; // GlobalValue_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(GlobalVariable);
class GlobalVariable_O : public GlobalValue_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::GlobalVariable, GlobalVariable_O, "GlobalVariable", GlobalValue_O);
  typedef llvm::GlobalVariable ExternalType;
  typedef llvm::GlobalVariable *PointerToExternalType;

public:
  static GlobalVariable_sp make(Module_sp module, Type_sp type, bool isConstant, core::Symbol_sp linkage, /*Constant_sp*/ core::T_sp initializer, core::String_sp name, /*GlobalVariable_sp*/ core::T_sp insertBefore, core::Symbol_sp threadLocalMode);

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  GlobalVariable_O() : Base(){};
  virtual ~GlobalVariable_O() {}

}; // GlobalVariable_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::GlobalVariable *, std::true_type> {
  typedef llvm::GlobalVariable *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::GlobalVariable_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::GlobalVariable *> {
  static core::T_sp convert(llvm::GlobalVariable *ptr) {
    _G();
    if (ptr)
      return ((core::RP_Create_wrapped<llvmo::GlobalVariable_O, llvm::GlobalVariable *>(ptr)));
    return _Nil<core::T_O>();
  }
};
};
    ;

namespace llvmo {
FORWARD(ExecutionEngine);
class ExecutionEngine_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ExecutionEngine, ExecutionEngine_O, "EXECUTION-ENGINE", core::ExternalObject_O);
  friend class EngineBuilder_O;
  typedef llvm::ExecutionEngine ExternalType;
  typedef llvm::ExecutionEngine *PointerToExternalType;

  void initialize();

GCPROTECTED:
  PointerToExternalType _ptr;
  core::HashTableEqual_sp _DependentModules;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  string __repr__() const;

  ExecutionEngine_O() : Base(), _ptr(NULL){};
  ~ExecutionEngine_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

  void addModule(Module_sp module);
  bool removeModule(Module_sp module);

  Function_sp find_function_named(core::String_sp name);

  void addNamedModule(const string &name, Module_sp module);
  bool hasNamedModule(const string &name);
  void removeNamedModule(const string &name);
  core::List_sp dependentModuleNames() const;

  void addGlobalMapping(GlobalValue_sp value, core::Pointer_sp ptr);
  /*! Add a global mapping for an object, give it a new name and return the GlobalVariable_sp */
  void addGlobalMappingForLoadTimeValueVector(GlobalValue_sp value, const string &name);

  void runFunction(Function_sp func, core::String_sp fileName); //, core::Cons_sp args );
};                                                           // ExecutionEngine_O
};                                                           // llvmo

namespace llvmo {
  FORWARD(Module);
};


namespace llvmo {

  static size_t global_NextModuleId;
class Module_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Module, Module_O, "module", core::ExternalObject_O);
  typedef llvm::Module ExternalType;
  typedef llvm::Module *PointerToExternalType;
  void initialize();
GCPROTECTED:
  size_t _Id;
  PointerToExternalType _ptr;
  core::HashTableEqual_sp _UniqueGlobalVariableStrings;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    if ( this->_ptr ) return this->_ptr;
    SIMPLE_ERROR_SPRINTF("The Module has a NULL pointer");
  }
  void reset_wrappedPtr() {
    this->_ptr = NULL;
  }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
 Module_O() : Base(), _ptr(NULL), _Id(++global_NextModuleId) {};
  ~Module_O() {
    if (_ptr != NULL) {
      // delete _ptr;   // Don't delete the module Delete the module when it's not used
      _ptr = NULL;
    };
  }
  std::string __repr__() const;
  CL_DEFMETHOD size_t module_id() const { return this->_Id;};
  static Module_sp make(llvm::StringRef module_name, LLVMContext_sp context);
  /*! Return true if the wrapped Module is defined */
  bool valid() const;
  /*! Return a Cons of all the globals for this module */
  core::List_sp getGlobalList() const;

public:
  /*! Return a list of all functions as a cons */
  core::List_sp getFunctionList() const;

  /*! Wrap the Module::getFunction function */
  llvm::Function *getFunction(core::String_sp dispatchName);

  void emit_version_ident_metadata();
  
  /*! Get or create a string GlobalVariable with the given name.
	  Make sure that the string passed is the same as the string
	  in the GlobalVariable.
	I created this method to avoid lots of duplicate strings being
	created as global variables within the Module. */
  GlobalVariable_sp getOrCreateUniquedStringGlobalVariable(const string &value, const string &name);

  /*! Delete the module */
  void moduleDelete();

  /*! Get a Cons of all named MetaData */
  void dump_namedMDList() const;

}; // Module_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Module *, std::true_type> {
  typedef llvm::Module *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Module_sp>(object)->wrappedPtr()){};
};
template <>
struct from_object<llvm::Module &, std::true_type> {
  typedef llvm::Module &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::Module_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */
namespace translate {
template <>
struct to_object<llvm::Module *> {
  static core::T_sp convert(llvm::Module *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Module_O, llvm::Module *>(ptr)));
  }
};
};
    ;

/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::ExecutionEngine *, std::true_type> {
  typedef llvm::ExecutionEngine *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ExecutionEngine_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ExecutionEngine *> {
  static core::T_sp convert(llvm::ExecutionEngine *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ExecutionEngine_O, llvm::ExecutionEngine *>(ptr)));
  }
};
};
    ;


//
// This is needed for llvm3.7     What did I do before this?????
//
namespace llvmo {
FORWARD(TargetLibraryInfoWrapperPass);
class TargetLibraryInfoWrapperPass_O : public ImmutablePass_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetLibraryInfoWrapperPass, TargetLibraryInfoWrapperPass_O, "TargetLibraryInfoWrapperPass", ImmutablePass_O);
  typedef llvm::TargetLibraryInfoWrapperPass ExternalType;
  typedef llvm::TargetLibraryInfoWrapperPass *PointerToExternalType;

public:
  static TargetLibraryInfoWrapperPass_sp make(llvm::Triple * triple);

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    //	    if (this->_ptr != NULL ) delete this->_ptr;
    this->_ptr = ptr;
  }
  TargetLibraryInfoWrapperPass_O() : Base(){};
  ~TargetLibraryInfoWrapperPass_O() { /*if (this->_ptr) delete this->_ptr;*/
  }
}; // TargetLibraryInfoWrapperPass_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::TargetLibraryInfoWrapperPass *, std::true_type> {
  typedef llvm::TargetLibraryInfoWrapperPass *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.as<llvmo::TargetLibraryInfoWrapperPass_O>()->wrappedPtr()){};
};
template <>
struct from_object<llvm::TargetLibraryInfoWrapperPass const &, std::true_type> {
  typedef llvm::TargetLibraryInfoWrapperPass const &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(object.as<llvmo::TargetLibraryInfoWrapperPass_O>()->wrappedPtr())){};
};
template <>
struct to_object<llvm::TargetLibraryInfoWrapperPass *> {
  static core::T_sp convert(llvm::TargetLibraryInfoWrapperPass *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::TargetLibraryInfoWrapperPass_O, llvm::TargetLibraryInfoWrapperPass *>(ptr)));
  }
};
template <>
struct to_object<const llvm::TargetLibraryInfoWrapperPass *> {
  static core::T_sp convert(const llvm::TargetLibraryInfoWrapperPass *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::TargetLibraryInfoWrapperPass_O, llvm::TargetLibraryInfoWrapperPass *>(const_cast<llvm::TargetLibraryInfoWrapperPass *>(ptr))));
  }
};
};

namespace llvmo {
FORWARD(FunctionPassManager);
class FunctionPassManager_O : public PassManagerBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::legacy::FunctionPassManager, FunctionPassManager_O, "FUNCTION-PASS-MANAGER", PassManagerBase_O);
  typedef llvm::legacy::FunctionPassManager ExternalType;
  typedef llvm::legacy::FunctionPassManager *PointerToExternalType;

public:
  static FunctionPassManager_sp make(llvm::Module *module);

public:
  PointerToExternalType wrappedPtr() { return /*dynamic_*/reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL)
      delete this->_ptr;
    this->_ptr = ptr;
  }
  FunctionPassManager_O() : Base(){};
  ~FunctionPassManager_O() {
    //	    if ( this->_ptr!=NULL ) delete this->_ptr;
  }

public:
}; // FunctionPassManager_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
  struct from_object<llvm::legacy::FunctionPassManager *, std::true_type> {
  typedef llvm::legacy::FunctionPassManager *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::FunctionPassManager_sp>(object)->wrappedPtr(); };
};
template <>
  struct from_object<llvm::legacy::FunctionPassManager &, std::true_type> {
  typedef llvm::legacy::FunctionPassManager &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::FunctionPassManager_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
  struct to_object<llvm::legacy::FunctionPassManager *> {
  static core::T_sp convert(llvm::legacy::FunctionPassManager *ptr) {
    return ((core::RP_Create_wrapped<llvmo::FunctionPassManager_O, llvm::legacy::FunctionPassManager *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(PassManager);
class PassManager_O : public PassManagerBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::legacy::PassManager, PassManager_O, "PASS-MANAGER", PassManagerBase_O);
  typedef llvm::legacy::PassManager ExternalType;
  typedef llvm::legacy::PassManager *PointerToExternalType;

public:
  static PassManager_sp make();

public:
  PointerToExternalType wrappedPtr() { return /*dynamic_*/reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL)
      delete this->_ptr;
    this->_ptr = ptr;
  }
  PassManager_O() : Base(){};
  ~PassManager_O() {
    //	    if ( this->_ptr!=NULL ) { delete this->_ptr; this->_ptr = NULL; };
  }

public:
}; // PassManager_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
  struct from_object<llvm::legacy::PassManager *, std::true_type> {
  typedef llvm::legacy::PassManager *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::PassManager_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
  struct to_object<llvm::legacy::PassManager *> {
  static core::T_sp convert(llvm::legacy::PassManager *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::PassManager_O, llvm::legacy::PassManager *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(EngineBuilder);
class EngineBuilder_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::EngineBuilder, EngineBuilder_O, "ENGINEBUILDER", core::ExternalObject_O);
  typedef llvm::EngineBuilder ExternalType;
  typedef llvm::EngineBuilder *PointerToExternalType;

protected:
  PointerToExternalType _ptr;
  string _ErrorStr; // store creation errors here
public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
CL_LISPIFY_NAME("error_string");
CL_DEFMETHOD   string error_string() const { return this->_ErrorStr; };
 CL_DEFMETHOD void setUseOrcMCJITReplacement(bool use);

  EngineBuilder_O() : Base(), _ptr(NULL){};
  ~EngineBuilder_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }
  static EngineBuilder_sp make(Module_sp module);

  /*! Create the ExecutionEngine */
  ExecutionEngine_sp createExecutionEngine();

  /*! kind can be INTERPRETER or JIT */
  void setEngineKind(core::Symbol_sp kind);

  /*! Set the target options (see llvm:EngineBuilder::setTargetOptions).
	  Options are passed as a p-list with keyword/value pairs like :jitemit-debug-info t.
	*/
  void setTargetOptions(TargetOptions_sp targetOptions);

  /*! Set to use MCJIT */
  //	void setUseMCJIT(bool mcjit);

}; // EngineBuilder_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::EngineBuilder *, std::true_type> {
  typedef llvm::EngineBuilder *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    this->_v = (gc::As<llvmo::EngineBuilder_sp>(object)->wrappedPtr());
  };
};
};
    ;
/* to_object translators */

namespace llvmo {
FORWARD(PassManagerBuilder);
class PassManagerBuilder_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::PassManagerBuilder, PassManagerBuilder_O, "PASS-MANAGER-BUILDER", core::ExternalObject_O);
  typedef llvm::PassManagerBuilder ExternalType;
  typedef llvm::PassManagerBuilder *PointerToExternalType;

protected:
  PointerToExternalType _ptr;
  string _ErrorStr; // store creation errors here
public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  string error_string() const { return this->_ErrorStr; };

  PassManagerBuilder_O() : Base(), _ptr(NULL){};
  ~PassManagerBuilder_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }
  static PassManagerBuilder_sp make();

  /*! Create the ExecutionEngine */
  ExecutionEngine_sp createExecutionEngine();

  /*! kind can be INTERPRETER or JIT */
  void setEngineKind(core::Symbol_sp kind);

  /*! Set the target options (see llvm:PassManagerBuilder::setTargetOptions).
	  Options are passed as a p-list with keyword/value pairs like :jitemit-debug-info t.
	*/
  //	void setTargetOptions(TargetOptions_sp targetOptions);

  /*! Set to use MCJIT */
  void setUseMCJIT(bool mcjit);

}; // PassManagerBuilder_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
  struct from_object<llvm::PassManagerBuilder *, std::true_type> {
  typedef llvm::PassManagerBuilder *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    this->_v = (gc::As<llvmo::PassManagerBuilder_sp>(object)->wrappedPtr());
  };
};
};
    ;
/* to_object translators */

namespace llvmo {
FORWARD(APFloat);
class APFloat_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::APFloat, APFloat_O, "APFLOAT", core::ExternalObject_O);

public:
  typedef llvm::APFloat ExternalType;
  llvm::APFloat _value;

public:
  static APFloat_sp makeAPFloatFloat(core::SingleFloat_sp value);
  static APFloat_sp makeAPFloatDouble(core::DoubleFloat_sp value);

public:
  APFloat_O() : Base(), _value(0.0){};
  ~APFloat_O(){};
}; // APFloat_O
}; // llvmo
namespace translate {
template <>
struct from_object<const llvm::APFloat &, std::true_type> {
  typedef llvm::APFloat DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::APFloat_sp>(object)->_value){};
};
};

/* to_object translators */

namespace llvmo {
FORWARD(APInt);
class APInt_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::APInt, APInt_O, "APINT", core::ExternalObject_O);

public:
  typedef llvm::APInt ExternalType;
  llvm::APInt _value;

public:
  static APInt_sp create(llvm::APInt i);
  /*! Return an APInt that has the value of (val) */
  static APInt_sp makeAPInt(core::Integer_sp val);
  static APInt_sp makeAPIntWidth(core::Integer_sp val, uint bitwidth, bool sign);
  static APInt_sp makeAPInt1(core::T_sp val);
  static APInt_sp makeAPInt32(core::Integer_sp val);
  static APInt_sp makeAPInt64(core::Integer_sp val);

public:
  string toString(int radix, bool isigned) const;
  core::Integer_sp toInteger(bool issigned) const;
  APInt_O() : Base(){};
  ~APInt_O(){};

public:
  string __repr__() const;
}; // APInt_O
}; // llvmo
/* from_object translators */
namespace translate {
template <>
struct from_object<const llvm::APInt &, std::true_type> {
  typedef llvm::APInt DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::APInt_sp>(object)->_value){};
};
/* to_object translators */
template <>
struct to_object<llvm::APInt> {
  static core::T_sp convert(llvm::APInt sr) { return llvmo::APInt_O::create(sr); };
};
};

namespace llvmo {
FORWARD(IRBuilderBase);
class IRBuilderBase_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IRBuilderBase, IRBuilderBase_O, "IRBuilderBase", core::ExternalObject_O);
  typedef llvm::IRBuilderBase ExternalType;
  typedef llvm::IRBuilderBase *PointerToExternalType;

protected:
  PointerToExternalType _ptr;
  bool _CurrentDebugLocationSet;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static IRBuilderBase_sp create(llvm::IRBuilderBase *ptr);
  core::T_sp getInsertPointInstruction();
  IRBuilderBase_O() : Base(), _ptr(NULL), _CurrentDebugLocationSet(false){};
  ~IRBuilderBase_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

public:
  void restoreIP(InsertPoint_sp insertPoint);
  InsertPoint_sp saveIP();

  void ClearCurrentDebugLocation();

  /*! Set the current debug location for generated code */
  void SetCurrentDebugLocation(DebugLoc_sp loc);
  /*! Set the current debug location by building a DebugLoc on the fly */
  void SetCurrentDebugLocationToLineColumnScope(int line, int col, DINode_sp scope);
CL_LISPIFY_NAME("CurrentDebugLocation");
CL_DEFMETHOD   core::T_sp CurrentDebugLocation() { return _lisp->_boolean(this->_CurrentDebugLocationSet); };
}; // IRBuilderBase_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::IRBuilderBase *, std::true_type> {
  typedef llvm::IRBuilderBase *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::IRBuilderBase_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::IRBuilderBase *> {
  static core::T_sp convert(llvm::IRBuilderBase *ptr) {
    _G();
    return ((llvmo::IRBuilderBase_O::create(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(IRBuilder);
class IRBuilder_O : public IRBuilderBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IRBuilder<>, IRBuilder_O, "IRBUILDER", IRBuilderBase_O);
  typedef llvm::IRBuilder<> ExternalType;
  typedef llvm::IRBuilder<> *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  IRBuilder_O() : Base(){};
  ~IRBuilder_O() {}
  static IRBuilder_sp make(LLVMContext_sp context);

public:
  llvm::InvokeInst *CreateInvoke(llvm::Value *Callee, llvm::BasicBlock *NormalDest, llvm::BasicBlock *UnwindDest, core::List_sp Args, const llvm::Twine &Name = "");
  llvm::Value* CreateConstGEP2_32(llvm::Type* ty, llvm::Value *ptr, int idx0, int idx1, const llvm::Twine &Name);
  llvm::Value* CreateConstGEP2_64(llvm::Value *Ptr, size_t idx0, size_t idx1, const llvm::Twine &Name);
  llvm::Value *CreateInBoundsGEP(llvm::Value *Ptr, core::List_sp IdxList, const llvm::Twine &Name = "");

  llvm::Value *CreateExtractValue(llvm::Value *Ptr, core::List_sp IdxList, const llvm::Twine &Name = "");

  llvm::Value *CreateInsertValue(llvm::Value *Agg, llvm::Value *Val, core::List_sp IdxList, const llvm::Twine &Name = "");

  string __repr__() const;
}; // IRBuilder_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(Instruction);

class Instruction_O : public User_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Instruction, Instruction_O, "Instruction", User_O);
  typedef llvm::Instruction ExternalType;
  typedef llvm::Instruction *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  core::T_sp getNextNode(); // instruction or nil
  core::T_sp getPrevNode(); // instruction or nil
  core::T_sp getParent(); // basic block or nil
  CL_DEFMETHOD bool CallInstP() const { return llvm::isa<llvm::CallInst>(this->wrappedPtr()); };
  CL_DEFMETHOD bool InvokeInstP() const { return llvm::isa<llvm::InvokeInst>(this->wrappedPtr()); };
  Instruction_O() : Base(){};
  ~Instruction_O() {}

public:
  void setMetadata(core::String_sp kind, MDNode_sp mdnode);

  bool terminatorInstP() const;
}; // Instruction_O
}; // llvmo
namespace translate {
template <>
struct from_object<llvm::Instruction *, std::true_type> {
  typedef llvm::Instruction *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::Instruction_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Instruction *> {
  static core::T_sp convert(llvm::Instruction *ptr) {
    // Wrap the Instruction* using the most derived class possible
    if (llvm::isa<llvm::CallInst>(ptr)) {
      return core::RP_Create_wrapped<llvmo::CallInst_O,llvm::CallInst*>(reinterpret_cast<llvm::CallInst*>(ptr));
    } else if (llvm::isa<llvm::InvokeInst>(ptr)) {
      return core::RP_Create_wrapped<llvmo::InvokeInst_O,llvm::InvokeInst*>(reinterpret_cast<llvm::InvokeInst*>(ptr));
    }
    return ((core::RP_Create_wrapped<llvmo::Instruction_O, llvm::Instruction *>(ptr)));
  }
};
};
    ;

/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(StoreInst);
class StoreInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::StoreInst, StoreInst_O, "StoreInst", Instruction_O);
  typedef llvm::StoreInst ExternalType;
  typedef llvm::StoreInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  StoreInst_O() : Base(){};
  ~StoreInst_O() {}

}; // StoreInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::StoreInst *, std::true_type> {
  typedef llvm::StoreInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::StoreInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::StoreInst *> {
  static core::T_sp convert(llvm::StoreInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::StoreInst_O, llvm::StoreInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(FenceInst);
class FenceInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::FenceInst, FenceInst_O, "FenceInst", Instruction_O);
  typedef llvm::FenceInst ExternalType;
  typedef llvm::FenceInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  FenceInst_O() : Base(){};
  ~FenceInst_O() {}

}; // FenceInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::FenceInst *, std::true_type> {
  typedef llvm::FenceInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::FenceInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::FenceInst *> {
  static core::T_sp convert(llvm::FenceInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::FenceInst_O, llvm::FenceInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(AtomicCmpXchgInst);
class AtomicCmpXchgInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AtomicCmpXchgInst, AtomicCmpXchgInst_O, "AtomicCmpXchgInst", Instruction_O);
  typedef llvm::AtomicCmpXchgInst ExternalType;
  typedef llvm::AtomicCmpXchgInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  AtomicCmpXchgInst_O() : Base(){};
  ~AtomicCmpXchgInst_O() {}

}; // AtomicCmpXchgInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::AtomicCmpXchgInst *, std::true_type> {
  typedef llvm::AtomicCmpXchgInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::AtomicCmpXchgInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::AtomicCmpXchgInst *> {
  static core::T_sp convert(llvm::AtomicCmpXchgInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::AtomicCmpXchgInst_O, llvm::AtomicCmpXchgInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(AtomicRMWInst);
class AtomicRMWInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AtomicRMWInst, AtomicRMWInst_O, "AtomicRMWInst", Instruction_O);
  typedef llvm::AtomicRMWInst ExternalType;
  typedef llvm::AtomicRMWInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  AtomicRMWInst_O() : Base(){};
  ~AtomicRMWInst_O() {}

}; // AtomicRMWInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::AtomicRMWInst *, std::true_type> {
  typedef llvm::AtomicRMWInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::AtomicRMWInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::AtomicRMWInst *> {
  static core::T_sp convert(llvm::AtomicRMWInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::AtomicRMWInst_O, llvm::AtomicRMWInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(PHINode);
class PHINode_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::PHINode, PHINode_O, "PHINode", Instruction_O);
  typedef llvm::PHINode ExternalType;
  typedef llvm::PHINode *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  PHINode_O() : Base(){};
  ~PHINode_O() {}

}; // PHINode_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::PHINode *, std::true_type> {
  typedef llvm::PHINode *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::PHINode_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::PHINode *> {
  static core::T_sp convert(llvm::PHINode *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::PHINode_O, llvm::PHINode *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(CallInst);
class CallInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::CallInst, CallInst_O, "CallInst", Instruction_O);
  typedef llvm::CallInst ExternalType;
  typedef llvm::CallInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void 	addParamAttr(unsigned ArgNo, llvm::Attribute::AttrKind Attr);
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  core::List_sp getArgumentList() const;
  llvm::Function* getCalledFunction();
  CL_DEFMETHOD bool CallInstP() const { return true; };
  CallInst_O() : Base(){};
  ~CallInst_O() {}

}; // CallInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::CallInst *, std::true_type> {
  typedef llvm::CallInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::CallInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::CallInst *> {
  static core::T_sp convert(llvm::CallInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::CallInst_O, llvm::CallInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(LandingPadInst);
class LandingPadInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LandingPadInst, LandingPadInst_O, "LandingPadInst", Instruction_O);
  typedef llvm::LandingPadInst ExternalType;
  typedef llvm::LandingPadInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  LandingPadInst_O() : Base(){};
  ~LandingPadInst_O() {}

}; // LandingPadInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::LandingPadInst *, std::true_type> {
  typedef llvm::LandingPadInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::LandingPadInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::LandingPadInst *> {
  static core::T_sp convert(llvm::LandingPadInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::LandingPadInst_O, llvm::LandingPadInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(UnaryInstruction);
class UnaryInstruction_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::UnaryInstruction, UnaryInstruction_O, "UnaryInstruction", Instruction_O);
  typedef llvm::UnaryInstruction ExternalType;
  typedef llvm::UnaryInstruction *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  UnaryInstruction_O() : Base(){};
  ~UnaryInstruction_O() {}

}; // UnaryInstruction_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(AllocaInst);
class AllocaInst_O : public UnaryInstruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AllocaInst, AllocaInst_O, "AllocaInst", UnaryInstruction_O);
  typedef llvm::AllocaInst ExternalType;
  typedef llvm::AllocaInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  AllocaInst_O() : Base(){};
  ~AllocaInst_O() {}
}; // AllocaInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::AllocaInst *, std::true_type> {
  typedef llvm::AllocaInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::AllocaInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::AllocaInst *> {
  static core::T_sp convert(llvm::AllocaInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::AllocaInst_O, llvm::AllocaInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(VAArgInst);
class VAArgInst_O : public UnaryInstruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::VAArgInst, VAArgInst_O, "VAArgInst", UnaryInstruction_O);
  typedef llvm::VAArgInst ExternalType;
  typedef llvm::VAArgInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  VAArgInst_O() : Base(){};
  ~VAArgInst_O() {}

}; // VAArgInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::VAArgInst *, std::true_type> {
  typedef llvm::VAArgInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::VAArgInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::VAArgInst *> {
  static core::T_sp convert(llvm::VAArgInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::VAArgInst_O, llvm::VAArgInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(LoadInst);
class LoadInst_O : public UnaryInstruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LoadInst, LoadInst_O, "LoadInst", UnaryInstruction_O);
  typedef llvm::LoadInst ExternalType;
  typedef llvm::LoadInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  LoadInst_O() : Base(){};
  ~LoadInst_O() {}

}; // LoadInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::LoadInst *, std::true_type> {
  typedef llvm::LoadInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::LoadInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::LoadInst *> {
  static core::T_sp convert(llvm::LoadInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::LoadInst_O, llvm::LoadInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(TerminatorInst);
class TerminatorInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TerminatorInst, TerminatorInst_O, "TerminatorInst", Instruction_O);
  typedef llvm::TerminatorInst ExternalType;
  typedef llvm::TerminatorInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  TerminatorInst_O() : Base(){};
  ~TerminatorInst_O() {}

}; // TerminatorInst_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(BranchInst);
class BranchInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::BranchInst, BranchInst_O, "BranchInst", TerminatorInst_O);
  typedef llvm::BranchInst ExternalType;
  typedef llvm::BranchInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  BranchInst_O() : Base(){};
  ~BranchInst_O() {}

}; // BranchInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::BranchInst *, std::true_type> {
  typedef llvm::BranchInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::BranchInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::BranchInst *> {
  static core::T_sp convert(llvm::BranchInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::BranchInst_O, llvm::BranchInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(SwitchInst);
class SwitchInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::SwitchInst, SwitchInst_O, "SwitchInst", TerminatorInst_O);
  typedef llvm::SwitchInst ExternalType;
  typedef llvm::SwitchInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  SwitchInst_O() : Base(){};
  ~SwitchInst_O() {}

  void addCase(ConstantInt_sp onVal, BasicBlock_sp dest);

}; // SwitchInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::SwitchInst *, std::true_type> {
  typedef llvm::SwitchInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::SwitchInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::SwitchInst *> {
  static core::T_sp convert(llvm::SwitchInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::SwitchInst_O, llvm::SwitchInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(IndirectBrInst);
class IndirectBrInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IndirectBrInst, IndirectBrInst_O, "IndirectBrInst", TerminatorInst_O);
  typedef llvm::IndirectBrInst ExternalType;
  typedef llvm::IndirectBrInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  IndirectBrInst_O() : Base(){};
  ~IndirectBrInst_O() {}

}; // IndirectBrInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::IndirectBrInst *, std::true_type> {
  typedef llvm::IndirectBrInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::IndirectBrInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::IndirectBrInst *> {
  static core::T_sp convert(llvm::IndirectBrInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::IndirectBrInst_O, llvm::IndirectBrInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(InvokeInst);
class InvokeInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::InvokeInst, InvokeInst_O, "InvokeInst", TerminatorInst_O);
  typedef llvm::InvokeInst ExternalType;
  typedef llvm::InvokeInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void 	addParamAttr(unsigned ArgNo, llvm::Attribute::AttrKind Attr);
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  core::List_sp getArgumentList() const;
  llvm::Function* getCalledFunction();
  CL_DEFMETHOD bool InvokeInstP() const { return true; };
  InvokeInst_O() : Base(){};
  ~InvokeInst_O() {}

}; // InvokeInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::InvokeInst *, std::true_type> {
  typedef llvm::InvokeInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::InvokeInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::InvokeInst *> {
  static core::T_sp convert(llvm::InvokeInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::InvokeInst_O, llvm::InvokeInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ResumeInst);
class ResumeInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ResumeInst, ResumeInst_O, "ResumeInst", TerminatorInst_O);
  typedef llvm::ResumeInst ExternalType;
  typedef llvm::ResumeInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  ResumeInst_O() : Base(){};
  ~ResumeInst_O() {}

}; // ResumeInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::ResumeInst *, std::true_type> {
  typedef llvm::ResumeInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::ResumeInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ResumeInst *> {
  static core::T_sp convert(llvm::ResumeInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ResumeInst_O, llvm::ResumeInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(UnreachableInst);
class UnreachableInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::UnreachableInst, UnreachableInst_O, "UnreachableInst", TerminatorInst_O);
  typedef llvm::UnreachableInst ExternalType;
  typedef llvm::UnreachableInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  UnreachableInst_O() : Base(){};
  ~UnreachableInst_O() {}

}; // UnreachableInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::UnreachableInst *, std::true_type> {
  typedef llvm::UnreachableInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::UnreachableInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::UnreachableInst *> {
  static core::T_sp convert(llvm::UnreachableInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::UnreachableInst_O, llvm::UnreachableInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ReturnInst);
class ReturnInst_O : public TerminatorInst_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ReturnInst, ReturnInst_O, "ReturnInst", TerminatorInst_O);
  typedef llvm::ReturnInst ExternalType;
  typedef llvm::ReturnInst *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  ReturnInst_O() : Base(){};
  ~ReturnInst_O() {}

}; // ReturnInst_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::ReturnInst *, std::true_type> {
  typedef llvm::ReturnInst *DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::ReturnInst_sp>(object)->wrappedPtr(); };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ReturnInst *> {
  static core::T_sp convert(llvm::ReturnInst *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ReturnInst_O, llvm::ReturnInst *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ConstantFP);
class ConstantFP_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantFP, ConstantFP_O, "ConstantFP", Constant_O);
  typedef llvm::ConstantFP ExternalType;
  typedef llvm::ConstantFP *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantFP_sp create(llvm::ConstantFP *ptr);
  ;
  ConstantFP_O() : Base(){};
  ~ConstantFP_O() {}

public:
  string __repr__() const;

}; // ConstantFP_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ConstantFP *> {
  static core::T_sp convert(llvm::ConstantFP *ptr) {
    _G();
    return ((llvmo::ConstantFP_O::create(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ConstantInt);
class ConstantInt_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantInt, ConstantInt_O, "ConstantInt", Constant_O);
  typedef llvm::ConstantInt ExternalType;
  typedef llvm::ConstantInt *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantInt_sp create(llvm::ConstantInt *ptr);
  ;
  ConstantInt_O() : Base(){};
  ~ConstantInt_O() {}

public:
  string __repr__() const;
}; // ConstantInt_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ConstantInt *> {
  static core::T_sp convert(llvm::ConstantInt *ptr) {
    _G();
    return ((llvmo::ConstantInt_O::create(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ConstantStruct);
class ConstantStruct_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantStruct, ConstantStruct_O, "ConstantStruct", Constant_O);
  typedef llvm::ConstantStruct ExternalType;
  typedef llvm::ConstantStruct *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantStruct_sp create(llvm::ConstantStruct *ptr);
  ;
  ConstantStruct_O() : Base(){};
  ~ConstantStruct_O() {}

public:
}; // ConstantStruct_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ConstantStruct *> {
  static core::T_sp convert(llvm::ConstantStruct *ptr) {
    _G();
    return ((llvmo::ConstantStruct_O::create(ptr)));
  }
};

};
    ;

namespace llvmo {
FORWARD(UndefValue);
class UndefValue_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::UndefValue, UndefValue_O, "UndefValue", Constant_O);
  typedef llvm::UndefValue ExternalType;
  typedef llvm::UndefValue *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static UndefValue_sp create(llvm::UndefValue *ptr);
  ;
  UndefValue_O() : Base(){};
  ~UndefValue_O() {}

public:
  string __repr__() const;
}; // UndefValue_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::UndefValue *> {
  static core::T_sp convert(llvm::UndefValue *ptr) {
    _G();
    return ((llvmo::UndefValue_O::create(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ConstantPointerNull);
class ConstantPointerNull_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantPointerNull, ConstantPointerNull_O, "ConstantPointerNull", Constant_O);
  typedef llvm::ConstantPointerNull ExternalType;
  typedef llvm::ConstantPointerNull *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantPointerNull_sp create(llvm::ConstantPointerNull *ptr);
  ;
  ConstantPointerNull_O() : Base(){};
  ~ConstantPointerNull_O() {}

public:
  string __repr__() const;
}; // ConstantPointerNull_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ConstantPointerNull *> {
  static core::T_sp convert(llvm::ConstantPointerNull *ptr) {
    _G();
    return ((llvmo::ConstantPointerNull_O::create(ptr)));
  }
};

template <>
struct from_object<llvm::ConstantPointerNull *, std::true_type> {
  typedef llvm::ConstantPointerNull *DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ConstantPointerNull_sp>(object)->wrappedPtr()){};
};

};
    ;

namespace llvmo {
FORWARD(MDNode);
class MDNode_O : public Metadata_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MDNode, MDNode_O, "MDNode", Metadata_O);
  typedef llvm::MDNode ExternalType;
  typedef llvm::MDNode *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  MDNode_O() : Base(){};
  ~MDNode_O() {}

public:
  static MDNode_sp get(LLVMContext_sp context, core::List_sp values);

}; // MDNode_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::MDNode *, std::true_type> {
  typedef llvm::MDNode *DeclareType;
  DeclareType _v;
  from_object(T_P o) : _v(o.nilp() ? NULL : gc::As<llvmo::MDNode_sp>(o)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::MDNode *> {
  static llvmo::MDNode_mv convert(llvm::MDNode *ptr) {
    _G();
    return (Values(core::RP_Create_wrapped<llvmo::MDNode_O, llvm::MDNode *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(MDString);
class MDString_O : public Metadata_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MDString, MDString_O, "MDString", Metadata_O);
  typedef llvm::MDString ExternalType;
  typedef llvm::MDString *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  MDString_O() : Base(){};
  ~MDString_O() {}

public:
  static MDString_sp get(LLVMContext_sp context, core::String_sp str);

}; // MDString_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::MDString *, std::true_type> {
  typedef llvm::MDString *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = gc::As<llvmo::MDString_sp>(object)->wrappedPtr();
  };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::MDString *> {
  static core::T_sp convert(llvm::MDString *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::MDString_O, llvm::MDString *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(ValueAsMetadata);
class ValueAsMetadata_O : public Metadata_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ValueAsMetadata, ValueAsMetadata_O, "ValueAsMetadata", Metadata_O);
  typedef llvm::ValueAsMetadata ExternalType;
  typedef llvm::ValueAsMetadata *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  ValueAsMetadata_O() : Base(){};
  ~ValueAsMetadata_O() {}

public:
  static ValueAsMetadata_sp get(Value_sp val);

}; // ValueAsMetadata_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::ValueAsMetadata *, std::true_type> {
  typedef llvm::ValueAsMetadata *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = gc::As<llvmo::ValueAsMetadata_sp>(object)->wrappedPtr();
  };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ValueAsMetadata *> {
  static core::T_sp convert(llvm::ValueAsMetadata *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ValueAsMetadata_O, llvm::ValueAsMetadata *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(NamedMDNode);
class NamedMDNode_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::NamedMDNode, NamedMDNode_O, "NamedMDNode", core::ExternalObject_O);
  typedef llvm::NamedMDNode ExternalType;
  typedef llvm::NamedMDNode *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  NamedMDNode_O() : Base(){};
  ~NamedMDNode_O() {}

public:
  llvm::MDNode *getOperand(uint i) { return this->_ptr->getOperand(i); };
  uint getNumOperands() { return this->_ptr->getNumOperands(); };
CL_LISPIFY_NAME("addOperand");
CL_DEFMETHOD   void addOperand(llvm::MDNode *m) { this->_ptr->addOperand(m); };
  string getName() { return this->_ptr->getName(); };

}; // NamedMDNode_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::NamedMDNode *, std::true_type> {
  typedef llvm::NamedMDNode *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = object.nilp() ? NULL : gc::As<llvmo::NamedMDNode_sp>(object)->wrappedPtr();
  };
};
};
    ;
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::NamedMDNode *> {
  static llvmo::NamedMDNode_mv convert(llvm::NamedMDNode *ptr) {
    _G();
    return (Values(core::RP_Create_wrapped<llvmo::NamedMDNode_O, llvm::NamedMDNode *>(ptr)));
  }
};
};
    ;

namespace llvmo {
FORWARD(Function);
class Function_O : public GlobalValue_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Function, Function_O, "FUNCTION", GlobalValue_O);
  typedef llvm::Function ExternalType;
  typedef llvm::Function *PointerToExternalType;

GCPRIVATE:
  core::LoadTimeValues_sp _RunTimeValues;

public:
#if 0
  /*! If a Function is compiled with COMPILE then quoted values and literals need to be stored
	  somewhere.  We store them in a LoadTimeValue array and associate it with the Function
	  so that if the Function is destructed then the LoadTimeValues get destructed as well */
  void setLiterals(core::LoadTimeValues_sp ltv);
  core::LoadTimeValues_sp literals() const;
#endif
public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  string __repr__() const;
  Function_O() : Base(){};
  ~Function_O() {}

  bool equal(core::T_sp obj) const;

  void addReturnAttr(typename llvm::Attribute::AttrKind);
  core::List_sp getArgumentList();
  void appendBasicBlock(BasicBlock_sp basicBlock);
  BasicBlock_sp getEntryBlock() const;
  core::List_sp basic_blocks() const;
}; // Function_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Function *> {
  static core::T_sp convert(llvm::Function *ptr) {
    _G();
    if (ptr == NULL)
      return ((_Nil<core::T_O>()));
    return ((core::RP_Create_wrapped<llvmo::Function_O, llvm::Function *>(ptr)));
  };
};

template <>
struct to_object<const llvm::Function &> {
  static core::T_sp convert(const llvm::Function &val) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Function_O, llvm::Function *>(const_cast<llvm::Function *>(&val))));
  };
};

template <>
struct from_object<llvm::Function *, std::true_type> {
  typedef llvm::Function *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.nilp()) {
      this->_v = NULL;
    } else {
      this->_v = static_cast<llvm::Function *>(gc::As<llvmo::Function_sp>(object)->wrappedPtr());
    }
  }
};
template <>
struct from_object<const llvm::Function &, std::true_type> {
  typedef llvm::Function const &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(static_cast<llvm::Function *>(gc::As<llvmo::Function_sp>(object)->wrappedPtr()))){};
};
template <>
struct from_object<llvm::Function &, std::true_type> {
  typedef llvm::Function &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(static_cast<llvm::Function *>(gc::As<llvmo::Function_sp>(object)->wrappedPtr()))){};
};
};

namespace llvmo {
FORWARD(BasicBlock);
class BasicBlock_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::BasicBlock, BasicBlock_O, "BasicBlock", Value_O);
  typedef llvm::BasicBlock ExternalType;
  typedef llvm::BasicBlock *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  BasicBlock_O() : Base(){};
  ~BasicBlock_O() {}

  bool empty();
  size_t size();
  Instruction_sp back();

  core::List_sp instructions() const;

}; // BasicBlock_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::BasicBlock *> {
  static core::T_sp convert(llvm::BasicBlock *ptr) {
    _G();
    if (ptr != NULL) {
      return ((core::RP_Create_wrapped<llvmo::BasicBlock_O, llvm::BasicBlock *>(ptr)));
    }
    return ((_Nil<core::T_O>()));
  };
};
template <>
struct from_object<llvm::BasicBlock *, std::true_type> {
  typedef llvm::BasicBlock *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = object.nilp() ? NULL : static_cast<llvm::BasicBlock *>(gc::As<llvmo::BasicBlock_sp>(object)->wrappedPtr());
  }
};
};

namespace llvmo {
FORWARD(Argument);
class Argument_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Argument, Argument_O, "Argument", Value_O);
  typedef llvm::Argument ExternalType;
  typedef llvm::Argument *PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  Argument_O() : Base(){};
  ~Argument_O() {}

  void addAttr(llvm::Attribute a);

public:
}; // Argument_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::Argument *> {
  static core::T_sp convert(llvm::Argument *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Argument_O, llvm::Argument *>(ptr)));
  };
};
template <>
struct to_object<llvm::Argument> {
  static core::T_sp convert(llvm::Argument &arg) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Argument_O, llvm::Argument *>(&arg)));
  };
};
};

namespace llvmo {
FORWARD(Type);
class Type_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Type, Type_O, "TYPE", core::ExternalObject_O);
  typedef llvm::Type ExternalType;
  typedef llvm::Type *PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void *externalObject() const {
    return this->_ptr;
  };
  PointerToExternalType wrappedPtr() const {
    return this->_ptr;
  }

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Type_sp create(llvm::Type *ptr);
  ;
  Type_O() : Base(), _ptr(NULL){};
  ~Type_O() {
    if (_ptr != NULL) { /* delete _ptr;*/
      _ptr = NULL;
    };
  }

  core::Integer_sp getArrayNumElements() const;

  PointerType_sp getPointerTo(int addressSpace = 0);

  bool equal(core::T_sp obj) const;
  LLVMContext_sp getContext() const;
  string __repr__() const;

}; // Type_O
}; // llvmo
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::Type *, std::true_type> {
  typedef llvm::Type *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if ( object.nilp() ) {
      this->_v = NULL;
      return;
    }
    this->_v = (gc::As<llvmo::Type_sp>(object)->wrappedPtr());
  };
};

/* to_object translators */
template <>
struct to_object<llvm::Type *> {
  static core::T_sp convert(llvm::Type *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::Type_O, llvm::Type *>(ptr)));
  };
};
};
    ;

namespace llvmo {
FORWARD(FunctionType);
class FunctionType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::FunctionType, FunctionType_O, "FUNCTION-TYPE", Type_O);
  typedef llvm::FunctionType ExternalType;
  typedef llvm::FunctionType *PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  FunctionType_O() : Base(){};
  ~FunctionType_O() {}

public: // static methods
  static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
}; // FunctionType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::FunctionType *> {
  static core::T_sp convert(llvm::FunctionType *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::FunctionType_O, llvm::FunctionType *>(ptr)));
  };
};
template <>
struct from_object<llvm::FunctionType *, std::true_type> {
  typedef llvm::FunctionType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::FunctionType *>(gc::As<llvmo::FunctionType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(IntegerType);
class IntegerType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IntegerType, IntegerType_O, "INTEGER-TYPE", Type_O);
  typedef llvm::IntegerType ExternalType;
  typedef llvm::IntegerType *PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  IntegerType_O() : Base(){};
  ~IntegerType_O() {}

public: // static methods
  static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
}; // IntegerType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::IntegerType *> {
  static core::T_sp convert(llvm::IntegerType *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::IntegerType_O, llvm::IntegerType *>(ptr)));
  };
};
template <>
struct from_object<llvm::IntegerType *, std::true_type> {
  typedef llvm::IntegerType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::IntegerType *>(gc::As<llvmo::IntegerType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(CompositeType);
class CompositeType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::CompositeType, CompositeType_O, "CompositeType", Type_O);
  typedef llvm::CompositeType ExternalType;
  typedef llvm::CompositeType *PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  CompositeType_O() : Base(){};
  ~CompositeType_O() {}

public: // static methods
  static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
}; // CompositeType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::CompositeType *> {
  static core::T_sp convert(llvm::CompositeType *ptr) {
    return ((core::RP_Create_wrapped<llvmo::CompositeType_O, llvm::CompositeType *>(ptr)));
  };
};
template <>
struct from_object<llvm::CompositeType *, std::true_type> {
  typedef llvm::CompositeType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::CompositeType *>(gc::As<llvmo::CompositeType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(StructType);
class StructType_O : public CompositeType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::StructType, StructType_O, "StructType", CompositeType_O);
  typedef llvm::StructType ExternalType;
  typedef llvm::StructType *PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  StructType_O() : Base(){};
  ~StructType_O() {}

public: // static methods
  /*! Get a structure using llvm:StructType::create(LLVMContext& context, ArrayRef<Type*>Elements,StringRef name,bool isPacked) */
  static StructType_sp make(LLVMContext_sp context, core::T_sp elements, core::String_sp name, core::T_sp isPacked);

  static StructType_sp get(LLVMContext_sp context, core::T_sp elements, bool isPacked = false);

public:
  void setBody(core::T_sp elements, core::T_sp isPacked);
}; // StructType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::StructType *> {
  static llvmo::StructType_mv convert(llvm::StructType *ptr) {
    _G();
    return (Values(core::RP_Create_wrapped<llvmo::StructType_O, llvm::StructType *>(ptr)));
  };
};
template <>
struct from_object<llvm::StructType *, std::true_type> {
  typedef llvm::StructType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::StructType *>(gc::As<llvmo::StructType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(SequentialType);
class SequentialType_O : public CompositeType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::SequentialType, SequentialType_O, "SequentialType", CompositeType_O);
  typedef llvm::SequentialType ExternalType;
  typedef llvm::SequentialType *PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  SequentialType_O() : Base(){};
  ~SequentialType_O() {}

public: // static methods
  static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
}; // SequentialType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::SequentialType *> {
  static core::T_sp convert(llvm::SequentialType *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::SequentialType_O, llvm::SequentialType *>(ptr)));
  };
};
template <>
struct from_object<llvm::SequentialType *, std::true_type> {
  typedef llvm::SequentialType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::SequentialType *>(gc::As<llvmo::SequentialType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(PointerType);
class PointerType_O : public SequentialType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::PointerType, PointerType_O, "PointerType", SequentialType_O);
  typedef llvm::PointerType ExternalType;
  typedef llvm::PointerType *PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  PointerType_O() : Base(){};
  ~PointerType_O() {}

public: // static methods
        //	static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
public: // static methods
  static PointerType_sp get(Type_sp elementType, uint addressSpace);

}; // PointerType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::PointerType *> {
  static llvmo::PointerType_mv convert(llvm::PointerType *ptr) {
    _G();
    return (Values(core::RP_Create_wrapped<llvmo::PointerType_O, llvm::PointerType *>(ptr)));
  };
};
template <>
struct from_object<llvm::PointerType *, std::true_type> {
  typedef llvm::PointerType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::PointerType *>(gc::As<llvmo::PointerType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(ArrayType);
class ArrayType_O : public SequentialType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ArrayType, ArrayType_O, "ArrayType", SequentialType_O);
  typedef llvm::ArrayType ExternalType;
  typedef llvm::ArrayType *ArrayToExternalType;

public:
  ArrayToExternalType wrapped() { return static_cast<ArrayToExternalType>(this->_ptr); };
  void set_wrapped(ArrayToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  ArrayType_O() : Base(){};
  ~ArrayType_O() {}

public: // static methods
  static ArrayType_sp get(Type_sp elementType, uint64_t numElements);
}; // ArrayType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::ArrayType *> {
  static core::T_sp convert(llvm::ArrayType *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::ArrayType_O, llvm::ArrayType *>(ptr)));
  };
};
template <>
struct from_object<llvm::ArrayType *, std::true_type> {
  typedef llvm::ArrayType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::ArrayType *>(gc::As<llvmo::ArrayType_sp>(object)->wrappedPtr());
  }
};

};
    ;

namespace llvmo {
FORWARD(VectorType);
class VectorType_O : public SequentialType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::VectorType, VectorType_O, "VectorType", SequentialType_O);
  typedef llvm::VectorType ExternalType;
  typedef llvm::VectorType *VectorToExternalType;

public:
  VectorToExternalType wrapped() { return static_cast<VectorToExternalType>(this->_ptr); };
  void set_wrapped(VectorToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  VectorType_O() : Base(){};
  ~VectorType_O() {}

public: // static methods
  static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
}; // VectorType_O
}; // llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <>
struct to_object<llvm::VectorType *> {
  static core::T_sp convert(llvm::VectorType *ptr) {
    _G();
    return ((core::RP_Create_wrapped<llvmo::VectorType_O, llvm::VectorType *>(ptr)));
  };
};
template <>
struct from_object<llvm::VectorType *, std::true_type> {
  typedef llvm::VectorType *DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = static_cast<llvm::VectorType *>(gc::As<llvmo::VectorType_sp>(object)->wrappedPtr());
  }
};
};

namespace translate {

template <>
struct from_object<const llvm::StringRef, std::true_type> {
  typedef llvm::StringRef DeclareType;
  DeclareType _v;
  string _Storage;
  from_object(T_P object) {
    this->_Storage = gc::As<core::String_sp>(object)->get();
    this->_v = llvm::StringRef(this->_Storage);
  }
};

template <>
struct from_object<llvm::GlobalValue::LinkageTypes, std::true_type> {
  typedef llvm::GlobalValue::LinkageTypes DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARglobal_value_linkage_typesSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::GlobalValue::LinkageTypes>(sym);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::GlobalValue::LinkageType", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::GlobalValue::ThreadLocalMode, std::true_type> {
  typedef llvm::GlobalValue::ThreadLocalMode DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if (object.notnilp()) {
      if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARglobal_ThreadLocalModesSTAR->symbolValue());
        this->_v = converter->enumForSymbol<llvm::GlobalValue::ThreadLocalMode>(sym);
        return;
      }
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::GlobalValue::ThreadLocalMode", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::AtomicOrdering, std::true_type> {
  typedef llvm::AtomicOrdering DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) {
    _G();
    if (object.notnilp()) {
      if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARatomic_orderingSTAR->symbolValue());
        this->_v = converter->enumForSymbol<llvm::AtomicOrdering>(sym);
        return;
      }
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::AtomicOrdering", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::AtomicRMWInst::BinOp, std::true_type> {
  typedef llvm::AtomicRMWInst::BinOp DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARAtomicRMWInstBinOpSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::AtomicRMWInst::BinOp>(sym);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::AtomicRMWInst::BinOp", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::Instruction::CastOps, std::true_type> {
  typedef llvm::Instruction::CastOps DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARInstructionCastOpsSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::Instruction::CastOps>(sym);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::Instruction::CastOps", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::Instruction::BinaryOps, std::true_type> {
  typedef llvm::Instruction::BinaryOps DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARBinaryOpsSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::Instruction::BinaryOps>(sym);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::Instruction::BinaryOps", _rep_(object).c_str());
  }
};

template <>
struct from_object<llvm::CmpInst::Predicate, std::true_type> {
  typedef llvm::CmpInst::Predicate DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    _G();
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARCmpInstPredicateSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CmpInst::Predicate>(sym);
      return;
    }
    SIMPLE_ERROR_SPRINTF("Cannot convert object %s to llvm::CmpInst::Predicate", _rep_(object).c_str());
  }
};
};

namespace llvmo {
  void finalizeEngineAndRegisterWithGcAndRunMainFunctions(ExecutionEngine_sp oengine);

  Module_sp llvm_sys__parseBitcodeFile(core::T_sp filename, LLVMContext_sp context);
  Module_sp llvm_sys__parseIRFile(core::T_sp filename, LLVMContext_sp context);

void initialize_llvmo_expose();
}



namespace llvmo {

  FORWARD(ModuleHandle);
  FORWARD(ClaspJIT);
  
  using namespace llvm;
  using namespace llvm::orc;

  void save_symbol_info(const llvm::object::ObjectFile& object_file, const llvm::RuntimeDyld::LoadedObjectInfo& loaded_object_info);
};

// Don't allow the object to move, but maybe I'll need to collect it
// if we create a ClaspJIT_O for each thread and need to collect it when
// the thread is killed
template <>
struct gctools::GCInfo<llvmo::ClaspJIT_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = false;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

namespace llvmo {
  class ClaspJIT_O : public core::General_O {
    LISP_CLASS(llvmo, LlvmoPkg, ClaspJIT_O, "clasp-jit", core::General_O);
  private:
    std::unique_ptr<llvm::TargetMachine> TM;
    const llvm::DataLayout DL;
//    NotifyObjectLoadedT NotifyObjectLoaded;
    RTDyldObjectLinkingLayer ObjectLayer;
    IRCompileLayer<decltype(ObjectLayer),SimpleCompiler> CompileLayer;
    typedef std::function<std::shared_ptr<Module>(std::shared_ptr<Module>)> OptimizeFunction;
    IRTransformLayer<decltype(CompileLayer), OptimizeFunction> OptimizeLayer;
    JITEventListener* GDBEventListener;
    core::List_sp ModuleHandles;
  public:
//    typedef decltype(OptimizeLayer)::ModuleSetHandleT ModuleHandle;
    typedef decltype(CompileLayer)::ModuleHandleT ModuleHandle;

    ClaspJIT_O();

    TargetMachine &getTargetMachine() { return *TM; }

    ModuleHandle_sp addModule(Module_sp M);
    core::Pointer_sp findSymbol(const std::string& Name);
    core::Pointer_sp findSymbolIn(ModuleHandle_sp handle, const std::string& Name, bool exportedSymbolsOnly );
    bool removeModule(ModuleHandle_sp H);

//    std::shared_ptr<llvm::Module> optimizeModule(std::shared_ptr<llvm::Module> M);
  };

};

template <>
struct gctools::GCInfo<llvmo::ModuleHandle_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};


namespace llvmo {
  class ModuleHandle_O : public core::General_O {
    LISP_CLASS(llvmo, LlvmoPkg, ModuleHandle_O, "module-handle", core::General_O);
  public:
    core::ShutdownFunction_fptr_type _shutdownFunction;
    ClaspJIT_O::ModuleHandle _Handle;
  public:
    static ModuleHandle_sp create(const ClaspJIT_O::ModuleHandle& val) {
      GC_ALLOCATE_VARIADIC(ModuleHandle_O,mh,val);
      return mh;
    }
  public:
    void set_shutdown_function(core::ShutdownFunction_fptr_type fn) {
      this->_shutdownFunction = fn;
    }
    void shutdown_module() {
      if (this->_shutdownFunction) {
        this->_shutdownFunction();
      }
    }
  ModuleHandle_O(const ClaspJIT_O::ModuleHandle& handle) : _shutdownFunction(NULL), _Handle(handle) {};
    // ModuleHandle's have finalizers installed to clean them up.
    // The destructor calls cmp:jit-remove-module to achieve this
    virtual ~ModuleHandle_O();
  };

  core::T_sp llvm_sys__lookup_jit_symbol_info(void* ptr);

  std::shared_ptr<llvm::Module> optimizeModule(std::shared_ptr<llvm::Module> M);
};



#endif //]
