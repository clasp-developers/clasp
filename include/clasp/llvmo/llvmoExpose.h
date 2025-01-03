#pragma once

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

// #define USE_JITLINKER 1

#include <clasp/core/common.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/array.h>
#include <clasp/core/symbol.h>
#include <clasp/core/ql.h>
#include <llvm/Object/SymbolSize.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Linker/Linker.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
// #include "llvm/ExecutionEngine/JIT.h"
#include <llvm/ExecutionEngine/MCJIT.h>
// #include "llvm/ExecutionEngine/JITMemoryManager.h"
#include <llvm/CodeGen/TargetPassConfig.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Passes/PassBuilder.h>
#if LLVM_VERSION_MAJOR < 17
#include <llvm/ADT/Triple.h>
#else
#include <llvm/TargetParser/Triple.h>
#endif
#include <llvm/Support/TargetSelect.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
// #include "llvm/Support/IRBuilder.h"

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
  typedef llvm::LLVMContext* PointerToExternalType;

public:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  string __repr__() const;
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  bool LLVMContext_equal(core::T_sp obj) const;
  static LLVMContext_sp create_llvm_context();
  ;
  LLVMContext_O() : Base(), _ptr(NULL){};
  ~LLVMContext_O() {
    delete _ptr;
    _ptr = NULL;
  }

}; // LLVMContext_O
}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::LLVMContext&> {
  typedef llvm::LLVMContext& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(gc::As<llvmo::LLVMContext_sp>(object)->wrappedPtr())){};
  ~from_object(){/*non trivial*/};
};

template <> struct to_object<llvm::LLVMContext&> {
  static core::T_sp convert(llvm::LLVMContext& lc) {
    return ((core::RP_Create_wrapped<llvmo::LLVMContext_O, llvm::LLVMContext*>(&lc)));
  };
};
template <> struct to_object<llvm::LLVMContext*> {
  static core::T_sp convert(llvm::LLVMContext* lc) {
    return ((core::RP_Create_wrapped<llvmo::LLVMContext_O, llvm::LLVMContext*>(lc)));
  };
};
}; // namespace translate
    ;
/* to_object translators */

namespace llvmo {
FORWARD(ThreadSafeContext);
class ThreadSafeContext_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::orc::ThreadSafeContext, ThreadSafeContext_O, "thread-safe-context",
                      core::ExternalObject_O);
  typedef llvm::orc::ThreadSafeContext ExternalType;
  typedef llvm::orc::ThreadSafeContext* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ThreadSafeContext_sp create_thread_safe_context();
  llvm::LLVMContext* getContext();
  ThreadSafeContext_O() : Base(), _ptr(NULL){};
  ~ThreadSafeContext_O() {
    delete _ptr;
    _ptr = NULL;
  }

}; // ThreadSafeContext_O

}; // namespace llvmo

namespace llvmo {
FORWARD(FunctionCallee);
class FunctionCallee_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, FunctionCallee_O, "FunctionCallee", core::CxxObject_O);

public:
  dont_expose<llvm::FunctionCallee> _Info;
  CL_DEFMETHOD llvm::FunctionType* getFunctionType() { return this->_Info._value.getFunctionType(); };
  CL_DEFMETHOD llvm::Value* getCallee() { return this->_Info._value.getCallee(); };
  FunctionCallee_O(llvm::FunctionType* ft, llvm::Value* v) : _Info(llvm::FunctionCallee(ft, v)){};
};
}; // namespace llvmo

namespace translate {
template <> struct from_object<const llvm::FunctionCallee&> {
  typedef llvm::FunctionCallee DeclareType;
  DeclareType _v;
  from_object(llvmo::FunctionCallee_sp object) : _v(object->getFunctionType(), object->getCallee()){};
  ~from_object(){/*non trivial*/};
};
}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::FunctionCallee> {
  static core::T_sp convert(llvm::FunctionCallee fc) {
    auto ofc = gctools::GC<llvmo::FunctionCallee_O>::allocate(fc.getFunctionType(), fc.getCallee());
    return ofc;
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(MemoryBuffer);
class MemoryBuffer_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MemoryBuffer, MemoryBuffer_O, "MemoryBuffer", core::ExternalObject_O);
  typedef llvm::MemoryBuffer ExternalType;
  typedef llvm::MemoryBuffer* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  MemoryBuffer_O() : Base(), _ptr(NULL){};
  MemoryBuffer_O(void* buf) : Base(), _ptr((PointerToExternalType)buf){};
  ~MemoryBuffer_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

}; // Linker_O
}; // namespace llvmo

namespace llvmo {
FORWARD(Linker);
class Linker_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Linker, Linker_O, "Linker", core::ExternalObject_O);
  typedef llvm::Linker ExternalType;
  typedef llvm::Linker* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  static Linker_sp make(Module_sp module);

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  Linker_O() : Base(), _ptr(NULL){};
  ~Linker_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

}; // Linker_O
}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::Linker&> {
  typedef llvm::Linker& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(gc::As<llvmo::Linker_sp>(object)->wrappedPtr())){};
  ~from_object(){/*non trivial*/};
};
}; // namespace translate
    ;
/* to_object translators */

namespace llvmo {
typedef enum { DebugObjectFilesOff, DebugObjectFilesPrint, DebugObjectFilesPrintSave } DebugObjectFilesEnum;
extern DebugObjectFilesEnum globalDebugObjectFiles;

}; // namespace llvmo

#ifdef DEBUG_OBJECT_FILES
#define DEBUG_OBJECT_FILES_PRINT(msg)                                                                                              \
  if (llvmo::globalDebugObjectFiles != llvmo::DebugObjectFilesOff) {                                                               \
    printf msg;                                                                                                                    \
  }
#else
#define DEBUG_OBJECT_FILES_PRINT(msg)
#endif

namespace llvmo {
FORWARD(JITDylib);
class JITDylib_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::orc::JITDylib, JITDylib_O, "JITDylib", core::ExternalObject_O);
  typedef llvm::orc::JITDylib ExternalType;
  typedef llvm::orc::JITDylib* PointerToExternalType;

private:
  JITDylib_O() = default;

public:
  PointerToExternalType _ptr;
  size_t _Id;
  core::SimpleBaseString_sp _name;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void dump(core::T_sp stream);

public:
#if 0
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
#endif
  JITDylib_O(core::SimpleBaseString_sp name, PointerToExternalType ptr) : Base(), _ptr(ptr), _name(name) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s name = %s  ptr = %p\n", __FILE__, __LINE__, __FUNCTION__, _rep_(name).c_str(), ptr));
  };
  ~JITDylib_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

}; // JITDylib_O
}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::orc::JITDylib*> {
  typedef llvm::orc::JITDylib* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::JITDylib_sp>(object)->wrappedPtr()){};
};
}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::orc::JITDylib*> {
  static core::T_sp convert(llvm::orc::JITDylib* ptr) {
    std::string name = ptr->getName();
    core::SimpleBaseString_sp sname = core::SimpleBaseString_O::make(name);
    return gctools::GC<llvmo::JITDylib_O>::allocate(sname, ptr);
  }
};
template <> struct to_object<llvm::orc::JITDylib&> {
  static core::T_sp convert(llvm::orc::JITDylib& jd) {
    std::string name = jd.getName();
    core::SimpleBaseString_sp sname = core::SimpleBaseString_O::make(name);
    return gctools::GC<llvmo::JITDylib_O>::allocate(sname, &jd);
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(AttributeSet);
class AttributeSet_O : public core::General_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AttributeSet, AttributeSet_O, "AttributeSet", core::General_O);

public:
  typedef llvm::AttributeSet ExternalType;

protected:
  dont_expose<llvm::AttributeSet> val;

public:
  llvm::AttributeSet getAttributeSet() { return this->val._value; };
  AttributeSet_O(llvm::AttributeSet v) : val(v){};
}; // AttributeSet_O
}; // namespace llvmo
/* from_object translators */

/* to_object translators */

namespace translate {
template <> struct from_object<llvm::AttributeSet> {
  typedef llvm::AttributeSet DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) : _v(gc::As<llvmo::AttributeSet_sp>(object)->getAttributeSet()){};
};
template <> struct to_object<llvm::AttributeSet> {
  static core::T_sp convert(llvm::AttributeSet val) {
    auto obj = gctools::GC<llvmo::AttributeSet_O>::allocate(val);
    return obj;
  };
};
}; // namespace translate

namespace translate {
template <> struct from_object<llvm::ArrayRef<llvm::Attribute::AttrKind>> {
  typedef std::vector<llvm::Attribute::AttrKind> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_AttributeEnum->symbolValue());
      for (auto cvals : lcvals) {
        llvm::Attribute::AttrKind ak =
            converter->enumForSymbol<llvm::Attribute::AttrKind>(gc::As<core::Symbol_sp>(core::oCar(cvals)));
        _v.push_back(ak);
      }
      return;
    }
    SIMPLE_ERROR("Could not convert {} to llvm::ArrayRef<llvm::Attribute::AttrKind>", core::_rep_(o));
  }
};
}; // namespace translate
template <> struct gctools::GCInfo<llvmo::Triple_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(Triple);
class Triple_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Triple, Triple_O, "Triple", core::ExternalObject_O);
  typedef llvm::Triple ExternalType;
  typedef llvm::Triple* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  static Triple_sp make(const string& triple);

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    delete this->_ptr;
    this->_ptr = ptr;
  }
  Triple_O() : Base(), _ptr(NULL){};
  ~Triple_O() {
    if (_ptr != NULL) {
      auto ptr = this->_ptr;
      //      printf("%s:%d:%s registering dtor\n", __FILE__, __LINE__, __FUNCTION__ );
      core::thread_local_register_cleanup([ptr](void) {
#ifdef DEBUG_DTORS
        printf("%s:%d:%s dtor %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
#endif
        delete ptr;
      });
      _ptr = NULL;
    };
  }

}; // Triple_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::Triple*> {
  typedef llvm::Triple* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::Triple_sp>(object)->wrappedPtr()){};
};

template <> struct from_object<llvm::Triple&> {
  typedef llvm::Triple& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::Triple_sp>(object)->wrappedPtr()){};
  ~from_object(){/*non trivial*/};
};
}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Triple*> {
  static core::T_sp convert(llvm::Triple* ptr) { return ((core::RP_Create_wrapped<llvmo::Triple_O, llvm::Triple*>(ptr))); }
};
}; // namespace translate
    ;

template <> struct gctools::GCInfo<llvmo::TargetOptions_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(TargetOptions);
class TargetOptions_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetOptions, TargetOptions_O, "TargetOptions", core::ExternalObject_O);
  typedef llvm::TargetOptions ExternalType;
  typedef llvm::TargetOptions* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  static TargetOptions_sp make(bool functionSections);

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
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
      auto ptr = this->_ptr;
      //      printf("%s:%d:%s registering dtor\n", __FILE__, __LINE__, __FUNCTION__ );
      core::thread_local_register_cleanup([ptr](void) {
#ifdef DEBUG_DTORS
        printf("%s:%d:%s dtor %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
#endif
        delete ptr;
      });
      _ptr = NULL;
    };
  }

}; // TargetOptions_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::TargetOptions*> {
  typedef llvm::TargetOptions* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::TargetOptions_sp>(object)->wrappedPtr()){};
};
template <> struct from_object<const llvm::TargetOptions&> {
  typedef const llvm::TargetOptions& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::TargetOptions_sp>(object)->wrappedPtr()){};
};
}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::TargetOptions*> {
  static core::T_sp convert(llvm::TargetOptions* ptr) {
    return ((core::RP_Create_wrapped<llvmo::TargetOptions_O, llvm::TargetOptions*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(Target);
class Target_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Target, Target_O, "Target", core::ExternalObject_O);
  typedef llvm::Target ExternalType;
  typedef llvm::Target* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  Target_O() : Base(), _ptr(NULL){};
  ~Target_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

}; // Target_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::Target*> {
  typedef llvm::Target* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::Target_sp>(object)->wrappedPtr()){};
};
}; // namespace translate
/* to_object translators */
namespace translate {
template <> struct to_object<llvm::Target*, translate::dont_adopt_pointer> {
  static core::T_sp convert(llvm::Target* ptr) { return ((core::RP_Create_wrapped<llvmo::Target_O, llvm::Target*>(ptr))); }
};

template <> struct to_object<const llvm::Target*> {
  static core::T_sp convert(const llvm::Target* ptr) {
    return ((core::RP_Create_wrapped<llvmo::Target_O, llvm::Target*>(const_cast<llvm::Target*>(ptr))));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(MCSubtargetInfo);
class MCSubtargetInfo_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MCSubtargetInfo, MCSubtargetInfo_O, "MCSubtargetInfo", core::ExternalObject_O);
  typedef llvm::MCSubtargetInfo ExternalType;
  typedef llvm::MCSubtargetInfo* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  MCSubtargetInfo_O() : Base(), _ptr(NULL){};
  ~MCSubtargetInfo_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

}; // MCSubtargetInfo_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::MCSubtargetInfo*> {
  typedef llvm::MCSubtargetInfo* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::MCSubtargetInfo_sp>(object)->wrappedPtr()){};
};
}; // namespace translate
/* to_object translators */
namespace translate {
template <> struct to_object<llvm::MCSubtargetInfo*, translate::dont_adopt_pointer> {
  static core::T_sp convert(llvm::MCSubtargetInfo* ptr) {
    return ((core::RP_Create_wrapped<llvmo::MCSubtargetInfo_O, llvm::MCSubtargetInfo*>(ptr)));
  }
};

template <> struct to_object<const llvm::MCSubtargetInfo*> {
  static core::T_sp convert(const llvm::MCSubtargetInfo* ptr) {
    return ((core::RP_Create_wrapped<llvmo::MCSubtargetInfo_O, llvm::MCSubtargetInfo*>(const_cast<llvm::MCSubtargetInfo*>(ptr))));
  }
};
}; // namespace translate
    ;

namespace llvmo {
class TargetSubtargetInfo_O : public MCSubtargetInfo_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetSubtargetInfo, TargetSubtargetInfo_O, "TargetSubtargetInfo", MCSubtargetInfo_O);
  typedef llvm::TargetSubtargetInfo ExternalType;
  typedef llvm::TargetSubtargetInfo* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // TargetSubtargetInfo_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::TargetSubtargetInfo*> {
  typedef llvm::TargetSubtargetInfo* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<gc::smart_ptr<llvmo::TargetSubtargetInfo_O>>(object)->wrappedPtr()){};
};
template <> struct to_object<const llvm::TargetSubtargetInfo*> {
  static core::T_sp convert(const llvm::TargetSubtargetInfo* ptr) {
    return ((core::RP_Create_wrapped<llvmo::TargetSubtargetInfo_O, llvm::TargetSubtargetInfo*>(
        const_cast<llvm::TargetSubtargetInfo*>(ptr))));
  }
};
}; // namespace translate

#if LLVM_VERSION_MAJOR < 18
namespace translate {
template <> struct from_object<llvm::CodeGenOpt::Level> {
  typedef llvm::CodeGenOpt::Level DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CodeGenOpt::Default) {
    if (object.nilp()) {
      SIMPLE_ERROR("You must pass a valid CodeGenOpt");
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeGenOpt->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CodeGenOpt::Level>(so);
    } else {
      SIMPLE_ERROR("You must pass a valid CodeGenOpt");
    }
  }
};
#else
namespace translate {
template <> struct from_object<llvm::CodeGenOptLevel> {
  typedef llvm::CodeGenOptLevel DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CodeGenOptLevel::Default) {
    if (object.nilp()) {
      SIMPLE_ERROR("You must pass a valid CodeGenOpt");
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeGenOpt->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CodeGenOptLevel>(so);
    } else {
      SIMPLE_ERROR("You must pass a valid CodeGenOptLevel");
    }
  }
};
#endif

#if LLVM_VERSION_MAJOR < 16
template <> struct from_object<llvm::Optional<llvm::Reloc::Model>> {
  typedef llvm::Optional<llvm::Reloc::Model> DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.nilp()) {
      //      SIMPLE_ERROR(("You must pass a valid RelocModel"));
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      if (so == llvmo::_sym_RelocModel_undefined) {
        // printf("%s:%d Leaving llvm::Reloc::Model Undefined\n", __FILE__, __LINE__ );
      } else {
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_RelocModel->symbolValue());
        this->_v = converter->enumForSymbol<llvm::Reloc::Model>(so);
      }
    } else {
      SIMPLE_ERROR("You must pass a valid RelocModel or {}", _rep_(llvmo::_sym_RelocModel_undefined));
    }
  }
};
#else
template <> struct from_object<std::optional<llvm::Reloc::Model>> {
  typedef std::optional<llvm::Reloc::Model> DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.nilp()) {
      //      SIMPLE_ERROR(("You must pass a valid RelocModel"));
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      if (so == llvmo::_sym_RelocModel_undefined) {
        // printf("%s:%d Leaving llvm::Reloc::Model Undefined\n", __FILE__, __LINE__ );
      } else {
        core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_RelocModel->symbolValue());
        this->_v = converter->enumForSymbol<llvm::Reloc::Model>(so);
      }
    } else {
      SIMPLE_ERROR("You must pass a valid RelocModel or {}", _rep_(llvmo::_sym_RelocModel_undefined));
    }
  }
};
#endif

template <> struct from_object<llvm::CodeModel::Model> {
  typedef llvm::CodeModel::Model DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CodeModel::Small) {
    if (object.nilp()) {
      SIMPLE_ERROR("You must pass a valid CodeModel");
    }
    if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeModel->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CodeModel::Model>(so);
    } else {
      SIMPLE_ERROR("You must pass a valid CodeModel");
    }
  }
};
#if LLVM_VERSION_MAJOR < 18
template <> struct from_object<llvm::CodeGenFileType> {
  typedef llvm::CodeGenFileType DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CGFT_ObjectFile) {
    if (object.notnilp()) {
      if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter =
            gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeGenFileType->symbolValue());
        this->_v = converter->enumForSymbol<llvm::CodeGenFileType>(so);
        return;
      }
    }
    SIMPLE_ERROR("You must pass a valid ");
  }
};
#else
template <> struct from_object<llvm::CodeGenFileType> {
  typedef llvm::CodeGenFileType DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(llvm::CodeGenFileType::ObjectFile) {
    if (object.notnilp()) {
      if (core::Symbol_sp so = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter =
            gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeGenFileType->symbolValue());
        this->_v = converter->enumForSymbol<llvm::CodeGenFileType>(so);
        return;
      }
    }
    SIMPLE_ERROR("You must pass a valid ");
  }
};
#endif
}; // namespace translate

template <> struct gctools::GCInfo<llvmo::TargetMachine_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(TargetMachine);
class TargetMachine_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::TargetMachine, TargetMachine_O, "TargetMachine", core::ExternalObject_O);
  typedef llvm::TargetMachine ExternalType;
  typedef llvm::TargetMachine* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

  core::T_sp emitModule(core::T_sp stream, core::T_sp dwo_stream, llvm::CodeGenFileType FileType, Module_sp module);

  TargetMachine_O() : Base(), _ptr(NULL){};
  ~TargetMachine_O() {
    if (_ptr != NULL) {
      auto ptr = this->_ptr;
      core::thread_local_register_cleanup([ptr](void) {
#ifdef DEBUG_DTORS
        printf("%s:%d:%s dtor %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
#endif
        delete ptr;
      });
      _ptr = NULL;
    };
  }
}; // TargetMachine_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::TargetMachine*> {
  typedef llvm::TargetMachine* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? NULL : gc::As<llvmo::TargetMachine_sp>(object)->wrappedPtr()){};
};
}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::TargetMachine*> {
  static core::T_sp convert(llvm::TargetMachine* ptr) {
    return ((core::RP_Create_wrapped<llvmo::TargetMachine_O, llvm::TargetMachine*>(ptr)));
  }
};
}; // namespace translate

namespace translate {
template <> struct to_object<llvm::TargetMachine&> {
  static core::T_sp convert(llvm::TargetMachine& obj) {
    return ((core::RP_Create_wrapped<llvmo::TargetMachine_O, llvm::TargetMachine*>(&obj)));
  }
};
}; // namespace translate

extern llvm::Value* llvm_cast_error_ptr;
template <typename T, typename U> T* llvm_cast(U* p) {
  if (!llvm::isa<T>(p)) {
    // save the pointer in a global so we can take a look at it
    llvm_cast_error_ptr = reinterpret_cast<llvm::Value*>(p);
    SIMPLE_ERROR("llvm_cast<T> argument of incompatible type - bad pointer stored in (void*)llvm_cast_error_ptr!");
  }
  return reinterpret_cast<T*>(p);
}

namespace llvmo {
FORWARD(LLVMTargetMachine);
class LLVMTargetMachine_O : public TargetMachine_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LLVMTargetMachine, LLVMTargetMachine_O, "LLVMTargetMachine", TargetMachine_O);
  typedef llvm::LLVMTargetMachine ExternalType;
  typedef llvm::LLVMTargetMachine* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return /*dynamic_*/ reinterpret_cast<ExternalType*>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // LLVMTargetMachine_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::LLVMTargetMachine*> {
  typedef llvm::LLVMTargetMachine* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::LLVMTargetMachine_sp>(object)->wrappedPtr()){};
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::LLVMTargetMachine*> {
  static core::T_sp convert(llvm::LLVMTargetMachine* ptr) {
    return ((core::RP_Create_wrapped<llvmo::LLVMTargetMachine_O, llvm::LLVMTargetMachine*>(ptr)));
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(Value);
class Value_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Value, Value_O, "value", core::ExternalObject_O);
  typedef llvm::Value ExternalType;
  typedef llvm::Value* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Value_sp create(llvm::Value* ptr);
  ;
  Value_O() : Base(), _ptr(NULL){};
  ~Value_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  LLVMContext_sp getContext() const;

  bool llvm_sys_value_p() const { return true; };
  string __repr__() const;
  void __write__(core::T_sp stream) const;
  bool valid() const;
}; // Value_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::Value*> {
  typedef llvm::Value* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Value_sp>(object)->wrappedPtr()){};
};
template <> struct from_object<llvm::ArrayRef<llvm::Value*>> {
  typedef llvm::ArrayRef<llvm::Value*> DeclareType;
  std::vector<llvm::Value*> _backing;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _backing.clear();
      this->_v = _backing;
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      for (auto cvals : lcvals) {
        llvm::Value* vP = gc::As<llvmo::Value_sp>(core::oCar(cvals))->wrappedPtr();
        _backing.push_back(vP);
      }
      this->_v = _backing;
      return;
    } else if (o.isA<core::AbstractSimpleVector_O>()
               || o.isA<core::ComplexVector_O>()) {
      core::Array_sp vvals = o.as_unsafe<core::Array_O>();
      _backing.resize(vvals->length());
      for (int i(0), iEnd(vvals->length()); i < iEnd; ++i) {
        _backing[i] = gc::As<llvmo::Value_sp>(vvals->rowMajorAref(i))->wrappedPtr();
        printf("%s:%d   Entry[%d] <-- %s\n", __FILE__, __LINE__, i, _rep_(vvals->rowMajorAref(i)).c_str());
      }
      this->_v = _backing;
      return;
    }
    SIMPLE_ERROR("Could not convert {} to llvm::ArrayRef<llvm::Value*>", core::_rep_(o));
  }
};

}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Value*> {
  static core::T_sp convert(llvm::Value* ptr) { return ((llvmo::Value_O::create(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(Metadata);
class Metadata_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Metadata, Metadata_O, "metadata", core::ExternalObject_O);
  typedef llvm::Metadata ExternalType;
  typedef llvm::Metadata* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Metadata_sp create(llvm::Metadata* ptr);
  ;
  Metadata_O() : Base(), _ptr(NULL){};
  ~Metadata_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  //	string __repr__() const;
  //	bool valid() const;
}; // Metadata_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::Metadata*> {
  typedef llvm::Metadata* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Metadata_sp>(object)->wrappedPtr()){};
};
template <> struct from_object<llvm::ArrayRef<llvm::Metadata*>> {
  typedef std::vector<llvm::Metadata*> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _v.clear();
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      for (auto cvals : lcvals) {
        llvm::Metadata* vP = gc::As<llvmo::Metadata_sp>(core::oCar(cvals))->wrappedPtr();
        _v.push_back(vP);
      }
      return;
    } else if (o.isA<core::AbstractSimpleVector_O>()
               || o.isA<core::ComplexVector_O>()) {
      core::Array_sp vvals = o.as_unsafe<core::Array_O>();
      _v.resize(vvals->length());
      for (int i(0), iEnd(vvals->length()); i < iEnd; ++i) {
        _v[i] = gc::As<llvmo::Metadata_sp>(vvals->rowMajorAref(i))->wrappedPtr();
      }
      return;
    }
    SIMPLE_ERROR("Could not convert {} to llvm::ArrayRef<llvm::Metadata*>", core::_rep_(o));
  }
};

}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Metadata*> {
  static core::T_sp convert(llvm::Metadata* ptr) { return ((llvmo::Metadata_O::create(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(User);
class User_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::User, User_O, "user", Value_O);
  typedef llvm::User ExternalType;
  typedef llvm::User* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // User_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(MetadataAsValue);
class MetadataAsValue_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MetadataAsValue, MetadataAsValue_O, "MetadataAsValue", Value_O);
  typedef llvm::MetadataAsValue ExternalType;
  typedef llvm::MetadataAsValue* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // MetadataAsValue_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::MetadataAsValue*> {
  typedef llvm::MetadataAsValue* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::MetadataAsValue_sp>(object)->wrappedPtr()){};
};
}; // namespace translate

namespace translate {
template <> struct to_object<llvm::MetadataAsValue*> {
  static core::T_sp convert(llvm::MetadataAsValue* mav) {
    auto oattr = gctools::GC<llvmo::MetadataAsValue_O>::allocate_with_default_constructor();
    oattr->set_wrapped(mav);
    return oattr;
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(Attribute);
class Attribute_O : public core::General_O {
  LISP_CLASS(llvmo, LlvmoPkg, Attribute_O, "Attribute", core::General_O);
  //    DECLARE_ARCHIVE();
public: // Simple default ctor/dtor
  DEFAULT_CTOR_DTOR(Attribute_O);

public: // ctor/dtor for classes with shared virtual base
        //    explicit Attribute_O() : T_O(), T(mc) {};
public:
private: // instance variables here
  dont_expose<llvm::Attribute> _Attribute;

public: // Functions here
  static Attribute_sp get(LLVMContext_sp context, core::List_sp attribute_symbols);

  llvm::Attribute attributes() { return this->_Attribute._value; };
  void setAttribute(llvm::Attribute attr) { this->_Attribute._value = attr; };
}; // Attribute class

}; // namespace llvmo
namespace translate {
template <> struct from_object<llvm::Attribute::AttrKind> {
  typedef llvm::Attribute::AttrKind DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) {
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_AttributeEnum->symbolValue());
      this->_v = converter->enumForSymbol<llvm::Attribute::AttrKind>(sym);
      return;
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::Attribute::AttrKind", _rep_(object));
  }
};

template <> struct from_object<llvm::Attribute> {
  typedef llvm::Attribute DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Attribute_sp>(object)->attributes()){};
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Attribute> {
  static core::T_sp convert(llvm::Attribute attr) {
    auto oattr = gctools::GC<llvmo::Attribute_O>::allocate_with_default_constructor();
    oattr->setAttribute(attr);
    return ((oattr));
  }
};

}; // namespace translate
    ;

template <> struct gctools::GCInfo<llvmo::DataLayout_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(DataLayout);
FORWARD(StructLayout);
FORWARD(StructType);
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
  StructLayout_sp getStructLayout(StructType_sp ty) const;
  DataLayout_O(const llvm::DataLayout& orig) { this->_DataLayout = new llvm::DataLayout(orig); };
  /*! Delete the default constructor because llvm::DataLayout doesn't have one */
  DataLayout_O() = delete;
  ~DataLayout_O() {
    if (this->_DataLayout) {
      auto ptr = this->_DataLayout;
      //      printf("%s:%d:%s registering dtor\n", __FILE__, __LINE__, __FUNCTION__ );
      core::thread_local_register_cleanup([ptr](void) {
#ifdef DEBUG_DTORS
        printf("%s:%d:%s dtor %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
#endif
        delete ptr;
      });
      this->_DataLayout = NULL;
    }
  }
  DataLayout_sp copy() const;

}; // DataLayout_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
// Since llvm3.8 there don't appear to be functions that
// take or return llvm::DataLayout* pointers.  So I am commenting out
// their converters and I changed the DataLayout_O class to store a llvm::DataLayout
template <> struct from_object<llvm::DataLayout const&> {
  typedef llvm::DataLayout const& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DataLayout_sp>(object)->dataLayout()){};
};

// ----------   to_object converters
template <> struct to_object<const llvm::DataLayout&> {
  static core::T_sp convert(const llvm::DataLayout& ref) {
    // Use the copy constructor to create a DataLayout_O
    auto val = gctools::GC<llvmo::DataLayout_O>::allocate(ref);
    return val;
  }
};

/*! This copies the DataLayout so it doesn't deal with pointers at all */
template <> struct to_object<llvm::DataLayout const, translate::dont_adopt_pointer> {
  static core::T_sp convert(llvm::DataLayout orig) {
    // Use the copy constructor to create a DataLayout_O
    auto val = gctools::GC<llvmo::DataLayout_O>::allocate(orig);
    return val;
  }
};
/*! This copies the DataLayout so it doesn't deal with pointers at all */
template <> struct to_object<llvm::DataLayout, translate::dont_adopt_pointer> {
  static core::T_sp convert(llvm::DataLayout orig) {
    // Use the copy constructor to create a DataLayout_O
    auto val = gctools::GC<llvmo::DataLayout_O>::allocate(orig);
    return val;
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(StructLayout);
/*! StructLayout_O
As of llvm3.7 the llvm::StructLayout seems to be passed around as a simple object
and pointers to it are no longer required by functions or returned by functions.
So I'm changing StructLayout_O so that it wraps a complete llvm::StructLayout object
*/
class StructLayout_O : public core::General_O {
  LISP_CLASS(llvmo, LlvmoPkg, StructLayout_O, "StructLayout", core::General_O);

protected:
  const llvm::StructLayout* _StructLayout;

public:
  const llvm::StructLayout& structLayout() { return *(this->_StructLayout); };
  size_t getSizeInBytes() const;
  size_t getElementOffset(size_t idx) const;
  /*! Delete the default constructor because llvm::StructLayout doesn't have one */
  StructLayout_O(const llvm::StructLayout* orig) { this->_StructLayout = orig; };
  StructLayout_O() = delete;
  ~StructLayout_O() { delete this->_StructLayout; }

}; // StructLayout_O
}; // namespace llvmo
/* from_object translators */

namespace llvmo {
FORWARD(Constant);
class Constant_O : public User_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Constant, Constant_O, "constant", User_O);
  typedef llvm::Constant ExternalType;
  typedef llvm::Constant* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Constant_sp create(llvm::Constant* ptr);
}; // Constant_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Constant*> {
  static core::T_sp convert(llvm::Constant* ptr) { return ((core::RP_Create_wrapped<llvmo::Constant_O, llvm::Constant*>(ptr))); };
};
template <> struct from_object<llvm::Constant*> {
  typedef llvm::Constant* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Constant_sp>(object)->wrappedPtr()){};
};

template <> struct from_object<llvm::ArrayRef<llvm::Constant*>> {
  typedef llvm::ArrayRef<llvm::Constant*> DeclareType;
  std::vector<llvm::Constant*> _backing;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.nilp()) {
      _backing.clear();
      this->_v = this->_backing;
      return;
    } else if (core::List_sp lcvals = o.asOrNull<core::Cons_O>()) {
      for (auto cvals : lcvals) {
        llvm::Constant* vP = gc::As<llvmo::Constant_sp>(core::oCar(cvals))->wrappedPtr();
        _backing.push_back(vP);
      }
      this->_v = this->_backing;
      return;
    } else if (o.isA<core::AbstractSimpleVector_O>()
               || o.isA<core::ComplexVector_O>()) {
      core::Array_sp vvals = o.as_unsafe<core::Array_O>();
      _backing.resize(vvals->length());
      for (int i(0), iEnd(vvals->length()); i < iEnd; ++i) {
        _backing[i] = gc::As<llvmo::Constant_sp>(vvals->rowMajorAref(i))->wrappedPtr();
      }
      this->_v = this->_backing;
      return;
    }
    SIMPLE_ERROR("Could not convert {} to llvm::ArrayRef<llvm::Constant*>", core::_rep_(o));
  }
};

}; // namespace translate
    ;

namespace llvmo {
FORWARD(ConstantArray);
class ConstantArray_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantArray, ConstantArray_O, "constant-array", Constant_O);
  typedef llvm::ConstantArray ExternalType;
  typedef llvm::ConstantArray* PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) { this->_ptr = ptr; }
public:
  static Constant_sp get(ArrayType_sp type, core::List_sp values);
}; // ConstantArray_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(BlockAddress);
class BlockAddress_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::BlockAddress, BlockAddress_O, "BlockAddress", Constant_O);
  typedef llvm::BlockAddress ExternalType;
  typedef llvm::BlockAddress* PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) { this->_ptr = ptr; }

public:
  static BlockAddress_sp get(Function_sp func, BasicBlock_sp bb);
}; // BlockAddress_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(ConstantDataSequential);
class ConstantDataSequential_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantDataSequential, ConstantDataSequential_O, "ConstantDataSequential",
                      Constant_O);
  typedef llvm::ConstantDataSequential ExternalType;
  typedef llvm::ConstantDataSequential* PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) { this->_ptr = ptr; }

public:
}; // ConstantDataSequential_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(ConstantDataArray);
class ConstantDataArray_O : public ConstantDataSequential_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantDataArray, ConstantDataArray_O, "constant-data-array",
                      ConstantDataSequential_O);
  typedef llvm::ConstantDataArray ExternalType;
  typedef llvm::ConstantDataArray* PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) { this->_ptr = ptr; }

public:
  static Constant_sp getUInt32(LLVMContext_sp context, core::T_sp values);
}; // ConstantDataArray_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(ConstantExpr);
class ConstantExpr_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantExpr, ConstantExpr_O, "ConstantExpr", Constant_O);
  typedef llvm::ConstantExpr ExternalType;
  typedef llvm::ConstantExpr* PointerToExternalType;

private:
public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) { this->_ptr = ptr; }

public:
  static Constant_sp getInBoundsGetElementPtr(llvm::Type* element_type, Constant_sp constant, core::List_sp idxList);

}; // ConstantExpr_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(GlobalValue);
class GlobalValue_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::GlobalValue, GlobalValue_O, "global-value", Constant_O);
  typedef llvm::GlobalValue ExternalType;
  typedef llvm::GlobalValue* PointerToExternalType;

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
  void setUnnamedAddr(llvm::GlobalValue::UnnamedAddr unnamed_addr);
  llvm::GlobalValue::UnnamedAddr getUnnamedAddr();

}; // GlobalValue_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(GlobalVariable);
class GlobalVariable_O : public GlobalValue_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::GlobalVariable, GlobalVariable_O, "GlobalVariable", GlobalValue_O);
  typedef llvm::GlobalVariable ExternalType;
  typedef llvm::GlobalVariable* PointerToExternalType;

public:
  static GlobalVariable_sp make(Module_sp module, Type_sp type, bool isConstant, llvm::GlobalValue::LinkageTypes linkage,
                                /*Constant_sp*/ core::T_sp initializer, core::String_sp name,
                                /*GlobalVariable_sp*/ core::T_sp insertBefore, llvm::GlobalValue::ThreadLocalMode threadLocalMode);

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // GlobalVariable_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::GlobalVariable*> {
  typedef llvm::GlobalVariable* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::GlobalVariable_sp>(object)->wrappedPtr()){};
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::GlobalVariable*> {
  static core::T_sp convert(llvm::GlobalVariable* ptr) {
    if (ptr)
      return ((core::RP_Create_wrapped<llvmo::GlobalVariable_O, llvm::GlobalVariable*>(ptr)));
    return nil<core::T_O>();
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ExecutionEngine);
class ExecutionEngine_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ExecutionEngine, ExecutionEngine_O, "EXECUTION-ENGINE", core::ExternalObject_O);
  friend class EngineBuilder_O;
  typedef llvm::ExecutionEngine ExternalType;
  typedef llvm::ExecutionEngine* PointerToExternalType;

  void initialize();

  GCPROTECTED : PointerToExternalType _ptr;
  core::HashTableEqual_sp _DependentModules;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  string __repr__() const;

  ExecutionEngine_O() : Base(), _ptr(NULL){};
  ~ExecutionEngine_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

  void addModule(Module_sp module);
  bool removeModule(Module_sp module);

  Function_sp find_function_named(core::String_sp name);

  void addNamedModule(const string& name, Module_sp module);
  bool hasNamedModule(const string& name);
  void removeNamedModule(const string& name);
  core::List_sp dependentModuleNames() const;

  void addGlobalMapping(GlobalValue_sp value, core::Pointer_sp ptr);
  /*! Add a global mapping for an object, give it a new name and return the GlobalVariable_sp */
  void addGlobalMappingForLoadTimeValueVector(GlobalValue_sp value, const string& name);

  void runFunction(Function_sp func, core::String_sp fileName); //, core::Cons_sp args );
};                                                              // ExecutionEngine_O
};                                                              // namespace llvmo

namespace llvmo {
FORWARD(Module);
};

namespace llvmo {

static std::atomic<size_t> global_NextModuleId;
class Module_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Module, Module_O, "module", core::ExternalObject_O);
  typedef llvm::Module ExternalType;
  typedef llvm::Module* PointerToExternalType;
  void initialize();
  GCPROTECTED : size_t _Id;
  std::string _UniqueName;
  PointerToExternalType _ptr;
  core::HashTableEqual_sp _UniqueGlobalVariableStrings;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const {
    if (this->_ptr)
      return this->_ptr;
    SIMPLE_ERROR("The Module has a NULL pointer");
  }
  void reset_wrappedPtr() { this->_ptr = NULL; }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  Module_O() : Base(), _ptr(NULL){};
  ~Module_O() {
    // delete _ptr;   // Don't delete the module Delete the module when it's not used
    _ptr = NULL;
  }
  std::string __repr__() const;
  CL_DEFMETHOD size_t module_id() const { return this->_Id; };
  static Module_sp make(const std::string& namePrefix, LLVMContext_sp context);
  /*! Return true if the wrapped Module is defined */
  bool valid() const;
  llvm::DataLayout getDataLayout() const;
  std::string getUniqueName() const { return this->_UniqueName; };

  /*! Return a Cons of all the globals for this module */
  core::List_sp getGlobalList() const;

public:
  /*! Return a list of all functions as a cons */
  core::List_sp getFunctionList() const;

  LLVMContext_sp getContext() const;

  /*! Wrap the Module::getFunction function */
  llvm::Function* getFunction(core::String_sp dispatchName);

  void emit_version_ident_metadata();

  /*! Get or create a string GlobalVariable with the given name.
          Make sure that the string passed is the same as the string
          in the GlobalVariable.
        I created this method to avoid lots of duplicate strings being
        created as global variables within the Module. */
  GlobalVariable_sp getOrCreateUniquedStringGlobalVariable(const string& value, const string& name);

  /*! Delete the module */
  void moduleDelete();

  /*! Get a Cons of all named MetaData */
  void dump_namedMDList() const;

}; // Module_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::Module*> {
  typedef llvm::Module* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::Module_sp>(object)->wrappedPtr()){};
};
template <> struct from_object<llvm::Module&> {
  typedef llvm::Module& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::Module_sp>(object)->wrappedPtr()){};
  ~from_object(){/*non trivial*/};
};
}; // namespace translate
    ;
/* to_object translators */
namespace translate {
template <> struct to_object<llvm::Module*> {
  static core::T_sp convert(llvm::Module* ptr) { return ((core::RP_Create_wrapped<llvmo::Module_O, llvm::Module*>(ptr))); }
};
}; // namespace translate
    ;

/* from_object translators */

namespace translate {
template <> struct from_object<llvm::ExecutionEngine*> {
  typedef llvm::ExecutionEngine* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ExecutionEngine_sp>(object)->wrappedPtr()){};
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ExecutionEngine*> {
  static core::T_sp convert(llvm::ExecutionEngine* ptr) {
    return ((core::RP_Create_wrapped<llvmo::ExecutionEngine_O, llvm::ExecutionEngine*>(ptr)));
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(EngineBuilder);
class EngineBuilder_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::EngineBuilder, EngineBuilder_O, "ENGINEBUILDER", core::ExternalObject_O);
  typedef llvm::EngineBuilder ExternalType;
  typedef llvm::EngineBuilder* PointerToExternalType;

protected:
  PointerToExternalType _ptr;
  string _ErrorStr; // store creation errors here
public:
  string __repr__() const;
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  CL_LISPIFY_NAME("error_string");
  CL_DEFMETHOD string error_string() const { return this->_ErrorStr; };
  // CL_DEFMETHOD void setUseOrcMCJITReplacement(bool use);

  EngineBuilder_O() : Base(), _ptr(NULL){};
  ~EngineBuilder_O() {
    /* delete _ptr;*/
    _ptr = NULL;
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
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::EngineBuilder*> {
  typedef llvm::EngineBuilder* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = (gc::As<llvmo::EngineBuilder_sp>(object)->wrappedPtr()); };
};
}; // namespace translate
/* to_object translators */

namespace llvmo {
FORWARD(APFloat);
class APFloat_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::APFloat, APFloat_O, "APFLOAT", core::ExternalObject_O);

public:
  typedef llvm::APFloat ExternalType;
  llvm::APFloat* _valueP;

public:
  static APFloat_sp makeAPFloatFloat(core::SingleFloat_sp value);
  static APFloat_sp makeAPFloatDouble(core::DoubleFloat_sp value);

public:
  APFloat_O() : Base(), _valueP(new llvm::APFloat(0.0)){};
  ~APFloat_O() { delete this->_valueP; };
}; // APFloat_O
}; // namespace llvmo
namespace translate {
template <> struct from_object<const llvm::APFloat&> {
  typedef llvm::APFloat DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::APFloat_sp>(object)->_valueP){};
};
}; // namespace translate

/* to_object translators */

namespace llvmo {
FORWARD(APInt);
class APInt_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::APInt, APInt_O, "APINT", core::ExternalObject_O);

public:
  typedef llvm::APInt ExternalType;
  dont_expose<llvm::APInt> _value;

public:
  static APInt_sp create(llvm::APInt i);
  /*! Return an APInt that has the value of (val) */
  static APInt_sp makeAPInt(core::Integer_sp val);
  static APInt_sp makeAPIntWidth(core::Integer_sp val, uint bitwidth, bool sign);
  static APInt_sp makeAPInt1(core::T_sp val);
  static APInt_sp makeAPInt32(core::Integer_sp val);
  static APInt_sp makeAPInt64(core::Integer_sp val);

public:
  core::Integer_sp toInteger(bool issigned) const;

public:
  string __repr__() const;
}; // APInt_O
}; // namespace llvmo
/* from_object translators */
namespace translate {
template <> struct from_object<const llvm::APInt&> {
  typedef llvm::APInt DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::APInt_sp>(object)->_value._value){};
};
/* to_object translators */
template <> struct to_object<llvm::APInt> {
  static core::T_sp convert(llvm::APInt sr) { return llvmo::APInt_O::create(sr); };
};
}; // namespace translate

template <> struct gctools::GCInfo<llvmo::IRBuilderBase_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(IRBuilderBase);
class IRBuilderBase_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IRBuilderBase, IRBuilderBase_O, "IRBuilderBase", core::ExternalObject_O);
  typedef llvm::IRBuilderBase ExternalType;
  typedef llvm::IRBuilderBase* PointerToExternalType;

protected:
  PointerToExternalType _ptr;
  bool _CurrentDebugLocationSet;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static IRBuilderBase_sp create(llvm::IRBuilderBase* ptr);
  core::T_sp getInsertPointInstruction();
  IRBuilderBase_O() : Base(), _ptr(NULL), _CurrentDebugLocationSet(false){};
  ~IRBuilderBase_O() {
    if (_ptr != NULL) {
      auto ptr = this->_ptr;
      //      printf("%s:%d:%s registering dtor\n", __FILE__, __LINE__, __FUNCTION__ );
      core::thread_local_register_cleanup([ptr](void) {
#ifdef DEBUG_DTORS
        printf("%s:%d:%s dtor %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
#endif
        delete ptr;
      });
      _ptr = NULL;
    };
  }

public:
  void restoreIP(InsertPoint_sp insertPoint);
  InsertPoint_sp saveIP();

  void ClearCurrentDebugLocation();

  /*! Set the current debug location for generated code */
  void SetCurrentDebugLocation(DILocation_sp diloc);
  /*! Set the current debug location by building a DebugLoc on the fly */
  void SetCurrentDebugLocationToLineColumnScope(int line, int col, DINode_sp scope);
  CL_LISPIFY_NAME("CurrentDebugLocation");
  CL_DEFMETHOD core::T_sp CurrentDebugLocation() { return _lisp->_boolean(this->_CurrentDebugLocationSet); };
}; // IRBuilderBase_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::IRBuilderBase*> {
  typedef llvm::IRBuilderBase* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::IRBuilderBase_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::IRBuilderBase*> {
  static core::T_sp convert(llvm::IRBuilderBase* ptr) { return ((llvmo::IRBuilderBase_O::create(ptr))); }
};
}; // namespace translate
    ;

template <> struct gctools::GCInfo<llvmo::IRBuilder_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(IRBuilder);
FORWARD(FunctionType);
class IRBuilder_O : public IRBuilderBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IRBuilder<>, IRBuilder_O, "IRBUILDER", IRBuilderBase_O);
  typedef llvm::IRBuilder<> ExternalType;
  typedef llvm::IRBuilder<>* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    delete this->_ptr;
    this->_ptr = ptr;
  }
  static IRBuilder_sp make(LLVMContext_sp context);

public:
  llvm::InvokeInst* CreateInvoke(FunctionType_sp function_type, llvm::Value* Callee, llvm::BasicBlock* NormalDest,
                                 llvm::BasicBlock* UnwindDest, core::List_sp Args, const llvm::Twine& Name = "");
  llvm::Value* CreateConstGEP2_32(llvm::Type* ty, llvm::Value* ptr, int idx0, int idx1, const llvm::Twine& Name);
  llvm::Value* CreateConstGEP2_64(llvm::Type* ty, llvm::Value* Ptr, size_t idx0, size_t idx1, const llvm::Twine& Name);
  llvm::Value* CreateInBoundsGEP(llvm::Type* ty, llvm::Value* Ptr, core::List_sp IdxList, const llvm::Twine& Name = "");

  llvm::Value* CreateExtractValue(llvm::Value* Ptr, core::List_sp IdxList, const llvm::Twine& Name = "");

  llvm::Value* CreateInsertValue(llvm::Value* Agg, llvm::Value* Val, core::List_sp IdxList, const llvm::Twine& Name = "");

  string __repr__() const;
}; // IRBuilder_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(Instruction);

class Instruction_O : public User_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Instruction, Instruction_O, "Instruction", User_O);
  typedef llvm::Instruction ExternalType;
  typedef llvm::Instruction* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  core::T_sp getNextNode(); // instruction or nil
  core::T_sp getPrevNode(); // instruction or nil
  core::T_sp getParent();   // basic block or nil
  CL_DEFMETHOD bool CallInstP() const { return llvm::isa<llvm::CallInst>(this->wrappedPtr()); };
  CL_DEFMETHOD bool InvokeInstP() const { return llvm::isa<llvm::InvokeInst>(this->wrappedPtr()); };

public:
  void setMetadata(core::String_sp kind, MDNode_sp mdnode);

  bool terminatorInstP() const;
}; // Instruction_O
}; // namespace llvmo
namespace translate {
template <> struct from_object<llvm::Instruction*> {
  typedef llvm::Instruction* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::Instruction_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Instruction*> {
  static core::T_sp convert(llvm::Instruction* ptr) {
    // Wrap the Instruction* using the most derived class possible
    if (llvm::isa<llvm::CallInst>(ptr)) {
      return core::RP_Create_wrapped<llvmo::CallInst_O, llvm::CallInst*>(reinterpret_cast<llvm::CallInst*>(ptr));
    } else if (llvm::isa<llvm::InvokeInst>(ptr)) {
      return core::RP_Create_wrapped<llvmo::InvokeInst_O, llvm::InvokeInst*>(reinterpret_cast<llvm::InvokeInst*>(ptr));
    }
    return ((core::RP_Create_wrapped<llvmo::Instruction_O, llvm::Instruction*>(ptr)));
  }
};
}; // namespace translate
    ;

/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(StoreInst);
class StoreInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::StoreInst, StoreInst_O, "StoreInst", Instruction_O);
  typedef llvm::StoreInst ExternalType;
  typedef llvm::StoreInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  void setAlignment(core::T_sp align);

}; // StoreInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::StoreInst*> {
  typedef llvm::StoreInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::StoreInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::StoreInst*> {
  static core::T_sp convert(llvm::StoreInst* ptr) { return ((core::RP_Create_wrapped<llvmo::StoreInst_O, llvm::StoreInst*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(FenceInst);
class FenceInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::FenceInst, FenceInst_O, "FenceInst", Instruction_O);
  typedef llvm::FenceInst ExternalType;
  typedef llvm::FenceInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // FenceInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::FenceInst*> {
  typedef llvm::FenceInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::FenceInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::FenceInst*> {
  static core::T_sp convert(llvm::FenceInst* ptr) { return ((core::RP_Create_wrapped<llvmo::FenceInst_O, llvm::FenceInst*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(AtomicCmpXchgInst);
class AtomicCmpXchgInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AtomicCmpXchgInst, AtomicCmpXchgInst_O, "AtomicCmpXchgInst", Instruction_O);
  typedef llvm::AtomicCmpXchgInst ExternalType;
  typedef llvm::AtomicCmpXchgInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // AtomicCmpXchgInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::AtomicCmpXchgInst*> {
  typedef llvm::AtomicCmpXchgInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::AtomicCmpXchgInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::AtomicCmpXchgInst*> {
  static core::T_sp convert(llvm::AtomicCmpXchgInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::AtomicCmpXchgInst_O, llvm::AtomicCmpXchgInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(AtomicRMWInst);
class AtomicRMWInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AtomicRMWInst, AtomicRMWInst_O, "AtomicRMWInst", Instruction_O);
  typedef llvm::AtomicRMWInst ExternalType;
  typedef llvm::AtomicRMWInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // AtomicRMWInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::AtomicRMWInst*> {
  typedef llvm::AtomicRMWInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::AtomicRMWInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::AtomicRMWInst*> {
  static core::T_sp convert(llvm::AtomicRMWInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::AtomicRMWInst_O, llvm::AtomicRMWInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(PHINode);
class PHINode_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::PHINode, PHINode_O, "PHINode", Instruction_O);
  typedef llvm::PHINode ExternalType;
  typedef llvm::PHINode* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // PHINode_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::PHINode*> {
  typedef llvm::PHINode* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::PHINode_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::PHINode*> {
  static core::T_sp convert(llvm::PHINode* ptr) { return ((core::RP_Create_wrapped<llvmo::PHINode_O, llvm::PHINode*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {

/* The LLVM enum for calling conventions is completely anonymous, which
 * makes translating Lisp function arguments annoying.
 * So here we define our own enum with what we need, and use that.
 * FIXME: Move, maybe? */

enum ClaspCallingConv { C = llvm::CallingConv::C, Fast = llvm::CallingConv::Fast };
}; // namespace llvmo

ENUM_TRANSLATOR(llvmo::ClaspCallingConv, llvmo::_sym_CallingConv);

namespace llvmo {

FORWARD(CallBase);
class CallBase_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::CallBase, CallBase_O, "CallBase", Instruction_O);
  typedef llvm::CallBase ExternalType;
  typedef llvm::CallBase* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  void addParamAttr(unsigned ArgNo, llvm::Attribute::AttrKind Attr);
  core::List_sp getArgumentList() const;
  llvm::Function* getCalledFunction();

}; // CallBase_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(CallInst);
class CallInst_O : public CallBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::CallInst, CallInst_O, "CallInst", CallBase_O);
  typedef llvm::CallInst ExternalType;
  typedef llvm::CallInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  CL_DEFMETHOD bool CallInstP() const { return true; };

}; // CallInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::CallInst*> {
  typedef llvm::CallInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::CallInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::CallInst*> {
  static core::T_sp convert(llvm::CallInst* ptr) { return ((core::RP_Create_wrapped<llvmo::CallInst_O, llvm::CallInst*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(LandingPadInst);
class LandingPadInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LandingPadInst, LandingPadInst_O, "LandingPadInst", Instruction_O);
  typedef llvm::LandingPadInst ExternalType;
  typedef llvm::LandingPadInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // LandingPadInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::LandingPadInst*> {
  typedef llvm::LandingPadInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::LandingPadInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::LandingPadInst*> {
  static core::T_sp convert(llvm::LandingPadInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::LandingPadInst_O, llvm::LandingPadInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(UnaryInstruction);
class UnaryInstruction_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::UnaryInstruction, UnaryInstruction_O, "UnaryInstruction", Instruction_O);
  typedef llvm::UnaryInstruction ExternalType;
  typedef llvm::UnaryInstruction* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

}; // UnaryInstruction_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace llvmo {
FORWARD(AllocaInst);
class AllocaInst_O : public UnaryInstruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::AllocaInst, AllocaInst_O, "AllocaInst", UnaryInstruction_O);
  typedef llvm::AllocaInst ExternalType;
  typedef llvm::AllocaInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  void setAlignment(core::T_sp align);

}; // AllocaInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::AllocaInst*> {
  typedef llvm::AllocaInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::AllocaInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::AllocaInst*> {
  static core::T_sp convert(llvm::AllocaInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::AllocaInst_O, llvm::AllocaInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(VAArgInst);
class VAArgInst_O : public UnaryInstruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::VAArgInst, VAArgInst_O, "VAArgInst", UnaryInstruction_O);
  typedef llvm::VAArgInst ExternalType;
  typedef llvm::VAArgInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // VAArgInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::VAArgInst*> {
  typedef llvm::VAArgInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::VAArgInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::VAArgInst*> {
  static core::T_sp convert(llvm::VAArgInst* ptr) { return ((core::RP_Create_wrapped<llvmo::VAArgInst_O, llvm::VAArgInst*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(LoadInst);
class LoadInst_O : public UnaryInstruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::LoadInst, LoadInst_O, "LoadInst", UnaryInstruction_O);
  typedef llvm::LoadInst ExternalType;
  typedef llvm::LoadInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  void setAlignment(core::T_sp align);
}; // LoadInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::LoadInst*> {
  typedef llvm::LoadInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::LoadInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::LoadInst*> {
  static core::T_sp convert(llvm::LoadInst* ptr) { return ((core::RP_Create_wrapped<llvmo::LoadInst_O, llvm::LoadInst*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(BranchInst);
class BranchInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::BranchInst, BranchInst_O, "BranchInst", Instruction_O);
  typedef llvm::BranchInst ExternalType;
  typedef llvm::BranchInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // BranchInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::BranchInst*> {
  typedef llvm::BranchInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::BranchInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::BranchInst*> {
  static core::T_sp convert(llvm::BranchInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::BranchInst_O, llvm::BranchInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(SwitchInst);
class SwitchInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::SwitchInst, SwitchInst_O, "SwitchInst", Instruction_O);
  typedef llvm::SwitchInst ExternalType;
  typedef llvm::SwitchInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  void addCase(ConstantInt_sp onVal, BasicBlock_sp dest);

}; // SwitchInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::SwitchInst*> {
  typedef llvm::SwitchInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::SwitchInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::SwitchInst*> {
  static core::T_sp convert(llvm::SwitchInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::SwitchInst_O, llvm::SwitchInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(IndirectBrInst);
class IndirectBrInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IndirectBrInst, IndirectBrInst_O, "IndirectBrInst", Instruction_O);
  typedef llvm::IndirectBrInst ExternalType;
  typedef llvm::IndirectBrInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // IndirectBrInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::IndirectBrInst*> {
  typedef llvm::IndirectBrInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::IndirectBrInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::IndirectBrInst*> {
  static core::T_sp convert(llvm::IndirectBrInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::IndirectBrInst_O, llvm::IndirectBrInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(InvokeInst);
class InvokeInst_O : public CallBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::InvokeInst, InvokeInst_O, "InvokeInst", CallBase_O);
  typedef llvm::InvokeInst ExternalType;
  typedef llvm::InvokeInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  CL_DEFMETHOD bool InvokeInstP() const { return true; };
}; // InvokeInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::InvokeInst*> {
  typedef llvm::InvokeInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::InvokeInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::InvokeInst*> {
  static core::T_sp convert(llvm::InvokeInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::InvokeInst_O, llvm::InvokeInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ResumeInst);
class ResumeInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ResumeInst, ResumeInst_O, "ResumeInst", Instruction_O);
  typedef llvm::ResumeInst ExternalType;
  typedef llvm::ResumeInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // ResumeInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::ResumeInst*> {
  typedef llvm::ResumeInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::ResumeInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ResumeInst*> {
  static core::T_sp convert(llvm::ResumeInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::ResumeInst_O, llvm::ResumeInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(UnreachableInst);
class UnreachableInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::UnreachableInst, UnreachableInst_O, "UnreachableInst", Instruction_O);
  typedef llvm::UnreachableInst ExternalType;
  typedef llvm::UnreachableInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // UnreachableInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::UnreachableInst*> {
  typedef llvm::UnreachableInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::UnreachableInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::UnreachableInst*> {
  static core::T_sp convert(llvm::UnreachableInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::UnreachableInst_O, llvm::UnreachableInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ReturnInst);
class ReturnInst_O : public Instruction_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ReturnInst, ReturnInst_O, "ReturnInst", Instruction_O);
  typedef llvm::ReturnInst ExternalType;
  typedef llvm::ReturnInst* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
}; // ReturnInst_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::ReturnInst*> {
  typedef llvm::ReturnInst* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::ReturnInst_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ReturnInst*> {
  static core::T_sp convert(llvm::ReturnInst* ptr) {
    return ((core::RP_Create_wrapped<llvmo::ReturnInst_O, llvm::ReturnInst*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ConstantFP);
class ConstantFP_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantFP, ConstantFP_O, "ConstantFP", Constant_O);
  typedef llvm::ConstantFP ExternalType;
  typedef llvm::ConstantFP* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantFP_sp create(llvm::ConstantFP* ptr);
  ;
public:
  string __repr__() const;

}; // ConstantFP_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ConstantFP*> {
  static core::T_sp convert(llvm::ConstantFP* ptr) { return ((llvmo::ConstantFP_O::create(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ConstantInt);
class ConstantInt_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantInt, ConstantInt_O, "ConstantInt", Constant_O);
  typedef llvm::ConstantInt ExternalType;
  typedef llvm::ConstantInt* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantInt_sp create(llvm::ConstantInt* ptr);
  ;
public:
  string __repr__() const;
}; // ConstantInt_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ConstantInt*> {
  static core::T_sp convert(llvm::ConstantInt* ptr) { return ((llvmo::ConstantInt_O::create(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ConstantStruct);
class ConstantStruct_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantStruct, ConstantStruct_O, "ConstantStruct", Constant_O);
  typedef llvm::ConstantStruct ExternalType;
  typedef llvm::ConstantStruct* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantStruct_sp create(llvm::ConstantStruct* ptr);
  ;
public:
}; // ConstantStruct_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ConstantStruct*> {
  static core::T_sp convert(llvm::ConstantStruct* ptr) { return ((llvmo::ConstantStruct_O::create(ptr))); }
};

}; // namespace translate
    ;

namespace llvmo {
FORWARD(UndefValue);
class UndefValue_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::UndefValue, UndefValue_O, "UndefValue", Constant_O);
  typedef llvm::UndefValue ExternalType;
  typedef llvm::UndefValue* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static UndefValue_sp create(llvm::UndefValue* ptr);
  ;
public:
  string __repr__() const;
}; // UndefValue_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::UndefValue*> {
  static core::T_sp convert(llvm::UndefValue* ptr) { return ((llvmo::UndefValue_O::create(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ConstantPointerNull);
class ConstantPointerNull_O : public Constant_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ConstantPointerNull, ConstantPointerNull_O, "ConstantPointerNull", Constant_O);
  typedef llvm::ConstantPointerNull ExternalType;
  typedef llvm::ConstantPointerNull* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return llvm_cast<ExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static ConstantPointerNull_sp create(llvm::ConstantPointerNull* ptr);
  ;
public:
  string __repr__() const;
}; // ConstantPointerNull_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ConstantPointerNull*> {
  static core::T_sp convert(llvm::ConstantPointerNull* ptr) { return ((llvmo::ConstantPointerNull_O::create(ptr))); }
};

template <> struct from_object<llvm::ConstantPointerNull*> {
  typedef llvm::ConstantPointerNull* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::ConstantPointerNull_sp>(object)->wrappedPtr()){};
};

}; // namespace translate
    ;

namespace llvmo {
FORWARD(MDNode);
class MDNode_O : public Metadata_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MDNode, MDNode_O, "MDNode", Metadata_O);
  typedef llvm::MDNode ExternalType;
  typedef llvm::MDNode* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
public:
  static MDNode_sp get(LLVMContext_sp context, core::List_sp values);

}; // MDNode_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::MDNode*> {
  typedef llvm::MDNode* DeclareType;
  DeclareType _v;
  from_object(T_P o) : _v(o.nilp() ? NULL : gc::As<llvmo::MDNode_sp>(o)->wrappedPtr()){};
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::MDNode*> {
  static llvmo::MDNode_mv convert(llvm::MDNode* ptr) {
    return (Values(core::RP_Create_wrapped<llvmo::MDNode_O, llvm::MDNode*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(MDString);
class MDString_O : public Metadata_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::MDString, MDString_O, "MDString", Metadata_O);
  typedef llvm::MDString ExternalType;
  typedef llvm::MDString* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public:
  static MDString_sp get(LLVMContext_sp context, core::String_sp str);

}; // MDString_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::MDString*> {
  typedef llvm::MDString* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::MDString_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::MDString*> {
  static core::T_sp convert(llvm::MDString* ptr) { return ((core::RP_Create_wrapped<llvmo::MDString_O, llvm::MDString*>(ptr))); }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(ValueAsMetadata);
class ValueAsMetadata_O : public Metadata_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ValueAsMetadata, ValueAsMetadata_O, "ValueAsMetadata", Metadata_O);
  typedef llvm::ValueAsMetadata ExternalType;
  typedef llvm::ValueAsMetadata* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public:
  static ValueAsMetadata_sp get(Value_sp val);

}; // ValueAsMetadata_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::ValueAsMetadata*> {
  typedef llvm::ValueAsMetadata* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = gc::As<llvmo::ValueAsMetadata_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ValueAsMetadata*> {
  static core::T_sp convert(llvm::ValueAsMetadata* ptr) {
    return ((core::RP_Create_wrapped<llvmo::ValueAsMetadata_O, llvm::ValueAsMetadata*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(NamedMDNode);
class NamedMDNode_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::NamedMDNode, NamedMDNode_O, "NamedMDNode", core::ExternalObject_O);
  typedef llvm::NamedMDNode ExternalType;
  typedef llvm::NamedMDNode* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  PointerToExternalType wrappedPtr() const { return llvm_cast<ExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public:
  llvm::MDNode* getOperand(uint i) { return this->_ptr->getOperand(i); };
  uint getNumOperands() { return this->_ptr->getNumOperands(); };
  CL_LISPIFY_NAME("addOperand");
  CL_DEFMETHOD void addOperand(llvm::MDNode* m) { this->_ptr->addOperand(m); };
  string getName() { return this->_ptr->getName().str(); };

}; // NamedMDNode_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::NamedMDNode*> {
  typedef llvm::NamedMDNode* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = object.nilp() ? NULL : gc::As<llvmo::NamedMDNode_sp>(object)->wrappedPtr(); };
};
}; // namespace translate
    ;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::NamedMDNode*> {
  static llvmo::NamedMDNode_mv convert(llvm::NamedMDNode* ptr) {
    return (Values(core::RP_Create_wrapped<llvmo::NamedMDNode_O, llvm::NamedMDNode*>(ptr)));
  }
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(Function);
class Function_O : public GlobalValue_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Function, Function_O, "FUNCTION", GlobalValue_O);
  typedef llvm::Function ExternalType;
  typedef llvm::Function* PointerToExternalType;

  GCPRIVATE : core::LoadTimeValues_sp _RunTimeValues;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  string __repr__() const;

  bool Function_equal(core::T_sp obj) const;

  LLVMContext_sp getContext() const;

  void addReturnAttr(typename llvm::Attribute::AttrKind);
  core::List_sp getArgumentList();
  void appendBasicBlock(BasicBlock_sp basicBlock);
  BasicBlock_sp getEntryBlock() const;
  core::List_sp basic_blocks() const;
}; // Function_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Function*> {
  static core::T_sp convert(llvm::Function* ptr) {
    if (ptr == NULL)
      return ((nil<core::T_O>()));
    return ((core::RP_Create_wrapped<llvmo::Function_O, llvm::Function*>(ptr)));
  };
};

template <> struct to_object<const llvm::Function&> {
  static core::T_sp convert(const llvm::Function& val) {
    return ((core::RP_Create_wrapped<llvmo::Function_O, llvm::Function*>(const_cast<llvm::Function*>(&val))));
  };
};

template <> struct from_object<llvm::Function*> {
  typedef llvm::Function* DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.nilp()) {
      this->_v = NULL;
    } else {
      this->_v = static_cast<llvm::Function*>(gc::As<llvmo::Function_sp>(object)->wrappedPtr());
    }
  }
};
template <> struct from_object<const llvm::Function&> {
  typedef llvm::Function const& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(static_cast<llvm::Function*>(gc::As<llvmo::Function_sp>(object)->wrappedPtr()))){};
};
template <> struct from_object<llvm::Function&> {
  typedef llvm::Function& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*(static_cast<llvm::Function*>(gc::As<llvmo::Function_sp>(object)->wrappedPtr()))){};
  ~from_object(){/*non trivial*/};
};
}; // namespace translate

namespace llvmo {
FORWARD(BasicBlock);
class BasicBlock_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::BasicBlock, BasicBlock_O, "BasicBlock", Value_O);
  typedef llvm::BasicBlock ExternalType;
  typedef llvm::BasicBlock* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

  bool empty();
  size_t size();
  Instruction_sp back();

  core::List_sp instructions() const;
  size_t number_of_instructions() const;

}; // BasicBlock_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::BasicBlock*> {
  static core::T_sp convert(llvm::BasicBlock* ptr) {
    if (ptr != NULL) {
      return ((core::RP_Create_wrapped<llvmo::BasicBlock_O, llvm::BasicBlock*>(ptr)));
    }
    return ((nil<core::T_O>()));
  };
};
template <> struct from_object<llvm::BasicBlock*> {
  typedef llvm::BasicBlock* DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    this->_v = object.nilp() ? NULL : static_cast<llvm::BasicBlock*>(gc::As<llvmo::BasicBlock_sp>(object)->wrappedPtr());
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(Argument);
class Argument_O : public Value_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Argument, Argument_O, "Argument", Value_O);
  typedef llvm::Argument ExternalType;
  typedef llvm::Argument* PointerToExternalType;

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

  void addAttr(llvm::Attribute a);

public:
}; // Argument_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::Argument*> {
  static core::T_sp convert(llvm::Argument* ptr) { return ((core::RP_Create_wrapped<llvmo::Argument_O, llvm::Argument*>(ptr))); };
};
template <> struct to_object<llvm::Argument> {
  static core::T_sp convert(llvm::Argument& arg) { return ((core::RP_Create_wrapped<llvmo::Argument_O, llvm::Argument*>(&arg))); };
};
}; // namespace translate

namespace llvmo {
FORWARD(Type);
class Type_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::Type, Type_O, "TYPE", core::ExternalObject_O);
  typedef llvm::Type ExternalType;
  typedef llvm::Type* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

public:
  PointerToExternalType wrappedPtr() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  static Type_sp create(llvm::Type* ptr);
  ;
  Type_O() : Base(), _ptr(NULL){};
  ~Type_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

  core::Integer_sp getArrayNumElements() const;

  PointerType_sp getPointerTo(int addressSpace = 0);

  bool Type_equal(core::T_sp obj) const;
  LLVMContext_sp getContext() const;
  string __repr__() const;
  void __write__(core::T_sp stream) const;

}; // Type_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::Type*> {
  typedef llvm::Type* DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.nilp()) {
      this->_v = NULL;
      return;
    }
    this->_v = (gc::As<llvmo::Type_sp>(object)->wrappedPtr());
  };
};

/* to_object translators */
template <> struct to_object<llvm::Type*> {
  static core::T_sp convert(llvm::Type* ptr) { return ((core::RP_Create_wrapped<llvmo::Type_O, llvm::Type*>(ptr))); };
};
}; // namespace translate
    ;

namespace llvmo {
FORWARD(FunctionType);
class FunctionType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::FunctionType, FunctionType_O, "FUNCTION-TYPE", Type_O);
  typedef llvm::FunctionType ExternalType;
  typedef llvm::FunctionType* PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public: // static methods
  static core::T_sp get(llvm::Type* result_type, core::T_sp params, core::T_sp is_var_arg);
}; // FunctionType_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::FunctionType*> {
  static core::T_sp convert(llvm::FunctionType* ptr) {
    return ((core::RP_Create_wrapped<llvmo::FunctionType_O, llvm::FunctionType*>(ptr)));
  };
};
template <> struct from_object<llvm::FunctionType*> {
  typedef llvm::FunctionType* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = static_cast<llvm::FunctionType*>(gc::As<llvmo::FunctionType_sp>(object)->wrappedPtr()); }
};

}; // namespace translate
    ;

namespace llvmo {
FORWARD(IntegerType);
class IntegerType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::IntegerType, IntegerType_O, "INTEGER-TYPE", Type_O);
  typedef llvm::IntegerType ExternalType;
  typedef llvm::IntegerType* PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public: // static methods
  static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
}; // IntegerType_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::IntegerType*> {
  static core::T_sp convert(llvm::IntegerType* ptr) {
    return ((core::RP_Create_wrapped<llvmo::IntegerType_O, llvm::IntegerType*>(ptr)));
  };
};
template <> struct from_object<llvm::IntegerType*> {
  typedef llvm::IntegerType* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = static_cast<llvm::IntegerType*>(gc::As<llvmo::IntegerType_sp>(object)->wrappedPtr()); }
};
}; // namespace translate

namespace llvmo {
FORWARD(StructType);
class StructType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::StructType, StructType_O, "StructType", Type_O);
  typedef llvm::StructType ExternalType;
  typedef llvm::StructType* PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public: // static methods
  /*! Get a structure using llvm:StructType::create(LLVMContext& context, ArrayRef<Type*>Elements,StringRef name,bool isPacked) */
  static StructType_sp make(LLVMContext_sp context, core::T_sp elements, llvm::StringRef name, core::T_sp isPacked);

  static StructType_sp get(LLVMContext_sp context, core::T_sp elements, bool isPacked = false);

public:
  void setBody(core::T_sp elements, core::T_sp isPacked);
  bool indexValid(unsigned idx);
}; // StructType_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::StructType*> {
  static llvmo::StructType_mv convert(llvm::StructType* ptr) {
    return (Values(core::RP_Create_wrapped<llvmo::StructType_O, llvm::StructType*>(ptr)));
  };
};
template <> struct from_object<llvm::StructType*> {
  typedef llvm::StructType* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = static_cast<llvm::StructType*>(gc::As<llvmo::StructType_sp>(object)->wrappedPtr()); }
};

}; // namespace translate

namespace llvmo {
FORWARD(PointerType);
class PointerType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::PointerType, PointerType_O, "PointerType", Type_O);
  typedef llvm::PointerType ExternalType;
  typedef llvm::PointerType* PointerToExternalType;

public:
  PointerToExternalType wrapped() { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public: // static methods
        //	static core::T_sp get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg);
public: // static methods
  static PointerType_sp get(Type_sp elementType, uint addressSpace);

  llvm::Type* getElementType() const;

}; // PointerType_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::PointerType*> {
  static llvmo::PointerType_mv convert(llvm::PointerType* ptr) {
    return (Values(core::RP_Create_wrapped<llvmo::PointerType_O, llvm::PointerType*>(ptr)));
  };
};
template <> struct from_object<llvm::PointerType*> {
  typedef llvm::PointerType* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = static_cast<llvm::PointerType*>(gc::As<llvmo::PointerType_sp>(object)->wrappedPtr()); }
};

}; // namespace translate
    ;

namespace llvmo {
FORWARD(ArrayType);
class ArrayType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::ArrayType, ArrayType_O, "ArrayType", Type_O);
  typedef llvm::ArrayType ExternalType;
  typedef llvm::ArrayType* ArrayToExternalType;

public:
  ArrayToExternalType wrapped() { return static_cast<ArrayToExternalType>(this->_ptr); };
  void set_wrapped(ArrayToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public: // static methods
  static ArrayType_sp get(Type_sp elementType, uint64_t numElements);

}; // ArrayType_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::ArrayType*> {
  static core::T_sp convert(llvm::ArrayType* ptr) {
    return ((core::RP_Create_wrapped<llvmo::ArrayType_O, llvm::ArrayType*>(ptr)));
  };
};
template <> struct from_object<llvm::ArrayType*> {
  typedef llvm::ArrayType* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = static_cast<llvm::ArrayType*>(gc::As<llvmo::ArrayType_sp>(object)->wrappedPtr()); }
};

}; // namespace translate
    ;

namespace llvmo {
FORWARD(VectorType);
class VectorType_O : public Type_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::VectorType, VectorType_O, "VectorType", Type_O);
  typedef llvm::VectorType ExternalType;
  typedef llvm::VectorType* VectorToExternalType;

public:
  VectorToExternalType wrapped() { return static_cast<VectorToExternalType>(this->_ptr); };
  void set_wrapped(VectorToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public: // static methods
  static VectorType_sp get(Type_sp etype, unsigned nelems, bool scalablep);
}; // VectorType_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::VectorType*> {
  static core::T_sp convert(llvm::VectorType* ptr) {
    return ((core::RP_Create_wrapped<llvmo::VectorType_O, llvm::VectorType*>(ptr)));
  };
};
template <> struct from_object<llvm::VectorType*> {
  typedef llvm::VectorType* DeclareType;
  DeclareType _v;
  from_object(T_P object) { this->_v = static_cast<llvm::VectorType*>(gc::As<llvmo::VectorType_sp>(object)->wrappedPtr()); }
};
}; // namespace translate

namespace translate {

template <> struct from_object<const llvm::StringRef> {
  typedef llvm::StringRef DeclareType;
  DeclareType _v;
  string _Storage;
  from_object(T_P object) : _v(this->_Storage), _Storage(gc::As<core::String_sp>(object)->get_std_string()){};
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _v(_Storage), _Storage(std::move(orig._Storage)){};
};

template <> struct from_object<llvm::GlobalValue::LinkageTypes> {
  typedef llvm::GlobalValue::LinkageTypes DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter =
          gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARglobal_value_linkage_typesSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::GlobalValue::LinkageTypes>(sym);
      return;
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::GlobalValue::LinkageType", _rep_(object));
  }
};

template <> struct from_object<llvm::GlobalValue::ThreadLocalMode> {
  typedef llvm::GlobalValue::ThreadLocalMode DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (object.notnilp()) {
      if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter =
            gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARglobal_ThreadLocalModesSTAR->symbolValue());
        this->_v = converter->enumForSymbol<llvm::GlobalValue::ThreadLocalMode>(sym);
        return;
      }
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::GlobalValue::ThreadLocalMode", _rep_(object));
  }
};

template <> struct from_object<llvm::AtomicOrdering> {
  typedef llvm::AtomicOrdering DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) {
    if (object.notnilp()) {
      if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
        core::SymbolToEnumConverter_sp converter =
            gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARatomic_orderingSTAR->symbolValue());
        this->_v = converter->enumForSymbol<llvm::AtomicOrdering>(sym);
        return;
      }
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::AtomicOrdering", _rep_(object));
  }
};

template <> struct from_object<llvm::AtomicRMWInst::BinOp> {
  typedef llvm::AtomicRMWInst::BinOp DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter =
          gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARAtomicRMWInstBinOpSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::AtomicRMWInst::BinOp>(sym);
      return;
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::AtomicRMWInst::BinOp", _rep_(object));
  }
};

template <> struct from_object<llvm::Instruction::CastOps> {
  typedef llvm::Instruction::CastOps DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter =
          gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARInstructionCastOpsSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::Instruction::CastOps>(sym);
      return;
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::Instruction::CastOps", _rep_(object));
  }
};

template <> struct from_object<llvm::Instruction::BinaryOps> {
  typedef llvm::Instruction::BinaryOps DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter =
          gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARBinaryOpsSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::Instruction::BinaryOps>(sym);
      return;
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::Instruction::BinaryOps", _rep_(object));
  }
};

template <> struct from_object<llvm::CmpInst::Predicate> {
  typedef llvm::CmpInst::Predicate DeclareType;
  DeclareType _v;
  from_object(T_P object) {
    if (core::Symbol_sp sym = object.asOrNull<core::Symbol_O>()) {
      core::SymbolToEnumConverter_sp converter =
          gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_STARCmpInstPredicateSTAR->symbolValue());
      this->_v = converter->enumForSymbol<llvm::CmpInst::Predicate>(sym);
      return;
    }
    SIMPLE_ERROR("Cannot convert object {} to llvm::CmpInst::Predicate", _rep_(object));
  }
};
}; // namespace translate

namespace llvmo {
void finalizeEngineAndRegisterWithGcAndRunMainFunctions(ExecutionEngine_sp oengine, core::T_sp startup_name);

Module_sp llvm_sys__parseBitcodeFile(core::T_sp filename, LLVMContext_sp context);
Module_sp llvm_sys__parseIRFile(core::T_sp filename, LLVMContext_sp context);
Module_sp llvm_sys__parseIRString(const std::string& llCode, LLVMContext_sp context, const std::string& bufferName);

void initialize_llvmo_expose();

} // namespace llvmo

namespace llvmo {

FORWARD(ModuleHandle);
FORWARD(ClaspJIT);

using namespace llvm;
using namespace llvm::orc;

//  void save_symbol_info(const llvm::object::ObjectFile& object_file, const llvm::RuntimeDyld::LoadedObjectInfo&
//  loaded_object_info);
}; // namespace llvmo

namespace llvmo {
class MDBuilder_O;
};

template <> struct gctools::GCInfo<llvmo::MDBuilder_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(MDBuilder);
class MDBuilder_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, MDBuilder_O, "MDBuilder", core::CxxObject_O);

public:
  ~MDBuilder_O() { delete this->_Builder; };

protected:
  llvm::MDBuilder* _Builder;

public:
  MDBuilder_O(llvm::LLVMContext& context) : _Builder(new llvm::MDBuilder(context)){};

public:
  CL_LISPIFY_NAME(make_mdbuilder);
  CL_DEF_CLASS_METHOD
  static MDBuilder_sp make(LLVMContext_sp context) {
    auto mdb = gctools::GC<MDBuilder_O>::allocate(*context->wrappedPtr());
    return mdb;
  };

public:
  CL_DEFMETHOD MDNode* createBranchWeightsTrueFalse(uint32_t trueWeight, uint32_t falseWeight) {
    return this->_Builder->createBranchWeights(trueWeight, falseWeight);
  };

}; // MDBuilder_O
}; // namespace llvmo
    /* from_object translators */

    ;

// SectionedAddress_O
namespace llvmo {
FORWARD(SectionedAddress);
class SectionedAddress_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::object::SectionedAddress, SectionedAddress_O, "SectionedAddress",
                      core::ExternalObject_O);

public:
  typedef llvm::object::SectionedAddress ExternalType;
  ExternalType _value;

public:
  static SectionedAddress_sp create(uint64_t SectionIndex, uint64_t Address);

public:
  SectionedAddress_O(uint64_t SectionIndex, uint64_t Address) : Base() {
    _value.SectionIndex = SectionIndex;
    _value.Address = Address;
  }
  std::string __repr__() const;

}; // SectionedAddress_O
}; // namespace llvmo
namespace translate {
template <> struct from_object<const llvm::object::SectionedAddress&> {
  typedef llvm::object::SectionedAddress DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::SectionedAddress_sp>(object)->_value){};
};
}; // namespace translate

ENUM_TRANSLATOR(llvm::GlobalValue::UnnamedAddr, llvmo::_sym_STARGlobalValueUnnamedAddrSTAR);

#if LLVM_VERSION_MAJOR < 16
namespace translate {
template <typename T> struct from_object<llvm::Optional<T>> {
  typedef llvm::Optional<T> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.unboundp()) {
      return;
    }
    llvm::Optional<T> val(from_object<T>(o)._v);
    this->_v = val;
    return;
  }
};
} // namespace translate
#else
namespace translate {
template <typename T> struct from_object<std::optional<T>> {
  typedef std::optional<T> DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) {
    if (o.unboundp()) {
      return;
    }
    std::optional<T> val(from_object<T>(o)._v);
    this->_v = val;
    return;
  }
};
} // namespace translate
#endif

namespace llvmo {

extern std::atomic<size_t> global_JITDylibCounter;

void dump_objects_for_debugger(std::ostream& fout, std::string indent);
LLVMContext_sp llvm_sys__thread_local_llvm_context();

std::string ensureUniqueMemoryBufferName(const std::string& prefix);
size_t objectIdFromName(const std::string& name);

llvm::raw_pwrite_stream* llvm_stream(core::T_sp stream, llvm::SmallString<1024>& stringOutput, bool& stringOutputStream);

core::T_sp llvm_sys__lookup_jit_symbol_info(void* ptr);

}; // namespace llvmo
