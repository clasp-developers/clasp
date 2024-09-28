#pragma once

/*
    File: debugInfoExpose.h
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

#include <clasp/core/common.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/array.h>
#include <clasp/core/debugger.h>
#include <clasp/core/ql.h>
// #include "llvm/DataLayout.h"

#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
// #include "llvm/ExecutionEngine/JIT.h"
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
// #include "llvm/Support/IRBuilder.h"
#include <llvm/DebugInfo/DIContext.h>

#include <llvm/DebugInfo/DWARF/DWARFContext.h>

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/metaClass.fwd.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/array.h>
#include <clasp/llvmo/debugInfoExpose.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/llvmo/insertPoint.fwd.h>
#include <clasp/llvmo/debugLoc.fwd.h>
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/llvmo/llvmoExpose.h>

namespace llvmo {
FORWARD(DILocation);
class DILocation_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocation, DILocation_O, "DILocation", MDNode_O);
  typedef llvm::DILocation ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  static DILocation_sp make(llvm::LLVMContext&, unsigned int, unsigned int, DINode_sp, core::T_sp);
  virtual operator llvm::DILocation*() { return reinterpret_cast<llvm::DILocation*>(this->_ptr); };
  virtual operator llvm::MDNode*() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  virtual operator llvm::Metadata*() { return reinterpret_cast<llvm::Metadata*>(this->_ptr); }
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

  //	virtual llvm::DILocation* operator ->() const { return (llvm::DILocation*)(this);};
  DILocation_O() : Base() {};
  virtual ~DILocation_O() {};
}; // DILocation_O
}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::DILocation*> {
  typedef llvm::DILocation* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocation_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DILocation*> {
  static core::T_sp convert(const llvm::DILocation* ptr) {
    return (core::RP_Create_wrapped<llvmo::DILocation_O, llvm::DILocation*>(const_cast<llvm::DILocation*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DINode);
class DINode_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DINode, DINode_O, "DINode", MDNode_O);
  typedef llvm::DINode ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  virtual operator llvm::MDNode*() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  virtual operator llvm::Metadata*() { return reinterpret_cast<llvm::Metadata*>(this->_ptr); }
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

  //	virtual llvm::DINode* operator ->() const { return (llvm::DINode*)(this);};
  DINode_O() : Base() {};
  virtual ~DINode_O() {};
}; // DINode_O
}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::DINode*> {
  typedef llvm::DINode* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DINode_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DINode*> {
  static core::T_sp convert(const llvm::DINode* ptr) {
    return (core::RP_Create_wrapped<llvmo::DINode_O, llvm::DINode*>(const_cast<llvm::DINode*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DIExpression);
class DIExpression_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIExpression, DIExpression_O, "DIExpression", MDNode_O);
  typedef llvm::DIExpression ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DIExpression*() { return reinterpret_cast<llvm::DIExpression*>(this->_ptr); };
  virtual operator llvm::MDNode*() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  virtual operator llvm::Metadata*() { return reinterpret_cast<llvm::Metadata*>(this->_ptr); }
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

  //	virtual llvm::DIExpression* operator ->() const { return (llvm::DIExpression*)(this);};
  DIExpression_O() : Base() {};
  virtual ~DIExpression_O() {};
}; // DIExpression_O
}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::DIExpression*> {
  typedef llvm::DIExpression* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIExpression_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIExpression*> {
  static core::T_sp convert(const llvm::DIExpression* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIExpression_O, llvm::DIExpression*>(const_cast<llvm::DIExpression*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DIScope);
class DIScope_O : public DINode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIScope, DIScope_O, "discope", DINode_O);
  typedef llvm::DIScope ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIScope_O() {};
  virtual ~DIScope_O() {}
}; // DIScope_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DIScope*> {
  typedef llvm::DIScope* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIScope_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIScope*> {
  static core::T_sp convert(const llvm::DIScope* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIScope_O, llvm::DIScope*>(const_cast<llvm::DIScope*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DINodeArray);
class DINodeArray_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, DINodeArray_O, "DINodeArray", core::CxxObject_O);

private:
  dont_expose<llvm::DINodeArray> _Val;

public:
  llvm::DINodeArray& get() { return this->_Val._value; };
  DINodeArray_O(const llvm::DINodeArray& v) : _Val(v) {};
  DINodeArray_O() : Base() {};
  virtual ~DINodeArray_O() {}

}; // DINodeArray_O
}; // namespace llvmo

namespace translate {
template <> struct to_object<llvm::DINodeArray> {
  static core::T_sp convert(const llvm::DINodeArray& val) {
    auto obj = gctools::GC<llvmo::DINodeArray_O>::allocate(val);
    return ((obj));
  };
};
template <> struct from_object<llvm::DINodeArray> {
  typedef llvm::DINodeArray DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(object.nilp() ? nullptr : gc::As<llvmo::DINodeArray_sp>(object)->get()) {};
};
}; // namespace translate

namespace llvmo {
FORWARD(DITypeRefArray);
class DITypeRefArray_O : public core::CxxObject_O {
  LISP_CLASS(llvmo, LlvmoPkg, DITypeRefArray_O, "DITypeRefArray", core::CxxObject_O);

private:
  dont_expose<llvm::DITypeRefArray> _Val;

public:
  llvm::DITypeRefArray& get() { return this->_Val._value; };
  DITypeRefArray_O(const llvm::DITypeRefArray& val) : _Val(val) {};
  DITypeRefArray_O() : Base(), _Val((llvm::DITypeRefArray)NULL) {};
  virtual ~DITypeRefArray_O() {}
}; // DITypeRefArray_O
}; // namespace llvmo

namespace translate {
template <> struct to_object<llvm::DITypeRefArray> {
  static core::T_sp convert(const llvm::DITypeRefArray& val) {
    auto obj = gctools::GC<llvmo::DITypeRefArray_O>::allocate(val);
    return ((obj));
  };
};
template <> struct from_object<llvm::DITypeRefArray> {
  typedef llvm::DITypeRefArray& DeclareType;
  DeclareType _v;
  from_object(core::T_sp object) : _v(gc::As<llvmo::DITypeRefArray_sp>(object)->get()) {};
};
}; // namespace translate

namespace llvmo {
FORWARD(DIFile);
class DIFile_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIFile, DIFile_O, "difile", DIScope_O);
  typedef llvm::DIFile ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  std::string __repr__() const;
  std::string getPath() const;
  DIFile_O() {};
  virtual ~DIFile_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DIFile*> {
  typedef llvm::DIFile* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIFile_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIFile*> {
  static core::T_sp convert(const llvm::DIFile* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIFile_O, llvm::DIFile*>(const_cast<llvm::DIFile*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DILocalScope);
class DILocalScope_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocalScope, DILocalScope_O, "dilocal-scope", DIScope_O);
  typedef llvm::DILocalScope ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILocalScope_O() {};
  virtual ~DILocalScope_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DILocalScope*> {
  typedef llvm::DILocalScope* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocalScope_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DILocalScope*> {
  static core::T_sp convert(const llvm::DILocalScope* ptr) {
    return (core::RP_Create_wrapped<llvmo::DILocalScope_O, llvm::DILocalScope*>(const_cast<llvm::DILocalScope*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DISubprogram);
class DISubprogram_O : public DILocalScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DISubprogram, DISubprogram_O, "disubprogram", DILocalScope_O);
  typedef llvm::DISubprogram ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  std::string __repr__() const;
  std::string getSubprogram() const;
  DISubprogram_O() {};
  virtual ~DISubprogram_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DISubprogram*> {
  typedef llvm::DISubprogram* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DISubprogram_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DISubprogram*> {
  static core::T_sp convert(const llvm::DISubprogram* ptr) {
    return (core::RP_Create_wrapped<llvmo::DISubprogram_O, llvm::DISubprogram*>(const_cast<llvm::DISubprogram*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DIType);
class DIType_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIType, DIType_O, "ditype", DIScope_O);
  typedef llvm::DIType ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIType_O() {};
  virtual ~DIType_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DIType*> {
  typedef llvm::DIType* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIType_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIType*> {
  static core::T_sp convert(const llvm::DIType* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIType_O, llvm::DIType*>(const_cast<llvm::DIType*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DIBasicType);
class DIBasicType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIBasicType, DIBasicType_O, "DIBasicType", DIType_O);
  typedef llvm::DIBasicType ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIBasicType_O() {};
  virtual ~DIBasicType_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DIBasicType*> {
  typedef llvm::DIBasicType* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIBasicType_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIBasicType*> {
  static core::T_sp convert(const llvm::DIBasicType* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIBasicType_O, llvm::DIBasicType*>(const_cast<llvm::DIBasicType*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DIDerivedType);
class DIDerivedType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIDerivedType, DIDerivedType_O, "DIDerivedType", DIType_O);
  typedef llvm::DIDerivedType ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIDerivedType_O() {};
  virtual ~DIDerivedType_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DIDerivedType*> {
  typedef llvm::DIDerivedType* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIDerivedType_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIDerivedType*> {
  static core::T_sp convert(const llvm::DIDerivedType* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIDerivedType_O, llvm::DIDerivedType*>(const_cast<llvm::DIDerivedType*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DICompositeType);
class DICompositeType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DICompositeType, DICompositeType_O, "DICompositeType", DIType_O);
  typedef llvm::DICompositeType ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DICompositeType_O() {};
  virtual ~DICompositeType_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DICompositeType*> {
  typedef llvm::DICompositeType* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DICompositeType_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DICompositeType*> {
  static core::T_sp convert(const llvm::DICompositeType* ptr) {
    return (core::RP_Create_wrapped<llvmo::DICompositeType_O, llvm::DICompositeType*>(const_cast<llvm::DICompositeType*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DISubroutineType);
class DISubroutineType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DISubroutineType, DISubroutineType_O, "DISubroutineType", DIType_O);
  typedef llvm::DISubroutineType ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DISubroutineType_O() {};
  virtual ~DISubroutineType_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DISubroutineType*> {
  typedef llvm::DISubroutineType* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DISubroutineType_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DISubroutineType*> {
  static core::T_sp convert(const llvm::DISubroutineType* ptr) {
    return (core::RP_Create_wrapped<llvmo::DISubroutineType_O, llvm::DISubroutineType*>(const_cast<llvm::DISubroutineType*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DILexicalBlockBase);
class DILexicalBlockBase_O : public DILocalScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILexicalBlockBase, DILexicalBlockBase_O, "DILexicalBlockBase", DILocalScope_O);
  typedef llvm::DILexicalBlockBase ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILexicalBlockBase_O() {};
  virtual ~DILexicalBlockBase_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DILexicalBlockBase*> {
  typedef llvm::DILexicalBlockBase* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILexicalBlockBase_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DILexicalBlockBase*> {
  static core::T_sp convert(const llvm::DILexicalBlockBase* ptr) {
    return (core::RP_Create_wrapped<llvmo::DILexicalBlockBase_O, llvm::DILexicalBlockBase*>(
        const_cast<llvm::DILexicalBlockBase*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DILexicalBlock);
class DILexicalBlock_O : public DILexicalBlockBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILexicalBlock, DILexicalBlock_O, "DILexicalBlock", DILexicalBlockBase_O);
  typedef llvm::DILexicalBlock ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILexicalBlock_O() {};
  virtual ~DILexicalBlock_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DILexicalBlock*> {
  typedef llvm::DILexicalBlock* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILexicalBlock_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DILexicalBlock*> {
  static core::T_sp convert(const llvm::DILexicalBlock* ptr) {
    return (core::RP_Create_wrapped<llvmo::DILexicalBlock_O, llvm::DILexicalBlock*>(const_cast<llvm::DILexicalBlock*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DICompileUnit);
class DICompileUnit_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DICompileUnit, DICompileUnit_O, "DICompileUnit", DIScope_O);
  typedef llvm::DICompileUnit ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DICompileUnit_O() {};
  virtual ~DICompileUnit_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DICompileUnit*> {
  typedef llvm::DICompileUnit* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DICompileUnit_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DICompileUnit*> {
  static core::T_sp convert(const llvm::DICompileUnit* ptr) {
    return (core::RP_Create_wrapped<llvmo::DICompileUnit_O, llvm::DICompileUnit*>(const_cast<llvm::DICompileUnit*>(ptr)));
  };
};
}; // namespace translate

template <> struct gctools::GCInfo<llvmo::DIBuilder_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = normal;
};

namespace llvmo {
FORWARD(DIBuilder);
class DIBuilder_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIBuilder, DIBuilder_O, "DIBuilder", core::ExternalObject_O);
  typedef llvm::DIBuilder ExternalType;
  typedef llvm::DIBuilder* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }

  core::T_sp insertDbgValueIntrinsicBasicBlock(llvm::Value* Val, llvm::DILocalVariable* VarInfo, llvm::DIExpression* Expr,
                                               const llvm::DILocation* DL, llvm::BasicBlock* InsertAtEnd);

public:
  static DIBuilder_sp make(Module_sp context);

public:
  void set_wrapped(PointerToExternalType ptr) {
    delete this->_ptr;
    this->_ptr = ptr;
  };
  DINodeArray_sp getOrCreateArray(core::T_sp elements);
  DITypeRefArray_sp getOrCreateTypeArray(core::List_sp elements);
  DIBuilder_O() : Base(), _ptr(NULL) {};
  virtual ~DIBuilder_O() {
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

}; // DIBuilder_O
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::DIBuilder&> {
  typedef llvm::DIBuilder& DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::DIBuilder_sp>(object)->wrappedPtr()) {};
  ~from_object() { /*non trivial*/ };
};
}; // namespace translate

namespace llvmo {
FORWARD(DIVariable);
class DIVariable_O : public DINode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIVariable, DIVariable_O, "DIVariable", DINode_O);
  typedef llvm::DIVariable ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIVariable_O() {};
  virtual ~DIVariable_O() {}
}; // DIVariable_O
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DIVariable*> {
  typedef llvm::DIVariable* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIVariable_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DIVariable*> {
  static core::T_sp convert(const llvm::DIVariable* ptr) {
    return (core::RP_Create_wrapped<llvmo::DIVariable_O, llvm::DIVariable*>(const_cast<llvm::DIVariable*>(ptr)));
  };
};
}; // namespace translate

namespace llvmo {
FORWARD(DILocalVariable);
class DILocalVariable_O : public DIVariable_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocalVariable, DILocalVariable_O, "DILocalVariable", DIVariable_O);
  typedef llvm::DILocalVariable ExternalType;
  typedef ExternalType* PointerToExternalType;

public:
  virtual operator llvm::DINode*() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }
  std::string __repr__() const;
  std::string getVariableName() const;
  DILocalVariable_O() {};
  virtual ~DILocalVariable_O() {}
};
}; // namespace llvmo
/* from_object translators */
/* to_object translators */

namespace translate {
template <> struct from_object<llvm::DILocalVariable*> {
  typedef llvm::DILocalVariable* DeclareType;
  DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocalVariable_sp>(o)->wrappedPtr()) {};
};
template <> struct to_object<llvm::DILocalVariable*> {
  static core::T_sp convert(const llvm::DILocalVariable* ptr) {
    return (core::RP_Create_wrapped<llvmo::DILocalVariable_O, llvm::DILocalVariable*>(const_cast<llvm::DILocalVariable*>(ptr)));
  };
};
}; // namespace translate

#if LLVM_VERSION_MAJOR < 16
namespace translate {
template <> struct from_object<llvm::Optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>>> {
  typedef llvm::Optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>> DeclareType;
  DeclareType _v;
  std::string _Storage;
  from_object(core::T_sp object) {
    //    printf("%s:%d:%s object = %s\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(object).c_str());
    core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CSKEnum->symbolValue());
    if (object.nilp()) {
      DeclareType none;
      this->_v = none;
      //      printf("%s:%d:%s ChecksumInfo RESET  this->_v -> %d\n", __FILE__, __LINE__, __FUNCTION__, this->_v.hasValue() );
    } else if (gc::IsA<core::Symbol_sp>(object)) {
      core::Symbol_sp sobject = gc::As<core::Symbol_sp>(object);
      this->_Storage = sobject->symbolNameAsString();
      llvm::DIFile::ChecksumKind kind = converter->enumForSymbol<llvm::DIFile::ChecksumKind>(sobject);
      for (int p = 0; p < this->_Storage.size(); p++) {
        if (this->_Storage[p] == '-')
          this->_Storage[p] = '_';
      }
      llvm::DIFile::ChecksumInfo<llvm::StringRef> checksum(kind, this->_Storage);
      this->_v = checksum;
      //      printf("%s:%d:%s ChecksumInfo kind = %d  str = %s \n", __FILE__, __LINE__, __FUNCTION__, kind, this->_Storage.c_str()
      //      );
    } else {
      SIMPLE_ERROR("You must pass a valid Checksum like :CSK_MD5");
    }
  }
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _v(orig._v), _Storage(std::move(orig._Storage)) {
    if (this->_v.hasValue()) {
      //      printf("%s:%d:%s from_object move ctor\n", __FILE__, __LINE__, __FUNCTION__ );
      llvm::DIFile::ChecksumInfo<llvm::StringRef> checksum(this->_v->Kind, this->_Storage);
      this->_v = checksum;
    } else {
      //      printf("%s:%d:%s from_object move ctor NIL\n", __FILE__, __LINE__, __FUNCTION__ );
    }
  }
};
}; // namespace translate
#else
namespace translate {
template <> struct from_object<std::optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>>> {
  typedef std::optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>> DeclareType;
  DeclareType _v;
  std::string _Storage;
  from_object(core::T_sp object) {
    //    printf("%s:%d:%s object = %s\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(object).c_str());
    core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CSKEnum->symbolValue());
    if (object.nilp()) {
      DeclareType none;
      this->_v = none;
      //      printf("%s:%d:%s ChecksumInfo RESET  this->_v -> %d\n", __FILE__, __LINE__, __FUNCTION__, this->_v.hasValue() );
    } else if (gc::IsA<core::Symbol_sp>(object)) {
      core::Symbol_sp sobject = gc::As<core::Symbol_sp>(object);
      this->_Storage = sobject->symbolNameAsString();
      llvm::DIFile::ChecksumKind kind = converter->enumForSymbol<llvm::DIFile::ChecksumKind>(sobject);
      for (int p = 0; p < this->_Storage.size(); p++) {
        if (this->_Storage[p] == '-')
          this->_Storage[p] = '_';
      }
      llvm::DIFile::ChecksumInfo<llvm::StringRef> checksum(kind, this->_Storage);
      this->_v = checksum;
      //      printf("%s:%d:%s ChecksumInfo kind = %d  str = %s \n", __FILE__, __LINE__, __FUNCTION__, kind, this->_Storage.c_str()
      //      );
    } else {
      SIMPLE_ERROR("You must pass a valid Checksum like :CSK_MD5");
    }
  }
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _Storage(std::move(orig._Storage)), _v(orig._v) {
    if (this->_v.has_value()) {
      //      printf("%s:%d:%s from_object move ctor\n", __FILE__, __LINE__, __FUNCTION__ );
      llvm::DIFile::ChecksumInfo<llvm::StringRef> checksum(this->_v->Kind, this->_Storage);
      this->_v = checksum;
    } else {
      //      printf("%s:%d:%s from_object move ctor NIL\n", __FILE__, __LINE__, __FUNCTION__ );
    }
  }
};
}; // namespace translate
#endif

#if LLVM_VERSION_MAJOR < 16
namespace translate {
template <> struct from_object<llvm::Optional<llvm::StringRef>> {
  typedef llvm::Optional<llvm::StringRef> DeclareType;
  std::string _Storage;
  DeclareType _v;
  from_object(core::T_sp object) {
    if (object.nilp()) {
      DeclareType none;
      _v = none;
    } else if (gc::IsA<core::String_sp>(object)) {
      _Storage = gc::As<core::String_sp>(object)->get_std_string();
      _v = _Storage;
    } else {
      SIMPLE_ERROR("You must pass a String or NIL");
    }
  }
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _Storage(std::move(orig._Storage)), _v(orig._v) {
    if (_v.hasValue())
      _v = _Storage;
  }
};
}; // namespace translate
#else
namespace translate {
template <> struct from_object<std::optional<llvm::StringRef>> {
  typedef std::optional<llvm::StringRef> DeclareType;
  std::string _Storage;
  DeclareType _v;
  from_object(core::T_sp object) {
    if (object.nilp()) {
      DeclareType none;
      _v = none;
    } else if (gc::IsA<core::String_sp>(object)) {
      _Storage = gc::As<core::String_sp>(object)->get_std_string();
      _v = _Storage;
    } else {
      SIMPLE_ERROR("You must pass a String or NIL");
    }
  }
  from_object(const from_object& orig) = delete;
  from_object(from_object&& orig) : _Storage(std::move(orig._Storage)), _v(orig._v) {
    if (_v.has_value())
      _v = _Storage;
  }
};
}; // namespace translate
#endif

// DIContext_O
namespace llvmo {
FORWARD(DIContext);
class DIContext_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIContext, DIContext_O, "DIContext", core::ExternalObject_O);
  typedef llvm::DIContext ExternalType;
  typedef llvm::DIContext* PointerToExternalType;

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
  DIContext_O() : Base(), _ptr(NULL) {};
  ~DIContext_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
}; // DIContext_O class def
}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::DIContext*> {
  typedef llvm::DIContext* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DIContext_sp>(object)->wrappedPtr()) {};
};

}; // namespace translate
;
/* to_object translators */

namespace translate {
template <> struct to_object<llvm::DIContext*> {
  static core::T_sp convert(llvm::DIContext* ptr) { return core::RP_Create_wrapped<llvmo::DIContext_O, llvm::DIContext*>(ptr); }
};
}; // namespace translate

// DWARFContext_O
namespace llvmo {
FORWARD(DWARFContext);
class DWARFContext_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DWARFContext, DWARFContext_O, "DWARFContext", core::ExternalObject_O);
  typedef llvm::DWARFContext ExternalType;
  typedef llvm::DWARFContext* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public:
  static DWARFContext_sp createDWARFContext(ObjectFile_sp);
  DWARFContext_O() : Base(), _ptr(NULL) {};
  DWARFContext_O(void* b) : Base(), _ptr((PointerToExternalType)b) {};
  ~DWARFContext_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  size_t getNumCompileUnits() const;
}; // DWARFContext_O class def
// FIXME: move?
const char* getFunctionNameForAddress(DWARFContext_sp, SectionedAddress_sp);
core::T_mv getLineInfoForAddress_(DWARFContext_sp, SectionedAddress_sp, bool verbose);
llvm::Expected<std::vector<llvm::DWARFAddressRange>> getAddressRangesForAddressInner(DWARFContext_sp, SectionedAddress_sp);

}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<llvm::DWARFContext*> {
  typedef llvm::DWARFContext* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DWARFContext_sp>(object)->wrappedPtr()) {};
};

}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::DWARFContext*> {
  static core::T_sp convert(llvm::DWARFContext* ptr) {
    return core::RP_Create_wrapped<llvmo::DWARFContext_O, llvm::DWARFContext*>(ptr);
  }
};
}; // namespace translate

namespace llvmo {
FORWARD(DWARFUnit);
class DWARFUnit_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DWARFUnit, DWARFUnit_O, "DWARFUnit", core::ExternalObject_O);
  typedef llvm::DWARFUnit ExternalType;
  typedef llvm::DWARFUnit* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return this->_ptr; };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public:
  DWARFUnit_O() : Base(), _ptr(NULL) {};
  ~DWARFUnit_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }

}; // DWARFUnit_O class def

}; // namespace llvmo

namespace translate {
template <> struct from_object<llvm::DWARFUnit*> {
  typedef llvm::DWARFUnit* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::DWARFUnit_sp>(object)->wrappedPtr()) {};
};

}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<llvm::DWARFUnit*> {
  static core::T_sp convert(llvm::DWARFUnit* ptr) { return core::RP_Create_wrapped<llvmo::DWARFUnit_O, llvm::DWARFUnit*>(ptr); }
};
}; // namespace translate

// LineTable_O
namespace llvmo {
FORWARD(LineTable);
class LineTable_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DWARFDebugLine::LineTable, LineTable_O, "LineTable", core::ExternalObject_O);
  typedef const llvm::DWARFDebugLine::LineTable ExternalType;
  typedef const llvm::DWARFDebugLine::LineTable* PointerToExternalType;

protected:
  PointerToExternalType _ptr;

public:
  virtual void* externalObject() const { return (void*)const_cast<llvm::DWARFDebugLine::LineTable*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return this->_ptr; }
  void set_wrapped(PointerToExternalType ptr) {
    /* delete this->_ptr; */
    this->_ptr = ptr;
  }

public:
  LineTable_O() : Base(), _ptr(NULL) {};
  ~LineTable_O() {
    /* delete _ptr;*/
    _ptr = NULL;
  }
  size_t size() const;
  core::List_sp element(size_t index) const;

}; // LineTable_O class def
// FIXME: move?

}; // namespace llvmo
/* from_object translators */

namespace translate {
template <> struct from_object<const llvm::DWARFDebugLine::LineTable*> {
  typedef const llvm::DWARFDebugLine::LineTable* DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(gc::As<llvmo::LineTable_sp>(object)->wrappedPtr()) {};
};

}; // namespace translate

/* to_object translators */

namespace translate {
template <> struct to_object<const llvm::DWARFDebugLine::LineTable*> {
  static core::T_sp convert(const llvm::DWARFDebugLine::LineTable* ptr) {
    return core::RP_Create_wrapped<llvmo::LineTable_O, const llvm::DWARFDebugLine::LineTable*>(ptr);
  }
};
}; // namespace translate

// ------------------------------------------------------------
//
// Translators for other types
//

ENUM_FROM_OBJECT_TRANSLATOR(llvm::DIFile::ChecksumKind, llvmo::_sym_CSKEnum);
ENUM_FROM_OBJECT_TRANSLATOR(llvm::DICompileUnit::DebugNameTableKind, llvmo::_sym_DNTKEnum);
