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
#ifndef debugInfoExpose_H //[
#define debugInfoExpose_H

#include <clasp/core/common.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/str.h>
#include <clasp/core/ql.h>
//#include "llvm/DataLayout.h"

#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
//#include "llvm/ExecutionEngine/JIT.h"
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
//#include "llvm/Support/IRBuilder.h"

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/metaClass.fwd.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/lispVector.h>
#include <clasp/llvmo/debugInfoExpose.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/core/vectorObjectsWithFillPtr.fwd.h>
#include <clasp/llvmo/insertPoint.fwd.h>
#include <clasp/llvmo/debugLoc.fwd.h>
#include <clasp/llvmo/llvmoPackage.h>
#include <clasp/llvmo/llvmoExpose.h>

namespace llvmo {
FORWARD(DINode);
class DINode_O : public MDNode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DINode, DINode_O, "DINode", MDNode_O );
  typedef llvm::DINode ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  typedef llvm::DINode OtherType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  virtual operator llvm::MDNode *() { return reinterpret_cast<llvm::MDNode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
 
  //	virtual llvm::DINode* operator ->() const { return (llvm::DINode*)(this);};
 DINode_O() : Base() {};
  virtual ~DINode_O(){};
}; // DINode_O
}; // llvmo
TRANSLATE(llvmo::DINode_O);

namespace translate {
  template <>
    struct from_object<llvm::DINode*,std::true_type> {
    typedef llvm::DINode* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DINode_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DINode*> {
    static core::T_sp convert(const llvm::DINode* ptr) {
      return (core::RP_Create_wrapped<llvmo::DINode_O, llvm::DINode*>(const_cast<llvm::DINode*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIScope);
class DIScope_O : public DINode_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIScope, DIScope_O, "discope", DINode_O);
  typedef llvm::DIScope ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIScope_O(){};
  virtual ~DIScope_O() {}
}; // DIScope_O
}; // llvmo
TRANSLATE(llvmo::DIScope_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIScope*,std::true_type> {
    typedef llvm::DIScope* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIScope_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIScope*> {
    static core::T_sp convert(const llvm::DIScope* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIScope_O, llvm::DIScope*>(const_cast<llvm::DIScope*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DINodeArray);
 class DINodeArray_O : public core::CxxObject_O {
   LISP_CLASS(llvmo, LlvmoPkg, DINodeArray_O, "DINodeArray",core::CxxObject_O);
  typedef llvm::DINodeArray OtherType;
 private:
  OtherType _Val;
public:
  OtherType& get() { return this->_Val;};
 DINodeArray_O(const OtherType& v) : _Val(v) {};
  DINodeArray_O() : Base(){};
  virtual ~DINodeArray_O() {}

}; // DINodeArray_O
}; // llvmo
TRANSLATE(llvmo::DINodeArray_O);

namespace translate {
template <>
struct to_object<llvm::DINodeArray> {
  static core::T_sp convert(const llvm::DINodeArray &val) {
    GC_ALLOCATE_VARIADIC(llvmo::DINodeArray_O, obj, val);
    return ((obj));
  };
};
template <>
struct from_object<llvm::DINodeArray, std::true_type> {
  typedef llvm::DINodeArray &DeclareType;
  DeclareType _v;
 from_object(T_P object) : _v(gc::As<llvmo::DINodeArray_sp>(object)->get()) {};
};
};


namespace llvmo {
FORWARD(DITypeRefArray);
 class DITypeRefArray_O : public core::CxxObject_O {
   LISP_CLASS(llvmo, LlvmoPkg, DITypeRefArray_O, "DITypeRefArray", core::CxxObject_O);
  typedef llvm::DITypeRefArray OtherType;
 private:
  OtherType _Val;
public:
  OtherType& get() { return this->_Val;};
 DITypeRefArray_O(const OtherType &val) : _Val(val) {};
 DITypeRefArray_O() : Base(), _Val(NULL) {};
  virtual ~DITypeRefArray_O() {}
}; // DITypeRefArray_O
}; // llvmo
TRANSLATE(llvmo::DITypeRefArray_O);

namespace translate {
template <>
struct to_object<llvm::DITypeRefArray> {
  static core::T_sp convert(const llvm::DITypeRefArray &val) {
    GC_ALLOCATE_VARIADIC(llvmo::DITypeRefArray_O, obj, val);
    return ((obj));
  };
};
template <>
struct from_object<llvm::DITypeRefArray, std::true_type> {
  typedef llvm::DITypeRefArray &DeclareType;
  DeclareType _v;
 from_object(core::T_sp object) : _v(gc::As<llvmo::DITypeRefArray_sp>(object)->get()){};
};
};



namespace llvmo {
FORWARD(DIFile);
class DIFile_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIFile, DIFile_O, "difile", DIScope_O);
  typedef llvm::DIFile ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIFile_O(){};
  virtual ~DIFile_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIFile_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIFile*,std::true_type> {
    typedef llvm::DIFile* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIFile_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIFile*> {
    static core::T_sp convert(const llvm::DIFile* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIFile_O, llvm::DIFile*>(const_cast<llvm::DIFile*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DILocalScope);
class DILocalScope_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILocalScope, DILocalScope_O, "dilocal-scope", DIScope_O);
  typedef llvm::DILocalScope ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILocalScope_O(){};
  virtual ~DILocalScope_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILocalScope_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILocalScope*,std::true_type> {
    typedef llvm::DILocalScope* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILocalScope_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILocalScope*> {
    static core::T_sp convert(const llvm::DILocalScope* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILocalScope_O, llvm::DILocalScope*>(const_cast<llvm::DILocalScope*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DISubprogram);
class DISubprogram_O : public DILocalScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DISubprogram, DISubprogram_O, "disubprogram", DILocalScope_O);
  typedef llvm::DISubprogram ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DISubprogram_O(){};
  virtual ~DISubprogram_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DISubprogram_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DISubprogram*,std::true_type> {
    typedef llvm::DISubprogram* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DISubprogram_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DISubprogram*> {
    static core::T_sp convert(const llvm::DISubprogram* ptr) {
      return (core::RP_Create_wrapped<llvmo::DISubprogram_O, llvm::DISubprogram*>(const_cast<llvm::DISubprogram*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIType);
class DIType_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIType, DIType_O, "ditype", DIScope_O);
  typedef llvm::DIType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIType_O(){};
  virtual ~DIType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIType*,std::true_type> {
    typedef llvm::DIType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIType*> {
    static core::T_sp convert(const llvm::DIType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIType_O, llvm::DIType*>(const_cast<llvm::DIType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIBasicType);
class DIBasicType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIBasicType, DIBasicType_O, "DIBasicType", DIType_O);
  typedef llvm::DIBasicType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIBasicType_O(){};
  virtual ~DIBasicType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIBasicType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIBasicType*,std::true_type> {
    typedef llvm::DIBasicType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIBasicType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIBasicType*> {
    static core::T_sp convert(const llvm::DIBasicType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIBasicType_O, llvm::DIBasicType*>(const_cast<llvm::DIBasicType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DIDerivedType);
class DIDerivedType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIDerivedType, DIDerivedType_O, "DIDerivedType", DIType_O);
  typedef llvm::DIDerivedType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DIDerivedType_O(){};
  virtual ~DIDerivedType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DIDerivedType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DIDerivedType*,std::true_type> {
    typedef llvm::DIDerivedType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DIDerivedType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DIDerivedType*> {
    static core::T_sp convert(const llvm::DIDerivedType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DIDerivedType_O, llvm::DIDerivedType*>(const_cast<llvm::DIDerivedType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DICompositeType);
class DICompositeType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DICompositeType, DICompositeType_O, "DICompositeType", DIType_O);
  typedef llvm::DICompositeType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DICompositeType_O(){};
  virtual ~DICompositeType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DICompositeType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DICompositeType*,std::true_type> {
    typedef llvm::DICompositeType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DICompositeType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DICompositeType*> {
    static core::T_sp convert(const llvm::DICompositeType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DICompositeType_O, llvm::DICompositeType*>(const_cast<llvm::DICompositeType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DISubroutineType);
class DISubroutineType_O : public DIType_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DISubroutineType, DISubroutineType_O, "DISubroutineType", DIType_O);
  typedef llvm::DISubroutineType ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DISubroutineType_O(){};
  virtual ~DISubroutineType_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DISubroutineType_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DISubroutineType*,std::true_type> {
    typedef llvm::DISubroutineType* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DISubroutineType_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DISubroutineType*> {
    static core::T_sp convert(const llvm::DISubroutineType* ptr) {
      return (core::RP_Create_wrapped<llvmo::DISubroutineType_O, llvm::DISubroutineType*>(const_cast<llvm::DISubroutineType*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DILexicalBlockBase);
class DILexicalBlockBase_O : public DILocalScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILexicalBlockBase, DILexicalBlockBase_O, "DILexicalBlockBase", DILocalScope_O);
  typedef llvm::DILexicalBlockBase ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILexicalBlockBase_O(){};
  virtual ~DILexicalBlockBase_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILexicalBlockBase_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILexicalBlockBase*,std::true_type> {
    typedef llvm::DILexicalBlockBase* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILexicalBlockBase_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILexicalBlockBase*> {
    static core::T_sp convert(const llvm::DILexicalBlockBase* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILexicalBlockBase_O, llvm::DILexicalBlockBase*>(const_cast<llvm::DILexicalBlockBase*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DILexicalBlock);
class DILexicalBlock_O : public DILexicalBlockBase_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DILexicalBlock, DILexicalBlock_O, "DILexicalBlock", DILexicalBlockBase_O);
  typedef llvm::DILexicalBlock ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DILexicalBlock_O(){};
  virtual ~DILexicalBlock_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DILexicalBlock_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DILexicalBlock*,std::true_type> {
    typedef llvm::DILexicalBlock* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DILexicalBlock_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DILexicalBlock*> {
    static core::T_sp convert(const llvm::DILexicalBlock* ptr) {
      return (core::RP_Create_wrapped<llvmo::DILexicalBlock_O, llvm::DILexicalBlock*>(const_cast<llvm::DILexicalBlock*>(ptr)));
    };
  };
};

namespace llvmo {
FORWARD(DICompileUnit);
class DICompileUnit_O : public DIScope_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DICompileUnit, DICompileUnit_O, "DICompileUnit", DIScope_O);
  typedef llvm::DICompileUnit ExternalType;
  typedef ExternalType* PointerToExternalType;
public:
  virtual operator llvm::DINode *() { return reinterpret_cast<llvm::DINode*>(this->_ptr); };
  PointerToExternalType wrappedPtr() const { return static_cast<PointerToExternalType>(this->_ptr); };
  void set_wrapped(PointerToExternalType ptr) {
    /*        if (this->_ptr != NULL ) delete this->_ptr; */
    this->_ptr = ptr;
  }
  DICompileUnit_O(){};
  virtual ~DICompileUnit_O() {}
};
}; // llvmo
TRANSLATE(llvmo::DICompileUnit_O);
/* from_object translators */
/* to_object translators */

namespace translate {
  template <>
    struct from_object<llvm::DICompileUnit*,std::true_type> {
    typedef llvm::DICompileUnit* DeclareType;
    DeclareType _v;
  from_object(core::T_sp o) : _v(o.nilp() ? NULL : gc::As<llvmo::DICompileUnit_sp>(o)->wrappedPtr()) {};
  };
  template <>
    struct to_object<llvm::DICompileUnit*> {
    static core::T_sp convert(const llvm::DICompileUnit* ptr) {
      return (core::RP_Create_wrapped<llvmo::DICompileUnit_O, llvm::DICompileUnit*>(const_cast<llvm::DICompileUnit*>(ptr)));
    };
  };
};



namespace llvmo {
FORWARD(DIBuilder);
class DIBuilder_O : public core::ExternalObject_O {
  LISP_EXTERNAL_CLASS(llvmo, LlvmoPkg, llvm::DIBuilder, DIBuilder_O, "DIBuilder", core::ExternalObject_O);
  typedef llvm::DIBuilder ExternalType;
  typedef llvm::DIBuilder *PointerToExternalType;
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
  static DIBuilder_sp make(Module_sp context);
public:
  void set_wrapped(PointerToExternalType ptr) {
    if (this->_ptr != NULL)
      delete this->_ptr;
    this->_ptr = ptr;
  };
  DINodeArray_sp getOrCreateArray(core::List_sp elements);
  DITypeRefArray_sp getOrCreateTypeArray(core::List_sp elements);
  DIBuilder_O() : Base(), _ptr(NULL){};
  virtual ~DIBuilder_O() {
    if (_ptr != NULL) {
      delete _ptr;
      _ptr = NULL;
    };
  }

}; // DIBuilder_O
}; // llvmo
TRANSLATE(llvmo::DIBuilder_O);
/* from_object translators */

namespace translate {
template <>
struct from_object<llvm::DIBuilder &, std::true_type> {
  typedef llvm::DIBuilder &DeclareType;
  DeclareType _v;
  from_object(T_P object) : _v(*gc::As<llvmo::DIBuilder_sp>(object)->wrappedPtr()){};
};
};
    ;
/* to_object translators */

;

#endif // debugInfo expose
