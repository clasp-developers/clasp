/*
    File: debugInfoExpose.cc
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
#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include "llvm/IR/AssemblyAnnotationWriter.h" // Should be llvm/IR was
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR

#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/package.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/environment.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/bignum.h>
#include <clasp/core/pointer.h>
#include <clasp/core/str.h>
#include <clasp/core/translators.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/external_wrappers.h>
#include <clasp/core/wrappers.h>

namespace llvmo {
EXPOSE_CLASS(llvmo, DebugInfo_O);

void DebugInfo_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DebugInfo_O>();
};

void DebugInfo_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIDescriptor_O);

void DIDescriptor_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIDescriptor_O>();
};

void DIDescriptor_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIScope_O);

void DIScope_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIScope_O>();
};

void DIScope_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIFile_O);

void DIFile_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIFile_O>();
};

void DIFile_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DISubprogram_O);

void DISubprogram_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DISubprogram_O>();
};

void DISubprogram_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIType_O);

void DIType_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIType_O>();
};

void DIType_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIBasicType_O);

void DIBasicType_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIBasicType_O>();
};

void DIBasicType_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIDerivedType_O);

void DIDerivedType_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIDerivedType_O>();
};

void DIDerivedType_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DICompositeType_O);

void DICompositeType_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DICompositeType_O>();
};

void DICompositeType_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DISubroutineType_O);

void DISubroutineType_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DISubroutineType_O>();
};

void DISubroutineType_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DIArray_O);

void DIArray_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DIArray_O>();
};

void DIArray_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DITypeArray_O);

void DITypeArray_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DITypeArray_O>();
};

void DITypeArray_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DILexicalBlock_O);

void DILexicalBlock_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DILexicalBlock_O>();
};

void DILexicalBlock_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {
EXPOSE_CLASS(llvmo, DICompileUnit_O);

void DICompileUnit_O::exposeCando(core::Lisp_sp lisp) {
  core::class_<DICompileUnit_O>();
};

void DICompileUnit_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};
}; // llvmo

namespace llvmo {

#define ARGS_DIBuilder_O_make "(module)"
#define DECL_DIBuilder_O_make ""
#define DOCS_DIBuilder_O_make "make DIBuilder args: module"
DIBuilder_sp DIBuilder_O::make(Module_sp module) {
  GC_ALLOCATE(DIBuilder_O, me);
  me->set_wrapped(new llvm::DIBuilder(*(module->wrappedPtr())));
  return me;
};

EXPOSE_CLASS(llvmo, DIBuilder_O);

void DIBuilder_O::exposeCando(core::Lisp_sp lisp) {
  using namespace llvm;
  DISubprogram (DIBuilder::*createFunction_ptr)(DIDescriptor,    // Scope
                                                StringRef,       // Name
                                                StringRef,       // LinkageName
                                                DIFile,          // File
                                                unsigned,        // lineno
                                                DICompositeType, // Ty
                                                bool,
                                                bool,
                                                unsigned,
                                                unsigned,
                                                bool,
                                                Function *,
                                                MDNode *,
                                                MDNode *) = &llvm::DIBuilder::createFunction;
  core::externalClass_<DIBuilder_O>()
      .def("createCompileUnit", &llvm::DIBuilder::createCompileUnit)
      .def("createFile", &llvm::DIBuilder::createFile)
      .def("createFunction", createFunction_ptr)
      .def("createLexicalBlock", &llvm::DIBuilder::createLexicalBlock)
      .def("createBasicType", &llvm::DIBuilder::createBasicType)
      .def("createNullPtrType", &llvm::DIBuilder::createNullPtrType)
      .def("getOrCreateArray", &DIBuilder_O::getOrCreateArray)
      .def("getOrCreateTypeArray", &DIBuilder_O::getOrCreateTypeArray)
      .def("createUnspecifiedParameter", &llvm::DIBuilder::createUnspecifiedParameter)
      .def("createSubroutineType", &llvm::DIBuilder::createSubroutineType)
      .def("finalize", &llvm::DIBuilder::finalize);
  core::af_def(LlvmoPkg, "make-dibuilder", &DIBuilder_O::make, ARGS_DIBuilder_O_make, DECL_DIBuilder_O_make, DOCS_DIBuilder_O_make);
};

void DIBuilder_O::exposePython(core::Lisp_sp lisp) {
  IMPLEMENT_ME();
};

CL_NAME("getOrCreateArray");
CL_DEFMETHOD DIArray_sp DIBuilder_O::getOrCreateArray(core::List_sp elements) {
  //		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
  //		printf("     cons --> %s\n", cur->__repr__().c_str() );
  vector<llvm::Metadata *> vector_values;
  for (auto cur : elements) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      //			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
      llvm::ValueAsMetadata *vd = llvm::ValueAsMetadata::get(val->wrappedPtr());
      vector_values.push_back(vd); // val->wrappedPtr());
    } else if (DebugInfo_sp di = oCar(cur).asOrNull<DebugInfo_O>()) {
      //			printf("      getting DIDescriptor*\n");
      llvm::DIDescriptor *didescriptor = di->operator llvm::DIDescriptor *();
      //			printf("      convert DIDescrptor* to MDNode* --> %p\n", didescriptor );
      llvm::MDNode *mdnode_didescriptor = *didescriptor;
      //			printf("      push_back mdnode_didescriptor --> %p\n", mdnode_didescriptor );
      vector_values.push_back(mdnode_didescriptor);
    } else {
      SIMPLE_ERROR(BF("Handle conversion of %s to llvm::Value*") % _rep_(oCar(cur)));
    }
  }
  llvm::ArrayRef<llvm::Metadata *> array(vector_values);
  llvm::DIArray diarray = this->wrappedPtr()->getOrCreateArray(array);
  GC_ALLOCATE_VARIADIC(llvmo::DIArray_O, obj, diarray);
  return obj;
}

CL_NAME("getOrCreateTypeArray");
CL_DEFMETHOD DITypeArray_sp DIBuilder_O::getOrCreateTypeArray(core::List_sp elements) {
  //		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
  //		printf("     cons --> %s\n", cur->__repr__().c_str() );
  vector<llvm::Metadata *> vector_values;
  for (auto cur : elements) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      //			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
      llvm::ValueAsMetadata *vd = llvm::ValueAsMetadata::get(val->wrappedPtr());
      vector_values.push_back(vd); // val->wrappedPtr());
                                   //vector_values.push_back(val->wrappedPtr());
    } else if (DebugInfo_sp di = oCar(cur).asOrNull<DebugInfo_O>()) {
      //			printf("      getting DIDescriptor*\n");
      llvm::DIDescriptor *didescriptor = di->operator llvm::DIDescriptor *();
      //			printf("      convert DIDescrptor* to MDNode* --> %p\n", didescriptor );
      llvm::MDNode *mdnode_didescriptor = *didescriptor;
      //			printf("      push_back mdnode_didescriptor --> %p\n", mdnode_didescriptor );
      vector_values.push_back(mdnode_didescriptor);
    } else {
      SIMPLE_ERROR(BF("Handle conversion of %s to llvm::Value*") % _rep_(oCar(cur)));
    }
  }
  llvm::ArrayRef<llvm::Metadata *> array(vector_values);
  llvm::DITypeArray diarray = this->wrappedPtr()->getOrCreateTypeArray(array);
  GC_ALLOCATE_VARIADIC(llvmo::DITypeArray_O, obj, diarray);
  return obj;
}

}; // llvmo
