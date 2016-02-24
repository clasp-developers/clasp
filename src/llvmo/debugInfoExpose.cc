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


}; // llvmo

namespace llvmo {



}; // llvmo

namespace llvmo {


};

namespace llvmo {


};

namespace llvmo {



}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {


;

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(make-dibuilder);
CL_DEFUN DIBuilder_sp DIBuilder_O::make(Module_sp module) {
  GC_ALLOCATE(DIBuilder_O, me);
  me->set_wrapped(new llvm::DIBuilder(*(module->wrappedPtr())));
  return me;
};



  CL_LISPIFY_NAME(createCompileUnit);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createCompileUnit);
  CL_LISPIFY_NAME(createFile);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createFile);
  CL_LISPIFY_NAME(createFunction);
  CL_EXTERN_DEFMETHOD(DIBuilder_O,
                      (llvm::DISubprogram
                       (llvm::DIBuilder::*)
                       ( llvm::DIDescriptor,    // Scope
                         llvm::StringRef,       // Name
                         llvm::StringRef,       // LinkageName
                         llvm::DIFile,          // File
                         unsigned,        // lineno
                         llvm::DICompositeType, // Ty
                         bool,
                         bool,
                         unsigned,
                         unsigned,
                         bool,
                         llvm::Function *,
                         llvm::MDNode *,
                         llvm::MDNode *))&llvm::DIBuilder::createFunction );
  CL_LISPIFY_NAME(createLexicalBlock);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createLexicalBlock);
  CL_LISPIFY_NAME(createBasicType);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createBasicType);
  CL_LISPIFY_NAME(createNullPtrType);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createNullPtrType);
  CL_LISPIFY_NAME(createUnspecifiedParameter);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createUnspecifiedParameter);
  CL_LISPIFY_NAME(createSubroutineType);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createSubroutineType);
  CL_LISPIFY_NAME(finalize);
  CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::finalize);

;


CL_LISPIFY_NAME("getOrCreateArray");
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

CL_LISPIFY_NAME("getOrCreateTypeArray");
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
