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
//#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <clasp/core/foundation.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Bitcode/BitcodeReader.h>
#/*
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
//#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <clasp/core/foundation.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
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
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include "llvm/IR/AssemblyAnnotationWriter.h" // Should be llvm/IR was
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/package.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/environment.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/bignum.h>
#include <clasp/core/pointer.h>
#include <clasp/core/array.h>
#include <clasp/core/translators.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/external_wrappers.h>
#include <clasp/core/wrappers.h>



namespace llvmo {

CL_LAMBDA(llvm-context line col scope &optional inlined-at);
CL_LISPIFY_NAME(get-dilocation);
CL_DEFUN DILocation_sp DILocation_O::make(llvm::LLVMContext& context,
                                          unsigned int line, unsigned int col,
                                          DINode_sp scope, core::T_sp inlinedAt) {
  llvm::Metadata* realScope = scope->operator llvm::Metadata *();
  llvm::Metadata* realInlinedAt;
  if (inlinedAt.nilp()) realInlinedAt = nullptr;
  else {
    DILocation_sp temp = gc::As<DILocation_sp>(inlinedAt);
    realInlinedAt = temp->operator llvm::Metadata *();
  }
  GC_ALLOCATE(DILocation_O, ret);
  ret->set_wrapped(llvm::DILocation::get(context, line, col, realScope, realInlinedAt));
  return ret;
}

CL_LAMBDA(module);
CL_LISPIFY_NAME(make-dibuilder);
CL_DEFUN DIBuilder_sp DIBuilder_O::make(Module_sp module) {
  _G();
  GC_ALLOCATE(DIBuilder_O, me);
  me->set_wrapped(new llvm::DIBuilder(*(module->wrappedPtr())));
  return me;
};

SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsZero); // Use it as zero value.
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsPrivate);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsProtected);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsPublic);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsFwdDecl);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsAppleBlock);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsBlockByrefStruct);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsVirtual);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsArtificial);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsExplicit);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsPrototyped);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsObjcClassComplete);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsObjectPointer);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsVector);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsStaticMember);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsLValueReference);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsRValueReference);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsExternalTypeRef);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsSingleInheritance);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsMultipleInheritance);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsVirtualInheritance);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsIntroducedVirtual);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsBitField);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsNoReturn);
SYMBOL_EXPORT_SC_(LlvmoPkg, DIFlagsEnum);
CL_BEGIN_ENUM(llvm::DINode::DIFlags,_sym_DIFlagsEnum,"DIFlagsEnum");
CL_VALUE_ENUM(_sym_DIFlagsZero,llvm::DINode::FlagZero); // Use it as zero value.
CL_VALUE_ENUM(_sym_DIFlagsPrivate,llvm::DINode::FlagPrivate);
CL_VALUE_ENUM(_sym_DIFlagsProtected,llvm::DINode::FlagProtected);
CL_VALUE_ENUM(_sym_DIFlagsPublic,llvm::DINode::FlagPublic);
CL_VALUE_ENUM(_sym_DIFlagsFwdDecl,llvm::DINode::FlagFwdDecl);
CL_VALUE_ENUM(_sym_DIFlagsAppleBlock,llvm::DINode::FlagAppleBlock);
CL_VALUE_ENUM(_sym_DIFlagsBlockByrefStruct,llvm::DINode::FlagBlockByrefStruct);
CL_VALUE_ENUM(_sym_DIFlagsVirtual,llvm::DINode::FlagVirtual);
CL_VALUE_ENUM(_sym_DIFlagsArtificial,llvm::DINode::FlagArtificial);
CL_VALUE_ENUM(_sym_DIFlagsExplicit,llvm::DINode::FlagExplicit);
CL_VALUE_ENUM(_sym_DIFlagsPrototyped,llvm::DINode::FlagPrototyped);
CL_VALUE_ENUM(_sym_DIFlagsObjcClassComplete,llvm::DINode::FlagObjcClassComplete);
CL_VALUE_ENUM(_sym_DIFlagsObjectPointer,llvm::DINode::FlagObjectPointer);
CL_VALUE_ENUM(_sym_DIFlagsVector,llvm::DINode::FlagVector);
CL_VALUE_ENUM(_sym_DIFlagsStaticMember,llvm::DINode::FlagStaticMember);
CL_VALUE_ENUM(_sym_DIFlagsLValueReference,llvm::DINode::FlagLValueReference);
CL_VALUE_ENUM(_sym_DIFlagsRValueReference,llvm::DINode::FlagRValueReference);
//CL_VALUE_ENUM(_sym_DIFlagsExternalTypeRef,llvm::DINode::FlagExternalTypeRef);
CL_VALUE_ENUM(_sym_DIFlagsSingleInheritance,llvm::DINode::FlagSingleInheritance);
CL_VALUE_ENUM(_sym_DIFlagsMultipleInheritance,llvm::DINode::FlagMultipleInheritance);
CL_VALUE_ENUM(_sym_DIFlagsVirtualInheritance,llvm::DINode::FlagVirtualInheritance);
CL_VALUE_ENUM(_sym_DIFlagsIntroducedVirtual,llvm::DINode::FlagIntroducedVirtual);
CL_VALUE_ENUM(_sym_DIFlagsBitField,llvm::DINode::FlagBitField);
CL_VALUE_ENUM(_sym_DIFlagsNoReturn,llvm::DINode::FlagNoReturn);
CL_END_ENUM(_sym_DIFlagsEnum);




SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagZero); // Use it as zero value.
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagVirtual);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagPureVirtual);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagLocalToUnit);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagDefinition);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagOptimized);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagPure);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagElemental);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagRecursive);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagMainSubprogram);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagNonvirtual);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagVirtuality);
SYMBOL_EXPORT_SC_(LlvmoPkg, DISPFlagEnum);
CL_BEGIN_ENUM(llvm::DISubprogram::DISPFlags,_sym_DISPFlagEnum,"DISPFlagEnum");
CL_VALUE_ENUM(_sym_DISPFlagZero,llvm::DISubprogram::SPFlagZero); // Use it as zero value.
CL_VALUE_ENUM(_sym_DISPFlagVirtual,llvm::DISubprogram::SPFlagVirtual);
CL_VALUE_ENUM(_sym_DISPFlagPureVirtual,llvm::DISubprogram::SPFlagPureVirtual);
CL_VALUE_ENUM(_sym_DISPFlagLocalToUnit,llvm::DISubprogram::SPFlagLocalToUnit);
CL_VALUE_ENUM(_sym_DISPFlagDefinition,llvm::DISubprogram::SPFlagDefinition);
CL_VALUE_ENUM(_sym_DISPFlagOptimized,llvm::DISubprogram::SPFlagOptimized);
CL_VALUE_ENUM(_sym_DISPFlagPure,llvm::DISubprogram::SPFlagPure);
CL_VALUE_ENUM(_sym_DISPFlagElemental,llvm::DISubprogram::SPFlagElemental);
CL_VALUE_ENUM(_sym_DISPFlagRecursive,llvm::DISubprogram::SPFlagRecursive);
CL_VALUE_ENUM(_sym_DISPFlagMainSubprogram,llvm::DISubprogram::SPFlagMainSubprogram);
CL_VALUE_ENUM(_sym_DISPFlagNonvirtual,llvm::DISubprogram::SPFlagNonvirtual);
CL_VALUE_ENUM(_sym_DISPFlagVirtuality,llvm::DISubprogram::SPFlagVirtuality);
CL_END_ENUM(_sym_DISPFlagEnum);



SYMBOL_EXPORT_SC_(KeywordPkg, CSK_None);
SYMBOL_EXPORT_SC_(KeywordPkg, CSK_MD5);
SYMBOL_EXPORT_SC_(KeywordPkg, CSK_SHA1);
SYMBOL_EXPORT_SC_(LlvmoPkg, CSKEnum);
CL_BEGIN_ENUM(llvm::DIFile::ChecksumKind,_sym_CSKEnum,"CSKEnum");
CL_VALUE_ENUM(kw::_sym_CSK_MD5,llvm::DIFile::CSK_MD5); // Use it as zero value.
CL_VALUE_ENUM(kw::_sym_CSK_SHA1,llvm::DIFile::CSK_SHA1); // Use it as zero value.
CL_END_ENUM(_sym_CSKEnum);


CL_LISPIFY_NAME(createExpression);
CL_EXTERN_DEFMETHOD(DIBuilder_O, (llvm::DIExpression* (llvm::DIBuilder::*)
                                  (llvm::ArrayRef<uint64_t>))&llvm::DIBuilder::createExpression);
CL_LISPIFY_NAME(createExpressionNone);
CL_DEFUN llvm::DIExpression* llvm_sys__createExpressionNone(DIBuilder_sp dib) {
  return dib->wrappedPtr()->createExpression();
}
  

SYMBOL_EXPORT_SC_(KeywordPkg, DNTK_Default);
SYMBOL_EXPORT_SC_(KeywordPkg, DNTK_GNU);
SYMBOL_EXPORT_SC_(KeywordPkg, DNTK_None);
SYMBOL_EXPORT_SC_(LlvmoPkg, DNTKEnum);
CL_BEGIN_ENUM(llvm::DICompileUnit::DebugNameTableKind,_sym_DNTKEnum,"DNTKEnum");
CL_VALUE_ENUM(kw::_sym_DNTK_Default,llvm::DICompileUnit::DebugNameTableKind::Default); // Use it as zero value.
CL_VALUE_ENUM(kw::_sym_DNTK_GNU,llvm::DICompileUnit::DebugNameTableKind::GNU);
CL_VALUE_ENUM(kw::_sym_DNTK_None,llvm::DICompileUnit::DebugNameTableKind::None);
CL_END_ENUM(_sym_DNTKEnum);


CL_LISPIFY_NAME(createCompileUnit);
CL_EXTERN_DEFMETHOD(DIBuilder_O,&llvm::DIBuilder::createCompileUnit);
CL_LISPIFY_NAME(createFile);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createFile);
CL_LISPIFY_NAME(createFunction);
CL_EXTERN_DEFMETHOD(DIBuilder_O,
                    (llvm::DISubprogram *
                    (llvm::DIBuilder::*)
                     (llvm::DIScope *Scope,
                      llvm::StringRef Name,
                      llvm::StringRef LinkageName,
                      llvm::DIFile *File,
                      unsigned LineNo,
                      llvm::DISubroutineType *Ty,
                      unsigned ScopeLine,
                      llvm::DINode::DIFlags Flags,
                      llvm::DISubprogram::DISPFlags SPFlags,
                      llvm::DITemplateParameterArray TParams,
                      llvm::DISubprogram *Decl,
                      llvm::DITypeArray ThrownTypes))
                    &llvm::DIBuilder::createFunction );
CL_LISPIFY_NAME(createLexicalBlock);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createLexicalBlock);
CL_LISPIFY_NAME(createBasicType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createBasicType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createTypedef);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createPointerType);

CL_LISPIFY_NAME(createNullPtrType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createNullPtrType);
CL_LISPIFY_NAME(createUnspecifiedParameter);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createUnspecifiedParameter);
CL_LISPIFY_NAME(createSubroutineType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createSubroutineType);
CL_LISPIFY_NAME(createAutoVariable);
CL_EXTERN_DEFMETHOD(DIBuilder_O,&llvm::DIBuilder::createAutoVariable);
CL_LISPIFY_NAME(createParameterVariable);
CL_EXTERN_DEFMETHOD(DIBuilder_O,&llvm::DIBuilder::createParameterVariable);

CL_LISPIFY_NAME(insertDbgValueIntrinsicBasicBlock);
CL_EXTERN_DEFMETHOD(DIBuilder_O,
                    (llvm::Instruction * (llvm::DIBuilder::*)
                     (llvm::Value *Val,
                      llvm::DILocalVariable *VarInfo,
                      llvm::DIExpression *Expr,
                      const llvm::DILocation *DL,
                      llvm::BasicBlock *InsertAtEnd))
                    &llvm::DIBuilder::insertDbgValueIntrinsic);


CL_LISPIFY_NAME(finalize);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::finalize);;
CL_LISPIFY_NAME(finalizeSubprogram);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::finalizeSubprogram);;



CL_LISPIFY_NAME(getOrCreateArray);
CL_DEFMETHOD DINodeArray_sp DIBuilder_O::getOrCreateArray(core::List_sp elements) {
  _G();
  //		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
  //		printf("     cons --> %s\n", cur->__repr__().c_str() );
  vector<llvm::Metadata *> vector_values;
  for (auto cur : elements) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      //			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
      llvm::ValueAsMetadata *vd = llvm::ValueAsMetadata::get(val->wrappedPtr());
      vector_values.push_back(vd); // val->wrappedPtr());
    } else if (DINode_sp di = oCar(cur).asOrNull<DINode_O>()) {
      llvm::MDNode *mdnode = di->operator llvm::MDNode *();
      vector_values.push_back(mdnode);
    } else {
      SIMPLE_ERROR(BF("Handle conversion of %s to llvm::Value*") % _rep_(oCar(cur)));
    }
  }
  llvm::ArrayRef<llvm::Metadata *> array(vector_values);
  llvm::DINodeArray diarray = this->wrappedPtr()->getOrCreateArray(array);
  GC_ALLOCATE_VARIADIC(llvmo::DINodeArray_O, obj, diarray);
  return obj;
}

CL_LISPIFY_NAME(getOrCreateTypeArray);
CL_DEFMETHOD DITypeRefArray_sp DIBuilder_O::getOrCreateTypeArray(core::List_sp elements) {
  _G();
  //		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
  //		printf("     cons --> %s\n", cur->__repr__().c_str() );
  vector<llvm::Metadata *> vector_values;
  for (auto cur : elements) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      //			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
      llvm::ValueAsMetadata *vd = llvm::ValueAsMetadata::get(val->wrappedPtr());
      vector_values.push_back(vd); // val->wrappedPtr());
                                   //vector_values.push_back(val->wrappedPtr());
    } else if (DINode_sp di = oCar(cur).asOrNull<DINode_O>()) {
      llvm::MDNode *mdnode = di->operator llvm::MDNode *();
      vector_values.push_back(mdnode);
    } else {
      SIMPLE_ERROR(BF("Handle conversion of %s to llvm::Value*") % _rep_(oCar(cur)));
    }
  }
  llvm::ArrayRef<llvm::Metadata *> array(vector_values);
  llvm::DITypeRefArray diarray = this->wrappedPtr()->getOrCreateTypeArray(array);
  GC_ALLOCATE_VARIADIC(llvmo::DITypeRefArray_O, obj, diarray);
  return obj;
}



}; // llvmo
