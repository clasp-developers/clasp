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
// #define DEBUG_LEVEL_FULL

// #include <llvm/Support/system_error.h>
#include <clasp/core/foundation.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/IR/LLVMContext.h>
#include <clasp/llvmo/code.h>
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
// #define DEBUG_LEVEL_FULL

// #include <llvm/Support/system_error.h>
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
// #include <llvm/IR/PrintModulePass.h> // will be llvm/IR

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/package.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/lispStream.h>
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
#include <clasp/core/backtrace.h> // DebuggerLocal_O

namespace llvmo {

std::string DISubprogram_O::__repr__() const {
  stringstream ss;
  ss << "#<DISubprogram " << this->wrappedPtr()->getName().str() << " @" << (void*)this << ">";
  return ss.str();
}

std::string DILocalVariable_O::__repr__() const {
  stringstream ss;
  ss << "#<DILOCAL-VARIABLE " << this->wrappedPtr()->getName().str() << " @" << (void*)this << ">";
  return ss.str();
}

CL_DEFMETHOD std::string DILocalVariable_O::getVariableName() const { return this->wrappedPtr()->getName().str(); }

CL_DEFMETHOD std::string DISubprogram_O::getSubprogram() const { return this->wrappedPtr()->getName().str(); }

CL_DEFMETHOD std::string DIFile_O::getPath() const {
  std::string filename = this->wrappedPtr()->getFilename().str();
  std::string directory = this->wrappedPtr()->getDirectory().str();
  return (directory == ".") ? filename : (directory + "/" + filename);
}

std::string DIFile_O::__repr__() const {
  stringstream ss;
  std::string path = this->getPath();
  ss << "#<DIFile " << path << " @" << (void*)this << ">";
  return ss.str();
}

CL_LAMBDA(llvm-context line col scope &optional inlined-at);
CL_LISPIFY_NAME(get-dilocation);
DOCGROUP(clasp);
CL_DEFUN DILocation_sp DILocation_O::make(llvm::LLVMContext& context, unsigned int line, unsigned int col, DINode_sp scope,
                                          core::T_sp inlinedAt) {
  llvm::Metadata* realScope = scope->operator llvm::Metadata*();
  llvm::Metadata* realInlinedAt;
  if (inlinedAt.nilp())
    realInlinedAt = nullptr;
  else {
    DILocation_sp temp = gc::As<DILocation_sp>(inlinedAt);
    realInlinedAt = temp->operator llvm::Metadata*();
  }
  auto ret = gctools::GC<DILocation_O>::allocate_with_default_constructor();
  ret->set_wrapped(llvm::DILocation::get(context, line, col, realScope, realInlinedAt));
  return ret;
}

CL_LAMBDA(module);
CL_LISPIFY_NAME(make-dibuilder);
DOCGROUP(clasp);
CL_DEFUN DIBuilder_sp DIBuilder_O::make(Module_sp module) {
  _G();
  auto me = gctools::GC<DIBuilder_O>::allocate_with_default_constructor();
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
CL_BEGIN_ENUM(llvm::DINode::DIFlags, _sym_DIFlagsEnum, "DIFlagsEnum");
CL_VALUE_ENUM(_sym_DIFlagsZero, llvm::DINode::FlagZero); // Use it as zero value.
CL_VALUE_ENUM(_sym_DIFlagsPrivate, llvm::DINode::FlagPrivate);
CL_VALUE_ENUM(_sym_DIFlagsProtected, llvm::DINode::FlagProtected);
CL_VALUE_ENUM(_sym_DIFlagsPublic, llvm::DINode::FlagPublic);
CL_VALUE_ENUM(_sym_DIFlagsFwdDecl, llvm::DINode::FlagFwdDecl);
CL_VALUE_ENUM(_sym_DIFlagsAppleBlock, llvm::DINode::FlagAppleBlock);
// CL_VALUE_ENUM(_sym_DIFlagsBlockByrefStruct,llvm::DINode::FlagBlockByrefStruct);
CL_VALUE_ENUM(_sym_DIFlagsVirtual, llvm::DINode::FlagVirtual);
CL_VALUE_ENUM(_sym_DIFlagsArtificial, llvm::DINode::FlagArtificial);
CL_VALUE_ENUM(_sym_DIFlagsExplicit, llvm::DINode::FlagExplicit);
CL_VALUE_ENUM(_sym_DIFlagsPrototyped, llvm::DINode::FlagPrototyped);
CL_VALUE_ENUM(_sym_DIFlagsObjcClassComplete, llvm::DINode::FlagObjcClassComplete);
CL_VALUE_ENUM(_sym_DIFlagsObjectPointer, llvm::DINode::FlagObjectPointer);
CL_VALUE_ENUM(_sym_DIFlagsVector, llvm::DINode::FlagVector);
CL_VALUE_ENUM(_sym_DIFlagsStaticMember, llvm::DINode::FlagStaticMember);
CL_VALUE_ENUM(_sym_DIFlagsLValueReference, llvm::DINode::FlagLValueReference);
CL_VALUE_ENUM(_sym_DIFlagsRValueReference, llvm::DINode::FlagRValueReference);
// CL_VALUE_ENUM(_sym_DIFlagsExternalTypeRef,llvm::DINode::FlagExternalTypeRef);
CL_VALUE_ENUM(_sym_DIFlagsSingleInheritance, llvm::DINode::FlagSingleInheritance);
CL_VALUE_ENUM(_sym_DIFlagsMultipleInheritance, llvm::DINode::FlagMultipleInheritance);
CL_VALUE_ENUM(_sym_DIFlagsVirtualInheritance, llvm::DINode::FlagVirtualInheritance);
CL_VALUE_ENUM(_sym_DIFlagsIntroducedVirtual, llvm::DINode::FlagIntroducedVirtual);
CL_VALUE_ENUM(_sym_DIFlagsBitField, llvm::DINode::FlagBitField);
CL_VALUE_ENUM(_sym_DIFlagsNoReturn, llvm::DINode::FlagNoReturn);
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
CL_BEGIN_ENUM(llvm::DISubprogram::DISPFlags, _sym_DISPFlagEnum, "DISPFlagEnum");
CL_VALUE_ENUM(_sym_DISPFlagZero, llvm::DISubprogram::SPFlagZero); // Use it as zero value.
CL_VALUE_ENUM(_sym_DISPFlagVirtual, llvm::DISubprogram::SPFlagVirtual);
CL_VALUE_ENUM(_sym_DISPFlagPureVirtual, llvm::DISubprogram::SPFlagPureVirtual);
CL_VALUE_ENUM(_sym_DISPFlagLocalToUnit, llvm::DISubprogram::SPFlagLocalToUnit);
CL_VALUE_ENUM(_sym_DISPFlagDefinition, llvm::DISubprogram::SPFlagDefinition);
CL_VALUE_ENUM(_sym_DISPFlagOptimized, llvm::DISubprogram::SPFlagOptimized);
CL_VALUE_ENUM(_sym_DISPFlagPure, llvm::DISubprogram::SPFlagPure);
CL_VALUE_ENUM(_sym_DISPFlagElemental, llvm::DISubprogram::SPFlagElemental);
CL_VALUE_ENUM(_sym_DISPFlagRecursive, llvm::DISubprogram::SPFlagRecursive);
CL_VALUE_ENUM(_sym_DISPFlagMainSubprogram, llvm::DISubprogram::SPFlagMainSubprogram);
CL_VALUE_ENUM(_sym_DISPFlagNonvirtual, llvm::DISubprogram::SPFlagNonvirtual);
CL_VALUE_ENUM(_sym_DISPFlagVirtuality, llvm::DISubprogram::SPFlagVirtuality);
CL_END_ENUM(_sym_DISPFlagEnum);

SYMBOL_EXPORT_SC_(KeywordPkg, CSK_None);
SYMBOL_EXPORT_SC_(KeywordPkg, CSK_MD5);
SYMBOL_EXPORT_SC_(KeywordPkg, CSK_SHA1);
SYMBOL_EXPORT_SC_(LlvmoPkg, CSKEnum);
CL_BEGIN_ENUM(llvm::DIFile::ChecksumKind, _sym_CSKEnum, "CSKEnum");
CL_VALUE_ENUM(kw::_sym_CSK_MD5, llvm::DIFile::CSK_MD5);   // Use it as zero value.
CL_VALUE_ENUM(kw::_sym_CSK_SHA1, llvm::DIFile::CSK_SHA1); // Use it as zero value.
CL_END_ENUM(_sym_CSKEnum);

CL_LAMBDA(dibuilder addr);
CL_LISPIFY_NAME(createExpression);
CL_EXTERN_DEFMETHOD(DIBuilder_O,
                    (llvm::DIExpression * (llvm::DIBuilder::*)(llvm::ArrayRef<uint64_t>)) & llvm::DIBuilder::createExpression);

CL_LISPIFY_NAME(createExpressionNone);
DOCGROUP(clasp);
CL_DEFUN llvm::DIExpression* llvm_sys__createExpressionNone(DIBuilder_sp dib) { return dib->wrappedPtr()->createExpression(); }

SYMBOL_EXPORT_SC_(KeywordPkg, DNTK_Default);
SYMBOL_EXPORT_SC_(KeywordPkg, DNTK_GNU);
SYMBOL_EXPORT_SC_(KeywordPkg, DNTK_None);
SYMBOL_EXPORT_SC_(LlvmoPkg, DNTKEnum);
CL_BEGIN_ENUM(llvm::DICompileUnit::DebugNameTableKind, _sym_DNTKEnum, "DNTKEnum");
CL_VALUE_ENUM(kw::_sym_DNTK_Default, llvm::DICompileUnit::DebugNameTableKind::Default); // Use it as zero value.
CL_VALUE_ENUM(kw::_sym_DNTK_GNU, llvm::DICompileUnit::DebugNameTableKind::GNU);
CL_VALUE_ENUM(kw::_sym_DNTK_None, llvm::DICompileUnit::DebugNameTableKind::None);
CL_END_ENUM(_sym_DNTKEnum);

CL_LAMBDA(dibuilder lang file producer optimizedp flags runtime-version split-name emission-kind DW-old split-debug-inlining debug-info-for-profiling name-table-kind ranges-base-address sysroot sdk);
CL_LISPIFY_NAME(createCompileUnit);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createCompileUnit);

CL_LAMBDA(dibuilder filename directory checksum source);
CL_LISPIFY_NAME(createFile);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createFile);

CL_LAMBDA(dibuilder scope name linkage-name file lineno ty scope-line flags subprogram-flags template-params decl thrown-types annotations target-func-name);
CL_LISPIFY_NAME(createFunction);
CL_EXTERN_DEFMETHOD(DIBuilder_O,
                    (llvm::DISubprogram * (llvm::DIBuilder::*)(llvm::DIScope * Scope, llvm::StringRef Name,
                                                               llvm::StringRef LinkageName, llvm::DIFile* File, unsigned LineNo,
                                                               llvm::DISubroutineType* Ty, unsigned ScopeLine,
                                                               llvm::DINode::DIFlags Flags, llvm::DISubprogram::DISPFlags SPFlags,
                                                               llvm::DITemplateParameterArray TParams, llvm::DISubprogram* Decl,
                                                               llvm::DITypeArray ThrownTypes, llvm::DINodeArray Annotations,
                                                               llvm::StringRef TargetFunctionName)) &
                        llvm::DIBuilder::createFunction);

CL_LAMBDA(dibuilder scope file lineno column);
CL_LISPIFY_NAME(createLexicalBlock);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createLexicalBlock);

CL_LAMBDA(dibuilder name size encoding flags);
CL_LISPIFY_NAME(createBasicType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createBasicType);
CL_LAMBDA(dibuilder type name file lineno context alignment flags annotations);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createTypedef);
CL_LAMBDA(dibuilder pointee-type size alignment dwarf-address-space name annotations);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createPointerType);

CL_LAMBDA(dibuilder);
CL_LISPIFY_NAME(createNullPtrType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createNullPtrType);
CL_LAMBDA(dibuilder scope name file lineno size alignment flags derived-from elements runtime-lang vtable-holder unique-id);
CL_LISPIFY_NAME(createStructType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createStructType);

CL_LAMBDA(ditype);
CL_LISPIFY_NAME(getSizeInBits);
CL_EXTERN_DEFMETHOD(DIType_O, &llvm::DIType::getSizeInBits);

CL_LAMBDA(dibuilder);
CL_LISPIFY_NAME(createUnspecifiedParameter);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createUnspecifiedParameter);
CL_LAMBDA(dibuilder parameter-types flags calling-convention);
CL_LISPIFY_NAME(createSubroutineType);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createSubroutineType);
CL_LAMBDA(dibuilder scope name file lineno type always-preserve-p flags alignment);
CL_LISPIFY_NAME(createAutoVariable);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createAutoVariable);
CL_LAMBDA(dibuilder scope name argno file lineno type always-preserve-p flags annotations);
CL_LISPIFY_NAME(createParameterVariable);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::createParameterVariable);

// We don't expose the instruction version since we don't really need it.
CL_LAMBDA(dibuilder val varinfo expr dilocation basic-block);
CL_LISPIFY_NAME(insertDbgValueIntrinsic);
CL_EXTERN_DEFMETHOD(DIBuilder_O, (llvm::Instruction * (llvm::DIBuilder::*)(llvm::Value * Val, llvm::DILocalVariable* VarInfo,
                                                                           llvm::DIExpression* Expr, const llvm::DILocation* DL,
                                                                           llvm::BasicBlock* InsertAtEnd)) &
                                     llvm::DIBuilder::insertDbgValueIntrinsic);

CL_LAMBDA(dibuilder);
CL_LISPIFY_NAME(finalize);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::finalize);
;
CL_LAMBDA(dibuilder subprogram);
CL_LISPIFY_NAME(finalizeSubprogram);
CL_EXTERN_DEFMETHOD(DIBuilder_O, &llvm::DIBuilder::finalizeSubprogram);
;

CL_LISPIFY_NAME(getOrCreateArray);
CL_DEFMETHOD DINodeArray_sp DIBuilder_O::getOrCreateArray(core::T_sp telements) {
  _G();
  //		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
  //		printf("     cons --> %s\n", cur->__repr__().c_str() );
  vector<llvm::Metadata*> vector_values;
  if (telements.consp()) {
    core::List_sp elements = gc::As_unsafe<core::List_sp>(telements);
    for (auto cur : elements) {
      if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
        //			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
        llvm::ValueAsMetadata* vd = llvm::ValueAsMetadata::get(val->wrappedPtr());
        vector_values.push_back(vd); // val->wrappedPtr());
      } else if (DINode_sp di = oCar(cur).asOrNull<DINode_O>()) {
        llvm::MDNode* mdnode = di->operator llvm::MDNode*();
        vector_values.push_back(mdnode);
      } else {
        SIMPLE_ERROR("Handle conversion of {} to llvm::Value*", _rep_(oCar(cur)));
      }
    }
  }
  llvm::ArrayRef<llvm::Metadata*> array(vector_values);
  llvm::DINodeArray diarray = this->wrappedPtr()->getOrCreateArray(array);
  auto obj = gctools::GC<llvmo::DINodeArray_O>::allocate(diarray);
  return obj;
}

CL_LISPIFY_NAME(getOrCreateTypeArray);
CL_DEFMETHOD DITypeRefArray_sp DIBuilder_O::getOrCreateTypeArray(core::List_sp elements) {
  _G();
  //		printf("%s:%d About to convert Cons into ArrayRef<llvm::Value*>\n", __FILE__, __LINE__);
  //		printf("     cons --> %s\n", cur->__repr__().c_str() );
  vector<llvm::Metadata*> vector_values;
  for (auto cur : elements) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      //			printf("      push_back val->wrappedPtr() --> %p\n", val->wrappedPtr());
      llvm::ValueAsMetadata* vd = llvm::ValueAsMetadata::get(val->wrappedPtr());
      vector_values.push_back(vd); // val->wrappedPtr());
                                   // vector_values.push_back(val->wrappedPtr());
    } else if (DINode_sp di = oCar(cur).asOrNull<DINode_O>()) {
      llvm::MDNode* mdnode = di->operator llvm::MDNode*();
      vector_values.push_back(mdnode);
    } else if (oCar(cur).nilp()) {
      // null metadata is used in a few places, such as to indicate
      // a void return type in a DISubroutineType.
      vector_values.push_back(nullptr);
    } else {
      SIMPLE_ERROR("Handle conversion of {} to llvm::Value*", _rep_(oCar(cur)));
    }
  }
  llvm::ArrayRef<llvm::Metadata*> array(vector_values);
  llvm::DITypeRefArray diarray = this->wrappedPtr()->getOrCreateTypeArray(array);
  auto obj = gctools::GC<llvmo::DITypeRefArray_O>::allocate(diarray);
  return obj;
}

}; // namespace llvmo

namespace llvmo { // DIContext_O

core::T_mv getLineInfoForAddressInner(llvm::DIContext* dicontext, llvm::object::SectionedAddress addr, bool verbose) {
  core::T_sp source;

  llvm::DILineInfoSpecifier lispec;
  lispec.FNKind = llvm::DILineInfoSpecifier::FunctionNameKind::LinkageName;
  lispec.FLIKind = llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath;

  if (verbose) {
    core::clasp_write_string(fmt::format("Entered {} {} {}\n", __FILE__, __LINE__, __FUNCTION__));
    core::clasp_write_string(
        fmt::format("  DIContext* {}  SectionedAddress: {} {}\n", (void*)dicontext, (void*)addr.SectionIndex, (void*)addr.Address));
  }
  llvm::DILineInfo info = dicontext->getLineInfoForAddress(addr, lispec);
  if (info.FileName == info.BadString) {
    if (verbose) {
      core::clasp_write_string(fmt::format("info.Filename is info.BadString:  {}\n", info.FileName));
      core::clasp_write_string(fmt::format("Source {}\n", _rep_(source)));
      core::clasp_write_string(fmt::format("info.Line {}\n", info.Line));
      core::clasp_write_string(fmt::format("info.Column {}\n", info.Column));
      core::clasp_write_string(fmt::format("info.StartLine {}\n", info.StartLine));
    }
    return nil<core::T_O>();
  }
#if LLVM_VERSION_MAJOR < 16
  if (info.Source.hasValue())
    source = core::SimpleBaseString_O::make(info.Source.getPointer()->str());
  else
    source = nil<core::T_O>();
#else
  if (info.Source.has_value())
    source = core::SimpleBaseString_O::make(info.Source.value().str());
  else
    source = nil<core::T_O>();
#endif
  if (verbose) {
    core::clasp_write_string(fmt::format("info.Filename {}\n", info.FileName));
    core::clasp_write_string(fmt::format("Source {}\n", _rep_(source)));
    core::clasp_write_string(fmt::format("info.Line {}\n", info.Line));
    core::clasp_write_string(fmt::format("info.Column {}\n", info.Column));
    core::clasp_write_string(fmt::format("info.StartLine {}\n", info.StartLine));
  }

  return Values(core::SimpleBaseString_O::make((info.FileName.substr(0, 2) == "./") ? info.FileName.substr(2) : info.FileName),
                core::SimpleBaseString_O::make(info.FunctionName), source, core::Integer_O::create(info.Line),
                core::Integer_O::create(info.Column), core::Integer_O::create(info.StartLine),
                core::Integer_O::create(info.Discriminator));
}

// Get the function name for an address, i.e. part of the line info.
// We go through DIEs to get a const char* we don't need to free.
// FIXME: This is all needs reorganization badly.
const char* getFunctionNameForAddress(DWARFContext_sp sdc, SectionedAddress_sp saddr) {
  llvm::DWARFContext* dicontext = sdc->wrappedPtr();
  llvm::object::SectionedAddress addr = saddr->_value;

  auto dies = dicontext->getDIEsForAddress(addr.Address);
  llvm::DWARFDie fdie = dies.FunctionDIE;
  if (fdie.isValid())
    return fdie.getSubroutineName(llvm::DINameKind::LinkageName);
  else
    return NULL;
}

// We can't translate a DWARFContext_sp into a DIContext* directly, apparently.
// The SectionedAddress translation is also a bust.
CL_LAMBDA(dwarfcontext sectioned-address &key verbose);
CL_LISPIFY_NAME(getLineInfoForAddress);
DOCGROUP(clasp);
CL_DEFUN core::T_mv getLineInfoForAddress_(DWARFContext_sp dc, SectionedAddress_sp addr, bool verbose) {
  return getLineInfoForAddressInner(dc->wrappedPtr(), addr->_value, verbose);
}

CL_LAMBDA(dwarfcontext sectioned-address);
CL_LISPIFY_NAME(getLocalsForAddress);
DOCGROUP(clasp);
CL_DEFUN core::T_sp getLocalsForAddress(DWARFContext_sp dc, SectionedAddress_sp addr) {
  llvm::DIContext* dicontext = dc->wrappedPtr();
  llvm::object::SectionedAddress saddr = addr->_value;
  ql::list res;
  for (llvm::DILocal loc : dicontext->getLocalsForAddress(saddr)) {
    res << core::DebuggerLocal_O::make(core::SimpleBaseString_O::make(loc.FunctionName), core::SimpleBaseString_O::make(loc.Name),
                                       core::SimpleBaseString_O::make(loc.DeclFile), core::Integer_O::create(loc.DeclLine));
  }
  return res.cons();
}

// FIXME: name
llvm::Expected<std::vector<llvm::DWARFAddressRange>> getAddressRangesForAddressInner(DWARFContext_sp dc, SectionedAddress_sp sa) {
  uint64_t addr = sa->_value.Address;
  llvm::DWARFContext* ldc = dc->wrappedPtr();
  llvm::DWARFContext::DIEsForAddress dies = ldc->getDIEsForAddress(addr);
  llvm::DWARFDie fdie = dies.FunctionDIE;
  if (fdie.isValid())
    return fdie.getAddressRanges();
  // Now what?
  return std::vector<llvm::DWARFAddressRange>();
}

CL_LAMBDA(dwarfcontext sectioned-address);
CL_LISPIFY_NAME(getAddressRangesForAddress);
CL_DOCSTRING(R"dx(Return the DWARF address ranges for the function DIE containing this address.)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp getAddressRangesForAddress(DWARFContext_sp dc, SectionedAddress_sp sa) {
  auto eranges = getAddressRangesForAddressInner(dc, sa);
  if (eranges) {
    ql::list res;
    for (auto range : eranges.get())
      res << core::Cons_O::create(core::Integer_O::create(range.LowPC), core::Integer_O::create(range.HighPC));
    return res.cons();
  } else // TODO: signal error?
    return nil<core::T_O>();
}

}; // namespace llvmo

namespace llvmo { // DWARFContext_O

CL_LAMBDA(object-file);
CL_LISPIFY_NAME(create-dwarf-context);
DOCGROUP(clasp);
CL_DEFUN DWARFContext_sp DWARFContext_O::createDWARFContext(ObjectFile_sp ofi) {
  llvm::StringRef sbuffer((const char*)ofi->_MemoryBuffer->getBufferStart(), ofi->_MemoryBuffer->getBufferSize());
  stringstream ss;
  ss << "DWARFContext/" << ofi->_FasoName;
  std::string uniqueName = ensureUniqueMemoryBufferName(ss.str());
  llvm::StringRef name(uniqueName);
  //  printf("%s:%d uniqueName = %s\n", __FILE__, __LINE__, uniqueName.c_str());
  std::unique_ptr<llvm::MemoryBuffer> mbuf = llvm::MemoryBuffer::getMemBuffer(sbuffer, name, false);
  llvm::MemoryBufferRef mbuf_ref(*mbuf);
  auto errOrObj = llvm::object::ObjectFile::createObjectFile(mbuf_ref);
  if (errOrObj) {
    auto& obj = *errOrObj;
    //    auto name = obj->getFileName();
    //    printf("%s:%d:%s filename: %s\n", __FILE__, __LINE__, __FUNCTION__, name.str().c_str());
    std::unique_ptr<llvm::DWARFContext> uptr = llvm::DWARFContext::create(*obj.release());
    return core::RP_Create_wrapped<llvmo::DWARFContext_O, llvm::DWARFContext*>(uptr.release());
  }
  SIMPLE_ERROR("Could not get ObjectFile:\n{}", llvm::toString(std::move(errOrObj.takeError())));
}

CL_DEFMETHOD size_t DWARFContext_O::getNumCompileUnits() const { return this->wrappedPtr()->getNumCompileUnits(); }

CL_LAMBDA(dwarf-context index);
CL_LISPIFY_NAME(get-unit-at-index);
CL_EXTERN_DEFMETHOD(DWARFContext_O, (llvm::DWARFUnit * (llvm::DWARFContext::*)(unsigned int)) & llvm::DWARFContext::getUnitAtIndex);

CL_LAMBDA(dwarf-context unit);
CL_LISPIFY_NAME(get-line-table-for-unit);
CL_EXTERN_DEFMETHOD(DWARFContext_O, (const llvm::DWARFDebugLine::LineTable* (llvm::DWARFContext::*)(DWARFUnit*)) &
                                        llvm::DWARFContext::getLineTableForUnit);

CL_LAMBDA(address &key verbose);
DOCGROUP(clasp);
CL_DEFUN core::T_mv llvm_sys__address_information(void* address, bool verbose) {
  core::T_mv objectinfo = object_file_for_instruction_pointer(address, verbose);
  if (objectinfo.notnilp()) {
    SectionedAddress_sp sectioned_address = gc::As<SectionedAddress_sp>(objectinfo);
    core::MultipleValues& mvn = core::lisp_multipleValues();
    ObjectFile_sp object_file = gc::As<ObjectFile_sp>(mvn.second(objectinfo.number_of_values()));
    DWARFContext_sp context = DWARFContext_O::createDWARFContext(object_file);
    if (verbose) {
      core::clasp_write_string(fmt::format("Address: {}\n", address));
      core::clasp_write_string(fmt::format("ObjectFile: {} SectionedAddress: {} DWARFContext: {}\n", _rep_(object_file),
                                           _rep_(sectioned_address), _rep_(context)));
      core::clasp_write_string(fmt::format("Object_File object: {}\n", _rep_(object_file)));
      core::clasp_write_string(
          fmt::format("Object_File _text start: {}   end: {}\n", object_file->_TextSectionStart, object_file->_TextSectionEnd));
      core::clasp_write_string(fmt::format("address ({}) - _TextSectionStart({}) -> {}\n", (void*)address,
                                           (void*)object_file->_TextSectionStart,
                                           (intptr_t)((uintptr_t)address - (uintptr_t)object_file->_TextSectionStart)));
    }
    return getLineInfoForAddress_(context, sectioned_address, verbose);
  }
  if (verbose) {
    core::clasp_write_string("Could not find ObjectFile\n");
  }
  return Values(nil<core::T_O>());
}

CL_DEFMETHOD size_t LineTable_O::size() const { return this->wrappedPtr()->Rows.size(); };

CL_DEFMETHOD core::List_sp LineTable_O::element(size_t index) const {
  if (index < this->size()) {
    const llvm::DWARFDebugLine::Row& row = this->wrappedPtr()->Rows[index];
    ql::list ll;
    ll << SectionedAddress_O::create(row.Address.SectionIndex, row.Address.Address);
    ll << core::make_fixnum(row.Line);
    ll << core::make_fixnum(row.Column);
    ll << core::make_fixnum(row.File);
    ll << core::make_fixnum(row.Discriminator);
    ll << core::make_fixnum(row.Isa);
    ll << core::make_fixnum(row.IsStmt);
    ll << core::make_fixnum(row.BasicBlock);
    ll << core::make_fixnum(row.EndSequence);
    ll << core::make_fixnum(row.PrologueEnd);
    ll << core::make_fixnum(row.EpilogueBegin);
    return ll.result();
  }
  return nil<core::T_O>();
}

CL_DEFUN void llvm_sys__dwarf_context_verify(DWARFContext_sp dwarfContext) {
  llvm::raw_ostream& OS = llvm::errs();
  dwarfContext->wrappedPtr()->verify(OS);
}

}; // namespace llvmo
