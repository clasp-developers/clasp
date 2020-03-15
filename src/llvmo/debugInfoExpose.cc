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

namespace llvmo { // DIContext_O

core::T_mv getLineInfoForAddressInner(llvm::DIContext* dicontext, llvm::object::SectionedAddress addr) {
  core::T_sp source;

  llvm::DILineInfoSpecifier lispec;
  lispec.FNKind = llvm::DILineInfoSpecifier::FunctionNameKind::LinkageName;
  lispec.FLIKind = llvm::DILineInfoSpecifier::FileLineInfoKind::AbsoluteFilePath;

  llvm::DILineInfo info = dicontext->getLineInfoForAddress(addr, lispec);

  if (info.Source.hasValue())
    source = core::SimpleBaseString_O::make(info.Source.getPointer()->str());
  else source = _Nil<core::T_O>();

  return Values(core::SimpleBaseString_O::make(info.FileName),
                core::SimpleBaseString_O::make(info.FunctionName),
                source,
                core::Integer_O::create(info.Line),
                core::Integer_O::create(info.Column),
                core::Integer_O::create(info.StartLine),
                core::Integer_O::create(info.Discriminator));
}

// We can't translate a DWARFContext_sp into a DIContext* directly, apparently.
// The SectionedAddress translation is also a bust.
CL_LAMBDA(dwarfcontext sectioned-address);
CL_LISPIFY_NAME(getLineInfoForAddress);
CL_DEFUN core::T_mv getLineInfoForAddress(DWARFContext_sp dc, SectionedAddress_sp addr) {
  return getLineInfoForAddressInner(dc->wrappedPtr(), addr->_value);
}

}; // llvmo, DIContext_O



namespace llvmo {

std::atomic<ObjectFileInfo*> global_object_files;


void save_object_file_info(const char* objectFileStart, size_t objectFileSize,
                           const char* faso_filename,
                           size_t faso_index,
                           size_t objectID )
{
//  register_object_file_with_gdb((void*)objectFileStart,objectFileSize);
//  printf("%s:%d:%s register object file %s\n", __FILE__, __LINE__, __FUNCTION__, faso_filename);
  ObjectFileInfo* ofi = new ObjectFileInfo();
  ofi->_faso_filename = faso_filename;
  ofi->_faso_index = faso_index;
  ofi->_objectID = objectID;
  ofi->_object_file_start = (void*)objectFileStart;
  ofi->_object_file_size = objectFileSize;
  ofi->_text_segment_start = my_thread->_text_segment_start;
  ofi->_text_segment_size = my_thread->_text_segment_size;
  ofi->_text_segment_SectionID = my_thread->_text_segment_SectionID;
  ofi->_stackmap_start = (void*)my_thread->_stackmap;
  ofi->_stackmap_size = my_thread->_stackmap_size;
  ObjectFileInfo* expected;
  ObjectFileInfo* current;
  do {
    current = global_object_files.load();
    ofi->_next = current;
    expected = current;
    global_object_files.compare_exchange_strong(expected,ofi);
  } while (expected!=current);
}

CL_DOCSTRING(R"doc(Identify the object file whose generated code range containss the instruction-pointer.
Return NIL if none or (values offset-from-start object-file). The index-from-start is the number of bytes of the instruction-pointer from the start of the code range.)doc");
CL_LISPIFY_NAME(object_file_for_instruction_pointer);
CL_DEFUN core::T_mv object_file_for_instruction_pointer(core::Pointer_sp instruction_pointer, bool verbose)
{
  ObjectFileInfo* cur = global_object_files.load();
  size_t count;
  char* ptr = (char*)instruction_pointer->ptr();
  if (!cur) {
    core::write_bf_stream(BF("No object files registered - cannot find object file for address %p\n") % (void*)ptr);
  }
  while (cur) {
    if (ptr>=(char*)cur->_text_segment_start&&ptr<((char*)cur->_text_segment_start+cur->_text_segment_size)) {
      // Here is the info for the SectionedAddress
      uintptr_t sectionID = cur->_text_segment_SectionID;
      uintptr_t offset = (ptr - (char*)cur->_text_segment_start);
      core::T_sp sectioned_address = SectionedAddress_O::create(sectionID, offset);
      // now the object file
      llvm::StringRef sbuffer((const char*)cur->_object_file_start, cur->_object_file_size);
      llvm::StringRef name("object-file-buffer");
      std::unique_ptr<llvm::MemoryBuffer> mbuf = llvm::MemoryBuffer::getMemBuffer(sbuffer, name, false);
      llvm::MemoryBufferRef mbuf_ref(*mbuf);
      auto eom = llvm::object::ObjectFile::createObjectFile(mbuf_ref);
      if (!eom)
        SIMPLE_ERROR(BF("Problem in objectFileForInstructionPointer"));
      ObjectFile_sp object_file = ObjectFile_O::create(eom->release());
      if (verbose) {
        core::write_bf_stream(BF("faso-file: %s  object-file-position: %lu  objectID: %lu\n") % cur->_faso_filename % cur->_faso_index % cur->_objectID);
        core::write_bf_stream(BF("SectionID: %lu    memory offset: %lu\n") % sectionID % offset );
      }
      return Values(sectioned_address,object_file);
    }
    cur = cur->_next;
    count++;
  }
  return Values(_Nil<core::T_O>());
}

CL_LISPIFY_NAME(number_of_object_files);
CL_DEFUN size_t number_of_object_files() {
  ObjectFileInfo* cur = global_object_files.load();
  size_t count=0;
  while (cur) {
    count++;
    cur = cur->_next;
  }
  return count;
}

CL_LISPIFY_NAME(total_memory_allocated_for_object_files);
CL_DEFUN size_t total_memory_allocated_for_object_files() {
  ObjectFileInfo* cur = global_object_files.load();
  size_t sz = 0;
  while (cur) {
    sz += cur->_object_file_size;
    cur = cur->_next;
  }
  return sz;
}
};

namespace llvmo { // DWARFContext_O

CL_LAMBDA(object-file);
CL_LISPIFY_NAME(createDwarfContext);
CL_DEFUN DWARFContext_sp DWARFContext_O::createDwarfContext(ObjectFile_sp of) {
  llvm::object::ObjectFile* ofptr = of->wrappedPtr();
  std::unique_ptr<llvm::DWARFContext> uptr = llvm::DWARFContext::create(*ofptr);
  return core::RP_Create_wrapped<llvmo::DWARFContext_O, llvm::DWARFContext *>(uptr.release());
}


CL_LAMBDA(address &key verbose);
CL_DEFUN core::T_mv llvm_sys__address_information(void* address, bool verbose)
{
  core::Pointer_sp instruction_pointer = core::Pointer_O::create(address);
  core::T_mv object_info = object_file_for_instruction_pointer(instruction_pointer,verbose);
  if (object_info.notnilp()) {
    SectionedAddress_sp sectioned_address = gc::As<SectionedAddress_sp>(object_info);
    ObjectFile_sp object_file = gc::As<ObjectFile_sp>(object_info.second());
    DWARFContext_sp context = DWARFContext_O::createDwarfContext(object_file);
    return getLineInfoForAddress(context,sectioned_address);
  }
  return Values(_Nil<core::T_O>());
}

  
}; // llvmo, DWARFContext_O

