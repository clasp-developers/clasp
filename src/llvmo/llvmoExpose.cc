/*
    File: llvmoExpose.cc

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
#include <dlfcn.h>
#include <clasp/core/foundation.h>
//
// The include for Debug.h must be first so we can force NDEBUG undefined
// otherwise setCurrentDebugTypes will be an empty macro
#ifdef NDEBUG
#undef NDEBUG
#include "llvm/Support/Debug.h"
#define NDEBUG
#else
#include "llvm/Support/Debug.h"
#endif

#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#define private public
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#undef private
#include <llvm/IR/LLVMContext.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IRReader/IRReader.h>
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDisassembler/MCDisassembler.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Error.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/LTO/legacy/ThinLTOCodeGenerator.h>
#include <llvm/Analysis/ModuleSummaryAnalysis.h>
#include <llvm/Analysis/ProfileSummaryInfo.h>
#include <llvm/ADT/Triple.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Mangler.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/CodeGen/TargetPassConfig.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Pass.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/AssemblyAnnotationWriter.h> // will be llvm/IR
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm-c/Disassembler.h>
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

#if defined(USE_LIBUNWIND) && defined(_TARGET_OS_LINUX)
#include <libunwind.h>
#endif

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispCallingConvention.h>
#include <clasp/core/package.h>
#include <clasp/core/debugger.h>
#include <clasp/core/environment.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bignum.h>
#include <clasp/core/compiler.h>
#include <clasp/core/bformat.h>
#include <clasp/core/pointer.h>
#include <clasp/core/array.h>
#include <clasp/gctools/gc_interface.fwd.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/core/lightProfiler.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/intrinsics.h>
#include <clasp/core/external_wrappers.h>
#include <clasp/core/wrappers.h>
#include <clasp/core/symbolTable.h>




llvm::Value* llvm_cast_error_ptr;



namespace llvmo {


llvm::raw_pwrite_stream* llvm_stream(core::T_sp stream,llvm::SmallString<1024>& stringOutput,bool& stringOutputStream) {
  llvm::raw_pwrite_stream *ostreamP;
  if (core::StringOutputStream_sp sos = stream.asOrNull<core::StringOutputStream_O>()) {
    (void)sos;
    ostreamP = new llvm::raw_svector_ostream(stringOutput);
    stringOutputStream = true;
  } else if (core::IOFileStream_sp fs = stream.asOrNull<core::IOFileStream_O>()) {
    ostreamP = new llvm::raw_fd_ostream(fs->fileDescriptor(), false, true);
  } else if (core::IOStreamStream_sp iostr = stream.asOrNull<core::IOStreamStream_O>()) {
    FILE *f = iostr->file();
    ostreamP = new llvm::raw_fd_ostream(fileno(f), false, true);
  } else {
    SIMPLE_ERROR(BF("Illegal file type %s for addPassesToEmitFileAndRunPassManager") % _rep_(stream));
  }
  return ostreamP;
}




CL_DEFUN bool llvm_sys__llvm_value_p(core::T_sp o) {
  if (o.nilp())
    return false;
  if (Value_sp v = o.asOrNull<Value_O>()) {
    (void)v;
    return true;
  }
  return false;
};

CL_DEFUN std::string llvm_sys__get_default_target_triple() {
  return llvm::sys::getDefaultTargetTriple();
}


#ifdef CLASP_THREADS
mp::Mutex* global_disassemble_mutex = NULL;
#endif
#define CALLBACK_BUFFER_SIZE 1024
char global_LLVMOpInfoCallbackBuffer[CALLBACK_BUFFER_SIZE];
char global_LLVMSymbolLookupCallbackBuffer[CALLBACK_BUFFER_SIZE];

int my_LLVMOpInfoCallback(void* DisInfo, uint64_t pc, uint64_t offset, uint64_t size, int tagType, void* TagBuf)
{
//  printf("%s:%d:%s pc->%p offset->%llu size->%llu tagType->%d\n",  __FILE__, __LINE__, __FUNCTION__, (void*)pc, offset, size, tagType);
  return 0;
}


const char* my_LLVMSymbolLookupCallback (void *DisInfo, uint64_t ReferenceValue, uint64_t *ReferenceType, uint64_t ReferencePC, const char **ReferenceName) {
  const char* symbol;
  uintptr_t start, end;
  char type;
  bool found = core::lookup_address((uintptr_t)ReferenceValue, symbol, start, end, type);
//  printf("%s:%d:%s ReferenceValue->%p ReferencePC->%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)ReferenceValue, (void*)ReferencePC);
  if (found) {
    stringstream ss;
    ss << (void*)ReferenceValue << "{";
    ss << symbol;
    if (ReferenceValue!=start) {
      ss << "+" << (ReferenceValue-start);
    }
    if (symbol[0]=='_'
        && strlen(symbol)>strlen(CONTAB_NAME)
        && strncmp(CONTAB_NAME,symbol+1,strlen(CONTAB_NAME))==0) {
      ss << "["<< dbg_safe_repr((uintptr_t)*(uintptr_t*)ReferenceValue)<<"]";
    }
    ss << "}";
    strcpy(global_LLVMSymbolLookupCallbackBuffer,ss.str().c_str());
    *ReferenceName = global_LLVMSymbolLookupCallbackBuffer;
//    printf("%s:%d:%s Returning symbol-table result |%s|\n", __FILE__, __LINE__, __FUNCTION__, *ReferenceName);
    return *ReferenceName;
  }
  Dl_info data;
  int ret = dladdr((void*)ReferenceValue,&data);
  if (ret!=0) {
    stringstream ss;
    ss << data.dli_sname;
    if (ReferenceValue != (uintptr_t)data.dli_saddr) {
      ss << "+" << (ReferenceValue-(uintptr_t)data.dli_saddr);
    }
    strcpy(global_LLVMSymbolLookupCallbackBuffer,ss.str().c_str());
    *ReferenceName = global_LLVMSymbolLookupCallbackBuffer;
//    printf("%s:%d:%s Returning dladdr result |%s|\n", __FILE__, __LINE__, __FUNCTION__, *ReferenceName);
    return *ReferenceName;
  }
  *ReferenceName = NULL;
  return NULL;
}

/*! Disassemble code from the start-address to end-address.
TODO: See the link below to make the disassbmely more informative by emitting comments, symbols and latency
   http://llvm.org/doxygen/Disassembler_8cpp_source.html#l00248
*/
CL_LAMBDA(target-triple start-address end-address)
CL_DEFUN void llvm_sys__disassemble_instructions(const std::string& striple,
                                                 core::Pointer_sp start_address,
                                                 core::Pointer_sp end_address)
{
#define DISASM_NUM_BYTES 32
#define DISASM_OUT_STRING_SIZE 128
  if (global_disassemble_mutex == NULL) {
    global_disassemble_mutex = new mp::Mutex(DISSASSM_NAMEWORD);
  }
  WITH_READ_WRITE_LOCK(*global_disassemble_mutex);
  LLVMDisasmContextRef dis = LLVMCreateDisasm(striple.c_str(),
                                              NULL,
                                              0,
                                              my_LLVMOpInfoCallback,
                                              my_LLVMSymbolLookupCallback);
  LLVMSetDisasmOptions(dis,LLVMDisassembler_Option_PrintImmHex|LLVMDisassembler_Option_PrintLatency
                       /*|LLVMDisassembler_Option_UseMarkup*/);
  size_t ii = 0;
  size_t offset = 0;
  for ( uint8_t* addr = (uint8_t*)start_address->ptr(); addr<(uint8_t*)end_address->ptr(); ) {
    ArrayRef<uint8_t> Bytes(addr,DISASM_NUM_BYTES);
    SmallVector<char, DISASM_OUT_STRING_SIZE> InsnStr;
    size_t sz = LLVMDisasmInstruction(dis,(unsigned char*)&Bytes[0],DISASM_NUM_BYTES,(uint64_t)addr,(char*)InsnStr.data(),DISASM_OUT_STRING_SIZE-1);
    const char* str = InsnStr.data();
    stringstream ss;
    ss << std::hex << (void*)addr << " <#" << std::dec << std::setw(3) << ii << "+" << offset <<  ">";
    core::clasp_write_string(ss.str());
    core::writestr_stream(str);
    core::clasp_terpri();
    if (sz==0) {
      ss << "STOPPING BECAUSE PREVIOUS INSTRUCTION HAS ZERO LENGTH!!!!! ";
      break;
    }
    addr += sz;
    ii++;
    offset += sz;
  }
  LLVMDisasmDispose(dis);
}


CL_DEFUN LLVMContext_sp LLVMContext_O::create_llvm_context() {
  GC_ALLOCATE(LLVMContext_O, context);
  llvm::LLVMContext* lc = new llvm::LLVMContext();
  context->_ptr = lc;
  return context;
};

CL_DEFMETHOD bool LLVMContext_O::LLVMContext_equal(core::T_sp obj) const {
  if (LLVMContext_sp t = obj.asOrNull<LLVMContext_O>()) {
    return t->_ptr == this->_ptr;
  }
  return false;
}

string LLVMContext_O::__repr__() const {
  stringstream ss;
  ss << "#<LLVM-CONTEXT " << (void*)this->_ptr << ">";
  return ss.str();
}
}

namespace llvmo {
CL_DEFUN ThreadSafeContext_sp ThreadSafeContext_O::create_thread_safe_context() {
  std::unique_ptr<llvm::LLVMContext> lc(new llvm::LLVMContext());
  GC_ALLOCATE(ThreadSafeContext_O, context);
  llvm::orc::ThreadSafeContext* tslc = new llvm::orc::ThreadSafeContext(std::move(lc));
  context->_ptr = tslc;
  return context;
};


CL_DEFMETHOD LLVMContext* ThreadSafeContext_O::getContext() { return this->wrappedPtr()->getContext(); };

};

namespace llvmo {

CL_LISPIFY_NAME(make-linker);
CL_DEFUN Linker_sp Linker_O::make(Module_sp module) {
  GC_ALLOCATE(Linker_O, self);
  self->_ptr = new llvm::Linker(*module->wrappedPtr());
  return self;
};

CL_DEFUN core::T_mv llvm_sys__link_in_module(Linker_sp linker, Module_sp module) {
  std::string errorMsg = "llvm::Linker::linkInModule reported an error";
  // Take ownership of the pointer and give it to the linker
  llvm::Module* mptr = module->wrappedPtr();
  module->reset_wrappedPtr();
  std::unique_ptr<llvm::Module> u_module(mptr);
  bool res = linker->wrappedPtr()->linkInModule(std::move(u_module));
  return Values(_lisp->_boolean(res), core::SimpleBaseString_O::make(errorMsg));
};



//  CL_LISPIFY_NAME(getModule);
//  CL_EXTERN_DEFMETHOD(Linker_O, &llvm::Linker::getModule);

;

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(jitdylib-dump);
CL_DEFMETHOD void JITDylib_O::dump(core::T_sp stream) {
  bool stringOutputStream = false;
  llvm::SmallString<1024> stringOutput;
  llvm::raw_pwrite_stream* ostreamP = llvm_stream(stream,stringOutput,stringOutputStream);
  this->wrappedPtr()->dump(*ostreamP);
  if (core::StringOutputStream_sp sos = stream.asOrNull<core::StringOutputStream_O>()) {
    sos->fill(stringOutput.c_str());
  }
};

};



namespace llvmo {

CL_DEFUN Instruction_sp llvm_sys__create_invoke_instruction_append_to_basic_block(llvm::Function* func, llvm::BasicBlock* normal_dest, llvm::BasicBlock* unwind_dest, core::List_sp args, core::String_sp label, llvm::BasicBlock* append_bb) {
  printf("%s:%d In create_invoke_instruction\n", __FILE__, __LINE__ );
  // Get the arguments
  vector<llvm::Value*> llvm_args;
  for ( auto cur : args ) {
    Value_sp arg = gc::As<Value_sp>(oCar(cur));
    llvm_args.push_back(arg->wrappedPtr());
  }
  llvm::InvokeInst* llvm_invoke = llvm::InvokeInst::Create(func,normal_dest,unwind_dest,llvm_args,label->get_std_string(),append_bb);
  Instruction_sp invoke = Instruction_O::create();
  invoke->set_wrapped(llvm_invoke);
  return invoke;
}

CL_DEFUN void llvm_sys__dump_instruction_pointers(llvm::Instruction* I)
{
  if (I == NULL) {
    printf("%s:%d  Instruction is NULL\n", __FILE__, __LINE__ );
  } else {
    auto prev = I->getPrevNode();
    auto next = I->getNextNode();
    const char* opcode = I->getOpcodeName();
    printf("%s:%d Instruction %15s @%p  prev@%p  next@%p\n", __FILE__, __LINE__, opcode, (void*)&I, (void*)prev, (void*)next);
  }
}

CL_DEFUN void llvm_sys__dump_instruction_list(BasicBlock_sp cbb)
{
  llvm::BasicBlock* bb = cbb->wrappedPtr();
  if (bb==NULL) {
    printf("%s:%d The instruction list is empty\n", __FILE__, __LINE__ );
    return;
  }
//  printf("%s:%d  bb->end()->getPrev() = @%p\n", __FILE__, __LINE__, bb->end()->getPrev());
  printf("%s:%d  &(bb->back())  = @%p\n", __FILE__, __LINE__, (void*)&(bb->back()));
  printf("%s:%d  Dumping instruction list\n", __FILE__, __LINE__);
  for ( auto &I : *bb ) {
    auto prev = I.getPrevNode();
    auto next = I.getNextNode();
    const char* opcode = I.getOpcodeName();
    printf("%s:%d Instruction %15s @%p  prev@%p  next@%p\n", __FILE__, __LINE__, opcode, (void*)&I, (void*)prev, (void*)next);
  }
}

CL_DEFUN void llvm_sys__sanity_check_module(Module_sp module, int depth)
{
  llvm::Module* modP = module->wrappedPtr();
  if (modP) {
    llvm::Module& mod = *modP;
    for ( auto &F : mod ) {
      if ( depth > 0 ) {
        for ( auto &B : F ) {
          if (depth > 1) {
            for ( auto &I : B )
              /*nothing*/;
          }
        }
      }
    }
  }
}
}; // llvmo

namespace llvmo {


CL_LISPIFY_NAME(createTargetMachine);
CL_EXTERN_DEFMETHOD(Target_O, &llvm::Target::createTargetMachine);


CL_DEFUN TargetPassConfig_sp llvm_sys__createPassConfig(TargetMachine_sp targetMachine, PassManagerBase_sp pmb) {
  llvm::TargetMachine* tm = targetMachine->wrappedPtr();
  llvm::LLVMTargetMachine* ltm = dynamic_cast<llvm::LLVMTargetMachine*>(tm);
  if (ltm==NULL) {
    SIMPLE_ERROR(BF("Could not get LLVMTargetMachine"));
  }
  llvm::TargetPassConfig* tpc = ltm->createPassConfig(*pmb->wrappedPtr());
  TargetPassConfig_sp tpcsp = gc::As_unsafe<TargetPassConfig_sp>(translate::to_object<llvm::TargetPassConfig*>::convert(tpc));
  return tpcsp;
}


CL_LISPIFY_NAME(setEnableTailMerge);
CL_EXTERN_DEFMETHOD(TargetPassConfig_O, &llvm::TargetPassConfig::setEnableTailMerge);

;

}; // llvmo


namespace llvmo {

CL_LISPIFY_NAME("addPassesToEmitFileAndRunPassManager");
CL_DEFMETHOD core::T_sp TargetMachine_O::addPassesToEmitFileAndRunPassManager(PassManager_sp passManager,
                                                                        core::T_sp stream,
                                                                        core::T_sp dwo_stream,
                                                                        llvm::TargetMachine::CodeGenFileType FileType,
                                                                        Module_sp module) {
  if (stream.nilp()) {
    SIMPLE_ERROR(BF("You must pass a valid stream"));
  }
  llvm::raw_pwrite_stream *ostreamP;
  llvm::SmallString<1024> stringOutput;
  bool stringOutputStream = false;
  if (core::StringOutputStream_sp sos = stream.asOrNull<core::StringOutputStream_O>()) {
    ostreamP = new llvm::raw_svector_ostream(stringOutput);
    stringOutputStream = true;
  } else if ( stream == kw::_sym_simple_vector_byte8 ) {
    (void)sos;
    ostreamP = new llvm::raw_svector_ostream(stringOutput);
  } else if (core::IOFileStream_sp fs = stream.asOrNull<core::IOFileStream_O>()) {
    ostreamP = new llvm::raw_fd_ostream(fs->fileDescriptor(), false, true);
  } else if (core::IOStreamStream_sp iostr = stream.asOrNull<core::IOStreamStream_O>()) {
    FILE *f = iostr->file();
    ostreamP = new llvm::raw_fd_ostream(fileno(f), false, true);
  } else {
    SIMPLE_ERROR(BF("Illegal file type %s for addPassesToEmitFileAndRunPassManager") % _rep_(stream));
  }
  llvm::SmallString<1024> dwo_stringOutput;
  llvm::raw_pwrite_stream *dwo_ostreamP;
  bool dwo_stringOutputStream = false;
  if (core::StringOutputStream_sp sos = dwo_stream.asOrNull<core::StringOutputStream_O>()) {
    (void)sos;
    dwo_ostreamP = new llvm::raw_svector_ostream(dwo_stringOutput);
    dwo_stringOutputStream = true;
  } else if (core::IOFileStream_sp fs = dwo_stream.asOrNull<core::IOFileStream_O>()) {
    dwo_ostreamP = new llvm::raw_fd_ostream(fs->fileDescriptor(), false, true);
  } else if (core::IOStreamStream_sp iostr = dwo_stream.asOrNull<core::IOStreamStream_O>()) {
    FILE *f = iostr->file();
    dwo_ostreamP = new llvm::raw_fd_ostream(fileno(f), false, true);
  } else if (dwo_stream.nilp()) {
    dwo_ostreamP = NULL;
  } else {
    SIMPLE_ERROR(BF("Illegal file type %s for addPassesToEmitFileAndRunPassManager") % _rep_(dwo_stream));
  }
  if (this->wrappedPtr()->addPassesToEmitFile(*passManager->wrappedPtr(), *ostreamP, dwo_ostreamP, FileType, true, nullptr)) {
    delete ostreamP;
    if (dwo_ostreamP) {
      delete dwo_ostreamP;
    }
    SIMPLE_ERROR(BF("Could not generate file type"));
  }
  passManager->wrappedPtr()->run(*module->wrappedPtr());
  if (core::StringOutputStream_sp sos = stream.asOrNull<core::StringOutputStream_O>()) {
    sos->fill(stringOutput.c_str());
  } else if (stream == kw::_sym_simple_vector_byte8) {
    SYMBOL_EXPORT_SC_(KeywordPkg,simple_vector_byte8);
    core::SimpleVector_byte8_t_sp vector_byte8 = core::SimpleVector_byte8_t_O::make(stringOutput.size(),0,false,stringOutput.size(),(const unsigned char*)stringOutput.data());
    if (dwo_stream.notnilp()) {
      SIMPLE_ERROR(BF("dwo_stream must be nil"));
    }
    return vector_byte8;
  }
  if (core::StringOutputStream_sp dwo_sos = dwo_stream.asOrNull<core::StringOutputStream_O>()) {
    dwo_sos->fill(dwo_stringOutput.c_str());
  }
  return _Nil<core::T_O>();
}

  CL_LISPIFY_NAME(createDataLayout);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::createDataLayout);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::setFastISel);
  CL_LISPIFY_NAME(getSubtargetImpl);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, (const llvm::TargetSubtargetInfo *(llvm::TargetMachine::*)() const) & llvm::TargetMachine::getSubtargetImpl);
  CL_LISPIFY_NAME(addPassesToEmitFileAndRunPassManager);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, &TargetMachine_O::addPassesToEmitFileAndRunPassManager);
CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::setFastISel);

  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType_Null);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType_AssemblyFile);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType_ObjectFile);
  CL_BEGIN_ENUM(llvm::TargetMachine::CodeGenFileType,_sym_CodeGenFileType, "CodeGenFileType");
  CL_VALUE_ENUM(_sym_CodeGenFileType_Null, llvm::TargetMachine::CGFT_Null);
  CL_VALUE_ENUM(_sym_CodeGenFileType_AssemblyFile, llvm::TargetMachine::CGFT_AssemblyFile);
  CL_VALUE_ENUM(_sym_CodeGenFileType_ObjectFile, llvm::TargetMachine::CGFT_ObjectFile);;
  CL_END_ENUM(_sym_CodeGenFileType);

  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_None);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_Less);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_Default);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_Aggressive);
  CL_BEGIN_ENUM(llvm::CodeGenOpt::Level,_sym_CodeGenOpt, "CodeGenOpt");
  CL_VALUE_ENUM(_sym_CodeGenOpt_None, llvm::CodeGenOpt::None);
  CL_VALUE_ENUM(_sym_CodeGenOpt_Less, llvm::CodeGenOpt::Less);
  CL_VALUE_ENUM(_sym_CodeGenOpt_Default, llvm::CodeGenOpt::Default);
  CL_VALUE_ENUM(_sym_CodeGenOpt_Aggressive, llvm::CodeGenOpt::Aggressive);;
  CL_END_ENUM(_sym_CodeGenOpt);

  SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_undefined);
  SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_Static);
  SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_PIC_);
  SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_DynamicNoPIC);
  CL_BEGIN_ENUM(llvm::Reloc::Model,_sym_RelocModel, "RelocModel");
  CL_VALUE_ENUM(_sym_RelocModel_Static, llvm::Reloc::Model::Static);
  CL_VALUE_ENUM(_sym_RelocModel_PIC_, llvm::Reloc::Model::PIC_);
  CL_VALUE_ENUM(_sym_RelocModel_DynamicNoPIC, llvm::Reloc::Model::DynamicNoPIC);;
  CL_END_ENUM(_sym_RelocModel);

  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Small);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Kernel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Medium);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Large);
  CL_BEGIN_ENUM(llvm::CodeModel::Model,_sym_CodeModel, "CodeModel");
  CL_VALUE_ENUM(_sym_CodeModel_Small, llvm::CodeModel::Small);
  CL_VALUE_ENUM(_sym_CodeModel_Kernel, llvm::CodeModel::Kernel);
  CL_VALUE_ENUM(_sym_CodeModel_Medium, llvm::CodeModel::Medium);
  CL_VALUE_ENUM(_sym_CodeModel_Large, llvm::CodeModel::Large);;
  CL_END_ENUM(_sym_CodeModel);


}; // llvmo

namespace llvmo {




CL_LAMBDA(triple-str);
CL_DECLARE();
CL_DOCSTRING("");
CL_PKG_NAME(LlvmoPkg,"make-triple");
CL_DEFUN Triple_sp Triple_O::make(const string &triple) {
  GC_ALLOCATE(Triple_O, self);
  self->_ptr = new llvm::Triple(triple);
  return self;
};

CL_PKG_NAME(LlvmoPkg,"triple-normalize");
CL_EXTERN_DEFUN(( std::string(*)(llvm::StringRef str))&llvm::Triple::normalize);

  CL_LISPIFY_NAME(getTriple);
  CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getTriple);
  CL_LISPIFY_NAME(getArchName);
  CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getArchName);
  CL_LISPIFY_NAME(getVendorName);
  CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getVendorName);
  CL_LISPIFY_NAME(getOSName);
  CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getOSName);
  CL_LISPIFY_NAME(getEnvironmentName);
  CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getEnvironmentName);
  CL_LISPIFY_NAME(getOSAndEnvironmentName);
  CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getOSAndEnvironmentName);

  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_UnknownArch);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_arm);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_armeb);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_aarch64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_aarch64_be);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_hexagon);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_mips);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_mipsel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_mips64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_mips64el);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_msp430);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_ppc);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_ppc64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_ppc64le);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_r600);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_sparc);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_sparcv9);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_systemz);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_tce);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_thumb);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_thumbeb);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_x86);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_x86_64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_xcore);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_nvptx);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_nvptx64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_le32);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_le64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_amdil);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_amdil64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_hsail);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_hsail64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_spir);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_spir64);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ArchType_kalimba);
  CL_BEGIN_ENUM(llvm::Triple::ArchType,_sym_ArchType, "ArchType");
  CL_VALUE_ENUM(_sym_ArchType_UnknownArch, llvm::Triple::UnknownArch);
  CL_VALUE_ENUM(_sym_ArchType_arm, llvm::Triple::arm);
  CL_VALUE_ENUM(_sym_ArchType_armeb, llvm::Triple::armeb);       // ARM (big endian): armeb
  CL_VALUE_ENUM(_sym_ArchType_aarch64, llvm::Triple::aarch64);   // AArch64 (little endian):     aarch64_be, // AArch64 (big endian): aarch64_be
  CL_VALUE_ENUM(_sym_ArchType_hexagon, llvm::Triple::hexagon);   // Hexagon: hexagon
  CL_VALUE_ENUM(_sym_ArchType_mips, llvm::Triple::mips);         // MIPS: mips, mipsallegrex
  CL_VALUE_ENUM(_sym_ArchType_mipsel, llvm::Triple::mipsel);     // MIPSEL: mipsel, mipsallegrexel
  CL_VALUE_ENUM(_sym_ArchType_mips64, llvm::Triple::mips64);     // MIPS64: mips64
  CL_VALUE_ENUM(_sym_ArchType_mips64el, llvm::Triple::mips64el); // MIPS64EL: mips64el
  CL_VALUE_ENUM(_sym_ArchType_msp430, llvm::Triple::msp430);     // MSP430: msp430
  CL_VALUE_ENUM(_sym_ArchType_ppc, llvm::Triple::ppc);           // PPC: powerpc
  CL_VALUE_ENUM(_sym_ArchType_ppc64, llvm::Triple::ppc64);       // PPC64: powerpc64, ppu
  CL_VALUE_ENUM(_sym_ArchType_ppc64le, llvm::Triple::ppc64le);   // PPC64LE: powerpc64le
  CL_VALUE_ENUM(_sym_ArchType_r600, llvm::Triple::r600);         // R600: AMD GPUs HD2XXX - HD6XXX
  CL_VALUE_ENUM(_sym_ArchType_sparc, llvm::Triple::sparc);       // Sparc: sparc
  CL_VALUE_ENUM(_sym_ArchType_sparcv9, llvm::Triple::sparcv9);   // Sparcv9: Sparcv9
  CL_VALUE_ENUM(_sym_ArchType_systemz, llvm::Triple::systemz);   // SystemZ: s390x
  CL_VALUE_ENUM(_sym_ArchType_tce, llvm::Triple::tce);           // TCE (http://tce.cs.tut.fi/): tce
  CL_VALUE_ENUM(_sym_ArchType_thumb, llvm::Triple::thumb);       // Thumb (little endian): thumb, thumbv.*
  CL_VALUE_ENUM(_sym_ArchType_thumbeb, llvm::Triple::thumbeb);   // Thumb (big endian): thumbeb
  CL_VALUE_ENUM(_sym_ArchType_x86, llvm::Triple::x86);           // X86: i[3-9]86
  CL_VALUE_ENUM(_sym_ArchType_x86_64, llvm::Triple::x86_64);     // X86-64: amd64, x86_64
  CL_VALUE_ENUM(_sym_ArchType_xcore, llvm::Triple::xcore);       // XCore: xcore
  CL_VALUE_ENUM(_sym_ArchType_nvptx, llvm::Triple::nvptx);       // NVPTX: 32-bit
  CL_VALUE_ENUM(_sym_ArchType_nvptx64, llvm::Triple::nvptx64);   // NVPTX: 64-bit
  CL_VALUE_ENUM(_sym_ArchType_le32, llvm::Triple::le32);         // le32: generic little-endian 32-bit CPU (PNaCl / Emscripten)
  CL_VALUE_ENUM(_sym_ArchType_le64, llvm::Triple::le64);         // le64: generic little-endian 64-bit CPU (PNaCl / Emscripten)
  CL_VALUE_ENUM(_sym_ArchType_amdil, llvm::Triple::amdil);       // AMDIL
  CL_VALUE_ENUM(_sym_ArchType_amdil64, llvm::Triple::amdil64);   // AMDIL with 64-bit pointers
  CL_VALUE_ENUM(_sym_ArchType_hsail, llvm::Triple::hsail);       // AMD HSAIL
  CL_VALUE_ENUM(_sym_ArchType_hsail64, llvm::Triple::hsail64);   // AMD HSAIL with 64-bit pointers
  CL_VALUE_ENUM(_sym_ArchType_spir, llvm::Triple::spir);         // SPIR: standard portable IR for OpenCL 32-bit version
  CL_VALUE_ENUM(_sym_ArchType_spir64, llvm::Triple::spir64);     // SPIR: standard portable IR for OpenCL 64-bit version
  CL_VALUE_ENUM(_sym_ArchType_kalimba, llvm::Triple::kalimba);   // Kalimba: generic kalimba
  CL_END_ENUM(_sym_ArchType);

  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_NoSubArch);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v8);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v7);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v7em);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v7m);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v7s);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v6);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v6m);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v6t2);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v5);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v5te);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_ARMSubArch_v4t);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_KalimbaSubArch_v3);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_KalimbaSubArch_v4);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType_KalimbaSubArch_v5);

  SYMBOL_EXPORT_SC_(LlvmoPkg, SubArchType);
  CL_BEGIN_ENUM(llvm::Triple::SubArchType,_sym_SubArchType, "SubArchType");
  CL_VALUE_ENUM(_sym_SubArchType_NoSubArch, llvm::Triple::NoSubArch);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v8, llvm::Triple::ARMSubArch_v8);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v7, llvm::Triple::ARMSubArch_v7);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v7em, llvm::Triple::ARMSubArch_v7em);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v7m, llvm::Triple::ARMSubArch_v7m);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v7s, llvm::Triple::ARMSubArch_v7s);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v6, llvm::Triple::ARMSubArch_v6);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v6m, llvm::Triple::ARMSubArch_v6m);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v6t2, llvm::Triple::ARMSubArch_v6t2);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v5, llvm::Triple::ARMSubArch_v5);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v5te, llvm::Triple::ARMSubArch_v5te);
  CL_VALUE_ENUM(_sym_SubArchType_ARMSubArch_v4t, llvm::Triple::ARMSubArch_v4t);
  CL_VALUE_ENUM(_sym_SubArchType_KalimbaSubArch_v3, llvm::Triple::KalimbaSubArch_v3);
  CL_VALUE_ENUM(_sym_SubArchType_KalimbaSubArch_v4, llvm::Triple::KalimbaSubArch_v4);
  CL_VALUE_ENUM(_sym_SubArchType_KalimbaSubArch_v5, llvm::Triple::KalimbaSubArch_v5);;
  CL_END_ENUM(_sym_SubArchType);

  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_UnknownVendor);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_Apple);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_PC);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_SCEI);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_BGP);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_BGQ);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_Freescale);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_IBM);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_ImaginationTechnologies);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_MipsTechnologies);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_NVIDIA);
  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_CSR);

  SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType);
  CL_BEGIN_ENUM(llvm::Triple::VendorType,_sym_VendorType, "VendorType");
  CL_VALUE_ENUM(_sym_VendorType_UnknownVendor, llvm::Triple::UnknownVendor);
  CL_VALUE_ENUM(_sym_VendorType_Apple, llvm::Triple::Apple);
  CL_VALUE_ENUM(_sym_VendorType_PC, llvm::Triple::PC);
  CL_VALUE_ENUM(_sym_VendorType_SCEI, llvm::Triple::SCEI);
  CL_VALUE_ENUM(_sym_VendorType_BGP, llvm::Triple::BGP);
  CL_VALUE_ENUM(_sym_VendorType_BGQ, llvm::Triple::BGQ);
  CL_VALUE_ENUM(_sym_VendorType_Freescale, llvm::Triple::Freescale);
  CL_VALUE_ENUM(_sym_VendorType_IBM, llvm::Triple::IBM);
  CL_VALUE_ENUM(_sym_VendorType_ImaginationTechnologies, llvm::Triple::ImaginationTechnologies);
  CL_VALUE_ENUM(_sym_VendorType_MipsTechnologies, llvm::Triple::MipsTechnologies);
  CL_VALUE_ENUM(_sym_VendorType_NVIDIA, llvm::Triple::NVIDIA);
  CL_VALUE_ENUM(_sym_VendorType_CSR, llvm::Triple::CSR);
  CL_END_ENUM(_sym_VendorType);

  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_UnknownOS);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Darwin);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_DragonFly);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_FreeBSD);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_IOS);
//  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_KFreeBSD);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Linux);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Lv2);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_MacOSX);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_NetBSD);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_OpenBSD);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Solaris);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Win32);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Haiku);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Minix);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_RTEMS);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_NaCl);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_CNK);
//  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Bitrig);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_AIX);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_CUDA);
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_NVCL);

  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType);
  CL_BEGIN_ENUM(llvm::Triple::OSType,_sym_OSType, "OSType");
  CL_VALUE_ENUM(_sym_OSType_UnknownOS, llvm::Triple::UnknownOS);
  CL_VALUE_ENUM(_sym_OSType_Darwin, llvm::Triple::Darwin);
  CL_VALUE_ENUM(_sym_OSType_DragonFly, llvm::Triple::DragonFly);
  CL_VALUE_ENUM(_sym_OSType_FreeBSD, llvm::Triple::FreeBSD);
  CL_VALUE_ENUM(_sym_OSType_IOS, llvm::Triple::IOS);
//  CL_VALUE_ENUM(_sym_OSType_KFreeBSD, llvm::Triple::KFreeBSD);
  CL_VALUE_ENUM(_sym_OSType_Linux, llvm::Triple::Linux);
  CL_VALUE_ENUM(_sym_OSType_Lv2, llvm::Triple::Lv2);
  CL_VALUE_ENUM(_sym_OSType_MacOSX, llvm::Triple::MacOSX);
  CL_VALUE_ENUM(_sym_OSType_NetBSD, llvm::Triple::NetBSD);
  CL_VALUE_ENUM(_sym_OSType_OpenBSD, llvm::Triple::OpenBSD);
  CL_VALUE_ENUM(_sym_OSType_Solaris, llvm::Triple::Solaris);
  CL_VALUE_ENUM(_sym_OSType_Win32, llvm::Triple::Win32);
  CL_VALUE_ENUM(_sym_OSType_Haiku, llvm::Triple::Haiku);
  CL_VALUE_ENUM(_sym_OSType_Minix, llvm::Triple::Minix);
  CL_VALUE_ENUM(_sym_OSType_RTEMS, llvm::Triple::RTEMS);
  CL_VALUE_ENUM(_sym_OSType_NaCl, llvm::Triple::NaCl);
  CL_VALUE_ENUM(_sym_OSType_CNK, llvm::Triple::CNK);
//  CL_VALUE_ENUM(_sym_OSType_Bitrig, llvm::Triple::Bitrig);
  CL_VALUE_ENUM(_sym_OSType_AIX, llvm::Triple::AIX);
  CL_VALUE_ENUM(_sym_OSType_CUDA, llvm::Triple::CUDA);
  CL_VALUE_ENUM(_sym_OSType_NVCL, llvm::Triple::NVCL);;
  CL_END_ENUM(_sym_OSType);

  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_UnknownEnvironment);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_GNU);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_GNUEABI);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_GNUEABIHF);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_GNUX32);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_CODE16);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_EABI);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_EABIHF);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_Android);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_MSVC);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_Itanium);
  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType_Cygnus);

  SYMBOL_EXPORT_SC_(LlvmoPkg, EnvironmentType);
  CL_BEGIN_ENUM(llvm::Triple::EnvironmentType,_sym_EnvironmentType, "EnvironmentType");
  CL_VALUE_ENUM(_sym_EnvironmentType_UnknownEnvironment, llvm::Triple::UnknownEnvironment);
  CL_VALUE_ENUM(_sym_EnvironmentType_GNU, llvm::Triple::GNU);
  CL_VALUE_ENUM(_sym_EnvironmentType_GNUEABI, llvm::Triple::GNUEABI);
  CL_VALUE_ENUM(_sym_EnvironmentType_GNUEABIHF, llvm::Triple::GNUEABIHF);
  CL_VALUE_ENUM(_sym_EnvironmentType_GNUX32, llvm::Triple::GNUX32);
  CL_VALUE_ENUM(_sym_EnvironmentType_CODE16, llvm::Triple::CODE16);
  CL_VALUE_ENUM(_sym_EnvironmentType_EABI, llvm::Triple::EABI);
  CL_VALUE_ENUM(_sym_EnvironmentType_EABIHF, llvm::Triple::EABIHF);
  CL_VALUE_ENUM(_sym_EnvironmentType_Android, llvm::Triple::Android);
  CL_VALUE_ENUM(_sym_EnvironmentType_MSVC, llvm::Triple::MSVC);
  CL_VALUE_ENUM(_sym_EnvironmentType_Itanium, llvm::Triple::Itanium);
  CL_VALUE_ENUM(_sym_EnvironmentType_Cygnus, llvm::Triple::Cygnus);
  CL_END_ENUM(_sym_EnvironmentType);

  SYMBOL_EXPORT_SC_(LlvmoPkg, ObjectFormatType_UnknownObjectFormat);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ObjectFormatType_COFF);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ObjectFormatType_ELF);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ObjectFormatType_MachO);

  SYMBOL_EXPORT_SC_(LlvmoPkg, ObjectFormatType);
  CL_BEGIN_ENUM(llvm::Triple::ObjectFormatType,_sym_ObjectFormatType, "ObjectFormatType");
  CL_VALUE_ENUM(_sym_ObjectFormatType_UnknownObjectFormat, llvm::Triple::UnknownObjectFormat);
  CL_VALUE_ENUM(_sym_ObjectFormatType_COFF, llvm::Triple::COFF);
  CL_VALUE_ENUM(_sym_ObjectFormatType_ELF, llvm::Triple::ELF);
  CL_VALUE_ENUM(_sym_ObjectFormatType_MachO, llvm::Triple::MachO);;
  CL_END_ENUM(_sym_ObjectFormatType);


}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(make-target-options);
CL_DEFUN TargetOptions_sp TargetOptions_O::make() {
  GC_ALLOCATE(TargetOptions_O, self);
  self->_ptr = new llvm::TargetOptions();
  return self;
};



}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME("LLVMTargetMachine_addPassesToEmitFile");
CL_DEFMETHOD bool LLVMTargetMachine_O::LLVMTargetMachine_addPassesToEmitFile(PassManagerBase_sp pm,
                                                                core::T_sp stream,
                                                                core::Symbol_sp fileType) {
  IMPLEMENT_ME();
}



;

}; // llvmo

namespace llvmo {
Value_sp Value_O::create(llvm::Value *ptr) {
  return core::RP_Create_wrapped<Value_O, llvm::Value *>(ptr);
};
}

namespace llvmo {
  CL_LISPIFY_NAME(getName);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::getName);
  CL_LISPIFY_NAME(setName);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::setName);
  CL_LISPIFY_NAME(getType);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::getType);;
  CL_LISPIFY_NAME(replaceAllUsesWith);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::replaceAllUsesWith);;

}; // llvmo


namespace llvmo {
  CL_LISPIFY_NAME(MetadataAsValue-get);
  CL_EXTERN_DEFUN(&llvm::MetadataAsValue::get);
  CL_LISPIFY_NAME(MetadataAsValue-getIfExists);
  CL_EXTERN_DEFUN(&llvm::MetadataAsValue::getIfExists);

}; // llvmo


namespace llvmo {
  CL_LISPIFY_NAME(ValueAsMetadata-get);
  CL_EXTERN_DEFUN(&llvm::ValueAsMetadata::get);
}; // llvmo


namespace llvmo {


CL_DEFUN core::T_mv llvm_sys__verifyModule(Module_sp module, core::Symbol_sp action) {
  string errorInfo;
  llvm::raw_string_ostream ei(errorInfo);
  llvm::Module *m = module->wrappedPtr();
  bool result = llvm::verifyModule(*m, &ei);
  return Values(_lisp->_boolean(result), core::SimpleBaseString_O::make(ei.str()));
};

CL_DEFUN core::T_mv llvm_sys__verifyFunction(Function_sp function) {
  llvm::Function *f = function->wrappedPtr();
  string errorInfo;
  llvm::raw_string_ostream ei(errorInfo);
  bool result = llvm::verifyFunction(*f, &ei);
  return Values(_lisp->_boolean(result), core::SimpleBaseString_O::make(ei.str()));
};

CL_DEFUN void llvm_sys__printFunctionToStream(Function_sp func, core::T_sp stream) {
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  llvm::AssemblyAnnotationWriter *aaw = new llvm::AssemblyAnnotationWriter();
  func->wrappedPtr()->print(sout, aaw);
  delete aaw;
  core::clasp_write_string(outstr,stream);
}

CL_DEFUN void llvm_sys__printModuleToStream(Module_sp module, core::T_sp stream) {
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  llvm::AssemblyAnnotationWriter *aaw = new llvm::AssemblyAnnotationWriter();
  module->wrappedPtr()->print(sout, aaw);
  delete aaw;
  core::clasp_write_string(outstr,stream);
}

CL_DEFUN void llvm_sys__writeIrToFile(Module_sp module, core::String_sp path) {
  std::error_code errcode;
  string pathName = path->get_std_string();
  llvm::raw_fd_ostream OS(pathName.c_str(), errcode, ::llvm::sys::fs::OpenFlags::F_None);
  if (errcode) {
    SIMPLE_ERROR(BF("Could not write bitcode to %s - problem: %s") % pathName % errcode.message());
  }
  llvm::AssemblyAnnotationWriter *aaw = new llvm::AssemblyAnnotationWriter();
  module->wrappedPtr()->print(OS, aaw);
  delete aaw;
}

CL_LAMBDA(module pathname &optional (use-thin-lto t));
CL_DEFUN void llvm_sys__writeBitcodeToFile(Module_sp module, core::String_sp pathname, bool useThinLTO) {
  string pn = pathname->get_std_string();
  std::error_code errcode;
  llvm::raw_fd_ostream OS(pn.c_str(), errcode, ::llvm::sys::fs::OpenFlags::F_None);
  if (errcode) {
    SIMPLE_ERROR(BF("Could not write bitcode to file[%s] - error: %s") % pn % errcode.message());
  }
  if (useThinLTO) {
    llvm::ProfileSummaryInfo PSI(*module->wrappedPtr());
    auto Index = llvm::buildModuleSummaryIndex(*(module->wrappedPtr()),NULL,&PSI);
    llvm::WriteBitcodeToFile(*module->wrappedPtr(), OS,false, &Index,true);
  } else {
    llvm::WriteBitcodeToFile(*module->wrappedPtr(),OS);
  }
};


CL_DEFUN Module_sp llvm_sys__parseIRFile(core::T_sp tfilename, LLVMContext_sp context) {
  if (tfilename.nilp()) SIMPLE_ERROR(BF("%s was about to pass nil to pathname") % __FUNCTION__);
  core::String_sp spathname = gc::As<core::String_sp>(core::cl__namestring(core::cl__pathname(tfilename)));
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getFile(spathname->get_std_string());
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR(BF("Could not read the file %s - error: %s") % spathname->get_std_string() % ec.message());
  }
  llvm::SMDiagnostic smd;
  std::unique_ptr<llvm::Module> module =
    llvm::parseIR(eo_membuf.get()->getMemBufferRef(), smd, *(context->wrappedPtr()));
  llvm::Module* m = module.release();
  if (!m) {
    std::string message = smd.getMessage();
    SIMPLE_ERROR(BF("Could not load bitcode for file %s - error: %s") % spathname->get_std_string() % message );
  }
  Module_sp omodule = core::RP_Create_wrapped<Module_O,llvm::Module*>(m);
  return omodule;
};

CL_LAMBDA("filename context");
CL_DEFUN Module_sp llvm_sys__parseBitcodeFile(core::T_sp tfilename, LLVMContext_sp context) {
  if (tfilename.nilp()) SIMPLE_ERROR(BF("%s was about to pass nil to pathname") % __FUNCTION__);
  core::String_sp spathname = gc::As<core::String_sp>(core::cl__namestring(core::cl__pathname(tfilename)));
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getFile(spathname->get_std_string());
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR(BF("Could not load bitcode for file %s - error: %s") % spathname->get_std_string() % ec.message());
  }
  llvm::Expected<std::unique_ptr<llvm::Module>> eom =
    llvm::parseBitcodeFile(eo_membuf.get()->getMemBufferRef(), *(context->wrappedPtr()));
  if (!eom) SIMPLE_ERROR(BF("Could not parse bitcode for file %s - there was an error") % spathname->get_std_string());
  Module_sp omodule = core::RP_Create_wrapped<Module_O, llvm::Module *>((*eom).release());
  return omodule;
};


CL_DEFUN Module_sp llvm_sys__clone_module(Module_sp original)
{
  llvm::Module* o = original->wrappedPtr();
  std::unique_ptr<llvm::Module> m = llvm::CloneModule(*o);
  Module_sp module = core::RP_Create_wrapped<Module_O,llvm::Module*>(m.release());
  return module;
}



  CL_DEFUN bool llvm_sys__valuep(core::T_sp arg) {
    return gc::IsA<Value_sp>(arg);
  };

#if 0
    // Jan 31, 2013 - the Attribute/Argument api is changing fast and I'm not using it right now
    // and I don't want to mess with this code until it settles down
  Attribute_sp Attribute_O::get(LLVMContext_sp context, core::Cons_sp attribute_symbols)
  {
    llvm::AttrBuilder attrBuilder;
    core::SymbolToEnumConverter_sp converter = _sym_AttributeEnum->symbolValue().as<core::SymbolToEnumConverter_O>();
    for ( core::Cons_sp cur=attribute_symbols;cur->notNil(); cur=cur->cdr() )
    {
      core::Symbol_sp sym = cur->ocar().as<core::Symbol_O>();
      llvm::Attribute::AttrKind e = converter->enumForSymbol<llvm::Attribute::AttrKind>(sym);
      attrBuilder.addAttribute(e);
    }
    llvm::Attribute at = llvm::Attribute::get(*(context->wrappedPtr()),attrBuilder);
    return translate::to_object<llvm::Attribute>::convert(at).as<Attribute_O>();
  }
#endif



  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNone);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeZExt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeSExt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoReturn);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeInReg);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeStructRet);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoUnwind);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoAlias);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeByVal);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNest);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeReadNone);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeReadOnly);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoInline);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeAlwaysInline);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeOptimizeForSize);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeStackProtect);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeStackProtectReq);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeAlignment);

  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoCapture);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoRedZone);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNoImplicitFloat);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNaked);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeInlineHint);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeStackAlignment);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeReturnsTwice);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeUWTable);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeNonLazyBind);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeAddressSafety);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeEnum);
  CL_BEGIN_ENUM(llvm::Attribute::AttrKind,_sym_AttributeEnum, "Attribute");
  CL_VALUE_ENUM(_sym_AttributeNone, llvm::Attribute::None);
  CL_VALUE_ENUM(_sym_AttributeZExt, llvm::Attribute::ZExt);
  CL_VALUE_ENUM(_sym_AttributeSExt, llvm::Attribute::SExt);
  CL_VALUE_ENUM(_sym_AttributeNoReturn, llvm::Attribute::NoReturn);
  CL_VALUE_ENUM(_sym_AttributeInReg, llvm::Attribute::InReg);
  CL_VALUE_ENUM(_sym_AttributeStructRet, llvm::Attribute::StructRet);
  CL_VALUE_ENUM(_sym_AttributeNoUnwind, llvm::Attribute::NoUnwind);
  CL_VALUE_ENUM(_sym_AttributeNoAlias, llvm::Attribute::NoAlias);
  CL_VALUE_ENUM(_sym_AttributeByVal, llvm::Attribute::ByVal);
  CL_VALUE_ENUM(_sym_AttributeNest, llvm::Attribute::Nest);
  CL_VALUE_ENUM(_sym_AttributeReadNone, llvm::Attribute::ReadNone);
  CL_VALUE_ENUM(_sym_AttributeReadOnly, llvm::Attribute::ReadOnly);
  CL_VALUE_ENUM(_sym_AttributeNoInline, llvm::Attribute::NoInline);
  CL_VALUE_ENUM(_sym_AttributeAlwaysInline, llvm::Attribute::AlwaysInline);
  CL_VALUE_ENUM(_sym_AttributeOptimizeForSize, llvm::Attribute::OptimizeForSize);
  CL_VALUE_ENUM(_sym_AttributeStackProtect, llvm::Attribute::StackProtect);
  CL_VALUE_ENUM(_sym_AttributeStackProtectReq, llvm::Attribute::StackProtectReq);
  CL_VALUE_ENUM(_sym_AttributeAlignment, llvm::Attribute::Alignment);
  CL_VALUE_ENUM(_sym_AttributeNoCapture, llvm::Attribute::NoCapture);
  CL_VALUE_ENUM(_sym_AttributeNoRedZone, llvm::Attribute::NoRedZone);
  CL_VALUE_ENUM(_sym_AttributeNoImplicitFloat, llvm::Attribute::NoImplicitFloat);
  CL_VALUE_ENUM(_sym_AttributeNaked, llvm::Attribute::Naked);
  CL_VALUE_ENUM(_sym_AttributeInlineHint, llvm::Attribute::InlineHint);
  CL_VALUE_ENUM(_sym_AttributeStackAlignment, llvm::Attribute::StackAlignment);
  CL_VALUE_ENUM(_sym_AttributeReturnsTwice, llvm::Attribute::ReturnsTwice);
  CL_VALUE_ENUM(_sym_AttributeUWTable, llvm::Attribute::UWTable);
  CL_VALUE_ENUM(_sym_AttributeNonLazyBind, llvm::Attribute::NonLazyBind);
      //	    .value(_sym_AttributeAddressSafety,llvm::Attribute::AddressSafety)
  CL_END_ENUM(_sym_AttributeEnum);
  SYMBOL_EXPORT_SC_(LlvmoPkg, attributesGet);




CL_LAMBDA(module value &optional label);
CL_DEFUN Value_sp llvm_sys__makeStringGlobal(Module_sp module, core::String_sp svalue, core::T_sp label) {
    llvm::Module &M = *(module->wrappedPtr());
    llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(M.getContext(), svalue->get_std_string());
    llvm::GlobalVariable *GV = new llvm::GlobalVariable(M, StrConstant->getType(),
                                                        true, llvm::GlobalValue::InternalLinkage,
                                                        StrConstant);
    if (label.nilp()) {
      GV->setName(":::str");
    } else {
      GV->setName(gctools::As<core::String_sp>(label)->get_std_string());
    }
    GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    return gc::As<Value_sp>(translate::to_object<llvm::Value *>::convert(GV));
  }

// Define Value_O::__repr__ which is prototyped in llvmoExpose.lisp
  string Value_O::__repr__() const {
    stringstream ss;
    ss << "#<" << this->_instanceClass()->_classNameAsString() << "@" << (void*)this->wrappedPtr() << " ";
    string str;
    llvm::raw_string_ostream ro(str);
    if (this->wrappedPtr() == 0) {
      ss << "wrappedPtr()==0";
    } else {
      try {
        this->wrappedPtr()->print(ro);
        ss << ro.str();
      } catch (...) {
        ss << "COULD_NOT__REPR__LLVM::Value";
      }
    }
    ss << ">";
    return ss.str();
  }

  bool Value_O::valid() const {
    return this->wrappedPtr() != NULL;
  }

  CL_DEFUN bool llvm_sys__valid(core::T_sp value) {
  // nil is a valid
    if (value.nilp())
      return true;
    else if (Value_sp val = gc::As<Value_sp>(value)) {
      return val->valid();
    }
    SIMPLE_ERROR(BF("Illegal argument for VALID: %s") % _rep_(value));
  }
};

namespace llvmo {

void convert_sequence_types_to_vector(core::T_sp elements, vector<llvm::Type *> &velements) {
  core::T_sp save_elements = elements;
  if (elements.nilp()) {
    return;
  } else if (core::Cons_sp celements = elements.asOrNull<core::Cons_O>()) {
    core::List_sp lelements = celements;
    for (auto cur : lelements) {
      velements.push_back(gc::As<Type_sp>(oCar(cur))->wrappedPtr());
    }
    return;
  } else if (core::Vector_sp vecelements = elements.asOrNull<core::Vector_O>()) {
    for (int i = 0; i < vecelements->length(); i++) {
      core::T_sp element = vecelements->rowMajorAref(i);
      Type_sp ty = gc::As<Type_sp>(element);
      velements.push_back(ty->wrappedPtr());
    }
    return;
  }
  QERROR_WRONG_TYPE_NTH_ARG(0, elements, ::cl::_sym_sequence);
}
}

namespace llvmo {
CL_PKG_NAME(LlvmoPkg,"make-Module");
CL_LAMBDA(module-name context);
CL_DEFUN Module_sp Module_O::make(llvm::StringRef module_name, LLVMContext_sp context) {
  GC_ALLOCATE(Module_O, self);
  self->_ptr = new llvm::Module(module_name, *(context->wrappedPtr()));
  return self;
};

std::string Module_O::__repr__() const {
  stringstream ss;
  ss << "#<MODULE ";
  ss << (void*)this->wrappedPtr() << ">";
  return ss.str();
}

CL_DEFUN core::List_sp llvm_sys__module_get_function_list(Module_sp module) {
  ql::list fl;
  for (llvm::Function &f : *module->wrappedPtr()) {
    Function_sp wrapped_func = gc::As<Function_sp>(translate::to_object<const llvm::Function &>::convert(f));
    fl << wrapped_func;
  }
  return fl.cons();
};
};

namespace llvmo {
  CL_LISPIFY_NAME(addModuleFlag);
  CL_EXTERN_DEFMETHOD(Module_O, (void (llvm::Module::*)(llvm::MDNode *))&llvm::Module::addModuleFlag);
  CL_LISPIFY_NAME(getModuleIdentifier);
  CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getModuleIdentifier);
  CL_LISPIFY_NAME(getGlobalVariable);
  CL_EXTERN_DEFMETHOD(Module_O, (llvm::GlobalVariable *(llvm::Module::*)(llvm::StringRef, bool)) &llvm::Module::getGlobalVariable);
  CL_LISPIFY_NAME(getNamedGlobal);
  CL_EXTERN_DEFMETHOD(Module_O, (llvm::GlobalVariable *(llvm::Module::*)(llvm::StringRef))&llvm::Module::getNamedGlobal);
  CL_LISPIFY_NAME(getOrInsertFunction);
CL_EXTERN_DEFMETHOD(Module_O, (llvm::FunctionCallee(llvm::Module::*)(llvm::StringRef,llvm::FunctionType*))&llvm::Module::getOrInsertFunction);
  CL_LISPIFY_NAME(getOrInsertGlobal);
  CL_EXTERN_DEFMETHOD(Module_O, (llvm::Constant*(llvm::Module::*)(llvm::StringRef,llvm::Type*))&llvm::Module::getOrInsertGlobal);
  CL_LISPIFY_NAME(getDataLayoutStr);
  CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getDataLayoutStr);
  CL_LISPIFY_NAME(getTargetTriple);
  CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getTargetTriple);
  CL_LISPIFY_NAME(setDataLayout);
  CL_EXTERN_DEFMETHOD(Module_O, (void (llvm::Module::*)(const llvm::DataLayout& )) & llvm::Module::setDataLayout);;
  CL_LISPIFY_NAME(setDataLayout.string);
  CL_EXTERN_DEFMETHOD(Module_O, (void (llvm::Module::*)(llvm::StringRef )) & llvm::Module::setDataLayout);;
  CL_EXTERN_DEFMETHOD(Module_O,&llvm::Module::setTargetTriple);

  SYMBOL_EXPORT_SC_(LlvmoPkg, verifyModule);
//  Defun(verifyModule);

  SYMBOL_EXPORT_SC_(LlvmoPkg, module_get_function_list);
//  Llvmo_temp_Defun(module_get_function_list);

  SYMBOL_EXPORT_SC_(LlvmoPkg, STARmoduleModFlagBehaviorSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, moduleFlagError);
  SYMBOL_EXPORT_SC_(LlvmoPkg, moduleFlagWarning);
  SYMBOL_EXPORT_SC_(LlvmoPkg, moduleFlagRequire);
  SYMBOL_EXPORT_SC_(LlvmoPkg, moduleFlagOverride);
  SYMBOL_EXPORT_SC_(LlvmoPkg, moduleFlagAppend);
  SYMBOL_EXPORT_SC_(LlvmoPkg, moduleFlagAppendUnique);
  CL_BEGIN_ENUM(llvm::Module::ModFlagBehavior,_sym_STARmoduleModFlagBehaviorSTAR, "llvm::Module::ModFlagBehavior");
  CL_VALUE_ENUM(_sym_moduleFlagError, llvm::Module::Error);
  CL_VALUE_ENUM(_sym_moduleFlagWarning, llvm::Module::Warning);
  CL_VALUE_ENUM(_sym_moduleFlagRequire, llvm::Module::Require);
  CL_VALUE_ENUM(_sym_moduleFlagOverride, llvm::Module::Override);
  CL_VALUE_ENUM(_sym_moduleFlagAppend, llvm::Module::Append);
  CL_VALUE_ENUM(_sym_moduleFlagAppendUnique, llvm::Module::AppendUnique);
  CL_END_ENUM(_sym_STARmoduleModFlagBehaviorSTAR);

  CL_BEGIN_ENUM(llvm::Module::ModFlagBehavior,_sym_STARmoduleModFlagBehaviorSTAR, "llvm::Module::ModFlagBehavior");
  CL_VALUE_ENUM(_sym_moduleFlagError, llvm::Module::Error);
  CL_VALUE_ENUM(_sym_moduleFlagWarning, llvm::Module::Warning);
  CL_VALUE_ENUM(_sym_moduleFlagRequire, llvm::Module::Require);
  CL_VALUE_ENUM(_sym_moduleFlagOverride, llvm::Module::Override);
  CL_VALUE_ENUM(_sym_moduleFlagAppend, llvm::Module::Append);
  CL_VALUE_ENUM(_sym_moduleFlagAppendUnique, llvm::Module::AppendUnique);;
  CL_END_ENUM(_sym_STARmoduleModFlagBehaviorSTAR);


CL_LISPIFY_NAME("getFunction");
CL_DEFMETHOD llvm::Function *Module_O::getFunction(core::String_sp dispatchName) {
  llvm::Module *module = this->wrappedPtr();
  string funcName = dispatchName->get_std_string();
  llvm::Function *func = module->getFunction(funcName);
  return func;
}

CL_LISPIFY_NAME("moduleValid");
CL_DEFMETHOD bool Module_O::valid() const {
  return this->wrappedPtr() != NULL;
}

CL_DEFMETHOD llvm::DataLayout Module_O::getDataLayout() const {
  return this->wrappedPtr()->getDataLayout ();
}

CL_LISPIFY_NAME("moduleDelete");
CL_DEFMETHOD void Module_O::moduleDelete() {
  ASSERT(this->wrappedPtr() != NULL);
  delete this->wrappedPtr();
  this->set_wrapped(NULL);
}

CL_LISPIFY_NAME("dump_namedMDList");
CL_DEFMETHOD void Module_O::dump_namedMDList() const {
  llvm::Module *M = this->wrappedPtr();
  IMPLEMENT_MEF("Come up with a way to dump the MDList without using dump() (only enabled when LLVM_ENABLE_DUMP is on)");
}

void Module_O::initialize() {
  this->Base::initialize();
  this->_UniqueGlobalVariableStrings = core::HashTableEqual_O::create_default();
}

CL_DEFMETHOD void Module_O::emit_version_ident_metadata()
{
  llvm::Module& TheModule = *this->wrappedPtr();
  llvm::NamedMDNode *IdentMetadata =
    TheModule.getOrInsertNamedMetadata("llvm.ident");
  std::string Version = "Clasp";
  llvm::LLVMContext &Ctx = TheModule.getContext();
  llvm::Metadata *IdentNode[] = {llvm::MDString::get(Ctx, Version)};
  IdentMetadata->addOperand(llvm::MDNode::get(Ctx, IdentNode));
}

CL_LISPIFY_NAME("getOrCreateUniquedStringGlobalVariable");
CL_DEFMETHOD GlobalVariable_sp Module_O::getOrCreateUniquedStringGlobalVariable(const string &value, const string &name) {
  core::SimpleBaseString_sp nameKey = core::SimpleBaseString_O::make(name);
  core::List_sp it = this->_UniqueGlobalVariableStrings->gethash(nameKey);
  //	map<string,GlobalVariableStringHolder>::iterator it = this->_UniqueGlobalVariableStrings.find(name);
  llvm::GlobalVariable *GV;
  if (it.nilp()) {
    llvm::Module *M = this->wrappedPtr();
    llvm::LLVMContext &context = M->getContext();
    llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(context, value,true);
    GV = new llvm::GlobalVariable(*M,
                                  StrConstant->getType(),
                                  true,
                                  llvm::GlobalValue::PrivateLinkage,
                                  StrConstant,
                                  "");
    GV->setName(name);
    GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    //	    GlobalVariableStringHolder holder;
    core::SimpleBaseString_sp first = core::SimpleBaseString_O::make(value);
    GlobalVariable_sp second = core::RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable *>(GV);
    core::Cons_sp pair = core::Cons_O::create(first, second);
    this->_UniqueGlobalVariableStrings->setf_gethash(nameKey, pair);
    //	    holder._String = value;
    //	    holder._LlvmValue = core::RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(GV);
    //	    this->_UniqueGlobalVariableStrings[name] = holder;
    //	    return holder._LlvmValue;
    return second;
  }
  if (gc::As<core::String_sp>(oCar(it))->get_std_string() != value)
  {
    SIMPLE_ERROR(BF("You tried to getOrCreateUniquedStringGlobalVariable with name[%s] and value[%s] - there was already a StringGlobalVariable with that name but it has a different value!!!! value[%s]") % name % value % gc::As<core::String_sp>(oCar(it))->get_std_string());
  }
  return gc::As<GlobalVariable_sp>(oCdr(it)); // it->second._LlvmValue;
}

CL_LISPIFY_NAME("getGlobalList");
CL_DEFMETHOD core::List_sp Module_O::getGlobalList() const {
  ql::list globals;
  llvm::Module *m = this->wrappedPtr();
  for (llvm::Module::global_iterator it = m->global_begin(); it != m->global_end(); it++) {
    globals << core::RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable *>(&(*it));
  }
  return globals.cons();
}
}; // llvmo

namespace llvmo {

void ExecutionEngine_O::initialize() {
  this->_DependentModules = core::HashTableEqual_O::create_default();
}

string ExecutionEngine_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << this->_ptr << " > ";
  return ss.str();
}

CL_LISPIFY_NAME("dependentModuleNames");
CL_DEFMETHOD core::List_sp ExecutionEngine_O::dependentModuleNames() const {
  ql::list l;
  this->_DependentModules->mapHash([&l](core::T_sp key, core::T_sp val) {
                l << key;
  });
  return l.cons();
}

void ExecutionEngine_O::addNamedModule(const string &name, Module_sp module) {
  core::SimpleBaseString_sp key = core::SimpleBaseString_O::make(name);
  if (this->_DependentModules->contains(key)) {
    //	if ( this->_DependentModules.count(name) != 0 )
    SIMPLE_ERROR(BF("A module named %s is already in this ExecutionEngine - remove it first before adding another") % name);
  }
  this->_DependentModules->setf_gethash(key, module);
  //	this->_DependentModules[name] = module;
  std::unique_ptr<llvm::Module> ownedModule(module->wrappedPtr());
  module->set_wrapped(NULL);
  this->wrappedPtr()->addModule(std::move(ownedModule));
}

CL_LISPIFY_NAME("hasNamedModule");
CL_DEFMETHOD bool ExecutionEngine_O::hasNamedModule(const string &name) {
  if (this->_DependentModules->contains(core::SimpleBaseString_O::make(name)))
    return true;
  return false;
}

void ExecutionEngine_O::removeNamedModule(const string &name) {
  core::SimpleBaseString_sp key = core::SimpleBaseString_O::make(name);
  core::T_mv mi = this->_DependentModules->gethash(key);
  //	core::StringMap<Module_O>::iterator mi = this->_DependentModules.find(name);
  if (mi.valueGet_(1).nilp()) // == this->_DependentModules.end() )
  {
    SIMPLE_ERROR(BF("Could not find named module %s") % name);
  }
  Module_sp mod = gc::As<Module_sp>(mi);
  this->wrappedPtr()->clearGlobalMappingsFromModule(mod->wrappedPtr());
  this->wrappedPtr()->removeModule(mod->wrappedPtr());
  this->_DependentModules->remhash(key);
}

CL_LISPIFY_NAME("addGlobalMapping");
CL_DEFMETHOD void ExecutionEngine_O::addGlobalMapping(GlobalValue_sp value, core::Pointer_sp ptr) {
  this->wrappedPtr()->addGlobalMapping(value->wrappedPtr(), ptr->ptr());
}

CL_LISPIFY_NAME("addModule");
CL_DEFMETHOD void ExecutionEngine_O::addModule(Module_sp module) {
  llvm::ExecutionEngine *ee = this->wrappedPtr();
  std::unique_ptr<llvm::Module> mod(module->wrappedPtr());
  module->set_wrapped(NULL);
  ee->addModule(std::move(mod));
}

CL_LISPIFY_NAME("removeModule");
CL_DEFMETHOD bool ExecutionEngine_O::removeModule(Module_sp module) {
  llvm::ExecutionEngine *ee = this->wrappedPtr();
  return ee->removeModule(module->wrappedPtr());
}

CL_LISPIFY_NAME("find_function_named");
CL_DEFMETHOD Function_sp ExecutionEngine_O::find_function_named(core::String_sp name) {
  return gc::As<Function_sp>(translate::to_object<llvm::Function *>::convert(this->wrappedPtr()->FindFunctionNamed(name->get_std_string().c_str())));
}

CL_LISPIFY_NAME(clearAllGlobalMappings);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::clearAllGlobalMappings);
CL_LISPIFY_NAME(getDataLayout);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getDataLayout);
CL_LISPIFY_NAME(getPointerToGlobalIfAvailable_GlobalValue);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, (void*(llvm::ExecutionEngine::*)(const llvm::GlobalValue *))&llvm::ExecutionEngine::getPointerToGlobalIfAvailable);
CL_LISPIFY_NAME(getPointerToGlobalIfAvailable_StringRef);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, (void*(llvm::ExecutionEngine::*)(llvm::StringRef))&llvm::ExecutionEngine::getPointerToGlobalIfAvailable);
CL_LISPIFY_NAME(getGlobalValueAddress);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getGlobalValueAddress);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getDataLayout);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getOrEmitGlobalVariable);

}; // llvmo


namespace llvmo {
CL_LISPIFY_NAME("DataLayoutCopy");
CL_DEFMETHOD DataLayout_sp DataLayout_O::copy() const {
  GC_ALLOCATE_VARIADIC(DataLayout_O, cp, *(this->_DataLayout));
  return cp;
};


CL_LISPIFY_NAME("data-layout-get-struct-layout");
CL_DEFMETHOD StructLayout_sp DataLayout_O::getStructLayout(StructType_sp ty) const {
  llvm::StructType* sty = ty->wrapped();
  const llvm::StructLayout* layout = this->_DataLayout->getStructLayout(sty);
  GC_ALLOCATE_VARIADIC(StructLayout_O,sl,layout);
  return sl;
}


CL_LISPIFY_NAME(DataLayout-getTypeAllocSize);
CL_DEFMETHOD size_t DataLayout_O::getTypeAllocSize(llvm::Type* ty)
{
  return this->_DataLayout->getTypeAllocSize(ty);
}

CL_LISPIFY_NAME(getStringRepresentation);
CL_EXTERN_DEFMETHOD(DataLayout_O,&llvm::DataLayout::getStringRepresentation);


CL_LISPIFY_NAME(struct-layout-get-element-offset);
CL_DEFMETHOD size_t StructLayout_O::getElementOffset(size_t idx) const
{
  size_t offset = this->_StructLayout->getElementOffset(idx);
  return this->_StructLayout->getElementOffset(idx);
}

}; // llvmo

// This is needed for llvm3.7
//
namespace llvmo {


CL_LAMBDA(triple);
CL_PKG_NAME(LlvmoPkg,"makeTargetLibraryInfoWRapperPass");
CL_DEFUN TargetLibraryInfoWrapperPass_sp TargetLibraryInfoWrapperPass_O::make(llvm::Triple *tripleP) {
  GC_ALLOCATE(TargetLibraryInfoWrapperPass_O, self);
  self->_ptr = new llvm::TargetLibraryInfoWrapperPass(*tripleP);
  return self;
};

;

}; // llvmo

namespace llvmo {
CL_LAMBDA(module);
CL_PKG_NAME(LlvmoPkg,"makeFunctionPassManager");
CL_DEFUN FunctionPassManager_sp FunctionPassManager_O::make(llvm::Module *module) {
  GC_ALLOCATE(FunctionPassManager_O, self);
  self->_ptr = new llvm::legacy::FunctionPassManager(module);
  return self;
};


  CL_LISPIFY_NAME(function-pass-manager-add);
CL_EXTERN_DEFMETHOD(FunctionPassManager_O, &llvm::legacy::FunctionPassManager::add);
  CL_LISPIFY_NAME(doInitialization);
  CL_EXTERN_DEFMETHOD(FunctionPassManager_O, &llvm::legacy::FunctionPassManager::doInitialization);
  CL_LISPIFY_NAME(doFinalization);
  CL_EXTERN_DEFMETHOD(FunctionPassManager_O, &llvm::legacy::FunctionPassManager::doFinalization);
  CL_LISPIFY_NAME(function-pass-manager-run);
  CL_EXTERN_DEFMETHOD(FunctionPassManager_O, &llvm::legacy::FunctionPassManager::run);;

;

}; // llvmo

namespace llvmo {
CL_LAMBDA();
CL_PKG_NAME(LlvmoPkg,"makePassManager");
CL_DEFUN PassManager_sp PassManager_O::make() {
  GC_ALLOCATE(PassManager_O, self);
  self->_ptr = new llvm::legacy::PassManager();
  return self;
};



  CL_LISPIFY_NAME(passManagerAdd);
CL_EXTERN_DEFMETHOD(PassManager_O, &llvm::legacy::PassManager::add);
  CL_LISPIFY_NAME(passManagerRun);
CL_EXTERN_DEFMETHOD(PassManager_O, &llvm::legacy::PassManager::run);;

;

}; // llvmo

namespace llvmo {

string EngineBuilder_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << this->_ptr << " > ";
  return ss.str();
}

CL_LAMBDA(module);
CL_PKG_NAME(LlvmoPkg,"make-EngineBuilder");
CL_DEFUN EngineBuilder_sp EngineBuilder_O::make(Module_sp module) {
  GC_ALLOCATE(EngineBuilder_O, self);
  std::unique_ptr<llvm::Module> ownedModule(module->wrappedPtr());
  module->set_wrapped(NULL);
  self->_ptr = new llvm::EngineBuilder(std::move(ownedModule));
  self->_ptr->setErrorStr(&(self->_ErrorStr));
  return self;
};

CL_LISPIFY_NAME("setEngineKind");
CL_DEFMETHOD void EngineBuilder_O::setEngineKind(core::Symbol_sp kind) {
  SYMBOL_EXPORT_SC_(LlvmoPkg, interpreter);
  SYMBOL_EXPORT_SC_(LlvmoPkg, jit);
  if (kind == _sym_interpreter) {
    this->wrappedPtr()->setEngineKind(llvm::EngineKind::Interpreter);
  } else if (kind == _sym_jit) {
    this->wrappedPtr()->setEngineKind(llvm::EngineKind::JIT);
  } else {
    stringstream ss;
    ss << "Engine kind can only be ";
    ss << _sym_interpreter->fullName() << " or ";
    ss << _sym_jit->fullName() << " - you gave: " << kind->fullName();
    SIMPLE_ERROR(BF("%s") % ss.str());
  }
}


CL_LISPIFY_NAME("setTargetOptions");
CL_DEFMETHOD void EngineBuilder_O::setTargetOptions(TargetOptions_sp options) {
  this->wrappedPtr()->setTargetOptions(*options->wrappedPtr());
}



;


CL_LISPIFY_NAME("create");
CL_DEFMETHOD ExecutionEngine_sp EngineBuilder_O::createExecutionEngine() {
  llvm::ExecutionEngine *ee = this->wrappedPtr()->create();
  ExecutionEngine_sp eeo = core::RP_Create_wrapped<ExecutionEngine_O, llvm::ExecutionEngine *>(ee);
  if (!eeo) {
    SIMPLE_ERROR(BF("Could not create the execution-engine - got NULL"));
  }
  return eeo;
}

}; // llvmo

namespace llvmo {
CL_LAMBDA();
CL_PKG_NAME(LlvmoPkg,"make-PassManagerBuilder");
CL_DEFUN PassManagerBuilder_sp PassManagerBuilder_O::make() {
  GC_ALLOCATE(PassManagerBuilder_O, self);
  self->_ptr = new llvm::PassManagerBuilder();
  return self;
};

CL_DEFUN void PassManagerBuilderSetfInliner(PassManagerBuilder_sp pmb, llvm::Pass *inliner) {
  //  printf("%s:%d Setting inliner for PassManagerBuilder to %p\n", __FILE__, __LINE__, inliner );
  pmb->wrappedPtr()->Inliner = inliner;
};

CL_DEFUN void PassManagerBuilderSetfOptLevel(PassManagerBuilder_sp pmb, int optLevel) {
  pmb->wrappedPtr()->OptLevel = optLevel;
};

CL_DEFUN void PassManagerBuilderSetfSizeLevel(PassManagerBuilder_sp pmb, int level) {
  pmb->wrappedPtr()->SizeLevel = level;
};



  CL_LISPIFY_NAME(populateModulePassManager);
  CL_EXTERN_DEFMETHOD(PassManagerBuilder_O, &llvm::PassManagerBuilder::populateModulePassManager);
  CL_LISPIFY_NAME(populateFunctionPassManager);
  CL_EXTERN_DEFMETHOD(PassManagerBuilder_O, &llvm::PassManagerBuilder::populateFunctionPassManager);
  CL_LISPIFY_NAME(populateLTOPassManager);
  CL_EXTERN_DEFMETHOD(PassManagerBuilder_O, &llvm::PassManagerBuilder::populateLTOPassManager);;

;


}; // llvmo

namespace llvmo {
Constant_sp Constant_O::create(llvm::Constant *ptr) {
  return core::RP_Create_wrapped<Constant_O, llvm::Constant *>(ptr);
};
}

namespace llvmo {
CL_LAMBDA(type values);
CL_LISPIFY_NAME(constant-data-array-get-uint32);
CL_DEFUN Constant_sp ConstantDataArray_O::getUInt32(LLVMContext_sp context, core::T_sp ovalues) {
  Constant_sp ca = ConstantDataArray_O::create();
  vector<uint32_t> vector_IdxList;
  if (core::Cons_sp cvalues = ovalues.asOrNull<core::Cons_O>()) {
    core::List_sp lvalues = cvalues;
    for (auto cur : lvalues) {
      vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(oCar(cur))));
    }
  } else if (core::Vector_sp vvalues = ovalues.asOrNull<core::Vector_O>()) {
    for (int i = 0; i < vvalues->length(); i++) {
      vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(vvalues->rowMajorAref(i))));
    }
  }
  llvm::ArrayRef<uint32_t> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant *llvm_ca = llvm::ConstantDataArray::get(*(context->wrappedPtr()), array_ref_vector_IdxList);
  ca->set_wrapped(llvm_ca);
  return ca;
}



;

}; // llvmo

namespace llvmo {
CL_LAMBDA(type values);
CL_LISPIFY_NAME(constant-array-get);
CL_DEFUN Constant_sp ConstantArray_O::get(ArrayType_sp type, core::List_sp values) {
  Constant_sp ca = ConstantArray_O::create();
  vector<llvm::Constant *> vector_IdxList;
  for (auto cur : values) {
    vector_IdxList.push_back(llvm::cast<llvm::Constant>(gc::As<Value_sp>(oCar(cur))->wrappedPtr()));
  }
  llvm::ArrayRef<llvm::Constant *> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant *llvm_ca = llvm::ConstantArray::get(type->wrapped(), array_ref_vector_IdxList);
  ca->set_wrapped(llvm_ca);
  return ca;
}



;

}; // llvmo

namespace llvmo {
CL_LAMBDA(function basic-block);
CL_LISPIFY_NAME(block-address-get);
CL_DEFUN BlockAddress_sp BlockAddress_O::get(Function_sp func, BasicBlock_sp bb) {
  BlockAddress_sp basp = BlockAddress_O::create();
  llvm::BlockAddress *ba = llvm::BlockAddress::get(func->wrappedPtr(), bb->wrappedPtr());
  basp->set_wrapped(ba);
  return basp;
}



;

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(constant-expr-get-in-bounds-get-element-ptr);
CL_DEFUN Constant_sp ConstantExpr_O::getInBoundsGetElementPtr(llvm::Type* element_type, Constant_sp constant, core::List_sp idxList) {
  GC_ALLOCATE(Constant_O, res);
  if (element_type==NULL) {
    SIMPLE_ERROR(BF("You must provide a type for ConstantExpr_O::getInBoundsGetElementPtr"));
  }
  vector<llvm::Constant *> vector_IdxList;
  for (auto cur : idxList) {
    vector_IdxList.push_back(gc::As<Constant_sp>(oCar(cur))->wrappedPtr());
  }

  llvm::ArrayRef<llvm::Constant *> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant* llvm_constant = constant->wrappedPtr();
  llvm::Constant *llvm_res = llvm::ConstantExpr::getInBoundsGetElementPtr(element_type,llvm_constant, array_ref_vector_IdxList);
  res->set_wrapped(llvm_res);
  return res;
}
}; // llvmo


namespace llvmo {
CL_DEFMETHOD void GlobalValue_O::setUnnamedAddr(llvm::GlobalValue::UnnamedAddr unnamed_addr) {
  this->wrappedPtr()->setUnnamedAddr(unnamed_addr);
}

CL_DEFMETHOD llvm::GlobalValue::UnnamedAddr GlobalValue_O::getUnnamedAddr() {
  return this->wrappedPtr()->getUnnamedAddr();
}


};
namespace llvmo {

CL_LAMBDA("module type is-constant linkage initializer name &optional (insert-before nil) (thread-local-mode 'llvm-sys:not-thread-local)");
CL_LISPIFY_NAME(make-global-variable);
CL_DEFUN GlobalVariable_sp GlobalVariable_O::make(Module_sp mod, Type_sp type, bool isConstant, core::Symbol_sp linkage, /*Constant_sp*/ core::T_sp initializer, core::String_sp name, /*GlobalVariable_sp*/ core::T_sp insertBefore, core::Symbol_sp threadLocalMode) {
  GC_ALLOCATE(GlobalVariable_O, me);
  translate::from_object<llvm::GlobalValue::LinkageTypes> llinkage(linkage);
  llvm::Constant *llvm_initializer = NULL;
  if (initializer.notnilp()) {
    llvm_initializer = gc::As<Constant_sp>(initializer)->wrappedPtr();
  }
  llvm::GlobalVariable *lInsertBefore = NULL;
  if (insertBefore.notnilp()) {
    lInsertBefore = gc::As<GlobalVariable_sp>(insertBefore)->wrappedPtr();
  }
  translate::from_object<llvm::GlobalValue::ThreadLocalMode> lThreadLocalMode(threadLocalMode);
  llvm::GlobalVariable *gv = new llvm::GlobalVariable(*(mod->wrappedPtr()), type->wrappedPtr(), isConstant, llinkage._v, llvm_initializer, name->get_std_string(), lInsertBefore, lThreadLocalMode._v);
  me->set_wrapped(gv);
  //	me->set_ptrIsOwned(true); // GlobalVariables made this way are responsible for freeing their pointers - I hope this isn't a disaster
  return me;
};



  CL_LISPIFY_NAME(eraseFromParent);
  CL_EXTERN_DEFMETHOD(GlobalVariable_O, &llvm::GlobalVariable::eraseFromParent);
  CL_LISPIFY_NAME(setInitializer);
  CL_EXTERN_DEFMETHOD(GlobalVariable_O, &llvm::GlobalVariable::setInitializer);;

;

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME("setMetadata");
CL_DEFMETHOD void Instruction_O::setMetadata(core::String_sp kind, MDNode_sp mdnode) {
  this->wrappedPtr()->setMetadata(kind->get_std_string(), mdnode->wrappedPtr());
}



  CL_LISPIFY_NAME(getParent);
  CL_EXTERN_DEFMETHOD(Instruction_O, (llvm::BasicBlock * (llvm::Instruction::*)()) & llvm::Instruction::getParent);

CL_LISPIFY_NAME(getDebugLocInfo);
CL_DEFUN core::T_mv llvm_sys__getDebugLocInfo(Instruction_sp instr) {
  const llvm::DebugLoc& debugLoc = instr->wrappedPtr()->getDebugLoc();
  if (debugLoc) {
    size_t lineno = debugLoc.getLine();
    size_t column = debugLoc.getCol();
    return Values(_lisp->_true(),core::make_fixnum(lineno),core::make_fixnum(column));
  }
  return Values(_Nil<core::T_O>());
}
CL_DOCSTRING("Erase the instruction from its parent basic block and return the next instruction or NIL");
CL_DEFUN void llvm_sys__instruction_eraseFromParent(Instruction_sp instr)
{
  llvm::SymbolTableList<llvm::Instruction>::iterator next = instr->wrappedPtr()->eraseFromParent();
}

CL_DOCSTRING("Return the next non-debug instruction or NIL if there is none");
CL_DEFUN core::T_sp llvm_sys__instruction_getNextNonDebugInstruction(Instruction_sp instr)
{
#if (LLVM_VERSION_X100<900)  
  const llvm::Instruction* next = instr->wrappedPtr()->getNextNode();
  for (; next; next = next->getNextNode()) {
    if (!llvm::isa<llvm::DbgInfoIntrinsic>(next)) break;
  }
#else
  const llvm::Instruction* next = instr->wrappedPtr()->getNextNonDebugInstruction();
#endif
  if (next!=NULL) {
    return translate::to_object<llvm::Instruction*>::convert(const_cast<llvm::Instruction*>(next));
  }
  return _Nil<core::T_O>();
}
;


CL_LISPIFY_NAME("insertAfter");
CL_EXTERN_DEFMETHOD(Instruction_O,&llvm::Instruction::insertAfter);
CL_LISPIFY_NAME("insertBefore");
CL_EXTERN_DEFMETHOD(Instruction_O,&llvm::Instruction::insertBefore);

CL_LISPIFY_NAME("terminatorInstP");
CL_DEFMETHOD bool Instruction_O::terminatorInstP() const {
  return this->wrappedPtr()->isTerminator();
}

}; // llvmo

namespace llvmo {

CL_DEFUN llvm::Instruction* llvm_sys__replace_call_keep_args(llvm::Function* func, llvm::Instruction* callOrInvoke) {
//  printf("%s:%d In llvm-sys::replace-call\n",__FILE__, __LINE__);
  llvm::CallSite CS(callOrInvoke);
  llvm::Instruction *NewCI = NULL;
  if (llvm::isa<llvm::CallInst>(callOrInvoke)) {
    llvm::CallInst* callInst = llvm::cast<llvm::CallInst>(callOrInvoke);
    llvm::SmallVector<llvm::Value *, 4> svargs(callInst->arg_operands());
    llvm::CallInst* NewCall = llvm::CallInst::Create(func,svargs);
    NewCall->setCallingConv(func->getCallingConv());
    NewCI = NewCall;
  } else if (llvm::isa<llvm::InvokeInst>(callOrInvoke)) {
    llvm::InvokeInst* invokeInst = llvm::cast<llvm::InvokeInst>(callOrInvoke);
    llvm::SmallVector<llvm::Value*,4> args(invokeInst->arg_operands());
    llvm::InvokeInst* invoke = llvm::cast<llvm::InvokeInst>(callOrInvoke);
    llvm::BasicBlock* ifNormal = invoke->getNormalDest();
    llvm::BasicBlock* ifException = invoke->getUnwindDest();
    llvm::InvokeInst* NewInvoke = llvm::InvokeInst::Create(func,ifNormal,ifException,args);
    NewInvoke->setCallingConv(func->getCallingConv());
    NewCI = NewInvoke;
  }
  if (!callOrInvoke->use_empty()) {
    callOrInvoke->replaceAllUsesWith(NewCI);
  }
  llvm::ReplaceInstWithInst(callOrInvoke,NewCI);
  return NewCI;
};


CL_DEFUN llvm::Instruction* llvm_sys__replace_call(llvm::Function* func, llvm::Instruction* callOrInvoke, llvm::ArrayRef<llvm::Value *> args) {
//  printf("%s:%d In llvm-sys::replace-call\n",__FILE__, __LINE__);
  llvm::CallSite CS(callOrInvoke);
  llvm::Instruction *NewCI = NULL;
  if (llvm::isa<llvm::CallInst>(callOrInvoke)) {
    llvm::CallInst* NewCall = llvm::CallInst::Create(func,args);
    NewCall->setCallingConv(func->getCallingConv());
    NewCI = NewCall;
  } else if (llvm::isa<llvm::InvokeInst>(callOrInvoke)) {
    llvm::InvokeInst* invoke = llvm::cast<llvm::InvokeInst>(callOrInvoke);
    llvm::BasicBlock* ifNormal = invoke->getNormalDest();
    llvm::BasicBlock* ifException = invoke->getUnwindDest();
    llvm::InvokeInst* NewInvoke = llvm::InvokeInst::Create(func,ifNormal,ifException,args);
    NewInvoke->setCallingConv(func->getCallingConv());
    NewCI = NewInvoke;
  }
  if (!callOrInvoke->use_empty()) {
    callOrInvoke->replaceAllUsesWith(NewCI);
  }
  llvm::ReplaceInstWithInst(callOrInvoke,NewCI);
  return NewCI;
};


core::List_sp CallInst_O::getArgumentList() const {
  ql::list l;
  for ( auto arg = this->wrappedPtr()->arg_begin(), argEnd(this->wrappedPtr()->arg_end());
        arg != argEnd; ++arg ) {
    l << translate::to_object<llvm::Value*>::convert(*arg);
  }
  return l.cons();
};  

CL_DEFUN core::List_sp llvm_sys__call_or_invoke_getArgumentList(Instruction_sp callOrInvoke) {
  if (gc::IsA<CallInst_sp>(callOrInvoke)) {
    return gc::As<CallInst_sp>(callOrInvoke)->getArgumentList();
  } else if (gc::IsA<InvokeInst_sp>(callOrInvoke)) {
    return gc::As<InvokeInst_sp>(callOrInvoke)->getArgumentList();
  }
  SIMPLE_ERROR(BF("Only call or invoke can provide arguments"));
}


CL_DEFMETHOD void CallInst_O::addParamAttr(unsigned index, llvm::Attribute::AttrKind attrkind)
{
  this->wrappedPtr()->addParamAttr(index,attrkind);
}

CL_DEFMETHOD llvm::Function* CallInst_O::getCalledFunction() {
  return this->wrappedPtr()->getCalledFunction();
}

CL_DEFMETHOD void InvokeInst_O::addParamAttr(unsigned index, llvm::Attribute::AttrKind attrkind)
{
  this->wrappedPtr()->addParamAttr(index,attrkind);
}

CL_DEFMETHOD llvm::Function* InvokeInst_O::getCalledFunction() {
  return this->wrappedPtr()->getCalledFunction();
}

core::List_sp InvokeInst_O::getArgumentList() const {
  ql::list l;
  for ( auto arg = this->wrappedPtr()->arg_begin(), argEnd(this->wrappedPtr()->arg_end());
        arg != argEnd; ++arg ) {
    l << translate::to_object<llvm::Value*>::convert(*arg);
  }
  return l.cons();
};  

}; // llvmo
namespace llvmo {


  CL_LISPIFY_NAME(addIncoming);
  CL_EXTERN_DEFMETHOD(PHINode_O, &llvm::PHINode::addIncoming);;

;

}; // llvmo

namespace llvmo {


  CL_LISPIFY_NAME(setCleanup);
  CL_EXTERN_DEFMETHOD(LandingPadInst_O, &llvm::LandingPadInst::setCleanup);
  CL_LISPIFY_NAME(isCleanup);
  CL_EXTERN_DEFMETHOD(LandingPadInst_O, &llvm::LandingPadInst::isCleanup);
  CL_LISPIFY_NAME(addClause);
  CL_EXTERN_DEFMETHOD(LandingPadInst_O, &llvm::LandingPadInst::addClause);;

;

}; // llvmo

namespace llvmo {


  CL_LISPIFY_NAME(setAlignment);
  CL_EXTERN_DEFMETHOD(AllocaInst_O, &AllocaInst_O::ExternalType::setAlignment);;

CL_DEFUN llvm::AllocaInst* llvm_sys__insert_alloca_before_terminator(llvm::Type* type, const llvm::Twine& name, llvm::BasicBlock* block)
{
//  printf("%s:%d   llvm-sys::insert-alloca\n", __FILE__, __LINE__ );
  llvm::Instruction* insertBefore = block->getTerminator();
  llvm::AllocaInst* alloca = new llvm::AllocaInst(type,0,name,insertBefore);
  return alloca;
}
;

}; // llvmo

namespace llvmo {


;


CL_LISPIFY_NAME("addCase");
CL_DEFMETHOD void SwitchInst_O::addCase(ConstantInt_sp onVal, BasicBlock_sp dest) {
  this->wrappedPtr()->addCase(onVal->wrappedPtr(), dest->wrappedPtr());
}

}; // llvmo

namespace llvmo {

  CL_LISPIFY_NAME(addDestination);
  CL_EXTERN_DEFMETHOD(IndirectBrInst_O, &llvm::IndirectBrInst::addDestination);;

;

}; // llvmo

namespace llvmo {
ConstantFP_sp ConstantFP_O::create(llvm::ConstantFP *ptr) {
  return core::RP_Create_wrapped<ConstantFP_O, llvm::ConstantFP *>(ptr);
};
}

namespace llvmo {


  CL_LISPIFY_NAME(constantFpGet);
  CL_EXTERN_DEFUN((llvm::ConstantFP *(*)(llvm::LLVMContext &, const llvm::APFloat &)) &llvm::ConstantFP::get);
  CL_LISPIFY_NAME(constantFpGetTypeDouble);
  CL_EXTERN_DEFUN((llvm::Constant *(*)(llvm::Type *, double)) &llvm::ConstantFP::get );
  CL_LISPIFY_NAME(constantFpGetTypeStringref);
  CL_EXTERN_DEFUN((llvm::Constant *(*)(llvm::Type * type, llvm::StringRef label))&llvm::ConstantFP::get);

;


string ConstantFP_O::__repr__() const {
  stringstream ss;
  llvm::APFloat const &val = this->wrappedPtr()->getValueAPF();
  llvm::SmallVector<char, 100> svistr;
  val.toString(svistr);
  std::string str(svistr.data(), svistr.size());
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << str << ">";
  return ss.str();
}

}; // llvmo

namespace llvmo {
ConstantInt_sp ConstantInt_O::create(llvm::ConstantInt *ptr) {
  return core::RP_Create_wrapped<ConstantInt_O, llvm::ConstantInt *>(ptr);
};


  CL_LISPIFY_NAME(constant-int-get);
  CL_EXTERN_DEFUN((llvm::ConstantInt *(*)(llvm::LLVMContext &, const llvm::APInt &)) &llvm::ConstantInt::get);

CL_LISPIFY_NAME(get-true);
CL_EXTERN_DEFUN((llvm::ConstantInt *(*)(llvm::LLVMContext &)) &llvm::ConstantInt::getTrue);

CL_LISPIFY_NAME(get-false);
CL_EXTERN_DEFUN((llvm::ConstantInt *(*)(llvm::LLVMContext &)) &llvm::ConstantInt::getFalse);


;


string ConstantInt_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << this->wrappedPtr()->getValue().toString(10, true) << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {
ConstantStruct_sp ConstantStruct_O::create(llvm::ConstantStruct *ptr) {
  return core::RP_Create_wrapped<ConstantStruct_O, llvm::ConstantStruct *>(ptr);
};



  CL_LISPIFY_NAME(CONSTANT-STRUCT-GET);
  CL_EXTERN_DEFUN((llvm::Constant *(*)(llvm::StructType *T, llvm::ArrayRef<llvm::Constant *>)) &llvm::ConstantStruct::get);

;


}; // llvmo

namespace llvmo {
UndefValue_sp UndefValue_O::create(llvm::UndefValue *ptr) {
  return core::RP_Create_wrapped<UndefValue_O, llvm::UndefValue *>(ptr);
};



  CL_LISPIFY_NAME(UNDEF_VALUE-GET);
CL_EXTERN_DEFUN((llvm::UndefValue* (*)(llvm::Type* type))&llvm::UndefValue::get);

;


string UndefValue_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {
ConstantPointerNull_sp ConstantPointerNull_O::create(llvm::ConstantPointerNull *ptr) {
  return core::RP_Create_wrapped<ConstantPointerNull_O, llvm::ConstantPointerNull *>(ptr);
};



  CL_LISPIFY_NAME(constant-pointer-null-get);
CL_EXTERN_DEFUN((llvm::ConstantPointerNull* (*)(llvm::PointerType *T))&llvm::ConstantPointerNull::get);

;


string ConstantPointerNull_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {

CL_LAMBDA(value);
CL_LISPIFY_NAME(make-apfloat-float);
CL_DEFUN APFloat_sp APFloat_O::makeAPFloatFloat(core::SingleFloat_sp value) {
  GC_ALLOCATE(APFloat_O, self);
  self->_value = llvm::APFloat(unbox_single_float(value));
  return self;
};

CL_LAMBDA(value);
CL_LISPIFY_NAME(makeAPFloatDouble);
CL_DEFUN APFloat_sp APFloat_O::makeAPFloatDouble(core::DoubleFloat_sp value) {
  GC_ALLOCATE(APFloat_O, self);
  self->_value = llvm::APFloat(value->get());
  return self;
};
}

namespace llvmo {

APInt_sp APInt_O::create(llvm::APInt api) {
  GC_ALLOCATE(APInt_O, self);
  self->_value = api;
  return self;
}

CL_DEFUN APInt_sp APInt_O::makeAPInt(core::Integer_sp value) {
  GC_ALLOCATE(APInt_O, self);
  if (core__fixnump(value)) {
    core::Fixnum_sp fixnum_value = gc::As<core::Fixnum_sp>(value);
    self->_value = llvm::APInt(gc::fixnum_bits, clasp_to_int(fixnum_value), true);
  } else {
    // It's a bignum so lets convert the bignum to a string and put it into an APInt
    char *asString = NULL;
    core::Bignum_sp bignum_value = gc::As<core::Bignum_sp>(value);
    mpz_class &mpz_val = bignum_value->ref();
    int mpz_size_in_bits = mpz_sizeinbase(mpz_val.get_mpz_t(), 2);
    asString = ::mpz_get_str(NULL, 10, mpz_val.get_mpz_t());
    self->_value = llvm::APInt(mpz_size_in_bits, llvm::StringRef(asString, strlen(asString)), 10);
    free(asString);
  }
  return self;
}
}

namespace llvmo {
CL_DEFUN APInt_sp APInt_O::makeAPInt1(core::T_sp value) {
  GC_ALLOCATE(APInt_O, self);
  if (core__fixnump(value)) {
    core::Fixnum_sp fixnum_value = gc::As<core::Fixnum_sp>(value);
    self->_value = llvm::APInt(1, clasp_to_int(fixnum_value) & 1, false);
  } else {
    if (value.isTrue()) {
      self->_value = llvm::APInt(1, 1, false);
    } else {
      self->_value = llvm::APInt(1, 0, false);
    }
  }
  return self;
}

CL_DEFUN APInt_sp APInt_O::makeAPIntWidth(core::Integer_sp value, uint width, bool sign) {
  GC_ALLOCATE(APInt_O, self);
  llvm::APInt apint;
  int numbits;
  if (value.fixnump()) {
    Fixnum fixnum_value = value.unsafe_fixnum();
    if (!sign && fixnum_value < 0) {
      SIMPLE_ERROR(BF("You tried to create an unsigned APInt32 with the negative value: %d") % fixnum_value);
    }
    apint = llvm::APInt(width, fixnum_value, sign);
    numbits = gc::fixnum_bits;
  } else {
    // It's a bignum so lets convert the bignum to a string and put it into an APInt
    char *asString = NULL;
    core::Bignum_sp bignum_value = gc::As<core::Bignum_sp>(value);
    mpz_class &mpz_val = bignum_value->ref();
    int mpz_size_in_bits = mpz_sizeinbase(mpz_val.get_mpz_t(), 2);
    asString = ::mpz_get_str(NULL, 10, mpz_val.get_mpz_t());
    apint = llvm::APInt(width, llvm::StringRef(asString, strlen(asString)), 10);
    free(asString);
    numbits = mpz_size_in_bits;
    if (numbits > width) {
      string numstr = asString;
      SIMPLE_ERROR(BF("You tried to create an unsigned I%d with a value[%s] that requires %d bits to represent") % width % numstr % mpz_size_in_bits);
    }
  }
  self->_value = apint;
  return self;
}

CL_DEFUN APInt_sp APInt_O::makeAPInt32(core::Integer_sp value) {
  return APInt_O::makeAPIntWidth(value, 32, true);
}

CL_DEFUN APInt_sp APInt_O::makeAPInt64(core::Integer_sp value) {
  return APInt_O::makeAPIntWidth(value, 64, true);
}

}

namespace llvmo {


  SYMBOL_EXPORT_SC_(LlvmoPkg, makeAPInt1);
  SYMBOL_EXPORT_SC_(LlvmoPkg, makeAPInt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, makeAPWidth);
  SYMBOL_EXPORT_SC_(LlvmoPkg, makeAP32);
  SYMBOL_EXPORT_SC_(LlvmoPkg, makeAP64);

;


CL_LISPIFY_NAME("toString");
CL_DEFMETHOD string APInt_O::toString(int radix, bool isigned) const {
  return this->_value.toString(radix, isigned);
}

CL_LAMBDA(api &optional (issigned t))
CL_LISPIFY_NAME("toInteger");
CL_DEFUN core::Integer_sp toInteger(APInt_sp api, bool issigned) {
  string s = api->toString(10,issigned);
  return core::Integer_O::create(s);
}

string APInt_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  ss << this->_value.toString(10, true);
  ss << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {


CL_LISPIFY_NAME(CreateGlobalString);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateGlobalString);
CL_PKG_NAME(LlvmoPkg,"SetInsertPointBasicBlock");
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(void (llvm::IRBuilderBase::*)(llvm::BasicBlock *))&llvm::IRBuilderBase::SetInsertPoint);
CL_PKG_NAME(LlvmoPkg,"SetInsertPointInstruction");
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(void (llvm::IRBuilderBase::*)(llvm::Instruction *))&llvm::IRBuilderBase::SetInsertPoint);

  CL_LISPIFY_NAME(SetInsertPointBasicBlock);
  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (void (llvm::IRBuilderBase::*)(llvm::BasicBlock *))&llvm::IRBuilderBase::SetInsertPoint);
  CL_LISPIFY_NAME(SetInsertPointInstruction);
  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (void (llvm::IRBuilderBase::*)(llvm::Instruction *))&llvm::IRBuilderBase::SetInsertPoint);;
  CL_LISPIFY_NAME(GetInsertBlock);
  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::GetInsertBlock);

;


CL_LISPIFY_NAME("restoreIP");
CL_DEFMETHOD void IRBuilderBase_O::restoreIP(InsertPoint_sp insertPoint) {
  this->wrappedPtr()->restoreIP(insertPoint->insertPoint());
}

CL_LISPIFY_NAME("saveIP");
CL_DEFMETHOD InsertPoint_sp IRBuilderBase_O::saveIP() {
  llvm::IRBuilderBase::InsertPoint ip = this->wrappedPtr()->saveIP();
  InsertPoint_sp oip = InsertPoint_O::create(ip);
  return oip;
}

CL_LISPIFY_NAME("getInsertPointInstruction");
CL_DEFMETHOD core::T_sp IRBuilderBase_O::getInsertPointInstruction() {
  llvm::BasicBlock::iterator ip = this->wrappedPtr()->GetInsertPoint();
  llvm::Instruction* ins = llvm::cast<llvm::Instruction>(ip);
  if (ins!=NULL) {
    Instruction_sp isp = Instruction_O::create();
    isp->set_wrapped(ins);
    return isp;
  }
  return _Nil<core::T_O>();
}


CL_LISPIFY_NAME("ClearCurrentDebugLocation");
CL_DEFMETHOD void IRBuilderBase_O::ClearCurrentDebugLocation() {
  this->_CurrentDebugLocationSet = false;
  llvm::DebugLoc dl;
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
}

CL_LISPIFY_NAME("SetCurrentDebugLocation");
CL_DEFMETHOD void IRBuilderBase_O::SetCurrentDebugLocation(DILocation_sp diloc) {
  this->_CurrentDebugLocationSet = true;
  llvm::DILocation* real_diloc = diloc->operator llvm::DILocation *();
  llvm::DebugLoc dl(real_diloc);
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
}

CL_LISPIFY_NAME("SetCurrentDebugLocationToLineColumnScope");
CL_DEFMETHOD void IRBuilderBase_O::SetCurrentDebugLocationToLineColumnScope(int line, int col,
                                                                            DINode_sp scope) {
  this->_CurrentDebugLocationSet = true;
  llvm::MDNode *mdnode = scope->operator llvm::MDNode *();
  llvm::DebugLoc dl = llvm::DebugLoc::get(line, col, mdnode);
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
}

}; // llvmo

namespace llvmo {
CL_LISPIFY_NAME(make-irbuilder);
CL_DEFUN IRBuilder_sp IRBuilder_O::make(LLVMContext_sp context) {
  GC_ALLOCATE(IRBuilder_O, self);
  self->set_wrapped(new llvm::IRBuilder<>(*(context->wrappedPtr())));
  return self;
};

CL_LISPIFY_NAME("CreateInvoke");
CL_DEFMETHOD llvm::InvokeInst *IRBuilder_O::CreateInvoke(llvm::Value *Callee, llvm::BasicBlock *NormalDest, llvm::BasicBlock *UnwindDest, core::List_sp Args, const llvm::Twine &Name) {
  vector<llvm::Value *> vector_Args;
  for (auto cur : Args) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      vector_Args.push_back(val->wrappedPtr());
    } else {
      vector_Args.push_back(NULL);
    }
  }
  llvm::ArrayRef<llvm::Value *> array_ref_vector_Args(vector_Args);
  return this->wrappedPtr()->CreateInvoke(Callee, NormalDest, UnwindDest, array_ref_vector_Args, Name);
}

CL_LISPIFY_NAME(CreateConstGEP2_32);
CL_DEFMETHOD llvm::Value *IRBuilder_O::CreateConstGEP2_32(llvm::Type* ty, llvm::Value* ptr, int idx0, int idx1, const llvm::Twine &Name) {
  int uidx0 = static_cast<int>(idx0);
  int uidx1 = static_cast<int>(idx1);
  return this->wrappedPtr()->CreateConstGEP2_32(ty,ptr,uidx0, uidx1,Name);
}

CL_LISPIFY_NAME(CreateConstGEP2_64);
CL_DEFMETHOD llvm::Value *IRBuilder_O::CreateConstGEP2_64(llvm::Value *Ptr, size_t idx0, size_t idx1, const llvm::Twine &Name) {
  size_t uidx0 = static_cast<size_t>(idx0);
  size_t uidx1 = static_cast<size_t>(idx1);
  return this->wrappedPtr()->CreateConstGEP2_64(Ptr,uidx0, uidx1,Name);
}

CL_LISPIFY_NAME("CreateInBoundsGEP");
CL_DEFMETHOD llvm::Value *IRBuilder_O::CreateInBoundsGEP(llvm::Value *Ptr, core::List_sp IdxList, const llvm::Twine &Name) {
  vector<llvm::Value *> vector_IdxList;
  for (auto cur : IdxList) {
    vector_IdxList.push_back(gc::As<Value_sp>(oCar(cur))->wrappedPtr());
  }
  llvm::ArrayRef<llvm::Value *> array_ref_vector_IdxList(vector_IdxList);
  return this->wrappedPtr()->CreateInBoundsGEP(Ptr, array_ref_vector_IdxList, Name);
}

CL_LISPIFY_NAME("CreateExtractValue");
CL_DEFMETHOD llvm::Value *IRBuilder_O::CreateExtractValue(llvm::Value *Ptr, core::List_sp IdxList, const llvm::Twine &Name) {
  vector<unsigned int> vector_IdxList;
  for (auto cur : IdxList) {
    vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(oCar(cur))));
  }
  llvm::ArrayRef<unsigned int> array_ref_vector_IdxList(vector_IdxList);
  return this->wrappedPtr()->CreateExtractValue(Ptr, array_ref_vector_IdxList, Name);
}

CL_LISPIFY_NAME("CreateInsertValue");
CL_DEFMETHOD llvm::Value *IRBuilder_O::CreateInsertValue(llvm::Value *Agg, llvm::Value *Val, core::List_sp IdxList, const llvm::Twine &Name) {
  vector<unsigned int> vector_IdxList;
  for (auto cur : IdxList) {
    vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(oCar(cur))));
  }
  llvm::ArrayRef<unsigned int> array_ref_vector_IdxList(vector_IdxList);
  return this->wrappedPtr()->CreateInsertValue(Agg, Val, array_ref_vector_IdxList, Name);
}

string IRBuilder_O::__repr__() const {
  IRBuilder_O *irbuilder = const_cast<IRBuilder_O *>(this);
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  llvm::BasicBlock *bb = irbuilder->wrappedPtr()->GetInsertBlock();
  if (bb) {
    ss << " :insert-block-name " << bb->getName().data();
    llvm::Function *func = bb->getParent();
    if (func) {
      ss << " :function " << func->getName().data();
    } else {
      ss << " :function UNDEFINED-FUNCTION! ";
    }
  } else {
    ss << " :insert-block-name UNDEFINED-BASIC_BLOCK! ";
  }
#ifdef USE_BOEHM
  ss << "@" << (void*)this->_ptr;
#endif
  ss << " >";
  return ss.str();
}

CL_LAMBDA (irbuilder cond true-branch false-branch &optional branch-weights unpred);
CL_LISPIFY_NAME(CreateCondBr);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::BranchInst * (IRBuilder_O::ExternalType::*)(llvm::Value *, llvm::BasicBlock *, llvm::BasicBlock *, llvm::MDNode *, llvm::MDNode *))&IRBuilder_O::ExternalType::CreateCondBr);
CL_LAMBDA("irbuilder lhs rhs &optional (name \"\") has-nuw has-nsw");
CL_LISPIFY_NAME(CreateAdd);
CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateAdd);

  CL_LISPIFY_NAME(CreateRet);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateRet);
  CL_LISPIFY_NAME(CreateRetVoid);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateRetVoid);
  CL_LISPIFY_NAME(CreateBr);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateBr);
  CL_LISPIFY_NAME(CreateSwitch);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSwitch);
  CL_LISPIFY_NAME(CreateIndirectBr);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateIndirectBr);
  CL_LISPIFY_NAME(CreateResume);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateResume);
  CL_LISPIFY_NAME(CreateUnreachable);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateUnreachable);
  CL_LISPIFY_NAME(CreateNSWAdd);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNSWAdd);
  CL_LISPIFY_NAME(CreateNUWAdd);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNUWAdd);
  CL_LISPIFY_NAME(CreateFAdd);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFAdd);
  CL_LISPIFY_NAME(CreateSub);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSub);
  CL_LISPIFY_NAME(CreateNSWSub);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNSWSub);
  CL_LISPIFY_NAME(CreateNUWSub);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNUWSub);
  CL_LISPIFY_NAME(CreateFSub);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFSub);
  CL_LISPIFY_NAME(CreateMul);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateMul);
  CL_LISPIFY_NAME(CreateNSWMul);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNSWMul);
  CL_LISPIFY_NAME(CreateNUWMul);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNUWMul);
  CL_LISPIFY_NAME(CreateFMul);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFMul);
  CL_LISPIFY_NAME(CreateUDiv);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateUDiv);
  CL_LISPIFY_NAME(CreateExactUDiv);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateExactUDiv);
  CL_LISPIFY_NAME(CreateSDiv);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSDiv);
  CL_LISPIFY_NAME(CreateExactSDiv);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateExactSDiv);
  CL_LISPIFY_NAME(CreateFDiv);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFDiv);
  CL_LISPIFY_NAME(CreateURem);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateURem);
  CL_LISPIFY_NAME(CreateSRem);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSRem);
  CL_LISPIFY_NAME(CreateFRem);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFRem);
  CL_LISPIFY_NAME(CreateNeg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNeg);
  CL_LISPIFY_NAME(CreateNSWNeg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNSWNeg);
  CL_LISPIFY_NAME(CreateNUWNeg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNUWNeg);
  CL_LISPIFY_NAME(CreateFNeg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFNeg);
  CL_LISPIFY_NAME(CreateNot);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateNot);
  CL_LISPIFY_NAME(CreateAlloca);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (AllocaInst* (IRBuilder_O::ExternalType::*)(llvm::Type *, llvm::Value *,
                           const Twine &))&IRBuilder_O::ExternalType::CreateAlloca);
  CL_LISPIFY_NAME(CreateStore);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::StoreInst* (llvm::IRBuilder<llvm::ConstantFolder, llvm::IRBuilderDefaultInserter>::*)(llvm::Value *Val, llvm::Value *Ptr, bool isVolatile)) &IRBuilder_O::ExternalType::CreateStore);
  CL_LISPIFY_NAME(CreateFence);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFence);
  CL_LISPIFY_NAME(CreateAtomicCmpXchg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateAtomicCmpXchg);
  CL_LISPIFY_NAME(CreateAtomicRMW);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateAtomicRMW);
CL_LISPIFY_NAME(CreateConstGEP1-32);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value *(IRBuilder_O::ExternalType::*)(llvm::Value *Ptr, unsigned Idx0, const llvm::Twine &Name )) &IRBuilder_O::ExternalType::CreateConstGEP1_32);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP1-32);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_32);
  CL_LISPIFY_NAME(CreateConstGEP2-32);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstGEP2_32);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP2-32);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP2_32);
  CL_LISPIFY_NAME(CreateConstGEP1-64);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value* (IRBuilder_O::ExternalType::*)(llvm::Value *, uint64_t, const llvm::Twine &))&IRBuilder_O::ExternalType::CreateConstGEP1_64);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP1-64);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value* (IRBuilder_O::ExternalType::*)(llvm::Value *, uint64_t, const llvm::Twine &))&IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_64);
//  CL_LISPIFY_NAME(CreateConstGEP2-64);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstGEP2_64);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP2-64);
CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value* (IRBuilder_O::ExternalType::*)(llvm::Value *, uint64_t, uint64_t, const llvm::Twine &))&IRBuilder_O::ExternalType::CreateConstInBoundsGEP2_64);
  CL_LISPIFY_NAME(CreateStructGEP);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value *(IRBuilder_O::ExternalType::*)(llvm::Type* Type, llvm::Value *Ptr, unsigned Idx0, const llvm::Twine &Name )) &IRBuilder_O::ExternalType::CreateStructGEP);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value* (IRBuilder_O::ExternalType::*)(llvm::Value *Ptr, unsigned Idx, const llvm::Twine&))&IRBuilder_O::ExternalType::CreateStructGEP);
  CL_LISPIFY_NAME(CreateGlobalStringPtr);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateGlobalStringPtr);
  CL_LISPIFY_NAME(CreateTrunc);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateTrunc);
  CL_LISPIFY_NAME(CreateZExt);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateZExt);
  CL_LISPIFY_NAME(CreateSExt);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSExt);
  CL_LISPIFY_NAME(CreateFPToUI);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFPToUI);
  CL_LISPIFY_NAME(CreateFPToSI);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFPToSI);
  CL_LISPIFY_NAME(CreateUIToFP);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateUIToFP);
  CL_LISPIFY_NAME(CreateSIToFP);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSIToFP);
  CL_LISPIFY_NAME(CreateFPTrunc);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFPTrunc);
  CL_LISPIFY_NAME(CreateFPExt);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFPExt);
  CL_LISPIFY_NAME(CreatePtrToInt);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreatePtrToInt);
  CL_LISPIFY_NAME(CreateIntToPtr);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateIntToPtr);
  CL_LISPIFY_NAME(CreateBitCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateBitCast);
  CL_LISPIFY_NAME(CreateZExtOrBitCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateZExtOrBitCast);
  CL_LISPIFY_NAME(CreateSExtOrBitCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSExtOrBitCast);
  CL_LISPIFY_NAME(CreateTruncOrBitCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateTruncOrBitCast);
  CL_LISPIFY_NAME(CreateCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateCast);
  CL_LISPIFY_NAME(CreatePointerCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreatePointerCast);
  CL_LISPIFY_NAME(CreateFPCast);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFPCast);
  CL_LISPIFY_NAME(CreateICmpEQ);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpEQ);
  CL_LISPIFY_NAME(CreateICmpNE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpNE);
  CL_LISPIFY_NAME(CreateICmpUGT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpUGT);
  CL_LISPIFY_NAME(CreateICmpUGE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpUGE);
  CL_LISPIFY_NAME(CreateICmpULT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpULT);
  CL_LISPIFY_NAME(CreateICmpULE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpULE);
  CL_LISPIFY_NAME(CreateICmpSGT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpSGT);
  CL_LISPIFY_NAME(CreateICmpSGE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpSGE);
  CL_LISPIFY_NAME(CreateICmpSLT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpSLT);
  CL_LISPIFY_NAME(CreateICmpSLE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmpSLE);
  CL_LISPIFY_NAME(CreateFCmpOEQ);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpOEQ);
  CL_LISPIFY_NAME(CreateFCmpOGT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpOGT);
  CL_LISPIFY_NAME(CreateFCmpOGE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpOGE);
  CL_LISPIFY_NAME(CreateFCmpOLT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpOLT);
  CL_LISPIFY_NAME(CreateFCmpOLE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpOLE);
  CL_LISPIFY_NAME(CreateFCmpONE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpONE);
  CL_LISPIFY_NAME(CreateFCmpORD);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpORD);
  CL_LISPIFY_NAME(CreateFCmpUNO);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpUNO);
  CL_LISPIFY_NAME(CreateFCmpUEQ);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpUEQ);
  CL_LISPIFY_NAME(CreateFCmpUGT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpUGT);
  CL_LISPIFY_NAME(CreateFCmpUGE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpUGE);
  CL_LISPIFY_NAME(CreateFCmpULT);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpULT);
  CL_LISPIFY_NAME(CreateFCmpULE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpULE);
  CL_LISPIFY_NAME(CreateFCmpUNE);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmpUNE);
  CL_LISPIFY_NAME(CreateICmp);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateICmp);
  CL_LISPIFY_NAME(CreateFCmp);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFCmp);
  CL_LISPIFY_NAME(CreatePHI);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreatePHI);
CL_LISPIFY_NAME(CreateCallArrayRef);
CL_LAMBDA(irbuilder callee args name &optional (fpmathtag nil));
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::CallInst *(IRBuilder_O::ExternalType::*)(llvm::Value *Callee, llvm::ArrayRef<llvm::Value *> Args, const llvm::Twine &Name, llvm::MDNode* FPMathTag ))&IRBuilder_O::ExternalType::CreateCall);


// (llvm::FunctionType *FTy, Value *Callee, ArrayRef< Value * > Args, const Twine &Name="", MDNode *FPMathTag=nullptr)
CL_LISPIFY_NAME(CreateCallFunctionPointer);
CL_LAMBDA(irbuilder function_type callee args name &optional (fpmathtag nil));
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::CallInst *(IRBuilder_O::ExternalType::*)(llvm::FunctionType *FTy, llvm::Value *Callee, llvm::ArrayRef<llvm::Value *> Args, const llvm::Twine &Name, llvm::MDNode* FPMathTag ))&IRBuilder_O::ExternalType::CreateCall);


CL_LISPIFY_NAME(CreateSelect);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateSelect);
  CL_LISPIFY_NAME(CreateVAArg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateVAArg);
  CL_LISPIFY_NAME(CreateExtractElement);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value*(IRBuilder_O::ExternalType::*) (llvm::Value *Vec, llvm::Value* Idx, const llvm::Twine &Name) )&IRBuilder_O::ExternalType::CreateExtractElement);
  CL_LISPIFY_NAME(CreateInsertElement);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value*(IRBuilder_O::ExternalType::*) (llvm::Value *Vec, llvm::Value *NewElt, llvm::Value* Idx, const llvm::Twine &Name) )&IRBuilder_O::ExternalType::CreateInsertElement);
CL_LISPIFY_NAME(CreateShuffleVector);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value*(IRBuilder_O::ExternalType::*) (llvm::Value *V1, llvm::Value *V2, llvm::Value *Mask, const llvm::Twine &Name) ) &IRBuilder_O::ExternalType::CreateShuffleVector);
CL_LISPIFY_NAME(CreateLandingPad);
CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateLandingPad);
  CL_LISPIFY_NAME(CreateIsNull);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateIsNull);
  CL_LISPIFY_NAME(CreateIsNotNull);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateIsNotNull);
  CL_LISPIFY_NAME(CreatePtrDiff);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreatePtrDiff);
  CL_LISPIFY_NAME(CreateBinOp);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateBinOp);
CL_LISPIFY_NAME(CreateShl_value_value);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &, bool, bool) )&IRBuilder_O::ExternalType::CreateShl);
CL_LISPIFY_NAME(CreateShl_value_apint);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const llvm::Twine &, bool, bool) )&IRBuilder_O::ExternalType::CreateShl);
CL_LISPIFY_NAME(CreateShl_value_uint64);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine &, bool, bool) )&IRBuilder_O::ExternalType::CreateShl);
CL_LISPIFY_NAME(CreateLShr_value_value);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &, bool) )&IRBuilder_O::ExternalType::CreateLShr);
CL_LISPIFY_NAME(CreateLShr_value_apint);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const llvm::Twine &, bool) )&IRBuilder_O::ExternalType::CreateLShr);
CL_LISPIFY_NAME(CreateLShr_value_uint64);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine &, bool) )&IRBuilder_O::ExternalType::CreateLShr);
CL_LISPIFY_NAME(CreateAShr_value_value);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &, bool) )&IRBuilder_O::ExternalType::CreateAShr);
CL_LISPIFY_NAME(CreateAShr_value_apint);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const llvm::Twine &, bool) )&IRBuilder_O::ExternalType::CreateAShr);
CL_LISPIFY_NAME(CreateAShr_value_uint64);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine &, bool) )&IRBuilder_O::ExternalType::CreateAShr);
CL_LISPIFY_NAME(CreateAnd_value_value);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateAnd);
CL_LISPIFY_NAME(CreateAnd_value_apint);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateAnd);
CL_LISPIFY_NAME(CreateAnd_value_uint64);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateAnd);
CL_LISPIFY_NAME(CreateOr_value_value);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateOr);
CL_LISPIFY_NAME(CreateOr_value_apint);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateOr);
CL_LISPIFY_NAME(CreateOr_value_uint64);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateOr);
CL_LISPIFY_NAME(CreateXor_value_value);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateXor);
CL_LISPIFY_NAME(CreateXor_value_apint);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateXor);
CL_LISPIFY_NAME(CreateXor_value_uint64);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateXor);
CL_LISPIFY_NAME(CreateLoad_value_twine);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::LoadInst *(IRBuilder_O::ExternalType::*) (llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateLoad);
CL_LISPIFY_NAME(CreateLoad_value_bool_twine);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::LoadInst *(IRBuilder_O::ExternalType::*) (llvm::Value *, bool, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateLoad);
CL_LISPIFY_NAME(CreateGEP0);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateGEP);
CL_LISPIFY_NAME(CreateGEPArray);
 CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::ArrayRef<llvm::Value *>, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateGEP);

CL_LISPIFY_NAME(CreateInBoundsGEPType);
CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::Value *(IRBuilder_O::ExternalType::*) (llvm::Type *, llvm::Value *, llvm::ArrayRef<llvm::Value *>, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateInBoundsGEP);

;

}; // llvmo

namespace llvmo {

  CL_LISPIFY_NAME(hasStructRetAttr);
  CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasStructRetAttr);
  CL_LISPIFY_NAME(hasNoAliasAttr);
  CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasNoAliasAttr);
  CL_LISPIFY_NAME(hasNestAttr);
  CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasNestAttr);
  CL_LISPIFY_NAME(hasByValAttr);
  CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasByValAttr);;

;


}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(mdnode-get);
CL_DEFUN MDNode_sp MDNode_O::get(LLVMContext_sp context, core::List_sp values) {
  vector<llvm::Metadata *> valvec;
  for (auto cur : values) {
    llvm::Metadata *val = gc::As<Metadata_sp>(oCar(cur))->wrappedPtr();
    valvec.push_back(val);
  }
  llvm::MDNode *mdnode = llvm::MDNode::get(*context->wrappedPtr(), valvec);
  MDNode_sp omd = core::RP_Create_wrapped<llvmo::MDNode_O, llvm::MDNode *>(mdnode);
  return omd;
}



  SYMBOL_EXPORT_SC_(LlvmoPkg, mdnodeGet);

;

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(mdstring-get);
CL_DEFUN MDString_sp MDString_O::get(LLVMContext_sp context, core::String_sp str) {
  llvm::MDString *mdstr = llvm::MDString::get(*context->wrappedPtr(), str->get_std_string());
  MDString_sp omd = core::RP_Create_wrapped<llvmo::MDString_O, llvm::MDString *>(mdstr);
  return omd;
}



  SYMBOL_EXPORT_SC_(LlvmoPkg, mdnodeGet);

;

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(value-as-metadata-get);
CL_DEFUN ValueAsMetadata_sp ValueAsMetadata_O::get(Value_sp val) {
  llvm::ValueAsMetadata *mdstr = llvm::ValueAsMetadata::get(val->wrappedPtr());
  ValueAsMetadata_sp omd = core::RP_Create_wrapped<llvmo::ValueAsMetadata_O, llvm::ValueAsMetadata *>(mdstr);
  return omd;
}



  SYMBOL_EXPORT_SC_(LlvmoPkg, ValueAsMetadataGet);

;

}; // llvmo

namespace llvmo {

CL_DEFUN Function_sp llvm_sys__FunctionCreate(FunctionType_sp tysp, llvm::GlobalValue::LinkageTypes linkage, core::String_sp nsp, Module_sp modulesp) {
  translate::from_object<llvm::FunctionType *> ty(tysp);
  translate::from_object<llvm::Module *> m(modulesp);
  //        printf("%s:%d FunctionCreate %s with linkage %d\n", __FILE__, __LINE__, nsp->get().c_str(), linkage);
  llvm::Function *func = llvm::Function::Create(ty._v, linkage, nsp->get_std_string(), m._v);
  Function_sp funcsp = gc::As<Function_sp>(translate::to_object<llvm::Function *>::convert(func));
  return funcsp;
};

CL_LISPIFY_NAME(getParent);
CL_EXTERN_DEFMETHOD(Function_O,(llvm::Module *(llvm::Function::*)())&llvm::Function::getParent);
CL_LISPIFY_NAME("setHasUWTable");
CL_EXTERN_DEFMETHOD(Function_O,&llvm::Function::setHasUWTable);
CL_LISPIFY_NAME("setDoesNotThrow");
CL_EXTERN_DEFMETHOD(Function_O,&llvm::Function::setDoesNotThrow);
CL_LISPIFY_NAME("addFnAttr");
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(Attribute::AttrKind Kind))&llvm::Function::addFnAttr);
CL_LISPIFY_NAME("removeFnAttr");
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(Attribute::AttrKind Kind))&llvm::Function::removeFnAttr);
CL_LISPIFY_NAME("hasFnAttribute");
CL_EXTERN_DEFMETHOD(Function_O, (bool (llvm::Function::*)(Attribute::AttrKind Kind) const)&llvm::Function::hasFnAttribute);
CL_LISPIFY_NAME("addAttribute");
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(unsigned i, typename llvm::Attribute::AttrKind Attr))&llvm::Function::addAttribute);
CL_LISPIFY_NAME("addParamAttr");
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(unsigned i, typename llvm::Attribute::AttrKind Attr))&llvm::Function::addParamAttr);

CL_LISPIFY_NAME("setSubprogram");
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(llvm::DISubprogram*))&llvm::Function::setSubprogram);

CL_LISPIFY_NAME("addReturnAttr");
CL_DEFMETHOD void Function_O::addReturnAttr(typename llvm::Attribute::AttrKind Attr) {
  this->wrappedPtr()->addAttribute(llvm::AttributeList::ReturnIndex, Attr);
}

CL_LISPIFY_NAME("getArgumentList");
CL_DEFMETHOD core::List_sp Function_O::getArgumentList() {
  ql::list l;
  llvm::Function* func = this->wrappedPtr();
  for ( auto arg = func->arg_begin(); arg!=func->arg_end(); ++arg ) {
    l << translate::to_object<llvm::Argument*>::convert(arg);
  }
  return l.cons();
}

CL_DEFMETHOD bool Function_O::Function_equal(core::T_sp obj) const {
  if (gc::IsA<Function_sp>(obj)) {
    Function_sp other = gc::As_unsafe<Function_sp>(obj);
    return this->_ptr == other->_ptr;
  }
  return false;
}

string Function_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << this->wrappedPtr()->getName().data() << ">";
  return ss.str();
}

CL_LISPIFY_NAME("appendBasicBlock");
CL_DEFMETHOD void Function_O::appendBasicBlock(BasicBlock_sp basicBlock) {
  this->wrappedPtr()->getBasicBlockList().push_back(basicBlock->wrappedPtr());
}

CL_LISPIFY_NAME("getEntryBlock");
CL_DEFMETHOD BasicBlock_sp Function_O::getEntryBlock() const {
  return gc::As<BasicBlock_sp>(translate::to_object<llvm::BasicBlock*>::convert(&this->wrappedPtr()->getEntryBlock()));
}

CL_LISPIFY_NAME("basic-blocks");
CL_DEFMETHOD core::List_sp Function_O::basic_blocks() const {
  llvm::Function::BasicBlockListType& Blocks = this->wrappedPtr()->getBasicBlockList();
  ql::list result;
  for (llvm::Function::iterator b = this->wrappedPtr()->begin(), be = this->wrappedPtr()->end(); b != be; ++b) {
    llvm::BasicBlock& BB = *b;
    // Delete the basic block from the old function, and the list of blocks
    core::T_sp tbb = translate::to_object<llvm::BasicBlock*>::convert(&BB);
    result << tbb;
  }
  return result.cons();
}

  CL_LISPIFY_NAME(getFunctionType);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::getFunctionType);
  CL_LISPIFY_NAME(eraseFromParent);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::eraseFromParent);
  CL_LISPIFY_NAME(empty);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::empty);
  CL_LISPIFY_NAME(arg_size);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::arg_size);
  CL_LISPIFY_NAME(setDoesNotThrow);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::setDoesNotThrow);
  CL_LISPIFY_NAME(doesNotThrow);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::doesNotThrow);
  CL_LISPIFY_NAME(setDoesNotReturn);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::setDoesNotReturn);
  CL_LISPIFY_NAME(doesNotReturn);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::doesNotReturn);
  CL_LISPIFY_NAME(setPersonalityFn);
  CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::setPersonalityFn);
  CL_LISPIFY_NAME(addFnAttr);
  CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(llvm::Attribute::AttrKind)) & llvm::Function::addFnAttr);;
//CL_LISPIFY_NAME(addFnAttr1String);
//CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(llvm::StringRef)) & llvm::Function::addFnAttr);;
CL_LISPIFY_NAME(addFnAttr2String);
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(llvm::StringRef,llvm::StringRef)) & llvm::Function::addFnAttr);;
;

#if 0
CL_LISPIFY_NAME("setLiterals");
CL_DEFMETHOD void Function_O::setLiterals(core::LoadTimeValues_sp ltv) {
  this->_RunTimeValues = ltv;
}

CL_LISPIFY_NAME("literals");
CL_DEFMETHOD core::LoadTimeValues_sp Function_O::literals() const {
  return this->_RunTimeValues;
}
#endif

}; // llvmo

namespace llvmo {


CL_LISPIFY_NAME(getParent);
CL_EXTERN_DEFMETHOD(BasicBlock_O,(llvm::Function *(llvm::BasicBlock::*)())&llvm::BasicBlock::getParent);

CL_LISPIFY_NAME(getTerminator);
CL_EXTERN_DEFMETHOD(BasicBlock_O,(llvm::Instruction *(llvm::BasicBlock::*)())&llvm::BasicBlock::getTerminator);

CL_LAMBDA("context &optional (name \"\") parent basic-block");
CL_LISPIFY_NAME(basic-block-create);
CL_EXTERN_DEFUN((llvm::BasicBlock * (*)(llvm::LLVMContext &Context, const llvm::Twine &Name, llvm::Function *Parent, llvm::BasicBlock *InsertBefore)) &llvm::BasicBlock::Create );

;


CL_LISPIFY_NAME("BasicBlockEmpty");
CL_DEFMETHOD bool BasicBlock_O::empty() {
  return this->wrappedPtr()->empty();
}

CL_LISPIFY_NAME("BasicBlock-size");
CL_DEFMETHOD size_t BasicBlock_O::size() {
  return this->wrappedPtr()->size();
}

CL_LISPIFY_NAME("instructions");
CL_DEFMETHOD core::List_sp BasicBlock_O::instructions() const {
  ql::list result;
  llvm::BasicBlock* bb = const_cast<BasicBlock_O*>(this)->wrappedPtr();
  for ( auto ic = bb->begin(); ic != bb->end(); ++ic ) {
    llvm::Instruction& II = *ic;
    result << translate::to_object<llvm::Instruction*>::convert(&II);
  }
  return result.cons();
}

CL_LISPIFY_NAME("number-of-instructions");
CL_DEFMETHOD size_t BasicBlock_O::number_of_instructions() const {
  llvm::BasicBlock* bb = const_cast<BasicBlock_O*>(this)->wrappedPtr();
  return std::distance(bb->begin(),bb->end());
}

CL_LISPIFY_NAME("BasicBlockBack");
CL_DEFMETHOD Instruction_sp BasicBlock_O::back() {
  llvm::BasicBlock* bbp = this->wrappedPtr();
  llvm::Instruction &inst = bbp->back();
  Instruction_sp instruction = core::RP_Create_wrapped<Instruction_O, llvm::Instruction *>(&inst);
  return instruction;
}

}; // llvmo

namespace llvmo {

CL_LISPIFY_NAME(get_contained_type);
CL_EXTERN_DEFMETHOD(Type_O,&llvm::Type::getContainedType);

CL_DEFMETHOD LLVMContext_sp Type_O::getContext() const {
  return gc::As<LLVMContext_sp>(translate::to_object<llvm::LLVMContext&>::convert(this->wrappedPtr()->getContext()));
}

CL_DEFMETHOD bool Type_O::Type_equal(core::T_sp obj) const {
  if (Type_sp t = obj.asOrNull<Type_O>()) {
    return t->_ptr == this->_ptr;
  }
  return false;
}

string Type_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  string str;
  llvm::raw_string_ostream ro(str);
  this->wrappedPtr()->print(ro);
  ss << ro.str() << ">";
  return ss.str();
}

CL_LAMBDA((self llvm-sys::type) &optional (addressSpace 0));
CL_DOCSTRING("Return a PointerType to the llvm Type");
CL_LISPIFY_NAME("type-get-pointer-to");
CL_DEFMETHOD PointerType_sp Type_O::getPointerTo(int addressSpace) {
  llvm::PointerType *ptrType = this->wrappedPtr()->getPointerTo();
  return translate::to_object<llvm::PointerType *>::convert(ptrType);
}

CL_LISPIFY_NAME("getArrayNumElements");
CL_DEFMETHOD core::Integer_sp Type_O::getArrayNumElements() const {
  gc::Fixnum v64 = this->wrappedPtr()->getArrayNumElements();
  core::Integer_sp ival = core::Integer_O::create(v64);
  return ival;
}
CL_EXTERN_DEFMETHOD(Type_O,&llvm::Type::getSequentialElementType);

CL_LISPIFY_NAME(getSequentialElementType);
CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::getSequentialElementType);;

  CL_LISPIFY_NAME("type-get-void-ty");
  CL_EXTERN_DEFUN((llvm::Type * (*) (llvm::LLVMContext &C)) &llvm::Type::getVoidTy);
  CL_LISPIFY_NAME("type-get-float-ty");
  CL_EXTERN_DEFUN((llvm::Type * (*) (llvm::LLVMContext &C)) &llvm::Type::getFloatTy);
  CL_LISPIFY_NAME("type-get-double-ty");
  CL_EXTERN_DEFUN((llvm::Type * (*) (llvm::LLVMContext &C)) &llvm::Type::getDoubleTy);

  CL_LISPIFY_NAME("type-get-metadata-ty");
  CL_EXTERN_DEFUN((llvm::Type * (*) (llvm::LLVMContext &C)) &llvm::Type::getMetadataTy);

CL_LISPIFY_NAME("type-get-int-nty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C, unsigned N)) &llvm::Type::getIntNTy);
  CL_LISPIFY_NAME("type-get-int1-ty");
  CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C))&llvm::Type::getInt1Ty);
  CL_LISPIFY_NAME("type-get-int8-ty");
  CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C))&llvm::Type::getInt8Ty);
  CL_LISPIFY_NAME("type-get-int16-ty");
  CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C))&llvm::Type::getInt16Ty);
  CL_LISPIFY_NAME("type-get-int32-ty");
  CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C))&llvm::Type::getInt32Ty);
  CL_LISPIFY_NAME("type-get-int64-ty");
  CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C))&llvm::Type::getInt64Ty);
  CL_LISPIFY_NAME("type-get-int128-ty");
  CL_EXTERN_DEFUN((llvm::IntegerType * (*) (llvm::LLVMContext &C))&llvm::Type::getInt128Ty);

  CL_LISPIFY_NAME("type-get-float-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getFloatPtrTy);
  CL_LISPIFY_NAME("type-get-double-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getDoublePtrTy);

  CL_LISPIFY_NAME("type-get-int-nptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getIntNPtrTy);
  CL_LISPIFY_NAME("type-get-int1-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getInt1PtrTy);
  CL_LISPIFY_NAME("type-get-int8-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getInt8PtrTy);
  CL_LISPIFY_NAME("type-get-int16-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getInt16PtrTy);
  CL_LISPIFY_NAME("type-get-int32-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getInt32PtrTy);
  CL_LISPIFY_NAME("type-get-int64-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*) (llvm::LLVMContext &C, unsigned AS))&llvm::Type::getInt64PtrTy);

;


}; // llvmo

namespace llvmo {
// I can't get the following to work yet
//CL_EXTERN_DEFMETHOD(FunctionType_O, &llvm::FunctionType::getReturnType);


CL_LAMBDA(result &optional params is-var-arg);
CL_LISPIFY_NAME(function-type-get);
CL_DEFUN core::T_sp FunctionType_O::get(core::T_sp result_type, core::T_sp params, core::T_sp is_var_arg) {
  translate::from_object<llvm::Type *> r(result_type);
  bool iva = is_var_arg.isTrue();
  llvm::FunctionType *result = NULL;
  if (params.nilp()) {
    result = llvm::FunctionType::get(r._v, iva);
  } else {
    vector<llvm::Type *> vparams;
    convert_sequence_types_to_vector(params, vparams);
    llvm::ArrayRef<llvm::Type *> p(vparams);
    result = llvm::FunctionType::get(r._v, p, iva);
  }
  return translate::to_object<llvm::FunctionType *>::convert(result);
};

;

};

namespace llvmo {


CL_LAMBDA(context &key elements name is-packed);
CL_LISPIFY_NAME(struct-type-create);
CL_DEFUN StructType_sp StructType_O::make(LLVMContext_sp context, core::T_sp elements, core::String_sp name, core::T_sp isPacked) {
  llvm::StructType *result = NULL;
  translate::from_object<llvm::StringRef> srname(name);
  if (elements.notnilp()) {
    vector<llvm::Type *> velements;
    convert_sequence_types_to_vector(elements, velements);
    llvm::ArrayRef<llvm::Type *> p(velements);
    result = llvm::StructType::create(*(context->wrappedPtr()), p, srname._v, isPacked.isTrue());
  } else {
    result = llvm::StructType::create(*(context->wrappedPtr()), srname._v);
  }
  return translate::to_object<llvm::StructType *>::convert(result);
}

CL_LISPIFY_NAME(struct-type-get);
CL_DEFUN StructType_sp StructType_O::get(LLVMContext_sp context, core::T_sp elements, bool isPacked) {
  llvm::StructType *result = NULL;
  if (elements.notnilp()) {
    vector<llvm::Type *> velements;
    convert_sequence_types_to_vector(elements, velements);
    llvm::ArrayRef<llvm::Type *> p(velements);
    result = llvm::StructType::get(*(context->wrappedPtr()), p, isPacked);
  } else {
    result = llvm::StructType::get(*(context->wrappedPtr()), isPacked);
  }
  return translate::to_object<llvm::StructType *>::convert(result);
}

CL_LISPIFY_NAME("setBody");
CL_DEFMETHOD void StructType_O::setBody(core::T_sp elements, core::T_sp isPacked) {
  llvm::StructType *st = this->wrapped();
  if (elements.notnilp()) {
    vector<llvm::Type *> velements;
    convert_sequence_types_to_vector(elements, velements);
    llvm::ArrayRef<llvm::Type *> p(velements);
    st->setBody(p, isPacked.isTrue());
  }
}

;

};

namespace llvmo {

CL_LAMBDA(element-type num-elements);
CL_LISPIFY_NAME(array-type-get);
CL_DEFUN ArrayType_sp ArrayType_O::get(Type_sp elementType, uint64_t numElements) {
  ArrayType_sp at = ArrayType_O::create();
  llvm::ArrayType *llvm_at = llvm::ArrayType::get(elementType->wrappedPtr(), numElements);
  at->set_wrapped(llvm_at);
  return at;
}



;

};

namespace llvmo {


CL_LAMBDA(element-type &optional (address-space 0));
CL_LISPIFY_NAME(pointer-type-get);
CL_DEFUN PointerType_sp PointerType_O::get(Type_sp elementType, uint addressSpace) {
  PointerType_sp at = PointerType_O::create();
  llvm::PointerType *llvm_at = llvm::PointerType::get(elementType->wrappedPtr(), addressSpace);
  at->set_wrapped(llvm_at);
  return at;
}


;

};

namespace llvmo {

CL_LAMBDA(time)
CL_DEFUN void llvm_sys__accumulate_llvm_usage_seconds(double time)
{
  core::DoubleFloat_sp df = core::DoubleFloat_O::create(time);
  _sym_STARmostRecentLlvmFinalizationTimeSTAR->setf_symbolValue(df);
  double accTime = clasp_to_double(_sym_STARaccumulatedLlvmFinalizationTimeSTAR->symbolValue());
  accTime += time;
  _sym_STARaccumulatedLlvmFinalizationTimeSTAR->setf_symbolValue(core::DoubleFloat_O::create(accTime));
  int num = unbox_fixnum(gc::As<core::Fixnum_sp>(_sym_STARnumberOfLlvmFinalizationsSTAR->symbolValue()));
  ++num;
  _sym_STARnumberOfLlvmFinalizationsSTAR->setf_symbolValue(core::make_fixnum(num));
}

void finalizeEngineAndTime(llvm::ExecutionEngine *engine) {
  core::LightTimer timer;
  timer.start();
  engine->finalizeObject();
  timer.stop();
  double thisTime = timer.getAccumulatedTime();
  llvm_sys__accumulate_llvm_usage_seconds(thisTime);
}


CL_DEFUN void finalizeEngineAndRegisterWithGcAndRunMainFunctions(ExecutionEngine_sp oengine) {
  // Stuff to support MCJIT
    llvm::ExecutionEngine *engine = oengine->wrappedPtr();
#ifdef DEBUG_STARTUP
    printf("%s:%d Entered %s\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
    finalizeEngineAndTime(engine);
    engine->runStaticConstructorsDestructors(false);
    if ( core::startup_functions_are_waiting() ) {
      core::startup_functions_invoke(NULL);
    } else {
      SIMPLE_ERROR(BF("There were no startup functions to invoke\n"));
    }
#ifdef DEBUG_STARTUP
    printf("%s:%d Leaving %s\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
  }


/*! Return (values target nil) if successful or (values nil error-message) if not */
  CL_DEFUN core::T_mv TargetRegistryLookupTarget(const std::string &ArchName, Triple_sp triple) {
    string message;
    llvm::Target *target = const_cast<llvm::Target *>(llvm::TargetRegistry::lookupTarget(ArchName, *triple->wrappedPtr(), message));
    if (target == NULL) {
      return Values(_Nil<core::T_O>(), core::SimpleBaseString_O::make(message));
    }
    Target_sp targeto = core::RP_Create_wrapped<Target_O, llvm::Target *>(target);
    return Values(targeto, _Nil<core::T_O>());
  }

/*! Return (values target nil) if successful or (values nil error-message) if not */
  CL_LISPIFY_NAME(TargetRegistryLookupTarget.string);
  CL_DEFUN core::T_mv TargetRegistryLookupTarget_string(const std::string& Triple) {
    string message;
    llvm::Target *target = const_cast<llvm::Target *>(llvm::TargetRegistry::lookupTarget(Triple,message));
    if (target == NULL) {
      return Values(_Nil<core::T_O>(), core::SimpleBaseString_O::make(message));
    }
    Target_sp targeto = core::RP_Create_wrapped<Target_O, llvm::Target *>(target);
    return Values(targeto, _Nil<core::T_O>());
  }

  SYMBOL_EXPORT_SC_(LlvmoPkg, verifyFunction);

  SYMBOL_SC_(LlvmoPkg, STARglobal_value_linkage_typesSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ExternalLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AvailableExternallyLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkOnceAnyLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkOnceODRLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, WeakAnyLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, WeakODRLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AppendingLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, InternalLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, PrivateLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkerPrivateLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkerPrivateWeakLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DLLImportLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, DLLExportLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ExternalWeakLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CommonLinkage);
  CL_BEGIN_ENUM(llvm::GlobalValue::LinkageTypes,_sym_STARglobal_value_linkage_typesSTAR, "llvm::GlobalValue::LinkageTypes");
  CL_VALUE_ENUM(_sym_ExternalLinkage, llvm::GlobalValue::ExternalLinkage);
  CL_VALUE_ENUM(_sym_AvailableExternallyLinkage, llvm::GlobalValue::AvailableExternallyLinkage);
  CL_VALUE_ENUM(_sym_LinkOnceAnyLinkage, llvm::GlobalValue::LinkOnceAnyLinkage);
  CL_VALUE_ENUM(_sym_LinkOnceODRLinkage, llvm::GlobalValue::LinkOnceODRLinkage);
      //	.value(_sym_LinkOnceODRAutoHideLinkage,llvm::GlobalValue::LinkOnceODRAutoHideLinkage)
  CL_VALUE_ENUM(_sym_WeakAnyLinkage, llvm::GlobalValue::WeakAnyLinkage);
  CL_VALUE_ENUM(_sym_WeakODRLinkage, llvm::GlobalValue::WeakODRLinkage);
  CL_VALUE_ENUM(_sym_AppendingLinkage, llvm::GlobalValue::AppendingLinkage);
  CL_VALUE_ENUM(_sym_InternalLinkage, llvm::GlobalValue::InternalLinkage);
  CL_VALUE_ENUM(_sym_PrivateLinkage, llvm::GlobalValue::PrivateLinkage);
      //	.value(_sym_LinkerPrivateLinkage,llvm::GlobalValue::LinkerPrivateLinkage)
      //	.value(_sym_LinkerPrivateWeakLinkage,llvm::GlobalValue::LinkerPrivateWeakLinkage)
      //	.value(_sym_DLLImportLinkage,llvm::GlobalValue::DLLImportLinkage)
      //	.value(_sym_DLLExportLinkage,llvm::GlobalValue::DLLExportLinkage)
  CL_VALUE_ENUM(_sym_ExternalWeakLinkage, llvm::GlobalValue::ExternalWeakLinkage);
  CL_VALUE_ENUM(_sym_CommonLinkage, llvm::GlobalValue::CommonLinkage);
  CL_END_ENUM(_sym_STARglobal_value_linkage_typesSTAR);

  SYMBOL_SC_(LlvmoPkg, STARglobal_ThreadLocalModesSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, NotThreadLocal);
  SYMBOL_EXPORT_SC_(LlvmoPkg, GeneralDynamicTLSModel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LocalDynamicTLSModel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, InitialExecTLSModel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LocalExecTLSModel);
  CL_BEGIN_ENUM(llvm::GlobalValue::ThreadLocalMode,_sym_STARglobal_ThreadLocalModesSTAR, "llvm::GlobalValue::ThreadLocalMode");
  CL_VALUE_ENUM(_sym_NotThreadLocal, llvm::GlobalValue::NotThreadLocal);
  CL_VALUE_ENUM(_sym_GeneralDynamicTLSModel, llvm::GlobalValue::GeneralDynamicTLSModel);
  CL_VALUE_ENUM(_sym_LocalDynamicTLSModel, llvm::GlobalValue::LocalDynamicTLSModel);
  CL_VALUE_ENUM(_sym_InitialExecTLSModel, llvm::GlobalValue::InitialExecTLSModel);
  CL_VALUE_ENUM(_sym_LocalExecTLSModel, llvm::GlobalValue::LocalExecTLSModel);;
  CL_END_ENUM(_sym_STARglobal_ThreadLocalModesSTAR);


SYMBOL_EXPORT_SC_(LlvmoPkg, STARGlobalValueUnnamedAddrSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, None);
SYMBOL_EXPORT_SC_(LlvmoPkg, Local);
SYMBOL_EXPORT_SC_(LlvmoPkg, Global);
CL_BEGIN_ENUM(llvm::GlobalValue::UnnamedAddr,_sym_STARGlobalValueUnnamedAddrSTAR, "llvm::GlobalValue::UnnamedAddr");
CL_VALUE_ENUM(_sym_None, llvm::GlobalValue::UnnamedAddr::None);
CL_VALUE_ENUM(_sym_Local, llvm::GlobalValue::UnnamedAddr::Local);
CL_VALUE_ENUM(_sym_Global, llvm::GlobalValue::UnnamedAddr::Global);
CL_END_ENUM(_sym_STARGlobalValueUnnamedAddrSTAR);
//
  // Compiler optimization passes
  //
  //    core::af_def(LlvmoPkg,"createDebugIRPass",&llvmo::af_createDebugIRPass);
//  CL_LISPIFY_NAME(createAliasAnalysisCounterPass);
//  CL_EXTERN_DEFUN( &llvm::createAliasAnalysisCounterPass);
  CL_LISPIFY_NAME(createFunctionInliningPass);
CL_EXTERN_DEFUN((llvm::Pass * (*)(unsigned, unsigned,bool)) & llvm::createFunctionInliningPass);

  CL_LISPIFY_NAME(createAlwaysInlinerLegacyPass);
  CL_EXTERN_DEFUN( (llvm::Pass * (*)()) & llvm::createAlwaysInlinerLegacyPass);

  CL_LISPIFY_NAME(createAAEvalPass);
  CL_EXTERN_DEFUN( &llvm::createAAEvalPass);

//  CL_LISPIFY_NAME(createScalarEvolutionAliasAnalysisPass);
//  CL_EXTERN_DEFUN( &llvm::createScalarEvolutionAliasAnalysisPass);
  CL_LISPIFY_NAME(createLazyValueInfoPass);
  CL_EXTERN_DEFUN( &llvm::createLazyValueInfoPass);
  CL_LISPIFY_NAME(createInstCountPass);
  CL_EXTERN_DEFUN( &llvm::createInstCountPass);
  //    core::af_def(LlvmoPkg,"createDbgInfoPrinterPass",&llvm::createDbgInfoPrinterPass);
  CL_LISPIFY_NAME(createRegionInfoPass);
  CL_EXTERN_DEFUN( &llvm::createRegionInfoPass);

#if 0
CL_LISPIFY_NAME(createCountingFunctionInserterPass);
CL_EXTERN_DEFUN( &llvm::createCountingFunctionInserterPass);
#endif

CL_LISPIFY_NAME(createModuleDebugInfoPrinterPass);
CL_EXTERN_DEFUN( &llvm::createModuleDebugInfoPrinterPass);
CL_LISPIFY_NAME(createMemDepPrinter);
CL_EXTERN_DEFUN( &llvm::createMemDepPrinter);
  //    core::af_def(LlvmoPkg,"createInstructionCombiningPass",&llvm::createInstructionCombiningPass);
  //    core::af_def(LlvmoPkg,"createReassociatePass",&llvm::createReassociatePass);
  //    core::af_def(LlvmoPkg,"createPostDomTree",&llvm::createPostDomTree);
  CL_LISPIFY_NAME(InitializeNativeTarget);
  CL_EXTERN_DEFUN( &llvm::InitializeNativeTarget);

  CL_LISPIFY_NAME(createAggressiveDCEPass);
  CL_EXTERN_DEFUN( &llvm::createAggressiveDCEPass);
  CL_LISPIFY_NAME(createCFGSimplificationPass);
  CL_EXTERN_DEFUN( &llvm::createCFGSimplificationPass);
  CL_LISPIFY_NAME(createDeadStoreEliminationPass);
  CL_EXTERN_DEFUN( &llvm::createDeadStoreEliminationPass);
  CL_LISPIFY_NAME(createGVNPass);
  CL_EXTERN_DEFUN( &llvm::createGVNPass);
  CL_LISPIFY_NAME(createIndVarSimplifyPass);
  CL_EXTERN_DEFUN( &llvm::createIndVarSimplifyPass);
  CL_LISPIFY_NAME(createInstructionCombiningPass);
  CL_EXTERN_DEFUN( &llvm::createInstructionCombiningPass);
  CL_LISPIFY_NAME(createJumpThreadingPass);
  CL_EXTERN_DEFUN( &llvm::createJumpThreadingPass);
#if 0
CL_LISPIFY_NAME(createLICMPass);
  CL_EXTERN_DEFUN( &llvm::createLICMPass);
#endif
  CL_LISPIFY_NAME(createLoopDeletionPass);
  CL_EXTERN_DEFUN( &llvm::createLoopDeletionPass);
  CL_LISPIFY_NAME(createLoopIdiomPass);
  CL_EXTERN_DEFUN( &llvm::createLoopIdiomPass);
  CL_LISPIFY_NAME(createLoopRotatePass);
  CL_EXTERN_DEFUN( &llvm::createLoopRotatePass);
  CL_LISPIFY_NAME(createLoopUnrollPass);
  CL_EXTERN_DEFUN( &llvm::createLoopUnrollPass);
  CL_LISPIFY_NAME(createLoopUnswitchPass);
  CL_EXTERN_DEFUN( &llvm::createLoopUnswitchPass);
  CL_LISPIFY_NAME(createMemCpyOptPass);
  CL_EXTERN_DEFUN( &llvm::createMemCpyOptPass);
  CL_LISPIFY_NAME(createPromoteMemoryToRegisterPass);
  CL_EXTERN_DEFUN( &llvm::createPromoteMemoryToRegisterPass);
  CL_LISPIFY_NAME(createReassociatePass);
  CL_EXTERN_DEFUN( &llvm::createReassociatePass);
  CL_LISPIFY_NAME(createSCCPPass);
  CL_EXTERN_DEFUN( &llvm::createSCCPPass);
//  CL_LISPIFY_NAME(createScalarReplAggregatesPass);
//  CL_EXTERN_DEFUN( &llvm::createScalarReplAggregatesPass);
  //    core::af_def(LlvmoPkg,"createScalarReplAggregatesPassSSA",&llvm::createScalarReplAggregatesPassSSA);
  //    core::af_def(LlvmoPkg,"createScalarReplAggregatesPassWithThreshold",&llvm::createScalarReplAggregatesPassWithThreshold);
  //    core::af_def(LlvmoPkg,"createSimplifyLibCallsPass",&llvm::createSimplifyLibCallsPass);
  CL_LISPIFY_NAME(createTailCallEliminationPass);
  CL_EXTERN_DEFUN(&llvm::createTailCallEliminationPass);
  CL_LISPIFY_NAME(createConstantPropagationPass);
  CL_EXTERN_DEFUN(&llvm::createConstantPropagationPass);
  //    core::af_def(LlvmoPkg,"createDemoteMemoryToRegisterPass",&llvm::createDemoteMemoryToRegisterPass);
  CL_LISPIFY_NAME(createVerifierPass);
  CL_EXTERN_DEFUN(&llvm::createVerifierPass);
  CL_LISPIFY_NAME(createCorrelatedValuePropagationPass);
  CL_EXTERN_DEFUN(&llvm::createCorrelatedValuePropagationPass);
  CL_LISPIFY_NAME(createEarlyCSEPass);
  CL_EXTERN_DEFUN(&llvm::createEarlyCSEPass);
  CL_LISPIFY_NAME(createLowerExpectIntrinsicPass);
  CL_EXTERN_DEFUN(   &llvm::createLowerExpectIntrinsicPass);
//  CL_LISPIFY_NAME(createTypeBasedAliasAnalysisPass);
//  CL_EXTERN_DEFUN( &llvm::createTypeBasedAliasAnalysisPass);
//  CL_LISPIFY_NAME(createBasicAliasAnalysisPass);
//  CL_EXTERN_DEFUN( &llvm::createBasicAliasAnalysisPass);

  SYMBOL_EXPORT_SC_(LlvmoPkg, STARatomic_orderingSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, NotAtomic);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Unordered);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Monotonic);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Acquire);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Release);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AquireRelease);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SequentiallyConsistent);
  CL_BEGIN_ENUM(llvm::AtomicOrdering,_sym_STARatomic_orderingSTAR, "llvm::AtomicOrdering");
  CL_VALUE_ENUM(_sym_NotAtomic, llvm::AtomicOrdering::NotAtomic);
  CL_VALUE_ENUM(_sym_Unordered, llvm::AtomicOrdering::Unordered);
  CL_VALUE_ENUM(_sym_Monotonic, llvm::AtomicOrdering::Monotonic);
  CL_VALUE_ENUM(_sym_Acquire, llvm::AtomicOrdering::Acquire);
  CL_VALUE_ENUM(_sym_Release, llvm::AtomicOrdering::Release);
      //	.value(_sym_AquireRelease,llvm::AtomicOrdering::AquireRelease)
  CL_VALUE_ENUM(_sym_SequentiallyConsistent, llvm::AtomicOrdering::SequentiallyConsistent);;
  CL_END_ENUM(_sym_STARatomic_orderingSTAR);

  SYMBOL_EXPORT_SC_(LlvmoPkg, STARAtomicRMWInstBinOpSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Xchg);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Add);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Sub);
  SYMBOL_EXPORT_SC_(LlvmoPkg, And);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Nand);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Or);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Xor);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Max);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Min);
  SYMBOL_EXPORT_SC_(LlvmoPkg, UMax);
  SYMBOL_EXPORT_SC_(LlvmoPkg, UMin);
  CL_BEGIN_ENUM(llvm::AtomicRMWInst::BinOp,_sym_STARAtomicRMWInstBinOpSTAR, "llvm::AtomicRMWInst::BinOp");
  CL_VALUE_ENUM(_sym_Xchg, llvm::AtomicRMWInst::Xchg);
  CL_VALUE_ENUM(_sym_Add, llvm::AtomicRMWInst::Add);
  CL_VALUE_ENUM(_sym_Sub, llvm::AtomicRMWInst::Sub);
  CL_VALUE_ENUM(_sym_And, llvm::AtomicRMWInst::And);
  CL_VALUE_ENUM(_sym_Nand, llvm::AtomicRMWInst::Nand);
  CL_VALUE_ENUM(_sym_Or, llvm::AtomicRMWInst::Or);
  CL_VALUE_ENUM(_sym_Xor, llvm::AtomicRMWInst::Xor);
  CL_VALUE_ENUM(_sym_Max, llvm::AtomicRMWInst::Max);
  CL_VALUE_ENUM(_sym_Min, llvm::AtomicRMWInst::Min);
  CL_VALUE_ENUM(_sym_UMax, llvm::AtomicRMWInst::UMax);
  CL_VALUE_ENUM(_sym_UMin, llvm::AtomicRMWInst::UMin);;
  CL_END_ENUM(_sym_STARAtomicRMWInstBinOpSTAR);

  SYMBOL_EXPORT_SC_(LlvmoPkg, Add);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FAdd);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Sub);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FSub);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Mul);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FMul);
  SYMBOL_EXPORT_SC_(LlvmoPkg, UDiv);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SDiv);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FDiv);
  SYMBOL_EXPORT_SC_(LlvmoPkg, URem);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SRem);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FRem);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Shl);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LShr);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AShr);
  SYMBOL_EXPORT_SC_(LlvmoPkg, And);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Or);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Xor);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARBinaryOpsSTAR);
  CL_BEGIN_ENUM(llvm::Instruction::BinaryOps,_sym_STARBinaryOpsSTAR, "llvm::Instruction::BinaryOps");
  CL_VALUE_ENUM(_sym_Add, llvm::Instruction::Add);
  CL_VALUE_ENUM(_sym_FAdd, llvm::Instruction::FAdd);
  CL_VALUE_ENUM(_sym_Sub, llvm::Instruction::Sub);
  CL_VALUE_ENUM(_sym_FSub, llvm::Instruction::FSub);
  CL_VALUE_ENUM(_sym_Mul, llvm::Instruction::Mul);
  CL_VALUE_ENUM(_sym_FMul, llvm::Instruction::FMul);
  CL_VALUE_ENUM(_sym_UDiv, llvm::Instruction::UDiv);
  CL_VALUE_ENUM(_sym_SDiv, llvm::Instruction::SDiv);
  CL_VALUE_ENUM(_sym_FDiv, llvm::Instruction::FDiv);
  CL_VALUE_ENUM(_sym_URem, llvm::Instruction::URem);
  CL_VALUE_ENUM(_sym_SRem, llvm::Instruction::SRem);
  CL_VALUE_ENUM(_sym_FRem, llvm::Instruction::FRem);
  CL_VALUE_ENUM(_sym_Shl, llvm::Instruction::Shl);
  CL_VALUE_ENUM(_sym_LShr, llvm::Instruction::LShr);
  CL_VALUE_ENUM(_sym_AShr, llvm::Instruction::AShr);
  CL_VALUE_ENUM(_sym_And, llvm::Instruction::And);
  CL_VALUE_ENUM(_sym_Or, llvm::Instruction::Or);
  CL_VALUE_ENUM(_sym_Xor, llvm::Instruction::Xor);;
  CL_END_ENUM(_sym_STARBinaryOpsSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, Trunc);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ZExt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SExt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FPToUI);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FPToSI);
  SYMBOL_EXPORT_SC_(LlvmoPkg, UIToFP);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SIToFP);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FPTrunc);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FPExt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, PtrToInt);
  SYMBOL_EXPORT_SC_(LlvmoPkg, IntToPtr);
  SYMBOL_EXPORT_SC_(LlvmoPkg, BitCast);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARInstructionCastOpsSTAR);
  CL_BEGIN_ENUM(llvm::Instruction::CastOps,_sym_STARInstructionCastOpsSTAR, "llvm::Instruction::CastOps");
  CL_VALUE_ENUM(_sym_Trunc, llvm::Instruction::Trunc);
  CL_VALUE_ENUM(_sym_ZExt, llvm::Instruction::ZExt);
  CL_VALUE_ENUM(_sym_SExt, llvm::Instruction::SExt);
  CL_VALUE_ENUM(_sym_FPToUI, llvm::Instruction::FPToUI);
  CL_VALUE_ENUM(_sym_FPToSI, llvm::Instruction::FPToSI);
  CL_VALUE_ENUM(_sym_UIToFP, llvm::Instruction::UIToFP);
  CL_VALUE_ENUM(_sym_SIToFP, llvm::Instruction::SIToFP);
  CL_VALUE_ENUM(_sym_FPTrunc, llvm::Instruction::FPTrunc);
  CL_VALUE_ENUM(_sym_FPExt, llvm::Instruction::FPExt);
  CL_VALUE_ENUM(_sym_PtrToInt, llvm::Instruction::PtrToInt);
  CL_VALUE_ENUM(_sym_IntToPtr, llvm::Instruction::IntToPtr);
  CL_VALUE_ENUM(_sym_BitCast, llvm::Instruction::BitCast);;
  CL_END_ENUM(_sym_STARInstructionCastOpsSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_FALSE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_OEQ);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_OGT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_OGE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_OLT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_OLE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_ONE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_ORD);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_UNO);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_UEQ);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_UGT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_UGE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_ULT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_ULE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_UNE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_TRUE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FIRST_FCMP_PREDICATE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_PREDICATE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, FCMP_PREDICATE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_EQ);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_NE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_UGT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_UGE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_ULT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_ULE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_SGT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_SGE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_SLT);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ICMP_SLE);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARCmpInstPredicateSTAR);
  CL_BEGIN_ENUM(llvm::CmpInst::Predicate,_sym_STARCmpInstPredicateSTAR, "llvm::CmpInst::Predicate");
  CL_VALUE_ENUM(_sym_FCMP_FALSE, llvm::CmpInst::FCMP_FALSE);
  CL_VALUE_ENUM(_sym_FCMP_OEQ, llvm::CmpInst::FCMP_OEQ);
  CL_VALUE_ENUM(_sym_FCMP_OGT, llvm::CmpInst::FCMP_OGT);
  CL_VALUE_ENUM(_sym_FCMP_OGE, llvm::CmpInst::FCMP_OGE);
  CL_VALUE_ENUM(_sym_FCMP_OLT, llvm::CmpInst::FCMP_OLT);
  CL_VALUE_ENUM(_sym_FCMP_OLE, llvm::CmpInst::FCMP_OLE);
  CL_VALUE_ENUM(_sym_FCMP_ONE, llvm::CmpInst::FCMP_ONE);
  CL_VALUE_ENUM(_sym_FCMP_ORD, llvm::CmpInst::FCMP_ORD);
  CL_VALUE_ENUM(_sym_FCMP_UNO, llvm::CmpInst::FCMP_UNO);
  CL_VALUE_ENUM(_sym_FCMP_UEQ, llvm::CmpInst::FCMP_UEQ);
  CL_VALUE_ENUM(_sym_FCMP_UGT, llvm::CmpInst::FCMP_UGT);
  CL_VALUE_ENUM(_sym_FCMP_UGE, llvm::CmpInst::FCMP_UGE);
  CL_VALUE_ENUM(_sym_FCMP_ULT, llvm::CmpInst::FCMP_ULT);
  CL_VALUE_ENUM(_sym_FCMP_ULE, llvm::CmpInst::FCMP_ULE);
  CL_VALUE_ENUM(_sym_FCMP_UNE, llvm::CmpInst::FCMP_UNE);
  CL_VALUE_ENUM(_sym_FCMP_TRUE, llvm::CmpInst::FCMP_TRUE);
  CL_VALUE_ENUM(_sym_ICMP_EQ, llvm::CmpInst::ICMP_EQ);
  CL_VALUE_ENUM(_sym_ICMP_NE, llvm::CmpInst::ICMP_NE);
  CL_VALUE_ENUM(_sym_ICMP_UGT, llvm::CmpInst::ICMP_UGT);
  CL_VALUE_ENUM(_sym_ICMP_UGE, llvm::CmpInst::ICMP_UGE);
  CL_VALUE_ENUM(_sym_ICMP_ULT, llvm::CmpInst::ICMP_ULT);
  CL_VALUE_ENUM(_sym_ICMP_ULE, llvm::CmpInst::ICMP_ULE);
  CL_VALUE_ENUM(_sym_ICMP_SGT, llvm::CmpInst::ICMP_SGT);
  CL_VALUE_ENUM(_sym_ICMP_SGE, llvm::CmpInst::ICMP_SGE);
  CL_VALUE_ENUM(_sym_ICMP_SLT, llvm::CmpInst::ICMP_SLT);
  CL_VALUE_ENUM(_sym_ICMP_SLE, llvm::CmpInst::ICMP_SLE);;
  CL_END_ENUM(_sym_STARCmpInstPredicateSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, valid);
  SYMBOL_EXPORT_SC_(LlvmoPkg, makeStringGlobal);
  SYMBOL_EXPORT_SC_(LlvmoPkg, valuep);
  SYMBOL_EXPORT_SC_(LlvmoPkg, parseBitcodeFile);
  SYMBOL_EXPORT_SC_(LlvmoPkg, writeBitcodeToFile);
  SYMBOL_EXPORT_SC_(LlvmoPkg, writeIrToFile);
  SYMBOL_EXPORT_SC_(LlvmoPkg, llvm_value_p);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARmostRecentLlvmFinalizationTimeSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARaccumulatedLlvmFinalizationTimeSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARnumberOfLlvmFinalizationsSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARaccumulatedClangLinkTimeSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARnumberOfClangLinksSTAR);

  void initialize_llvmo_expose() {
    _sym_STARmostRecentLlvmFinalizationTimeSTAR->defparameter(core::DoubleFloat_O::create(0.0));
    _sym_STARaccumulatedLlvmFinalizationTimeSTAR->defparameter(core::DoubleFloat_O::create(0.0));
    _sym_STARnumberOfLlvmFinalizationsSTAR->defparameter(core::make_fixnum(0));
    _sym_STARaccumulatedClangLinkTimeSTAR->defparameter(core::DoubleFloat_O::create(0.0));
    _sym_STARnumberOfClangLinksSTAR->defparameter(core::make_fixnum(0));
  }

}; // llvmo


namespace llvmo {

using namespace llvm;
using namespace llvm::orc;

//#define MONITOR_JIT_MEMORY_MANAGER 1    // monitor SectionMemoryManager
//#define DUMP_OBJECT_FILES 1

std::atomic<size_t> fileNum;
void dumpObjectFile(size_t num, const char* start, size_t size) {
  std::stringstream filename;
  filename << "object-file-" << num << ".o";
  std::ofstream fout;
  fout.open(filename.str(), std::ios::out | std::ios::binary );
  fout.write(start,size);
  fout.close();
}


////////////////////////////////////////////////////////////
//
// Register Jitted object files with gdb
//
// Using interface described here:
//     https://sourceware.org/gdb/current/onlinedocs/gdb/JIT-Interface.html#JIT-Interface

typedef enum
{
  JIT_NOACTION = 0,
  JIT_REGISTER_FN,
  JIT_UNREGISTER_FN
} jit_actions_t;

struct jit_code_entry
{
  struct jit_code_entry *next_entry;
  struct jit_code_entry *prev_entry;
  const char *symfile_addr;
  uint64_t symfile_size;
};

struct jit_descriptor
{
  uint32_t version;
  /* This type should be jit_actions_t, but we use uint32_t
     to be explicit about the bitwidth.  */
  uint32_t action_flag;
  struct jit_code_entry *relevant_entry;
  struct jit_code_entry *first_entry;
};

/* GDB puts a breakpoint in this function.  */
void __attribute__((noinline)) __jit_debug_register_code () { }

/* Make sure to specify the version statically, because the
   debugger may check the version before we can set it.  */
struct jit_descriptor __jit_debug_descriptor = { 1, 0, 0, 0 };
mp::Mutex* global_jit_descriptor = NULL;

void register_object_file_with_gdb(void* object_file, size_t size)
{
    if (global_jit_descriptor==NULL) {
        global_jit_descriptor = new mp::Mutex(JITGDBIF_NAMEWORD);
    }
    global_jit_descriptor->lock();
    jit_code_entry* entry = (jit_code_entry*)malloc(sizeof(jit_code_entry));
    entry->symfile_addr = (const char*)object_file;
    entry->symfile_size = size;
    entry->prev_entry = __jit_debug_descriptor.relevant_entry;
    __jit_debug_descriptor.relevant_entry = entry;
    if (entry->prev_entry != NULL) {
        entry->prev_entry->next_entry = entry;
    } else {
        __jit_debug_descriptor.first_entry = entry;
    }
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_register_code();
    printf("%s:%d Registered object file at %p size: %lu\n", __FILE__, __LINE__, object_file, size );
    global_jit_descriptor->unlock();
};

};
SYMBOL_EXPORT_SC_(LlvmoPkg,make_StkSizeRecord);
SYMBOL_EXPORT_SC_(LlvmoPkg,make_StkMapRecord_Location);
SYMBOL_EXPORT_SC_(LlvmoPkg,make_StkMapRecord_LiveOut);
SYMBOL_EXPORT_SC_(LlvmoPkg,make_StkMapRecord);
SYMBOL_EXPORT_SC_(LlvmoPkg,make_StackMap);
SYMBOL_EXPORT_SC_(KeywordPkg,register);
SYMBOL_EXPORT_SC_(KeywordPkg,direct);
SYMBOL_EXPORT_SC_(KeywordPkg,indirect);
SYMBOL_EXPORT_SC_(KeywordPkg,constant);
SYMBOL_EXPORT_SC_(KeywordPkg,constant_index);



namespace llvmo {


#if 0
CL_DEFUN core::T_sp llvm_sys__vmmap()
{
  auto task = task_for_pid();
  return _Nil<core::T_O>();x
}
#endif


SYMBOL_EXPORT_SC_(LlvmoPkg,library);
};
    
namespace llvmo {

class ClaspSectionMemoryManager : public SectionMemoryManager {

  uint8_t* allocateCodeSection( uintptr_t Size, unsigned Alignment,
                                unsigned SectionID,
                                StringRef SectionName ) {
    uint8_t* ptr = this->SectionMemoryManager::allocateCodeSection(Size,Alignment,SectionID,SectionName);
    my_thread->_text_segment_start = (void*)ptr;
    my_thread->_text_segment_size = Size;
    my_thread->_text_segment_SectionID = SectionID;
    if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
      printf("%s", ( BF("%s:%d  allocateCodeSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s --> allocated at: %p\n") % __FILE__% __LINE__% Size% Alignment% SectionID% SectionName.str() % (void*)ptr ).str().c_str());
    }
    return ptr;
  }

#ifdef _TARGET_OS_DARWIN    
#define STACKMAPS_NAME "__llvm_stackmaps"
#elif defined(_TARGET_OS_LINUX)
#define STACKMAPS_NAME ".llvm_stackmaps"
#elif defined(_TARGET_OS_FREEBSD)
#define STACKMAPS_NAME ".llvm_stackmaps"
#else
#error "What is the name of stackmaps section on this OS??? __llvm_stackmaps or .llvm_stackmaps"
#endif
  uint8_t* allocateDataSection( uintptr_t Size, unsigned Alignment,
                                unsigned SectionID,
                                StringRef SectionName,
                                bool isReadOnly) {
    uint8_t* ptr = this->SectionMemoryManager::allocateDataSection(Size,Alignment,SectionID,SectionName,isReadOnly);
//    printf("%s:%d:%s allocateDataSection: %s size: %lu ptr -> %p\n", __FILE__, __LINE__, __FUNCTION__, SectionName.str().c_str(), Size, ptr);
    if (SectionName.str() == STACKMAPS_NAME) {
      my_thread->_stackmap = (uintptr_t)ptr;
      my_thread->_stackmap_size = (size_t)Size;
#if 0
      printf("%s:%d recorded __llvm_stackmap allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n" ,
             __FILE__, __LINE__,
             Size , Alignment, SectionID, SectionName.str().c_str(), isReadOnly, (void*)ptr );
#endif
      LOG(BF("STACKMAP_LOG  recorded __llvm_stackmap allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n") %
          Size% Alignment% SectionID% SectionName.str().c_str() % isReadOnly% (void*)ptr);
    }
    if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
      core::write_bf_stream(BF("%s:%d  allocateDataSection Size: %lu  Alignment: %u SectionId: %u SectionName: %s isReadOnly: %d --> allocated at: %p\n") % __FILE__% __LINE__% Size% Alignment% SectionID% SectionName.str() % isReadOnly% (void*)ptr );
    }
    return ptr;
  }

  void 	notifyObjectLoaded (RuntimeDyld &RTDyld, const object::ObjectFile &Obj) {
//    printf("%s:%d:%s entered\n", __FILE__, __LINE__, __FUNCTION__ );
#if 0
      // DONT DELETE DONT DELETE DONT DELETE
      // This is trying to use the gdb jit interface described here.
      // https://v8.dev/docs/gdb-jit
      // https://llvm.org/docs/DebuggingJITedCode.html
      // https://doc.ecoscentric.com/gnutools/doc/gdb/JIT-Interface.html
      // Example: https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=blob;f=gdb/testsuite/gdb.base/jit-main.c
      // Simple example: https://stackoverflow.com/questions/20046943/gdb-jit-interface-simpliest-example
      //
      // What I don't like about this is that the ObjectFile is going to be destroyed by the caller
      // and so I make a copy of the ObjectFile here.
      //
    {
      llvm::MemoryBufferRef mem = Obj.getMemoryBufferRef();
#if 1
          // Copy the ObjectFile - I can't be sure that it will persist once this callback returns
      void* obj_file_copy = (void*)malloc(mem.getBufferSize());
      memcpy( (void*)obj_file_copy,mem.getBufferStart(),mem.getBufferSize());
      register_object_file_with_gdb(obj_file_copy,mem.getBufferSize());
#else
          // Try using the ObjectFile directly - see the comment above about it persisting
      register_object_file_with_gdb((void*)mem.getBufferStart(),mem.getBufferSize());
#endif
    }
#endif
#if 0
    uintptr_t stackmap = 0;
    size_t stackmap_size = 0;
    for ( auto section : Obj.sections() ) {
      llvm::StringRef name;
      section.getName(name);
//      printf("%s:%d name: %s\n", __FILE__, __LINE__, name.str().c_str());
      if (name=="__llvm_stackmaps") {
        auto reloc = section.getRelocatedSection();
        stackmap = (uintptr_t)reloc->getAddress();
        stackmap_size = (size_t)reloc->getSize();
        printf("%s:%d Found stackmap at %p size: %lu\n", __FILE__, __LINE__, (void*) stackmap, stackmap_size);
      }
    }
    unsigned long section_size = 0;
    void* p_section = NULL;
#else
    unsigned long section_size = 0;
    void* p_section = NULL;
#if 0
    if (my_thread->_stackmap>0) {
      p_section = reinterpret_cast<void*>(my_thread->_stackmap);
      section_size = my_thread->_stackmap_size;
      my_thread->_stackmap = 0;
    }
#endif
#endif
    if (p_section!=nullptr) {
      printf("%s:%d LLVM_STACKMAPS  p_section@%p section_size=%lu\n", __FILE__, __LINE__, (void*)p_section, section_size );
//      core::register_llvm_stackmaps((uintptr_t)p_section,(uintptr_t)p_section+section_size);
    } else {
//      printf("%s:%d     Could not find LLVM_STACKMAPS\n", __FILE__, __LINE__ );
    }
    if (llvmo::_sym_STARdebugObjectFilesSTAR->symbolValue().notnilp()) {
      llvm::MemoryBufferRef mem = Obj.getMemoryBufferRef();
      core::write_bf_stream( BF("%s:%d notifyObjectLoaded was invoked\n") % __FILE__ % __LINE__ );
      core::write_bf_stream( BF("%s:%d      --> sizeof(ObjectFile) -> %lu  MemoryBufferRef start: %p   size: %lu\n") % __FILE__ % __LINE__ % sizeof(Obj) % (void*) mem.getBufferStart() % mem.getBufferSize() );
      void** words = (void**)(&Obj);
      core::write_bf_stream( BF("%s:%d      --> ObjectFile words:\n") % __FILE__% __LINE__ );
      core::write_bf_stream( BF("%s:%d            0x00: %18p %18p\n") % __FILE__% __LINE__% words[0]% words[1]);
      core::write_bf_stream( BF("%s:%d            0x10: %18p %18p\n") % __FILE__% __LINE__% words[2]% words[3]);
      core::write_bf_stream( BF("%s:%d            0x20: %18p %18p\n") % __FILE__% __LINE__% words[4]% words[5]);
    }
    if (llvmo::_sym_STARdumpObjectFilesSTAR->symbolValue().notnilp()) {
      llvm::MemoryBufferRef mem = Obj.getMemoryBufferRef();
      dumpObjectFile(fileNum++,mem.getBufferStart(),mem.getBufferSize());
    }
  }

  bool finalizeMemory(std::string* ErrMsg = nullptr) {
//    printf("%s:%d finalizeMemory\n", __FILE__, __LINE__);
    LOG(BF("STACKMAP_LOG %s entered\n") % __FUNCTION__ );
    bool result = this->SectionMemoryManager::finalizeMemory(ErrMsg);
    unsigned long section_size = 0;
    void* p_section = NULL;
    if (my_thread->_stackmap>0 && my_thread->_stackmap_size!=0) {
      p_section = reinterpret_cast<void*>(my_thread->_stackmap);
      section_size = my_thread->_stackmap_size;
      LOG(BF("STACKMAP_LOG   p_section@%p section_size=%lu\n") % (void*)p_section % section_size );
      core::register_llvm_stackmaps((uintptr_t)p_section,(uintptr_t)p_section+section_size,1);
//      core::process_llvm_stackmaps();
      my_thread->_stackmap = 0;
    } else {
//      printf("%s:%d     Could not find LLVM_STACKMAPS\n", __FILE__, __LINE__ );
    }
    return result;
  }




};




void register_symbol_with_libunwind(const std::string& name, uint64_t start, size_t size) {
#if defined(USE_LIBUNWIND) && (defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD))
  unw_dyn_info_t info;
  info.start_ip = start;
  info.end_ip = start+size;
  info.gp = 0;
  info.format = UNW_INFO_FORMAT_DYNAMIC;
  char* saved_name = (char*)malloc(name.size()+1);
  strncpy( saved_name, name.c_str(), name.size());
  saved_name[name.size()] = '\0';
  info.u.pi.name_ptr = saved_name;
  info.u.pi.segbase = 0;
  info.u.pi.table_len = 0;
  info.u.pi.table_data = 0;
  dyn_register(&info);
#endif
}

void save_symbol_info(const llvm::object::ObjectFile& object_file, const llvm::RuntimeDyld::LoadedObjectInfo& loaded_object_info)
{
  std::vector< std::pair< llvm::object::SymbolRef, uint64_t > > symbol_sizes = llvm::object::computeSymbolSizes(object_file);
#if defined(_TARGET_OS_DARWIN)
  std::string startup_name = "__claspObjectFileStartUp";
#endif
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
  std::string startup_name = "_claspObjectFileStartUp";
#endif
#if !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD) && !defined(_TARGET_OS_DARWIN)
#error You need to decide here
#endif

  for ( auto p : symbol_sizes ) {
    llvm::object::SymbolRef symbol = p.first;
    Expected<StringRef> expected_symbol_name = symbol.getName();
    if (expected_symbol_name) {
      auto &symbol_name = *expected_symbol_name;
      uint64_t size = p.second;
      std::string name(symbol_name.data());
      uint64_t address = symbol.getValue();
      Expected<llvm::object::section_iterator> expected_section_iterator = symbol.getSection();
      if (expected_section_iterator) {
        const llvm::object::SectionRef& section_ref = **expected_section_iterator;
        uint64_t section_address = loaded_object_info.getSectionLoadAddress(section_ref);
        if (((char*)section_address+address) != NULL ) {
          core::register_jitted_object(name,section_address+address,size);
          core::Cons_sp symbol_info = core::Cons_O::createList(core::make_fixnum((Fixnum)size),core::Pointer_O::create((void*)((char*)section_address+address)));
          register_symbol_with_libunwind(name,section_address+address,size);
          if ((!comp::_sym_jit_register_symbol.unboundp()) && comp::_sym_jit_register_symbol->fboundp()) {
            core::eval::funcall(comp::_sym_jit_register_symbol,core::SimpleBaseString_O::make(name),symbol_info);
            if (name == startup_name) {
              my_thread->_ObjectFileStartUp = (void*)((char*)section_address+address);
            }
//          printf("%s:%d  Registering symbol -> %s : %s\n", __FILE__, __LINE__, name.c_str(), _rep_(symbol_info).c_str() );
//          gc::As<core::HashTableEqual_sp>(comp::_sym_STARjit_saved_symbol_infoSTAR->symbolValue())->hash_table_setf_gethash(core::SimpleBaseString_O::make(name),symbol_info);
          }
        }
      }
    }
  }
}


CL_DEFUN core::T_sp llvm_sys__lookup_jit_symbol_info(void* ptr) {
  core::HashTableEqual_sp ht = gc::As<core::HashTableEqual_sp>(comp::_sym_STARjit_saved_symbol_infoSTAR->symbolValue());
  core::T_sp result = _Nil<core::T_O>();
  ht->map_while_true([ptr,&result] (core::T_sp key, core::T_sp value) -> bool {
                       if (value.consp()) {
                         core::T_sp address = value.unsafe_cons()->ocadr();
                         core::T_sp size = value.unsafe_cons()->ocar();
                         char* start = (char*)(gc::As<core::Pointer_sp>(address)->ptr());
                         if (size.fixnump()) {
                           char* end = start+size.unsafe_fixnum();
//          printf("%s:%d  Comparing ptr@%p to %p - %p\n", __FILE__, __LINE__, ptr, start, end);
                           if (start<=(char*)ptr && ptr<end) {
                             result = core::Cons_O::create(key,value);
//            printf("Found a match\n");
                             return false;
                           }
                         }
                       }
                       return true;
                     });
  return result;
}
          

/*! Remove the llvm.global_ctors array and any functions contained within it.
    The proper way to remove them is to never allow them into the Module.
    That would require a lot of C++ header file rearrangement.
    The ctors should have been called by the executable so these ctors are unused.
*/
CL_DEFUN void llvm_sys__remove_useless_global_ctors(Module_sp module) {
  llvm::Module* M = module->wrappedPtr();
  llvm::GlobalVariable* ctors = M->getGlobalVariable("llvm.global_ctors");
  if (ctors) {
    Value* init = ctors->getInitializer();
    ConstantArray *list = llvm::dyn_cast<ConstantArray>(init);
    std::vector<Function*> ctors_to_delete;
    if (list) {
      for ( unsigned i = 0, e= list->getNumOperands(); i != e; ++i ) {
        llvm::ConstantStruct* oneStruct = llvm::dyn_cast<llvm::ConstantStruct>(list->getOperand(i));
        if (!oneStruct) continue;
        llvm::Function* oneFunc = llvm::dyn_cast<llvm::Function>(oneStruct->getOperand(1));
        if (!oneFunc) continue;
//        printf("%s:%d  oneFunc[%u] = %s\n", __FILE__, __LINE__, i, oneFunc->getName().str().c_str());
        ctors_to_delete.push_back(oneFunc);
      }
    }
    ctors->eraseFromParent();
    for ( auto ctor : ctors_to_delete ) {
      ctor->eraseFromParent();
    }
  }
}
    
  


void removeAlwaysInlineFunctions(llvm::Module* M) {
  // Silently remove always-inline functions from the module
  std::vector<llvm::Function*> inline_funcs;
  for (auto &F : *M) {
    if (F.hasFnAttribute(llvm::Attribute::AlwaysInline)) {
      inline_funcs.push_back(&F);
    }
  }
  for ( auto f : inline_funcs) {
//    printf("%s:%d Erasing function: %s\n", __FILE__, __LINE__, f->getName().str().c_str());
    f->eraseFromParent();
  }
}

CL_DEFUN void llvm_sys__removeAlwaysInlineFunctions(llvm::Module* module)
{
  removeAlwaysInlineFunctions(module);
}


std::shared_ptr<llvm::Module> optimizeModule(std::shared_ptr<llvm::Module> M) {
#if 0
  // An attempt to move optimization into Common Lisp
  Module_sp om = Module_O::create();
  om->set_wrapped(&*M);
  om = core::eval::funcall(comp::_sym_optimize_module_for_compile,om);
  std::shared_ptr<llvm::Module> result(om->wrappedPtr());
  printf("%s:%d  Returning module\n", __FILE__, __LINE__ );
  return result;
#else
  if (comp::_sym_STARoptimization_levelSTAR->symbolValue().fixnump() &&
      comp::_sym_STARoptimization_levelSTAR->symbolValue().unsafe_fixnum() >= 2) {
  // Create a function pass manager.
    auto FPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(M.get());

  // Add some optimizations.
    printf("%s:%d Creating optimization passes - some of these have gone missing\n", __FILE__, __LINE__ );
    FPM->add(createInstructionCombiningPass());
    FPM->add(createReassociatePass());
    FPM->add(createNewGVNPass());
    FPM->add(createCFGSimplificationPass());
    FPM->add(createPromoteMemoryToRegisterPass());
    FPM->doInitialization();

  // !!!! I run this after inlining again -
  // - But if I don't run it here - it crashes when I try to clone the module for disassemble
  // Run the optimizations over all functions in the module being added to the JIT.
    for (auto &F : *M)
      FPM->run(F);
  
    llvm::legacy::PassManager my_passes;
    my_passes.add(llvm::createFunctionInliningPass(4096));
    my_passes.run(*M);

    // After inlining - run the optimizations over all functions again
    for (auto &F : *M)
      FPM->run(F);
  
  // Silently remove llvm.used functions if they are defined
  //     I may use this to prevent functions from being removed from the bitcode
  //     by clang before we need them.
    llvm::GlobalVariable* used = M->getGlobalVariable("llvm.used");
    if (used) {
      Value* init = used->getInitializer();
      used->eraseFromParent();
    }

    removeAlwaysInlineFunctions(&*M);

  }
  if ((!comp::_sym_STARsave_module_for_disassembleSTAR.unboundp()) &&
      comp::_sym_STARsave_module_for_disassembleSTAR->symbolValue().notnilp()) {
    //printf("%s:%d     About to save the module *save-module-for-disassemble*->%s\n",__FILE__, __LINE__, _rep_(comp::_sym_STARsave_module_for_disassembleSTAR->symbolValue()).c_str());
    llvm::Module* o = &*M;
    std::unique_ptr<llvm::Module> cm = llvm::CloneModule(*o);
    Module_sp module = core::RP_Create_wrapped<Module_O,llvm::Module*>(cm.release());
    comp::_sym_STARsaved_module_from_clasp_jitSTAR->setf_symbolValue(module);
  }
  // Check if we should dump the module for debugging
  {
    Module_sp module = core::RP_Create_wrapped<Module_O,llvm::Module*>(&*M);
    core::SimpleBaseString_sp label = core::SimpleBaseString_O::make("after-optimize");
    core::eval::funcall(comp::_sym_compile_quick_module_dump,module,label);
  }
  //printf("%s:%d  Done optimizeModule\n", __FILE__, __LINE__ );
  return M;
#endif
}


CL_DEFUN llvm::Module* llvm_sys__optimizeModule(llvm::Module* module)
{
  std::shared_ptr<llvm::Module> M(module);
  return &*optimizeModule(std::move(M));
}


SYMBOL_EXPORT_SC_(CorePkg,repl);

CL_DEFUN core::Function_sp llvm_sys__jitFinalizeReplFunction(ClaspJIT_sp jit, const string& replName, const string& startupName, const string& shutdownName, core::T_sp initialData) {
  // Stuff to support MCJIT
#ifdef DEBUG_MONITOR  
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    MONITOR(BF("startup llvm_sys__jitFinalizeReplFunction replName-> %s\n") % replName);
  }
#endif
  // As of May 2019 we use a static ctor to register the startup function
  // 
#if 0
  // Run the static constructors
  // The static constructor should call the startup function
  //  but ORC doesn't seem to do this as of llvm9
  //    So use the code below
  llvm::ExitOnError ExitOnErr;
  ExitOnErr(jit->_Jit->runConstructors());
  gctools::smart_ptr<core::ClosureWithSlots_O> functoid;
  if (core::startup_functions_are_waiting()) {
    core::T_O* replPtrRaw = core::startup_functions_invoke(initialData.raw_());
    core::CompiledClosure_fptr_type lisp_funcPtr = (core::CompiledClosure_fptr_type)(replPtrRaw);
    functoid = core::ClosureWithSlots_O::make_bclasp_closure( core::_sym_repl,
                                                              lisp_funcPtr,
                                                              kw::_sym_function,
                                                              _Nil<core::T_O>(),
                                                              _Nil<core::T_O>() );
  } else {
    printf("%s:%d No startup functions were available!!!\n", __FILE__, __LINE__);
    abort();
  }
#else
  // So the startupName is of an external linkage function that is
  // always unique 
  core::Pointer_sp startupPtr;
  if (startupName!="") {
    startupPtr = gc::As<core::Pointer_sp>(jit->lookup(jit->ES->getMainJITDylib(),startupName));
  }
  core::T_O* replPtrRaw = NULL;
  if (startupPtr && startupPtr->ptr()) {
    fnStartUp startup = reinterpret_cast<fnStartUp>(gc::As_unsafe<core::Pointer_sp>(startupPtr)->ptr());
//    printf("%s:%d:%s About to invoke startup @p=%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)startup);
    replPtrRaw = startup(initialData.raw_());
    if (replPtrRaw==NULL) {
        printf("%s:%d The return repl function pointer is NULL - we won't be able to call it\n", __FILE__, __LINE__ );
        abort();
    }
  } else {
      printf("%s:%d The startup function %s resolved to NULL - no code is available!!!\n", __FILE__, __LINE__, startupName.c_str());
      abort();
  }    
  gctools::smart_ptr<core::ClosureWithSlots_O> functoid;
  core::CompiledClosure_fptr_type lisp_funcPtr = (core::CompiledClosure_fptr_type)(replPtrRaw);
  functoid = core::ClosureWithSlots_O::make_bclasp_closure( core::_sym_repl,
                                                            lisp_funcPtr,
                                                            kw::_sym_function,
                                                            _Nil<core::T_O>(),
                                                            _Nil<core::T_O>() );
#endif  
  return functoid;
}
};


namespace llvm {
namespace orc {
class ClaspDynamicLibrarySearchGenerator : public DynamicLibrarySearchGenerator {

  ClaspDynamicLibrarySearchGenerator(sys::DynamicLibrary Dylib, char GlobalPrefix, SymbolPredicate Allow)
    : DynamicLibrarySearchGenerator(Dylib,GlobalPrefix,Allow) {};

  virtual Expected<SymbolNameSet> operator()(JITDylib &JD, const SymbolNameSet &Names) {
    printf("%s:%d In operator ()\n", __FILE__, __LINE__ );
    orc::SymbolNameSet Added;
    orc::SymbolMap NewSymbols;
 
    bool HasGlobalPrefix = (GlobalPrefix != '\0');
 
    for (auto &Name : Names) {
      if ((*Name).empty())
        continue;
 
      if (Allow && !Allow(Name))
        continue;
 
      if (HasGlobalPrefix && (*Name).front() != GlobalPrefix)
        continue;
 
      std::string Tmp((*Name).data() + HasGlobalPrefix,
                      (*Name).size() - HasGlobalPrefix);
      if (void *Addr = Dylib.getAddressOfSymbol(Tmp.c_str())) {
        if (core::_sym_STARdebug_symbol_lookupSTAR->symbolValue().notnilp()) {
          core::write_bf_stream(BF("Symbol |%s|  address: %p\n") % Tmp % Addr );
        }
        Added.insert(Name);
        NewSymbols[Name] = JITEvaluatedSymbol(
                                              static_cast<JITTargetAddress>(reinterpret_cast<uintptr_t>(Addr)),
                                              JITSymbolFlags::Exported);
      }
    }
 
   // Add any new symbols to JD. Since the generator is only called for symbols
   // that are not already defined, this will never trigger a duplicate
   // definition error, so we can wrap this call in a 'cantFail'.
    if (!NewSymbols.empty())
      cantFail(JD.define(absoluteSymbols(std::move(NewSymbols))));
 
    return Added;
  }
};


};
};



namespace llvmo {


void handleObjectEmitted(VModuleKey K, std::unique_ptr<MemoryBuffer> O) {
//  printf("%s:%d:%s Received emitted object buffer obj@%p\n", __FILE__,__LINE__, __FUNCTION__, (void*)O->getBufferStart());
}





CL_DEFUN ClaspJIT_sp llvm_sys__make_clasp_jit(DataLayout_sp data_layout)
{
  GC_ALLOCATE_VARIADIC(ClaspJIT_O,cj,data_layout->dataLayout());
  return cj;
}


ClaspJIT_O::ClaspJIT_O(const llvm::DataLayout& data_layout) :_DataLayout(data_layout) {
#if 0
    // Detect the host and set code model to small.
  llvm::ExitOnError ExitOnErr;
  auto JTMB = ExitOnErr(llvm::orc::JITTargetMachineBuilder::detectHost());
  core::SymbolToEnumConverter_sp converter = _sym_AttributeEnum->symbolValue().as<core::SymbolToEnumConverter_O>();
  
  JTMB.setCodeModel(CodeModel::Small);


  // Create an LLJIT instance with an ObjectLinkingLayer as the base layer.
  auto J = ExitOnErr(
                     llvm::orc::LLJITBuilder()
                     .setJITTargetMachineBuilder(std::move(JTMB))
                     .setObjectLinkingLayerCreator(
                                                   [&](llvm::orc::ExecutionSession &ES) {
                                                     return make_unique<ObjectLinkingLayer>(
                                                                                            ES, make_unique<llvm::jitlink::InProcessMemoryManager>());
                                                   })
                     .create());
#endif
  
  this->ES = new llvm::orc::ExecutionSession();
  auto GetMemMgr = []() { return llvm::make_unique<llvmo::ClaspSectionMemoryManager>(); };
#ifdef USE_JITLINKER
    #error "JITLinker support needed"
#else
  this->LinkLayer = new llvm::orc::RTDyldObjectLinkingLayer(*this->ES,GetMemMgr);
  this->LinkLayer->setProcessAllSections(true);
  this->LinkLayer->setNotifyLoaded( [&] (VModuleKey, const llvm::object::ObjectFile &Obj, const llvm::RuntimeDyld::LoadedObjectInfo &loadedObjectInfo) {
//                                      printf("%s:%d  NotifyLoaded ObjectFile@%p\n", __FILE__, __LINE__, &Obj);
                                      save_symbol_info(Obj,loadedObjectInfo);
                                    });
#endif
  auto JTMB = llvm::orc::JITTargetMachineBuilder::detectHost();
#if 0
  // Don't set the code model - the default should work fine.
  // If it doesn't - invoke the (cmp:code-model :jit xxx :compile-file-parallel yyy)
  // function
  core::SymbolToEnumConverter_sp converter = gc::As<core::SymbolToEnumConverter_sp>(llvmo::_sym_CodeModel->symbolValue());
  core::T_sp code_model_symbol = llvmo::_sym_STARdefault_code_modelSTAR->symbolValue();
  auto cm = converter->enumForSymbol<llvm::CodeModel::Model>(code_model_symbol);
  JTMB->setCodeModel(cm);
#endif
  this->Compiler = new llvm::orc::ConcurrentIRCompiler(*JTMB);
  this->CompileLayer = new llvm::orc::IRCompileLayer(*this->ES,*this->LinkLayer,*this->Compiler);
  //  printf("%s:%d Registering ClaspDynamicLibarySearchGenerator\n", __FILE__, __LINE__ );
  this->ES->getMainJITDylib().setGenerator(llvm::cantFail(ClaspDynamicLibrarySearchGenerator::GetForCurrentProcess(data_layout.getGlobalPrefix())));
}

ClaspJIT_O::~ClaspJIT_O()
{
  printf("%s:%d Shutdown the ClaspJIT\n", __FILE__, __LINE__);
}


CL_DEFMETHOD core::Pointer_sp ClaspJIT_O::lookup(JITDylib& dylib, const std::string& Name) {
//  printf("%s:%d:%s Name = %s\n", __FILE__, __LINE__, __FUNCTION__, Name.c_str());
  llvm::ExitOnError ExitOnErr;
//  llvm::ArrayRef<llvm::orc::JITDylib*>  dylibs(&this->ES->getMainJITDylib());
#if defined(_TARGET_OS_DARWIN)
  // gotta put a _ in front of the name on DARWIN but not Unixes? Why? Dunno.
  std::string mangledName = "_" + Name;
#endif
#if defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_FREEBSD)
  std::string mangledName = Name;
#endif

#if !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD) && !defined(_TARGET_OS_DARWIN)
#error You need to decide here
#endif

  llvm::Expected<llvm::JITEvaluatedSymbol> symbol = this->ES->lookup(llvm::orc::JITDylibSearchList({{&dylib,true}}),
                                                                     this->ES->intern(mangledName));   
  if (!symbol) {
    llvm::handleAllErrors(symbol.takeError(), [](const llvm::ErrorInfoBase &E) {
                                                errs() << "Symbolizer failed to get line: " << E.message() << "\n";
                                              });
#if 0
    std::stringstream ss;
    ss << __FILE__ <<":" <<__LINE__ << ":  " << symbol.takeError().message();
    printf("%s\n", ss.str().c_str());
    abort();
#endif
  }
//  printf("%s:%d:%s !!symbol -> %d  symbol->getAddress() -> %p\n", __FILE__, __LINE__, __FUNCTION__, !!symbol, (void*)symbol->getAddress());
  return core::Pointer_O::create((void*)symbol->getAddress());
}

CL_DEFMETHOD void ClaspJIT_O::addIRModule(Module_sp module, ThreadSafeContext_sp context) {
  std::unique_ptr<llvm::Module> umodule(module->wrappedPtr());
  llvm::ExitOnError ExitOnErr;
  ExitOnErr(this->CompileLayer->add(this->ES->getMainJITDylib(),llvm::orc::ThreadSafeModule(std::move(umodule),*context->wrappedPtr())));
}

void ClaspJIT_O::saveObjectFileInfo(const char* objectFileStart, size_t objectFileSize)
{
  ObjectFileInfo* ofi = new ObjectFileInfo();
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
    current = this->_ObjectFiles.load();
    ofi->_next = current;
    expected = current;
    this->_ObjectFiles.compare_exchange_strong(expected,ofi);
  } while (expected!=current);
}

CL_DOCSTRING(R"doc(Identify the object file whose generated code range containss the instruction-pointer.
Return NIL if none or (values offset-from-start object-file). The index-from-start is the number of bytes of the instruction-pointer from the start of the code range.)doc");
CL_DEFMETHOD core::T_mv ClaspJIT_O::objectFileForInstructionPointer(core::Pointer_sp instruction_pointer)
{
  ObjectFileInfo* cur = this->_ObjectFiles.load();
  size_t count;
  char* ptr = (char*)instruction_pointer->ptr();
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
      return Values(sectioned_address,object_file);
    }
    cur = cur->_next;
    count++;
  }
  return Values(_Nil<core::T_O>());
}

CL_DEFMETHOD size_t ClaspJIT_O::numberOfObjectFiles() {
  ObjectFileInfo* cur = this->_ObjectFiles.load();
  size_t count;
  while (cur) {
    cur = cur->_next;
    count++;
  }
  return count;
}


void ClaspJIT_O::addObjectFile(const char* rbuffer, size_t bytes,size_t startupID, JITDylib& dylib,  bool print )
{
  // Create an llvm::MemoryBuffer for the ObjectFile bytes
  if (print) core::write_bf_stream(BF("%s:%d Adding object file at %p  %lu bytes\n")  % __FILE__ % __LINE__  % (void*)rbuffer % bytes );
  llvm::StringRef sbuffer((const char*)rbuffer,bytes);
  llvm::StringRef name("buffer-name");
  std::unique_ptr<llvm::MemoryBuffer> mbuffer = llvm::MemoryBuffer::getMemBuffer(sbuffer,name,false);
  // Force the object file to be linked using MaterializationUnit::doMaterialize(...)
  if (print) core::write_bf_stream(BF("%s:%d Materializing\n") % __FILE__ % __LINE__ );
  auto erro = this->LinkLayer->add(dylib,std::move(mbuffer),this->ES->allocateVModule());
  if (erro) {
    printf("%s:%d Could not addObjectFile\n", __FILE__, __LINE__ );
  }
  core::T_mv startup_name_and_linkage = core::core__startup_function_name_and_linkage(startupID);
  std::string startup_name = gc::As<core::String_sp>(startup_name_and_linkage)->get_std_string();
  core::Pointer_sp startup = this->lookup(dylib,startup_name);
  // Now the my_thread thread local data structure will contain information about the new linked object file.
  this->saveObjectFileInfo(rbuffer,bytes);
  
  // Lookup the address of the ObjectFileStartUp function and invoke it
  void* thread_local_startup = startup->ptr();
  my_thread->_ObjectFileStartUp = NULL;
  if (thread_local_startup) {
    if (print) core::write_bf_stream(BF("%s:%d thread_local_startup -> %p\n") % __FILE__ % __LINE__ % (void*)thread_local_startup);
    fnStartUp startup = reinterpret_cast<fnStartUp>(thread_local_startup);
    core::T_O* replPtrRaw = startup(NULL);
  } else {
    if (print) core::write_bf_stream(BF("No startup function was defined\n"));
  }
  // Running the ObjectFileStartUp function registers the startup functions - now we can invoke them
  if (core::startup_functions_are_waiting()) {
    if (print) core::write_bf_stream(BF("%s:%d  startup functions are waiting - INVOKING\n") % __FILE__ % __LINE__ );
    core::startup_functions_invoke(NULL);
  } else {
    core::write_bf_stream(BF("%s:%d  No startup functions are waiting\n") % __FILE__ % __LINE__ );
  }
}

CL_DEFMETHOD JITDylib& ClaspJIT_O::getMainJITDylib() {
  return this->ES->getMainJITDylib();
}

CL_DEFMETHOD JITDylib_sp ClaspJIT_O::createAndRegisterJITDylib(const std::string& name) {
  JITDylib& dylib(this->ES->createJITDylib(name));
  dylib.setGenerator(llvm::cantFail(ClaspDynamicLibrarySearchGenerator::GetForCurrentProcess(this->_DataLayout.getGlobalPrefix())));
  JITDylib_sp dylib_sp = core::RP_Create_wrapped<JITDylib_O>(&dylib);
#if 0
  Cons_sp cell = core::Cons_O::create(dylib_sp,_Nil<core::T_O>());
  T_sp expected;
  T_sp current;
  // Use CAS to push the new JITDylib into the list of JITDylibs.
  do {
    current = _lisp->Roots._JITDylibs.load();
    expected = current;
    cell->rplacd(current);
    _lisp->Roots._JITDylibs.compare_exchange_strong(expected,gc::As_unsafe<core::T_sp>(cell));
  } while (expected != current);
#endif
  return dylib_sp;
}

CL_DOCSTRING(R"doc(Tell LLVM what LLVM_DEBUG messages to turn on. Pass a list of strings like \"dyld\" - which
turns on messages from RuntimeDyld.cpp if NDEBUG is NOT defined for the llvm build.)doc");
CL_DEFUN void llvm_sys__set_current_debug_types(core::List_sp types)
{
  using namespace llvm;
  size_t numStrings = core::cl__length(types);
  char** array = (char**)malloc(sizeof(char*)*numStrings);
  size_t index = 0;
  for ( auto cur : types ) {
    core::String_sp name = gc::As<core::String_sp>(CONS_CAR(cur));
    std::string sname(name->get_std_string());
    char* oneName = (char*)malloc(sname.size()+1);
    strncpy(oneName,sname.c_str(),sname.size());
    oneName[sname.size()] = '\0';
    array[index] = oneName;
    index++;
  };
  // Call setCurrentDebugTypes
  setCurrentDebugTypes((const char**)array,index);
  for ( size_t idx = 0; idx<index; ++idx ) {
    free(array[idx]);
  }
  free(array);
};  

}; // namespace llvmo

namespace llvmo { // ObjectFile_O

ObjectFile_sp ObjectFile_O::create(llvm::object::ObjectFile *ptr) {
  return core::RP_Create_wrapped<llvmo::ObjectFile_O, llvm::object::ObjectFile *>(ptr);
}

}; // namespace llvmo, ObjectFile_O

namespace llvmo { // SectionedAddress_O

SectionedAddress_sp SectionedAddress_O::create(uint64_t SectionIndex, uint64_t Address) {
  GC_ALLOCATE_VARIADIC(SectionedAddress_O, sa, SectionIndex, Address);
  return sa;
}
}; // namespace llvmo, SectionedAddress_O
