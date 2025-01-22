/*
    File: llvmoExpose.cc

*/

#define DEBUG_LEVEL_FULL

// #include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <string>
#include <clasp/core/foundation.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/snapshotSaveLoad.h>
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

#include <llvm/ADT/SmallString.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
// #include <llvm/ExecutionEngine/Orc/MachOPlatform.h>

namespace llvm {

namespace orc {
Error enableObjCRegistration(const char* PathToLibObjC);
}
}; // namespace llvm

#ifdef NDEBUG
#undef NDEBUG
#define private public
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#undef private
#define NDEBUG
#else
#define private public
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#undef private
#endif
#include <llvm/IR/LLVMContext.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/IR/DiagnosticPrinter.h>
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
#if LLVM_VERSION_MAJOR < 17
#include <llvm/ADT/Triple.h>
#else
#include <llvm/TargetParser/Triple.h>
#endif
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/AbstractCallSite.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Mangler.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/ExecutionEngine/JITLink/EHFrameSupport.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/CodeGen/TargetPassConfig.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Pass.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/AssemblyAnnotationWriter.h> // will be llvm/IR
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/DebugObjectManagerPlugin.h>
#include <llvm/ExecutionEngine/Orc/TargetProcess/RegisterEHFrames.h>
#include <llvm/ExecutionEngine/Orc/TargetProcess/JITLoaderGDB.h>
#include <llvm-c/Disassembler.h>
// #include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

#include <clasp/core/foundation.h>
#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispCallingConvention.h>
#include <clasp/core/package.h>
#include <clasp/core/debugger.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/pathname.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bignum.h>
#include <clasp/core/compiler.h>
#include <clasp/core/bformat.h>
#include <clasp/core/pointer.h>
#include <clasp/core/fli.h>
#include <clasp/core/array.h>
#include <clasp/gctools/gc_interface.fwd.h>
#include <clasp/llvmo/debugInfoExpose.h>
#include <clasp/llvmo/jit.h>
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

SYMBOL_EXPORT_SC_(LlvmoPkg, debugObjectFilesOff);
SYMBOL_EXPORT_SC_(LlvmoPkg, debugObjectFilesPrint);
SYMBOL_EXPORT_SC_(LlvmoPkg, debugObjectFilesPrintSave);

DebugObjectFilesEnum globalDebugObjectFiles;

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__debugObjectFiles(core::Symbol_sp sym) {
  if (sym == llvmo::_sym_debugObjectFilesOff) {
    globalDebugObjectFiles = DebugObjectFilesOff;
  } else if (sym == llvmo::_sym_debugObjectFilesPrint) {
    globalDebugObjectFiles = DebugObjectFilesPrint;
  } else if (sym == llvmo::_sym_debugObjectFilesPrintSave) {
    globalDebugObjectFiles = DebugObjectFilesPrintSave;
  } else {
    SIMPLE_ERROR("Argument must be one of {} in the llvm-sys package",
                 _rep_(core::Cons_O::createList(llvmo::_sym_debugObjectFilesOff, llvmo::_sym_debugObjectFilesPrint,
                                                llvmo::_sym_debugObjectFilesPrintSave)));
  }
}

std::string ensureUniqueMemoryBufferName(const std::string& name) {
  if (countObjectFileNames(name) > 0) {
    stringstream ss;
    ss << name << "-" << core::core__next_jit_compile_counter();
    return ensureUniqueMemoryBufferName(ss.str());
  }
  return name;
}

/*! Return the id from a name with the format xxxx#num
 */
size_t objectIdFromName(const std::string& name) {
  size_t pos = name.find_first_of("0123456789");
  size_t lpos = name.find_last_of("0123456789");
  std::string snum = name.substr(pos, lpos + 1);
  size_t num = std::stoull(snum);
  return num;
}

llvm::raw_pwrite_stream* llvm_stream(core::T_sp stream, llvm::SmallString<1024>& stringOutput, bool& stringOutputStream) {
  if (stream.isA<core::SynonymStream_O>())
    return llvm_stream(stream.as_unsafe<core::SynonymStream_O>()->stream(), stringOutput, stringOutputStream);

  if (stream.isA<core::TwoWayStream_O>())
    return llvm_stream(stream.as_unsafe<core::TwoWayStream_O>()->output_stream(), stringOutput, stringOutputStream);

  if (stream.isA<core::StringOutputStream_O>()) {
    stringOutputStream = true;
    return new llvm::raw_svector_ostream(stringOutput);
  }

  int fd = core::stream_file_descriptor(stream, core::StreamDirection::output);
  if (fd >= 0)
    return new llvm::raw_fd_ostream(fd, false, true);

  SIMPLE_ERROR("Illegal file type {} for llvm_stream", _rep_(stream));
}

DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__llvm_value_p(core::T_sp o) {
  if (o.nilp())
    return false;
  if (Value_sp v = o.asOrNull<Value_O>()) {
    (void)v;
    return true;
  }
  return false;
};

DOCGROUP(clasp);
CL_DEFUN std::string llvm_sys__get_default_target_triple() { return llvm::sys::getDefaultTargetTriple(); }

#ifdef CLASP_THREADS
mp::Mutex* global_disassemble_mutex = NULL;
#endif
#define CALLBACK_BUFFER_SIZE 1024
char global_LLVMSymbolLookupCallbackBuffer[CALLBACK_BUFFER_SIZE];

const char* my_LLVMSymbolLookupCallback(void* DisInfo, uint64_t ReferenceValue, uint64_t* ReferenceType, uint64_t ReferencePC,
                                        const char** ReferenceName) {
  const char* symbol;
  uintptr_t start, end;
  // This tells the disassembler not to print "# symbol stub:" or anything.
  *ReferenceType = LLVMDisassembler_ReferenceType_InOut_None;
  bool found = core::lookup_address((uintptr_t)ReferenceValue, symbol, start, end);
  //  printf("%s:%d:%s ReferenceValue->%p ReferencePC->%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)ReferenceValue,
  //  (void*)ReferencePC);
  if (found) {
    stringstream ss;
    ss << (void*)ReferenceValue << "{";
    ss << symbol;
    if (ReferenceValue != start) {
      ss << "+" << (ReferenceValue - start);
    }
    if (symbol[0] == LITERALS_NAME[0] && strlen(symbol) > strlen(LITERALS_NAME) &&
        strncmp(LITERALS_NAME, symbol + 1, strlen(LITERALS_NAME)) == 0) {
      ss << "[" << dbg_safe_repr(*(void**)ReferenceValue) << "]";
    }
    ss << "}";
    strcpy(global_LLVMSymbolLookupCallbackBuffer, ss.str().c_str());
    *ReferenceName = global_LLVMSymbolLookupCallbackBuffer;
    //    printf("%s:%d:%s Returning symbol-table result |%s|\n", __FILE__, __LINE__, __FUNCTION__, *ReferenceName);
    return *ReferenceName;
  }
  Dl_info data;
  int ret = dladdr((void*)ReferenceValue, &data);
  if (ret != 0) {
    stringstream ss;
    ss << data.dli_sname;
    if (ReferenceValue != (uintptr_t)data.dli_saddr) {
      ss << "+" << (ReferenceValue - (uintptr_t)data.dli_saddr);
    }
    strcpy(global_LLVMSymbolLookupCallbackBuffer, ss.str().c_str());
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
DOCGROUP(clasp);
CL_LAMBDA(target-triple start-address end-address);
CL_DEFUN void llvm_sys__disassemble_instructions(const std::string& striple, core::Pointer_sp start_address,
                                                 core::Pointer_sp end_address) {
#define DISASM_NUM_BYTES 32
#define DISASM_OUT_STRING_SIZE 128
  if (global_disassemble_mutex == NULL) {
    global_disassemble_mutex = new mp::Mutex(DISSASSM_NAMEWORD);
  }
  WITH_READ_WRITE_LOCK(*global_disassemble_mutex);
  LLVMDisasmContextRef dis = LLVMCreateDisasm(striple.c_str(), NULL, 0, NULL, my_LLVMSymbolLookupCallback);
  LLVMSetDisasmOptions(dis, LLVMDisassembler_Option_PrintImmHex
                       /*| LLVMDisassembler_Option_PrintLatency */
                       /*| LLVMDisassembler_Option_UseMarkup */);
  size_t ii = 0;
  size_t offset = 0;
  for (uint8_t* addr = (uint8_t*)start_address->ptr(); addr < (uint8_t*)end_address->ptr();) {
    ArrayRef<uint8_t> Bytes(addr, DISASM_NUM_BYTES);
    SmallVector<char, DISASM_OUT_STRING_SIZE> InsnStr;
    size_t sz = LLVMDisasmInstruction(dis, (unsigned char*)&Bytes[0], DISASM_NUM_BYTES, (uint64_t)addr, (char*)InsnStr.data(),
                                      DISASM_OUT_STRING_SIZE - 1);
    const char* str = InsnStr.data();
    stringstream ss;
    ss << std::hex << (void*)addr << " <#" << std::dec << std::setw(3) << ii << "+" << offset << ">";
    std::string sstr = ss.str();
    core::clasp_write_string(sstr);
    if (sstr.size() < 24) {
      core::clasp_write_string(std::string("                      ").substr(0, 24 - sstr.size()));
    }
    core::clasp_write_string(str);
    core::cl__terpri();
    if (sz == 0) {
      ss << "STOPPING BECAUSE PREVIOUS INSTRUCTION HAS ZERO LENGTH!!!!! ";
      break;
    }
    addr += sz;
    ii++;
    offset += sz;
  }
  LLVMDisasmDispose(dis);
}

DOCGROUP(clasp);
CL_DEFUN LLVMContext_sp LLVMContext_O::create_llvm_context() {
  auto context = gctools::GC<LLVMContext_O>::allocate_with_default_constructor();
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
} // namespace llvmo

namespace llvmo {
DOCGROUP(clasp);
CL_DEFUN ThreadSafeContext_sp ThreadSafeContext_O::create_thread_safe_context() {
  std::unique_ptr<llvm::LLVMContext> lc(new llvm::LLVMContext());
  auto context = gctools::GC<ThreadSafeContext_O>::allocate_with_default_constructor();
  llvm::orc::ThreadSafeContext* tslc = new llvm::orc::ThreadSafeContext(std::move(lc));
  context->_ptr = tslc;
  return context;
};

CL_DEFMETHOD LLVMContext* ThreadSafeContext_O::getContext() { return this->wrappedPtr()->getContext(); };

DOCGROUP(clasp);
CL_DEFUN LLVMContext_sp llvm_sys__thread_local_llvm_context() {
  ThreadSafeContext_sp tsc = gc::As<ThreadSafeContext_sp>(comp::_sym_STARthread_safe_contextSTAR->symbolValue());
  llvm::LLVMContext* lc = tsc->wrappedPtr()->getContext();
  auto context = gctools::GC<LLVMContext_O>::allocate_with_default_constructor();
  context->_ptr = lc;
  return context;
}

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(make-linker);
DOCGROUP(clasp);
CL_DEFUN Linker_sp Linker_O::make(Module_sp module) {
  auto self = gctools::GC<Linker_O>::allocate_with_default_constructor();
  self->_ptr = new llvm::Linker(*module->wrappedPtr());
  return self;
};

#if LLVM_VERSION_MAJOR < 19
static void expectNoDiags(const llvm::DiagnosticInfo& DI, void* C) { fmt::print("{}:{} Got a diagnostic\n", __FILE__, __LINE__); }
#else
static void expectNoDiags(const llvm::DiagnosticInfo* DI, void* C) { fmt::print("{}:{} Got a diagnostic\n", __FILE__, __LINE__); }
#endif

}; // namespace llvmo
namespace {
struct ClaspDiagnosticHandler : public llvm::DiagnosticHandler {
  bool handleDiagnostics(const llvm::DiagnosticInfo& DI) override {
    llvm::raw_ostream& OS = llvm::errs();
    OS << "llvm-lto: ";
    switch (DI.getSeverity()) {
    case llvm::DS_Error:
      OS << "error";
      break;
    case llvm::DS_Warning:
      OS << "warning";
      break;
    case llvm::DS_Remark:
      OS << "remark";
      break;
    case llvm::DS_Note:
      OS << "note";
      break;
    }
    llvm::DiagnosticPrinterRawOStream DP(llvm::errs());
    DI.print(DP);
    return true;
  }
};
} // namespace

namespace llvmo {
DOCGROUP(clasp);
CL_DEFUN core::T_mv llvm_sys__link_in_module(Linker_sp linker, Module_sp module) {
  std::string errorMsg = "llvm::Linker::linkInModule reported an error";
  // Take ownership of the pointer and give it to the linker
  llvm::Module* mptr = module->wrappedPtr();
  llvm::LLVMContext& Ctx = mptr->getContext();
  module->reset_wrappedPtr();
  std::unique_ptr<llvm::Module> u_module(mptr);
  Ctx.setDiagnosticHandlerCallBack(expectNoDiags);
  bool res = linker->wrappedPtr()->linkInModule(std::move(u_module));
  Ctx.setDiagnosticHandlerCallBack(NULL);
  return Values(_lisp->_boolean(res), core::SimpleBaseString_O::make(errorMsg));
};

DOCGROUP(clasp);
CL_DEFUN core::T_mv llvm_sys__linkModules(Module_sp linkedModule, Module_sp module) {
  std::string errorMsg = "llvm::Linker::linkModules reported an error";
  // Take ownership of the pointer and give it to the linker
  llvm::Module* linkedModulePtr = linkedModule->wrappedPtr();
  llvm::Module* mptr = module->wrappedPtr();
  module->reset_wrappedPtr();
  std::unique_ptr<llvm::Module> u_module(mptr);
  //  Ctx.setDiagnosticHandler(std::make_unique<ClaspDiagnosticHandler>(), true);
  bool res = llvm::Linker::linkModules(*linkedModulePtr, std::move(u_module));
  //  Ctx.setDiagnosticHandlerCallBack(NULL);
  return Values(_lisp->_boolean(res), core::SimpleBaseString_O::make(errorMsg));
};

//  CL_LISPIFY_NAME(getModule);
//  CL_EXTERN_DEFMETHOD(Linker_O, &llvm::Linker::getModule);

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(jitdylib-dump);
CL_DEFMETHOD void JITDylib_O::dump(core::T_sp stream) {
  bool stringOutputStream = false;
  llvm::SmallString<1024> stringOutput;
  llvm::raw_pwrite_stream* ostreamP = llvm_stream(stream, stringOutput, stringOutputStream);
  this->wrappedPtr()->dump(*ostreamP);
  core::clasp_write_string(stringOutput.c_str(), stream);
};

}; // namespace llvmo

namespace llvmo {

DOCGROUP(clasp);
CL_DEFUN Instruction_sp llvm_sys__create_invoke_instruction_append_to_basic_block(llvm::Function* func,
                                                                                  llvm::BasicBlock* normal_dest,
                                                                                  llvm::BasicBlock* unwind_dest, core::List_sp args,
                                                                                  core::String_sp label,
                                                                                  llvm::BasicBlock* append_bb) {
  printf("%s:%d In create_invoke_instruction\n", __FILE__, __LINE__);
  // Get the arguments
  vector<llvm::Value*> llvm_args;
  for (auto cur : args) {
    Value_sp arg = gc::As<Value_sp>(oCar(cur));
    llvm_args.push_back(arg->wrappedPtr());
  }
  llvm::InvokeInst* llvm_invoke =
      llvm::InvokeInst::Create(func, normal_dest, unwind_dest, llvm_args, label->get_std_string(), append_bb);
  Instruction_sp invoke = Instruction_O::create();
  invoke->set_wrapped(llvm_invoke);
  return invoke;
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__dump_instruction_pointers(llvm::Instruction* I) {
  if (I == NULL) {
    printf("%s:%d  Instruction is NULL\n", __FILE__, __LINE__);
  } else {
    auto prev = I->getPrevNode();
    auto next = I->getNextNode();
    const char* opcode = I->getOpcodeName();
    printf("%s:%d Instruction %15s @%p  prev@%p  next@%p\n", __FILE__, __LINE__, opcode, (void*)&I, (void*)prev, (void*)next);
  }
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__dump_instruction_list(BasicBlock_sp cbb) {
  llvm::BasicBlock* bb = cbb->wrappedPtr();
  if (bb == NULL) {
    printf("%s:%d The instruction list is empty\n", __FILE__, __LINE__);
    return;
  }
  //  printf("%s:%d  bb->end()->getPrev() = @%p\n", __FILE__, __LINE__, bb->end()->getPrev());
  printf("%s:%d  &(bb->back())  = @%p\n", __FILE__, __LINE__, (void*)&(bb->back()));
  printf("%s:%d  Dumping instruction list\n", __FILE__, __LINE__);
  for (auto& I : *bb) {
    auto prev = I.getPrevNode();
    auto next = I.getNextNode();
    const char* opcode = I.getOpcodeName();
    printf("%s:%d Instruction %15s @%p  prev@%p  next@%p\n", __FILE__, __LINE__, opcode, (void*)&I, (void*)prev, (void*)next);
  }
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__sanity_check_module(Module_sp module, int depth) {
  llvm::Module* modP = module->wrappedPtr();
  if (modP) {
    llvm::Module& mod = *modP;
    for (auto& F : mod) {
      if (depth > 0) {
        for (auto& B : F) {
          if (depth > 1) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
            for (auto& I : B)
              /*nothing*/;
#pragma clang diagnostic pop
          }
        }
      }
    }
  }
}
}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(createTargetMachine);
#if LLVM_VERSION_MAJOR < 17
CL_EXTERN_DEFMETHOD(Target_O, (llvm::TargetMachine * (llvm::Target::*)(llvm::StringRef, llvm::StringRef, llvm::StringRef,
                                                                       const llvm::TargetOptions&, Optional<Reloc::Model>,
                                                                       Optional<CodeModel::Model>, CodeGenOpt::Level, bool) const) &
                                  llvm::Target::createTargetMachine);
#elif LLVM_VERSION_MAJOR == 17
CL_EXTERN_DEFMETHOD(Target_O,
                    (llvm::TargetMachine * (llvm::Target::*)(llvm::StringRef, llvm::StringRef, llvm::StringRef,
                                                             const llvm::TargetOptions&, std::optional<Reloc::Model>,
                                                             std::optional<CodeModel::Model>, CodeGenOpt::Level, bool) const) &
                        llvm::Target::createTargetMachine);
#else
CL_EXTERN_DEFMETHOD(Target_O,
                    (llvm::TargetMachine * (llvm::Target::*)(llvm::StringRef, llvm::StringRef, llvm::StringRef,
                                                             const llvm::TargetOptions&, std::optional<Reloc::Model>,
                                                             std::optional<CodeModel::Model>, CodeGenOptLevel, bool) const) &
                        llvm::Target::createTargetMachine);
#endif

}; // namespace llvmo

namespace llvmo {

struct Safe_raw_pwrite_stream {
  llvm::raw_pwrite_stream* ostreamP;
  Safe_raw_pwrite_stream() : ostreamP(NULL) {};
  void set_stream(llvm::raw_pwrite_stream* s) { ostreamP = s; };
  llvm::raw_pwrite_stream* get_stream() const { return ostreamP; };
  ~Safe_raw_pwrite_stream() {
    if (ostreamP) {
      delete ostreamP;
      ostreamP = NULL;
    }
  }
};

CL_LISPIFY_NAME("emitModule")
CL_DEFMETHOD core::T_sp TargetMachine_O::emitModule(core::T_sp stream, core::T_sp dwo_stream, llvm::CodeGenFileType FileType,
                                                    Module_sp module) {
  if (stream.nilp()) {
    SIMPLE_ERROR("You must pass a valid stream");
  }

  llvm::TargetMachine* TM = this->wrappedPtr();

  Safe_raw_pwrite_stream ostream;
  llvm::SmallString<1024> stringOutput;
  bool stringOutputStream = false;

  if (stream.isA<core::StringOutputStream_O>()) {
    ostream.set_stream(new llvm::raw_svector_ostream(stringOutput));
    stringOutputStream = true;
  } else if (stream == kw::_sym_simple_vector_byte8) {
    ostream.set_stream(new llvm::raw_svector_ostream(stringOutput));
  } else {
    int fd = core::stream_file_descriptor(stream, core::StreamDirection::output);
    if (fd < 0)
      SIMPLE_ERROR("Illegal file type {} for addPassesToEmitFileAndRunPassManager", _rep_(stream));
    ostream.set_stream(new llvm::raw_fd_ostream(fd, false, true));
  }

  llvm::SmallString<1024> dwo_stringOutput;
  Safe_raw_pwrite_stream dwo_ostream;
  bool dwo_stringOutputStream = false;

  if (dwo_stream.nilp()) { // do nothing
  } else if (dwo_stream.isA<core::StringOutputStream_O>()) {
    dwo_ostream.set_stream(new llvm::raw_svector_ostream(dwo_stringOutput));
    dwo_stringOutputStream = true;
  } else if (dwo_stream == kw::_sym_simple_vector_byte8) {
    dwo_ostream.set_stream(new llvm::raw_svector_ostream(dwo_stringOutput));
  } else {
    int fd = core::stream_file_descriptor(dwo_stream, core::StreamDirection::output);
    if (fd < 0)
      SIMPLE_ERROR("Illegal file type {} for addPassesToEmitFileAndRunPassManager", _rep_(dwo_stream));
    dwo_ostream.set_stream(new llvm::raw_fd_ostream(fd, false, true));
  }

  llvm::legacy::PassManager PM;

  llvm::LLVMTargetMachine* LTM = dynamic_cast<llvm::LLVMTargetMachine*>(TM);
  if (LTM) {
    llvm::TargetPassConfig* TPC = LTM->createPassConfig(PM);
    // Disable tail merges to improve debug info accuracy
    TPC->setEnableTailMerge(false);
  }

  PM.add(new llvm::TargetLibraryInfoWrapperPass(TM->getTargetTriple()));
  module->wrappedPtr()->setDataLayout(TM->createDataLayout());
  if (TM->addPassesToEmitFile(PM, *ostream.get_stream(), dwo_ostream.get_stream(), FileType, true, nullptr)) {
    SIMPLE_ERROR("Could not generate file type");
  }
  PM.run(*module->wrappedPtr());

  if (stream == kw::_sym_simple_vector_byte8) {
    SYMBOL_EXPORT_SC_(KeywordPkg, simple_vector_byte8);
    core::SimpleVector_byte8_t_sp vector_byte8 = core::SimpleVector_byte8_t_O::make(stringOutput);
    if (dwo_stream.notnilp()) {
      SIMPLE_ERROR("dwo_stream must be nil");
    }
    return vector_byte8;
  } else if (stream_p(stream)) {
    clasp_write_string(stringOutput.c_str(), stream);
  }

  if (stream_p(dwo_stream)) {
    clasp_write_string(dwo_stringOutput.c_str(), dwo_stream);
  }

  return nil<core::T_O>();
}

CL_LISPIFY_NAME(createDataLayout);
CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::createDataLayout);
CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::setFastISel);
CL_LISPIFY_NAME(getSubtargetImpl);
CL_EXTERN_DEFMETHOD(TargetMachine_O,
                    (const llvm::TargetSubtargetInfo* (llvm::TargetMachine::*)() const) & llvm::TargetMachine::getSubtargetImpl);
CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::setFastISel);

SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType_Null);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType_AssemblyFile);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenFileType_ObjectFile);
CL_BEGIN_ENUM(llvm::CodeGenFileType, _sym_CodeGenFileType, "CodeGenFileType");
#if LLVM_VERSION_MAJOR < 18
CL_VALUE_ENUM(_sym_CodeGenFileType_Null, llvm::CGFT_Null);
CL_VALUE_ENUM(_sym_CodeGenFileType_AssemblyFile, llvm::CGFT_AssemblyFile);
CL_VALUE_ENUM(_sym_CodeGenFileType_ObjectFile, llvm::CGFT_ObjectFile);
;
#else
CL_VALUE_ENUM(_sym_CodeGenFileType_Null, llvm::CodeGenFileType::Null);
CL_VALUE_ENUM(_sym_CodeGenFileType_AssemblyFile, llvm::CodeGenFileType::AssemblyFile);
CL_VALUE_ENUM(_sym_CodeGenFileType_ObjectFile, llvm::CodeGenFileType::ObjectFile);
;
#endif
CL_END_ENUM(_sym_CodeGenFileType);

SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_None);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_Less);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_Default);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeGenOpt_Aggressive);
#if LLVM_VERSION_MAJOR < 18
CL_BEGIN_ENUM(llvm::CodeGenOpt::Level, _sym_CodeGenOpt, "CodeGenOpt");
CL_VALUE_ENUM(_sym_CodeGenOpt_None, llvm::CodeGenOpt::None);
CL_VALUE_ENUM(_sym_CodeGenOpt_Less, llvm::CodeGenOpt::Less);
CL_VALUE_ENUM(_sym_CodeGenOpt_Default, llvm::CodeGenOpt::Default);
CL_VALUE_ENUM(_sym_CodeGenOpt_Aggressive, llvm::CodeGenOpt::Aggressive);
;
CL_END_ENUM(_sym_CodeGenOpt);
#else
CL_BEGIN_ENUM(llvm::CodeGenOptLevel, _sym_CodeGenOpt, "CodeGenOpt");
CL_VALUE_ENUM(_sym_CodeGenOpt_None, llvm::CodeGenOptLevel::None);
CL_VALUE_ENUM(_sym_CodeGenOpt_Less, llvm::CodeGenOptLevel::Less);
CL_VALUE_ENUM(_sym_CodeGenOpt_Default, llvm::CodeGenOptLevel::Default);
CL_VALUE_ENUM(_sym_CodeGenOpt_Aggressive, llvm::CodeGenOptLevel::Aggressive);
;
CL_END_ENUM(_sym_CodeGenOpt);
#endif

SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel);
SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_undefined);
SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_Static);
SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_PIC_);
SYMBOL_EXPORT_SC_(LlvmoPkg, RelocModel_DynamicNoPIC);
CL_BEGIN_ENUM(llvm::Reloc::Model, _sym_RelocModel, "RelocModel");
CL_VALUE_ENUM(_sym_RelocModel_Static, llvm::Reloc::Model::Static);
CL_VALUE_ENUM(_sym_RelocModel_PIC_, llvm::Reloc::Model::PIC_);
CL_VALUE_ENUM(_sym_RelocModel_DynamicNoPIC, llvm::Reloc::Model::DynamicNoPIC);
;
CL_END_ENUM(_sym_RelocModel);

SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Small);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Kernel);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Medium);
SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Large);
CL_BEGIN_ENUM(llvm::CodeModel::Model, _sym_CodeModel, "CodeModel");
CL_VALUE_ENUM(_sym_CodeModel_Small, llvm::CodeModel::Small);
CL_VALUE_ENUM(_sym_CodeModel_Kernel, llvm::CodeModel::Kernel);
CL_VALUE_ENUM(_sym_CodeModel_Medium, llvm::CodeModel::Medium);
CL_VALUE_ENUM(_sym_CodeModel_Large, llvm::CodeModel::Large);
;
CL_END_ENUM(_sym_CodeModel);

}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(triple-str);
CL_DECLARE();
CL_DOCSTRING(R"dx()dx");
CL_PKG_NAME(LlvmoPkg,"make-triple");
DOCGROUP(clasp);
CL_DEFUN Triple_sp Triple_O::make(const string& triple) {
  auto self = gctools::GC<Triple_O>::allocate_with_default_constructor();
  self->_ptr = new llvm::Triple(triple);
  return self;
};

CL_PKG_NAME(LlvmoPkg,"triple-normalize");
CL_EXTERN_DEFUN((std::string(*)(llvm::StringRef str))&llvm::Triple::normalize);

CL_LISPIFY_NAME(getTriple);
CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getTriple);
CL_LISPIFY_NAME(getArchName);
CL_EXTERN_DEFMETHOD(Triple_O, (llvm::StringRef(llvm::Triple::*)() const)&llvm::Triple::getArchName);
CL_LISPIFY_NAME(getVendorName);
CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getVendorName);
CL_LISPIFY_NAME(getOSName);
CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getOSName);
CL_LISPIFY_NAME(getEnvironmentName);
CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getEnvironmentName);
CL_LISPIFY_NAME(getOSAndEnvironmentName);
CL_EXTERN_DEFMETHOD(Triple_O, &llvm::Triple::getOSAndEnvironmentName);

CL_PKG_NAME(LlvmoPkg, "lookup-intrinsic-id");
CL_EXTERN_DEFUN((llvm::Intrinsic::ID(*)(llvm::StringRef Name))&llvm::Function::lookupIntrinsicID);

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
CL_BEGIN_ENUM(llvm::Triple::ArchType, _sym_ArchType, "ArchType");
CL_VALUE_ENUM(_sym_ArchType_UnknownArch, llvm::Triple::UnknownArch);
CL_VALUE_ENUM(_sym_ArchType_arm, llvm::Triple::arm);
CL_VALUE_ENUM(_sym_ArchType_armeb, llvm::Triple::armeb); // ARM (big endian): armeb
CL_VALUE_ENUM(_sym_ArchType_aarch64,
              llvm::Triple::aarch64); // AArch64 (little endian):     aarch64_be, // AArch64 (big endian): aarch64_be
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
CL_BEGIN_ENUM(llvm::Triple::SubArchType, _sym_SubArchType, "SubArchType");
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
CL_VALUE_ENUM(_sym_SubArchType_KalimbaSubArch_v5, llvm::Triple::KalimbaSubArch_v5);
;
CL_END_ENUM(_sym_SubArchType);

SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_UnknownVendor);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_Apple);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_PC);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_SCEI);
// SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_BGP);
// SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_BGQ);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_Freescale);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_IBM);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_ImaginationTechnologies);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_MipsTechnologies);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_NVIDIA);
SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType_CSR);

SYMBOL_EXPORT_SC_(LlvmoPkg, VendorType);
CL_BEGIN_ENUM(llvm::Triple::VendorType, _sym_VendorType, "VendorType");
CL_VALUE_ENUM(_sym_VendorType_UnknownVendor, llvm::Triple::UnknownVendor);
CL_VALUE_ENUM(_sym_VendorType_Apple, llvm::Triple::Apple);
CL_VALUE_ENUM(_sym_VendorType_PC, llvm::Triple::PC);
CL_VALUE_ENUM(_sym_VendorType_SCEI, llvm::Triple::SCEI);
// CL_VALUE_ENUM(_sym_VendorType_BGP, llvm::Triple::BGP);
// CL_VALUE_ENUM(_sym_VendorType_BGQ, llvm::Triple::BGQ);
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
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_KFreeBSD);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Linux);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Lv2);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_MacOSX);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_NetBSD);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_OpenBSD);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Solaris);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Win32);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Haiku);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_RTEMS);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_NaCl);
// SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_CNK);
//   SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Bitrig);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_AIX);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_CUDA);
SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_NVCL);

SYMBOL_EXPORT_SC_(LlvmoPkg, OSType);
CL_BEGIN_ENUM(llvm::Triple::OSType, _sym_OSType, "OSType");
CL_VALUE_ENUM(_sym_OSType_UnknownOS, llvm::Triple::UnknownOS);
CL_VALUE_ENUM(_sym_OSType_Darwin, llvm::Triple::Darwin);
CL_VALUE_ENUM(_sym_OSType_DragonFly, llvm::Triple::DragonFly);
CL_VALUE_ENUM(_sym_OSType_FreeBSD, llvm::Triple::FreeBSD);
CL_VALUE_ENUM(_sym_OSType_IOS, llvm::Triple::IOS);
CL_VALUE_ENUM(_sym_OSType_KFreeBSD, llvm::Triple::KFreeBSD);
CL_VALUE_ENUM(_sym_OSType_Linux, llvm::Triple::Linux);
CL_VALUE_ENUM(_sym_OSType_Lv2, llvm::Triple::Lv2);
CL_VALUE_ENUM(_sym_OSType_MacOSX, llvm::Triple::MacOSX);
CL_VALUE_ENUM(_sym_OSType_NetBSD, llvm::Triple::NetBSD);
CL_VALUE_ENUM(_sym_OSType_OpenBSD, llvm::Triple::OpenBSD);
CL_VALUE_ENUM(_sym_OSType_Solaris, llvm::Triple::Solaris);
CL_VALUE_ENUM(_sym_OSType_Win32, llvm::Triple::Win32);
CL_VALUE_ENUM(_sym_OSType_Haiku, llvm::Triple::Haiku);
CL_VALUE_ENUM(_sym_OSType_RTEMS, llvm::Triple::RTEMS);
CL_VALUE_ENUM(_sym_OSType_NaCl, llvm::Triple::NaCl);
// CL_VALUE_ENUM(_sym_OSType_CNK, llvm::Triple::CNK);
//   CL_VALUE_ENUM(_sym_OSType_Bitrig, llvm::Triple::Bitrig);
CL_VALUE_ENUM(_sym_OSType_AIX, llvm::Triple::AIX);
CL_VALUE_ENUM(_sym_OSType_CUDA, llvm::Triple::CUDA);
CL_VALUE_ENUM(_sym_OSType_NVCL, llvm::Triple::NVCL);
;
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
CL_BEGIN_ENUM(llvm::Triple::EnvironmentType, _sym_EnvironmentType, "EnvironmentType");
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
CL_BEGIN_ENUM(llvm::Triple::ObjectFormatType, _sym_ObjectFormatType, "ObjectFormatType");
CL_VALUE_ENUM(_sym_ObjectFormatType_UnknownObjectFormat, llvm::Triple::UnknownObjectFormat);
CL_VALUE_ENUM(_sym_ObjectFormatType_COFF, llvm::Triple::COFF);
CL_VALUE_ENUM(_sym_ObjectFormatType_ELF, llvm::Triple::ELF);
CL_VALUE_ENUM(_sym_ObjectFormatType_MachO, llvm::Triple::MachO);
;
CL_END_ENUM(_sym_ObjectFormatType);

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(make-target-options);
CL_LAMBDA(&key function-sections);
DOCGROUP(clasp);
CL_DEFUN TargetOptions_sp TargetOptions_O::make(bool functionSections) {
  auto self = gctools::GC<TargetOptions_O>::allocate_with_default_constructor();
  self->_ptr = new llvm::TargetOptions();
  self->_ptr->FunctionSections = functionSections;
  return self;
};

}; // namespace llvmo

namespace llvmo {
Value_sp Value_O::create(llvm::Value* ptr) { return core::RP_Create_wrapped<Value_O, llvm::Value*>(ptr); };

CL_DEFMETHOD LLVMContext_sp Value_O::getContext() const {
  return gc::As<LLVMContext_sp>(translate::to_object<llvm::LLVMContext&>::convert(this->wrappedPtr()->getContext()));
}

} // namespace llvmo

namespace llvmo {
CL_LISPIFY_NAME(getName);
CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::getName);
CL_LISPIFY_NAME(setName);
CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::setName);
CL_LISPIFY_NAME(getType);
CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::getType);
;
CL_LISPIFY_NAME(replaceAllUsesWith);
CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::replaceAllUsesWith);
;

}; // namespace llvmo

namespace llvmo {
CL_LISPIFY_NAME(MetadataAsValue-get);
CL_EXTERN_DEFUN(&llvm::MetadataAsValue::get);
CL_LISPIFY_NAME(MetadataAsValue-getIfExists);
CL_EXTERN_DEFUN(&llvm::MetadataAsValue::getIfExists);

}; // namespace llvmo

namespace llvmo {
CL_LISPIFY_NAME(ValueAsMetadata-get);
CL_EXTERN_DEFUN(&llvm::ValueAsMetadata::get);
}; // namespace llvmo

namespace llvmo {

DOCGROUP(clasp);
CL_DEFUN core::T_mv llvm_sys__verifyModule(Module_sp module, core::Symbol_sp action) {
  string errorInfo;
  llvm::raw_string_ostream ei(errorInfo);
  llvm::Module* m = module->wrappedPtr();
  bool result = llvm::verifyModule(*m, &ei);
  return Values(_lisp->_boolean(result), core::SimpleBaseString_O::make(ei.str()));
};

DOCGROUP(clasp);
CL_DEFUN core::T_mv llvm_sys__verifyFunction(Function_sp function) {
  llvm::Function* f = function->wrappedPtr();
  string errorInfo;
  llvm::raw_string_ostream ei(errorInfo);
  bool result = llvm::verifyFunction(*f, &ei);
  return Values(_lisp->_boolean(result), core::SimpleBaseString_O::make(ei.str()));
};

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__printFunctionToStream(Function_sp func, core::T_sp stream) {
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  llvm::AssemblyAnnotationWriter* aaw = new llvm::AssemblyAnnotationWriter();
  func->wrappedPtr()->print(sout, aaw);
  delete aaw;
  core::clasp_write_string(outstr, stream);
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__printModuleToStream(Module_sp module, core::T_sp stream) {
  string outstr;
  llvm::raw_string_ostream sout(outstr);
  llvm::AssemblyAnnotationWriter* aaw = new llvm::AssemblyAnnotationWriter();
  module->wrappedPtr()->print(sout, aaw);
  delete aaw;
  core::clasp_write_string(outstr, stream);
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__writeIrToFile(Module_sp module, core::String_sp path) {
  std::error_code errcode;
  string pathName = path->get_std_string();
  llvm::raw_fd_ostream OS(pathName.c_str(), errcode, ::llvm::sys::fs::OpenFlags::OF_None);
  if (errcode) {
    SIMPLE_ERROR("Could not write bitcode to {} - problem: {}", pathName, errcode.message());
  }
  llvm::AssemblyAnnotationWriter* aaw = new llvm::AssemblyAnnotationWriter();
  module->wrappedPtr()->print(OS, aaw);
  delete aaw;
}

CL_LAMBDA(module pathname &optional (use-thin-lto t));
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__writeBitcodeToFile(Module_sp module, core::String_sp pathname, bool useThinLTO) {
  string pn = pathname->get_std_string();
  std::error_code errcode;
  llvm::raw_fd_ostream OS(pn.c_str(), errcode, ::llvm::sys::fs::OpenFlags::OF_None);
  if (errcode) {
    SIMPLE_ERROR("Could not write bitcode to file[{}] - error: {}", pn, errcode.message());
  }
  if (useThinLTO) {
    llvm::ProfileSummaryInfo PSI(*module->wrappedPtr());
    auto Index = llvm::buildModuleSummaryIndex(*(module->wrappedPtr()), NULL, &PSI);
    llvm::WriteBitcodeToFile(*module->wrappedPtr(), OS, false, &Index, true);
  } else {
    llvm::WriteBitcodeToFile(*module->wrappedPtr(), OS);
  }
};

#if 0
DOCGROUP(clasp);
CL_DEFUN Module_sp llvm_sys__parseIRString(core::T_sp llvm_ir_string, LLVMContext_sp context) {
  core::String_sp source = gc::As<core::String_sp>(llvm_ir_string);
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getMemBufferCopy(source->get_std_string());
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR("Could not read the string {} - error: {}", source->get_std_string() , ec.message());
  }
  llvm::SMDiagnostic smd;
  std::unique_ptr<llvm::Module> module =
    llvm::parseIR(eo_membuf.get()->getMemBufferRef(), smd, *(context->wrappedPtr()));
  llvm::Module* m = module.release();
  if (!m) {
    std::string message = smd.getMessage().str();
    SIMPLE_ERROR("Could not load llvm-ir from string {} - error: {}", source->get_std_string() , message );
  }
  Module_sp omodule = core::RP_Create_wrapped<Module_O,llvm::Module*>(m);
  return omodule;
};

#endif
DOCGROUP(clasp);

CL_DEFUN Module_sp llvm_sys__parseIRFile(core::T_sp tfilename, LLVMContext_sp context) {
  if (tfilename.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  core::String_sp spathname = gc::As<core::String_sp>(core::cl__namestring(core::cl__pathname(tfilename)));
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getFile(spathname->get_std_string());
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR("Could not read the file {} - error: {}", spathname->get_std_string(), ec.message());
  }
  llvm::SMDiagnostic smd;
  std::unique_ptr<llvm::Module> module = llvm::parseIR(eo_membuf.get()->getMemBufferRef(), smd, *(context->wrappedPtr()));
  llvm::Module* m = module.release();
  if (!m) {
    std::string message = smd.getMessage().str();
    SIMPLE_ERROR("Could not load llvm-ir for file {} - error: {}", spathname->get_std_string(), message);
  }
  Module_sp omodule = core::RP_Create_wrapped<Module_O, llvm::Module*>(m);
  return omodule;
};

DOCGROUP(clasp);
CL_DEFUN Module_sp llvm_sys__parseIRString(const std::string& llCode, LLVMContext_sp context, const std::string& bufferName) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getMemBuffer(llCode, bufferName);
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR("Could not construct a memory buffer for the string {} error: {}", llCode, ec.message());
  }
  llvm::SMDiagnostic smd;
  std::unique_ptr<llvm::Module> module = llvm::parseIR(eo_membuf.get()->getMemBufferRef(), smd, *(context->wrappedPtr()));
  llvm::Module* m = module.release();
  if (!m) {
    std::string message = smd.getMessage().str();
    SIMPLE_ERROR("Could not load llvm-ir for string {} - error: {}", llCode, message);
  }
  Module_sp omodule = core::RP_Create_wrapped<Module_O, llvm::Module*>(m);
  return omodule;
};

CL_LAMBDA("filename context");
DOCGROUP(clasp);
CL_DEFUN Module_sp llvm_sys__parseBitcodeFile(core::T_sp tfilename, LLVMContext_sp context) {
  if (tfilename.nilp())
    SIMPLE_ERROR("{} was about to pass nil to pathname", __FUNCTION__);
  core::String_sp spathname = gc::As<core::String_sp>(core::cl__namestring(core::cl__pathname(tfilename)));
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getFile(spathname->get_std_string());
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR("Could not load bitcode for file {} - error: {}", spathname->get_std_string(), ec.message());
  }
  llvm::Expected<std::unique_ptr<llvm::Module>> eom =
      llvm::parseBitcodeFile(eo_membuf.get()->getMemBufferRef(), *(context->wrappedPtr()));
  if (!eom)
    SIMPLE_ERROR("Could not parse bitcode for file {} - there was an error\n{}", spathname->get_std_string(),
                 llvm::toString(std::move(eom.takeError())));
  Module_sp omodule = core::RP_Create_wrapped<Module_O, llvm::Module*>((*eom).release());
  return omodule;
};

DOCGROUP(clasp);
CL_DEFUN Module_sp llvm_sys__clone_module(Module_sp original) {
  llvm::Module* o = original->wrappedPtr();
  std::unique_ptr<llvm::Module> m = llvm::CloneModule(*o);
  Module_sp module = core::RP_Create_wrapped<Module_O, llvm::Module*>(m.release());
  return module;
}

DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__valuep(core::T_sp arg) { return gc::IsA<Value_sp>(arg); };

#if 0
    // Jan 31, 2013 - the Attribute/Argument api is changing fast and I'm not using it right now
    // and I don't want to mess with this code until it settles down
Attribute_sp Attribute_O::get(LLVMContext_sp context, core::Cons_sp attribute_symbols)
{
  llvm::AttrBuilder attrBuilder;
  core::SymbolToEnumConverter_sp converter = _sym_AttributeEnum->symbolValue().as<core::SymbolToEnumConverter_O>();
  for ( core::Cons_sp cur=attribute_symbols;cur->notNil(); cur=cur->cdr() )
  {
    core::Symbol_sp sym = cur->car().as<core::Symbol_O>();
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
SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeInAlloca);
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
SYMBOL_EXPORT_SC_(LlvmoPkg, AttributeOptimizeNone);
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
CL_BEGIN_ENUM(llvm::Attribute::AttrKind, _sym_AttributeEnum, "Attribute");
CL_VALUE_ENUM(_sym_AttributeNone, llvm::Attribute::None);
CL_VALUE_ENUM(_sym_AttributeInAlloca, llvm::Attribute::InAlloca);
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
CL_VALUE_ENUM(_sym_AttributeOptimizeNone, llvm::Attribute::OptimizeNone);
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
SYMBOL_EXPORT_SC_(LlvmoPkg, CallingConv);
SYMBOL_EXPORT_SC_(LlvmoPkg, C);
SYMBOL_EXPORT_SC_(LlvmoPkg, fastcc);
CL_BEGIN_ENUM(llvmo::ClaspCallingConv, _sym_CallingConv, "CallingConv");
CL_VALUE_ENUM(_sym_C, llvmo::ClaspCallingConv::C);
CL_VALUE_ENUM(_sym_fastcc, llvmo::ClaspCallingConv::Fast);
CL_END_ENUM(_sym_CallingConv)

CL_LAMBDA(module value &optional label);
DOCGROUP(clasp);
CL_DEFUN Value_sp llvm_sys__makeStringGlobal(Module_sp module, core::String_sp svalue, core::T_sp label) {
  llvm::Module& M = *(module->wrappedPtr());
  llvm::Constant* StrConstant = llvm::ConstantDataArray::getString(M.getContext(), svalue->get_std_string());
  llvm::GlobalVariable* GV =
      new llvm::GlobalVariable(M, StrConstant->getType(), true, llvm::GlobalValue::InternalLinkage, StrConstant);
  if (label.nilp()) {
    GV->setName(":::str");
  } else {
    GV->setName(gctools::As<core::String_sp>(label)->get_std_string());
  }
  GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return gc::As<Value_sp>(translate::to_object<llvm::Value*>::convert(GV));
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

void Value_O::__write__(core::T_sp stream) const { core::clasp_write_string(this->__repr__(), stream); }

bool Value_O::valid() const { return this->wrappedPtr() != NULL; }

DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__valid(core::T_sp value) {
  // nil is a valid
  if (value.nilp())
    return true;
  else if (Value_sp val = gc::As<Value_sp>(value)) {
    return val->valid();
  }
  SIMPLE_ERROR("Illegal argument for VALID: {}", _rep_(value));
}
}; // namespace llvmo

namespace llvmo {

void convert_sequence_types_to_vector(core::T_sp elements, vector<llvm::Type*>& velements) {
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
  // nil is a little stupid here but this is called from many
  // different functions and I don't want to bother adding an
  // argument just for this unlikely error message.
  ERROR_WRONG_TYPE_NTH_ARG(nil<core::T_O>(), 0, elements, ::cl::_sym_sequence);
}
} // namespace llvmo

namespace llvmo {
CL_PKG_NAME(LlvmoPkg,"make-module");
CL_LAMBDA(module-name context);
DOCGROUP(clasp);
CL_DEFUN Module_sp Module_O::make(const std::string& namePrefix, LLVMContext_sp context) {
  auto self = gctools::GC<Module_O>::allocate_with_default_constructor();
  self->_Id = core::core__next_jit_compile_counter();
  stringstream uniqueName;
  uniqueName << namePrefix << "-" << self->_Id;
  self->_UniqueName = uniqueName.str();
  self->_ptr = new llvm::Module(self->_UniqueName, *(context->wrappedPtr()));
  return self;
};

CL_DEFMETHOD LLVMContext_sp Module_O::getContext() const {
  return gc::As<LLVMContext_sp>(translate::to_object<llvm::LLVMContext&>::convert(this->wrappedPtr()->getContext()));
}

std::string Module_O::__repr__() const {
  stringstream ss;
  ss << "#<MODULE";
  //  ss << " " << (void*)this->wrappedPtr();
  ss << ">";
  return ss.str();
}

DOCGROUP(clasp);
CL_DEFUN core::List_sp llvm_sys__module_get_function_list(Module_sp module) {
  ql::list fl;
  for (llvm::Function& f : *module->wrappedPtr()) {
    Function_sp wrapped_func = gc::As<Function_sp>(translate::to_object<const llvm::Function&>::convert(f));
    fl << wrapped_func;
  }
  return fl.cons();
};
}; // namespace llvmo

namespace llvmo {
CL_LISPIFY_NAME(addModuleFlag);
CL_EXTERN_DEFMETHOD(Module_O, (void(llvm::Module::*)(llvm::MDNode*)) & llvm::Module::addModuleFlag);
CL_LISPIFY_NAME(getModuleIdentifier);
CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getModuleIdentifier);
CL_LISPIFY_NAME(getGlobalVariable);
CL_EXTERN_DEFMETHOD(Module_O, (llvm::GlobalVariable * (llvm::Module::*)(llvm::StringRef, bool)) & llvm::Module::getGlobalVariable);
CL_LISPIFY_NAME(getNamedGlobal);
CL_EXTERN_DEFMETHOD(Module_O, (llvm::GlobalVariable * (llvm::Module::*)(llvm::StringRef)) & llvm::Module::getNamedGlobal);
CL_LISPIFY_NAME(getOrInsertFunction);
CL_EXTERN_DEFMETHOD(Module_O, (llvm::FunctionCallee(llvm::Module::*)(llvm::StringRef,
                                                                     llvm::FunctionType*))&llvm::Module::getOrInsertFunction);
CL_LISPIFY_NAME(getOrInsertGlobal);
CL_EXTERN_DEFMETHOD(Module_O, (llvm::Constant * (llvm::Module::*)(llvm::StringRef, llvm::Type*)) & llvm::Module::getOrInsertGlobal);
CL_LISPIFY_NAME(getDataLayoutStr);
CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getDataLayoutStr);
CL_LISPIFY_NAME(getTargetTriple);
CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getTargetTriple);
CL_LISPIFY_NAME(setDataLayout);
CL_EXTERN_DEFMETHOD(Module_O, (void(llvm::Module::*)(const llvm::DataLayout&)) & llvm::Module::setDataLayout);
;
CL_LISPIFY_NAME(setDataLayout.string);
CL_EXTERN_DEFMETHOD(Module_O, (void(llvm::Module::*)(llvm::StringRef)) & llvm::Module::setDataLayout);
;
// CL_EXTERN_DEFMETHOD(Module_O,&llvm::Module::setTargetTriple);
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__setTargetTriple(llvm::Module* module, llvm::StringRef triple) { module->setTargetTriple(triple); }

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
CL_BEGIN_ENUM(llvm::Module::ModFlagBehavior, _sym_STARmoduleModFlagBehaviorSTAR, "llvm::Module::ModFlagBehavior");
CL_VALUE_ENUM(_sym_moduleFlagError, llvm::Module::Error);
CL_VALUE_ENUM(_sym_moduleFlagWarning, llvm::Module::Warning);
CL_VALUE_ENUM(_sym_moduleFlagRequire, llvm::Module::Require);
CL_VALUE_ENUM(_sym_moduleFlagOverride, llvm::Module::Override);
CL_VALUE_ENUM(_sym_moduleFlagAppend, llvm::Module::Append);
CL_VALUE_ENUM(_sym_moduleFlagAppendUnique, llvm::Module::AppendUnique);
CL_END_ENUM(_sym_STARmoduleModFlagBehaviorSTAR);

CL_BEGIN_ENUM(llvm::Module::ModFlagBehavior, _sym_STARmoduleModFlagBehaviorSTAR, "llvm::Module::ModFlagBehavior");
CL_VALUE_ENUM(_sym_moduleFlagError, llvm::Module::Error);
CL_VALUE_ENUM(_sym_moduleFlagWarning, llvm::Module::Warning);
CL_VALUE_ENUM(_sym_moduleFlagRequire, llvm::Module::Require);
CL_VALUE_ENUM(_sym_moduleFlagOverride, llvm::Module::Override);
CL_VALUE_ENUM(_sym_moduleFlagAppend, llvm::Module::Append);
CL_VALUE_ENUM(_sym_moduleFlagAppendUnique, llvm::Module::AppendUnique);
;
CL_END_ENUM(_sym_STARmoduleModFlagBehaviorSTAR);

CL_LISPIFY_NAME("getFunction");
CL_DEFMETHOD llvm::Function* Module_O::getFunction(core::String_sp dispatchName) {
  llvm::Module* module = this->wrappedPtr();
  string funcName = dispatchName->get_std_string();
  llvm::Function* func = module->getFunction(funcName);
  return func;
}

CL_LISPIFY_NAME("moduleValid");
CL_DEFMETHOD bool Module_O::valid() const { return this->wrappedPtr() != NULL; }

CL_DEFMETHOD llvm::DataLayout Module_O::getDataLayout() const { return this->wrappedPtr()->getDataLayout(); }

CL_LISPIFY_NAME("moduleDelete");
CL_DEFMETHOD void Module_O::moduleDelete() {
  ASSERT(this->wrappedPtr() != NULL);
  delete this->wrappedPtr();
  this->set_wrapped(NULL);
}

CL_LISPIFY_NAME("dump_namedMDList");
CL_DEFMETHOD void Module_O::dump_namedMDList() const {
  IMPLEMENT_MEF("Come up with a way to dump the MDList without using dump() (only enabled when LLVM_ENABLE_DUMP is on)");
}

void Module_O::initialize() {
  this->Base::initialize();
  this->_UniqueGlobalVariableStrings = core::HashTable_O::createEqual();
}

CL_DEFMETHOD void Module_O::emit_version_ident_metadata() {
  llvm::Module& TheModule = *this->wrappedPtr();
  llvm::NamedMDNode* IdentMetadata = TheModule.getOrInsertNamedMetadata("llvm.ident");
  std::string Version = "Clasp";
  llvm::LLVMContext& Ctx = TheModule.getContext();
  llvm::Metadata* IdentNode[] = {llvm::MDString::get(Ctx, Version)};
  IdentMetadata->addOperand(llvm::MDNode::get(Ctx, IdentNode));
}

CL_LISPIFY_NAME("getOrCreateUniquedStringGlobalVariable");
CL_DEFMETHOD GlobalVariable_sp Module_O::getOrCreateUniquedStringGlobalVariable(const string& value, const string& name) {
  core::SimpleBaseString_sp nameKey = core::SimpleBaseString_O::make(name);
  core::List_sp it = this->_UniqueGlobalVariableStrings->gethash(nameKey);
  //	map<string,GlobalVariableStringHolder>::iterator it = this->_UniqueGlobalVariableStrings.find(name);
  llvm::GlobalVariable* GV;
  if (it.nilp()) {
    llvm::Module* M = this->wrappedPtr();
    llvm::LLVMContext& context = M->getContext();
    llvm::Constant* StrConstant = llvm::ConstantDataArray::getString(context, value, true);
    GV = new llvm::GlobalVariable(*M, StrConstant->getType(), true, llvm::GlobalValue::PrivateLinkage, StrConstant, "");
    GV->setName(name);
    GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    //	    GlobalVariableStringHolder holder;
    core::SimpleBaseString_sp first = core::SimpleBaseString_O::make(value);
    GlobalVariable_sp second = core::RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable*>(GV);
    core::Cons_sp pair = core::Cons_O::create(first, second);
    this->_UniqueGlobalVariableStrings->setf_gethash(nameKey, pair);
    //	    holder._String = value;
    //	    holder._LlvmValue = core::RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(GV);
    //	    this->_UniqueGlobalVariableStrings[name] = holder;
    //	    return holder._LlvmValue;
    return second;
  }
  if (gc::As<core::String_sp>(oCar(it))->get_std_string() != value) {
    SIMPLE_ERROR("You tried to getOrCreateUniquedStringGlobalVariable with name[{}] and value[{}] - there was already a "
                 "StringGlobalVariable with that name but it has a different value!!!! value[{}]",
                 name, value, gc::As<core::String_sp>(oCar(it))->get_std_string());
  }
  return gc::As<GlobalVariable_sp>(oCdr(it)); // it->second._LlvmValue;
}

CL_LISPIFY_NAME("getGlobalList");
CL_DEFMETHOD core::List_sp Module_O::getGlobalList() const {
  ql::list globals;
  llvm::Module* m = this->wrappedPtr();
  for (llvm::Module::global_iterator it = m->global_begin(); it != m->global_end(); it++) {
    globals << core::RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable*>(&(*it));
  }
  return globals.cons();
}
}; // namespace llvmo

namespace llvmo {

void ExecutionEngine_O::initialize() { this->_DependentModules = core::HashTable_O::createEqual(); }

string ExecutionEngine_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << this->_ptr << " > ";
  return ss.str();
}

CL_LISPIFY_NAME("dependentModuleNames");
CL_DEFMETHOD core::List_sp ExecutionEngine_O::dependentModuleNames() const {
  ql::list l;
  this->_DependentModules->mapHash([&l](core::T_sp key, core::T_sp val) { l << key; });
  return l.cons();
}

void ExecutionEngine_O::addNamedModule(const string& name, Module_sp module) {
  core::SimpleBaseString_sp key = core::SimpleBaseString_O::make(name);
  if (this->_DependentModules->contains(key)) {
    //	if ( this->_DependentModules.count(name) != 0 )
    SIMPLE_ERROR("A module named {} is already in this ExecutionEngine - remove it first before adding another", name);
  }
  this->_DependentModules->setf_gethash(key, module);
  //	this->_DependentModules[name] = module;
  std::unique_ptr<llvm::Module> ownedModule(module->wrappedPtr());
  module->set_wrapped(NULL);
  this->wrappedPtr()->addModule(std::move(ownedModule));
}

CL_LISPIFY_NAME("hasNamedModule");
CL_DEFMETHOD bool ExecutionEngine_O::hasNamedModule(const string& name) {
  if (this->_DependentModules->contains(core::SimpleBaseString_O::make(name)))
    return true;
  return false;
}

void ExecutionEngine_O::removeNamedModule(const string& name) {
  core::SimpleBaseString_sp key = core::SimpleBaseString_O::make(name);
  core::T_mv mi = this->_DependentModules->gethash(key);
  //	core::StringMap<Module_O>::iterator mi = this->_DependentModules.find(name);
  core::MultipleValues& mvn = core::lisp_multipleValues();
  if (mvn.valueGet(1, mi.number_of_values()).nilp()) // == this->_DependentModules.end() )
  {
    SIMPLE_ERROR("Could not find named module {}", name);
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
  llvm::ExecutionEngine* ee = this->wrappedPtr();
  std::unique_ptr<llvm::Module> mod(module->wrappedPtr());
  module->set_wrapped(NULL);
  ee->addModule(std::move(mod));
}

CL_LISPIFY_NAME("removeModule");
CL_DEFMETHOD bool ExecutionEngine_O::removeModule(Module_sp module) {
  llvm::ExecutionEngine* ee = this->wrappedPtr();
  return ee->removeModule(module->wrappedPtr());
}

CL_LISPIFY_NAME("find_function_named");
CL_DEFMETHOD Function_sp ExecutionEngine_O::find_function_named(core::String_sp name) {
  return gc::As<Function_sp>(
      translate::to_object<llvm::Function*>::convert(this->wrappedPtr()->FindFunctionNamed(name->get_std_string().c_str())));
}

CL_LISPIFY_NAME(clearAllGlobalMappings);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::clearAllGlobalMappings);
CL_LISPIFY_NAME(getDataLayout);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getDataLayout);
CL_LISPIFY_NAME(getPointerToGlobalIfAvailable_GlobalValue);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, (void* (llvm::ExecutionEngine::*)(const llvm::GlobalValue*)) &
                                           llvm::ExecutionEngine::getPointerToGlobalIfAvailable);
CL_LISPIFY_NAME(getPointerToGlobalIfAvailable_StringRef);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O,
                    (void* (llvm::ExecutionEngine::*)(llvm::StringRef)) & llvm::ExecutionEngine::getPointerToGlobalIfAvailable);
CL_LISPIFY_NAME(getGlobalValueAddress);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getGlobalValueAddress);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getDataLayout);
CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getOrEmitGlobalVariable);

}; // namespace llvmo

namespace llvmo {

#if LLVM_VERSION_MAJOR < 18
CL_DEFUN bool llvm_sys__isOpaqueOrPointeeTypeMatches(Type_sp ptrType, Type_sp ty) {
  return dyn_cast<llvm::PointerType>(ptrType->wrappedPtr())->isOpaqueOrPointeeTypeMatches(ty->wrappedPtr());
}
#endif

}; // namespace llvmo

namespace llvmo {
CL_LISPIFY_NAME("DataLayoutCopy");
CL_DEFMETHOD DataLayout_sp DataLayout_O::copy() const {
  auto cp = gctools::GC<DataLayout_O>::allocate(*(this->_DataLayout));
  return cp;
};

CL_LISPIFY_NAME("data-layout-get-struct-layout");
CL_DEFMETHOD StructLayout_sp DataLayout_O::getStructLayout(StructType_sp ty) const {
  llvm::StructType* sty = ty->wrapped();
  const llvm::StructLayout* layout = this->_DataLayout->getStructLayout(sty);
  auto sl = gctools::GC<StructLayout_O>::allocate(layout);
  return sl;
}

CL_LISPIFY_NAME(DataLayout-getTypeAllocSize);
CL_DEFMETHOD size_t DataLayout_O::getTypeAllocSize(llvm::Type* ty) { return this->_DataLayout->getTypeAllocSize(ty); }

CL_LISPIFY_NAME(getStringRepresentation);
CL_EXTERN_DEFMETHOD(DataLayout_O, &llvm::DataLayout::getStringRepresentation);

CL_LISPIFY_NAME(struct-layout-get-element-offset);
CL_DEFMETHOD size_t StructLayout_O::getElementOffset(size_t idx) const {
  size_t offset = this->_StructLayout->getElementOffset(idx);
  return offset;
}

}; // namespace llvmo

namespace llvmo {

string EngineBuilder_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << this->_ptr << " > ";
  return ss.str();
}

CL_LAMBDA(module);
CL_PKG_NAME(LlvmoPkg,"make-EngineBuilder");
DOCGROUP(clasp);
CL_DEFUN EngineBuilder_sp EngineBuilder_O::make(Module_sp module) {
  auto self = gctools::GC<EngineBuilder_O>::allocate_with_default_constructor();
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
    SIMPLE_ERROR("{}", ss.str());
  }
}

CL_LISPIFY_NAME("setTargetOptions");
CL_DEFMETHOD void EngineBuilder_O::setTargetOptions(TargetOptions_sp options) {
  this->wrappedPtr()->setTargetOptions(*options->wrappedPtr());
}

;

CL_LISPIFY_NAME("create");
CL_DEFMETHOD ExecutionEngine_sp EngineBuilder_O::createExecutionEngine() {
  llvm::ExecutionEngine* ee = this->wrappedPtr()->create();
  ExecutionEngine_sp eeo = core::RP_Create_wrapped<ExecutionEngine_O, llvm::ExecutionEngine*>(ee);
  if (!eeo) {
    SIMPLE_ERROR("Could not create the execution-engine - got NULL");
  }
  return eeo;
}

}; // namespace llvmo

namespace llvmo {
Constant_sp Constant_O::create(llvm::Constant* ptr) { return core::RP_Create_wrapped<Constant_O, llvm::Constant*>(ptr); };
} // namespace llvmo

namespace llvmo {
CL_LAMBDA(context values);
CL_LISPIFY_NAME(constant-data-array-get-uint32);
DOCGROUP(clasp);
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
  llvm::Constant* llvm_ca = llvm::ConstantDataArray::get(*(context->wrappedPtr()), array_ref_vector_IdxList);
  ca->set_wrapped(llvm_ca);
  return ca;
}

;

}; // namespace llvmo

namespace llvmo {
CL_LAMBDA(type values);
CL_LISPIFY_NAME(constant-array-get);
DOCGROUP(clasp);
CL_DEFUN Constant_sp ConstantArray_O::get(ArrayType_sp type, core::List_sp values) {
  Constant_sp ca = ConstantArray_O::create();
  vector<llvm::Constant*> vector_IdxList;
  for (auto cur : values) {
    vector_IdxList.push_back(llvm::cast<llvm::Constant>(gc::As<Value_sp>(oCar(cur))->wrappedPtr()));
  }
  llvm::ArrayRef<llvm::Constant*> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant* llvm_ca = llvm::ConstantArray::get(type->wrapped(), array_ref_vector_IdxList);
  ca->set_wrapped(llvm_ca);
  return ca;
}

;

}; // namespace llvmo

namespace llvmo {
CL_LAMBDA(function basic-block);
CL_LISPIFY_NAME(block-address-get);
DOCGROUP(clasp);
CL_DEFUN BlockAddress_sp BlockAddress_O::get(Function_sp func, BasicBlock_sp bb) {
  BlockAddress_sp basp = BlockAddress_O::create();
  llvm::BlockAddress* ba = llvm::BlockAddress::get(func->wrappedPtr(), bb->wrappedPtr());
  basp->set_wrapped(ba);
  return basp;
}

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(constant-expr-get-in-bounds-get-element-ptr);
DOCGROUP(clasp);
CL_DEFUN Constant_sp ConstantExpr_O::getInBoundsGetElementPtr(llvm::Type* element_type, Constant_sp constant,
                                                              core::List_sp idxList) {
  auto res = gctools::GC<Constant_O>::allocate_with_default_constructor();
  if (element_type == NULL) {
    SIMPLE_ERROR("You must provide a type for ConstantExpr_O::getInBoundsGetElementPtr");
  }
  vector<llvm::Constant*> vector_IdxList;
  for (auto cur : idxList) {
    vector_IdxList.push_back(gc::As<Constant_sp>(oCar(cur))->wrappedPtr());
  }

  llvm::ArrayRef<llvm::Constant*> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant* llvm_constant = constant->wrappedPtr();
  llvm::Constant* llvm_res = llvm::ConstantExpr::getInBoundsGetElementPtr(element_type, llvm_constant, array_ref_vector_IdxList);
  res->set_wrapped(llvm_res);
  return res;
}
}; // namespace llvmo

namespace llvmo {
CL_DEFMETHOD void GlobalValue_O::setUnnamedAddr(llvm::GlobalValue::UnnamedAddr unnamed_addr) {
  this->wrappedPtr()->setUnnamedAddr(unnamed_addr);
}

CL_DEFMETHOD llvm::GlobalValue::UnnamedAddr GlobalValue_O::getUnnamedAddr() { return this->wrappedPtr()->getUnnamedAddr(); }

}; // namespace llvmo
namespace llvmo {

CL_LAMBDA("module type is-constant linkage initializer name &optional (insert-before nil) (thread-local-mode 'llvm-sys:not-thread-local)");
CL_LISPIFY_NAME(make-global-variable);
DOCGROUP(clasp);
CL_DEFUN GlobalVariable_sp GlobalVariable_O::make(Module_sp mod, Type_sp type, bool isConstant,
                                                  llvm::GlobalValue::LinkageTypes linkage,
                                                  /*Constant_sp*/ core::T_sp initializer, core::String_sp name,
                                                  /*GlobalVariable_sp*/ core::T_sp insertBefore,
                                                  llvm::GlobalValue::ThreadLocalMode threadLocalMode) {
  auto me = gctools::GC<GlobalVariable_O>::allocate_with_default_constructor();
  llvm::Constant* llvm_initializer = NULL;
  if (initializer.notnilp()) {
    llvm_initializer = gc::As<Constant_sp>(initializer)->wrappedPtr();
  }
  llvm::GlobalVariable* lInsertBefore = NULL;
  if (insertBefore.notnilp()) {
    lInsertBefore = gc::As<GlobalVariable_sp>(insertBefore)->wrappedPtr();
  }
  llvm::GlobalVariable* gv = new llvm::GlobalVariable(*(mod->wrappedPtr()), type->wrappedPtr(), isConstant, linkage,
                                                      llvm_initializer, name->get_std_string(), lInsertBefore, threadLocalMode);
  me->set_wrapped(gv);
  //	me->set_ptrIsOwned(true); // GlobalVariables made this way are responsible for freeing their pointers - I hope this isn't a
  // disaster
  return me;
};

CL_LISPIFY_NAME(eraseFromParent);
CL_EXTERN_DEFMETHOD(GlobalVariable_O, &llvm::GlobalVariable::eraseFromParent);
CL_LISPIFY_NAME(setInitializer);
CL_EXTERN_DEFMETHOD(GlobalVariable_O, &llvm::GlobalVariable::setInitializer);
;

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME("setMetadata");
CL_DEFMETHOD void Instruction_O::setMetadata(core::String_sp kind, MDNode_sp mdnode) {
  this->wrappedPtr()->setMetadata(kind->get_std_string(), mdnode->wrappedPtr());
}

CL_LISPIFY_NAME(getParent);
CL_EXTERN_DEFMETHOD(Instruction_O, (llvm::BasicBlock * (llvm::Instruction::*)()) & llvm::Instruction::getParent);

CL_LISPIFY_NAME(getDebugLocInfo);
DOCGROUP(clasp);
CL_DEFUN core::T_mv llvm_sys__getDebugLocInfo(Instruction_sp instr) {
  const llvm::DebugLoc& debugLoc = instr->wrappedPtr()->getDebugLoc();
  if (debugLoc) {
    size_t lineno = debugLoc.getLine();
    size_t column = debugLoc.getCol();
    return Values(_lisp->_true(), core::make_fixnum(lineno), core::make_fixnum(column));
  }
  return Values(nil<core::T_O>());
}
CL_DOCSTRING(R"dx(Erase the instruction from its parent basic block and return the next instruction or NIL)dx");
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__instruction_eraseFromParent(Instruction_sp instr) {
#if LLVM_VERSION_MAJOR < 18
  [[maybe_unused]] llvm::SymbolTableList<llvm::Instruction>::iterator next = instr->wrappedPtr()->eraseFromParent();
#else
  [[maybe_unused]] llvm::AllocaInst::InstListType::iterator next = instr->wrappedPtr()->eraseFromParent();
#endif
}

CL_DOCSTRING(R"dx(Return the next non-debug instruction or NIL if there is none)dx");
DOCGROUP(clasp);
CL_DEFUN core::T_sp llvm_sys__instruction_getNextNonDebugInstruction(Instruction_sp instr) {
  const llvm::Instruction* next = instr->wrappedPtr()->getNextNonDebugInstruction();
  if (next != NULL) {
    return translate::to_object<llvm::Instruction*>::convert(const_cast<llvm::Instruction*>(next));
  }
  return nil<core::T_O>();
};

CL_LISPIFY_NAME("insertAfter");
CL_EXTERN_DEFMETHOD(Instruction_O, &llvm::Instruction::insertAfter);
#if LLVM_VERSION_MAJOR < 18
CL_LISPIFY_NAME("insertBefore");
CL_EXTERN_DEFMETHOD(Instruction_O, &llvm::Instruction::insertBefore);
#endif

CL_LISPIFY_NAME("terminatorInstP");
CL_DEFMETHOD bool Instruction_O::terminatorInstP() const { return this->wrappedPtr()->isTerminator(); }

}; // namespace llvmo

namespace llvmo {
#if 0
DOCGROUP(clasp);
CL_DEFUN llvm::Instruction* llvm_sys__replace_call_keep_args(llvm::Function* func, llvm::Instruction* callOrInvoke) {
//  printf("%s:%d In llvm-sys::replace-call\n",__FILE__, __LINE__);
//  llvm::CallSite CS(callOrInvoke);
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
#endif

DOCGROUP(clasp);
CL_DEFUN llvm::Instruction* llvm_sys__replace_call(llvm::Function* func, llvm::Instruction* callOrInvoke,
                                                   llvm::ArrayRef<llvm::Value*> args) {
  //  printf("%s:%d In llvm-sys::replace-call\n",__FILE__, __LINE__);
  //  llvm::CallSite CS(callOrInvoke);
  llvm::Instruction* NewCI = NULL;
  if (llvm::isa<llvm::CallInst>(callOrInvoke)) {
    llvm::CallInst* NewCall = llvm::CallInst::Create(func, args);
    NewCall->setCallingConv(func->getCallingConv());
    NewCI = NewCall;
  } else if (llvm::isa<llvm::InvokeInst>(callOrInvoke)) {
    llvm::InvokeInst* invoke = llvm::cast<llvm::InvokeInst>(callOrInvoke);
    llvm::BasicBlock* ifNormal = invoke->getNormalDest();
    llvm::BasicBlock* ifException = invoke->getUnwindDest();
    llvm::InvokeInst* NewInvoke = llvm::InvokeInst::Create(func, ifNormal, ifException, args);
    NewInvoke->setCallingConv(func->getCallingConv());
    NewCI = NewInvoke;
  }
  if (!callOrInvoke->use_empty()) {
    callOrInvoke->replaceAllUsesWith(NewCI);
  }
  llvm::ReplaceInstWithInst(callOrInvoke, NewCI);
  return NewCI;
};

// FIXME: Should be made into a generic function or something.
CL_LISPIFY_NAME(getCallingConv);
CL_EXTERN_DEFMETHOD(CallBase_O, (llvmo::ClaspCallingConv(llvm::CallBase::*)())&CallBase_O::ExternalType::getCallingConv);
CL_LISPIFY_NAME(setCallingConv);
CL_EXTERN_DEFMETHOD(CallBase_O, (void(llvm::CallBase::*)(llvmo::ClaspCallingConv)) & CallBase_O::ExternalType::setCallingConv);

core::List_sp CallBase_O::getArgumentList() const {
  ql::list l;
  for (auto arg = this->wrappedPtr()->arg_begin(), argEnd(this->wrappedPtr()->arg_end()); arg != argEnd; ++arg) {
    l << translate::to_object<llvm::Value*>::convert(*arg);
  }
  return l.cons();
};

DOCGROUP(clasp);
CL_DEFUN core::List_sp llvm_sys__call_or_invoke_getArgumentList(CallBase_sp callOrInvoke) {
  core::List_sp result = callOrInvoke->getArgumentList();
#ifdef DEBUG_EVALUATE
  if (core::_sym_STARdebugEvalSTAR && core::_sym_STARdebugEvalSTAR->symbolValue().notnilp()) {
    printf("%s:%d:%s on %s result = %s\n", __FILE__, __LINE__, __FUNCTION__, _rep_(callOrInvoke).c_str(), _rep_(result).c_str());
  }
#endif
  return result;
}

CL_DEFMETHOD void CallBase_O::addParamAttr(unsigned index, llvm::Attribute::AttrKind attrkind) {
  this->wrappedPtr()->addParamAttr(index, attrkind);
}

CL_DEFMETHOD llvm::Function* CallBase_O::getCalledFunction() { return this->wrappedPtr()->getCalledFunction(); };

}; // namespace llvmo
namespace llvmo {

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(setCleanup);
CL_EXTERN_DEFMETHOD(LandingPadInst_O, &llvm::LandingPadInst::setCleanup);
CL_LISPIFY_NAME(isCleanup);
CL_EXTERN_DEFMETHOD(LandingPadInst_O, &llvm::LandingPadInst::isCleanup);
CL_LISPIFY_NAME(addClause);
CL_EXTERN_DEFMETHOD(LandingPadInst_O, &llvm::LandingPadInst::addClause);
;

;

}; // namespace llvmo

namespace llvmo {
DOCGROUP(clasp);
CL_DEFUN llvm::AllocaInst* llvm_sys__insert_alloca_before_terminator(llvm::Type* type, const llvm::Twine& name,
                                                                     llvm::BasicBlock* block) {
  //  printf("%s:%d   llvm-sys::insert-alloca\n", __FILE__, __LINE__ );
  llvm::Instruction* insertBefore = block->getTerminator();
  llvm::AllocaInst* alloca = new llvm::AllocaInst(type, 0, name, insertBefore);
  return alloca;
}

/*! I can't seem to get from_object working for MaybeAlign type */
CL_DEFMETHOD void AllocaInst_O::setAlignment(core::T_sp align) {
  if (align.nilp()) {
    llvm::Align a;
    this->wrappedPtr()->setAlignment(a);
    return;
  } else if (align.fixnump()) {
    llvm::Align a(align.unsafe_fixnum());
    this->wrappedPtr()->setAlignment(a);
    return;
  }
  SIMPLE_ERROR("{} is an invalid argument for setAlignment", _rep_(align));
}

}; // namespace llvmo

namespace llvmo {

;

CL_LISPIFY_NAME("addCase");
CL_DEFMETHOD void SwitchInst_O::addCase(ConstantInt_sp onVal, BasicBlock_sp dest) {
  this->wrappedPtr()->addCase(onVal->wrappedPtr(), dest->wrappedPtr());
}

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(addDestination);
CL_EXTERN_DEFMETHOD(IndirectBrInst_O, &llvm::IndirectBrInst::addDestination);
;

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(set_atomic);
CL_EXTERN_DEFMETHOD(StoreInst_O, &StoreInst_O::ExternalType::setAtomic);

/*! I can't seem to get from_object working for MaybeAlign type */
CL_DEFMETHOD void StoreInst_O::setAlignment(core::T_sp align) {
  if (align.nilp()) {
    llvm::Align a;
    this->wrappedPtr()->setAlignment(a);
    return;
  } else if (align.fixnump()) {
    llvm::Align a(align.unsafe_fixnum());
    this->wrappedPtr()->setAlignment(a);
    return;
  }
  SIMPLE_ERROR("{} is an invalid argument for setAlignment", _rep_(align));
}
CL_LISPIFY_NAME(set_atomic);
CL_EXTERN_DEFMETHOD(LoadInst_O, &LoadInst_O::ExternalType::setAtomic);

/*! I can't seem to get from_object working for MaybeAlign type */
CL_DEFMETHOD void LoadInst_O::setAlignment(core::T_sp align) {
  if (align.nilp()) {
    llvm::Align a;
    this->wrappedPtr()->setAlignment(a);
    return;
  } else if (align.fixnump()) {
    llvm::Align a(align.unsafe_fixnum());
    this->wrappedPtr()->setAlignment(a);
    return;
  }
  SIMPLE_ERROR("{} is an invalid argument for setAlignment", _rep_(align));
}

}; // namespace llvmo

namespace llvmo {
ConstantFP_sp ConstantFP_O::create(llvm::ConstantFP* ptr) { return core::RP_Create_wrapped<ConstantFP_O, llvm::ConstantFP*>(ptr); };
} // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(constantFpGet);
CL_EXTERN_DEFUN((llvm::ConstantFP * (*)(llvm::LLVMContext&, const llvm::APFloat&)) & llvm::ConstantFP::get);
CL_LISPIFY_NAME(constantFpGetTypeDouble);
CL_EXTERN_DEFUN((llvm::Constant * (*)(llvm::Type*, double)) & llvm::ConstantFP::get);
CL_LISPIFY_NAME(constantFpGetTypeStringref);
CL_EXTERN_DEFUN((llvm::Constant * (*)(llvm::Type * type, llvm::StringRef label)) & llvm::ConstantFP::get);

;

string ConstantFP_O::__repr__() const {
  stringstream ss;
  llvm::APFloat const& val = this->wrappedPtr()->getValueAPF();
  llvm::SmallVector<char, 100> svistr;
  val.toString(svistr);
  std::string str(svistr.data(), svistr.size());
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << str << ">";
  return ss.str();
}

}; // namespace llvmo

namespace llvmo {
ConstantInt_sp ConstantInt_O::create(llvm::ConstantInt* ptr) {
  return core::RP_Create_wrapped<ConstantInt_O, llvm::ConstantInt*>(ptr);
};

CL_LISPIFY_NAME(constant-int-get);
CL_EXTERN_DEFUN((llvm::ConstantInt * (*)(llvm::LLVMContext&, const llvm::APInt&)) & llvm::ConstantInt::get);

CL_LISPIFY_NAME(get-true);
CL_EXTERN_DEFUN((llvm::ConstantInt * (*)(llvm::LLVMContext&)) & llvm::ConstantInt::getTrue);

CL_LISPIFY_NAME(get-false);
CL_EXTERN_DEFUN((llvm::ConstantInt * (*)(llvm::LLVMContext&)) & llvm::ConstantInt::getFalse);

;

string ConstantInt_O::__repr__() const {
  stringstream ss;
  llvm::SmallString<40> nstr;
  this->wrappedPtr()->getValue().toString(nstr, 10, true);
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " " << nstr.str().str() << ">";
  return ss.str();
}
}; // namespace llvmo

namespace llvmo {
ConstantStruct_sp ConstantStruct_O::create(llvm::ConstantStruct* ptr) {
  return core::RP_Create_wrapped<ConstantStruct_O, llvm::ConstantStruct*>(ptr);
};

CL_LISPIFY_NAME(CONSTANT-STRUCT-GET);
CL_EXTERN_DEFUN((llvm::Constant * (*)(llvm::StructType * T, llvm::ArrayRef<llvm::Constant*>)) & llvm::ConstantStruct::get);

;

}; // namespace llvmo

namespace llvmo {
UndefValue_sp UndefValue_O::create(llvm::UndefValue* ptr) { return core::RP_Create_wrapped<UndefValue_O, llvm::UndefValue*>(ptr); };

CL_LISPIFY_NAME(UNDEF_VALUE-GET);
CL_EXTERN_DEFUN((llvm::UndefValue * (*)(llvm::Type * type)) & llvm::UndefValue::get);

;

string UndefValue_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << ">";
  return ss.str();
}
}; // namespace llvmo

namespace llvmo {
ConstantPointerNull_sp ConstantPointerNull_O::create(llvm::ConstantPointerNull* ptr) {
  return core::RP_Create_wrapped<ConstantPointerNull_O, llvm::ConstantPointerNull*>(ptr);
};

CL_LISPIFY_NAME(constant-pointer-null-get);
CL_EXTERN_DEFUN((llvm::ConstantPointerNull * (*)(llvm::PointerType * T)) & llvm::ConstantPointerNull::get);

;

string ConstantPointerNull_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << ">";
  return ss.str();
}
}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(value);
CL_LISPIFY_NAME(make-apfloat-float);
DOCGROUP(clasp);
CL_DEFUN APFloat_sp APFloat_O::makeAPFloatFloat(core::SingleFloat_sp value) {
  auto self = gctools::GC<APFloat_O>::allocate_with_default_constructor();
  self->_valueP = new llvm::APFloat(unbox_single_float(value));
  return self;
};

CL_LAMBDA(value);
CL_LISPIFY_NAME(makeAPFloatDouble);
DOCGROUP(clasp);
CL_DEFUN APFloat_sp APFloat_O::makeAPFloatDouble(core::DoubleFloat_sp value) {
  auto self = gctools::GC<APFloat_O>::allocate_with_default_constructor();
  self->_valueP = new llvm::APFloat(value->get());
  return self;
};
} // namespace llvmo

namespace llvmo {

APInt_sp APInt_O::create(llvm::APInt api) {
  auto self = gctools::GC<APInt_O>::allocate_with_default_constructor();
  self->_value = api;
  return self;
}

} // namespace llvmo

namespace llvmo {
DOCGROUP(clasp);
CL_DEFUN APInt_sp APInt_O::makeAPInt1(core::T_sp value) {
  auto self = gctools::GC<APInt_O>::allocate_with_default_constructor();
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

DOCGROUP(clasp);
CL_DEFUN APInt_sp APInt_O::makeAPIntWidth(core::Integer_sp value, uint width, bool sign) {
  auto self = gctools::GC<APInt_O>::allocate_with_default_constructor();
  llvm::APInt apint;
  int numbits;
  if (value.fixnump()) {
    Fixnum fixnum_value = value.unsafe_fixnum();
    if (!sign && fixnum_value < 0) {
      SIMPLE_ERROR("You tried to create an unsigned APInt32 with the negative value: {}", fixnum_value);
    }
    apint = llvm::APInt(width, fixnum_value, sign);
    numbits = gc::fixnum_bits;
  } else { // bignum
    // TODO: use As_unsafe
    core::Bignum_sp bignum_value = gc::As<core::Bignum_sp>(value);
    mp_size_t len = bignum_value->length();
    mp_size_t size = std::abs(len);
    const mp_limb_t* limbs = bignum_value->limbs();
    uint64_t words[size];
    if (len < 0)
      for (size_t i = 0; i < size; ++i)
        words[i] = -(limbs[i]);
    else
      for (size_t i = 0; i < size; ++i)
        words[i] = limbs[i];
    // Note that APInt has its own storage, so it's fine that words expires.
#if LLVM_VERSION_MAJOR < 19    
    apint = llvm::APInt(width, llvm::makeArrayRef(words, size));
#else
    apint = llvm::APInt(width, llvm::ArrayRef(words, size));
#endif
  }
  self->_value = apint;
  return self;
}

DOCGROUP(clasp);
CL_DEFUN APInt_sp APInt_O::makeAPInt32(core::Integer_sp value) { return APInt_O::makeAPIntWidth(value, 32, true); }

DOCGROUP(clasp);
CL_DEFUN APInt_sp APInt_O::makeAPInt64(core::Integer_sp value) { return APInt_O::makeAPIntWidth(value, 64, true); }

} // namespace llvmo

namespace llvmo {

SYMBOL_EXPORT_SC_(LlvmoPkg, makeAPInt1);
SYMBOL_EXPORT_SC_(LlvmoPkg, makeAPInt);
SYMBOL_EXPORT_SC_(LlvmoPkg, makeAPWidth);
SYMBOL_EXPORT_SC_(LlvmoPkg, makeAP32);
SYMBOL_EXPORT_SC_(LlvmoPkg, makeAP64);

;

__attribute__((noinline)) CL_DEFUN string llvm_sys__apint_to_string(APInt_sp api, int radix, bool isigned) {
  llvm::SmallString<256> istr;
  api->_value._value.toString(istr, radix, isigned);
  return istr.str().str();
}

CL_LAMBDA(api &optional (issigned t))CL_LISPIFY_NAME("toInteger");
DOCGROUP(clasp);
CL_DEFUN core::Integer_sp toInteger(APInt_sp api, bool issigned) {
  llvm::SmallString<256> istr;
  api->_value._value.toString(istr, 10, issigned);
  return core::Integer_O::create(istr.str().str());
}

string APInt_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  llvm::SmallString<256> istr;
  this->_value._value.toString(istr, 10, true);
  ss << istr.str().str();
  ss << ">";
  return ss.str();
}
}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(CreateGlobalString);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateGlobalString);
CL_PKG_NAME(LlvmoPkg,"SetInsertPointBasicBlock");
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (void(llvm::IRBuilderBase::*)(llvm::BasicBlock*)) & llvm::IRBuilderBase::SetInsertPoint);
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
  if (ins != NULL) {
    Instruction_sp isp = Instruction_O::create();
    isp->set_wrapped(ins);
    return isp;
  }
  return nil<core::T_O>();
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
  llvm::DILocation* real_diloc = diloc->operator llvm::DILocation*();
  llvm::DebugLoc dl(real_diloc);
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
}

CL_LISPIFY_NAME("SetCurrentDebugLocationToLineColumnScope");
CL_DEFMETHOD void IRBuilderBase_O::SetCurrentDebugLocationToLineColumnScope(int line, int col, DINode_sp scope) {
  this->_CurrentDebugLocationSet = true;
  llvm::MDNode* mdnode = scope->operator llvm::MDNode*();
  llvm::DILocation* dl = llvm::DILocation::get(mdnode->getContext(), line, col, mdnode);
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
}

}; // namespace llvmo

namespace llvmo {
CL_LISPIFY_NAME(make-irbuilder);
DOCGROUP(clasp);
CL_DEFUN IRBuilder_sp IRBuilder_O::make(LLVMContext_sp context) {
  auto self = gctools::GC<IRBuilder_O>::allocate_with_default_constructor();
  self->set_wrapped(new llvm::IRBuilder<>(*(context->wrappedPtr())));
  return self;
};

CL_LISPIFY_NAME("CreateInvoke");
CL_DEFMETHOD llvm::InvokeInst* IRBuilder_O::CreateInvoke(FunctionType_sp function_type, llvm::Value* Callee,
                                                         llvm::BasicBlock* NormalDest, llvm::BasicBlock* UnwindDest,
                                                         core::List_sp Args, const llvm::Twine& Name) {
  vector<llvm::Value*> vector_Args;
  for (auto cur : Args) {
    if (Value_sp val = oCar(cur).asOrNull<Value_O>()) {
      vector_Args.push_back(val->wrappedPtr());
    } else {
      vector_Args.push_back(NULL);
    }
  }
  llvm::ArrayRef<llvm::Value*> array_ref_vector_Args(vector_Args);
  llvm::FunctionCallee function_callee(function_type->wrapped(), Callee);
  return this->wrappedPtr()->CreateInvoke(function_callee, NormalDest, UnwindDest, array_ref_vector_Args, Name);
}

CL_LISPIFY_NAME(CreateConstGEP2_32);
CL_DEFMETHOD llvm::Value* IRBuilder_O::CreateConstGEP2_32(llvm::Type* ty, llvm::Value* ptr, int idx0, int idx1,
                                                          const llvm::Twine& Name) {
  int uidx0 = static_cast<int>(idx0);
  int uidx1 = static_cast<int>(idx1);
  return this->wrappedPtr()->CreateConstGEP2_32(ty, ptr, uidx0, uidx1, Name);
}

CL_LISPIFY_NAME(CreateConstGEP2_64);
CL_DEFMETHOD llvm::Value* IRBuilder_O::CreateConstGEP2_64(llvm::Type* type, llvm::Value* Ptr, size_t idx0, size_t idx1,
                                                          const llvm::Twine& Name) {
  size_t uidx0 = static_cast<size_t>(idx0);
  size_t uidx1 = static_cast<size_t>(idx1);
  return this->wrappedPtr()->CreateConstGEP2_64(type, Ptr, uidx0, uidx1, Name);
}

CL_LISPIFY_NAME("CreateInBoundsGEP");
CL_DEFMETHOD llvm::Value* IRBuilder_O::CreateInBoundsGEP(llvm::Type* type, llvm::Value* Ptr, core::List_sp IdxList,
                                                         const llvm::Twine& Name) {
  vector<llvm::Value*> vector_IdxList;
  for (auto cur : IdxList) {
    vector_IdxList.push_back(gc::As<Value_sp>(oCar(cur))->wrappedPtr());
  }
  llvm::ArrayRef<llvm::Value*> array_ref_vector_IdxList(vector_IdxList);
  return this->wrappedPtr()->CreateInBoundsGEP(type, Ptr, array_ref_vector_IdxList, Name);
}

CL_LISPIFY_NAME("CreateExtractValue");
CL_DEFMETHOD llvm::Value* IRBuilder_O::CreateExtractValue(llvm::Value* Ptr, core::List_sp IdxList, const llvm::Twine& Name) {
  vector<unsigned int> vector_IdxList;
  for (auto cur : IdxList) {
    vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(oCar(cur))));
  }
  llvm::ArrayRef<unsigned int> array_ref_vector_IdxList(vector_IdxList);
  return this->wrappedPtr()->CreateExtractValue(Ptr, array_ref_vector_IdxList, Name);
}

CL_LISPIFY_NAME("CreateInsertValue");
CL_DEFMETHOD llvm::Value* IRBuilder_O::CreateInsertValue(llvm::Value* Agg, llvm::Value* Val, core::List_sp IdxList,
                                                         const llvm::Twine& Name) {
  vector<unsigned int> vector_IdxList;
  for (auto cur : IdxList) {
    vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(oCar(cur))));
  }
  llvm::ArrayRef<unsigned int> array_ref_vector_IdxList(vector_IdxList);
  return this->wrappedPtr()->CreateInsertValue(Agg, Val, array_ref_vector_IdxList, Name);
}

string IRBuilder_O::__repr__() const {
  IRBuilder_O* irbuilder = const_cast<IRBuilder_O*>(this);
  stringstream ss;
  ss << "#<" << this->_instanceClass()->_classNameAsString() << " ";
  llvm::BasicBlock* bb = irbuilder->wrappedPtr()->GetInsertBlock();
  if (bb) {
    ss << " :insert-block-name " << bb->getName().data();
    llvm::Function* func = bb->getParent();
    if (func) {
      ss << " :function " << func->getName().data();
    } else {
      ss << " :function UNDEFINED-FUNCTION! ";
    }
  } else {
    ss << " :insert-block-name UNDEFINED-BASIC_BLOCK! ";
  }
#ifdef NON_MOVING_GC
  ss << "@" << (void*)this->_ptr;
#endif
  ss << " >";
  return ss.str();
}

// ------------------------------------------------------------
// ------------------------------------------------------------
// ------------------------------------------------------------
// #include "src/llvmo/fixinlines.cc"

CL_LISPIFY_NAME(addIncoming);
// CL_EXTERN_DEFMETHOD(PHINode_O, &llvm::PHINode::addIncoming);;
DOCGROUP(clasp);
CL_DEFUN void addIncoming(llvm::PHINode* object, llvm::Value* V, llvm::BasicBlock* BB) { object->addIncoming(V, BB); }

CL_PKG_NAME(LlvmoPkg,"SetInsertPointInstruction");
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (void (llvm::IRBuilderBase::*)(llvm::Instruction *))&llvm::IRBuilderBase::SetInsertPoint);
DOCGROUP(clasp);
CL_DEFUN void SetInsertPointInstruction(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Instruction* instruction) {
  object->SetInsertPoint(instruction);
}

CL_LISPIFY_NAME(CreateBinOp);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateBinOp);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateBinOp(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Instruction::BinaryOps Opc, llvm::Value* LHS,
                                  llvm::Value* RHS, const llvm::Twine& Name, llvm::MDNode* FPMathTag) {
  return object->CreateBinOp(Opc, LHS, RHS, Name, FPMathTag);
}

CL_LISPIFY_NAME(CreateCast);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateCast);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateCast(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Instruction::CastOps Op, llvm::Value* V,
                                 llvm::Type* DestTy, const llvm::Twine& Name) {
  return object->CreateCast(Op, V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateInsertElement);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::Value*(IRBuilderBase_O::ExternalType::*) (llvm::Value *Vec, llvm::Value *NewElt,
// llvm::Value* Idx, const llvm::Twine &Name) )&IRBuilderBase_O::ExternalType::CreateInsertElement);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateInsertElement(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* Vec, llvm::Value* NewElt,
                                          llvm::Value* Idx, const llvm::Twine& Name) {
  return object->CreateInsertElement(Vec, NewElt, Idx, Name);
}

CL_LISPIFY_NAME(CreateExtractElement);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::Value*(IRBuilderBase_O::ExternalType::*) (llvm::Value *Vec, llvm::Value* Idx, const
// llvm::Twine &Name) )&IRBuilderBase_O::ExternalType::CreateExtractElement);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateExtractElement(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* Vec, llvm::Value* Idx,
                                           const llvm::Twine& Name) {
  return object->CreateExtractElement(Vec, Idx, Name);
}

CL_LISPIFY_NAME(CreateCallFunctionPointer);
CL_LAMBDA(irbuilder function_type callee args name &optional (fpmathtag nil));
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::CallInst *(IRBuilderBase_O::ExternalType::*)(llvm::FunctionType *FTy, llvm::Value
// *Callee, llvm::ArrayRef<llvm::Value *> Args, const llvm::Twine &Name, llvm::MDNode* FPMathTag
// ))&IRBuilderBase_O::ExternalType::CreateCall);
DOCGROUP(clasp);
CL_DEFUN llvm::CallInst* CreateCall(llvmo::IRBuilderBase_O::ExternalType* object, llvm::FunctionType* FTy, llvm::Value* Callee,
                                    llvm::ArrayRef<llvm::Value*> Args, const llvm::Twine& Name, llvm::MDNode* FPMathTag) {
  return object->CreateCall(FTy, Callee, Args, Name, FPMathTag);
}

CL_LISPIFY_NAME(CreatePHI);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreatePHI);
DOCGROUP(clasp);
CL_DEFUN llvm::PHINode* CreatePHI(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Type* Ty, unsigned NumReservedValues,
                                  const llvm::Twine& Name) {
  return object->CreatePHI(Ty, NumReservedValues, Name);
}

CL_LISPIFY_NAME(CreateICmp);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmp);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateICmp(llvmo::IRBuilderBase_O::ExternalType* object, llvm::CmpInst::Predicate P, llvm::Value* LHS,
                                 llvm::Value* RHS, const llvm::Twine& Name) {
  return object->CreateICmp(P, LHS, RHS, Name);
}

CL_LISPIFY_NAME(CreatePointerCast);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreatePointerCast);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreatePointerCast(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                        const llvm::Twine& Name) {
  return object->CreatePointerCast(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateTruncOrBitCast);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateTruncOrBitCast);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateTruncOrBitCast(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                           const llvm::Twine& Name) {
  return object->CreateTruncOrBitCast(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateZExtOrBitCast);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateZExtOrBitCast);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateZExtOrBitCast(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                          const llvm::Twine& Name) {
  return object->CreateZExtOrBitCast(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateConstInBoundsGEP2_32);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateConstInBoundsGEP2_32);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateConstInBoundsGEP2_32(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Type* Ty, llvm::Value* Ptr,
                                                 unsigned Idx0, unsigned Idx1, const llvm::Twine& Name) {
  return object->CreateConstInBoundsGEP2_32(Ty, Ptr, Idx0, Idx1, Name);
}

CL_LISPIFY_NAME(CreateAtomicRMW);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateAtomicRMW);
DOCGROUP(clasp);
CL_DEFUN llvm::AtomicRMWInst* CreateAtomicRMW(llvmo::IRBuilderBase_O::ExternalType* object, llvm::AtomicRMWInst::BinOp Op,
                                              llvm::Value* Ptr, llvm::Value* Val, llvm::MaybeAlign Align,
                                              llvm::AtomicOrdering Ordering, llvm::SyncScope::ID SSID) {
  return object->CreateAtomicRMW(Op, Ptr, Val, Align, Ordering, SSID);
}

CL_LISPIFY_NAME(CreateAtomicCmpXchg);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateAtomicCmpXchg);
DOCGROUP(clasp);
CL_DEFUN llvm::AtomicCmpXchgInst* CreateAtomicCmpXchg(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* Ptr,
                                                      llvm::Value* Cmp, llvm::Value* New, llvm::MaybeAlign Align,
                                                      llvm::AtomicOrdering SuccessOrdering, llvm::AtomicOrdering FailureOrdering,
                                                      llvm::SyncScope::ID SSID) {
  return object->CreateAtomicCmpXchg(Ptr, Cmp, New, Align, SuccessOrdering, FailureOrdering, SSID);
}

CL_LISPIFY_NAME(CreateFence);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFence);
DOCGROUP(clasp);
CL_DEFUN llvm::FenceInst* CreateFence(llvmo::IRBuilderBase_O::ExternalType* object, llvm::AtomicOrdering Ordering,
                                      llvm::SyncScope::ID SSID, const llvm::Twine& Name) {
  return object->CreateFence(Ordering, SSID, Name);
}

CL_LISPIFY_NAME(CreateAlloca);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (AllocaInst* (IRBuilderBase_O::ExternalType::*)(llvm::Type *, llvm::Value *, const
// llvm::Twine &))&IRBuilderBase_O::ExternalType::CreateAlloca);
DOCGROUP(clasp);
CL_DEFUN llvm::AllocaInst* CreateAlloca(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Type* Ty, llvm::Value* ArraySize,
                                        const llvm::Twine& Name) {
  return object->CreateAlloca(Ty, ArraySize, Name);
}

CL_LISPIFY_NAME(CreateNot);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNot);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateNot(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, const llvm::Twine& Name) {
  return object->CreateNot(V, Name);
}

CL_LISPIFY_NAME(CreateFNeg);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFNeg);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateFNeg(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, const llvm::Twine& Name,
                                 llvm::MDNode* FPMathTag) {
  return object->CreateFNeg(V, Name, FPMathTag);
}

CL_LISPIFY_NAME(CreateNeg);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNeg);
DOCGROUP(clasp);
#if LLVM_VERSION_MAJOR < 19
CL_DEFUN llvm::Value* CreateNeg(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, const llvm::Twine& Name, bool HasNUW,
                                bool HasNSW) {
  return object->CreateNeg(V, Name, HasNUW, HasNSW);
}
#else
CL_DEFUN llvm::Value* CreateNeg(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, const llvm::Twine& Name,
                                bool HasNSW) {
  return object->CreateNeg(V, Name, HasNSW);
}
#endif

CL_LISPIFY_NAME(CreateSRem);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSRem);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateSRem(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                 const llvm::Twine& Name) {
  return object->CreateSRem(LHS, RHS, Name);
}

CL_LISPIFY_NAME(CreateURem);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateURem);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateURem(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                 const llvm::Twine& Name) {
  return object->CreateURem(LHS, RHS, Name);
}

CL_LISPIFY_NAME(CreateFDiv);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFDiv);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateFDiv(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* L, llvm::Value* R,
                                 const llvm::Twine& Name, llvm::MDNode* FPMD) {
  return object->CreateFDiv(L, R, Name, FPMD);
}

CL_LISPIFY_NAME(CreateSDiv);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSDiv);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateSDiv(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                 const llvm::Twine& Name, bool isExact) {
  return object->CreateSDiv(LHS, RHS, Name, isExact);
}

CL_LISPIFY_NAME(CreateUDiv);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateUDiv);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateUDiv(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                 const llvm::Twine& Name, bool isExact) {
  return object->CreateUDiv(LHS, RHS, Name, isExact);
}

CL_LISPIFY_NAME(CreateFMul);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFMul);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateFMul(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* L, llvm::Value* R,
                                 const llvm::Twine& Name, llvm::MDNode* FPMD) {
  return object->CreateFMul(L, R, Name, FPMD);
}

CL_LISPIFY_NAME(CreateMul);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateMul);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateMul(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                const llvm::Twine& Name, bool HasNUW, bool HasNSW) {
  return object->CreateMul(LHS, RHS, Name, HasNUW, HasNSW);
}

CL_LISPIFY_NAME(CreateFSub);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFSub);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateFSub(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* L, llvm::Value* R,
                                 const llvm::Twine& Name, llvm::MDNode* FPMD) {
  return object->CreateFSub(L, R, Name, FPMD);
}

CL_LISPIFY_NAME(CreateSub);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSub);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateSub(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                const llvm::Twine& Name, bool HasNUW, bool HasNSW) {
  return object->CreateSub(LHS, RHS, Name, HasNUW, HasNSW);
}

CL_LISPIFY_NAME(CreateFAdd);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFAdd);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateFAdd(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* L, llvm::Value* R,
                                 const llvm::Twine& Name, llvm::MDNode* FPMD) {
  return object->CreateFAdd(L, R, Name, FPMD);
}

CL_LISPIFY_NAME(CreateUnreachable);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateUnreachable);
DOCGROUP(clasp);
CL_DEFUN llvm::UnreachableInst* CreateUnreachable(llvmo::IRBuilderBase_O::ExternalType* object) {
  return object->CreateUnreachable();
}

CL_LISPIFY_NAME(CreateSwitch);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSwitch);
DOCGROUP(clasp);
CL_DEFUN llvm::SwitchInst* CreateSwitch(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::BasicBlock* Dest,
                                        unsigned NumCases, llvm::MDNode* BranchWeights, llvm::MDNode* Unpredictable) {
  return object->CreateSwitch(V, Dest, NumCases, BranchWeights, Unpredictable);
}

CL_LISPIFY_NAME(CreateBr);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateBr);
DOCGROUP(clasp);
CL_DEFUN llvm::BranchInst* CreateBr(llvmo::IRBuilderBase_O::ExternalType* object, llvm::BasicBlock* Dest) {
  return object->CreateBr(Dest);
}

CL_LISPIFY_NAME(CreateRetVoid);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateRetVoid);
DOCGROUP(clasp);
CL_DEFUN llvm::ReturnInst* CreateRetVoid(llvmo::IRBuilderBase_O::ExternalType* object) { return object->CreateRetVoid(); }

CL_LISPIFY_NAME(CreateRet);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateRet);
DOCGROUP(clasp);
CL_DEFUN llvm::ReturnInst* CreateRet(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V) { return object->CreateRet(V); }

CL_LISPIFY_NAME(CreateAdd);
CL_LAMBDA("irbuilder lhs rhs &optional (name \"\") has-nuw has-nsw");
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateAdd);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* CreateAdd(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, llvm::Value* RHS,
                                const llvm::Twine& Name, bool HasNUW, bool HasNSW) {
  return object->CreateAdd(LHS, RHS, Name, HasNUW, HasNSW);
}

CL_LISPIFY_NAME(CreateCondBr);
CL_LAMBDA (irbuilder cond true-branch false-branch &optional branch-weights unpred);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::BranchInst * (IRBuilderBase_O::ExternalType::*)(llvm::Value *, llvm::BasicBlock *,
// llvm::BasicBlock *, llvm::MDNode *, llvm::MDNode *))&IRBuilderBase_O::ExternalType::CreateCondBr);
DOCGROUP(clasp);
CL_DEFUN llvm::BranchInst* CreateCondBr(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* Cond, llvm::BasicBlock* True,
                                        llvm::BasicBlock* False, llvm::MDNode* BranchWeights, llvm::MDNode* Unpredictable) {
  return object->CreateCondBr(Cond, True, False, BranchWeights, Unpredictable);
}

// ------------------------------------------------------------
// ------------------------------------------------------------

CL_LISPIFY_NAME(CreateIndirectBr);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateIndirectBr);
CL_LISPIFY_NAME(CreateResume);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateResume);
CL_LISPIFY_NAME(CreateNSWAdd);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNSWAdd);
CL_LISPIFY_NAME(CreateNUWAdd);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNUWAdd);
CL_LISPIFY_NAME(CreateNSWSub);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNSWSub);
CL_LISPIFY_NAME(CreateNUWSub);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNUWSub);
CL_LISPIFY_NAME(CreateNSWMul);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNSWMul);
CL_LISPIFY_NAME(CreateNUWMul);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNUWMul);
CL_LISPIFY_NAME(CreateExactUDiv);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateExactUDiv);
CL_LISPIFY_NAME(CreateExactSDiv);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateExactSDiv);
CL_LISPIFY_NAME(CreateFRem);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFRem);
CL_LISPIFY_NAME(CreateNSWNeg);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNSWNeg);
#if LLVM_VERSION_MAJOR < 19
CL_LISPIFY_NAME(CreateNUWNeg);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateNUWNeg);
#endif
CL_LISPIFY_NAME(CreateStore);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::StoreInst * (llvm::IRBuilderBase::*)(llvm::Value * Val, llvm::Value* Ptr, bool isVolatile)) &
                        IRBuilderBase_O::ExternalType::CreateStore);
CL_LISPIFY_NAME(CreateConstGEP1-32);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value * Ptr, unsigned Idx0, const llvm::Twine& Name)) &
                        IRBuilderBase_O::ExternalType::CreateConstGEP1_32);
//  CL_LISPIFY_NAME(CreateConstInBoundsGEP1-32);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_32);
//  CL_LISPIFY_NAME(CreateConstGEP2-32);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstGEP2_32);
CL_LISPIFY_NAME(CreateConstGEP1-64);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, uint64_t, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateConstGEP1_64);
CL_LISPIFY_NAME(CreateConstInBoundsGEP1-64);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, uint64_t, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateConstInBoundsGEP1_64);
//  CL_LISPIFY_NAME(CreateConstGEP2-64);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateConstGEP2_64);
CL_LISPIFY_NAME(CreateConstInBoundsGEP2-64);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, uint64_t, uint64_t, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateConstInBoundsGEP2_64);
CL_LISPIFY_NAME(CreateStructGEP);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Type * Type, llvm::Value* Ptr,
                                                                                       unsigned Idx0, const llvm::Twine& Name)) &
                                         IRBuilderBase_O::ExternalType::CreateStructGEP);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value * Ptr, unsigned Idx, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateStructGEP);
CL_LISPIFY_NAME(CreateGlobalStringPtr);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateGlobalStringPtr);
CL_LISPIFY_NAME(CreateTrunc);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateTrunc);
CL_LISPIFY_NAME(CreateZExt);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateZExt);
CL_LISPIFY_NAME(CreateSExt);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSExt);
CL_LISPIFY_NAME(CreateFPToUI);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFPToUI);
CL_LISPIFY_NAME(CreateFPToSI);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFPToSI);
CL_LISPIFY_NAME(CreateUIToFP);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateUIToFP);
// llvm::IRBuilderBase::CreateUIToFP(llvm::Value*, llvm::Type*, llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateUIToFP(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                             const llvm::Twine& Name) {
  return object->CreateUIToFP(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateSIToFP);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSIToFP);
// llvm::IRBuilderBase::CreateSIToFP(llvm::Value*, llvm::Type*, llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateSIToFP(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                             const llvm::Twine& Name) {
  return object->CreateSIToFP(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateFPTrunc);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFPTrunc);
// llvm::IRBuilderBase::CreateFPTrunc(llvm::Value*, llvm::Type*, llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateFPTrunc(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                              const llvm::Twine& Name) {
  return object->CreateFPTrunc(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreateFPExt);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFPExt);
// llvm::IRBuilderBase::CreateFPExt(llvm::Value*, llvm::Type*, llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateFPExt(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* V, llvm::Type* DestTy,
                                            const llvm::Twine& Name) {
  return object->CreateFPExt(V, DestTy, Name);
}

CL_LISPIFY_NAME(CreatePtrToInt);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreatePtrToInt);
CL_LISPIFY_NAME(CreateIntToPtr);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateIntToPtr);
CL_LISPIFY_NAME(CreateBitCast);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateBitCast);
CL_LISPIFY_NAME(CreateSExtOrBitCast);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSExtOrBitCast);
CL_LISPIFY_NAME(CreateFPCast);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFPCast);
CL_LISPIFY_NAME(CreateICmpEQ);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpEQ);
CL_LISPIFY_NAME(CreateICmpNE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpNE);
CL_LISPIFY_NAME(CreateICmpUGT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpUGT);
CL_LISPIFY_NAME(CreateICmpUGE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpUGE);
CL_LISPIFY_NAME(CreateICmpULT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpULT);
CL_LISPIFY_NAME(CreateICmpULE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpULE);
CL_LISPIFY_NAME(CreateICmpSGT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpSGT);
CL_LISPIFY_NAME(CreateICmpSGE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpSGE);
CL_LISPIFY_NAME(CreateICmpSLT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpSLT);
CL_LISPIFY_NAME(CreateICmpSLE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateICmpSLE);
CL_LISPIFY_NAME(CreateFCmpOEQ);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpOEQ);
CL_LISPIFY_NAME(CreateFCmpOGT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpOGT);
CL_LISPIFY_NAME(CreateFCmpOGE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpOGE);
CL_LISPIFY_NAME(CreateFCmpOLT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpOLT);
CL_LISPIFY_NAME(CreateFCmpOLE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpOLE);
CL_LISPIFY_NAME(CreateFCmpONE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpONE);
CL_LISPIFY_NAME(CreateFCmpORD);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpORD);
CL_LISPIFY_NAME(CreateFCmpUNO);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpUNO);
CL_LISPIFY_NAME(CreateFCmpUEQ);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpUEQ);
CL_LISPIFY_NAME(CreateFCmpUGT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpUGT);
CL_LISPIFY_NAME(CreateFCmpUGE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpUGE);
CL_LISPIFY_NAME(CreateFCmpULT);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpULT);
CL_LISPIFY_NAME(CreateFCmpULE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpULE);
CL_LISPIFY_NAME(CreateFCmpUNE);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmpUNE);
CL_LISPIFY_NAME(CreateFCmp);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateFCmp);

// (llvm::FunctionType *FTy, Value *Callee, ArrayRef< Value * > Args, const Twine &Name="", MDNode *FPMathTag=nullptr)

CL_LISPIFY_NAME(CreateSelect);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateSelect);
CL_LISPIFY_NAME(CreateVAArg);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateVAArg);
CL_LISPIFY_NAME(CreateShuffleVector);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value * V1, llvm::Value* V2, llvm::Value* Mask,
                                                                      const llvm::Twine& Name)) &
                        IRBuilderBase_O::ExternalType::CreateShuffleVector);
CL_LISPIFY_NAME(CreateLandingPad);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateLandingPad);
CL_LISPIFY_NAME(CreateIsNull);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateIsNull);
CL_LISPIFY_NAME(CreateIsNotNull);
//  CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreateIsNotNull);
// llvm::IRBuilderBase::CreateIsNotNull(llvm::Value*, llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateIsNotNull(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* Arg,
                                                const llvm::Twine& Name) {
  return object->CreateIsNotNull(Arg, Name);
}

CL_LISPIFY_NAME(CreatePtrDiff);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, &IRBuilderBase_O::ExternalType::CreatePtrDiff);
CL_LISPIFY_NAME(CreateShl_value_value);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::Value *, const
// llvm::Twine &, bool, bool) )&IRBuilderBase_O::ExternalType::CreateShl);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateShl_value_value(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* value1,
                                                      llvm::Value* value2, const llvm::Twine& label, bool HasNUW = false,
                                                      bool HasNSW = false) {
  return object->CreateShl(value1, value2, label, HasNUW, HasNSW);
}
CL_LISPIFY_NAME(CreateShl_value_apint);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, llvm::APInt const&,
                                                                                       const llvm::Twine&, bool, bool)) &
                                         IRBuilderBase_O::ExternalType::CreateShl);
CL_LISPIFY_NAME(CreateShl_value_uint64);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine
// &, bool, bool) )&IRBuilderBase_O::ExternalType::CreateShl);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateShl_value_uint64(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, uint64_t RHS,
                                                       const llvm::Twine& name, bool HasNUW, bool HasNSW) {
  return object->CreateShl(LHS, RHS, name, HasNUW, HasNSW);
}
CL_LISPIFY_NAME(CreateLShr_value_value);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::Value *, const
// llvm::Twine &, bool) )&IRBuilderBase_O::ExternalType::CreateLShr);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateLShr_value_value(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* value1,
                                                       llvm::Value* value2, const llvm::Twine& label, bool isExact) {
  return object->CreateLShr(value1, value2, label, isExact);
}

CL_LISPIFY_NAME(CreateLShr_value_apint);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, llvm::APInt const&, const llvm::Twine&, bool)) &
                        IRBuilderBase_O::ExternalType::CreateLShr);
CL_LISPIFY_NAME(CreateLShr_value_uint64);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine
// &, bool) )&IRBuilderBase_O::ExternalType::CreateLShr); llvm::IRBuilderBase::CreateLShr(llvm::Value*, unsigned long, llvm::Twine
// const& , bool)
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateLShr_value_uint64(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS,
                                                        unsigned long RHS, const llvm::Twine& Name, bool isExact) {
  return object->CreateLShr(LHS, RHS, Name, isExact);
}

CL_LISPIFY_NAME(CreateAShr_value_value);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::Value *, const
// llvm::Twine &, bool) )&IRBuilderBase_O::ExternalType::CreateAShr);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateAShr_value_value(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* value1,
                                                       llvm::Value* value2, const llvm::Twine& label, bool isExact) {
  return object->CreateAShr(value1, value2, label, isExact);
}

CL_LISPIFY_NAME(CreateAShr_value_apint);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const
// llvm::Twine &, bool) )&IRBuilderBase_O::ExternalType::CreateAShr); llvm::IRBuilderBase::CreateAShr(llvm::Value*, llvm::APInt
// const& , llvm::Twine const& , bool)
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateAShr_value_apint(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS,
                                                       const llvm::APInt& RHS, const llvm::Twine& Name, bool isExact) {
  return object->CreateAShr(LHS, RHS, Name, isExact);
}

CL_LISPIFY_NAME(CreateAShr_value_uint64);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, uint64_t, const llvm::Twine&, bool)) &
                        IRBuilderBase_O::ExternalType::CreateAShr);
CL_LISPIFY_NAME(CreateAnd_value_value);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::Value *, const
// llvm::Twine &) )&IRBuilderBase_O::ExternalType::CreateAnd);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateAnd_value_value(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* value1,
                                                      llvm::Value* value2, const llvm::Twine& label) {
  return object->CreateAnd(value1, value2, label);
}

CL_LISPIFY_NAME(CreateAnd_value_apint);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const
// llvm::Twine &) )&IRBuilderBase_O::ExternalType::CreateAnd);
//  llvm::IRBuilderBase::CreateAnd(llvm::Value*, llvm::APInt const& , llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateAnd_value_apint(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS,
                                                      const llvm::APInt& RHS, const llvm::Twine& Name) {
  return object->CreateAnd(LHS, RHS, Name);
}

CL_LISPIFY_NAME(CreateAnd_value_uint64);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, uint64_t, const llvm::Twine
// &) )&IRBuilderBase_O::ExternalType::CreateAnd);
//  llvm::IRBuilderBase::CreateAnd(llvm::Value*, unsigned long, llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateAnd_value_uint64(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS, uint64_t RHS,
                                                       const llvm::Twine& Name) {
  return object->CreateAnd(LHS, RHS, Name);
}

CL_LISPIFY_NAME(CreateOr_value_value);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::Value *, const
// llvm::Twine &) )&IRBuilderBase_O::ExternalType::CreateOr);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateOr_value_value(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* value1,
                                                     llvm::Value* value2, const llvm::Twine& label) {
  return object->CreateOr(value1, value2, label);
}

CL_LISPIFY_NAME(CreateOr_value_apint);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::APInt const &, const
// llvm::Twine &) )&IRBuilderBase_O::ExternalType::CreateOr);
//  llvm::IRBuilderBase::CreateOr(llvm::Value*, llvm::APInt const& , llvm::Twine const& )
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateOr_value_apint(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* LHS,
                                                     const llvm::APInt& RHS, const llvm::Twine& Name) {
  return object->CreateOr(LHS, RHS, Name);
}

CL_LISPIFY_NAME(CreateOr_value_uint64);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, uint64_t, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateOr);
CL_LISPIFY_NAME(CreateXor_value_value);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Value *, llvm::Value *, const
// llvm::Twine &) )&IRBuilderBase_O::ExternalType::CreateXor);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateXor_value_value(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Value* value1,
                                                      llvm::Value* value2, const llvm::Twine& label) {
  return object->CreateXor(value1, value2, label);
}

CL_LISPIFY_NAME(CreateXor_value_apint);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, llvm::APInt const&, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateXor);
CL_LISPIFY_NAME(CreateXor_value_uint64);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Value*, uint64_t, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateXor);
CL_LISPIFY_NAME(CreateLoad_type_value_twine);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::LoadInst * (IRBuilderBase_O::ExternalType::*)(llvm::Type * Ty, llvm::Value*, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateLoad);
CL_LISPIFY_NAME(CreateLoad_type_value_bool_twine);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::LoadInst * (IRBuilderBase_O::ExternalType::*)(llvm::Type * Ty, llvm::Value*, bool, const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateLoad);
CL_DOCSTRING("The type must be equiv to firstValue->getType()->getScalarType()->getPointerElementType()");
CL_LISPIFY_NAME(CreateGEP0);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O, (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Type*, llvm::Value*, llvm::Value*,
                                                                                       const llvm::Twine&)) &
                                         IRBuilderBase_O::ExternalType::CreateGEP);
CL_DOCSTRING("The type must be equiv to firstValue->getType()->getScalarType()->getPointerElementType()");
CL_LISPIFY_NAME(CreateGEPArray);
CL_EXTERN_DEFMETHOD(IRBuilderBase_O,
                    (llvm::Value * (IRBuilderBase_O::ExternalType::*)(llvm::Type*, llvm::Value*, llvm::ArrayRef<llvm::Value*>,
                                                                      const llvm::Twine&)) &
                        IRBuilderBase_O::ExternalType::CreateGEP);

CL_LISPIFY_NAME(CreateInBoundsGEPType);
// CL_EXTERN_DEFMETHOD(IRBuilderBase_O,(llvm::Value *(IRBuilderBase_O::ExternalType::*) (llvm::Type *, llvm::Value *,
// llvm::ArrayRef<llvm::Value *>, const llvm::Twine &) )&IRBuilderBase_O::ExternalType::CreateInBoundsGEP);
DOCGROUP(clasp);
CL_DEFUN llvm::Value* llvm_sys__CreateInBoundsGEPType(llvmo::IRBuilderBase_O::ExternalType* object, llvm::Type* type,
                                                      llvm::Value* value, llvm::ArrayRef<llvm::Value*> array,
                                                      const llvm::Twine& label) {
  return object->CreateInBoundsGEP(type, value, array, label);
}

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(hasStructRetAttr);
CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasStructRetAttr);
CL_LISPIFY_NAME(hasNoAliasAttr);
CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasNoAliasAttr);
CL_LISPIFY_NAME(hasNestAttr);
CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasNestAttr);
CL_LISPIFY_NAME(hasByValAttr);
CL_EXTERN_DEFMETHOD(Argument_O, &llvm::Argument::hasByValAttr);
;

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(mdnode-get);
DOCGROUP(clasp);
CL_DEFUN MDNode_sp MDNode_O::get(LLVMContext_sp context, core::List_sp values) {
  vector<llvm::Metadata*> valvec;
  for (auto cur : values) {
    llvm::Metadata* val = gc::As<Metadata_sp>(oCar(cur))->wrappedPtr();
    valvec.push_back(val);
  }
  llvm::MDNode* mdnode = llvm::MDNode::get(*context->wrappedPtr(), valvec);
  MDNode_sp omd = core::RP_Create_wrapped<llvmo::MDNode_O, llvm::MDNode*>(mdnode);
  return omd;
}

SYMBOL_EXPORT_SC_(LlvmoPkg, mdnodeGet);

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(mdstring-get);
DOCGROUP(clasp);
CL_DEFUN MDString_sp MDString_O::get(LLVMContext_sp context, core::String_sp str) {
  llvm::MDString* mdstr = llvm::MDString::get(*context->wrappedPtr(), str->get_std_string());
  MDString_sp omd = core::RP_Create_wrapped<llvmo::MDString_O, llvm::MDString*>(mdstr);
  return omd;
}

SYMBOL_EXPORT_SC_(LlvmoPkg, mdnodeGet);

;

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(value-as-metadata-get);
DOCGROUP(clasp);
CL_DEFUN ValueAsMetadata_sp ValueAsMetadata_O::get(Value_sp val) {
  llvm::ValueAsMetadata* mdstr = llvm::ValueAsMetadata::get(val->wrappedPtr());
  ValueAsMetadata_sp omd = core::RP_Create_wrapped<llvmo::ValueAsMetadata_O, llvm::ValueAsMetadata*>(mdstr);
  return omd;
}

SYMBOL_EXPORT_SC_(LlvmoPkg, ValueAsMetadataGet);

;

}; // namespace llvmo

namespace llvmo {

DOCGROUP(clasp);
CL_DEFUN Function_sp llvm_sys__FunctionCreate(llvm::FunctionType* ty, llvm::GlobalValue::LinkageTypes linkage, core::String_sp nsp,
                                              llvm::Module* m) {
  //        printf("%s:%d FunctionCreate %s with linkage %d\n", __FILE__, __LINE__, nsp->get().c_str(), linkage);
  llvm::Function* func = llvm::Function::Create(ty, linkage, nsp->get_std_string(), m);
  Function_sp funcsp = gc::As<Function_sp>(translate::to_object<llvm::Function*>::convert(func));
  return funcsp;
};

CL_LISPIFY_NAME(getParent);
CL_EXTERN_DEFMETHOD(Function_O, (llvm::Module * (llvm::Function::*)()) & llvm::Function::getParent);
CL_LISPIFY_NAME("setDoesNotThrow");
CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::setDoesNotThrow);
CL_LISPIFY_NAME("isVarArg");
CL_EXTERN_DEFMETHOD(Function_O, &llvm::Function::isVarArg);
CL_LISPIFY_NAME("addFnAttr");
CL_EXTERN_DEFMETHOD(Function_O, (void(llvm::Function::*)(Attribute::AttrKind Kind)) & llvm::Function::addFnAttr);
CL_LISPIFY_NAME("removeFnAttr");
CL_EXTERN_DEFMETHOD(Function_O, (void(llvm::Function::*)(Attribute::AttrKind Kind)) & llvm::Function::removeFnAttr);
CL_LISPIFY_NAME("hasFnAttribute");
CL_EXTERN_DEFMETHOD(Function_O, (bool(llvm::Function::*)(Attribute::AttrKind Kind) const) & llvm::Function::hasFnAttribute);
#if 0
CL_LISPIFY_NAME("addAttribute");
CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(unsigned i, typename llvm::Attribute::AttrKind Attr))&llvm::Function::addAttribute);
#endif
CL_LISPIFY_NAME("addParamAttr");
CL_EXTERN_DEFMETHOD(Function_O,
                    (void(llvm::Function::*)(unsigned i, typename llvm::Attribute::AttrKind Attr)) & llvm::Function::addParamAttr);

CL_LISPIFY_NAME("setSubprogram");
CL_EXTERN_DEFMETHOD(Function_O, (void(llvm::Function::*)(llvm::DISubprogram*)) & llvm::Function::setSubprogram);

CL_LISPIFY_NAME("addReturnAttr");
CL_DEFMETHOD void Function_O::addReturnAttr(typename llvm::Attribute::AttrKind Attr) {}

CL_DEFMETHOD LLVMContext_sp Function_O::getContext() const {
  return gc::As<LLVMContext_sp>(translate::to_object<llvm::LLVMContext&>::convert(this->wrappedPtr()->getContext()));
}

CL_LISPIFY_NAME("getArgumentList");
CL_DEFMETHOD core::List_sp Function_O::getArgumentList() {
  ql::list l;
  llvm::Function* func = this->wrappedPtr();
  for (auto arg = func->arg_begin(); arg != func->arg_end(); ++arg) {
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
#if LLVM_VERSION_MAJOR < 16
  this->wrappedPtr()->getBasicBlockList().push_back(basicBlock->wrappedPtr());
#else
  this->wrappedPtr()->insert(this->wrappedPtr()->end(), basicBlock->wrappedPtr());
#endif
}

CL_LISPIFY_NAME("getEntryBlock");
CL_DEFMETHOD BasicBlock_sp Function_O::getEntryBlock() const {
  return gc::As<BasicBlock_sp>(translate::to_object<llvm::BasicBlock*>::convert(&this->wrappedPtr()->getEntryBlock()));
}

CL_LISPIFY_NAME("basic-blocks");
CL_DEFMETHOD core::List_sp Function_O::basic_blocks() const {
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
CL_LISPIFY_NAME(getCallingConv);
CL_EXTERN_DEFMETHOD(Function_O, (llvmo::ClaspCallingConv(llvm::Function::*)())&Function_O::ExternalType::getCallingConv);
CL_LISPIFY_NAME(setCallingConv);
CL_EXTERN_DEFMETHOD(Function_O, (void(llvm::Function::*)(llvmo::ClaspCallingConv)) & Function_O::ExternalType::setCallingConv);
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
CL_EXTERN_DEFMETHOD(Function_O, (void(llvm::Function::*)(llvm::Attribute::AttrKind)) & llvm::Function::addFnAttr);
;
// CL_LISPIFY_NAME(addFnAttr1String);
// CL_EXTERN_DEFMETHOD(Function_O, (void (llvm::Function::*)(llvm::StringRef)) & llvm::Function::addFnAttr);;
CL_LISPIFY_NAME(addFnAttr2String);
CL_EXTERN_DEFMETHOD(Function_O, (void(llvm::Function::*)(llvm::StringRef, llvm::StringRef)) & llvm::Function::addFnAttr);
;
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

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(getParent);
CL_EXTERN_DEFMETHOD(BasicBlock_O, (llvm::Function * (llvm::BasicBlock::*)()) & llvm::BasicBlock::getParent);

CL_LISPIFY_NAME(getTerminator);
CL_EXTERN_DEFMETHOD(BasicBlock_O, (llvm::Instruction * (llvm::BasicBlock::*)()) & llvm::BasicBlock::getTerminator);

CL_LAMBDA("context &optional (name \"\") parent basic-block");
CL_LISPIFY_NAME(basic-block-create);
// CL_EXTERN_DEFUN((llvm::BasicBlock * (*)(llvm::LLVMContext &Context, const llvm::Twine &Name, llvm::Function *Parent,
// llvm::BasicBlock *InsertBefore)) &llvm::BasicBlock::Create );
//  llvm::BasicBlock::Create(llvm::LLVMContext& , llvm::Twine const& , llvm::Function*, llvm::BasicBlock*)
DOCGROUP(clasp);
CL_DEFUN llvm::BasicBlock* llvm_sys__BasicBlockCreate(llvm::LLVMContext& Context, const llvm::Twine& Name, llvm::Function* Parent,
                                                      llvm::BasicBlock* InsertBefore) {
  return llvm::BasicBlock::Create(Context, Name, Parent, InsertBefore);
}

;

CL_LISPIFY_NAME("BasicBlockEmpty");
CL_DEFMETHOD bool BasicBlock_O::empty() { return this->wrappedPtr()->empty(); }

CL_LISPIFY_NAME("BasicBlock-size");
CL_DEFMETHOD size_t BasicBlock_O::size() { return this->wrappedPtr()->size(); }

CL_LISPIFY_NAME("instructions");
CL_DEFMETHOD core::List_sp BasicBlock_O::instructions() const {
  ql::list result;
  llvm::BasicBlock* bb = const_cast<BasicBlock_O*>(this)->wrappedPtr();
  for (auto ic = bb->begin(); ic != bb->end(); ++ic) {
    llvm::Instruction& II = *ic;
    result << translate::to_object<llvm::Instruction*>::convert(&II);
  }
  return result.cons();
}

CL_LISPIFY_NAME("number-of-instructions");
CL_DEFMETHOD size_t BasicBlock_O::number_of_instructions() const {
  llvm::BasicBlock* bb = const_cast<BasicBlock_O*>(this)->wrappedPtr();
  return std::distance(bb->begin(), bb->end());
}

CL_LISPIFY_NAME("BasicBlockBack");
CL_DEFMETHOD Instruction_sp BasicBlock_O::back() {
  llvm::BasicBlock* bbp = this->wrappedPtr();
  llvm::Instruction& inst = bbp->back();
  Instruction_sp instruction = core::RP_Create_wrapped<Instruction_O, llvm::Instruction*>(&inst);
  return instruction;
}

}; // namespace llvmo

namespace llvmo {

CL_LISPIFY_NAME(get_contained_type);
CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::getContainedType);

CL_LISPIFY_NAME(get_num_contained_types);
CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::getNumContainedTypes);

CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::isPointerTy);

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

void Type_O::__write__(core::T_sp stream) const { core::clasp_write_string(this->__repr__(), stream); }

CL_LAMBDA((self llvm-sys::type) &optional (addressSpace 0));
CL_DOCSTRING(R"dx(Return a PointerType to the llvm Type)dx");
CL_LISPIFY_NAME("type-get-pointer-to");
CL_DEFMETHOD PointerType_sp Type_O::getPointerTo(int addressSpace) {
  llvm::PointerType* ptrType = this->wrappedPtr()->getPointerTo();
  return translate::to_object<llvm::PointerType*>::convert(ptrType);
}

CL_LISPIFY_NAME("getArrayNumElements");
CL_DEFMETHOD core::Integer_sp Type_O::getArrayNumElements() const {
  gc::Fixnum v64 = this->wrappedPtr()->getArrayNumElements();
  core::Integer_sp ival = core::Integer_O::create(v64);
  return ival;
}
#if 0
CL_EXTERN_DEFMETHOD(Type_O,&llvm::Type::getSequentialElementType);

CL_LISPIFY_NAME(getSequentialElementType);
CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::getSequentialElementType);;
#endif
CL_LISPIFY_NAME(getScalarType);
CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::getScalarType);

CL_LISPIFY_NAME("type-get-void-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getVoidTy);
CL_LISPIFY_NAME("type-get-half-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getHalfTy);
CL_LISPIFY_NAME("type-get-bfloat-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getBFloatTy);
CL_LISPIFY_NAME("type-get-float-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getFloatTy);
CL_LISPIFY_NAME("type-get-double-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getDoubleTy);
CL_LISPIFY_NAME("type-get-x86-fp80-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getX86_FP80Ty);
CL_LISPIFY_NAME("type-get-fp128-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getFP128Ty);

CL_LISPIFY_NAME("type-get-metadata-ty");
CL_EXTERN_DEFUN((llvm::Type * (*)(llvm::LLVMContext & C)) & llvm::Type::getMetadataTy);

CL_LISPIFY_NAME("type-get-int-nty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C, unsigned N)) & llvm::Type::getIntNTy);
CL_LISPIFY_NAME("type-get-int1-ty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C)) & llvm::Type::getInt1Ty);
CL_LISPIFY_NAME("type-get-int8-ty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C)) & llvm::Type::getInt8Ty);
CL_LISPIFY_NAME("type-get-int16-ty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C)) & llvm::Type::getInt16Ty);
CL_LISPIFY_NAME("type-get-int32-ty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C)) & llvm::Type::getInt32Ty);
CL_LISPIFY_NAME("type-get-int64-ty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C)) & llvm::Type::getInt64Ty);
CL_LISPIFY_NAME("type-get-int128-ty");
CL_EXTERN_DEFUN((llvm::IntegerType * (*)(llvm::LLVMContext & C)) & llvm::Type::getInt128Ty);

#if LLVM_VERSION_MAJOR < 18
CL_LISPIFY_NAME("type-get-float-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getFloatPtrTy);
CL_LISPIFY_NAME("type-get-double-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getDoublePtrTy);

CL_LISPIFY_NAME("type-get-int-nptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getIntNPtrTy);
CL_LISPIFY_NAME("type-get-int1-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getInt1PtrTy);
CL_LISPIFY_NAME("type-get-int8-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getInt8PtrTy);
CL_LISPIFY_NAME("type-get-int16-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getInt16PtrTy);
CL_LISPIFY_NAME("type-get-int32-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getInt32PtrTy);
CL_LISPIFY_NAME("type-get-int64-ptr-ty");
CL_EXTERN_DEFUN((llvm::PointerType * (*)(llvm::LLVMContext & C, unsigned AS)) & llvm::Type::getInt64PtrTy);
;
#endif

}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(result &optional params is-var-arg);
CL_LISPIFY_NAME(function-type-get);
DOCGROUP(clasp);
CL_DEFUN core::T_sp FunctionType_O::get(llvm::Type* result_type, core::T_sp params, core::T_sp is_var_arg) {
  bool iva = is_var_arg.isTrue();
  llvm::FunctionType* result = NULL;
  if (params.nilp()) {
    result = llvm::FunctionType::get(result_type, iva);
  } else {
    vector<llvm::Type*> vparams;
    convert_sequence_types_to_vector(params, vparams);
    llvm::ArrayRef<llvm::Type*> p(vparams);
    result = llvm::FunctionType::get(result_type, p, iva);
  }
  return translate::to_object<llvm::FunctionType*>::convert(result);
};

// I can't get the following to work yet
// CL_EXTERN_DEFMETHOD(FunctionType_O, &llvm::FunctionType::getReturnType);

DOCGROUP(clasp);
CL_DEFUN core::T_sp llvm_sys__function_type_param_types(llvmo::FunctionType_sp ftype) {
  ql::list paramtypes;
  llvm::FunctionType* fty = ftype->wrapped();
  for (auto ptype = fty->param_begin(); ptype != fty->param_end(); ++ptype) {
    paramtypes << translate::to_object<llvm::Type*>::convert(*ptype);
  }
  return paramtypes.cons();
}

DOCGROUP(clasp);
CL_DEFUN bool llvm_sys__function_type_vararg_p(llvmo::FunctionType_sp ftype) { return ftype->wrapped()->isVarArg(); }

}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(context &key elements name is-packed);
CL_LISPIFY_NAME(struct-type-create);
DOCGROUP(clasp);
CL_DEFUN StructType_sp StructType_O::make(LLVMContext_sp context, core::T_sp elements, llvm::StringRef srname,
                                          core::T_sp isPacked) {
  llvm::StructType* result = NULL;
  if (elements.notnilp()) {
    vector<llvm::Type*> velements;
    convert_sequence_types_to_vector(elements, velements);
    llvm::ArrayRef<llvm::Type*> p(velements);
    result = llvm::StructType::create(*(context->wrappedPtr()), p, srname, isPacked.isTrue());
  } else {
    result = llvm::StructType::create(*(context->wrappedPtr()), srname);
  }
  return translate::to_object<llvm::StructType*>::convert(result);
}

CL_LISPIFY_NAME(struct-type-get);
DOCGROUP(clasp);
CL_DEFUN StructType_sp StructType_O::get(LLVMContext_sp context, core::T_sp elements, bool isPacked) {
  llvm::StructType* result = NULL;
  if (elements.notnilp()) {
    vector<llvm::Type*> velements;
    convert_sequence_types_to_vector(elements, velements);
    llvm::ArrayRef<llvm::Type*> p(velements);
    result = llvm::StructType::get(*(context->wrappedPtr()), p, isPacked);
  } else {
    result = llvm::StructType::get(*(context->wrappedPtr()), isPacked);
  }
  return translate::to_object<llvm::StructType*>::convert(result);
}

CL_LISPIFY_NAME("setBody");
CL_DEFMETHOD void StructType_O::setBody(core::T_sp elements, core::T_sp isPacked) {
  llvm::StructType* st = this->wrapped();
  if (elements.notnilp()) {
    vector<llvm::Type*> velements;
    convert_sequence_types_to_vector(elements, velements);
    llvm::ArrayRef<llvm::Type*> p(velements);
    st->setBody(p, isPacked.isTrue());
  }
}

// Takes a Type rather than a StructType because we frequently deal with
// un-downcasted Types in the compiler (e.g., from get-type)
CL_LISPIFY_NAME("indexValid");
CL_DEFUN bool indexValid(Type_sp type, unsigned idx) {
  llvm::StructType* st = dyn_cast<llvm::StructType>(type->wrappedPtr());
  if (st)
    return st->indexValid(idx);
  else
    SIMPLE_ERROR("Could not cast {} to struct type", _rep_(type));
}

;

}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(element-type num-elements);
CL_LISPIFY_NAME(array-type-get);
DOCGROUP(clasp);
CL_DEFUN ArrayType_sp ArrayType_O::get(Type_sp elementType, uint64_t numElements) {
  ArrayType_sp at = ArrayType_O::create();
  llvm::ArrayType* llvm_at = llvm::ArrayType::get(elementType->wrappedPtr(), numElements);
  at->set_wrapped(llvm_at);
  return at;
}

CL_DEFUN size_t llvm_sys__getNumElements(Type_sp ty) {
  llvm::ArrayType* array = dyn_cast<llvm::ArrayType>(ty->wrappedPtr());
  if (!array) {
    SIMPLE_ERROR("Could not cast {} to array", _rep_(ty));
  }
  return array->getNumElements();
}

}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(element-type &optional (address-space 0));
CL_LISPIFY_NAME(pointer-type-get);
DOCGROUP(clasp);
CL_DEFUN PointerType_sp PointerType_O::get(Type_sp elementType, uint addressSpace) {
  PointerType_sp at = PointerType_O::create();
  llvm::PointerType* llvm_at = llvm::PointerType::get(elementType->wrappedPtr(), addressSpace);
  at->set_wrapped(llvm_at);
  return at;
}

;

}; // namespace llvmo

namespace llvmo {

CL_LAMBDA(element-type nelems scalablep);
CL_LISPIFY_NAME(vector-type-get);
DOCGROUP(clasp);
CL_DEFUN VectorType_sp VectorType_O::get(Type_sp elementType, unsigned nelems, bool scalablep) {
  VectorType_sp vt = VectorType_O::create();
  llvm::VectorType* llvm_vt = llvm::VectorType::get(elementType->wrappedPtr(), nelems, scalablep);
  vt->set_wrapped(llvm_vt);
  return vt;
}
}; // namespace llvmo

namespace llvmo {

DOCGROUP(clasp);
CL_LAMBDA(time);
CL_DEFUN void llvm_sys__accumulate_llvm_usage_seconds(double time) {
  core::DoubleFloat_sp df = core::DoubleFloat_O::create(time);
  _sym_STARmostRecentLlvmFinalizationTimeSTAR->setf_symbolValue(df);
  double accTime = clasp_to_double(_sym_STARaccumulatedLlvmFinalizationTimeSTAR->symbolValue());
  accTime += time;
  _sym_STARaccumulatedLlvmFinalizationTimeSTAR->setf_symbolValue(core::DoubleFloat_O::create(accTime));
  int num = unbox_fixnum(gc::As<core::Fixnum_sp>(_sym_STARnumberOfLlvmFinalizationsSTAR->symbolValue()));
  ++num;
  _sym_STARnumberOfLlvmFinalizationsSTAR->setf_symbolValue(core::make_fixnum(num));
}

void finalizeEngineAndTime(llvm::ExecutionEngine* engine) {
  core::LightTimer timer;
  timer.start();
  engine->finalizeObject();
  timer.stop();
  double thisTime = timer.getAccumulatedTime();
  llvm_sys__accumulate_llvm_usage_seconds(thisTime);
}

DOCGROUP(clasp);
CL_DEFUN void finalizeEngineAndRegisterWithGcAndRunMainFunctions(ExecutionEngine_sp oengine, core::T_sp startup_name) {
  // Stuff to support MCJIT
  llvm::ExecutionEngine* engine = oengine->wrappedPtr();
#ifdef DEBUG_STARTUP
  printf("%s:%d Entered %s\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  finalizeEngineAndTime(engine);
  if (gc::IsA<core::String_sp>(startup_name)) {
    core::String_sp str = gc::As_unsafe<core::String_sp>(startup_name);
    std::string sstr = str->get_std_string();
    llvm::StringRef strref = sstr;
    void* fn = engine->getPointerToNamedFunction(strref);
    typedef void (*fptr)();
    fptr ffn = (fptr)fn;
    printf("%s:%d About to run function %s at %p\n", __FILE__, __LINE__, sstr.c_str(), fn);
    ffn();
  } else if (startup_name.nilp()) {
    engine->runStaticConstructorsDestructors(false);
  } else {
    SIMPLE_ERROR("Only NIL or a function name are allowed as the startup_name - you provided {}", _rep_(startup_name));
  }
  if (core::startup_functions_are_waiting()) {
    core::startup_functions_invoke(NULL);
  } else {
    SIMPLE_ERROR("There were no startup functions to invoke\n");
  }
#ifdef DEBUG_STARTUP
  printf("%s:%d Leaving %s\n", __FILE__, __LINE__, __FUNCTION__);
#endif
}

/*! Return (values target nil) if successful or (values nil error-message) if not */
DOCGROUP(clasp);
CL_DEFUN core::T_mv TargetRegistryLookupTarget(const std::string& ArchName, Triple_sp triple) {
  string message;
  llvm::Target* target = const_cast<llvm::Target*>(llvm::TargetRegistry::lookupTarget(ArchName, *triple->wrappedPtr(), message));
  if (target == NULL) {
    return Values(nil<core::T_O>(), core::SimpleBaseString_O::make(message));
  }
  Target_sp targeto = core::RP_Create_wrapped<Target_O, llvm::Target*>(target);
  return Values(targeto, nil<core::T_O>());
}

/*! Return (values target nil) if successful or (values nil error-message) if not */
CL_LISPIFY_NAME(TargetRegistryLookupTarget.string);
DOCGROUP(clasp);
CL_DEFUN core::T_mv TargetRegistryLookupTarget_string(const std::string& Triple) {
  string message;
  llvm::Target* target = const_cast<llvm::Target*>(llvm::TargetRegistry::lookupTarget(Triple, message));
  if (target == NULL) {
    return Values(nil<core::T_O>(), core::SimpleBaseString_O::make(message));
  }
  Target_sp targeto = core::RP_Create_wrapped<Target_O, llvm::Target*>(target);
  return Values(targeto, nil<core::T_O>());
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
CL_BEGIN_ENUM(llvm::GlobalValue::LinkageTypes, _sym_STARglobal_value_linkage_typesSTAR, "llvm::GlobalValue::LinkageTypes");
CL_VALUE_ENUM(_sym_ExternalLinkage, llvm::GlobalValue::ExternalLinkage);
CL_VALUE_ENUM(_sym_AvailableExternallyLinkage, llvm::GlobalValue::AvailableExternallyLinkage);
CL_VALUE_ENUM(_sym_LinkOnceAnyLinkage, llvm::GlobalValue::LinkOnceAnyLinkage);
CL_VALUE_ENUM(_sym_LinkOnceODRLinkage, llvm::GlobalValue::LinkOnceODRLinkage);
CL_VALUE_ENUM(_sym_WeakAnyLinkage, llvm::GlobalValue::WeakAnyLinkage);
CL_VALUE_ENUM(_sym_WeakODRLinkage, llvm::GlobalValue::WeakODRLinkage);
CL_VALUE_ENUM(_sym_AppendingLinkage, llvm::GlobalValue::AppendingLinkage);
CL_VALUE_ENUM(_sym_InternalLinkage, llvm::GlobalValue::InternalLinkage);
CL_VALUE_ENUM(_sym_PrivateLinkage, llvm::GlobalValue::PrivateLinkage);
CL_VALUE_ENUM(_sym_ExternalWeakLinkage, llvm::GlobalValue::ExternalWeakLinkage);
CL_VALUE_ENUM(_sym_CommonLinkage, llvm::GlobalValue::CommonLinkage);
CL_END_ENUM(_sym_STARglobal_value_linkage_typesSTAR);

SYMBOL_SC_(LlvmoPkg, STARglobal_ThreadLocalModesSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, NotThreadLocal);
SYMBOL_EXPORT_SC_(LlvmoPkg, GeneralDynamicTLSModel);
SYMBOL_EXPORT_SC_(LlvmoPkg, LocalDynamicTLSModel);
SYMBOL_EXPORT_SC_(LlvmoPkg, InitialExecTLSModel);
SYMBOL_EXPORT_SC_(LlvmoPkg, LocalExecTLSModel);
CL_BEGIN_ENUM(llvm::GlobalValue::ThreadLocalMode, _sym_STARglobal_ThreadLocalModesSTAR, "llvm::GlobalValue::ThreadLocalMode");
CL_VALUE_ENUM(_sym_NotThreadLocal, llvm::GlobalValue::NotThreadLocal);
CL_VALUE_ENUM(_sym_GeneralDynamicTLSModel, llvm::GlobalValue::GeneralDynamicTLSModel);
CL_VALUE_ENUM(_sym_LocalDynamicTLSModel, llvm::GlobalValue::LocalDynamicTLSModel);
CL_VALUE_ENUM(_sym_InitialExecTLSModel, llvm::GlobalValue::InitialExecTLSModel);
CL_VALUE_ENUM(_sym_LocalExecTLSModel, llvm::GlobalValue::LocalExecTLSModel);
;
CL_END_ENUM(_sym_STARglobal_ThreadLocalModesSTAR);

SYMBOL_EXPORT_SC_(LlvmoPkg, STARGlobalValueUnnamedAddrSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, None);
SYMBOL_EXPORT_SC_(LlvmoPkg, Local);
SYMBOL_EXPORT_SC_(LlvmoPkg, Global);
CL_BEGIN_ENUM(llvm::GlobalValue::UnnamedAddr, _sym_STARGlobalValueUnnamedAddrSTAR, "llvm::GlobalValue::UnnamedAddr");
CL_VALUE_ENUM(_sym_None, llvm::GlobalValue::UnnamedAddr::None);
CL_VALUE_ENUM(_sym_Local, llvm::GlobalValue::UnnamedAddr::Local);
CL_VALUE_ENUM(_sym_Global, llvm::GlobalValue::UnnamedAddr::Global);
CL_END_ENUM(_sym_STARGlobalValueUnnamedAddrSTAR);

CL_LISPIFY_NAME(InitializeNativeTarget);
CL_EXTERN_DEFUN(&llvm::InitializeNativeTarget);

SYMBOL_EXPORT_SC_(LlvmoPkg, STARatomic_orderingSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, NotAtomic);
SYMBOL_EXPORT_SC_(LlvmoPkg, Unordered);
SYMBOL_EXPORT_SC_(LlvmoPkg, Monotonic);
SYMBOL_EXPORT_SC_(LlvmoPkg, Acquire);
SYMBOL_EXPORT_SC_(LlvmoPkg, Release);
SYMBOL_EXPORT_SC_(LlvmoPkg, AcquireRelease);
SYMBOL_EXPORT_SC_(LlvmoPkg, SequentiallyConsistent);
CL_BEGIN_ENUM(llvm::AtomicOrdering, _sym_STARatomic_orderingSTAR, "llvm::AtomicOrdering");
CL_VALUE_ENUM(_sym_NotAtomic, llvm::AtomicOrdering::NotAtomic);
CL_VALUE_ENUM(_sym_Unordered, llvm::AtomicOrdering::Unordered);
CL_VALUE_ENUM(_sym_Monotonic, llvm::AtomicOrdering::Monotonic);
CL_VALUE_ENUM(_sym_Acquire, llvm::AtomicOrdering::Acquire);
CL_VALUE_ENUM(_sym_Release, llvm::AtomicOrdering::Release);
CL_VALUE_ENUM(_sym_AcquireRelease, llvm::AtomicOrdering::AcquireRelease);
CL_VALUE_ENUM(_sym_SequentiallyConsistent, llvm::AtomicOrdering::SequentiallyConsistent);
;
CL_END_ENUM(_sym_STARatomic_orderingSTAR);

SYMBOL_EXPORT_SC_(LlvmoPkg, STARsync_scopeSTAR);
SYMBOL_EXPORT_SC_(LlvmoPkg, SingleThread);
SYMBOL_EXPORT_SC_(LlvmoPkg, System);
CL_BEGIN_ENUM(llvm::SyncScope::ID, _sym_STARsync_scopeSTAR, "llvm::SyncScope::ID");
CL_VALUE_ENUM(_sym_SingleThread, llvm::SyncScope::SingleThread);
CL_VALUE_ENUM(_sym_System, llvm::SyncScope::System);
CL_END_ENUM(_sym_STARsync_scopeSTAR);

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
CL_BEGIN_ENUM(llvm::AtomicRMWInst::BinOp, _sym_STARAtomicRMWInstBinOpSTAR, "llvm::AtomicRMWInst::BinOp");
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
CL_VALUE_ENUM(_sym_UMin, llvm::AtomicRMWInst::UMin);
;
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
CL_BEGIN_ENUM(llvm::Instruction::BinaryOps, _sym_STARBinaryOpsSTAR, "llvm::Instruction::BinaryOps");
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
CL_VALUE_ENUM(_sym_Xor, llvm::Instruction::Xor);
;
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
CL_BEGIN_ENUM(llvm::Instruction::CastOps, _sym_STARInstructionCastOpsSTAR, "llvm::Instruction::CastOps");
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
CL_VALUE_ENUM(_sym_BitCast, llvm::Instruction::BitCast);
;
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
CL_BEGIN_ENUM(llvm::CmpInst::Predicate, _sym_STARCmpInstPredicateSTAR, "llvm::CmpInst::Predicate");
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
CL_VALUE_ENUM(_sym_ICMP_SLE, llvm::CmpInst::ICMP_SLE);
;
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

}; // namespace llvmo

namespace llvmo {

using namespace llvm;
using namespace llvm::orc;

// #define MONITOR_JIT_MEMORY_MANAGER 1    // monitor SectionMemoryManager
#define DUMP_OBJECT_FILES 1

////////////////////////////////////////////////////////////
//
// Register Jitted object files with gdb
//
// Using interface described here:
//     https://sourceware.org/gdb/current/onlinedocs/gdb/JIT-Interface.html#JIT-Interface
#if 0
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
#endif

mp::Mutex* global_jit_descriptor = NULL;

void register_object_file_with_gdb(const llvm::object::ObjectFile& Obj,
                                   const llvm::RuntimeDyld::LoadedObjectInfo& loadedObjectInfo) {
  //  printf("%s:%d:%s  ObjectFile@%p\n", __FILE__, __LINE__, __FUNCTION__, &Obj);
  uint64_t Key = static_cast<uint64_t>(reinterpret_cast<uintptr_t>(Obj.getData().data()));
  if (global_jit_descriptor == NULL) {
    global_jit_descriptor = new mp::Mutex(JITGDBIF_NAMEWORD);
  }
  WITH_READ_WRITE_LOCK(*global_jit_descriptor);
  llvm::JITEventListener* listener = JITEventListener::createGDBRegistrationListener();
  listener->notifyObjectLoaded(Key, Obj, loadedObjectInfo);
}

#if 0
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
//    printf("%s:%d Registered object file at %p size: %lu\n", __FILE__, __LINE__, object_file, size );
    global_jit_descriptor->unlock();
};
#endif

}; // namespace llvmo

LLVM_ATTRIBUTE_USED void linkComponents() {
  llvm::errs() << (void*)&llvm_orc_registerEHFrameSectionWrapper << (void*)&llvm_orc_deregisterEHFrameSectionWrapper
               << (void*)&llvm_orc_registerJITLoaderGDBWrapper;
}

SYMBOL_EXPORT_SC_(LlvmoPkg, make_StkSizeRecord);
SYMBOL_EXPORT_SC_(LlvmoPkg, make_StkMapRecord_Location);
SYMBOL_EXPORT_SC_(LlvmoPkg, make_StkMapRecord_LiveOut);
SYMBOL_EXPORT_SC_(LlvmoPkg, make_StkMapRecord);
SYMBOL_EXPORT_SC_(LlvmoPkg, make_StackMap);
SYMBOL_EXPORT_SC_(KeywordPkg, register);
SYMBOL_EXPORT_SC_(KeywordPkg, direct);
SYMBOL_EXPORT_SC_(KeywordPkg, indirect);
SYMBOL_EXPORT_SC_(KeywordPkg, constant);
SYMBOL_EXPORT_SC_(KeywordPkg, constant_index);

namespace llvmo {

using namespace llvm;

#if 0
DOCGROUP(clasp);
CL_DEFUN core::T_sp llvm_sys__vmmap()
{
  auto task = task_for_pid();
  return nil<core::T_O>();x
}
#endif

SYMBOL_EXPORT_SC_(LlvmoPkg, library);
}; // namespace llvmo

namespace llvmo {

DOCGROUP(clasp);
CL_DEFUN core::T_sp llvm_sys__lookup_jit_symbol_info(void* ptr) {
  printf("%s:%d:%s ptr = %p\n", __FILE__, __LINE__, __FUNCTION__, ptr);
  core::HashTable_sp ht = gc::As<core::HashTable_sp>(comp::_sym_STARjit_saved_symbol_infoSTAR->symbolValue());
  core::T_sp result = nil<core::T_O>();
  ht->map_while_true([ptr, &result](core::T_sp key, core::T_sp value) -> bool {
    if (value.consp()) {
      core::T_sp address = value.unsafe_cons()->ocadr();
      core::T_sp size = value.unsafe_cons()->car();
      char* start = (char*)(gc::As<core::Pointer_sp>(address)->ptr());
      if (size.fixnump()) {
        char* end = start + size.unsafe_fixnum();
        //          printf("%s:%d  Comparing ptr@%p to %p - %p\n", __FILE__, __LINE__, ptr, start, end);
        if (start <= (char*)ptr && ptr < end) {
          result = core::Cons_O::create(key, value);
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
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__remove_useless_global_ctors(llvmo::Module_sp module) {
  llvm::Module* M = module->wrappedPtr();
  llvm::GlobalVariable* ctors = M->getGlobalVariable("llvm.global_ctors");
  if (ctors) {
    Value* init = ctors->getInitializer();
    ConstantArray* list = llvm::dyn_cast<ConstantArray>(init);
    std::vector<Function*> ctors_to_delete;
    if (list) {
      for (unsigned i = 0, e = list->getNumOperands(); i != e; ++i) {
        llvm::ConstantStruct* oneStruct = llvm::dyn_cast<llvm::ConstantStruct>(list->getOperand(i));
        if (!oneStruct)
          continue;
        llvm::Function* oneFunc = llvm::dyn_cast<llvm::Function>(oneStruct->getOperand(1));
        if (!oneFunc)
          continue;
        //        printf("%s:%d  oneFunc[%u] = %s\n", __FILE__, __LINE__, i, oneFunc->getName().str().c_str());
        ctors_to_delete.push_back(oneFunc);
      }
    }
    ctors->eraseFromParent();
    for (auto ctor : ctors_to_delete) {
      ctor->eraseFromParent();
    }
  }
}

void removeAlwaysInlineFunctions(llvm::Module* M) {
  // Silently remove always-inline functions from the module
  std::vector<llvm::Function*> inline_funcs;
  for (auto& F : *M) {
    if (F.hasFnAttribute(llvm::Attribute::AlwaysInline)) {
      inline_funcs.push_back(&F);
    }
  }
  for (auto f : inline_funcs) {
    //    printf("%s:%d Erasing function: %s\n", __FILE__, __LINE__, f->getName().str().c_str());
    f->eraseFromParent();
  }
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__removeAlwaysInlineFunctions(llvm::Module* module) { removeAlwaysInlineFunctions(module); }

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__optimizeModule(llvm::Module* module, int level) {
  llvm::LoopAnalysisManager LAM;
  llvm::FunctionAnalysisManager FAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;

  llvm::PipelineTuningOptions pipeline_opts;
#if LLVM_VERSION_MAJOR > 15
  pipeline_opts.InlinerThreshold = 0;
#endif

  llvm::PassBuilder PB(NULL, pipeline_opts);
  llvm::ModulePassManager MPM;

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::OptimizationLevel opt_level = OptimizationLevel::O0;

#if LLVM_VERSION_MAJOR > 15
  switch (level) {
  case 1:
    opt_level = OptimizationLevel::O1;
    break;
  case 2:
    opt_level = OptimizationLevel::O2;
    break;
  case 3:
    opt_level = OptimizationLevel::O3;
    break;
  }
#endif

  PB.buildPerModuleDefaultPipeline(opt_level);

  MPM.run(*module, MAM);
}

SYMBOL_EXPORT_SC_(CorePkg, repl);
SYMBOL_EXPORT_SC_(KeywordPkg, dump_repl_object_files);
DOCGROUP(clasp);
CL_DEFUN core::Function_sp llvm_sys__jitFinalizeReplFunction(ClaspJIT_sp jit, const string& startupName, const string& shutdownName,
                                                             core::T_sp initialData) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Entered\n", __FILE__, __LINE__, __FUNCTION__));
#ifdef DEBUG_MONITOR
  if (core::_sym_STARdebugStartupSTAR->symbolValue().notnilp()) {
    MONITOR(BF("startup llvm_sys__jitFinalizeReplFunction startupName-> %s\n"), startupName);
  }
#endif
  void* replPtrRaw;
  // Run the startup code by looking up a symbol
  DEBUG_OBJECT_FILES_PRINT(
      ("%s:%d:%s    About to runStartupCode name = %s\n", __FILE__, __LINE__, __FUNCTION__, startupName.c_str()));
  ObjectFile_sp codeObject;
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Lookup %s in JITDylib_sp %p JITDylib* %p JITLINKDylib* %p\n", __FILE__, __LINE__,
                            __FUNCTION__, startupName.c_str(), jit->getMainJITDylib().raw_(), jit->getMainJITDylib()->wrappedPtr(),
                            llvm::cast<JITLinkDylib>(jit->getMainJITDylib()->wrappedPtr())));
  replPtrRaw = jit->runStartupCode(jit->getMainJITDylib(), startupName, initialData);
  core::Function_sp functoid((gctools::Tagged)replPtrRaw);
  DEBUG_OBJECT_FILES_PRINT(
      ("%s:%d:%s   We should have captured the ObjectFile_O and Code_O object\n", __FILE__, __LINE__, __FUNCTION__));
  return functoid;
}

DOCGROUP(clasp);
CL_DEFUN void llvm_sys__jitFinalizeRunCxxFunction(ClaspJIT_sp jit, JITDylib_sp dylib, const string& cxxName) {
  DEPRECATED();
#if 0
  // Run the static constructors
  // The static constructor should call the startup function
  //  but ORC doesn't seem to do this as of llvm9
  //    So use the code below
  llvm::ExitOnError ExitOnErr;
  ExitOnErr(jit->_Jit->runConstructors());
  gctools::smart_ptr<core::Closure_O> functoid;
  if (core::startup_functions_are_waiting()) {
    core::T_O* replPtrRaw = core::startup_functions_invoke(initialData.raw_());
    core::CompiledClosure_fptr_type lisp_funcPtr = (core::CompiledClosure_fptr_type)(replPtrRaw);
    functoid = core::Closure_O::make_bclasp_closure( core::_sym_repl,
                                                              lisp_funcPtr,
                                                              kw::_sym_function,
                                                              nil<core::T_O>(),
                                                              nil<core::T_O>() );
  } else {
    printf("%s:%d No startup functions were available!!!\n", __FILE__, __LINE__);
    abort();
  }
#else
  // So the cxxName is of an external linkage function that is
  // always unique
  core::Pointer_sp startupPtr;
  void* ptr;
  bool found = jit->do_lookup(dylib, cxxName, ptr);
  if (!found) {
    SIMPLE_ERROR("Could not find function {}", cxxName);
  }
  voidStartUp startup = reinterpret_cast<voidStartUp>(ptr);
  //    printf("%s:%d:%s About to invoke startup @p=%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)startup);
  startup();
  // If we load a bitcode file generated by clasp - then startup_functions will be waiting - so run them
  if (core::startup_functions_are_waiting()) {
    core::startup_functions_invoke(NULL);
  }
#endif
}

}; // namespace llvmo

namespace llvmo {

CL_DOCSTRING(R"dx(Tell LLVM what LLVM_DEBUG messages to turn on.)dx");
CL_DOCSTRING_LONG(R"dx(Pass a list of strings like \"dyld\" - which
turns on messages from RuntimeDyld.cpp if NDEBUG is NOT defined for the llvm build.)dx")
DOCGROUP(clasp);
CL_DEFUN void llvm_sys__set_current_debug_types(core::List_sp types) {
  using namespace llvm;
  size_t numStrings = core::cl__length(types);
  char** array = (char**)malloc(sizeof(char*) * numStrings);
  size_t index = 0;
  for (auto cur : types) {
    core::String_sp name = gc::As<core::String_sp>(CONS_CAR(cur));
    std::string sname(name->get_std_string());
    char* oneName = (char*)malloc(sname.size() + 1);
    strncpy(oneName, sname.c_str(), sname.size());
    oneName[sname.size()] = '\0';
    array[index] = oneName;
    index++;
  };
  // Call setCurrentDebugTypes
  setCurrentDebugTypes((const char**)array, index);
  for (size_t idx = 0; idx < index; ++idx) {
    free(array[idx]);
  }
  free(array);
};

}; // namespace llvmo

namespace llvmo { // SectionedAddress_O

SectionedAddress_sp SectionedAddress_O::create(uint64_t SectionIndex, uint64_t Address) {
  auto sa = gctools::GC<SectionedAddress_O>::allocate(SectionIndex, Address);
  return sa;
}

std::string SectionedAddress_O::__repr__() const {
  stringstream ss;
  ss << "#<SECTIONED-ADDRESS ";
  ss << ":section-index " << this->_value.SectionIndex << " ";
  ss << ":address " << (void*)this->_value.Address << ">";
  return ss.str();
}

void python_dump_field(std::ostream& fout, const char* name, bool comma, gctools::Data_types dt, size_t offset, size_t sz = 0) {
  if (comma)
    fmt::print(fout, ",");
  fmt::print(fout, "[ \"{}\", {}, {}, {} ]\n", name, (int)dt, offset, sz);
}

void dump_objects_for_debugger(std::ostream& fout, std::string indent) {
  fmt::print(fout, "{}Init_struct(\"gctools::Header_s::StampWtagMtag\",sizeof={},fields=[ \n", indent.c_str(),
             sizeof(gctools::Header_s::StampWtagMtag));
  python_dump_field(fout, "_value", false, gctools::ctype_int, offsetof(gctools::Header_s::StampWtagMtag, _value),
                    sizeof(gctools::Header_s::StampWtagMtag::_value));
  python_dump_field(fout, "_header_badge", true, gctools::ctype_int, offsetof(gctools::Header_s::BadgeStampWtagMtag, _header_badge),
                    sizeof(gctools::Header_s::BadgeStampWtagMtag::_header_badge));
  fmt::print(fout, "] )\n");
  fmt::print(fout, "{}Init_struct(\"gctools::Header_s\",sizeof={},fields=[ \n", indent.c_str(), sizeof(gctools::Header_s));
  python_dump_field(fout, "_badge_stamp_wtag_mtag._value", false, gctools::ctype_int,
                    offsetof(gctools::Header_s, _badge_stamp_wtag_mtag._value),
                    sizeof(gctools::Header_s::_badge_stamp_wtag_mtag._value));
  python_dump_field(fout, "_stamp_wtag_mtag._header_badge", true, gctools::ctype_int,
                    offsetof(gctools::Header_s, _badge_stamp_wtag_mtag._header_badge),
                    sizeof(gctools::Header_s::_badge_stamp_wtag_mtag._header_badge));
#ifdef DEBUG_GUARD
  python_dump_field(fout, "_tail_start", true, gctools::ctype_int, offsetof(gctools::Header_s, _tail_start),
                    sizeof(gctools::Header_s::_tail_start));
  python_dump_field(fout, "_tail_size", true, gctools::ctype_int, offsetof(gctools::Header_s, _tail_size),
                    sizeof(gctools::Header_s::_tail_size));
  python_dump_field(fout, "_guard", true, gctools::ctype_size_t, offsetof(gctools::Header_s, _guard),
                    sizeof(gctools::Header_s::_guard));
  python_dump_field(fout, "_source", true, gctools::ctype_size_t, offsetof(gctools::Header_s, _source),
                    sizeof(gctools::Header_s::_source));
  python_dump_field(fout, "_guard2", true, gctools::ctype_size_t, offsetof(gctools::Header_s, _guard2),
                    sizeof(gctools::Header_s::_guard2));
  python_dump_field(fout, "_dup_badge_stamp_wtag_mtag", true, gctools::ctype_size_t,
                    offsetof(gctools::Header_s, _dup_badge_stamp_wtag_mtag), sizeof(gctools::Header_s::_dup_badge_stamp_wtag_mtag));
#endif
  fmt::print(fout, "] )\n");
#if 0
  fprintf(fout,"%sInit_struct(\"llvmo::ObjectFileInfo\",sizeof=%lu,fields=[ \n", indent.c_str(), sizeof(llvmo::ObjectFileInfo));
  python_dump_field(fout,"_faso_filename",false,gctools::ctype_const_char_ptr,offsetof(ObjectFileInfo,_faso_filename));
  python_dump_field(fout,"_faso_index",true,gctools::ctype_size_t ,offsetof(ObjectFileInfo,_faso_index));
  python_dump_field(fout,"_objectID",true,gctools::ctype_size_t ,offsetof(ObjectFileInfo,_objectID));
  python_dump_field(fout,"_object_file_start",true,gctools::ctype_opaque_ptr ,offsetof(ObjectFileInfo,_object_file_start));
  python_dump_field(fout,"_object_file_size",true,gctools::ctype_size_t ,offsetof(ObjectFileInfo,_object_file_size));
  python_dump_field(fout,"_stackmap_start",true,gctools::ctype_opaque_ptr ,offsetof(ObjectFileInfo,_stackmap_start));
  python_dump_field(fout,"_stackmap_size",true,gctools::ctype_size_t ,offsetof(ObjectFileInfo,_stackmap_size));
  python_dump_field(fout,"_next",true,gctools::ctype_opaque_ptr ,offsetof(ObjectFileInfo,_next));
  fmt::print(fout,"] )\n");
#endif
};

}; // namespace llvmo

namespace llvmo {

DOCGROUP(clasp);
CL_DEFUN std::string llvm_sys__getDefaultTargetTriple() {
  std::string triple(llvm::sys::getDefaultTargetTriple());
  return triple;
}

}; // namespace llvmo
