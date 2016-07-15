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
#include <clasp/core/foundation.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/ADT/Triple.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Pass.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include "llvm/IR/AssemblyAnnotationWriter.h" // will be llvm/IR
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

#include <clasp/core/common.h>
#include <clasp/core/cons.h>
#include <clasp/core/evaluator.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/lispCallingConvention.h>
#include <clasp/core/package.h>
#include <clasp/core/environment.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/multipleValues.h>
#include <clasp/core/environment.h>
#include <clasp/core/sourceFileInfo.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/bignum.h>
#include <clasp/core/compiler.h>
#include <clasp/core/pointer.h>
#include <clasp/core/str.h>
#include <clasp/core/vectorObjectsWithFillPtr.h>
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

namespace llvmo {


CL_DEFUN void compiler__setAssociatedFuncs(core::CompiledFunction_sp cf, core::List_sp associatedFuncs) {
  cf->setAssociatedFunctions(associatedFuncs);
};

CL_DEFUN bool llvm_sys__llvm_value_p(core::T_sp o) {
  if (o.nilp())
    return false;
  if (Value_sp v = o.asOrNull<Value_O>()) {
    (void)v;
    return true;
  }
  return false;
};


CL_DEFUN LLVMContext_sp LLVMContext_O::get_global_context() {
  static llvm::LLVMContext* static_llvm_context = NULL;
  GC_ALLOCATE(LLVMContext_O, context);
  if ( static_llvm_context == NULL ) {
    static_llvm_context = new llvm::LLVMContext();
  }
  context->_ptr = static_llvm_context;
  return context;
};
}

namespace llvmo {


;

}; // llvmo

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
  return Values(_lisp->_boolean(res), core::Str_O::create(errorMsg));
};



//  CL_LISPIFY_NAME(getModule);
//  CL_EXTERN_DEFMETHOD(Linker_O, &llvm::Linker::getModule);

;

}; // llvmo

namespace llvmo {



;

}; // llvmo

namespace llvmo {


  CL_LISPIFY_NAME(createTargetMachine);
  CL_EXTERN_DEFMETHOD(Target_O, &llvm::Target::createTargetMachine);

;

}; // llvmo


namespace llvmo {


CL_LISPIFY_NAME("addPassesToEmitFileAndRunPassManager");
CL_DEFMETHOD void TargetMachine_O::addPassesToEmitFileAndRunPassManager(PassManager_sp passManager,
                                                           core::T_sp stream,
                                                           llvm::TargetMachine::CodeGenFileType FileType,
                                                           Module_sp module) {
  if (stream.nilp()) {
    SIMPLE_ERROR(BF("You must pass a valid stream"));
  }
  llvm::raw_pwrite_stream *ostreamP;
  llvm::SmallString<1024> stringOutput;
  bool stringOutputStream = false;
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
  if (this->wrappedPtr()->addPassesToEmitFile(*passManager->wrappedPtr(), *ostreamP, FileType, true, nullptr, nullptr)) {
    delete ostreamP;
    SIMPLE_ERROR(BF("Could not generate file type"));
  }
  passManager->wrappedPtr()->run(*module->wrappedPtr());
  if (core::StringOutputStream_sp sos = stream.asOrNull<core::StringOutputStream_O>()) {
    sos->fill(stringOutput.c_str());
  }
}

// This was depreciated in llvm3.7
  CL_LISPIFY_NAME(createDataLayout);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, &llvm::TargetMachine::createDataLayout);
  CL_LISPIFY_NAME(getSubtargetImpl);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, (const llvm::TargetSubtargetInfo *(llvm::TargetMachine::*)() const) & llvm::TargetMachine::getSubtargetImpl);
  CL_LISPIFY_NAME(addPassesToEmitFileAndRunPassManager);
  CL_EXTERN_DEFMETHOD(TargetMachine_O, &TargetMachine_O::addPassesToEmitFileAndRunPassManager);

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
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Default);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_JITDefault);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Small);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Kernel);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Medium);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CodeModel_Large);
  CL_BEGIN_ENUM(llvm::CodeModel::Model,_sym_CodeModel, "CodeModel");
  CL_VALUE_ENUM(_sym_CodeModel_Default, llvm::CodeModel::Default);
  CL_VALUE_ENUM(_sym_CodeModel_JITDefault, llvm::CodeModel::JITDefault);
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
CL_EXTERN_DEFUN(&llvm::Triple::normalize);

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
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_KFreeBSD);
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
  SYMBOL_EXPORT_SC_(LlvmoPkg, OSType_Bitrig);
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
  CL_VALUE_ENUM(_sym_OSType_KFreeBSD, llvm::Triple::KFreeBSD);
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
  CL_VALUE_ENUM(_sym_OSType_Bitrig, llvm::Triple::Bitrig);
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


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
Value_sp Value_O::create(llvm::Value *ptr) {
  return core::RP_Create_wrapped<Value_O, llvm::Value *>(ptr);
};
}

namespace llvmo {


  CL_LISPIFY_NAME(dump);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::dump);
  CL_LISPIFY_NAME(getName);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::getName);
  CL_LISPIFY_NAME(setName);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::setName);
  CL_LISPIFY_NAME(getType);
  CL_EXTERN_DEFMETHOD(Value_O, &llvm::Value::getType);;

}; // llvmo


namespace llvmo {

CL_DEFUN void llvm_sys__writeIrToFile(Module_sp module, core::Str_sp path) {
  std::error_code errcode;
  string pathName = path->get();
  llvm::raw_fd_ostream OS(pathName.c_str(), errcode, ::llvm::sys::fs::OpenFlags::F_None);
  if (errcode) {
    SIMPLE_ERROR(BF("Could not write bitcode to %s - problem: %s") % pathName % errcode.message());
  }
  llvm::AssemblyAnnotationWriter *aaw = new llvm::AssemblyAnnotationWriter();
  module->wrappedPtr()->print(OS, aaw);
  delete aaw;
}

CL_DEFUN core::T_mv llvm_sys__verifyModule(Module_sp module, core::Symbol_sp action) {
  string errorInfo;
  llvm::raw_string_ostream ei(errorInfo);
  llvm::Module *m = module->wrappedPtr();
  bool result = llvm::verifyModule(*m, &ei);
  return Values(_lisp->_boolean(result), core::Str_O::create(ei.str()));
};

CL_DEFUN core::T_mv llvm_sys__verifyFunction(Function_sp function) {
  llvm::Function *f = function->wrappedPtr();
  string errorInfo;
  llvm::raw_string_ostream ei(errorInfo);
  bool result = llvm::verifyFunction(*f, &ei);
  return Values(_lisp->_boolean(result), core::Str_O::create(ei.str()));
};

CL_DEFUN void llvm_sys__writeBitcodeToFile(Module_sp module, core::Str_sp pathname) {
  string pn = pathname->get();
  std::error_code errcode;
  llvm::raw_fd_ostream OS(pn.c_str(), errcode, ::llvm::sys::fs::OpenFlags::F_None);
  if (errcode) {
    SIMPLE_ERROR(BF("Could not write bitcode to file[%s] - error: %s") % pn % errcode.message());
  }
  llvm::WriteBitcodeToFile(module->wrappedPtr(), OS);
};

CL_DEFUN Module_sp llvm_sys__parseBitcodeFile(core::Str_sp filename, LLVMContext_sp context) {
  //	printf("%s:%d af_parseBitcodeFile %s\n", __FILE__, __LINE__, filename->c_str() );
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> eo_membuf = llvm::MemoryBuffer::getFile(filename->get());
  if (std::error_code ec = eo_membuf.getError()) {
    SIMPLE_ERROR(BF("Could not load bitcode for file %s - error: %s") % filename->get() % ec.message());
  }
  llvm::ErrorOr<std::unique_ptr<llvm::Module>> eom = llvm::parseBitcodeFile(eo_membuf.get()->getMemBufferRef(), *(context->wrappedPtr()));
  if (std::error_code eo2 = eom.getError()) {
    SIMPLE_ERROR(BF("Could not parse bitcode for file %s - error: %s") % filename->get() % eo2.message());
  }

#if 0
	if ( engine->hasNamedModule(filename->get()))
	    {
		engine->removeNamedModule(filename->get());
		LOG(BF("Removed existing module: %s") % filename->get());
	    }
	Module_sp omodule = core::RP_Create_wrapped<Module_O,llvm::Module*>(m);
	engine->addNamedModule(filename->get(),omodule);
	LOG(BF("Added module: %s") % filename->get());
#else
        Module_sp omodule = core::RP_Create_wrapped<Module_O, llvm::Module *>(eom.get().release());
#endif
  return omodule;
};

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





  CL_PKG_NAME(LlvmoPkg,"attributeSetGet");
  CL_EXTERN_DEFUN((llvm::AttributeSet (*)(llvm::LLVMContext &, unsigned, llvm::ArrayRef<llvm::Attribute::AttrKind>)) & llvm::AttributeSet::get);



CL_DEFUN Value_sp llvm_sys__makeStringGlobal(Module_sp module, core::Str_sp svalue) {
  llvm::Module &M = *(module->wrappedPtr());
  llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(M.getContext(), svalue->get());
  llvm::GlobalVariable *GV = new llvm::GlobalVariable(M, StrConstant->getType(),
                                                      true, llvm::GlobalValue::InternalLinkage,
                                                      StrConstant);
  GV->setName(":::str");
  GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return gc::As<Value_sp>(translate::to_object<llvm::Value *>::convert(GV));
}

// Define Value_O::__repr__ which is prototyped in llvmoExpose.lisp
string Value_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
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
      core::T_sp element = vecelements->elt(i);
      Type_sp ty = gc::As<Type_sp>(element);
      velements.push_back(ty->wrappedPtr());
    }
    return;
  }
  QERROR_WRONG_TYPE_NTH_ARG(0, elements, cl::_sym_sequence);
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

CL_DEFUN core::List_sp llvm_sys__module_get_function_list(Module_sp module) {
  ql::list fl(_lisp);
  for (llvm::Function &f : *module->wrappedPtr()) {
    Function_sp wrapped_func = gc::As<Function_sp>(translate::to_object<const llvm::Function &>::convert(f));
    fl << wrapped_func;
  }
  return fl.cons();
};
};

namespace llvmo {
  CL_LISPIFY_NAME(dump);
  CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::dump);
  CL_LISPIFY_NAME(addModuleFlag);
  CL_EXTERN_DEFMETHOD(Module_O, (void (llvm::Module::*)(llvm::MDNode *))&llvm::Module::addModuleFlag);
  CL_LISPIFY_NAME(getModuleIdentifier);
  CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getModuleIdentifier);
  CL_LISPIFY_NAME(getGlobalVariable);
  CL_EXTERN_DEFMETHOD(Module_O, (llvm::GlobalVariable *(llvm::Module::*)(llvm::StringRef, bool)) &llvm::Module::getGlobalVariable);
  CL_LISPIFY_NAME(getNamedGlobal);
  CL_EXTERN_DEFMETHOD(Module_O, (llvm::GlobalVariable *(llvm::Module::*)(llvm::StringRef))&llvm::Module::getNamedGlobal);
  CL_LISPIFY_NAME(getOrInsertGlobal);
  CL_EXTERN_DEFMETHOD(Module_O, &llvm::Module::getOrInsertGlobal);
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
CL_DEFMETHOD llvm::Function *Module_O::getFunction(core::Str_sp dispatchName) {
  llvm::Module *module = this->wrappedPtr();
  string funcName = dispatchName->get();
  llvm::Function *func = module->getFunction(funcName);
  return func;
}

CL_LISPIFY_NAME("moduleValid");
CL_DEFMETHOD bool Module_O::valid() const {
  return this->wrappedPtr() != NULL;
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
  for (llvm::Module::const_named_metadata_iterator it = M->named_metadata_begin();
       it != M->named_metadata_end(); it++) {
    (*it).dump();
  }
}

void Module_O::initialize() {
  this->Base::initialize();
  this->_UniqueGlobalVariableStrings = core::HashTableEqual_O::create_default();
}

CL_LISPIFY_NAME("getOrCreateUniquedStringGlobalVariable");
CL_DEFMETHOD GlobalVariable_sp Module_O::getOrCreateUniquedStringGlobalVariable(const string &value, const string &name) {
  core::Str_sp nameKey = core::Str_O::create(name);
  core::List_sp it = this->_UniqueGlobalVariableStrings->gethash(nameKey);
  //	map<string,GlobalVariableStringHolder>::iterator it = this->_UniqueGlobalVariableStrings.find(name);
  llvm::GlobalVariable *GV;
  if (it.nilp()) {
    llvm::Module *M = this->wrappedPtr();
    llvm::LLVMContext &context = M->getContext();
    llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(context, value);
    GV = new llvm::GlobalVariable(*M, StrConstant->getType(),
                                  true, llvm::GlobalValue::InternalLinkage,
                                  StrConstant);
    GV->setName(name);
    GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    //	    GlobalVariableStringHolder holder;
    core::Str_sp first = core::Str_O::create(value);
    GlobalVariable_sp second = core::RP_Create_wrapped<GlobalVariable_O, llvm::GlobalVariable *>(GV);
    core::Cons_sp pair = core::Cons_O::create(first, second);
    this->_UniqueGlobalVariableStrings->setf_gethash(nameKey, pair);
    //	    holder._String = value;
    //	    holder._LlvmValue = core::RP_Create_wrapped<GlobalVariable_O,llvm::GlobalVariable*>(GV);
    //	    this->_UniqueGlobalVariableStrings[name] = holder;
    //	    return holder._LlvmValue;
    return second;
  }
  if (gc::As<core::Str_sp>(oCar(it))->get() != value) // as<Str_Oit->second._String != value )
  {
    SIMPLE_ERROR(BF("You tried to getOrCreateUniquedStringGlobalVariable with name[%s] and value[%s] - there was already a StringGlobalVariable with that name but it has a different value!!!! value[%s]") % name % value % gc::As<core::Str_sp>(oCar(it))->get()); // it->second._String );
  }
  return gc::As<GlobalVariable_sp>(oCdr(it)); // it->second._LlvmValue;
}

CL_LISPIFY_NAME("getGlobalList");
CL_DEFMETHOD core::List_sp Module_O::getGlobalList() const {
  ql::list globals(_lisp);
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
  ss << "#<" << this->_instanceClass()->classNameAsString() << " " << this->_ptr << " > ";
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
  core::Str_sp key = core::Str_O::create(name);
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
  if (this->_DependentModules->contains(core::Str_O::create(name)))
    return true;
  return false;
}

void ExecutionEngine_O::removeNamedModule(const string &name) {
  core::Str_sp key = core::Str_O::create(name);
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

CL_LISPIFY_NAME("find_function_named");
CL_DEFMETHOD Function_sp ExecutionEngine_O::find_function_named(core::Str_sp name) {
  return translate::to_object<llvm::Function *>::convert(this->wrappedPtr()->FindFunctionNamed(name->get().c_str()));
}



  CL_LISPIFY_NAME(clearAllGlobalMappings);
  CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::clearAllGlobalMappings);
  CL_LISPIFY_NAME(getDataLayout);
  CL_EXTERN_DEFMETHOD(ExecutionEngine_O, &llvm::ExecutionEngine::getDataLayout);


}; // llvmo


namespace llvmo {
CL_LISPIFY_NAME("DataLayoutCopy");
CL_DEFMETHOD DataLayout_sp DataLayout_O::copy() const {
  GC_ALLOCATE_VARIADIC(DataLayout_O, cp, *(this->_DataLayout));
  return cp;
};



CL_LISPIFY_NAME(DataLayout-getTypeAllocSize);
CL_DEFMETHOD size_t DataLayout_O::getTypeAllocSize(llvm::Type* ty)
{
  return this->_DataLayout->getTypeAllocSize(ty);
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

#if 0 // TargetData was depreciated
namespace llvmo
{


CL_LAMBDA(module);
CL_PKG_NAME(LlvmoPkg,"target-data-copy");
CL_DEFUN TargetData_sp TargetData_O::copy(llvm::TargetData const & orig)
    {
        GC_ALLOCATE(TargetData_O,self );
	self->_ptr = new llvm::TargetData(orig);
	return self;
    };


};

    void TargetData_O::exposePython(core::Lisp_sp lisp)
 llvmo
namespace llvmo
{
}

#endif

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
}

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
}

namespace llvmo {
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

#if 0
    void EngineBuilder_O::setUseMCJIT(bool use_mcjit)
    {
	this->wrappedPtr()->setUseMCJIT(use_mcjit);
#if 0
	if ( use_mcjit )
	{
	    // Setup to use MCJIT
	    llvm::EngineBuilder* builder = this->wrappedPtr();
	    builder->setRelocationModel(llvm::Reloc::Default);
	    builder->setOptLevel(llvm::CodeGenOpt::Default);
	    llvm::JITMemoryManager* mm = new llvm::SectionMemoryManager();
	    this->wrappedPtr()->setJITMemoryManager(mm);
	}
#endif
    }
#endif

CL_LISPIFY_NAME("setTargetOptions");
CL_DEFMETHOD void EngineBuilder_O::setTargetOptions(TargetOptions_sp options) {
  this->wrappedPtr()->setTargetOptions(*options->wrappedPtr());
}



;


CL_LISPIFY_NAME("create");
CL_DEFMETHOD ExecutionEngine_sp EngineBuilder_O::createExecutionEngine() {
  llvm::ExecutionEngine *ee = this->wrappedPtr()->create();
  ExecutionEngine_sp eeo = core::RP_Create_wrapped<ExecutionEngine_O, llvm::ExecutionEngine *>(ee);
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


;

}; // llvmo

namespace llvmo {



;

}; // llvmo
namespace llvmo {
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
      vector_IdxList.push_back(unbox_fixnum(gc::As<core::Fixnum_sp>(vvalues->svref(i))));
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
}

namespace llvmo {
CL_LAMBDA(type values);
CL_LISPIFY_NAME(constant-array-get);
CL_DEFUN Constant_sp ConstantArray_O::get(ArrayType_sp type, core::List_sp values) {
  Constant_sp ca = ConstantArray_O::create();
  vector<llvm::Constant *> vector_IdxList;
  for (auto cur : values) {
    vector_IdxList.push_back(gc::As<Constant_sp>(oCar(cur))->wrappedPtr());
  }
  llvm::ArrayRef<llvm::Constant *> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant *llvm_ca = llvm::ConstantArray::get(type->wrapped(), array_ref_vector_IdxList);
  ca->set_wrapped(llvm_ca);
  return ca;
}



;

}; // llvmo
namespace llvmo {
}

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
}

namespace llvmo {

CL_LISPIFY_NAME(constant-expr-get-in-bounds-get-element-ptr);
CL_DEFUN Constant_sp ConstantExpr_O::getInBoundsGetElementPtr(llvm::Type* element_type, Constant_sp constant, core::List_sp idxList) {
  GC_ALLOCATE(Constant_O, res);
  vector<llvm::Constant *> vector_IdxList;
  for (auto cur : idxList) {
    vector_IdxList.push_back(gc::As<Constant_sp>(oCar(cur))->wrappedPtr());
  }
  llvm::ArrayRef<llvm::Constant *> array_ref_vector_IdxList(vector_IdxList);
  llvm::Constant *llvm_res = llvm::ConstantExpr::getInBoundsGetElementPtr(element_type,constant->wrappedPtr(), array_ref_vector_IdxList);
  res->set_wrapped(llvm_res);
  return res;
}



;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {

CL_LAMBDA("module type is-constant linkage initializer name &optional (insert-before nil) (thread-local-mode 'llvm-sys:not-thread-local)");
CL_LISPIFY_NAME(make-global-variable);
CL_DEFUN GlobalVariable_sp GlobalVariable_O::make(Module_sp mod, Type_sp type, bool isConstant, core::Symbol_sp linkage, /*Constant_sp*/ core::T_sp initializer, core::Str_sp name, /*GlobalVariable_sp*/ core::T_sp insertBefore, core::Symbol_sp threadLocalMode) {
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
  llvm::GlobalVariable *gv = new llvm::GlobalVariable(*(mod->wrappedPtr()), type->wrappedPtr(), isConstant, llinkage._v, llvm_initializer, name->get(), lInsertBefore, lThreadLocalMode._v);
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
CL_DEFMETHOD void Instruction_O::setMetadata(core::Str_sp kind, MDNode_sp mdnode) {
  this->wrappedPtr()->setMetadata(kind->get(), mdnode->wrappedPtr());
}



  CL_LISPIFY_NAME(getParent);
  CL_EXTERN_DEFMETHOD(Instruction_O, (llvm::BasicBlock * (llvm::Instruction::*)()) & llvm::Instruction::getParent);

;


CL_LISPIFY_NAME("terminatorInstP");
CL_DEFMETHOD bool Instruction_O::terminatorInstP() const {
  return llvm::TerminatorInst::classof(this->wrappedPtr());
}

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


  CL_LISPIFY_NAME(addIncoming);
  CL_EXTERN_DEFMETHOD(PHINode_O, &llvm::PHINode::addIncoming);;

;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

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
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


  CL_LISPIFY_NAME(setAlignment);
  CL_EXTERN_DEFMETHOD(AllocaInst_O, &AllocaInst_O::ExternalType::setAlignment);;

;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;


CL_LISPIFY_NAME("addCase");
CL_DEFMETHOD void SwitchInst_O::addCase(ConstantInt_sp onVal, BasicBlock_sp dest) {
  this->wrappedPtr()->addCase(onVal->wrappedPtr(), dest->wrappedPtr());
}

}; // llvmo
namespace llvmo {
}

namespace llvmo {

  CL_LISPIFY_NAME(addDestination);
  CL_EXTERN_DEFMETHOD(IndirectBrInst_O, &llvm::IndirectBrInst::addDestination);;

;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


;

}; // llvmo
namespace llvmo {
}

namespace llvmo {


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
  CL_EXTERN_DEFUN((llvm::Constant *(*)(llvm::Type *, llvm::StringRef))&llvm::ConstantFP::get);

;


string ConstantFP_O::__repr__() const {
  stringstream ss;
  llvm::APFloat const &val = this->wrappedPtr()->getValueAPF();
  llvm::SmallVector<char, 100> svistr;
  val.toString(svistr);
  std::string str(svistr.data(), svistr.size());
  ss << "#<" << this->_instanceClass()->classNameAsString() << " " << str << ">";
  return ss.str();
}

}; // llvmo
namespace llvmo {
}

namespace llvmo {
ConstantInt_sp ConstantInt_O::create(llvm::ConstantInt *ptr) {
  return core::RP_Create_wrapped<ConstantInt_O, llvm::ConstantInt *>(ptr);
};


  CL_LISPIFY_NAME(constant-int-get);
  CL_EXTERN_DEFUN((llvm::ConstantInt *(*)(llvm::LLVMContext &, const llvm::APInt &)) &llvm::ConstantInt::get);

;


string ConstantInt_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " " << this->wrappedPtr()->getValue().toString(10, true) << ">";
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
  CL_EXTERN_DEFUN(&llvm::UndefValue::get);

;


string UndefValue_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {
ConstantPointerNull_sp ConstantPointerNull_O::create(llvm::ConstantPointerNull *ptr) {
  return core::RP_Create_wrapped<ConstantPointerNull_O, llvm::ConstantPointerNull *>(ptr);
};



  CL_LISPIFY_NAME(constant-pointer-null-get);
  CL_EXTERN_DEFUN(&llvm::ConstantPointerNull::get);

;


string ConstantPointerNull_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {
}

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


;

}; // llvmo

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
    core::Fixnum_sp fixnum_value = gc::As<core::Fixnum_sp>(value);
    if (!sign && unbox_fixnum(fixnum_value) < 0) {
      SIMPLE_ERROR(BF("You tried to create an unsigned APInt32 with the negative value: %d") % unbox_fixnum(fixnum_value));
    }
    apint = llvm::APInt(width, clasp_to_fixnum(fixnum_value), sign);
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
#if 0
	if ( numbits < width )
	{
	    apint = apint.zext(width);
	}
#endif
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
  ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
  ss << this->_value.toString(10, true);
  ss << ">";
  return ss.str();
}
}; // llvmo

namespace llvmo {


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

CL_LISPIFY_NAME("SetCurrentDebugLocation");
CL_DEFMETHOD void IRBuilderBase_O::SetCurrentDebugLocation(DebugLoc_sp loc) {
  //	llvm::DebugLoc dlold = this->wrappedPtr()->getCurrentDebugLocation();
  //	printf("                       old DebugLocation: %d\n", dlold.getLine() );
  this->_CurrentDebugLocationSet = true;
  llvm::DebugLoc &dl = loc->debugLoc();
  //	printf("%s:%d IRBuilderBase_O::SetCurrentDebugLoc changing to line %d\n", __FILE__, __LINE__, dl.getLine() );
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
  //	llvm::DebugLoc dlnew = this->wrappedPtr()->getCurrentDebugLocation();
  //	printf("                       new DebugLocation: %d\n", dlnew.getLine() );
}

#if 0
CL_LISPIFY_NAME("SetCurrentDebugLocationToLineColumnScope");
CL_DEFMETHOD void IRBuilderBase_O::SetCurrentDebugLocationToLineColumnScope(int line, int col, DebugInfo_sp scope) {
  this->_CurrentDebugLocationSet = true;
  llvm::MDNode *mdnode = scope->operator llvm::MDNode *();
  llvm::DebugLoc dl = llvm::DebugLoc::get(line, col, mdnode);
  this->wrappedPtr()->SetCurrentDebugLocation(dl);
}
#endif

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
  ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
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
  ss << " >";
  return ss.str();
}

  CL_LAMBDA (irbuilder cond true-branch false-branch &optional branch-weights unpred);
  CL_LISPIFY_NAME(CreateCondBr);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateCondBr);

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
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateAlloca);
  CL_LISPIFY_NAME(CreateStore);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateStore);
  CL_LISPIFY_NAME(CreateFence);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateFence);
  CL_LISPIFY_NAME(CreateAtomicCmpXchg);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateAtomicCmpXchg);
  CL_LISPIFY_NAME(CreateAtomicRMW);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateAtomicRMW);
CL_LISPIFY_NAME(CreateConstGEP1-32);
CL_EXTERN_DEFMETHOD(IRBuilder_O, (llvm::Value *(IRBuilder_O::ExternalType::*)(llvm::Value *Ptr, unsigned Idx0, const llvm::Twine &Name ))
                    &IRBuilder_O::ExternalType::CreateConstGEP1_32);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP1-32);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_32);
  CL_LISPIFY_NAME(CreateConstGEP2-32);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstGEP2_32);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP2-32);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP2_32);
  CL_LISPIFY_NAME(CreateConstGEP1-64);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstGEP1_64);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP1-64);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP1_64);
  CL_LISPIFY_NAME(CreateConstGEP2-64);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstGEP2_64);
  CL_LISPIFY_NAME(CreateConstInBoundsGEP2-64);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateConstInBoundsGEP2_64);
  CL_LISPIFY_NAME(CreateStructGEP);
  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateStructGEP);
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
//CL_LISPIFY_NAME(CreateCall0);
// CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::CallInst *(IRBuilder_O::ExternalType::*) (llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateCall);
//CL_LISPIFY_NAME(CreateCall1);
// CL_EXTERN_DEFMETHOD(IRBuilder_O,(llvm::CallInst *(IRBuilder_O::ExternalType::*) (llvm::Value *, llvm::Value *, const llvm::Twine &) )&IRBuilder_O::ExternalType::CreateCall);
//  CL_LISPIFY_NAME(CreateCall2);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateCall2);
//  CL_LISPIFY_NAME(CreateCall3);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateCall3);
//  CL_LISPIFY_NAME(CreateCall4);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateCall4);
//  CL_LISPIFY_NAME(CreateCall5);
//  CL_EXTERN_DEFMETHOD(IRBuilder_O, &IRBuilder_O::ExternalType::CreateCall5);
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

;

}; // llvmo

namespace llvmo {

  CL_LISPIFY_NAME(addAttr);
  CL_EXTERN_DEFMETHOD(Argument_O, (void(llvm::Argument::*)(llvm::AttributeSet))&llvm::Argument::addAttr);
  CL_LISPIFY_NAME(removeAttr);
CL_EXTERN_DEFMETHOD(Argument_O, (void(llvm::Argument::*)(llvm::AttributeSet))&llvm::Argument::removeAttr);
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
}

namespace llvmo {

CL_LISPIFY_NAME(mdstring-get);
CL_DEFUN MDString_sp MDString_O::get(LLVMContext_sp context, core::Str_sp str) {
  llvm::MDString *mdstr = llvm::MDString::get(*context->wrappedPtr(), str->get());
  MDString_sp omd = core::RP_Create_wrapped<llvmo::MDString_O, llvm::MDString *>(mdstr);
  return omd;
}



  SYMBOL_EXPORT_SC_(LlvmoPkg, mdnodeGet);

;

}; // llvmo
namespace llvmo {
}

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
}

namespace llvmo {



;

}; // llvmo

namespace llvmo {

CL_DEFUN Function_sp llvm_sys__FunctionCreate(FunctionType_sp tysp, llvm::GlobalValue::LinkageTypes linkage, core::Str_sp nsp, Module_sp modulesp) {
  translate::from_object<llvm::FunctionType *> ty(tysp);
  translate::from_object<llvm::Module *> m(modulesp);
  //        printf("%s:%d FunctionCreate %s with linkage %d\n", __FILE__, __LINE__, nsp->get().c_str(), linkage);
  llvm::Function *func = llvm::Function::Create(ty._v, linkage, nsp->get(), m._v);
  Function_sp funcsp = gc::As<Function_sp>(translate::to_object<llvm::Function *>::convert(func));
  return funcsp;
};

CL_LISPIFY_NAME("getArgumentList");
CL_DEFMETHOD core::List_sp Function_O::getArgumentList() {
  ql::list l(_lisp);
  llvm::Function::ArgumentListType &args = this->wrappedPtr()->getArgumentList();
  return translate::to_object<llvm::Function::ArgumentListType &>::convert(args);
}

string Function_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << ">";
  //this->wrappedPtr()->dump();
  return ss.str();
}

CL_LISPIFY_NAME("appendBasicBlock");
CL_DEFMETHOD void Function_O::appendBasicBlock(BasicBlock_sp basicBlock) {
  this->wrappedPtr()->getBasicBlockList().push_back(basicBlock->wrappedPtr());
}

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
;


CL_LISPIFY_NAME("setLiterals");
CL_DEFMETHOD void Function_O::setLiterals(core::LoadTimeValues_sp ltv) {
  this->_RunTimeValues = ltv;
}

CL_LISPIFY_NAME("literals");
CL_DEFMETHOD core::LoadTimeValues_sp Function_O::literals() const {
  return this->_RunTimeValues;
}

}; // llvmo

namespace llvmo {


CL_LISPIFY_NAME(getParent);
CL_EXTERN_DEFMETHOD(BasicBlock_O,(llvm::Function *(llvm::BasicBlock::*)())&llvm::BasicBlock::getParent);

CL_LAMBDA("context &optional (name \"\") parent basic-block");
CL_LISPIFY_NAME(basic-block-create);
CL_EXTERN_DEFUN( &llvm::BasicBlock::Create );

;


CL_LISPIFY_NAME("BasicBlockEmpty");
CL_DEFMETHOD bool BasicBlock_O::empty() {
  return this->wrappedPtr()->empty();
}

CL_LISPIFY_NAME("BasicBlockBack");
CL_DEFMETHOD Instruction_sp BasicBlock_O::back() {
  llvm::Instruction &inst = this->wrappedPtr()->back();
  return core::RP_Create_wrapped<Instruction_O, llvm::Instruction *>(&inst);
}

}; // llvmo

namespace llvmo {


bool Type_O::equal(core::T_sp obj) const {
  if (Type_sp t = obj.asOrNull<Type_O>()) {
    return t->_ptr == this->_ptr;
  }
  return false;
}

string Type_O::__repr__() const {
  stringstream ss;
  ss << "#<" << this->_instanceClass()->classNameAsString() << " ";
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
CL_EXTERN_DEFMETHOD(Type_O,&llvm::Type::dump);
CL_EXTERN_DEFMETHOD(Type_O,&llvm::Type::getSequentialElementType);

CL_PKG_NAME(LlvmoPkg, "type-get-float-ty");
CL_EXTERN_DEFUN(&llvm::Type::getFloatTy);

  CL_LISPIFY_NAME(dump);
  CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::dump);
  CL_LISPIFY_NAME(getSequentialElementType);
  CL_EXTERN_DEFMETHOD(Type_O, &llvm::Type::getSequentialElementType);;

  CL_LISPIFY_NAME(type-get-float-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getFloatTy);
  CL_LISPIFY_NAME(type-get-double-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getDoubleTy);
  CL_LISPIFY_NAME(type-get-void-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getVoidTy);
  CL_LISPIFY_NAME(type-get-int1-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt1Ty);
  CL_LISPIFY_NAME(type-get-int8-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt8Ty);
  CL_LISPIFY_NAME(type-get-int32-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt32Ty);
  CL_LISPIFY_NAME(type-get-int64-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt64Ty);
  CL_LISPIFY_NAME(type-get-int8-ptr-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt8PtrTy);
  CL_LISPIFY_NAME(type-get-int32-ptr-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt32PtrTy);
  CL_LISPIFY_NAME(type-get-int64-ptr-ty);
  CL_EXTERN_DEFUN( &llvm::Type::getInt64PtrTy);

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


;

};

namespace llvmo {


;

};

namespace llvmo {


CL_LAMBDA(context &key elements name is-packed);
CL_LISPIFY_NAME(struct-type-create);
CL_DEFUN StructType_sp StructType_O::make(LLVMContext_sp context, core::T_sp elements, core::Str_sp name, core::T_sp isPacked) {
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


;

};

namespace llvmo {

void finalizeEngineAndTime(llvm::ExecutionEngine *engine) {
  core::LightTimer timer;
  timer.start();
  engine->finalizeObject();
  timer.stop();
  double thisTime = timer.getAccumulatedTime();
  core::DoubleFloat_sp df = core::DoubleFloat_O::create(thisTime);
  _sym_STARmostRecentLlvmFinalizationTimeSTAR->setf_symbolValue(df);
  double accTime = clasp_to_double(gc::As<core::Float_sp>(_sym_STARaccumulatedLlvmFinalizationTimeSTAR->symbolValue()));
  accTime += thisTime;
  _sym_STARaccumulatedLlvmFinalizationTimeSTAR->setf_symbolValue(core::DoubleFloat_O::create(accTime));
  int num = unbox_fixnum(gc::As<core::Fixnum_sp>(_sym_STARnumberOfLlvmFinalizationsSTAR->symbolValue()));
  ++num;
  _sym_STARnumberOfLlvmFinalizationsSTAR->setf_symbolValue(core::make_fixnum(num));
}

CL_DEFUN core::Function_sp finalizeEngineAndRegisterWithGcAndGetCompiledFunction(ExecutionEngine_sp oengine, core::T_sp functionName, Function_sp fn, core::T_sp activationFrameEnvironment, core::Str_sp globalRunTimeValueName, core::T_sp fileName, size_t filePos, int linenumber, core::T_sp lambdaList) {
  // Stuff to support MCJIT
  llvm::ExecutionEngine *engine = oengine->wrappedPtr();
  finalizeEngineAndTime(engine);
  ASSERTF(fn.notnilp(), BF("The Function must never be nil"));
  void *p = engine->getPointerToFunction(fn->wrappedPtr());
  if (!p) {
    SIMPLE_ERROR(BF("Could not get a pointer to the function finalizeEngineAndRegisterWithGcAndGetCompiledFunction: %s") % _rep_(functionName));
  }
  core::CompiledClosure_fptr_type lisp_funcPtr = (core::CompiledClosure_fptr_type)(p);
  core::Cons_sp associatedFunctions = core::Cons_O::create(fn, _Nil<core::T_O>());
  core::SourceFileInfo_mv sfi = core__source_file_info(fileName);
  int sfindex = unbox_fixnum(gc::As<core::Fixnum_sp>(sfi.valueGet_(1)));
  //	printf("%s:%d  Allocating CompiledClosure with name: %s\n", __FILE__, __LINE__, _rep_(sym).c_str() );
  gctools::smart_ptr<core::CompiledClosure_O> functoid = gctools::GC<core::CompiledClosure_O>::allocate(functionName, kw::_sym_function, lisp_funcPtr, fn, activationFrameEnvironment, associatedFunctions, lambdaList, sfindex, filePos, linenumber, 0);
  return functoid;
}

struct CtorStruct {
  int priority;
  void (*ctor)();
  char* obj;
};


CL_DEFUN void finalizeEngineAndRegisterWithGcAndRunMainFunctions(ExecutionEngine_sp oengine, core::Str_sp globalRunTimeValueName, core::T_sp fileName) {
  // Stuff to support MCJIT
  llvm::ExecutionEngine *engine = oengine->wrappedPtr();
#ifdef DEBUG_STARTUP
  printf("%s:%d Entered %s\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
  finalizeEngineAndTime(engine);
#if 1
  engine->runStaticConstructorsDestructors(false);
#else
  void (*clasp_ctor)() = reinterpret_cast<void(*)()>(engine->getGlobalValueAddress(CLASP_CTOR_FUNCTION_NAME));
//  printf("%s:%d clasp_ctor --> %p\n", __FILE__, __LINE__, clasp_ctor );
  if ( clasp_ctor == NULL ) {
    SIMPLE_ERROR(BF("Could not get a pointer to %s in finalizeEngineAndRegisterWithGcAndRunMainFunctions") % CLASP_CTOR_FUNCTION_NAME );
  }
#ifdef DEBUG_STARTUP
  printf("%s:%d About to call clasp_ctor\n", __FILE__, __LINE__ );
#endif
  (clasp_ctor)();
#ifdef DEBUG_STARTUP
  printf("%s:%d Returned from call clasp_ctor\n", __FILE__, __LINE__ );
#endif
#endif
  if ( core::startup_functions_are_waiting() ) {
    core::startup_functions_invoke();
  } else {
    SIMPLE_ERROR(BF("There were no startup functions to invoke\n"));
  }
#ifdef DEBUG_STARTUP
  printf("%s:%d Leaving %s\n", __FILE__, __LINE__, __FUNCTION__ );
#endif
}

CL_DEFUN void finalizeClosure(ExecutionEngine_sp oengine, core::Function_sp func) {
  llvm::ExecutionEngine *engine = oengine->wrappedPtr();
  auto closure = func.as<core::CompiledClosure_O>();
  llvmo::Function_sp llvm_func = closure->llvmFunction;
  void *p = engine->getPointerToFunction(llvm_func->wrappedPtr());
  core::CompiledClosure_fptr_type lisp_funcPtr = (core::CompiledClosure_fptr_type)(p);
  closure->fptr = lisp_funcPtr;
}


/*! Return (values target nil) if successful or (values nil error-message) if not */
CL_DEFUN core::T_mv TargetRegistryLookupTarget(const std::string &ArchName, Triple_sp triple) {
  string message;
  llvm::Target *target = const_cast<llvm::Target *>(llvm::TargetRegistry::lookupTarget(ArchName, *triple->wrappedPtr(), message));
  if (target == NULL) {
    return Values(_Nil<core::T_O>(), core::Str_O::create(message));
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
    return Values(_Nil<core::T_O>(), core::Str_O::create(message));
  }
  Target_sp targeto = core::RP_Create_wrapped<Target_O, llvm::Target *>(target);
  return Values(targeto, _Nil<core::T_O>());
}





    SYMBOL_SC_(LlvmoPkg, STARglobal_value_linkage_typesSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, ExternalLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, AvailableExternallyLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkOnceAnyLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkOnceODRLinkage);
  SYMBOL_EXPORT_SC_(LlvmoPkg, LinkOnceODRAutoHideLinkage);
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
  SYMBOL_EXPORT_SC_(LlvmoPkg, verifyFunction);
  //
  // Compiler optimization passes
  //
  //    core::af_def(LlvmoPkg,"createDebugIRPass",&llvmo::af_createDebugIRPass);
//  CL_LISPIFY_NAME(createAliasAnalysisCounterPass);
//  CL_EXTERN_DEFUN( &llvm::createAliasAnalysisCounterPass);
  CL_LISPIFY_NAME(createFunctionInliningPass);
  CL_EXTERN_DEFUN( (llvm::Pass * (*)(unsigned, unsigned)) & llvm::createFunctionInliningPass);

  CL_LISPIFY_NAME(createAlwaysInlinerPass);
  CL_EXTERN_DEFUN( (llvm::Pass * (*)()) & llvm::createAlwaysInlinerPass);

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
  CL_LISPIFY_NAME(createLICMPass);
  CL_EXTERN_DEFUN( &llvm::createLICMPass);
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
  CL_EXTERN_DEFUN( &llvm::createTailCallEliminationPass);
  CL_LISPIFY_NAME(createConstantPropagationPass);
  CL_EXTERN_DEFUN( &llvm::createConstantPropagationPass);
  //    core::af_def(LlvmoPkg,"createDemoteMemoryToRegisterPass",&llvm::createDemoteMemoryToRegisterPass);
  CL_LISPIFY_NAME(createVerifierPass);
  CL_EXTERN_DEFUN( &llvm::createVerifierPass);
  CL_LISPIFY_NAME(createCorrelatedValuePropagationPass);
  CL_EXTERN_DEFUN( &llvm::createCorrelatedValuePropagationPass);
  CL_LISPIFY_NAME(createEarlyCSEPass);
  CL_EXTERN_DEFUN( &llvm::createEarlyCSEPass);
  CL_LISPIFY_NAME(createLowerExpectIntrinsicPass);
  CL_EXTERN_DEFUN( &llvm::createLowerExpectIntrinsicPass);
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
  
  SYMBOL_EXPORT_SC_(LlvmoPkg, STARsynchronization_scopeSTAR);
  SYMBOL_EXPORT_SC_(LlvmoPkg, SingleThread);
  SYMBOL_EXPORT_SC_(LlvmoPkg, CrossThread);
  CL_BEGIN_ENUM(llvm::SynchronizationScope,_sym_STARsynchronization_scopeSTAR, "llvm::SynchronizationScope");
  CL_VALUE_ENUM(_sym_SingleThread, llvm::SingleThread);
  CL_VALUE_ENUM(_sym_CrossThread, llvm::CrossThread);;
  CL_END_ENUM(_sym_STARsynchronization_scopeSTAR);

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



  void initialize_llvmo_expose() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    _sym_STARmostRecentLlvmFinalizationTimeSTAR->defparameter(core::DoubleFloat_O::create(0.0));
    _sym_STARaccumulatedLlvmFinalizationTimeSTAR->defparameter(core::DoubleFloat_O::create(0.0));
    _sym_STARnumberOfLlvmFinalizationsSTAR->defparameter(core::make_fixnum(0));
    llvm::initializeScalarOpts(*llvm::PassRegistry::getPassRegistry());
  }

}; // llvmo
