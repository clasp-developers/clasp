#pragma once

/*
    File: jit.h
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

// #define USE_JITLINKER 1

#include <clasp/core/common.h>
#include <clasp/core/symbolToEnumConverter.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/array.h>
#include <clasp/core/ql.h>
#include <llvm/Object/SymbolSize.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Linker/Linker.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/CodeGen/TargetSubtargetInfo.h>
// #include "llvm/ExecutionEngine/JIT.h"
#include <llvm/ExecutionEngine/MCJIT.h>
// #include "llvm/ExecutionEngine/JITMemoryManager.h"
#include <llvm/CodeGen/TargetPassConfig.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/ExecutionEngine/JITSymbol.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Passes/PassBuilder.h>
#if LLVM_VERSION_MAJOR < 17
#include <llvm/ADT/Triple.h>
#else
#include <llvm/TargetParser/Triple.h>
#endif
#include <llvm/Support/TargetSelect.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/JITLink/JITLink.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
// #include "llvm/Support/IRBuilder.h"

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <clasp/core/object.h>
#include <clasp/core/metaClass.fwd.h>
#include <clasp/core/externalObject.h>
#include <clasp/core/array.h>
#include <clasp/llvmo/llvmoExpose.fwd.h>
#include <clasp/core/symbolTable.h>
#include <clasp/llvmo/debugInfoExpose.fwd.h>
#include <clasp/core/loadTimeValues.fwd.h>
#include <clasp/llvmo/translators.h>
#include <clasp/llvmo/insertPoint.fwd.h>
#include <clasp/llvmo/debugLoc.fwd.h>
#include <clasp/llvmo/llvmoPackage.h>

namespace llvmo {

FORWARD(ModuleHandle);
FORWARD(ClaspJIT);

using namespace llvm;
using namespace llvm::orc;

//  void save_symbol_info(const llvm::object::ObjectFile& object_file, const llvm::RuntimeDyld::LoadedObjectInfo&
//  loaded_object_info);
}; // namespace llvmo

// Don't allow the object to move, but maybe I'll need to collect it
// if we create a ClaspJIT_O for each thread and need to collect it when
// the thread is killed
template <> struct gctools::GCInfo<llvmo::ClaspJIT_O> {
  static bool constexpr NeedsInitialization = false;
  static bool constexpr NeedsFinalization = true;
  static GCInfo_policy constexpr Policy = collectable_immobile;
};

namespace llvmo {

FORWARD(ObjectFile);
FORWARD(ClaspJIT);
class ClaspJIT_O : public core::General_O {
  LISP_CLASS(llvmo, LlvmoPkg, ClaspJIT_O, "clasp-jit", core::General_O);

public:
  bool do_lookup(JITDylib_sp dylib, const std::string& Name, void*& pointer);
  core::Pointer_sp lookup(JITDylib_sp dylib, const std::string& Name);
  core::T_sp lookup_all_dylibs(const std::string& Name);
  JITDylib_sp getMainJITDylib();
  JITDylib_sp createAndRegisterJITDylib(const std::string& name);
  void registerJITDylibAfterLoad(JITDylib_sp jitDylib);

  ObjectFile_sp addIRModule(JITDylib_sp dylib, Module_sp cM, ThreadSafeContext_sp context, size_t startupID);
  ObjectFile_sp addObjectFile(JITDylib_sp dylib, std::unique_ptr<llvm::MemoryBuffer> objectFile, bool print, size_t startupId);
  void runStartupCode(JITDylib_sp dylib, const std::string& startupName, core::T_sp initialDataOrUnbound);
  void installMainJITDylib();
  void adjustMainJITDylib(JITDylib_sp dylib);
  ClaspJIT_O();

public:
  JITDylib_sp _MainJITDylib;
  std::unique_ptr<llvm::orc::LLJIT> _LLJIT;
};

}; // namespace llvmo

#ifdef _TARGET_OS_DARWIN
#define TEXT_NAME "__TEXT,__text"
#define DATA_NAME "__DATA,__data"
#define EH_FRAME_NAME "__TEXT,__eh_frame"
#define BSS_NAME "__DATA,__bss"
#define STACKMAPS_NAME "__LLVM_STACKMAPS,__llvm_stackmaps"
#define OS_GCROOTS_IN_MODULE_NAME ("_" GCROOTS_IN_MODULE_NAME)
#define OS_LITERALS_NAME ("_" LITERALS_NAME)
#elif defined(_TARGET_OS_LINUX)
#define TEXT_NAME ".text"
#define EH_FRAME_NAME ".eh_frame"
#define DATA_NAME ".data"
#define BSS_NAME ".bss"
#define STACKMAPS_NAME ".llvm_stackmaps"
#define OS_GCROOTS_IN_MODULE_NAME (GCROOTS_IN_MODULE_NAME)
#define OS_LITERALS_NAME (LITERALS_NAME)
#elif defined(_TARGET_OS_FREEBSD)
#define TEXT_NAME ".text"
#define EH_FRAME_NAME ".eh_frame"
#define DATA_NAME ".data"
#define BSS_NAME ".bss"
#define STACKMAPS_NAME ".llvm_stackmaps"
#define OS_GCROOTS_IN_MODULE_NAME (GCROOTS_IN_MODULE_NAME)
#define OS_LITERALS_NAME (LITERALS_NAME)
#else
#error "What is the name of stackmaps section on this OS??? __llvm_stackmaps or .llvm_stackmaps"
#endif

namespace llvmo {
extern std::string gcroots_in_module_name;
extern std::string literals_name;
extern std::atomic<size_t> global_JITDylibCounter;

void ClaspReturnObjectBuffer(std::unique_ptr<llvm::MemoryBuffer> buffer);

uint64_t getModuleSectionIndexForText(llvm::object::ObjectFile& objf);
void llvm_sys__create_lljit_thread_pool();

}; // namespace llvmo
