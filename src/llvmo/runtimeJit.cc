/*
    File: runtimeJit.cc

*/

#define DEBUG_LEVEL_FULL

// #include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <string>
#include <llvm/Config/llvm-config.h>
#if LLVM_VERSION_MAJOR < 18
#include <llvm/ExecutionEngine/Orc/DebuggerSupportPlugin.h>
#else
#include <llvm/ExecutionEngine/Orc/Debugging/DebuggerSupportPlugin.h>
#endif
#include <llvm/ExecutionEngine/Orc/TargetProcess/JITLoaderGDB.h>
#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/cons.h>
#include <clasp/core/mpPackage.h>
#include <clasp/llvmo/code.h>
#include <clasp/gctools/snapshotSaveLoad.h>
#include <clasp/llvmo/jit.h>

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

#include <clasp/gctools/gcFunctions.h>
#include <clasp/core/fli.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/external/thread-pool/thread_pool.h>
#include <clasp/llvmo/jit.h>

namespace llvmo {

using namespace llvm;
using namespace llvm::jitlink;

std::atomic<size_t> global_object_file_number;
std::atomic<size_t> global_JITDylibCounter;

std::string gcroots_in_module_name = OS_GCROOTS_IN_MODULE_NAME;
std::string literals_name = OS_LITERALS_NAME;

core::SimpleBaseString_sp createSimpleBaseStringForStage(const std::string& sname) {
  core::SimpleBaseString_sp name;
  if (snapshotSaveLoad::global_InSnapshotLoad) {
    name = core::SimpleBaseString_O::make<gctools::SnapshotLoadStage>(sname);
  } else {
    name = core::SimpleBaseString_O::make<gctools::RuntimeStage>(sname);
  }
  return name;
}

uint64_t getModuleSectionIndexForText(llvm::object::ObjectFile& objf) {
  for (llvm::object::SectionRef Sec : objf.sections()) {
    if (!Sec.isText() || Sec.isVirtual())
      continue;
    if (Sec.getName()->str() == TEXT_NAME) {
      return Sec.getIndex();
    }
  }
  return llvm::object::SectionedAddress::UndefSection;
}

class ClaspAllocator : public JITLinkMemoryManager {
public:
  ClaspAllocator() {}
  ClaspAllocator(ClaspAllocator&&) = delete;
  ~ClaspAllocator() {}

public:
  mp::Mutex MyMapMutex;
  std::map<void*, std::vector<orc::shared::WrapperFunctionCall>> MyMap;

public:
  static Expected<std::unique_ptr<ClaspAllocator>> Create() {
    Error Err = Error::success();
    std::unique_ptr<ClaspAllocator> Allocator(new ClaspAllocator());
    return std::move(Allocator);
  }

  void allocate(const JITLinkDylib* JD, LinkGraph& G, OnAllocatedFunction OnAllocated) {

    typedef void* BoehmAllocHandle;

    struct MyOpaqueAlloc {
      MyOpaqueAlloc(BoehmAllocHandle StandardSegsMem) : StandardSegsMem(std::move(StandardSegsMem)) {}
      BoehmAllocHandle StandardSegsMem;
      //      std::vector<AllocationAction> DeallocActions;
    };

    class MyInFlightAlloc : public JITLinkMemoryManager::InFlightAlloc {
    public:
      ClaspAllocator* MyAllocator;

    public:
      MyInFlightAlloc(ClaspAllocator* MA, BasicLayout BL) : MyAllocator(MA), BL(std::move(BL)) {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s What do I do here?\n", __FILE__, __LINE__, __FUNCTION__));
      }

      void finalize(OnFinalizedFunction OnFinalized) override {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Entered\n", __FILE__, __LINE__, __FUNCTION__));
        // {1} Apply memory protections to A.DeallocSegsMem and FinalizedSegsMem.
        if (auto Err = applyProtections()) {
          OnFinalized(std::move(Err));
          return;
        }
        // (2) Scrape BL.getGraph().allocationActions() to run finalization actions
        //     and record dealloc actions in A->DeallocActions.
        // I don't know what this is so I'm doing my own thing.
        // (3) Release FinalizeSegsMem.
        // I don't know what this is so I'm ignoring it.
        // (4) Call OnFinalized(pointerToJITTargetAddress(A.release()));
        void* execMemory = NULL;
        size_t numSegments = 0;
        for (auto& KV : BL.segments()) {
          const auto& AG = KV.first;
          auto& Seg = KV.second;
          auto Prot = toSysMemoryProtectionFlags(AG.getMemProt());
          if (Prot & sys::Memory::MF_EXEC)
            execMemory = (void*)Seg.WorkingMem;
          ++numSegments;
        }
        if (auto DeallocActions = runFinalizeActions(BL.getGraph().allocActions())) {
          RAIILock lock(this->MyAllocator->MyMapMutex);
          this->MyAllocator->MyMap[execMemory] = std::move(*DeallocActions);
        } else {
          return OnFinalized(DeallocActions.takeError());
        }
        llvm::orc::ExecutorAddr ea((uintptr_t)execMemory);
        OnFinalized(llvm::jitlink::InProcessMemoryManager::FinalizedAlloc(ea));
      }
      virtual void abandon(OnAbandonedFunction OnAbandoned) {
        printf("%s:%d:%s I have no idea what to do here - calling OnAbandoned and continuing\n", __FILE__, __LINE__, __FUNCTION__);
        if (getenv("CLASP_TRAP_ONABANDONED")!=NULL) {
          printf("!\n!\n!\n!\n         You set CLASP_TRAP_ONABANDONED - sleeping for 9999 seconds - connect using debugger to pid %d\n!\n!\n!\n!\n", getpid());
          sleep(9999);
        }
        printf("!\n!\n!\n!\n         If you want to trap here set CLASP_TRAP_ONABANDONED - restarting for now\n!\n!\n!\n!\n");
        exit(1);
        //OnAbandoned(std::move(llvm::Error::success()));
      }
    private:
      Error applyProtections() {
        JITMemoryReadExecute(BL);
        return Error::success();
      }

    private:
      BasicLayout BL;
    };

    //
    // New llvm-13.0.1 allocation code
    //
    BasicLayout BL(G);
    size_t segmentCount = 0;
    for ([[maybe_unused]] auto& KV : BL.segments()) {
      segmentCount++;
    }

    //
    // Find space in a CodeBlock and set Addr and WorkingMem of each Seg in BL.segments()
    //   if the current CodeBlock doesn't have room then allocate another one.
    //   Return the CodeBlock in codeBlock
    //
    DEBUG_OBJECT_FILES_PRINT(
        ("%s:%d:%s I have BasicLayout - fill it in with allocateInCodeBlock ?\n", __FILE__, __LINE__, __FUNCTION__));
    if (segmentCount > 1) {
      CodeBlock_sp codeBlock;

      if (snapshotSaveLoad::global_InSnapshotLoad) {
        allocateInCodeBlock<gctools::SnapshotLoadStage>(BL, codeBlock);
      } else {
        allocateInCodeBlock<gctools::RuntimeStage>(BL, codeBlock);
      }

      //
      // Create the codeObject and add it to the global AllObjectFiles
      //
#if 0
      core::List_sp jitdylibs = _lisp->_Roots._JITDylibs.load();
      JITDylib_sp theJITDylib = unbound<JITDylib_O>();
      core::T_sp cur = jitdylibs;
      while (cur.consp()) {
        JITDylib_sp one = gc::As<JITDylib_sp>(CONS_CAR(cur));
        JITDylib* jitdylib = one->wrappedPtr();
        JITLinkDylib* jitlinkdylib = llvm::cast<JITLinkDylib>(jitdylib);
        if (jitlinkdylib == JD) theJITDylib = one;
        //DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s one JITDylib_sp = %p JITDylib* = %p JITLinkDylib* = %p\n", __FILE__, __LINE__, __FUNCTION__, one.raw_(), one->wrappedPtr(), llvm::cast<JITLinkDylib>(one->wrappedPtr()) ));
        cur = CONS_CDR(cur);
      }
      if (theJITDylib.unboundp()) {
        printf("%s:%d:%s Could not identify the JITDylib_sp for JITLinkDylib* %p\n", __FILE__, __LINE__, __FUNCTION__, JD );
        abort();
      }
#endif
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s allocating JD = %p \n", __FILE__, __LINE__, __FUNCTION__, JD));
      ObjectFile_sp codeObject;
      codeObject = lookupObjectFile(G.getName());
      codeObject->_CodeBlock = codeBlock;
      [[maybe_unused]] core::SimpleBaseString_sp codeName = createSimpleBaseStringForStage(G.getName());
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s looked up codeObject = %p with name: %s\n", __FILE__, __LINE__, __FUNCTION__,
                                &*codeObject, _rep_(codeName).c_str()));
    } else {
      for (auto& KV : BL.segments()) {
#ifdef DEBUG_OBJECT_FILES
        auto allocGroup = KV.first;
        std::string back;
        llvm::raw_string_ostream ss(back);
        llvm::jitlink::operator<<(ss, allocGroup);
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s single segment allocGroup = %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str()));
#endif
        auto& Seg = KV.second;
        uint64_t ZeroFillStart = Seg.ContentSize;
        size_t SegmentSize = (uintptr_t)gctools::AlignUp(ZeroFillStart + Seg.ZeroFillSize, Seg.Alignment.value());
        void* base;
        base = aligned_alloc(Seg.Alignment.value(), SegmentSize);
        llvm::orc::ExecutorAddr eabase((uintptr_t)base);
        Seg.Addr = eabase;
        Seg.WorkingMem = jitTargetAddressToPointer<char*>((llvm::JITTargetAddress)base);
      }
    }

    if (auto Err = BL.apply()) {
      OnAllocated(std::move(Err));
      return;
    }

    OnAllocated(std::make_unique<MyInFlightAlloc>(this, BL));
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Returned from OnAllocated\n", __FILE__, __LINE__, __FUNCTION__));
  }

  //
  // deallocate only happens at shutdown - so I don't have a Mutex or much else to do
  //
  virtual void deallocate(std::vector<FinalizedAlloc> Allocs, OnDeallocatedFunction OnDeallocated) {
    // std::lock_guard<std::mutex> Lock(FinalizedAllocsMutex);
    Error DeallocErr = Error::success();
    RAIILock lock(this->MyMapMutex);
    for (auto& Alloc : Allocs) {
      // See if there are any dealloc actions to run.
      auto I = this->MyMap.find((void*)Alloc.release().getValue());
      if (I != this->MyMap.end()) {
        DeallocErr = joinErrors(std::move(DeallocErr), runDeallocActions(I->second));
        this->MyMap.erase(I);
      }
    }
    OnDeallocated(std::move(DeallocErr));
  };
};

}; // namespace llvmo

namespace llvmo {

class ClaspPlugin : public llvm::orc::ObjectLinkingLayer::Plugin {
  void modifyPassConfig(llvm::orc::MaterializationResponsibility& MR, llvm::jitlink::LinkGraph& G,
                        llvm::jitlink::PassConfiguration& Config) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s ClaspPlugin modifyPassConfig\n", __FILE__, __LINE__, __FUNCTION__));
    auto PersonalitySymbol = MR.getTargetJITDylib().getExecutionSession().intern("DW.ref.__gxx_personality_v0");
    if (!MR.getSymbols().count(PersonalitySymbol))
      Config.PrePrunePasses.insert(Config.PrePrunePasses.begin(), [this](jitlink::LinkGraph& G) -> Error {
        for (auto ssym : G.defined_symbols()) {
          if (ssym->getName() == "DW.ref.__gxx_personality_v0") {
            DEBUG_OBJECT_FILES_PRINT(
                ("%s:%d:%s PrePrunePass found DW.ref.__gxx_personality_v0 setting Strong Linkage and Local scope\n", __FILE__,
                 __LINE__, __FUNCTION__));
            ssym->setLinkage(Linkage::Strong);
            ssym->setScope(Scope::Local);
            break;
          }
        }
        return Error::success();
      });
    Config.PrePrunePasses.push_back([this](jitlink::LinkGraph& G) -> Error {
      size_t count = 0;
      for (auto& Sec : G.sections()) {
        if (Sec.getName() == EH_FRAME_NAME)
          for (auto* S : Sec.symbols()) {
            S->setLive(true);
            count++;
          }
      }
      for (auto ssym : G.defined_symbols()) {
        std::string sname = ssym->getName().str();
#ifdef DEBUG_OBJECT_FILES
        if (ssym->getName().str() != "") {
          DEBUG_OBJECT_FILES_PRINT(
              ("%s:%d:%s PrePrunePass Symbol: %s\n", __FILE__, __LINE__, __FUNCTION__, ssym->getName().str().c_str()));
        }
#endif

        bool keptAlive = false;
#ifdef _TARGET_OS_LINUX
        keptAlive = true;
//                                        if (ssym->getName().str() != "") {printf("%s:%d:%s Symbol: %s\n", __FILE__, __LINE__,
//                                        __FUNCTION__, ssym->getName().str().c_str() ); };
#endif
        if (sname.find(gcroots_in_module_name) != std::string::npos) {
          keptAlive = true;
        } else if (sname.find(literals_name) != std::string::npos) {
          keptAlive = true;
#if 0
                                          // I'd like to do this on linux because jit symbols need to be exposed
                                          // but it slows down startup enormously
                                        } else if ( sname.find("^^") != std::string::npos ) {
                                          // Keep alive mangled symbols that we care about
//                                          keptAlive = true;
#endif
        }
#ifdef DEBUG_OBJECT_FILES
        if (sname != "") {
          DEBUG_OBJECT_FILES_PRINT(
              ("%s:%d:%s preprune symbol: %s  alive: %d\n", __FILE__, __LINE__, __FUNCTION__, sname.c_str(), keptAlive));
        }
#endif
        if (keptAlive)
          ssym->setLive(true);
      }
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePass setLive %lu symbols\n", __FILE__, __LINE__, __FUNCTION__, count));
      return Error::success();
    });
    Config.PrePrunePasses.push_back([this](jitlink::LinkGraph& G) -> Error {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePasses\n", __FILE__, __LINE__, __FUNCTION__));
      keepAliveStackmap(G);
      // printLinkGraph(G, "PrePrune:");
      return Error::success();
    });
    Config.PostFixupPasses.push_back([this](jitlink::LinkGraph& G) -> Error {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PostFixupPasses\n", __FILE__, __LINE__, __FUNCTION__));
      parseLinkGraph(G);
      // printLinkGraph(G, "PostFixup:");
      return Error::success();
    });
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__));
  }

  void notifyLoaded(llvm::orc::MaterializationResponsibility& MR) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__));
  }

  llvm::Error notifyFailed(llvm::orc::MaterializationResponsibility& MR) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__));
    return Error::success();
  }

#if LLVM_VERSION_MAJOR < 16
  llvm::Error notifyRemovingResources(ResourceKey K) { return Error::success(); }

  void notifyTransferringResources(ResourceKey DstKey, ResourceKey SrcKey) {
    printf("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__);
  }
#else
  Error notifyRemovingResources(JITDylib& JD, ResourceKey K) { return Error::success(); };

  void notifyTransferringResources(JITDylib& JD, ResourceKey DstKey, ResourceKey SrcKey) {
    printf("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__);
  }
#endif

  void keepAliveStackmap(llvm::jitlink::LinkGraph& G) {
    for (auto& S : G.sections()) {
      DEBUG_OBJECT_FILES_PRINT(
          ("%s:%d:%s   section: %s getOrdinal->%u\n", __FILE__, __LINE__, __FUNCTION__, S.getName().str().c_str(), S.getOrdinal()));
      if (S.getName().str() == STACKMAPS_NAME) {
        for (auto& sym : S.symbols()) {
          sym->setLive(true);
        }
      }
    }
  }

  void parseLinkGraph(llvm::jitlink::LinkGraph& G) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Entered with G.getName() = %s\n", __FILE__, __LINE__, __FUNCTION__, G.getName().c_str()));
    uintptr_t textStart = ~0;
    uintptr_t textEnd = 0;
    ObjectFile_sp currentCode = lookupObjectFile(G.getName());
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s     currentCode: %p\n", __FILE__, __LINE__, __FUNCTION__, &*currentCode));
    for (auto& S : G.sections()) {
      DEBUG_OBJECT_FILES_PRINT(
          ("%s:%d:%s  section: %s getOrdinal->%u \n", __FILE__, __LINE__, __FUNCTION__, S.getName().str().c_str(), S.getOrdinal()));
      std::string sectionName = S.getName().str();
      auto Prot = toSysMemoryProtectionFlags(S.getMemProt());
      if ((sectionName.find(BSS_NAME) != string::npos) || (sectionName.find(DATA_NAME) != string::npos)) {
        llvm::jitlink::SectionRange range(S);
        for (auto& sym : S.symbols()) {
          std::string name = sym->getName().str();
          DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s     section: %s symbol:  %s at %p size: %lu\n", __FILE__, __LINE__, __FUNCTION__,
                                    S.getName().str().c_str(), name.c_str(), address, size));
        }
      }
#if 0
      else if (sectionName.find(DATA_NAME)!=string::npos) {
        // If we want to handle the .data section differently than .bss then add more code here
	llvm::jitlink::SectionRange range(S);
	for ( auto& sym : S.symbols() ) {
          // If we need to grab symbols from DATA_NAME segment do it here
	}
      }
#endif
      else if (Prot & llvm::sys::Memory::MF_EXEC) {
        // Text section
        llvm::jitlink::SectionRange range(S);
        if ((uintptr_t)range.getStart().getValue() < textStart)
          textStart = (uintptr_t)range.getStart().getValue();
        uintptr_t tend = (uintptr_t)range.getStart().getValue() + range.getSize();
        if (textEnd < tend)
          textEnd = tend;
        currentCode->_TextSectionStart = (void*)range.getStart().getValue();
        currentCode->_TextSectionEnd = (void*)((char*)range.getStart().getValue() + range.getSize());
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s --- TextSectionStart - TextSectionEnd = %p - %p\n", __FILE__, __LINE__, __FUNCTION__,
                                  currentCode->_TextSectionStart, currentCode->_TextSectionEnd));
#if 0
        if (snapshotSaveLoad::global_debugSnapshot) {
          printf("%s:%d:%s ---------- ObjectFile_sp %p Code_sp %p start %p  end %p\n",
                 __FILE__, __LINE__, __FUNCTION__,
                 my_thread->topObjectFile().raw_(),
                 currentCode.raw_(),
                 currentCode->_TextSectionStart,
                 currentCode->_TextSectionEnd );
        }
#endif
        for (auto& sym : S.symbols()) {
          if (sym->isCallable() && sym->hasName()) {
            std::string name = sym->getName().str();
            void* address = (void*)sym->getAddress().getValue();
            size_t size = sym->getSize();
            core::core__jit_register_symbol(name, size, (void*)address);
          }
        }
      } else if (sectionName.find(EH_FRAME_NAME) != string::npos) {
        llvm::jitlink::SectionRange range(S);
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   eh_frame section segment_start = %p  segment_size = %lu\n", __FILE__, __LINE__,
                                  __FUNCTION__, start, size));
      } else if (sectionName.find(STACKMAPS_NAME) != string::npos) {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   Saving stackmaps range currentCode %s\n", __FILE__, __LINE__, __FUNCTION__,
                                  core::_rep_(currentCode).c_str()));
        llvm::jitlink::SectionRange range(S);
        currentCode->_StackmapStart = (void*)range.getStart().getValue();
        currentCode->_StackmapSize = (size_t)range.getSize();
      }
    }
    // Keep track of the executable region
    if (textStart) {
      //      printf("%s:%d:%s  textStart %p - textStop %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)textStart, (void*)textEnd );
      currentCode->_TextSectionStart = (void*)textStart;
      currentCode->_TextSectionEnd = (void*)textEnd;
      DEBUG_OBJECT_FILES_PRINT(
          ("%s:%d:%s --- ObjectFile_sp %p badge: 0x%0x name: %s ---> final TextSectionStart - TextSectionEnd = %p - %p\n", __FILE__,
           __LINE__, __FUNCTION__, currentCode.raw_(), lisp_badge(currentCode), _rep_(currentCode).c_str(),
           currentCode->_TextSectionStart, currentCode->_TextSectionEnd));
      if (snapshotSaveLoad::global_debugSnapshot) {
        printf("%s:%d:%s ---------- ObjectFile_sp %p text section start %p  end %p\n", __FILE__, __LINE__, __FUNCTION__,
               currentCode.raw_(), currentCode->_TextSectionStart, currentCode->_TextSectionEnd);
      }
    } else {
      printf("%s:%d:%s No executable region was found for the Code_O object for graph %s\n", __FILE__, __LINE__, __FUNCTION__,
             G.getName().c_str());
      for (auto* Sym : G.external_symbols()) {
        printf("       Symbol: %s\n", Sym->getName().str().c_str());
      }
    }
    //
    bool found_gcroots_in_module = false;
    gctools::GCRootsInModule* roots;
    bool found_literals = false;
    for (auto ssym : G.defined_symbols()) {
      if (ssym->getName() == "DW.ref.__gxx_personality_v0") {
        DEBUG_OBJECT_FILES_PRINT(
            ("%s:%d:%s PrePrunePass found DW.ref.__gxx_personality_v0 setting Strong Linkage and Local scope\n", __FILE__, __LINE__,
             __FUNCTION__));
        ssym->setLinkage(Linkage::Strong);
        ssym->setScope(Scope::Local);
        break;
      }
    }
    for (auto ssym : G.defined_symbols()) {
#ifdef DEBUG_OBJECT_FILES
      if (ssym->hasName()) {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s defined_symbol -> hasName: %d name: %s at %p size: %llu\n", __FILE__, __LINE__,
                                  __FUNCTION__, ssym->hasName(), ssym->getName().str().c_str(),
                                  (void*)ssym->getAddress().getValue(), (size_t)ssym->getSize()));
      }
#endif
      if (ssym->hasName()) {
        std::string sname = ssym->getName().str();
        size_t pos;
        pos = sname.find(gcroots_in_module_name);
        if (pos != std::string::npos) {
          found_gcroots_in_module = true;
          roots = (gctools::GCRootsInModule*)ssym->getAddress().getValue();
          continue;
        }
        pos = sname.find(literals_name);
        if (pos != std::string::npos) {
          found_literals = true;
          currentCode->setLiteralVectorStart((void*)ssym->getAddress().getValue());
          DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s currentCode = %p  _LiteralVectorStart -> %p\n"
                                    "   ssym->getSize() = %llu\n",
                                    __FILE__, __LINE__, __FUNCTION__, &*currentCode, (void*)currentCode->_LiteralVectorStart,
                                    (size_t)ssym->getSize()));
          size_t origSymbolSize = (size_t)ssym->getSize();
          size_t symbolSize = origSymbolSize;
          if (symbolSize == 1) {
            // A symbol of size 1 is really zero
            symbolSize = 0;
#ifdef _TARGET_OS_DARWIN
          } else if (symbolSize == 8) {
            // On DARWIN assume symbolSize of 8 is really 0
            // because DARWIN doesn't save symbol size
            // This may be a cludge - I'm not sure (Chris Schafmeister, 2023)
#if 0
            printf("%s:%d:%s Assuming symbolSize is 0 on DARWIN for currentCode->_LiteralVectorStart = %p\n",
                   __FILE__, __LINE__, __FUNCTION__, (void*)currentCode->_LiteralVectorStart );
#endif
            symbolSize = 0;
#endif

          } else if ((symbolSize & 7) != 0) {
            printf("%s:%d:%s The symbol %s is %lu bytes in size but it must be a multiple of 8 bytes!!!\n", __FILE__, __LINE__,
                   __FUNCTION__, sname.c_str(), symbolSize);
            abort();
          }
          currentCode->_LiteralVectorSizeBytes = symbolSize;
          DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Assignment currentCode->_LiteralVectorSizeBytes = %llu\n"
                                    "   origSymbolSize = %llu\n",
                                    __FILE__, __LINE__, __FUNCTION__, (size_t)currentCode->_LiteralVectorSizeBytes,
                                    (size_t)origSymbolSize));
        }
      }
    }
    if (!found_literals) {
      printf("%s:%d Did NOT FIND %s\n", __FILE__, __LINE__, literals_name.c_str());
      abort();
    }
    //
    void* literalStart = (void*)currentCode->getLiteralVectorStart();
    size_t literalCount = currentCode->_LiteralVectorSizeBytes / sizeof(void*);
    if (found_gcroots_in_module) {
      // if we have a GCRoots object, set it up properly.
      // Note that BTB compilation will _not_ have a GCRoots. This is OK.
      roots->_module_memory = literalStart;
      roots->_num_entries = literalCount;
    }
    gctools::clasp_gc_registerRoots(literalStart, literalCount);
#ifdef DEBUG_OBJECT_FILES
    for (auto* Sym : G.external_symbols())
      if (Sym->getName() == "DW.ref.__gxx_personality_v0") {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Graph %s has external DW.ref.__gxx_personality_v0 reference.\n", __FILE__, __LINE__,
                                  __FUNCTION__, G.getName().c_str()));
        break;
      }
#endif
  }

  void printLinkGraph(llvm::jitlink::LinkGraph& G, llvm::StringRef Title) {
    constexpr llvm::JITTargetAddress LineWidth = 16;
    stringstream ss;
    ss << "--- " << Title.str() << "---\n";
    for (auto& S : G.sections()) {
      ss << "  section: " << S.getName().str() << "\n";
      for (auto* B : S.blocks()) {
        ss << "    block@";
        ss << formatv("{0:x16}", B->getAddress()).str();
        ss << ":\n";
        if (B->isZeroFill())
          continue;
        llvm::JITTargetAddress InitAddr = B->getAddress().getValue() & ~(LineWidth - 1);
        llvm::JITTargetAddress StartAddr = B->getAddress().getValue();
        llvm::JITTargetAddress EndAddr = B->getAddress().getValue() + B->getSize();
        auto* Data = reinterpret_cast<const uint8_t*>(B->getContent().data());
        for (llvm::JITTargetAddress CurAddr = InitAddr; CurAddr != EndAddr; ++CurAddr) {
          if (CurAddr % LineWidth == 0)
            ss << "    " << formatv("{0:x16}", CurAddr).str() << ": ";
          if (CurAddr < StartAddr)
            ss << "   ";
          else
            ss << formatv("{0:x-2}", Data[CurAddr - StartAddr]).str() << " ";
          if (CurAddr % LineWidth == LineWidth - 1)
            ss << "\n";
        }
        if (EndAddr % LineWidth != 0)
          ss << "\n";
        ss << "\n";
      }
    }
    printf("%s\n", ss.str().c_str());
  }
};

}; // namespace llvmo

namespace llvmo {

/*! Call this after fork() to create a thread-pool for lljit
 */
CL_DEFUN void llvm_sys__create_lljit_thread_pool() {
#if 0
  gctools::global_thread_pool = new thread_pool<ThreadManager>(thread_pool<ThreadManager>::sane_number_of_threads());
  ClaspJIT_O* jit = &*gctools::As<ClaspJIT_sp>(_lisp->_Roots._ClaspJIT);
  jit->_LLJIT->getExecutionSession().setDispatchTask([jit](std::unique_ptr<Task> T) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s pushing an LLJIT task\n", __FILE__, __LINE__, __FUNCTION__ ));
      // FIXME: We should be able to use move-capture here, but ThreadPool's
      // AsyncTaskTys are std::functions rather than unique_functions
      // (because MSVC's std::packaged_tasks don't support move-only types).
      // Fix this when all the above gets sorted out.
    global_thread_pool->push_task([UnownedT = T.release()]() mutable {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s A task %p is running in the thread-pool\n", __FILE__, __LINE__, __FUNCTION__, UnownedT ));
      std::unique_ptr<Task> T(UnownedT);
      T->run();
    });
  } );
#endif
}

ClaspJIT_O::ClaspJIT_O(bool loading, JITDylib_O* mainJITDylib) {
  llvm::ExitOnError ExitOnErr;
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Initializing ClaspJIT_O\n", __FILE__, __LINE__, __FUNCTION__));
  auto JTMB = ExitOnErr(JITTargetMachineBuilder::detectHost());
  TargetOptions to;
  to.FunctionSections = false;
  JTMB.setOptions(to);
  JTMB.setCodeModel(CodeModel::Small);
  JTMB.setRelocationModel(Reloc::Model::PIC_);
  auto TPC = ExitOnErr(orc::SelfExecutorProcessControl::Create(std::make_shared<orc::SymbolStringPool>()));
  auto J = ExitOnErr(
      LLJITBuilder()
          .setExecutionSession(std::make_unique<ExecutionSession>(std::move(TPC)))
          .setNumCompileThreads(0) // <<<<<<< In May 2021 a path will open to use multicores for LLJIT.
          .setJITTargetMachineBuilder(std::move(JTMB))
          .setObjectLinkingLayerCreator([this, &ExitOnErr](ExecutionSession& ES, const Triple& TT) {
            auto ObjLinkingLayer = std::make_unique<ObjectLinkingLayer>(ES, std::make_unique<ClaspAllocator>());
            ObjLinkingLayer->addPlugin(
                std::make_unique<EHFrameRegistrationPlugin>(ES, std::make_unique<jitlink::InProcessEHFrameRegistrar>()));
            DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to addPlugin for ClaspPlugin\n", __FILE__, __LINE__, __FUNCTION__));
            ObjLinkingLayer->addPlugin(std::make_unique<ClaspPlugin>());
            // GDB registrar isn't working at the moment
            if (!getenv("CLASP_NO_JIT_GDB")) {
              ObjLinkingLayer->addPlugin(
                  std::make_unique<orc::DebugObjectManagerPlugin>(ES, ExitOnErr(orc::createJITLoaderGDBRegistrar(ES))));
              if (TT.isOSBinFormatMachO()) {
                ObjLinkingLayer->addPlugin(std::make_unique<GDBJITDebugInfoRegistrationPlugin>(
                    llvm::orc::ExecutorAddr::fromPtr(&llvm_orc_registerJITLoaderGDBWrapper)));
              }
            }
            ObjLinkingLayer->setReturnObjectBuffer(ClaspReturnObjectBuffer); // <<< Capture the ObjectBuffer after JITting code
            return ObjLinkingLayer;
          })
          .create());
  this->_LLJIT = std::move(J);
  if (loading) {
    // Fixup the JITDylib_sp object in place
    if (!mainJITDylib) {
      printf("%s:%d:%s the jitdylib @%p is null!\n", __FILE__, __LINE__, __FUNCTION__, &this->_MainJITDylib);
      abort();
    }
    JITDylib& dylib = this->_LLJIT->getMainJITDylib();
    core::SimpleBaseString_sp sname = core::SimpleBaseString_O::make(dylib.getName());
    new (mainJITDylib) JITDylib_O(sname, &dylib);
    this->_MainJITDylib.rawRef_() = (llvmo::JITDylib_O*)gctools::tag_general<llvmo::JITDylib_O*>(mainJITDylib);
  } else {
    if (mainJITDylib) {
      printf("%s:%d:%s the jitdylib @%p must null!\n", __FILE__, __LINE__, __FUNCTION__, &mainJITDylib);
      abort();
    }
    // Create a new JITDylib_sp object
    core::SimpleBaseString_sp sname = core::SimpleBaseString_O::make(this->_LLJIT->getMainJITDylib().getName());
    this->_MainJITDylib = gc::GC<JITDylib_O>::allocate(sname, &this->_LLJIT->getMainJITDylib());
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Created MainJITDylib %p\n", __FILE__, __LINE__, __FUNCTION__, this->_MainJITDylib.raw_()));
    core::Cons_sp pair = core::Cons_O::create(this->_MainJITDylib, nil<core::T_O>());
    _lisp->_Roots._JITDylibs.store(pair);
  }
  this->_LLJIT->getMainJITDylib().addGenerator(
      llvm::cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->_LLJIT->getDataLayout().getGlobalPrefix())));
  llvm_sys__create_lljit_thread_pool();
}

ClaspJIT_O::~ClaspJIT_O() {
  // Remove all the CodeBlocks
#if 0
  _lisp->_Roots._AllCodeBlocks.store(nil<core::T_O>());
  _lisp->_Roots._AllObjectFiles.store(nil<core::T_O>());
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  gctools::gctools__garbage_collect();
  delete global_thread_pool;
#endif
  // printf("%s:%d Shutdown the ClaspJIT\n", __FILE__, __LINE__);
}

bool ClaspJIT_O::do_lookup(JITDylib_sp dylibsp, const std::string& Name, void*& ptr) {
  llvm::ExitOnError ExitOnErr;
  JITDylib& dylib = *dylibsp->wrappedPtr();
  std::string mangledName = Name;
#if defined(_TARGET_OS_DARWIN)
  // gotta put a _ in front of the name on DARWIN but not Unixes? Why? Dunno.
//  mangledName = "_" + Name;
#endif
#if !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD) && !defined(_TARGET_OS_DARWIN)
#error You need to decide here
#endif
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s do_lookup %s in JITDylib_sp %p JITDylib* %p JITLINKDylib* %p\n", __FILE__, __LINE__,
                            __FUNCTION__, mangledName.c_str(), dylibsp.raw_(), &dylib, llvm::cast<JITLinkDylib>(&dylib)));
  auto symbol = this->_LLJIT->lookup(dylib, mangledName);
  if (!symbol) {
    // printf("%s:%d could not find external linkage symbol named: %s\n", __FILE__, __LINE__, mangledName.c_str() );
    // dylib.dump(llvm::errs());
    return false;
  }
  //  printf("%s:%d:%s !!symbol -> %d  symbol->getAddress() -> %p\n", __FILE__, __LINE__, __FUNCTION__, !!symbol,
  //  (void*)symbol->getAddress());
  ptr = (void*)symbol->getValue();
  return true;
}

CL_DEFMETHOD core::Pointer_sp ClaspJIT_O::lookup(JITDylib_sp dylibsp, const std::string& Name) {
  void* ptr;
  bool found = this->do_lookup(dylibsp, Name, ptr);
  if (!found) {
    SIMPLE_ERROR("Could not find pointer for name |{}|", Name);
  }
  return core::Pointer_O::create(ptr);
}

CL_DEFMETHOD core::T_sp ClaspJIT_O::lookup_all_dylibs(const std::string& name) {
  core::T_sp jcur = _lisp->_Roots._JITDylibs.load();
  void* ptr;
  while (jcur.consp()) {
    JITDylib_sp jitdylib = gc::As<JITDylib_sp>(CONS_CAR(jcur));
    bool found = this->do_lookup(jitdylib, name, ptr);
    if (found) {
      clasp_ffi::ForeignData_sp sp_sym = clasp_ffi::ForeignData_O::create(ptr);
      sp_sym->set_kind(kw::_sym_clasp_foreign_data_kind_symbol_pointer);
      return sp_sym;
    }
    jcur = CONS_CDR(jcur);
  }
  return nil<core::T_O>();
}

ObjectFile_sp prepareObjectFileForMaterialization(JITDylib_sp dylib, const std::string& uniqueObjectFileName, size_t objectId) {
  core::SimpleBaseString_sp sbs = core::SimpleBaseString_O::make(uniqueObjectFileName);
  ObjectFile_sp codeObject = gc::GC<ObjectFile_O>::allocate<gctools::RuntimeStage>(sbs, unbound<CodeBlock_O>(), dylib, objectId);
  registerObjectFile<gctools::RuntimeStage>(codeObject);
  return codeObject;
};

CL_DEFMETHOD ObjectFile_sp ClaspJIT_O::addIRModule(JITDylib_sp dylib, Module_sp module, ThreadSafeContext_sp context,
                                                   size_t startupID) {
  //  printf("%s:%d:%s module = %p\n", __FILE__, __LINE__, __FUNCTION__, module.raw_() );
  std::unique_ptr<llvm::Module> umodule(module->wrappedPtr());
  llvm::ExitOnError ExitOnErr;
  std::string prefix;
  std::string futureName = createIRModuleObjectFileName(startupID, prefix);
  module->wrappedPtr()->setModuleIdentifier(prefix);
  ObjectFile_sp codeObject = prepareObjectFileForMaterialization(dylib, futureName, startupID);
  ExitOnErr(
      this->_LLJIT->addIRModule(*dylib->wrappedPtr(), llvm::orc::ThreadSafeModule(std::move(umodule), *context->wrappedPtr())));
  return codeObject;
}

ObjectFile_sp ClaspJIT_O::addObjectFile(JITDylib_sp dylib, std::unique_ptr<llvm::MemoryBuffer> objectFile, bool print,
                                        size_t startupId) {
  // Create an llvm::MemoryBuffer for the ObjectFile bytes
  //  printf("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__ );
  if (print)
    core::clasp_write_string(fmt::format("{}:{} Adding object file at {}  {} bytes\n", __FILE__, __LINE__,
                                         (void*)objectFile->getBufferStart(), objectFile->getBufferSize()));
  // Force the object file to be linked using MaterializationUnit::doMaterialize(...)
  if (print)
    core::clasp_write_string(fmt::format("{}:{} Materializing\n", __FILE__, __LINE__));
  //  llvmo::ObjectFile_sp of = llvmo::ObjectFile_O::createForObjectFile(objectFile->getBufferIdentifier().str(), dylib );
  llvm::ExitOnError ExitOnErr;
  ObjectFile_sp codeObject = prepareObjectFileForMaterialization(dylib, objectFile->getBufferIdentifier().str(), startupId);
  ExitOnErr(this->_LLJIT->addObjectFile(*dylib->wrappedPtr(), std::move(objectFile)));
  return codeObject;
}

/*
 * runLoadtimeCode
 */
void* ClaspJIT_O::runStartupCode(JITDylib_sp dylibsp, const std::string& startupName, core::T_sp initialDataOrUnbound) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to evaluate the LoadtimeCode - with startupName: %s\n", __FILE__, __LINE__,
                            __FUNCTION__, startupName.c_str()));
  void* ptr;
  bool found = this->do_lookup(dylibsp, startupName, ptr);
  if (!found) {
    SIMPLE_ERROR("Could not find function {} - exit program and look at llvm::errs() stream", startupName);
  }
  T_OStartUp startup = reinterpret_cast<T_OStartUp>(ptr);
  //    printf("%s:%d:%s About to invoke startup @p=%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)startup);
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to invoke startup @p=%p initialDataOrUnbound = %s\n", __FILE__, __LINE__, __FUNCTION__,
                            (void*)startup, (initialDataOrUnbound.unboundp() ? "unbound" : _rep_(initialDataOrUnbound).c_str())));
  core::T_O* arg0 = initialDataOrUnbound.unboundp() ? (core::T_O*)NULL : initialDataOrUnbound.raw_();
  core::T_O* replPtrRaw = startup(arg0);
  // If we load a bitcode file generated by clasp - then startup_functions will be waiting - so run them
  if (!initialDataOrUnbound.unboundp()) {
    //    printf("%s:%d:%s There is initialData -> %s\n", __FILE__, __LINE__, __FUNCTION__,
    //    core::_rep_(initialDataOrUnbound).c_str());
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Returned from startup function with %p\n", __FILE__, __LINE__, __FUNCTION__, replPtrRaw));
    // Clear out the current ObjectFile and Code
    // printf("%s:%d:%s I need the name of the object file and then look it up\n", __FILE__, __LINE__, __FUNCTION__ );
    return (void*)replPtrRaw;
  }
  // Running the ObjectFileStartUp function registers the startup functions - now we can invoke them
  if (core::startup_functions_are_waiting()) {
    // This is where we can take the my_thread->_ObjectFile and my_thread->_Code and write it into the FunctionDescription_O objects
    // that are bound to functions.
    void* result = core::startup_functions_invoke(NULL);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s The startup functions were INVOKED\n", __FILE__, __LINE__, __FUNCTION__));
    // Clear out the current ObjectFile and Code
    // printf("%s:%d:%s I need the name of the object file and then look it up\n", __FILE__, __LINE__, __FUNCTION__ );
    return result;
  }
  SIMPLE_ERROR("No startup functions are waiting after runInitializers\n");
}

CL_DEFMETHOD JITDylib_sp ClaspJIT_O::getMainJITDylib() {
  if (this->_MainJITDylib.raw_() && gctools::untag_general<core::T_O*>(this->_MainJITDylib.raw_())) {
    return this->_MainJITDylib;
  }
  SIMPLE_ERROR("The main-jit-dylib of the JIT is not setup properly: raw -> {}\n", (void*)this->_MainJITDylib.raw_());
}

CL_DEFMETHOD JITDylib_sp ClaspJIT_O::createAndRegisterJITDylib(const std::string& name) {
  stringstream sname;
  sname << name << "-" << global_JITDylibCounter;
  //  printf("%s:%d:%s  name -> %s\n", __FILE__, __LINE__, __FUNCTION__, sname.str().c_str());
  auto dy = this->_LLJIT->createJITDylib(sname.str());
  JITDylib& dylib(*dy);
  dylib.addGenerator(
      llvm::cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->_LLJIT->getDataLayout().getGlobalPrefix())));
  core::SimpleBaseString_sp ssname = core::SimpleBaseString_O::make(sname.str());
  JITDylib_sp dylib_sp = gc::GC<JITDylib_O>::allocate(ssname, &dylib);
  dylib_sp->_Id = ++global_JITDylibCounter;
  core::Cons_sp cell = core::Cons_O::create(dylib_sp, nil<core::T_O>());
  core::T_sp expected;
  core::T_sp current;
  // Use CAS to push the new JITDylib into the list of JITDylibs.
  do {
    current = _lisp->_Roots._JITDylibs.load();
    expected = current;
    cell->rplacd(current);
    _lisp->_Roots._JITDylibs.compare_exchange_strong(expected, gc::As_unsafe<core::T_sp>(cell));
  } while (expected != current);
  return dylib_sp;
}

void ClaspReturnObjectBuffer(std::unique_ptr<llvm::MemoryBuffer> buffer) {
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s You now OWN MemoryBuffer %s  buffer->getBufferStart() -> %p  size: %lu\n", __FILE__, __LINE__,
                            __FUNCTION__, buffer->getBufferIdentifier().str().c_str(), buffer->getBufferStart(),
                            buffer->getBufferSize()));
#ifdef DEBUG_OBJECT_FILES
  if (globalDebugObjectFiles == DebugObjectFilesPrintSave) {
    stringstream ss;
    ss << core::core__monitor_directory();
    ss << buffer->getBufferIdentifier().str() << "-" << ++global_object_file_number << ".o";
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Writing object file to: %s\n", __FILE__, __LINE__, __FUNCTION__, ss.str().c_str()));
    ObjectFile_O::writeToFile(ss.str(), buffer->getBufferStart(), buffer->getBufferSize());
  }
#endif
  // Grab the buffer and put it in the current ObjectFile

  ObjectFile_sp code = lookupObjectFile(buffer->getBufferIdentifier().str());
  code->_MemoryBuffer = std::move(buffer);
//  printf("%s:%d:%s loadedObjectFile %p  _MemoryBuffer = %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)&*code, (void*)code->_MemoryBuffer.get() );
  auto objf = code->getObjectFile();
  llvm::object::ObjectFile& of = *objf->get();
#if defined(_TARGET_OS_LINUX)
  uint64_t secId = getModuleSectionIndexForText(of);
  code->_TextSectionId = secId;
#elif defined(_TARGET_OS_DARWIN)
  code->_TextSectionId = 0;
#else
  printf("%s:%d:%s Add support to set _TextSectionID for this os\n", __FILE__, __LINE__, __FUNCTION__);
#endif
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s MemoryBuffer is %p\n", __FILE__, __LINE__, __FUNCTION__, code->_MemoryBuffer.get()));
}

void ClaspJIT_O::registerJITDylibAfterLoad(JITDylib_O* jitDylib) {
  if (jitDylib->_Id >= global_JITDylibCounter.load()) {
    global_JITDylibCounter.store(jitDylib->_Id + 1);
  }
}

#if 0
/void handleObjectEmitted(VModuleKey K, std::unique_ptr<MemoryBuffer> O) {
//  printf("%s:%d:%s Received emitted object buffer obj@%p\n", __FILE__,__LINE__, __FUNCTION__, (void*)O->getBufferStart());
}
#endif

DOCGROUP(clasp);
CL_DEFUN ClaspJIT_sp llvm_sys__make_clasp_jit() {
  printf("%s:%d:%s Creating JIT - we did this already at startup!!!!\n", __FILE__, __LINE__, __FUNCTION__);
  auto cj = gctools::GC<ClaspJIT_O>::allocate(false, (llvmo::JITDylib_O*)NULL);
  return cj;
}

}; // namespace llvmo
