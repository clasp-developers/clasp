/*
    File: jit.cc

*/

#define DEBUG_LEVEL_FULL

//#include <llvm/Support/system_error.h>
#include <dlfcn.h>
#include <iomanip>
#include <string>
#include <clasp/core/foundation.h>
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
//#include <llvm/ExecutionEngine/Orc/MachOPlatform.h>
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
#include <llvm/ADT/Triple.h>
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
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

#include <clasp/core/fli.h>
#include <clasp/core/compiler.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/evaluator.h>
#include <clasp/llvmo/jit.h>


namespace llvmo {

  using namespace llvm;
  using namespace llvm::jitlink;
  
class ClaspSnapshotAllocator final : public JITLinkMemoryManager {
public:
  ClaspSnapshotAllocator() {
  }
public:
  static Expected<std::unique_ptr<ClaspSnapshotAllocator>>
  Create() {
    Error Err = Error::success();
    std::unique_ptr<ClaspSnapshotAllocator> Allocator(
        new ClaspSnapshotAllocator());
    return std::move(Allocator);
  }

  void allocate(const JITLinkDylib *JD, LinkGraph &G, OnAllocatedFunction OnAllocated )
  {
    using AllocationMap = DenseMap<unsigned, sys::MemoryBlock>;
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s ClaspSnapshotAllocator allocate entered\n", __FILE__, __LINE__, __FUNCTION__  ));

        // Start duplicating InProcessMemoryManager

    BasicLayout BL(G);
    printf("%s:%d:%s I have BasicLayout - what do I do with it?\n", __FILE__, __LINE__, __FUNCTION__ );
    UNREACHABLE();
#if 0
    // Local class for allocation.
    class IPMMAlloc : public Allocation {
    public:
    IPMMAlloc(ClaspSnapshotAllocator &Parent, AllocationMap SegBlocks)
      : Parent(Parent), SegBlocks(std::move(SegBlocks)) {}
      MutableArrayRef<char> getWorkingMemory(ProtectionFlags Seg) override {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Seg = 0x%x base = %p  size = %lu\n", __FILE__, __LINE__, __FUNCTION__, Seg, static_cast<char *>(SegBlocks[Seg].base()), SegBlocks[Seg].allocatedSize() ));
        assert(SegBlocks.count(Seg) && "No allocation for segment");
        return {static_cast<char *>(SegBlocks[Seg].base()), SegBlocks[Seg].allocatedSize()};
      }
      JITTargetAddress getTargetMemory(ProtectionFlags Seg) override {
        assert(SegBlocks.count(Seg) && "No allocation for segment");
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Seg = 0x%x Returning %p\n", __FILE__, __LINE__, __FUNCTION__, Seg, (void*)pointerToJITTargetAddress(SegBlocks[Seg].base()))); 
        return pointerToJITTargetAddress(SegBlocks[Seg].base());
      }
      void finalizeAsync(FinalizeContinuation OnFinalize) override {
        OnFinalize(applyProtections());
      }
      Error deallocate() override {
        for (auto &KV : SegBlocks)
          if (auto EC = sys::Memory::releaseMappedMemory(KV.second))
            return errorCodeToError(EC);
        return Error::success();
      }

    private:
      Error applyProtections() {
        for (auto &KV : SegBlocks) {
          auto &Prot = KV.first;
          auto &Block = KV.second;
          if (Prot & sys::Memory::MF_EXEC)
            if (auto EC = sys::Memory::protectMappedMemory(Block,sys::Memory::MF_RWE_MASK))
              return errorCodeToError(EC);
            sys::Memory::InvalidateInstructionCache(Block.base(),
                                                    Block.allocatedSize());
        }
        return Error::success();
      }

      ClaspSnapshotAllocator &Parent;
      AllocationMap SegBlocks;
    };

    // Scan the request and calculate the group and total sizes.
    // Check that segment size is no larger than page
    

    AllocationMap Blocks;

    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s  Interating Request\n", __FILE__, __LINE__, __FUNCTION__ ));
    size_t totalSize = 0;
    size_t scanSize = 0;
    for (auto &KV : Request) {
      auto &Seg = KV.second;
      uint64_t ZeroFillStart = Seg.getContentSize();
      uint64_t SegmentSize = gctools::AlignUp((ZeroFillStart+Seg.getZeroFillSize()),Seg.getAlignment());
//      printf("%s:%d:%s    allocation KV.first = 0x%x Seg info align/ContentSize/ZeroFillSize = %llu/%lu/%llu  \n", __FILE__, __LINE__, __FUNCTION__, KV.first, (unsigned long long)Seg.getAlignment(), Seg.getContentSize(), (unsigned long long)Seg.getZeroFillSize());
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s    allocation KV.first = 0x%x Seg info align/ContentSize/ZeroFillSize = %llu/%lu/%llu  \n", __FILE__, __LINE__, __FUNCTION__, KV.first, (unsigned long long)Seg.getAlignment(), Seg.getContentSize(), (unsigned long long)Seg.getZeroFillSize()));
      // Add Seg.getAlignment() just in case we need a bit more space to make alignment.
      if ((llvm::sys::Memory::MF_RWE_MASK & KV.first) == ( llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_WRITE )) {
        // We have to scan the entire RW data region (sigh) for pointers
        scanSize = SegmentSize;
      }
      totalSize += gctools::AlignUp(Seg.getContentSize()+Seg.getZeroFillSize(),Seg.getAlignment())+Seg.getAlignment();
    }
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s allocation scanSize = %lu  totalSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, scanSize, totalSize));
    bool allocatingCodeObject = (Request.size()>1);
    Code_sp codeObject(unbound<Code_O>());
    if (allocatingCodeObject) { // It's the allocation that creates a Code object
    // Associate the Code object with the current ObjectFile
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to register ObjectFile/Code objects\n", __FILE__, __LINE__, __FUNCTION__ ));
#if 0      
      ObjectFile_sp of = gc::As_unsafe<ObjectFile_sp>(my_thread->topObjectFile());
#endif
      size_t capacity = totalSize;
      size_t realSize = gctools::sizeof_container_with_header<Code_O>(capacity);
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Code object realSize = %lu scanSize = %lu totalSize = %lu\n", __FILE__, __LINE__, __FUNCTION__, realSize, scanSize, totalSize ));
      
      codeObject = Code_O::makeInSnapshotLoad( scanSize, totalSize );
      // Use CAS to push the new Code_sp into the list of AllSnapshotLoadCode
      core::Cons_sp cell = core::Cons_O::createInSnapshotLoad(codeObject,nil<core::T_O>());
      core::T_sp current;
      core::T_sp expected;
      do {
        current = _lisp->_Roots._AllSnapshotLoadCodes.load();
        expected = current;
        cell->rplacd(current);
        _lisp->_Roots._AllSnapshotLoadCodes.compare_exchange_strong(expected,gc::As_unsafe<core::T_sp>(cell));
      } while (expected != current);
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s is associated with Code_sp at %p\n", __FILE__, __LINE__, __FUNCTION__, codeObject.raw_()));
    }
    for (auto &KV : Request) {
      auto &Seg = KV.second;
      uint64_t ZeroFillStart = Seg.getContentSize();
      size_t SegmentSize = (uintptr_t)gctools::AlignUp(ZeroFillStart+Seg.getZeroFillSize(),Seg.getAlignment());
      void* base;
      if (allocatingCodeObject) {
        if ((llvm::sys::Memory::MF_RWE_MASK & KV.first) == ( llvm::sys::Memory::MF_READ | llvm::sys::Memory::MF_WRITE )) {
          base = codeObject->allocateHead(SegmentSize,Seg.getAlignment());
          DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s allocating Prot 0x%x from the head base = %p\n", __FILE__, __LINE__, __FUNCTION__, KV.first, base ));
        } else {
        base = codeObject->allocateTail(SegmentSize,Seg.getAlignment());
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s allocating Prot 0x%x from the tail base = %p\n", __FILE__, __LINE__, __FUNCTION__, KV.first, base ));
        }
      } else {
        base = aligned_alloc(Seg.getAlignment(),SegmentSize);
      }
      sys::MemoryBlock SegMem(base,SegmentSize);
        // Zero out the zero-fill memory
      memset(static_cast<char*>(SegMem.base())+ZeroFillStart, 0,
             Seg.getZeroFillSize());
        // Record the block for this segment
      Blocks[KV.first] = std::move(SegMem);
    }
    return std::unique_ptr<InProcessMemoryManager::Allocation>(new IPMMAlloc(*this, std::move(Blocks)));
#endif
  }

  
  void  deallocate(std::vector<FinalizedAlloc> Allocs,
             OnDeallocatedFunction OnDeallocated) {
    printf("%s:%d:%s What do I do here?", __FILE__, __LINE__, __FUNCTION__ );
#if 0    
  std::vector<sys::MemoryBlock> StandardSegmentsList;
  std::vector<std::vector<AllocActionCall>> DeallocActionsList;

  {
    std::lock_guard<std::mutex> Lock(FinalizedAllocsMutex);
    for (auto &Alloc : Allocs) {
      auto *FA =
          jitTargetAddressToPointer<FinalizedAllocInfo *>(Alloc.release());
      StandardSegmentsList.push_back(std::move(FA->StandardSegments));
      if (!FA->DeallocActions.empty())
        DeallocActionsList.push_back(std::move(FA->DeallocActions));
      FA->~FinalizedAllocInfo();
      FinalizedAllocInfos.Deallocate(FA);
    }
  }
#endif
};


};

};

namespace llvmo {

class ClaspSnapshotPlugin : public llvm::orc::ObjectLinkingLayer::Plugin {
  void modifyPassConfig(llvm::orc::MaterializationResponsibility &MR,
                        llvm::jitlink::LinkGraph &G,
                        llvm::jitlink::PassConfiguration &Config) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s ClaspSnapshotPlugin modifyPassConfig\n", __FILE__, __LINE__, __FUNCTION__ ));
    auto PersonalitySymbol =
      MR.getTargetJITDylib().getExecutionSession().intern("DW.ref.__gxx_personality_v0");
    if (!MR.getSymbols().count(PersonalitySymbol))
      Config.PrePrunePasses.insert( Config.PrePrunePasses.begin(),
                                  [this](jitlink::LinkGraph&G) -> Error {
                                    for (auto ssym : G.defined_symbols()) {
                                      if (ssym->getName() == "DW.ref.__gxx_personality_v0") {
                                        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePass found DW.ref.__gxx_personality_v0 setting Strong Linkage and Local scope\n", __FILE__, __LINE__, __FUNCTION__ ));
                                        ssym->setLinkage(Linkage::Strong);
                                        ssym->setScope(Scope::Local);
                                        break;
                                      }
                                    }
                                    return Error::success();
                                  });
    Config.PrePrunePasses.push_back(
                                    [this](jitlink::LinkGraph &G) -> Error {
                                      size_t count = 0;
                                      for (auto &Sec : G.sections()) {
                                        if (Sec.getName() == EH_FRAME_NAME )
                                          for (auto *S : Sec.symbols()) {
                                            S->setLive(true);
                                            count++;
                                          }
                                      }
                                      for (auto ssym : G.defined_symbols()) {
                                        std::string sname = ssym->getName().str();
                                        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePass Symbol: %s\n", __FILE__, __LINE__, __FUNCTION__, ssym->getName().str().c_str()));
                                        bool keptAlive = false;
                                        if ( sname.find(gcroots_in_module_name) != std::string::npos ) {
                                          keptAlive = true;
                                        } else if ( sname.find(literals_name) != std::string::npos ) {
                                          keptAlive = true;
#if 1
                                          // I'd like to do this on linux because jit symbols need to be exposed but it slows down startup enormously
                                        } else if ( sname.find("^^") != std::string::npos ) {
                                          // Keep alive mangled symbols that we care about
//                                          keptAlive = true;
#endif
                                        } 
                                        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s preprune symbol: %s  alive: %d\n", __FILE__, __LINE__, __FUNCTION__, sname.c_str(), keptAlive ));
                                        if (keptAlive) ssym->setLive(true);
                                      }
                                      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePass setLive %lu symbols\n", __FILE__, __LINE__, __FUNCTION__, count ));
                                      return Error::success();
                                    });
    Config.PrePrunePasses.push_back([this](jitlink::LinkGraph &G) -> Error {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePasses\n", __FILE__, __LINE__, __FUNCTION__));
      keepAliveStackmap(G);
                                      //printLinkGraph(G, "PrePrune:");
      return Error::success();
    });
    Config.PostFixupPasses.push_back([this](jitlink::LinkGraph &G) -> Error {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PostFixupPasses\n", __FILE__, __LINE__, __FUNCTION__));
      parseLinkGraph(G);
                                       // printLinkGraph(G, "PostFixup:");
      return Error::success();
    });
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__));
  }

  void notifyLoaded(llvm::orc::MaterializationResponsibility& MR) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ ));
  }

  llvm::Error  notifyFailed(llvm::orc::MaterializationResponsibility& MR) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ ));
    return Error::success();
  }

  llvm::Error notifyRemovingResources(ResourceKey K) {
    printf("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__ );
    return Error::success();
  }

  void notifyTransferringResources(ResourceKey DstKey, ResourceKey SrcKey) {
    printf("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__ );
  }


  
  void keepAliveStackmap(llvm::jitlink::LinkGraph &G) {
    for (auto &S : G.sections()) {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   section: %s getOrdinal->%u\n", __FILE__, __LINE__, __FUNCTION__, S.getName().str().c_str(), S.getOrdinal()));
      if (S.getName().str() == STACKMAPS_NAME) {
        for ( auto& sym : S.symbols() ) {
          sym->setLive(true);
        }
      }
    }
  }
  
  void parseLinkGraph(llvm::jitlink::LinkGraph &G) {
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Entered  LinkGraph name: %s\n", __FILE__, __LINE__, __FUNCTION__, G.getName().c_str() ));
    uintptr_t textStart = ~0;
    uintptr_t textEnd = 0;
    bool gotGcroots = false;
    ObjectFile_sp currentCode = lookupObjectFile(G.getName());
    for (auto &S : G.sections()) {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s  section: %s getOrdinal->%u \n", __FILE__, __LINE__, __FUNCTION__, S.getName().str().c_str(), S.getOrdinal()));
      std::string sectionName = S.getName().str();
      auto Prot = toSysMemoryProtectionFlags(S.getMemProt());
      if ( (sectionName.find(BSS_NAME)!=string::npos) ||
	   (sectionName.find(DATA_NAME)!=string::npos) ) {
        llvm::jitlink::SectionRange range(S);
        for ( auto& sym : S.symbols() ) {
          std::string name = sym->getName().str();
          void* address = (void*)sym->getAddress();
          size_t size = sym->getSize();
          DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s     section: %s symbol:  %s at %p size: %lu\n", __FILE__, __LINE__, __FUNCTION__, S.getName().str().c_str(), name.c_str(), address, size));
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
      else if ( Prot & llvm::sys::Memory::MF_EXEC ) {
	// Text section
        llvm::jitlink::SectionRange range(S);
	if ((uintptr_t)range.getStart() < textStart) textStart = (uintptr_t)range.getStart();
	uintptr_t tend = (uintptr_t)range.getStart()+range.getSize();
	if ( textEnd < tend ) textEnd = tend;
        currentCode->_TextSectionStart = (void*)range.getStart();
        currentCode->_TextSectionEnd = (void*)((char*)range.getStart()+range.getSize());
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s --- TextSectionStart - TextSectionEnd = %p - %p\n", __FILE__, __LINE__, __FUNCTION__, currentCode->_TextSectionStart, currentCode->_TextSectionEnd ));
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
        for ( auto& sym : S.symbols() ) {
          if (sym->isCallable()&&sym->hasName()) {
            std::string name = sym->getName().str();
            void* address = (void*)sym->getAddress();
            size_t size = sym->getSize();
#ifdef DEBUG_JIT_LOG_SYMBOLS
            core::Cons_sp symbol_info = core::Cons_O::createList(core::make_fixnum((Fixnum)size),core::Pointer_O::create((void*)address));
            if ((!comp::_sym_jit_register_symbol.unboundp()) && comp::_sym_jit_register_symbol->fboundp()) {
              DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Registering jit symbol %s address: %p  size: %lu\n", __FILE__, __LINE__, __FUNCTION__, name.c_str(), (void*)address, size ));
              core::eval::funcall(comp::_sym_jit_register_symbol,core::SimpleBaseString_O::make(name),symbol_info);
            }
#endif
          }
        }
      } else if (sectionName.find(EH_FRAME_NAME)!=string::npos) {
        llvm::jitlink::SectionRange range(S);
        void* start = (void*)range.getStart();
        uintptr_t size = range.getSize();
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   eh_frame section segment_start = %p  segment_size = %lu\n", __FILE__, __LINE__, __FUNCTION__, start, size ));
      } else if (sectionName.find(STACKMAPS_NAME)!=string::npos) {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s   Saving stackmaps range in thread local storage\n", __FILE__, __LINE__, __FUNCTION__ ));
        llvm::jitlink::SectionRange range(S);
        currentCode->_StackmapStart = (void*)range.getStart();
        currentCode->_StackmapSize = (size_t)range.getSize();
      }
    }
    // Keep track of the executable region
    if (textStart) {
      printf("%s:%d:%s  We set this above - do we need to do it again???  textStart %p - textStop %p\n", __FILE__, __LINE__, __FUNCTION__, (void*)textStart, (void*)textEnd );
      currentCode->_TextSectionStart = (void*)textStart;
      currentCode->_TextSectionEnd = (void*)textEnd;
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s --- Final TextSectionStart - TextSectionEnd = %p - %p\n", __FILE__, __LINE__, __FUNCTION__, currentCode->_TextSectionStart, currentCode->_TextSectionEnd ));
      if (snapshotSaveLoad::global_debugSnapshot) {
	printf("%s:%d:%s ---------- ObjectFile_sp %p start %p  end %p\n",
	       __FILE__, __LINE__, __FUNCTION__,
	       currentCode.raw_(),
	       currentCode->_TextSectionStart,
	       currentCode->_TextSectionEnd );
      }
    } else {
      printf("%s:%d:%s No executable region was found for the Code_O object\n", __FILE__, __LINE__, __FUNCTION__ );
    }
    //
    size_t gcroots_in_module_name_len = gcroots_in_module_name.size();
    size_t literals_name_len = literals_name.size();
    bool found_gcroots_in_module = false;
    bool found_literals = false;
    for (auto ssym : G.defined_symbols()) {
      if (ssym->getName() == "DW.ref.__gxx_personality_v0") {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s PrePrunePass found DW.ref.__gxx_personality_v0 setting Strong Linkage and Local scope\n", __FILE__, __LINE__, __FUNCTION__ ));
        ssym->setLinkage(Linkage::Strong);
        ssym->setScope(Scope::Local);
        break;
      }
    }
    for (auto ssym : G.defined_symbols()) {
      DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s defined_symbol -> hasName: %d name: %s at %p size: %lu\n",
                                __FILE__, __LINE__, __FUNCTION__,
                                ssym->hasName(),
                                ssym->getName().str().c_str(),
                                (void*)ssym->getAddress(),
                                (size_t)ssym->getSize()));
      if (ssym->hasName()) {
        std::string sname = ssym->getName().str();
        size_t pos = sname.find(gcroots_in_module_name);
        if (pos!=std::string::npos) {
          found_gcroots_in_module = true;
          void* address = (void*)ssym->getAddress();
          size_t size = ssym->getSize();
          DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Symbol-info %s %p %lu\n", __FILE__, __LINE__, __FUNCTION__,
                                    gcroots_in_module_name.c_str(),
                                    address, size ));
          currentCode->_gcroots = (gctools::GCRootsInModule*)address;
          continue;
        }
        pos = sname.find(literals_name);
        if (pos != std::string::npos) {
          found_literals = true;
          currentCode->_LiteralVectorStart = (uintptr_t)ssym->getAddress();
          size_t symbolSize = (size_t)ssym->getSize();
          if (symbolSize==1) {
            // A symbol of size 1 is really zero
            symbolSize = 0;
          } else if ((symbolSize&7) != 0) {
            printf("%s:%d:%s The symbol %s is %lu bytes in size but it must be a multiple of 8 bytes!!!\n",
                   __FILE__, __LINE__, __FUNCTION__, sname.c_str(), symbolSize );
            abort();
          }
          currentCode->_LiteralVectorSizeBytes = symbolSize;
        }
      }        
    }
    if (!found_literals) {
      printf("%s:%d Did NOT FIND %s\n", __FILE__, __LINE__, literals_name.c_str() );
      abort();
    }
    if (!found_gcroots_in_module) {
      printf("%s:%d Did NOT FIND %s\n", __FILE__, __LINE__, gcroots_in_module_name.c_str() );
      abort();
    }
    //
    // Write the address of literals vector into the gcroots structure
    // Later when we run the code we will check that the gcroots structure
    //  has the correct values
    //
    currentCode->_gcroots->_module_memory = (void*)currentCode->_LiteralVectorStart;
    currentCode->_gcroots->_num_entries = currentCode->_LiteralVectorSizeBytes/sizeof(void*);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s currentCode->_gcroots @%p literals %p num: %lu\n", __FILE__, __LINE__, __FUNCTION__,
			      (gctools::GCRootsInModule*)currentCode->_gcroots,
			      currentCode->_gcroots->_module_memory,
                              currentCode->_gcroots->_num_entries ));

#ifdef DEBUG_OBJECT_FILES
    for (auto *Sym : G.external_symbols())
      if (Sym->getName() == "DW.ref.__gxx_personality_v0") {
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Graph %s has external DW.ref.__gxx_personality_v0 reference.\n", __FILE__, __LINE__, __FUNCTION__, G.getName().c_str()));
        break;
      }
#endif
  }
  
  void printLinkGraph(llvm::jitlink::LinkGraph &G, llvm::StringRef Title) {
    constexpr llvm::JITTargetAddress LineWidth = 16;
    stringstream ss;
    ss << "--- " << Title.str() << "---\n";
    for (auto &S : G.sections()) {
      ss << "  section: " << S.getName().str() << "\n";
      for (auto *B : S.blocks()) {
        ss << "    block@";
        ss << formatv("{0:x16}", B->getAddress()).str();
        ss << ":\n";
        if (B->isZeroFill())
          continue;
        llvm::JITTargetAddress InitAddr = B->getAddress() & ~(LineWidth - 1);
        llvm::JITTargetAddress StartAddr = B->getAddress();
        llvm::JITTargetAddress EndAddr = B->getAddress() + B->getSize();
        auto *Data = reinterpret_cast<const uint8_t *>(B->getContent().data());
        for (llvm::JITTargetAddress CurAddr = InitAddr; CurAddr != EndAddr;
             ++CurAddr) {
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

};


namespace llvmo {

ClaspLinkerJIT_O::ClaspLinkerJIT_O(bool loading, JITDylib_O* mainJITDylib) {
        llvm::ExitOnError ExitOnErr;
        DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Initializing ClaspLinkerJIT_O\n", __FILE__, __LINE__, __FUNCTION__));
        auto JTMB = ExitOnErr(JITTargetMachineBuilder::detectHost());
        JTMB.setCodeModel(CodeModel::Small);
        JTMB.setRelocationModel(Reloc::Model::PIC_);
        this->_TPC = ExitOnErr(orc::SelfExecutorProcessControl::Create(std::make_shared<orc::SymbolStringPool>()));
        auto J = ExitOnErr(
                           LLJITBuilder()
                           .setExecutionSession(std::make_unique<ExecutionSession>(std::move(this->_TPC)))
                           .setNumCompileThreads(0)  // <<<<<<< In May 2021 a path will open to use multicores for LLJIT.
                           .setJITTargetMachineBuilder(std::move(JTMB))
//                           .setPlatformSetUp(orc::setUpMachOPlatform)
                           .setObjectLinkingLayerCreator([this,&ExitOnErr](ExecutionSession &ES, const Triple &TT) {
                       auto ObjLinkingLayer = std::make_unique<ObjectLinkingLayer>(ES, std::make_unique<ClaspSnapshotAllocator>());
                       ObjLinkingLayer->addPlugin(std::make_unique<EHFrameRegistrationPlugin>(ES,std::make_unique<jitlink::InProcessEHFrameRegistrar>()));
                       DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to addPlugin for ClaspPlugin\n", __FILE__, __LINE__, __FUNCTION__ ));
                       ObjLinkingLayer->addPlugin(std::make_unique<ClaspSnapshotPlugin>());
                       // GDB registrar isn't working at the moment
                       if (!getenv("CLASP_NO_JIT_GDB")) {
//			 printf("%s:%d:%s CLASP_NO_JIT_GDB not defined Adding ObjLinkingLayer plugin for orc::createJITLoaderGDBRegistrar\n", __FILE__, __LINE__, __FUNCTION__ );
                         ObjLinkingLayer->addPlugin(std::make_unique<orc::DebugObjectManagerPlugin>(ES,ExitOnErr(orc::createJITLoaderGDBRegistrar(ES))));
                       }
                       ObjLinkingLayer->setReturnObjectBuffer(ClaspReturnObjectBuffer); // <<< Capture the ObjectBuffer after JITting code
                       return ObjLinkingLayer;
                     })
                     .create());
  this->_LLJIT =  std::move(J);
  
#if 0
  if (loading) {
    // Fixup the JITDylib_sp object in place
    JITDylib& dylib = this->_LLJIT->getMainJITDylib();
    dylib.addGenerator(llvm::cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->_LLJIT->getDataLayout().getGlobalPrefix())));
    if (!mainJITDylib) {
      printf("%s:%d:%s the jitdylib @%p is null!\n", __FILE__, __LINE__, __FUNCTION__, &this->_MainJITDylib );
      abort();
    }
    mainJITDylib->set_wrapped( &dylib );
    this->_MainJITDylib.rawRef_() = (llvmo::JITDylib_O*)gctools::tag_general<llvmo::JITDylib_O*>(mainJITDylib);
  } else {
    // Create a new JITDylib_sp object
    this->_MainJITDylib = core::RP_Create_wrapped<JITDylib_O>(&this->_LLJIT->getMainJITDylib());
  }
//  printf("%s:%d Creating ClaspLinkerJIT_O  globalPrefix = %c\n", __FILE__, __LINE__, this->_LLJIT->getDataLayout().getGlobalPrefix());
  this->_LLJIT->getMainJITDylib().addGenerator(llvm::cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->_LLJIT->getDataLayout().getGlobalPrefix())));
#endif
}


ClaspLinkerJIT_O::~ClaspLinkerJIT_O()
{
  printf("%s:%d Shutdown the ClaspJIT\n", __FILE__, __LINE__);
}

bool ClaspLinkerJIT_O::do_lookup(JITDylib& dylib, const std::string& Name, void*& ptr) {
  llvm::ExitOnError ExitOnErr;
  std::string mangledName = Name;
#if defined(_TARGET_OS_DARWIN)
  // gotta put a _ in front of the name on DARWIN but not Unixes? Why? Dunno.
//  mangledName = "_" + Name;
#endif
#if !defined(_TARGET_OS_LINUX) && !defined(_TARGET_OS_FREEBSD) && !defined(_TARGET_OS_DARWIN)
#error You need to decide here
#endif
  llvm::Expected<llvm::JITEvaluatedSymbol> symbol = this->_LLJIT->lookup(dylib,mangledName);
  if (!symbol) {
    // printf("%s:%d could not find external linkage symbol named: %s\n", __FILE__, __LINE__, mangledName.c_str() );
    // dylib.dump(llvm::errs());
    return false;
  }
//  printf("%s:%d:%s !!symbol -> %d  symbol->getAddress() -> %p\n", __FILE__, __LINE__, __FUNCTION__, !!symbol, (void*)symbol->getAddress());
  ptr = (void*)symbol->getAddress();
  return true;
}

core::Pointer_sp ClaspLinkerJIT_O::lookup(JITDylib& dylib, const std::string& Name) {
     void* ptr;
     bool found = this->do_lookup(dylib,Name,ptr);
     if (!found) {
       SIMPLE_ERROR(BF("Could not find pointer for name |%s|") % Name );
     }
     return core::Pointer_O::create(ptr);
 }


core::T_sp ClaspLinkerJIT_O::lookup_all_dylibs(const std::string& name) {
    core::T_sp jcur = _lisp->_Roots._JITDylibs.load();
    void* ptr;
    while (jcur.consp()) {
      JITDylib_sp jitdylib = gc::As<JITDylib_sp>(CONS_CAR(jcur));
      JITDylib& jd = *jitdylib->wrappedPtr();
        bool found = this->do_lookup(jd,name,ptr);
        if (found) {
            clasp_ffi::ForeignData_sp sp_sym = clasp_ffi::ForeignData_O::create(ptr);
            sp_sym->set_kind( kw::_sym_clasp_foreign_data_kind_symbol_pointer );
            return sp_sym;
        }
        jcur = CONS_CDR(jcur);
    }
    return nil<core::T_O>();
}
            
        
void ClaspLinkerJIT_O::addObjectFile(llvm::orc::JITDylib& jitdylib, std::unique_ptr<llvm::MemoryBuffer> objectFile, bool print)
{
  // Create an llvm::MemoryBuffer for the ObjectFile bytes
  //  printf("%s:%d:%s \n", __FILE__, __LINE__, __FUNCTION__ );
  if (print) core::write_bf_stream(BF("%s:%d Adding object file at %p  %lu bytes\n")  % __FILE__ % __LINE__  % (void*)objectFile->getBufferStart() % objectFile->getBufferSize() );
  // Force the object file to be linked using MaterializationUnit::doMaterialize(...)
  if (print) core::write_bf_stream(BF("%s:%d Materializing\n") % __FILE__ % __LINE__ );
  llvm::ExitOnError ExitOnErr;
//  my_thread->pushObjectFile(of);
  ExitOnErr(this->_LLJIT->addObjectFile(jitdylib, std::move(objectFile)));
}

/*
 * runLoadtimeCode 
 */
void* ClaspLinkerJIT_O::runStartupCode(JITDylib& dylib, const std::string& startupName, core::T_sp initialDataOrUnbound, ObjectFile_sp& codeObject )
{
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to evaluate the LoadtimeCode - with startupName: %s\n", __FILE__, __LINE__, __FUNCTION__, startupName.c_str() ));
  void* ptr;
  bool found = this->do_lookup(dylib,startupName,ptr);
  if (!found) {
    SIMPLE_ERROR(BF("Could not find function %s - exit program and look at llvm::errs() stream") % startupName.c_str() );
  }
  T_OStartUp startup = reinterpret_cast<T_OStartUp>(ptr);
//    printf("%s:%d:%s About to invoke startup @p=%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)startup);
  DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s About to invoke startup @p=%p\n", __FILE__, __LINE__, __FUNCTION__, (void*)startup));
  core::T_O* arg0 = initialDataOrUnbound.unboundp() ? (core::T_O*)NULL : initialDataOrUnbound.raw_();
  core::T_O* replPtrRaw = startup(arg0);
  // If we load a bitcode file generated by clasp - then startup_functions will be waiting - so run them
  if (!initialDataOrUnbound.unboundp()) {
    printf("%s:%d:%s What do I do with the codeObject There is initialData -> %s\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(initialDataOrUnbound).c_str());
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s Returned from startup function with %p\n", __FILE__, __LINE__, __FUNCTION__, replPtrRaw ));
    // Clear out the current ObjectFile and Code
#if 0
    codeObject->_gcroots = my_thread->_GCRootsInModule;
    my_thread->popObjectFile();
#endif
    return (void*)replPtrRaw;
  }
  // Running the ObjectFileStartUp function registers the startup functions - now we can invoke them
  if (core::startup_functions_are_waiting()) {
    // This is where we can take the my_thread->_ObjectFile and my_thread->_Code and write it into the FunctionDescription_O objects that are bound to functions.
    void* result = core::startup_functions_invoke(NULL);
    DEBUG_OBJECT_FILES_PRINT(("%s:%d:%s The startup functions were INVOKED\n", __FILE__, __LINE__, __FUNCTION__ ));
    // Clear out the current ObjectFile and Code
    printf("%s:%d:%s What do I do with the codeObject There is initialData -> %s\n", __FILE__, __LINE__, __FUNCTION__, core::_rep_(initialDataOrUnbound).c_str() );
#if 0
    codeObject= my_thread->topObjectFile()->_Code;
    codeObject->_gcroots = my_thread->_GCRootsInModule;
    my_thread->popObjectFile();
#endif
    return result;
  }
  SIMPLE_ERROR(BF("No startup functions are waiting after runInitializers\n"));
}

void ClaspLinkerJIT_O::registerJITDylibAfterLoad(JITDylib_O* jitDylib ) {
  core::SimpleBaseString_sp name = jitDylib->_name;
  auto dy = this->_LLJIT->createJITDylib(name->get_std_string());
  JITDylib& dylib(*dy);
  if ( jitDylib->_Id >= global_JITDylibCounter.load() ) {
    global_JITDylibCounter.store(jitDylib->_Id+1);
  }
  dylib.addGenerator(llvm::cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->_LLJIT->getDataLayout().getGlobalPrefix())));
  jitDylib->set_wrapped( &dylib );
}


CL_DEFMETHOD JITDylib_sp ClaspLinkerJIT_O::createAndRegisterJITDylib(const std::string& name) {
  stringstream sname;
  sname << name << "-" << global_JITDylibCounter;
//  printf("%s:%d:%s  name -> %s\n", __FILE__, __LINE__, __FUNCTION__, sname.str().c_str());
  auto dy = this->_LLJIT->createJITDylib(sname.str());
  JITDylib& dylib(*dy);
  dylib.addGenerator(llvm::cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(this->_LLJIT->getDataLayout().getGlobalPrefix())));
  JITDylib_sp dylib_sp = core::RP_Create_wrapped<JITDylib_O>(&dylib);
  dylib_sp->_name = core::SimpleBaseString_O::make(sname.str());
  dylib_sp->_Id = ++global_JITDylibCounter;
  core::Cons_sp cell = core::Cons_O::create(dylib_sp,nil<core::T_O>());
  core::T_sp expected;
  core::T_sp current;
  // Use CAS to push the new JITDylib into the list of JITDylibs.
  do {
    current = _lisp->_Roots._JITDylibs.load();
    expected = current;
    cell->rplacd(current);
    _lisp->_Roots._JITDylibs.compare_exchange_strong(expected,gc::As_unsafe<core::T_sp>(cell));
  } while (expected != current);
  return dylib_sp;
}



};
namespace llvm {
namespace orc {
Error enableObjCRegistration(const char *PathToLibObjC);
}
};

namespace llvmo {


};


