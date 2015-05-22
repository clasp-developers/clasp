/*
    File: gc_interface.cc
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
typedef bool _Bool;
#include <type_traits>
//#include <llvm/Support/system_error.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
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
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Pass.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include "llvm/IR/AssemblyAnnotationWriter.h" // will be llvm/IR
//#include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

#include <clang/Frontend/ASTUnit.h>
#include <clang/AST/Comment.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/DeclFriend.h>
#include <clang/AST/DeclOpenMP.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtCXX.h>
#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>
#include <clang/AST/StmtOpenMP.h>

#include <clang/Basic/Version.h>
#include <clang/Tooling/JSONCompilationDatabase.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/Comment.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Tooling/Refactoring.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Lex/Lexer.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/ASTMatchers/Dynamic/VariantValue.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>

#include <clasp/gctools/symbolTable.h>
#include <clasp/sockets/symbolTable.h>
#include <clasp/serveEvent/symbolTable.h>
#include <clasp/clbind/symbolTable.h>

#include <clasp/gctools/gctoolsPackage.h>

#include <clasp/core/foundation.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/weakKeyMapping.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/funcallableStandardClass.h>
#include <clasp/core/structureClass.h>
//#include "core/symbolVector.h"
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/userData.h>
#include <clasp/core/sexpLoadArchive.h>
#include <clasp/core/sexpSaveArchive.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/specialForm.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/arguments.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/package.h>
#include <clasp/core/character.h>
#include <clasp/core/reader.h>
#include <clasp/core/singleDispatchEffectiveMethodFunction.h>
#include <clasp/core/microHeap.h>
#include <clasp/core/regex.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/forwardReferencedClass.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/stringSet.h>
#include <clasp/core/symbolSet.h>
#include <clasp/core/readtable.h>
#include <clasp/core/arrayObjects.h>
#include <clasp/core/intArray.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/primitives.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/binder.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/objectSet.h>
#include <clasp/core/symbolList.h>
#include <clasp/core/stringList.h>
#include <clasp/core/null.h>
#include <clasp/core/multiStringBuffer.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/pointer.h>
#include <clasp/core/objRef.h>
#include <clasp/core/smallMap.h>
#include <clasp/core/pathname.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/weakHashTable.h>

#include <clasp/clbind/clbind.h>

#include <clasp/cffi/cffi.h>

#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugInfoExpose.h>

#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/asttooling/astVisitor.h>
#include <clasp/asttooling/example.h>
#include <clasp/asttooling/Registry.h>
#include <clasp/asttooling/Diagnostics.h>
#include <clasp/asttooling/Marshallers.h>
#include <clasp/asttooling/testAST.h>

#define NAMESPACE_gctools
#define NAMESPACE_core
#include <clasp/main/gc_interface.h>
#undef NAMESPACE_gctools
#undef NAMESPACE_core

#ifdef USE_MPS

#ifdef DEBUG_MPS
#define MPS_LOG(fm) \
  { printf("%s:%d %s --> %s\n", __FILE__, __LINE__, __FUNCTION__, (fm).str().c_str()); }
#else
#define MPS_LOG(fm)
#endif

extern "C" {
using namespace gctools;

const char *obj_name(gctools::GCKindEnum kind) {
#ifndef RUNNING_GC_BUILDER
#define GC_KIND_NAME_MAP_TABLE
#include <clasp/main/clasp_gc.cc>
#undef GC_KIND_NAME_MAP_TABLE
  goto *(KIND_NAME_MAP_table[kind]);
#define GC_KIND_NAME_MAP
#include <clasp/main/clasp_gc.cc>
#undef GC_KIND_NAME_MAP
#endif
  return "NONE";
}
};

extern "C" {

using namespace gctools;

/*! I'm using a format_header so MPS gives me the object-pointer */
mps_addr_t obj_skip(mps_addr_t client) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SKIP_TABLE
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_SKIP_TABLE
#endif
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  MPS_LOG(BF("obj_skip client = %p   header=%p  header-desc: %s") % client % header % header->description());
  if (header->kindP()) {
    gctools::GCKindEnum kind = header->kind();
#ifndef RUNNING_GC_BUILDER
    goto *(OBJ_SKIP_table[kind]);
#define GC_OBJ_SKIP
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_SKIP
#else
    return NULL;
#endif
  } else if (header->fwdP()) {
    client = (char *)(client) + header->fwdSize();
  } else if (header->pad1P()) {
    client = (char *)(client) + header->pad1Size();
  } else if (header->padP()) {
    client = (char *)(client) + header->padSize();
  } else {
    THROW_HARD_ERROR(BF("Illegal header at %p") % header);
  }
  DEBUG_MPS_MESSAGE(BF("Leaving obj_skip with client@%p") % client);
  return client;
}
};

extern "C" {
/*! I'm using a format_header so MPS gives me the object-pointer */
void obj_dump_base(mps_addr_t base) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_DUMP_MAP_TABLE
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_DUMP_MAP_TABLE
#endif

  mps_bool_t inArena = mps_arena_has_addr(_global_arena, base);
  if (!inArena) {
    printf("Address@%p is not in the arena\n", base);
    return;
  }
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(base);
  void *client = BasePtrToMostDerivedPtr<void>(base);
  MPS_LOG(BF("obj_dump base=%p header-desc: %s") % base % header->description());
  stringstream sout;
  if (header->kindP()) {
    gctools::GCKindEnum kind = header->kind();
#ifndef RUNNING_GC_BUILDER
    goto *(OBJ_DUMP_MAP_table[kind]);
#define GC_OBJ_DUMP_MAP
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_DUMP_MAP
#else
// do nothing
#endif
  } else if (header->fwdP()) {
    void *forwardPointer = header->fwdPointer();
    sout << "FWD pointer[" << forwardPointer << "] size[" << header->fwdSize() << "]";
  } else if (header->pad1P()) {
    sout << "PAD1 size[" << header->pad1Size() << "]";
  } else if (header->padP()) {
    sout << "PAD size[" << header->padSize() << "]";
  } else {
    sout << "INVALID HEADER!!!!!";
  }
BOTTOM:
  printf("Base@%p %s\n", base, sout.str().c_str());
}

int trap_obj_scan = 0;

//core::_sym_STARdebugLoadTimeValuesSTAR && core::_sym_STARdebugLoadTimeValuesSTAR.notnilp()

#ifdef DEBUG_LOAD_TIME_VALUES
#define SHIELD_SAFE_TELEMETRY(CLIENT, FMT)                                                                         \
  if (core::_sym_STARdebugLoadTimeValuesSTAR && core::_sym_STARdebugLoadTimeValuesSTAR->symbolValue().notnilp()) { \
    Seg seg;                                                                                                       \
    if (SegOfAddr(&seg, gctools::_global_arena, CLIENT)) {                                                         \
      ShieldExpose(gctools::_global_arena, seg);                                                                   \
      printf("%s\n", (FMT).str().c_str());                                                                         \
      ShieldCover(gctools::_global_arena, seg);                                                                    \
    }                                                                                                              \
  }
#else
#define SHIELD_SAFE_TELEMETRY(CLIENT, PARGS)
#endif

GC_RESULT obj_scan(mps_ss_t ss, mps_addr_t client, mps_addr_t limit) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN_TABLE
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_SCAN_TABLE
#endif

  DEBUG_MPS_MESSAGE(BF("obj_scan started - Incoming client %p   limit: %p") % client % limit);
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    while (client < limit) {
      gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
      MPS_LOG(BF("obj_skip client = %p   header=%p  header-desc: %s") % client % header % header->description());
      if (header->kindP()) {
        GCKindEnum kind = header->kind();
#ifndef RUNNING_GC_BUILDER
        goto *(OBJ_SCAN_table[kind]);
#define GC_OBJ_SCAN
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_SCAN
#endif
      } else if (header->fwdP()) {
        client = (char *)(client) + header->fwdSize();
      } else if (header->pad1P()) {
        client = (char *)(client) + header->pad1Size();
      } else if (header->padP()) {
        client = (char *)(client) + header->padSize();
      } else {
        THROW_HARD_ERROR(BF("Illegal header at %p") % header);
      }
    TOP:
      continue;
    }
  }
  MPS_SCAN_END(GC_SCAN_STATE);
  return MPS_RES_OK;
}

/*! I'm using a format_header so MPS gives me the object-pointer */
#define GC_FINALIZE_METHOD
void obj_finalize(mps_addr_t client) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_FINALIZE_TABLE
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_FINALIZE_TABLE
#endif

  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  ASSERTF(header->kindP(), BF("obj_finalized called without a valid object"));
  gctools::GCKindEnum kind = (GCKindEnum)(header->kind());
  DEBUG_MPS_MESSAGE(BF("Finalizing client@%p   kind=%s") % client % header->description());
#ifndef RUNNING_GC_BUILDER
  goto *(OBJ_FINALIZE_table[kind]);
#define GC_OBJ_FINALIZE
#include <clasp/main/clasp_gc.cc>
#undef GC_OBJ_FINALIZE
#else
// do nothing
#endif
};
#undef GC_FINALIZE_METHOD

vector<core::LoadTimeValues_O **> globalLoadTimeValuesRoots;

void registerLoadTimeValuesRoot(core::LoadTimeValues_O **ptr) {
  globalLoadTimeValuesRoots.push_back(ptr);
}

mps_res_t main_thread_roots_scan(mps_ss_t ss, void *gc__p, size_t gc__s) {
  DEBUG_MPS_MESSAGE(BF("in main_thread_roots_scan"));
  //	mps_thr_t gc__thr = 0; // This isn't passed in but the scanners need it
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    MPS_LOG(BF("Starting rooted_HeapRoots"));

    //            printf("%s:%d  Fixing globalLoadTimeValuesRoots[%d]\n", __FILE__, __LINE__, globalLoadTimeValuesRoots.size() );
    for (auto &it : globalLoadTimeValuesRoots) {
      POINTER_FIX(*it);
    }
//            printf("---------Done\n");

// Do I need to fix these pointers explicitly???
//gctools::global_Symbol_OP_nil       = symbol_nil.raw_();
//gctools::global_Symbol_OP_unbound   = symbol_unbound.raw_();
//gctools::global_Symbol_OP_deleted   = symbol_deleted.raw_();
//gctools::global_Symbol_OP_sameAsKey = symbol_sameAsKey.raw_();

#ifndef RUNNING_GC_BUILDER
#define GC_GLOBALS
#include <clasp/main/clasp_gc.cc>
#undef GC_GLOBALS
#endif

#ifndef RUNNING_GC_BUILDER
#if USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
#define GC_GLOBAL_SYMBOLS
#include "main/clasp_gc.cc"
#undef GC_GLOBAL_SYMBOLS
#else

//
// Ok, this looks nasty but it allows us to avoid running the static analyzer
// every time we add or remove a symbol.  Every symbol that is scraped from
// the source must be listed here and fixed for the garbage collector
//

#define AstToolingPkg_SYMBOLS
#define CffiPkg_SYMBOLS
#define ClPkg_SYMBOLS
//#define ClangAstPkg_SYMBOLS    // symbols not declared global
#define ClbindPkg_SYMBOLS
#define ClosPkg_SYMBOLS
#define CommonLispUserPkg_SYMBOLS
#define CompPkg_SYMBOLS
#define CorePkg_SYMBOLS
#define ExtPkg_SYMBOLS
#define GcToolsPkg_SYMBOLS
#define GrayPkg_SYMBOLS
#define KeywordPkg_SYMBOLS
#define LlvmoPkg_SYMBOLS
//#define MpiPkg_SYMBOLS
#define ServeEventPkg_SYMBOLS
#define SocketsPkg_SYMBOLS

#define AstToolingPkg asttooling
#define CffiPkg cffi
#define ClPkg cl
//#define ClangAstPkg clang
#define ClbindPkg clbind
#define ClosPkg clos
#define CommonLispUserPkg
#define CompPkg comp
#define CorePkg core
#define ExtPkg ext
#define GcToolsPkg gctools
#define GrayPkg gray
#define KeywordPkg kw
#define LlvmoPkg llvmo
//#define MpiPkg mpi
#define ServeEventPkg serveEvent
#define SocketsPkg sockets

#define DO_SYMBOL(sym, id, pkg, name, exprt) SMART_PTR_FIX(pkg::sym)
#include <clasp/core/generated/symbols_scraped_inc.h>
#include <clasp/asttooling/generated/symbols_scraped_inc.h>
#include <clasp/cffi/generated/symbols_scraped_inc.h>
#include <clasp/clbind/generated/symbols_scraped_inc.h>
#include <clasp/core/generated/symbols_scraped_inc.h>
#include <clasp/gctools/generated/symbols_scraped_inc.h>
#include <clasp/llvmo/generated/symbols_scraped_inc.h>
#include <clasp/mpip/generated/symbols_scraped_inc.h>
#include <clasp/serveEvent/generated/symbols_scraped_inc.h>
#include <clasp/sockets/generated/symbols_scraped_inc.h>

#undef AstToolingPkg
#undef CffiPkg
#undef ClPkg
//#undef ClangAstPkg
#undef ClbindPkg
#undef ClosPkg
#undef CommonLispUserPkg
#undef CompPkg
#undef CorePkg
#undef ExtPkg
#undef GcToolsPkg
#undef GrayPkg
#undef KeywordPkg
#undef LlvmoPkg
//#undef MpiPkg
#undef ServeEventPkg
#undef SocketsPkg

#undef AstToolingPkg_SYMBOLS
#undef CffiPkg_SYMBOLS
#undef ClPkg_SYMBOLS
//#undef ClangAstPkg_SYMBOLS
#undef ClbindPkg_SYMBOLS
#undef ClosPkg_SYMBOLS
#undef CommonLispUserPkg_SYMBOLS
#undef CompPkg_SYMBOLS
#undef CorePkg_SYMBOLS
#undef ExtPkg_SYMBOLS
#undef GcToolsPkg_SYMBOLS
#undef GrayPkg_SYMBOLS
#undef KeywordPkg_SYMBOLS
#undef LlvmoPkg_SYMBOLS
//#undef MpiPkg_SYMBOLS
#undef ServeEventPkg_SYMBOLS
#undef SocketsPkg_SYMBOLS

#endif
#endif // RUNNING_GC_BUILDER

    MPS_LOG(BF("Done roots_scan"));
  }
  MPS_SCAN_END(GC_SCAN_STATE);
  return MPS_RES_OK;
}
};

namespace gctools {
};
//
// We don't want the static analyzer gc-builder.lsp to see the generated scanners
//
#ifndef RUNNING_GC_BUILDER
#define HOUSEKEEPING_SCANNERS
#include <clasp/main/clasp_gc.cc>
#undef HOUSEKEEPING_SCANNERS
#endif

//
// Turn the following on if you want potentially dangerous local variables
// identified by the static analyzer to throw errors in the compilation
// of gc_interface.cc
//
#if 0
#ifndef RUNNING_GC_BUILDER
#define GC_LOCAL_VARIABLES_DANGEROUS_UNROOTED
#include <clasp/main/clasp_gc.cc>
#undef GC_LOCAL_VARIABLES_DANGEROUS_UNROOTED
#endif
#endif

#endif // ifdef USE_MPS
