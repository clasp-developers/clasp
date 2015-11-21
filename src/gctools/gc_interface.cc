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

#include <clasp/gctools/telemetry.h>
#include <clasp/gctools/symbolTable.h>
#include <clasp/sockets/symbolTable.h>
#include <clasp/serveEvent/symbolTable.h>
#include <clasp/clbind/symbolTable.h>

#include <clasp/gctools/gctoolsPackage.h>

#include <clasp/core/foundation.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/cxxClass.h>
#include <clasp/core/random.h>
#include <clasp/core/weakKeyMapping.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/bitVector.h>
#include <clasp/core/smallMultimap.h>
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
#include <clasp/core/regex.h>
#include <clasp/core/structureObject.h>
#include <clasp/core/forwardReferencedClass.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/readtable.h>
#include <clasp/core/arrayObjects.h>
#include <clasp/core/arrayDisplaced.h>
#include <clasp/core/intArray.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/primitives.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/binder.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/vectorDisplaced.h>
#include <clasp/core/null.h>
#include <clasp/core/multiStringBuffer.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/pointer.h>
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

#define GC_INTERFACE_INCLUDE
#include PROJECT_HEADERS_INCLUDE
#undef GC_INTERFACE_INCLUDE

#define NAMESPACE_gctools
#define NAMESPACE_core
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_gctools
#undef NAMESPACE_core

extern "C" {
using namespace gctools;

size_t obj_kind(core::T_O *tagged_ptr) {
  core::T_O *client = untag_object<core::T_O *>(tagged_ptr);
  Header_s *header = reinterpret_cast<Header_s *>(ClientPtrToBasePtr(client));
  return (size_t)(header->kind());
}

char *obj_kind_name(core::T_O *tagged_ptr) {
  core::T_O *client = untag_object<core::T_O *>(tagged_ptr);
  Header_s *header = reinterpret_cast<Header_s *>(ClientPtrToBasePtr(client));
  return obj_name(header->kind());
}

char *obj_name(gctools::GCKindEnum kind) {
  if (kind == KIND_null) {
    return "UNDEFINED";
  }
#ifdef USE_BOEHM
#ifndef USE_CXX_DYNAMIC_CAST
#define GC_KIND_NAME_MAP_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_KIND_NAME_MAP_TABLE
  goto *(KIND_NAME_MAP_table[kind]);
#define GC_KIND_NAME_MAP
#include STATIC_ANALYZER_PRODUCT
#undef GC_KIND_NAME_MAP
#endif
#endif
#ifdef USE_MPS
#ifndef RUNNING_GC_BUILDER
#define GC_KIND_NAME_MAP_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_KIND_NAME_MAP_TABLE
  goto *(KIND_NAME_MAP_table[kind]);
#define GC_KIND_NAME_MAP
#include STATIC_ANALYZER_PRODUCT
#undef GC_KIND_NAME_MAP
#endif
#endif
  return "NONE";
}

/*! I'm using a format_header so MPS gives me the object-pointer */
#define GC_DEALLOCATOR_METHOD
void obj_deallocate_unmanaged_instance(gctools::smart_ptr<core::T_O> obj ) {
  void* client = &*obj;
  printf("%s:%d in obj_deallocate_unmanaged_instance %s\n", __FILE__, __LINE__, _rep_(obj).c_str() );
#if 0
  // The client must have a valid header
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_DEALLOCATOR_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_DEALLOCATOR_TABLE
#endif

  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  ASSERTF(header->kindP(), BF("obj_deallocate_unmanaged_instance called without a valid object"));
  gctools::GCKindEnum kind = (GCKindEnum)(header->kind());
  GC_TELEMETRY1(telemetry::label_obj_deallocate_unmanaged_instance, (uintptr_t)client);
#ifndef RUNNING_GC_BUILDER
  goto *(OBJ_DEALLOCATOR_table[kind]);
#define GC_OBJ_DEALLOCATOR
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_DEALLOCATOR
#else
// do nothing
#endif
#endif
};
#undef GC_DEALLOCATOR_METHOD

};

extern "C" {
/*! I'm using a format_header so MPS gives me the object-pointer */
void obj_dump_base(void *base) {
#ifdef USE_BOEHM
#ifdef USE_CXX_DYNAMIC_CAST
// Do nothing
#else
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_DUMP_MAP_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_DUMP_MAP_TABLE
#endif
#endif
#endif
#ifdef USE_MPS
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_DUMP_MAP_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_DUMP_MAP_TABLE
#endif
#endif

#ifdef USE_MPS
  mps_bool_t inArena = mps_arena_has_addr(_global_arena, base);
  if (!inArena) {
    printf("Address@%p is not in the arena\n", base);
    return;
  }
#endif
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(base);
  void *client = BasePtrToMostDerivedPtr<void>(base);
  stringstream sout;
  if (header->kindP()) {
    gctools::GCKindEnum kind = header->kind();
#ifdef USE_BOEHM
#ifndef USE_CXX_DYNAMIC_CAST
    goto *(OBJ_DUMP_MAP_table[kind]);
#define GC_OBJ_DUMP_MAP
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_DUMP_MAP
#else
    sout << "BOEHMDC_UNKNOWN"; // do nothing
#endif
#endif
#ifdef USE_MPS
#ifndef RUNNING_GC_BUILDER
    goto *(OBJ_DUMP_MAP_table[kind]);
#define GC_OBJ_DUMP_MAP
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_DUMP_MAP
#else
// do nothing
#endif
#endif
#ifdef USE_MPS
  } else if (header->fwdP()) {
    void *forwardPointer = header->fwdPointer();
    sout << "FWD pointer[" << forwardPointer << "] size[" << header->fwdSize() << "]";
  } else if (header->pad1P()) {
    sout << "PAD1 size[" << header->pad1Size() << "]";
  } else if (header->padP()) {
    sout << "PAD size[" << header->padSize() << "]";
  } else {
    sout << "INVALID HEADER!!!!!";
#endif
  }
BOTTOM:
  printf("%s:%d obj_dump_base: %s\n", __FILE__, __LINE__, sout.str().c_str());
}
};

#ifdef USE_MPS
extern "C" {

using namespace gctools;

/*! I'm using a format_header so MPS gives me the object-pointer */
mps_addr_t obj_skip(mps_addr_t client) {
  mps_addr_t oldClient = client;
// The client must have a valid header
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SKIP_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_SKIP_TABLE
#endif
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  if (header->kindP()) {
    gctools::GCKindEnum kind = header->kind();
#ifndef RUNNING_GC_BUILDER
    goto *(OBJ_SKIP_table[kind]);
#define GC_OBJ_SKIP
#include STATIC_ANALYZER_PRODUCT
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
DONE:
  GC_TELEMETRY3(telemetry::label_obj_skip,
                (uintptr_t)oldClient,
                (uintptr_t)client,
                (uintptr_t)((char *)client - (char *)oldClient));
  return client;
}
};

int trap_obj_scan = 0;

//core::_sym_STARdebugLoadTimeValuesSTAR && core::_sym_STARdebugLoadTimeValuesSTAR.notnilp()

namespace gctools {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN_HELPERS
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_SCAN_HELPERS
#endif
};

extern "C" {
GC_RESULT obj_scan(mps_ss_t ss, mps_addr_t client, mps_addr_t limit) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_SCAN_TABLE
#endif

  GC_TELEMETRY2(telemetry::label_obj_scan_start,
                (uintptr_t)client,
                (uintptr_t)limit);
  mps_addr_t original_client;
  GCKindEnum kind;
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    while (client < limit) {
      // The client must have a valid header
      DEBUG_THROW_IF_INVALID_CLIENT(client);
      gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
      original_client = (mps_addr_t)client;
      if (header->kindP()) {
        kind = header->kind();
#ifndef RUNNING_GC_BUILDER
        goto *(OBJ_SCAN_table[kind]);
#define GC_OBJ_SCAN
#include STATIC_ANALYZER_PRODUCT
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
      GC_TELEMETRY3(telemetry::label_obj_scan,
                    (uintptr_t)original_client,
                    (uintptr_t)client,
                    (uintptr_t)kind);
      continue;
    }
  }
  MPS_SCAN_END(GC_SCAN_STATE);
  return MPS_RES_OK;
}

/*! I'm using a format_header so MPS gives me the object-pointer */
#define GC_FINALIZE_METHOD
void obj_finalize(mps_addr_t client) {
  // The client must have a valid header
  DEBUG_THROW_IF_INVALID_CLIENT(client);
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_FINALIZE_TABLE
#include STATIC_ANALYZER_PRODUCT
#undef GC_OBJ_FINALIZE_TABLE
#endif

  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  ASSERTF(header->kindP(), BF("obj_finalized called without a valid object"));
  gctools::GCKindEnum kind = (GCKindEnum)(header->kind());
  GC_TELEMETRY1(telemetry::label_obj_finalize,
                (uintptr_t)client);
#ifndef RUNNING_GC_BUILDER
  goto *(OBJ_FINALIZE_table[kind]);
#define GC_OBJ_FINALIZE
#include STATIC_ANALYZER_PRODUCT
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
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    GC_TELEMETRY0(telemetry::label_root_scan_start);
    for (auto &it : globalLoadTimeValuesRoots) {
      SIMPLE_POINTER_FIX(*it);
    }
//            printf("---------Done\n");

// Do I need to fix these pointers explicitly???
//gctools::global_Symbol_OP_nil       = symbol_nil.raw_();
//gctools::global_Symbol_OP_unbound   = symbol_unbound.raw_();
//gctools::global_Symbol_OP_deleted   = symbol_deleted.raw_();
//gctools::global_Symbol_OP_sameAsKey = symbol_sameAsKey.raw_();

#ifndef RUNNING_GC_BUILDER
#define GC_GLOBALS
#include STATIC_ANALYZER_PRODUCT
#undef GC_GLOBALS
#endif

#ifndef RUNNING_GC_BUILDER
#if USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
#define GC_GLOBAL_SYMBOLS
#include STATIC_ANALYZER_PRODUCT
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
#include SYMBOLS_SCRAPED_INC_H

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
  }
  MPS_SCAN_END(GC_SCAN_STATE);
  GC_TELEMETRY0(telemetry::label_root_scan_stop);
  return MPS_RES_OK;
}
};

//
// We don't want the static analyzer gc-builder.lsp to see the generated scanners
//
#ifndef RUNNING_GC_gBUILDER
#define HOUSEKEEPING_SCANNERS
#include STATIC_ANALYZER_PRODUCT
#undef HOUSEKEEPING_SCANNERS
#endif

#endif // ifdef USE_MPS
