#define DEBUG_LEVEL_FULL
typedef bool _Bool;
#include <clasp/core/foundation.h>
#include <type_traits>
// #include <llvm/Support/system_error.h>
#include <llvm/ExecutionEngine/GenericValue.h>
// #include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/LinkAllPasses.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#if LLVM_VERSION_MAJOR < 20
#include <llvm/Transforms/Instrumentation.h>
#else
#include <llvm/Transforms/Utils/Instrumentation.h>
#endif
#include <llvm/Transforms/IPO.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/CodeGen/TargetPassConfig.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Pass.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Verifier.h>
#include "llvm/IR/AssemblyAnnotationWriter.h" // will be llvm/IR
// #include <llvm/IR/PrintModulePass.h> // will be llvm/IR  was llvm/Assembly

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

#include <clasp/core/symbolTable.h>

#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/metaClass.h>
#include <clasp/gctools/gcStack.h>
#include <clasp/gctools/containers.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/random.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/bytecode.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/object.h>
#include <clasp/gctools/gcweak.h>
#include <clasp/core/smallMultimap.h>
// #include "core/symbolVector.h"
#include <clasp/core/designators.h>
#include <clasp/core/hashTable.h>
#include <clasp/core/hashTableEq.h>
#include <clasp/core/hashTableEql.h>
#include <clasp/core/hashTableEqual.h>
#include <clasp/core/hashTableEqualp.h>
#include <clasp/core/hashTableCustom.h>

#include <clasp/core/userData.h>
#include <clasp/core/loadTimeValues.h>
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/arguments.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/package.h>
#include <clasp/core/character.h>
// #include <clasp/core/reader.h>
// #include <clasp/core/regex.h>
#include <clasp/core/array.h>
#include <clasp/core/readtable.h>
#include <clasp/core/nativeVector.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/primitives.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/null.h>
#include <clasp/core/pointer.h>
#include <clasp/core/debugger.h> // Frame_O
#include <clasp/core/backtrace.h>
#include <clasp/core/smallMap.h>
#include <clasp/core/pathname.h>
#include <clasp/core/sharpEqualWrapper.h>
#include <clasp/core/unwind.h>
#include <clasp/core/fli.h>
#include <clasp/gctools/gc_boot.h>

#include <clasp/mpip/claspMpi.h>

#include <clasp/clbind/clbind.h>

#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/jit.h>
#include <clasp/llvmo/code.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugInfoExpose.h>

#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/asttooling/asttoolingPackage.h>
#include <clasp/asttooling/example.h>

#include <clasp/asttooling/translators.h>

#ifndef SCRAPING
#include HEADER_INCLUDES_INC_H
#endif

#define NAMESPACE_gctools
#define NAMESPACE_core
#include <clasp/gctools/gc_interface.h>
#undef NAMESPACE_gctools
#undef NAMESPACE_core

/*! Template types with commas in them can't be passed
to macros unless they are wrapped in this.
eg: offsetof(MACRO_SAFE_TYPE(a<b,c>),d)
*/
#define SAFE_TYPE_MACRO(...) __VA_ARGS__

#ifdef _TARGET_OS_DARWIN
// The OS X offsetof macro is defined as __offsetof which is defined as __builtin_offsetof - take out one level of macro so that the
// MACRO_SAFE_TYPE hack works
#undef offsetof
#define offsetof(t, x) __builtin_offsetof(t, x)
#endif

/* ----------------------------------------------------------------------
 *
 *  Expose functions
 *
 */

#define BAD_HEADER(msg, hdr)                                                                                                       \
  printf("%s:%d Illegal header@%p in %s  header->header=%" PRIxPTR "  header->data[0]=%" PRIxPTR "  header->data[1]=%" PRIxPTR     \
         "\n",                                                                                                                     \
         __FILE__, __LINE__, &hdr, msg, hdr.header._value, hdr.additional_data[0], hdr.additional_data[1]);

template <typename RT, typename... ARGS>
NOINLINE void expose_function(const std::string& pkg_sym, RT (*fp)(ARGS...), const std::string& lambdaList) {
  maybe_register_symbol_using_dladdr(*(void**)&fp, sizeof(fp), pkg_sym);
  std::string pkgName;
  std::string symbolName;
  core::colon_split(pkg_sym, pkgName, symbolName);
  //  printf("%s:%d  expose_function   pkgName=%s  symbolName=%s\n", __FILE__, __LINE__, pkgName.c_str(), symbolName.c_str() );
  core::wrap_function(pkgName, symbolName, fp, lambdaList);
}

template <typename RT, typename... ARGS>
NOINLINE void expose_function_setf(const std::string& pkg_sym, RT (*fp)(ARGS...), const std::string& lambdaList) {
  std::string pkgName;
  std::string symbolName;
  core::colon_split(pkg_sym, pkgName, symbolName);
  core::wrap_function_setf(pkgName, symbolName, fp, lambdaList);
}
