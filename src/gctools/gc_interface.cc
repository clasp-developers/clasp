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
#include <clasp/core/foundation.h>
#include <type_traits>
//#include <llvm/Support/system_error.h>
#include <llvm/ExecutionEngine/GenericValue.h>
//#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/LinkAllPasses.h>
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
#include <clasp/core/symbolTable.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/symbolTable.h>
#include <clasp/core/symbolTable.h>

#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/gctools/gcStack.h>
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
#include <clasp/core/designators.h>
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
//#include <clasp/core/singleDispatchEffectiveMethodFunction.h>
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
#include <clasp/core/sharpEqualWrapper.h>
#include <clasp/core/strWithFillPtr.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/gctools/gc_boot.h>

//#include <clasp/core/clc.h>
#include <clasp/core/clcenv.h>

#include <clasp/clbind/clbind.h>

#include <clasp/llvmo/intrinsics.h>
#include <clasp/llvmo/llvmoExpose.h>
#include <clasp/llvmo/debugLoc.h>
#include <clasp/llvmo/insertPoint.h>
#include <clasp/llvmo/debugInfoExpose.h>

#include <clasp/asttooling/astExpose.h>
#include <clasp/asttooling/clangTooling.h>
#include <clasp/asttooling/astVisitor.h>
#include <clasp/asttooling/example.h>
//#include <clasp/asttooling/Registry.h>
//#include <clasp/asttooling/Diagnostics.h>
//#include <clasp/asttooling/Marshallers.h>

#define GC_INTERFACE_INCLUDE
#include PLUGIN_HEADERS_INCLUDE
#undef GC_INTERFACE_INCLUDE

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
// The OS X offsetof macro is defined as __offsetof which is defined as __builtin_offsetof - take out one level of macro so that the MACRO_SAFE_TYPE hack works
#undef offsetof
#define offsetof(t,x) __builtin_offsetof(t,x)
#endif

namespace gctools {

/* This is where the class_layout codes are included
from clasp_gc.cc
They are generated by the layout analyzer. */

Layout_code* get_kind_layout_codes() {
  static Layout_code codes[] = {
#if defined(USE_MPS) || (defined(USE_BOEHM) && !defined(USE_CXX_DYNAMIC_CAST))
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN_HELPERS
#include CLASP_GC_CC
#undef GC_OBJ_SCAN_HELPERS
#endif // #ifndef RUNNING_GC_BUILDER
#endif // #if defined(USE_MPS) || (defined(USE_BOEHM) && !defined(USE_CXX_DYNAMIC_CAST))
      {layout_end, 0, 0, 0, "" }
  };
  return &codes[0];
};
};

/* ----------------------------------------------------------------------
 *
 *  Expose functions
 *
 */

#define BAD_HEADER(msg,hdr) \
    printf("%s:%d Illegal header@%p in %s  header->header=%lX  header->data[0]=%lX  header->data[1]=%lX\n", __FILE__, __LINE__, hdr, msg, hdr->header, hdr->data[0], hdr->data[1]);

template <typename RT, typename...ARGS>
NOINLINE void expose_function(const std::string& pkg_sym,
                     bool exported,
                     RT (*fp)(ARGS...),
                     const std::string& lambdaList)
{
  std::string pkgName;
  std::string symbolName;
  core::colon_split(pkg_sym,pkgName,symbolName);
//  printf("%s:%d  expose_function   pkgName=%s  symbolName=%s\n", __FILE__, __LINE__, pkgName.c_str(), symbolName.c_str() );
  core::wrap_function(pkgName,symbolName,fp,lambdaList);
}

#ifndef SCRAPING
  #define EXPOSE_FUNCTION_SIGNATURES
  #include INIT_FUNCTIONS_INC_H
  #undef EXPOSE_FUNCTION_SIGNATURES
#endif

#ifndef SCRAPING
  #define EXPOSE_FUNCTION_BINDINGS_HELPERS
  #undef EXPOSE_FUNCTION_BINDINGS
  #include INIT_FUNCTIONS_INC_H
  #undef EXPOSE_FUNCTION_BINDINGS_HELPERS
#endif

void initialize_functions()
{
//  printf("%s:%d About to initialize_functions\n", __FILE__, __LINE__ );
#ifndef SCRAPING
  #define EXPOSE_FUNCTION_BINDINGS
  #include INIT_FUNCTIONS_INC_H
  #undef EXPOSE_FUNCTION_BINDINGS
#endif
};

typedef enum { code_kind, method_kind, class_kind, unknown_kind } source_info_kind;
NOINLINE void define_source_info(source_info_kind kind,
                        const string& lisp_name,
                        const string& file, size_t character_offset,
                        size_t line, const string& docstring ) {
  std::string package_part, symbol_part;
  core::colon_split(lisp_name,package_part,symbol_part);
  core::Symbol_sp sym = core::lisp_intern(symbol_part,package_part);
  if ( kind == code_kind ) {
    core::Function_sp func = core::coerce::functionDesignator(sym);
    core::Str_sp sourceFile = core::Str_O::create(file);
    func->setSourcePosInfo(sourceFile, character_offset, line, 0 );
    core::Str_sp docs = core::Str_O::create(docstring);
    ext__annotate(sym,cl::_sym_documentation,cl::_sym_function, docs);
    ext__annotate(func,cl::_sym_documentation,cl::_sym_function, docs);
  } else if ( kind == class_kind ) {
  }
}

#define SOURCE_INFO_HELPERS
#undef SOURCE_INFO
#ifndef SCRAPING
#include SOURCE_INFO_INC_H
#endif
#undef SOURCE_INFO_HELPERS

void initialize_source_info() {
#define SOURCE_INFO
#ifndef SCRAPING
#include SOURCE_INFO_INC_H
#endif
#undef SOURCE_INFO
};




extern "C" {
using namespace gctools;

size_t obj_kind(core::T_O *tagged_ptr) {
  core::T_O *client = untag_object<core::T_O *>(tagged_ptr);
  Header_s *header = reinterpret_cast<Header_s *>(ClientPtrToBasePtr(client));
  return (size_t)(header->kind());
}

const char *obj_kind_name(core::T_O *tagged_ptr) {
  core::T_O *client = untag_object<core::T_O *>(tagged_ptr);
  Header_s *header = reinterpret_cast<Header_s *>(ClientPtrToBasePtr(client));
  return obj_name(header->kind());
}

const char *obj_name(gctools::kind_t kind) {
  if (kind == (gctools::kind_t)KIND_null) {
    return "UNDEFINED";
  }
  if ( kind > KIND_max ) kind = GCKind<core::Instance_O>::Kind;
  size_t kind_index = (size_t)kind;
  ASSERT(kind_index<=global_kind_max);
//  printf("%s:%d obj_name kind= %d  kind_index = %d\n", __FILE__, __LINE__, kind, kind_index);
  return global_kind_info[kind_index].name;
}

/*! I'm using a format_header so MPS gives me the object-pointer */
#define GC_DEALLOCATOR_METHOD
void obj_deallocate_unmanaged_instance(gctools::smart_ptr<core::T_O> obj ) {
  void* client = &*obj;
  printf("%s:%d About to obj_deallocate_unmanaged_instance %s\n", __FILE__, __LINE__, _rep_(obj).c_str() );
  // The client must have a valid header
#ifndef RUNNING_GC_BUILDER
  #ifndef USE_CXX_DYNAMIC_CAST
    #define GC_OBJ_DEALLOCATOR_TABLE
    #include CLASP_GC_CC
    #undef GC_OBJ_DEALLOCATOR_TABLE
  #endif // USE_CXX_DYNAMIC_CAST
#endif

  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  ASSERTF(header->kindP(), BF("obj_deallocate_unmanaged_instance called without a valid object"));
  gctools::GCKindEnum kind = (GCKindEnum)(header->kind());
  GC_TELEMETRY1(telemetry::label_obj_deallocate_unmanaged_instance, (uintptr_t)client);
#ifndef RUNNING_GC_BUILDER
  #ifndef USE_CXX_DYNAMIC_CAST
  size_t jump_table_index = (size_t)kind - kind_first_general;
  goto *(OBJ_DEALLOCATOR_table[jump_table_index]);
    #define GC_OBJ_DEALLOCATOR
    #include CLASP_GC_CC
    #undef GC_OBJ_DEALLOCATOR
  #endif // USE_CXX_DYNAMIC_CASE
#else
// do nothing
#endif
};
#undef GC_DEALLOCATOR_METHOD

};


// ----------------------------------------------------------------------
//
// Declare all global symbols
//
//
#define DECLARE_ALL_SYMBOLS
#ifndef SCRAPING
#include SYMBOLS_SCRAPED_INC_H
#endif
#undef DECLARE_ALL_SYMBOLS



#ifdef USE_MPS
extern "C" {
using namespace gctools;
/*! I'm using a format_header so MPS gives me the object-pointer */
mps_addr_t obj_skip(mps_addr_t client) {
  mps_addr_t oldClient = client;
  size_t size = 0;
// The client must have a valid header
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SKIP_TABLE
#include CLASP_GC_CC
#undef GC_OBJ_SKIP_TABLE
#endif
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
#ifdef DEBUG_VALIDATE_GUARD
  header->validate();
#endif
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  if (header->kindP()) {
    gctools::GCKindEnum kind = header->kind();
    if ( kind > KIND_max ) kind = GCKind<core::Instance_O>::Kind;
    const Kind_layout& kind_layout = global_kind_layout[kind];
#ifndef RUNNING_GC_BUILDER
    if ( kind_layout.layout_op == class_container_op ) {
      size = kind_layout.size;
      if ( kind_layout.container_layout ) {
        Container_layout& container_layout = *kind_layout.container_layout;
        size_t capacity = *(size_t*)((const char*)client + container_layout.capacity_offset);
        size = container_layout.element_size*capacity + container_layout.data_offset;
      }
    } else {
      size = ((core::General_O*)client)->templatedSizeof();
    }
        client = (mps_addr_t)((char*)client + AlignUp(size + sizeof(Header_s))
#ifdef DEBUG_GUARD
                              + header->tail_size
#endif
                              );
#endif // #ifndef RUNNING_GC_BUILDER
  } else if (header->fwdP()) {
    client = (char *)(client) + header->fwdSize();
  } else if (header->pad1P()) {
    client = (char *)(client) + header->pad1Size();
  } else if (header->padP()) {
    client = (char *)(client) + header->padSize();
  } else {
    TELEMETRY_FLUSH();
    BAD_HEADER("obj_skip",header);
    abort();
  }
 FINISH:
  GC_TELEMETRY3(telemetry::label_obj_skip,
                (uintptr_t)oldClient,
                (uintptr_t)client,
                (uintptr_t)((char *)client - (char *)oldClient));
  return client;
}
};
#endif // ifdef USE_MPS

#ifdef USE_MPS
int trap_obj_scan = 0;
#endif // #ifdef USE_MPS


#ifdef USE_MPS
extern "C" {
GC_RESULT obj_scan(mps_ss_t ss, mps_addr_t client, mps_addr_t limit) {
  GC_TELEMETRY2(telemetry::label_obj_scan_start,
                (uintptr_t)client,
                (uintptr_t)limit);
  mps_addr_t original_client;
  size_t size = 0;  // Used to store the size of the object
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN_TABLE
#include CLASP_GC_CC
#undef GC_OBJ_SCAN_TABLE
#endif
  GCKindEnum kind;
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    while (client < limit) {
      // The client must have a valid header
      DEBUG_THROW_IF_INVALID_CLIENT(client);
      gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
#ifdef DEBUG_VALIDATE_GUARD
      header->validate();
#endif
      original_client = (mps_addr_t)client;
      if (header->kindP()) {
        kind = header->kind();
        if ( kind > KIND_max ) kind = GCKind<core::Instance_O>::Kind;
        const Kind_layout& kind_layout = global_kind_layout[kind];
#ifndef RUNNING_GC_BUILDER
        size = kind_layout.size;
        if ( kind_layout.field_layout_start ) {
          int num_fields = kind_layout.number_of_fields;
          Field_layout* field_layout_cur = kind_layout.field_layout_start;
          for ( int i=0; i<num_fields; ++i ) {
            core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
            POINTER_FIX(field);
            ++field_layout_cur;
          }
        }
        if ( kind_layout.container_layout ) {
          Container_layout& container_layout = *kind_layout.container_layout;
          size_t capacity = *(size_t*)((const char*)client + container_layout.capacity_offset);
          size = container_layout.element_size*capacity + container_layout.data_offset;
          size_t end = *(size_t*)((const char*)client + container_layout.end_offset);
          for ( int i=0; i<end; ++i ) {
            Field_layout* field_layout_cur = container_layout.field_layout_start;
            ASSERT(field_layout_cur);
            const char* element = ((const char*)client + container_layout.data_offset + container_layout.element_size*i);
            for ( int j=0; j<container_layout.number_of_fields; ++j ) {
              core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
              POINTER_FIX(field);
              ++field_layout_cur;
            }
          }
        }
        if (kind_layout.layout_op != class_container_op) {
          size = ((core::General_O*)client)->templatedSizeof();
        }
        client = (mps_addr_t)((char*)client + AlignUp(size + sizeof(Header_s))
#ifdef DEBUG_GUARD
                              + header->tail_size
#endif
                              );
          // Here add tail
#endif // #ifndef RUNNING_GC_BUILDER
      } else if (header->fwdP()) {
        client = (char *)(client) + header->fwdSize();
      } else if (header->pad1P()) {
        client = (char *)(client) + header->pad1Size();
      } else if (header->padP()) {
        client = (char *)(client) + header->padSize();
      } else {
        BAD_HEADER("obj_scan",header);
        abort();
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
};
#endif // ifdef USE_MPS


extern "C" {
/*!
 * client_validate_internal
 *
 * Validate this client and the clients that it points to.
 */
#if 0
void client_validate_internal(void* tagged_client) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_VALIDATE_TABLE
#include CLASP_GC_CC
#undef GC_OBJ_VALIDATE_TABLE
#endif
  if (!gctools::tagged_objectp(tagged_client)) return;
  GCKindEnum kind;
      // The client must have a valid header
  void* client = gctools::untag_object(tagged_client);
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
#ifdef DEBUG_VALIDATE_GUARD
  header->validate();
#endif
  if (header->kindP()) {
    kind = header->kind();
    const Kind_layout& kind_layout = global_kind_layout[kind];
    if (kind_layout.layout_operation == class_operation) {
#ifndef RUNNING_GC_BUILDER
      const Class_layout& class_layout = kind_layout.class_;
      int num_fields = class_layout.number_of_fields;
      Field_layout* field_layout_cur = class_layout.field_layout_start;
      for ( int i=0; i<num_fields; ++i ) {
        core::T_O* child_client = *(core::T_O**)((const char*)client + field_layout_cur->field_offset);
        client_validate(child_client);
        ++field_layout_cur;
      }
#endif RUNNING_GC_BUILDER
    } else {
          // Container or templated_class - special case - use jump table
      size_t jump_table_index = global_kind_layout[kind].jump.jump_table_index;
#ifndef RUNNING_GC_BUILDER
      goto *(OBJ_VALIDATE_table[jump_table_index]);
#define SMART_PTR_VALIDATE(x) client_validate((x).rawRef_())
#define TAGGED_POINTER_VALIDATE(x) client_validate((x).rawRef_())
#define GC_OBJ_VALIDATE
#include CLASP_GC_CC
#undef GC_OBJ_VALIDATE
#undef SMART_PTR_VALIDATE
#undef TAGGED_PTR_VALIDATE
    VALIDATE_ADVANCE:
#endif // #ifndef RUNNING_GC_BUILDER
    }
  }
};
#endif // #if 0

/*!
 * client_validate_recursive
 *
 * Recursively walk the tagged pointers within this client and validate them.
 * Keep track of which tagged pointers have been seen using the _seen_ set.
 */
#if 0
void client_validate_recursive(void* tagged_client, std::set<void*>& seen) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_VALIDATE_TABLE
#include CLASP_GC_CC
#undef GC_OBJ_VALIDATE_TABLE
#endif
  if ( !gctools::tagged_objectp(tagged_client) ) return;
  GCKindEnum kind;
      // The client must have a valid header
  core::T_O* client = gctools::untag_object(tagged_client);
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
#ifdef DEBUG_VALIDATE_GUARD
  header->validate();
#endif
  if (header->kindP()) {
    kind = header->kind();
    const Kind_layout& kind_layout = global_kind_layout[kind];
    if (kind_layout.layout_operation == class_operation) {
#ifndef RUNNING_GC_BUILDER
      const Class_layout& class_layout = kind_layout.class_;
      int num_fields = class_layout.number_of_fields;
      Field_layout* field_layout_cur = class_layout.field_layout_start;
      for ( int i=0; i<num_fields; ++i ) {
        core::T_O* child_client = *(core::T_O**)((const char*)client + field_layout_cur->field_offset);
        if ( !seen.count(child_client) ) {
          client_validate_recursive(child_client,seen);
          seen.insert(child_client);
        }
        ++field_layout_cur;
      }
#endif RUNNING_GC_BUILDER
    } else {
          // Container or templated_class - special case - use jump table
      size_t jump_table_index = global_kind_layout[kind].jump.jump_table_index;
#ifndef RUNNING_GC_BUILDER
      goto *(OBJ_VALIDATE_table[jump_table_index]);
#define SMART_PTR_VALIDATE(x) {if (!seen.count(x.rawRef_())) { seen.insert((x).rawRef_()); client_validate_recursive((x).rawRef_(),seen);}};
#define TAGGED_POINTER_VALIDATE(x) {if (!seen.count(x.rawRef_())) { seen.insert((x).rawRef_()); client_validate_recursive((x).rawRef_(),seen);}};
#define GC_OBJ_VALIDATE
#include CLASP_GC_CC
#undef GC_OBJ_VALIDATE
#undef SMART_PTR_VALIDATE
#undef TAGGED_PTR_VALIDATE
    VALIDATE_ADVANCE:
#endif // #ifndef RUNNING_GC_BUILDER
    }
  }
};
#endif // #if 0
};




#ifdef USE_MPS
extern "C" {
/*! I'm using a format_header so MPS gives me the object-pointer */
  #define GC_FINALIZE_METHOD
void obj_finalize(mps_addr_t client) {
  // The client must have a valid header
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  #ifndef RUNNING_GC_BUILDER
    #define GC_OBJ_FINALIZE_TABLE
    #include CLASP_GC_CC
    #undef GC_OBJ_FINALIZE_TABLE
  #endif // ifndef RUNNING_GC_BUILDER
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(client));
  ASSERTF(header->kindP(), BF("obj_finalized called without a valid object"));
  gctools::GCKindEnum kind = (GCKindEnum)(header->kind());
  GC_TELEMETRY1(telemetry::label_obj_finalize,
                (uintptr_t)client);
  #ifndef RUNNING_GC_BUILDER
  size_t table_index = (size_t)kind - kind_first_general;
  goto *(OBJ_FINALIZE_table[table_index]);
    #define GC_OBJ_FINALIZE
    #include CLASP_GC_CC
    #undef GC_OBJ_FINALIZE
  #endif // ifndef RUNNING_GC_BUILDER
}; // obj_finalize
}; // extern "C"
  #undef GC_FINALIZE_METHOD
#endif // ifdef USE_MPS


#ifdef USE_MPS
extern "C" {
vector<core::LoadTimeValues_O **> globalLoadTimeValuesRoots;

void registerLoadTimeValuesRoot(core::LoadTimeValues_O **ptr) {
  globalLoadTimeValuesRoots.push_back(ptr);
}
};
#endif // ifdef USE_MPS

#ifdef USE_MPS
extern "C" {
mps_res_t main_thread_roots_scan(mps_ss_t ss, void *gc__p, size_t gc__s) {
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    GC_TELEMETRY0(telemetry::label_root_scan_start);
    for (auto &it : globalLoadTimeValuesRoots) {
      POINTER_FIX(it);
    }
#ifndef RUNNING_GC_BUILDER
#define GC_GLOBALS
#include CLASP_GC_CC
#undef GC_GLOBALS
#endif

#ifdef USE_SYMBOLS_IN_GLOBAL_ARRAY
    for ( int i=0; i<global_symbol_count; ++i ) {
      SMART_PTR_FIX(global_symbols[i]);
    }
#else
#if USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
  #ifndef RUNNING_GC_BUILDER
  #define GC_GLOBAL_SYMBOLS
  #include CLASP_GC_CC
  #undef GC_GLOBAL_SYMBOLS
  #endif // if RUNNING_GC_BUILDER
#else
//
// Ok, this looks nasty but it allows us to avoid running the static analyzer
// every time we add or remove a symbol.  Every symbol that is scraped from
// the source must be listed here and fixed for the garbage collector
//
  #define GARBAGE_COLLECT_ALL_SYMBOLS
  #ifndef RUNNING_GC_BUILDER
    #ifndef SCRAPING
        #include SYMBOLS_SCRAPED_INC_H
    #endif
  #endif // ifndef RUNNING_GC_BUILDER
  #undef GARBAGE_COLLECT_ALL_SYMBOLS
#endif // else ifndef USE_STATIC_ANALYZER_GLOBAL_SYMBOLS
#endif
  }
  MPS_SCAN_END(GC_SCAN_STATE);
  GC_TELEMETRY0(telemetry::label_root_scan_stop);
  return MPS_RES_OK;
}
};
#endif // USE_MPS

//
// We don't want the static analyzer gc-builder.lsp to see the generated scanners
//
#ifdef USE_MPS
  #ifndef RUNNING_GC_BUILDER
    #ifndef SCRAPING
      #define HOUSEKEEPING_SCANNERS
      #include CLASP_GC_CC
      #undef HOUSEKEEPING_SCANNERS
    #endif // ifdef USE_MPS
  #endif // ifndef RUNNING_GC_BUILDER
#endif // ifdef USE_MPS

//
// Bootstrapping
//

void setup_bootstrap_packages(core::BootStrapCoreSymbolMap* bootStrapSymbolMap)
{
  #define BOOTSTRAP_PACKAGES
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
  #undef BOOTSTRAP_PACKAGES
}

template <class TheClass>
void set_one_static_class_symbol(core::BootStrapCoreSymbolMap* symbols, const std::string& full_name )
{
  std::string orig_package_part, orig_symbol_part;
  core::colon_split( full_name, orig_package_part, orig_symbol_part);
  std::string package_part, symbol_part;
  package_part = core::lispify_symbol_name(orig_package_part);
  symbol_part = core::lispify_symbol_name(orig_symbol_part);
//  printf("%s:%d set_one_static_class_symbol --> %s:%s\n", __FILE__, __LINE__, package_part.c_str(), symbol_part.c_str() );
  core::SymbolStorage store;
  bool found =  symbols->lookupSymbol(package_part,symbol_part, store );
  if ( !found ) {
    printf("%s:%d ERROR!!!! The static class symbol %s was not found orig_symbol_part=|%s| symbol_part=|%s|!\n", __FILE__, __LINE__, full_name.c_str(), orig_symbol_part.c_str(), symbol_part.c_str() );
    abort();
  }
  TheClass::set_static_class_symbol(store._Symbol);
}

void set_static_class_symbols(core::BootStrapCoreSymbolMap* bootStrapSymbolMap)
{
#define SET_CLASS_SYMBOLS
#ifndef SCRAPING
#include INIT_CLASSES_INC_H
#endif
#undef SET_CLASS_SYMBOLS
}

#define ALLOCATE_ALL_SYMBOLS_HELPERS
#undef ALLOCATE_ALL_SYMBOLS
#ifndef SCRAPING
#include SYMBOLS_SCRAPED_INC_H
#endif
#undef ALLOCATE_ALL_SYMBOLS_HELPERS

void allocate_symbols(core::BootStrapCoreSymbolMap* symbols)
{
#define ALLOCATE_ALL_SYMBOLS
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
#undef ALLOCATE_ALL_SYMBOLS
};

template <class TheClass, class Metaclass>
NOINLINE  gc::smart_ptr<Metaclass> allocate_one_class()
{
  gc::smart_ptr<Metaclass> class_val = Metaclass::createUncollectable();
  class_val->__setup_stage1_with_sharedPtr_lisp_sid(class_val,_lisp,TheClass::static_classSymbol());
  reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<TheClass>::id,TheClass::static_classSymbol());
  TheClass::static_class = class_val;
  TheClass::static_Kind = gctools::GCKind<TheClass>::Kind;
  core::core__setf_find_class(class_val,TheClass::static_classSymbol(),true,_Nil<core::T_O>());
  gctools::smart_ptr<core::LispObjectCreator<TheClass>> cb = gctools::GC<core::LispObjectCreator<TheClass>>::allocate();
  TheClass::set_static_creator(cb);
  class_val->setCreator(TheClass::static_creator);
  return class_val;
}

template <class TheMetaClass>
struct TempClass {
  static gctools::smart_ptr<TheMetaClass> holder;
};



void create_packages()
{
  #define CREATE_ALL_PACKAGES
  #ifndef SCRAPING
    #include SYMBOLS_SCRAPED_INC_H
  #endif
  #undef CREATE_ALL_PACKAGES
}


void define_base_classes()
{
  IMPLEMENT_MEF(BF("define_base_classes"));
}


void calculate_class_precedence_lists()
{
  IMPLEMENT_MEF(BF("calculate_class_precendence_lists"));
}

// ----------------------------------------------------------------------
//
// Expose classes and methods
//
// Code generated by scraper
//
//
#include <clasp/core/wrappers.h>
#include <clasp/core/external_wrappers.h>

#define EXPOSE_STATIC_CLASS_VARIABLES
#ifndef SCRAPING
  #include INIT_CLASSES_INC_H
#endif
#undef EXPOSE_STATIC_CLASS_VARIABLES

#define EXPOSE_METHODS
#ifndef SCRAPING
  #include INIT_CLASSES_INC_H
#endif
#undef EXPOSE_METHODS

void initialize_enums()
{
  #define ALL_ENUMS
  #ifndef SCRAPING
    #include <generated/enum_inc.h>
  #endif
  #undef ALL_ENUMS
};


void initialize_classes_and_methods()
{
#define EXPOSE_CLASSES_AND_METHODS
#ifndef SCRAPING
  #include INIT_CLASSES_INC_H
#endif
#undef EXPOSE_CLASSES_AND_METHODS
}

#if 0
#define MPS_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, x);
#else
#define MPS_LOG(x)
#endif

void initialize_clasp()
{
  // The bootStrapCoreSymbolMap keeps track of packages and symbols while they
  // are half-way initialized.
  MPS_LOG("initialize_clasp");
  core::BootStrapCoreSymbolMap bootStrapCoreSymbolMap;
  setup_bootstrap_packages(&bootStrapCoreSymbolMap);

  MPS_LOG("initialize_clasp allocate_symbols");
  allocate_symbols(&bootStrapCoreSymbolMap);
  
  MPS_LOG("initialize_clasp set_static_class_symbols");
  set_static_class_symbols(&bootStrapCoreSymbolMap);

  MPS_LOG("initialize_clasp ALLOCATE_ALL_CLASSES");
  #define ALLOCATE_ALL_CLASSES
  #ifndef SCRAPING
    #include INIT_CLASSES_INC_H
  #endif
  #undef ALLOCATE_ALL_CLASSES
  
  create_packages();

  bootStrapCoreSymbolMap.finish_setup_of_symbols();

  // Define base classes
  #define SET_BASES_ALL_CLASSES
  #ifndef SCRAPING
    #include INIT_CLASSES_INC_H
  #endif
  #undef SET_BASES_ALL_CLASSES

    // Define base classes
  #define CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES
  #ifndef SCRAPING
    #include INIT_CLASSES_INC_H
  #endif
  #undef CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES

  reg::lisp_registerClassSymbol<core::Character_I>(cl::_sym_character);
  reg::lisp_registerClassSymbol<core::Fixnum_I>(cl::_sym_fixnum);
  reg::lisp_registerClassSymbol<core::SingleFloat_I>(cl::_sym_single_float);

  initialize_enums();
  
// Moved to lisp.cc
//  initialize_functions();
  // initialize methods???
//  initialize_source_info();
};

extern "C" {

#ifndef SCRAPING
#include C_WRAPPERS
#endif
};

