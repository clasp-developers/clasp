
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
#ifndef SCRAPING // #endif at bottom
#include <clasp/core/foundation.h>
#include <type_traits>
//#include <llvm/Support/system_error.h>
#include <llvm/ExecutionEngine/GenericValue.h>
//#include <llvm/ExecutionEngine/SectionMemoryManager.h>
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

#include <clasp/core/symbolTable.h>

#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/metaClass.h>
#include <clasp/gctools/gcStack.h>
#include <clasp/gctools/containers.h>
#include <clasp/core/weakPointer.h>
#include <clasp/core/random.h>
#include <clasp/core/mpPackage.h>
#include <clasp/core/queue.h>
#include <clasp/core/unixfsys.h>
#include <clasp/core/weakKeyMapping.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/smallMultimap.h>
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
#include <clasp/core/instance.h>
#include <clasp/core/funcallableInstance.h>
#include <clasp/core/singleDispatchGenericFunction.h>
#include <clasp/core/arguments.h>
#include <clasp/core/bootStrapCoreSymbolMap.h>
#include <clasp/core/corePackage.h>
#include <clasp/core/lambdaListHandler.h>
#include <clasp/core/package.h>
#include <clasp/core/character.h>
#include <clasp/core/reader.h>
//#include <clasp/core/regex.h>
#include <clasp/core/array.h>
#include <clasp/core/readtable.h>
#include <clasp/core/nativeVector.h>
#include <clasp/core/lispStream.h>
#include <clasp/core/primitives.h>
#include <clasp/core/singleDispatchMethod.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/null.h>
#include <clasp/core/multiStringBuffer.h>
#include <clasp/core/posixTime.h>
#include <clasp/core/pointer.h>
#include <clasp/core/smallMap.h>
#include <clasp/core/pathname.h>
#include <clasp/core/sharpEqualWrapper.h>
#include <clasp/core/weakHashTable.h>
#include <clasp/core/intArray.h>
#include <clasp/core/fli.h>
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

#ifdef BUILD_EXTENSION
#define GC_INTERFACE_INCLUDES
#include <project_headers.h>
#undef GC_INTERFACE_INCLUDES
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
// The OS X offsetof macro is defined as __offsetof which is defined as __builtin_offsetof - take out one level of macro so that the MACRO_SAFE_TYPE hack works
#undef offsetof
#define offsetof(t,x) __builtin_offsetof(t,x)
#endif

namespace gctools {

/* This is where the class_layout codes are included
from clasp_gc.cc
They are generated by the layout analyzer. */

Layout_code* get_stamp_layout_codes() {
  static Layout_code codes[] = {
#if defined(USE_MPS)
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN_HELPERS
#include CLASP_GC_FILENAME
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
  printf("%s:%d Illegal header@%p in %s  header->header=%" PXuintptr_t "  header->data[0]=%" PXuintptr_t "  header->data[1]=%" PXuintptr_t "\n", __FILE__, __LINE__, &hdr, msg, hdr.header._value, hdr.additional_data[0], hdr.additional_data[1]);

template <typename RT, typename...ARGS>
NOINLINE void expose_function(const std::string& pkg_sym,
                              RT (*fp)(ARGS...),
                              const std::string& lambdaList)
{
  std::string pkgName;
  std::string symbolName;
  core::colon_split(pkg_sym,pkgName,symbolName);
//  printf("%s:%d  expose_function   pkgName=%s  symbolName=%s\n", __FILE__, __LINE__, pkgName.c_str(), symbolName.c_str() );
  core::wrap_function(pkgName,symbolName,fp,lambdaList);
}

template <typename RT, typename...ARGS>
NOINLINE void expose_function_setf(const std::string& pkg_sym,
                                   RT (*fp)(ARGS...),
                                   const std::string& lambdaList)
{
  std::string pkgName;
  std::string symbolName;
  core::colon_split(pkg_sym,pkgName,symbolName);
  core::wrap_function_setf(pkgName,symbolName,fp,lambdaList);
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


extern "C" {
using namespace gctools;

size_t obj_kind( core::T_O *tagged_ptr) {
  const core::T_O *client = untag_object<const core::T_O *>(tagged_ptr);
  const Header_s *header = reinterpret_cast<const Header_s *>(ClientPtrToBasePtr(client));
  return (size_t)(header->stamp());
}

const char *obj_kind_name(core::T_O *tagged_ptr) {
  core::T_O *client = untag_object<core::T_O *>(tagged_ptr);
  const Header_s *header = reinterpret_cast<const Header_s *>(ClientPtrToBasePtr(client));
  return obj_name(header->stamp());
}

const char *obj_name(gctools::stamp_t stamp) {
  if (stamp == (gctools::stamp_t)STAMP_null) {
    return "UNDEFINED";
  }
  if ( stamp > STAMP_max ) stamp = gctools::GCStamp<core::Instance_O>::Stamp;
  size_t stamp_index = (size_t)stamp;
  ASSERT(stamp_index<=global_stamp_max);
//  printf("%s:%d obj_name stamp= %d  stamp_index = %d\n", __FILE__, __LINE__, stamp, stamp_index);
  return global_stamp_info[stamp_index].name;
}

/*! I'm using a format_header so MPS gives me the object-pointer */
#define GC_DEALLOCATOR_METHOD
void obj_deallocate_unmanaged_instance(gctools::smart_ptr<core::T_O> obj ) {
  void* client = &*obj;
  printf("%s:%d About to obj_deallocate_unmanaged_instance %s\n", __FILE__, __LINE__, _rep_(obj).c_str() );
  // The client must have a valid header
#ifdef USE_MPS
  #ifndef RUNNING_GC_BUILDER
    #define GC_OBJ_DEALLOCATOR_TABLE
    #include CLASP_GC_FILENAME
    #undef GC_OBJ_DEALLOCATOR_TABLE
  #endif
#endif

  const gctools::Header_s *header = reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client));
  ASSERTF(header->stampP(), BF("obj_deallocate_unmanaged_instance called without a valid object"));
  gctools::GCStampEnum stamp = (GCStampEnum)(header->stamp());
#ifndef RUNNING_GC_BUILDER
  #ifdef USE_MPS
  size_t jump_table_index = (size_t)stamp; // - stamp_first_general;
  printf("%s:%d Calculated jump_table_index %lu\n", __FILE__, __LINE__, jump_table_index);
  goto *(OBJ_DEALLOCATOR_table[jump_table_index]);
    #define GC_OBJ_DEALLOCATOR
    #include CLASP_GC_FILENAME
    #undef GC_OBJ_DEALLOCATOR
  #endif // USE_MPS
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
  const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client));
  const Header_s::Value& header_value = header.header;
  tagged_stamp_t tag = header_value.tag();
  switch (tag) {
  case gctools::Header_s::stamp_tag: {
#ifdef DEBUG_VALIDATE_GUARD
    header->validate();
#endif
    gctools::GCStampEnum stamp = header_value.stamp();
    if ( stamp == STAMP_core__DerivableCxxObject_O ) {
        // If this is true then I think we need to call virtual functions on the client
        // to determine the Instance_O offset and the total size of the object.
      printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__ );
    }
    const Stamp_layout& stamp_layout = global_stamp_layout[stamp];
    if ( stamp == STAMP_core__SimpleBitVector_O ) {
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = core::SimpleBitVector_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
        // Do other bitunit vectors here
    } else if (stamp == gctools::STAMP_core__SimpleBaseString_O) {
          // Account for the SimpleBaseString additional byte for \0
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset) + 1;
      size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
      goto STAMP_CONTINUE;
    }
    if ( stamp_layout.container_layout ) {
      // special cases
      Container_layout& container_layout = *stamp_layout.container_layout;
      size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
      size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
    } else {
      if (stamp_layout.layout_op == templated_op) {
        size = ((core::General_O*)client)->templatedSizeof();
      } else {
        size = stamp_layout.size;
      }
    }
    STAMP_CONTINUE:
    client = (mps_addr_t)((char*)client + AlignUp(size + sizeof(Header_s)) + header.tail_size());
    break;
  }
  case gctools::Header_s::fwd_tag: {
    client = (char *)(client) + header.fwdSize();
    break;
  }
  case gctools::Header_s::pad_tag: {
    if (header_value.pad1P()) {
      client = (char *)(client) + header.pad1Size();
    } else {
      client = (char *)(client) + header.padSize();
    }
    break;
  }
  case gctools::Header_s::invalid_tag: {
    throw_hard_error_bad_client((void*)client);
  }
  }
  return client;
}
};
#endif // ifdef USE_MPS


#ifdef USE_MPS
extern "C" {
GC_RESULT obj_scan(mps_ss_t ss, mps_addr_t client, mps_addr_t limit) {
  mps_addr_t oldClient;
  size_t size = 0;  // Used to store the size of the object
  GCStampEnum stamp;
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
    while (client < limit) {
      oldClient = (mps_addr_t)client;
      // The client must have a valid header
      const gctools::Header_s& header = *reinterpret_cast<const gctools::Header_s *>(ClientPtrToBasePtr(client));
      const Header_s::Value& header_value = header.header;
      tagged_stamp_t tag = header_value.tag();
      switch (tag) {
      case gctools::Header_s::stamp_tag: {
#ifdef DEBUG_VALIDATE_GUARD
        header->validate();
#endif
        stamp = header_value.stamp();
        const Stamp_layout& stamp_layout = global_stamp_layout[stamp];
        if ( stamp == STAMP_core__DerivableCxxObject_O ) {
        // If this is true then I think we need to call virtual functions on the client
        // to determine the Instance_O offset and the total size of the object.
          printf("%s:%d Handle STAMP_core__DerivableCxxObject_O\n", __FILE__, __LINE__ );
        }
        if ( stamp == STAMP_core__SimpleBitVector_O ) {
          size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
          size = core::SimpleBitVector_O::bitunit_array_type::sizeof_for_length(capacity) + stamp_layout.data_offset;
          goto STAMP_CONTINUE;
        // Do other bitunit vectors here
        } else if (stamp == gctools::STAMP_core__SimpleBaseString_O) {
          // Account for the SimpleBaseString additional byte for \0
          size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset) + 1;
          size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
          goto STAMP_CONTINUE;
        }
        if (stamp_layout.layout_op == templated_op ) {
          size = ((core::General_O*)client)->templatedSizeof();
        } else {
          size = stamp_layout.size;
        }
        if ( stamp_layout.field_layout_start ) {
          int num_fields = stamp_layout.number_of_fields;
          const Field_layout* field_layout_cur = stamp_layout.field_layout_start;
          for ( int i=0; i<num_fields; ++i ) {
            core::T_O** field = (core::T_O**)((const char*)client + field_layout_cur->field_offset);
            POINTER_FIX(field);
            ++field_layout_cur;
          }
        }
        if ( stamp_layout.container_layout ) {
          const Container_layout& container_layout = *stamp_layout.container_layout;
          size_t capacity = *(size_t*)((const char*)client + stamp_layout.capacity_offset);
          size = stamp_layout.element_size*capacity + stamp_layout.data_offset;
          size_t end = *(size_t*)((const char*)client + stamp_layout.end_offset);
          for ( int i=0; i<end; ++i ) {
            Field_layout* field_layout_cur = container_layout.field_layout_start;
            ASSERT(field_layout_cur);
            const char* element = ((const char*)client + stamp_layout.data_offset + stamp_layout.element_size*i);
            for ( int j=0; j<container_layout.number_of_fields; ++j ) {
              core::T_O** field = (core::T_O**)((const char*)element + field_layout_cur->field_offset);
              POINTER_FIX(field);
              ++field_layout_cur;
            }
          }
        }
        STAMP_CONTINUE:
        client = (mps_addr_t)((char*)client + AlignUp(size + sizeof(Header_s)) + header.tail_size());
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object with stamp %u will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, header.stamp(), scan_size, skip_size);
          }
        }
#endif
        break;
      }
      case gctools::Header_s::fwd_tag: {
        client = (char *)(client) + header.fwdSize();
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object with fwd_tag will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, scan_size, skip_size);
          }
        }
#endif
        break;
      }
      case gctools::Header_s::pad_tag: {
        if (header_value.pad1P()) {
          client = (char *)(client) + header.pad1Size();
        } else if (header.padP()) {
          client = (char *)(client) + header.padSize();
        }
#ifdef DEBUG_MPS_SIZE
        {
          size_t scan_size = ((char*)client-(char*)oldClient);
          size_t skip_size = ((char*)obj_skip(oldClient)-(char*)oldClient);
          if (scan_size != skip_size) {
            printf("%s:%d The size of the object with pad_tag will not be calculated properly - obj_scan -> %lu  obj_skip -> %lu\n",
                   __FILE__, __LINE__, scan_size, skip_size);
          }
        }
#endif
        break;
      }
      case gctools::Header_s::invalid_tag: {
        throw_hard_error_bad_client((void*)client);
      }
      }
    }
  } MPS_SCAN_END(GC_SCAN_STATE);
  return MPS_RES_OK;
}
};
#endif // ifdef USE_MPS


#ifdef USE_MPS
extern "C" {
/*! I'm using a format_header so MPS gives me the object-pointer */
  #define GC_FINALIZE_METHOD
void obj_finalize(mps_addr_t client) {
  // The client must have a valid header
  DEBUG_THROW_IF_INVALID_CLIENT(client);
  mps_addr_t next_client = obj_skip(client);
  size_t block_size = (char*)next_client-(char*)client;
  #ifndef RUNNING_GC_BUILDER
    #define GC_OBJ_FINALIZE_TABLE
    #include CLASP_GC_FILENAME
    #undef GC_OBJ_FINALIZE_TABLE
  #endif // ifndef RUNNING_GC_BUILDER
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(const_cast<void*>(ClientPtrToBasePtr(client)));
  ASSERTF(header->stampP(), BF("obj_finalized called without a valid object"));
  gctools::GCStampEnum stamp = (GCStampEnum)(header->stamp());
  #ifndef RUNNING_GC_BUILDER
  size_t table_index = (size_t)stamp;
  goto *(OBJ_FINALIZE_table[table_index]);
    #define GC_OBJ_FINALIZE
    #include CLASP_GC_FILENAME
    #undef GC_OBJ_FINALIZE
  #endif // ifndef RUNNING_GC_BUILDER
 finalize_done:
  // Now replace the object with a pad object
  header->setPadSize(block_size);
  header->setPad(Header_s::pad_tag);
}; // obj_finalize
}; // extern "C"
  #undef GC_FINALIZE_METHOD
#endif // ifdef USE_MPS



#ifdef USE_MPS
extern "C" {
mps_res_t main_thread_roots_scan(mps_ss_t ss, void *gc__p, size_t gc__s) {
  MPS_SCAN_BEGIN(GC_SCAN_STATE) {
#if 0
    // We don't scan global load-time-value tables any more here
    // they are added as tables of roots as they are created
    for (auto &it : global_roots) {
      POINTER_FIX(it);
    }
#endif
#ifndef RUNNING_GC_BUILDER
#define GC_GLOBALS
#include CLASP_GC_FILENAME
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
  #include CLASP_GC_FILENAME
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
      #include CLASP_GC_FILENAME
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
  bool found =  symbols->find_symbol(package_part,symbol_part, store );
  if ( !found ) {
    printf("%s:%d ERROR!!!! The static class symbol %s was not found orig_symbol_part=|%s| symbol_part=|%s|!\n", __FILE__, __LINE__, full_name.c_str(), orig_symbol_part.c_str(), symbol_part.c_str() );
    abort();
  }
  if (store._PackageName != package_part) {
    printf("%s:%d For symbol %s there is a mismatch in the package desired %s and the one retrieved %s\n", __FILE__, __LINE__, full_name.c_str(), package_part.c_str(), store._PackageName.c_str());
    SIMPLE_ERROR(BF("Mismatch of package when setting a class symbol"));
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

template <class TheClass>
NOINLINE void set_one_static_class_Header() {
  Stamp the_stamp = gctools::NextStamp(gctools::GCStamp<TheClass>::Stamp);
  if (gctools::GCStamp<TheClass>::Stamp!=0) {
    TheClass::static_HeaderValue = gctools::Header_s::Value::make<TheClass>();
  } else {
    TheClass::static_HeaderValue = gctools::Header_s::Value::make_unknown((GCStampEnum)the_stamp);
  }
}


template <class TheClass>
NOINLINE  gc::smart_ptr<core::Instance_O> allocate_one_metaclass(Fixnum theStamp, core::Symbol_sp classSymbol, core::Instance_sp metaClass)
{
  core::FunctionDescription* fdesc = core::makeFunctionDescription(kw::_sym_create);
  auto cb = gctools::GC<TheClass>::allocate(fdesc);
  gc::smart_ptr<core::Instance_O> class_val = core::Instance_O::createClassUncollectable(theStamp,metaClass,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS,cb);
  class_val->__setup_stage1_with_sharedPtr_lisp_sid(class_val,classSymbol);
//  reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<TheClass>::id,TheClass::static_classSymbol());
//  TheClass::static_class = class_val;
  _lisp->boot_setf_findClass(classSymbol,class_val);
//  core::core__setf_find_class(class_val,classSymbol);
  return class_val;
}


template <class TheClass>
NOINLINE  gc::smart_ptr<core::Instance_O> allocate_one_class(core::Instance_sp metaClass)
{
  gctools::smart_ptr<core::BuiltInObjectCreator<TheClass>> cb = gctools::GC<core::BuiltInObjectCreator<TheClass>>::allocate();
  TheClass::set_static_creator(cb);
  gc::smart_ptr<core::Instance_O> class_val = core::Instance_O::createClassUncollectable(TheClass::static_HeaderValue.stamp(),metaClass,REF_CLASS_NUMBER_OF_SLOTS_IN_STANDARD_CLASS,cb);
  class_val->__setup_stage1_with_sharedPtr_lisp_sid(class_val,TheClass::static_classSymbol());
  reg::lisp_associateClassIdWithClassSymbol(reg::registered_class<TheClass>::id,TheClass::static_classSymbol());
  TheClass::static_class = class_val;
//  core::core__setf_find_class(class_val,TheClass::static_classSymbol()); //,true,_Nil<core::T_O>()
  _lisp->boot_setf_findClass(TheClass::static_classSymbol(),class_val);
  return class_val;
}

template <class TheMetaClass>
struct TempClass {
  static gctools::smart_ptr<TheMetaClass> holder;
};


std::map<std::string,size_t> global_stamp_name_map;
std::vector<std::string> global_stamp_names;
size_t _global_last_stamp = 0;

void register_stamp_name(const std::string& stamp_name, size_t stamp_num) {
  global_stamp_name_map[stamp_name] = stamp_num;
  if (stamp_num>=global_stamp_names.size()) {
    global_stamp_names.resize(stamp_num+1,"");
    global_stamp_names[stamp_num] = stamp_name;
  }
}

void define_builtin_cxx_classes() {
#ifndef SCRAPING
 #define GC_ENUM_NAMES
  #include INIT_CLASSES_INC_H
 #undef GC_ENUM_NAMES
#endif
}


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
  IMPLEMENT_MEF("define_base_classes");
}


void calculate_class_precedence_lists()
{
  IMPLEMENT_MEF("calculate_class_precendence_lists");
}

// ------------------------------------------------------------
//
// Generate type specifier -> header value (range) map
//

template <typename TSingle>
void add_single_typeq_test(const string& cname, core::HashTable_sp theMap) {
  Fixnum header_val = gctools::Header_s::Value::GenerateHeaderValue<TSingle>();
//  printf("%s:%d Header value for type %s -> %lld    stamp: %u  flags: %zu\n", __FILE__, __LINE__, _rep_(TSingle::static_class_symbol).c_str(), header_val, gctools::GCStamp<TSingle>::Stamp, gctools::GCStamp<TSingle>::Flags);
  theMap->setf_gethash(TSingle::static_class_symbol,core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<TSingle>()));
}

template <typename TRangeFirst,typename TRangeLast>
void add_range_typeq_test(const string& cname, core::HashTable_sp theMap) {
  
  theMap->setf_gethash(TRangeFirst::static_class_symbol,
                       core::Cons_O::create(core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<TRangeFirst>()),
                                            core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<TRangeLast>())));
}
template <typename TSingle>
void add_single_typeq_test_instance(core::HashTable_sp theMap) {
  theMap->setf_gethash(TSingle::static_class_symbol,core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<TSingle>()));
}

template <typename TRangeFirst,typename TRangeLast>
void add_range_typeq_test_instance(core::HashTable_sp theMap) {
  theMap->setf_gethash(TRangeFirst::static_class_symbol,
                       core::Cons_O::create(core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<TRangeFirst>()),
                                            core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<TRangeLast>())));
}
  
void initialize_typeq_map() {
  core::HashTableEqual_sp classNameToLispName = core::HashTableEqual_O::create_default();
  core::HashTableEq_sp theTypeqMap = core::HashTableEq_O::create_default();
#define ADD_SINGLE_TYPEQ_TEST(type,stamp) { \
    classNameToLispName->setf_gethash(core::SimpleBaseString_O::make(#type),type::static_class_symbol); \
    theTypeqMap->setf_gethash(type::static_class_symbol,core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<type>())); \
  }
#define ADD_RANGE_TYPEQ_TEST(type_low,type_high,stamp_low,stamp_high) { \
    classNameToLispName->setf_gethash(core::SimpleBaseString_O::make(#type_low),type_low::static_class_symbol); \
    theTypeqMap->setf_gethash(type_low::static_class_symbol, \
                              core::Cons_O::create(core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<type_low>()), \
                                                   core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<type_high>()))); \
  }
#define ADD_SINGLE_TYPEQ_TEST_INSTANCE(type,stamp) { \
    classNameToLispName->setf_gethash(core::SimpleBaseString_O::make(#type),type::static_class_symbol); \
    theTypeqMap->setf_gethash(type::static_class_symbol,core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<type>())); \
  }
#define ADD_RANGE_TYPEQ_TEST_INSTANCE(type_low,type_high,stamp_low,stamp_high) { \
    classNameToLispName->setf_gethash(core::SimpleBaseString_O::make(#type_low),type_low::static_class_symbol); \
    theTypeqMap->setf_gethash(type_low::static_class_symbol, \
                              core::Cons_O::create(core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<type_low>()), \
                                                   core::make_fixnum(gctools::Header_s::Value::GenerateHeaderValue<type_high>()))); \
  }
#ifndef SCRAPING
 #ifdef USE_BOEHM
  #define GC_TYPEQ
    #include INIT_CLASSES_INC_H // REPLACED CLASP_GC_FILENAME
  #undef GC_TYPEQ
 #endif
 #if defined(USE_MPS) && !defined(RUNNING_GC_BUILDER)
  #define GC_TYPEQ
   #include CLASP_GC_FILENAME
  #undef GC_TYPEQ
 #endif
#endif
  core::_sym__PLUS_class_name_to_lisp_name_PLUS_->defparameter(classNameToLispName);
  core::_sym__PLUS_type_header_value_map_PLUS_->defparameter(theTypeqMap);
};

// ----------------------------------------------------------------------
//
// Expose classes and methods
//
// Code generated by scraper
//
//
#include <clasp/core/wrappers.h>
#include <clasp/core/external_wrappers.h>

#ifndef SCRAPING
 #define EXPOSE_STATIC_CLASS_VARIABLES
  #include INIT_CLASSES_INC_H
 #undef EXPOSE_STATIC_CLASS_VARIABLES
#endif

#ifndef SCRAPING
 #define EXPOSE_METHODS
  #include INIT_CLASSES_INC_H
 #undef EXPOSE_METHODS
#endif

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
#ifndef SCRAPING
 #define EXPOSE_CLASSES_AND_METHODS
  #include INIT_CLASSES_INC_H
 #undef EXPOSE_CLASSES_AND_METHODS
#endif
}

#if 0
#define MPS_LOG(x) printf("%s:%d %s\n", __FILE__, __LINE__, x);
#else
#define MPS_LOG(x)
#endif

void initialize_clasp_Kinds()
{
  #ifndef SCRAPING
   #define SET_CLASS_KINDS
    #include INIT_CLASSES_INC_H
   #undef SET_CLASS_KINDS
  #endif
}



Fixnum global_TheClassRep_stamp;

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

  Fixnum TheClass_stamp = gctools::NextStamp();
  Fixnum TheBuiltInClass_stamp = gctools::NextStamp();
  Fixnum TheStandardClass_stamp = gctools::NextStamp();
  Fixnum TheStructureClass_stamp = gctools::NextStamp();
  Fixnum TheDerivableCxxClass_stamp = gctools::NextStamp();
  Fixnum TheClbindCxxClass_stamp = gctools::NextStamp();
  global_TheClassRep_stamp = gctools::GCStamp<clbind::ClassRep_O>::Stamp;
  _lisp->_Roots._TheClass = allocate_one_metaclass<core::StandardClassCreator_O>(TheClass_stamp,cl::_sym_class,_Unbound<core::Instance_O>());
  _lisp->_Roots._TheBuiltInClass = allocate_one_metaclass<core::StandardClassCreator_O>(TheBuiltInClass_stamp,cl::_sym_built_in_class,_Unbound<core::Instance_O>());
  _lisp->_Roots._TheStandardClass = allocate_one_metaclass<core::StandardClassCreator_O>(TheStandardClass_stamp,cl::_sym_standard_class,_Unbound<core::Instance_O>());
  _lisp->_Roots._TheStructureClass = allocate_one_metaclass<core::StandardClassCreator_O>(TheStructureClass_stamp,cl::_sym_structure_class,_Unbound<core::Instance_O>());
  _lisp->_Roots._TheDerivableCxxClass = allocate_one_metaclass<core::DerivableCxxClassCreator_O>(TheDerivableCxxClass_stamp,core::_sym_derivable_cxx_class,_Unbound<core::Instance_O>());
  _lisp->_Roots._TheClbindCxxClass = allocate_one_metaclass<core::ClassRepCreator_O>(TheClbindCxxClass_stamp,core::_sym_clbind_cxx_class,_Unbound<core::Instance_O>());
  _lisp->_Roots._TheClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheBuiltInClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheStandardClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheStructureClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheDerivableCxxClass->_Class = _lisp->_Roots._TheStandardClass;
  _lisp->_Roots._TheClbindCxxClass->_Class = _lisp->_Roots._TheStandardClass;
  MPS_LOG("initialize_clasp ALLOCATE_ALL_CLASSES");
  #ifndef SCRAPING
   #define ALLOCATE_ALL_CLASSES
    #include INIT_CLASSES_INC_H
   #undef ALLOCATE_ALL_CLASSES
  #endif
  core_T_O_var->setInstanceBaseClasses(_Nil<core::T_O>());
  // ClassRep_O is initialized like other class objects - but we need to save it in a special system-wide variable
//  _lisp->_Roots._TheClassRep = clbind_ClassRep_O_var;
  
  create_packages();
  // have to do this before symbols are finalized so that keywords are all bound properly.
  gc::As<core::Package_sp>(_lisp->findPackage("KEYWORD"))->setKeywordPackage(true);
  
  define_builtin_cxx_classes();

  bootStrapCoreSymbolMap.finish_setup_of_symbols();

  // Define base classes
  #ifndef SCRAPING
   #define SET_BASES_ALL_CLASSES
    #include INIT_CLASSES_INC_H
   #undef SET_BASES_ALL_CLASSES
  #endif

    // Define base classes
  #ifndef SCRAPING
   #define CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES
    #include INIT_CLASSES_INC_H
   #undef CALCULATE_CLASS_PRECEDENCE_ALL_CLASSES
  #endif

  _lisp->_Roots._TheClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS,_Nil<core::T_O>());
  _lisp->_Roots._TheBuiltInClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheBuiltInClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheBuiltInClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheBuiltInClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS,_Nil<core::T_O>());
  _lisp->_Roots._TheStandardClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheStandardClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheStandardClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheStandardClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS,_Nil<core::T_O>());
  _lisp->_Roots._TheStructureClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheStructureClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheStructureClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheStructureClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS,_Nil<core::T_O>());
  _lisp->_Roots._TheDerivableCxxClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheDerivableCxxClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheDerivableCxxClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheDerivableCxxClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS,_Nil<core::T_O>());
  _lisp->_Roots._TheClbindCxxClass->stamp_set(TheStandardClass_stamp);
  _lisp->_Roots._TheClbindCxxClass->instanceSet(core::Instance_O::REF_CLASS_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheClbindCxxClass->instanceSet(core::Instance_O::REF_CLASS_DIRECT_SLOTS,_Nil<core::T_O>());
  _lisp->_Roots._TheClbindCxxClass->instanceSet(core::Instance_O::REF_CLASS_DEFAULT_INITARGS,_Nil<core::T_O>());

  _lisp->_Roots._TheBuiltInClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheStandardClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheStructureClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheDerivableCxxClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));
  _lisp->_Roots._TheClbindCxxClass->setInstanceBaseClasses(core::Cons_O::createList(_lisp->_Roots._TheClass));

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
#endif // #ifndef SCRAPING at top
