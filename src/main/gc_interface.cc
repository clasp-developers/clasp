typedef bool _Bool;
#include <type_traits>
#include <llvm/Support/system_error.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/CodeGen/LinkAllCodegenComponents.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include "llvm/Support/raw_ostream.h"
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


#include <gctools/symbolTable.h>
#include <sockets/symbolTable.h>
#include <serveEvent/symbolTable.h>
#include <clbind/symbolTable.h>

#include "gctools/gctoolsPackage.h"



#include <core/foundation.h>
#include "core/weakReference.h"
#include "core/bitVector.h"
#include "core/funcallableStandardClass.h"
#include "core/structureClass.h"
//#include "core/symbolVector.h"
#include "core/hashTable.h"
#include "core/hashTableEq.h"
#include "core/hashTableEql.h"
#include "core/hashTableEqual.h"
#include "core/hashTableEqualp.h"
#include "core/userData.h"
#include "core/sexpLoadArchive.h"
#include "core/sexpSaveArchive.h"
#include "core/loadTimeValues.h"
#include "core/specialForm.h"
#include "core/singleDispatchGenericFunction.h"
#include "core/arguments.h"
#include "core/bootStrapCoreSymbolMap.h"
#include "core/corePackage.h"
#include "core/lambdaListHandler.h"
#include "core/package.h"
#include "core/character.h"
#include "core/reader.h"
#include "core/singleDispatchEffectiveMethodFunction.h"
#include "core/microHeap.h"
#include "core/regex.h"
#include "core/structureObject.h"
#include "core/forwardReferencedClass.h"
#include "core/standardClass.h"
#include "core/stringSet.h"
#include "core/symbolSet.h"
#include "core/readTable.h"
#include "core/arrayObjects.h"
#include "core/intArray.h"
#include "core/lispStream.h"
#include "core/primitives.h"
#include "core/singleDispatchMethod.h"
#include "core/binder.h"
#include "core/fileSystem.h"
#include "core/objectSet.h"
#include "core/symbolList.h"
#include "core/stringList.h"
#include "core/null.h"
#include "core/multiStringBuffer.h"
#include "core/posixTime.h"
#include "core/pointer.h"
#include "core/objRef.h"
#include "core/pathname.h"
#include "core/strWithFillPtr.h"
#include "core/weakHashTable.h"

#include "clbind/clbind.h"

#include "cffi/cffi.h"

#include "llvmo/intrinsics.h"
#include "llvmo/llvmoExpose.h"
#include "llvmo/debugLoc.h"
#include "llvmo/insertPoint.h"
#include "llvmo/debugInfoExpose.h"

#include "asttooling/astExpose.h"
#include "asttooling/clangTooling.h"
#include "asttooling/astVisitor.h"
#include "asttooling/example.h"
#include "asttooling/Registry.h"
#include "asttooling/Diagnostics.h"
#include "asttooling/Marshallers.h"
#include "asttooling/testAST.h"


#define NAMESPACE_gctools
#define NAMESPACE_core
#include "gc_interface.h"
#undef NAMESPACE_gctools
#undef NAMESPACE_core



#ifdef USE_MPS

#ifdef DEBUG_MPS
#define MPS_LOG(fm) {printf("%s:%d %s --> %s\n", __FILE__, __LINE__, __FUNCTION__, (fm).str().c_str());}
#else
#define MPS_LOG(fm)
#endif

extern "C"  {
    using namespace gctools;
    const char* obj_name( gctools::GCKindEnum kind )
    {
	switch (kind) {
#ifndef RUNNING_GC_BUILDER
#define GC_KIND_NAME_MAP
#include "main/clasp_gc.cc"
#undef GC_KIND_NAME_MAP
#else
            // RUNNING_GC_BUILDER

#endif
	default: {
	    return "UNKNOWN KIND in obj_name";
	}
	}
    }


};


extern "C" {


    using namespace gctools;





    /*! I'm using a format_header so MPS gives me the object-pointer */
    mps_addr_t obj_skip( mps_addr_t client )
    {
        gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
        MPS_LOG(BF("obj_skip client = %p   header=%p  header-desc: %s") % client % header % header->description());
        if ( header->kindP() ) {
#if 0
            Seg seg;
            gctools::GCKindEnum kind;
            unsigned int length;
            if (SegOfAddr(&seg,gctools::_global_arena,header)) {
                ShieldExpose(gctools::_global_arena,seg);
                kind = (gctools::GCKindEnum)(header->kind.Kind);
                length = header->kind.Length;
                ShieldCover(gctools::_global_arena,seg);
            };
#else
            gctools::GCKindEnum kind = header->kind();
#endif        
            switch (kind) {     
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SKIP
#include "main/clasp_gc.cc"
#undef GC_OBJ_SKIP
#endif
        default: {
            fprintf(stderr,"Garbage collection tried to obj_skip an object of unknown kind: %s\n", header->description().c_str() );
            assert(0);
            abort();
        }
        };
        } else if (header->fwdP()) {
            client = (char*)(client)+header->fwdSize()+global_alignup_sizeof_header;
        } else if (header->pad1P()) {
            client = (char*)(client)+header->pad1Size()+global_alignup_sizeof_header;
        } else if (header->padP()) {
            client = (char*)(client)+header->padSize()+global_alignup_sizeof_header;
        } else {
            THROW_HARD_ERROR(BF("Illegal header at %p") % header );
        }
        DEBUG_MPS_MESSAGE(BF("Leaving obj_skip with client@%p") % client);
	return client;
    }


    int trap_obj_scan = 0;

    GC_RESULT obj_scan(mps_ss_t ss, mps_addr_t client, mps_addr_t limit)
    {
        DEBUG_MPS_MESSAGE(BF("obj_scan started - Incoming client %p   limit: %p") % client % limit );
        MPS_SCAN_BEGIN(GC_SCAN_STATE) {
            while (client<limit)
            {
                gctools::Header_s* header = reinterpret_cast<gctools::Header_s*>(ClientPtrToBasePtr(client));
                MPS_LOG(BF("obj_skip client = %p   header=%p  header-desc: %s") % client % header % header->description());
                if ( header->kindP() ) {
#if 0
                    Seg seg;
                    gctools::GCKindEnum kind;
                    unsigned int length;
                    if (SegOfAddr(&seg,gctools::_global_arena,header)) {
                        ShieldExpose(gctools::_global_arena,seg);
                        kind = (gctools::GCKindEnum)(header->kind.Kind);
                        length = header->kind.Length;
                        ShieldCover(gctools::_global_arena,seg);
                    };
#else
                    GCKindEnum kind = header->kind();
#endif        
                    switch (kind) {     
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_SCAN
#include "main/clasp_gc.cc"
#undef GC_OBJ_SCAN
#endif
                    default: {
                        fprintf(stderr,"Garbage collection tried to obj_skip an object of unknown kind[%s]\n", header->description().c_str() );
                        assert(0);
                        abort();
                    }
                    };
                } else if (header->fwdP()) {
                    client = (char*)(client)+header->fwdSize()+global_alignup_sizeof_header;
                } else if (header->pad1P()) {
                    client = (char*)(client)+header->pad1Size()+global_alignup_sizeof_header;
                } else if (header->padP()) {
                    client = (char*)(client)+header->padSize()+global_alignup_sizeof_header;
                } else {
                    THROW_HARD_ERROR(BF("Illegal header at %p") % header );
                }
            }
        } MPS_SCAN_END(GC_SCAN_STATE);
        return MPS_RES_OK;
    }



            /*! I'm using a format_header so MPS gives me the object-pointer */
#define GC_FINALIZE_METHOD
            void obj_finalize( mps_addr_t ptr )
            {
                THROW_HARD_ERROR(BF("Check what the ptr pointer is - is it a client or a base pointer"));
#if 0
            gctools::Header_s<void>* header = reinterpret_cast<gctools::Header_s<void>*>(base);
            gctools::GCKindEnum kind = (GCKindEnum)(header->kind.Kind);
            DEBUG_MPS_MESSAGE(BF("Finalizing base@%p   kind=%s") % base  % obj_name((gctools::GCKindEnum)(header->kind.Kind)) );
            switch (kind) {
#ifndef RUNNING_GC_BUILDER
#define GC_OBJ_FINALIZE
#include "main/clasp_gc.cc"
#undef GC_OBJ_FINALIZE
#endif
        default: {
            fprintf(stderr,"Garbage collection tried to obj_finalize an object of unknown   kind[%d]\n", kind);
            assert(0);
            abort();
        }
        };
#endif
    }
#undef GC_FINALIZE_METHOD










    mps_res_t main_thread_roots_scan(mps_ss_t ss, void *gc__p, size_t gc__s)
    {
        DEBUG_MPS_MESSAGE(BF("in main_thread_roots_scan"));
//	mps_thr_t gc__thr = 0; // This isn't passed in but the scanners need it 
        MPS_SCAN_BEGIN(GC_SCAN_STATE) {
            MPS_LOG(BF("Starting rooted_HeapRoots"));
#if 0
            for ( HeapRoot* scur=rooted_HeapRoots; scur!=NULL; scur=scur->_next ) {
                scur->onHeapScanGCRoots(GC_SCAN_ARGS_PASS);
            }
            MPS_LOG(BF("Starting rooted_StackRoots"));
            for ( StackRoot* lcur=rooted_StackRoots; lcur!=NULL; lcur=lcur->_next ) {
                lcur->onStackScanGCRoots(GC_SCAN_ARGS_PASS);
            }
#endif
#ifndef RUNNING_GC_BUILDER
#define GC_GLOBALS
#include "main/clasp_gc.cc"
#undef GC_GLOBALS
#endif
            MPS_LOG(BF("Done roots_scan"));
        } MPS_SCAN_END(GC_SCAN_STATE);
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
#include "main/clasp_gc.cc"
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
#include "main/clasp_gc.cc"
#undef GC_LOCAL_VARIABLES_DANGEROUS_UNROOTED
#endif
#endif




#endif // ifdef USE_MPS


