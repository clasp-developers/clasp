/*
    File: mpsGarbageCollection.cc
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



#include "core/foundation.h"
#include "core/object.h"
#include "core/numbers.h"
#include "core/str.h"
#include "core/builtInClass.h"
#include "core/loadTimeValues.h"
#include "core/posixTime.h" // was core/posixTime.cc???
#include "core/symbolTable.h"
#include "core/standardClass.h"
#include "core/structureClass.h"
#include "core/wrappers.h"
#include "main/gc_interface.fwd.h"


extern "C" {
#include "mps/code/mpscawl.h"  // MVFF pool
#include "mps/code/mpscamc.h" // AMC pool

};


#include "gctoolsPackage.h"



extern "C" {
    struct PointerSearcher {
        PointerSearcher() :  poolObjects(0) {};
        int     poolObjects;
        int     stackAddresses;
        vector<mps_addr_t>       poolMatches;
        vector<mps_addr_t>       stackMatches;
    };


    void pointerSearcherAddRef(PointerSearcher* searcher, mps_addr_t ref)
    {
        searcher->poolMatches.push_back(ref);
    }

    extern void memory_find_ref(mps_arena_t arena, mps_addr_t ref, PointerSearcher* searcher );
};





namespace gctools
{

    MpsMetrics globalMpsMetrics;

/* --------------------------------------------------
   --------------------------------------------------
   --------------------------------------------------
   Global variables for MPS
   --------------------------------------------------
   --------------------------------------------------
*/

    mps_arena_t	_global_arena;
//    mps_pool_t _global_mvff_pool;
    mps_pool_t _global_amc_pool;
    mps_pool_t _global_amcz_pool;
    mps_pool_t _global_awl_pool;
    mps_ap_t _global_weak_link_allocation_point;
    mps_ap_t _global_strong_link_allocation_point;
//    mps_ap_t _global_mvff_allocation_point;
    mps_ap_t _global_automatic_mostly_copying_zero_rank_allocation_point;

    mps_pool_t global_non_moving_pool;
    mps_ap_t global_non_moving_ap;
    size_t global_sizeof_fwd;
    size_t global_alignup_sizeof_header;


#ifdef DEBUG_MPS
#define MPS_LOG(fm) {printf("%s:%d %s --> %s\n", __FILE__, __LINE__, __FUNCTION__, (fm).str().c_str());}
#else
#define MPS_LOG(fm)
#endif


    string gcResultToString(GC_RESULT res)
    {
        switch (res) {
        case MPS_RES_OK: return "operation succeeded.";
        case MPS_RES_FAIL: return "operation failed.";
        case MPS_RES_IO: return "an input/output error occurred.";
        case MPS_RES_LIMIT: return "an internal limitation was exceeded.";
        case MPS_RES_MEMORY: return "needed memory could not be obtained.";
        case MPS_RES_RESOURCE: return "a needed resource could not be obtained.";
        case MPS_RES_UNIMPL: return "operation is not implemented.";
        case MPS_RES_COMMIT_LIMIT: return "the arena’s commit limit would be exceeded.";
        case MPS_RES_PARAM: return "an invalid parameter was passed.";
        default:
            return "Unknown GC_RESULT";
        };
    };



    void searchMemoryForAddress(mps_addr_t addr)
    {
        PointerSearcher searcher;
//        memory_find_ref(_global_arena, addr, &searcher );
        
        // Search the stack
        uintptr_t* sptr = reinterpret_cast<uintptr_t*>(&searcher)+1;
        for ( ; sptr<_global_stack_marker; ++sptr ) {
            if ( *sptr == reinterpret_cast<uintptr_t>(addr) ) {
                searcher.stackMatches.push_back(sptr);
            }
            ++(searcher.stackAddresses);
        }
        printf("Searched through %d pool objects\n", searcher.poolObjects );
        for ( auto hit : searcher.poolMatches ) {
            printf("addr=%p found in pool at  @%p\n", addr, hit );
        }
        printf("Searched through %d stack addresses\n", searcher.stackAddresses );
        for ( auto sit : searcher.poolMatches ) {
            printf("addr=%p found on stack at @%p\n", addr, sit );
        }
        printf("--------------------------- Search done\n");
    };


#define GC_RESULT_ERROR(res, msg) {                                     \
        string error = gcResultToString(res);                           \
        THROW_HARD_ERROR(BF("GC_RESULT error: %s   %s\n") % error % msg ); \
    }

    static void obj_fwd(mps_addr_t old_client, mps_addr_t new_client)
    {
        DEBUG_MPS_MESSAGE(BF("obj_fwd old_client = %p   new_client = %p") % old_client % new_client );
	mps_addr_t limit = obj_skip(old_client);
	size_t size = (char *)limit - (char *)old_client;
        if ( size < global_sizeof_fwd ) {
            THROW_HARD_ERROR(BF("obj_fwd needs size >= %zu") % global_sizeof_fwd );
        }
        Header_s* header = reinterpret_cast<Header_s*>(ClientPtrToBasePtr(old_client));
        header->setFwdSize(size);
        header->setFwdPointer(new_client);
    }



    static mps_addr_t obj_isfwd(mps_addr_t client)
    {
        Header_s* header = reinterpret_cast<Header_s*>(ClientPtrToBasePtr(client));
        MPS_LOG(BF(" client = %p base=%p") % client % header );
        MPS_LOG(BF("    kind = %s") % header->description() );
        if (header->fwdP()) {
            MPS_LOG(BF("   isfwd=TRUE returning %p") % header->fwdPointer() );
            return header->fwdPointer();
        }
	MPS_LOG(BF("   isfwd=FALSE returning NULL  kind = %s") % obj_name((gctools::GCKindEnum)(header->kind())));
        return NULL;
    }


    static void obj_pad(mps_addr_t base, size_t size)
    {
	size_t alignment = Alignment();
	MPS_LOG(BF("base = %p ALIGNMENT = %zd size=%zu ") % base % alignment % size );
	assert(size >= alignment );
	Header_s* header = reinterpret_cast<Header_s*>(base);
        if (size == alignment) {
            header->setPad(Header_s::pad1_tag);
	} else {
            header->setPad(Header_s::pad_tag);
            header->setPadSize(size);
	}
    }






// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------

};

extern "C" {

    int processMpsMessages(void)
    {
        int messages(0);
        int mFinalize(0);
        int mGcStart(0);
        int mGc(0);
        core::Number_sp startTime = core::cl_getInternalRunTime().as<core::Number_O>();
        mps_message_type_t type;
        while (mps_message_queue_type(&type, gctools::_global_arena)) {
            mps_message_t message;
            mps_bool_t b;
            b = mps_message_get(&message, gctools::_global_arena, type);
            ++messages;
            assert(b); /* we just checked there was one */
            if (type == mps_message_type_gc_start()) {
                ++mGcStart;
#if 0
                DEBUG_MPS_MESSAGE(BF("Message: mps_message_type_gc_start"));
                printf("Message: mps_message_type_gc_start\n");
                printf("Collection started.\n");
                printf("  Why: %s\n", mps_message_gc_start_why(_global_arena, message));
                printf("  clock: %lu\n", (unsigned long)mps_message_clock(_global_arena, message));
#endif
            } else if ( type == mps_message_type_gc() ) {
                ++mGc;
                DEBUG_MPS_MESSAGE(BF("Message: mps_message_type_gc"));
#if 0
                printf("Message: mps_message_type_gc()\n");
                size_t live = mps_message_gc_live_size(_global_arena, message);
                size_t condemned = mps_message_gc_condemned_size(_global_arena, message);
                size_t not_condemned = mps_message_gc_not_condemned_size(_global_arena, message);
                printf("Collection finished.\n");
                printf("    live %lu\n", (unsigned long)live);
                printf("    condemned %lu\n", (unsigned long)condemned);
                printf("    not_condemned %lu\n", (unsigned long)not_condemned);
                printf("    clock: %lu\n", (unsigned long)mps_message_clock(_global_arena, message));
#endif
            } else if ( type == mps_message_type_finalization() ) {
                ++mFinalize;
//                printf("%s:%d mps_message_type_finalization received\n", __FILE__, __LINE__);
                DEBUG_MPS_MESSAGE(BF("Message: mps_message_type_finalization"));
                mps_addr_t ref_o;
                mps_message_finalization_ref(&ref_o,gctools::_global_arena,message);
                obj_finalize(ref_o);
            } else {
                printf("Message: UNKNOWN!!!!!\n");
            }
            mps_message_discard(gctools::_global_arena, message);
        }
#if 0
//        printf("%s:%d Leaving processMpsMessages\n",__FILE__,__LINE__);
        core::Number_sp endTime = core::cl_getInternalRunTime().as<core::Number_O>();
        core::Number_sp deltaTime = core::contagen_mul(core::contagen_sub(endTime,startTime),core::Fixnum_O::create(1000));
        core::Number_sp deltaSeconds = core::contagen_div(deltaTime,cl::_sym_internalTimeUnitsPerSecond->symbolValue().as<core::Number_O>());
        printf("%s:%d [processMpsMessages %s millisecs for  %d finalization/ %d gc-start/ %d gc messages]\n", __FILE__, __LINE__, _rep_(deltaSeconds).c_str(), mFinalize, mGcStart, mGc );
        fflush(stdout);
#endif
        return messages;
    };

};


namespace gctools {


    void test_mps_allocation()
    {
        int numAllocations = 10000;
        printf("Starting test_mps_allocation -> allocating %d objects\n", numAllocations );
        for ( int i=0; i<numAllocations; ++i ) {
            core::Str_sp ss = core::Str_O::create("Hi there, this is a test");
            processMpsMessages();
        }
        printf("Done test_mps_allocation - allocated %d objects\n", numAllocations);
    }



    mps_addr_t awlFindDependent(mps_addr_t other)
    {
        gctools::WeakObject* wo = reinterpret_cast<gctools::WeakObject*>(other);
        return reinterpret_cast<mps_addr_t>(wo->dependentPtr());
    }



/* -------------------------------------------------- */

    mps_addr_t dummyAwlFindDependent(mps_addr_t addr)
    {
        return NULL;
    }


    bool parseClaspMpsConfig(size_t& arenaMb, size_t& spareCommitLimitMb, size_t& nurseryKb, size_t& nurseryMortalityPercent, size_t& generation1Kb, size_t& generation1MortalityPercent )
    {
        char* cur = getenv("CLASP_MPS_CONFIG");
        size_t values[20];
        int numValues = 0;
        if ( cur ) {
            while (*cur && numValues<20) {
                values[numValues] = strtol(cur,&cur,10);
                ++numValues;
            }
            if (numValues > 6) numValues = 6;
            switch (numValues) {
            case 6: generation1MortalityPercent = values[5];
            case 5: generation1Kb = values[4];
            case 4: nurseryMortalityPercent = values[3];
            case 3: nurseryKb = values[2];
            case 2: spareCommitLimitMb = values[1];
            case 1: arenaMb = values[0];
                printf("CLASP_MPS_CONFIG...\n");
                printf("                    arenaMb = %lu\n", arenaMb);
                printf("         spareCommitLimitMb = %lu\n", spareCommitLimitMb);
                printf("                  nurseryKb = %lu\n", nurseryKb );
                printf("    nurseryMortalityPercent = %lu\n", nurseryMortalityPercent);
                printf("              generation1Kb = %lu\n", generation1Kb);
                printf("generation1MortalityPercent = %lu\n", generation1MortalityPercent);
                return true;
                break;
            default:
                break;
            };
        }
        return false;
    }
        

#define LENGTH(array)	(sizeof(array) / sizeof(array[0]))

    int initializeMemoryPoolSystem( MainFunctionType startupFn, int argc, char* argv[], bool mpiEnabled, int mpiRank, int mpiSize)
    {
        if ( Alignment() == 16 ) {
//            printf("%s:%d WARNING   Alignment is 16 - it should be 8 - check the Alignment() function\n!\n!\n!\n!\n",__FILE__,__LINE__);
        }
        global_sizeof_fwd = AlignUp(sizeof(Header_s)+sizeof(uintptr_t));
        global_alignup_sizeof_header = AlignUp(sizeof(Header_s));


#define CHAIN_SIZE 6400 // 256 // 6400
        size_t arenaSizeMb = 320;
        size_t spareCommitLimitMb = 320;
        size_t nurseryKb = CHAIN_SIZE;
        size_t nurseryMortalityPercent = 80;
        size_t generation1Kb = CHAIN_SIZE*4;
        size_t generation1MortalityPercent = 50;

        parseClaspMpsConfig( arenaSizeMb, spareCommitLimitMb, nurseryKb, nurseryMortalityPercent, generation1Kb, generation1MortalityPercent );

        double nurseryMortalityFraction = nurseryMortalityPercent/100.0;
        double generation1MortalityFraction = generation1MortalityPercent/100.0;

//        printf( "arenaSizeMb[%lu] spareCommitLimitMb[%lu] nurseryKb[%lu] nurseryMortalityFraction[%f] generation1Kb[%lu] generation1MortalityFraction[%f]\n", arenaSizeMb, spareCommitLimitMb, nurseryKb, nurseryMortalityFraction, generation1Kb, generation1MortalityFraction );

#define AMC_CHAIN_SIZE CHAIN_SIZE
        // Now the generation chain
        mps_gen_param_s gen_params[] = {
            { nurseryKb, nurseryMortalityFraction },   // { Nursery_size, Nursery_mortality }
            { generation1Kb, generation1MortalityFraction },   // { Generation1_size, Generation1_mortality }
        };

        mps_res_t res;
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args,MPS_KEY_ARENA_INCREMENTAL, 0 );
            MPS_ARGS_ADD(args,MPS_KEY_ARENA_SIZE, arenaSizeMb*1024*1024 );
            res = mps_arena_create_k(&_global_arena, mps_arena_class_vm(), args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create MPS arena");

        // David suggested this - it never gives back memory to the OS
        mps_arena_spare_commit_limit_set(_global_arena, spareCommitLimitMb*1024*1024 );

        mps_fmt_t obj_fmt;
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FMT_HEADER_SIZE, sizeof(Header_s) );
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, Alignment());
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, obj_scan);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
            res = mps_fmt_create_k(&obj_fmt, _global_arena, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create obj format");

        mps_chain_t only_chain;
        res = mps_chain_create(&only_chain,
                               _global_arena,
                               LENGTH(gen_params),
                               gen_params);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create amc chain");

        // Create the AMC pool
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, only_chain);
            MPS_ARGS_ADD(args, MPS_KEY_INTERIOR, 1);
            res = mps_pool_create_k(&_global_amc_pool, _global_arena, mps_class_amc(), args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create amc pool");


        /*! Use an AWL pool rather than and AMS pool until the AMS bug gets fixed */
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, only_chain);
            MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, dummyAwlFindDependent );
            res = mps_pool_create_k(&global_non_moving_pool, _global_arena, mps_class_awl(), args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create ams pool");
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
            res = mps_ap_create_k(&global_non_moving_ap, global_non_moving_pool, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create global_non_moving_ap");




        // Create the AMCZ pool
        mps_pool_t _global_amcz_pool;
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, only_chain);
            res = mps_pool_create_k(&_global_amcz_pool, _global_arena, mps_class_amcz(), args);
        } MPS_ARGS_END(args);



        mps_fmt_t weak_obj_fmt;
        MPS_ARGS_BEGIN(args) {
#ifndef RUNNING_GC_BUILDER
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, Alignment());
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, weak_obj_scan);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, weak_obj_skip);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, weak_obj_fwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, weak_obj_isfwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, weak_obj_pad);
#endif
            res = mps_fmt_create_k(&weak_obj_fmt, _global_arena, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create obj format");



        // Create the AWL pool for weak hash tables here
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, weak_obj_fmt);
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, only_chain);
            MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, awlFindDependent );
            res = mps_pool_create_k(&_global_awl_pool, _global_arena, mps_class_awl(), args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create awl pool");
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
            res = mps_ap_create_k(&_global_strong_link_allocation_point, _global_awl_pool, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create global_strong_link_allocation_point");
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_weak());
            res = mps_ap_create_k(&_global_weak_link_allocation_point, _global_awl_pool, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create global_weak_link_allocation_point");







        // And the allocation points
        res = mps_ap_create_k(&_global_automatic_mostly_copying_allocation_point,
                              _global_amc_pool, mps_args_none );
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create mostly_copying_allocation_point");

        // res = mps_ap_create_k(&_global_mvff_allocation_point, _global_mvff_pool, mps_args_none );
        // if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create mvff_allocation_point");

        res = mps_ap_create_k(&_global_automatic_mostly_copying_zero_rank_allocation_point,
                              _global_amcz_pool, mps_args_none );
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create mostly_copying_zero_rank_allocation_point");


        // register the current and only thread
        mps_thr_t global_thread;
        res = mps_thread_reg(&global_thread,_global_arena);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not register thread");

        // register the main thread stack scanner
        mps_root_t global_stack_root;
        res = mps_root_create_reg(&global_stack_root,
                                  _global_arena,
                                  mps_rank_ambig(),
                                  0,
                                  global_thread,
                                  mps_stack_scan_ambig,
                                  _global_stack_marker,
                                  0);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create stack root");



        /* Deal with finalization!!!! */
        mps_message_type_enable(_global_arena, mps_message_type_finalization());
        mps_message_type_enable(_global_arena, mps_message_type_gc());
        mps_message_type_enable(_global_arena, mps_message_type_gc_start());

        // register the main thread roots in static and heap space

//#define TEST_MPS        1

        int exit_code = 0;

        mps_root_t global_scan_root;
        res = mps_root_create(&global_scan_root,
                              _global_arena,
                              mps_rank_exact(),
                              0,
                              main_thread_roots_scan,
                              NULL,
                              0);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create scan root");

        registerLoadTimeValuesRoot(&globalRunTimeValues);

        exit_code = startupFn(argc,argv,mpiEnabled,mpiRank,mpiSize);

        processMpsMessages();

        mps_root_destroy(global_scan_root);
        mps_root_destroy(global_stack_root);
        mps_thread_dereg(global_thread);
        mps_ap_destroy(_global_automatic_mostly_copying_zero_rank_allocation_point);
        mps_ap_destroy(_global_automatic_mostly_copying_allocation_point);
        mps_ap_destroy(_global_weak_link_allocation_point);
        mps_ap_destroy(_global_strong_link_allocation_point);
        mps_pool_destroy(_global_awl_pool);
        mps_pool_destroy(_global_amcz_pool);
        mps_ap_destroy(global_non_moving_ap);
        mps_pool_destroy(global_non_moving_pool);
        mps_pool_destroy(_global_amc_pool);
        mps_arena_park(_global_arena);
        mps_chain_destroy(only_chain);
        mps_fmt_destroy(weak_obj_fmt);
        mps_fmt_destroy(obj_fmt);
        mps_arena_destroy(_global_arena);

        return exit_code;

    };



    
    
    





};
