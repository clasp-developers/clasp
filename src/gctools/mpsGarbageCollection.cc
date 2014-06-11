


#include "core/foundation.h"
#include "core/object.h"
#include "core/numbers.h"
#include "core/str.h"
#include "core/builtInClass.h"
#include "core/posixTime.cc"
#include "core/standardClass.h"
#include "core/structureClass.h"
#include "core/wrappers.h"


extern "C" {
#include "mps/code/mpscams.h"  // AMS pool
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


/* --------------------------------------------------
   --------------------------------------------------
   --------------------------------------------------
   Global variables for MPS
   --------------------------------------------------
   --------------------------------------------------
*/

    mps_arena_t	_global_arena;
    void* _global_stack_marker;
    mps_pool_t _global_ams_pool;
    mps_pool_t _global_amc_pool;
    mps_pool_t _global_amcz_pool;
    mps_pool_t _global_awl_pool;
    mps_ap_t _global_automatic_weak_link_allocation_point;
    mps_ap_t _global_automatic_mostly_copying_zero_rank_allocation_point;


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

    static void obj_fwd(mps_addr_t old_base, mps_addr_t new_base)
    {
	MPS_LOG(BF("old_base = %p   new_base = %p") % old_base % new_base);
	mps_addr_t limit = obj_skip(old_base);
	size_t size = (char *)limit - (char *)old_base;
//	mps_addr_t obj_ptr = BASE_TO_OBJ_PTR(old_base);
	assert(size >= AlignUp(sizeof_with_header<Fwd2_s>()));
	if (size == AlignUp(sizeof_with_header<Fwd2_s>())) {
//	    printf("%s:%d  old_base=%p  size=%lu  ALIGN_UP(sizeof_with_header<Fwd2_s>())=%lu\n", __FILE__,__LINE__, old_base, size, ALIGN_UP(sizeof_with_header<Fwd2_s>()));
	    THROW_HARD_ERROR(BF("KIND_fwd2 should not be used in obj_fwd"));
//	    GcFwd2* obj2 = reinterpret_cast<GcFwd2*>(obj_ptr);
//	    BASE_PTR_KIND(old_base) = KIND_fwd2;
//	    obj2->_Fwd = ((obj_t)new_base);
	} else {
//	    GcFwd* obj = reinterpret_cast<GcFwd*>(obj_ptr);
//	    BASE_PTR_KIND(old_base) = KIND_fwd;
	    Header_s* header = reinterpret_cast<Header_s*>(old_base);
	    header->fwd._Kind = KIND_SYSTEM_fwd;
	    header->fwd._Fwd = ((Header_t)new_base);
	    header->fwd._Size = size;
	}
    }



    static mps_addr_t obj_isfwd(mps_addr_t addr)
    {
	MPS_LOG(BF(" addr = %p") % addr );
	Header_s* header = reinterpret_cast<Header_s*>(addr);
	switch (header->kind._Kind) {
	case KIND_SYSTEM_fwd2: {
	    THROW_HARD_ERROR(BF("KIND_fwd2 should never be needed"));
//	    GcFwd2* obj2 = reinterpret_cast<GcFwd2*>(obj_ptr);
//	    MPS_LOG(BF("              KIND_fwd2 returning %p") % obj2->_Fwd );
//	    return obj2->_Fwd;
	}
	case KIND_SYSTEM_fwd: {
//	    GcFwd* obj = reinterpret_cast<GcFwd*>(obj_ptr);
	    MPS_LOG(BF("              KIND_fwd returning %p") % header->fwd._Fwd );
	    return header->fwd._Fwd;
	}
	default: {
	    // do nothing - fall through
	}
	}
	MPS_LOG(BF("               Not fwd returning NULL  kind = %s") % obj_name(header->kind._Kind));
	return NULL;
    }



    static void obj_pad(mps_addr_t base, size_t size)
    {
	size_t alignment = Alignment();
//	size_t sizeofPad1_s = sizeof_with_header<Pad1_s>();
//	size_t align_sizeofPad1_s = ALIGN(sizeofPad1_s);
//	size_t align_up_sizeofPad1_s = ALIGN_UP(sizeofPad1_s);
	size_t sizeof_Header_s = sizeof(Header_s);
	size_t align_up_sizeof_Header_s = AlignUp(sizeof_Header_s);
	MPS_LOG(BF("base = %p ALIGNMENT = %d size=%lu  ALIGN_UP(sizeof_with_header<Pad1_s>()): %d") % base % alignment % size % AlignUp(sizeof_with_header(Pad1_s)));
	MPS_LOG(BF("sizeof_Header_s = %lu   align_up_sizeof_Header_s = %lu") % sizeof_Header_s % align_up_sizeof_Header_s );
	assert(size >= AlignUp(sizeof_with_header<Pad1_s>()));
	Header_s* header = reinterpret_cast<Header_s*>(base);
	if (size == AlignUp(sizeof_with_header<Pad1_s>())) {
	    header->pad1._Kind = KIND_SYSTEM_pad1;
	} else {
//	    BASE_PTR_KIND(base) = KIND_pad;
//	    mps_base_t obj_ptr = BASE_TO_OBJ_PTR(base);
	    header->pad._Kind = KIND_SYSTEM_pad;
	    header->pad._Size = size;
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
        core::Number_sp startTime = core::cl_getInternalRunTime().as<core::Number_O>();
        mps_message_type_t type;
        while (mps_message_queue_type(&type, gctools::_global_arena)) {
            mps_message_t message;
            mps_bool_t b;
            b = mps_message_get(&message, gctools::_global_arena, type);
            assert(b); /* we just checked there was one */
            ++messages;
            if (type == mps_message_type_gc_start()) {
#if 0
                printf("Message: mps_message_type_gc_start\n");
                printf("Collection started.\n");
                printf("  Why: %s\n", mps_message_gc_start_why(_global_arena, message));
                printf("  clock: %lu\n", (unsigned long)mps_message_clock(_global_arena, message));
#endif
            } else if ( type == mps_message_type_gc() ) {
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
//        printf("%s:%d Leaving processMpsMessages\n",__FILE__,__LINE__);
        core::Number_sp endTime = core::cl_getInternalRunTime().as<core::Number_O>();
        core::Number_sp deltaTime = core::contagen_sub(endTime,startTime);
        core::Number_sp deltaSeconds = core::contagen_div(deltaTime,cl::_sym_internalTimeUnitsPerSecond->symbolValue().as<core::Number_O>());
        printf("[processMpsMessages %s seconds]\n", _rep_(deltaSeconds).c_str());
        fflush(stdout);
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





/* -------------------------------------------------- */

    struct mps_fmt_auto_header_s default_obj_fmt_s = {
        Alignment(),
        obj_scan,
        obj_skip,
        obj_fwd,
        obj_isfwd,
        obj_pad,
        0 // ALIGN(sizeof(Header_s))
    };



#define LENGTH(array)	(sizeof(array) / sizeof(array[0]))

    int initializeMemoryPoolSystem( MainFunctionType startupFn, int argc, char* argv[], mps_fmt_auto_header_s* obj_fmt_sP, bool mpiEnabled, int mpiRank, int mpiSize)
    {

        // Create the object format
        if ( obj_fmt_sP != NULL )
        {
            THROW_HARD_ERROR(BF("Handle obj_fmt_sP != NULL"));
        }

        void* local_stack_marker;
        _global_stack_marker = &local_stack_marker;

        mps_res_t res;
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args,MPS_KEY_ARENA_SIZE, 10 * 32 * 1024 * 1024 );
            res = mps_arena_create_k(&_global_arena, mps_arena_class_vm(), args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create MPS arena");
        
        mps_fmt_t obj_fmt;
        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, Alignment());
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, obj_scan);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, obj_skip);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD, obj_fwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, obj_isfwd);
            MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, obj_pad);
            res = mps_fmt_create_k(&obj_fmt, _global_arena, args);
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create obj format");


#define AMC_CHAIN_SIZE 16
        // Now the generation chain
        mps_gen_param_s amc_gen_params[] = {
            { AMC_CHAIN_SIZE, 0.85 },
            { AMC_CHAIN_SIZE, 0.45 },
        };

        mps_chain_t amc_chain;
        res = mps_chain_create(&amc_chain,
                               _global_arena,
                               LENGTH(amc_gen_params),
                               amc_gen_params);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create obj chain");



        // And then the pool
        mps_pool_t _global_amc_pool;
        res = mps_pool_create(&_global_amc_pool,
                              _global_arena,
                              mps_class_amc(),
                              obj_fmt,
                              amc_chain);


#define AMS_CHAIN_SIZE 20480
        // Now the generation chain
        mps_gen_param_s ams_gen_params[] = {
            { AMS_CHAIN_SIZE, 0.85 },
            { AMS_CHAIN_SIZE, 0.45 },
        };

        mps_chain_t ams_chain;
        res = mps_chain_create(&ams_chain,
                               _global_arena,
                               LENGTH(ams_gen_params),
                               ams_gen_params);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create obj chain");


#ifdef DEBUG_MPS_AMS_POOL
        mps_pool_debug_option_s debug_options = {
            (const void*)"postpost", 8,
            (const void*)"freefree", 8,
        };
#endif

#undef DEBUG_MPS_AMS_POOL
        MPS_ARGS_BEGIN(args) {
#ifdef DEBUG_MPS_AMS_POOL
            MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options );
#endif
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, ams_chain);
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
            MPS_ARGS_ADD(args, MPS_KEY_AMS_SUPPORT_AMBIGUOUS, 1);
#ifdef DEBUG_MPS_AMS_POOL
            res = mps_pool_create_k(&_global_ams_pool, _global_arena, mps_class_ams_debug(), args);
#else
            res = mps_pool_create_k(&_global_ams_pool, _global_arena, mps_class_ams(), args);
#endif
        } MPS_ARGS_END(args);
        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Could not create ams pool");

        // And the allocation point
        res = mps_ap_create_k(&_global_automatic_mostly_copying_allocation_point, _global_amc_pool, mps_args_none );
        res = mps_ap_create_k(&_global_automatic_mark_sweep_allocation_point, _global_ams_pool, mps_args_none );


        IMPLEMENT_MEF(BF("Setup the AMCZ and AWL pools"));


        if (res != MPS_RES_OK) GC_RESULT_ERROR(res,"Couldn't create obj allocation point");

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

        exit_code = startupFn(argc,argv,mpiEnabled,mpiRank,mpiSize);

        mps_root_destroy(global_scan_root);

        processMpsMessages();

        mps_root_destroy(global_stack_root);
        mps_thread_dereg(global_thread);
        mps_ap_destroy(_global_automatic_mark_sweep_allocation_point);
        mps_ap_destroy(_global_automatic_mostly_copying_allocation_point);
        mps_pool_destroy(_global_ams_pool);
        mps_pool_destroy(_global_amc_pool);
        mps_chain_destroy(ams_chain);
        mps_chain_destroy(amc_chain);
        mps_fmt_destroy(obj_fmt);
        mps_arena_destroy(_global_arena);

        return exit_code;

    };



    
    
    





};



