#include <boost/mpl/list.hpp>

#include <stdint.h>

#include "core/foundation.h"
#include "core/object.h"
#include "core/lisp.h"
#include "core/builtInClass.h"
#include "core/fileSystem.h"
#include "core/environment.h"
#include "core/standardClass.h"
#include "core/activationFrame.h"
#include "core/structureClass.h"
#include "core/str.h"
#include "symbolTable.h"
#include "gctoolsPackage.h"
#include "core/wrappers.h"




using namespace core;


namespace gctools
{

    bool _GlobalDebugAllocations = false;



    
#define ARGS_af_testVec0 "()"
#define DECL_af_testVec0 ""
#define DOCS_af_testVec0 "testVec0"
    void af_testVec0()
    {_G();
        int N = 4;
        printf("Creating vec(4)\n");
        gctools::Vec0<TestingClass> v;
        v.resize(4);
        vector_dump(v);
        printf("About to Push_back TestingClass(2)\n");
        v.push_back(TestingClass(2));
        vector_dump(v);
        printf("About to Push_back %d times TestingClass(i)\n", N);
        for ( int i(0); i<N; ++i ) {v.push_back(TestingClass(i));}
        vector_dump(v); 
        printf("About to Push_back %d times TestingClass(i+10)\n", N);
        for ( int i(0); i<N; ++i )
        {
            v.push_back(TestingClass(i+10));
            vector_dump(v,"step");
        }
        vector_dump(v,"done"); 
        printf("Start again");
        gctools::Vec0<TestingClass> u;
        u.resize(4);
        vector_dump(u,"new");
        u.resize(8,TestingClass(-8));
        vector_dump(u,"resized to 8 ");
        u.resize(16,TestingClass(-16));
        vector_dump(u,"resized to 16");
        u.resize(4,TestingClass(-99999));
        vector_dump(u,"resized to 4 ");
        printf("Test emplace and erase\n");
        gctools::Vec0<TestingClass> w;
        for ( int zi(0); zi<20; ++zi ) {
            w.emplace(w.begin(),zi);
        }
        vector_dump(w,"after 10 emplace");
        w.erase(w.begin()+4);
        vector_dump(w,"removed begin()+4");
        for ( int zz(0); zz<5; ++zz ) w.erase(w.begin()+4);
        vector_dump(w,"removed begin()+4 4 times");
        w.erase(w.begin()+13);
        vector_dump(w,"removed begin()+13 times");
    }


#define ARGS_af_testArray0 "()"
#define DECL_af_testArray0 ""
#define DOCS_af_testArray0 "testArray0"
    void af_testArray0()
    {_G();
        int N = 4;
        printf("Creating Array0(4)\n");
        gctools::Array0<core::T_sp> v;
        v.allocate(4,_Nil<T_O>());
        Array0_dump(v,"Nil*4");
        gctools::Array0<core::T_sp> w;
        w.allocate(4,_Nil<T_O>());
        for (int i(0); i<4; ++i ) {
            w[i] = core::Fixnum_O::create(i);
        }
        Array0_dump(w,"Fixnum*4");
        printf("Creating ValueFrame\n");
        core::ValueFrame_sp vf = ValueFrame_O::create_fill_args(_Nil<core::ActivationFrame_O>(),core::Fixnum_O::create(1), core::Fixnum_O::create(2), core::Fixnum_O::create(3));
        printf("Creating another 1 ValueFrame\n");
        vf = ValueFrame_O::create_fill_args(_Nil<core::ActivationFrame_O>(),core::Fixnum_O::create(1), core::Fixnum_O::create(2), core::Fixnum_O::create(3));
        printf("Creating another 2 ValueFrame\n");
        vf = ValueFrame_O::create_fill_args(_Nil<core::ActivationFrame_O>(),core::Fixnum_O::create(1), core::Fixnum_O::create(2), core::Fixnum_O::create(3));
        printf("Leaving scope\n");
    }

};

#ifdef USE_BOEHM

struct ReachableClass {
    ReachableClass() : typeidName(NULL) {};
    ReachableClass(const char* tn) : typeidName(tn), instances(0), totalSize(0) {}
    void update(size_t sz) { ++this->instances; this->totalSize += sz;};
    const char* typeidName;
    size_t      instances;
    size_t      totalSize;
    size_t print(const std::string& shortName) {
        printf("%s: total_size: %10lu count: %8lu avg.sz: %8lu %s\n", shortName.c_str(),
               this->totalSize, this->instances, this->totalSize/this->instances, this->typeidName );
        return this->totalSize;
    }
};

struct ReachableContainer {
    ReachableContainer() : typeidName(NULL) {};
    ReachableContainer(const char* tn) : typeidName(tn), instances(0), totalSize(0), largest(0) {}
    void update(size_t sz) {
        ++this->instances;
        this->totalSize += sz;
        if (sz > this->largest) {
            this->largest = sz;
        }
    };
    const char* typeidName;
    size_t      instances;
    size_t      totalSize;
    size_t      largest;
    size_t print(const std::string& shortName) {
        printf("%s: total_size: %10lu count: %8lu  avg.sz: %8lu  largest: %8lu %s\n", shortName.c_str(),
               this->totalSize, this->instances, this->totalSize/this->instances, this->largest, this->typeidName );
        return this->totalSize;
    }

};

    typedef map<const char*,ReachableClass>    ReachableClassMap;
    typedef map<const char*,ReachableContainer> ReachableContainerMap;
    static ReachableClassMap*     static_ReachableClassKinds;
    static ReachableClassMap*     static_ReachableLispKinds;
    static ReachableContainerMap* static_ReachableContainerKinds;
    static ReachableContainerMap* static_ReachableStringKinds;
static size_t invalidHeaderTotalSize = 0;
int globalSearchMarker = 0;
extern "C" {    
    extern void GC_mercury_enumerate_reachable_objects();
    void GC_callback_reachable_object(void* ptr, size_t sz)
    {
        sz <<= 2; // convert to bytes
        gctools::Header_s* h = reinterpret_cast<gctools::Header_s*>(ptr);
        if (h->isValid() ) {
            if ( h->markerMatches(globalSearchMarker)) {
                switch (h->kind()) {
                case gctools::BoehmClassKind: {
                    ReachableClassMap::iterator it = static_ReachableClassKinds->find(h->name());
                    if ( it == static_ReachableClassKinds->end() ) {
                        ReachableClass reachableClass(h->name());
                        reachableClass.update(sz);
                        (*static_ReachableClassKinds)[h->name()] = reachableClass;
                    } else {
                        it->second.update(sz);
                    }
                    break;
                }
                case gctools::BoehmLispKind: {
                    ReachableClassMap::iterator it = static_ReachableLispKinds->find(h->name());
                    if ( it == static_ReachableLispKinds->end() ) {
                        ReachableClass reachableClass(h->name());
                        reachableClass.update(sz);
                        (*static_ReachableLispKinds)[h->name()] = reachableClass;
                    } else {
                        it->second.update(sz);
                    }
                    break;
                }
                case gctools::BoehmContainerKind: {
                    ReachableContainerMap::iterator it = static_ReachableContainerKinds->find(h->name());
                    if ( it == static_ReachableContainerKinds->end() ) {
                        ReachableContainer reachableContainer(h->name());
                        reachableContainer.update(sz);
                        (*static_ReachableContainerKinds)[h->name()] = reachableContainer;
                    } else {
                        it->second.update(sz);
                    }
                    break;
                }
                case gctools::BoehmStringKind: {
                    ReachableContainerMap::iterator it = static_ReachableStringKinds->find(h->name());
                    if ( it == static_ReachableStringKinds->end() ) {
                        ReachableContainer reachableContainer(h->name());
                        reachableContainer.update(sz);
                        (*static_ReachableStringKinds)[h->name()] = reachableContainer;
                    } else {
                        it->second.update(sz);
                    }
                    break;
                }
                }
            }
        } else {
            invalidHeaderTotalSize += sz;
        }
    }
};


template <typename T>
size_t dumpResults(const std::string& name, const std::string& shortName, T* data)
{
    printf("-------------------- %s -------------------\n", name.c_str() );
    typedef typename T::value_type::second_type value_type;
    vector<value_type> values;
    for (auto it : *data ) {
        values.push_back(it.second);
    }
    size_t totalSize(0);
    sort(values.begin(), values.end(), [](value_type& x, value_type& y) {
            return (x.totalSize > y.totalSize);
        });
    for ( auto it : values ) {
        totalSize += it.print(shortName);
    }
    return totalSize;
}


#endif

namespace gctools {


#define ARGS_af_gcInfo "(&optional x (marker 0))"
#define DECL_af_gcInfo ""
#define DOCS_af_gcInfo "gcInfo - Return info about the reachable objects"
    T_mv af_gcInfo(T_sp x, Fixnum_sp marker)
    {_G();
#ifdef USE_MPS
        return Values(_Nil<core::T_O>());
#endif
#ifdef USE_BOEHM
        return Values(_Nil<core::T_O>());
#endif
    };



    
    
#define ARGS_af_gcMarker "(&optional marker)"
#define DECL_af_gcMarker ""
#define DOCS_af_gcMarker "gcMarker"
    int af_gcMarker(Fixnum_sp marker)
    {_G();
#ifdef USE_BOEHM_MEMORY_MARKER
        if ( marker.nilp() ) {
            return gctools::globalBoehmMarker;
        }
        int oldm = gctools::globalBoehmMarker;
        int m = marker->get();
        gctools::globalBoehmMarker = m;
        return oldm;
#endif
        return 0;
    }


#define ARGS_af_gcReachableObjects "(&optional x (marker 0) msg)"
#define DECL_af_gcReachableObjects ""
#define DOCS_af_gcReachableObjects "gcReachableObjects - Return info about the reachable objects.  x can be T, nil, :default - as in ROOM.  marker can be a fixnum (0 - matches everything, any other number/only objects with that marker)"
    T_mv af_gcReachable(T_sp x, Fixnum_sp marker, Str_sp msg)
    {_G();
        string smsg = "Total";
        if ( msg.notnilp() ) {
        smsg = msg->get();
    }
#ifdef USE_MPS
        return Values(_Nil<T_O>());
#endif
#ifdef USE_BOEHM
        globalSearchMarker = marker->get();
        static_ReachableClassKinds = new(ReachableClassMap);
        static_ReachableLispKinds = new(ReachableClassMap);
        static_ReachableContainerKinds = new(ReachableContainerMap);
        static_ReachableStringKinds = new(ReachableContainerMap);
        invalidHeaderTotalSize = 0;
        GC_mercury_enumerate_reachable_objects();
        printf("Walked LispKinds\n");
        size_t totalSize(0);
        totalSize += dumpResults("Reachable StringKinds", "strs", static_ReachableStringKinds);
        totalSize += dumpResults("Reachable ContainerKinds",  "cont", static_ReachableContainerKinds);
        totalSize += dumpResults("Reachable LispKinds", "lisp", static_ReachableLispKinds);
        totalSize += dumpResults("Reachable ClassKinds", "class", static_ReachableClassKinds);
        printf("Done walk of memory  %lu ClassKinds   %lu LispKinds   %lu ContainerKinds   %lu StringKinds\n",
            static_ReachableClassKinds->size(),
            static_ReachableLispKinds->size(),
            static_ReachableContainerKinds->size(),
            static_ReachableStringKinds->size() );
        printf("%s invalidHeaderTotalSize = %12lu\n", smsg.c_str(), invalidHeaderTotalSize );
        printf("%s memory usage (bytes):    %12lu\n", smsg.c_str(), totalSize );
        printf("%s GC_get_heap_size()       %12lu\n", smsg.c_str(),GC_get_heap_size() );
        printf("%s GC_get_free_bytes()      %12lu\n", smsg.c_str(),GC_get_free_bytes() );
        printf("%s GC_get_bytes_since_gc()  %12lu\n", smsg.c_str(),GC_get_bytes_since_gc() );
        printf("%s GC_get_total_bytes()     %12lu\n", smsg.c_str(),GC_get_total_bytes() );

        delete static_ReachableClassKinds;
        delete static_ReachableLispKinds;
        delete static_ReachableContainerKinds;
        delete static_ReachableStringKinds;
        return Values(_Nil<core::T_O>());
#endif
    };


#define ARGS_af_garbageCollect "()"
#define DECL_af_garbageCollect ""
#define DOCS_af_garbageCollect "garbageCollect"
    void af_garbageCollect()
    {_G();
#ifdef USE_BOEHM
        GC_gcollect();
#endif
//        printf("%s:%d Starting garbage collection of arena\n", __FILE__, __LINE__ );
#ifdef USE_MPS
        mps_arena_collect(_global_arena);
        processMpsMessages();
#endif
//        printf("Garbage collection done\n");
    };


#define ARGS_af_cleanup "()"
#define DECL_af_cleanup ""
#define DOCS_af_cleanup "cleanup"
    void af_cleanup()
    {_G();
#ifdef USE_MPS
        int messages = processMpsMessages();
#endif
    };


    
    
#define ARGS_af_debugAllocations "(arg)"
#define DECL_af_debugAllocations ""
#define DOCS_af_debugAllocations "debugAllocations"
    void af_debugAllocations(T_sp debugOn)
    {_G();
        _GlobalDebugAllocations = debugOn.isTrue();
    };


    
#ifdef USE_GC_REF_COUNT_WRAPPER    
#define ARGS_af_gcheader "(addr &optional (msg \"\") )"
#define DECL_af_gcheader ""
#define DOCS_af_gcheader "gcheader"
    void af_gcheader(core::Pointer_sp addr, const string& msg)
    {_G();
        GCWrapper<core::T_O>::GCHeader* gch = reinterpret_cast<GCWrapper<core::T_O>::GCHeader*>(addr->ptr());
//        printf("%s Kind = %lu   RefCount=%d\n", msg.c_str(), gch->_Kind, gch->_ReferenceCount);
    };



    
    
#define ARGS_af_gcaddress "(obj)"
#define DECL_af_gcaddress ""
#define DOCS_af_gcaddress "gcaddress"
    core::Pointer_sp af_gcaddress(core::T_sp obj)
    {_G();
        T_O* ptr = dynamic_cast<T_O*>(obj.get());
        void* hptr = reinterpret_cast<void*>(GCWrapper<core::T_O>::gcHeader(ptr));
        core::Pointer_sp po = core::Pointer_O::create(hptr);
        return po;
    };




    
    
#define ARGS_af_testReferenceCounting "()"
#define DECL_af_testReferenceCounting ""
#define DOCS_af_testReferenceCounting "testReferenceCounting"
    void af_testReferenceCounting()
    {_G();
        core::Pointer_sp p;
        {
            core::Fixnum_sp fn = core::Fixnum_O::create(20);
            p = af_gcaddress(fn);
            af_gcheader(p,"Start");
            core::Fixnum_sp fn2 = fn;
            af_gcheader(p,"Assignment");
            fn2 = fn2;
            af_gcheader(p,"fn2 = fn2");
            fn2 = core::Fixnum_O::create(1);
            af_gcheader(p,"fn2 = Fixnum_O::create(1)");
            fn = core::Fixnum_O::create(0);
//            af_gcheader(p,"fn = Fixnum_O::create(0)");
        }
//        af_gcheader(p,"fn went out of scope");
    };
#endif




// -----------------------------------------------------------
// -----------------------------------------------------------
// -----------------------------------------------------------

/*! Hardcode a few kinds of objects for bootstrapping
 *
 *
 *
 *
 */

    // 
    // It would be cool if I modified the static analyzer to
    // search for the following global variable and extract
    // the bootstrap classes from its template parameter list
    //
    template <int SpeciesNumber, typename...ARGS>
    struct HardwiredSpeciesKinds {};

    HardwiredSpeciesKinds<0                             // SpeciesNumber = 0
                          , core::T_O                   // Kind = 1
                          , core::StandardObject_O      // 2
                          , core::Metaobject_O          // 3
                          , core::Specializer_O         // 4
                          , core::Class_O               // 5
                          , core::BuiltInClass_O        // 6
                          , core::StdClass_O            // 7
                          , core::StandardClass_O       // 8
                          , core::Symbol_O              // 9
                          , core::Str_O                 // 10
                          >    globalHardwiredSpeciesKinds;


    const char* global_HardcodedKinds[] = {
        ""
        , "core::T_O"
        , "core::StandardObject_O"
        , "core::Metaobject_O"
        , "core::Specializer_O"
        , "core::Class_O"
        , "core::BuiltInClass_O"
        , "core::StdClass_O"
        , "core::StandardClass_O"
        , "core::StructureClass_O"
        , "core::Symbol_O"
        , "core::Str_O"
    };

#define ARGS_af_maxBootstrapKinds "()"
#define DECL_af_maxBootstrapKinds ""
#define DOCS_af_maxBootstrapKinds "maxBootstrapKinds"
    int af_maxBootstrapKinds()
    {_G();
        return sizeof(global_HardcodedKinds)/sizeof(global_HardcodedKinds[0]);
    }


    int iBootstrapKind(const string& name)
    {
        for ( int i(0), iEnd(af_maxBootstrapKinds()); i<iEnd; ++i )
        {
//            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
            if (strcmp(global_HardcodedKinds[i],name.c_str()) == 0 ) {
                return i;
            }
        }
        SIMPLE_ERROR(BF("Illegal bootstrap-kind %s") % name );
    }

    void initialize_bootstrap_kinds()
    {
        DEPRECIATED();
        // Hard-coded bootstrap kinds
#define SetupKind(_x_) _x_::static_Kind = iBootstrapKind(#_x_)
        SetupKind(core::T_O);
        SetupKind(core::StandardObject_O);
        SetupKind(core::Metaobject_O);
        SetupKind(core::Specializer_O);
        SetupKind(core::Class_O);
        SetupKind(core::BuiltInClass_O);
        SetupKind(core::StdClass_O);
        SetupKind(core::StandardClass_O);
        SetupKind(core::StructureClass_O);
        SetupKind(core::Symbol_O);
        SetupKind(core::Str_O);
    }


    
    



#define ARGS_af_bootstrapKindSymbols "()"
#define DECL_af_bootstrapKindSymbols ""
#define DOCS_af_bootstrapKindSymbols "bootstrapKindSymbols"
    core::Cons_sp af_bootstrapKindSymbols()
    {_G();
        core::Cons_sp list(_Nil<core::Cons_O>());
        for ( int i(af_maxBootstrapKinds()-1); i>0; --i ) {
            string name = global_HardcodedKinds[i];
            list = core::Cons_O::create(core::Str_O::create(name),list);
        }
        return list;
    }



    
    
#define ARGS_af_bootstrapKindP "(arg)"
#define DECL_af_bootstrapKindP ""
#define DOCS_af_bootstrapKindP "bootstrap-kind-p return a generalized boolean of the bootstrap-kind - either the boostrap kind index or nil"
    core::T_sp af_bootstrapKindP(const string& name)
    {_G();
        for ( int i(0), iEnd(af_maxBootstrapKinds()); i<iEnd; ++i )
        {
//            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
            if (strcmp(global_HardcodedKinds[i],name.c_str()) == 0 ) {
                return core::Fixnum_O::create(i);
            }
        }
        return _Nil<core::T_O>();
    }





#pragma GCC visibility push(default)
#define GcToolsPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkgName,lispName,export) core::Symbol_sp cname = UNDEFINED_SYMBOL;
#include "symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GcToolsPkg_SYMBOLS
#pragma GCC visibility pop



    void GcToolsExposer::expose(core::Lisp_sp lisp,core::Exposer::WhatToExpose what) const
    {_G();
	switch (what)
	{
	case candoClasses:
	{


#define GcToolsPkg_SYMBOLS
#define DO_SYMBOL(cname,idx,pkg,lispname,exportp) {gctools::cname = _lisp->internWithPackageName(pkg,lispname); gctools::cname->exportYourself(exportp);}
#include "gctools/symbols_scraped_inc.h"
#undef DO_SYMBOL
#undef GcToolsPkg_SYMBOLS


	}	
	break;
	case candoFunctions:
	{       
            


            SYMBOL_EXPORT_SC_(GcToolsPkg,garbageCollect);
            SYMBOL_EXPORT_SC_(GcToolsPkg,maxBootstrapKinds);
            SYMBOL_EXPORT_SC_(GcToolsPkg,bootstrapKindsP);
            SYMBOL_EXPORT_SC_(GcToolsPkg,bootstrapKindSymbols);
            core::af_def(GcToolsPkg,"testVec0",&af_testVec0);
            core::af_def(GcToolsPkg,"testArray0",&af_testArray0);
            core::af_def(GcToolsPkg,"gcInfo",&af_gcInfo);
            core::af_def(GcToolsPkg,"gcMarker",&af_gcMarker, ARGS_af_gcMarker, DECL_af_gcMarker, DOCS_af_gcMarker);
            core::af_def(GcToolsPkg,"gcReachableObjects",&af_gcReachable,ARGS_af_gcReachableObjects, DECL_af_gcReachableObjects, DOCS_af_gcReachableObjects);
            core::af_def(GcToolsPkg,"garbageCollect",&af_garbageCollect);
            core::af_def(GcToolsPkg,"cleanup",&af_cleanup);
            core::af_def(GcToolsPkg,"maxBootstrapKinds",&af_maxBootstrapKinds);
            core::af_def(GcToolsPkg,"bootstrapKindP",&af_bootstrapKindP);
            core::af_def(GcToolsPkg,"bootstrapKindSymbols",&af_bootstrapKindSymbols);






//	    SYMBOL_EXPORT_SC_(GcTools,linkExternalGlobalsInModule);
//	    Defun(linkExternalGlobalsInModule);
            Defun(debugAllocations);
#ifdef USE_GC_REF_COUNT_WRAPPER
            Defun(gcheader);
            Defun(gcaddress);
            Defun(testReferenceCounting);
#endif
	    //nothing
	};
	break;
	case candoGlobals:
	{
	};
	break;
	case pythonClasses:
	case pythonFunctions:
	case pythonGlobals:
	{
	    IMPLEMENT_ME();
	}
	break;
	}
    }


};





#if USE_INTRUSIVE_SMART_PTR==1
#define EXPAND_CLASS_MACROS
#define _CLASS_MACRO(_U_)				\
    STATIC_CLASS_INFO(_U_);			\
    INTRUSIVE_POINTER_REFERENCE_COUNT_ACCESSORS(_U_)
#include "gctools_initClasses_inc.h"
#undef _CLASS_MACRO
#undef EXPAND_CLASS_MACROS
#endif

