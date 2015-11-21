/* -^- */
#include <boost/mpl/list.hpp>
#ifdef USE_BOEHM
#include <clasp/gc/gc_mark.h>
#endif
#ifdef USE_MPS
extern "C" {
#include <clasp/mps/code/mpscamc.h>
};
#endif

#include <stdint.h>

#include <clasp/core/foundation.h>
#include <clasp/core/object.h>
#include <clasp/core/lisp.h>
#include <clasp/core/builtInClass.h>
#include <clasp/core/fileSystem.h>
#include <clasp/core/environment.h>
#include <clasp/core/standardClass.h>
#include <clasp/core/activationFrame.h>
#include <clasp/core/structureClass.h>
#include <clasp/core/str.h>
#include <clasp/gctools/symbolTable.h>
#include <clasp/gctools/gctoolsPackage.h>
#include <clasp/core/wrappers.h>

namespace gctools {

using namespace core;

/*! Hardcode a few kinds of objects for bootstrapping
 */

const char *global_HardcodedKinds[] = {
    "", "core::T_O", "core::StandardObject_O", "core::Metaobject_O", "core::Specializer_O", "core::Class_O", "core::BuiltInClass_O", "core::StdClass_O", "core::StandardClass_O", "core::StructureClass_O", "core::Symbol_O", "core::Str_O"};

#define ARGS_af_maxBootstrapKinds "()"
#define DECL_af_maxBootstrapKinds ""
#define DOCS_af_maxBootstrapKinds "maxBootstrapKinds"
int af_maxBootstrapKinds() {
  _G();
  return sizeof(global_HardcodedKinds) / sizeof(global_HardcodedKinds[0]);
}

int iBootstrapKind(const string &name) {
  for (int i(0), iEnd(af_maxBootstrapKinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return i;
    }
  }
  SIMPLE_ERROR(BF("Illegal bootstrap-kind %s") % name);
}

void initialize_bootstrap_kinds() {
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
core::Cons_sp af_bootstrapKindSymbols() {
  _G();
  core::Cons_sp list(_Nil<core::Cons_O>());
  for (int i(af_maxBootstrapKinds() - 1); i > 0; --i) {
    string name = global_HardcodedKinds[i];
    list = core::Cons_O::create(core::Str_O::create(name), list);
  }
  return list;
}

#define ARGS_af_bootstrapKindP "(arg)"
#define DECL_af_bootstrapKindP ""
#define DOCS_af_bootstrapKindP "bootstrap-kind-p return a generalized boolean of the bootstrap-kind - either the boostrap kind index or nil"
core::T_sp af_bootstrapKindP(const string &name) {
  _G();
  for (int i(0), iEnd(af_maxBootstrapKinds()); i < iEnd; ++i) {
    //            printf("%s:%d i[%d]Checking if %s == %s\n", __FILE__, __LINE__, i, global_HardcodedKinds[i], name.c_str());
    if (strcmp(global_HardcodedKinds[i], name.c_str()) == 0) {
      return core::make_fixnum(i);
    }
  }
  return _Nil<core::T_O>();
}



#define ARGS_gc_deallocate_unmanaged_instance "()"
#define DECL_gc_deallocate_unmanaged_instance ""
#define DOCS_gc_deallocate_unmanaged_instance "Deallocate the memory for unmanaged instances and do nothing for other instances"
void gc_deallocate_unmanaged_instance(T_sp obj) {
  obj_deallocate_unmanaged_instance(obj);
}

#define ARGS_gc_bytes_allocated "()"
#define DECL_gc_bytes_allocated ""
#define DOCS_gc_bytes_allocated "Return the number of bytes allocated since Clasp started. Two values are returned the number reported by the GC and the number calculated by Clasp"
core::T_mv gc_bytes_allocated() {
  size_t gc_bytes = 0;
#ifdef USE_BOEHM
  gc_bytes = GC_get_total_bytes();
#endif
#ifdef USE_MPS
  gc_bytes = 0; // IMPLEMENT_MEF(BF("Figure out how to get the total bytes allocated using MPS"));
#endif
  size_t my_bytes = globalBytesAllocated;
  ASSERT(gc_bytes < gc::most_positive_fixnum && my_bytes < gc::most_positive_fixnum);
  return Values(core::clasp_make_fixnum(gc_bytes), core::clasp_make_fixnum(my_bytes));
}

#define ARGS_core_header_kind "()"
#define DECL_core_header_kind ""
#define DOCS_core_header_kind "Return the header kind for the object"
Fixnum core_header_kind(T_sp obj) {
  if (obj.consp()) {
#if defined(USE_BOEHM) && defined(USE_CXX_DYNAMIC_CAST)
    return reinterpret_cast<Fixnum>(&typeid(*obj));
#else
    return gctools::GCKind<core::Cons_O>::Kind;
#endif
  } else if (obj.generalp()) {
#if defined(USE_BOEHM) && defined(USE_CXX_DYNAMIC_CAST)
    return reinterpret_cast<Fixnum>(&typeid(*obj));
#else
    void *mostDerived = gctools::untag_general<void *>(obj.raw_());
    gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(ClientPtrToBasePtr(mostDerived));
#ifdef USE_MPS
    ASSERT(header->kindP());
#endif
    return header->kind();
#endif
  } else if (obj.fixnump()) {
    return kind_fixnum;
  } else if (obj.single_floatp()) {
    return kind_single_float;
  } else if (obj.characterp()) {
    return kind_character;
  }
  printf("%s:%d HEADER-KIND requested for a non-general object - Clasp needs to define hard-coded kinds for non-general objects - returning -1 for now", __FILE__, __LINE__);
  return clasp_make_fixnum(-1);
}

#define ARGS_core_hardwired_kinds "()"
#define DECL_core_hardwired_kinds ""
#define DOCS_core_hardwired_kinds "Return the header kind for the object"
core::T_mv core_hardwired_kinds() {
  List_sp result = Cons_O::createList(Cons_O::create(core::Str_O::create("FIXNUM"), clasp_make_fixnum(kind_fixnum)),
                                      Cons_O::create(core::Str_O::create("SINGLE_FLOAT"), clasp_make_fixnum(kind_single_float)),
                                      Cons_O::create(core::Str_O::create("CHARACTER"), clasp_make_fixnum(kind_character)));
  List_sp ignoreClasses = _Nil<T_O>(); // Cons_O::createList(Str_O::create("core__Cons_O") <-- future when CONS are in their own pool
  return Values(result, ignoreClasses, clasp_make_fixnum(kind_first_general), clasp_make_fixnum(kind_first_alien), clasp_make_fixnum(kind_last_alien), clasp_make_fixnum(kind_first_instance));
}

#if 0
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
#endif

#if 0
#define ARGS_af_testArray0 "()"
#define DECL_af_testArray0 ""
#define DOCS_af_testArray0 "testArray0"
    void af_testArray0()
    {_G();
//        int N = 4;
        printf("Creating Array0(4)\n");
        gctools::Array0<core::T_sp> v;
        v.allocate(4,_Nil<T_O>());
        Array0_dump(v,"Nil*4");
        gctools::Array0<core::T_sp> w;
        w.allocate(4,_Nil<T_O>());
        for (int i(0); i<4; ++i ) {
            w[i] = core::make_fixnum(i);
        }
        Array0_dump(w,"Fixnum*4");
        printf("Creating ValueFrame\n");
        core::ValueFrame_sp vf = ValueFrame_O::create_fill_args(_Nil<core::ActivationFrame_O>(),core::make_fixnum(1), core::make_fixnum(2), core::make_fixnum(3));
        printf("Creating another 1 ValueFrame\n");
        vf = ValueFrame_O::create_fill_args(_Nil<core::ActivationFrame_O>(),core::make_fixnum(1), core::make_fixnum(2), core::make_fixnum(3));
        printf("Creating another 2 ValueFrame\n");
        vf = ValueFrame_O::create_fill_args(_Nil<core::ActivationFrame_O>(),core::make_fixnum(1), core::make_fixnum(2), core::make_fixnum(3));
        printf("Leaving scope\n");
    }
#endif
};

#ifdef USE_BOEHM

struct ReachableClass {
  ReachableClass() : _Kind(gctools::KIND_null){};
  ReachableClass(gctools::GCKindEnum tn) : _Kind(tn), instances(0), totalSize(0) {}
  void update(size_t sz) {
    ++this->instances;
    this->totalSize += sz;
  };
  gctools::GCKindEnum _Kind;
  size_t instances;
  size_t totalSize;
  size_t print(const std::string &shortName) {
    core::Fixnum k = this->_Kind;
    stringstream className;
    if (k <= gctools::KIND_max) {
      className << obj_name(this->_Kind);
    } else {
      className << "Unknown";
    }
    printf("%s: total_size: %10lu count: %8lu avg.sz: %8lu kind: %s/%zu\n", shortName.c_str(),
           this->totalSize, this->instances, this->totalSize / this->instances, className.str().c_str(), k);
    return this->totalSize;
  }
};

#if 0
struct ReachableContainer {
  ReachableContainer() : typeidName(NULL){};
  ReachableContainer(const char *tn) : typeidName(tn), instances(0), totalSize(0), largest(0) {}
  void update(size_t sz) {
    ++this->instances;
    this->totalSize += sz;
    if (sz > this->largest) {
      this->largest = sz;
    }
  };
  const char *typeidName;
  size_t instances;
  size_t totalSize;
  size_t largest;
  size_t print(const std::string &shortName) {
    printf("%s: total_size: %10lu count: %8lu  avg.sz: %8lu  largest: %8lu %s\n", shortName.c_str(),
           this->totalSize, this->instances, this->totalSize / this->instances, this->largest, this->typeidName);
    return this->totalSize;
  }
};
#endif

typedef map<gctools::GCKindEnum, ReachableClass> ReachableClassMap;
static ReachableClassMap *static_ReachableClassKinds;
static size_t invalidHeaderTotalSize = 0;
int globalSearchMarker = 0;
extern "C" {
void boehm_callback_reachable_object(void *ptr, size_t sz, void *client_data) {
  gctools::Header_s *h = reinterpret_cast<gctools::Header_s *>(ptr);
  if (h->isValid()) {
    if (h->markerMatches(globalSearchMarker)) {
      ReachableClassMap::iterator it = static_ReachableClassKinds->find(h->kind());
      if (it == static_ReachableClassKinds->end()) {
        ReachableClass reachableClass(h->kind());
        reachableClass.update(sz);
        (*static_ReachableClassKinds)[h->kind()] = reachableClass;
      } else {
        it->second.update(sz);
      }
    }
  } else {
    invalidHeaderTotalSize += sz;
  }
}
};

template <typename T>
size_t dumpResults(const std::string &name, const std::string &shortName, T *data) {
  printf("-------------------- %s -------------------\n", name.c_str());
  typedef typename T::value_type::second_type value_type;
  vector<value_type> values;
  for (auto it : *data) {
    values.push_back(it.second);
  }
  size_t totalSize(0);
  sort(values.begin(), values.end(), [](const value_type &x, const value_type &y) {
            return (x.totalSize > y.totalSize);
  });
  for (auto it : values) {
    totalSize += it.print(shortName);
  }
  return totalSize;
}

#endif

#ifdef USE_MPS
struct ReachableMPSObject {
  ReachableMPSObject(int k) : kind(k){};
  int kind = 0;
  size_t instances = 0;
  size_t totalMemory = 0;
  size_t largest = 0;
  size_t print(const std::string &shortName) {
    if (this->instances > 0) {
      printf("%s: totalMemory: %10lu count: %8lu largest: %8lu avg.sz: %8lu %s/%d\n", shortName.c_str(),
             this->totalMemory, this->instances, this->largest, this->totalMemory / this->instances, obj_name((gctools::GCKindEnum) this->kind), this->kind);
    }
    return this->totalMemory;
  }
};

extern "C" {
void amc_apply_stepper(mps_addr_t client, void *p, size_t s) {
  gctools::Header_s *header = reinterpret_cast<gctools::Header_s *>(gctools::ClientPtrToBasePtr(client));
  vector<ReachableMPSObject> *reachablesP = reinterpret_cast<vector<ReachableMPSObject> *>(p);
  if (header->kindP()) {
    ReachableMPSObject &obj = (*reachablesP)[header->kind()];
    ++obj.instances;
    size_t sz = (char *)(obj_skip(client)) - (char *)client;
    obj.totalMemory += sz;
    if (sz > obj.largest)
      obj.largest = sz;
  }
}
};

size_t dumpMPSResults(const std::string &name, const std::string &shortName, vector<ReachableMPSObject> &values) {
  printf("-------------------- %s -------------------\n", name.c_str());
  size_t totalSize(0);
  typedef ReachableMPSObject value_type;
  sort(values.begin(), values.end(), [](const value_type &x, const value_type &y) {
            return (x.totalMemory > y.totalMemory);
  });
  for (auto it : values) {
    totalSize += it.print(shortName);
  }
  return totalSize;
}

#endif // USE_MPS

namespace gctools {

#define ARGS_af_gcInfo "(&optional x (marker 0))"
#define DECL_af_gcInfo ""
#define DOCS_af_gcInfo "gcInfo - Return info about the reachable objects"
T_mv af_gcInfo(T_sp x, Fixnum_sp marker) {
  _G();
#ifdef USE_MPS
  return Values(_Nil<core::T_O>());
#endif
#ifdef USE_BOEHM
  return Values(_Nil<core::T_O>());
#endif
};

#define ARGS_af_monitorAllocations "(on &key (backtrace-start 0) (backtrace-count 0) (backtrace-depth 6))"
#define DECL_af_monitorAllocations ""
#define DOCS_af_monitorAllocations "gcMonitorAllocations"
void af_monitorAllocations(bool on, Fixnum_sp backtraceStart, Fixnum_sp backtraceCount, Fixnum_sp backtraceDepth) {
  global_monitorAllocations.on = on;
  global_monitorAllocations.counter = 0;
  if (backtraceStart.unsafe_fixnum() < 0 ||
      backtraceCount.unsafe_fixnum() < 0 ||
      backtraceDepth.unsafe_fixnum() < 0) {
    SIMPLE_ERROR(BF("Keyword arguments must all be >= 0"));
  }
  global_monitorAllocations.start = backtraceStart.unsafe_fixnum();
  global_monitorAllocations.end = backtraceStart.unsafe_fixnum() + backtraceCount.unsafe_fixnum();
  global_monitorAllocations.backtraceDepth = backtraceDepth.unsafe_fixnum();
  printf("%s:%d  monitorAllocations set to %d\n", __FILE__, __LINE__, on);
};

#define ARGS_af_gcMarker "(&optional marker)"
#define DECL_af_gcMarker ""
#define DOCS_af_gcMarker "gcMarker"
Fixnum af_gcMarker(Fixnum_sp marker) {
  _G();
#ifdef USE_BOEHM
#ifdef USE_BOEHM_MEMORY_MARKER
  if (marker.nilp()) {
    return gctools::globalBoehmMarker;
  }
  ASSERT(marker.fixnump());
  Fixnum oldm = gctools::globalBoehmMarker;
  Fixnum m = marker.unsafe_fixnum();
  gctools::globalBoehmMarker = m;
  return oldm;
#endif
#else
  printf("%s:%d Only boehm supports memory markers\n", __FILE__, __LINE__);
#endif
  return 0;
}

SYMBOL_EXPORT_SC_(GcToolsPkg, STARallocPatternStackSTAR);
SYMBOL_EXPORT_SC_(GcToolsPkg, ramp);
SYMBOL_EXPORT_SC_(GcToolsPkg, rampCollectAll);

#define ARGS_af_allocPatternBegin "(pattern)"
#define DECL_af_allocPatternBegin ""
#define DOCS_af_allocPatternBegin "allocPatternBegin - pass either gctools:ramp or gctools:ramp-collect-all"
void af_allocPatternBegin(Symbol_sp pattern) {
#ifdef USE_MPS
  if (pattern == _sym_ramp || pattern == _sym_rampCollectAll) {
    core::List_sp patternStack = gctools::_sym_STARallocPatternStackSTAR->symbolValue();
    patternStack = core::Cons_O::create(pattern, patternStack);
    gctools::_sym_STARallocPatternStackSTAR->setf_symbolValue(patternStack);
    if (pattern == _sym_ramp) {
      mps_ap_alloc_pattern_begin(_global_automatic_mostly_copying_allocation_point, mps_alloc_pattern_ramp());
    } else {
      mps_ap_alloc_pattern_begin(_global_automatic_mostly_copying_allocation_point, mps_alloc_pattern_ramp_collect_all());
    }
    return;
  }
  SIMPLE_ERROR(BF("Only options for alloc-pattern-begin is %s@%p and %s@%p - you passed: %s@%p") % _rep_(_sym_ramp) % _sym_rampCollectAll.raw_() % _rep_(_sym_rampCollectAll) % _sym_rampCollectAll.raw_() % _rep_(pattern) % pattern.raw_());
#endif
};

#define ARGS_af_allocPatternEnd "()"
#define DECL_af_allocPatternEnd ""
#define DOCS_af_allocPatternEnd "allocPatternEnd - end the current alloc-pattern - return what it was"
Symbol_sp af_allocPatternEnd() {
  Symbol_sp pattern(_Nil<core::Symbol_O>());
#ifdef USE_MPS
  core::List_sp patternStack = gctools::_sym_STARallocPatternStackSTAR->symbolValue();
  if (patternStack.nilp())
    return _Nil<core::Symbol_O>();
  pattern = gc::As<core::Symbol_sp>(oCar(patternStack));
  gctools::_sym_STARallocPatternStackSTAR->setf_symbolValue(oCdr(patternStack));
  if (pattern == _sym_ramp) {
    mps_ap_alloc_pattern_end(_global_automatic_mostly_copying_allocation_point, mps_alloc_pattern_ramp());
  } else {
    mps_ap_alloc_pattern_end(_global_automatic_mostly_copying_allocation_point, mps_alloc_pattern_ramp_collect_all());
  }
#endif
  return pattern;
};

#define ARGS_af_room "(&optional x (marker 0) msg)"
#define DECL_af_room ""
#define DOCS_af_room "room - Return info about the reachable objects.  x can be T, nil, :default - as in ROOM.  marker can be a fixnum (0 - matches everything, any other number/only objects with that marker)"
T_mv af_room(T_sp x, Fixnum_sp marker, T_sp tmsg) {
  string smsg = "Total";
  if (Str_sp msg = tmsg.asOrNull<Str_O>()) {
    smsg = msg->get();
  }
#ifdef USE_MPS
  mps_word_t numCollections = mps_collections(gctools::_global_arena);
  size_t arena_committed = mps_arena_committed(gctools::_global_arena);
  size_t arena_reserved = mps_arena_reserved(gctools::_global_arena);
  printf("%12lu collections\n", numCollections);
  printf("%12lu mps_arena_committed\n", arena_committed);
  printf("%12lu mps_arena_reserved\n", arena_reserved);
  printf("%12lu finalization requests\n", globalMpsMetrics.finalizationRequests);
  size_t totalAllocations = globalMpsMetrics.nonMovingAllocations + globalMpsMetrics.movingAllocations + globalMpsMetrics.movingZeroRankAllocations + globalMpsMetrics.unknownAllocations;
  printf("%12lu total allocations\n", totalAllocations);
  printf("%12lu    non-moving(AWL) allocations\n", globalMpsMetrics.nonMovingAllocations);
  printf("%12lu    moving(AMC) allocations\n", globalMpsMetrics.movingAllocations);
  printf("%12lu    moving zero-rank(AMCZ) allocations\n", globalMpsMetrics.movingZeroRankAllocations);
  printf("%12lu    unknown(configurable) allocations\n", globalMpsMetrics.unknownAllocations);
  printf("%12lu total memory allocated\n", globalMpsMetrics.totalMemoryAllocated);
  vector<ReachableMPSObject> reachables;
  for (int i = 0; i < gctools::KIND_max; ++i) {
    reachables.push_back(ReachableMPSObject(i));
  }
  mps_amc_apply(_global_amc_pool, amc_apply_stepper, &reachables, 0);
  dumpMPSResults("Reachable Kinds", "AMCpool", reachables);
#endif
#ifdef USE_BOEHM
  globalSearchMarker = core::unbox_fixnum(marker);
  static_ReachableClassKinds = new (ReachableClassMap);
  invalidHeaderTotalSize = 0;
  GC_enumerate_reachable_objects_inner(boehm_callback_reachable_object, NULL);
  printf("Walked LispKinds\n");
  size_t totalSize(0);
  totalSize += dumpResults("Reachable ClassKinds", "class", static_ReachableClassKinds);
  printf("Done walk of memory  %lu ClassKinds\n", static_ReachableClassKinds->size());
  printf("%s invalidHeaderTotalSize = %12lu\n", smsg.c_str(), invalidHeaderTotalSize);
  printf("%s memory usage (bytes):    %12lu\n", smsg.c_str(), totalSize);
  printf("%s GC_get_heap_size()       %12lu\n", smsg.c_str(), GC_get_heap_size());
  printf("%s GC_get_free_bytes()      %12lu\n", smsg.c_str(), GC_get_free_bytes());
  printf("%s GC_get_bytes_since_gc()  %12lu\n", smsg.c_str(), GC_get_bytes_since_gc());
  printf("%s GC_get_total_bytes()     %12lu\n", smsg.c_str(), GC_get_total_bytes());

  delete static_ReachableClassKinds;
#endif
  gc::GCStack *stack = threadLocalStack();
  size_t totalMaxSize = stack->maxSize();
#if defined(USE_BOEHM) && defined(BOEHM_ONE_BIG_STACK)
  printf("Lisp-stack bottom %p cur %p limit %p\n", stack->_StackBottom, stack->_StackCur, stack->_StackLimit);
#endif
  printf("High water mark (max used) side-stack size: %u\n", totalMaxSize);
  return Values(_Nil<core::T_O>());
};
};

extern "C" {
void dbg_room() {
  af_room(_Nil<core::T_O>(), core::make_fixnum(0), _Nil<core::T_O>());
}
}

namespace gctools {

#ifdef USE_MPS

#define ARGS_af_mpsTelemetryFlush "()"
#define DECL_af_mpsTelemetryFlush ""
#define DOCS_af_mpsTelemetryFlush "mpsTelemetryFlush"
void af_mpsTelemetryFlush() {
  _G();
  mps_telemetry_flush();
};

#define ARGS_af_mpsTelemetrySet "(flags)"
#define DECL_af_mpsTelemetrySet ""
#define DOCS_af_mpsTelemetrySet "mpsTelemetrySet"
void af_mpsTelemetrySet(Fixnum_sp flags) {
  _G();
  mps_telemetry_set(unbox_fixnum(flags));
};

#define ARGS_af_mpsTelemetryReset "(flags)"
#define DECL_af_mpsTelemetryReset ""
#define DOCS_af_mpsTelemetryReset "mpsTelemetryReset"
void af_mpsTelemetryReset(Fixnum_sp flags) {
  _G();
  mps_telemetry_reset(unbox_fixnum(flags));
};

#endif

#define ARGS_af_stackDepth "()"
#define DECL_af_stackDepth ""
#define DOCS_af_stackDepth "stackDepth"
core::T_sp af_stackDepth() {
  int z = 0;
  void *zp = &z;
  size_t stackDepth = (char *)_global_stack_marker - (char *)zp;
  return core::make_fixnum((uint)stackDepth);
};

#define ARGS_af_garbageCollect "()"
#define DECL_af_garbageCollect ""
#define DOCS_af_garbageCollect "garbageCollect"
void af_garbageCollect() {
  _G();
#ifdef USE_BOEHM
  GC_gcollect();
#endif
//        printf("%s:%d Starting garbage collection of arena\n", __FILE__, __LINE__ );
#ifdef USE_MPS
  mps_arena_collect(_global_arena);
  processMpsMessages();
  mps_arena_release(_global_arena);
#endif
  //        printf("Garbage collection done\n");
};

#define ARGS_af_cleanup "()"
#define DECL_af_cleanup ""
#define DOCS_af_cleanup "cleanup"
void af_cleanup() {
  _G();
#ifdef USE_MPS
  processMpsMessages();
#endif
};

#define ARGS_af_debugAllocations "(arg)"
#define DECL_af_debugAllocations ""
#define DOCS_af_debugAllocations "debugAllocations"
void af_debugAllocations(T_sp debugOn) {
  _G();
  _GlobalDebugAllocations = debugOn.isTrue();
};

#ifdef USE_GC_REF_COUNT_WRAPPER
#define ARGS_af_gcheader "(addr &optional (msg \"\") )"
#define DECL_af_gcheader ""
#define DOCS_af_gcheader "gcheader"
void af_gcheader(core::Pointer_sp addr, const string &msg) {
  _G();
  GCWrapper<core::T_O>::GCHeader *gch = reinterpret_cast<GCWrapper<core::T_O>::GCHeader *>(addr->ptr());
  //        printf("%s Kind = %lu   RefCount=%d\n", msg.c_str(), gch->_Kind, gch->_ReferenceCount);
};

#define ARGS_af_gcaddress "(obj)"
#define DECL_af_gcaddress ""
#define DOCS_af_gcaddress "gcaddress"
core::Pointer_sp af_gcaddress(core::T_sp obj) {
  _G();
  T_O *ptr = dynamic_cast<T_O *>(obj.get());
  void *hptr = reinterpret_cast<void *>(GCWrapper<core::T_O>::gcHeader(ptr));
  core::Pointer_sp po = core::Pointer_O::create(hptr);
  return po;
};

#define ARGS_af_testReferenceCounting "()"
#define DECL_af_testReferenceCounting ""
#define DOCS_af_testReferenceCounting "testReferenceCounting"
void af_testReferenceCounting() {
  _G();
  core::Pointer_sp p;
  {
    core::Fixnum_sp fn = core::make_fixnum(20);
    p = af_gcaddress(fn);
    af_gcheader(p, "Start");
    core::Fixnum_sp fn2 = fn;
    af_gcheader(p, "Assignment");
    fn2 = fn2;
    af_gcheader(p, "fn2 = fn2");
    fn2 = core::make_fixnum(1);
    af_gcheader(p, "fn2 = make_fixnum(1)");
    fn = core::make_fixnum(0);
    //            af_gcheader(p,"fn = make_fixnum(0)");
  }
  //        af_gcheader(p,"fn went out of scope");
};
#endif

void initialize_gc_functions() {

  //            core::af_def(GcToolsPkg,"testVec0",&af_testVec0);
  //            core::af_def(GcToolsPkg,"testArray0",&af_testArray0);
  core::af_def(GcToolsPkg, "gcInfo", &af_gcInfo);
  core::af_def(GcToolsPkg, "gcMarker", &af_gcMarker, ARGS_af_gcMarker, DECL_af_gcMarker, DOCS_af_gcMarker);
  core::af_def(GcToolsPkg, "monitorAllocations", &af_monitorAllocations, ARGS_af_monitorAllocations, DECL_af_monitorAllocations, DOCS_af_monitorAllocations);
  core::af_def(ClPkg, "room", &af_room, ARGS_af_room, DECL_af_room, DOCS_af_room);
  core::af_def(GcToolsPkg, "garbageCollect", &af_garbageCollect);
  core::af_def(GcToolsPkg, "stackDepth", &af_stackDepth);
  core::af_def(GcToolsPkg, "cleanup", &af_cleanup);
  core::af_def(GcToolsPkg, "maxBootstrapKinds", &af_maxBootstrapKinds);
  core::af_def(GcToolsPkg, "bootstrapKindP", &af_bootstrapKindP);
  core::af_def(GcToolsPkg, "bootstrapKindSymbols", &af_bootstrapKindSymbols);
  core::af_def(GcToolsPkg, "allocPatternBegin", &af_allocPatternBegin);
  core::af_def(GcToolsPkg, "allocPatternEnd", &af_allocPatternEnd);
  core::af_def(GcToolsPkg, "bytes_allocated", &gc_bytes_allocated);
  core::af_def(GcToolsPkg, "deallocate_unmanaged_instance", &gc_deallocate_unmanaged_instance );

  _sym_STARallocPatternStackSTAR->defparameter(_Nil<core::T_O>());
#ifdef USE_MPS
  core::af_def(GcToolsPkg, "mpsTelemetrySet", &af_mpsTelemetrySet);
  core::af_def(GcToolsPkg, "mpsTelemetryReset", &af_mpsTelemetryReset);
  core::af_def(GcToolsPkg, "mpsTelemetryFlush", &af_mpsTelemetryFlush);
#endif

  //	    SYMBOL_EXPORT_SC_(GcTools,linkExternalGlobalsInModule);
  //	    Defun(linkExternalGlobalsInModule);
  Defun(debugAllocations);
  CoreDefun(header_kind);
  CoreDefun(hardwired_kinds);
};
};
